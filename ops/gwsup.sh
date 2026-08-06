#!/bin/bash
# gwsup.sh <pier-name> <ames-port>
#
# Supervisor for a Groundwire light-client ship.  Filed against gwbtc/node#1:
# tcp-sidecar SIGSEGVs, %bitcoin-client keeps believing its peers are live,
# every send returns "no such connection" forever, and the ship stops making
# progress while looking healthy.  It does not self-heal and restarting the
# sidecar alone does not fix it -- the agent's peer table has to be cleared
# with &kill-peer-connections and re-seeded.
#
# LIVENESS SIGNAL: event-log progress, not block height and not peer count.
# NOTE: <pier>/.urb/log is a DIRECTORY and its mtime never advances (LMDB
# writes into an already-created data.mdb; the dirent is untouched).  Verified
# on a known-live ship: /opt/gw/piers/smoke/.urb/log mtime was 21h stale while
# the ship was demonstrably processing events.  The real signal is the newest
# mtime over <pier>/.urb/log/*/data.mdb (one dir per epoch).
#
# Interventions are logged to /opt/gw/sup-<name>.log with a UTC timestamp and
# a counter, so "how often did the watchdog fire" is a countable number.

set -u
P="${1:?pier name}"
PORT="${2:?ames port}"
PIER="/opt/gw/piers/$P"

# SINGLETON.  Two supervisors on one pier both see VERE-DOWN, both relaunch,
# and the loser's ship dies on "mesa: bind: address already in use" -- which
# reads exactly like a crash loop.  Observed live: every one of the three
# droplets was running TWO gwsup.sh instances for the same pier, which is also
# why the ships came back after being stopped.  flock makes a second start a
# no-op instead of a hazard.
exec 9>"/opt/gw/.gwsup-$P.lock"
if ! flock -n 9; then
  echo "gwsup.sh: a supervisor for $P is already running; refusing" >&2
  exit 0
fi
VLOG="/opt/gw/$P.log"
SLOG="/opt/gw/sup-$P.log"
POOL="/opt/gw/peerpool.txt"
USED="/opt/gw/used-$P.txt"
STALE=300          # seconds of no event-log write => WEDGED
POLL=30
COOLDOWN=300       # min seconds between recovery attempts
LOOM=32

touch "$USED" "$POOL"

log() { echo "$(date -u +%FT%TZ) [$P] $*" >> "$SLOG"; }

# --- process identification (exact, never prefix-matched) --------------------
king_pid() { ps -eo pid=,args= | awk -v p="$PIER" '$NF==p && /gw-vere/ {print $1}'; }
serf_pid() { ps -eo pid=,args= | awk -v p="$PIER" '/snap-dir/ { for(i=1;i<=NF;i++) if($i=="--snap-dir" && $(i+1)==p) print $1 }'; }
sc_pids()  { for x in $(pgrep -f 'bin/tcp-sidecar' 2>/dev/null); do
               [ "$(readlink /proc/$x/cwd 2>/dev/null)" = "$PIER" ] && echo "$x"
             done; }

evt_age() {
  local newest now
  newest=$(stat -c %Y "$PIER"/.urb/log/*/data.mdb 2>/dev/null | sort -n | tail -1)
  [ -z "$newest" ] && { echo 99999; return; }
  now=$(date +%s)
  echo $(( now - newest ))
}

# --- peer pool --------------------------------------------------------------
# Blacklist expiry inside %bitcoin-client is ~d3, so a recovery cycle burns
# through seed IPs and reusing the same five is useless.  Refill from the DNS
# seeds whenever the unused remainder runs low.
pool_left() { grep -vxF -f "$USED" "$POOL" 2>/dev/null | wc -l; }

pool_fill() {
  python3 /opt/gw/poolfill.py "$POOL" >> "$SLOG" 2>&1
}

# echo N unused IPs and mark them used
take_peers() {
  local n=$1 ips
  ips=$(grep -vxF -f "$USED" "$POOL" 2>/dev/null | head -n "$n")
  [ -n "$ips" ] && echo "$ips" >> "$USED"
  echo "$ips"
}

add_peers() {
  local ips="$1"
  [ -z "$ips" ] && return 1
  timeout 180 python3 /opt/gw/addpeers.py "$PIER" $ips >> "$SLOG" 2>&1
}

peer_count() {
  timeout 90 python3 /opt/gw/peercount.py "$PIER" 2>/dev/null
}

ensure_sidecar() {
  if [ -z "$(sc_pids)" ]; then
    log "sidecar not running -> starting"
    ( cd "$PIER" && SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt \
        setsid nohup /opt/gw/bin/tcp-sidecar . >> /opt/gw/sc-$P.log 2>&1 </dev/null & )
    sleep 5
    return 0
  fi
  return 1
}

# A stale ship from an earlier run holding our ames port makes every restart
# die with "mesa: bind: address already in use", which looks exactly like a
# crash loop.  Seen twice in this campaign.  Evict any gw-vere bound to our
# port that is not this pier.
free_port() {
  local pid pier
  for pid in $(ss -lunp 2>/dev/null | awk -v p=":$PORT" '$5 ~ p {print $0}' \
               | grep -o 'pid=[0-9]*' | cut -d= -f2 | sort -u); do
    pier=$(ps -o args= -p "$pid" 2>/dev/null | awk '{print $NF}')
    if [ -n "$pier" ] && [ "$pier" != "$PIER" ]; then
      log "  evicting pid $pid ($pier) from port $PORT"
      kill -9 "$pid" 2>/dev/null
      sleep 2
    fi
  done
}

ensure_vere() {
  if [ -z "$(king_pid)" ] && [ -z "$(serf_pid)" ]; then
    N_VERE=$((N_VERE+1))
    log "INTERVENTION #$((N_WEDGE+N_VERE+N_SIDE)) VERE-DOWN (restart #$N_VERE) -> relaunching"
    free_port
    rm -f "$PIER/.vere.lock"
    setsid nohup /opt/gw/bin/gw-vere -t --loom $LOOM -p "$PORT" "$PIER" \
      >> "$VLOG" 2>&1 </dev/null &
    sleep 60
    return 0
  fi
  return 1
}

recover() {
  local why="$1"
  N_WEDGE=$((N_WEDGE+1))
  log "INTERVENTION #$((N_WEDGE+N_VERE+N_SIDE)) WEDGE (recover #$N_WEDGE): $why"
  ensure_sidecar
  sleep 3
  if timeout 150 python3 /opt/gw/poke.py "$PIER" bitcoin-client kill-peer-connections '!>(~)' \
       >> "$SLOG" 2>&1; then
    log "  kill-peer-connections ok"
  else
    log "  kill-peer-connections FAILED (conn.sock unresponsive)"
  fi
  sleep 5
  [ "$(pool_left)" -lt 20 ] && pool_fill
  local ip
  ip=$(take_peers 1)
  if add_peers "$ip"; then log "  re-seeded 1 peer: $ip"; else log "  re-seed FAILED ($ip)"; fi
  LAST_RECOVER=$(date +%s)
}

# ----------------------------------------------------------------------------
# Counters survive a supervisor restart: they are re-derived from the log, so
# "how often did the watchdog fire" stays a true cumulative number.
# NB `grep -c` PRINTS 0 and EXITS 1 when there is no match, so the old
# `$(grep -c ... || echo 0)` produced the two-line string "0\n0" and the first
# $((N_VERE+1)) was a bash syntax error -- the watchdog killed itself at the
# exact moment it was first needed.  Observed live: q2's vere SIGSEGVed, the
# supervisor woke to restart it and died on this instead, and the ship stayed
# down.  head -1 takes grep's own count; ${x:-0} covers a missing file.
_count() { local n; n=$(grep -c "$1" "$SLOG" 2>/dev/null | head -1); echo "${n:-0}"; }
N_WEDGE=$(_count 'WEDGE (recover')
N_VERE=$(_count 'VERE-DOWN')
N_SIDE=$(_count 'SIDECAR-DOWN')
LAST_RECOVER=0
log "supervisor start (pier=$PIER port=$PORT stale=${STALE}s poll=${POLL}s)"
[ "$(pool_left)" -lt 40 ] && pool_fill

while true; do
  # 1. runtime alive?
  if ensure_vere; then continue; fi

  # 2. sidecar alive?  (don't wait out the 5-minute wedge window for this)
  if [ -z "$(sc_pids)" ]; then
    N_SIDE=$((N_SIDE+1))
    log "INTERVENTION #$((N_WEDGE+N_VERE+N_SIDE)) SIDECAR-DOWN (restart #$N_SIDE)"
    ensure_sidecar
    # the sidecar dying is exactly the gwbtc/node#1 trigger: the agent still
    # believes its peers are live, so clear them and re-seed straight away.
    now=$(date +%s)
    if [ $(( now - LAST_RECOVER )) -ge $COOLDOWN ]; then
      recover "sidecar had died"
    fi
    sleep $POLL; continue
  fi

  # 3. event progress
  AGE=$(evt_age)
  if [ "$AGE" -gt "$STALE" ]; then
    now=$(date +%s)
    if [ $(( now - LAST_RECOVER )) -ge $COOLDOWN ]; then
      recover "event log stale ${AGE}s"
    fi
  fi

  sleep $POLL
done
