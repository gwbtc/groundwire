#!/bin/bash
# stopship.sh <pier-name> -- stop a supervised Groundwire ship, in the only
# order that works: supervisor first, then runtime, then sidecar.
#
# Stopping vere while gwsup.sh is alive is a no-op: the supervisor sees
# VERE-DOWN within one 30s poll and relaunches it.  There is no shutdown
# procedure anywhere in OPERATIONS.md; this is it.
#
# Every process is matched EXACTLY (never `pkill -f urbit`, never a prefix):
#   supervisor  argv == "/bin/bash /opt/gw/gwsup.sh <name> <port>"
#   king/serf   the pier as a standalone argv FIELD (see below)
#   sidecar     /proc/<pid>/cwd == <pier>
#
# TWO BUGS FIXED 2026-08-06, both found by a run that believed it had stopped
# three ships and had not:
#
#   1. The king was matched with `$NF==<pier>` -- the pier as the TRAILING
#      argv field.  That only holds for the RESTART form
#      (`gw-vere -t --loom 32 -p <port> <pier>`).  A ship booted with the
#      CREATE form ends `... -G <feed> -B <pill>`, so the trailing field is
#      the PILL and the matcher found nothing at all.  Every comet in every
#      live campaign is first booted with the create form.  Match the pier as
#      an exact standalone FIELD anywhere in argv instead, which covers both
#      forms and is still immune to the `p4c1` / `p4c1b` prefix trap.
#
#   2. `gw-vere -t` does not exit on SIGTERM.  Measured on all three campaign
#      ships: TERM, then a 40s wait, left every king alive with its original
#      PID; SIGINT likewise; only SIGKILL stopped them.  The old script sent
#      one TERM, printed the residue, and returned 0 regardless -- so the
#      caller saw success while the ship kept writing events, and a `cp -a`
#      "backup" taken next captured a live LMDB.  Escalate TERM -> INT -> KILL
#      and EXIT NON-ZERO if anything survives.  A SIGKILL is safe here: the
#      event log is LMDB and durable, and the pier replays on next boot.
set -u
P="${1:?pier name}"
PIER="/opt/gw/piers/$P"

# every gw-vere process (king OR serf) naming exactly this pier
vere_pids() {
  ps -eo pid=,args= | awk -v p="$PIER" '
    /gw-vere/ { for (i = 2; i <= NF; i++) if ($i == p) { print $1; break } }'
}

echo "== stopping $P ($PIER) =="

# 1. supervisors -- there can be several; kill them first or they relaunch.
SUP=$(ps -eo pid=,args= | awk -v n="$P" '$0 ~ /gwsup\.sh/ && $4 == n {print $1}')
for pid in $SUP; do
  echo "  supervisor pid $pid -> TERM"
  kill "$pid" 2>/dev/null
done
sleep 2
for pid in $SUP; do kill -9 "$pid" 2>/dev/null; done

# 2. runtime.  TERM -> INT -> KILL, verifying after each; `-t` ignores the
#    first two.  The serf exits with the king and the event log is durable.
for sig in TERM INT KILL; do
  cur=$(vere_pids)
  [ -z "$cur" ] && break
  echo "  $sig -> $(echo "$cur" | tr '\n' ' ')"
  for pid in $cur; do kill -s "$sig" "$pid" 2>/dev/null; done
  for _ in $(seq 1 20); do [ -z "$(vere_pids)" ] && break; sleep 1; done
done

LEFT=$(vere_pids)
if [ -n "$LEFT" ]; then
  echo "  *** FAILED: gw-vere still alive for $PIER: $(echo "$LEFT" | tr '\n' ' ')"
  echo "  *** the pier is NOT safe to copy or boot from"
  exit 1
fi
echo "  all gw-vere for $PIER are gone"

# 3. sidecar, identified by cwd (it has no port and no distinctive argv).
for pid in $(pgrep -f 'bin/tcp-sidecar' 2>/dev/null); do
  if [ "$(readlink /proc/$pid/cwd 2>/dev/null)" = "$PIER" ]; then
    echo "  sidecar pid $pid -> TERM"
    kill "$pid" 2>/dev/null
  fi
done
sleep 2
for pid in $(pgrep -f 'bin/tcp-sidecar' 2>/dev/null); do
  if [ "$(readlink /proc/$pid/cwd 2>/dev/null)" = "$PIER" ]; then
    echo "  sidecar pid $pid -> KILL"
    kill -9 "$pid" 2>/dev/null
  fi
done

echo "-- residue for $PIER (must be empty) --"
ps -eo pid=,args= | awk -v p="$PIER" '{for (i=2;i<=NF;i++) if ($i==p) {print; break}}'
echo "-- pier preserved --"
ls -d "$PIER" && du -sh "$PIER"
echo "-- stopped cleanly; safe to back up or reboot --"
