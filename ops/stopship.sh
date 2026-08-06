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
#   king        last argv field == <pier>
#   serf        the field after --snap-dir == <pier>
#   sidecar     /proc/<pid>/cwd == <pier>
set -u
P="${1:?pier name}"
PIER="/opt/gw/piers/$P"

echo "== stopping $P ($PIER) =="

# 1. supervisors -- there can be several; gwsup.sh has no singleton guard.
SUP=$(ps -eo pid=,args= | awk -v n="$P" '$0 ~ /gwsup\.sh/ && $4 == n {print $1}')
for pid in $SUP; do
  echo "  supervisor pid $pid -> TERM"
  kill "$pid" 2>/dev/null
done
sleep 2
for pid in $SUP; do kill -9 "$pid" 2>/dev/null; done

# 2. runtime.  SIGTERM to the king; the serf exits with it and the event log
#    is durable, so this is a clean stop with no state loss.
KING=$(ps -eo pid=,args= | awk -v p="$PIER" '$NF==p && /gw-vere/ {print $1}')
for pid in $KING; do
  echo "  king pid $pid -> TERM"
  kill "$pid" 2>/dev/null
done
for i in $(seq 1 30); do
  still=$(ps -eo pid=,args= | awk -v p="$PIER" '$NF==p && /gw-vere/ {print $1}')
  [ -z "$still" ] && break
  sleep 1
done

# 3. sidecar, identified by cwd (it has no port and no distinctive argv).
for pid in $(pgrep -f 'bin/tcp-sidecar' 2>/dev/null); do
  if [ "$(readlink /proc/$pid/cwd 2>/dev/null)" = "$PIER" ]; then
    echo "  sidecar pid $pid -> TERM"
    kill "$pid" 2>/dev/null
  fi
done
sleep 2

echo "-- residue --"
ps -eo pid=,args= | awk -v p="$PIER" '$NF==p || $0 ~ p' | grep -v awk
SERF=$(ps -eo pid=,args= | awk -v p="$PIER" '/snap-dir/ { for(i=1;i<=NF;i++) if($i=="--snap-dir" && $(i+1)==p) print $1 }')
[ -n "$SERF" ] && echo "  WARNING serf still up: $SERF"
echo "-- pier preserved --"
ls -d "$PIER" && du -sh "$PIER"
