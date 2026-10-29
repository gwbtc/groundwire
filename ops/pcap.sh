#!/bin/bash
# pcap.sh <outfile> <seconds> <bpf-expression>   -- run ON a droplet
#
# Start it detached:
#   setsid nohup ops/pcap.sh OUT SECS 'udp and host 1.2.3.4' >LOG 2>&1 </dev/null &
#
# WHY THIS EXISTS.  Ames log lines are not evidence of delivery.  `plea` is
# the SEND path -- it means the sender queued a message, not that anyone got
# it -- and `|hi ... successful` has never once fired on this rig (0
# occurrences across 113.166 lines on three ships, including pairs that
# demonstrably exchange messages), so a test scored by grepping for it
# reports failure regardless of what the network did.
#
# Run this at BOTH ends of a test and compare the two files.  Then neither
# ship's claim rests on the other's log, and a direction with zero packets
# is a measurement rather than an absence of logging.
set -u
OUT="${1:?out}"; SECS="${2:?secs}"; BPF="${3:?bpf}"
rm -f "$OUT"
echo "$(date -u +%FT%TZ) capture start: $BPF -> $OUT for ${SECS}s"
timeout "$SECS" tcpdump -nni any -s 300 -U -w "$OUT" "$BPF"
echo "$(date -u +%FT%TZ) capture end rc=$? ($(stat -c %s "$OUT" 2>/dev/null) bytes)"
