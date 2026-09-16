#!/bin/bash
# pcapsum.sh <pcap>  -- packets by src->dst, with first/last timestamps.
#
# The counterpart to pcap.sh.  Summarising BY DIRECTION is the whole point:
# the results that mattered in this campaign were all of the shape "N out,
# M back", and the interesting value of M was repeatedly zero.
#
#   f1 -> f2 (no fief, no sponsor):   8 out,  0 back
#   g2 <-> f2 (sponsor committed):   49 out, 14 back
#   tampered clone -> f2:            38 out,  0 back
#
# It refuses an empty or missing file rather than reporting "0 packets",
# because a capture that never started and a direction that sent nothing
# look identical in a total.
set -u
F="${1:?pcap}"
[ -s "$F" ] || { echo "EMPTY OR MISSING: $F"; exit 1; }
echo "--- $F ($(stat -c %s "$F") bytes) ---"
tcpdump -nnr "$F" 2>/dev/null | awk '{sub(/:$/,"",$4); print $2" -> "$4}' | sort | uniq -c | sort -rn
echo "--- first / last ---"
tcpdump -nnr "$F" 2>/dev/null | head -3
echo "  ..."
tcpdump -nnr "$F" 2>/dev/null | tail -3
echo "--- total: $(tcpdump -nnr "$F" 2>/dev/null | wc -l) packets ---"
