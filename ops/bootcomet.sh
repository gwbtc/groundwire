#!/bin/bash
# boottip.sh <pier-name> <ames-port> <patp-no-sig> <feed>
#
# Fresh boot of a Groundwire confidential comet from the CLEANROOM tip pill
# (sha256 4e0af17b937abe2efcdcaeb965bfbb9013270285837c9a47bfd249edd55e6851,
#  built locally by `fyrd (solid:pill ...)` from urbit@de3222d36a --
#  hd/cc-kernel tip).  Neither commit past the P67 pill touches sys/vane/,
#  so the vane logic is byte-identical; the delta is test/aqua scaffolding
#  and the deletion of lib/gw-btc-pass.hoon.
#
# -w and -G are given TOGETHER: a bare -G falls through to [%come ~] in
# _boothack_doom() and SELF-MINES A DIFFERENT COMET (OPERATIONS.md 5.3).
# -p is pinned because every one of these comets commits a fief naming this
# exact IP:port; if the ship is not reachable there the fief is a lie.
set -e
P="$1"; PORT="$2"; WHO="$3"; FEED="$4"
PILL=/opt/gw/pills/gw-cc-kernel-solid-TIP.pill
PIER="/opt/gw/piers/$P"
want=4e0af17b937abe2efcdcaeb965bfbb9013270285837c9a47bfd249edd55e6851

[ -e "$PIER" ] && { echo "REFUSING: $PIER already exists"; exit 1; }
[ -f "$PILL" ] || { echo "REFUSING: $PILL missing"; exit 1; }
got=$(sha256sum "$PILL" | cut -d' ' -f1)
[ "$got" = "$want" ] || { echo "REFUSING: pill sha256 $got != $want"; exit 1; }
if ss -lunp 2>/dev/null | grep -q ":$PORT "; then
  echo "REFUSING: udp/$PORT already bound:"; ss -lunp | grep ":$PORT "; exit 1
fi

mkdir -p /opt/gw/piers
: > "/opt/gw/$P.log"
setsid nohup /opt/gw/bin/gw-vere -t --loom 32 \
  -c "$PIER" -p "$PORT" -w "$WHO" -G "$FEED" \
  -B "$PILL" \
  >> "/opt/gw/$P.log" 2>&1 </dev/null &
echo "booting $P (udp/$PORT) as ~$WHO from the TIP pill, pid $!"
