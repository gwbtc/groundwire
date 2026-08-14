#!/bin/bash
# bootcomet.sh <pier-name> <ames-port> <patp-no-sig> <feed> <pill> <pill-sha256> <desk-src>
#
# Full bringup of a fresh Groundwire confidential comet: boot, install the
# desk, hand to the supervisor, seed peers, report readiness.
#
# FOUR THINGS THIS GETS RIGHT, every one of them found by an earlier version
# failing live.  Read them before changing anything here.
#
#  1. THE PILL IS AN ARGUMENT, AND ITS HASH IS CHECKED.  This script used to
#     hardcode a pill it called "the tip", built from urbit@de3222d36a.  That
#     commit PREDATES 37059d9b6f ("the domain agent's answer is a %verdict"),
#     so sys/lull.hoon has no `+$ verdict` and %gw-btc will not build:
#     `-find.verdict` at app/gw-btc.hoon.  A pill named "TIP" was a week
#     stale and nothing said so.  Pass the pill you mean and the hash you
#     expect; a mismatch refuses rather than booting something else.
#
#  2. ONE VERE BINARY THROUGHOUT.  Booting with one build and supervising
#     with another means the supervisor's first restart reads the pier's
#     snapshot as stale, wants a replay, and dies on it:
#         stale snapshot, downgrade runtime to replay
#         pier: serf unexpectedly shut down
#     -> VERE-DOWN every 60s, forever.  $VERE is used for the boot AND
#     exported as GWSUP_VERE so gwsup.sh relaunches with the same one.
#
#  3. ONLY %gw-btc IS INSTALLED.  The pill already provisions base,
#     vitriol, mcp, kids, node, tcp-sidecar and groundwire.  Only groundwire
#     needs replacing, because the pill's copy predates the
#     %groundwire->%gw-btc rename -- a ship booted from it has %urb-watcher
#     and no %gw-btc.  node and tcp-sidecar are correct as shipped and must
#     NOT be passed: `gwctl.py desks` opens each named desk with a
#     kiln-merge %init, which CLOBBERS one the pill already populated, and
#     clay then refuses every re-commit SILENTLY -- 87 attempts, "revision
#     1 -> 1" each time, nothing printed and nothing failed.  Revision 1 is
#     their correct terminal state.
#
#  4. THE DESK INSTALL'S EXIT STATUS IS NOT SWALLOWED.  An earlier version
#     piped it through `tail`, which discarded the status and let a ship
#     with no %gw-btc report a clean install.
#
# -w and -G go together: a bare -G falls through to [%come ~] and self-mines
# a DIFFERENT comet (OPERATIONS.md 5.3).  -p is pinned because a comet that
# commits a fief naming an IP:port must actually be reachable there, or the
# fief is a lie.
set -euo pipefail

P="${1:?pier name}"
PORT="${2:?ames port}"
WHO="${3:?patp, no sig}"
FEED="${4:?boot feed}"
PILL="${5:?pill path}"
WANT="${6:?expected pill sha256}"
DESK="${7:?desk source dir, e.g. /opt/gw/desks/gw-<commit>}"

GW_ROOT="${GW_ROOT:-/opt/gw}"
VERE="${GWSUP_VERE:-$GW_ROOT/bin/gw-vere}"
PIER="$GW_ROOT/piers/$P"
LOOM="${LOOM:-32}"

cd "$GW_ROOT"

echo "=== 0. preflight ==="
[ -e "$PIER" ] && { echo "REFUSING: $PIER exists (delete it first)"; exit 1; }
[ -f "$PILL" ] || { echo "REFUSING: $PILL missing"; exit 1; }
[ -x "$VERE" ] || { echo "REFUSING: $VERE is not executable"; exit 1; }
[ -d "$DESK" ] || { echo "REFUSING: $DESK missing"; exit 1; }
got=$(sha256sum "$PILL" | cut -d' ' -f1)
[ "$got" = "$WANT" ] || { echo "REFUSING: pill sha256 $got != $WANT"; exit 1; }
if ss -lunp 2>/dev/null | grep -q ":$PORT "; then
  echo "REFUSING: udp/$PORT already bound:"; ss -lunp | grep ":$PORT "; exit 1
fi
echo "pill $WANT OK; runtime $VERE; desk $DESK"

echo "=== 1. boot $P as ~$WHO on udp/$PORT ==="
: > "$GW_ROOT/$P.log"
setsid nohup "$VERE" -t --loom "$LOOM" \
  -c "$PIER" -p "$PORT" -w "$WHO" -G "$FEED" -B "$PILL" \
  >> "$GW_ROOT/$P.log" 2>&1 </dev/null &
echo "booting, pid $!"

echo "=== 2. wait for conn.sock ==="
for i in $(seq 1 240); do
  [ -S "$PIER/.urb/conn.sock" ] && break
  sleep 5
done
[ -S "$PIER/.urb/conn.sock" ] || { echo "FAILED: no conn.sock after 20m"; exit 1; }
echo "conn.sock up after $((i*5))s"
sleep 30

echo "=== 3. install ONLY %gw-btc (see note 3 above) ==="
set +e
timeout 2400 python3 "$GW_ROOT/ops/gwctl.py" desks "$PIER" "gw-btc=$DESK" \
  > "$GW_ROOT/desks-$P.log" 2>&1
RC=$?
set -e
tail -6 "$GW_ROOT/desks-$P.log"
if [ $RC -ne 0 ]; then
  echo "DESK INSTALL FAILED rc=$RC -- see $GW_ROOT/desks-$P.log and $GW_ROOT/$P.log"
  echo "NOT starting a supervisor; a ship with no %gw-btc is not worth restarting."
  exit 1
fi
echo "desks OK"

echo "=== 4. hand to gwsup with the SAME binary (see note 2 above) ==="
GWSUP_VERE="$VERE" setsid nohup "$GW_ROOT/ops/gwsup.sh" "$P" "$PORT" \
  > "$GW_ROOT/gwsup-$P.out" 2>&1 </dev/null &
echo "gwsup launched, GWSUP_VERE=$VERE"
sleep 90

echo "=== 5. seed compact-filter peers (~25 at a time; bulk adds SIGSEGV the sidecar) ==="
timeout 1800 python3 "$GW_ROOT/ops/gwctl.py" seed "$PIER" 6 25 2>&1 | tail -8

echo "=== 6. readiness [synced tip indexing] ==="
timeout 300 python3 "$GW_ROOT/ops/gwctl.py" ready "$PIER" 2>&1 | tail -2
echo "=== bringup of $P DONE ==="
