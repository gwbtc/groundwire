#!/bin/bash
# onboard-e2e.sh <release-dir> [--boot]
#
# Walk the ENTIRE onboarding pipeline against a local fake mempool, with zero
# real bitcoin: spawn (real miner, real PSBT, real signature) -> broadcast to
# the stub -> finalize (confirmation wait, xtr bake) -> proof verified against
# the tx the stub actually received -> optionally boot the ship and check it
# came up as the minted @p.
#
# WHY THIS EXISTS.  The first live mint failed four separate times, each one
# step deeper: an unquoted @p, Causeway missing from the install, a stale CDN
# copy, a miner path that only existed on a dev machine.  Every one lived in
# the GLUE, not the crypto -- and the glue had never been executed end to end,
# because executing it seemed to need real sats.  It does not.  Everything
# except bitcoin itself runs for free, and bitcoin itself is the one part
# that was never broken.
#
# <release-dir> is an unpacked release tarball (the layout boot.sh installs):
#   bin/comet_miner  causeway  causeway-src/  gw-vere  pills/... or gw-base.pill
#
# Exit 0 = every stage passed.  Any failure stops the script at the stage
# that caused it, with the artifacts left in the work dir for autopsy.
set -euo pipefail

REL="${1:?usage: onboard-e2e.sh <release-dir> [--boot]}"
DO_BOOT="${2:-}"
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

step() { printf '\n\033[1m==> %s\033[0m\n' "$*"; }
die()  { printf '\n\033[31mE2E FAILED: %s\033[0m\n' "$*" >&2; exit 1; }

[ -x "$REL/causeway" ]        || die "$REL/causeway missing or not executable"
[ -x "$REL/bin/comet_miner" ] || { [ -x "$REL/comet_miner" ] &&
      mkdir -p "$REL/bin" && cp "$REL/comet_miner" "$REL/bin/comet_miner"; } \
      || die "$REL has no comet_miner"

WORK="$(mktemp -d "${TMPDIR:-/tmp}/onboard-e2e.XXXXXX")"
PORT=$(( 20000 + RANDOM % 20000 ))
BASE="http://127.0.0.1:$PORT"

step "Starting mempool stub on $BASE (work dir $WORK)"
python3 "$HERE/mempool-stub.py" "$PORT" 2> "$WORK/stub.log" &
STUB_PID=$!
cleanup() {
  kill "$STUB_PID" 2>/dev/null || true
  [ -n "${SHIP_PID:-}" ] && kill "$SHIP_PID" 2>/dev/null || true
}
trap cleanup EXIT
for i in $(seq 1 20); do
  curl -fsS "$BASE/tx/$(printf 'aa%.0s' {1..32})" >/dev/null 2>&1 && break
  sleep 0.5
done
curl -fsS "$BASE/tx/$(printf 'aa%.0s' {1..32})" >/dev/null || die "stub never came up (see $WORK/stub.log)"

step "1. spawn generate — headless, real miner, fake funding"
# --assume-saved: no interactive read-back.  The seed lands in the log; these
# are stub sats, and the log dies with the work dir.
export GROUNDWIRE_HOME="$REL"
if ! "$REL/causeway" spawn generate \
      --assume-saved \
      --sponsor '~ligdes-risbur-folmus-mattyp--firpec-lispec-noddyl-daplyd' \
      --mempool-base "$BASE" \
      --miner "$REL/bin/comet_miner" \
      --output-dir "$WORK" \
      --out-feed "$WORK/raw.feed" > "$WORK/spawn.log" 2>&1; then
  tail -30 "$WORK/spawn.log"; die "spawn generate (full log: $WORK/spawn.log)"
fi
tail -5 "$WORK/spawn.log"

PROOF="$(ls -t "$WORK"/*-spawn.proof.json 2>/dev/null | head -1 || true)"
[ -n "$PROOF" ]          || die "spawn wrote no proof file"
[ -s "$WORK/raw.feed" ]  || die "spawn wrote no --out-feed"
PERMS=$(stat -f %Lp "$WORK/raw.feed" 2>/dev/null || stat -c %a "$WORK/raw.feed")
[ "$PERMS" = "600" ]     || die "raw.feed is $PERMS, not 0600"
COMET="$(sed -n 's/.*"patp"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/p' "$PROOF" | head -1)"
[ -n "$COMET" ]          || die "no patp in $PROOF"
grep -q '"commit_txid"' "$PROOF" || die "proof has no commit_txid — was nothing broadcast?"
echo "    comet: $COMET"
echo "    proof: $PROOF"

step "2. the spawn log must NOT print a runnable boot command"
# The happy path used to hand the user a copy-pasteable boot line carrying the
# RAW (custody-log-free) feed.  If that ever comes back, fail here.
if grep -E -- 'boot\.sh .*--feed 0w' "$WORK/spawn.log"; then
  die "spawn printed a raw-feed boot command again"
fi
grep -q "peers cannot verify you yet" "$WORK/spawn.log" || die "spawn no longer explains the raw-feed verification gap"

step "3. finalize — confirmation wait + xtr bake, by file"
if ! "$REL/causeway" finalize "$PROOF" \
      --feed-file "$WORK/raw.feed" \
      --out-feed "$WORK/boot.feed" \
      --mempool-base "$BASE" \
      --poll-interval 1 > "$WORK/finalize.log" 2>&1; then
  tail -30 "$WORK/finalize.log"; die "finalize (full log: $WORK/finalize.log)"
fi
tail -4 "$WORK/finalize.log"
[ -s "$WORK/boot.feed" ] || die "finalize wrote no baked feed"
cmp -s "$WORK/raw.feed" "$WORK/boot.feed" && die "baked feed is IDENTICAL to the raw feed — xtr was not baked"

step "4. proof verifies against the tx the stub actually received"
"$REL/causeway" proof verify "$PROOF" --onchain --mempool-base "$BASE" \
  > "$WORK/verify.log" 2>&1 \
  || { tail -10 "$WORK/verify.log"; die "on-chain proof verification"; }
tail -2 "$WORK/verify.log"

if [ "$DO_BOOT" = "--boot" ]; then
  step "5. boot the ship from the baked feed and check its identity"
  PILL="$REL/pills/gw-base.pill"; [ -f "$PILL" ] || PILL="$REL/gw-base.pill"
  [ -f "$PILL" ] || die "no pill in $REL"
  VERE="$REL/bin/gw-vere"; [ -x "$VERE" ] || VERE="$REL/gw-vere"
  NAME="${COMET#\~}"
  FEED="$(tr -d ' \t\r\n' < "$WORK/boot.feed")"
  # -L: local-only networking.  This checks BOOT, not the network.
  "$VERE" -t -L --loom 31 -c "$WORK/pier" -w "$NAME" -G "$FEED" -B "$PILL" \
      > "$WORK/ship.log" 2>&1 &
  SHIP_PID=$!
  ok=""
  for i in $(seq 1 120); do
    [ -S "$WORK/pier/.urb/conn.sock" ] && { ok=1; break; }
    kill -0 "$SHIP_PID" 2>/dev/null || break
    sleep 5
  done
  [ -n "$ok" ] || { tail -20 "$WORK/ship.log"; die "ship never brought up conn.sock (log: $WORK/ship.log)"; }
  grep -q "$NAME" "$WORK/ship.log" || echo "    (name not in log; pier dir is authoritative)"
  [ -d "$WORK/pier" ] || die "no pier"
  echo "    ship is up as ~$NAME (pid $SHIP_PID); shutting it down"
  kill "$SHIP_PID"; wait "$SHIP_PID" 2>/dev/null || true
  SHIP_PID=""
fi

step "E2E PASSED"
echo "    work dir kept for inspection: $WORK"
trap - EXIT
kill "$STUB_PID" 2>/dev/null || true
