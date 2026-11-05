#!/usr/bin/env bash
#
# boot.sh is gone; its job moved into the `gw` command that ships in the
# same tarball (one binary: install, mint, start, stop, status, code, logs,
# name, peers, update, invite).  This file stays for one more release so
# the old curl line and the website's installer keep working: it translates
# the old flags into gw subcommands and execs gw.
#
#   boot.sh --mint [--invite CODE] [--vps] [--version TAG]   -> gw mint ...
#   boot.sh --comet ~x [--feed-file F]                        -> gw start ~x [--feed-file F]
#   boot.sh --status / --code / --stop --comet ~x             -> gw status / gw code / gw stop ~x
#
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GW_DIR="${GROUNDWIRE_DIR:-$HOME/.groundwire}"
GW=""
for c in "$HERE/gw" "$GW_DIR/bin/gw" "$GW_DIR/current/gw"; do
  if [ -x "$c" ]; then GW="$c"; break; fi
done
if [ -z "$GW" ] && command -v gw >/dev/null 2>&1; then GW="$(command -v gw)"; fi
if [ -z "$GW" ]; then
  echo "boot.sh: this release has no gw next to it; install it first:" >&2
  echo "  curl -fsSL https://groundwire.io/install.sh | bash" >&2
  exit 1
fi

mode=""; comet=""; args=()
while [ $# -gt 0 ]; do
  case "$1" in
    --mint|--remint)       mode="mint"; shift ;;
    --skip-attestation)    mode="mint"; args+=(--skip-attestation); shift ;;
    --invite)              mode="mint"; args+=(--invite "$2"); shift 2 ;;
    --sponsor|--fief|--port|--ames-port|--gateway|--loom|--proof)
                           args+=("$1" "$2"); shift 2 ;;
    --version)             args+=(--version "$2"); shift 2 ;;
    --feed-file)           args+=(--feed-file "$2"); shift 2 ;;
    --feed)                f="$(mktemp)"; chmod 600 "$f"; printf '%s\n' "$2" > "$f"; args+=(--feed-file "$f"); shift 2 ;;
    --comet)               comet="$2"; shift 2 ;;
    --vps)                 args+=(--vps); shift ;;
    --headless)            args+=(--headless); shift ;;
    --resume)              args+=(--resume); shift ;;
    --no-armillary)        args+=(--no-armillary); shift ;;
    --no-bitcoin)          args+=(--no-bitcoin); shift ;;
    --no-wait|--detach)    args+=(--no-wait); shift ;;
    --status)              mode="status"; shift ;;
    --code)                mode="code"; shift ;;
    --stop)                mode="stop"; shift ;;
    --dir)                 export GROUNDWIRE_DIR="$2"; shift 2 ;;
    --sha256|--redownload|--no-supervisor) [ "$1" = "--sha256" ] && shift; shift ;;
    -h|--help)             exec "$GW" --help ;;
    *) echo "boot.sh: unknown argument $1 (this script only translates to gw; run gw --help)" >&2; exit 2 ;;
  esac
done

echo "boot.sh is retired: running gw instead" >&2
case "$mode" in
  mint)   exec "$GW" mint "${args[@]}" ;;
  status) exec "$GW" status ${comet:+"$comet"} ;;
  code)   exec "$GW" code ${comet:+"$comet"} ;;
  stop)   exec "$GW" stop ${comet:+"$comet"} ;;
  *)      if [ -n "$comet" ]; then exec "$GW" start "$comet" "${args[@]}"; fi
          exec "$GW" start "${args[@]}" ;;
esac
