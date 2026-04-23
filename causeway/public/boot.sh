#!/bin/bash
#
# Causeway boot script — download the Groundwire runtime and launch a comet
# that was spawned by Causeway. Minimal by design: no GitHub release juggling,
# no onboarding flow, no interactive prompts. You already did the spawn in
# Causeway; this just gets you booted.
#
# Usage:
#   curl -fsSL https://groundwire.io/causeway/boot.sh | bash -s -- \
#     --comet ~sampel-palnet \
#     --feed 0vnjSej.O-CNr...
#
# Optional flags:
#   --dir <path>       where to install the runtime (default ~/.groundwire)
#   --port <port>      HTTP port for vere (default 8080)
#   --version <tag>    pin a release tag instead of "latest"
#   --proof <path>     path to a comet.proof.json file from Desktop Causeway
#                      (confidential-comet attestation proof). Saved to
#                      $INSTALL_DIR/<patp>.proof.json so future runtime
#                      versions can ingest it. The current runtime does NOT
#                      consume this file.

set -euo pipefail

COMET=""
FEED=""
PROOF=""
INSTALL_DIR="${GROUNDWIRE_DIR:-$HOME/.groundwire}"
PORT="${GROUNDWIRE_PORT:-8080}"
TAG="${GROUNDWIRE_VERSION:-latest}"
REPO="gwbtc/urbit"

usage() {
  cat <<EOF
Causeway boot script.

Usage:
  curl -fsSL https://groundwire.io/causeway/boot.sh | bash -s -- \\
    --comet ~sampel-palnet \\
    --feed 0v...

Required:
  --comet <@p>       your Causeway-spawned comet name (include the ~)
  --feed <0v...>     the feed atom Causeway gave you

Optional:
  --dir <path>       install path (default ~/.groundwire)
  --port <n>         HTTP port for the ship (default 8080)
  --version <tag>    pin a release (default: latest)
  --proof <file>     confidential-comet proof.json (saved, not yet consumed)
EOF
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --comet)   COMET="$2"; shift 2 ;;
    --feed)    FEED="$2"; shift 2 ;;
    --proof)   PROOF="$2"; shift 2 ;;
    --dir)     INSTALL_DIR="$2"; shift 2 ;;
    --port)    PORT="$2"; shift 2 ;;
    --version) TAG="$2"; shift 2 ;;
    -h|--help) usage; exit 0 ;;
    *) echo "unknown arg: $1" >&2; usage; exit 1 ;;
  esac
done

[[ -z "$COMET" ]] && { echo "error: --comet is required" >&2; usage; exit 1; }
[[ -z "$FEED"  ]] && { echo "error: --feed is required" >&2; usage; exit 1; }

# Detect OS/arch
os="$(uname -s | tr '[:upper:]' '[:lower:]')"
arch="$(uname -m)"
case "$os" in
  darwin) asset_os="macos" ;;
  linux)  asset_os="linux" ;;
  *) echo "error: unsupported OS: $os" >&2; exit 1 ;;
esac
case "$arch" in
  arm64|aarch64) asset_arch="aarch64" ;;
  x86_64|amd64)  asset_arch="x86_64"  ;;
  *) echo "error: unsupported arch: $arch" >&2; exit 1 ;;
esac

# Resolve release URL
if [[ "$TAG" == "latest" ]]; then
  # Use the redirect from /releases/latest to discover the tag without a JSON parse.
  TAG="$(curl -fsSLI -o /dev/null -w '%{url_effective}' \
        "https://github.com/${REPO}/releases/latest" | sed 's|.*/tag/||')"
fi
ASSET_URL="https://github.com/${REPO}/releases/download/${TAG}/groundwire-${asset_os}-${asset_arch}.tar.gz"

# Download + extract if we don't already have it
mkdir -p "$INSTALL_DIR"
if [[ ! -x "$INSTALL_DIR/gw-vere" || ! -f "$INSTALL_DIR/gw-base.pill" ]]; then
  echo "Fetching Groundwire $TAG ($asset_os/$asset_arch)..."
  tmp="$(mktemp -d)"
  trap 'rm -rf "$tmp"' EXIT
  curl -fsSL "$ASSET_URL" -o "$tmp/gw.tar.gz"
  tar -xzf "$tmp/gw.tar.gz" -C "$INSTALL_DIR"
  # Flatten: if the tarball has a top-level folder, move its contents up.
  top="$(find "$INSTALL_DIR" -mindepth 1 -maxdepth 1 -type d | head -1)"
  if [[ -n "$top" && ! -x "$INSTALL_DIR/gw-vere" ]]; then
    mv "$top"/* "$INSTALL_DIR/" 2>/dev/null || true
    rmdir "$top" 2>/dev/null || true
  fi
  chmod +x "$INSTALL_DIR/gw-vere" 2>/dev/null || true
fi

VERE="$INSTALL_DIR/gw-vere"
PILL="$INSTALL_DIR/gw-base.pill"
[[ -x "$VERE" ]] || { echo "error: gw-vere not found in $INSTALL_DIR" >&2; exit 1; }
[[ -f "$PILL" ]] || { echo "error: gw-base.pill not found in $INSTALL_DIR" >&2; exit 1; }

pier="${COMET#\~}"   # strip the leading ~

# Optional: stash the confidential-comet attestation proof alongside the pier.
# The current runtime ignores this file; saving it now means future runtime
# versions can pick it up and inject into Ames self-attestation without another
# round-trip through Causeway.
if [[ -n "$PROOF" ]]; then
  if [[ ! -f "$PROOF" ]]; then
    echo "error: --proof $PROOF does not exist" >&2
    exit 1
  fi
  cp "$PROOF" "$INSTALL_DIR/${pier}.proof.json"
  echo "Saved proof → $INSTALL_DIR/${pier}.proof.json"
  echo "NOTE: runtime does not yet consume proof.json. See"
  echo "      https://groundwire.io/causeway/docs/CONFIDENTIAL-COMETS"
fi

echo ""
echo "Booting $COMET on port $PORT..."
cd "$INSTALL_DIR"
exec "$VERE" -w "$pier" -B "$PILL" -G "$FEED" --http-port "$PORT"
