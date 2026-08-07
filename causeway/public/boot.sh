#!/usr/bin/env bash
#
# Groundwire installer.
#
#   curl -fsSL https://groundwire.io/causeway/boot.sh | bash -s -- \
#     --comet ~sampel-palnet-sampel-palnet--sampel-palnet-sampel-palnet \
#     --feed 0w2M5n3.su88A...
#
# Causeway (web or desktop) mints the comet and hands you a @p and a feed.
# This script takes it from there and does the whole bring-up:
#
#   1. detect the platform and refuse the ones CI does not build
#   2. fetch and unpack the release tarball from gwbtc/urbit
#   3. boot the ship from the pill, with -w and -G given TOGETHER, and
#      prove afterwards that the @p that came up is the @p you asked for
#   4. start the tcp-sidecar -- the light client's only Bitcoin transport
#   5. put both under a supervisor that also performs the gwbtc/node#1
#      wedge recovery, which a bare `Restart=always` does not
#   6. seed NODE_COMPACT_FILTERS-capable peers, in batches
#   7. report sync progress until the light client reaches the chain tip
#
# It is the scripted form of groundwire/doc/OPERATIONS.md sections 5.3, 5.5,
# 5.6, 5.7 and 5.9. Every non-obvious thing it does is there because that
# document records it costing somebody hours; the comments name the section.
#
# This is a curl-pipe-to-bash installer. It runs as you, installs under your
# home directory, asks for no privilege it does not need, never writes outside
# its install directory, and never overwrites an existing pier. Where it is
# not sure what you meant, it stops and says so rather than guessing.
#
# WHAT IS NOT VERIFIED: gwbtc/urbit publishes no SHA256SUMS and no signatures
# with its releases. The only authentication on the download is GitHub's TLS.
# This script prints the SHA-256 of what it fetched and can enforce one you
# supply out of band (--sha256), but on its own it cannot tell you that the
# tarball is the one CI built. That gap is real; see the report in --help.
#
# WHAT HAS ACTUALLY BEEN RUN, as of 2026-08-07, on macos-aarch64 against
# release groundwire-daily-2026.8.7 and a locally mined throwaway comet:
#
#   exercised   argument parsing and every refusal path; platform detection;
#               release-tag resolution; download; sha256; unpack; the boot
#               invocation (a real comet booted and answered); identity
#               read-back over the control socket; the desk-list query; the
#               degradation path when the sidecar and desks are absent;
#               process matching, event-log liveness, @ux formatting;
#               --status end to end.
#
#   NOT run     the sidecar fetch, start and supervision -- the artifact does
#               not exist yet, so no part of that path has been executed;
#               peer seeding against a live %bitcoin-client; the sync
#               progress loop with real heights; the supervisor's recovery
#               triggers; --stop; linux-x86_64 and linux-aarch64, which
#               differ in ss/lsof, /proc and stat(1) and are handled but
#               untried. Treat all of that as written-not-proven.
#
# Two bugs in this script were found by running it and are fixed here: a
# unix-socket path over the 104-byte macOS limit (the ship looked hung when
# it was idle and up), and a @p regex that stopped at a comet's double
# hyphen, which would have made the identity check reject every comet.

set -euo pipefail

BOOT_SH_VERSION="2026.8.7"
REPO="${GROUNDWIRE_REPO:-gwbtc/urbit}"

# How to tell the user to invoke us again. Under `curl | bash` there is no
# file to point at, so we quote the pipeline instead of printing "bash".
if [ -f "${0:-}" ] && [ -r "${0:-}" ]; then
  SELF="$0"
else
  SELF="curl -fsSL https://groundwire.io/causeway/boot.sh | bash -s --"
fi

# ---------------------------------------------------------------- defaults --
GW_DIR="${GROUNDWIRE_DIR:-$HOME/.groundwire}"
HTTP_PORT="${GROUNDWIRE_PORT:-8080}"
AMES_PORT=""
TAG="${GROUNDWIRE_VERSION:-latest}"
LOOM="${GROUNDWIRE_LOOM:-32}"
EXPECT_SHA="${GROUNDWIRE_SHA256:-}"
COMET=""
FEED=""
PROOF=""
MODE="install"
DO_BITCOIN=1
DO_SUPERVISOR=1
DO_WAIT=1
FORCE_REDOWNLOAD=0
SEED_BATCHES="${GROUNDWIRE_SEED_BATCHES:-6}"
SEED_BATCH_SIZE=25          # OPERATIONS.md 5.6: bulk adds SIGSEGV the sidecar
MIN_FREE_GB=20              # OPERATIONS.md 2: a synced pier is ~2.2 GB

usage() {
  cat <<EOF
Groundwire installer ${BOOT_SH_VERSION} -- boot a Causeway-minted comet and
sync its Bitcoin light client.

USAGE
  curl -fsSL https://groundwire.io/causeway/boot.sh | bash -s -- \\
    --comet ~sampel-palnet-... --feed 0w...

  boot.sh --status                 report on an existing install and exit
  boot.sh --stop                   stop a running ship, in the safe order

REQUIRED (for an install)
  --comet <@p>       the comet Causeway minted for you, with the leading ~
  --feed  <0w...>    the boot feed Causeway gave you (a @uw atom)

  Both, together, always. \`vere -G <feed>\` with no \`-w <name>\` does not fail:
  it falls through to [%come ~] and self-mines a DIFFERENT comet, quietly
  (king.c:810). This script passes them together and then checks that the
  ship that came up is the one you named.

  If Causeway printed more than one feed, use the one \`causeway finalize\`
  produced -- the xtr-baked one. Booting the miner's raw feed gives you the
  right @p at the right life with an EMPTY custody log, and no peer can ever
  verify you. See OPERATIONS.md 6.

OPTIONS
  --dir <path>       install root (default ~/.groundwire, \$GROUNDWIRE_DIR)
  --port <n>         HTTP port for the ship (default 8080)
  --ames-port <n>    UDP port for ames. Pin this if your comet commits a
                     fief: the fief names an exact IP:port and the ship has
                     to actually be reachable there.
  --version <tag>    pin a release tag (default: the latest release)
  --sha256 <hex>     require the tarball to have this SHA-256. Use it if you
                     got the digest from somewhere you trust; there is no
                     SHA256SUMS in the releases to check against.
  --loom <n>         vere loom exponent (default 32 = 4 GB address space)
  --proof <file>     a comet.proof.json from Desktop Causeway. Saved next to
                     the pier. The runtime does not consume it yet.
  --no-bitcoin       boot the ship only: no sidecar, no peers, no sync
  --no-supervisor    do not start the supervisor (you get no crash recovery)
  --no-wait          set everything running and exit without watching sync
  --redownload       re-fetch the release even if it is already installed
  --help             this text

WHAT TO EXPECT
  The download is 30-48 MB. Boot takes seconds to a couple of minutes.
  Then the light client syncs from genesis, and that is the long pole:
  block headers first (~55-65 min, and filter headers correctly sit at 1
  for the whole of it), then filter headers. Published measurements of the
  total range from ~1h20m to ~2.5h depending on the peer set. This script
  prints progress and a live rate the whole way; it has not hung.

  A ship is only synced when \`[%is-synced %.y]\` appears AND both %headers
  and %filter-headers are at the tip. Height alone is not the signal.

EXIT CODES
  0  success, or "your ship is up but the release cannot do Bitcoin yet"
  1  error
  2  bad usage

WHAT THIS DOES NOT DO
  It does not mint anything and never touches your wallet, your keys or the
  chain. Minting is Causeway's job and it happens before this script runs.
  It does not install a system service; the supervisor is a plain background
  process that dies with your machine and is restarted by re-running this.
EOF
}

# --------------------------------------------------------------- reporting --
if [ -t 1 ] && [ -z "${NO_COLOR:-}" ]; then
  C_B=$'\033[1m'; C_R=$'\033[31m'; C_Y=$'\033[33m'; C_G=$'\033[32m'; C_0=$'\033[0m'
else
  C_B=""; C_R=""; C_Y=""; C_G=""; C_0=""
fi
step()  { printf '\n%s==>%s %s\n' "$C_B" "$C_0" "$*"; }
info()  { printf '    %s\n' "$*"; }
good()  { printf '    %s%s%s\n' "$C_G" "$*" "$C_0"; }
warn()  { printf '%swarning:%s %s\n' "$C_Y" "$C_0" "$*" >&2; }
die()   { printf '\n%serror:%s %s\n' "$C_R" "$C_0" "$*" >&2; exit 1; }
usagedie() { printf '%serror:%s %s\n\n' "$C_R" "$C_0" "$*" >&2; usage >&2; exit 2; }
have()  { command -v "$1" >/dev/null 2>&1; }

# ------------------------------------------------------------ argument scan --
while [ $# -gt 0 ]; do
  case "$1" in
    --comet)      [ $# -ge 2 ] || usagedie "--comet needs a value"; COMET="$2"; shift 2 ;;
    --feed)       [ $# -ge 2 ] || usagedie "--feed needs a value"; FEED="$2"; shift 2 ;;
    --proof)      [ $# -ge 2 ] || usagedie "--proof needs a value"; PROOF="$2"; shift 2 ;;
    --dir)        [ $# -ge 2 ] || usagedie "--dir needs a value"; GW_DIR="$2"; shift 2 ;;
    --port)       [ $# -ge 2 ] || usagedie "--port needs a value"; HTTP_PORT="$2"; shift 2 ;;
    --ames-port)  [ $# -ge 2 ] || usagedie "--ames-port needs a value"; AMES_PORT="$2"; shift 2 ;;
    --version)    [ $# -ge 2 ] || usagedie "--version needs a value"; TAG="$2"; shift 2 ;;
    --sha256)     [ $# -ge 2 ] || usagedie "--sha256 needs a value"; EXPECT_SHA="$2"; shift 2 ;;
    --loom)       [ $# -ge 2 ] || usagedie "--loom needs a value"; LOOM="$2"; shift 2 ;;
    --no-bitcoin) DO_BITCOIN=0; shift ;;
    --no-supervisor) DO_SUPERVISOR=0; shift ;;
    --no-wait)    DO_WAIT=0; shift ;;
    --redownload) FORCE_REDOWNLOAD=1; shift ;;
    --status)     MODE="status"; shift ;;
    --stop)       MODE="stop"; shift ;;
    -h|--help)    usage; exit 0 ;;
    *) usagedie "unknown argument: $1" ;;
  esac
done

# ================================================================ platform ==
detect_platform() {
  local os arch
  os="$(uname -s | tr '[:upper:]' '[:lower:]')"
  arch="$(uname -m)"
  case "$os/$arch" in
    linux/x86_64|linux/amd64)     PLATFORM="linux-x86_64" ;;
    linux/aarch64|linux/arm64)    PLATFORM="linux-aarch64" ;;
    darwin/arm64|darwin/aarch64)  PLATFORM="macos-aarch64" ;;
    darwin/x86_64)
      die "Intel macOS is not supported: CI builds no macos-x86_64 artifact,
    and the macos-aarch64 binaries will not run under Rosetta (Rosetta
    translates x86_64 for arm64 Macs, not the other way round).
    Use a Linux host, or an Apple Silicon Mac." ;;
    *)
      die "unsupported platform $os/$arch.
    CI builds linux-x86_64, linux-aarch64 and macos-aarch64." ;;
  esac
}

# The @p of a comet is 8 syllable pairs: four, then '--', then four. Accept a
# 4-pair name too (a moon-shaped name), reject everything else rather than
# hand vere something it will turn into a different ship. The authoritative
# check is post-boot: we ask the running ship who it is.
validate_comet() {
  local n="${COMET#\~}"
  case "$n" in
    "") usagedie "--comet is required (the ~name Causeway gave you)" ;;
    *[!a-z-]*) usagedie "--comet contains characters that are not in a @p: $COMET" ;;
  esac
  local pairs
  pairs="$(printf '%s' "$n" | tr -cd '-' | wc -c | tr -d ' ')"
  case "$pairs" in
    3|4|7|8) : ;;
    *) usagedie "--comet does not look like a comet or moon @p: $COMET
    A comet is eight syllable pairs, e.g.
    ~sampel-palnet-sampel-palnet--sampel-palnet-sampel-palnet" ;;
  esac
  NAME="$n"
}

# vere parses -G with (slaw %uw ...) -- king.c:707. A 0v atom, a hex string or
# a mnemonic is rejected at boot with "dawn: invalid private keys", after the
# pier has already been created. Catch it here instead.
validate_feed() {
  case "$FEED" in
    "") usagedie "--feed is required (the 0w... atom Causeway gave you)" ;;
    0w*) : ;;
    0v*) usagedie "--feed must be a @uw (0w...), not a @uv (0v...).
    vere parses the feed with (slaw %uw ...); a 0v atom is refused at boot.
    Causeway and comet_miner both print the 0w form." ;;
    *) usagedie "--feed must be a @uw atom starting with 0w: $FEED" ;;
  esac
  case "$FEED" in
    *[!0-9A-Za-z.~-]*) usagedie "--feed has characters that are not @uw digits" ;;
  esac
}

validate_port() {
  case "$2" in
    ""|*[!0-9]*) usagedie "$1 must be a number: $2" ;;
  esac
  if [ "$2" -lt 1 ] || [ "$2" -gt 65535 ]; then usagedie "$1 out of range: $2"; fi
}

# ================================================================== system ==
free_gb() {
  local p="$1"
  while [ ! -d "$p" ] && [ "$p" != "/" ]; do p="$(dirname "$p")"; done
  df -Pk "$p" 2>/dev/null | awk 'NR==2 {printf "%d", $4/1048576}'
}

sha256_of() {
  if have sha256sum; then sha256sum "$1" | awk '{print $1}'
  elif have shasum; then shasum -a 256 "$1" | awk '{print $1}'
  elif have openssl; then openssl dgst -sha256 "$1" | awk '{print $NF}'
  else printf ''; fi
}

# 0 busy, 1 free, 2 could not tell
port_busy() {
  local proto="$1" port="$2"
  if have lsof; then
    if lsof -nP -i"${proto}:${port}" >/dev/null 2>&1; then return 0; fi
    return 1
  fi
  if have ss; then
    local flag="-lntH"
    [ "$proto" = "UDP" ] && flag="-lnuH"
    if ss "$flag" 2>/dev/null | awk '{print $4}' | grep -qE "[:.]${port}\$"; then return 0; fi
    return 1
  fi
  if have netstat; then
    if netstat -an 2>/dev/null | grep -qE "[.:]${port}[[:space:]].*LISTEN"; then return 0; fi
    return 1
  fi
  return 2
}

preflight() {
  step "Checking this machine"
  have curl || die "curl is required"
  have tar  || die "tar is required"
  have uname || die "uname is required"
  info "platform      $PLATFORM"

  local gb
  gb="$(free_gb "$GW_DIR")"
  if [ -n "$gb" ] && [ "$gb" -lt "$MIN_FREE_GB" ]; then
    die "only ${gb} GB free where $GW_DIR will live; ${MIN_FREE_GB} GB is the
    documented minimum (OPERATIONS.md 2). A synced pier is ~2.2 GB, and a
    full disk blocks every tool on the box including df.
    Free some space, or point --dir at a bigger filesystem."
  fi
  info "free disk     ${gb:-?} GB"

  # The control socket. Everything after boot -- installing desks, adding
  # peers, reading readiness -- goes through the pier's Lick socket, because
  # a ship booted with -t has no dojo (OPERATIONS.md 5.3).
  pick_sock_tool
  info "control sock  $SOCK_TOOL"

  if [ "$DO_BITCOIN" = 1 ]; then
    pick_dns_tool
    info "dns for seeds ${DNS_TOOL:-none}"
    if [ -z "$DNS_TOOL" ]; then
      warn "no dig/host/getent/python3: peer seeding will not be able to
    resolve the DNS seeds, and the light client cannot start without peers.
    Install dnsutils (Debian/Ubuntu: apt-get install -y dnsutils)."
    fi
  fi
}

pick_sock_tool() {
  SOCK_TOOL=""
  # python3 first: it is the only one of these that can wait a long time for
  # the FIRST byte and still return the moment the reply is complete. A khan
  # call during filter-header sync has been measured taking 120-380 s
  # (OPERATIONS.md 5.7), and nc's only knob is an idle timeout that it also
  # burns after the reply arrives.
  if have python3 && python3 -c 'print(1)' >/dev/null 2>&1; then
    SOCK_TOOL="python3"
  elif have nc && nc -h 2>&1 | grep -q 'W recvlimit'; then
    SOCK_TOOL="nc-W"
  elif have nc; then
    SOCK_TOOL="nc"
  elif have socat; then
    SOCK_TOOL="socat"
  else
    die "need python3, nc or socat to talk to the ship's control socket"
  fi
}

pick_dns_tool() {
  DNS_TOOL=""
  if have dig; then DNS_TOOL="dig"
  elif have host; then DNS_TOOL="host"
  elif have getent && [ "$(uname -s)" = "Linux" ]; then DNS_TOOL="getent"
  elif have python3 && python3 -c 'print(1)' >/dev/null 2>&1; then DNS_TOOL="python3"
  fi
}

# ================================================================= release ==
resolve_tag() {
  if [ "$TAG" = "latest" ]; then
    local url
    url="$(curl -fsSLI -o /dev/null -w '%{url_effective}' \
           "https://github.com/${REPO}/releases/latest" 2>/dev/null)" \
      || die "could not reach github.com to find the latest release"
    case "$url" in
      */tag/*) TAG="${url##*/tag/}" ;;
      *) die "could not work out the latest release tag from $url" ;;
    esac
  fi
}

fetch_release() {
  local asset="groundwire-${PLATFORM}.tar.gz"
  local url="https://github.com/${REPO}/releases/download/${TAG}/${asset}"
  local dl="$GW_DIR/var/dl"
  local tarball="$dl/${TAG}-${asset}"
  mkdir -p "$dl"

  step "Fetching Groundwire $TAG ($PLATFORM)"
  if [ -f "$tarball" ] && [ "$FORCE_REDOWNLOAD" = 0 ]; then
    info "already downloaded: $tarball"
  else
    [ "$FORCE_REDOWNLOAD" = 1 ] && rm -f "$tarball"
    info "$url"
    local flags="-fL --retry 3 --retry-delay 2"
    if [ -t 1 ]; then flags="$flags --progress-bar"; else flags="$flags -sS"; fi
    # shellcheck disable=SC2086
    if ! curl $flags -C - -o "${tarball}.part" "$url"; then
      # A stale or complete .part makes the server refuse the range; start over
      # rather than leaving the user with a half file and an opaque error.
      rm -f "${tarball}.part"
      # shellcheck disable=SC2086
      curl $flags -o "${tarball}.part" "$url" || {
        rm -f "${tarball}.part"
        die "download failed.
    If this release has no ${asset}, pass --version with one that does:
      https://github.com/${REPO}/releases"
      }
    fi
    mv "${tarball}.part" "$tarball"
  fi

  TARBALL="$tarball"
  verify_release
}

verify_release() {
  local got sums url
  got="$(sha256_of "$TARBALL")"
  if [ -z "$got" ]; then
    warn "no sha256sum/shasum/openssl on this machine: cannot even hash the download"
  else
    info "sha256        $got"
  fi

  if [ -n "$EXPECT_SHA" ]; then
    [ -n "$got" ] || die "--sha256 was given but nothing here can compute a SHA-256"
    if [ "$got" != "$EXPECT_SHA" ]; then
      rm -f "$TARBALL"
      die "sha256 mismatch: expected $EXPECT_SHA, got $got. Download deleted."
    fi
    good "sha256 matches the digest you supplied"
  else
    # Future-proofing: if the release ever grows a SHA256SUMS, use it.
    url="https://github.com/${REPO}/releases/download/${TAG}/SHA256SUMS"
    if sums="$(curl -fsSL "$url" 2>/dev/null)" && [ -n "$sums" ]; then
      local want
      want="$(printf '%s\n' "$sums" | awk -v a="groundwire-${PLATFORM}.tar.gz" \
              '$2 ~ a {print $1}' | head -1)"
      if [ -n "$want" ] && [ "$want" = "$got" ]; then
        good "sha256 matches the release's SHA256SUMS"
      elif [ -n "$want" ]; then
        rm -f "$TARBALL"
        die "sha256 does NOT match the release's SHA256SUMS. Download deleted."
      fi
    else
      warn "this release publishes no SHA256SUMS and no signature, so the only
    thing authenticating this download is GitHub's TLS certificate. The
    tarball has not been verified against anything CI published. If you have
    a digest from a source you trust, re-run with --sha256 <hex>."
    fi
  fi

  tar -tzf "$TARBALL" >/dev/null 2>&1 || die "the downloaded tarball is not readable as gzip'd tar"
}

install_release() {
  VERE="$GW_DIR/bin/gw-vere"
  PILL="$GW_DIR/pills/gw-base.pill"
  SIDECAR="$GW_DIR/bin/tcp-sidecar"

  # Idempotence: an interrupted run re-enters here with the work already done.
  # Re-extracting would also swap the binary under a ship that is still
  # running, which is a thing to do deliberately, not by accident.
  if [ "$FORCE_REDOWNLOAD" = 0 ] && [ -x "$VERE" ] && [ -f "$PILL" ] &&
     grep -qx "tag=$TAG" "$GW_DIR/var/release.txt" 2>/dev/null; then
    info "release $TAG is already installed"
    HAVE_SIDECAR=0
    [ -x "$SIDECAR" ] && HAVE_SIDECAR=1
    return 0
  fi

  local stage="$GW_DIR/var/stage"
  rm -rf "$stage"; mkdir -p "$stage"
  tar -xzf "$TARBALL" -C "$stage"
  # Older tarballs had a single top-level directory; the current ones are flat.
  if [ ! -f "$stage/gw-vere" ]; then
    local top
    top="$(find "$stage" -mindepth 1 -maxdepth 1 -type d | head -1)"
    if [ -n "$top" ] && [ -f "$top/gw-vere" ]; then
      mv "$top"/* "$stage/" 2>/dev/null || true
      rmdir "$top" 2>/dev/null || true
    fi
  fi

  [ -f "$stage/gw-vere" ]      || die "the release tarball has no gw-vere in it"
  [ -f "$stage/gw-base.pill" ] || die "the release tarball has no gw-base.pill in it"

  mkdir -p "$GW_DIR/bin" "$GW_DIR/pills" "$GW_DIR/piers" "$GW_DIR/var" "$GW_DIR/lib"
  local f
  for f in gw-vere tcp-sidecar comet_miner gw-onboard; do
    if [ -f "$stage/$f" ]; then
      mv -f "$stage/$f" "$GW_DIR/bin/$f"
      chmod +x "$GW_DIR/bin/$f"
    fi
  done
  mv -f "$stage/gw-base.pill" "$GW_DIR/pills/gw-base.pill"
  [ -f "$stage/requirements.txt" ] && mv -f "$stage/requirements.txt" "$GW_DIR/var/requirements.txt"
  rm -rf "$stage"

  [ -x "$VERE" ] || die "gw-vere did not install as executable"

  printf 'tag=%s\nplatform=%s\nsha256=%s\ninstalled=%s\n' \
    "$TAG" "$PLATFORM" "$(sha256_of "$TARBALL")" "$(date -u +%FT%TZ)" \
    > "$GW_DIR/var/release.txt"

  good "installed gw-vere and gw-base.pill under $GW_DIR"
  if [ -x "$SIDECAR" ]; then
    good "tcp-sidecar is in this release"
    HAVE_SIDECAR=1
  else
    HAVE_SIDECAR=0
  fi
}

# ============================================================ ship helpers ==
# gwlib.sh is written to disk rather than kept in this script because the
# supervisor needs exactly the same helpers, and this script may have been
# piped in from curl -- there is no file for the supervisor to re-exec.
# One implementation, two users, and both are auditable after the fact.
write_helpers() {
  mkdir -p "$GW_DIR/lib" "$GW_DIR/bin"
  cat > "$GW_DIR/lib/gwlib.sh" <<'GWLIB'
#!/usr/bin/env bash
# gwlib.sh -- shared helpers for the Groundwire installer and its supervisor.
# Written by causeway/public/boot.sh; edit that, not this.
#
# Expects, from the caller: GW_DIR GW_NAME GW_PIER GW_VERE GW_LOG GW_SC_LOG
#                           GW_SIDECAR GW_AMES_PORT GW_LOOM SOCK_TOOL DNS_TOOL

gwl_have() { command -v "$1" >/dev/null 2>&1; }

gwl_pick_sock_tool() {
  if [ -n "${SOCK_TOOL:-}" ]; then return 0; fi
  if gwl_have python3 && python3 -c 'print(1)' >/dev/null 2>&1; then SOCK_TOOL=python3
  elif gwl_have nc && nc -h 2>&1 | grep -q 'W recvlimit'; then SOCK_TOOL=nc-W
  elif gwl_have nc; then SOCK_TOOL=nc
  elif gwl_have socat; then SOCK_TOOL=socat
  else SOCK_TOOL=""; fi
}

# stdin: request bytes.  stdout: reply bytes.  $1: seconds to wait for the
# FIRST byte of the reply.  A ship chewing through filter-header batches has
# been measured taking 120-380 s to answer (OPERATIONS.md 5.7), so this has to
# be generous; once bytes start arriving the reply completes immediately.
# NB the connection is made from INSIDE the pier, by relative path. A unix
# socket address is capped at 104 bytes on macOS (108 on Linux), and
# <pier>/.urb/conn.sock with a 56-character comet name in it goes straight
# through that ceiling for any pier more than a couple of directories deep:
# python reports "AF_UNIX path too long", nc reports nothing at all, and the
# ship looks hung when it is in fact up and idle. Measured on a real boot.
# vere itself is unaffected -- it binds the socket from within the pier.
gwl_sock() {
  local first="${1:-60}" sock=".urb/conn.sock"
  cd "$GW_PIER" 2>/dev/null || return 1
  case "${SOCK_TOOL:-}" in
    python3) python3 -c '
import socket, sys
sock, first = sys.argv[1], float(sys.argv[2])
data = sys.stdin.buffer.read()
s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
s.settimeout(first)
try:
    s.connect(sock)
    s.sendall(data)
    out = []
    while True:
        try:
            b = s.recv(65536)
        except socket.timeout:
            break
        if not b:
            break
        out.append(b)
        s.settimeout(0.4)          # reply started; drain and go
    sys.stdout.buffer.write(b"".join(out))
except OSError as e:
    print("gwlib: control socket: %s" % e, file=sys.stderr)
finally:
    s.close()
' "$sock" "$first" ;;
    nc-W)  nc -U -W 3 -w "$first" "$sock" ;;
    nc)    nc -U -w 5 "$sock" ;;
    socat) socat -T5 - "UNIX-CONNECT:$sock" ;;
    *)     return 1 ;;
  esac
}

# Wrap a strand body (on stdin) in the khan FYRD envelope.  The body and the
# ''' delimiters must share an indentation column, so we indent both here.
gwl_thread() {
  printf '%s\n' \
    ':*  0' \
    '    %fyrd' \
    '    %base' \
    '    %khan-eval' \
    '    %noun' \
    '    %ted-eval' \
    '    :_  :~  /sur/spider/hoon' \
    '            /lib/strandio/hoon' \
    '        ==' \
    "    '''" \
    '    =/  m  (strand ,vase)' \
    '    ^-  form:m' \
    '    ;<  our=@p   bind:m  get-our' \
    '    ;<  now=@da  bind:m  get-time'
  sed 's/^/    /'
  printf '%s\n' "    '''" '=='
}

# Run a strand body (stdin) on the ship; print the decoded reply.
# $1: seconds to wait for the first byte (default 60).
# NB every pipeline in this file ends in `|| true`. gwlib.sh is sourced by a
# script running `set -eo pipefail`, where one grep that matches nothing
# would otherwise take the whole installer down -- and "no [%headers N] in
# the log yet" is the normal state for the first minute of every sync.
gwl_eval() {
  # gwl_sock cd's into the pier; it is inside a pipeline, so that cd happens
  # in a subshell and cannot leak into the caller.
  gwl_thread | "$GW_VERE" eval -jn 2>/dev/null | gwl_sock "${1:-60}" \
    | "$GW_VERE" eval -cn 2>/dev/null || true
}

# $1 agent  $2 mark  $3 vase expression  [$4 timeout]
gwl_poke() {
  printf ';<  ~  bind:m  (poke-our %%%s %%%s %s)\n(pure:m !>(%%ok))\n' "$1" "$2" "$3" \
    | gwl_eval "${4:-60}"
}

gwl_our() {
  # A comet @p has a DOUBLE hyphen in the middle: four syllable pairs, '--',
  # four more. A '-'-only pattern matches the first half and stops, and the
  # identity check then rejects every comet ever minted. Measured on a real
  # boot: ~raclep-habfus-sogwer-tilrep for ~raclep-...-mipdeb.
  printf '(pure:m !>((scot %%p our)))\n' | gwl_eval "${1:-120}" \
    | grep -oE '~[a-z]{6}(-{1,2}[a-z]{6})+' | head -1 || true
}

# /x/ready, but ONLY if %gw-btc is actually running.  A `%gx` scry into an
# agent gall is not running -- or into a path that agent's +on-peek does not
# handle -- is not a soft miss.  It bails, and the bail takes %spider with it:
#
#   peek bad result
#   "unexpected scry into %urb-watcher on path /x/ready"
#   spider crashed, killing all strands: %arvo-response
#
# "all strands" includes kiln's OTA sync strands, one per desk carrying a
# desk.ship, and each one logs its own death:
#
#   kiln: activation failed into %groundwire from ~watwyd-.../%groundwire; retrying sync
#
# That line is about the OTA sync, not about the desk, and the desk stays
# live either way -- but it reads like an activation failure, and a --status
# run against a release whose pill predates %gw-btc printed one per desk and
# sent an afternoon chasing a bug that was not there.  Measured on a fresh
# comet booted from groundwire-daily-2026.8.7.
#
# `mule` does not help: the bail is in gall's peek, not in our nock, so it is
# not ours to catch.  The only safe guard is not to send the scry.  This is
# the same hazard the +gwl_agent_installed comment below describes; that one
# was written about %gu and the rule is general.
gwl_ready() {
  if ! gwl_agent_installed gw-btc; then return 0; fi
  printf '%s\n' \
    '=/  r  .^(* %gx /(scot %p our)/gw-btc/(scot %da now)/ready/noun)' \
    '(pure:m !>(r))' | gwl_eval "${1:-120}"
}

gwl_desks() {
  printf '%s\n' \
    ';<  dez=(set desk)  bind:m  (scry (set desk) %cd %$ /)' \
    "(pure:m !>((crip (tape (join ' ' ~(tap in dez))))))" | gwl_eval "${1:-120}"
}

# Has a gall agent been installed?  Read it out of the ship's log, not with a
# scry.  `.^(? %gu ...)` is the dojo idiom for this and it does NOT survive
# being run inside a khan thread: measured on a live ship it returns
# [%thread-fail %cancelled] even for %dojo, which is definitely running.  A
# scry a vane declines is worse than useless here -- it can bail the strand
# and take every OTHER in-flight strand on the ship down with it, including a
# running verification (see the warning in ops/gwctl.py cmd_pass).
gwl_agent_installed() {
  grep -q "gall: installing %$1\b" "$GW_LOG" 2>/dev/null
}

# -------------------------------------------------------------- processes --
# Matched exactly, never by pgrep -f prefix: p4c1 prefix-matches p4c1b, and
# killing the wrong pier is worse than killing none (OPERATIONS.md 5.9).
gwl_king_pid() {
  ps -eo pid=,args= 2>/dev/null | awk -v p="$GW_PIER" '
    /gw-vere/ && !/--snap-dir/ { for (i=2;i<=NF;i++) if ($i==p) { print $1; break } }'
}
gwl_serf_pid() {
  ps -eo pid=,args= 2>/dev/null | awk -v p="$GW_PIER" '
    /snap-dir/ { for (i=1;i<=NF;i++) if ($i=="--snap-dir" && $(i+1)==p) print $1 }'
}
gwl_proc_cwd() {
  if [ -r "/proc/$1/cwd" ]; then
    readlink "/proc/$1/cwd" 2>/dev/null
  elif gwl_have lsof; then
    lsof -a -p "$1" -d cwd -Fn 2>/dev/null | sed -n 's/^n//p' | head -1
  fi
}
# The sidecar has no port and no distinctive argv: its pier is its cwd.
gwl_sidecar_pids() {
  local pid
  for pid in $(pgrep -f 'tcp-sidecar' 2>/dev/null || true); do
    [ "$(gwl_proc_cwd "$pid")" = "$GW_PIER" ] && echo "$pid"
  done
  return 0
}

gwl_mtime() {
  if stat -c %Y "$1" >/dev/null 2>&1; then stat -c %Y "$1"
  else stat -f %m "$1" 2>/dev/null; fi
}

# Liveness is the newest mtime across <pier>/.urb/log/*/data.mdb.  The
# .urb/log DIRECTORY is a dirent that LMDB never touches -- measured 21 h
# stale on a ship demonstrably processing events (OPERATIONS.md 5.9).  Piers
# roll epochs, so glob the epoch dirs rather than assuming 0i0.
gwl_evt_age() {
  local f newest="" t now
  for f in "$GW_PIER"/.urb/log/*/data.mdb; do
    [ -f "$f" ] || continue
    t="$(gwl_mtime "$f")"
    [ -n "$t" ] || continue
    if [ -z "$newest" ] || [ "$t" -gt "$newest" ]; then newest="$t"; fi
  done
  if [ -z "$newest" ]; then echo 99999; return; fi
  now="$(date +%s)"
  echo $(( now - newest ))
}

gwl_start_sidecar() {
  [ -x "${GW_SIDECAR:-}" ] || return 1
  local cert=""
  for cert in /etc/ssl/certs/ca-certificates.crt /etc/ssl/cert.pem \
              /etc/pki/tls/certs/ca-bundle.crt ""; do
    [ -n "$cert" ] && [ -f "$cert" ] && break
  done
  # main.c:618 sets SSL_VERIFY_PEER with the default verify paths; a host that
  # keeps its roots somewhere unusual needs SSL_CERT_FILE pointing at them.
  ( cd "$GW_PIER" || exit 1
    if [ -n "$cert" ]; then export SSL_CERT_FILE="$cert"; fi
    if gwl_have setsid; then
      setsid nohup "$GW_SIDECAR" . >> "$GW_SC_LOG" 2>&1 </dev/null &
    else
      nohup "$GW_SIDECAR" . >> "$GW_SC_LOG" 2>&1 </dev/null &
    fi ) >/dev/null 2>&1
  sleep 3
  [ -n "$(gwl_sidecar_pids)" ]
}

gwl_start_vere_restart() {
  # The restart form: no -c, -w, -G or -B; the pier is the trailing argument.
  rm -f "$GW_PIER/.vere.lock"
  local args="-t --loom $GW_LOOM"
  [ -n "${GW_AMES_PORT:-}" ] && args="$args -p $GW_AMES_PORT"
  if gwl_have setsid; then
    # shellcheck disable=SC2086
    setsid nohup "$GW_VERE" $args "$GW_PIER" >> "$GW_LOG" 2>&1 </dev/null &
  else
    # shellcheck disable=SC2086
    nohup "$GW_VERE" $args "$GW_PIER" >> "$GW_LOG" 2>&1 </dev/null &
  fi
}

# ------------------------------------------------------------- peer pool ---
# Filter headers are only servable by peers advertising NODE_COMPACT_FILTERS.
# %bitcoin-client makes it a REQUIRED service, so a pool from unfiltered DNS
# seeds leaves filter sync at height 1 forever -- the single biggest time sink
# in the procedure, and what killed the Phase 4 run (OPERATIONS.md 5.6).
# x49 = NODE_NETWORK(1) | NODE_WITNESS(8) | NODE_COMPACT_FILTERS(64).
GWL_SEEDS="seed.bitcoin.sipa.be dnsseed.bluematt.me seed.bitcoinstats.com
seed.bitcoin.jonasschnelli.ch dnsseed.emzy.de seed.bitcoin.wiz.biz
seed.btc.petertodd.net seed.bitcoin.sprovoost.nl seed.mainnet.achownodes.xyz
dnsseed.bitcoin.dashjr-list-of-p2p-nodes.us"

gwl_resolve_a() {
  case "${DNS_TOOL:-}" in
    dig)  dig +short +time=3 +tries=1 A "$1" 2>/dev/null ;;
    host) host -W 3 -t A "$1" 2>/dev/null | awk '/has address/ {print $NF}' ;;
    getent) getent ahostsv4 "$1" 2>/dev/null | awk '{print $1}' ;;
    python3) python3 -c '
import socket, sys
try:
    for r in socket.getaddrinfo(sys.argv[1], 8333, socket.AF_INET):
        print(r[4][0])
except Exception:
    pass
' "$1" ;;
    *) return 1 ;;
  esac
}

# Each seed returns a small random slice per query, so ask repeatedly.
gwl_pool_fill() {
  local rounds="${1:-6}" pool="$GW_DIR/var/peerpool.txt" s i before after
  touch "$pool"
  before="$(wc -l < "$pool" | tr -d ' ')"
  i=0
  while [ "$i" -lt "$rounds" ]; do
    for s in $GWL_SEEDS; do
      gwl_resolve_a "x49.$s"
    done
    i=$(( i + 1 ))
  done | grep -E '^[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+$' \
       | grep -vE '^(0\.|10\.|127\.|169\.254\.|172\.(1[6-9]|2[0-9]|3[01])\.|192\.168\.)' \
       | sort -u >> "$pool" || true
  sort -u "$pool" -o "$pool"
  after="$(wc -l < "$pool" | tr -d ' ')"
  echo "$(( after - before ))"
}

gwl_pool_left() {
  local pool="$GW_DIR/var/peerpool.txt" used="$GW_DIR/var/used-$GW_NAME.txt"
  touch "$pool" "$used"
  grep -vxF -f "$used" "$pool" 2>/dev/null | grep -c . || echo 0
}

# An IP handed to the same ship twice is wasted: %bitcoin-client's blacklist
# expiry is ~d3, so a burned seed stays burned for three days.
gwl_take_peers() {
  local n="$1" pool="$GW_DIR/var/peerpool.txt" used="$GW_DIR/var/used-$GW_NAME.txt" ips
  touch "$pool" "$used"
  ips="$(grep -vxF -f "$used" "$pool" 2>/dev/null | head -n "$n" || true)"
  [ -n "$ips" ] && printf '%s\n' "$ips" >> "$used"
  printf '%s' "$ips"
}

# A Hoon @ux literal is dot-grouped every four hex digits from the right, and
# the leading group carries no padding zeros.
gwl_hoonhex() {
  local h="$1" out=""
  while [ "${#h}" -gt 4 ]; do
    out=".${h: -4}$out"
    h="${h:0:${#h}-4}"
  done
  h="$(printf '%s' "$h" | sed 's/^0*//')"
  [ -z "$h" ] && h=0
  printf '0x%s%s' "$h" "$out"
}

gwl_ip_hoon() {
  local a b c d
  IFS=. read -r a b c d <<EOF
$1
EOF
  gwl_hoonhex "$(printf '%x' $(( (a << 24) | (b << 16) | (c << 8) | d )))"
}

# One strand for the whole batch.  ~25 at a time: adding 200-300 at once
# reliably SIGSEGVs the sidecar, after which live-earth-peers goes to 0 and
# sync stalls (gwbtc/node#1, OPERATIONS.md 5.6).
gwl_add_peers() {
  local ip vals=""
  for ip in $1; do
    vals="$vals $(gwl_ip_hoon "$ip")"
  done
  [ -n "$vals" ] || return 1
  # shellcheck disable=SC2016  # $(ips t.ips) is Hoon recursion, not shell
  { printf '=/  ips=(list @ux)  ~[%s]\n' "$vals"
    printf '|-  ^-  form:m\n'
    printf "?~  ips  (pure:m !>('done'))\n"
    printf ';<  ~  bind:m  (poke-our %%bitcoin-client %%add-earth-peer !>([%%ipv4 i.ips 8.333]))\n'
    printf '$(ips t.ips)\n'
  } | gwl_eval 300
}

# %bitcoin-client's ++peek is literally ~ for every path
# (bitcoin-client.hoon:181-184), so status cannot be scried: &log-info dumps
# it into the ship's log and we read it back out of there.
gwl_log_info() { gwl_poke bitcoin-client log-info '!>(~)' 30 >/dev/null 2>&1 || true; }

# $1 key, e.g. %headers.  Prints the last value seen, dots stripped.
gwl_log_last() {
  tail -n 4000 "$GW_LOG" 2>/dev/null \
    | grep -oE "\[%$1 [0-9.]+\]" | tail -1 \
    | grep -oE '[0-9.]+' | tr -d '.' || true
}
gwl_log_synced() {
  tail -n 4000 "$GW_LOG" 2>/dev/null \
    | grep -oE '\[%is-synced %\.[yn]\]' | tail -1 || true
}
GWLIB
  chmod +x "$GW_DIR/lib/gwlib.sh"
  write_supervisor
}

# The supervisor.  ops/gwsup.sh is the campaign's version of this and it is
# NOT reused verbatim, for three reasons: it hardcodes the droplet layout
# (/opt/gw/piers/<name>, /opt/gw/bin, /opt/gw/*.log) which does not exist in a
# user-space install; it is Linux-only (flock(1), stat -c, /proc/<pid>/cwd,
# ss) and this installer supports macOS; and it shells out to four Python
# tools that expect the gwharness package importable from /opt/gw, which is
# not in any release artifact. The ALGORITHM is reused unchanged, trap for
# trap -- singleton, data.mdb liveness, exact process matching, and the
# kill-peer-connections-plus-reseed that makes a sidecar restart actually
# recover. Keep the two in sync when either changes.
write_supervisor() {
  cat > "$GW_DIR/bin/gwsup.sh" <<'GWSUP'
#!/usr/bin/env bash
# gwsup.sh <name> -- supervisor for one Groundwire ship.
# Written by causeway/public/boot.sh. Derived from ops/gwsup.sh; see the note
# in that script for why this is a separate implementation.
#
# Filed against gwbtc/node#1: the tcp-sidecar SIGSEGVs, %bitcoin-client goes
# on believing its peers are live, every send returns "no such connection"
# forever, and the ship stops following the chain while looking healthy. It
# does not self-heal, and restarting the sidecar alone does NOT fix it -- the
# agent's peer table has to be cleared with &kill-peer-connections and
# re-seeded. A plain Restart=always unit gets the process back and leaves the
# ship wedged.
set -u

GW_NAME="${1:?usage: gwsup.sh <name>}"
GW_DIR="$(cd "$(dirname "$0")/.." && pwd)"
[ -f "$GW_DIR/var/$GW_NAME.env" ] || { echo "no $GW_DIR/var/$GW_NAME.env" >&2; exit 1; }
# shellcheck disable=SC1090
. "$GW_DIR/var/$GW_NAME.env"
# shellcheck disable=SC1091
. "$GW_DIR/lib/gwlib.sh"
gwl_pick_sock_tool

SUPLOG="$GW_DIR/var/sup-$GW_NAME.log"
STALE=300      # seconds with no event-log write => WEDGED
POLL=30
COOLDOWN=300   # minimum seconds between recoveries

# SINGLETON. Two supervisors on one pier both see VERE-DOWN, both relaunch,
# and the loser's ship dies on "mesa: bind: address already in use", which
# reads exactly like a crash loop. In one cleanroom run all three droplets
# were found running two supervisors per pier, which is also why ships that
# had been deliberately stopped came back. flock(1) is Linux-only, so this is
# a mkdir lock -- atomic everywhere -- with a liveness check so a supervisor
# killed with SIGKILL does not lock the pier out forever.
LOCK="$GW_DIR/var/sup-$GW_NAME.lock"
if ! mkdir "$LOCK" 2>/dev/null; then
  oldpid="$(cat "$LOCK/pid" 2>/dev/null || echo)"
  if [ -n "$oldpid" ] && kill -0 "$oldpid" 2>/dev/null &&
     ps -o args= -p "$oldpid" 2>/dev/null | grep -q gwsup.sh; then
    echo "gwsup.sh: a supervisor for $GW_NAME is already running (pid $oldpid); refusing"
    exit 0
  fi
  rm -rf "$LOCK"
  mkdir "$LOCK" 2>/dev/null || { echo "gwsup.sh: lost the lock race; refusing"; exit 0; }
fi
echo $$ > "$LOCK/pid"
trap 'rm -rf "$LOCK"' EXIT INT TERM
sleep 1
[ "$(cat "$LOCK/pid" 2>/dev/null)" = "$$" ] || { echo "gwsup.sh: lost the lock race; refusing"; exit 0; }

log() { echo "$(date -u +%FT%TZ) [$GW_NAME] $*" >> "$SUPLOG"; }

# Counters are re-derived from the log so "how often did this fire" stays a
# true cumulative number across supervisor restarts. NB `grep -c` prints 0 AND
# exits 1 when there is no match, which in the original cost a ship: the
# arithmetic that followed became a syntax error and the watchdog killed
# itself at the moment it was first needed.
_count() { local n; n="$(grep -c "$1" "$SUPLOG" 2>/dev/null | head -1)"; echo "${n:-0}"; }
N_WEDGE="$(_count 'WEDGE (recover')"
N_VERE="$(_count 'VERE-DOWN')"
N_SIDE="$(_count 'SIDECAR-DOWN')"
LAST_RECOVER=0

recover() {
  N_WEDGE=$(( N_WEDGE + 1 ))
  log "INTERVENTION #$(( N_WEDGE + N_VERE + N_SIDE )) WEDGE (recover #$N_WEDGE): $1"
  [ -n "$(gwl_sidecar_pids)" ] || gwl_start_sidecar
  sleep 3
  if gwl_poke bitcoin-client kill-peer-connections '!>(~)' 150 >/dev/null 2>&1; then
    log "  kill-peer-connections ok"
  else
    log "  kill-peer-connections FAILED (control socket unresponsive)"
  fi
  sleep 5
  left="$(gwl_pool_left)"
  [ "${left:-0}" -lt 20 ] && log "  pool refill: +$(gwl_pool_fill 4)"
  ip="$(gwl_take_peers 1)"
  # One peer, then let getaddr gossip refill: header sync asks ONE peer for
  # 2000 headers and waits, so extra peers buy resilience, not speed.
  if [ -n "$ip" ] && gwl_add_peers "$ip" >/dev/null 2>&1; then
    log "  re-seeded 1 peer: $ip"
  else
    log "  re-seed FAILED ($ip)"
  fi
  LAST_RECOVER="$(date +%s)"
}

log "supervisor start (pier=$GW_PIER stale=${STALE}s poll=${POLL}s sidecar=${GW_SIDECAR:-none})"

while true; do
  # 1. runtime alive?
  if [ -z "$(gwl_king_pid)" ] && [ -z "$(gwl_serf_pid)" ]; then
    N_VERE=$(( N_VERE + 1 ))
    log "INTERVENTION #$(( N_WEDGE + N_VERE + N_SIDE )) VERE-DOWN (restart #$N_VERE)"
    gwl_start_vere_restart
    sleep 60
    continue
  fi

  # 2. sidecar alive?  A dead sidecar IS the wedge trigger, so do not wait out
  #    the staleness window for it.
  if [ -x "${GW_SIDECAR:-}" ] && [ -z "$(gwl_sidecar_pids)" ]; then
    N_SIDE=$(( N_SIDE + 1 ))
    log "INTERVENTION #$(( N_WEDGE + N_VERE + N_SIDE )) SIDECAR-DOWN (restart #$N_SIDE)"
    gwl_start_sidecar
    now="$(date +%s)"
    if [ $(( now - LAST_RECOVER )) -ge $COOLDOWN ]; then recover "sidecar had died"; fi
    sleep $POLL
    continue
  fi

  # 3. event-log progress
  AGE="$(gwl_evt_age)"
  if [ "$AGE" -gt "$STALE" ]; then
    now="$(date +%s)"
    if [ $(( now - LAST_RECOVER )) -ge $COOLDOWN ]; then recover "event log stale ${AGE}s"; fi
  fi

  sleep $POLL
done
GWSUP
  chmod +x "$GW_DIR/bin/gwsup.sh"
}

# ==================================================================== boot ==
# Adopt a pier from the pre-2026.8 flat layout rather than booting a second
# copy of the same identity next to it.
locate_pier() {
  GW_PIER="$GW_DIR/piers/$NAME"
  if [ ! -d "$GW_PIER" ] && [ -d "$GW_DIR/$NAME/.urb" ]; then
    GW_PIER="$GW_DIR/$NAME"
    info "using the existing pier at $GW_PIER (older layout)"
  fi
}

export_env() {
  cat > "$GW_DIR/var/$NAME.env" <<EOF
GW_NAME='$NAME'
GW_PIER='$GW_PIER'
GW_VERE='$VERE'
GW_LOG='$GW_LOG'
GW_SC_LOG='$GW_SC_LOG'
GW_SIDECAR='${GW_SIDECAR:-}'
GW_AMES_PORT='$AMES_PORT'
GW_LOOM='$LOOM'
EOF
}

boot_ship() {
  step "Booting $COMET"
  GW_LOG="$GW_DIR/var/$NAME.log"
  GW_SC_LOG="$GW_DIR/var/sc-$NAME.log"
  if [ "${HAVE_SIDECAR:-0}" = 1 ]; then GW_SIDECAR="$SIDECAR"; else GW_SIDECAR=""; fi
  export GW_NAME="$NAME" GW_PIER GW_VERE="$VERE" GW_LOG GW_SC_LOG GW_LOOM="$LOOM"
  export GW_AMES_PORT="$AMES_PORT" GW_SIDECAR GW_DIR SOCK_TOOL

  if [ -n "$(gwl_king_pid)" ] || [ -n "$(gwl_serf_pid)" ]; then
    info "already running (pid $(gwl_king_pid) $(gwl_serf_pid))"
    return 0
  fi

  if [ -d "$GW_PIER/.urb" ]; then
    info "pier exists at $GW_PIER -- restarting it, not re-creating it"
    info "(nothing here ever overwrites or deletes a pier)"
    gwl_start_vere_restart
  else
    [ -e "$GW_PIER" ] && die "$GW_PIER exists but is not a pier; refusing to touch it"
    check_ports
    mkdir -p "$GW_DIR/piers"
    : > "$GW_LOG"
    local args="-t --loom $LOOM -c $GW_PIER -w $NAME -G $FEED -B $PILL --http-port $HTTP_PORT"
    [ -n "$AMES_PORT" ] && args="$args -p $AMES_PORT"
    info "gw-vere -t --loom $LOOM -c $GW_PIER -w $NAME -G <feed> -B <pill> --http-port $HTTP_PORT${AMES_PORT:+ -p $AMES_PORT}"
    if have setsid; then
      # shellcheck disable=SC2086
      setsid nohup "$VERE" $args >> "$GW_LOG" 2>&1 </dev/null &
    else
      # shellcheck disable=SC2086
      nohup "$VERE" $args >> "$GW_LOG" 2>&1 </dev/null &
    fi
  fi
  wait_for_ship
  check_identity
}

check_ports() {
  local r
  r=0; port_busy TCP "$HTTP_PORT" || r=$?
  if [ "$r" = 0 ]; then
    die "TCP port $HTTP_PORT is already in use. Pass --port <n> with a free one."
  fi
  if [ -n "$AMES_PORT" ]; then
    r=0; port_busy UDP "$AMES_PORT" || r=$?
    if [ "$r" = 0 ]; then
      die "UDP port $AMES_PORT is already in use -- probably a stale ship or an
    old supervisor squatting it. Booting onto a bound port produces
    'mesa: bind: address already in use', which reads like a crash loop.
    Find it by exact pier match before you kill anything."
    fi
  fi
}

wait_for_ship() {
  local waited=0 limit=900
  printf '    waiting for the ship'
  while [ "$waited" -lt "$limit" ]; do
    if [ -S "$GW_PIER/.urb/conn.sock" ]; then
      if [ -n "$(gwl_our 30)" ]; then printf '\n'; good "ship is up and answering"; return 0; fi
    fi
    if grep -q 'dawn: invalid private keys' "$GW_LOG" 2>/dev/null; then
      printf '\n'; die "vere refused the feed: 'dawn: invalid private keys'.
    The feed must be the @uw (0w...) atom Causeway printed. See $GW_LOG."
    fi
    if grep -q 'bind: address already in use' "$GW_LOG" 2>/dev/null; then
      printf '\n'; die "another process holds this ship's UDP port. See $GW_LOG."
    fi
    if [ -z "$(gwl_king_pid)" ] && [ -z "$(gwl_serf_pid)" ] && [ "$waited" -gt 20 ]; then
      printf '\n'; die "gw-vere exited during boot. The last lines of $GW_LOG:
$(tail -n 15 "$GW_LOG" 2>/dev/null | sed 's/^/      /')"
    fi
    printf '.'
    sleep 5
    waited=$(( waited + 5 ))
  done
  printf '\n'
  die "the ship did not come up within $(( limit / 60 )) minutes. See $GW_LOG."
}

# The -w/-G guard, checked from the other end: whatever vere did, this is who
# actually came up. A self-mined comet is silent otherwise.
check_identity() {
  local our
  our="$(gwl_our 120)"
  if [ -z "$our" ]; then
    warn "could not read the ship's identity back over the control socket;
    continuing, but verify by hand:  grep -m1 '~' $GW_LOG"
    return 0
  fi
  if [ "$our" != "~$NAME" ]; then
    die "THE SHIP THAT CAME UP IS NOT THE ONE YOU ASKED FOR.
    asked for : ~$NAME
    booted as : $our
    This is what a bad -w/-G pairing looks like. The pier at $GW_PIER holds
    the wrong identity; nothing here will delete it for you. Stop the ship
    ($SELF --stop --comet $our), move that pier aside, and re-run with the
    feed Causeway printed for ~$NAME."
  fi
  good "identity confirmed: $our"
}

save_proof() {
  [ -n "$PROOF" ] || return 0
  [ -f "$PROOF" ] || die "--proof $PROOF does not exist"
  cp "$PROOF" "$GW_DIR/var/${NAME}.proof.json"
  info "saved proof -> $GW_DIR/var/${NAME}.proof.json"
  info "NOTE: the runtime does not consume proof.json yet. The custody log a"
  info "peer actually checks rides your boot feed; bake it with"
  info "'causeway finalize <proof> --feed <feed>' and boot THAT feed."
}

# ================================================================ bitcoin ===
bitcoin_preconditions() {
  local desks missing=""
  step "Checking the light-client stack"

  # The binary is a file test, so ask that before bothering the ship. If it is
  # absent the release predates #67 and the desks cannot be there either.
  if [ "${HAVE_SIDECAR:-0}" != 1 ]; then
    missing="tcp-sidecar binary"
  else
    desks="$(gwl_desks 180)"
    case "$desks" in
      *base*)
        # A readable desk list. Now the two that matter.
        case "$desks" in *node*) : ;; *) missing="${missing:+$missing, }%node desk" ;; esac
        case "$desks" in *tcp-sidecar*) : ;; *) missing="${missing:+$missing, }%tcp-sidecar desk" ;; esac ;;
      *)
        # No %base in the answer means we did not get a desk list at all --
        # say so rather than reporting desks missing on that evidence.
        warn "could not read the ship's desk list over the control socket.
    Continuing; if %node is not really there, the next step will say so.
    Check by hand with:  $SELF --status --comet $COMET" ;;
    esac
  fi

  if [ -n "$missing" ]; then
    printf '\n%s  This release cannot run the Bitcoin light client.%s\n' "$C_Y" "$C_0"
    info ""
    info "  missing: $missing"
    info ""
    info "  The sidecar binary and the %node / %tcp-sidecar desks in the pill"
    info "  both land with gwbtc/urbit PR #67, which was open when this script"
    info "  was written. Release $TAG does not have them."
    info ""
    info "  Your comet is booted and is a working ship. What it cannot do yet"
    info "  is reach Bitcoin, so it cannot verify any peer and no peer can"
    info "  verify it."
    info ""
    info "  Either:"
    info "    - re-run this installer when a release newer than #67 exists"
    info "      (it will restart your existing pier, not re-create it), or"
    info "    - build the sidecar and install the two desks by hand:"
    info "      groundwire/doc/OPERATIONS.md sections 3.3 and 5.4."
    return 1
  fi
  good "sidecar binary present, %node and %tcp-sidecar are in the pill"
  return 0
}

start_sidecar() {
  step "Starting the tcp-sidecar"
  if [ -n "$(gwl_sidecar_pids)" ]; then
    info "already running (pid $(gwl_sidecar_pids))"
    return 0
  fi
  if gwl_start_sidecar; then
    good "sidecar running (pid $(gwl_sidecar_pids)), log $GW_SC_LOG"
  else
    die "the sidecar did not stay up. See $GW_SC_LOG.
    It is the light client's only transport; nothing syncs without it."
  fi
}

# A pill-baked desk activates locally whether or not it carries a desk.ship.
# This comment used to claim the opposite; it was wrong.  +on-init in
# lib/hood/kiln.hoon walks every desk in the pill and emits
# `%zest <desk> %live` unconditionally, and only THEN, if the desk has a
# desk.ship naming someone else, additionally opens an OTA sync to them.  The
# sync is a second channel, not a gate.  Measured on a fresh comet booted
# from groundwire-daily-2026.8.7, whose %groundwire desk does carry a
# desk.ship pointing at an unreachable distribution ship:
#
#   .^((set [dude live=?]) %ge /<our>/groundwire/<now>/$)
#     ~[[dude=%urb-watcher live=%.y] [dude=%reg-tester live=%.y]
#       [dude=%urb-snapshot live=%.y]]
#
# The "kiln: activation failed ...; retrying sync" line names that OTA sync
# and nothing else -- see the +gwl_ready comment for what actually emits it.
#
# So this is not a workaround for desk.ship.  It is a fallback for the plain
# case where a desk is not in the pill at all, which is every release older
# than the one that baked in %node and %tcp-sidecar.  A poke is safe where a
# scry into a missing agent is not.
ensure_agents() {
  local a desk
  for a in bitcoin-client tcp; do
    case "$a" in
      bitcoin-client) desk=node ;;
      tcp)            desk=tcp-sidecar ;;
    esac
    if gwl_agent_installed "$a"; then continue; fi
    info "%$a is not installed yet; installing %$desk"
    gwl_poke hood kiln-install "!>([%$desk our %$desk])" 120 >/dev/null || true
    sleep 20
    if gwl_agent_installed "$a"; then
      good "%$a installed"
    else
      warn "%$a still is not in the log after a kiln-install of %$desk.
    Sync will not start without it. Check $GW_LOG."
    fi
  done
}

seed_peers() {
  step "Seeding peers"
  local left added batch i n
  left="$(gwl_pool_left)"
  if [ "${left:-0}" -lt $(( SEED_BATCHES * SEED_BATCH_SIZE )) ]; then
    info "resolving x49.* DNS seeds (NODE_COMPACT_FILTERS only)"
    added="$(gwl_pool_fill 6)"
    info "pool: +$added new addresses"
  fi
  left="$(gwl_pool_left)"
  info "$left unused addresses available for ~$NAME"
  if [ "${left:-0}" = 0 ]; then
    warn "no peer addresses. The light client connects only to addresses you
    poke in -- there is no DNS seeding and no hardcoded list inside
    %bitcoin-client -- so sync will not start. Check DNS and re-run."
    return 1
  fi

  i=0
  while [ "$i" -lt "$SEED_BATCHES" ]; do
    batch="$(gwl_take_peers "$SEED_BATCH_SIZE")"
    [ -n "$batch" ] || break
    n="$(printf '%s\n' "$batch" | grep -c .)"
    i=$(( i + 1 ))
    printf '    batch %d: %d peers ... ' "$i" "$n"
    if gwl_add_peers "$batch" >/dev/null 2>&1; then printf 'ok\n'; else printf 'no reply (the ship may be busy; continuing)\n'; fi
    # Batches of ~25 with a pause. Bulk-adding SIGSEGVs the sidecar.
    [ "$i" -lt "$SEED_BATCHES" ] && sleep 30
  done
  good "seeded $(( i * SEED_BATCH_SIZE )) peer addresses in $i batches"
}

start_supervisor() {
  step "Starting the supervisor"
  export_env
  if [ -d "$GW_DIR/var/sup-$NAME.lock" ]; then
    local p
    p="$(cat "$GW_DIR/var/sup-$NAME.lock/pid" 2>/dev/null || echo)"
    if [ -n "$p" ] && kill -0 "$p" 2>/dev/null; then
      info "already supervised (pid $p)"
      return 0
    fi
  fi
  if have setsid; then
    setsid nohup "$GW_DIR/bin/gwsup.sh" "$NAME" >> "$GW_DIR/var/sup-$NAME.out" 2>&1 </dev/null &
  else
    nohup "$GW_DIR/bin/gwsup.sh" "$NAME" >> "$GW_DIR/var/sup-$NAME.out" 2>&1 </dev/null &
  fi
  sleep 2
  good "supervisor running; log $GW_DIR/var/sup-$NAME.log"
  info "it restarts vere and the sidecar, and after a sidecar restart it also"
  info "runs the gwbtc/node#1 recovery -- kill-peer-connections and a re-seed"
  info "-- which is the part a plain Restart=always unit gets wrong."
}

# ---------------------------------------------------------------- progress --
hms() { printf '%02d:%02d:%02d' $(( $1 / 3600 )) $(( ($1 % 3600) / 60 )) $(( $1 % 60 )); }

watch_sync() {
  step "Syncing the light client"
  info "From genesis this is the long pole, and it runs in two phases:"
  info "  1. block headers   -- and filter headers correctly sit at 1 for the"
  info "                        whole of it. That is not a fault. Tearing down"
  info "                        a healthy peer set at the halfway mark because"
  info "                        of it has happened (OPERATIONS.md 9)."
  info "  2. filter headers  -- only peers advertising NODE_COMPACT_FILTERS can"
  info "                        serve these, which is why the seeds above were"
  info "                        x49-filtered."
  info "Published runs put the total between ~1h20m and ~2.5h."
  info ""
  info "Safe to Ctrl-C: the ship and its supervisor keep running. Re-attach"
  info "with:  $SELF --status --comet $COMET"
  info ""

  # A tip height makes the ETA real instead of a guess. It is public read-only
  # data and entirely optional -- no tip, no ETA, everything else unchanged.
  local tip
  tip="$(curl -fsSL --max-time 6 https://mempool.space/api/blocks/tip/height 2>/dev/null | tr -cd '0-9' || true)"
  if [ -n "$tip" ]; then info "chain tip is $tip (mempool.space)"; else info "chain tip unknown; reporting rate without an ETA"; fi

  local start now el h fh peers synced phase target cur prev=0 prevt=0 rate eta
  start="$(date +%s)"
  while true; do
    gwl_log_info
    sleep 8
    now="$(date +%s)"; el=$(( now - start ))
    h="$(gwl_log_last headers)";              h="${h:-0}"
    fh="$(gwl_log_last filter-headers)";      fh="${fh:-0}"
    peers="$(gwl_log_last live-earth-peers)"; peers="${peers:-0}"
    synced="$(gwl_log_synced)"

    # ++continue-syncing-headers asks for no filter header until block headers
    # are synced, so exactly one of the two is moving at any time.
    if [ -n "$tip" ] && [ "$h" -ge $(( tip - 2000 )) ]; then
      phase="filter-headers"; cur="$fh"
    elif [ "$fh" -gt 1 ]; then
      phase="filter-headers"; cur="$fh"
    else
      phase="block-headers";  cur="$h"
    fi
    target="${tip:-0}"

    rate=""; eta=""
    if [ "$prevt" -gt 0 ] && [ "$cur" -gt "$prev" ] && [ "$now" -gt "$prevt" ]; then
      rate=$(( (cur - prev) * 60 / (now - prevt) ))
      if [ "$rate" -gt 0 ] && [ "$target" -gt "$cur" ]; then
        eta="  eta ~$(( (target - cur) / rate / 60 ))m of $phase"
      fi
    fi
    prev="$cur"; prevt="$now"

    printf '    [%s] headers %s  filter-headers %s  peers %s%s%s\n' \
      "$(hms "$el")" "$h" "$fh" "$peers" "${rate:+  ${rate}/min}" "$eta"

    # [%is-synced %.y] is the only authoritative answer: ++is-fully-synced is
    # an equality between the best block hash and the best filter-header hash,
    # not a height comparison (OPERATIONS.md 5.7).
    case "$synced" in
      *'%.y'*)
        good "[%is-synced %.y] -- block headers and filter headers are both at the tip"
        return 0 ;;
    esac

    if [ "$el" -gt 300 ] && [ "$peers" = 0 ]; then
      warn "no live peers after $(hms "$el"). Either the sidecar died -- check
    $GW_SC_LOG for '--- CRASH: signal 11 ---' -- or every seeded address was
    unreachable. The supervisor retries both; re-running this script re-seeds."
    fi
    sleep 52
  done
}

# ================================================================== status ==
cmd_status() {
  [ -d "$GW_DIR" ] || die "no install at $GW_DIR"
  if [ -z "$COMET" ]; then
    local n
    n="$(find "$GW_DIR/piers" -mindepth 1 -maxdepth 1 -type d -exec basename {} \; 2>/dev/null | head -2)"
    [ -n "$n" ] || die "no piers under $GW_DIR/piers; pass --comet"
    if [ "$(printf '%s\n' "$n" | grep -c .)" -gt 1 ]; then
      die "more than one pier under $GW_DIR/piers; say which with --comet"
    fi
    NAME="$n"; COMET="~$n"
  else
    validate_comet
  fi
  VERE="$GW_DIR/bin/gw-vere"; [ -x "$VERE" ] || VERE="$GW_DIR/gw-vere"
  [ -x "$VERE" ] || die "no gw-vere under $GW_DIR"
  SIDECAR="$GW_DIR/bin/tcp-sidecar"
  locate_pier
  GW_LOG="$GW_DIR/var/$NAME.log"; GW_SC_LOG="$GW_DIR/var/sc-$NAME.log"
  [ -f "$GW_LOG" ] || GW_LOG="$GW_DIR/$NAME.log"
  export GW_NAME="$NAME" GW_PIER GW_VERE="$VERE" GW_LOG GW_SC_LOG GW_DIR
  export GW_SIDECAR="$SIDECAR" GW_LOOM="$LOOM" GW_AMES_PORT="$AMES_PORT"
  pick_sock_tool
  # shellcheck source=/dev/null
  . "$GW_DIR/lib/gwlib.sh" 2>/dev/null || die "no $GW_DIR/lib/gwlib.sh; re-run an install first"

  step "Status of ~$NAME"
  [ -f "$GW_DIR/var/release.txt" ] && sed 's/^/    /' "$GW_DIR/var/release.txt"
  info "pier          $GW_PIER"
  local kp sp scp age sup
  kp="$(gwl_king_pid)"; sp="$(gwl_serf_pid)"; scp="$(gwl_sidecar_pids)"
  info "vere          ${kp:-${sp:-not running}}"
  info "sidecar       ${scp:-not running}"
  age="$(gwl_evt_age)"
  info "event log     ${age}s since the last write (>300s is the wedge threshold)"
  sup="$(cat "$GW_DIR/var/sup-$NAME.lock/pid" 2>/dev/null || echo)"
  if [ -n "$sup" ] && kill -0 "$sup" 2>/dev/null; then
    info "supervisor    pid $sup"
  else
    info "supervisor    not running"
  fi
  if [ -n "$kp$sp" ]; then
    info "identity      $(gwl_our 60)"
    gwl_log_info
    sleep 6
    info "headers       $(gwl_log_last headers)"
    info "filter-hdrs   $(gwl_log_last filter-headers)"
    info "live peers    $(gwl_log_last live-earth-peers)"
    info "is-synced     $(gwl_log_synced)"
    local rdy
    rdy="$(gwl_ready 60 | tr -d '\n')"
    case "$rdy" in
      *thread-fail*|"")
        info "/x/ready      unavailable -- %gw-btc is not installed on this ship" ;;
      *) info "/x/ready      $rdy" ;;
    esac
  fi
  info ""
  info "interventions $(grep -c INTERVENTION "$GW_DIR/var/sup-$NAME.log" 2>/dev/null || echo 0) logged by the supervisor"
}

# ==================================================================== stop ==
# Order matters, and it is the order that caused an incident when it was not
# written down: stopping a supervised ship without stopping its supervisor
# first is a no-op -- the supervisor notices VERE-DOWN and relaunches within
# one poll. Supervisor, then runtime, then sidecar (OPERATIONS.md 5.10).
cmd_stop() {
  [ -n "$COMET" ] || die "--stop needs --comet <@p> so it kills exactly one ship"
  validate_comet
  VERE="$GW_DIR/bin/gw-vere"; SIDECAR="$GW_DIR/bin/tcp-sidecar"
  locate_pier
  GW_LOG="$GW_DIR/var/$NAME.log"; GW_SC_LOG="$GW_DIR/var/sc-$NAME.log"
  export GW_NAME="$NAME" GW_PIER GW_VERE="$VERE" GW_LOG GW_SC_LOG GW_DIR
  export GW_SIDECAR="$SIDECAR" GW_LOOM="$LOOM" GW_AMES_PORT="$AMES_PORT"
  pick_sock_tool
  # shellcheck source=/dev/null
  . "$GW_DIR/lib/gwlib.sh" 2>/dev/null || die "no $GW_DIR/lib/gwlib.sh"

  step "Stopping ~$NAME"
  local p
  p="$(cat "$GW_DIR/var/sup-$NAME.lock/pid" 2>/dev/null || echo)"
  if [ -n "$p" ] && kill -0 "$p" 2>/dev/null; then
    kill "$p" 2>/dev/null || true
    sleep 2
    info "supervisor stopped (pid $p)"
  else
    info "no supervisor running"
  fi
  rm -rf "$GW_DIR/var/sup-$NAME.lock"

  for p in $(gwl_king_pid); do
    kill "$p" 2>/dev/null || true
    info "SIGTERM to vere (pid $p); the serf exits with it and the pier replays"
  done
  sleep 5
  for p in $(gwl_sidecar_pids); do
    kill "$p" 2>/dev/null || true
    info "stopped sidecar (pid $p)"
  done
  good "stopped. The pier is untouched; re-run boot.sh to bring it back."
}

# =================================================================== install =
cmd_install() {
  validate_comet
  validate_feed
  validate_port --port "$HTTP_PORT"
  [ -n "$AMES_PORT" ] && validate_port --ames-port "$AMES_PORT"
  case "$LOOM" in ''|*[!0-9]*) usagedie "--loom must be a number" ;; esac

  detect_platform
  preflight
  resolve_tag
  fetch_release
  install_release
  write_helpers
  locate_pier
  # shellcheck source=/dev/null
  . "$GW_DIR/lib/gwlib.sh"
  save_proof
  boot_ship
  export_env

  if [ "$DO_BITCOIN" = 0 ]; then
    step "Done (--no-bitcoin)"
    info "ship is running; the light client was not started."
    summary_lines
    exit 0
  fi

  if ! bitcoin_preconditions; then
    printf '\n'
    summary_lines
    exit 0
  fi
  start_sidecar
  ensure_agents
  [ "$DO_SUPERVISOR" = 1 ] && start_supervisor
  seed_peers || true

  if [ "$DO_WAIT" = 1 ]; then
    watch_sync
  else
    step "Running (--no-wait)"
    info "sync is under way; check on it with:  $SELF --status --comet $COMET"
  fi
  summary_lines
}

summary_lines() {
  step "Where things are"
  info "pier        $GW_PIER"
  info "ship log    $GW_LOG"
  [ -f "$GW_SC_LOG" ] && info "sidecar log $GW_SC_LOG"
  [ -f "$GW_DIR/var/sup-$NAME.log" ] && info "supervisor  $GW_DIR/var/sup-$NAME.log"
  info "http        http://127.0.0.1:$HTTP_PORT"
  info ""
  info "status      $SELF --status --comet $COMET"
  info "stop        $SELF --stop --comet $COMET"
  info "runbook     groundwire/doc/OPERATIONS.md"
}

case "$MODE" in
  status) cmd_status ;;
  stop)   cmd_stop ;;
  *)      cmd_install ;;
esac
