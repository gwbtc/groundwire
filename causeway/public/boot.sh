#!/usr/bin/env bash
#
# Groundwire installer.
#
#   curl -fsSL https://groundwire.io/causeway/boot.sh | bash -s -- \
#     --comet '~sampel-palnet-sampel-palnet--sampel-palnet-sampel-palnet' \
#     --feed 0w2M5n3.su88A...
#
# Causeway (web or desktop) mints the comet and hands you a @p and a feed.
# This script takes it from there and does the whole bring-up:
#
#   1. detect the platform and refuse the ones CI does not build
#   2. fetch the release tarball from gwbtc/urbit, check it against the
#      release's SHA256SUMS, and unpack it
#   3. boot the ship from the pill, with -w and -G given TOGETHER, and
#      prove afterwards that the @p that came up is the @p you asked for
#   4. start the tcp-sidecar -- the light client's only Bitcoin transport
#   5. put both under a supervisor that also performs the gwbtc/node#1
#      wedge recovery, which a bare `Restart=always` does not
#   6. seed NODE_COMPACT_FILTERS-capable peers, in batches
#   7. report sync progress until the light client reaches the chain tip
#
# It is the scripted form of ops/doc/OPERATIONS.md sections 5.3, 5.5,
# 5.6, 5.7 and 5.9. Every non-obvious thing it does is there because that
# document records it costing somebody hours; the comments name the section.
#
# This is a curl-pipe-to-bash installer. It runs as you, installs under your
# home directory, asks for no privilege it does not need, never writes outside
# its install directory, and never overwrites an existing pier. Where it is
# not sure what you meant, it stops and says so rather than guessing.
#
# WHAT IS AND IS NOT VERIFIED: from gwbtc/urbit#67 the release publishes a
# SHA256SUMS, and this script fetches it and checks the tarball against it
# before unpacking anything, deleting the download on a mismatch. That catches
# a corrupted or truncated fetch and gives you one digest to compare against a
# mirror. It is NOT a defence against a compromised GitHub: those digests
# arrive over the same TLS connection as the tarball, and nothing in the
# release is signed. Pass --sha256 <hex> to pin a digest you got from
# somewhere you trust more. Releases older than #67 publish no SHA256SUMS and
# this script says so rather than pretending otherwise.
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
#
# CHANGED SINCE, AND NOT RE-RUN AGAINST A LIVE SHIP: gwlib.sh and gwsup.sh
# moved out of this file into the release tarball (same bytes, different
# delivery), and the SHA256SUMS path in +verify_release went from dead
# future-proofing to a check that runs -- including a fix for a case that
# would have deleted a perfectly good download on any machine with no
# sha256sum, shasum or openssl on it. Both were exercised against a tarball
# built by hand; neither has been through a real install.

set -euo pipefail

BOOT_SH_VERSION="2026.8.10"
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
PORT_EXPLICIT="${GROUNDWIRE_PORT:+1}"
AMES_PORT=""
TAG="${GROUNDWIRE_VERSION:-latest}"
LOOM="${GROUNDWIRE_LOOM:-32}"
EXPECT_SHA="${GROUNDWIRE_SHA256:-}"
COMET=""
FEED=""
FEED_FILE=""
MEMPOOL_API="${MEMPOOL_API:-https://mempool.space/api}"
PROOF=""
MODE="install"
MINT_XPUB=""
MINT_SPONSOR=""
MINT_FIEF=""
MINT_RESUME=0
MINT_UI=tui
MINT_ARGS=""
REMINT=""
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
  Mint a new confidential comet and boot it, in one command:

    curl -fsSL https://groundwire.io/causeway/boot.sh | bash -s -- --mint

  Boot a comet you have already minted and finalized:

    curl -fsSL https://groundwire.io/causeway/boot.sh | bash -s -- \\
      --comet '~sampel-palnet-...' --feed-file ./my.feed

  boot.sh --status                 report on an existing install and exit
  boot.sh --stop                   stop a running ship, in the safe order

MINT MODE (--mint)
  Installs the release, runs Causeway to spawn a comet, waits for the spawn
  transaction to confirm, bakes the custody log into the boot feed, and boots.
  Interactive: it asks you to fund an address and to write down a recovery
  phrase.

  --xpub <descriptor> sign with your own wallet instead of one Causeway
                     generates (a BIP-380 taproot descriptor or xpub)
  --sponsor <@p>     sponsor to commit in the initial snapshot. Peers reach a
                     confidential comet through its sponsor.
  --fief <IP:PORT>   commit a static endpoint. Implies --ames-port <PORT>,
                     because a fief the ship does not bind is a lie. A comet
                     that others will name as their sponsor needs one.
  --remint           ignore a finished mint in var/mint (archived, not deleted)
                     and mint a fresh comet. Without it, re-running --mint after
                     a completed mint boots the comet you already have.
  --resume           a previous mint died after you funded the wallet: re-enter
                     that run's seed phrase instead of minting a fresh wallet,
                     and the spawn picks up your already-funded address.
                     (Implies --headless for now.)
  --headless         use the plain prompt-based flow instead of the TUI.
                     The default at a terminal is the TUI; no terminal, or
                     --resume/--xpub, falls back to prompts automatically.

REQUIRED (for an install)
  --comet <@p>       the comet Causeway minted for you, with the leading ~.
                     QUOTE IT: '~sampel-...'. A bare leading ~ is a home
                     directory to the shell, and zsh (the macOS default)
                     fails outright with "no such user or named directory".
                     The ~ is optional here if you would rather not quote.
  --feed-file <path> the boot feed, read from a file. PREFER THIS: a feed is
                     your ship's private key, and an argument lands in shell
                     history. \`causeway finalize --out-feed\` writes one.
  --feed  <0w...>    the same thing inline. Convenient, and it puts a private
                     key in your history.

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
  --sha256 <hex>     require the tarball to have this SHA-256. The release's
                     own SHA256SUMS is fetched and checked without this; pass
                     it to pin a digest you got somewhere you trust more.
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
    --feed-file)  [ $# -ge 2 ] || usagedie "--feed-file needs a value"; FEED_FILE="$2"; shift 2 ;;
    --mint)       MODE="mint"; shift ;;
    --xpub)       [ $# -ge 2 ] || usagedie "--xpub needs a value"; MINT_XPUB="$2"; shift 2 ;;
    --sponsor)    [ $# -ge 2 ] || usagedie "--sponsor needs a value"; MINT_SPONSOR="$2"; shift 2 ;;
    --fief)       [ $# -ge 2 ] || usagedie "--fief needs a value"; MINT_FIEF="$2"; shift 2 ;;
    --resume)     MINT_RESUME=1; shift ;;
    --remint)     REMINT=1; shift ;;
    --headless)   MINT_UI=cli; shift ;;
    --proof)      [ $# -ge 2 ] || usagedie "--proof needs a value"; PROOF="$2"; shift 2 ;;
    --dir)        [ $# -ge 2 ] || usagedie "--dir needs a value"; GW_DIR="$2"; shift 2 ;;
    --port)       [ $# -ge 2 ] || usagedie "--port needs a value"; HTTP_PORT="$2"; PORT_EXPLICIT=1; shift 2 ;;
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
  # A feed is the ship's PRIVATE KEY.  Given inline it lands in shell history
  # and -- worse -- in vere's own argv, where `ps` shows it to every local user
  # for as long as the pier runs.  --feed-file keeps it off both.  vere still
  # takes -G on the command line, so this narrows the exposure to the ship's
  # runtime rather than removing it; closing that needs a vere change.
  if [ -n "$FEED_FILE" ]; then
    [ -n "$FEED" ] && usagedie "pass --feed or --feed-file, not both"
    [ -r "$FEED_FILE" ] || usagedie "--feed-file cannot be read: $FEED_FILE"
    FEED="$(tr -d '"'"' \t\r\n'"'"' < "$FEED_FILE")"
    [ -n "$FEED" ] || usagedie "--feed-file is empty: $FEED_FILE"
  fi
  case "$FEED" in
    "") usagedie "--feed or --feed-file is required (from \`causeway finalize\`)" ;;
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
  local got sums url want
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
    # Releases from gwbtc/urbit#67 onward publish a SHA256SUMS. Be clear about
    # what checking it buys: the digests come down the same TLS connection as
    # the tarball and nothing is signed, so this is no defence against a
    # compromised GitHub. It does catch a corrupted or truncated download, and
    # it gives you one line to compare against a copy somebody else mirrored.
    url="https://github.com/${REPO}/releases/download/${TAG}/SHA256SUMS"
    if sums="$(curl -fsSL --max-time 30 "$url" 2>/dev/null)" && [ -n "$sums" ]; then
      # sha256sum's format is "<hex>  <name>", with a '*' before the name in
      # binary mode. Match the name EXACTLY: a substring or regex match lets
      # the line for one platform answer for another, which is a check that
      # passes while verifying nothing.
      want="$(printf '%s\n' "$sums" | awk -v a="groundwire-${PLATFORM}.tar.gz" \
              '{n=$2; sub(/^\*/,"",n)} n==a {print $1; exit}')"
      if [ -z "$want" ]; then
        warn "this release's SHA256SUMS has no line for
    groundwire-${PLATFORM}.tar.gz, so the download is not verified. That is a
    fault in the release rather than on this machine; please report it."
      elif [ -z "$got" ]; then
        # Do not delete the tarball here. An empty $got means this machine has
        # no sha256sum, shasum or openssl -- not that the download is bad.
        warn "this release publishes a SHA256SUMS, but nothing on this machine
    can compute a SHA-256, so the download has not been verified."
      elif [ "$want" = "$got" ]; then
        good "sha256 matches the release's SHA256SUMS"
      else
        rm -f "$TARBALL"
        die "sha256 does NOT match the release's SHA256SUMS.
    expected  $want
    got       $got
    The download has been deleted. Try once more; if it happens again, do not
    install this and say so at https://github.com/${REPO}/issues."
      fi
    else
      warn "this release publishes no SHA256SUMS and no signature, so the only
    thing authenticating this download is GitHub's TLS certificate. Releases
    from gwbtc/urbit#67 onward do publish one. If you have a digest from a
    source you trust, re-run with --sha256 <hex>."
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
  # "Already installed" must mean COMPLETELY installed.  release.txt is
  # written after the moves, but an older boot.sh wrote it after moving fewer
  # files -- a machine that installed this tag before Causeway shipped has a
  # truthful release.txt and no Causeway, and skipping here would pin that
  # state forever.  So the short-circuit also requires every file this
  # version knows how to install, when the tarball provides it.
  if [ "$FORCE_REDOWNLOAD" = 0 ] && [ -x "$VERE" ] && [ -f "$PILL" ] &&
     grep -qx "tag=$TAG" "$GW_DIR/var/release.txt" 2>/dev/null &&
     { ! tar tzf "$TARBALL" 2>/dev/null | grep -qx './causeway' ||
       [ -x "$GW_DIR/causeway" ]; }; then
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
  # The installer's own two scripts. Absent from releases older than
  # gwbtc/urbit#67; install_helpers is where that is refused, so that the
  # message can name the release rather than a missing file.
  [ -f "$stage/gwlib.sh" ] && mv -f "$stage/gwlib.sh" "$GW_DIR/lib/gwlib.sh"
  [ -f "$stage/gwsup.sh" ] && mv -f "$stage/gwsup.sh" "$GW_DIR/bin/gwsup.sh"
  # Causeway: the launcher plus its source tree.  These go at the TOP of
  # GW_DIR rather than into bin/, because the launcher locates causeway-src/
  # relative to itself and builds its venv beside it.
  #
  # Packaging a file into the tarball and installing it are two different
  # jobs, and doing only the first is silent: the tarball verified, the
  # install reported success, and the mint died several steps later claiming
  # the RELEASE lacked Causeway when the release had it all along and this
  # function had simply walked past it.
  if [ -d "$stage/causeway-src" ]; then
    rm -rf "$GW_DIR/causeway-src"
    mv -f "$stage/causeway-src" "$GW_DIR/causeway-src"
  fi
  [ -f "$stage/causeway" ] && { mv -f "$stage/causeway" "$GW_DIR/causeway"; chmod +x "$GW_DIR/causeway"; }
  # boot.sh ships in the release too, so a pier can be managed without
  # re-fetching this script from the network.
  [ -f "$stage/boot.sh" ] && { mv -f "$stage/boot.sh" "$GW_DIR/boot.sh"; chmod +x "$GW_DIR/boot.sh"; }
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
# gwlib.sh (the shared helpers) and gwsup.sh (the per-ship supervisor) ride in
# the release tarball next to gw-vere and tcp-sidecar, and install_release put
# them on disk a moment ago. Their source is gwbtc/urbit, automation/installer.
#
# They used to be heredocs in this script -- 463 of its 1,521 lines, written
# out and executed later -- because a script piped in from curl has no file
# for the supervisor to re-exec. That reason was true and the price was worse:
# a third of a curl-pipe-to-bash installer that you had to audit before you
# could believe the other two thirds, on something that runs as you and
# installs a daemon mediating all your Bitcoin traffic. What is left here is
# about 250 lines of "does this fetch and exec the right thing", which you can
# finish before pressing return. The two files are covered instead by the
# release SHA256SUMS, which +verify_release checks above, and by being in git.
#
# Both land with gwbtc/urbit#67. A release older than that ships neither, and
# this refuses rather than half-installing a ship with no supervisor.
install_helpers() {
  local missing=""
  [ -f "$GW_DIR/lib/gwlib.sh" ] || missing="gwlib.sh"
  [ -f "$GW_DIR/bin/gwsup.sh" ] || missing="${missing:+$missing and }gwsup.sh"
  if [ -n "$missing" ]; then
    die "release $TAG ships no $missing.

    That is the installer's helper library and its per-ship supervisor. They
    were carried inside this script until gwbtc/urbit#67 and are in the
    release tarball from #67 onward, so a release built before it cannot be
    installed by this version of the script.

    Use a newer release -- omit --version to take the latest, or pick one:
      https://github.com/${REPO}/releases
    Nothing has been booted and no pier has been touched."
  fi
  chmod +x "$GW_DIR/lib/gwlib.sh" "$GW_DIR/bin/gwsup.sh"
  info "helpers       $GW_DIR/lib/gwlib.sh"
  info "supervisor    $GW_DIR/bin/gwsup.sh"
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
GW_HTTP_PORT='$HTTP_PORT'
GW_LOOM='$LOOM'
EOF
}

boot_ship() {
  step "Booting $COMET"
  GW_LOG="$GW_DIR/var/$NAME.log"
  GW_SC_LOG="$GW_DIR/var/sc-$NAME.log"
  if [ "${HAVE_SIDECAR:-0}" = 1 ]; then GW_SIDECAR="$SIDECAR"; else GW_SIDECAR=""; fi
  export GW_NAME="$NAME" GW_PIER GW_VERE="$VERE" GW_LOG GW_SC_LOG GW_LOOM="$LOOM"
  export GW_AMES_PORT="$AMES_PORT" GW_HTTP_PORT="$HTTP_PORT" GW_SIDECAR GW_DIR SOCK_TOOL

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
    # The HTTP port is a loopback convenience -- nothing on chain or in the
    # PKI names it (unlike an ames port pinned by a fief).  So when the user
    # did not pick it, a squatter on 8080 is OUR problem, not theirs: scan
    # forward and take the first free port.  An explicit --port stays an
    # explicit promise and a collision on it is still fatal.
    if [ "$PORT_EXPLICIT" = 1 ]; then
      die "TCP port $HTTP_PORT is already in use and you pinned it with --port.
    Pass a free port, or drop --port to let the installer pick one."
    fi
    local try
    for try in $(seq $((HTTP_PORT+1)) $((HTTP_PORT+50))); do
      if ! port_busy TCP "$try"; then
        info "port $HTTP_PORT is in use; using $try for HTTP instead"
        HTTP_PORT="$try"
        break
      fi
    done
    if port_busy TCP "$HTTP_PORT"; then
      die "no free TCP port found in $((HTTP_PORT-50))-$HTTP_PORT for the ship's HTTP listener."
    fi
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
    ($SELF --stop --comet '$our'), move that pier aside, and re-run with the
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
    Check by hand with:  $SELF --status --comet '$COMET'" ;;
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
    info "      ops/doc/OPERATIONS.md sections 3.3 and 5.4."
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
  info "with:  $SELF --status --comet '$COMET'"
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
  export GW_SIDECAR="$SIDECAR" GW_LOOM="$LOOM" GW_AMES_PORT="$AMES_PORT" GW_HTTP_PORT="$HTTP_PORT"
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
  export GW_SIDECAR="$SIDECAR" GW_LOOM="$LOOM" GW_AMES_PORT="$AMES_PORT" GW_HTTP_PORT="$HTTP_PORT"
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

# ====================================================================== mint =
# One command from nothing to a booted, verifiable confidential comet.
#
# Before this, the pieces existed but nothing joined them: Causeway was in no
# release at all (the tarball shipped gw-onboard, which mines the RETIRED v9
# %urb-watcher format), so the only way to mint an identity for the protocol
# in the pill was to clone the repo.
#
# The order is forced by the protocol and cannot be rearranged:
#   1. install the release        -- Causeway needs comet_miner from it
#   2. spawn                      -- pick a funding UTXO, THEN mine: the @p
#                                    commits that outpoint, so the comet does
#                                    not exist until the coin is chosen
#   3. wait for confirmation      -- the custody log needs a block
#   4. finalize                   -- bake xtr into the feed; this is the first
#                                    moment a BOOTABLE feed exists
#   5. boot                       -- with the baked feed, by file
#
# Skipping 4 is the expensive mistake: a ship booted from the raw miner feed
# has the right @p at the right life and an EMPTY custody log, so no peer can
# ever verify it, and the sats are spent.
cmd_mint() {
  validate_port --port "$HTTP_PORT"
  [ -n "$AMES_PORT" ] && validate_port --ames-port "$AMES_PORT"
  case "$LOOM" in ''|*[!0-9]*) usagedie "--loom must be a number" ;; esac

  # A fief names an exact IP:port, so the ship has to actually bind it.  Minting
  # one and then booting on a random port commits a promise on chain that the
  # ship does not keep, and the only repair is another on-chain rekey.
  if [ -n "$MINT_FIEF" ] && [ -z "$AMES_PORT" ]; then
    local fief_port="${MINT_FIEF##*:}"
    case "$fief_port" in
      ''|*[!0-9]*) usagedie "--fief must be IP:PORT, got $MINT_FIEF" ;;
    esac
    AMES_PORT="$fief_port"
    info "--fief given: pinning --ames-port $AMES_PORT so the fief is true"
  fi

  detect_platform
  preflight
  # A comet minted on an RC must not come back on a different channel: the
  # first real mint resumed without --version, resolved "latest" to the
  # daily -- a release whose PILL cannot do confidential comets -- and baked
  # that kernel into the pier.  The mint dir remembers its release; an
  # explicit --version still wins.
  if [ "$TAG" = latest ] && [ -s "$GW_DIR/var/mint/release-tag" ]; then
    TAG="$(tr -d " \t\r\n" < "$GW_DIR/var/mint/release-tag")"
    info "using the release this mint was made with: $TAG (override with --version)"
  fi
  resolve_tag
  fetch_release
  install_release
  install_helpers
  mkdir -p "$GW_DIR/var/mint" && printf '%s\n' "$TAG" > "$GW_DIR/var/mint/release-tag"

  # Distinguish the two ways this can be missing.  The first version of this
  # check blamed the release for what was in fact an installer that never
  # copied the file -- a message that sends someone to cut a new release to
  # fix a bug in the line above.
  local cw="$GW_DIR/causeway"
  if [ ! -x "$cw" ]; then
    if tar tzf "$TARBALL" 2>/dev/null | grep -qx './causeway'; then
      die "release $TAG contains Causeway but it was not installed to $cw.
    That is a bug in this script's install step, not in the release.
    Re-run with --redownload; if it persists, report it."
    fi
    die "release $TAG does not ship Causeway.
    Releases before gwbtc/urbit a19776f3 predate it being packaged. Use a
    newer one, or mint from a checkout of gwbtc/groundwire and re-run with
    --comet/--feed-file."
  fi

  local mintdir="$GW_DIR/var/mint"
  mkdir -p "$mintdir"; chmod 700 "$mintdir"

  local raw="$mintdir/spawn.feed" baked="$mintdir/boot.feed"

  # ---- already minted?  A mint's outputs are durable (proof + baked feed),
  # and a previous run can die AFTER them -- a port collision at boot did
  # exactly this on the first real run.  Re-running must keep the promise
  # the TUI makes ("re-running the installer picks up from here"): boot the
  # comet that exists instead of minting a second one.  --remint archives
  # the finished mint and starts over on purpose.
  if [ "$REMINT" = 1 ] && [ -e "$baked" ]; then
    local stamp; stamp="$(date -u +%Y%m%dT%H%M%SZ)"
    mv "$mintdir" "$GW_DIR/var/mint-$stamp"
    info "--remint: previous mint archived to $GW_DIR/var/mint-$stamp"
    mkdir -p "$mintdir"; chmod 700 "$mintdir"
  fi
  if [ -s "$baked" ]; then
    local prior
    prior="$(ls -t "$mintdir"/*-spawn*.proof.json 2>/dev/null | head -1 || true)"
    if [ -n "$prior" ]; then
      COMET="$(sed -n 's/.*"patp"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/p' "$prior" | head -1)"
    fi
    if [ -n "${COMET:-}" ]; then
      step "Resuming a finished mint"
      info "found $COMET already minted and finalized (proof + baked feed)."
      info "booting it. To mint a DIFFERENT comet instead, re-run with --remint."
      printf '\n'
      FEED_FILE="$baked"
      PROOF="$prior"
      MODE="install"
      cmd_install
      return
    fi
  fi

  # ---- which face?  The TUI is the default for a person at a terminal; the
  # CLI prompts remain for --headless, for --resume (not yet a TUI flow), for
  # --xpub, and for any environment without a tty.  The TUI takes its
  # arguments through CAUSEWAY_* env vars -- a full-screen app has no flags --
  # and hands back by DISK: it writes the proof and the raw feed into
  # $mintdir and exits.  Exit codes from a full-screen app are not evidence
  # (quitting is exit 0 too), so completion is judged by a proof file NEWER
  # than the launch marker, never by status.
  local ui="$MINT_UI"
  [ -e /dev/tty ] || ui=cli
  [ "$MINT_RESUME" = 1 ] && ui=cli
  [ -n "$MINT_XPUB" ] && ui=cli
  # A release older than the handoff contract has a TUI that ignores the env
  # vars entirely -- it would open on a blank spawn form and never hand back.
  if [ "$ui" = tui ] && ! grep -q "CAUSEWAY_HANDOFF" "$GW_DIR/causeway-src/causeway_tui.py" 2>/dev/null; then
    info "this release's TUI predates the mint handoff; using the CLI prompts"
    ui=cli
  fi

  local proof
  if [ "$ui" = tui ]; then
    step "Minting a confidential comet — Causeway TUI"
    info "complete the spawn in the interface; the installer resumes when you quit."
    printf '\n'
    local marker="$mintdir/.mint-start"
    touch "$marker"; sleep 1
    CAUSEWAY_SPONSOR="$MINT_SPONSOR" CAUSEWAY_FIEF="$MINT_FIEF" \
      CAUSEWAY_OUTPUT_DIR="$mintdir" CAUSEWAY_HANDOFF=1 \
      "$cw" tui </dev/tty >/dev/tty 2>&1 || true
    # Both proof spellings: the CLI writes <name>-spawn.proof.json, the TUI
    # writes <name>-spawn-<txid>.proof.json.  The narrow glob missed the
    # TUI's, which would fail a SUCCESSFUL spawn as "exited without
    # completing" -- found by reading, unreachable by the headless harness.
    #
    # The TUI writes the proof and the raw feed the moment the spawn
    # transaction is BUILT -- before the wallet signs it -- because the
    # wallet normally broadcasts too and nothing may be lost if it does.
    # So a proof on disk means "built", not "sent": ask the chain which of
    # the candidate transactions actually exists (mempool or block).  Newest
    # first; a Back-and-rebuild leaves an older proof for a tx that never
    # went anywhere.
    local cand txid seen=""
    proof=""
    # shellcheck disable=SC2045,SC2046  # names are <patp>-spawn-<txid>.proof.json: no spaces
    for cand in $(ls -t $(find "$mintdir" -maxdepth 1 -name '*-spawn*.proof.json' -newer "$marker" 2>/dev/null) 2>/dev/null); do
      [ -s "${cand%.json}.feed" ] || continue
      txid="$(sed -n 's/.*"commit_txid"[[:space:]]*:[[:space:]]*"\([0-9a-f]*\)".*/\1/p' "$cand" | head -1)"
      [ -n "$txid" ] || continue
      if curl -fsS --max-time 10 "$MEMPOOL_API/tx/$txid/status" >/dev/null 2>&1; then
        proof="$cand"; seen="$txid"; break
      fi
      [ -z "$proof" ] && proof="$cand"   # remember the newest, in case the network is unreachable
    done
    if [ -z "$proof" ]; then
      die "the TUI exited without completing a spawn.
    Nothing was booted. Re-run to try again, or add --headless for the
    prompt-based flow."
    fi
    if [ -z "$seen" ]; then
      txid="$(sed -n 's/.*"commit_txid"[[:space:]]*:[[:space:]]*"\([0-9a-f]*\)".*/\1/p' "$proof" | head -1)"
      if curl -fsS --max-time 10 "$MEMPOOL_API/blocks/tip/height" >/dev/null 2>&1; then
        die "the spawn transaction was built but the network has not seen it:
      $txid
    Nothing was spent and nothing was booted.  If your wallet DID just
    broadcast it, give it a minute and finish by hand:
      $GW_DIR/causeway finalize $proof --feed-file ${proof%.json}.feed --out-feed $baked
    then boot with --comet '<the @p in $proof>' --feed-file $baked.
    Otherwise re-run this installer to start over."
      fi
      warn "cannot reach $MEMPOOL_API to check the spawn transaction; continuing with the newest proof"
    fi
    # The TUI writes the raw feed beside the proof: <name>-spawn.proof.feed
    raw="${proof%.json}.feed"
    [ -s "$raw" ] || die "the TUI wrote a proof but no feed file ($raw).
    Treat this as a bug; finish by hand with 'causeway finalize'."
  else
    local args=(spawn)
    if [ -n "$MINT_XPUB" ]; then args+=(connect --xpub "$MINT_XPUB"); else args+=(generate); fi
    args+=(--output-dir "$mintdir" --out-feed "$raw")
    # Explicit, even though the launcher also exports GROUNDWIRE_HOME: two
    # independent routes to the same binary, either alone sufficient.
    args+=(--miner "$GW_DIR/bin/comet_miner")
    [ -n "$MINT_SPONSOR" ] && args+=(--sponsor "$MINT_SPONSOR")
    [ -n "$MINT_FIEF" ]    && args+=(--fief "$MINT_FIEF")
    # --resume: a previous run died after the wallet was funded.  Causeway
    # prompts for the phrase from that run instead of minting a fresh wallet
    # (which would strand the previous run's sats at an address nothing
    # watches).  generate-flow only; a connect flow re-runs with the same xpub.
    [ "$MINT_RESUME" = 1 ] && [ -z "$MINT_XPUB" ] && args+=(--resume)

    step "Minting a confidential comet with Causeway"
    info "this is interactive: it will ask you to fund an address and to write"
    info "down a recovery phrase. Do not skip the phrase."
    printf '\n'
    # </dev/tty because boot.sh is usually running under `curl | bash`, where
    # stdin is the SCRIPT, not the keyboard.  Without this the first prompt eats
    # the rest of the script and the mint dies half way through.
    "$cw" "${args[@]}" </dev/tty || die "causeway spawn failed or was cancelled"

    [ -s "$raw" ] || die "causeway spawn did not write a feed to $raw.
    Nothing has been booted. If the transaction broadcast, your proof is in
    $mintdir and you can finish by hand with 'causeway finalize'."
    proof="$(ls -t "$mintdir"/*-spawn.proof.json 2>/dev/null | head -1 || true)"
    [ -n "$proof" ] || die "no spawn proof found in $mintdir"
  fi

  COMET="$(sed -n 's/.*"patp"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/p' "$proof" | head -1)"
  [ -n "$COMET" ] || die "could not read the comet @p out of $proof"
  good "minted $COMET"

  step "Baking the custody log into the boot feed"
  info "instant if Causeway already saw the confirmation; otherwise this"
  info "waits for the spawn transaction to confirm first."
  printf '\n'
  "$cw" finalize "$proof" --feed-file "$raw" --out-feed "$baked" </dev/tty \
    || die "causeway finalize failed.
    Your comet is minted and on chain. Nothing is lost: re-run
      $GW_DIR/causeway finalize $proof --feed-file $raw --out-feed $baked
    and then boot with --comet '$COMET' --feed-file $baked"

  [ -s "$baked" ] || die "finalize wrote no baked feed to $baked.
    Refusing to boot: the raw feed would give you an unverifiable comet."

  FEED_FILE="$baked"
  PROOF="$proof"
  good "custody log baked; booting"
  MODE="install"
  cmd_install
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
  install_helpers
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
    info "sync is under way; check on it with:  $SELF --status --comet '$COMET'"
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
  info "status      $GW_DIR/boot.sh --status --comet '$COMET'"
  info "stop        $GW_DIR/boot.sh --stop --comet '$COMET'"
  info "runbook     ops/doc/OPERATIONS.md"

  # The printout above is gone with the scrollback; the same answers,
  # durable.  "How do I turn it off" must never require finding this
  # script's output again.
  write_ship_readme
}

write_ship_readme() {
  local feedline=""
  [ -n "${FEED_FILE:-}" ] && feedline=" --feed-file $FEED_FILE"
  cat > "$GW_DIR/README" <<EOF
Your Groundwire ship: $COMET

  pier (the ship; all its state):  $GW_PIER
  ship log:                        $GW_LOG
  web:                             http://127.0.0.1:$HTTP_PORT

  status:  $GW_DIR/boot.sh --status --comet '$COMET'
  stop:    $GW_DIR/boot.sh --stop   --comet '$COMET'
  start:   $GW_DIR/boot.sh --comet '$COMET'$feedline

The ship runs detached with a supervisor that restarts it if it crashes.
It does NOT survive a reboot of this machine: run the start line above.
Stopping is always safe; the pier holds everything.

Identity custody: the proof.json + feed files (see $GW_DIR/var/mint/).
Back those up; everything else here is replaceable.
EOF
  info ""
  info "the above is saved in $GW_DIR/README"
}

case "$MODE" in
  status) cmd_status ;;
  stop)   cmd_stop ;;
  mint)   cmd_mint ;;
  *)      cmd_install ;;
esac
