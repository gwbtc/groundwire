#!/usr/bin/env python3
"""
gw-onboard.py — Groundwire comet onboarding script

Generates a random @q master ticket, derives a taproot address,
watches Bitcoin Core for funding, mines a comet with the correct tweak,
boots it, and directs the user to the spv-wallet interface.
"""

import argparse
import base64
import contextlib
import dataclasses
import ipaddress
import json
import os
import platform
import re
import secrets
import shutil
import socket
import subprocess
import sys
import time
import webbrowser

# ---------------------------------------------------------------------------
# CA bundle fix for frozen builds — certifi's path breaks when bundled, and
# the system fallback assumes Debian's path. Search common locations instead.
# Must run BEFORE importing requests, which caches the cert path at import.
# ---------------------------------------------------------------------------
if getattr(sys, "frozen", False) and not os.environ.get("REQUESTS_CA_BUNDLE"):
    _CA_CANDIDATES = [
        "/etc/ssl/certs/ca-certificates.crt",  # Debian/Ubuntu
        "/etc/pki/tls/certs/ca-bundle.crt",  # RHEL/Fedora/OpenMandriva
        "/etc/ssl/ca-bundle.pem",  # OpenSUSE
        "/etc/ssl/cert.pem",  # macOS / Alpine
    ]
    for _ca in _CA_CANDIDATES:
        if os.path.isfile(_ca):
            os.environ["REQUESTS_CA_BUNDLE"] = _ca
            break

import hashlib

import requests  # must come after CA bundle fix above
import nacl.bindings
import click
from embit import bip32, bip39, ec, psbt, script
from embit.networks import NETWORKS
from embit.transaction import Transaction, TransactionInput, TransactionOutput, Witness

# ---------------------------------------------------------------------------
# Configuration defaults (overridable via CLI)
# ---------------------------------------------------------------------------

RPC_URL = "https://alpha.groundwire.dev/rpc"
RPC_USER = "mainnetrpcuser"
RPC_PASS = "fc3d36ce83e15484e75a658b2a9a8a90a66f4cb017ace74c8631fe082b93adbf"


# --- zig output directories -------------------------------------------------
#
# `zig build` installs each binary into a directory named for the *resolved*
# target triple (`build.zig`: `dest_dir = .{ .override = .{ .custom =
# target_query.zigTriple(..) } }`).  That triple is not simply
# "<arch>-<os>-none".  Both vere/build.zig and comet-miner/build.zig rewrite a
# native Linux build to musl:
#
#     if (t.os.tag == .linux and target.query.isNative() and !asan and !ubsan)
#         b.resolveTargetQuery(.{ .abi = .musl })
#
# so a plain `zig build` on Linux lands in `zig-out/x86_64-linux-musl/`
# (or `aarch64-linux-musl` on ARM).  Only an `-Dasan`/`-Dubsan` build escapes
# the rewrite and keeps the native glibc ABI, landing in `x86_64-linux-gnu`.
# macOS has no rewrite and resolves to `-none`, e.g. `aarch64-macos-none`.
#
# `x86_64-linux-none` is a triple zig NEVER emits — the `supported_targets`
# whitelist in both build.zig files only admits musl and gnu for Linux — so it
# must never be a default.  We therefore probe the candidate triples on disk,
# most-likely first, and remember the whole list so a miss can name it.


def _zig_target_candidates(machine: str = "", system: str = "") -> list[str]:
    """Zig target triples this platform may have built into, most likely first."""
    machine = (machine or platform.machine()).lower()
    system = (system or platform.system()).lower()
    arch = {"x86_64": "x86_64", "amd64": "x86_64", "aarch64": "aarch64", "arm64": "aarch64"}.get(
        machine, machine
    )
    if system == "darwin":
        return [f"{arch}-macos-none"]
    if system == "linux":
        # musl is what a plain `zig build` produces; gnu only for asan/ubsan
        # builds and explicit `-Dtarget=…-linux-gnu` cross builds.
        return [f"{arch}-linux-musl", f"{arch}-linux-gnu"]
    if system == "windows":
        return [f"{arch}-windows-gnu"]
    return [f"{arch}-{system}-none"]


def _detect_zig_target() -> str:
    """The single most likely zig target triple for this platform."""
    return _zig_target_candidates()[0]


# resolved default path -> every path we looked at, for the not-found message
_ZIG_SEARCHED: dict[str, list[str]] = {}


def _zig_out_bin(tree: str, binary: str) -> str:
    """Resolve `<tree>/zig-out/<triple>/<binary>` against the triples zig emits.

    Returns the first candidate that exists.  If none do, returns the most
    likely candidate and records the full search list, so the caller's
    `os.path.isfile` check can report what was actually looked for rather than
    a single invented path.
    """
    searched = [f"{tree}/zig-out/{triple}/{binary}" for triple in _zig_target_candidates()]
    for path in searched:
        if os.path.isfile(path):
            return path
    _ZIG_SEARCHED[searched[0]] = searched
    return searched[0]


def _not_found_hint(path: str) -> str:
    """Extra guidance for a default binary path that resolved to nothing."""
    searched = _ZIG_SEARCHED.get(path)
    if not searched or len(searched) == 1:
        return ""
    others = "\n".join(f"    {p}" for p in searched[1:])
    return f"  Also looked for it at:\n{others}\n"


_ZIG_TARGET = _detect_zig_target()
# In frozen builds, binaries live alongside the executable in the same directory.
# In dev, they're in the zig build output trees.
if getattr(sys, "frozen", False):
    # PyInstaller build (gw-onboard's world): binaries beside the executable.
    _BIN_DIR = os.path.dirname(os.path.abspath(sys.executable))
    COMET_MINER_BIN = os.path.join(_BIN_DIR, "comet_miner")
    VERE_BIN = os.path.join(_BIN_DIR, "gw-vere")
    GW_PILL = os.path.join(_BIN_DIR, "gw-base.pill")
elif os.environ.get("GROUNDWIRE_HOME"):
    # Release install: the `causeway` launcher exports GROUNDWIRE_HOME as the
    # directory it lives in, where the tarball put bin/comet_miner, bin/gw-vere
    # and pills/gw-base.pill.  This branch exists because the venv the launcher
    # builds is NEITHER frozen NOR a dev checkout: sys.frozen is False and the
    # zig-out trees below are relative to the CWD, so a shipped Causeway used
    # to go looking for a build tree in whatever directory the user happened
    # to be standing in -- and found one only on a developer's machine.
    _GW_HOME = os.environ["GROUNDWIRE_HOME"]
    COMET_MINER_BIN = os.path.join(_GW_HOME, "bin", "comet_miner")
    VERE_BIN = os.path.join(_GW_HOME, "bin", "gw-vere")
    GW_PILL = os.path.join(_GW_HOME, "pills", "gw-base.pill")
else:
    # Dev checkout: zig build outputs, relative to the repo root.
    COMET_MINER_BIN = _zig_out_bin("./comet-miner", "comet_miner")
    VERE_BIN = _zig_out_bin("./vere", "urbit")
    GW_PILL = "./gw-base.pill"
REQUIRED_SATS = 1_000
POLL_INTERVAL = 15  # seconds between UTXO scans
MEMPOOL_TX_URL = "https://mempool.space/tx"
MEMPOOL_API_URL = "https://mempool.space/api"

FAUCET_URL = "https://alpha.groundwire.dev/faucet"
FAUCET_API_KEY = "e8ec9ac94a4f5396da27091f3b7f8099cf27856b5b2921aa20fb9fd59b967ebe"

SPONSOR_URL = "http://143.198.70.9:8081"
SPONSOR_SHIP = "~daplyd"  # star — comet mines under this
ESCAPE_SPONSOR = "~linluc-palnus-barpub-dalweg--miptyp-molfer-pitren-daplyd"  # networking sponsor for escape
BLOCK_CONFIRMATIONS = 2


# =========================================================================
#  Terminal helpers
# =========================================================================


def tx_link(txid: str) -> str:
    """Return an OSC 8 clickable hyperlink for a txid pointing to mempool.space."""
    url = f"{MEMPOOL_TX_URL}/{txid}"
    return f"\033]8;;{url}\033\\\033[4;36m{url}\033[0m\033]8;;\033\\"


# --- prompting ------------------------------------------------------------
#
# EVERY interactive read in Causeway goes through prompt().  Causeway is
# routinely driven over SSH and from scripts, and a prompt that cannot be
# answered is a FATAL condition, never something to retry: the old
# `except EOFError: ... continue` loops turned a headless run into a pinned
# core writing ~110 MB/min of prompt spam until the disk filled
# (ops/doc/live-tests/PHASE67-RESULTS.md 7.1).  So:
#
#   * no TTY on stdin              -> abort, naming the flag that supplies it
#   * EOF (stdin closed / at end)  -> abort, naming the flag
#   * Ctrl-C                       -> abort 130
#
# Anything reachable from a prompt therefore also has a CLI flag; `flag` is
# the string we tell the operator to pass instead.


PROMPT_EXIT_CODE = 2       # a prompt could not be answered
INTERRUPT_EXIT_CODE = 130  # conventional 128 + SIGINT


def _stdin_is_tty() -> bool:
    """True iff stdin is an interactive terminal we can prompt on."""
    try:
        return bool(sys.stdin is not None and sys.stdin.isatty())
    except (AttributeError, ValueError):  # detached / closed stdin
        return False


def _abort_prompt(what: str, flag: str | None) -> None:
    """Fail fast on an unanswerable prompt. Never returns."""
    print(f"causeway: cannot read {what}: no interactive terminal on stdin",
          file=sys.stderr)
    if flag:
        print(f"causeway: pass {flag} to supply it non-interactively",
              file=sys.stderr)
    else:
        print("causeway: this prompt has no non-interactive equivalent — "
              "run the command on a terminal", file=sys.stderr)
    raise SystemExit(PROMPT_EXIT_CODE)


def prompt(message: str, *, what: str, flag: str | None = None) -> str:
    """Read one line from the operator, or abort with a non-zero exit.

    `what` names the thing being asked for (used in the error), `flag` is the
    CLI option that supplies the same answer headlessly."""
    if not _stdin_is_tty():
        _abort_prompt(what, flag)
    try:
        return input(message)
    except EOFError:
        print(file=sys.stderr)
        _abort_prompt(what, flag)
    except KeyboardInterrupt:
        print(file=sys.stderr)
        print(f"causeway: interrupted while reading {what}", file=sys.stderr)
        raise SystemExit(INTERRUPT_EXIT_CODE)
    raise AssertionError("unreachable")  # pragma: no cover


def normalize_ticket(raw: str) -> str:
    """Normalize user input for ticket comparison: strip whitespace, ensure leading ~."""
    t = raw.strip()
    if not t.startswith("~"):
        t = "~" + t
    return t


def confirm_master_ticket(ticket: str, *, assume_saved: bool = False) -> None:
    """Require the user to re-enter their master ticket before proceeding.

    `assume_saved` (the --assume-saved flag) skips the read-back for scripted
    runs; the ticket is still printed, so the caller has to capture it."""
    if assume_saved:
        print("  --assume-saved: skipping the master-ticket read-back.")
        print("  You are responsible for having captured it from this output.")
        return
    print("  Please re-enter your master ticket to confirm you saved it:")
    while True:
        entry = prompt("  > ", what="the master-ticket confirmation",
                       flag="--assume-saved")
        if normalize_ticket(entry) == ticket:
            print("  Confirmed!")
            print()
            return
        print()
        print("  That doesn't match. Your master ticket is:")
        print(f"  {ticket}")
        print()
        print("  Please re-enter it exactly:")


RAW_TX_PREFIX = "rawtx:"


def _looks_like_raw_tx(raw: bytes) -> bool:
    """A finalized bitcoin transaction, not a PSBT: version LE32 (1 or 2),
    then either the segwit marker 00 01 or an input count."""
    if len(raw) < 60:
        return False
    ver = int.from_bytes(raw[:4], "little")
    return ver in (1, 2) and (raw[4:6] == b"\x00\x01" or 1 <= raw[4] <= 0x10)


def signed_input_to_base64(data: bytes) -> str | None:
    """Like psbt_bytes_to_base64, but ALSO accepts a fully-signed raw
    transaction (what Sparrow's Save Transaction emits once every input is
    signed) and returns it as RAW_TX_PREFIX + hex.

    A signed tx is strictly better than a signed PSBT for us -- it is the
    thing _extract_tx_from_psbt exists to produce -- so refusing it was
    silly.  The txid comparison against what we built still runs on it."""
    b64 = psbt_bytes_to_base64(data)
    if b64:
        return b64
    if not data:
        return None
    raw = data
    if not _looks_like_raw_tx(raw):
        txt = "".join(data.decode("ascii", "ignore").split())
        if txt and all(c in "0123456789abcdefABCDEF" for c in txt) and len(txt) % 2 == 0:
            try:
                raw = bytes.fromhex(txt)
            except ValueError:
                return None
        else:
            return None
    if _looks_like_raw_tx(raw):
        try:
            Transaction.parse(raw)          # embit must agree it is a tx
        except Exception:  # noqa: BLE001
            return None
        return RAW_TX_PREFIX + raw.hex()
    return None


def psbt_bytes_to_base64(data: bytes) -> str | None:
    """Normalize a PSBT in ANY of the forms a wallet emits into clean base64,
    or None if it is not a PSBT at all.

    Wallets disagree: Sparrow's Save Transaction writes raw binary
    (`psbt\xff...`) for .psbt, base64 text for "as text", and hex in some
    export paths; a clipboard paste arrives with newlines and stray spaces.
    The TUI once emptied its own input box on the binary case and then
    reported "Invalid PSBT magic" -- embit's message for an EMPTY string --
    which named the symptom and hid the cause.  One normalizer, and the
    caller says which form it saw."""
    import base64 as _b64
    if not data:
        return None
    if data[:5] == b"psbt\xff":
        return _b64.b64encode(data).decode()
    txt = data.decode("ascii", "ignore")
    compact = "".join(txt.split())
    if not compact:
        return None
    # hex?
    if all(c in "0123456789abcdefABCDEF" for c in compact) and len(compact) % 2 == 0:
        try:
            raw = bytes.fromhex(compact)
            if raw[:5] == b"psbt\xff":
                return _b64.b64encode(raw).decode()
        except ValueError:
            pass
    # base64?
    try:
        raw = _b64.b64decode(compact + "=" * (-len(compact) % 4), validate=False)
        if raw[:5] == b"psbt\xff":
            return _b64.b64encode(raw).decode()
    except Exception:  # noqa: BLE001
        pass
    return None


def paste_from_clipboard() -> str | None:
    """Read the system clipboard, or None.  Mirror of copy_to_clipboard: a
    full-screen TUI owns the mouse, so a signed PSBT is as unpasteable as
    the unsigned one was uncopyable."""
    system = platform.system()
    cmds = []
    if system == "Darwin":
        cmds.append(["pbpaste"])
    elif system == "Linux":
        if "microsoft" in platform.uname().release.lower():
            cmds.append(["powershell.exe", "-command", "Get-Clipboard"])
        if os.environ.get("WAYLAND_DISPLAY"):
            cmds.append(["wl-paste", "--no-newline"])
        cmds.append(["xclip", "-selection", "clipboard", "-o"])
        cmds.append(["xsel", "--clipboard", "--output"])
    for cmd in cmds:
        if shutil.which(cmd[0]):
            try:
                r = subprocess.run(cmd, capture_output=True, text=True, check=True, timeout=5)
                return r.stdout
            except (subprocess.SubprocessError, OSError):
                continue
    return None


def copy_to_clipboard(text: str) -> bool:
    """Copy text to the system clipboard. Returns True on success."""
    system = platform.system()
    cmds_to_try = []
    if system == "Darwin":
        cmds_to_try.append(["pbcopy"])
    elif system == "Linux":
        # Check for WSL
        if "microsoft" in platform.uname().release.lower():
            cmds_to_try.append(["clip.exe"])
        # Wayland
        if os.environ.get("WAYLAND_DISPLAY"):
            cmds_to_try.append(["wl-copy"])
        # X11 fallbacks
        cmds_to_try.append(["xclip", "-selection", "clipboard"])
        cmds_to_try.append(["xsel", "--clipboard", "--input"])

    for cmd in cmds_to_try:
        if shutil.which(cmd[0]):
            try:
                subprocess.run(cmd, input=text, text=True, check=True, timeout=5)
                return True
            except (subprocess.SubprocessError, OSError):
                continue
    return False


# =========================================================================
#  @q encoding — Urbit phonemic syllable tables
# =========================================================================

# Prefix syllables (256 entries) — used for even-indexed bytes in pairs
# Source: vere/pkg/noun/jets/c/po.c  u3_po_to_prefix()
# fmt: off
PREFIXES = [
    "doz", "mar", "bin", "wan", "sam", "lit", "sig", "hid",  # 0-7
    "fid", "lis", "sog", "dir", "wac", "sab", "wis", "sib",  # 8-15
    "rig", "sol", "dop", "mod", "fog", "lid", "hop", "dar",  # 16-23
    "dor", "lor", "hod", "fol", "rin", "tog", "sil", "mir",  # 24-31
    "hol", "pas", "lac", "rov", "liv", "dal", "sat", "lib",  # 32-39
    "tab", "han", "tic", "pid", "tor", "bol", "fos", "dot",  # 40-47
    "los", "dil", "for", "pil", "ram", "tir", "win", "tad",  # 48-55
    "bic", "dif", "roc", "wid", "bis", "das", "mid", "lop",  # 56-63
    "ril", "nar", "dap", "mol", "san", "loc", "nov", "sit",  # 64-71
    "nid", "tip", "sic", "rop", "wit", "nat", "pan", "min",  # 72-79
    "rit", "pod", "mot", "tam", "tol", "sav", "pos", "nap",  # 80-87
    "nop", "som", "fin", "fon", "ban", "mor", "wor", "sip",  # 88-95
    "ron", "nor", "bot", "wic", "soc", "wat", "dol", "mag",  # 96-103
    "pic", "dav", "bid", "bal", "tim", "tas", "mal", "lig",  # 104-111
    "siv", "tag", "pad", "sal", "div", "dac", "tan", "sid",  # 112-119
    "fab", "tar", "mon", "ran", "nis", "wol", "mis", "pal",  # 120-127
    "las", "dis", "map", "rab", "tob", "rol", "lat", "lon",  # 128-135
    "nod", "nav", "fig", "nom", "nib", "pag", "sop", "ral",  # 136-143
    "bil", "had", "doc", "rid", "moc", "pac", "rav", "rip",  # 144-151
    "fal", "tod", "til", "tin", "hap", "mic", "fan", "pat",  # 152-159
    "tac", "lab", "mog", "sim", "son", "pin", "lom", "ric",  # 160-167
    "tap", "fir", "has", "bos", "bat", "poc", "hac", "tid",  # 168-175
    "hav", "sap", "lin", "dib", "hos", "dab", "bit", "bar",  # 176-183
    "rac", "par", "lod", "dos", "bor", "toc", "hil", "mac",  # 184-191
    "tom", "dig", "fil", "fas", "mit", "hob", "har", "mig",  # 192-199
    "hin", "rad", "mas", "hal", "rag", "lag", "fad", "top",  # 200-207
    "mop", "hab", "nil", "nos", "mil", "fop", "fam", "dat",  # 208-215
    "nol", "din", "hat", "nac", "ris", "fot", "rib", "hoc",  # 216-223
    "nim", "lar", "fit", "wal", "rap", "sar", "nal", "mos",  # 224-231
    "lan", "don", "dan", "lad", "dov", "riv", "bac", "pol",  # 232-239
    "lap", "tal", "pit", "nam", "bon", "ros", "ton", "fod",  # 240-247
    "pon", "sov", "noc", "sor", "lav", "mat", "mip", "fip",  # 248-255
]

# Suffix syllables (256 entries) — used for odd-indexed bytes in pairs
# Source: vere/pkg/noun/jets/c/po.c  u3_po_to_suffix()
SUFFIXES = [
    "zod", "nec", "bud", "wes", "sev", "per", "sut", "let",  # 0-7
    "ful", "pen", "syt", "dur", "wep", "ser", "wyl", "sun",  # 8-15
    "ryp", "syx", "dyr", "nup", "heb", "peg", "lup", "dep",  # 16-23
    "dys", "put", "lug", "hec", "ryt", "tyv", "syd", "nex",  # 24-31
    "lun", "mep", "lut", "sep", "pes", "del", "sul", "ped",  # 32-39
    "tem", "led", "tul", "met", "wen", "byn", "hex", "feb",  # 40-47
    "pyl", "dul", "het", "mev", "rut", "tyl", "wyd", "tep",  # 48-55
    "bes", "dex", "sef", "wyc", "bur", "der", "nep", "pur",  # 56-63
    "rys", "reb", "den", "nut", "sub", "pet", "rul", "syn",  # 64-71
    "reg", "tyd", "sup", "sem", "wyn", "rec", "meg", "net",  # 72-79
    "sec", "mul", "nym", "tev", "web", "sum", "mut", "nyx",  # 80-87
    "rex", "teb", "fus", "hep", "ben", "mus", "wyx", "sym",  # 88-95
    "sel", "ruc", "dec", "wex", "syr", "wet", "dyl", "myn",  # 96-103
    "mes", "det", "bet", "bel", "tux", "tug", "myr", "pel",  # 104-111
    "syp", "ter", "meb", "set", "dut", "deg", "tex", "sur",  # 112-119
    "fel", "tud", "nux", "rux", "ren", "wyt", "nub", "med",  # 120-127
    "lyt", "dus", "neb", "rum", "tyn", "seg", "lyx", "pun",  # 128-135
    "res", "red", "fun", "rev", "ref", "mec", "ted", "rus",  # 136-143
    "bex", "leb", "dux", "ryn", "num", "pyx", "ryg", "ryx",  # 144-151
    "fep", "tyr", "tus", "tyc", "leg", "nem", "fer", "mer",  # 152-159
    "ten", "lus", "nus", "syl", "tec", "mex", "pub", "rym",  # 160-167
    "tuc", "fyl", "lep", "deb", "ber", "mug", "hut", "tun",  # 168-175
    "byl", "sud", "pem", "dev", "lur", "def", "bus", "bep",  # 176-183
    "run", "mel", "pex", "dyt", "byt", "typ", "lev", "myl",  # 184-191
    "wed", "duc", "fur", "fex", "nul", "luc", "len", "ner",  # 192-199
    "lex", "rup", "ned", "lec", "ryd", "lyd", "fen", "wel",  # 200-207
    "nyd", "hus", "rel", "rud", "nes", "hes", "fet", "des",  # 208-215
    "ret", "dun", "ler", "nyr", "seb", "hul", "ryl", "lud",  # 216-223
    "rem", "lys", "fyn", "wer", "ryc", "sug", "nys", "nyl",  # 224-231
    "lyn", "dyn", "dem", "lux", "fed", "sed", "bec", "mun",  # 232-239
    "lyr", "tes", "mud", "nyt", "byr", "sen", "weg", "fyr",  # 240-247
    "mur", "tel", "rep", "teg", "pec", "nel", "nev", "fes",  # 248-255
]
# fmt: on


def encode_q(value: int) -> str:
    """Encode an integer as an Urbit @q phonemic string."""
    if value == 0:
        return "~zod"

    # Get the byte representation (little-endian)
    n_bytes = (value.bit_length() + 7) // 8
    raw = value.to_bytes(n_bytes, "little")

    syllables = []
    i = 0
    while i < len(raw):
        if i + 1 < len(raw):
            # Pair: prefix + suffix
            syllables.append(PREFIXES[raw[i]] + SUFFIXES[raw[i + 1]])
            i += 2
        else:
            # Odd trailing byte: suffix only
            syllables.append(SUFFIXES[raw[i]])
            i += 1

    # Group into blocks of 4 syllable-pairs separated by --
    # Individual syllables separated by -
    parts = []
    for idx, syl in enumerate(syllables):
        if idx > 0 and idx % 4 == 0:
            parts.append("--")
        elif idx > 0:
            parts.append("-")
        parts.append(syl)

    return "~" + "".join(parts)


def decode_q(q_str: str) -> int:
    """Decode an Urbit @q phonemic string back to an integer."""
    q = q_str.lstrip("~").replace("--", "-")
    syllable_strs = q.split("-")

    raw_bytes = []
    for syl in syllable_strs:
        if len(syl) == 6:
            # Paired syllable: prefix (3) + suffix (3)
            pre, suf = syl[:3], syl[3:]
            raw_bytes.append(PREFIXES.index(pre))
            raw_bytes.append(SUFFIXES.index(suf))
        elif len(syl) == 3:
            # Single suffix syllable
            raw_bytes.append(SUFFIXES.index(syl))
        else:
            raise ValueError(f"Invalid @q syllable: {syl!r}")

    return int.from_bytes(bytes(raw_bytes), "little")


# =========================================================================
#  Bitcoin Core RPC
# =========================================================================


def rpc_call(
    method: str, params=None, rpc_url=RPC_URL, rpc_user=RPC_USER, rpc_pass=RPC_PASS, timeout=60
):
    """Make a JSON-RPC call to Bitcoin Core."""
    payload = {"jsonrpc": "2.0", "id": "gw-onboard", "method": method, "params": params or []}
    resp = requests.post(rpc_url, json=payload, auth=(rpc_user, rpc_pass), timeout=timeout)
    data = resp.json()
    if data.get("error"):
        raise RuntimeError(f"RPC error: {data['error']}")
    return data.get("result")


def request_faucet(address: str, invite: str | None = None) -> str | None:
    """Request sats from the faucet. Returns txid on success, None on failure."""
    try:
        payload = {"address": address, "api_key": FAUCET_API_KEY}
        if invite:
            payload["invite_code"] = invite
        resp = requests.post(FAUCET_URL, json=payload, timeout=30)
        if resp.ok:
            try:
                data = json.loads(resp.text)
                txid = data.get("txid")
                sats = data.get("amount_sats", "?")
                print(f"  Faucet sent {sats} sats.")
                if txid:
                    print(f"  Transaction: {tx_link(txid)}")
                return txid
            except (json.JSONDecodeError, KeyError):
                print(f"  Faucet: {resp.text.strip()}")
                return ""
        else:
            # Try to extract a human-readable message from the faucet error response.
            msg = resp.text.strip()
            try:
                err_data = json.loads(resp.text)
                msg = err_data.get("error") or err_data.get("message") or msg
            except (json.JSONDecodeError, AttributeError):
                pass
            print(f"  Faucet error: {msg}")
            return None
    except Exception as e:
        print(f"  Could not reach faucet: {e}")
        return None


def scan_for_utxo(address: str, *, mempool_base: str = MEMPOOL_API_URL, **_rpc_kwargs) -> dict | None:
    """Check the mempool API for a confirmed UTXO at `address`.

    Takes the base URL like every other network call here.  It was hardcoded
    to mempool.space, which quietly ignored --mempool-base for exactly one
    request -- the funding poll -- making the flow untestable against a stub
    and unusable against any other backend.
    """
    try:
        resp = requests.get(f"{mempool_base}/address/{address}/utxo", timeout=15)
        if not resp.ok:
            return None
        utxos = resp.json()
        for utxo in utxos:
            if not utxo.get("status", {}).get("confirmed", False):
                continue
            if utxo["value"] >= REQUIRED_SATS:
                return {
                    "txid": utxo["txid"],
                    "vout": utxo["vout"],
                    "_sats": utxo["value"],
                }
    except Exception:
        return None
    return None


def wait_for_funding(address: str, poll_interval: int = POLL_INTERVAL, **rpc_kwargs) -> dict:
    """Block until a confirmed UTXO with >= REQUIRED_SATS appears at `address`."""
    print(f"\nWaiting for funding transaction to confirm (checking every {poll_interval}s)...")
    start = time.monotonic()
    while True:
        utxo = scan_for_utxo(address, **rpc_kwargs)
        if utxo:
            return utxo
        elapsed = int(time.monotonic() - start)
        print(f"  No confirmed funding yet ({elapsed}s elapsed)   ", end="\r")
        time.sleep(poll_interval)


# =========================================================================
#  Tweak construction — Hoon expression for comet-miner's --tweak flag
# =========================================================================


def format_hoon_ux(hex_str: str) -> str:
    """Format a hex string as Hoon @ux: 0xdead.beef.cafe.babe"""
    h = hex_str.lstrip("0") or "0"
    # Group into chunks of 4 from the right, dot-separated
    chunks = []
    while len(h) > 4:
        chunks.append(h[-4:])
        h = h[:-4]
    chunks.append(h)
    return "0x" + ".".join(reversed(chunks))


# =========================================================================
#  kelvin-9 (%gw-btc, OP_RETURN) confidential-comets protocol encoders.
#
#  Bit-exact port of gw-btc/lib/gw-btc-pass.hoon +
#  sur/self-attestation.hoon.  All values are pinned by the shared golden
#  vectors (vectors/gw-kelvin-9.json) and the Hoon golden test
#  (gw-btc/tests/lib/gw-btc-pass.hoon).  See the module docstring in
#  that lib for the design; the two amending specs are
#  ops/doc/opret-revision/{01-spec-revision,04-decisions-addendum}.md.
#
#  Byte conventions, pinned by the vectors: H_tag is the BIP-340 tagged
#  hash over BIG-endian byte strings; a jammed noun enters a hash message
#  (or an OP_RETURN payload) as its minimal LITTLE-endian byte dump — the
#  ordinary serialization of a jam.  All commitment hashes are 32 bytes.
# =========================================================================


PKI_DOM = "gw-btc"   # the %gw-btc PKI domain tag; consensus-critical (in dat)
KELVIN = 9           # protocol version, plaintext in dat and the OP_RETURN


def jam_bytes(noun) -> bytes:
    """A jammed noun as its minimal little-endian byte dump.

    This is how a jam enters a hash preimage or an OP_RETURN payload, and
    it is exactly +jam-octs:gw-btc-pass: that arm's (rev 3 wid) reverses
    the jam atom's bytes so that, read back big-endian by the tagged-hash,
    they land in this original little-endian order.
    """
    a = hoon_jam(noun)
    return a.to_bytes((a.bit_length() + 7) // 8, "little") if a else b""


def _unit(value):
    """(unit x): the null unit ~ is 0; a full unit [~ v] is the cell [0 v]."""
    return 0 if value is None else (0, value)


# --- `dat` — the immutable name-committing tweak (original spec §2.1) --------
#
#     dat = (can 0 (mat %gw-btc) (mat 9) (mat (jam spawn-sont)) ~)
#
# Three self-delimiting mat items: the domain tag, the kelvin, and the spawn
# satpoint's canonical jam -- Jake and Christian's original design
# ((cat 0 (mat dom) <spawn satpoint>) in sur/stealth.hoon), plus the kelvin.
# The satpoint is PLAINTEXT: any holder of a pass reads it, and the verifier
# requires it to equal entry 0's spawn-opening.  Because dat is hashed into
# the signing key, the comet's @p commits to it forever.  Ames reads only the
# leading `mat %gw-btc` to route the pass to the %gw-btc agent.
#
# A hiding commitment (d = H_tag(jam(sont) || blind)) sat here from
# 2026-08-03 to 2026-08-18.  Removed: it protected nobody -- the pass and the
# attestation are one object, so every pass-holder held the opening too --
# and it cost a 32-byte secret to lose.  Byte-identical to
# +make-dat:gw-btc-pass; pinned by the golden vector.


def spawn_sont_noun(txid_hex: str, vout: int, off: int = 0) -> tuple:
    """The $sont:ord spawn satpoint as a noun: [txid=@ux vout=@ud off=@ud]."""
    return (int(txid_hex, 16), (vout, off))


def build_dat_atom(txid_hex: str, vout: int, off: int, dom: str = PKI_DOM) -> int:
    """The kelvin-9 dat as a Hoon atom (integer):

        dat = (can 0 (mat dom) (mat 9) (mat (jam spawn-sont)) ~)

    Depends on nothing but the satpoint, the domain and the kelvin -- so it
    is fixed the moment the funding UTXO is chosen, and identical across
    every candidate seed the miner tries."""
    w = BitWriter()
    w.write_mat(int.from_bytes(dom.encode("ascii"), "little"))
    w.write_mat(KELVIN)
    w.write_mat(hoon_jam(spawn_sont_noun(txid_hex, vout, off)))
    return w.to_int()


def build_dat_bytes(txid_hex: str, vout: int, off: int, dom: str = PKI_DOM) -> bytes:
    """The kelvin-9 dat as minimal LE atom bytes."""
    a = build_dat_atom(txid_hex, vout, off, dom)
    return a.to_bytes((a.bit_length() + 7) // 8, "little")


def parse_dat_atom(dat: int) -> tuple[str, int, tuple]:
    """(dom, kelvin, spawn-sont-noun) read straight out of a dat atom -- the
    Python twin of +parse-dat:gw-btc-pass.  Rejects trailing bits, exactly
    as the Hoon does (`?> =((met 0 dat) (add pos p.spn))`)."""
    w1, dom_atom = _hoon_rub(0, dat)
    w2, kel = _hoon_rub(w1, dat)
    w3, spawn_jam = _hoon_rub(w1 + w2, dat)
    if w1 + w2 + w3 != dat.bit_length():
        raise ValueError("dat has trailing data after the spawn satpoint")
    dom = dom_atom.to_bytes((dom_atom.bit_length() + 7) // 8, "little").decode("ascii")
    spawn = hoon_cue(spawn_jam)
    # Canonical, or refused -- the twin of +parse-dat's `?> =(dat (make-dat
    # spawn))`.  mat length-prefixes whatever it is given and cue ignores
    # trailing bits inside that atom, so a padded third item cues to the
    # SAME satpoint but is a DIFFERENT dat and therefore a different @p:
    # many names per sat.  Only +make-dat's own encoding is accepted.
    txid, (vout, off) = spawn
    # Re-encode with the domain AND kelvin the dat itself carries: a foreign
    # kelvin must still parse (the caller decides silence from `kel`), it
    # just must not be malleable.
    w = BitWriter()
    w.write_mat(dom_atom); w.write_mat(kel); w.write_mat(hoon_jam(spawn))
    if dat != w.to_int():
        raise ValueError("dat is not the canonical encoding of its satpoint")
    return dom, kel, spawn


def make_dat_expr(txid_hex: str, vout: int, off: int, dom: str = PKI_DOM) -> str:
    """Hoon expression that reconstructs the concrete kelvin-9 dat, for
    comet-miner's --tweak flag.  Every term is a literal, so the miner's
    +wish evaluates exactly what +make-dat:gw-btc-pass would build."""
    txid_ux = format_hoon_ux(txid_hex)
    return (f"(can 0 (mat %{dom}) (mat {KELVIN}) "
            f"(mat (jam [txid={txid_ux} vout={vout} off={off}])) ~)")


# --- state snapshot + on-chain state commitment (spec §3) -----------------
#
#     snapshot = [life rift key sponsor=(unit @p) fief=(unit fief)]
#     c        = H_tag("gw/state-commit", (jam snapshot))
#     leaf     = OP_RETURN PUSH2 "gw" PUSH32 <c>            (37 bytes)
#     root     = H_TapLeaf(0xc0 || compact_size(leaf) || leaf)   (single leaf)
#     Q        = lift_x(x(P)) + H_TapTweak(x(P) || root)*G;  output key = x(Q)
#
# The sat-carrying output's scriptPubKey is OP_1 PUSH32 Q.  A chain
# observer sees only Q, indistinguishable from any P2TR key; the snapshot
# is revealed only to attestation verifiers (or on-chain, when published).


# --- $fief (sur/urb.hoon) -------------------------------------------------
#
#     +$  fief  $%  [%turf p=(list turf) q=@udE]
#                   [%if p=@ifF q=@udE]
#                   [%is p=@isH q=@udE]
#               ==
#
# The fief rides the snapshot (decisions-addendum 2), so it enters the
# state commitment: state-key is taken over the JAMMED snapshot, and a
# dropped fief silently produces a different output key.  Canonical
# in-process form is the NOUN itself — [tag [p q]] — so it can go straight
# into the jam.  A JSON round-trip (proof.json stores the snapshot verbatim)
# turns those tuples into lists, so normalize on the way in.

FIEF_TAGS = {n: int.from_bytes(n.encode(), "little") for n in ("turf", "if", "is")}
_FIEF_TAG_NAMES = {v: k for k, v in FIEF_TAGS.items()}


def _noun_from_json(n):
    """Nested JSON lists back to noun tuples (atoms stay ints). A 2-element
    list is unambiguously a cell, so this is lossless."""
    if isinstance(n, (tuple, list)):
        if len(n) != 2:
            raise ValueError(f"not a cell: {n!r}")
        return (_noun_from_json(n[0]), _noun_from_json(n[1]))
    return int(n)


def parse_fief_arg(s):
    """`IP:PORT` -> the $fief noun [%if [ip port]], or None if absent.

    The only way a comet acquires routing after it is minted. Until this
    existed, `--fief` was a spawn-only flag and every state-update path
    hardcoded carry-forward, so a comet minted without a fief could never
    get one — and since a comet with neither fief nor sponsor is
    unreachable by design, and cannot learn a route from packets it
    receives (ames gates the heard-lane update on the sender not being its
    own sponsor), that made it permanently unreachable with no remedy.
    The docs called publication-with-a-fief "the escape hatch"; nothing
    could build it.

    Byte-identical to what `ops/gwmint.py` emits at spawn: the tag is the
    little-endian cord %if, the address a big-endian 4-octet atom.
    """
    if s is None:
        return None
    if ":" not in s:
        raise ValueError(f"fief must be IP:PORT, got {s!r}")
    ip_s, port_s = s.rsplit(":", 1)
    octets = ip_s.split(".")
    if len(octets) != 4:
        raise ValueError(f"fief address must be dotted-quad IPv4, got {ip_s!r}")
    try:
        vals = [int(o) for o in octets]
        port = int(port_s)
    except ValueError:
        raise ValueError(f"fief must be IP:PORT with integer parts, got {s!r}")
    if any(v < 0 or v > 255 for v in vals):
        raise ValueError(f"fief octet out of range in {ip_s!r}")
    if port < 1 or port > 65535:
        raise ValueError(f"fief port out of range: {port}")
    return (FIEF_TAGS["if"], (int.from_bytes(bytes(vals), "big"), port))


def fief_noun(fief):
    """Normalize a fief to its noun [tag [p q]], or None if absent.

    Accepts the noun (nested tuples) or the same shape after a JSON
    round-trip (nested lists). Raises on anything that is not a $fief."""
    if fief is None:
        return None
    n = _noun_from_json(fief)
    if not isinstance(n, tuple):
        raise ValueError(f"fief is not a [tag [p q]] noun: {fief!r}")
    tag, rest = n
    if tag not in _FIEF_TAG_NAMES:
        raise ValueError(f"unknown fief tag {tag!r} (want one of {sorted(FIEF_TAGS)})")
    if not isinstance(rest, tuple):
        raise ValueError(f"fief tail is not [p q]: {rest!r}")
    p, q = rest
    if not isinstance(q, int):
        raise ValueError(f"fief port is not an atom: {q!r}")
    if _FIEF_TAG_NAMES[tag] != "turf" and not isinstance(p, int):
        raise ValueError(f"%{_FIEF_TAG_NAMES[tag]} address is not an atom: {p!r}")
    return n


def _noun_list(n) -> list:
    """A null-terminated Hoon list noun as a python list of its elements."""
    out = []
    while n != 0:
        if not isinstance(n, tuple) or len(n) != 2:
            raise ValueError(f"improper list: {n!r}")
        out.append(n[0])
        n = n[1]
    return out


def snapshot_noun(
    life: int, rift: int, key: int, sponsor=None, fief=None
) -> tuple:
    """[life rift key sponsor=(unit @p) fief=(unit fief)] as a noun.

    sponsor is an @p integer or None; fief is None (absent) or a fief noun
    (see fief_noun, which also accepts the JSON-round-tripped form)."""
    return (life, (rift, (key, (_unit(sponsor), _unit(fief_noun(fief))))))


def snapshot_dict_to_noun(snap: dict) -> tuple:
    """Convert a {life, rift, key, sponsor, fief} dict to its snapshot noun."""
    return snapshot_noun(
        snap["life"], snap["rift"], snap["key"],
        snap.get("sponsor"), snap.get("fief"),
    )


def state_commit(snap: dict) -> bytes:
    """c = H_tag('gw/state-commit', jam(snapshot)). 32 bytes."""
    return _tagged_hash("gw/state-commit", jam_bytes(snapshot_dict_to_noun(snap)))


def state_leaf_script(c: bytes) -> bytes:
    """The unspendable commitment tapleaf script:
    OP_RETURN PUSH2 'gw' PUSH32 <c> = 6a 02 67 77 20 || c (37 bytes)."""
    return bytes([0x6A, 0x02, 0x67, 0x77, 0x20]) + c


def state_leaf_hash(script_bytes: bytes) -> bytes:
    """TapLeaf hash: H_tag('TapLeaf', 0xc0 || compact_size(len) || script).
    For the 37-byte state leaf, compact_size(37) = bytes([37])."""
    return _tapleaf_hash(0xC0, script_bytes)


def state_output_key(internal_key: bytes, snap: dict) -> bytes:
    """Q — the 32-byte x-only P2TR output key committing `snap` under the
    internal key.  `internal_key` is the 33-byte compressed key (02/03) or
    a bare 32-byte x-only key.  Single-leaf tree ⇒ merkle root = leaf hash."""
    xonly = internal_key[1:] if len(internal_key) == 33 else internal_key
    leaf_hash = state_leaf_hash(state_leaf_script(state_commit(snap)))
    q, _parity = _taproot_tweak_pubkey(xonly, leaf_hash)
    return q


# --- opening / publication / xtr (spec §5–6, sur/self-attestation) --------
#
#     spawn-opening = [spawn=sont start-height=@ud]
#     opening       = [internal-key=@ux snapshot spawn-opening=(unit ...)]
#     publication   = [pass opening]
#     custody-entry = [txid height opening=(unit opening)]
#     xtr           = (jam (list custody-entry))    oldest first


def _spawn_opening_noun(spawn: dict, start_height: int) -> tuple:
    """[spawn=sont start-height] — spawn is {txid_hex, vout, off}."""
    return (
        spawn_sont_noun(spawn["txid_hex"], spawn["vout"], spawn.get("off", 0)),
        start_height,
    )


def opening_noun(opening: dict) -> tuple:
    """[internal-key snapshot spawn-opening=(unit ...)] as a noun.

    opening = {internal_key (int), snapshot (dict), spawn_opening (None or
    {spawn, start_height})}.  `blind_opening` is accepted as a legacy key
    name for the same shape (its blind, if present, is ignored)."""
    so = opening.get("spawn_opening", opening.get("blind_opening"))
    bo_unit = 0 if so is None else _unit(
        _spawn_opening_noun(so["spawn"], so["start_height"])
    )
    return (
        opening["internal_key"],
        (snapshot_dict_to_noun(opening["snapshot"]), bo_unit),
    )


def publication_noun(pass_atom: int, opening: dict) -> tuple:
    """[pass opening] as a noun."""
    return (pass_atom, opening_noun(opening))


def pass_with_xtr(pass_atom: int, xtr: int) -> int:
    """The same suite-%c pass carrying a different xtr tail.

    Layout, exactly what +pub:ex:cric writes and +nol/+com read:

        'c'(8 bits) | ugn(256) | cry(256) | mat(dat) | xtr

    with xtr riding at its exact bit length and OMITTED entirely when 0.
    ugn, cry and dat are copied verbatim, so `fig` — the comet's @p — is
    unchanged by construction: xtr is outside the key tweak.

    This is what makes a publication the comet's WHOLE attestation packet:
    the pass in the OP_RETURN is the pass a peer receives, custody log and
    all.  Mirrors +with-xtr:gw-btc-pass; pinned by the `full-packet`
    golden vector."""
    if pass_atom & 0xFF != ord("c"):
        raise ValueError("pass_with_xtr: not a suite-C pass")
    bod = pass_atom >> 8
    ugn = bod & ((1 << 256) - 1)
    cry = (bod >> 256) & ((1 << 256) - 1)
    _p, dat = _hoon_rub(512, bod)
    w = BitWriter()
    w.write(8, ord("c"))
    w.write(256, ugn)
    w.write(256, cry)
    w.write_mat(dat)
    if xtr:
        w.write(xtr.bit_length(), xtr)
    return w.to_int()


def xtr_of_pass(pass_atom: int) -> int:
    """The custody log riding a suite-%c pass, as the jammed atom (0 = none).

    The exact inverse of pass_with_xtr, reading the same layout
    ('c' | ugn(256) | cry(256) | mat(dat) | xtr) and returning everything past
    the dat mat.  A boot pass answers 0.

    This exists so a publication can be checked against the transaction that
    carries it rather than against the variable we hope we passed in: the two
    shapes are indistinguishable in a transaction decode, so the only honest
    read-back goes through the bytes.  See assert_publication_carries_log."""
    if pass_atom & 0xFF != ord("c"):
        raise ValueError("xtr_of_pass: not a suite-C pass")
    bod = pass_atom >> 8
    p, _dat = _hoon_rub(512, bod)
    return bod >> (512 + p)


#  The byte cap on a publication payload.  1024, and the number is not
#  arbitrary: it is the PACKET bound (decisions addendum §6 fixes a complete
#  jammed attestation at one Mesa fragment, ~1 KiB), and a publication carries
#  that same packet.  A pass core is ~114 B, entry 0's opening ~90 B and the
#  terminal opening ~100 B, so the floor is ~300 B and each further custody hop
#  adds ~40 B — 1024 is ~18 hops, against the four that 512 allowed.  An
#  OP_RETURN is all non-witness data, so that is a ~1160 vB transaction:
#  ~2320 sats at 2 sat/vB.  MUST equal +max-publication:gw-btc-pass byte for
#  byte -- the Hoon is the other half of this pair, and the only other half.
MAX_PUBLICATION = 1024


def push_data(payload: bytes) -> bytes:
    """The minimal Bitcoin push opcode(s) for `payload`.

    A direct push (opcode = length) reaches 75.  PUSHDATA1 (0x4c) carries ONE
    length byte and so stops at 255 — far below this codec's own 1024-byte cap,
    so a fief-carrying publication (265–269 bytes in practice) already needs
    PUSHDATA2 (0x4d) and its TWO-byte LITTLE-ENDIAN length, and a full-packet
    one is never anything else.  Byte-for-byte identical to
    +push-data:gw-btc-pass.
    """
    n = len(payload)
    if n <= 75:
        return bytes([n])
    if n <= 0xFF:
        return b"\x4c" + bytes([n])
    if n <= 0xFFFF:
        return b"\x4d" + n.to_bytes(2, "little")
    raise ValueError(f"push {n} bytes is too big for OP_PUSHDATA2")


def make_publication_script(pass_atom: int, opening: dict) -> bytes:
    """The OP_RETURN scriptPubKey for a deliberate on-chain publication:

        OP_RETURN PUSH2 'gw' PUSH1 <kelvin> <pushdata payload>
        payload = (jam [pass opening])

    `pass_atom` is the comet's FULL attestation pass (custody log in its
    xtr — see pass_with_xtr) and `opening` is the hop this transaction
    performs; a watcher completes the log with this transaction's own
    [txid height opening].

    Payloads over 75 bytes use PUSHDATA1 (0x4c len), over 255 PUSHDATA2
    (0x4d len-lo len-hi); cap MAX_PUBLICATION bytes."""
    payload = jam_bytes(publication_noun(pass_atom, opening))
    if len(payload) > MAX_PUBLICATION:
        raise ValueError(f"publication payload {len(payload)} > {MAX_PUBLICATION}")
    # 6a 02 'gw' 01 <kelvin> — matches +publication-script:gw-btc-pass.
    return (bytes([0x6A, 0x02, 0x67, 0x77, 0x01, KELVIN])
            + push_data(payload) + payload)


def parse_publication_script(script: bytes) -> tuple[int, bytes] | None:
    """(kelvin, payload) from an OP_RETURN publication script, or None.

    The inverse of make_publication_script, and a port of
    +parse-publication:gw-btc-pass: it reads the three push forms push_data can
    emit — a direct push (1–75), OP_PUSHDATA1 (0x4c, one length byte) and
    OP_PUSHDATA2 (0x4d, two LITTLE-endian length bytes) — and refuses anything
    else rather than reading some other opcode as a length.  The kelvin is
    RETURNED, not checked, so a caller can say which version it found."""
    if len(script) < 6:
        return None
    if script[:4] != bytes([0x6A, 0x02, 0x67, 0x77]) or script[4] != 0x01:
        return None
    kelvin = script[5]
    rest = script[6:]
    if not rest:
        return None
    opc = rest[0]
    if opc == 0x4C:
        if len(rest) < 2:
            return None
        length, head = rest[1], 2
    elif opc == 0x4D:
        if len(rest) < 3:
            return None
        length, head = int.from_bytes(rest[1:3], "little"), 3
    elif opc <= 75:
        length, head = opc, 1
    else:
        return None
    payload = rest[head:]
    if len(payload) != length or length > MAX_PUBLICATION:
        return None
    return kelvin, payload


def read_publication_script(script: bytes) -> tuple[int, tuple] | None:
    """(pass_atom, opening_noun) read back out of a publication script, or None.

    +read-publication:gw-btc-pass, in Python: parse the envelope, undo
    jam_bytes' little-endian byte dump, and cue.  A foreign kelvin answers None,
    exactly as the Hoon does — a watcher at another protocol version ignores
    this output rather than mis-parsing it."""
    env = parse_publication_script(script)
    if env is None:
        return None
    kelvin, payload = env
    if kelvin != KELVIN:
        return None
    try:
        noun = hoon_cue(int.from_bytes(payload, "little"))
    except Exception:
        return None
    if not isinstance(noun, tuple) or len(noun) != 2:
        return None
    return noun


def assert_publication_carries_log(script: bytes, *, xtr: int, entries: int) -> None:
    """Read the payload back OUT of the script about to be broadcast and prove
    the pass inside it carries the custody log.  Raises ValueError if not.

    THIS IS THE CHECK THAT SEPARATES A LATE PUBLICATION FROM A CLAIMED SPAWN,
    and it has to read the bytes rather than the variable that produced them.
    A boot-pass publication and a whole-packet publication are INDISTINGUISHABLE
    in a transaction decode: same OP_RETURN envelope, same terminal opening,
    roughly 200 bytes shorter.  Nothing about the difference is visible until a
    stranger declines to declassify the comet — by which time the miner fee is
    spent and the identity sat has moved.

    Publish a boot pass (xtr empty) and a watcher completes a ONE-ENTRY log
    whose single entry is this transaction — the degenerate SPAWN shape — so
    +run-checks requires input 0 to spend the spawn satpoint.  A state update
    spends the satpoint its LAST hop landed on, so the check fails and the comet
    never declassifies.  That is measured, not theorised: it is why the four
    publications already on mainnet do not verify.

    The comparison is against the log baked into the proof by `causeway
    finalize`, not against the pass we think we built, so a bug anywhere between
    the two is caught here."""
    pub = read_publication_script(script)
    if pub is None:
        raise ValueError(
            "the publication output does not read back as a kelvin-"
            f"{KELVIN} publication — refusing to broadcast a payload no "
            "watcher will parse"
        )
    pass_atom, _opening = pub
    if not isinstance(pass_atom, int):
        raise ValueError("published payload's head is not a pass atom")
    try:
        got = xtr_of_pass(pass_atom)
    except ValueError as e:
        raise ValueError(f"published pass is unreadable: {e}")
    if got == 0:
        raise ValueError(
            "the published pass carries NO custody log — this is the boot "
            "pass, and a watcher would read it as a claimed spawn whose input "
            "0 must be the spawn satpoint.  A state update spends a later "
            "satpoint, so it would never verify."
        )
    if got != xtr:
        raise ValueError(
            f"the published pass carries a DIFFERENT custody log than the one "
            f"finalize baked ({(got.bit_length() + 7) // 8} bytes vs "
            f"{(xtr.bit_length() + 7) // 8})"
        )
    n = len(cue_custody_log(got))
    if n != entries:
        raise ValueError(
            f"the published custody log has {n} entries, expected {entries}")


def _hoon_unit(rendered: str | None) -> str:
    """(unit x) as a Hoon literal: ~ or `value."""
    return "~" if rendered is None else f"`{rendered}"


def _hoon_ud(n: int) -> str:
    """A Hoon @ud literal. Hoon REQUIRES dot grouping above 999 — a bare
    `900142` is a syntax error, so an undotted height makes the printed poke
    unpasteable."""
    return f"{int(n):,}".replace(",", ".")


def _hoon_dotted(value: int, groups: int, bits: int, base: int) -> str:
    """Hoon's dot-separated fixed-width atom rendering (+ro-co:co), used by
    the IP auras: @if is 4 base-10 groups of 8 bits, @is 8 base-16 groups of
    16. Most significant group first, no zero padding, leading dot."""
    mask = (1 << bits) - 1
    digits = "0123456789abcdef"
    out = []
    for i in reversed(range(groups)):
        g = (int(value) >> (bits * i)) & mask
        s = ""
        while True:
            s = digits[g % base] + s
            g //= base
            if g == 0:
                break
        out.append(s)
    return "." + ".".join(out)


def _hoon_cord(atom: int) -> str:
    """A @t literal, e.g. 'com'. Restricted to hostname characters — a turf
    label with a quote in it would produce unpasteable Hoon."""
    a = int(atom)
    s = a.to_bytes((a.bit_length() + 7) // 8, "little").decode("ascii", "replace")
    if not re.fullmatch(r"[a-z0-9-]+", s):
        raise ValueError(f"turf label {s!r} is not a plain hostname label")
    return f"'{s}'"


def format_hoon_fief(fief) -> str | None:
    """A $fief (sur/urb.hoon) as a Hoon literal — or None when absent, so it
    composes with _hoon_unit into `~` / `` `[%if .64.227.13.22 34.343] ``.

    The fief is part of the snapshot and therefore part of the on-chain
    state commitment; a printed poke that omits it recomputes a different
    state-key and can never match the chain."""
    n = fief_noun(fief)
    if n is None:
        return None
    tag, (p, q) = n
    name = _FIEF_TAG_NAMES[tag]
    if name == "if":                       # @ifF — .64.227.13.22
        p_hoon = _hoon_dotted(p, 4, 8, 10)
    elif name == "is":                     # @isH — .0.0.0.0.0.0.0.1
        p_hoon = _hoon_dotted(p, 8, 16, 16)
    else:                                  # %turf — ~[~['com' 'example']]
        turfs = _noun_list(p)
        if not turfs:
            p_hoon = "~"
        else:
            p_hoon = "~[" + " ".join(
                ("~" if not _noun_list(t)
                 else "~[" + " ".join(_hoon_cord(c) for c in _noun_list(t)) + "]")
                for t in turfs
            ) + "]"
    return f"[%{name} {p_hoon} {_hoon_ud(q)}]"


def format_custody_entry_poke(entry: dict) -> str:
    """The dojo line that hands ONE xtr entry to the ship's own %gw-btc.

    This is the in-band %anew path of decisions-addendum section 5: after a
    custody transaction confirms, the agent takes the entry, re-verifies the
    WHOLE extended log against the chain through %light-client, and — only on
    a positive verdict — re-encodes the ship's pass around it and answers
    jael's %anew. Nothing here is trusted; the poke is evidence, not
    authority. See $ingest in gw-btc/sur/self-attestation.hoon.

    The mark is %noun so no mark file is needed on either side.
    """
    o = entry["opening"]
    snap = o["snapshot"]
    sponsor = snap.get("sponsor")
    snap_hoon = (
        f'[{_hoon_ud(snap["life"])} {_hoon_ud(snap["rift"])} '
        f'{format_hoon_ux(format(int(snap["key"]), "x"))} '
        f'{_hoon_unit(int_to_patp(int(sponsor)) if sponsor is not None else None)} '
        # The fief is snapshot state (decisions-addendum 2) and enters the
        # state commitment. Printing `~` for a comet that committed one
        # yields a poke whose state-key cannot match the chain, and %anew
        # fails SILENTLY — so this field must be the entry's real fief.
        f'{_hoon_unit(format_hoon_fief(snap.get("fief")))}]'
    )
    so = o.get("spawn_opening", o.get("blind_opening"))
    if so is None:
        bo_hoon = "~"
    else:
        sp = so["spawn"]
        bo_hoon = (
            f'`[[{format_hoon_ux(sp["txid_hex"])} {_hoon_ud(sp["vout"])} {_hoon_ud(sp.get("off", 0))}] '
            f'{_hoon_ud(so["start_height"])}]'
        )
    opening_hoon = (
        f'`[{format_hoon_ux(format(int(o["internal_key"]), "x"))} {snap_hoon} {bo_hoon}]'
    )
    return (
        ":gw-btc &noun [%gw-custody-entry "
        f'[{format_hoon_ux(entry["txid_hex"])} {_hoon_ud(entry["height"])} {opening_hoon}]]'
    )


def build_xtr_atom(entries: list[dict]) -> int:
    """Jam the custody log, oldest first, terminating in ~ (0).

    Each entry: {txid_hex, height, opening}, where opening is None (a plain
    custody hop) or an opening dict (see opening_noun). Exactly one entry —
    entry 0, the spawn — carries a spawn_opening naming the sat and its start height."""
    log = 0  # ~ (null-terminated list)
    for e in reversed(entries):
        op = e.get("opening")
        op_unit = 0 if op is None else _unit(opening_noun(op))
        node = (int(e["txid_hex"], 16), (e["height"], op_unit))
        log = (node, log)
    return hoon_jam(log)


def cue_custody_log(xtr: int) -> list[dict]:
    """A baked xtr atom back into its entries, oldest first — build_xtr_atom's
    inverse.

    Each entry answers {txid_hex, height, opening}, where `opening` is the raw
    opening noun or None for a plain custody hop.  `txid_hex` is 64 hex digits,
    zero-padded, so it compares directly against a proof's `commit_txid`.

    A publication has to read its own log: to prove the last hop is the outpoint
    input 0 is about to spend (else the packet fails `N-continuity` on chain
    after the fee is paid), and to prove the payload it signs really carries
    that log.  Mirrors `;;(custody-log:sa (cue xtr.u.meta))` in
    +process-publication:urb-core, which likewise refuses a log it cannot
    read."""
    if xtr == 0:
        return []
    cur = hoon_cue(xtr)
    out: list[dict] = []
    while cur != 0:
        if not isinstance(cur, tuple):
            raise ValueError("xtr is not a null-terminated custody log")
        node, cur = cur
        try:
            txid, (height, opening) = node
        except (TypeError, ValueError) as e:
            raise ValueError(f"xtr entry is not [txid height (unit opening)]: {e}")
        out.append({
            "txid_hex": f"{int(txid):064x}",
            "height": int(height),
            "opening": None if opening == 0 else opening[1],
        })
    return out


def append_xtr_to_ring(ring_int: int, xtr: int) -> int:
    """Rebuild a suite-C ring ('C' | sed 64B | mat(dat) | xtr) with the given
    reveal log appended, mirroring +sec:ex:cric. The miner emits xtr-less
    rings (xtr can only be built after the spawn commit confirms); the boot
    feed should carry the baked log so pass.ames-state serves it to peers
    (spec §2.5). The @p is unchanged: the name commits to ugn+dat only."""
    assert ring_int & 0xFF == ord("C"), "not a suite-C ring"
    bod = ring_int >> 8
    sed = bod & ((1 << 512) - 1)
    p, dat = _hoon_rub(512, bod)
    w = BitWriter()
    w.write(8, ord("C"))
    w.write(512, sed)
    w.write_mat(dat)
    if xtr:
        w.write(xtr.bit_length(), xtr)
    return w.to_int()


def rebuild_feed(comet_p: int, rift: int, life: int, ring_int: int) -> int:
    """Re-jam the boot feed noun [[2 0] comet rift [[life ring] 0]] around an
    updated ring (e.g. after append_xtr_to_ring)."""
    return hoon_jam(((2, 0), (comet_p, (rift, ((life, ring_int), 0)))))


def _hoon_rub(a: int, b: int) -> tuple[int, int]:
    """Hoon ++rub: decode a mat at bit offset `a` of atom `b` -> (p, q)."""
    c = 0
    while ((b >> (a + c)) & 1) == 0:
        c += 1
        if c > 2000:
            raise ValueError("rub: too many zeros")
    if c == 0:
        return (1, 0)
    d = a + c + 1
    low = (b >> d) & ((1 << (c - 1)) - 1) if c > 1 else 0
    e = (1 << (c - 1)) + low
    val = (b >> (d + (c - 1))) & ((1 << e) - 1)
    return (2 * c + e, val)


# =========================================================================
#  HD wallet derivation — BIP-32 / BIP-86 taproot
# =========================================================================


def derive_taproot_address(seed_bytes: bytes) -> str:
    """
    Derive the first taproot receiving address (m/86'/1'/0'/0/0)
    from raw seed bytes, matching what spv-wallet does for @q seeds.

    spv-wallet's seed-to-bytes for %q uses raw atom bytes directly
    as the BIP-32 seed (no BIP-39 mnemonic/PBKDF2 step).
    """
    root = bip32.HDKey.from_seed(seed_bytes, version=NETWORKS["main"]["xprv"])
    child = root.derive("m/86h/1h/0h/0/0")
    # BIP-86 taproot: key-path only (empty script tree)
    addr = script.p2tr(child.key).address(NETWORKS["main"])
    return addr


# =========================================================================
#  @uw encoding — Urbit base-64 encoding
# =========================================================================


_UW_CHARS = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-~"
_UW_MAP = {c: i for i, c in enumerate(_UW_CHARS)}


def decode_uw(uw_str: str) -> int:
    """Decode an Urbit @uw base-64 string to an integer.

    Canonical @uw literals are 0w-prefixed (what comet-miner emits and what
    vere's -G parses via slaw %uw). NB: the old implementation did
    lstrip("0v"), which left the 'w' of a 0w prefix in the digit stream and
    silently corrupted the atom's high bits.
    """
    s = uw_str.strip()
    if s.startswith("0w") or s.startswith("0v"):
        s = s[2:]
    s = s.replace(".", "")
    if not s:
        return 0
    result = 0
    for ch in s:
        result = result * 64 + _UW_MAP[ch]
    return result


def encode_uw(value: int) -> str:
    """Encode an integer as a canonical Urbit @uw literal (0w prefix, dots
    every 5 digits) — the form slaw %uw (vere -G) accepts."""
    if value == 0:
        return "0w0"
    digits = ""
    v = value
    while v > 0:
        digits = _UW_CHARS[v & 63] + digits
        v >>= 6
    groups = []
    i = len(digits)
    while i > 0:
        groups.insert(0, digits[max(0, i - 5):i])
        i -= 5
    return "0w" + ".".join(groups)


# =========================================================================
#  Hoon mat — length-prefix encoding for atoms
# =========================================================================


def hoon_mat(a: int) -> tuple[int, int]:
    """Hoon mat: length-encode an atom. Returns (bit_count, bits_value).

    Encoding format (bit-level, LSB-first in the stream):
    - value 0: single bit 1
    - value a > 0: c zeros, separator 1, c-1 low bits of b, then b bits of a
      where b = bit_length(a), c = bit_length(b)
    """
    if a == 0:
        return (1, 1)
    b = a.bit_length()
    c = b.bit_length()
    p = 2 * c + b
    bex_c = 1 << c
    low_b = b & ((1 << (c - 1)) - 1) if c > 1 else 0
    shifted_a = a << (c - 1) if c > 1 else a
    mixed = low_b ^ shifted_a
    q = bex_c | (mixed << bex_c.bit_length())
    return (p, q)


# =========================================================================
#  Hoon jam — noun serialization
# =========================================================================


def hoon_jam(noun) -> int:
    """Jam a noun (atom or [head, tail] pair) into an atom.

    Nouns are represented as: int for atoms, tuple (head, tail) for cells.
    This is a minimal implementation sufficient for key derivation.
    """
    bits = []
    pos = 0
    refs = {}

    def write_bit(b):
        nonlocal pos
        bits.append(1 if b else 0)
        pos += 1

    def write_bits(val, count):
        nonlocal pos
        for i in range(count):
            bits.append(1 if (val >> i) & 1 else 0)
        pos += count

    def write_mat(val):
        if val == 0:
            bits.append(1)
            nonlocal pos
            pos += 1
            return
        p, q = hoon_mat(val)
        write_bits(q, p)

    def encode(n):
        nonlocal pos
        start = pos
        if isinstance(n, tuple):
            # Cell
            if n in refs:
                write_bit(1); write_bit(1)  # back-reference tag
                write_mat(refs[n])
            else:
                refs[n] = start
                write_bit(1); write_bit(0)  # cell tag
                encode(n[0])
                encode(n[1])
        else:
            # Atom
            if n in refs:
                n_bits = n.bit_length()
                ref_bits = refs[n].bit_length() if refs[n] > 0 else 1
                if n_bits <= ref_bits:
                    write_bit(0)
                    write_mat(n)
                else:
                    write_bit(1); write_bit(1)
                    write_mat(refs[n])
            else:
                refs[n] = start
                write_bit(0)
                write_mat(n)

    encode(noun)
    result = 0
    for i, b in enumerate(bits):
        result |= b << i
    return result


def hoon_cue(atom: int):
    """Cue a jammed atom back into a noun (int for atoms, tuple for cells).

    Inverse of hoon_jam; used by `causeway finalize` to unpack a boot feed.
    """
    refs: dict[int, object] = {}

    def rub(pos: int) -> tuple[int, int]:
        return _hoon_rub(pos, atom)

    def decode(pos: int):
        start = pos
        tag0 = (atom >> pos) & 1
        if tag0 == 0:
            width, val = rub(pos + 1)
            refs[start] = val
            return val, pos + 1 + width
        tag1 = (atom >> (pos + 1)) & 1
        if tag1 == 0:
            head, pos2 = decode(pos + 2)
            tail, pos3 = decode(pos2)
            cell = (head, tail)
            refs[start] = cell
            return cell, pos3
        width, ref = rub(pos + 2)
        return refs[ref], pos + 2 + width

    noun, _end = decode(0)
    return noun


# =========================================================================
#  Hoon crypto primitives — shal, shaf, shax for luck:ed key derivation
# =========================================================================


def hoon_shax(atom: int) -> int:
    """Hoon shax: SHA-256 of an atom's minimal bytes. Returns atom."""
    n = (atom.bit_length() + 7) // 8
    data = atom.to_bytes(n, "little") if n > 0 else b""
    digest = hashlib.sha256(data).digest()
    return int.from_bytes(digest, "little")


def hoon_shal(length: int, atom: int) -> int:
    """Hoon shal: SHA-512 with explicit byte length. Returns atom.

    ++  shal  |=([len=@ ruz=@] ...)  ::  sha-512 with length
    Hashes the first `length` bytes of the atom.
    """
    data = atom.to_bytes(max(length, 1), "little")[:length] if length > 0 else b""
    digest = hashlib.sha512(data).digest()
    return int.from_bytes(digest, "little")


def hoon_luck_ed(seed_atom: int, b: int = 256) -> tuple[bytes, bytes]:
    """Hoon luck:ed: derive ed25519 keypair from seed atom.

    From zuse.hoon:
      =+  h=(shal (rsh [0 3] b) sed)     :: h = SHA-512(first 32 bytes of seed)
      =+  a = bex(b-2) + lsh(0, 3, cut(0, [3 (sub b 5)] h))  :: clamp
      =+  aa=(scalarmult-base a)
      [aa (can 0 ~[[b a] [b (cut 0 [b b] h)]])]

    Returns (public_key_32bytes, secret_part).
    """
    cb = b >> 3  # 32 for ed25519
    # h = shal(32, seed) = SHA-512 of first 32 bytes of seed
    h = hoon_shal(cb, seed_atom)

    # Clamp: a = bex(b-2) + lsh(0, 3, cut(0, [3 (sub b 5)] h))
    # cut(0, [3 (sub b 5)] h) = bits 3 through (b-5+3-1) = bits 3 through (b-3) of h
    # For b=256: cut(0, [3 251] h) = bits 3..253 of h (251 bits)
    low_bits = (h >> 3) & ((1 << (b - 5)) - 1)
    # lsh(0, 3, x) = x << 3
    shifted = low_bits << 3
    # bex(b-2) = 2^254
    a = (1 << (b - 2)) + shifted

    # scalarmult-base
    a_bytes = a.to_bytes(32, "little")
    pub = nacl.bindings.crypto_scalarmult_ed25519_base_noclamp(a_bytes)

    # Secret: (can 0 ~[[b a] [b (cut 0 [b b] h)]])
    upper_h = (h >> b) & ((1 << b) - 1)
    sek_atom = a | (upper_h << b)

    return (pub, a_bytes)


# =========================================================================
#  Bitstream writer — builds packed bit arrays for urb encoding
# =========================================================================


class BitWriter:
    """Builds a bitstream by appending fixed-width values, LSB-first."""

    def __init__(self):
        self._bits = 0  # accumulated bits as an integer
        self._pos = 0   # current bit position

    def write(self, width: int, value: int):
        """Append `width` bits of `value` to the stream."""
        if width == 0:
            return
        mask = (1 << width) - 1
        self._bits |= (value & mask) << self._pos
        self._pos += width

    def write_mat(self, value: int):
        """Write a mat-encoded atom to the stream."""
        p, q = hoon_mat(value)
        self.write(p, q)

    def to_bytes(self) -> bytes:
        """Return the bitstream as a byte string (little-endian, zero-padded)."""
        n_bytes = (self._pos + 7) // 8
        return self._bits.to_bytes(n_bytes, "little") if n_bytes > 0 else b""

    def to_int(self) -> int:
        """Return the bitstream as a raw integer."""
        return self._bits

    @property
    def bit_length(self) -> int:
        return self._pos


# =========================================================================
#  Taproot helpers — address derivation with script tree
# =========================================================================


def _tagged_hash(tag: str, data: bytes) -> bytes:
    """BIP-340 tagged hash: SHA256(SHA256(tag) || SHA256(tag) || data)."""
    tag_hash = hashlib.sha256(tag.encode()).digest()
    return hashlib.sha256(tag_hash + tag_hash + data).digest()


def _tapleaf_hash(leaf_version: int, script_bytes: bytes) -> bytes:
    """Compute BIP-341 TapLeaf hash."""
    # Compact encoding of script length
    script_len = len(script_bytes)
    if script_len < 0xFD:
        compact = bytes([script_len])
    elif script_len <= 0xFFFF:
        compact = b"\xfd" + script_len.to_bytes(2, "little")
    else:
        compact = b"\xfe" + script_len.to_bytes(4, "little")
    return _tagged_hash("TapLeaf", bytes([leaf_version]) + compact + script_bytes)


def _taproot_tweak_pubkey(internal_key: bytes, merkle_root: bytes | None) -> tuple[bytes, int]:
    """Compute tweaked output key and parity from internal key and merkle root.

    Returns (x-only output key, parity).
    """
    from embit.util import secp256k1
    h = merkle_root if merkle_root is not None else b""
    tweak = _tagged_hash("TapTweak", internal_key + h)
    # Parse internal key with even y (BIP-341 requirement)
    point = secp256k1.ec_pubkey_parse(b"\x02" + internal_key)
    # Add tweak*G to get output key
    pub = secp256k1.ec_pubkey_add(point, tweak)
    # Serialize to get actual parity (embit's taproot_tweak discards this)
    sec = secp256k1.ec_pubkey_serialize(pub)
    parity = 0 if sec[0] == 0x02 else 1
    x_only = sec[1:33]
    return (x_only, parity)


# =========================================================================
#  Ring / Pass — derive Suite C networking key from comet miner output
# =========================================================================


def derive_pass_from_ring(ring_uw: str, tweak_bytes: bytes | None = None) -> int:
    """Derive the Suite C pass (public networking key) from a ring.

    Replicates Hoon's pub:ex:(nol:nu:cric:crypto ring).

    ring_uw: the ring value as a @uw string from comet-miner output
    tweak_bytes: UNUSED (kept for call-site compatibility) — the tweak data
        (dat) is parsed out of the ring itself, as +nol does.

    Returns the pass as a Hoon atom (integer).
    """
    ring_int = decode_uw(ring_uw)

    # Parse ring: 'C'(1 byte) + seed(64 bytes) + mat(tweak)
    ring_byte_len = (ring_int.bit_length() + 7) // 8
    ring_raw = ring_int.to_bytes(ring_byte_len, "little")

    tag = ring_raw[0]
    assert tag == ord("C"), f"Expected Suite C ring tag 'C' (0x43), got 0x{tag:02x}"

    # bod = ring >> 8 (strip tag byte)
    bod = ring_int >> 8

    # Hoon: s = luck:ed(end(8, bod)), c = luck:ed(cut(8, [1 1], bod))
    # end(8, bod) = bod & ((1<<256)-1) = signing seed (low 32 bytes)
    # cut(8, [1 1], bod) = (bod >> 256) & ((1<<256)-1) = crypto seed (next 32 bytes)
    s_seed_atom = bod & ((1 << 256) - 1)
    c_seed_atom = (bod >> 256) & ((1 << 256) - 1)

    # luck:ed derives ed25519 keypair using Hoon's shal (NOT standard ed25519)
    s_pub, s_sek = hoon_luck_ed(s_seed_atom)
    c_pub, c_sek = hoon_luck_ed(c_seed_atom)

    # Extract tweak data from ring: rub at bit 512 of bod
    # (rub 512 bod) gives [bit_count, tweak_data]; everything after the
    # mat is xtr, the off-chain custody log (kelvin-9) — empty on
    # miner-fresh rings, present after append_xtr_to_ring.
    _cur, dat = _hoon_rub(512, bod)
    xtr = bod >> (512 + _cur)

    # Compute tweak: mit = shax(can(3, [32 pub.s] [(met 3 dat) dat] ~))
    # shax hashes the minimal bytes of the atom
    s_pub_atom = int.from_bytes(s_pub, "little")
    can_result = s_pub_atom | (dat << 256)  # can 3: s_pub in low 32 bytes, dat above
    mit = hoon_shax(can_result)

    # Apply tweak: t = scad:ed(pub.s, sek.s, mit)
    # scad does: pub' = scalarmult-base(a + mit) where a is the clamped secret
    # But for the pass, we just need the tweaked public key
    mit_bytes = mit.to_bytes(32, "little")
    tw_point = nacl.bindings.crypto_scalarmult_ed25519_base_noclamp(mit_bytes)
    tweaked_s_pub = nacl.bindings.crypto_core_ed25519_add(s_pub, tw_point)

    # Build pass: 'c' + ugn(untweaked s_pub) + cry(c_pub ed25519) + mat(dat) + xtr
    # From zuse.hoon +pub:ex:cric: pub [cry=pub.c sgn=pub.t tw=[ugn=pub.s dat=dat xtr=xtr]]
    # cry is the ed25519 public key from luck, NOT curve25519; xtr rides at
    # its exact bit length ((met 0 xtr)^xtr), omitted entirely when 0.
    mat_p, mat_q = hoon_mat(dat)

    w = BitWriter()
    w.write(8, ord("c"))
    w.write(256, int.from_bytes(s_pub, "little"))   # ugn = untweaked s_pub
    w.write(256, int.from_bytes(c_pub, "little"))    # cry = ed25519 pub from luck
    w.write(mat_p, mat_q)                                 # mat(dat)
    if xtr:
        w.write(xtr.bit_length(), xtr)                    # custody log (kelvin-9)

    return w.to_int()


# =========================================================================
#  Messaging key — the snapshot's `key` field (cry.pub of the suite-C pass)
# =========================================================================


def messaging_key_from_pass(pass_atom: int) -> int:
    """cry — the suite-C messaging (encryption) public key, which is the
    snapshot's `key` field.  Pass layout (see derive_pass_from_ring):
    'c'(8 bits) | ugn(256) | cry(256) | mat(dat) | xtr."""
    return (pass_atom >> (8 + 256)) & ((1 << 256) - 1)


# =========================================================================
#  Transaction building — spawn (one tx, key-path) and rekey (state update)
# =========================================================================


def _derive_key_at_index(seed_bytes: bytes, index: int) -> bip32.HDKey:
    """Derive BIP-86 key at m/86'/1'/0'/0/<index> from raw seed bytes."""
    root = bip32.HDKey.from_seed(seed_bytes, version=NETWORKS["main"]["xprv"])
    return root.derive(f"m/86h/1h/0h/0/{index}")


def build_spawn_psbt(
    *,
    utxo_txid: str,
    utxo_vout: int,
    utxo_value: int,
    utxo_script_pubkey: bytes,
    funding_internal_xonly: bytes,
    funding_path: str,
    funding_fingerprint: bytes,
    snapshot: dict,
    publication_pass_atom: int | None = None,
    publication_opening: dict | None = None,
    fee_rate: int = 2,
    change_internal_xonly: bytes | None = None,
    change_script_pubkey: bytes | None = None,
    change_path: str | None = None,
    network: str = "main",
) -> tuple["psbt.PSBT", dict]:
    """Build the kelvin-9 spawn transaction — ONE tx, no reveal (spec §7.3).

    Shape:
      * input 0  — the chosen funding UTXO, spent via P2TR key-path.
      * output 0 — the sat-carrying P2TR output whose key Q = state_output_key
        (funding xonly, initial snapshot).  The sat lands here and travels with
        its state commitment; the owner re-spends it key-path for every future
        custody move.
      * OP_RETURN publication output — PUBLIC spawns only.  A confidential spawn
        omits it (there is nothing on-chain to grep).  Pass publication_pass_atom
        + publication_opening to include it.
      * change (optional).

    Returns (psbt, proof_dict); proof stores the snapshot + opening so a later
    finalize can bake the xtr and a rekey can re-derive the current leaf hash.
    """
    from embit import psbt as _psbt
    from embit import ec as _ec
    from embit.transaction import (
        Transaction as _Tx,
        TransactionInput as _TxIn,
        TransactionOutput as _TxOut,
    )

    # The sat-carrying output commits the initial snapshot under the funding key.
    q = state_output_key(funding_internal_xonly, snapshot)
    sat_spk = bytes([0x51, 0x20]) + q
    leaf_hash = state_leaf_hash(state_leaf_script(state_commit(snapshot)))

    publish = publication_pass_atom is not None and publication_opening is not None
    pub_script = (
        make_publication_script(publication_pass_atom, publication_opening)
        if publish else None
    )

    # Rough vbyte estimate: 1 key-path input (~57.5), each P2TR out ~43,
    # overhead ~10.5, an OP_RETURN publication ~ (9 + len)/... counted as bytes.
    est_vbytes = 68 + 43  # input + sat output + overhead-ish
    if pub_script is not None:
        est_vbytes += 11 + len(pub_script)
    if change_script_pubkey is not None:
        est_vbytes += 43
    fee = est_vbytes * fee_rate

    outputs: list = []
    if change_script_pubkey is None:
        sat_value = utxo_value - fee
        if sat_value < 330:
            raise RuntimeError(
                f"UTXO {utxo_value} sats too small: need >= 330 + {fee} (fee)"
            )
        outputs.append(_TxOut(sat_value, _script_from_spk(sat_spk)))
        if pub_script is not None:
            outputs.append(_TxOut(0, _script_from_spk(pub_script)))
    else:
        sat_value = min(1_000, utxo_value - fee - 330)
        if sat_value < 330:
            raise RuntimeError(
                f"UTXO {utxo_value} sats too small: need >= {330 + 330 + fee} "
                f"(sat output + change dust + fee)"
            )
        change_value = utxo_value - sat_value - fee
        outputs.append(_TxOut(sat_value, _script_from_spk(sat_spk)))
        if pub_script is not None:
            outputs.append(_TxOut(0, _script_from_spk(pub_script)))
        outputs.append(_TxOut(change_value, _script_from_spk(change_script_pubkey)))

    txid_bytes = bytes.fromhex(utxo_txid)
    tx = _Tx(
        version=2,
        vin=[_TxIn(txid_bytes, utxo_vout, sequence=0xFFFFFFFF)],
        vout=outputs,
        locktime=0,
    )

    p = _psbt.PSBT(tx)
    inp = p.inputs[0]
    inp.witness_utxo = _TxOut(utxo_value, _script_from_spk(utxo_script_pubkey))
    funding_pubkey = _ec.PublicKey.from_xonly(funding_internal_xonly)
    inp.taproot_internal_key = funding_pubkey
    inp.taproot_bip32_derivations[funding_pubkey] = (
        [],  # key-path spend of a plain P2TR input — no leaf hashes
        _psbt.DerivationPath(funding_fingerprint, _parse_path(funding_path)),
    )

    # Change output metadata (known-derivation output for the signer).
    if change_script_pubkey is not None and change_internal_xonly is not None and change_path is not None:
        change_pubkey = _ec.PublicKey.from_xonly(change_internal_xonly)
        out_change = p.outputs[-1]
        out_change.taproot_internal_key = change_pubkey
        out_change.taproot_bip32_derivations[change_pubkey] = (
            [],
            _psbt.DerivationPath(funding_fingerprint, _parse_path(change_path)),
        )

    proof = {
        "version": 2,
        "protocol": "kelvin-9",
        "sat_vout": 0,
        "internal_pubkey_hex": funding_internal_xonly.hex(),
        "snapshot": snapshot,
        "leaf_hash_hex": leaf_hash.hex(),   # merkle root for the next rekey's key-path spend
        "sat_script_pubkey_hex": sat_spk.hex(),
        "sat_value": outputs[0].value,
        "published": bool(publish),
        "network": network,
        "funding": {
            "txid": utxo_txid,
            "vout": utxo_vout,
            "value": utxo_value,
            "path": funding_path,
            "fingerprint_hex": funding_fingerprint.hex(),
        },
    }
    return p, proof


def add_tap_merkle_root_hint(psbt_obj, input_idx: int, merkle_root: bytes) -> None:
    """Set PSBT_IN_TAP_MERKLE_ROOT on a PSBT input. Required when re-spending a
    prior sat-carrying output via key-path: the signer needs merkle_root (= the
    state leaf hash of the CURRENT snapshot) to compute the taproot tweak, and
    won't know it from their descriptor alone."""
    inp = psbt_obj.inputs[input_idx]
    inp.taproot_merkle_root = merkle_root


#  Sizing constants for the rekey/state-update builder.  A P2TR key-path input
#  is 57.5 vB (41 base + 66/4 witness); a P2TR output is 43 vB; tx overhead with
#  a segwit marker is ~10.5 vB.  111 = overhead + one input + one output, the
#  historical single-input estimate, kept exactly so an unfunded rekey builds
#  byte-identically to before.
P2TR_KEYPATH_INPUT_VB = 58
P2TR_OUTPUT_VB = 43
P2TR_DUST = 330


def normalize_funding_input(f: dict) -> dict:
    """Validate + normalize one funding-input spec for build_rekey_psbt.

    Accepts hex or bytes for the binary fields, and the key names that
    scan_addresses() already produces (`scriptpubkey`, `xonly`, `path`).
    """
    def _b(v, name, length=None):
        if v is None:
            raise ValueError(f"funding input is missing {name!r}")
        b = bytes.fromhex(v) if isinstance(v, str) else bytes(v)
        if length is not None and len(b) != length:
            raise ValueError(f"funding input {name!r} must be {length} bytes, got {len(b)}")
        return b

    txid = str(f.get("txid", "")).strip().lower()
    if len(txid) != 64 or any(c not in "0123456789abcdef" for c in txid):
        raise ValueError(f"funding input txid {txid!r} is not 64 hex characters")
    vout = int(f["vout"])
    if vout < 0:
        raise ValueError("funding input vout must be >= 0")
    value = int(f["value"])
    if value <= 0:
        raise ValueError("funding input value must be positive")
    spk = _b(f.get("script_pubkey", f.get("scriptpubkey")), "script_pubkey")
    xonly = _b(f.get("xonly", f.get("internal_xonly")), "xonly", 32)
    fpr = _b(f.get("fingerprint", f.get("fingerprint_hex")) or b"\x00\x00\x00\x00",
             "fingerprint", 4)
    return {
        "txid": txid,
        "vout": vout,
        "value": value,
        "script_pubkey": spk,
        "xonly": xonly,
        "path": str(f.get("path") or "m/86h/0h/0h/0/0"),
        "fingerprint": fpr,
    }


def assert_identity_input_zero(tx, prior_txid: str, prior_vout: int) -> None:
    """Refuse any state-update transaction whose input 0 is not the identity sat.

    This is the ONE structural invariant a funded state update must not break,
    and it is load-bearing twice over:

      * ownership — the verifier reads only input 0.  self-attestation.hoon:674
        takes `(snag-input 0 this)` and :678-680 requires its outpoint to equal
        the tracked satpoint (`input-zero`, `continuity`); :683-689 derives the
        key-path check from input 0's prevout alone.  An identity input
        anywhere else is simply not seen.

      * ordinals — sats are assigned to outputs in input order, so the tracked
        sat's transaction-wide index equals its offset within input 0's prevout
        ONLY while input 0 is first.  +index-to-sont (urb-core.hoon:491) is fed
        exactly that offset (self-attestation.hoon:695), so an input ahead of
        the identity would shift the sat by that input's whole value and land it
        in the wrong output — or in the fee.

    Funding inputs after input 0 contribute sats strictly behind the tracked one
    and cannot move it, which is why topping up is safe at all.
    """
    if len(tx.vin) == 0:
        raise ValueError("state update has no inputs")
    #  embit stores TransactionInput.txid in DISPLAY order and reverses it on
    #  the wire, so this compares like for like with the proof's txid hex.
    got_txid = tx.vin[0].txid.hex()
    if got_txid != prior_txid.lower() or int(tx.vin[0].vout) != int(prior_vout):
        raise ValueError(
            "input 0 must be the identity satpoint "
            f"{prior_txid}:{prior_vout}, got {got_txid}:{tx.vin[0].vout}. "
            "Funding inputs belong AFTER input 0 — the verifier reads input 0 "
            "only (self-attestation.hoon:674-689), and an input ahead of the "
            "identity shifts the sat's ordinal index by its whole value."
        )


def build_rekey_psbt(
    *,
    prior_proof: dict,
    new_snapshot: dict,
    publication_pass_atom: int | None = None,
    publication_opening: dict | None = None,
    fee_rate: int = 2,
    network: str = "main",
    funding_inputs: list[dict] | None = None,
    sat_target: int | None = None,
    change_script_pubkey: bytes | None = None,
    change_internal_xonly: bytes | None = None,
    change_path: str | None = None,
) -> tuple["psbt.PSBT", dict]:
    """Build a rekey / state-update transaction — the one surviving on-chain
    management op (spec §5, §7.4).

    Spends the point's current sat-carrying output key-path (embit needs the
    taproot internal key + merkle root = leaf hash of the CURRENT snapshot's
    state leaf, so the external signer can compute the key-path tweak), and
    commits the new snapshot at output 0 = P2TR(new Q).  An OP_RETURN
    publication output is added for public comets.

    Shape:
      * input 0    — the identity sat, ALWAYS.  See assert_identity_input_zero.
      * input 1..n — optional funding inputs from the operator's wallet.
      * output 0   — the sat-carrying P2TR output committing `new_snapshot`.
      * OP_RETURN publication output, if publishing.
      * change, last.

    Without `funding_inputs` a state update pays its fee out of the identity sat
    and so shrinks monotonically — the failure mode that priced a live comet
    down to 754 sats.  With them, output 0 can GROW: `sat_target` names the
    value output 0 should end up holding (default: everything that is left after
    the fee, i.e. the whole top-up lands on the identity sat and there is no
    change).  Change, when it exists, goes last so it cannot displace output 0.

    Returns (psbt, new_proof_dict) — a chained proof.
    """
    from embit import psbt as _psbt
    from embit import ec as _ec
    from embit.transaction import (
        Transaction as _Tx,
        TransactionInput as _TxIn,
        TransactionOutput as _TxOut,
    )

    internal_xonly = bytes.fromhex(prior_proof["internal_pubkey_hex"])
    prior_merkle_root = bytes.fromhex(prior_proof["leaf_hash_hex"])
    prior_txid = prior_proof.get("commit_txid") or prior_proof.get("sat_txid")
    if not prior_txid:
        raise ValueError("prior proof has no commit_txid — was the prior tx broadcast?")
    prior_vout = int(prior_proof.get("sat_vout", 0))
    prior_value = int(prior_proof["sat_value"])
    prior_spk = bytes.fromhex(prior_proof["sat_script_pubkey_hex"])

    funding = prior_proof.get("funding", {})
    funding_path = funding.get("path", "m/86h/0h/0h/0/0")
    funding_fingerprint = bytes.fromhex(funding.get("fingerprint_hex", "00000000"))

    new_q = state_output_key(internal_xonly, new_snapshot)
    new_sat_spk = bytes([0x51, 0x20]) + new_q
    new_leaf_hash = state_leaf_hash(state_leaf_script(state_commit(new_snapshot)))

    publish = publication_pass_atom is not None and publication_opening is not None
    pub_script = (
        make_publication_script(publication_pass_atom, publication_opening)
        if publish else None
    )

    funds = [normalize_funding_input(f) for f in (funding_inputs or [])]
    seen = {(prior_txid.lower(), int(prior_vout))}
    for f in funds:
        key = (f["txid"], f["vout"])
        if key in seen:
            raise ValueError(
                f"funding input {f['txid']}:{f['vout']} is already an input "
                f"(the identity sat, or a duplicate) — a tx cannot spend an "
                f"outpoint twice"
            )
        seen.add(key)

    total_in = prior_value + sum(f["value"] for f in funds)

    # 111 = overhead + the identity input + the sat output (unchanged for the
    # unfunded case, so an ordinary rekey builds exactly as it always has).
    base_vbytes = 111 + P2TR_KEYPATH_INPUT_VB * len(funds)
    if pub_script is not None:
        base_vbytes += 11 + len(pub_script)

    fee_no_change = base_vbytes * fee_rate
    fee_with_change = (base_vbytes + P2TR_OUTPUT_VB) * fee_rate

    if sat_target is None:
        #  Everything the inputs carry, minus the fee, lands on the identity
        #  sat.  For an unfunded rekey this is the historical behaviour
        #  (shrink by the fee); with funding it is a full top-up.
        fee = fee_no_change
        new_value = total_in - fee
        change_value = 0
    else:
        new_value = int(sat_target)
        change_value = total_in - fee_with_change - new_value
        fee = fee_with_change
        if total_in - fee_no_change < new_value:
            raise RuntimeError(
                f"sat_target={new_value} is more than the inputs can pay for: "
                f"{total_in} sats in, {fee_no_change} sats of fee leaves "
                f"{total_in - fee_no_change}. Add more funding or lower the target."
            )
        if change_value < P2TR_DUST:
            #  Change would be dust: drop the output and give the remainder to
            #  the identity sat rather than burning it as fee.
            fee = fee_no_change
            new_value = total_in - fee
            change_value = 0
        elif change_script_pubkey is None:
            raise ValueError(
                f"sat_target={new_value} leaves {change_value} sats of change "
                f"but no change_script_pubkey was given — pass one, or drop "
                f"sat_target to put the whole top-up on the identity sat"
            )

    if new_value < P2TR_DUST:
        have = (f"prior sat output {prior_value} sats"
                if not funds else
                f"prior sat output {prior_value} sats + {total_in - prior_value} "
                f"sats of funding = {total_in}")
        raise RuntimeError(
            f"{have} too small: need >= {P2TR_DUST} + {fee} (fee). "
            f"Add a funding input (rekey --fund-xpub/--fund-utxo) or lower the "
            f"fee-rate."
        )

    #  Ordinal floor: the sat sits at `sat_off` bytes into the prevout, and
    #  +index-to-sont (urb-core.hoon:491) lands it in output 0 only while
    #  sat_off < output 0's value.  Shrinking output 0 past the offset would
    #  silently move the identity to a later output — or into the fee.
    sat_off = int(prior_proof.get("sat_off", 0))
    if new_value <= sat_off:
        raise RuntimeError(
            f"sat output {new_value} would be at or below the tracked sat's "
            f"offset {sat_off}: the identity would not land in output 0"
        )

    outputs = [_TxOut(new_value, _script_from_spk(new_sat_spk))]
    if pub_script is not None:
        outputs.append(_TxOut(0, _script_from_spk(pub_script)))
    if change_value:
        outputs.append(_TxOut(change_value, _script_from_spk(change_script_pubkey)))

    #  Input 0 is the identity sat; funding follows.  Never the other way round.
    vin = [_TxIn(bytes.fromhex(prior_txid), prior_vout, sequence=0xFFFFFFFF)]
    vin += [_TxIn(bytes.fromhex(f["txid"]), f["vout"], sequence=0xFFFFFFFF)
            for f in funds]

    tx = _Tx(version=2, vin=vin, vout=outputs, locktime=0)
    assert_identity_input_zero(tx, prior_txid, prior_vout)
    p = _psbt.PSBT(tx)

    # Key-path spend of the tweaked sat output: the signer needs the current
    # snapshot's leaf hash as PSBT_IN_TAP_MERKLE_ROOT to compute the tweak.
    inp = p.inputs[0]
    inp.witness_utxo = _TxOut(prior_value, _script_from_spk(prior_spk))
    internal_pubkey = _ec.PublicKey.from_xonly(internal_xonly)
    inp.taproot_internal_key = internal_pubkey
    inp.taproot_merkle_root = prior_merkle_root
    inp.taproot_bip32_derivations[internal_pubkey] = (
        [],
        _psbt.DerivationPath(funding_fingerprint, _parse_path(funding_path)),
    )

    #  Funding inputs are plain BIP-86 P2TR key-path spends: internal key +
    #  derivation, and NO merkle root (they carry no state commitment).
    for i, f in enumerate(funds, start=1):
        fin = p.inputs[i]
        fin.witness_utxo = _TxOut(f["value"], _script_from_spk(f["script_pubkey"]))
        fpub = _ec.PublicKey.from_xonly(f["xonly"])
        fin.taproot_internal_key = fpub
        fin.taproot_bip32_derivations[fpub] = (
            [],
            _psbt.DerivationPath(f["fingerprint"], _parse_path(f["path"])),
        )

    #  Change output metadata (known-derivation output for the signer).
    if change_value and change_internal_xonly is not None and change_path is not None:
        change_pubkey = _ec.PublicKey.from_xonly(change_internal_xonly)
        out_change = p.outputs[-1]
        out_change.taproot_internal_key = change_pubkey
        out_change.taproot_bip32_derivations[change_pubkey] = (
            [],
            _psbt.DerivationPath(funding_fingerprint, _parse_path(change_path)),
        )

    new_proof = {
        "version": 2,
        "protocol": "kelvin-9",
        "sat_vout": 0,
        "internal_pubkey_hex": internal_xonly.hex(),
        "snapshot": new_snapshot,
        "leaf_hash_hex": new_leaf_hash.hex(),
        "sat_script_pubkey_hex": new_sat_spk.hex(),
        "sat_value": new_value,
        #  The sat's offset inside output 0.  Funding lands strictly behind it
        #  (inputs are consumed in order), so a top-up never moves it.
        "sat_off": sat_off,
        "published": bool(publish),
        "network": network,
        "funding": {
            "txid": prior_txid,   # the sat's prior home we spent (provenance)
            "vout": prior_vout,
            "value": prior_value,
            "path": funding_path,
            "fingerprint_hex": funding_fingerprint.hex(),
        },
        "prior_proof": {
            "commit_txid": prior_txid,
            "sat_vout": prior_vout,
        },
    }
    if funds:
        new_proof["funding_inputs"] = [
            {"txid": f["txid"], "vout": f["vout"], "value": f["value"],
             "path": f["path"]}
            for f in funds
        ]
        new_proof["topped_up_by"] = new_value - prior_value
    if change_value:
        new_proof["change_value"] = change_value
    return p, new_proof


def _build_p2tr_spk(internal_xonly: bytes, merkle_root: bytes | None) -> bytes:
    """scriptPubKey = OP_1 <32-byte tweaked_xonly>."""
    output_xonly, _parity = _taproot_tweak_pubkey(internal_xonly, merkle_root)
    return bytes([0x51, 0x20]) + output_xonly


def _script_from_spk(spk: bytes):
    """Wrap a raw scriptPubKey in embit's Script type."""
    return script.Script(spk)


def _parse_path(path: str) -> list[int]:
    """Parse a BIP-32 derivation path like "m/86'/0'/0'/0/3" into a list of uint32."""
    parts = path.strip().lstrip("m/").split("/")
    out: list[int] = []
    for part in parts:
        if not part:
            continue
        hardened = part.endswith("'") or part.endswith("h")
        n = int(part.rstrip("'h"))
        if hardened:
            n |= 0x80000000
        out.append(n)
    return out


# =========================================================================
#  Block confirmation — wait for N confirmations
# =========================================================================


def wait_for_confirmations(
    txid: str, required: int = BLOCK_CONFIRMATIONS,
    poll_interval: int = POLL_INTERVAL, **rpc_kwargs
) -> None:
    """Block until a transaction has at least `required` confirmations."""
    print(f"\nWaiting for {required} block confirmation(s) (checking every {poll_interval}s)...")
    start = time.monotonic()
    while True:
        try:
            tx_info = rpc_call("getrawtransaction", [txid, True], **rpc_kwargs)
            confs = tx_info.get("confirmations", 0) if tx_info else 0
            if confs >= required:
                elapsed = int(time.monotonic() - start)
                print(f"\n  Transaction confirmed! ({confs} confirmations, {elapsed}s elapsed)")
                return
            elapsed = int(time.monotonic() - start)
            print(f"  {confs}/{required} confirmations ({elapsed}s elapsed)   ", end="\r")
        except Exception:
            pass
        time.sleep(poll_interval)


# =========================================================================
#  @p helpers — convert @p strings to integers
# =========================================================================


def patp_to_int(patp: str) -> int:
    """Convert an @p string like ~sampel-palnet to its integer value.

    Uses the @q syllable tables (which are the same as @p for the byte mapping)
    but @p has a different scrambling for galaxies/stars/planets. For comets
    (128-bit), the @p IS the raw integer with syllable encoding.
    """
    # For comets (128-bit @p), the value is simply the decoded syllable bytes.
    # Remove ~ prefix and separators
    p = patp.lstrip("~").replace("--", "-")
    syls = p.split("-")

    raw_bytes = []
    for syl in syls:
        if len(syl) == 6:
            pre, suf = syl[:3], syl[3:]
            raw_bytes.append(PREFIXES.index(pre))
            raw_bytes.append(SUFFIXES.index(suf))
        elif len(syl) == 3:
            raw_bytes.append(SUFFIXES.index(syl))
        else:
            raise ValueError(f"Invalid @p syllable: {syl!r}")

    # @p for comets uses Hoon's @p scrambling — the bytes need to be
    # interpreted differently than @q. For simplicity, we use the @q decoder
    # which gives the raw integer, then apply the @p unscrambling.
    # However, for comet-sized values (128-bit), @p encoding is the same
    # as just putting the bytes in big-endian order with the syllable mapping.
    # Actually @p uses a Feistel cipher for ships ≤ 32 bits. For comets (128 bits),
    # @p is just the raw bytes in big-endian byte order.
    return int.from_bytes(bytes(raw_bytes), "big")


def int_to_patp(atom: int) -> str:
    """Render a comet @p atom as its @p string. Ported from urbit-ob's `patp`.

    The Feistel scramble (`fein`) applies ONLY to planets [2**16, 2**32); it is
    identity for galaxies, stars, moons and comets. Causeway only handles comets
    [2**64, 2**128), so this direct (unscrambled) encoding is exact for our use;
    it must NOT be used for planet-range atoms. Inverse of patp_to_int here."""
    dyx = (atom.bit_length() + 7) // 8  # met(3): byte count
    if dyx <= 1:
        return "~" + SUFFIXES[atom]      # <=1 byte → lone suffix (~zod, ~nec, …)
    dyy = (atom.bit_length() + 15) // 16  # met(4): number of 16-bit words
    res = ""
    t = atom
    for timp in range(dyy):
        word = t & 0xFFFF
        syl = PREFIXES[(word >> 8) & 0xFF] + SUFFIXES[word & 0xFF]
        etc = ("" if timp == 0 else "--") if timp % 4 == 0 else "-"
        res = syl + etc + res
        t >>= 16
    return "~" + res


# =========================================================================
#  Mnemonyms — human-memorable rendering of a Groundwire ID
#
#  A comet's @p (a 128-bit atom) is rendered as a BIP-39-style "mnemonym":
#  the atom split into 11-bit groups indexing a 2048-word list, with a SHA-256
#  checksum, dot-joined, prefixed "." (tweaked/Groundwire) or ".." (untweaked).
#  Reference + wordlist vendored from gwbtc/mnemonyms in ./vendor. The @p stays
#  the canonical machine identity (boot --comet, pier name, proof.json).
# =========================================================================

import importlib.util as _ilu

COMET_STRENGTH = 128
COMET_TWEAKED = True


def _mnemo_dir() -> str:
    """Locate the vendored mnemonyms module + wordlist across dev, installed,
    and frozen (PyInstaller) layouts."""
    candidates = []
    with contextlib.suppress(NameError):
        candidates.append(os.path.join(os.path.dirname(os.path.abspath(__file__)), "vendor"))
    # pip install: setuptools data-files land at sys.prefix/vendor (inside a
    # venv, <venv>/vendor), NOT beside the module in site-packages.  This is
    # the layout the release launcher's venv produces, and it was missing --
    # so a shipped Causeway mined a comet successfully and then crashed
    # RENDERING ITS NAME, one line later.
    candidates.append(os.path.join(sys.prefix, "vendor"))
    # And beside GROUNDWIRE_HOME's source tree, for a by-hand invocation of
    # causeway-src under the release environment.
    if os.environ.get("GROUNDWIRE_HOME"):
        candidates.append(os.path.join(
            os.environ["GROUNDWIRE_HOME"], "causeway-src", "vendor"))
    meipass = getattr(sys, "_MEIPASS", None)
    if meipass:
        candidates.append(os.path.join(meipass, "vendor"))
    if getattr(sys, "frozen", False):
        candidates.append(os.path.join(os.path.dirname(os.path.abspath(sys.executable)), "vendor"))
    for d in candidates:
        if os.path.isfile(os.path.join(d, "mnemonyms-english.txt")):
            return d
    # Fall back to the first candidate for a clear error at open() time.
    return candidates[0] if candidates else "vendor"


_MNEMO_DIR = _mnemo_dir()
_mnemo_wordlist_cache: list[str] | None = None
_mnemo_class = None


def _mnemonym_wordlist() -> list[str]:
    global _mnemo_wordlist_cache
    if _mnemo_wordlist_cache is None:
        path = os.path.join(_MNEMO_DIR, "mnemonyms-english.txt")
        with open(path) as f:
            _mnemo_wordlist_cache = [w for w in f.read().split("\n") if w]
    return _mnemo_wordlist_cache


def _mnemonym_ctx(tweaked: bool = COMET_TWEAKED, strength: int = COMET_STRENGTH):
    global _mnemo_class
    if _mnemo_class is None:
        spec = _ilu.spec_from_file_location("_gw_mnemonyms", os.path.join(_MNEMO_DIR, "mnemonyms.py"))
        mod = _ilu.module_from_spec(spec)
        spec.loader.exec_module(mod)
        _mnemo_class = mod.Mnemonym
    return _mnemo_class(tweaked=tweaked, strength=strength, wordlist=_mnemonym_wordlist())


def atom_to_mnemonym(atom: int, tweaked: bool = COMET_TWEAKED, strength: int = COMET_STRENGTH) -> str:
    """Render a Groundwire ID atom (e.g. a comet @p) as a mnemonym."""
    return _mnemonym_ctx(tweaked, strength).to_nym(atom.to_bytes(strength // 8, "big"))


def mnemonym_to_atom(nym: str, strength: int = COMET_STRENGTH) -> int:
    """Decode a mnemonym back to its ID atom (checksum-verified)."""
    return int(_mnemonym_ctx(COMET_TWEAKED, strength).to_eny(nym.strip()), 16)


def patp_to_mnemonym(patp: str) -> str:
    """Render a comet @p string as its tweaked mnemonym."""
    return atom_to_mnemonym(patp_to_int(patp))


def is_mnemonym(s: str) -> bool:
    return s.strip().startswith(".")


def _split_nym(nym: str) -> tuple[str, list[str]]:
    prefix = ".." if nym.startswith("..") else "."
    return prefix, nym[len(prefix):].split(".")


def abridge_mnemonym(nym: str) -> str:
    """Compact form for tight UI: '.first...last' (mirrors +abridge)."""
    prefix, words = _split_nym(nym)
    if len(words) <= 2:
        return nym
    return f"{prefix}{words[0]}...{words[-1]}"


def foreshorten_mnemonym(nym: str) -> str:
    """Medium form: '.first.second..penultimate.last' (mirrors +foreshorten)."""
    prefix, words = _split_nym(nym)
    if len(words) <= 4:
        return nym
    return f"{prefix}{words[0]}.{words[1]}..{words[-2]}.{words[-1]}"


def resolve_id(entry: str) -> int:
    """Parse a user-supplied Groundwire ID that may be a mnemonym ('.…') or a
    @p ('~…'), returning the atom."""
    entry = entry.strip()
    return mnemonym_to_atom(entry) if is_mnemonym(entry) else patp_to_int(entry)


# =========================================================================
#  Comet miner
# =========================================================================


def run_comet_miner(tweak_expr: str, miner_bin: str) -> dict:
    """
    Invoke comet_miner -c --tweak <hoon-expr> daplyd
    and parse the output for seed, ring, comet, and feed.
    """
    if not os.path.isfile(miner_bin):
        print(f"\n  ERROR: Comet miner not found at: {miner_bin}")
        print(_not_found_hint(miner_bin), end="")
        print("  Build it with `zig build` in comet-miner/, or pass --miner.")
        sys.exit(1)

    cmd = [miner_bin, "-c", "--tweak", tweak_expr, "daplyd"]

    print("\nMining your comet identity (this may take a few minutes)...")
    print()

    try:
        process = subprocess.Popen(
            cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True, bufsize=1
        )
    except PermissionError:
        print(f"  ERROR: Cannot execute comet miner at: {miner_bin}")
        print(f"  Try: chmod +x {miner_bin}")
        sys.exit(1)

    output_lines = []
    for line in process.stdout:
        line = line.rstrip("\n")
        output_lines.append(line)
        # Show progress lines but don't clutter
        if line.startswith("tries:"):
            print(f"  {line}", end="\r")
        else:
            print(f"  {line}")

    process.wait()
    print()  # clear the last \r

    if process.returncode != 0:
        print(f"  ERROR: Comet miner failed (exit code {process.returncode}).")
        print("  Output:")
        for line in output_lines[-10:]:
            print(f"    {line}")
        print()
        print("  Please share the output above when reporting this issue.")
        sys.exit(1)

    # Parse output
    result = {}
    for line in output_lines:
        for key in ("seed", "ring", "comet", "feed"):
            if line.startswith(f"{key}:"):
                result[key] = line.split(":", 1)[1].strip()
                break

    for required in ("comet", "feed"):
        if required not in result:
            print(f"  ERROR: Comet miner did not produce a '{required}' value.")
            print("  Last 10 lines of output:")
            for line in output_lines[-10:]:
                print(f"    {line}")
            print()
            print("  Please share the output above when reporting this issue.")
            sys.exit(1)

    return result


# =========================================================================
#  Comet boot
# =========================================================================


def find_available_port(start: int = 8080, max_tries: int = 100) -> int:
    """Return the first available TCP port starting from *start*."""
    for port in range(start, start + max_tries):
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
            try:
                s.bind(("127.0.0.1", port))
                return port
            except OSError:
                continue
    raise RuntimeError(f"No available port found in range {start}-{start + max_tries - 1}")


def boot_comet(
    comet_name: str, feed: str, vere_bin: str, pill: str = GW_PILL, master_ticket: str = ""
) -> None:
    """Boot a comet in the foreground, replacing this process.

    Prints instructions and opens the browser before handing control to vere.
    Does not return.
    """
    pier_name = comet_name.lstrip("~")

    if not os.path.isfile(vere_bin):
        print(f"\n  ERROR: Urbit runtime not found at: {vere_bin}")
        print(_not_found_hint(vere_bin), end="")
        print("  Build it with `zig build` in vere/, or pass --vere.")
        sys.exit(1)

    if not os.path.isfile(pill):
        print(f"\n  ERROR: Pill file not found at: {pill}")
        print("  Make sure gw-base.pill is in the expected location.")
        sys.exit(1)

    port = find_available_port()
    url = f"http://localhost:{port}"

    # Print instructions before handing off to vere
    print()
    print("  ┌──────────────────────────────────────────────────────────┐")
    print("  │  Your ship is about to boot. This will take a while.    │")
    print("  │                                                          │")
    print("  │  When the boot finishes, type +code in the dojo         │")
    print("  │  to get your login code.                                │")
    print("  │                                                          │")
    print(f"  │  Wallet UI: {url + '/spv-wallet':<46s}│")
    print(f"  │  Landscape: {url + '/apps/landscape':<46s}│")
    print("  └──────────────────────────────────────────────────────────┘")
    print()
    print(f"  To restart later: {vere_bin} {pier_name}")
    print()

    # Try to open browser
    with contextlib.suppress(Exception):
        webbrowser.open(f"{url}/spv-wallet")

    # Replace this process with vere
    cmd = [vere_bin, "-w", pier_name, "-B", pill, "-G", feed, "--http-port", str(port)]
    os.execvp(vere_bin, cmd)


# =========================================================================
#  Main orchestration
# =========================================================================




# =========================================================================
#  Proof.json — off-chain attestation artifact for confidential comets
# =========================================================================


PROOF_SCHEMA_VERSION = 2


def write_proof_json(proof: dict, path: str) -> None:
    """Write a proof dict to `path` as pretty-printed JSON, 0600.

    A proof carries the comet's committed snapshot chain -- sponsor, fief,
    life, rift per hop -- which is choice data no phrase regenerates, and it
    is one half of the identity bundle (the feed is the other).  Written
    0600 like the feed: mode set at open, so there is no window where the
    file exists wider.
    """
    fd = os.open(path, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o600)
    with os.fdopen(fd, "w") as f:
        json.dump(proof, f, indent=2, sort_keys=True)
        f.write("\n")


def load_proof_json(path: str) -> dict:
    """Load a proof dict from `path`."""
    with open(path) as f:
        return json.load(f)


def load_proof_chain(proofs: "tuple[str, ...] | list[str]") -> list[dict]:
    """Load every proof for a point, OLDEST FIRST, and refuse a broken chain.

    Each proof after the spawn records the one it chained off in
    `prior_proof.commit_txid`; a mismatch means the files are out of order or
    one hop is missing.  Both `finalize` and `publish` walk exactly this list —
    the custody log is only as sound as its ordering, and a gap in it is a gap
    in the evidence a stranger walks."""
    chain = [load_proof_json(p) for p in proofs]
    for i in range(1, len(chain)):
        prior = chain[i].get("prior_proof") or {}
        if prior.get("commit_txid") not in (None, "", chain[i - 1].get("commit_txid")):
            raise click.UsageError(
                f"{proofs[i]}: prior_proof.commit_txid does not match {proofs[i - 1]} — "
                "pass the proofs oldest-first, one unbroken chain"
            )
    return chain


def verify_proof_self(proof: dict) -> tuple[bool, str]:
    """Recompute the sat-carrying output key Q from the proof's internal key and
    snapshot, and check it against the stored scriptPubKey.  Returns (ok, reason).
    Does NOT check on-chain existence; see verify_proof_onchain."""
    try:
        internal = bytes.fromhex(proof["internal_pubkey_hex"])
        snapshot = proof["snapshot"]
        expected_spk = bytes.fromhex(proof["sat_script_pubkey_hex"])
    except (KeyError, ValueError) as e:
        return False, f"malformed proof: {e}"
    try:
        q = state_output_key(internal, snapshot)
        computed_spk = bytes([0x51, 0x20]) + q
    except Exception as e:
        return False, f"error recomputing sat output key: {e}"
    if computed_spk != expected_spk:
        return False, (
            f"spk mismatch: computed {computed_spk.hex()}, expected {expected_spk.hex()}"
        )
    # If a leaf hash is recorded, confirm it matches the snapshot too.
    lh = proof.get("leaf_hash_hex")
    if lh is not None:
        want = state_leaf_hash(state_leaf_script(state_commit(snapshot))).hex()
        if lh.lower() != want.lower():
            return False, f"leaf_hash mismatch: stored {lh}, computed {want}"
    return True, "OK"


def verify_proof_onchain(proof: dict, *, mempool_base: str = MEMPOOL_API_URL) -> tuple[bool, str]:
    """Fetch the custody tx from mempool.space and verify its sat output matches the proof."""
    ok, reason = verify_proof_self(proof)
    if not ok:
        return False, reason
    txid = proof.get("commit_txid") or proof.get("sat_txid")
    if not txid:
        return False, "proof has no commit_txid — was the tx broadcast?"
    try:
        r = requests.get(f"{mempool_base}/tx/{txid}", timeout=20)
        r.raise_for_status()
        tx = r.json()
    except Exception as e:
        return False, f"could not fetch tx {txid}: {e}"
    vout_idx = int(proof.get("sat_vout", 0))
    try:
        vout = tx["vout"][vout_idx]
    except (KeyError, IndexError):
        return False, f"tx {txid} has no vout {vout_idx}"
    onchain_spk = vout.get("scriptpubkey", "")
    if onchain_spk.lower() != proof["sat_script_pubkey_hex"].lower():
        return False, (
            f"on-chain spk {onchain_spk} != proof spk {proof['sat_script_pubkey_hex']}"
        )
    confs = (tx.get("status") or {}).get("confirmed", False)
    return True, f"OK ({'confirmed' if confs else 'unconfirmed'})"


# =========================================================================
#  Xpub / key-source parsing
# =========================================================================


_DESCRIPTOR_RE = re.compile(
    r"^(tr|wpkh|pkh|sh)\("
    r"(?:\[([0-9a-fA-F]{8})((?:/[0-9]+[h']?)*)\])?"  # optional key origin; hardened marker is ' or h
    r"([a-zA-Z0-9]+)"                                 # xpub / tpub
    r"(?:/([0-9/*h']+))?"                             # child-derivation pattern
    r"\)(?:#[a-z0-9]+)?$"
)


@dataclasses.dataclass
class KeySource:
    """User-supplied key source. Holds what's needed for address derivation + PSBT metadata."""

    xpub: bip32.HDKey             # at the account-level (e.g. m/86'/0'/0')
    master_fingerprint: bytes     # 4 bytes
    account_path: list[int]       # list of hardened-encoded uint32 (e.g. [0x80000056, 0x80000000, 0x80000000])
    network: str = "main"

    def derive_address(self, change: int, index: int) -> tuple[str, bytes, bytes, str]:
        """Return (address, scriptPubKey, xonly, full_path) for m/<account>/<change>/<index>."""
        sub = self.xpub.derive([change, index])
        xonly = sub.key.xonly()
        addr = script.p2tr(sub.key).address(NETWORKS[self.network])
        spk = script.p2tr(sub.key).data
        full_path = "m/" + "/".join(_path_to_str(self.account_path + [change, index]).split("/")[1:])
        return addr, spk, xonly, full_path


def _path_to_str(path: list[int]) -> str:
    """Convert a list of uint32 (with hardened bit) to a BIP-32 path string."""
    parts = ["m"]
    for n in path:
        if n & 0x80000000:
            parts.append(f"{n & 0x7fffffff}h")
        else:
            parts.append(str(n))
    return "/".join(parts)


def _hardened(n: int) -> int:
    return n | 0x80000000


def parse_key_source(input_str: str, network: str = "main") -> KeySource:
    """Accepts either a bare xpub (in which case BIP-86 path is assumed) or a BIP-380
    output descriptor with origin info like `tr([fingerprint/86h/0h/0h]xpub...)/0/*`."""
    s = input_str.strip()
    if not s:
        raise ValueError("empty key source")

    m = _DESCRIPTOR_RE.match(s)
    if m:
        kind = m.group(1)
        if kind != "tr":
            raise ValueError(f"only taproot (tr) descriptors are supported; got {kind}")
        fpr_hex = m.group(2)
        if not fpr_hex:
            raise ValueError(
                "the descriptor is missing its key origin [fingerprint/derivation]. "
                "A later rekey PSBT must name your wallet's master fingerprint or no "
                "signer (Bitcoin Core included) can match it to your seed, so paste the "
                "FULL output descriptor your wallet exports, e.g. "
                "tr([f3d36842/86h/0h/0h]xpub.../<0;1>/*)"
            )
        origin_path_str = m.group(3) or ""
        xpub_str = m.group(4)
        # child_pattern intentionally ignored — we always derive /<change>/<index> below
        master_fpr = bytes.fromhex(fpr_hex)
        account_path = _parse_path(origin_path_str) if origin_path_str else [_hardened(86), _hardened(0 if network == "main" else 1), _hardened(0)]
        xpub = bip32.HDKey.from_base58(xpub_str)
        return KeySource(xpub=xpub, master_fingerprint=master_fpr, account_path=account_path, network=network)

    # Bare xpub. Reject if it's tpub on mainnet or xpub on testnet (mismatch).
    try:
        xpub = bip32.HDKey.from_base58(s)
    except Exception as e:
        raise ValueError(f"not a valid descriptor or xpub: {e}") from e
    # A bare xpub carries no master fingerprint. Without it a later rekey PSBT names
    # fingerprint 00000000, which no signer (Core included) can match to a seed -- so
    # the comet could never be re-keyed. Require the full descriptor with origin.
    raise ValueError(
        "a bare xpub has no master fingerprint, which a later rekey needs in order to "
        "sign. Paste the FULL output descriptor with origin instead, e.g. "
        "tr([fingerprint/86h/0h/0h]xpub.../<0;1>/*) -- your wallet can export it "
        "(Sparrow: right-click the wallet -> Export Wallet... -> Output Descriptor)."
    )


# =========================================================================
#  UTXO discovery — scan xpub-derived addresses against mempool.space
# =========================================================================


def mempool_get(path: str, *, base: str = MEMPOOL_API_URL) -> list | dict:
    r = requests.get(f"{base}{path}", timeout=20)
    r.raise_for_status()
    return r.json()


def scan_addresses(
    source: KeySource,
    *,
    n_receive: int = 20,
    n_change: int = 10,
    mempool_base: str = MEMPOOL_API_URL,
) -> list[dict]:
    """Walk the first n addresses of change 0 and 1, return a flat list of UTXOs.

    Each entry includes: {address, scriptpubkey, xonly, path, txid, vout, value, status}.
    """
    found: list[dict] = []
    for change, n in ((0, n_receive), (1, n_change)):
        for i in range(n):
            addr, spk, xonly, path = source.derive_address(change, i)
            try:
                utxos = mempool_get(f"/address/{addr}/utxo", base=mempool_base)
            except Exception as e:
                print(f"  warning: failed to fetch UTXOs for {addr}: {e}")
                continue
            for u in utxos:
                found.append({
                    "address": addr,
                    "scriptpubkey": spk,
                    "xonly": xonly,
                    "path": path,
                    "change": change,
                    "index": i,
                    "txid": u["txid"],
                    "vout": u["vout"],
                    "value": u["value"],
                    "confirmed": u.get("status", {}).get("confirmed", False),
                    # The block that created this outpoint. For the UTXO a spawn
                    # is minted from, this is the spawn-opening's start-height —
                    # see resolve_start_height().
                    "height": u.get("status", {}).get("block_height"),
                })
    return found


def parse_outpoint(text: str) -> tuple[str, int]:
    """`<txid>:<vout>` -> (lowercase display-hex txid, vout). Raises ValueError."""
    raw = text.strip()
    txid, sep, vout_s = raw.rpartition(":")
    if not sep:
        raise ValueError("expected <txid>:<vout>")
    txid = txid.strip().lower()
    if len(txid) != 64 or any(c not in "0123456789abcdef" for c in txid):
        raise ValueError(f"{txid!r} is not a 64-hex-character txid")
    try:
        vout = int(vout_s.strip(), 10)
    except ValueError:
        raise ValueError(f"{vout_s.strip()!r} is not a vout index") from None
    if vout < 0:
        raise ValueError("vout must be >= 0")
    return txid, vout


def _describe_utxos(utxos: list[dict]) -> str:
    return "\n".join(
        f"    {u['txid']}:{u['vout']}  {u['value']} sat"
        f"  m/.../{u['change']}/{u['index']}"
        f"{'' if u['confirmed'] else '  (unconfirmed)'}"
        for u in utxos
    )


def pick_utxo_interactive(utxos: list[dict], *, min_value: int = 630,
                          select: str | None = None) -> dict:
    """Render a numbered UTXO list and prompt the user to pick one.

    `select` is the --utxo flag: an explicit `<txid>:<vout>` outpoint, which
    makes the choice headlessly. It must name one of the scanned candidates."""
    if not utxos:
        raise SystemExit("No UTXOs found. Fund one of your xpub's addresses and try again.")
    if select is not None:
        try:
            want_txid, want_vout = parse_outpoint(select)
        except ValueError as e:
            raise SystemExit(f"--utxo {select!r}: {e}")
        for u in utxos:
            if u["txid"].lower() == want_txid and int(u["vout"]) == want_vout:
                if u["value"] < min_value:
                    raise SystemExit(
                        f"--utxo {want_txid}:{want_vout} holds {u['value']} sat; "
                        f"need >= {min_value} for commit + fee."
                    )
                print(f"\n  Using --utxo {want_txid}:{want_vout} "
                      f"({u['value']} sat, m/.../{u['change']}/{u['index']})")
                return u
        raise SystemExit(
            f"--utxo {want_txid}:{want_vout} is not among this wallet's "
            f"{len(utxos)} scanned UTXO(s). Candidates:\n{_describe_utxos(utxos)}"
        )
    print()
    print("  Available UTXOs:")
    print("  ---------------")
    for i, u in enumerate(utxos, 1):
        flag = "" if u["confirmed"] else " (unconfirmed)"
        suitable = u["value"] >= min_value
        marker = " " if suitable else "*"
        print(f"  [{i:2}]{marker}{u['value']:>10} sat  {u['address'][:24]}…  {u['txid'][:12]}…:{u['vout']}  m/.../{u['change']}/{u['index']}{flag}")
    if any(not (u["value"] >= min_value) for u in utxos):
        print(f"\n  (*) UTXO is too small — need >= {min_value} sats for commit + fee.")
    print()
    while True:
        choice = prompt("  Pick a UTXO number: ", what="a UTXO choice",
                        flag="--utxo <txid>:<vout>").strip()
        try:
            idx = int(choice) - 1
            if 0 <= idx < len(utxos):
                if utxos[idx]["value"] < min_value:
                    print(f"  That UTXO is too small ({utxos[idx]['value']} < {min_value}). Pick another.")
                    continue
                return utxos[idx]
        except ValueError:
            pass
        print(f"  Please enter a number 1–{len(utxos)}.")


# =========================================================================
#  Mnemonic (Generate New Wallet mode)
# =========================================================================


def generate_new_mnemonic(*, strength_bits: int = 128) -> str:
    """Generate a fresh BIP-39 mnemonic (12 words for 128 bits, 24 for 256)."""
    entropy = secrets.token_bytes(strength_bits // 8)
    return bip39.mnemonic_from_bytes(entropy)


def mnemonic_to_hdkey(mnemonic: str, passphrase: str = "", network: str = "main") -> bip32.HDKey:
    """BIP-39 → HDKey at root."""
    seed = bip39.mnemonic_to_seed(mnemonic, passphrase)
    return bip32.HDKey.from_seed(seed, version=NETWORKS[network]["xprv"])


def hdkey_fingerprint(root: bip32.HDKey) -> bytes:
    """4-byte master key fingerprint (hash160 of the compressed pubkey, first 4 bytes).

    Uses embit's hash160, which falls back to a pure-Python ripemd160 — unlike
    hashlib.new("ripemd160"), which raises on OpenSSL-3 hosts (Ubuntu 22.04+/
    Debian 12+) that disable the legacy provider, crashing `spawn generate`.
    """
    from embit import hashes as _eh
    compressed = root.key.get_public_key().sec()
    return _eh.hash160(compressed)[:4]


WALLET_SEED_BOX_HEADER = (
    "SAVE THIS — your BIP-39 seed phrase controls your comet identity",
    "and the funds that back it. Write it down. Losing it is fatal.",
)

def print_seed_box(mnemonic: str, *, header: tuple[str, ...] = WALLET_SEED_BOX_HEADER) -> None:
    """Render a mnemonic in a warning box.

    `header` is the text above the words.  (Overridable; the only phrase
    Causeway prints today is the generate-flow wallet seed.)"""
    words = mnemonic.split()
    width = max(len(w) for w in words) + 4
    rows = (len(words) + 2) // 3
    print()
    print("  ┌" + "─" * 68 + "┐")
    for line in header:
        print("  │  " + line.ljust(66) + "│")
    print("  │" + " " * 68 + "│")
    for r in range(rows):
        cells = []
        for c in range(3):
            idx = r + rows * c
            if idx < len(words):
                cells.append(f"{idx+1:>2}. {words[idx]:<{width-4}}")
            else:
                cells.append(" " * width)
        print("  │  " + "  ".join(cells).ljust(66) + "│")
    print("  └" + "─" * 68 + "┘")


def confirm_seed_saved(mnemonic: str, *, label: str = "seed phrase",
                       assume_saved: bool = False) -> None:
    """Make the operator read the phrase back before continuing.

    `assume_saved` (the --assume-saved flag) skips the read-back so a scripted
    spawn can run at all. The phrase is still printed above; a caller that does
    not capture it has lost the identity, hence the shouty note."""
    if assume_saved:
        print()
        print(f"  --assume-saved: skipping the {label} read-back.")
        print("  The phrase is printed above and NOWHERE ELSE. If this run's")
        print("  output is not captured and stored, the comet is unrecoverable.")
        return
    print()
    print(f"  Please re-enter your {label} to confirm you wrote it down")
    print("  (space-separated — spelling must match exactly):")
    while True:
        entry = prompt("  > ", what=f"the {label} confirmation",
                       flag="--assume-saved").strip()
        if entry.lower() == mnemonic.lower():
            print("  Confirmed!")
            return
        print("  That doesn't match. Try again. (Your phrase is above.)")


def mine_comet_from_utxo(
    txid_hex: str,
    vout: int,
    off: int = 0,
    miner_bin: str = COMET_MINER_BIN,
    dom: str = PKI_DOM,
) -> dict:
    """Build the kelvin-9 dat expression for the given funding UTXO and run
    the comet miner.

    dat = (can 0 (mat dom) (mat 9) (mat (jam spawn-sont)) ~) -- fixed the
    moment the funding UTXO is chosen.  The miner varies only the ship's own
    key material to hit the PoW target; nothing the caller holds enters the
    tweak but the satpoint."""
    tweak_expr = make_dat_expr(txid_hex, vout, off, dom)
    return run_comet_miner(tweak_expr, miner_bin)


# =========================================================================
#  Causeway command-line interface (click)
# =========================================================================


@click.group()
@click.version_option("0.1.0")
def cli():
    """Causeway — confidential comet spawn + management via any Bitcoin wallet."""


@cli.group()
def spawn():
    """Spawn a new comet."""


@cli.group()
def proof():
    """Inspect or verify a comet.proof.json file."""


_MGMT_PRIOR_PROOF_HELP = (
    "Path to the prior proof.json for this point (spawn's or last rekey's). "
    "The rekey spends its sat-carrying output key-path — required so the "
    "custody chain (input-0 spends) stays valid."
)


@cli.command("rekey")
@click.option("--point", required=True, help="Target comet — mnemonym or @p")
@click.option("--prior-proof", required=True, type=click.Path(exists=True, dir_okay=False), help=_MGMT_PRIOR_PROOF_HELP)
@click.option("--new-pass-hex", required=True, help="New networking key (pass), hex — derived from your ship's new ring")
@click.option("--breach", is_flag=True, default=False, help="Bump rift (a breach rekey; also bumps life)")
@click.option("--fee-rate", type=int, default=2, show_default=True)
@click.option("--network", type=click.Choice(["main", "testnet"]), default="main", show_default=True)
@click.option("--output-dir", type=click.Path(file_okay=False, dir_okay=True), default=".", show_default=True)
@click.option("--mempool-base", default=MEMPOOL_API_URL, show_default=True)
@click.option("--sponsor", default=None,
              help="Set the sponsor committed by the NEW snapshot (@p or mnemonym). "
                   "Omit to carry the prior snapshot's sponsor forward.")
@click.option("--fief", "fief_arg", default=None, metavar="IP:PORT",
              help="Set the fief committed by the NEW snapshot. Omit to carry the "
                   "prior snapshot's fief forward. This is the ONLY way a comet "
                   "minted without routing can acquire it: a comet with neither "
                   "fief nor sponsor is unreachable and cannot learn a route from "
                   "packets it receives.")
@click.option("--no-route", is_flag=True, default=False,
              help="Allow a new snapshot with no sponsor and no fief (outbound-only). "
                   "Without this, a state update that would strand the comet is refused.")
@click.option("--signed-psbt", default=None, metavar="PATH|-",
              help="Read the signed PSBT (or signed raw transaction) from a file "
                   "(or `-` for stdin) instead of watching the chain / prompting. "
                   "A named pipe works: the unsigned PSBT is written to "
                   "<patp>-rekey.psbt first.")
@click.option("--fund-xpub", default=None,
              help="TOP-UP: taproot xpub or descriptor of a wallet holding sats to "
                   "add to the identity output. Without this a state update pays "
                   "its fee out of the identity sat, which shrinks every time and "
                   "eventually prices the comet out of its own identity.")
@click.option("--fund-utxo", default=None, metavar="TXID:VOUT",
              help="Which --fund-xpub UTXO to spend as the funding input. Omit to "
                   "pick interactively from the scan.")
@click.option("--sat-target", type=int, default=None,
              help="Value the identity output should end up holding, in sats. "
                   "Default: everything the inputs carry after the fee (no change).")
def cmd_rekey(point, prior_proof, new_pass_hex, breach, fee_rate, network, output_dir, mempool_base,
              sponsor, fief_arg, no_route, signed_psbt, fund_xpub, fund_utxo, sat_target):
    """Rotate a comet's messaging key — a state update committed in the sat
    output's taproot tweak. Spends the current sat-carrying UTXO key-path;
    chains off --prior-proof.

    Pass --fund-xpub to add a funding input AFTER the identity sat and top the
    identity output up instead of shrinking it by the fee.
    """
    point = int_to_patp(resolve_id(point))  # accept mnemonym or @p
    new_pass = int.from_bytes(bytes.fromhex(new_pass_hex), "little")
    new_key = messaging_key_from_pass(new_pass)
    _run_rekey_op(point, prior_proof, new_key, breach, fee_rate, network, output_dir, mempool_base,
                  sponsor=sponsor, fief_arg=fief_arg,
                  no_route=no_route, signed_psbt=signed_psbt,
                  fund_xpub=fund_xpub, fund_utxo=fund_utxo, sat_target=sat_target,
                  new_pass=new_pass)


def _resolve_rekey_funding(*, fund_xpub: str | None, fund_utxo: str | None,
                           sat_target: int | None, network: str,
                           mempool_base: str) -> dict:
    """Turn --fund-xpub/--fund-utxo/--sat-target into build_rekey_psbt kwargs.

    Returns {} when no funding was asked for, so an ordinary rekey calls the
    builder exactly as it always did.  Change (needed only when --sat-target
    leaves a remainder) is derived from the same xpub at m/<account>/1/0, the
    same place a spawn puts its change, and is appended LAST so it can never
    displace the sat-carrying output 0.
    """
    if fund_xpub is None:
        if fund_utxo is not None:
            raise SystemExit("--fund-utxo needs --fund-xpub to know whose UTXO it is")
        if sat_target is not None:
            raise SystemExit(
                "--sat-target only means something with a funding input; "
                "pass --fund-xpub (without funding, output 0 is whatever the "
                "identity sat has left after the fee)"
            )
        return {}

    source = parse_key_source(fund_xpub, network=network)
    print("\n  Scanning --fund-xpub for a funding UTXO...")
    utxos = scan_addresses(source, mempool_base=mempool_base)
    utxo = pick_utxo_interactive(utxos, min_value=P2TR_DUST, select=fund_utxo)
    print(f"  Funding input: {utxo['txid']}:{utxo['vout']}  {utxo['value']} sat "
          f"(added AFTER the identity sat, as input 1)")

    kwargs: dict = {
        "funding_inputs": [{
            "txid": utxo["txid"],
            "vout": utxo["vout"],
            "value": utxo["value"],
            "script_pubkey": utxo["scriptpubkey"],
            "xonly": utxo["xonly"],
            "path": utxo["path"],
            "fingerprint": source.master_fingerprint,
        }],
    }
    if sat_target is not None:
        _addr, change_spk, change_xonly, change_path = source.derive_address(1, 0)
        kwargs.update(
            sat_target=int(sat_target),
            change_script_pubkey=change_spk,
            change_internal_xonly=change_xonly,
            change_path=change_path,
        )
    return kwargs


def _run_rekey_op(point: str, prior_proof_path: str, new_key: int, breach: bool, fee_rate: int, network: str, output_dir: str, mempool_base: str,
                  sponsor: str | None = None, fief_arg: str | None = None,
                  no_route: bool = False,
                  signed_psbt: str | None = None,
                  fund_xpub: str | None = None, fund_utxo: str | None = None,
                  sat_target: int | None = None,
                  new_pass: int | None = None) -> None:
    """Spend the point's current sat-carrying output key-path, commit a new
    snapshot (life+1, rift+1 on breach, rotated key), await signed PSBT,
    broadcast, and emit `<patp>-rekey-<txid>.proof.json`.

    Sponsorship and escape are off-chain in kelvin-9; only key rotation / breach
    is an on-chain state update.  The sponsor/fief carry forward from the prior
    snapshot unchanged.

    `fund_xpub`/`fund_utxo` add a funding input AFTER the identity sat so the
    identity output can be topped up rather than shrinking by the fee."""
    print()
    print("=" * 60)
    print(f"  CAUSEWAY — kelvin-9 REKEY for {patp_to_mnemonym(point)}")
    print(f"  (@p {point})")
    print("=" * 60)

    try:
        prior = load_proof_json(prior_proof_path)
    except Exception as e:
        click.echo(click.style(f"  cannot load --prior-proof: {e}", fg="red"))
        sys.exit(1)

    if prior.get("patp") and prior["patp"] != point:
        click.echo(click.style(
            f"  prior proof is for {prior['patp']}, not {point}", fg="red"))
        sys.exit(1)

    prior_snap = prior.get("snapshot") or {}
    sponsor_atom = resolve_sponsor(sponsor)
    new_snapshot = {
        "life": int(prior_snap.get("life", 0)) + 1,   # every change bumps life
        "rift": int(prior_snap.get("rift", 0)) + (1 if breach else 0),
        "key": new_key,
        # --sponsor sets it; otherwise the prior snapshot's carries forward
        "sponsor": sponsor_atom if sponsor_atom is not None else prior_snap.get("sponsor"),
        # --fief sets it; otherwise the prior snapshot's carries forward
        "fief": (parse_fief_arg(fief_arg) if fief_arg is not None
                 else prior_snap.get("fief")),
    }
    # A state update that leaves the comet with neither a sponsor nor a fief
    # strands it from this life onward — refuse unless deliberately opted out.
    assert_routable(new_snapshot, no_route)

    prior_txid = prior.get("commit_txid") or prior.get("sat_txid")
    print(f"\n  Chaining from prior {prior.get('op', '?')} op: "
          f"sat={str(prior_txid)[:16]}..:{prior.get('sat_vout', 0)} "
          f"value={prior.get('sat_value', '?')}  life {prior_snap.get('life','?')}→{new_snapshot['life']}")

    fund_kwargs = _resolve_rekey_funding(
        fund_xpub=fund_xpub, fund_utxo=fund_utxo, sat_target=sat_target,
        network=network, mempool_base=mempool_base,
    )

    psbt_obj, proof = build_rekey_psbt(
        prior_proof=prior,
        new_snapshot=new_snapshot,
        fee_rate=fee_rate,
        network=network,
        **fund_kwargs,
    )
    if proof.get("topped_up_by") is not None:
        delta = proof["topped_up_by"]
        #  Colour by SIGN, not by "was there funding".  This printed green
        #  whenever funding was present, so a rekey that funded 400 sats and
        #  paid 3.380 in fees announced itself in green as though it were a
        #  top-up.  That is the annunciation of the exact failure that once
        #  priced a live comet down to 754 sats.
        click.echo(click.style(
            f"  Identity output: {prior.get('sat_value', '?')} → {proof['sat_value']} sats "
            f"({delta:+d}){'  [TOP-UP]' if delta > 0 else '  [SHRANK — the fee exceeded the funding]'}",
            fg="green" if delta > 0 else "yellow"))
    proof["op"] = "rekey"
    proof["patp"] = point
    #  The pass this rekey rotates TO.  A spawn proof records `pass_atom_hex`
    #  and nothing used to record it again, so after a rotation the comet's
    #  current pass lived only in the ship — and `causeway publish`, which must
    #  publish the pass a peer actually receives, had nowhere to read it from.
    if new_pass is not None:
        proof["pass_atom_hex"] = hex(new_pass)

    os.makedirs(output_dir, exist_ok=True)
    pier = point.lstrip("~")
    psbt_path = os.path.join(output_dir, f"{pier}-rekey.psbt")
    unsigned_b64 = psbt_obj.to_base64()
    with open(psbt_path, "w") as f:
        f.write(unsigned_b64 + "\n")
    print(f"\n  Wrote unsigned PSBT: {psbt_path}")
    click.echo(click.style(
        "\n  Note: this PSBT's input has PSBT_IN_TAP_MERKLE_ROOT set (= the\n"
        "  current snapshot's state leaf hash) so your signer can compute the\n"
        "  taproot key-path tweak. BIP-371 software signers (Sparrow-class,\n"
        "  Core descriptor wallets) support this.",
        fg="yellow",
    ))

    #  The proof is written BEFORE the wallet is asked to sign: the wallet
    #  normally broadcasts too, and a segwit txid is fixed before signing, so
    #  the record can -- and must -- exist before the transaction can land.
    #  It is the durable record the next state update chains off.
    commit_txid = psbt_obj.tx.txid().hex()
    proof["commit_txid"] = commit_txid
    proof_path = os.path.join(output_dir, f"{pier}-rekey-{commit_txid[:12]}.proof.json")
    write_proof_json(proof, proof_path)
    print(f"  Wrote proof: {proof_path}")

    signed_b64 = _await_signed_psbt(unsigned_b64, signed_psbt,
                                    mempool_base=mempool_base, proof_path=proof_path)
    try:
        signed_txid, tx_hex = _extract_tx_from_psbt(signed_b64)
    except Exception as e:
        click.echo(click.style(f"\n  Error extracting signed tx: {e}", fg="red"))
        sys.exit(1)

    #  The signer (or the chain) handed back a transaction; make sure it is
    #  the one the proof above describes before anything is broadcast.
    assert_signed_is_what_we_built(unsigned_b64, signed_b64)

    print("\n  Broadcasting rekey...")
    try:
        broadcast_id = _broadcast_tx(tx_hex, mempool_base=mempool_base)
    except Exception as e:
        click.echo(click.style(f"  Broadcast failed: {e}\n  Signed tx hex: {tx_hex}", fg="red"))
        sys.exit(1)
    print(f"  Broadcast: {tx_link(broadcast_id)}")

    print("\n" + "=" * 60)
    click.echo(click.style(
        f"  REKEY broadcast for {patp_to_mnemonym(point)}.\n"
        f"  Custody chain: {str(prior_txid)[:10]}..:0 → {commit_txid[:10]}..:0\n"
        f"  Use {proof_path} as --prior-proof for the NEXT state update.\n"
        f"  Once it confirms, run\n"
        f"    causeway finalize <spawn.proof.json> {proof_path}\n"
        f"  and paste the `:gw-btc &noun [%gw-custody-entry ...]` line it prints\n"
        f"  into your RUNNING ship's dojo.  The agent re-verifies the whole\n"
        f"  custody log on-chain and only then refreshes your pass through\n"
        f"  jael's %anew -- no reboot, no hand-crafted ring.\n",
        fg="yellow",
    ))


@spawn.command("connect")
@click.option("--xpub", required=True, help="Taproot xpub, zpub, or BIP-380 descriptor (tr([fp/86h/0h/0h]xpub...)/0/*)")
@click.option("--invite", default=None, help="Optional faucet invite code (sends 1000 sats to your wallet)")
@click.option("--fee-rate", type=int, default=2, show_default=True, help="sat/vbyte")
@click.option("--network", type=click.Choice(["main", "testnet"]), default="main", show_default=True)
@click.option("--output-dir", type=click.Path(file_okay=False, dir_okay=True), default=".", show_default=True, help="Directory to write psbt + proof files")
@click.option("--miner", default=COMET_MINER_BIN, show_default=True, help="Path to comet_miner binary")
@click.option("--mempool-base", default=MEMPOOL_API_URL, show_default=True)
@click.option("--publish", is_flag=True, default=False,
              help="Public spawn: add an OP_RETURN publication output (default off = confidential)")
@click.option("--sponsor", default=None,
              help="Sponsor for the initial snapshot (@p or mnemonym). Peers route to a "
                   "confidential comet through the sponsor committed on-chain.")
@click.option("--fief", "fief_arg", default=None, metavar="IP:PORT",
              help="Static endpoint to commit in the initial snapshot. A comet with "
                   "a fief is reachable at exactly this IP:port, so the ship MUST "
                   "actually bind it (boot.sh --ames-port). Required in practice for "
                   "a comet other comets will name as their sponsor.")
@click.option("--no-route", is_flag=True, default=False,
              help="Deliberately mint an UNROUTABLE comet (no sponsor, no fief). "
                   "Outbound-only: no peer will ever be able to contact it first.")
@click.option("--utxo", "utxo_outpoint", default=None, metavar="TXID:VOUT",
              help="Spend this outpoint instead of prompting for one. It must be "
                   "among the UTXOs the xpub scan finds.")
@click.option("--signed-psbt", default=None, metavar="PATH|-",
              help="Read the signed PSBT (or signed raw transaction) from a file "
                   "(or `-` for stdin) instead of watching the chain / prompting. "
                   "A named pipe works: the unsigned PSBT is written to "
                   "<patp>-spawn.psbt first.")
@click.option("--out-feed", "out_feed", default=None, metavar="PATH",
              help="Write the boot feed to this file (0600) instead of printing it. A feed is a private key; an argument lands in shell history and in the ship's argv.")
@click.option("--assume-saved", is_flag=True, default=False,
              help="No-op (there is no phrase to save in this flow). Kept for script compatibility.")
def spawn_connect(xpub, invite, fee_rate, network, output_dir, miner, mempool_base, publish,
                  sponsor, fief_arg, no_route, utxo_outpoint, signed_psbt, out_feed, assume_saved):
    """Spawn using a user-provided wallet (xpub / descriptor). You sign the PSBT externally.

    Your wallet's seed never reaches Causeway. Your identity bundle is the
    proof file + the feed file; back both up like a wallet."""
    run_spawn_connect(xpub, invite, fee_rate, network, output_dir, miner, mempool_base, publish,
                      sponsor=sponsor, fief_arg=fief_arg,
                      no_route=no_route,
                      utxo_outpoint=utxo_outpoint, signed_psbt=signed_psbt,
                      out_feed=out_feed, assume_saved=assume_saved)


@spawn.command("generate")
@click.option("--invite", default=None)
@click.option("--fee-rate", type=int, default=2, show_default=True)
@click.option("--network", type=click.Choice(["main", "testnet"]), default="main", show_default=True)
@click.option("--output-dir", type=click.Path(file_okay=False, dir_okay=True), default=".", show_default=True)
@click.option("--miner", default=COMET_MINER_BIN, show_default=True)
@click.option("--mempool-base", default=MEMPOOL_API_URL, show_default=True)
@click.option("--publish", is_flag=True, default=False,
              help="Public spawn: add an OP_RETURN publication output (default off = confidential)")
@click.option("--sponsor", default=None,
              help="Sponsor for the initial snapshot (@p or mnemonym). Peers route to a "
                   "confidential comet through the sponsor committed on-chain.")
@click.option("--fief", "fief_arg", default=None, metavar="IP:PORT",
              help="Static endpoint to commit in the initial snapshot. A comet with "
                   "a fief is reachable at exactly this IP:port, so the ship MUST "
                   "actually bind it (boot.sh --ames-port). Required in practice for "
                   "a comet other comets will name as their sponsor.")
@click.option("--no-route", is_flag=True, default=False,
              help="Deliberately mint an UNROUTABLE comet (no sponsor, no fief). "
                   "Outbound-only: no peer will ever be able to contact it first.")
@click.option("--resume", is_flag=True, default=False,
              help="Resume a previous spawn: prompt for the seed phrase you already "
                   "wrote down instead of generating a new wallet. Use this if a "
                   "spawn died after you funded the address.")
@click.option("--mnemonic-file", "mnemonic_file", default=None, metavar="PATH",
              help="Headless resume: read the seed phrase from a file. A phrase on "
                   "the command line would land in shell history.")
@click.option("--out-feed", "out_feed", default=None, metavar="PATH",
              help="Write the boot feed to this file (0600) instead of printing it. A feed is a private key; an argument lands in shell history and in the ship's argv.")
@click.option("--assume-saved", is_flag=True, default=False,
              help="Skip the seed-phrase read-back prompts. Scripted runs MUST "
                   "capture this command's output — the generated BIP-39 phrase is "
                   "printed nowhere else, and it controls the coins.")
def spawn_generate(invite, fee_rate, network, output_dir, miner, mempool_base, publish, sponsor, fief_arg,
                   no_route, resume, mnemonic_file, out_feed, assume_saved):
    """Generate a fresh BIP-39 wallet, fund it, spawn, and emit a boot one-liner.

    The generated seed phrase controls the coins; the comet's identity is the
    proof file + the feed file."""
    run_spawn_generate(invite, fee_rate, network, output_dir, miner, mempool_base, publish,
                       sponsor=sponsor, fief_arg=fief_arg, no_route=no_route,
                       resume=resume, mnemonic_file=mnemonic_file,
                       out_feed=out_feed, assume_saved=assume_saved)


@proof.command("show")
@click.argument("path", type=click.Path(exists=True, dir_okay=False))
def proof_show(path):
    """Pretty-print a proof.json file."""
    data = load_proof_json(path)
    click.echo(json.dumps(data, indent=2, sort_keys=True))


@proof.command("verify")
@click.argument("path", type=click.Path(exists=True, dir_okay=False))
@click.option("--onchain/--offline", default=True, help="Check against mempool.space (default) or skip the network check")
@click.option("--mempool-base", default=MEMPOOL_API_URL, show_default=True)
def proof_verify(path, onchain, mempool_base):
    """Verify that a proof.json internally consistent and optionally matches its on-chain commit tx."""
    data = load_proof_json(path)
    if onchain and "commit_txid" in data:
        ok, reason = verify_proof_onchain(data, mempool_base=mempool_base)
    else:
        ok, reason = verify_proof_self(data)
    if ok:
        click.echo(click.style(f"OK — {reason}", fg="green"))
    else:
        click.echo(click.style(f"FAIL — {reason}", fg="red"))
        sys.exit(1)


@cli.command("finalize")
@click.argument("proofs", nargs=-1, required=True, type=click.Path(exists=True, dir_okay=False))
@click.option("--feed", default=None, help="Boot feed (@uw, from the miner) to re-bake with the reveal log")
@click.option("--feed-file", "feed_file", default=None, metavar="PATH",
              help="Read the feed from a file instead of the command line. A feed is a private key; an argument lands in shell history.")
@click.option("--out-feed", "out_feed", default=None, metavar="PATH",
              help="Write the xtr-baked feed to this file (0600) instead of only printing it. This is what boot.sh --feed-file consumes.")
@click.option("--wait/--no-wait", default=True, show_default=True, help="Poll until each commit confirms")
@click.option("--poll-interval", type=int, default=POLL_INTERVAL, show_default=True)
@click.option("--mempool-base", default=MEMPOOL_API_URL, show_default=True)
def cmd_finalize(proofs, feed, wait, poll_interval, mempool_base,
                 feed_file=None, out_feed=None):
    # feed_file/out_feed sit at the end with defaults: click binds options by
    # NAME, so order is free here, and keeping the original prefix intact
    # means anything calling .callback() positionally still works.
    """Bake the kelvin-9 custody log (xtr) into proofs and, optionally, a boot feed.

    Give every proof.json for the point, OLDEST FIRST (spawn first, then each
    rekey). Once each custody tx has confirmed, its block hash is recorded in
    the proof, an xtr entry (with the snapshot opening) is jammed per hop, and —
    if --feed is given — the feed is re-encoded with xtr baked into the ring, so
    the booted ship's pass carries its own attestation. Entry 0 (the spawn)
    additionally names the spawn sat via its spawn-opening. Without
    this step the ship boots fine but serves an empty log until an %anew
    round-trip refreshes it.
    """
    chain = load_proof_chain(proofs)

    entries = []
    for idx, (path, proof) in enumerate(zip(proofs, chain)):
        txid = proof.get("commit_txid")
        if not txid:
            raise click.UsageError(f"{path} has no commit_txid — was its tx broadcast?")
        while True:
            status = mempool_get(f"/tx/{txid}", base=mempool_base).get("status", {})
            if status.get("confirmed"):
                break
            if not wait:
                click.echo(click.style(f"  {txid} unconfirmed — rerun once it confirms", fg="red"))
                sys.exit(1)
            print(f"  waiting for {txid[:16]}… to confirm ({poll_interval}s)", end="\r")
            time.sleep(poll_interval)
        proof["block_hash"] = status["block_hash"]
        proof["block_height"] = status["block_height"]
        height = int(proof["block_height"])

        # Each hop's opening reveals the state committed in its sat output.
        # 33-byte compressed internal key, even parity per BIP-341 lift_x.
        internal_key = int("02" + proof["internal_pubkey_hex"], 16)
        opening = dict(
            internal_key=internal_key,
            snapshot=proof["snapshot"],
            spawn_opening=None,
        )
        # Entry 0 — the spawn — names the sat and its start height.  The
        # satpoint is ALSO in the pass's dat, in plaintext; the verifier
        # requires the two to agree, so this is a restatement plus the one
        # datum dat does not carry (the funding block height).
        if idx == 0:
            f = proof.get("funding", {})
            if not f.get("txid"):
                raise click.UsageError(f"{path} is the spawn but records no funding outpoint")
            # start-height names the FUNDING tx's block, never this spawn tx's
            # (`height`) — that is the transaction the verifier fetches first.
            start_height = resolve_start_height(proof, mempool_base=mempool_base)
            proof["start_height"] = start_height
            proof["funding"]["height"] = start_height
            opening["spawn_opening"] = dict(
                spawn=dict(txid_hex=f["txid"], vout=int(f["vout"]), off=int(f.get("off", 0))),
                start_height=start_height,
            )
        entries.append(dict(txid_hex=txid, height=height, opening=opening))
        print(f"  {os.path.basename(path)}: confirmed in block {height}")

    xtr = build_xtr_atom(entries)
    chain[-1]["xtr_hex"] = hex(xtr)
    for path, proof in zip(proofs, chain):
        write_proof_json(proof, path)
    print(f"  reveal log: {len(entries)} entr{'y' if len(entries) == 1 else 'ies'}, "
          f"xtr = {(xtr.bit_length() + 7) // 8} bytes (recorded in {proofs[-1]})")

    # The in-band route (decisions-addendum 5): a RUNNING ship does not need a
    # new feed at all. Poke its own %gw-btc with the latest entry; the agent
    # re-verifies the whole extended log on-chain and, only then, refreshes the
    # pass through jael's %anew. No reboot, no hand-crafted ring.
    click.echo(click.style(
        "\n  To extend a RUNNING ship's reveal log in place (no reboot), poke\n"
        "  its own %gw-btc with the newest entry — it re-verifies the whole\n"
        "  log on-chain before it will refresh your pass:\n", fg="cyan"))
    print(f"    {format_custody_entry_poke(entries[-1])}")

    if feed_file and feed:
        raise click.UsageError("pass --feed or --feed-file, not both")
    if feed_file:
        feed = read_feed_file(feed_file)

    if feed:
        noun = hoon_cue(decode_uw(feed))
        try:
            (_two, _zero), (comet_p, (rift, ((life, ring_int), _nil))) = noun
        except (TypeError, ValueError):
            raise click.UsageError("--feed does not cue to a boot feed [[2 0] comet rift [[life ring] 0]]")
        new_feed = encode_uw(rebuild_feed(comet_p, rift, life, append_xtr_to_ring(ring_int, xtr)))
        patp = chain[-1].get("patp") or chain[0].get("patp") or "<your-comet>"
        if out_feed:
            written = write_feed_file(out_feed, new_feed)
            print(f"\n  Baked feed written to {written} (0600)")
            print(f"  Boot with:  {boot_sh()} --comet '{patp}' --feed-file {written}")
        else:
            print("\n  Boot with the xtr-baked feed:")
            _print_boot_oneliner(patp, new_feed, proofs[-1])


# =========================================================================
#  publish — the on-chain declassification, over finalize's inputs
# =========================================================================


def _resolve_publication_pass(chain: list[dict], proofs, pass_hex: str | None,
                              prior_snapshot: dict) -> int:
    """The comet's CURRENT pass atom, for a publication to wrap the log around.

    `--pass-hex` wins; otherwise the newest proof in the chain that recorded one.
    Two hex conventions exist in this CLI and both are accepted, told apart by
    the `0x` prefix: proof files store `pass_atom_hex` as a numeric atom
    (`hex(pass_atom)`), while `rekey --new-pass-hex` takes a little-endian byte
    dump.  A misread cannot get far — a pass whose low byte is not 'c' is not a
    suite-C pass and pass_with_xtr refuses it.

    The resolved pass is then checked AGAINST THE CHAIN: its messaging key must
    be the one the latest snapshot commits.  A rekey rotates cry, so a stale
    pass here would publish a packet advertising a key the comet no longer
    uses — verifiable, and wrong, which is the worst combination."""
    if pass_hex is not None:
        text = pass_hex.strip()
        atom = (int(text, 16) if text.lower().startswith("0x")
                else int.from_bytes(bytes.fromhex(text), "little"))
        source = "--pass-hex"
    else:
        found = [(p, pr) for p, pr in zip(proofs, chain) if pr.get("pass_atom_hex")]
        if not found:
            raise click.UsageError(
                "none of these proofs records `pass_atom_hex`, so the pass to "
                "publish is unknown — pass it with --pass-hex (it is the "
                "`pass_atom_hex` a spawn proof records, or the pass whose key "
                "the last rekey committed)"
            )
        path, proof = found[-1]
        atom = int(proof["pass_atom_hex"], 16)
        source = os.path.basename(path)
    if atom & 0xFF != ord("c"):
        raise click.UsageError(
            f"the pass from {source} is not a suite-C pass (low byte "
            f"0x{atom & 0xFF:02x}, expected 0x63 'c') — a comet's pass always "
            f"is.  If this came from --pass-hex, check the hex convention: "
            f"`0x…` is read as an atom, bare hex as a little-endian byte dump."
        )
    want = prior_snapshot.get("key")
    if want is not None and messaging_key_from_pass(atom) != int(want):
        raise click.UsageError(
            f"the pass from {source} is not the comet's current pass: its "
            f"messaging key is not the one the latest snapshot commits.  A "
            f"rekey rotates the key, and the publication must carry the pass a "
            f"peer would actually receive — pass the current one with "
            f"--pass-hex."
        )
    return atom


@cli.command("publish")
@click.argument("proofs", nargs=-1, required=True, type=click.Path(exists=True, dir_okay=False))
@click.option("--fee-rate", type=int, default=2, show_default=True, help="sat/vbyte")
@click.option("--network", type=click.Choice(["main", "testnet"]), default="main", show_default=True)
@click.option("--output-dir", type=click.Path(file_okay=False, dir_okay=True), default=".",
              show_default=True, help="Directory to write psbt + proof files")
@click.option("--mempool-base", default=MEMPOOL_API_URL, show_default=True)
@click.option("--pass-hex", default=None,
              help="The comet's current pass atom, if no proof here records one. "
                   "`0x…` is read as an atom (the proofs' `pass_atom_hex`), bare "
                   "hex as a little-endian byte dump (rekey's --new-pass-hex).")
@click.option("--fund-xpub", default=None,
              help="Taproot xpub / descriptor to take a FUNDING input from, added "
                   "AFTER the identity sat so the fee tops the identity output up "
                   "instead of shrinking it. A packet publication runs ~400 vB and "
                   "an identity sat may not cover it.")
@click.option("--fund-utxo", default=None, metavar="TXID:VOUT",
              help="Use this outpoint as the funding input instead of prompting. "
                   "Must be among the UTXOs the --fund-xpub scan finds.")
@click.option("--sat-target", type=int, default=None,
              help="Value the identity output should end up holding. Needs "
                   "--fund-xpub; the remainder goes to change at m/<account>/1/0, "
                   "appended LAST so it can never displace output 0.")
@click.option("--no-route", is_flag=True, default=False,
              help="Publish even though the snapshot has neither sponsor nor fief.")
@click.option("--signed-psbt", default=None, metavar="PATH|-",
              help="Read the signed PSBT (or signed raw transaction) from a file "
                   "(or `-` for stdin) instead of watching the chain / prompting. "
                   "A named pipe works: the unsigned PSBT is written to "
                   "<patp>-publish.psbt first.")
@click.option("--dry-run", is_flag=True, default=False,
              help="Build the transaction, run every gate and write the unsigned "
                   "PSBT, then stop. Nothing is signed and nothing is broadcast.")
def cmd_publish(proofs, fee_rate, network, output_dir, mempool_base, pass_hex,
                fund_xpub, fund_utxo, sat_target, no_route, signed_psbt, dry_run):
    """Publish a comet's attestation packet on chain — declassify it.

    Give every proof.json for the point, OLDEST FIRST, exactly as `finalize`
    takes them, and run `finalize` first: the custody log this publishes is the
    `xtr_hex` it bakes onto the last proof.

    WHAT GOES ON CHAIN.  One transaction, and it is a state update: input 0
    spends the comet's identity sat (only its holder can, so the publication is
    the OWNER'S consent to declassify), output 0 re-commits the snapshot at
    life+1, and an OP_RETURN carries the comet's WHOLE ATTESTATION PACKET — the
    pass a peer would receive over ames, custody log and all, plus the opening
    for the hop this very transaction performs.  That last opening is the one
    thing the packet cannot contain, because the transaction's txid does not
    exist until it is signed; the watcher completes the log from the block it is
    reading and hands the result to the same +run-checks a mailed attestation
    gets.  So a STRANGER can verify it, and the comet may publish LATE.

    Publishing is one way.  The packet names the comet, its spawn satpoint and
    every custody hop since, in public, forever.
    """
    chain = load_proof_chain(proofs)
    last, last_path = chain[-1], proofs[-1]
    point = last.get("patp") or chain[0].get("patp")

    print()
    print("=" * 60)
    print(f"  CAUSEWAY — kelvin-9 PUBLICATION"
          f"{' for ' + patp_to_mnemonym(point) if point else ''}")
    if point:
        print(f"  (@p {point})")
    print("=" * 60)

    #  The log comes from `finalize`, which is the only thing that resolves each
    #  hop's height on chain.  Without it there is nothing to publish.
    if not last.get("xtr_hex"):
        raise click.UsageError(
            f"{last_path} has no `xtr_hex` — run `causeway finalize "
            f"{' '.join(proofs)}` first.  It resolves each hop's block height "
            f"on chain and bakes the custody log onto the last proof; that log "
            f"IS the publication's payload."
        )
    xtr = int(last["xtr_hex"], 16)
    try:
        log = cue_custody_log(xtr)
    except ValueError as e:
        raise click.UsageError(f"{last_path}: xtr_hex does not read as a custody log ({e})")
    if not log:
        raise click.UsageError(f"{last_path}: the custody log is empty; there is nothing to publish")

    prior_txid = (last.get("commit_txid") or last.get("sat_txid") or "").lower()
    prior_vout = int(last.get("sat_vout", 0))
    if not prior_txid:
        raise click.UsageError(f"{last_path} has no commit_txid — was its tx broadcast?")

    #  GUARD 1 — THE LOG MUST END WHERE THIS TRANSACTION BEGINS.  Input 0 spends
    #  the output the last hop landed on; if the comet has moved its sat since
    #  the log was baked, the artifact is short by exactly those hops and the
    #  packet fails `N-continuity` on chain — after the fee is paid and after
    #  the identity sat has already moved.  Refuse instead.
    if log[-1]["txid_hex"] != prior_txid:
        raise click.UsageError(
            f"the custody log ends at {log[-1]['txid_hex']} @ {log[-1]['height']}, "
            f"but input 0 spends {prior_txid}:{prior_vout} — the log is missing "
            f"the hops in between, and the published packet would fail "
            f"`N-continuity`.  Re-run `causeway finalize` over EVERY proof for "
            f"this point, including the ones minted since."
        )
    if len(log) != len(chain):
        raise click.UsageError(
            f"the custody log has {len(log)} entries but {len(chain)} proofs were "
            f"given — the log was baked from a different set of hops.  Re-run "
            f"`causeway finalize` over exactly these proofs."
        )

    prior_snap = last.get("snapshot") or {}
    pass_atom = _resolve_publication_pass(chain, proofs, pass_hex, prior_snap)

    #  A publication is a state update and nothing more: life+1, everything else
    #  carried forward.  Rotating a key at the same time would be a rekey, and
    #  `causeway rekey` is where that lives.
    new_snapshot = {
        "life": int(prior_snap.get("life", 0)) + 1,
        "rift": int(prior_snap.get("rift", 0)),
        "key": prior_snap.get("key"),
        "sponsor": prior_snap.get("sponsor"),
        "fief": prior_snap.get("fief"),
    }
    assert_routable(new_snapshot, no_route)

    print(f"\n  Custody log: {len(log)} entr{'y' if len(log) == 1 else 'ies'}, "
          f"{log[0]['txid_hex'][:12]}… @ {log[0]['height']} → "
          f"{log[-1]['txid_hex'][:12]}… @ {log[-1]['height']}")
    print(f"  Spending   : {prior_txid[:16]}…:{prior_vout} "
          f"= {last.get('sat_value', '?')} sats")
    print(f"  Life       : {prior_snap.get('life', '?')} → {new_snapshot['life']}")

    #  GUARD 2 — THE TERMINAL OPENING CARRIES NO SPAWN-OPENING.  The spawn opening
    #  may sit on entry 0 only (`spawn-opening-zero` in +run-checks), and entry 0
    #  is inside the xtr above, carrying the real start-height finalize recorded.
    #  This transaction is entry N>0, so its own opening opens nothing.
    pub_pass = pass_with_xtr(pass_atom, xtr)
    pub_opening = {
        "internal_key": int("02" + last["internal_pubkey_hex"], 16),
        "snapshot": new_snapshot,
        "spawn_opening": None,
    }

    #  GUARD 3 — THE CAP, CHECKED BEFORE ANYTHING IS BUILT AND SAID OUT LOUD.
    #  make_publication_script raises over it too, but not until the builder is
    #  half-way through, and not with the hop count that explains why.
    payload = jam_bytes(publication_noun(pub_pass, pub_opening))
    print(f"  Payload    : {len(payload)} bytes of {MAX_PUBLICATION} "
          f"({MAX_PUBLICATION - len(payload)} to spare, ~40 per further hop)")
    if len(payload) > MAX_PUBLICATION:
        raise click.UsageError(
            f"the publication payload is {len(payload)} bytes, over the "
            f"{MAX_PUBLICATION}-byte cap, at {len(log)} custody hops.  The cap is "
            f"the PACKET bound — a publication carries the same packet a peer "
            f"receives over ames — so this comet's log no longer fits in one "
            f"transaction and cannot be published as it stands."
        )

    fund_kwargs = _resolve_rekey_funding(
        fund_xpub=fund_xpub, fund_utxo=fund_utxo, sat_target=sat_target,
        network=network, mempool_base=mempool_base,
    )

    psbt_obj, proof = build_rekey_psbt(
        prior_proof=last,
        new_snapshot=new_snapshot,
        publication_pass_atom=pub_pass,
        publication_opening=pub_opening,
        fee_rate=fee_rate,
        network=network,
        **fund_kwargs,
    )
    proof["op"] = "publish"
    if point:
        proof["patp"] = point
    #  WHICH SHAPE WAS PUBLISHED.  `published` is already set by the builder,
    #  but a boot-pass publication and a packet publication are
    #  indistinguishable in a transaction decode, so WHICH one went out has to
    #  be written down deliberately or the record cannot answer it later.
    proof["published_pass_hex"] = hex(pub_pass)
    proof["published_log_entries"] = len(log)

    if proof.get("topped_up_by") is not None:
        delta = proof["topped_up_by"]
        click.echo(click.style(
            f"  Identity output: {last.get('sat_value', '?')} → {proof['sat_value']} sats "
            f"({delta:+d}){'  [TOP-UP]' if delta > 0 else ''}",
            fg="green" if delta > 0 else "yellow"))
    elif int(proof["sat_value"]) < int(last.get("sat_value", 0)):
        click.echo(click.style(
            f"  Identity output: {last.get('sat_value')} → {proof['sat_value']} sats "
            f"(the fee comes out of the identity sat; --fund-xpub tops it up "
            f"instead)", fg="yellow"))

    #  GUARD 4, FIRST PASS — on the transaction we are about to ask a human to
    #  sign, so a bad payload is caught before a signer ever sees it.
    _assert_publication_output(psbt_obj.tx, xtr=xtr, entries=len(log))
    assert_identity_input_zero(psbt_obj.tx, prior_txid, prior_vout)

    os.makedirs(output_dir, exist_ok=True)
    pier = (point or "comet").lstrip("~")
    psbt_path = os.path.join(output_dir, f"{pier}-publish.psbt")
    unsigned_b64 = psbt_obj.to_base64()
    with open(psbt_path, "w") as f:
        f.write(unsigned_b64 + "\n")
    print(f"\n  Wrote unsigned PSBT: {psbt_path}")
    click.echo(click.style(
        "\n  Note: this PSBT's input 0 has PSBT_IN_TAP_MERKLE_ROOT set (= the\n"
        "  current snapshot's state leaf hash) so your signer can compute the\n"
        "  taproot key-path tweak. BIP-371 software signers (Sparrow-class,\n"
        "  Core descriptor wallets) support this.",
        fg="yellow"))

    if dry_run:
        click.echo(click.style(
            f"\n  --dry-run: every gate passed. Nothing signed, nothing "
            f"broadcast.\n  Publication payload {len(payload)} bytes, "
            f"{len(log)} custody entries, pass carries the log.\n",
            fg="green"))
        return

    click.echo(click.style(
        "\n  PUBLISHING IS ONE WAY. This names the comet, its spawn satpoint\n"
        "  and every custody hop since, in public, forever.",
        fg="red"))

    #  Proof first (see the rekey flow): the wallet may broadcast on its own.
    commit_txid = psbt_obj.tx.txid().hex()
    proof["commit_txid"] = commit_txid
    proof_path = os.path.join(output_dir, f"{pier}-publish-{commit_txid[:12]}.proof.json")
    write_proof_json(proof, proof_path)
    print(f"  Wrote proof: {proof_path}")

    signed_b64 = _await_signed_psbt(unsigned_b64, signed_psbt,
                                    mempool_base=mempool_base, proof_path=proof_path)
    try:
        signed_txid, tx_hex = _extract_tx_from_psbt(signed_b64)
    except Exception as e:
        click.echo(click.style(f"\n  Error extracting signed tx: {e}", fg="red"))
        sys.exit(1)

    #  GUARD 0 — is this even our transaction?  The two guards below are the
    #  strongest in the tool and they still leave a hole: they check input 0
    #  and they check the OP_RETURN, and neither looks at output 0.  A stale
    #  publish PSBT for the SAME comet with the SAME xtr therefore passes
    #  both while paying the identity sat somewhere else entirely.
    assert_signed_is_what_we_built(unsigned_b64, signed_b64)

    #  GUARD 4, SECOND PASS — on the FINAL bytes, after signing and before the
    #  broadcast that cannot be taken back.  The signer returned this
    #  transaction; nothing here trusts that it is the one we handed over.
    signed_tx = Transaction.parse(bytes.fromhex(tx_hex))
    try:
        _assert_publication_output(signed_tx, xtr=xtr, entries=len(log))
        assert_identity_input_zero(signed_tx, prior_txid, prior_vout)
    except ValueError as e:
        click.echo(click.style(
            f"\n  GATE FAILED — NOT BROADCASTING: {e}\n"
            f"  Signed tx hex (unbroadcast): {tx_hex}", fg="red"))
        sys.exit(1)

    print("\n  Broadcasting publication...")
    try:
        broadcast_id = _broadcast_tx(tx_hex, mempool_base=mempool_base)
    except Exception as e:
        click.echo(click.style(
            f"  Broadcast failed: {e}\n  Signed tx hex: {tx_hex}", fg="red"))
        sys.exit(1)
    print(f"  Broadcast: {tx_link(broadcast_id)}")

    print("\n" + "=" * 60)
    click.echo(click.style(
        f"  PUBLICATION broadcast{' for ' + patp_to_mnemonym(point) if point else ''}.\n"
        f"  {len(log)} custody entries went on chain; the watcher completes the\n"
        f"  log with this transaction and runs the same +run-checks a mailed\n"
        f"  attestation gets.\n"
        f"  Once it confirms, run\n"
        f"    causeway finalize {' '.join(proofs)} {proof_path}\n"
        f"  so this hop joins the log too — a publication is a custody move like\n"
        f"  any other, and the next one must carry it.\n",
        fg="yellow"))


def _assert_publication_output(tx, *, xtr: int, entries: int) -> None:
    """Find the OP_RETURN publication among `tx`'s outputs and put it through
    assert_publication_carries_log. Raises ValueError if there is not exactly
    one, or if the one there does not carry the log."""
    found = [o for o in tx.vout
             if parse_publication_script(bytes(o.script_pubkey.data)) is not None]
    if len(found) != 1:
        raise ValueError(
            f"expected exactly one OP_RETURN publication output, found {len(found)}")
    assert_publication_carries_log(
        bytes(found[0].script_pubkey.data), xtr=xtr, entries=entries)


# =========================================================================
#  Spawn flow orchestration — shared between `spawn connect` and `spawn generate`
# =========================================================================


def _build_spawn_psbt_and_proof(
    *,
    utxo: dict,
    source: KeySource,
    snapshot: dict,
    fee_rate: int,
    include_change: bool = True,
    publication_pass_atom: int | None = None,
    publication_opening: dict | None = None,
) -> tuple[psbt.PSBT, dict]:
    """Assemble the kelvin-9 spawn PSBT for a spawn op.

    The funding UTXO's own xonly is the sat-carrying output's internal key, so
    the sat stays key-path-spendable by the point owner for every future custody
    move. If include_change and funding is large enough, the remainder splits
    into a change output at m/<account>/1/0."""
    change_args: dict = {}
    if include_change and utxo["value"] > 2_000:
        change_addr, change_spk, change_xonly, change_path = source.derive_address(1, 0)
        change_args = dict(
            change_internal_xonly=change_xonly,
            change_script_pubkey=change_spk,
            change_path=change_path,
        )

    psbt_obj, proof = build_spawn_psbt(
        utxo_txid=utxo["txid"],
        utxo_vout=utxo["vout"],
        utxo_value=utxo["value"],
        utxo_script_pubkey=utxo["scriptpubkey"],
        funding_internal_xonly=utxo["xonly"],
        funding_path=utxo["path"],
        funding_fingerprint=source.master_fingerprint,
        snapshot=snapshot,
        publication_pass_atom=publication_pass_atom,
        publication_opening=publication_opening,
        fee_rate=fee_rate,
        network=source.network,
        **change_args,
    )
    # Carry the funding tx's block height into the proof: it IS the
    # spawn-opening's start-height (see resolve_start_height), and knowing it
    # here saves finalize a lookup — and saves it entirely if the outpoint is
    # later pruned from the API's view.
    if utxo.get("height"):
        proof["funding"]["height"] = int(utxo["height"])
        proof["start_height"] = int(utxo["height"])
    return psbt_obj, proof


def _load_signed_psbt(spec: str) -> str:
    """--signed-psbt <path|-> -> validated signed-PSBT base64.

    `-` reads stdin to EOF. A path may be a named pipe, which is how a scripted
    run bridges the gap: Causeway writes the unsigned PSBT to disk, an external
    signer picks it up and writes the signed one back into the FIFO."""
    try:
        if spec == "-":
            data = sys.stdin.buffer.read()
        else:
            with open(spec, "rb") as f:          # BINARY: Sparrow's .psbt is raw
                data = f.read()
    except OSError as e:
        raise SystemExit(f"--signed-psbt {spec!r}: {e}")
    if not data.strip():
        raise SystemExit(
            f"--signed-psbt {spec!r}: empty "
            f"({'stdin' if spec == '-' else 'file'} contained nothing)"
        )
    entry = signed_input_to_base64(data)
    if not entry:
        raise SystemExit(
            f"--signed-psbt {spec!r}: not a signed PSBT (binary psbt\\xff, base64 "
            "cHNidP8..., hex 70736274ff...) nor a signed raw transaction (hex 0200...)")
    if not entry.startswith(RAW_TX_PREFIX):
        try:
            psbt.PSBT.from_base64(entry)
        except Exception as e:
            raise SystemExit(f"--signed-psbt {spec!r}: not a valid PSBT ({e})")
    print(f"\n  Read signed PSBT from {'stdin' if spec == '-' else spec}.")
    return entry


def _await_signed_psbt(unsigned_b64: str, signed_psbt: str | None = None, *,
                       mempool_base: str = MEMPOOL_API_URL,
                       poll: int = 20,
                       proof_path: str | None = None) -> str:
    """Hand the unsigned PSBT to the operator's wallet and wait for the
    signed transaction to come back BY EITHER ROUTE:

      * the wallet broadcasts it itself -- the normal case; Sparrow and
        friends sign-and-send in one motion.  A segwit txid is fixed before
        signing, so Causeway already knows what to look for and simply
        watches the chain.  Returns RAW_TX_PREFIX + hex, fetched from the
        network.
      * the wallet can only sign (air-gapped, or the operator prefers it):
        the signed PSBT or signed raw transaction is pasted / loaded here and
        Causeway broadcasts it.  Returns the signed PSBT base64 or
        RAW_TX_PREFIX + hex.

    `signed_psbt` is the --signed-psbt flag (path, or `-` for stdin); when it
    is given nothing is read from the terminal.  Without a terminal and
    without the flag, only the chain is watched.

    `proof_path`: the proof written for this transaction BEFORE the wait
    (it has to exist before the wallet can broadcast, or a crash mid-wait
    would leave a spent sat with no record).  On Ctrl-C / EOF, if the chain
    has not seen the transaction, that proof is removed again so a stale
    record of a never-broadcast tx cannot be picked up as a --prior-proof.
    """
    txid = psbt.PSBT.from_base64(unsigned_b64).tx.txid().hex()
    print()
    print("  Next steps:")
    print("    1. Load the unsigned PSBT below into your Bitcoin wallet (Sparrow, BlueWallet,")
    print("       Passport, Keystone, Coldcard, etc).")
    print("    2. Review, sign, and BROADCAST it from the wallet.")
    print()
    print(f"  Causeway is watching the chain for  {txid}")
    print("  and continues by itself as soon as the network has it.")
    print()
    print("  If your wallet can only sign, paste the signed PSBT (or the signed raw")
    print("  transaction) here instead and Causeway will broadcast it.")
    print()
    print("  Unsigned PSBT:")
    print()
    print(f"    {unsigned_b64}")
    print()
    if signed_psbt is not None:
        return _load_signed_psbt(signed_psbt)

    def _abort() -> None:
        seen = tx_hex_if_seen(txid, mempool_base=mempool_base)
        if seen:
            print(f"  {txid} is on chain; keeping {proof_path or 'the proof'}.", file=sys.stderr)
        elif proof_path and os.path.exists(proof_path):
            os.remove(proof_path)
            print(f"  nothing broadcast; removed {proof_path}", file=sys.stderr)
        raise SystemExit(INTERRUPT_EXIT_CODE)

    tty = _stdin_is_tty()
    if not tty:
        print("  (no terminal: watching the chain only; pass --signed-psbt to hand over a signed PSBT)")
    else:
        print("  Signed PSBT / tx (optional) > ", end="", flush=True)
    try:
        import select as _select
    except ImportError:  # pragma: no cover
        _select = None
    pending = ""
    try:
        while True:
            seen = tx_hex_if_seen(txid, mempool_base=mempool_base)
            if seen:
                print(f"\n  Seen on the network: {tx_link(txid)}")
                return RAW_TX_PREFIX + seen
            if not tty or _select is None:
                time.sleep(poll)
                continue
            try:
                ready, _, _ = _select.select([sys.stdin], [], [], poll)
            except (OSError, ValueError):
                # No select on this stdin (Windows console): fall back to a
                # blocking read; the chain is checked once the paste lands.
                line = sys.stdin.readline()
                if line == "":
                    _abort()
                ready = [True]
                pending += line
            else:
                if not ready:
                    continue
                line = sys.stdin.readline()
                if line == "":
                    _abort()
                pending += line
                # a wrapped paste arrives as several lines in one burst
                while True:
                    more, _, _ = _select.select([sys.stdin], [], [], 0.2)
                    if not more:
                        break
                    extra = sys.stdin.readline()
                    if extra == "":
                        break
                    pending += extra
            entry = signed_input_to_base64(pending.strip().encode())
            if entry:
                return entry
            if pending.strip():
                print("  That doesn't look like a signed PSBT or transaction. Try again --")
                print("  or just broadcast from your wallet; the chain is being watched.")
                print("  Signed PSBT / tx (optional) > ", end="", flush=True)
            pending = ""
    except KeyboardInterrupt:
        print(file=sys.stderr)
        print("causeway: interrupted while waiting for the signed transaction", file=sys.stderr)
        _abort()
    raise AssertionError("unreachable")  # pragma: no cover


def assert_signed_is_what_we_built(unsigned_b64: str, signed_b64: str) -> str:
    """Refuse to broadcast a transaction we did not build. Returns the txid.

    Signing must not change what is being spent or where it goes, and for
    a segwit transaction it cannot change the txid either: the txid
    commits to everything EXCEPT the witness, so the value computed at
    build time survives signing byte for byte. That makes the check one
    comparison, and it is free.

    Without it, every broadcast path except `publish` sent whatever came
    back from the signer. Reproduced with the broadcast stubbed: hand
    `rekey --signed-psbt` a PSBT that sweeps the same identity outpoint
    to an unrelated address and it exits 0, broadcasts, and then writes
    a proof file asserting a `sat_script_pubkey_hex` that is not on
    chain. The proof outlives the transaction -- it is what the next
    state update chains off -- so a wrong one costs the identity, not
    just the sats.

    This does not need a malicious signer. `<patp>-rekey.psbt` is a
    fixed filename that every run overwrites, so a stale or wrong
    `--signed-psbt` reaches exactly the same place.
    """
    want = psbt.PSBT.from_base64(unsigned_b64).tx.txid().hex()
    got, _ = _extract_tx_from_psbt(signed_b64)
    if want != got:
        raise SystemExit(
            "\n  REFUSING TO BROADCAST: the signed transaction is not the one "
            "this tool built.\n"
            f"    built:  {want}\n"
            f"    signed: {got}\n"
            "  A segwit txid does not change when a transaction is signed, so "
            "these differ\n"
            "  only if the inputs or outputs differ. Check you passed the "
            "right --signed-psbt;\n"
            "  the filename is reused across runs and a stale one lands here."
        )
    return got


def _extract_tx_from_psbt(signed_b64: str) -> tuple[str, str]:
    """Finalize and extract raw tx hex from a signed PSBT. Returns (txid, tx_hex).

    Also accepts RAW_TX_PREFIX + hex -- an already-finalized transaction --
    and returns it as-is; there is nothing to extract."""
    if signed_b64.startswith(RAW_TX_PREFIX):
        tx_hex = signed_b64[len(RAW_TX_PREFIX):]
        return Transaction.parse(bytes.fromhex(tx_hex)).txid().hex(), tx_hex
    p = psbt.PSBT.from_base64(signed_b64)
    # Manual finalization for P2TR key-path spends: if taproot_key_sig is set on an input,
    # its witness is just [sig]. embit may not auto-finalize; we do it by hand.
    raw_tx = p.tx
    for i, inp in enumerate(p.inputs):
        if inp.final_scriptwitness is not None:
            raw_tx.vin[i].witness = inp.final_scriptwitness
            continue
        # BIP-371: taproot key-path — PSBT_IN_TAP_KEY_SIG is 0x13 in the unknown map, or on taproot_key_sig attr
        tap_key_sig = getattr(inp, "taproot_key_sig", None)
        if tap_key_sig is None:
            # Fallback: scan raw unknown key-type 0x13
            for k, v in (inp.unknown or {}).items():
                if k == b"\x13":
                    tap_key_sig = v
                    break
        if tap_key_sig is None:
            raise RuntimeError(f"input {i} has no schnorr sig; PSBT not signed")
        raw_tx.vin[i].witness = Witness([tap_key_sig])
    tx_hex = raw_tx.serialize().hex()
    # embit's Transaction.txid() returns the display-order (segwit-stripped)
    # txid directly. The old code called serialize(segwit=False), which embit
    # (>=0.8) rejects — TypeError: write_to() takes no 'segwit' kwarg — so every
    # broadcast path crashed here after signing.
    txid = raw_tx.txid().hex()
    return txid, tx_hex


def tx_hex_if_seen(txid: str, *, mempool_base: str = MEMPOOL_API_URL) -> str | None:
    """The raw hex of `txid` if the network has it (mempool or a block), else
    None.  A 404 is the normal "not yet"; any other failure is also None --
    callers poll, and a blip must not read as an answer."""
    try:
        r = requests.get(f"{mempool_base}/tx/{txid}/hex", timeout=20)
    except requests.RequestException:
        return None
    if r.status_code != 200:
        return None
    body = r.text.strip()
    if not body or any(c not in "0123456789abcdefABCDEF" for c in body):
        return None
    return body


def _broadcast_tx(tx_hex: str, *, mempool_base: str = MEMPOOL_API_URL) -> str:
    """Broadcast, and treat "the network already has it" as success.

    The wallet that signed the transaction is normally the one that
    broadcasts it (Causeway just watches the chain), and a previous run may
    have got this far too.  mempool.space answers those with a 400 whose
    text varies ('Transaction already in block chain',
    'txn-already-in-mempool', ...); rather than pattern-match it, ask the
    chain for the txid, which is known before broadcasting."""
    r = requests.post(f"{mempool_base}/tx", data=tx_hex, timeout=30)
    if r.ok:
        return r.text.strip()
    txid = Transaction.parse(bytes.fromhex(tx_hex)).txid().hex()
    if tx_hex_if_seen(txid, mempool_base=mempool_base):
        return txid
    raise RuntimeError(f"broadcast failed: {r.status_code} {r.text}")


def resolve_start_height(proof: dict, *, mempool_base: str = MEMPOOL_API_URL) -> int:
    """The block height of the transaction that CREATED the spawn satpoint.

    sur/self-attestation.hoon defines a spawn-opening's start-height as "the
    block containing the transaction that CREATED the spawn satpoint" — i.e.
    the FUNDING tx, the one whose output the spawn spends. That is the first
    transaction the verifier fetches, and it can only fetch by [height txid]
    (the light client has no lookup by bare txid), so a wrong height is not a
    cosmetic error: the fetch fails with attestation-tx-not-found and the
    attestation dies with no verdict at all.

    It is emphatically NOT the spawn tx's own height. Defaulting to that made
    every comet Causeway had ever minted unverifiable.

    Order of preference: a start_height already recorded on the proof, the
    funding entry's height (recorded at spawn time from the UTXO scan), then
    mempool.space. Never the spawn height, and never a silent default — if
    the funding height cannot be established, say so and stop.
    """
    for candidate in (proof.get("start_height"), (proof.get("funding") or {}).get("height")):
        # 0 is the pre-broadcast placeholder, not a real funding height.
        if candidate not in (None, "", 0):
            return int(candidate)
    txid = (proof.get("funding") or {}).get("txid")
    if not txid:
        raise click.UsageError(
            "proof carries no funding txid, so the spawn's start-height "
            "(the funding tx's block) cannot be determined"
        )
    status = mempool_get(f"/tx/{txid}", base=mempool_base).get("status", {})
    height = status.get("block_height")
    if not status.get("confirmed") or not height:
        raise click.UsageError(
            f"funding tx {txid} is not confirmed; its block height is the "
            "spawn-opening's start-height and must not be guessed"
        )
    return int(height)


NO_ROUTE_MESSAGE = (
    "this snapshot has neither a sponsor nor a fief, so nothing can "
    "cold-contact the comet: the verifier projects an absent sponsor to SELF "
    "(+urb-point-to-jael), so no peer that has forgotten the identity can ever "
    "find it again. Pass --sponsor <@p or mnemonym>, or pass --no-route if you "
    "really do want an outbound-only identity."
)


def snapshot_is_routable(snapshot: dict) -> bool:
    """Can anything cold-contact a comet in this committed state?

    A confidential comet has no fixed address; peers reach it through the
    sponsor committed in its verified snapshot, or (for static infrastructure)
    through its fief. With NEITHER, it is a one-way identity — legal protocol
    (ops/doc/opret-revision/04-decisions-addendum.md section 2, "Fief scope"), so
    the verifier must never reject it, but not something Causeway will mint by
    accident."""
    return snapshot.get("sponsor") is not None or snapshot.get("fief") is not None


def assert_routable(snapshot: dict, no_route: bool = False) -> None:
    """Refuse to build a spawn / state update that strands the comet."""
    if no_route or snapshot_is_routable(snapshot):
        return
    raise click.UsageError(NO_ROUTE_MESSAGE)


#  Groundwire's own sponsor comet: minted publicly on mainnet (spawn tx
#  48c2ea65... @ 963.104), fief 146.190.199.0:34344 (a reserved IP), run
#  by the project.  It is a CONVENIENCE DEFAULT, never a silent one: both
#  faces show it and say whose it is before anything is spent.
DEFAULT_SPONSOR = "~barmul-bolmet-ronlus-lighul--rovtun-satryc-moclug-daplyd"


def apply_default_sponsor(sponsor: str | None, fief_arg: str | None,
                          no_route: bool) -> tuple[str | None, bool]:
    """The routing default: a spawn that names no sponsor, no fief and did
    not ask for --no-route gets Groundwire's sponsor instead of a refusal.
    Returns (sponsor, defaulted) so callers can SAY so out loud -- a user
    must never discover whose sponsor they got from the chain."""
    if sponsor is None and not fief_arg and not no_route:
        return DEFAULT_SPONSOR, True
    return sponsor, False


def resolve_sponsor(sponsor: str | None) -> int | None:
    """Parse a --sponsor option (mnemonym or @p) into a ship atom."""
    if sponsor is None:
        return None
    sponsor = sponsor.strip()
    if not sponsor:
        return None
    try:
        return resolve_id(sponsor)
    except Exception as e:  # noqa: BLE001
        raise click.UsageError(f"--sponsor {sponsor!r}: {e}")


def boot_sh() -> str:
    """A RUNNABLE path to boot.sh for printed hints.

    `boot.sh` bare is not on anyone's PATH -- printing it produced
    "zsh: command not found: boot.sh" for the first user who pasted the
    hint.  An installed Causeway (GROUNDWIRE_HOME set by the launcher)
    has the real copy next to it; otherwise fall back to the documented
    install location, and only then to the bare name."""
    home = os.environ.get("GROUNDWIRE_HOME", "")
    for base in ([home] if home else []) + [os.path.expanduser("~/.groundwire")]:
        cand = os.path.join(base, "boot.sh")
        if os.path.isfile(cand):
            return cand.replace(os.path.expanduser("~"), "~", 1)
    return "boot.sh"


def write_feed_file(path: str, feed: str) -> str:
    """Write a boot feed to `path` with 0600, and return the path.

    A feed IS the ship's private key.  Passing one as a command-line argument
    puts it in shell history and, worse, in the ship's own argv for as long as
    the pier runs -- `vere -G <feed>` is visible to every local user in `ps`
    for the lifetime of the process.  boot.sh's own log line already redacts
    it; this is the other half of that.

    Writing the file 0600 BEFORE the content lands avoids the window where a
    fresh file is briefly world-readable.
    """
    path = os.path.abspath(path)
    os.makedirs(os.path.dirname(path) or ".", exist_ok=True)
    fd = os.open(path, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o600)
    with os.fdopen(fd, "w") as f:
        f.write(feed.strip() + "\n")
    return path


def read_feed_file(path: str) -> str:
    """Read a feed written by write_feed_file, refusing an empty one."""
    with open(path) as f:
        feed = f.read().strip()
    if not feed:
        raise click.UsageError(f"--feed-file {path!r} is empty")
    if not feed.startswith("0w"):
        raise click.UsageError(
            f"--feed-file {path!r} does not contain a @uw feed (expected 0w...)")
    return feed


def _normalize_mnemonic(raw: str) -> str:
    words = raw.strip().lower().split()
    if len(words) not in (12, 15, 18, 21, 24):
        raise click.UsageError(
            f"that is {len(words)} words; a BIP-39 phrase is 12, 15, 18, 21 or 24")
    return " ".join(words)


def read_mnemonic_file(path: str) -> str:
    """Read a seed phrase from a file (headless resume).  The file, not an
    argument: a seed phrase on a command line lands in shell history."""
    with open(path) as f:
        phrase = _normalize_mnemonic(f.read())
    # Validate the checksum NOW, with a named error -- mnemonic_to_hdkey would
    # throw embit's own exception several lines later.
    try:
        mnemonic_to_hdkey(phrase, network="main")
    except Exception as e:  # noqa: BLE001
        raise click.UsageError(f"--mnemonic-file {path!r}: not a valid BIP-39 phrase ({e})")
    return phrase


def prompt_existing_mnemonic() -> str:
    """Interactive resume: re-enter the phrase from a previous run.

    Exists because a spawn can die AFTER the wallet is funded (the first live
    one did), and re-running `spawn generate` mints a FRESH wallet -- leaving
    the previous run's sats at an address the new run never looks at.
    Resuming with the same phrase finds the same funded UTXO; the comet
    minted from it is fresh (mining is seeded from system entropy) -- which
    is fine, since a spawn that died before broadcast minted nothing.
    """
    print("\n  Resuming a previous spawn: enter the seed phrase you wrote down.")
    while True:
        phrase = input("  > ")
        try:
            phrase = _normalize_mnemonic(phrase)
            mnemonic_to_hdkey(phrase, network="main")
            return phrase
        except click.UsageError as e:
            print(f"  {e.message}. Try again.")
        except Exception:  # noqa: BLE001
            print("  Not a valid BIP-39 phrase (checksum failed). Check the words and try again.")


def qr_ascii(text: str) -> str:
    """Render `text` as a QR code in unicode half-blocks -- one char per
    module wide, half a line tall, compact enough for a TUI panel.

    Funding an address is the one moment when a phone wallet is most likely
    to be the sender, and a QR needs no clipboard at all -- which matters
    because a full-screen TUI captures the mouse, so select-to-copy silently
    stops working at exactly that screen.
    """
    import qrcode
    qr = qrcode.QRCode(border=1, error_correction=qrcode.constants.ERROR_CORRECT_L)
    qr.add_data(text)
    qr.make(fit=True)
    m = qr.get_matrix()
    if len(m) % 2:
        m = m + [[False] * len(m[0])]
    out = []
    for y in range(0, len(m), 2):
        row = []
        for x in range(len(m[0])):
            top, bot = m[y][x], m[y + 1][x]
            row.append("█" if top and bot else "▀" if top else "▄" if bot else " ")
        out.append("".join(row))
    return "\n".join(out)


def require_miner(miner: str) -> None:
    """Refuse to start a spawn whose miner does not exist.

    Called BEFORE the wallet is generated and before the user is asked to send
    money.  The first live mint failed the other way around: seed phrase
    written down, address funded, 1100 sats confirmed -- and THEN the miner
    path was found to be wrong.  Everything that can fail for environmental
    reasons must fail before anything costs the user ink or sats.
    """
    if os.path.isfile(miner) and os.access(miner, os.X_OK):
        return
    raise SystemExit(
        f"  ERROR: comet miner not found at: {miner}\n"
        + _not_found_hint(miner)
        + "  From a release install, run causeway via the launcher\n"
        + "  (~/.groundwire/causeway), which points it at the bundled miner.\n"
        + "  From a dev checkout, `zig build` in comet-miner/, or pass --miner.\n"
        + "  Nothing has been generated, funded or spent."
    )


def _initial_snapshot(pass_atom: int, sponsor: int | None = None,
                      fief: tuple | None = None) -> dict:
    """The snapshot a fresh spawn commits: life 1, rift 0, messaging key from
    the mined pass, and (in kelvin-9) an optional sponsor and fief that carry
    in the snapshot; an absent sponsor projects to self-sponsorship.

    `fief` is the $fief noun from +parse_fief_arg, not a string.  It was
    hardcoded None here until it wasn't: a comet could only acquire a static
    endpoint by spending a SECOND on-chain transaction (`causeway rekey
    --fief`), which mattered most for the one identity that always needs one
    -- a sponsor, since peers reach a confidential comet through its
    sponsor's fief."""
    return {
        "life": 1,
        "rift": 0,
        "key": messaging_key_from_pass(pass_atom),
        "sponsor": sponsor,
        "fief": fief,
    }


def _spawn_publication_opening(internal_xonly: bytes, snapshot: dict, utxo: dict) -> dict:
    """The opening a PUBLIC spawn publishes on-chain: reveals the snapshot and
    names the spawn sat (already in the pass's dat) plus its start height.

    start_height is the FUNDING transaction's block, and it is REQUIRED.  It
    used to be written as 0 on the theory that a spawn publication rides the
    transaction it describes and so cannot know its own block — but
    start-height names the funding tx, which is a confirmed PARENT of this
    transaction and is therefore known at build time.  It matters now because a
    publication is a complete attestation packet: a watcher fetches that
    transaction by [height txid] before it can walk anything, and the light
    client has no lookup by bare txid.  A 0 here is a comet nobody can verify,
    failing hours later on somebody else's machine.
    """
    height = utxo.get("height")
    if not height:
        raise ValueError(
            "publish: the funding UTXO has no confirmed block height, so the "
            "spawn's start-height is unknown; wait for it to confirm (or spawn "
            "confidentially and publish later)"
        )
    return {
        "internal_key": int.from_bytes(b"\x02" + internal_xonly, "big"),
        "snapshot": snapshot,
        "spawn_opening": {
            "spawn": {"txid_hex": utxo["txid"], "vout": utxo["vout"], "off": 0},
            "start_height": int(height),
        },
    }


def _finish_spawn_proof(
    proof: dict,
    *,
    comet: str,
    pass_atom: int,
    utxo: dict,
    peer_discovery: bool = True,
) -> None:
    """Attach the kelvin-9 spawn bookkeeping to a freshly built spawn proof.

    dat is the plaintext spawn satpoint (plus domain and kelvin), so the
    proof records the satpoint and the dat it produces, and nothing secret:
    the identity's only secret is the ring, which lives in the feed."""
    proof["op"] = "spawn"
    proof["patp"] = comet
    proof["dom"] = PKI_DOM
    proof["kelvin"] = KELVIN
    proof["pass_atom_hex"] = hex(pass_atom)
    proof["spawn_sont"] = {"txid_hex": utxo["txid"], "vout": utxo["vout"], "off": 0}
    proof["dat_hex"] = hex(build_dat_atom(utxo["txid"], utxo["vout"], 0))
    #  boot.sh reads this after boot to enable (or not) %gevulot peer
    #  discovery.  Absent is treated as on, so this only ever turns it OFF.
    proof["peer_discovery"] = peer_discovery


def run_spawn_connect(xpub_str: str, invite: str | None, fee_rate: int, network: str, output_dir: str, miner: str, mempool_base: str, publish: bool = False,
                      sponsor: str | None = None, fief_arg: str | None = None,
                      no_route: bool = False,
                      utxo_outpoint: str | None = None, signed_psbt: str | None = None,
                      out_feed: str | None = None,
                      assume_saved: bool = False) -> None:
    print()
    print("=" * 60)
    print(f"  CAUSEWAY — {'Public' if publish else 'Confidential'} Comet Spawn (Connect Wallet)")
    print("=" * 60)

    # Routing, checked BEFORE any faucet / scan / mining work: an unroutable
    # comet must be refused up front, not after a proof-of-work search and a
    # broadcast the user cannot take back.
    sponsor, defaulted = apply_default_sponsor(sponsor, fief_arg, no_route)
    if defaulted:
        click.echo(click.style(
            f"\n  No sponsor given: using Groundwire's default sponsor\n"
            f"    {DEFAULT_SPONSOR}\n"
            f"  (pass --sponsor to choose your own, or --no-route for none)",
            fg="yellow"))
    sponsor_atom = resolve_sponsor(sponsor)
    try:
        parsed_fief = parse_fief_arg(fief_arg)
    except ValueError as e:
        raise click.UsageError(f"--fief: {e}")
    assert_routable({"sponsor": sponsor_atom, "fief": parsed_fief}, no_route)
    require_miner(miner)

    source = parse_key_source(xpub_str, network=network)
    print(f"\n  Parsed key source: network={source.network}, account={_path_to_str(source.account_path)}")
    print(f"  Master fingerprint: {source.master_fingerprint.hex()}")
    if source.master_fingerprint == b"\x00\x00\x00\x00":
        click.echo(click.style(
            "\n  WARNING: no master fingerprint. Provide a BIP-380 descriptor\n"
            "  (e.g. tr([abcd1234/86h/0h/0h]xpub...)/0/*) so your signer can match keys.",
            fg="yellow",
        ))

    # Optionally fund via faucet (we show the first receive address).
    first_addr, _spk, _xonly, _p = source.derive_address(0, 0)
    if invite:
        print(f"\n  Requesting 1000 sats from faucet → {first_addr}")
        fxid = request_faucet(first_addr, invite=invite)
        if fxid:
            print(f"  Faucet sent: {tx_link(fxid)}")
        else:
            print("  Faucet failed (continuing anyway — you can fund manually).")

    print("\n  Scanning for UTXOs...")
    utxos = scan_addresses(source, mempool_base=mempool_base)
    utxo = pick_utxo_interactive(utxos, select=utxo_outpoint)

    print(f"\n  Mining comet (kelvin-9 dat) from ({utxo['txid']}:{utxo['vout']},0)...")
    miner_result = mine_comet_from_utxo(utxo["txid"], utxo["vout"], 0, miner)
    comet = miner_result["comet"]
    feed = miner_result["feed"]
    ring_uw = miner_result.get("ring", "")
    print(f"  Mined: {patp_to_mnemonym(comet)}")
    print(f"         @p {comet}")

    pass_atom = derive_pass_from_ring(ring_uw)
    snapshot = _initial_snapshot(pass_atom, sponsor_atom, parsed_fief)
    # Re-check against the real snapshot (belt-and-braces: the early check
    # above is what saves the user's time, this one is what saves the comet).
    assert_routable(snapshot, no_route)

    pub_pass = pub_opening = None
    if publish:
        pub_pass = pass_atom
        pub_opening = _spawn_publication_opening(utxo["xonly"], snapshot, utxo)

    psbt_obj, proof = _build_spawn_psbt_and_proof(
        utxo=utxo,
        source=source,
        snapshot=snapshot,
        fee_rate=fee_rate,
        publication_pass_atom=pub_pass,
        publication_opening=pub_opening,
    )
    _finish_spawn_proof(proof, comet=comet, pass_atom=pass_atom, utxo=utxo)

    os.makedirs(output_dir, exist_ok=True)
    pier = comet.lstrip("~")
    psbt_path = os.path.join(output_dir, f"{pier}-spawn.psbt")
    proof_path = os.path.join(output_dir, f"{pier}-spawn.proof.json")

    unsigned_b64 = psbt_obj.to_base64()
    with open(psbt_path, "w") as f:
        f.write(unsigned_b64)
        f.write("\n")
    print(f"\n  Wrote unsigned PSBT: {psbt_path}")

    #  Proof AND feed on disk before the wallet is asked to sign.  The wallet
    #  normally broadcasts as well, and from that moment the sat is spent:
    #  the proof is the record of what was committed and the feed is the
    #  comet's private key -- neither is regenerable, and a crash between
    #  the wallet's send and our noticing it must not lose either.
    commit_txid = psbt_obj.tx.txid().hex()
    proof["commit_txid"] = commit_txid
    write_proof_json(proof, proof_path)
    print(f"  Wrote proof: {proof_path}")
    if out_feed:
        print(f"  Wrote feed:  {write_feed_file(out_feed, feed)} (0600, NOT yet xtr-baked)")

    signed_b64 = _await_signed_psbt(unsigned_b64, signed_psbt,
                                    mempool_base=mempool_base, proof_path=proof_path)
    try:
        signed_txid, tx_hex = _extract_tx_from_psbt(signed_b64)
    except Exception as e:
        click.echo(click.style(f"\n  Error extracting signed tx: {e}", fg="red"))
        sys.exit(1)

    assert_signed_is_what_we_built(unsigned_b64, signed_b64)

    print("\n  Broadcasting spawn...")
    try:
        broadcast_id = _broadcast_tx(tx_hex, mempool_base=mempool_base)
    except Exception as e:
        click.echo(click.style(f"  Broadcast failed: {e}\n  Signed tx hex: {tx_hex}", fg="red"))
        sys.exit(1)
    print(f"  Broadcast: {tx_link(broadcast_id)}")

    print("\n  Your identity bundle is the proof file + the feed file. Back both up.")
    _print_spawn_next_steps(comet, feed, proof_path, out_feed)


def run_spawn_generate(invite: str | None, fee_rate: int, network: str, output_dir: str, miner: str, mempool_base: str, publish: bool = False,
                       sponsor: str | None = None, fief_arg: str | None = None, no_route: bool = False, out_feed: str | None = None,
                       resume: bool = False, mnemonic_file: str | None = None,
                       assume_saved: bool = False) -> None:
    print()
    print("=" * 60)
    print(f"  CAUSEWAY — {'Public' if publish else 'Confidential'} Comet Spawn (Generate New Wallet)")
    print("=" * 60)

    # Refuse an unroutable mint before generating a wallet or asking for funds.
    sponsor, defaulted = apply_default_sponsor(sponsor, fief_arg, no_route)
    if defaulted:
        click.echo(click.style(
            f"\n  No sponsor given: using Groundwire's default sponsor\n"
            f"    {DEFAULT_SPONSOR}\n"
            f"  (pass --sponsor to choose your own, or --no-route for none)",
            fg="yellow"))
    sponsor_atom = resolve_sponsor(sponsor)
    try:
        parsed_fief = parse_fief_arg(fief_arg)
    except ValueError as e:
        raise click.UsageError(f"--fief: {e}")
    assert_routable({"sponsor": sponsor_atom, "fief": parsed_fief}, no_route)
    require_miner(miner)

    if mnemonic_file:
        mnemonic = read_mnemonic_file(mnemonic_file)
        print("\n  Resuming with the wallet from --mnemonic-file.")
    elif resume:
        mnemonic = prompt_existing_mnemonic()
    else:
        mnemonic = generate_new_mnemonic(strength_bits=128)
    if not (resume or mnemonic_file):
        print_seed_box(mnemonic)
        confirm_seed_saved(mnemonic, assume_saved=assume_saved)

    root = mnemonic_to_hdkey(mnemonic, network=network)
    account_path = [_hardened(86), _hardened(0 if network == "main" else 1), _hardened(0)]
    account_xpub = root.derive(_path_to_str(account_path))
    fpr = hdkey_fingerprint(root)
    source = KeySource(xpub=account_xpub, master_fingerprint=fpr, account_path=account_path, network=network)

    first_addr, first_spk, first_xonly, first_path = source.derive_address(0, 0)
    print(f"\n  First receive address: {first_addr}")

    if invite:
        print("\n  Requesting 1000 sats from faucet...")
        fxid = request_faucet(first_addr, invite=invite)
        if fxid:
            print(f"  Faucet sent: {tx_link(fxid)}")
        else:
            print("  Faucet failed — please fund the address manually.")
    else:
        print(f"\n  Please send >= {REQUIRED_SATS} sats to the above address.")

    print("\n  Polling mempool.space for confirmation...")
    while True:
        utxos = scan_addresses(source, n_receive=5, n_change=2, mempool_base=mempool_base)
        confirmed = [u for u in utxos if u["confirmed"]]
        if confirmed:
            utxo = max(confirmed, key=lambda u: u["value"])
            print(f"  Found UTXO: {utxo['value']} sat at {utxo['txid']}:{utxo['vout']}")
            break
        print(f"  No confirmed UTXO yet ({len(utxos)} unconfirmed). Sleeping {POLL_INTERVAL}s...", end="\r")
        time.sleep(POLL_INTERVAL)

    print("\n  Mining comet (kelvin-9 dat)...")
    miner_result = mine_comet_from_utxo(utxo["txid"], utxo["vout"], 0, miner)
    comet = miner_result["comet"]
    feed = miner_result["feed"]
    ring_uw = miner_result.get("ring", "")
    print(f"  Mined: {patp_to_mnemonym(comet)}")
    print(f"         @p {comet}")

    pass_atom = derive_pass_from_ring(ring_uw)
    snapshot = _initial_snapshot(pass_atom, sponsor_atom, parsed_fief)
    # Re-check against the real snapshot (belt-and-braces: the early check
    # above is what saves the user's time, this one is what saves the comet).
    assert_routable(snapshot, no_route)

    pub_pass = pub_opening = None
    if publish:
        pub_pass = pass_atom
        pub_opening = _spawn_publication_opening(utxo["xonly"], snapshot, utxo)

    psbt_obj, proof = _build_spawn_psbt_and_proof(
        utxo=utxo,
        source=source,
        snapshot=snapshot,
        fee_rate=fee_rate,
        publication_pass_atom=pub_pass,
        publication_opening=pub_opening,
    )
    _finish_spawn_proof(proof, comet=comet, pass_atom=pass_atom, utxo=utxo)

    # Sign the PSBT in-process (we have the seed).
    p_signed = psbt_obj
    p_signed.sign_with(root)
    signed_b64 = p_signed.to_base64()
    commit_txid, tx_hex = _extract_tx_from_psbt(signed_b64)

    proof["commit_txid"] = commit_txid
    os.makedirs(output_dir, exist_ok=True)
    pier = comet.lstrip("~")
    proof_path = os.path.join(output_dir, f"{pier}-spawn.proof.json")
    write_proof_json(proof, proof_path)

    print("\n  Broadcasting spawn...")
    try:
        broadcast_id = _broadcast_tx(tx_hex, mempool_base=mempool_base)
    except Exception as e:
        click.echo(click.style(f"  Broadcast failed: {e}\n  Signed tx hex: {tx_hex}", fg="red"))
        sys.exit(1)
    print(f"  Broadcast: {tx_link(broadcast_id)}")

    print("\n  WRITE DOWN YOUR SEED PHRASE AGAIN — last chance.")
    print("  It controls the funds. Your comet's identity is the proof file +")
    print("  the feed file; back those up too:")
    print_seed_box(mnemonic)
    confirm_seed_saved(mnemonic, assume_saved=assume_saved)

    _print_spawn_next_steps(comet, feed, proof_path, out_feed)


def _print_spawn_next_steps(comet: str, feed: str, proof_path: str,
                            out_feed: str | None = None) -> None:
    """What to do after a spawn broadcasts.  Deliberately NOT a boot command.

    This used to print a copy-pasteable `boot.sh ... --feed <feed>` line using
    the RAW miner feed, with the "now run causeway finalize" note underneath
    it.  A copy-pasteable command is for copying, so the happy path handed the
    user a way to boot an identity with an EMPTY custody log -- right @p, right
    life, and no peer can ever verify it -- after they had already spent real
    sats.  boot.sh's own docs warn about exactly this.

    The boot command now comes from `causeway finalize`, which is the first
    point at which the correct feed exists.
    """
    pier = comet.lstrip("~")
    print("\n" + "=" * 60)
    print("  SPAWN BROADCAST")
    print("=" * 60)
    print(f"\n  Your comet: {patp_to_mnemonym(comet)}")
    print(f"  @p:         {comet}")
    print(f"  Proof:      {proof_path}")
    if out_feed:
        written = write_feed_file(out_feed, feed)
        print(f"  Feed:       {written} (0600, NOT yet xtr-baked)")
    else:
        print(f"  Feed atom:  {feed[:52]}{'...' if len(feed) > 52 else ''}")
    click.echo(click.style(
        "\n  You CAN boot this feed now, but peers cannot verify you yet.\n"
        "  The ship would come up with the right name and can reach out --\n"
        "  but its pass (the ID it shows peers) does not yet carry the\n"
        "  on-chain evidence for that name: the custody log. Peers who hear\n"
        "  from an unproven comet quietly ignore it rather than trust it.\n"
        "\n"
        "  Finalize fixes that: it waits for the spawn tx to confirm, then\n"
        "  bakes the evidence into the feed -- so a ship booted from the\n"
        "  BAKED feed is verifiable from its very first packet. (A running\n"
        "  ship can also be handed the evidence later, with a\n"
        "  %gw-custody-entry poke; it then re-checks the chain itself, which\n"
        "  needs its own light client synced first -- about an hour.)\n",
        fg="yellow"))
    print("  Next, once the spawn transaction confirms (~10-60 min):")
    if out_feed:
        print(f"    causeway finalize {proof_path} \\")
        print(f"      --feed-file {out_feed} --out-feed {out_feed}.baked")
        print(f"    {boot_sh()} --comet '{comet}' --feed-file {out_feed}.baked")
    else:
        print(f"    causeway finalize {proof_path} --feed <the feed above> \\")
        print(f"      --out-feed ./{pier}.feed")
        print(f"    {boot_sh()} --comet '{comet}' --feed-file ./{pier}.feed")
    print()


def _print_boot_oneliner(comet: str, feed: str, proof_path: str) -> None:
    pier = comet.lstrip("~")
    print("\n" + "=" * 60)
    print("  SPAWN COMPLETE")
    print("=" * 60)
    print(f"\n  Your comet: {patp_to_mnemonym(comet)}")
    print(f"  @p:         {comet}")
    print(f"  Feed atom: {feed[:60]}{'...' if len(feed) > 60 else ''}")
    print(f"  Proof:     {proof_path}")
    print()
    print("  To boot (the runtime uses the @p as the machine form of your ID):")
    print(f"    curl -fsSL https://groundwire.io/causeway/boot.sh | \\")
    print(f"      bash -s -- --comet '{comet}' --feed {feed} --proof {proof_path}")
    print()
    click.echo(click.style(
        "  NOTE: The --proof argument is preserved for future runtime support.\n"
        "  The runtime does not yet consume confidential-comet proofs; your\n"
        "  comet will boot and claim its identity, but other ships will only\n"
        "  be able to verify the identity once runtime proof-ingest lands.\n",
        fg="yellow",
    ))
    click.echo(click.style(
        "  Once the spawn tx confirms, bake the custody log (xtr) into your\n"
        "  boot feed so peers can verify you on first contact (kelvin-9 §5):\n"
        f"    causeway finalize {proof_path} --feed {feed[:24]}…\n",
        fg="cyan",
    ))


# =========================================================================
#  Entry points
# =========================================================================


def main():
    cli()


if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        print("\n\nInterrupted. You can re-run to resume.")
        os._exit(130)
