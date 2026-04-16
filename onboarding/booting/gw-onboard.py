#!/usr/bin/env python3
"""
gw-onboard.py — Groundwire comet onboarding script

Generates a random @q master ticket, derives a taproot address,
watches Bitcoin Core for funding, mines a comet with the correct tweak,
boots it, and directs the user to the spv-wallet interface.
"""

import argparse
import contextlib
import json
import os
import platform
import secrets
import shutil
import socket
import subprocess
import sys
import threading
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
from embit import bip32, ec, script
from embit.networks import NETWORKS
from embit.transaction import Transaction, TransactionInput, TransactionOutput, Witness

# ---------------------------------------------------------------------------
# Configuration defaults (overridable via CLI)
# ---------------------------------------------------------------------------

RPC_URL = "https://alpha.groundwire.dev/rpc"
RPC_USER = "mainnetrpcuser"
RPC_PASS = "fc3d36ce83e15484e75a658b2a9a8a90a66f4cb017ace74c8631fe082b93adbf"


def _detect_zig_target() -> str:
    """Map the current platform to its Zig target triple for binary paths."""
    machine = platform.machine().lower()
    system = platform.system().lower()
    arch = {"x86_64": "x86_64", "amd64": "x86_64", "aarch64": "aarch64", "arm64": "aarch64"}.get(
        machine, machine
    )
    os_name = {"darwin": "macos", "linux": "linux"}.get(system, system)
    return f"{arch}-{os_name}-none"


_ZIG_TARGET = _detect_zig_target()
# In frozen builds, binaries live alongside the executable in the same directory.
# In dev, they're in the zig build output trees.
if getattr(sys, "frozen", False):
    _BIN_DIR = os.path.dirname(os.path.realpath(sys.executable))
    COMET_MINER_BIN = os.path.join(_BIN_DIR, "comet_miner")
    VERE_BIN = os.path.join(_BIN_DIR, "gw-vere")
    GW_PILL = os.path.join(_BIN_DIR, "gw-base.pill")
else:
    COMET_MINER_BIN = f"./comet-miner/zig-out/{_ZIG_TARGET}/comet_miner"
    VERE_BIN = f"./vere/zig-out/{_ZIG_TARGET}/urbit"
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

_BOLD   = "\033[1m"
_ACCENT = "\033[38;2;72;199;142m"
_LINK   = "\033[4;36m"
_NC     = "\033[0m"


def step_header(text: str) -> str:
    """Format a step header with bold accent color."""
    return f"{_ACCENT}{_BOLD}{text}{_NC}"


def tx_link(txid: str) -> str:
    """Return an OSC 8 clickable hyperlink for a txid pointing to mempool.space."""
    url = f"{MEMPOOL_TX_URL}/{txid}"
    return f"\033]8;;{url}\033\\{_LINK}{url}{_NC}\033]8;;\033\\"


def normalize_ticket(raw: str) -> str:
    """Normalize user input for ticket comparison: strip whitespace, ensure leading ~."""
    t = raw.strip()
    if not t.startswith("~"):
        t = "~" + t
    return t


def confirm_master_ticket(ticket: str) -> None:
    """Require the user to re-enter their master ticket before proceeding."""
    print("Please re-enter your master ticket to confirm you saved it:")
    while True:
        try:
            entry = input("> ")
        except EOFError:
            print()
            continue
        if normalize_ticket(entry) == ticket:
            print()
            return
        print()
        print("That doesn't match. Your master ticket is:")
        print(f"{ticket}")
        print()
        print("Please re-enter it exactly:")


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
                print(f"Faucet sent {sats} sats.")
                if txid:
                    print(f"Transaction: {tx_link(txid)}")
                return txid
            except (json.JSONDecodeError, KeyError):
                print(f"Faucet: {resp.text.strip()}")
                return ""
        else:
            # Try to extract a human-readable message from the faucet error response.
            msg = resp.text.strip()
            try:
                err_data = json.loads(resp.text)
                msg = err_data.get("error") or err_data.get("message") or msg
            except (json.JSONDecodeError, AttributeError):
                pass
            print(f"Faucet error: {msg}")
            return None
    except Exception as e:
        print(f"Could not reach faucet: {e}")
        return None


def scan_for_utxo(address: str, **_rpc_kwargs) -> dict | None:
    """Check mempool.space API for a confirmed UTXO at `address`."""
    try:
        resp = requests.get(f"{MEMPOOL_API_URL}/address/{address}/utxo", timeout=15)
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
        print(f"No confirmed funding yet ({elapsed}s elapsed)   ", end="\r")
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


def make_tweak_expr(txid_hex: str, vout: int, off: int = 0) -> str:
    """
    Build the Hoon expression string for the Groundwire tweak.

    comet-miner's --tweak evaluates this via u3v_wish().
    Tweak format v9: (rap 3 ~[%9 ~tyr %urb-watcher %btc %gw %9 txid vout off])
    """
    txid_ux = format_hoon_ux(txid_hex)
    return f"(rap 3 ~[%9 ~tyr %urb-watcher %btc %gw %9 {txid_ux} {vout} {off}])"


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
    """Decode an Urbit @uw base-64 string to an integer."""
    s = uw_str.lstrip("0v").replace(".", "")
    if not s:
        return 0
    result = 0
    for ch in s:
        result = result * 64 + _UW_MAP[ch]
    return result


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
#  URB attestation encoder — encodes SOTx for Taproot script embedding
# =========================================================================


def _encode_spawn_skim(
    w: "BitWriter",
    pass_atom: int,
    fief: tuple | None,
    spkh: bytes,
    vout: int | None,
    off: int,
    tej: int,
) -> None:
    """Encode a %spawn skim into the BitWriter."""
    w.write(7, 1)          # opcode: %spawn
    w.write(1, 0)          # pad
    w.write_mat(pass_atom) # networking key (pass)

    # Fief (inline unit): ~ = no fief
    if fief is None:
        w.write(2, 0)  # no fief
    elif fief[0] == "if":
        w.write(2, 2)         # %if
        w.write(32, fief[1])  # IPv4 address (32 bits)
        w.write(16, fief[2])  # port (16 bits)
    elif fief[0] == "is":
        w.write(2, 3)          # %is
        w.write(128, fief[1])  # IPv6 address (128 bits)
        w.write(16, fief[2])   # port (16 bits)

    # To fields
    spkh_int = int.from_bytes(spkh, "little")
    w.write(256, spkh_int)  # spkh (256 bits)
    w.write_mat(off)        # offset
    w.write_mat(tej)        # tej
    if vout is not None:
        w.write(2, 1)       # vout present
        w.write_mat(vout)   # vout value
    else:
        w.write(2, 0)       # no vout


def encode_spawn_sotx(
    comet_p: int,
    pass_atom: int,
    spkh: bytes,
    vout: int | None,
    off: int,
    tej: int,
    fief: tuple | None,
) -> bytes:
    """Encode a spawn-only SOTx (no escape) as raw bytes for script embedding.

    Used in fief/sponsor mode where no escape transaction is needed.
    """
    w = BitWriter()

    # -- Top-level SOTx header --
    w.write(2, 0)          # Sig: no sig (type 0)
    w.write(128, comet_p)  # Ship: comet @p (128 bits)

    # -- Spawn skim (no batch wrapper) --
    _encode_spawn_skim(w, pass_atom, fief, spkh, vout, off, tej)

    return w.to_bytes()


def encode_batch_sotx(
    comet_p: int,
    pass_atom: int,
    spkh: bytes,
    vout: int | None,
    off: int,
    tej: int,
    fief: tuple | None,
    sponsor_p: int,
    sponsor_sig: int,
) -> bytes:
    """Encode a batch SOTx (spawn + escape) as raw bytes for script embedding.

    Returns the encoded data as bytes ready for wrapping in a urb Taproot script.
    """
    w = BitWriter()

    # -- Top-level SOTx header --
    w.write(2, 0)          # Sig: no sig (type 0)
    w.write(128, comet_p)  # Ship: comet @p (128 bits)

    # -- Batch skim --
    w.write(7, 10)         # opcode: %batch
    w.write_mat(2)         # count: 2 items in batch

    # -- Sub-item 1: Spawn --
    _encode_spawn_skim(w, pass_atom, fief, spkh, vout, off, tej)

    # -- Sub-item 2: Escape --
    w.write(7, 3)            # opcode: %escape
    w.write(1, 0)            # pad
    w.write(128, sponsor_p)  # sponsor ship (128 bits)
    # Sponsor sig: present
    w.write(2, 1)            # sig type: present
    w.write(512, sponsor_sig)  # sig (512 bits)

    return w.to_bytes()


def wrap_urb_script(data: bytes, xonly_pubkey: bytes) -> bytes:
    """Wrap encoded URB data in a Taproot script envelope.

    Returns the raw script bytes for:
      OP_FALSE OP_IF OP_PUSH "urb" <push data chunks> OP_ENDIF
      <xonly_pubkey> OP_CHECKSIG
    """
    parts = bytearray()
    parts.append(0x00)  # OP_0 (OP_FALSE)
    parts.append(0x63)  # OP_IF
    parts.append(0x03)  # PUSH 3 bytes
    parts.extend(b"urb")

    # Push data in chunks of at most 520 bytes
    offset = 0
    while offset < len(data):
        chunk = data[offset:offset + 520]
        chunk_len = len(chunk)
        if chunk_len <= 0x4B:
            parts.append(chunk_len)
        elif chunk_len <= 0xFF:
            parts.append(0x4C)
            parts.append(chunk_len)
        elif chunk_len <= 0xFFFF:
            parts.append(0x4D)
            parts.extend(chunk_len.to_bytes(2, "little"))
        else:
            raise ValueError(f"Chunk too large: {chunk_len}")
        parts.extend(chunk)
        offset += 520

    parts.append(0x68)  # OP_ENDIF
    # Require a signature from the reveal key to spend via script-path
    parts.append(0x20)  # PUSH 32 bytes
    parts.extend(xonly_pubkey)
    parts.append(0xAC)  # OP_CHECKSIG
    return bytes(parts)


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


def tapscript_address(internal_xonly: bytes, script_bytes: bytes, network: str = "main") -> str:
    """Derive a bech32m taproot address for a single-leaf script tree.

    internal_xonly: 32-byte x-only internal public key
    script_bytes: the raw script for the single leaf (version 0xc0)
    """
    leaf_hash = _tapleaf_hash(0xC0, script_bytes)
    output_xonly, _parity = _taproot_tweak_pubkey(internal_xonly, leaf_hash)
    # Construct P2TR scriptPubKey directly from the tweaked output key
    # (do NOT pass through script.p2tr() which would apply a second tweak)
    sc = script.Script(b"\x51\x20" + output_xonly)
    return sc.address(NETWORKS[network])


# =========================================================================
#  Ring / Pass — derive Suite C networking key from comet miner output
# =========================================================================


def _hoon_atom_to_bytes(atom: int, width: int) -> bytes:
    """Convert a Hoon atom to little-endian bytes of a given width."""
    return atom.to_bytes(width, "little")


def _bytes_to_hoon_atom(b: bytes) -> int:
    """Convert little-endian bytes to a Hoon atom (integer)."""
    return int.from_bytes(b, "little")


def build_tweak_bytes(txid_hex: str, vout: int, off: int = 0) -> bytes:
    """Build the tweak atom bytes: (rap 3 ~[%9 ~tyr %urb-watcher %btc %gw %9 txid vout off]).

    This is the same tweak as make_tweak_expr but as raw bytes.
    """
    # Each element's bytes are concatenated (rap 3 = byte-level concat)
    parts = bytearray()
    parts.extend(b"\x09")             # %9 version tag (atom 9)
    parts.extend(b"\x99")             # ~tyr (galaxy 153)
    parts.extend(b"urb-watcher")      # %urb-watcher
    parts.extend(b"btc")              # %btc
    parts.extend(b"gw")               # %gw
    parts.extend(b"\x09")             # %9 (atom 9)
    # txid as Hoon atom bytes (little-endian, 32 bytes for a 256-bit hash)
    txid_int = int(txid_hex, 16)
    parts.extend(txid_int.to_bytes(32, "little"))
    # vout: only add bytes if nonzero (met 3 of 0 = 0)
    if vout > 0:
        n = (vout.bit_length() + 7) // 8
        parts.extend(vout.to_bytes(n, "little"))
    # off: only add bytes if nonzero
    if off > 0:
        n = (off.bit_length() + 7) // 8
        parts.extend(off.to_bytes(n, "little"))
    return bytes(parts)


def derive_pass_from_ring(ring_uw: str, tweak_bytes: bytes) -> int:
    """Derive the Suite C pass (public networking key) from a ring.

    Replicates Hoon's pub:ex:(nol:nu:cric:crypto ring).

    ring_uw: the ring value as a @uw string from comet-miner output
    tweak_bytes: the tweak bytes (from build_tweak_bytes)

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
    # (rub 512 bod) gives [bit_count, tweak_data]
    def hoon_rub(a, b):
        c = 0
        while ((b >> (a + c)) & 1) == 0:
            c += 1
            if c > 2000:
                raise ValueError("too many zeros in rub")
        if c == 0:
            return (1, 0)
        d = a + c + 1
        low = (b >> d) & ((1 << (c - 1)) - 1) if c > 1 else 0
        e = (1 << (c - 1)) + low
        val = (b >> (d + (c - 1))) & ((1 << e) - 1)
        return (2 * c + e, val)

    _cur, dat = hoon_rub(512, bod)

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

    # Build pass: 'c' + ugn(untweaked s_pub) + cry(c_pub ed25519) + mat(dat)
    # From zuse.hoon line 1605: pub [cry=pub.c sgn=pub.t tw=[ugn=pub.s dat=dat xtr=xtr]]
    # cry is the ed25519 public key from luck, NOT curve25519
    mat_p, mat_q = hoon_mat(dat)

    w = BitWriter()
    w.write(8, ord("c"))
    w.write(256, int.from_bytes(s_pub, "little"))   # ugn = untweaked s_pub
    w.write(256, int.from_bytes(c_pub, "little"))    # cry = ed25519 pub from luck
    w.write(mat_p, mat_q)                                 # mat(dat)

    return w.to_int()


# =========================================================================
#  SPKH — script pubkey hash for spawn verification
# =========================================================================


def compute_spkh(funding_address: str, funding_value_sats: int) -> bytes:
    """Compute spkh = SHA-256(script_pubkey || value).

    This matches boot.hoon's extract-spawn-fields:
      =/  en-out  (can 3 script-pubkey 8^amount.input ~)
      =/  spkh  (shay (add 8 wid.script-pubkey) en-out)
    """
    # Decode the taproot address to get the script pubkey
    sc = script.Script.from_address(funding_address)
    spk_bytes = sc.data  # raw script bytes (OP_1 <32-byte-key>)
    # Hoon's (can 3 [wid dat] [8 val] ~) concatenates the LE bytes of the
    # script-pubkey atom followed by the LE bytes of the value atom.
    # The script-pubkey atom's LE bytes are the REVERSE of the standard
    # script byte order.
    spk_atom = int.from_bytes(spk_bytes, "big")  # standard bytes → atom
    spk_le = spk_atom.to_bytes(len(spk_bytes), "little")  # atom → LE bytes
    value_le = funding_value_sats.to_bytes(8, "little")
    data = spk_le + value_le
    return hashlib.sha256(data).digest()


# =========================================================================
#  Sponsor signature — HTTP request to sponsor-signer agent
# =========================================================================


def request_sponsor_signature(
    comet_name: str, sponsor_url: str = SPONSOR_URL
) -> tuple[int, int]:
    """Request an escape signature from the sponsor-signer agent.

    Returns (sig, height).
    """
    resp = requests.post(
        f"{sponsor_url}/apps/sponsor-signer/sign",
        json={"ship": comet_name},
        timeout=30,
    )
    resp.raise_for_status()
    data = resp.json()
    sig_str = data["sig"]
    # sig is returned as Hoon @ux: 0x1234.5678...
    sig_hex = sig_str.lstrip("0x").replace(".", "")
    sig = int(sig_hex, 16) if sig_hex else 0
    height = int(data["height"])
    return (sig, height)


# =========================================================================
#  Transaction building — commit and reveal
# =========================================================================


def _derive_key_at_index(seed_bytes: bytes, index: int) -> bip32.HDKey:
    """Derive BIP-86 key at m/86'/1'/0'/0/<index> from raw seed bytes."""
    root = bip32.HDKey.from_seed(seed_bytes, version=NETWORKS["main"]["xprv"])
    return root.derive(f"m/86h/1h/0h/0/{index}")


def build_and_broadcast_attestation(
    seed_bytes: bytes,
    txid_hex: str,
    vout: int,
    sats: int,
    address: str,
    comet_name: str,
    comet_p_int: int,
    pass_atom: int,
    rpc_cfg: dict,
    fief: tuple | None = None,
    sponsor_name: str | None = None,
    sponsor_p_int: int | None = None,
    sponsor_sig: int | None = None,
) -> tuple[str, str]:
    """Build and broadcast commit + reveal transactions.

    Returns (commit_txid, reveal_txid).
    """
    # -- Build attestation script --
    spkh = compute_spkh(address, sats)
    if fief is not None:
        encoded = encode_spawn_sotx(
            comet_p=comet_p_int,
            pass_atom=pass_atom,
            spkh=spkh,
            vout=vout,
            off=0,
            tej=0,
            fief=fief,
        )
    else:
        encoded = encode_batch_sotx(
            comet_p=comet_p_int,
            pass_atom=pass_atom,
            spkh=spkh,
            vout=vout,
            off=0,
            tej=0,
            fief=None,
            sponsor_p=sponsor_p_int,
            sponsor_sig=sponsor_sig,
        )
    # -- Derive keys --
    # Index 0: funding address key (for spending the UTXO)
    key0 = _derive_key_at_index(seed_bytes, 0)
    # Index 1: commit address internal key (also signs the script-path spend)
    key1 = _derive_key_at_index(seed_bytes, 1)
    # Index 2: reveal destination address
    key2 = _derive_key_at_index(seed_bytes, 2)

    spawn_script = wrap_urb_script(encoded, key1.key.xonly())

    key1_xonly = key1.key.xonly()
    reveal_addr = script.p2tr(key2.key).address(NETWORKS["main"])

    # -- Commit address = tapscript address with spawn script --
    commit_addr = tapscript_address(key1_xonly, spawn_script)

    # -- Build commit tx --
    fee_rate = 2  # sat/vB
    # P2TR key-path input: ~58 vbytes; P2TR output: ~43 vbytes; overhead: ~11 vbytes
    commit_vbytes = 11 + 58 + 43
    commit_fee = commit_vbytes * fee_rate
    commit_value = sats - commit_fee

    if commit_value <= 0:
        raise RuntimeError(f"Funding UTXO too small for commit tx: {sats} sats, need > {commit_fee}")

    print(f"Commit: {sats} sats → {commit_value} sats + {commit_fee} fee")

    # Build raw commit tx
    txid_bytes = bytes.fromhex(txid_hex)
    commit_tx = Transaction(
        version=2,
        vin=[TransactionInput(txid_bytes, vout, sequence=0xFFFFFFFF)],
        vout=[TransactionOutput(commit_value, script.Script.from_address(commit_addr))],
        locktime=0,
    )

    # Sign commit tx (P2TR key-path spend — must use tweaked private key)
    funding_script = script.Script.from_address(address)
    sighash = commit_tx.sighash_taproot(
        0, [funding_script], [sats]
    )
    tweaked_key0 = key0.key.taproot_tweak(b"")  # no script tree
    sig = tweaked_key0.schnorr_sign(sighash).serialize()
    commit_tx.vin[0].witness = Witness([sig])

    commit_hex = commit_tx.serialize().hex()

    # Broadcast commit
    commit_txid = rpc_call("sendrawtransaction", [commit_hex], **rpc_cfg)
    print(f"Commit broadcast: {tx_link(commit_txid)}")

    # -- Build reveal tx --
    # Script-path spend is heavier: ~265 + script_len for the input
    input_weight = 265 + len(spawn_script)
    input_vb = (input_weight + 3) // 4
    reveal_vbytes = 11 + input_vb + 43
    reveal_fee = reveal_vbytes * fee_rate
    reveal_value = commit_value - reveal_fee

    if reveal_value < 330:
        raise RuntimeError(f"Commit output too small for reveal: {commit_value} sats, need > {reveal_fee + 330}")

    print(f"Reveal: {commit_value} sats → {reveal_value} sats + {reveal_fee} fee ({reveal_vbytes} vB)")

    commit_txid_bytes = bytes.fromhex(commit_txid)
    reveal_tx = Transaction(
        version=2,
        vin=[TransactionInput(commit_txid_bytes, 0, sequence=0xFFFFFFFF)],
        vout=[TransactionOutput(reveal_value, script.Script.from_address(reveal_addr))],
        locktime=0,
    )

    # Sign reveal tx (P2TR script-path spend)
    commit_script_obj = script.Script.from_address(commit_addr)
    spawn_script_obj = script.Script(spawn_script)
    leaf_hash = _tapleaf_hash(0xC0, spawn_script)
    reveal_sighash = reveal_tx.sighash_taproot(
        0, [commit_script_obj], [commit_value], ext_flag=1,
        script=spawn_script_obj, leaf_version=0xC0,
    )
    reveal_sig = key1.key.schnorr_sign(reveal_sighash).serialize()

    # Control block: leaf_version | parity, internal key, (no merkle proof for single leaf)
    _output_xonly, parity = _taproot_tweak_pubkey(key1_xonly, leaf_hash)
    control_block = bytes([0xC0 | parity]) + key1_xonly

    reveal_tx.vin[0].witness = Witness([reveal_sig, spawn_script, control_block])

    reveal_hex = reveal_tx.serialize().hex()

    # Broadcast reveal
    reveal_txid = rpc_call("sendrawtransaction", [reveal_hex], **rpc_cfg)
    print(f"Reveal broadcast: {tx_link(reveal_txid)}")

    return (commit_txid, reveal_txid)


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
            print(f"{confs}/{required} confirmations ({elapsed}s elapsed)   ", end="\r")
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
        print("Make sure the comet_miner binary is in the expected location.")
        sys.exit(1)

    cmd = [miner_bin, "-c", "--tweak", tweak_expr, "daplyd"]

    try:
        process = subprocess.Popen(
            cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True, bufsize=1
        )
    except PermissionError:
        print(f"ERROR: Cannot execute comet miner at: {miner_bin}")
        print(f"Try: chmod +x {miner_bin}")
        sys.exit(1)

    output_lines = []
    for line in process.stdout:
        line = line.rstrip("\n")
        output_lines.append(line)
        # Show progress lines but don't clutter
        if line.startswith("tries:"):
            print(f"\033[90m{line}\033[0m", end="\r")
        else:
            print(f"\033[90m{line}\033[0m")

    process.wait()
    print()  # clear the last \r

    if process.returncode != 0:
        print(f"ERROR: Comet miner failed (exit code {process.returncode}).")
        print("Output:")
        for line in output_lines[-10:]:
            print(f"\033[90m{line}\033[0m")
        print()
        print("Please share the output above when reporting this issue.")
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
            print(f"ERROR: Comet miner did not produce a '{required}' value.")
            print("Last 10 lines of output:")
            for line in output_lines[-10:]:
                print(f"{line}")
            print()
            print("Please share the output above when reporting this issue.")
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


def send_fyrd(vere_bin: str, conn_sock: str, fyrd_hoon: str) -> str:
    """Send a FYRD to a running ship via its conn.sock and return the decoded response.

    Replicates the shell pipeline in fyrd.sh:
      echo "$fyrd" | vere eval -jn | nc -U -W 3 <sock> | vere eval -cn
    """
    # Step 1: encode the Hoon noun to wire format
    enc = subprocess.run([vere_bin, "eval", "-jn"], input=fyrd_hoon.encode(), capture_output=True)
    if not enc.stdout:
        return ""

    # Step 2: send encoded bytes over the Unix socket and read response
    try:
        with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as sock:
            sock.settimeout(3)
            sock.connect(conn_sock)
            sock.sendall(enc.stdout)
            chunks = []
            try:
                while True:
                    chunk = sock.recv(4096)
                    if not chunk:
                        break
                    chunks.append(chunk)
            except TimeoutError:
                pass
            raw = b"".join(chunks)
    except OSError:
        return ""

    if not raw:
        return ""

    # Step 3: decode the response noun back to text
    dec = subprocess.run([vere_bin, "eval", "-cn"], input=raw, capture_output=True)
    return dec.stdout.decode(errors="replace")


_IDLE_FYRD = """:*  0
                    %fyrd
                    %base
                    %khan-eval
                    %noun
                    %ted-eval
                    :_  ~
                    '''
                    =/  m  (strand ,vase)
                    (pure:m !>('Success!'))
                    '''
                =="""

_EXIT_DOJO = """:*  0
                    %fyrd
                    %base
                    %khan-eval
                    %noun
                    %ted-eval
                    :_  :~  /sur/spider/hoon
                            /lib/strandio/hoon
                        ==
                    '''
                    =/  m  (strand ,vase)
                    ;<   ~  bind:m
                      %-  send-raw-card
                      :*  %pass  /
                          %arvo  %d
                          %belt  [%txt (tuba "|exit")]
                      ==
                    ;<   ~  bind:m
                      %-  send-raw-card
                      :*  %pass  /
                          %arvo  %d
                          %belt  [%ret ~]
                      ==
                    (pure:m !>(~))
                    '''
                =="""

_INSTALL_GW_APPS = """:*  0
                          %fyrd
                          %base
                          %khan-eval
                          %noun
                          %ted-eval
                          :_  :~  /sur/spider/hoon
                                  /lib/strandio/hoon
                              ==
                          '''
                          =/  m  (strand ,vase)
                          ;<  ~  bind:m
                            %:  poke-our
                                %hood
                                %kiln-install
                                !>
                                :*  %groups
                                    ~ribsyp-lidwex-mitdev-sopsyn--difrel-mapler-mitnyt-daplyd
                                    %tlon-408
                                ==
                            ==
                          ;<  ~  bind:m
                            %:  poke-our
                                %hood
                                %kiln-install
                                !>
                                :*  %landscape
                                    ~ribsyp-lidwex-mitdev-sopsyn--difrel-mapler-mitnyt-daplyd
                                    %landscape
                                ==
                            ==
                          (pure:m !>(~))
                          '''
                      =="""


def wait_for_idle(
    vere_bin: str, conn_sock: str, poll_interval: int = 1, max_attempts: int = 60
) -> None:
    """Poll the ship's conn.sock until it responds to a FYRD (i.e. is idle and booted)."""
    for attempt in range(1, max_attempts + 1):
        elapsed = attempt * poll_interval
        result = send_fyrd(vere_bin, conn_sock, _IDLE_FYRD)
        if "%avow" in result:
            return
        time.sleep(poll_interval)
    print()
    print(f"ERROR: Ship did not become idle after {max_attempts * poll_interval}s.")
    sys.exit(1)


def _tee_gray(stream: "subprocess.IO[bytes]") -> None:
    """Re-emit lines from *stream* in gray ANSI to signal a non-interactive background process."""
    for raw in iter(stream.readline, b""):
        line = raw.decode(errors="replace").rstrip("\n")
        sys.stdout.write(f"\033[90m{line}\033[0m\n")
        sys.stdout.flush()


_MUTED = "\033[38;2;90;100;128m"


def print_master_ticket(master_ticket: str) -> None:
    """Print the master ticket with a save warning."""
    print(f"{_BOLD}WRITE DOWN YOUR MASTER TICKET{_NC}")
    print()
    print("Your master ticket is your HD wallet seed")
    print("Your master ticket is the root of your login credentials")
    print("If you lose your master ticket, you lose your comet")
    print("If you lose your master ticket, you lose your coins")
    print()
    print("You cannot retrieve your master ticket from your ship")
    print("This is your only chance to write down your master ticket")
    print()
    print(f"Your master ticket is {_BOLD}{master_ticket}{_NC}")
    print()


def print_boot_success(url: str, master_ticket: str, pier_name: str) -> None:
    """Print the post-boot success banner."""
    port = url.split(":")[-1] if ":" in url else "8080"
    print()
    print(f"{_ACCENT}{_BOLD}Your ship was booted and shut down successfully{_NC}")
    print()
    print(f"{_BOLD}To start using your ship, run these lines in another tab:{_NC}")
    print(f"$ cd {os.getcwd()}")
    print(f"$ ./{pier_name}/.run --http-port {port}")
    print()
    print(f"{_BOLD}When your ship starts up, it will resume downloading the default apps{_NC}")
    print(f"- Manage your apps at {_LINK}{url}/apps/landscape{_NC}")
    print(f"- Use your Bitcoin hot wallet at {_LINK}{url}/spv-wallet{_NC}")
    print(f"- Chat in P2P groups and DMs at {_LINK}{url}/apps/groups{_NC}")
    print(f"- Use Nostr at {_LINK}{url}/apps/groups{_NC}")
    print(f"- Connect your AI agent to {_LINK}{url}/mcp{_NC}")
    print()
    print(f"{_BOLD}To use your ship from the browser, you'll need your web login code{_NC}")
    print("Type +code in your ship's terminal to get your login code at any time")
    print()
    print(f"{_BOLD}Next steps:{_NC}")
    print("1. Open your SPV wallet and set up your sponsor")
    print("  - Choose the default sponsor and wait 2 block confirmations")
    print("2. Say hi in the Groundwire Foundation group on Tlon")


def boot_comet(comet_name: str, feed: str, vere_bin: str, pill: str = GW_PILL) -> str:
    """Boot a comet, wait until idle, kill the process, then return the local URL.

    Starts vere, polls conn.sock until the ship responds to a FYRD, then kills
    the process and returns the http://localhost:<port> URL for the caller to display.
    """
    pier_name = comet_name.lstrip("~")

    if not os.path.isfile(vere_bin):
        print(f"\n  ERROR: Urbit runtime not found at: {vere_bin}")
        print("Make sure the gw-vere binary is in the expected location.")
        sys.exit(1)

    if not os.path.isfile(pill):
        print(f"\n  ERROR: Pill file not found at: {pill}")
        print("Make sure gw-base.pill is in the expected location.")
        sys.exit(1)

    port = find_available_port()
    url = f"http://localhost:{port}"

    cmd = [vere_bin, "-d", "-w", pier_name, "-B", pill, "-G", feed, "--http-port", str(port)]
    proc = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    threading.Thread(target=_tee_gray, args=(proc.stdout,), daemon=True).start()

    # conn.sock appears once the Arvo event loop is running
    conn_sock = os.path.join(pier_name, ".urb", "conn.sock")
    print()
    for _ in range(60):
        if os.path.exists(conn_sock):
            break
        time.sleep(5)
    else:
        print(f"ERROR: {conn_sock} never appeared. Check vere logs.")
        proc.kill()
        sys.exit(1)

    wait_for_idle(vere_bin, conn_sock)

    send_fyrd(vere_bin, conn_sock, _INSTALL_GW_APPS)
    send_fyrd(vere_bin, conn_sock, _EXIT_DOJO)
    proc.wait()

    return url


# =========================================================================
#  Main orchestration
# =========================================================================


def main():
    parser = argparse.ArgumentParser(description="Groundwire comet onboarding script")
    parser.add_argument(
        "--rpc-url", default=RPC_URL, help=f"Bitcoin Core RPC URL (default: {RPC_URL})"
    )
    parser.add_argument("--rpc-user", default=RPC_USER, help="RPC username")
    parser.add_argument("--rpc-pass", default=RPC_PASS, help="RPC password")
    parser.add_argument(
        "--miner",
        default=COMET_MINER_BIN,
        help=f"Path to comet_miner binary (default: {COMET_MINER_BIN})",
    )
    parser.add_argument(
        "--vere", default=VERE_BIN, help=f"Path to vere/urbit binary (default: {VERE_BIN})"
    )
    parser.add_argument(
        "--poll-interval",
        type=int,
        default=POLL_INTERVAL,
        help=f"Seconds between UTXO scans (default: {POLL_INTERVAL})",
    )
    parser.add_argument(
        "--skip-boot",
        action="store_true",
        help="Skip the comet boot step (just mine and print results)",
    )
    parser.add_argument(
        "--master-ticket",
        default=None,
        help="Resume with an existing master ticket instead of generating a new one",
    )
    parser.add_argument(
        "--sponsor-url",
        default=SPONSOR_URL,
        help=f"URL of the sponsor-signer agent (default: {SPONSOR_URL})",
    )
    parser.add_argument(
        "--invite",
        default=None,
        help="Invite code for faucet funding",
    )
    parser.add_argument(
        "--fief",
        default=None,
        metavar="IP:PORT",
        help="Static IP and port for direct routing (skips escape/sponsor)",
    )
    parser.add_argument(
        "--skip-attestation",
        action="store_true",
        help="Skip all funding and attestation steps — mines and boots the comet without a Bitcoin transaction",
    )

    args = parser.parse_args()

    # Parse --fief flag (ip:port)
    fief = None
    if args.fief:
        parts = args.fief.rsplit(":", 1)
        if len(parts) != 2:
            print("Error: --fief must be in IP:PORT format (e.g. 1.2.3.4:34543)")
            sys.exit(1)
        ip_str, port_str = parts
        try:
            port = int(port_str)
        except ValueError:
            print(f"Error: invalid port number: {port_str}")
            sys.exit(1)
        if ":" in ip_str:
            # IPv6
            import ipaddress
            ip_int = int(ipaddress.IPv6Address(ip_str))
            fief = ("is", ip_int, port)
        else:
            # IPv4
            import ipaddress
            ip_int = int(ipaddress.IPv4Address(ip_str))
            fief = ("if", ip_int, port)

    if args.skip_attestation:
        total_steps = 3 if args.skip_boot else 4
    else:
        total_steps = 6 if fief is not None else 7

    # Use CLI args for RPC config
    rpc_cfg = {"rpc_url": args.rpc_url, "rpc_user": args.rpc_user, "rpc_pass": args.rpc_pass}

    # ------------------------------------------------------------------
    # Step 1: Generate or restore @q master ticket
    # ------------------------------------------------------------------
    print()

    # Reopen stdin from the terminal if it was consumed by a pipe
    # (e.g. curl ... | sh -> ./gw-onboard).
    if not sys.stdin.isatty():
        with contextlib.suppress(OSError):
            sys.stdin = open("/dev/tty")  # noqa: SIM115

    if args.master_ticket:
        master_ticket = args.master_ticket
        try:
            seed_int = decode_q(master_ticket)
        except (ValueError, IndexError):
            print(f"ERROR: Invalid master ticket format: {master_ticket}")
            print("The ticket should look like: ~sampel-palnet-sampel-palnet")
            sys.exit(1)
        n_bytes = (seed_int.bit_length() + 7) // 8
        seed_bytes = seed_int.to_bytes(n_bytes, "little")
        print(f"Resuming with master ticket: {master_ticket}")
        print()
    else:
        seed_bytes = secrets.token_bytes(16)
        seed_int = int.from_bytes(seed_bytes, "little")
        master_ticket = encode_q(seed_int)

        print(step_header(f"Step 1/{total_steps}: Generating master ticket"))
        print()
        print_master_ticket(master_ticket)
        confirm_master_ticket(master_ticket)

    # ------------------------------------------------------------------
    # Step 2: Derive taproot address
    # ------------------------------------------------------------------
    try:
        address = derive_taproot_address(seed_bytes)
    except Exception as e:
        print(f"ERROR: Could not derive funding address: {e}")
        print("This is unexpected — please report this issue.")
        sys.exit(1)

    print(step_header(f"Step 2/{total_steps}: Deriving funding address"))
    print()
    print(f"Address: {address}")
    # ------------------------------------------------------------------
    # Step 3: Auto-fund via faucet, then wait for confirmation
    # ------------------------------------------------------------------
    if args.skip_attestation:
        txid = "0" * 64
        vout = 0
        sats = REQUIRED_SATS
    else:
        print(step_header(f"Step 3/{total_steps}: Funding your address"))
        print()
        print("Requesting bitcoin from faucet...")
        faucet_result = request_faucet(address, invite=args.invite)
        if faucet_result is not None:
            print("Waiting for transaction to confirm...\n")
        else:
            print()
            print(f"Automatic funding failed. Please send at least {_BOLD}{REQUIRED_SATS} sats{_NC} manually to:")
            print(f"{address}")
            print()

        try:
            block_count = rpc_call("getblockcount", **rpc_cfg)
            print(f"Connected to Bitcoin (block height: {block_count:,})")
        except Exception as e:
            print(f"ERROR: Cannot reach Bitcoin Core at {rpc_cfg['rpc_url']}")
            print(f"{e}")
            print()
            print("Make sure bitcoind is running and the RPC")
            print("credentials match, then re-run this script.")
            sys.exit(1)

        utxo = wait_for_funding(address, args.poll_interval, **rpc_cfg)

        txid = utxo["txid"]
        vout = utxo["vout"]
        sats = utxo["_sats"]
        print(f"\n  Funding confirmed! Received {sats:,} sats.")
        print(f"Transaction: {tx_link(txid)}:{vout}")

    # ------------------------------------------------------------------
    # Step 4: Construct tweak and mine comet
    # ------------------------------------------------------------------
    off = 0
    tweak_expr = make_tweak_expr(txid, vout, off)

    print()
    mining_step = 3 if args.skip_attestation else 4
    print(step_header(f"Step {mining_step}/{total_steps}: Mining your comet identity"))
    print()
    miner_result = run_comet_miner(tweak_expr, args.miner)

    comet = miner_result["comet"]
    feed = miner_result["feed"]
    ring_uw = miner_result.get("ring", "")

    if args.skip_attestation:
        if args.skip_boot:
            print(f"comet: {comet}")
            print(f"feed: {feed}")
        else:
            print(step_header(f"Step 4/{total_steps}: Booting your ship"))
            print()
            print("Give it a second...")
            url = boot_comet(comet, feed, args.vere)
            print_boot_success(url, master_ticket, pier_name=comet.lstrip("~"))
        return

    # Derive the networking key (pass) from the ring
    tweak_raw = build_tweak_bytes(txid, vout, off)
    pass_atom = derive_pass_from_ring(ring_uw, tweak_raw)
    comet_p_int = patp_to_int(comet)

    if fief is not None:
        # ------------------------------------------------------------------
        # Fief mode: spawn-only, no escape/sponsor needed
        # ------------------------------------------------------------------
        print()
        print(step_header(f"Step 5/{total_steps}: Building and broadcasting attestation (fief mode)"))

        commit_txid, reveal_txid = build_and_broadcast_attestation(
            seed_bytes=seed_bytes,
            txid_hex=txid,
            vout=vout,
            sats=sats,
            address=address,
            comet_name=comet,
            comet_p_int=comet_p_int,
            pass_atom=pass_atom,
            fief=fief,
            sponsor_name=None,
            sponsor_p_int=None,
            sponsor_sig=None,
            rpc_cfg=rpc_cfg,
        )
    else:
        # ------------------------------------------------------------------
        # Normal mode: batch spawn + escape
        # ------------------------------------------------------------------
        # Step 5: Request sponsor signature
        print()
        print(step_header(f"Step 5/{total_steps}: Requesting sponsor signature"))
        sponsor_url = args.sponsor_url
        try:
            sponsor_sig, sponsor_height = request_sponsor_signature(comet, sponsor_url)
            print(f"Sponsor signed at block height {sponsor_height}")
        except Exception as e:
            print()
            print(f"Could not reach the Groundwire sponsor ({e}).")
            print()
            print("This is usually temporary — the sponsor may be restarting")
            print("or the network may be briefly unavailable.")
            print()
            print("To retry, re-run with your master ticket:")
            print(f"./gw-onboard --master-ticket '{master_ticket}'")
            sys.exit(1)

        # Step 6: Build and broadcast attestation transactions
        print()
        print(step_header(f"Step 6/{total_steps}: Building and broadcasting attestation"))

        escape_sponsor_p_int = patp_to_int(ESCAPE_SPONSOR)

        commit_txid, reveal_txid = build_and_broadcast_attestation(
            seed_bytes=seed_bytes,
            txid_hex=txid,
            vout=vout,
            sats=sats,
            address=address,
            comet_name=comet,
            comet_p_int=comet_p_int,
            pass_atom=pass_atom,
            fief=None,
            sponsor_name=ESCAPE_SPONSOR,
            sponsor_p_int=escape_sponsor_p_int,
            sponsor_sig=sponsor_sig,
            rpc_cfg=rpc_cfg,
        )

    # Wait for block confirmations
    wait_for_confirmations(reveal_txid, BLOCK_CONFIRMATIONS, args.poll_interval, **rpc_cfg)

    # ------------------------------------------------------------------
    # Boot comet
    # ------------------------------------------------------------------
    pier_name = comet.lstrip("~")
    if args.skip_boot:
        print(f"comet: {comet}")
        print(f"feed: {feed}")
        print()
        print("To boot manually later, run:")
        print(f"{args.vere} -w {pier_name} -B {GW_PILL} -G {feed}")
    else:
        print()
        print(step_header(f"Step {total_steps}/{total_steps}: Booting your ship"))
        print()
        print("Give it a second...")
        url = boot_comet(comet, feed, args.vere)
        print_boot_success(url, master_ticket, pier_name)

if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        try:
            print("\n\nInterrupted. Your master ticket is still valid — rerun with")
            print("--master-ticket to resume.")
        except Exception:
            pass
        os._exit(130)
    except SystemExit:
        raise
    except Exception as e:
        print()
        print("=" * 60)
        print("Something went wrong.")
        print("=" * 60)
        print()
        print(f"Error: {e}")
        print()
        print("Your master ticket is still valid if one was generated.")
        print("Rerun with --master-ticket to resume from where you left off.")
        print()
        print("If this keeps happening, please report the issue with the")
        print("error message above.")
        sys.exit(1)
