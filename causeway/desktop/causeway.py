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
    _BIN_DIR = os.path.dirname(os.path.abspath(sys.executable))
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


def tx_link(txid: str) -> str:
    """Return an OSC 8 clickable hyperlink for a txid pointing to mempool.space."""
    url = f"{MEMPOOL_TX_URL}/{txid}"
    return f"\033]8;;{url}\033\\\033[4;36m{url}\033[0m\033]8;;\033\\"


def normalize_ticket(raw: str) -> str:
    """Normalize user input for ticket comparison: strip whitespace, ensure leading ~."""
    t = raw.strip()
    if not t.startswith("~"):
        t = "~" + t
    return t


def confirm_master_ticket(ticket: str) -> None:
    """Require the user to re-enter their master ticket before proceeding."""
    print("  Please re-enter your master ticket to confirm you saved it:")
    while True:
        try:
            entry = input("  > ")
        except EOFError:
            print()
            continue
        if normalize_ticket(entry) == ticket:
            print("  Confirmed!")
            print()
            return
        print()
        print("  That doesn't match. Your master ticket is:")
        print(f"  {ticket}")
        print()
        print("  Please re-enter it exactly:")


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


def _encode_keys_skim(w: "BitWriter", pass_atom: int, breach: bool) -> None:
    """Encode a %keys skim (rekey / breach)."""
    w.write(7, 2)               # opcode %keys
    w.write(1, 1 if breach else 0)
    w.write_mat(pass_atom)


def _encode_escape_skim(w: "BitWriter", parent_p: int, escape_sig: int | None) -> None:
    """Encode a %escape skim. escape_sig is the sponsor's off-chain signature (512 bits) or None."""
    w.write(7, 3)               # opcode %escape
    w.write(1, 0)               # pad
    w.write(128, parent_p)
    if escape_sig is None:
        w.write(2, 0)
    else:
        w.write(2, 1)
        w.write(512, escape_sig)


def _encode_ship_skim(w: "BitWriter", opcode: int, ship_p: int) -> None:
    """Encode a cancel-escape/adopt/reject/detach skim — each takes a single ship."""
    if opcode not in (4, 5, 6, 7):
        raise ValueError(f"ship skim opcode must be one of 4,5,6,7; got {opcode}")
    w.write(7, opcode)
    w.write(1, 0)
    w.write(128, ship_p)


def _encode_fief_skim(w: "BitWriter", fief: tuple | None) -> None:
    """Encode a standalone %fief skim."""
    w.write(7, 11)              # opcode %fief
    w.write(1, 0)               # pad
    if fief is None:
        w.write(2, 0)
    elif fief[0] == "if":
        w.write(2, 2); w.write(32, fief[1]); w.write(16, fief[2])
    elif fief[0] == "is":
        w.write(2, 3); w.write(128, fief[1]); w.write(16, fief[2])
    else:
        raise ValueError(f"unknown fief kind: {fief[0]}")


def _encode_mang_skim(w: "BitWriter", mang: tuple | None) -> None:
    """Encode a %set-mang skim. mang is None, ('sont',txid,vout,off), or ('pass',pass_atom)."""
    w.write(7, 8)
    if mang is None:
        w.write(2, 0)
    elif mang[0] == "sont":
        w.write(2, 1)
        w.write(1, 0)                                    # pad before sont
        w.write(256, int.from_bytes(mang[1], "little"))  # txid (32 bytes)
        w.write_mat(mang[2])                             # vout
        w.write_mat(mang[3])                             # off
    elif mang[0] == "pass":
        w.write(2, 2)
        w.write(256, mang[1])
    else:
        raise ValueError(f"unknown mang kind: {mang[0]}")


def _write_outer_header(w: "BitWriter", from_ship_p: int, tx_sig: int | None) -> None:
    """Write the top-level sotx header: en-sig(tx_sig) + [128 from_ship]."""
    if tx_sig is None:
        w.write(2, 0)
    else:
        w.write(2, 1)
        w.write(512, tx_sig)
    w.write(128, from_ship_p)


def encode_keys_sotx(comet_p: int, pass_atom: int, breach: bool = False, tx_sig: int | None = None) -> bytes:
    """%keys (rekey) sotx — the comet rotates its networking key."""
    w = BitWriter()
    _write_outer_header(w, comet_p, tx_sig)
    _encode_keys_skim(w, pass_atom, breach)
    return w.to_bytes()


def encode_escape_sotx(
    comet_p: int,
    parent_p: int,
    escape_sig: int | None = None,
    tx_sig: int | None = None,
) -> bytes:
    """%escape sotx — comet requests adoption by `parent_p`. If escape_sig is provided,
    it is the sponsor's off-chain signature over (shaz (jam [comet_p block_height]))."""
    w = BitWriter()
    _write_outer_header(w, comet_p, tx_sig)
    _encode_escape_skim(w, parent_p, escape_sig)
    return w.to_bytes()


def encode_cancel_escape_sotx(comet_p: int, parent_p: int, tx_sig: int | None = None) -> bytes:
    """%cancel-escape sotx — comet cancels a pending escape request."""
    w = BitWriter()
    _write_outer_header(w, comet_p, tx_sig)
    _encode_ship_skim(w, 4, parent_p)
    return w.to_bytes()


def encode_adopt_sotx(sponsor_p: int, child_p: int, tx_sig: int | None = None) -> bytes:
    """%adopt sotx — sponsor accepts a child's pending escape. **Reveal-mandatory.**"""
    w = BitWriter()
    _write_outer_header(w, sponsor_p, tx_sig)
    _encode_ship_skim(w, 5, child_p)
    return w.to_bytes()


def encode_reject_sotx(sponsor_p: int, child_p: int, tx_sig: int | None = None) -> bytes:
    """%reject sotx — sponsor denies a pending escape. **Reveal-mandatory.**"""
    w = BitWriter()
    _write_outer_header(w, sponsor_p, tx_sig)
    _encode_ship_skim(w, 6, child_p)
    return w.to_bytes()


def encode_detach_sotx(sponsor_p: int, child_p: int, tx_sig: int | None = None) -> bytes:
    """%detach sotx — sponsor releases a child. **Reveal-mandatory.**"""
    w = BitWriter()
    _write_outer_header(w, sponsor_p, tx_sig)
    _encode_ship_skim(w, 7, child_p)
    return w.to_bytes()


def encode_fief_sotx(comet_p: int, fief: tuple | None, tx_sig: int | None = None) -> bytes:
    """%fief sotx — comet sets or clears its own network-routing fief.
    fief is None or ('if', ipv4_int, port) or ('is', ipv6_int, port)."""
    w = BitWriter()
    _write_outer_header(w, comet_p, tx_sig)
    _encode_fief_skim(w, fief)
    return w.to_bytes()


def encode_set_mang_sotx(comet_p: int, mang: tuple | None, tx_sig: int | None = None) -> bytes:
    """%set-mang sotx — designate a manager for PKI ops. Core doesn't process this yet."""
    w = BitWriter()
    _write_outer_header(w, comet_p, tx_sig)
    _encode_mang_skim(w, mang)
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


# BIP-341 "nothing up my sleeve" point H. Retained for the tapleaf script's
# OP_CHECKSIG suffix so the leaf is provably-unspendable via script-path, but
# NOT used as the commit output's internal key — urb-core's ++is-sont-in-input
# (lib/urb-core.hoon ~L595) requires the point's sont to be re-spent as the
# input of the next management tx, so the internal key must be one the point
# owner holds. Using NUMS there would strand the sat forever.
NUMS_XONLY = bytes.fromhex("50929b74c1a04954b78b4b6035e97a5e078a5a0f28ec96d547bfee9ace803ac0")


def build_confidential_commit_psbt(
    *,
    utxo_txid: str,
    utxo_vout: int,
    utxo_value: int,
    utxo_script_pubkey: bytes,
    funding_internal_xonly: bytes,
    funding_path: str,
    funding_fingerprint: bytes,
    attestation_bytes: bytes,
    fee_rate: int = 2,
    change_internal_xonly: bytes | None = None,
    change_script_pubkey: bytes | None = None,
    change_path: str | None = None,
    network: str = "main",
) -> tuple["psbt.PSBT", dict]:
    """Build a confidential-commit PSBT.

    Shape:
      * input 0 — the funding UTXO, spent via P2TR key-path.
      * output 0 — P2TR committing to the urb attestation tapleaf, with the
        funding UTXO's own xonly as the internal key. This is load-bearing:
        urb-core's sont-chain invariant requires the next management op's
        commit to spend this exact UTXO via key-path (++is-sont-in-input in
        lib/urb-core around L595), so the internal key must be one the point
        owner can sign for. The output remains script-path-committed to the
        attestation (tapleaf with the encoded sotx) but script-path-
        unspendable (the leaf's OP_CHECKSIG uses the NUMS xonly, for which
        nobody holds the private key).
      * output 1 (optional) — change back to the caller's next change address.

    For the NEXT management op on this point, pass merkle_root (=leaf_hash for
    a single-leaf tree, stored as merkle_root_hex in the proof) via
    PSBT_IN_TAP_MERKLE_ROOT on the re-spend input so the external signer can
    compute the key-path tweak. See `add_tap_merkle_root_hint`.

    Returns (psbt, proof_dict) where proof_dict is the on-disk schema for
    comet.proof.json / <patp>-<op>-<txid>.proof.json.
    """
    from embit import psbt as _psbt
    from embit import ec as _ec
    from embit.transaction import (
        Transaction as _Tx,
        TransactionInput as _TxIn,
        TransactionOutput as _TxOut,
    )

    leaf_script = wrap_urb_script(attestation_bytes, NUMS_XONLY)
    leaf_version = 0xC0
    leaf_hash = _tapleaf_hash(leaf_version, leaf_script)
    # Internal key = funding UTXO's xonly. The sat at output 0 stays key-path
    # spendable by the point owner for the next management op.
    commit_spk = _build_p2tr_spk(funding_internal_xonly, leaf_hash)

    # Tx-size estimation: 1x P2TR key-path input ~57.5 vbytes, each P2TR
    # output ~43 vbytes, overhead ~10.5. 1-out commit ~111; 2-out ~154.
    est_vbytes = 154 if change_script_pubkey is not None else 111
    fee = est_vbytes * fee_rate

    if change_script_pubkey is None:
        # All funds land in the commit output. The sat is re-spendable by the
        # owner, and this is the "home" for any future management op.
        commit_value = utxo_value - fee
        if commit_value < 330:
            raise RuntimeError(
                f"UTXO {utxo_value} sats too small: need >= 330 + {fee} (fee)"
            )
        outputs: list[_TxOut] = [_TxOut(commit_value, _script_from_spk(commit_spk))]
    else:
        # Split: commit output holds a comfortable re-spend reserve (up to
        # 1000 sats, enough for a few future ops at 2 sat/vbyte). Change goes
        # back to the wallet.
        commit_value = min(1_000, utxo_value - fee - 330)
        if commit_value < 330:
            raise RuntimeError(
                f"UTXO {utxo_value} sats too small: need >= {330 + 330 + fee} "
                f"(commit + change dust + fee)"
            )
        change_value = utxo_value - commit_value - fee
        outputs = [
            _TxOut(commit_value, _script_from_spk(commit_spk)),
            _TxOut(change_value, _script_from_spk(change_script_pubkey)),
        ]

    txid_bytes = bytes.fromhex(utxo_txid)
    tx = _Tx(
        version=2,
        vin=[_TxIn(txid_bytes, utxo_vout, sequence=0xFFFFFFFF)],
        vout=outputs,
        locktime=0,
    )

    p = _psbt.PSBT(tx)
    inp = p.inputs[0]
    # BIP-371 input metadata — key-path spend of a plain P2TR input.
    inp.witness_utxo = _TxOut(utxo_value, _script_from_spk(utxo_script_pubkey))
    funding_pubkey = _ec.PublicKey.from_xonly(funding_internal_xonly)
    inp.taproot_internal_key = funding_pubkey
    inp.taproot_bip32_derivations[funding_pubkey] = (
        [],  # no leaf hashes — key-path spend of a plain P2TR input
        _psbt.DerivationPath(funding_fingerprint, _parse_path(funding_path)),
    )

    # BIP-371 output metadata so a watcher / the user's wallet can recognize
    # the commit output and its derivation (otherwise the tweaked SPK won't
    # match any standard descriptor address).
    out0 = p.outputs[0]
    out0.taproot_internal_key = funding_pubkey
    out0.taproot_bip32_derivations[funding_pubkey] = (
        [],
        _psbt.DerivationPath(funding_fingerprint, _parse_path(funding_path)),
    )

    # Change output metadata (if present) — points at the change address path
    # so the signer treats it as a known-derivation output.
    if change_script_pubkey is not None and change_internal_xonly is not None and change_path is not None:
        change_pubkey = _ec.PublicKey.from_xonly(change_internal_xonly)
        out1 = p.outputs[1]
        out1.taproot_internal_key = change_pubkey
        out1.taproot_bip32_derivations[change_pubkey] = (
            [],
            _psbt.DerivationPath(funding_fingerprint, _parse_path(change_path)),
        )

    proof = {
        "version": 1,
        "commit_vout": 0,
        "internal_pubkey_hex": funding_internal_xonly.hex(),
        "leaf_version": leaf_version,
        "leaf_script_hex": leaf_script.hex(),
        "merkle_path": [],  # single-leaf tree; merkle_root == leaf_hash
        "merkle_root_hex": leaf_hash.hex(),  # feed into next-op PSBT_IN_TAP_MERKLE_ROOT
        "attestation_bytes_hex": attestation_bytes.hex(),
        "commit_script_pubkey_hex": commit_spk.hex(),
        "commit_value": outputs[0].value,
        "network": network,
        "funding": {
            # The sat's on-chain home after this commit: the input of any
            # future management op MUST be (commit_txid, 0). That input needs
            # PSBT_IN_TAP_MERKLE_ROOT = merkle_root_hex so the signer can
            # compute the key-path tweak.
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
    prior Causeway commit output via key-path: the signer needs merkle_root to
    compute the taproot tweak, and won't know it from their descriptor alone."""
    inp = psbt_obj.inputs[input_idx]
    inp.taproot_merkle_root = merkle_root


def build_chained_commit_psbt(
    *,
    prior_proof: dict,
    new_attestation_bytes: bytes,
    fee_rate: int = 2,
    network: str = "main",
) -> tuple["psbt.PSBT", dict]:
    """Build a management-op commit PSBT that spends the prior op's commit
    output. This is the sont-preserving path: urb-core's ++is-sont-in-input
    requires that the next management op's input IS the point's current sont.

    Takes a prior proof.json dict (from a previous spawn or management op) and
    the new sotx bytes. Produces a PSBT that:
      * input 0 = (prior_proof.commit_txid, prior_proof.commit_vout), key-path
        spend of the tweaked taproot output. Includes PSBT_IN_TAP_MERKLE_ROOT
        so the external signer can compute the tweak.
      * output 0 = new P2TR tweaked by the new attestation, same internal key
        (the point owner's).
      * no change output — the sat's value stays on the new commit output so
        future ops can continue the chain.

    Returns (psbt, new_proof_dict).
    """
    from embit import psbt as _psbt
    from embit import ec as _ec
    from embit.transaction import (
        Transaction as _Tx,
        TransactionInput as _TxIn,
        TransactionOutput as _TxOut,
    )

    # Unpack the prior proof.
    prior_internal_xonly = bytes.fromhex(prior_proof["internal_pubkey_hex"])
    prior_merkle_root = bytes.fromhex(prior_proof["merkle_root_hex"])
    prior_commit_txid = prior_proof.get("commit_txid")
    if not prior_commit_txid:
        raise ValueError("prior proof has no commit_txid — was the prior commit broadcast?")
    prior_commit_vout = int(prior_proof.get("commit_vout", 0))
    prior_commit_value = int(prior_proof["commit_value"])
    prior_commit_spk = bytes.fromhex(prior_proof["commit_script_pubkey_hex"])
    funding = prior_proof.get("funding", {})
    funding_path = funding.get("path", "m/86h/0h/0h/0/0")
    funding_fingerprint = bytes.fromhex(funding.get("fingerprint_hex", "00000000"))

    # Build the new tapleaf.
    leaf_script = wrap_urb_script(new_attestation_bytes, NUMS_XONLY)
    leaf_version = 0xC0
    leaf_hash = _tapleaf_hash(leaf_version, leaf_script)
    new_commit_spk = _build_p2tr_spk(prior_internal_xonly, leaf_hash)

    # Single in/out → ~111 vbytes.
    est_vbytes = 111
    fee = est_vbytes * fee_rate
    new_commit_value = prior_commit_value - fee
    if new_commit_value < 330:
        raise RuntimeError(
            f"prior commit {prior_commit_value} sats too small: need >= 330 + {fee} "
            f"(fee). Top up by sending more sats to the point's commit output, or "
            f"use a lower fee-rate."
        )

    txid_bytes = bytes.fromhex(prior_commit_txid)
    tx = _Tx(
        version=2,
        vin=[_TxIn(txid_bytes, prior_commit_vout, sequence=0xFFFFFFFF)],
        vout=[_TxOut(new_commit_value, _script_from_spk(new_commit_spk))],
        locktime=0,
    )
    p = _psbt.PSBT(tx)

    # Input: key-path spend of a tweaked P2TR. The signer needs merkle_root to
    # compute the tweak — that's what PSBT_IN_TAP_MERKLE_ROOT is for.
    inp = p.inputs[0]
    inp.witness_utxo = _TxOut(prior_commit_value, _script_from_spk(prior_commit_spk))
    internal_pubkey = _ec.PublicKey.from_xonly(prior_internal_xonly)
    inp.taproot_internal_key = internal_pubkey
    inp.taproot_merkle_root = prior_merkle_root
    inp.taproot_bip32_derivations[internal_pubkey] = (
        [],
        _psbt.DerivationPath(funding_fingerprint, _parse_path(funding_path)),
    )

    # Output metadata: new commit output has the same internal key.
    out0 = p.outputs[0]
    out0.taproot_internal_key = internal_pubkey
    out0.taproot_bip32_derivations[internal_pubkey] = (
        [],
        _psbt.DerivationPath(funding_fingerprint, _parse_path(funding_path)),
    )

    new_proof = {
        "version": 1,
        "commit_vout": 0,
        "internal_pubkey_hex": prior_internal_xonly.hex(),
        "leaf_version": leaf_version,
        "leaf_script_hex": leaf_script.hex(),
        "merkle_path": [],
        "merkle_root_hex": leaf_hash.hex(),
        "attestation_bytes_hex": new_attestation_bytes.hex(),
        "commit_script_pubkey_hex": new_commit_spk.hex(),
        "commit_value": new_commit_value,
        "network": network,
        "funding": {
            # The sat moved: new home is (new_commit_txid, 0) once broadcast.
            "txid": prior_commit_txid,  # the INPUT we spent (for provenance)
            "vout": prior_commit_vout,
            "value": prior_commit_value,
            "path": funding_path,
            "fingerprint_hex": funding_fingerprint.hex(),
        },
        "prior_proof": {
            "commit_txid": prior_commit_txid,
            "commit_vout": prior_commit_vout,
        },
    }
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
        print("  Make sure the comet_miner binary is in the expected location.")
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
        print("  Make sure the gw-vere binary is in the expected location.")
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


PROOF_SCHEMA_VERSION = 1


def write_proof_json(proof: dict, path: str) -> None:
    """Write a proof dict to `path` as pretty-printed JSON."""
    with open(path, "w") as f:
        json.dump(proof, f, indent=2, sort_keys=True)
        f.write("\n")


def load_proof_json(path: str) -> dict:
    """Load a proof dict from `path`."""
    with open(path) as f:
        return json.load(f)


def verify_proof_self(proof: dict) -> tuple[bool, str]:
    """Recompute the tapleaf hash and tweaked output xonly from the proof's fields.
    Returns (ok, reason). Does NOT check on-chain existence; see verify_proof_onchain."""
    try:
        internal = bytes.fromhex(proof["internal_pubkey_hex"])
        leaf_script = bytes.fromhex(proof["leaf_script_hex"])
        leaf_version = int(proof["leaf_version"])
        expected_spk = bytes.fromhex(proof["commit_script_pubkey_hex"])
    except (KeyError, ValueError) as e:
        return False, f"malformed proof: {e}"
    try:
        leaf_hash = _tapleaf_hash(leaf_version, leaf_script)
        # Walk the merkle path (empty for single-leaf trees).
        root = leaf_hash
        for sib_hex in proof.get("merkle_path", []):
            sib = bytes.fromhex(sib_hex)
            root = _tagged_hash("TapBranch", min(root, sib) + max(root, sib))
        computed_spk = _build_p2tr_spk(internal, root)
    except Exception as e:
        return False, f"error recomputing spk: {e}"
    if computed_spk != expected_spk:
        return False, (
            f"spk mismatch: computed {computed_spk.hex()}, expected {expected_spk.hex()}"
        )
    return True, "OK"


def verify_proof_onchain(proof: dict, *, mempool_base: str = MEMPOOL_API_URL) -> tuple[bool, str]:
    """Fetch the commit tx from mempool.space and verify its output matches the proof."""
    ok, reason = verify_proof_self(proof)
    if not ok:
        return False, reason
    txid = proof.get("commit_txid")
    if not txid:
        return False, "proof has no commit_txid — was the commit broadcast?"
    try:
        r = requests.get(f"{mempool_base}/tx/{txid}", timeout=20)
        r.raise_for_status()
        tx = r.json()
    except Exception as e:
        return False, f"could not fetch tx {txid}: {e}"
    vout_idx = int(proof.get("commit_vout", 0))
    try:
        vout = tx["vout"][vout_idx]
    except (KeyError, IndexError):
        return False, f"tx {txid} has no vout {vout_idx}"
    onchain_spk = vout.get("scriptpubkey", "")
    if onchain_spk.lower() != proof["commit_script_pubkey_hex"].lower():
        return False, (
            f"on-chain spk {onchain_spk} != proof spk {proof['commit_script_pubkey_hex']}"
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
        fpr_hex = m.group(2) or "00000000"
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
    # Infer a default origin: BIP-86 m/86'/0'/0' on main, m/86'/1'/0' on testnet.
    # Note: fingerprint is unknown for bare-xpub mode.
    default_coin = 0 if network == "main" else 1
    account_path = [_hardened(86), _hardened(default_coin), _hardened(0)]
    return KeySource(xpub=xpub, master_fingerprint=b"\x00\x00\x00\x00", account_path=account_path, network=network)


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
                })
    return found


def pick_utxo_interactive(utxos: list[dict], *, min_value: int = 630) -> dict:
    """Render a numbered UTXO list and prompt the user to pick one."""
    if not utxos:
        raise SystemExit("No UTXOs found. Fund one of your xpub's addresses and try again.")
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
        choice = input("  Pick a UTXO number: ").strip()
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
    """4-byte master key fingerprint (hash160 of the compressed pubkey, first 4 bytes)."""
    compressed = root.key.get_public_key().sec()
    import hashlib as _hl
    h = _hl.new("ripemd160", _hl.sha256(compressed).digest()).digest()
    return h[:4]


def print_seed_box(mnemonic: str) -> None:
    words = mnemonic.split()
    width = max(len(w) for w in words) + 4
    rows = (len(words) + 2) // 3
    print()
    print("  ┌" + "─" * 68 + "┐")
    print("  │  SAVE THIS — your BIP-39 seed phrase controls your comet identity│")
    print("  │  and the funds that back it. Write it down. Losing it is fatal.  │")
    print("  │                                                                  │")
    for r in range(rows):
        cells = []
        for c in range(3):
            idx = r + rows * c
            if idx < len(words):
                cells.append(f"{idx+1:>2}. {words[idx]:<{width-4}}")
            else:
                cells.append(" " * width)
        print("  │  " + "  ".join(cells) + "│")
    print("  └" + "─" * 68 + "┘")


def confirm_seed_saved(mnemonic: str) -> None:
    print()
    print("  Please re-enter your seed phrase to confirm you wrote it down")
    print("  (space-separated — spelling must match exactly):")
    while True:
        try:
            entry = input("  > ").strip()
        except EOFError:
            print()
            continue
        if entry.lower() == mnemonic.lower():
            print("  Confirmed!")
            return
        print("  That doesn't match. Try again. (Your seed phrase is above.)")


# =========================================================================
#  Miner wrapper — either subprocess to comet_miner binary, or pure-JS port (TBD)
# =========================================================================


def mine_comet(tweak_hex: str, miner_bin: str = COMET_MINER_BIN) -> dict:
    """Call the comet_miner binary with a tweak expression, return parsed result.

    Uses the existing make_tweak_expr + run_comet_miner flow.
    """
    # Need to reconstruct the tweak expression from the hex. tweak_hex is raw bytes.
    tweak_bytes = bytes.fromhex(tweak_hex)
    # Parse txid / vout / off back out of the tweak bytes structure so we can use make_tweak_expr.
    # NOTE: this is fragile; simpler to just pass the tweak atom directly. The miner accepts a Hoon
    # expression, so we wrap the raw bytes as =@ (^&@ux bytes) and run:
    # For our purposes the caller can pass (txid, vout, off) and we build the expr via make_tweak_expr.
    raise NotImplementedError("use mine_comet_from_utxo below")


def mine_comet_from_utxo(
    txid_hex: str,
    vout: int,
    off: int = 0,
    miner_bin: str = COMET_MINER_BIN,
) -> dict:
    """Build a tweak expression for the given funding UTXO and run the comet miner."""
    tweak_expr = make_tweak_expr(txid_hex, vout, off)
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
    "Path to the prior proof.json for this point (spawn's or last mgmt op's). "
    "The new commit spends its commit output — required so urb-core's sont chain stays valid."
)


@cli.command("rekey")
@click.option("--point", required=True, help="Target @p (the comet being rekeyed)")
@click.option("--prior-proof", required=True, type=click.Path(exists=True, dir_okay=False), help=_MGMT_PRIOR_PROOF_HELP)
@click.option("--new-pass-hex", required=True, help="New networking key (pass), hex — derived from your ship's new ring")
@click.option("--breach", is_flag=True, default=False, help="Bump rift (a breach rekey)")
@click.option("--fee-rate", type=int, default=2, show_default=True)
@click.option("--network", type=click.Choice(["main", "testnet"]), default="main", show_default=True)
@click.option("--output-dir", type=click.Path(file_okay=False, dir_okay=True), default=".", show_default=True)
@click.option("--mempool-base", default=MEMPOOL_API_URL, show_default=True)
def cmd_rekey(point, prior_proof, new_pass_hex, breach, fee_rate, network, output_dir, mempool_base):
    """%keys sotx — rotate a comet's networking key. Confidential commit; chains off --prior-proof."""
    comet_p = patp_to_int(point)
    new_pass = int.from_bytes(bytes.fromhex(new_pass_hex), "little")
    attestation = encode_keys_sotx(comet_p=comet_p, pass_atom=new_pass, breach=breach)
    _run_management_op("keys", point, prior_proof, fee_rate, network, output_dir, mempool_base, attestation)


@cli.command("escape")
@click.option("--point", required=True, help="The escaping @p")
@click.option("--parent", required=True, help="The requested parent @p")
@click.option("--prior-proof", required=True, type=click.Path(exists=True, dir_okay=False), help=_MGMT_PRIOR_PROOF_HELP)
@click.option("--sig-hex", default=None, help="Sponsor's off-chain pre-signature (hex, 64 bytes) — optional")
@click.option("--fee-rate", type=int, default=2, show_default=True)
@click.option("--network", type=click.Choice(["main", "testnet"]), default="main", show_default=True)
@click.option("--output-dir", type=click.Path(file_okay=False, dir_okay=True), default=".", show_default=True)
@click.option("--mempool-base", default=MEMPOOL_API_URL, show_default=True)
def cmd_escape(point, parent, prior_proof, sig_hex, fee_rate, network, output_dir, mempool_base):
    """%escape sotx — ask a new parent to adopt you. Chains off --prior-proof."""
    comet_p = patp_to_int(point)
    parent_p = patp_to_int(parent)
    escape_sig = int.from_bytes(bytes.fromhex(sig_hex), "little") if sig_hex else None
    attestation = encode_escape_sotx(comet_p=comet_p, parent_p=parent_p, escape_sig=escape_sig)
    _run_management_op("escape", point, prior_proof, fee_rate, network, output_dir, mempool_base, attestation)


@cli.command("cancel-escape")
@click.option("--point", required=True)
@click.option("--parent", required=True, help="Parent of the escape to cancel")
@click.option("--prior-proof", required=True, type=click.Path(exists=True, dir_okay=False), help=_MGMT_PRIOR_PROOF_HELP)
@click.option("--fee-rate", type=int, default=2, show_default=True)
@click.option("--network", type=click.Choice(["main", "testnet"]), default="main", show_default=True)
@click.option("--output-dir", type=click.Path(file_okay=False, dir_okay=True), default=".", show_default=True)
@click.option("--mempool-base", default=MEMPOOL_API_URL, show_default=True)
def cmd_cancel_escape(point, parent, prior_proof, fee_rate, network, output_dir, mempool_base):
    """%cancel-escape sotx — rescind a pending escape request. Chains off --prior-proof."""
    comet_p = patp_to_int(point)
    parent_p = patp_to_int(parent)
    attestation = encode_cancel_escape_sotx(comet_p=comet_p, parent_p=parent_p)
    _run_management_op("cancel-escape", point, prior_proof, fee_rate, network, output_dir, mempool_base, attestation)


@cli.command("fief")
@click.option("--point", required=True)
@click.option("--prior-proof", required=True, type=click.Path(exists=True, dir_okay=False), help=_MGMT_PRIOR_PROOF_HELP)
@click.option("--ip", default=None, help="IPv4 or IPv6 address. Omit to clear fief.")
@click.option("--port", type=int, default=None, help="Port (required if --ip is given)")
@click.option("--fee-rate", type=int, default=2, show_default=True)
@click.option("--network", type=click.Choice(["main", "testnet"]), default="main", show_default=True)
@click.option("--output-dir", type=click.Path(file_okay=False, dir_okay=True), default=".", show_default=True)
@click.option("--mempool-base", default=MEMPOOL_API_URL, show_default=True)
def cmd_fief(point, prior_proof, ip, port, fee_rate, network, output_dir, mempool_base):
    """%fief sotx — set or clear a comet's static IP/port fief. Chains off --prior-proof."""
    comet_p = patp_to_int(point)
    if ip is None:
        fief = None
    else:
        if port is None:
            raise click.UsageError("--port is required when --ip is given")
        try:
            if ":" in ip:
                ip_int = int(ipaddress.IPv6Address(ip))
                fief = ("is", ip_int, port)
            else:
                ip_int = int(ipaddress.IPv4Address(ip))
                fief = ("if", ip_int, port)
        except ValueError as e:
            raise click.UsageError(f"bad IP address {ip!r}: {e}")
    attestation = encode_fief_sotx(comet_p=comet_p, fief=fief)
    _run_management_op("fief", point, prior_proof, fee_rate, network, output_dir, mempool_base, attestation)


def _run_management_op(op: str, point: str, prior_proof_path: str, fee_rate: int, network: str, output_dir: str, mempool_base: str, attestation: bytes) -> None:
    """Shared flow for management ops: spend the point's current sont (the
    commit output of the prior op, identified by `--prior-proof`), build a new
    confidential commit for `attestation`, await signed PSBT, broadcast, emit
    `<patp>-<op>-<txid>.proof.json`.

    This is required by urb-core's ++is-sont-in-input invariant — the next
    management op must spend the point's previous on-chain home, not an
    unrelated wallet UTXO.
    """
    print()
    print("=" * 60)
    print(f"  CAUSEWAY — Confidential {op.upper()} for {point}")
    print("=" * 60)

    try:
        prior = load_proof_json(prior_proof_path)
    except Exception as e:
        click.echo(click.style(f"  cannot load --prior-proof: {e}", fg="red"))
        sys.exit(1)

    # Sanity: prior proof must be for the same point.
    if prior.get("patp") and prior["patp"] != point:
        click.echo(click.style(
            f"  prior proof is for {prior['patp']}, not {point}", fg="red"))
        sys.exit(1)

    print(f"\n  Chaining from prior {prior.get('op', '?')} op: "
          f"commit={prior.get('commit_txid','?')[:16]}..:{prior.get('commit_vout',0)} "
          f"value={prior.get('commit_value','?')}")

    psbt_obj, proof = build_chained_commit_psbt(
        prior_proof=prior,
        new_attestation_bytes=attestation,
        fee_rate=fee_rate,
        network=network,
    )
    proof["op"] = op
    proof["patp"] = point

    os.makedirs(output_dir, exist_ok=True)
    pier = point.lstrip("~")
    psbt_path = os.path.join(output_dir, f"{pier}-{op}.psbt")
    unsigned_b64 = psbt_obj.to_base64()
    with open(psbt_path, "w") as f:
        f.write(unsigned_b64 + "\n")
    print(f"\n  Wrote unsigned PSBT: {psbt_path}")
    click.echo(click.style(
        "\n  Note: this PSBT's input has PSBT_IN_TAP_MERKLE_ROOT set so your\n"
        "  signer can compute the taproot tweak. Sparrow v1.8+ supports this;\n"
        "  older signers will refuse to sign.",
        fg="yellow",
    ))

    signed_b64 = _await_signed_psbt(unsigned_b64)
    try:
        commit_txid, tx_hex = _extract_tx_from_psbt(signed_b64)
    except Exception as e:
        click.echo(click.style(f"\n  Error extracting signed tx: {e}", fg="red"))
        sys.exit(1)

    proof["commit_txid"] = commit_txid
    proof_path = os.path.join(output_dir, f"{pier}-{op}-{commit_txid[:12]}.proof.json")
    write_proof_json(proof, proof_path)
    print(f"  Wrote proof: {proof_path}")

    print("\n  Broadcasting commit...")
    try:
        broadcast_id = _broadcast_tx(tx_hex, mempool_base=mempool_base)
    except Exception as e:
        click.echo(click.style(f"  Broadcast failed: {e}\n  Signed tx hex: {tx_hex}", fg="red"))
        sys.exit(1)
    print(f"  Broadcast: {tx_link(broadcast_id)}")

    print("\n" + "=" * 60)
    click.echo(click.style(
        f"  {op.upper()} attestation broadcast for {point}.\n"
        f"  Sont chain: {prior.get('commit_txid','?')[:10]}..:0 → {commit_txid[:10]}..:0\n"
        f"  Use {proof_path} as --prior-proof for the NEXT management op.\n"
        f"  Also import into your ship's %spv-wallet state:\n"
        f"    :spv-wallet|import-proof '{open(proof_path).read()[:40]}...'\n",
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
def spawn_connect(xpub, invite, fee_rate, network, output_dir, miner, mempool_base):
    """Spawn using a user-provided wallet (xpub / descriptor). You sign the PSBT externally."""
    run_spawn_connect(xpub, invite, fee_rate, network, output_dir, miner, mempool_base)


@spawn.command("generate")
@click.option("--invite", default=None)
@click.option("--fee-rate", type=int, default=2, show_default=True)
@click.option("--network", type=click.Choice(["main", "testnet"]), default="main", show_default=True)
@click.option("--output-dir", type=click.Path(file_okay=False, dir_okay=True), default=".", show_default=True)
@click.option("--miner", default=COMET_MINER_BIN, show_default=True)
@click.option("--mempool-base", default=MEMPOOL_API_URL, show_default=True)
def spawn_generate(invite, fee_rate, network, output_dir, miner, mempool_base):
    """Generate a fresh BIP-39 wallet, fund it, spawn, and emit a boot one-liner."""
    run_spawn_generate(invite, fee_rate, network, output_dir, miner, mempool_base)


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


# =========================================================================
#  Spawn flow orchestration — shared between `spawn connect` and `spawn generate`
# =========================================================================


def _build_spawn_psbt_and_proof(
    *,
    utxo: dict,
    source: KeySource,
    attestation_bytes: bytes,
    fee_rate: int,
    include_change: bool = True,
) -> tuple[psbt.PSBT, dict]:
    """Assemble the confidential commit PSBT for a spawn op.

    Uses the funding UTXO's own xonly as the commit output's internal key so
    the sat stays key-path-spendable by the point owner. If include_change and
    funding is large enough, splits the remainder into a change output at
    m/<account>/1/0.
    """
    change_args: dict = {}
    if include_change and utxo["value"] > 2_000:
        change_addr, change_spk, change_xonly, change_path = source.derive_address(1, 0)
        change_args = dict(
            change_internal_xonly=change_xonly,
            change_script_pubkey=change_spk,
            change_path=change_path,
        )

    psbt_obj, proof = build_confidential_commit_psbt(
        utxo_txid=utxo["txid"],
        utxo_vout=utxo["vout"],
        utxo_value=utxo["value"],
        utxo_script_pubkey=utxo["scriptpubkey"],
        funding_internal_xonly=utxo["xonly"],
        funding_path=utxo["path"],
        funding_fingerprint=source.master_fingerprint,
        attestation_bytes=attestation_bytes,
        fee_rate=fee_rate,
        network=source.network,
        **change_args,
    )
    return psbt_obj, proof


def _await_signed_psbt(unsigned_b64: str) -> str:
    """Prompt the user to paste the signed PSBT back. Returns signed-psbt base64."""
    print()
    print("  Next steps:")
    print("    1. Load the unsigned PSBT below into your Bitcoin wallet (Sparrow, BlueWallet,")
    print("       Passport, Keystone, Coldcard, etc).")
    print("    2. Review + sign.")
    print("    3. Paste the signed PSBT back here (base64).")
    print()
    print("  Unsigned PSBT:")
    print()
    print(f"    {unsigned_b64}")
    print()
    while True:
        try:
            entry = input("  Signed PSBT (base64) > ").strip()
        except EOFError:
            sys.exit(1)
        if not entry:
            continue
        try:
            psbt.PSBT.from_base64(entry)
            return entry
        except Exception as e:
            print(f"  That doesn't look like a valid PSBT ({e}). Try again.")


def _extract_tx_from_psbt(signed_b64: str) -> tuple[str, str]:
    """Finalize and extract raw tx hex from a signed PSBT. Returns (txid, tx_hex)."""
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
    # txid = double-sha256 of stripped (no-witness) tx, reversed to display order
    stripped = raw_tx.serialize(segwit=False)
    import hashlib as _hl
    h1 = _hl.sha256(stripped).digest()
    h2 = _hl.sha256(h1).digest()
    txid = h2[::-1].hex()
    return txid, tx_hex


def _broadcast_tx(tx_hex: str, *, mempool_base: str = MEMPOOL_API_URL) -> str:
    r = requests.post(f"{mempool_base}/tx", data=tx_hex, timeout=30)
    if not r.ok:
        raise RuntimeError(f"broadcast failed: {r.status_code} {r.text}")
    return r.text.strip()


def run_spawn_connect(xpub_str: str, invite: str | None, fee_rate: int, network: str, output_dir: str, miner: str, mempool_base: str) -> None:
    print()
    print("=" * 60)
    print("  CAUSEWAY — Confidential Comet Spawn (Connect Wallet)")
    print("=" * 60)

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
    utxo = pick_utxo_interactive(utxos)

    print(f"\n  Mining comet with tweak from ({utxo['txid']}:{utxo['vout']},0)...")
    miner_result = mine_comet_from_utxo(utxo["txid"], utxo["vout"], 0, miner)
    comet = miner_result["comet"]
    feed = miner_result["feed"]
    ring_uw = miner_result.get("ring", "")
    print(f"  Mined: {comet}")

    tweak_raw = build_tweak_bytes(utxo["txid"], utxo["vout"], 0)
    pass_atom = derive_pass_from_ring(ring_uw, tweak_raw)
    comet_p = patp_to_int(comet)

    # Build the attestation
    spkh = compute_spkh(utxo["address"], utxo["value"])
    attestation = encode_spawn_sotx(
        comet_p=comet_p,
        pass_atom=pass_atom,
        spkh=spkh,
        vout=utxo["vout"],
        off=0,
        tej=0,
        fief=None,
    )

    psbt_obj, proof = _build_spawn_psbt_and_proof(
        utxo=utxo,
        source=source,
        attestation_bytes=attestation,
        fee_rate=fee_rate,
    )
    proof["op"] = "spawn"
    proof["patp"] = comet
    proof["pass_atom_hex"] = hex(pass_atom)

    os.makedirs(output_dir, exist_ok=True)
    pier = comet.lstrip("~")
    psbt_path = os.path.join(output_dir, f"{pier}-spawn.psbt")
    proof_path = os.path.join(output_dir, f"{pier}-spawn.proof.json")

    unsigned_b64 = psbt_obj.to_base64()
    with open(psbt_path, "w") as f:
        f.write(unsigned_b64)
        f.write("\n")
    print(f"\n  Wrote unsigned PSBT: {psbt_path}")

    signed_b64 = _await_signed_psbt(unsigned_b64)
    try:
        commit_txid, tx_hex = _extract_tx_from_psbt(signed_b64)
    except Exception as e:
        click.echo(click.style(f"\n  Error extracting signed tx: {e}", fg="red"))
        sys.exit(1)

    proof["commit_txid"] = commit_txid
    write_proof_json(proof, proof_path)
    print(f"  Wrote proof: {proof_path}")

    print("\n  Broadcasting commit...")
    try:
        broadcast_id = _broadcast_tx(tx_hex, mempool_base=mempool_base)
    except Exception as e:
        click.echo(click.style(f"  Broadcast failed: {e}\n  Signed tx hex: {tx_hex}", fg="red"))
        sys.exit(1)
    print(f"  Broadcast: {tx_link(broadcast_id)}")

    _print_boot_oneliner(comet, feed, proof_path)


def run_spawn_generate(invite: str | None, fee_rate: int, network: str, output_dir: str, miner: str, mempool_base: str) -> None:
    print()
    print("=" * 60)
    print("  CAUSEWAY — Confidential Comet Spawn (Generate New Wallet)")
    print("=" * 60)

    mnemonic = generate_new_mnemonic(strength_bits=128)
    print_seed_box(mnemonic)
    confirm_seed_saved(mnemonic)

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

    print(f"\n  Mining comet...")
    miner_result = mine_comet_from_utxo(utxo["txid"], utxo["vout"], 0, miner)
    comet = miner_result["comet"]
    feed = miner_result["feed"]
    ring_uw = miner_result.get("ring", "")
    print(f"  Mined: {comet}")

    tweak_raw = build_tweak_bytes(utxo["txid"], utxo["vout"], 0)
    pass_atom = derive_pass_from_ring(ring_uw, tweak_raw)
    comet_p = patp_to_int(comet)

    spkh = compute_spkh(utxo["address"], utxo["value"])
    attestation = encode_spawn_sotx(
        comet_p=comet_p,
        pass_atom=pass_atom,
        spkh=spkh,
        vout=utxo["vout"],
        off=0,
        tej=0,
        fief=None,
    )
    psbt_obj, proof = _build_spawn_psbt_and_proof(
        utxo=utxo,
        source=source,
        attestation_bytes=attestation,
        fee_rate=fee_rate,
    )
    proof["op"] = "spawn"
    proof["patp"] = comet
    proof["pass_atom_hex"] = hex(pass_atom)

    # Sign the PSBT in-process (we have the seed).
    p_signed = psbt_obj
    # Populate sighash, sign with taproot key at the funding derivation.
    sub_path = source.account_path + [utxo["change"], utxo["index"]]
    sub_xprv = root.derive(_path_to_str(sub_path))
    sub_pub = sub_xprv.key.get_public_key()
    # sign_with will sign any input where the descendant key matches
    p_signed.sign_with(root)
    # Extract finalised tx
    # The PSBT now has taproot_key_sig on input 0
    signed_b64 = p_signed.to_base64()
    commit_txid, tx_hex = _extract_tx_from_psbt(signed_b64)

    proof["commit_txid"] = commit_txid
    os.makedirs(output_dir, exist_ok=True)
    pier = comet.lstrip("~")
    proof_path = os.path.join(output_dir, f"{pier}-spawn.proof.json")
    write_proof_json(proof, proof_path)

    print("\n  Broadcasting commit...")
    try:
        broadcast_id = _broadcast_tx(tx_hex, mempool_base=mempool_base)
    except Exception as e:
        click.echo(click.style(f"  Broadcast failed: {e}\n  Signed tx hex: {tx_hex}", fg="red"))
        sys.exit(1)
    print(f"  Broadcast: {tx_link(broadcast_id)}")

    print("\n  WRITE DOWN YOUR SEED PHRASE AGAIN — last chance:")
    print_seed_box(mnemonic)
    confirm_seed_saved(mnemonic)

    _print_boot_oneliner(comet, feed, proof_path)


def _print_boot_oneliner(comet: str, feed: str, proof_path: str) -> None:
    pier = comet.lstrip("~")
    print("\n" + "=" * 60)
    print("  SPAWN COMPLETE")
    print("=" * 60)
    print(f"\n  Your comet: {comet}")
    print(f"  Feed atom: {feed[:60]}{'...' if len(feed) > 60 else ''}")
    print(f"  Proof:     {proof_path}")
    print()
    print("  To boot:")
    print(f"    curl -fsSL https://groundwire.io/causeway/boot.sh | \\")
    print(f"      bash -s -- --comet {comet} --feed {feed} --proof {proof_path}")
    print()
    click.echo(click.style(
        "  NOTE: The --proof argument is preserved for future runtime support.\n"
        "  The runtime does not yet consume confidential-comet proofs; your\n"
        "  comet will boot and claim its identity, but other ships will only\n"
        "  be able to verify the identity once runtime proof-ingest lands.\n",
        fg="yellow",
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
