#!/usr/bin/env python3
"""
Dump bit-level golden vectors for the urb skim-sotx / full-sotx encoders.

Output: tests/fixtures/encoder-vectors.json

The reference implementation of `hoon_mat` + `BitWriter` is duplicated here
from gw-onboard.py so this script has no third-party deps. It exercises every
operation in sur/urb.hoon (spawn, keys, escape, cancel-escape, adopt, reject,
detach, fief, set-mang) plus a batch, with several field-value combinations.

Run:
    python3 tests/dump_vectors.py
"""

import json
import os
import secrets
from typing import Optional, Tuple


# -- hoon mat, copied from gw-onboard.py --------------------------------------

def hoon_mat(a: int) -> Tuple[int, int]:
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


# -- BitWriter, copied from gw-onboard.py -------------------------------------

class BitWriter:
    def __init__(self):
        self._bits = 0
        self._pos = 0

    def write(self, width: int, value: int):
        if width == 0:
            return
        mask = (1 << width) - 1
        self._bits |= (value & mask) << self._pos
        self._pos += width

    def write_mat(self, value: int):
        p, q = hoon_mat(value)
        self.write(p, q)

    def to_bytes(self) -> bytes:
        n = (self._pos + 7) // 8
        return self._bits.to_bytes(n, "little") if n > 0 else b""


# -- Encoder (mirrors lib/urb-encoder.hoon arm-by-arm) ------------------------

def en_sig(w: BitWriter, sig: Optional[int]) -> None:
    if sig is None:
        w.write(2, 0)
    else:
        w.write(2, 1)
        w.write(512, sig)


def en_fief_inline(w: BitWriter, fief) -> None:
    if fief is None:
        w.write(2, 0); return
    kind = fief["type"]
    if kind == "turf":
        raise ValueError("%turf not supported")
    if kind == "if":
        w.write(2, 2); w.write(32, fief["ip"]); w.write(16, fief["port"])
    elif kind == "is":
        w.write(2, 3); w.write(128, fief["ip"]); w.write(16, fief["port"])
    else:
        raise ValueError(f"bad fief: {kind}")


def en_sont(w: BitWriter, sont) -> None:
    w.write(1, 0)
    w.write(256, sont["txid"])  # atom
    w.write_mat(sont["vout"])
    w.write_mat(sont["off"])


def en_single(w: BitWriter, s) -> None:
    op = s["op"]
    if op == "spawn":
        w.write(7, 1); w.write(1, 0); w.write_mat(s["pass"])
        en_fief_inline(w, s["fief"])
        to = s["to"]
        w.write(256, to["spkh"])
        w.write_mat(to["off"]); w.write_mat(to["tej"])
        if to["vout"] is None:
            w.write(2, 0)
        else:
            w.write(2, 1); w.write_mat(to["vout"])
    elif op == "keys":
        w.write(7, 2); w.write(1, 1 if s["breach"] else 0); w.write_mat(s["pass"])
    elif op == "escape":
        w.write(7, 3); w.write(1, 0); w.write(128, s["parent"]); en_sig(w, s["sig"])
    elif op == "cancel-escape":
        w.write(7, 4); w.write(1, 0); w.write(128, s["parent"])
    elif op == "adopt":
        w.write(7, 5); w.write(1, 0); w.write(128, s["ship"])
    elif op == "reject":
        w.write(7, 6); w.write(1, 0); w.write(128, s["ship"])
    elif op == "detach":
        w.write(7, 7); w.write(1, 0); w.write(128, s["ship"])
    elif op == "fief":
        w.write(7, 11); w.write(1, 0); en_fief_inline(w, s["fief"])
    elif op == "set-mang":
        w.write(7, 8)
        m = s["mang"]
        if m is None:
            w.write(2, 0)
        elif m["type"] == "sont":
            w.write(2, 1); en_sont(w, m["sont"])
        else:
            w.write(2, 2); w.write(256, m["pass"])
    else:
        raise ValueError(f"bad op: {op}")


def write_skim(w: BitWriter, sot) -> None:
    if sot["op"] == "batch":
        items = sot["items"]
        assert len(items) > 1
        w.write(7, 10); w.write_mat(len(items))
        for it in items:
            en_single(w, it)
    else:
        en_single(w, sot)


def encode_skim(sot) -> bytes:
    w = BitWriter(); write_skim(w, sot); return w.to_bytes()


def encode_full(sots) -> bytes:
    w = BitWriter()
    for s in sots:
        en_sig(w, s["sig"]); w.write(128, s["ship"]); write_skim(w, s["skim"])
    return w.to_bytes()


# -- Fixture definitions ------------------------------------------------------

# Deterministic test values so vectors are stable across runs.
TXID = bytes.fromhex(
    "1122334455667788"
    "99aabbccddeeff00"
    "0123456789abcdef"
    "fedcba9876543210"
)
SPKH = bytes.fromhex(
    "deadbeefcafebabe"
    "0011223344556677"
    "8899aabbccddeeff"
    "00112233445566ff"
)
PASS_ATOM = 0x434347fad01_deadbeef_cafef00d_12345678 & ((1 << 256) - 1)
SIG_ATOM = (1 << 511) | 0xabcdef01  # 512-bit-ish


def txid_atom(b: bytes) -> int:
    return int.from_bytes(b, "little")


def spkh_atom(b: bytes) -> int:
    return int.from_bytes(b, "little")


def b2h(b: bytes) -> str:
    return b.hex()


def fixture(name: str, sot, kind: str = "skim"):
    if kind == "skim":
        out = encode_skim(sot)
    else:
        out = encode_full(sot)
    return {"name": name, "kind": kind, "input": sot, "output_hex": b2h(out)}


def main():
    fixtures = []

    # spawn, no fief, no vout
    fixtures.append(fixture("spawn/no-fief/no-vout", {
        "op": "spawn",
        "pass": PASS_ATOM,
        "fief": None,
        "to": {"spkh": spkh_atom(SPKH), "off": 0, "tej": 0, "vout": None},
    }))

    # spawn, if-fief, with vout
    fixtures.append(fixture("spawn/if-fief/vout", {
        "op": "spawn",
        "pass": PASS_ATOM,
        "fief": {"type": "if", "ip": 0xC0A80101, "port": 8443},
        "to": {"spkh": spkh_atom(SPKH), "off": 7, "tej": 42, "vout": 3},
    }))

    # spawn, is-fief (v6)
    fixtures.append(fixture("spawn/is-fief", {
        "op": "spawn",
        "pass": PASS_ATOM,
        "fief": {"type": "is", "ip": (0x2001 << 112) | 1, "port": 9000},
        "to": {"spkh": spkh_atom(SPKH), "off": 1, "tej": 2, "vout": None},
    }))

    # keys, no breach
    fixtures.append(fixture("keys/no-breach", {
        "op": "keys", "pass": PASS_ATOM, "breach": False,
    }))

    # keys, breach
    fixtures.append(fixture("keys/breach", {
        "op": "keys", "pass": PASS_ATOM, "breach": True,
    }))

    # escape, no sig
    fixtures.append(fixture("escape/no-sig", {
        "op": "escape", "parent": 0x1234_5678_9abc_def0_1122_3344_5566_7788, "sig": None,
    }))

    # escape, with sig
    fixtures.append(fixture("escape/with-sig", {
        "op": "escape", "parent": 0xDEADBEEF, "sig": SIG_ATOM,
    }))

    # cancel-escape / adopt / reject / detach
    for op in ("cancel-escape", "adopt", "reject", "detach"):
        key = "parent" if op == "cancel-escape" else "ship"
        fixtures.append(fixture(f"{op}/basic", {
            "op": op, key: 0xCAFE_BABE_DEAD_BEEF,
        }))

    # fief, none
    fixtures.append(fixture("fief/none", {"op": "fief", "fief": None}))

    # fief, if
    fixtures.append(fixture("fief/if", {
        "op": "fief", "fief": {"type": "if", "ip": 0x7F000001, "port": 80},
    }))

    # fief, is
    fixtures.append(fixture("fief/is", {
        "op": "fief", "fief": {"type": "is", "ip": 1, "port": 443},
    }))

    # set-mang, none
    fixtures.append(fixture("set-mang/none", {"op": "set-mang", "mang": None}))

    # set-mang, sont
    fixtures.append(fixture("set-mang/sont", {
        "op": "set-mang",
        "mang": {"type": "sont", "sont": {"txid": txid_atom(TXID), "vout": 0, "off": 0}},
    }))

    # set-mang, pass
    fixtures.append(fixture("set-mang/pass", {
        "op": "set-mang", "mang": {"type": "pass", "pass": PASS_ATOM},
    }))

    # batch: spawn + escape (mirrors gw-onboard flow)
    fixtures.append(fixture("batch/spawn+escape", {
        "op": "batch",
        "items": [
            {
                "op": "spawn",
                "pass": PASS_ATOM,
                "fief": None,
                "to": {"spkh": spkh_atom(SPKH), "off": 0, "tej": 0, "vout": None},
            },
            {"op": "escape", "parent": 0xAABB, "sig": SIG_ATOM},
        ],
    }))

    # full-sotx: one full envelope (no sig, then ship, then skim)
    fixtures.append(fixture("full/one-keys", [
        {
            "ship": 0x1122_3344_5566_7788_99AA_BBCC_DDEE_FF00,
            "sig": None,
            "skim": {"op": "keys", "pass": PASS_ATOM, "breach": False},
        }
    ], kind="full"))

    # full-sotx: two envelopes, second one with sig
    fixtures.append(fixture("full/two", [
        {
            "ship": 0xDEAD_BEEF,
            "sig": None,
            "skim": {"op": "fief", "fief": {"type": "if", "ip": 0x01020304, "port": 22}},
        },
        {
            "ship": 0xCAFE_F00D,
            "sig": SIG_ATOM,
            "skim": {"op": "adopt", "ship": 0xBEEF},
        },
    ], kind="full"))

    out_path = os.path.join(
        os.path.dirname(os.path.abspath(__file__)),
        "fixtures", "encoder-vectors.json"
    )
    os.makedirs(os.path.dirname(out_path), exist_ok=True)
    with open(out_path, "w") as f:
        # stringify bigints as decimal strings; JS reads with BigInt()
        def prep(v):
            if isinstance(v, dict):
                return {k: prep(x) for k, x in v.items()}
            if isinstance(v, list):
                return [prep(x) for x in v]
            if isinstance(v, int) and (v.bit_length() > 53 or v < 0 == False and v > 2**31):
                return {"__bigint__": str(v)}
            return v
        json.dump([prep(x) for x in fixtures], f, indent=2)
    print(f"wrote {len(fixtures)} vectors to {out_path}")


if __name__ == "__main__":
    main()
