#!/usr/bin/env python3
"""Dump jam(noun) -> atom vectors for cue.ts round-trip testing.

Output: tests/fixtures/jam-vectors.json
"""

import json
import os
from typing import Tuple


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


def hoon_jam(noun) -> int:
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
        nonlocal pos
        if val == 0:
            bits.append(1)
            pos += 1
            return
        p, q = hoon_mat(val)
        write_bits(q, p)

    def encode(n):
        nonlocal pos
        start = pos
        if isinstance(n, tuple):
            if n in refs:
                write_bit(1); write_bit(1)
                write_mat(refs[n])
            else:
                refs[n] = start
                write_bit(1); write_bit(0)
                encode(n[0]); encode(n[1])
        else:
            if n in refs:
                n_bits = n.bit_length()
                ref_bits = refs[n].bit_length() if refs[n] > 0 else 1
                if n_bits <= ref_bits:
                    write_bit(0); write_mat(n)
                else:
                    write_bit(1); write_bit(1); write_mat(refs[n])
            else:
                refs[n] = start
                write_bit(0); write_mat(n)

    encode(noun)
    result = 0
    for i, b in enumerate(bits):
        result |= b << i
    return result


def n_to_json(n):
    """Serialize noun to JSON: atoms as {__atom__: "decimal"}, cells as [a, b]."""
    if isinstance(n, tuple):
        return [n_to_json(n[0]), n_to_json(n[1])]
    return {"__atom__": str(n)}


def pair(*items):
    """Build right-nested cell [a b c d] = (a, (b, (c, d)))."""
    if len(items) < 2:
        raise ValueError("pair needs >= 2")
    cur = items[-1]
    for x in reversed(items[:-1]):
        cur = (x, cur)
    return cur


def main():
    cases = []

    def add(name, noun):
        j = hoon_jam(noun)
        cases.append({
            "name": name,
            "noun": n_to_json(noun),
            "jam_hex": j.to_bytes((j.bit_length() + 7) // 8, "little").hex() if j > 0 else "01",
            "jam_bits": j.bit_length() if j > 0 else 1,
            "jam_dec": str(j),
        })

    add("atom/0", 0)
    add("atom/1", 1)
    add("atom/42", 42)
    add("atom/big", 0xDEADBEEFCAFEBABE_0123456789ABCDEF)
    add("cell/[0 0]", (0, 0))
    add("cell/[1 2]", (1, 2))
    add("cell/[[1 2] 3]", ((1, 2), 3))
    add("cell/4tuple", pair(1, 2, 3, 4))
    add("cell/nested", pair((1, 2), (3, (4, 5)), 6))
    # back-ref case: same big atom twice
    big = 0x0123456789ABCDEF_FEDCBA9876543210
    add("cell/backref-atom", (big, big))
    # back-ref cell case
    shared = (42, 99)
    add("cell/backref-cell", (shared, shared))
    # list-like: [1 2 3 ~] = (1, (2, (3, 0)))
    add("list/123", pair(1, 2, 3, 0))
    # unit containing a pair
    add("unit/some", (0, (5, 7)))
    # small map-like treap: [[k v] [left right]]
    add("map/tiny", ((1, 100), ((2, 200), 0), 0))

    out_path = os.path.join(
        os.path.dirname(os.path.abspath(__file__)),
        "fixtures", "jam-vectors.json"
    )
    os.makedirs(os.path.dirname(out_path), exist_ok=True)
    with open(out_path, "w") as f:
        json.dump(cases, f, indent=2)
    print(f"wrote {len(cases)} jam vectors to {out_path}")


if __name__ == "__main__":
    main()
