"""Noun (de)serialization for talking to a vere conn.sock.

Nouns are represented in Python as:
  - atom  -> int  (>= 0)
  - cell  -> tuple (head, tail)

This module provides jam/cue (the Urbit noun serialization), newt framing
(the conn.sock wire format), and helpers for building/walking the noun shapes
the harness needs (terms, paths, null-terminated lists, treap maps, tapes).

jam/mat are ported from the golden-fixture-validated implementation in
causeway/desktop/causeway.py; cue/rub are the exact inverses.
"""

from __future__ import annotations

from typing import Iterator


# ---------------------------------------------------------------------------
#  mat / rub  (length-prefixed atom coding)
# ---------------------------------------------------------------------------

def mat(a: int) -> tuple[int, int]:
    """Length-encode an atom. Returns (bit_count, bits_value), LSB-first.

    value 0          -> single 1 bit
    value a > 0      -> c zeros, separator 1, (c-1) low bits of b, then b bits
                        of a, where b = a.bit_length(), c = b.bit_length().
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


# ---------------------------------------------------------------------------
#  jam
# ---------------------------------------------------------------------------

def jam(noun) -> int:
    """Serialize a noun to an atom."""
    bits: list[int] = []
    pos = 0
    refs: dict = {}

    def write_bit(b: int) -> None:
        nonlocal pos
        bits.append(1 if b else 0)
        pos += 1

    def write_bits(val: int, count: int) -> None:
        nonlocal pos
        for i in range(count):
            bits.append((val >> i) & 1)
        pos += count

    def write_mat(val: int) -> None:
        p, q = mat(val)
        write_bits(q, p)

    def encode(n) -> None:
        nonlocal pos
        start = pos
        if isinstance(n, tuple):
            if n in refs:
                write_bit(1)
                write_bit(1)
                write_mat(refs[n])
            else:
                refs[n] = start
                write_bit(1)
                write_bit(0)
                encode(n[0])
                encode(n[1])
        else:
            if n in refs:
                n_bits = n.bit_length() if n > 0 else 1
                ref_bits = refs[n].bit_length() if refs[n] > 0 else 1
                if n_bits <= ref_bits:
                    write_bit(0)
                    write_mat(n)
                else:
                    write_bit(1)
                    write_bit(1)
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


# ---------------------------------------------------------------------------
#  cue
# ---------------------------------------------------------------------------

class _BitReader:
    __slots__ = ("val", "pos")

    def __init__(self, val: int):
        self.val = val
        self.pos = 0

    def bit(self) -> int:
        b = (self.val >> self.pos) & 1
        self.pos += 1
        return b

    def bits(self, count: int) -> int:
        out = 0
        for i in range(count):
            out |= self.bit() << i
        return out

    def rub(self) -> int:
        c = 0
        while self.bit() == 0:
            c += 1
        if c == 0:
            return 0
        low = self.bits(c - 1)
        b = (1 << (c - 1)) | low
        return self.bits(b)


def cue(data: int):
    """Deserialize an atom back into a noun (int | tuple)."""
    r = _BitReader(data)
    refs: dict[int, object] = {}

    def decode():
        start = r.pos
        if r.bit() == 0:                 # 0  -> atom
            a = r.rub()
            refs[start] = a
            return a
        if r.bit() == 0:                 # 10 -> cell
            head = decode()
            tail = decode()
            cell = (head, tail)
            refs[start] = cell
            return cell
        return refs[r.rub()]             # 11 -> backref

    return decode()


# ---------------------------------------------------------------------------
#  newt framing  (conn.sock wire format)
# ---------------------------------------------------------------------------

def jam_to_bytes(atom: int) -> bytes:
    n = (atom.bit_length() + 7) // 8
    return atom.to_bytes(n, "little") if n > 0 else b"\x00"


def bytes_to_atom(b: bytes) -> int:
    return int.from_bytes(b, "little")


def newt_frame(payload: bytes) -> bytes:
    """5-byte header (tag 0x00 + u32 LE length) + payload."""
    return b"\x00" + len(payload).to_bytes(4, "little") + payload


def newt_encode(noun) -> bytes:
    return newt_frame(jam_to_bytes(jam(noun)))


# ---------------------------------------------------------------------------
#  noun construction helpers
# ---------------------------------------------------------------------------

def tas(s: str) -> int:
    """@tas / @ta / cord: little-endian ascii bytes as an atom."""
    return int.from_bytes(s.encode("ascii"), "little") if s else 0


def cord(s: str) -> int:
    """@t cord: little-endian utf-8 bytes as an atom (for arbitrary text)."""
    return int.from_bytes(s.encode("utf-8"), "little") if s else 0


def from_tas(a: int) -> str:
    n = (a.bit_length() + 7) // 8
    return a.to_bytes(n, "little").decode("ascii", "replace") if n else ""


def patp_to_atom(patp: str) -> int:
    """@p text -> atom. Defers to urbit-ob via causeway if available; else a
    minimal decoder for galaxy/star/planet/moon/comet phonetic strings."""
    from . import obphon
    return obphon.patp_to_num(patp)


def cell(*items):
    """Right-folded cell with NO null terminator: cell(a,b,c) -> (a,(b,c))."""
    if len(items) == 1:
        return items[0]
    out = items[-1]
    for x in reversed(items[:-1]):
        out = (x, out)
    return out


def nlist(items) -> object:
    """Null-terminated list: [i0 i1 ... ~]  ->  (i0,(i1,(...,0)))."""
    out: object = 0
    for x in reversed(list(items)):
        out = (x, out)
    return out


def path(*knots: str) -> object:
    """A path: null-terminated list of @ta knot atoms."""
    return nlist(tas(k) for k in knots)


def unit(x) -> object:
    """(some x) -> [~ x] = (0, x);  ~ stays 0 via unit_none."""
    return (0, x)


unit_none = 0


# ---------------------------------------------------------------------------
#  noun walking helpers
# ---------------------------------------------------------------------------

def list_iter(noun) -> Iterator:
    """Walk a null-terminated list noun, yielding elements."""
    while noun != 0:
        if not isinstance(noun, tuple):
            raise ValueError(f"improper list tail: {noun!r}")
        yield noun[0]
        noun = noun[1]


def map_iter(noun) -> Iterator[tuple]:
    """Walk a hoon treap map ((tree (pair key value))), yielding (key, value).

    A node is [n=[key value] l=tree r=tree]; ~ is 0.
    """
    if noun == 0:
        return
    if not isinstance(noun, tuple):
        raise ValueError(f"not a treap: {noun!r}")
    n, lr = noun
    l, r = lr
    key, value = n
    yield (key, value)
    yield from map_iter(l)
    yield from map_iter(r)


def set_iter(noun) -> Iterator:
    """Walk a hoon treap set ((tree item)), yielding items."""
    if noun == 0:
        return
    n, lr = noun
    l, r = lr
    yield n
    yield from set_iter(l)
    yield from set_iter(r)


def from_tape(noun) -> str:
    """A tape ((list @tD)) -> str."""
    return "".join(chr(c) for c in list_iter(noun))


def from_cord(a: int) -> str:
    return from_tas(a)
