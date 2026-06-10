"""Noun (de)serialization for talking to a vere conn.sock.

Nouns are represented in Python as:
  - atom  -> int  (>= 0)
  - cell  -> tuple (head, tail)   (2-tuples, right-nested)

This module provides jam/cue (noun serialization), mat/rub (length-prefixed
atom coding), newt framing (the conn.sock wire format), and helpers for
building/walking the noun shapes the harness needs (terms, paths,
null-terminated lists, treap maps, tapes, @p).

`jam(noun)` returns little-endian **bytes** (`bytes_to_atom` converts if the
atom view is wanted); `cue` accepts bytes or an int atom. Both are iterative
(deep tapes in goof tangs would blow Python's recursion limit) and cue is
bounds-checked (truncated input raises instead of spinning).

The atom-backref rule matches vere's u3s_jam (pkg/noun/serial.c): an atom is
re-encoded inline unless it is strictly wider than its backref position;
cells always backref once seen. Byte-for-byte cross-validated against
`urbit eval -j -n` / `-c -n` in tests/test_noun.py.
"""

from __future__ import annotations

from typing import Iterator, Union

from . import obphon

Noun = Union[int, tuple]


# ---------------------------------------------------------------------------
#  mat / rub  (length-prefixed atom coding, bit-level, LSB-first)
# ---------------------------------------------------------------------------

def mat(a: int) -> tuple[int, int]:
    """Length-encode an atom. Returns (bit_count, bits_value), LSB-first.

    value 0          -> single 1 bit
    value a > 0      -> c zeros, separator 1, (c-1) low bits of b, then b bits
                        of a, where b = a.bit_length(), c = b.bit_length().
    """
    if a < 0:
        raise ValueError("atoms are non-negative")
    if a == 0:
        return (1, 1)
    b = a.bit_length()
    c = b.bit_length()
    low_b = b & ((1 << (c - 1)) - 1) if c > 1 else 0
    mixed = low_b | (a << (c - 1))
    q = (1 << c) | (mixed << (c + 1))
    return (2 * c + b, q)


def rub(val: int, pos: int, limit: int) -> tuple[int, int]:
    """Decode a mat-encoded atom from bit `pos` of `val`; `limit` is the
    total number of valid bits. Returns (atom, new_pos)."""
    c = 0
    while not (val >> (pos + c)) & 1:
        c += 1
        if pos + c > limit:
            raise ValueError("rub: truncated mat (ran off end of input)")
    pos += c + 1
    if c == 0:
        return 0, pos
    low = (val >> pos) & ((1 << (c - 1)) - 1)
    pos += c - 1
    b = (1 << (c - 1)) | low
    if pos + b > limit:
        raise ValueError("rub: truncated atom body")
    return (val >> pos) & ((1 << b) - 1), pos + b


# ---------------------------------------------------------------------------
#  jam / cue
# ---------------------------------------------------------------------------

class _BitWriter:
    __slots__ = ("buf", "pos")

    def __init__(self) -> None:
        self.buf = bytearray()
        self.pos = 0  # total bits written

    def bit(self, b: int) -> None:
        if self.pos & 7 == 0:
            self.buf.append(0)
        if b:
            self.buf[-1] |= 1 << (self.pos & 7)
        self.pos += 1

    def bits(self, val: int, count: int) -> None:
        for i in range(count):
            self.bit((val >> i) & 1)

    def mat(self, a: int) -> None:
        p, q = mat(a)
        self.bits(q, p)


def jam(noun: Noun) -> bytes:
    """Serialize a noun to its jam, as little-endian bytes."""
    w = _BitWriter()
    refs: dict = {}
    stack: list = [noun]
    while stack:
        n = stack.pop()
        if isinstance(n, tuple):
            if len(n) != 2:
                raise ValueError(f"cells must be 2-tuples, got {len(n)}-tuple")
            bak = refs.get(n)
            if bak is not None:
                w.bit(1)
                w.bit(1)
                w.mat(bak)
            else:
                refs[n] = w.pos
                w.bit(1)
                w.bit(0)
                stack.append(n[1])
                stack.append(n[0])
        elif isinstance(n, int) and not isinstance(n, bool):
            if n < 0:
                raise ValueError("atoms are non-negative")
            bak = refs.get(n)
            #  vere (u3s_jam): inline the atom unless it is strictly wider
            #  than the backref position (met-0 semantics: width of 0 is 0)
            if bak is not None and n.bit_length() > bak.bit_length():
                w.bit(1)
                w.bit(1)
                w.mat(bak)
            else:
                if bak is None:
                    refs[n] = w.pos
                w.bit(0)
                w.mat(n)
        else:
            raise TypeError(f"not a noun: {type(n).__name__}")
    return bytes(w.buf)


def cue(data: Union[bytes, bytearray, int]) -> Noun:
    """Deserialize a jam (little-endian bytes, or the atom as an int)."""
    if isinstance(data, (bytes, bytearray)):
        val = int.from_bytes(data, "little")
        limit = len(data) * 8
    else:
        val = int(data)
        if val < 0:
            raise ValueError("cue: negative atom")
        #  a valid jam always ends in a 1 bit, so bit_length is exact
        limit = val.bit_length()
    if limit == 0:
        raise ValueError("cue: empty input")
    refs: dict[int, Noun] = {}
    stack: list = []  # frames: [start_pos, head_or_None]
    pos = 0
    while True:
        start = pos
        if not (val >> pos) & 1:                  # 0   -> atom (mat)
            noun, pos = rub(val, pos + 1, limit)
            refs[start] = noun
        elif not (val >> (pos + 1)) & 1:          # 1,0 -> cell
            pos += 2
            stack.append([start, None])
            continue
        else:                                     # 1,1 -> backref (mat)
            bak, pos = rub(val, pos + 2, limit)
            try:
                noun = refs[bak]
            except KeyError:
                raise ValueError(f"cue: dangling backref to bit {bak}") from None
        #  a noun is complete; unwind it through pending cell frames
        while True:
            if not stack:
                return noun
            frame = stack[-1]
            if frame[1] is None:
                frame[1] = noun           # head done; tail decodes next
                break
            stack.pop()
            noun = (frame[1], noun)
            refs[frame[0]] = noun


# ---------------------------------------------------------------------------
#  newt framing  (conn.sock / `urbit eval -n` wire format)
# ---------------------------------------------------------------------------

NEWT_TAG = 0x00


def jam_to_bytes(atom: int) -> bytes:
    """A jammed noun held as an int -> minimal little-endian bytes."""
    n = (atom.bit_length() + 7) // 8
    return atom.to_bytes(n, "little") if n > 0 else b"\x00"


def bytes_to_atom(b: bytes) -> int:
    return int.from_bytes(b, "little")


def newt_frame(payload: bytes) -> bytes:
    """5-byte header (tag 0x00 + u32 LE byte count) + payload."""
    return b"\x00" + len(payload).to_bytes(4, "little") + payload


def newt_encode(noun: Noun) -> bytes:
    """jam a noun and wrap it in a newt frame."""
    return newt_frame(jam(noun))


def read_newt(sock_file) -> bytes:
    """Read one newt frame from a file-like object; return the payload.

    Raises EOFError on clean EOF before a header, ValueError on a bad tag
    byte or a truncated frame.
    """
    head = _read_exactly(sock_file, 5, allow_empty_eof=True)
    if head is None:
        raise EOFError("newt: connection closed")
    if head[0] != NEWT_TAG:
        raise ValueError(f"newt: bad tag byte 0x{head[0]:02x}")
    length = int.from_bytes(head[1:5], "little")
    return _read_exactly(sock_file, length)


def _read_exactly(f, n: int, allow_empty_eof: bool = False):
    buf = b""
    while len(buf) < n:
        chunk = f.read(n - len(buf))
        if not chunk:
            if allow_empty_eof and not buf:
                return None
            raise ValueError(f"newt: short read ({len(buf)}/{n} bytes)")
        buf += chunk
    return buf


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


def from_cord(a: int) -> str:
    n = (a.bit_length() + 7) // 8
    return a.to_bytes(n, "little").decode("utf-8", "replace") if n else ""


def cell(*items: Noun) -> Noun:
    """Right-folded cell with NO null terminator: cell(a,b,c) -> (a,(b,c))."""
    if not items:
        raise ValueError("cell() needs at least one item")
    if len(items) == 1:
        return items[0]
    out = items[-1]
    for x in reversed(items[:-1]):
        out = (x, out)
    return out


def nlist(items) -> Noun:
    """Null-terminated list: [i0 i1 ... ~]  ->  (i0,(i1,(...,0)))."""
    out: Noun = 0
    for x in reversed(list(items)):
        out = (x, out)
    return out


def path(*knots) -> Noun:
    """A path: null-terminated list of @ta knots. Atom knots pass through.

    path('foo', 'bar') == /foo/bar == [%foo %bar ~]
    """
    return nlist(k if isinstance(k, int) else tas(k) for k in knots)


def tape(s: str) -> Noun:
    """str -> tape ((list @tD): null-terminated single-byte atoms)."""
    return nlist(s.encode("utf-8"))


def unit(x: Noun) -> Noun:
    """(some x) -> [~ x] = (0, x);  ~ stays 0 via unit_none."""
    return (0, x)


unit_none = 0


# ---------------------------------------------------------------------------
#  noun walking helpers
# ---------------------------------------------------------------------------

def list_iter(noun: Noun) -> Iterator[Noun]:
    """Walk a null-terminated list noun, yielding elements."""
    while noun != 0:
        if not isinstance(noun, tuple):
            raise ValueError(f"improper list tail: {noun!r}")
        yield noun[0]
        noun = noun[1]


def map_iter(noun: Noun) -> Iterator[tuple]:
    """Walk a hoon treap map ((tree (pair key value))), yielding (key, value).

    A node is [n=[key value] l=tree r=tree]; ~ is 0. Iterative — gall maps
    can be deep.
    """
    stack = [noun]
    while stack:
        node = stack.pop()
        if node == 0:
            continue
        if not isinstance(node, tuple):
            raise ValueError(f"not a treap: {node!r}")
        n, lr = node
        l, r = lr
        yield n
        stack.append(r)
        stack.append(l)


def set_iter(noun: Noun) -> Iterator[Noun]:
    """Walk a hoon treap set ((tree item)), yielding items."""
    yield from map_iter(noun)


def from_tape(noun: Noun) -> str:
    """A tape ((list @tD)) -> str."""
    return "".join(chr(c) for c in list_iter(noun))


# ---------------------------------------------------------------------------
#  @p  (phonemic ship names; the ob feistel lives in obphon.py)
# ---------------------------------------------------------------------------

def patp_int(patp: str) -> int:
    """@p text like '~sampel-palnet' or a comet -> ship number (atom)."""
    return obphon.patp_to_num(patp)


def patp_str(ship: int) -> str:
    """Ship number -> @p text (inverse of patp_int)."""
    return obphon.num_to_patp(ship)


#  legacy aliases
patp_to_atom = patp_int
atom_to_patp = patp_str
