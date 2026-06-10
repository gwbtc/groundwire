"""@p <-> number: phonemic syllables + the `ob` feistel un/scramble.

@p renders a ship number as big-endian byte pairs (prefix+suffix syllables)
after scrambling with +fein:ob; parsing decodes syllables and un-scrambles
with +fynd:ob. The scramble only touches the planet range (0x1.0000 ..
0xffff.ffff) and the low 32 bits of moons; galaxies/stars/comets pass
through unchanged — but comets still need the big-endian byte order, so
always go through this codec (NOT the little-endian @q codec).

Ported from ++ob in urbit pkg/arvo/sys/hoon.hoon and the vere jet
vere/pkg/noun/jets/e/fein_ob.c. Two details that differ from what the
urbit-ob JS package's lineage suggests, both confirmed against the jet:
  - the cycle-walk bound k is a*b = 0xffff.0000 (hoon +feis), and
  - for an even round count the final combination is ell*a + arr unless
    arr == a (the "legendary @max19" legacy case in fein_ob.c).
Validated value-for-value against `urbit eval` (see tests/test_noun.py).
"""

from __future__ import annotations

from ._potables import PREFIXES, SUFFIXES

_PRE = {s: i for i, s in enumerate(PREFIXES)}
_SUF = {s: i for i, s in enumerate(SUFFIXES)}

#  +raku:ob — murmur3 seeds for the round function
_RAKU = (0xB76D5EED, 0xEE281300, 0x85BCAE01, 0x4B387AF7)
_U32 = 0xFFFFFFFF
_A = 0xFFFF
_B = 0x10000
_K = _A * _B  # 0xffff.0000


def _murmur3_32(data: bytes, seed: int) -> int:
    """MurmurHash3 x86 32-bit (matches ++muk / vere's MurmurHash3_x86_32)."""
    c1, c2 = 0xCC9E2D51, 0x1B873593
    h = seed & _U32
    n = len(data)
    rounded = n & ~3
    for i in range(0, rounded, 4):
        k = int.from_bytes(data[i : i + 4], "little")
        k = (k * c1) & _U32
        k = ((k << 15) | (k >> 17)) & _U32
        k = (k * c2) & _U32
        h ^= k
        h = ((h << 13) | (h >> 19)) & _U32
        h = (h * 5 + 0xE6546B64) & _U32
    k = 0
    t = data[rounded:]
    if len(t) >= 3:
        k ^= t[2] << 16
    if len(t) >= 2:
        k ^= t[1] << 8
    if len(t) >= 1:
        k ^= t[0]
        k = (k * c1) & _U32
        k = ((k << 15) | (k >> 17)) & _U32
        k = (k * c2) & _U32
        h ^= k
    h ^= n
    h ^= h >> 16
    h = (h * 0x85EBCA6B) & _U32
    h ^= h >> 13
    h = (h * 0xC2B2AE35) & _U32
    h ^= h >> 16
    return h


def muk(syd: int, key: int) -> int:
    """++muk with len=2: murmur3 of the two low LE bytes of `key`."""
    return _murmur3_32(bytes([key & 0xFF, (key >> 8) & 0xFF]), syd)


def _eff(j: int, m: int) -> int:
    return muk(_RAKU[j], m)


def _fe(r: int, m: int) -> int:
    """++fe — Feistel rounds; final combine per the fein_ob.c jet."""
    ell, arr = m % _A, m // _A
    for j in range(1, r + 1):
        eff = _eff(j - 1, arr)
        tmp = (ell + eff) % (_A if j % 2 == 1 else _B)
        ell, arr = arr, tmp
    if r % 2 != 0 or arr == _A:
        return _A * arr + ell
    return _A * ell + arr


def _fen(r: int, m: int) -> int:
    """++fen — inverse Feistel rounds."""
    if r % 2 == 0:
        ahh, ale = m % _A, m // _A
    else:
        ahh, ale = m // _A, m % _A
    if ale == _A:
        ell, arr = ahh, ale
    else:
        ell, arr = ale, ahh
    for j in range(r, 0, -1):
        eff = _eff(j - 1, ell)
        if j % 2 == 1:
            tmp = (arr + _A - (eff % _A)) % _A
        else:
            tmp = (arr + _B - (eff % _B)) % _B
        ell, arr = tmp, ell
    return _A * arr + ell


def feis(m: int) -> int:
    """++feis:ob — cipher over [0, 0xfffe.ffff]."""
    c = _fe(4, m)
    return c if c < _K else _fe(4, c)


def tail(m: int) -> int:
    """++tail:ob — inverse of feis."""
    c = _fen(4, m)
    return c if c < _K else _fen(4, c)


def fein(pyn: int) -> int:
    """++fein:ob — scramble a ship number for @p display."""
    if 0x10000 <= pyn <= 0xFFFFFFFF:
        return 0x10000 + feis(pyn - 0x10000)
    if 0x100000000 <= pyn <= 0xFFFFFFFFFFFFFFFF:
        return (pyn & 0xFFFFFFFF00000000) | fein(pyn & 0xFFFFFFFF)
    return pyn


def fynd(cry: int) -> int:
    """++fynd:ob — un-scramble a parsed @p display value."""
    if 0x10000 <= cry <= 0xFFFFFFFF:
        return 0x10000 + tail(cry - 0x10000)
    if 0x100000000 <= cry <= 0xFFFFFFFFFFFFFFFF:
        return (cry & 0xFFFFFFFF00000000) | fynd(cry & 0xFFFFFFFF)
    return cry


def patp_to_num(patp: str) -> int:
    """'~sampel-palnet' -> 1624961343; '~daplyd' -> 17101; comets too."""
    p = patp.strip().lstrip("~").replace("--", "-")
    if not p:
        raise ValueError("empty @p")
    raw = bytearray()
    for syl in p.split("-"):
        if len(syl) == 6:
            try:
                raw.append(_PRE[syl[:3]])
                raw.append(_SUF[syl[3:]])
            except KeyError:
                raise ValueError(f"bad @p syllable {syl!r} in {patp!r}") from None
        elif len(syl) == 3:
            if raw:
                raise ValueError(f"suffix-only syllable must lead: {patp!r}")
            try:
                raw.append(_SUF[syl])
            except KeyError:
                raise ValueError(f"bad @p syllable {syl!r} in {patp!r}") from None
        else:
            raise ValueError(f"bad @p syllable {syl!r} in {patp!r}")
    return fynd(int.from_bytes(bytes(raw), "big"))


def num_to_patp(value: int) -> str:
    """Exact inverse of patp_to_num. Matches vere's @p renderer, including
    `--` group separators every 4 syllable-pairs counted from the right."""
    if value < 0:
        raise ValueError("ship numbers are non-negative")
    v = fein(value)
    if v < 0x100:
        return "~" + SUFFIXES[v]
    n = (v.bit_length() + 7) // 8
    if n % 2:
        n += 1
    raw = v.to_bytes(n, "big")
    syls = [PREFIXES[raw[i]] + SUFFIXES[raw[i + 1]] for i in range(0, n, 2)]
    parts: list[str] = []
    for idx, syl in enumerate(syls):
        if idx:
            parts.append("--" if (len(syls) - idx) % 4 == 0 else "-")
        parts.append(syl)
    return "~" + "".join(parts)


def is_comet(value: int) -> bool:
    return value >= (1 << 64)
