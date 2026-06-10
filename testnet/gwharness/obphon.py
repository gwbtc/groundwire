"""@p <-> number, for comets.

A comet's @p (>= 2**64) is rendered with no `ob` scramble (the Feistel scramble
only applies to the 16/32-bit galaxy/star/planet range, which the harness never
mints). @p uses BIG-ENDIAN byte order: syllables read left-to-right are the
most-significant bytes first, each syllable = PREFIX(high byte) + SUFFIX(low
byte) of a byte pair. (Note this differs from the @q codec, which is
little-endian.) Verified against `scot %p` / `slav %p` from the gw vere.

For correctness-critical paths the harness also reads each booted comet's
numeric @p straight from its own ship (get-our), so this codec is a convenience
/ cross-check, not the sole source of truth.
"""

from __future__ import annotations

from ._potables import PREFIXES, SUFFIXES

_PRE = {s: i for i, s in enumerate(PREFIXES)}
_SUF = {s: i for i, s in enumerate(SUFFIXES)}


def patp_to_num(patp: str) -> int:
    q = patp.strip().lstrip("~").replace("--", "-")
    if not q:
        return 0
    raw = bytearray()
    for syl in q.split("-"):
        if len(syl) == 6:
            raw.append(_PRE[syl[:3]])
            raw.append(_SUF[syl[3:]])
        elif len(syl) == 3:        # lone (least-significant) suffix
            raw.append(_SUF[syl])
        else:
            raise ValueError(f"bad @p syllable {syl!r} in {patp!r}")
    return int.from_bytes(bytes(raw), "big")


def num_to_patp(value: int) -> str:
    if value == 0:
        return "~zod"
    n = (value.bit_length() + 7) // 8
    raw = value.to_bytes(n, "big")
    syllables: list[str] = []
    i = 0
    if len(raw) % 2 == 1:          # lone leading suffix
        syllables.append(SUFFIXES[raw[0]])
        i = 1
    while i < len(raw):
        syllables.append(PREFIXES[raw[i]] + SUFFIXES[raw[i + 1]])
        i += 2
    count = len(syllables)
    out = []
    for j, syl in enumerate(syllables):
        if j:
            out.append("--" if (count - j) % 4 == 0 else "-")
        out.append(syl)
    return "~" + "".join(out)


def is_comet(value: int) -> bool:
    return value >= (1 << 64)
