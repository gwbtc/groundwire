"""Offline plumbing self-tests (no ship, no bitcoind required).

Validates the noun layer against values captured from `urbit eval`, the @p
codec against `scot %p`/`slav %p`, and the peek-result unwrapper.
"""

from __future__ import annotations

from . import noun as N
from . import obphon
from .connsock import _unwrap_cask

# jam values captured from the gw vere `urbit eval`:
_JAM_VECTORS = {
    0: 2, 1: 12, 255: 130592, 65536: 134217920,
    (1, 2): 4657, (1, (2, 3)): 3426417, (0, 0): 41,
    (1, 1): 817, ((1, 2), (1, 2)): 4835525,
    0x123456789abc: 163971058432970112,
}

# @p <-> num captured from `scot %p` / `slav %p`:
_PATP_VECTOR = (
    160624567156906014433698560513724465869,
    "~fabdes-ticryx-pidfun-padber--hostyd-nocnup-narmec-daplyd",
)


def run() -> bool:
    ok = True

    def check(name: str, cond: bool) -> None:
        nonlocal ok
        ok = ok and cond
        print(f"  [{'ok' if cond else 'XX'}] {name}")

    for noun, expect in _JAM_VECTORS.items():
        check(f"jam {noun!r} == {expect}", N.jam(noun) == expect)
        check(f"cue(jam {noun!r}) roundtrip", N.cue(N.jam(noun)) == noun)

    atom, patp = _PATP_VECTOR
    check("patp_to_num", obphon.patp_to_num(patp) == atom)
    check("num_to_patp", obphon.num_to_patp(atom) == patp)

    NOUN = N.tas("noun")
    check("unwrap (unit cask)", _unwrap_cask((0, (NOUN, 42))) == 42)
    check("unwrap (unit unit cask)", _unwrap_cask((0, (0, (NOUN, 42)))) == 42)
    check("unwrap none", _unwrap_cask(0) is None)

    print(f"\nselftest: {'PASS' if ok else 'FAIL'}")
    return ok
