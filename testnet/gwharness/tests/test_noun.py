"""Noun plumbing tests: round trips, golden fixtures, and live cross-validation
against the real vere binary (`urbit eval -j -n` / `urbit eval -c -n`).

Golden fixtures below were captured from
/Users/trent/gw-building/vere/zig-out/aarch64-macos-none/urbit; the live tests
re-derive them at test time and are skipped if the binary is missing.
"""

from __future__ import annotations

import io
import random
import subprocess
from pathlib import Path

import pytest

from gwharness import noun as N
from gwharness import obphon
from gwharness.config import load_config

try:
    URBIT = Path(load_config().vere)
except Exception:  # pragma: no cover - config missing/invalid
    URBIT = Path("/Users/trent/gw-building/vere/zig-out/aarch64-macos-none/urbit")

needs_vere = pytest.mark.skipif(
    not URBIT.exists(), reason=f"vere binary not found at {URBIT}"
)


def hoon_ux(v: int) -> str:
    """Format an int as a hoon @ux literal (dots every 4 hex digits)."""
    h = format(v, "x")
    chunks = []
    while len(h) > 4:
        chunks.append(h[-4:])
        h = h[:-4]
    chunks.append(h)
    return "0x" + ".".join(reversed(chunks))


def vere_jam(expr: str) -> bytes:
    """Evaluate a hoon expression with the real binary; return jam bytes."""
    out = subprocess.run(
        [str(URBIT), "eval", "-j", "-n"],
        input=expr.encode(),
        capture_output=True,
        timeout=120,
    ).stdout
    assert out and out[0] == 0x00, f"vere eval failed for {expr!r}"
    n = int.from_bytes(out[1:5], "little")
    assert len(out) >= 5 + n
    return out[5 : 5 + n]


def vere_cue_pretty(payload: bytes) -> str:
    """Feed a newt-framed jam to `urbit eval -c -n`; return the pretty-print."""
    out = subprocess.run(
        [str(URBIT), "eval", "-c", "-n"],
        input=N.newt_frame(payload),
        capture_output=True,
        timeout=120,
    ).stdout
    return out.decode().strip()


ATOM_300 = (1 << 299) | 12345

#  expr -> (python noun, golden jam bytes from vere)
GOLDEN = {
    "0": (0, "02"),
    "1": (1, "0c"),
    "'hello'": (N.tas("hello"), "8007ad8c8ded0d"),
    "[1 2]": ((1, 2), "3112"),
    "[1 [2 3]]": ((1, (2, 3)), "714834"),
    "/foo/bar": (N.path("foo", "bar"), "01cfeced1df0c4c2e402"),
    hoon_ux(ATOM_300): (
        ATOM_300,
        "0064c981010000000000000000000000000000000000000000000000000000000000000000000040",
    ),
    #  shared-structure cases exercise backrefs
    "[[1 2] [1 2]]": (((1, 2), (1, 2)), "c5c849"),
    "[7 7 7]": ((7, (7, 7)), "e1373909"),
}


# ---------------------------------------------------------------------------
#  mat / rub
# ---------------------------------------------------------------------------

def test_mat_rub_roundtrip():
    values = list(range(70)) + [1 << k for k in (7, 8, 31, 32, 64, 200)]
    values += [random.Random(1).randrange(1 << 256) for _ in range(20)]
    for v in values:
        p, q = N.mat(v)
        got, pos = N.rub(q, 0, p)
        assert got == v and pos == p, f"mat/rub broke for {v}"


def test_mat_zero():
    assert N.mat(0) == (1, 1)


# ---------------------------------------------------------------------------
#  jam / cue against golden fixtures
# ---------------------------------------------------------------------------

@pytest.mark.parametrize("expr", list(GOLDEN))
def test_jam_golden(expr):
    noun, hexbytes = GOLDEN[expr]
    assert N.jam(noun) == bytes.fromhex(hexbytes), f"jam({expr})"


@pytest.mark.parametrize("expr", list(GOLDEN))
def test_cue_golden(expr):
    noun, hexbytes = GOLDEN[expr]
    assert N.cue(bytes.fromhex(hexbytes)) == noun, f"cue({expr})"


def _random_noun(rng: random.Random, depth: int = 0):
    if depth > 6 or rng.random() < 0.4:
        return rng.randrange(1 << rng.choice((1, 8, 16, 64, 200)))
    return (_random_noun(rng, depth + 1), _random_noun(rng, depth + 1))


def test_jam_cue_roundtrip_random():
    rng = random.Random(42)
    for _ in range(200):
        noun = _random_noun(rng)
        assert N.cue(N.jam(noun)) == noun


def test_deep_list_no_recursion_limit():
    #  jam/cue must be iterative: a goof tang's tapes nest thousands deep.
    #  (compare via list_iter — CPython's tuple == is itself recursive)
    deep = N.nlist(range(5000))
    out = N.cue(N.jam(deep))
    assert list(N.list_iter(out)) == list(range(5000))


def test_cue_truncated_raises():
    good = N.jam((N.tas("foo"), (N.tas("bar"), 0)))
    with pytest.raises((ValueError, EOFError)):
        N.cue(good[:-2])
    with pytest.raises((ValueError, EOFError)):
        N.cue(b"")


def test_jam_rejects_non_nouns():
    with pytest.raises((TypeError, ValueError)):
        N.jam("not a noun")
    with pytest.raises((TypeError, ValueError)):
        N.jam(-1)


# ---------------------------------------------------------------------------
#  newt framing
# ---------------------------------------------------------------------------

def test_newt_frame_layout():
    f = N.newt_frame(b"\xde\xad\xbe\xef")
    assert f == b"\x00\x04\x00\x00\x00\xde\xad\xbe\xef"


def test_read_newt_roundtrip():
    payload = N.jam((1, (2, 3)))
    buf = io.BytesIO(N.newt_frame(payload) + N.newt_frame(b"\x02"))
    assert N.read_newt(buf) == payload
    assert N.read_newt(buf) == b"\x02"
    with pytest.raises(EOFError):
        N.read_newt(buf)


def test_read_newt_bad_tag():
    with pytest.raises(ValueError):
        N.read_newt(io.BytesIO(b"\x07\x01\x00\x00\x00\xff"))


def test_read_newt_short_body():
    with pytest.raises(ValueError):
        N.read_newt(io.BytesIO(b"\x00\x0a\x00\x00\x00abc"))


# ---------------------------------------------------------------------------
#  helpers
# ---------------------------------------------------------------------------

def test_tas():
    assert N.tas("foo") == 0x6F6F66
    assert N.from_tas(N.tas("watcher-config")) == "watcher-config"


def test_path():
    assert N.path("foo", "bar") == (N.tas("foo"), (N.tas("bar"), 0))
    assert N.path() == 0


def test_list_iter():
    assert list(N.list_iter(N.nlist([5, 6, 7]))) == [5, 6, 7]
    assert list(N.list_iter(0)) == []
    with pytest.raises(ValueError):
        list(N.list_iter((1, 2)))  # improper terminator


def test_tape():
    tape = N.nlist(b"hi there")
    assert N.from_tape(tape) == "hi there"


# ---------------------------------------------------------------------------
#  @p  (ob feistel)
# ---------------------------------------------------------------------------

#  ship number -> @p text; every value verified against `urbit eval`
PATP_VECTORS = {
    0: "~zod",
    255: "~fes",
    256: "~marzod",
    17101: "~daplyd",                      # the harness star
    65536: "~dapnep-ronmyl",               # first planet (feistel-scrambled)
    1624961343: "~sampel-palnet",
    0xFFFFFFFF: "~dostec-risfen",          # last planet
    2**32: "~doznec-dozzod-dozzod",        # first moon (low half scrambled)
    2**64: "~doznec--dozzod-dozzod-dozzod-dozzod",   # odd-width comet
    2**128 - 1: "~fipfes-fipfes-fipfes-fipfes--fipfes-fipfes-fipfes-fipfes",
}


@pytest.mark.parametrize("num,txt", list(PATP_VECTORS.items()))
def test_patp_vectors(num, txt):
    assert N.patp_int(txt) == num
    assert N.patp_str(num) == txt


def test_patp_roundtrip_random():
    rng = random.Random(99)
    for _ in range(300):
        v = rng.randrange(1 << rng.choice((16, 32, 48, 64, 96, 128)))
        assert N.patp_int(N.patp_str(v)) == v


def test_feis_known_values():
    #  from `urbit eval` '(feis:ob 1)' etc.
    assert obphon.feis(1) == 3560901323
    assert obphon.feis(74350415) == 1334352701
    assert obphon.tail(3560901323) == 1
    assert obphon.fynd(74415951) == 1624961343
    assert obphon.fein(1624961343) == 74415951


def test_muk_known_values():
    #  from `urbit eval` '(muk 0xb76d.5eed 2 0)' / '... 2 1'
    assert obphon.muk(0xB76D5EED, 0) == 3187942399
    assert obphon.muk(0xB76D5EED, 1) == 1952606156


def test_patp_rejects_garbage():
    for bad in ("~xxxxxx", "~zo", "", "~marzod-", "~dozzodmar"):
        with pytest.raises(ValueError):
            N.patp_int(bad)


# ---------------------------------------------------------------------------
#  live cross-validation against the real binary
# ---------------------------------------------------------------------------

@needs_vere
@pytest.mark.parametrize("expr", list(GOLDEN))
def test_live_jam_matches_vere(expr):
    noun, _ = GOLDEN[expr]
    assert N.jam(noun) == vere_jam(expr), f"python jam != vere jam for {expr}"


@needs_vere
@pytest.mark.parametrize("expr", list(GOLDEN))
def test_live_cue_of_vere_jam(expr):
    noun, _ = GOLDEN[expr]
    assert N.cue(vere_jam(expr)) == noun, f"python cue(vere jam) != {expr}"


@needs_vere
def test_live_vere_cues_our_jam():
    #  vere `eval -c -n` cues our newt-framed jam and pretty-prints it
    assert vere_cue_pretty(N.jam((1, 2))) == "[1 2]"
    assert vere_cue_pretty(N.jam(0)) == "0"


@needs_vere
def test_live_patp_matches_vere():
    rng = random.Random(7)
    ships = [65536, 2**64,
             rng.randrange(65536, 2**32),
             rng.randrange(2**32, 2**64),
             rng.randrange(2**64, 2**128)]
    for v in ships:
        txt = N.patp_str(v)
        assert N.cue(vere_jam(txt)) == v, f"vere parses {txt} differently"
