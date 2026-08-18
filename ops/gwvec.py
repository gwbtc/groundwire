#!/usr/bin/env python3
"""gwvec.py -- build adversarial attestation vectors from a comet artifact.

The Phase-2 matrix (`doc/opret-revision/05-live-test-plan.md`, tests 2.2-2.16)
needs suite-C passes carrying DELIBERATELY WRONG custody logs, fed straight
into a live `%gw-btc` as `%jael-writ` pokes.  This builds them.

Nothing here invents chain data.  Every txid, height, satpoint and snapshot is
cued out of a real comet's own on-chain `xtr`; a mutation edits one field of
that and re-jams it.  Every encoder is causeway's own
(`build_xtr_atom` -> `append_xtr_to_ring` -> `derive_pass_from_ring`), so a bug
in causeway's encoder cannot be papered over by a second implementation here.

Phase 2 built the same thing as a throwaway `genvec.py` in a scratch directory
and it was lost, which is why the whole matrix had to be rebuilt from nothing
to re-run it.  Hence this file.

    gwvec.py list    <artifact.json>
    gwvec.py build   <artifact.json> [-o vectors.json]
    gwvec.py show    <artifact.json>

`<artifact.json>` is what `gwmint.py artifact` writes (`~/.gw-comet-<n>.json`).
Two of its fields are load-bearing: `ring` (the BARE suite-C ring, no xtr) and
`xtr_hex` (the jammed custody log).  The `@p` never changes when the xtr does
-- the name commits to ugn+dat only -- so a mutated log still fingerprints to
the same comet, which is exactly what makes these tests possible.  Test 2.9 is
the exception: it rewrites `dat`, which is inside the tweak, so the `@p` moves.

Output is JSON: [{label, patp, pass_hex, pass_hoon, bytes, note}].  `pass_hoon`
is dot-grouped every four hex digits because Hoon will not parse a bare 0x
literal of 600+ digits.  Feed it to a ship with

    gwctl.py writ <pier> <patp> <pass_hex>

or poke `[%jael-writ %gw-btc `@p`<patp> `@ux`<pass_hoon>]` directly.

DANGER, and it is not theoretical: several of these produce a NEGATIVE verdict,
which is a jael %fail and an ames snub of the named comet on the verifier you
poke.  A later positive verdict does lift a snub (kernel `f68a547b2b`), but the
snub drops the very packet that would earn one on EVERY transport -- classic
ames `%hear`, and mesa's `%page` since `b0a8e962ff` -- so assume you have to
clear it yourself: `gwsnub.py del <pier> <patp>`, or the same ames
`%snub %deny %del` task by hand -- from a khan thread that is
`(send-raw-card %pass /unsnub %arvo %a %snub %deny %del ~[<patp>])`, with a
BARE `send-raw-card`: `send-raw-card:strandio` does not resolve inside a
khan-eval, where strandio is already in the subject.
"""
import argparse
import json
import sys
from pathlib import Path

sys.setrecursionlimit(60_000)          # a 1025-entry log is a 1025-deep noun

_HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(_HERE.parent / "causeway" / "desktop"))
import causeway as C                                            # noqa: E402


# ----------------------------------------------------------------- custody log
def cue_log(xtr_hex):
    """The artifact's xtr atom -> the list of $custody-entry nouns, oldest first."""
    cur = C.hoon_cue(int(xtr_hex, 16))
    out = []
    while cur != 0:
        node, cur = cur
        out.append(node)
    return out


def jam_log(entries):
    log = 0
    for e in reversed(entries):
        log = (e, log)
    return C.hoon_jam(log)


def unpack(entry):
    """[txid [height (unit opening)]] -> a dict, or {op: None} for a plain hop."""
    txid, (height, opu) = entry
    if opu == 0:
        return dict(txid=txid, height=height, op=None)
    _, op = opu
    ikey, (snap, bo) = op
    life, (rift, (key, (sponsor, fief))) = snap
    return dict(txid=txid, height=height, ikey=ikey, life=life, rift=rift,
                key=key, sponsor=sponsor, fief=fief, bo=bo)


def repack(d):
    if d.get("op", "-") is None:
        return (d["txid"], (d["height"], 0))
    snap = (d["life"], (d["rift"], (d["key"], (d["sponsor"], d["fief"]))))
    return (d["txid"], (d["height"], (0, (d["ikey"], (snap, d["bo"])))))


# ------------------------------------------------------------------- the pass
def pass_of(art, entries):
    """Rebuild the ship's suite-C pass around this custody log.

    entries=None means NO xtr at all (raw 0, the public-onboarding shape);
    entries=[] means an xtr that decodes to the empty list, which is a
    different thing and gets a different verdict -- see 2.12 vs 2.13.
    """
    ring = C.decode_uw(art["ring"])
    xtr = 0 if entries is None else jam_log(entries)
    return C.derive_pass_from_ring(C.encode_uw(C.append_xtr_to_ring(ring, xtr)))


def hoonux(v):
    """An int as a Hoon @ux literal, dot-grouped in 4-hex chunks."""
    h, out = f"{v:x}", []
    while len(h) > 4:
        out.append(h[-4:])
        h = h[:-4]
    out.append(h)
    return "0x" + ".".join(reversed(out))


def _dat_with_kelvin(dat, kelvin):
    """dat = (can 0 (mat %gw-btc) (mat KEL) [256 d]); swap the kelvin."""
    p1, dom = C._hoon_rub(0, dat)
    p2, _old = C._hoon_rub(p1, dat)
    d = (dat >> (p1 + p2)) & ((1 << 256) - 1)
    w = C.BitWriter()
    w.write_mat(dom)
    w.write_mat(kelvin)
    w.write(256, d)
    return w.to_int()


def _ring_with_dat(ring, dat, xtr):
    """'C' | seed(512) | mat(dat) | xtr -- mirroring +sec:ex:cric."""
    assert ring & 0xFF == ord("C"), "not a suite-C ring"
    sed = (ring >> 8) & ((1 << 512) - 1)
    w = C.BitWriter()
    w.write(8, ord("C"))
    w.write(512, sed)
    w.write_mat(dat)
    if xtr:
        w.write(xtr.bit_length(), xtr)
    return w.to_int()


# ------------------------------------------------------------------- the cases
def build(art):
    """Every mutation the plan's tests 2.2-2.14 call for, plus the baselines.

    A case is skipped rather than faked when this comet cannot express it: a
    single-entry log has no entry 1 to regress, and only a comet whose sat has
    actually moved on chain can produce a spent tip.
    """
    log = cue_log(art["xtr_hex"])
    patp = art["comet"]
    out = []

    def add(label, p, note, who=patp):
        out.append(dict(label=label, patp=who, pass_hex=hex(p),
                        pass_hoon=hoonux(p), bytes=(p.bit_length() + 7) // 8,
                        note=note))

    add("base-full", pass_of(art, log),
        f"the comet's GENUINE {len(log)}-entry log, unmodified (test 2.1)")

    # 2.8 / 5.3 -- a truncated log's tip is a satpoint the NEXT entry spent, so
    # any comet that has ever done a state update carries its own spent-tip
    # vector on chain, for free and with no broadcast.
    for n in range(1, len(log)):
        add(f"prefix-{n}", pass_of(art, log[:n]),
            f"truncated to {n} entr{'y' if n == 1 else 'ies'}: an OLDER but "
            f"genuine log whose tip was spent by entry {n} (tests 2.8, 5.3/5.4)")

    # 2.6 -- entry 0's spawn-opening names a DIFFERENT satpoint than the
    # pass's plaintext dat: the `spawn-matches` binding fails.  (This was
    # "one bit of the blind" while dat was a hiding commitment.)
    e0 = unpack(log[0])
    if e0.get("bo"):
        _, (spawn, start_h) = e0["bo"]
        txid, (vout, off) = spawn
        d = dict(e0, bo=(0, ((txid, (vout + 1, off)), start_h)))
        add("t2.6-spawn-mismatch", pass_of(art, [repack(d)] + log[1:]),
            "entry 0's spawn-opening vout+1: does not match the pass's dat (test 2.6)")

    if len(log) > 1:
        e1 = unpack(log[1])
        # 2.5 -- the snapshot no longer commits to the on-chain output key.
        # rift rather than life, so life-order stays satisfied and the
        # commitment failure is the SOLE failing check.
        add("t2.5-commitment",
            pass_of(art, [log[0], repack(dict(e1, rift=e1["rift"] + 1))]),
            "entry 1 rift+1: state key no longer matches the chain (test 2.5)")
        # 2.4 -- life regresses across openings
        add("t2.4-life-regress",
            pass_of(art, [repack(dict(e0, life=e1["life"])),
                          repack(dict(e1, life=e0["life"]))]),
            "entry 0 and entry 1 lives swapped: life REGRESSES (test 2.4)")

    # 2.7 -- one over the 1024 cap.  Dropped at the agent, before the thread.
    pad = log[-1]
    add("t2.7-over-cap", pass_of(art, log + [pad] * (1025 - len(log))),
        "1025-entry custody log; the cap is 1024 (test 2.7)")

    # 2.12 vs 2.13 -- the distinction the plan singles out
    add("t2.12-jam-null", pass_of(art, []),
        "xtr = (jam ~): PRESENT but empty -> negative verdict (test 2.12)")
    add("t2.13-bare-pass", pass_of(art, None),
        "no xtr at all: the public-onboarding packet -> SILENCE (test 2.13)")

    # 2.9 -- a foreign protocol kelvin.  dat is inside the tweak, so this is a
    # DIFFERENT @p; read it off the ship with
    #   (scot %p fig:ex:(com:nu:cric:crypto pass))
    ring = C.decode_uw(art["ring"])
    _, dat = C._hoon_rub(512, ring >> 8)
    r8 = _ring_with_dat(ring, _dat_with_kelvin(dat, 8), jam_log(log))
    add("t2.9-kelvin8", C.derive_pass_from_ring(C.encode_uw(r8)),
        "dat carries protocol kelvin 8 -> SILENCE, and a DIFFERENT @p (test 2.9)",
        who="COMPUTE-ON-SHIP")

    # 2.14 -- dat is not a decodable +mat, so +pass-pki-dom:ames answers ~ and
    # the packet dies in ames before jael or %gw-btc is involved
    r0 = _ring_with_dat(ring, 0, jam_log(log))
    add("t2.14-nonmat-dat", C.derive_pass_from_ring(C.encode_uw(r0)),
        "dat = 0, not a valid +mat -> dropped in ames (test 2.14)",
        who="COMPUTE-ON-SHIP")

    return out


# 2.2 (append a real foreign tx as a custody hop) and 2.3 (a genuine pass under
# another comet's @p) each need a SECOND comet, so they are composed rather than
# built: for 2.2 append `(txid, (height, 0))` of any confirmed transaction to
# base-full; for 2.3 take another artifact's base-full pass and poke it under
# this comet's @p.

def compose_foreign_hop(art, other_art):
    """2.2 -- append the OTHER comet's spawn transaction as a bogus custody hop.

    It is a real transaction at a real height, so +fetch-tx-at resolves it and
    the failure lands on +derive-tip -- input 0 does not spend our tip.
    """
    log = cue_log(art["xtr_hex"])
    o0 = unpack(cue_log(other_art["xtr_hex"])[0])
    p = pass_of(art, log + [(o0["txid"], (o0["height"], 0))])
    return dict(label="t2.2-foreign-hop", patp=art["comet"], pass_hex=hex(p),
                pass_hoon=hoonux(p), bytes=(p.bit_length() + 7) // 8,
                note=f"{other_art['comet'][:20]}…'s real spawn tx appended as a "
                     "custody hop of ours (test 2.2)")


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("cmd", choices=["list", "build", "show"])
    ap.add_argument("artifact")
    ap.add_argument("-o", "--out")
    ap.add_argument("--foreign", help="a second artifact, for test 2.2")
    a = ap.parse_args()
    art = json.loads(Path(a.artifact).read_text())

    if a.cmd == "show":
        print(f"{art['comet']}  confidential={art.get('confidential')}")
        for i, e in enumerate(cue_log(art["xtr_hex"])):
            d = unpack(e)
            print(f"  [{i}] txid={d['txid']:064x} height={d['height']}")
            if d.get("op", "-") is not None:
                print(f"      life={d['life']} rift={d['rift']} "
                      f"sponsor={'~' if d['sponsor'] == 0 else d['sponsor'][1]} "
                      f"fief={'~' if d['fief'] == 0 else d['fief'][1]}")
        return

    vecs = build(art)
    if a.foreign:
        vecs.append(compose_foreign_hop(art, json.loads(Path(a.foreign).read_text())))
    if a.cmd == "list":
        for v in vecs:
            print(f"{v['label']:22s} {v['bytes']:5d} B  {v['patp'][:24]:24s} {v['note']}")
        return
    dest = Path(a.out or "vectors.json")
    dest.write_text(json.dumps(vecs, indent=1))
    print(f"{len(vecs)} vectors -> {dest}")


if __name__ == "__main__":
    main()
