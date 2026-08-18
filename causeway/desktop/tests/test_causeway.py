import pathlib
"""Pytest suite for causeway.py — kelvin-9 (%gw-btc, OP_RETURN) encoders,
spawn/rekey PSBT builders, proof round-trip, xpub parsing, mnemonyms.

The encoder tests are pinned to the shared golden vectors
(groundwire/vectors/gw-kelvin-9.json), which agree byte-for-byte with the
compiled Hoon lib gw-btc-pass.hoon and its passing test.

Run with: cd causeway/desktop && python -m pytest -q
"""

import json
import os
import re
import sys

import pytest

# Make ../causeway.py importable
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))

import causeway as cw


# ---------------------------------------------------------------------------
# Golden vectors — locate vectors/gw-kelvin-9.json by walking up from here.
# ---------------------------------------------------------------------------


def _find_vectors() -> dict:
    d = os.path.dirname(os.path.abspath(__file__))
    for _ in range(8):
        cand = os.path.join(d, "vectors", "gw-kelvin-9.json")
        if os.path.isfile(cand):
            return json.load(open(cand))
        d = os.path.dirname(d)
    pytest.skip("vectors/gw-kelvin-9.json not found")


_V = _find_vectors()
_VECS = {v["name"]: v for v in _V["vectors"]}
BASIC = _VECS["basic"]
SKP = _VECS["state-key-pin"]
PD2 = _VECS["pushdata2-fief"]
FULL = _VECS["full-packet"]

# Byte convention (pinned by the JSON + the compiled Hoon lib gw-btc-pass.hoon
# and its passing test, the real authority): a jammed noun enters a tagged-hash
# preimage as its natural LITTLE-endian byte serialization — the same jam_*_le
# bytes, NOT byte-reversed.  So d = H_tag('gw/spawn-commit', jam_spawn_le || blind)
# and c = H_tag('gw/state-commit', jam_snapshot_le).  Our encoders do exactly
# this (cw.jam_bytes returns the LE serialization), so every golden below is
# asserted directly against the JSON.


def _snap(vec) -> dict:
    """A snapshot dict from a vector's snapshot block (sponsor 'zod' -> ~zod=0)."""
    s = vec["snapshot"]
    sponsor = s.get("sponsor")
    if sponsor == "zod":
        sponsor = 0  # ~zod @p
    elif sponsor is not None:
        sponsor = cw.patp_to_int("~" + sponsor)
    # A vector's fief block carries the $fief NOUN verbatim under "noun"
    # ([tag, [p, q]]), which is what causeway.fief_noun consumes.
    fief = s.get("fief")
    if fief is not None:
        fief = fief["noun"]
    return {
        "life": s["life"], "rift": s["rift"],
        "key": int(s["key"], 16), "sponsor": sponsor, "fief": fief,
    }


# ---------------------------------------------------------------------------
# Golden vectors — hiding dat / blind / d
# ---------------------------------------------------------------------------


def test_golden_jam_spawn():
    sp = BASIC["spawn_sont"]
    noun = cw.spawn_sont_noun(sp["txid"], sp["vout"], sp["off"])
    assert cw.jam_bytes(noun).hex() == BASIC["jam_spawn_le"]


def test_golden_dat():
    sp = BASIC["spawn_sont"]
    dat = cw.build_dat_atom(sp["txid"], sp["vout"], sp["off"])
    assert dat == int(BASIC["dat"], 16)
    # mat("gw-btc") 59 bits + mat(9) 10 bits + mat(jam [txid vout off]) -> 167 bits
    # for this 8-byte-txid fixture -> 21 bytes.  Plaintext: the satpoint reads back.
    assert len(cw.build_dat_bytes(sp["txid"], sp["vout"], sp["off"])) == 21
    dom, kel, spawn = cw.parse_dat_atom(dat)
    assert (dom, kel) == ("gw-btc", 9)
    assert spawn == cw.spawn_sont_noun(sp["txid"], sp["vout"], sp["off"])


def test_dat_domain_is_rub_extractable():
    # +pass-pki-dom does (rub 0 dat); the head must decode to the domain tag.
    sp = BASIC["spawn_sont"]
    dat = cw.build_dat_atom(sp["txid"], sp["vout"], sp["off"])
    _width, dom_atom = cw._hoon_rub(0, dat)
    assert dom_atom.to_bytes(6, "little").decode() == "gw-btc"


def test_dat_expr_is_a_concrete_can():
    """The --tweak expression is all literals: no arm the miner's +wish
    would have to resolve, and it evaluates to exactly +make-dat."""
    sp = BASIC["spawn_sont"]
    expr = cw.make_dat_expr(sp["txid"], sp["vout"], sp["off"])
    assert expr.startswith("(can 0 (mat %gw-btc) (mat 9) (mat (jam [txid=0x")
    assert expr.endswith(" vout=1 off=0])) ~)")
    assert "blind" not in expr and "H_tag" not in expr


# ---------------------------------------------------------------------------
# Spawn flow plumbing (network + miner stubbed)
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Golden vectors — snapshot / state commitment / leaf / output key Q
# ---------------------------------------------------------------------------


def test_golden_jam_snapshot_basic():
    assert cw.jam_bytes(cw.snapshot_dict_to_noun(_snap(BASIC))).hex() == BASIC["jam_snapshot_le"]


def test_golden_jam_snapshot_state_key_pin():
    assert cw.jam_bytes(cw.snapshot_dict_to_noun(_snap(SKP))).hex() == SKP["jam_snapshot_le"]


def test_golden_state_commit():
    # c = H_tag('gw/state-commit', jam_snapshot_le) — both vectors.
    assert cw.state_commit(_snap(SKP)).hex() == SKP["state_commit_c"]
    assert cw.state_commit(_snap(BASIC)).hex() == BASIC["state_commit_c"]


def test_golden_state_leaf_script():
    for vec in (SKP, BASIC):
        c = cw.state_commit(_snap(vec))
        assert cw.state_leaf_script(c).hex() == vec["state_leaf_script"]
        assert len(cw.state_leaf_script(c)) == 37


def test_golden_state_leaf_hash():
    leaf = cw.state_leaf_script(cw.state_commit(_snap(SKP)))
    assert cw.state_leaf_hash(leaf).hex() == SKP["state_leaf_hash"]


def test_golden_state_output_key_q():
    p = bytes.fromhex(SKP["internal_key_compressed"])
    assert cw.state_output_key(p, _snap(SKP)).hex() == SKP["state_output_key_q"]


def test_state_output_key_accepts_xonly_and_compressed():
    p33 = bytes.fromhex(SKP["internal_key_compressed"])
    q1 = cw.state_output_key(p33, _snap(SKP))
    q2 = cw.state_output_key(p33[1:], _snap(SKP))  # bare x-only
    assert q1 == q2


# ---------------------------------------------------------------------------
# Golden vectors — publication + xtr (jam of full openings)
# ---------------------------------------------------------------------------


def _basic_opening() -> dict:
    """The opening used by the basic vector's publication and xtr entry 0."""
    pub = BASIC["publication"]
    sp = BASIC["spawn_sont"]
    return {
        "internal_key": int(pub["internal_key"], 16),
        "snapshot": _snap(BASIC),
        "spawn_opening": {
            "spawn": {"txid_hex": sp["txid"], "vout": sp["vout"], "off": sp["off"]},
            "start_height": pub["start_height"],
        },
    }


def test_golden_publication_jam():
    pub = BASIC["publication"]
    noun = cw.publication_noun(int(pub["pass"], 16), _basic_opening())
    assert cw.jam_bytes(noun).hex() == pub["jam_publication_le"]


def test_publication_script_shape():
    pub = BASIC["publication"]
    script = cw.make_publication_script(int(pub["pass"], 16), _basic_opening())
    # OP_RETURN PUSH3 'urb' PUSH1 <kelvin=0x09> then pushdata.
    assert script[:7] == bytes([0x6A, 0x03, 0x75, 0x72, 0x62, 0x01, 0x09])
    payload = bytes.fromhex(pub["jam_publication_le"])
    assert len(payload) == 34  # <= 75 -> a direct length push, not PUSHDATA1
    assert script[7:] == bytes([len(payload)]) + payload
    # ... and the whole script is pinned in the shared vector
    assert script.hex() == pub["op_return_script"]
    assert len(script) == pub["op_return_script_bytes"]
    assert script[7:8].hex() == pub["pushdata"]


def test_publication_small_payload_uses_direct_push():
    # A tiny opening (< 75 byte payload) must use a direct length push, no 0x4c.
    opening = {"internal_key": 1, "snapshot": {"life": 1, "rift": 0, "key": 1,
               "sponsor": None, "fief": None}, "spawn_opening": None}
    script = cw.make_publication_script(1, opening)
    payload = cw.jam_bytes(cw.publication_noun(1, opening))
    assert len(payload) <= 75
    assert script[7] == len(payload)  # direct push, not 0x4c


# ---------------------------------------------------------------------------
# OP_PUSHDATA2 — the publication that does not fit in one length byte.
#
# PUSHDATA1 (0x4c) carries a SINGLE length byte and so stops at 255, but
# MAX_PUBLICATION is 1024 and every fief-carrying publication measures 265–269.
# This used to raise here ("bytes must be in range(0, 256)") and, worse,
# silently emit a corrupt script on the Hoon side.  The vector below is the
# shared cross-language pin: Hoon's +make-publication produces these exact
# bytes for these exact inputs.
# ---------------------------------------------------------------------------


def _pd2_opening() -> dict:
    pub = PD2["publication"]
    sp = PD2["spawn_sont"]
    return {
        "internal_key": int(pub["internal_key"], 16),
        "snapshot": _snap(PD2),
        "spawn_opening": {
            "spawn": {"txid_hex": sp["txid"], "vout": sp["vout"], "off": sp["off"]},
            "start_height": pub["start_height"],
        },
    }


def test_golden_pushdata2_publication_script():
    """Byte-identical to Hoon for the fief-carrying spawn publication.

    This was THE PUSHDATA2 vector: with a 32-byte blind in the opening the
    payload was 269 bytes, past OP_PUSHDATA1's 255 ceiling.  Plaintext dat
    dropped it to 241, so it now pins the PUSHDATA1 side of that boundary;
    the full-packet vector below still exercises PUSHDATA2."""
    pub = PD2["publication"]
    pass_atom = int(pub["pass"], 16)
    payload = cw.jam_bytes(cw.publication_noun(pass_atom, _pd2_opening()))
    assert payload.hex() == pub["jam_publication_le"]
    assert len(payload) == pub["payload_bytes"] == 241
    assert 75 < len(payload) <= 255  # PUSHDATA1 territory

    script = cw.make_publication_script(pass_atom, _pd2_opening())
    assert script.hex() == pub["op_return_script"]
    assert len(script) == pub["op_return_script_bytes"] == 250
    # envelope, then OP_PUSHDATA1 with a ONE-byte length
    assert script[:7] == bytes([0x6A, 0x03, 0x75, 0x72, 0x62, 0x01, 0x09])
    assert script[7:9].hex() == pub["pushdata"] == "4cf1"
    assert script[8] == 241
    assert script[9:] == payload


# ---------------------------------------------------------------------------
# THE FULL-PACKET PUBLICATION.
#
# A kelvin-9 publication is the comet's whole attestation packet: the pass a
# peer receives over ames, custody log and all, plus the opening for the hop
# the carrying transaction performs.  Two things have to agree across the three
# implementations for that to work at all — the xtr re-encoding (pass_with_xtr,
# +with-xtr:gw-btc-pass, passWithXtr) and the script — and a realistic seven-hop
# packet lands at 588 payload bytes, which is why the 512-byte cap could not
# survive the change.
# ---------------------------------------------------------------------------


def _full_snapshot(s: dict) -> dict:
    sponsor = s.get("sponsor")
    if sponsor == "zod":
        sponsor = 0
    f = s.get("fief")
    return {"life": s["life"], "rift": s["rift"], "key": int(s["key"], 16),
            "sponsor": sponsor, "fief": None if f is None else f["noun"]}


def _full_opening(o: dict) -> dict:
    so = o.get("spawn_opening")
    out = {"internal_key": int(o["internal_key"], 16),
           "snapshot": _full_snapshot(o["snapshot"]),
           "spawn_opening": None}
    if so is not None:
        sp = so["spawn_sont"]
        out["spawn_opening"] = {
            "spawn": {"txid_hex": sp["txid"], "vout": sp["vout"], "off": sp["off"]},
            "start_height": so["start_height"],
        }
    return out


def _full_log() -> list:
    return [{"txid_hex": e["txid"], "height": e["height"],
             "opening": None if e["opening"] is None else _full_opening(e["opening"])}
            for e in FULL["custody_log"]]


def test_golden_full_packet_xtr_and_pass():
    """The custody log jams to the vector's xtr, and re-encoding the 108-byte
    pass around it reproduces the full pass byte for byte."""
    xtr = cw.build_xtr_atom(_full_log())
    assert xtr.to_bytes((xtr.bit_length() + 7) // 8, "little").hex() == FULL["jam_xtr_le"]

    empty = int(FULL["pass_empty"], 16)
    assert (empty.bit_length() + 7) // 8 == FULL["pass_empty_bytes"] == 114
    full = cw.pass_with_xtr(empty, xtr)
    assert format(full, "x").rjust(FULL["pass_full_bytes"] * 2, "0") == FULL["pass_full"]
    assert (full.bit_length() + 7) // 8 == FULL["pass_full_bytes"] == 471
    # xtr rides outside the key tweak: 'c', ugn, cry and dat are untouched
    assert full & ((1 << (8 + 256 + 256)) - 1) == empty & ((1 << (8 + 256 + 256)) - 1)


def test_golden_full_packet_publication_script():
    """Byte-identical to Hoon and TS for a 588-byte payload — over the OLD cap."""
    pass_full = int(FULL["pass_full"], 16)
    opening = _full_opening(FULL["terminal_opening"])
    payload = cw.jam_bytes(cw.publication_noun(pass_full, opening))
    assert payload.hex() == FULL["jam_publication_le"]
    assert len(payload) == FULL["payload_bytes"] == 560
    assert len(payload) > 512, "this is the packet the old cap could not carry"

    script = cw.make_publication_script(pass_full, opening)
    assert script.hex() == FULL["op_return_script"]
    assert len(script) == FULL["op_return_script_bytes"] == 570
    assert script[7:10].hex() == FULL["pushdata"] == "4d3002"
    assert script[8] | (script[9] << 8) == 560


def test_max_publication_is_the_agreed_cap():
    assert cw.MAX_PUBLICATION == FULL["max_publication"] == 1024


@pytest.mark.parametrize("n,head", [
    (3, "03"), (75, "4b"),                      # direct push: opcode IS the length
    (76, "4c4c"), (254, "4cfe"), (255, "4cff"),  # PUSHDATA1, one length byte
    (256, "4d0001"), (269, "4d0d01"), (512, "4d0002"),  # PUSHDATA2, LE length
    (560, "4d3002"), (588, "4d4c02"), (1024, "4d0004"),
])
def test_push_data_boundaries(n, head):
    assert cw.push_data(b"\xab" * n).hex() == head


def test_push_data_refuses_what_it_cannot_express():
    assert len(cw.push_data(b"\xab" * 0xFFFF)) == 3
    with pytest.raises(ValueError):
        cw.push_data(b"\xab" * 0x10000)


def test_publication_script_refuses_over_cap():
    """The encoder is LOUD: over the cap it raises, it never emits a script."""
    big = {"internal_key": int("ab" * 500, 16),
           "snapshot": {"life": 1, "rift": 0, "key": int("cd" * 500, 16),
                        "sponsor": None, "fief": None},
           "spawn_opening": None}
    over = int("ef" * 400, 16)
    assert len(cw.jam_bytes(cw.publication_noun(over, big))) > cw.MAX_PUBLICATION
    with pytest.raises(ValueError, match="publication payload"):
        cw.make_publication_script(over, big)


def test_golden_xtr_jam():
    sp = BASIC["spawn_sont"]
    entries = [
        # entry 0 — spawn, full opening (with blind-opening)
        {"txid_hex": sp["txid"], "height": 778000, "opening": _basic_opening()},
        # entry 1 — plain custody hop, no opening
        {"txid_hex": "feedface", "height": 778010, "opening": None},
    ]
    # jam_xtr_le is a little-endian byte string (first byte = LSB), so the jam
    # atom equals int.from_bytes(..., "little").
    assert cw.build_xtr_atom(entries) == int.from_bytes(
        bytes.fromhex(BASIC["xtr"]["jam_xtr_le"]), "little"
    )


def test_xtr_single_custody_only_entry():
    entries = [{"txid_hex": "ab12", "height": 100, "opening": None}]
    # jam of [[0xab12 [100 0]] 0] — round-trips through hoon_cue.
    noun = cw.hoon_cue(cw.build_xtr_atom(entries))
    assert noun == ((0xAB12, (100, 0)), 0)


# ---------------------------------------------------------------------------
# Spawn PSBT builder — one tx, key-path, sat output = state_output_key
# ---------------------------------------------------------------------------


def _fake_utxo() -> dict:
    from embit import ec as _ec
    priv = _ec.PrivateKey(b"\x42" * 32)
    xonly = priv.get_public_key().xonly()
    return dict(
        utxo_txid="ab" * 32,
        utxo_vout=0,
        utxo_value=10_000,
        utxo_script_pubkey=bytes([0x51, 0x20]) + xonly,
        funding_internal_xonly=xonly,
        funding_path="m/86'/0'/0'/0/3",
        funding_fingerprint=b"\xaa\xbb\xcc\xdd",
    )


def _fake_snapshot() -> dict:
    return {"life": 1, "rift": 0, "key": 0xABCD, "sponsor": None, "fief": None}


def test_spawn_psbt_round_trip():
    p, proof = cw.build_spawn_psbt(**_fake_utxo(), snapshot=_fake_snapshot())
    from embit import psbt as _p
    restored = _p.PSBT.from_base64(p.to_base64())
    assert restored.tx.vin[0].vout == 0
    assert len(restored.inputs[0].taproot_bip32_derivations) == 1
    assert restored.inputs[0].taproot_internal_key is not None


def test_spawn_output_is_state_output_key():
    u = _fake_utxo()
    snap = _fake_snapshot()
    p, proof = cw.build_spawn_psbt(**u, snapshot=snap)
    q = cw.state_output_key(u["funding_internal_xonly"], snap)
    assert proof["sat_script_pubkey_hex"] == (bytes([0x51, 0x20]) + q).hex()
    assert proof["snapshot"] == snap
    # leaf hash (merkle root) is stored for the next rekey's key-path spend.
    want = cw.state_leaf_hash(cw.state_leaf_script(cw.state_commit(snap))).hex()
    assert proof["leaf_hash_hex"] == want


def test_spawn_confidential_has_no_op_return():
    p, _ = cw.build_spawn_psbt(**_fake_utxo(), snapshot=_fake_snapshot())
    scripts = [o.script_pubkey.data for o in p.tx.vout]
    assert not any(s[:1] == b"\x6a" for s in scripts), "confidential spawn must not publish"


def test_spawn_public_adds_op_return():
    u = _fake_utxo()
    snap = _fake_snapshot()
    opening = {
        "internal_key": int.from_bytes(b"\x02" + u["funding_internal_xonly"], "big"),
        "snapshot": snap,
        "spawn_opening": {"spawn": {"txid_hex": u["utxo_txid"], "vout": 0, "off": 0},
                          "start_height": 0},
    }
    p, proof = cw.build_spawn_psbt(**u, snapshot=snap,
                                   publication_pass_atom=0xDEAD, publication_opening=opening)
    op_returns = [o for o in p.tx.vout if o.script_pubkey.data[:1] == b"\x6a"]
    assert len(op_returns) == 1
    assert op_returns[0].script_pubkey.data[:7] == bytes([0x6A, 0x03, 0x75, 0x72, 0x62, 0x01, 0x09])
    assert proof["published"] is True


def test_spawn_rejects_undersized_utxo():
    u = _fake_utxo()
    u["utxo_value"] = 100
    with pytest.raises(RuntimeError):
        cw.build_spawn_psbt(**u, snapshot=_fake_snapshot())


def test_spawn_output_has_no_taptree_metadata():
    # The tweaked sat output must not claim the wallet's derivation without a
    # tap tree, or strict signers flag a change-substitution.
    p, _ = cw.build_spawn_psbt(**_fake_utxo(), snapshot=_fake_snapshot())
    assert p.outputs[0].taproot_internal_key is None
    assert not p.outputs[0].taproot_bip32_derivations


# ---------------------------------------------------------------------------
# Rekey PSBT builder — key-path spend of the current sat output
# ---------------------------------------------------------------------------


def _spawn_proof_broadcast() -> dict:
    _, proof = cw.build_spawn_psbt(**_fake_utxo(), snapshot=_fake_snapshot())
    proof["commit_txid"] = "de" * 32
    proof["op"] = "spawn"
    proof["patp"] = "~sampel-palnet"
    return proof


def test_rekey_spends_prior_sat_output_key_path():
    spawn_proof = _spawn_proof_broadcast()
    new_snap = dict(spawn_proof["snapshot"], life=2, key=0x9999)
    rekey_psbt, rekey_proof = cw.build_rekey_psbt(prior_proof=spawn_proof, new_snapshot=new_snap)
    # Input references the prior sat output.
    assert rekey_psbt.tx.vin[0].vout == spawn_proof["sat_vout"]
    # PSBT_IN_TAP_MERKLE_ROOT = the CURRENT snapshot's leaf hash so external
    # signers can compute the key-path tweak.
    assert rekey_psbt.inputs[0].taproot_merkle_root == bytes.fromhex(spawn_proof["leaf_hash_hex"])
    # Same internal key (same point owner); new output commits the new snapshot.
    assert rekey_proof["internal_pubkey_hex"] == spawn_proof["internal_pubkey_hex"]
    q = cw.state_output_key(bytes.fromhex(spawn_proof["internal_pubkey_hex"]), new_snap)
    assert rekey_proof["sat_script_pubkey_hex"] == (bytes([0x51, 0x20]) + q).hex()
    assert rekey_proof["prior_proof"]["commit_txid"] == spawn_proof["commit_txid"]


def test_rekey_rejects_unbroadcast_prior():
    _, spawn_proof = cw.build_spawn_psbt(**_fake_utxo(), snapshot=_fake_snapshot())
    with pytest.raises(ValueError, match="no commit_txid"):
        cw.build_rekey_psbt(prior_proof=spawn_proof, new_snapshot=_fake_snapshot())


def test_rekey_output_has_no_taptree_metadata():
    spawn_proof = _spawn_proof_broadcast()
    new_snap = dict(spawn_proof["snapshot"], life=2)
    p, _ = cw.build_rekey_psbt(prior_proof=spawn_proof, new_snapshot=new_snap)
    assert p.outputs[0].taproot_internal_key is None
    assert not p.outputs[0].taproot_bip32_derivations


def test_add_tap_merkle_root_hint():
    p, _ = cw.build_spawn_psbt(**_fake_utxo(), snapshot=_fake_snapshot())
    fake_root = b"\x01" * 32
    cw.add_tap_merkle_root_hint(p, 0, fake_root)
    assert p.inputs[0].taproot_merkle_root == fake_root


# ---------------------------------------------------------------------------
# proof.json — write / load / self-verify against the sat output key
# ---------------------------------------------------------------------------


def test_proof_json_round_trip(tmp_path):
    _, proof = cw.build_spawn_psbt(**_fake_utxo(), snapshot=_fake_snapshot())
    proof["op"] = "spawn"
    proof["patp"] = "~sampel-palnet"
    proof["commit_txid"] = "de" * 32
    path = tmp_path / "x.proof.json"
    cw.write_proof_json(proof, str(path))
    assert cw.load_proof_json(str(path)) == proof


def test_proof_self_verify_passes_for_fresh_spawn():
    _, proof = cw.build_spawn_psbt(**_fake_utxo(), snapshot=_fake_snapshot())
    ok, reason = cw.verify_proof_self(proof)
    assert ok, reason


def test_proof_self_verify_fails_on_tampered_snapshot():
    _, proof = cw.build_spawn_psbt(**_fake_utxo(), snapshot=_fake_snapshot())
    proof["snapshot"] = dict(proof["snapshot"], key=proof["snapshot"]["key"] ^ 1)
    ok, _ = cw.verify_proof_self(proof)
    assert not ok


def test_proof_self_verify_matches_rekey_chain():
    spawn_proof = _spawn_proof_broadcast()
    new_snap = dict(spawn_proof["snapshot"], life=2, key=0x9999)
    _, rekey_proof = cw.build_rekey_psbt(prior_proof=spawn_proof, new_snapshot=new_snap)
    ok, reason = cw.verify_proof_self(rekey_proof)
    assert ok, reason


# ---------------------------------------------------------------------------
# Messaging key extraction + ring/xtr append round-trip
# ---------------------------------------------------------------------------


def test_messaging_key_extracts_cry():
    # Build a pass 'c'(8) | ugn(256) | cry(256) | mat(dat) | ... and read cry back.
    ugn = 0x11 << 248
    cry = 0x22 << 248
    dat = 0xABCD
    w = cw.BitWriter()
    w.write(8, ord("c"))
    w.write(256, ugn)
    w.write(256, cry)
    w.write_mat(dat)
    assert cw.messaging_key_from_pass(w.to_int()) == cry


def test_ring_xtr_append_and_pass_roundtrip():
    # A miner-fresh ring has no xtr; appending one must leave dat (and hence the
    # @p) unchanged and surface the xtr in the derived pass.
    sp = BASIC["spawn_sont"]
    dat = cw.build_dat_atom(sp["txid"], sp["vout"], sp["off"])
    w = cw.BitWriter()
    w.write(8, ord("C"))
    w.write(512, 0xDEAD << 496 | 0xBEEF)  # arbitrary 64-byte seed material
    w.write_mat(dat)
    ring0 = w.to_int()
    xtr = cw.build_xtr_atom([{"txid_hex": "ab" * 32, "height": 123, "opening": None}])
    ring1 = cw.append_xtr_to_ring(ring0, xtr)

    bod0, bod1 = ring0 >> 8, ring1 >> 8
    p0, dat0 = cw._hoon_rub(512, bod0)
    p1, dat1 = cw._hoon_rub(512, bod1)
    assert dat0 == dat1 == dat
    assert bod1 >> (512 + p1) == xtr
    assert bod0 >> (512 + p0) == 0

    pass0 = cw.derive_pass_from_ring(cw.encode_uw(ring0))
    pass1 = cw.derive_pass_from_ring(cw.encode_uw(ring1))
    assert pass0 != pass1
    # ugn/cry/dat identical; only the xtr tail differs.
    assert pass1 & ((1 << (8 + 256 + 256)) - 1) == pass0 & ((1 << (8 + 256 + 256)) - 1)


def test_hoon_cue_roundtrips_jam():
    noun = ((2, 0), (0x42CD, (0, ((1, 0xABCDEF0123456789), 0))))
    assert cw.hoon_cue(cw.hoon_jam(noun)) == noun


def test_rebuild_feed_matches_feed_shape():
    ring = 0x43 | (99 << 8)
    feed = cw.rebuild_feed(0x42CD, 0, 1, ring)
    assert cw.hoon_cue(feed) == ((2, 0), (0x42CD, (0, ((1, ring), 0))))


# ---------------------------------------------------------------------------
# Xpub / descriptor parsing
# ---------------------------------------------------------------------------


# BIP-86 test vector, mainnet.
BIP86_XPUB = (
    "xpub6BgBgsespWvERF3LHQu6CnqdvfEvtMcQjYrcRzx53QJjSxarj2afYWcLteoGVky7D3UKDP9QyrLprQ3VCECoY49yfdDEHGCtMMj92pReUsQ"
)


def test_parse_key_source_bare_xpub():
    src = cw.parse_key_source(BIP86_XPUB, network="main")
    assert src.master_fingerprint == b"\x00\x00\x00\x00"
    assert src.account_path == [0x80000000 | 86, 0x80000000, 0x80000000]


def test_parse_key_source_descriptor_with_origin():
    desc = f"tr([abcd1234/86h/0h/0h]{BIP86_XPUB}/0/*)"
    src = cw.parse_key_source(desc, network="main")
    assert src.master_fingerprint == bytes.fromhex("abcd1234")
    assert src.account_path == [0x80000000 | 86, 0x80000000, 0x80000000]


def test_parse_key_source_rejects_non_tr_descriptor():
    with pytest.raises(ValueError, match="taproot"):
        cw.parse_key_source(f"wpkh([abcd1234/84h/0h/0h]{BIP86_XPUB})", network="main")


def test_derive_address_yields_mainnet_bech32m():
    src = cw.parse_key_source(BIP86_XPUB, network="main")
    addr, spk, xonly, path = src.derive_address(0, 0)
    assert addr.startswith("bc1p"), f"expected bc1p prefix, got {addr}"
    assert len(spk) == 34
    assert spk[:2] == bytes([0x51, 0x20])
    assert len(xonly) == 32
    assert path.endswith("/0/0")


# ---------------------------------------------------------------------------
# Mnemonic generation + key derivation
# ---------------------------------------------------------------------------


def test_generate_new_mnemonic_is_12_words_by_default():
    assert len(cw.generate_new_mnemonic(strength_bits=128).split()) == 12


def test_generate_new_mnemonic_24_words_for_256_bits():
    assert len(cw.generate_new_mnemonic(strength_bits=256).split()) == 24


def test_mnemonic_to_hdkey_and_fingerprint_deterministic():
    m = "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about"
    k1 = cw.mnemonic_to_hdkey(m)
    k2 = cw.mnemonic_to_hdkey(m)
    assert k1.to_base58() == k2.to_base58()
    fp1 = cw.hdkey_fingerprint(k1)
    fp2 = cw.hdkey_fingerprint(k2)
    assert fp1 == fp2
    assert len(fp1) == 4


def test_hdkey_fingerprint_no_openssl3_crash():
    root = cw.mnemonic_to_hdkey(cw.generate_new_mnemonic())
    fp = cw.hdkey_fingerprint(root)
    assert isinstance(fp, bytes) and len(fp) == 4


def test_extract_tx_does_not_use_segwit_kwarg():
    from embit.transaction import Witness
    p, _ = cw.build_spawn_psbt(**_fake_utxo(), snapshot=_fake_snapshot())
    p.inputs[0].final_scriptwitness = Witness([b"\x00" * 64])
    txid, tx_hex = cw._extract_tx_from_psbt(p.to_base64())
    assert len(txid) == 64
    assert len(tx_hex) > 0


# ---------------------------------------------------------------------------
# Mnemonyms — cross-checked against gwbtc/mnemonyms reference vectors.
# ---------------------------------------------------------------------------


def test_mnemonym_comet_roundtrip_and_matches_web():
    patp = "~mosnyt-londen-lacrux-rosmeg--mitrev-larder-loddux-daplyd"
    atom = cw.patp_to_int(patp)
    assert atom == 0xE7F38742227BF54EC48BE13DBA9242CD
    nym = cw.patp_to_mnemonym(patp)
    assert nym == (
        ".routine.inhale.regimes.consent.pretense.misspeaks"
        ".amassed.enjoin.concern.relieves.forestall.infests"
    )
    assert cw.mnemonym_to_atom(nym) == atom
    assert cw.resolve_id(nym) == atom
    assert cw.resolve_id(patp) == atom


def test_mnemonym_edge_cases_and_shortening():
    assert cw.atom_to_mnemonym(0) == ".abducts"
    assert cw.mnemonym_to_atom(".abducts") == 0
    assert cw.atom_to_mnemonym(0x42CD) == ".allied.inflict"  # ~daplyd star value
    long = cw.atom_to_mnemonym(0xDEADBEEFCAFEBABE1122334455667788)
    assert cw.abridge_mnemonym(long) == ".respects...aloft"
    assert cw.is_mnemonym(".abducts") and not cw.is_mnemonym("~sampel-palnet")


def test_int_to_patp_comet_roundtrip():
    import secrets
    for _ in range(200):
        atom = (1 << 64) + secrets.randbits(64)
        assert cw.patp_to_int(cw.int_to_patp(atom)) == atom
    assert cw.int_to_patp(0xE7F38742227BF54EC48BE13DBA9242CD) == (
        "~mosnyt-londen-lacrux-rosmeg--mitrev-larder-loddux-daplyd"
    )
    assert cw.int_to_patp(0x42CD) == "~daplyd"


# ---------------------------------------------------------------------------
# start-height: the FUNDING tx's block, never the spawn tx's
#
# sur/self-attestation defines a blind-opening's start-height as the block
# containing the transaction that CREATED the spawn satpoint. `finalize` used
# to write the SPAWN tx's height instead, and no proof ever carried an explicit
# start_height, so the wrong default always won. The verifier fetches that
# height first and can only address a tx by [height txid], so the packet died
# with attestation-tx-not-found and NO verdict: every comet Causeway had ever
# minted was unverifiable. Confirmed live on mainnet against all three of the
# comets minted with the old code.
# ---------------------------------------------------------------------------

FUNDING_TXID = "aa" * 32
SPAWN_TXID = "bb" * 32
FUNDING_HEIGHT = 961_044
SPAWN_HEIGHT = 961_055


def _spawn_proof(**over) -> dict:
    proof = {
        "version": 2,
        "protocol": "kelvin-9",
        "sat_vout": 0,
        "internal_pubkey_hex": "11" * 32,
        "snapshot": {"life": 1, "rift": 0, "key": 0xABCD, "sponsor": None, "fief": None},
        "network": "main",
        "funding": {"txid": FUNDING_TXID, "vout": 1, "value": 2_000},
        "commit_txid": SPAWN_TXID,
    }
    proof.update(over)
    return proof


def _stub_mempool(monkeypatch, *, funding_confirmed=True, funding_height=FUNDING_HEIGHT):
    """mempool.space stub: the spawn confirms at SPAWN_HEIGHT, its funding at
    funding_height. The two heights differ, which is the whole point."""
    calls: list[str] = []

    def stub(path, base=None):
        calls.append(path)
        if path == f"/tx/{SPAWN_TXID}":
            return {"status": {"confirmed": True, "block_height": SPAWN_HEIGHT,
                               "block_hash": "de" * 32}}
        if path == f"/tx/{FUNDING_TXID}":
            if not funding_confirmed:
                return {"status": {"confirmed": False}}
            return {"status": {"confirmed": True, "block_height": funding_height,
                               "block_hash": "ad" * 32}}
        raise AssertionError(f"unexpected mempool call {path}")

    monkeypatch.setattr(cw, "mempool_get", stub)
    return calls


def test_resolve_start_height_uses_the_funding_tx_not_the_spawn(monkeypatch):
    _stub_mempool(monkeypatch)
    assert cw.resolve_start_height(_spawn_proof()) == FUNDING_HEIGHT


def test_resolve_start_height_prefers_a_recorded_height(monkeypatch):
    def boom(path, base=None):
        raise AssertionError("must not hit the network when the height is known")
    monkeypatch.setattr(cw, "mempool_get", boom)
    p = _spawn_proof()
    p["funding"]["height"] = FUNDING_HEIGHT
    assert cw.resolve_start_height(p) == FUNDING_HEIGHT
    assert cw.resolve_start_height(_spawn_proof(start_height=FUNDING_HEIGHT)) == FUNDING_HEIGHT


def test_resolve_start_height_ignores_the_zero_placeholder(monkeypatch):
    # 0 is what a published spawn's opening carries (unknowable pre-broadcast);
    # it must never be mistaken for a real funding height.
    _stub_mempool(monkeypatch)
    p = _spawn_proof(start_height=0)
    p["funding"]["height"] = 0
    assert cw.resolve_start_height(p) == FUNDING_HEIGHT


def test_resolve_start_height_refuses_to_guess(monkeypatch):
    _stub_mempool(monkeypatch, funding_confirmed=False)
    with pytest.raises(Exception):
        cw.resolve_start_height(_spawn_proof())
    monkeypatch.setattr(cw, "mempool_get", lambda p, base=None: {})
    no_funding = _spawn_proof()
    no_funding["funding"] = {}
    with pytest.raises(Exception):
        cw.resolve_start_height(no_funding)


def _xtr_start_height(xtr: int) -> int:
    """Dig the spawn entry's blind-opening start-height out of a jammed xtr.

    xtr           = (jam (list custody-entry))
    custody-entry = [txid height opening=(unit opening)]
    opening       = [internal-key snapshot spawn-opening=(unit spawn-opening)]
    spawn-opening = [spawn start-height]
    """
    log = cw.hoon_cue(xtr)
    entry0 = log[0]
    opening = entry0[1][1][1]          # (unit opening) -> opening
    spawn_opening = opening[1][1][1]   # (unit spawn-opening) -> spawn-opening
    return spawn_opening[1]            # start-height is the tail


def test_finalize_bakes_the_funding_height_into_the_spawn_opening(tmp_path, monkeypatch):
    _stub_mempool(monkeypatch)
    path = tmp_path / "spawn.json"
    cw.write_proof_json(_spawn_proof(), str(path))

    cw.cmd_finalize.callback(
        proofs=(str(path),), feed=None, wait=False,
        poll_interval=1, mempool_base="stub",
    )

    out = cw.load_proof_json(str(path))
    # the spawn tx's own height is recorded, and is NOT the start-height
    assert out["block_height"] == SPAWN_HEIGHT
    assert out["start_height"] == FUNDING_HEIGHT
    assert out["funding"]["height"] == FUNDING_HEIGHT
    assert _xtr_start_height(int(out["xtr_hex"], 16)) == FUNDING_HEIGHT


# ---------------------------------------------------------------------------
# Routability: Causeway refuses to mint an unroutable comet
#
# A snapshot with NEITHER sponsor NOR fief is a one-way identity:
# +urb-point-to-jael projects an absent sponsor to SELF, so nothing can route
# to it, and once a peer drops its state it can never be re-contacted. That is
# legal protocol (decisions-addendum section 2) — the verifier must never
# reject it — so the refusal lives in the mint tools, with an explicit
# --no-route opt-out for exotic outbound-only uses.
# ---------------------------------------------------------------------------


SPONSOR_PATP = "~marzod"


def test_snapshot_is_routable():
    assert not cw.snapshot_is_routable({"sponsor": None, "fief": None})
    assert cw.snapshot_is_routable({"sponsor": 42, "fief": None})
    assert cw.snapshot_is_routable({"sponsor": None, "fief": ("%if", 1, 2)})


def test_assert_routable_refuses_by_default():
    with pytest.raises(Exception) as e:
        cw.assert_routable({"sponsor": None, "fief": None})
    assert "neither a sponsor nor a fief" in str(e.value)


def test_assert_routable_opt_out_is_explicit():
    cw.assert_routable({"sponsor": None, "fief": None}, no_route=True)


def test_resolve_sponsor_accepts_a_patp():
    assert cw.resolve_sponsor(SPONSOR_PATP) == cw.patp_to_int(SPONSOR_PATP)
    assert cw.resolve_sponsor(None) is None
    assert cw.resolve_sponsor("  ") is None
    with pytest.raises(Exception):
        cw.resolve_sponsor("~not-a-real-ship-name-at-all")


def test_spawn_refuses_an_unroutable_comet_before_doing_any_work(monkeypatch):
    """The refusal must land BEFORE the faucet / UTXO scan / proof-of-work, so
    an operator never burns a mine (or a broadcast) on a stranded identity."""
    def boom(*a, **k):
        raise AssertionError("must not touch the network / miner")

    monkeypatch.setattr(cw, "parse_key_source", boom)
    monkeypatch.setattr(cw, "scan_addresses", boom)
    monkeypatch.setattr(cw, "mine_comet_from_utxo", boom)
    monkeypatch.setattr(cw, "generate_new_mnemonic", boom)

    with pytest.raises(Exception) as e:
        cw.run_spawn_connect("xpub-does-not-matter", None, 2, "main", ".", FAKE_MINER, "stub")
    assert "neither a sponsor nor a fief" in str(e.value)

    with pytest.raises(Exception) as e:
        cw.run_spawn_generate(None, 2, "main", ".", FAKE_MINER, "stub")
    assert "neither a sponsor nor a fief" in str(e.value)


def test_spawn_with_a_sponsor_passes_the_routability_gate(monkeypatch):
    """With --sponsor the gate lets the flow proceed; it stops at the first
    real step instead (proving the gate, not the network, was the blocker)."""
    sentinel = RuntimeError("reached parse_key_source")

    def stop(*a, **k):
        raise sentinel

    monkeypatch.setattr(cw, "parse_key_source", stop)
    with pytest.raises(RuntimeError) as e:
        cw.run_spawn_connect("xpub", None, 2, "main", ".", FAKE_MINER, "stub",
                             sponsor=SPONSOR_PATP)
    assert e.value is sentinel


def test_spawn_no_route_flag_passes_the_gate(monkeypatch):
    sentinel = RuntimeError("reached parse_key_source")

    def stop(*a, **k):
        raise sentinel

    monkeypatch.setattr(cw, "parse_key_source", stop)
    with pytest.raises(RuntimeError) as e:
        cw.run_spawn_connect("xpub", None, 2, "main", ".", FAKE_MINER, "stub",
                             no_route=True)
    assert e.value is sentinel


# require_miner() now runs before any wallet or funding work, so a spawn
# runner can no longer be driven past its preflight with a made-up miner
# path.  The preflight checks existence and executability, not behaviour --
# every test that goes deeper stubs the mining call itself -- so any real
# executable satisfies it.
FAKE_MINER = "/bin/ls"


def _rekey_prior(tmp_path, sponsor=None):
    prior = _spawn_proof()
    prior["patp"] = "~sampel-palnet"
    prior["snapshot"] = {"life": 1, "rift": 0, "key": 7, "sponsor": sponsor, "fief": None}
    path = tmp_path / "prior.proof.json"
    cw.write_proof_json(prior, str(path))
    return str(path)


def test_rekey_refuses_to_strand_an_unsponsored_comet(tmp_path, monkeypatch):
    def boom(*a, **k):
        raise AssertionError("must not build a PSBT for a stranded comet")

    monkeypatch.setattr(cw, "build_rekey_psbt", boom)
    with pytest.raises(Exception) as e:
        cw._run_rekey_op("~sampel-palnet", _rekey_prior(tmp_path), 9, False, 2,
                         "main", str(tmp_path), "stub")
    assert "neither a sponsor nor a fief" in str(e.value)


def test_rekey_carries_the_prior_sponsor_forward(tmp_path, monkeypatch):
    """A sponsored comet rekeys freely: the gate sees the carried-forward
    sponsor and the flow reaches the PSBT builder."""
    seen = {}

    def capture(prior_proof, new_snapshot, fee_rate, network):
        seen.update(new_snapshot)
        raise RuntimeError("stop after the snapshot")

    monkeypatch.setattr(cw, "build_rekey_psbt", capture)
    with pytest.raises(RuntimeError):
        cw._run_rekey_op("~sampel-palnet", _rekey_prior(tmp_path, sponsor=1234), 9,
                         False, 2, "main", str(tmp_path), "stub")
    assert seen["sponsor"] == 1234
    assert seen["life"] == 2


def test_rekey_sponsor_flag_sets_a_new_sponsor(tmp_path, monkeypatch):
    seen = {}

    def capture(prior_proof, new_snapshot, fee_rate, network):
        seen.update(new_snapshot)
        raise RuntimeError("stop after the snapshot")

    monkeypatch.setattr(cw, "build_rekey_psbt", capture)
    with pytest.raises(RuntimeError):
        cw._run_rekey_op("~sampel-palnet", _rekey_prior(tmp_path), 9, False, 2,
                         "main", str(tmp_path), "stub", sponsor=SPONSOR_PATP)
    assert seen["sponsor"] == cw.patp_to_int(SPONSOR_PATP)


def test_rekey_no_route_allows_a_deliberate_strand(tmp_path, monkeypatch):
    seen = {}

    def capture(prior_proof, new_snapshot, fee_rate, network):
        seen.update(new_snapshot)
        raise RuntimeError("stop after the snapshot")

    monkeypatch.setattr(cw, "build_rekey_psbt", capture)
    with pytest.raises(RuntimeError):
        cw._run_rekey_op("~sampel-palnet", _rekey_prior(tmp_path), 9, False, 2,
                         "main", str(tmp_path), "stub", no_route=True)
    assert seen["sponsor"] is None


def test_tui_spawn_and_rekey_go_through_the_routability_gate():
    """The TUI is a third mint path; it must not bypass the refusal."""
    pytest.importorskip("textual")
    import inspect
    import causeway_tui as tui
    assert "assert_routable" in inspect.getsource(tui.SpawnMethodScreen.on_button_pressed)
    assert "assert_routable" in inspect.getsource(tui.MiningScreen.run_mine)
    assert "assert_routable" in inspect.getsource(tui.ManageFormScreen)


# ---------------------------------------------------------------------------
# The in-band %anew hand-off: the dojo poke `causeway finalize` prints.
#
# decisions-addendum section 5: after a custody tx confirms, the owner hands
# the ship's OWN %gw-btc the new xtr entry + opening; the agent re-verifies
# the whole extended log on-chain before refreshing the pass. The poke rides
# the %noun mark ($ingest in gw-btc/sur/self-attestation.hoon), so this
# string is the desktop half of that contract.
# ---------------------------------------------------------------------------


def test_custody_entry_poke_shape_for_a_spawn():
    entry = {
        "txid_hex": "de" * 32,
        "height": 900_142,
        "opening": {
            "internal_key": int("02" + "ab" * 32, 16),
            "snapshot": {"life": 1, "rift": 0, "key": 0xC0FFEE, "sponsor": None, "fief": None},
            "spawn_opening": {
                "spawn": {"txid_hex": "ad" * 32, "vout": 1, "off": 0},
                "start_height": 900_100,
            },
        },
    }
    line = cw.format_custody_entry_poke(entry)
    assert line.startswith(":gw-btc &noun [%gw-custody-entry ")
    # entry = [txid height opening]; the opening is PRESENT (a spawn)
    assert " 900.142 `[" in line
    # the spawn-opening carries the FUNDING tx's height, not the entry's
    assert "900.100" in line
    assert "900.142" != "900.100"
    # sponsor absent, fief absent -> two bare ~ inside the snapshot
    assert "~ ~]" in line
    # balanced brackets — the dojo has to parse this
    assert line.count("[") == line.count("]")
    # Hoon REQUIRES dot grouping above 999; a bare 900142 is a syntax error
    # and would make the printed line unpasteable.
    assert re.search(r"(?<![.\dx])\d{4,}", line) is None
    # exact shape, pinned: this literal has been cast against the Hoon mold
    # ($ingest in gw-btc/sur/self-attestation.hoon) with `urbit eval`.
    assert line == (
        ":gw-btc &noun [%gw-custody-entry ["
        + cw.format_hoon_ux("de" * 32)
        + " 900.142 `["
        + cw.format_hoon_ux("02" + "ab" * 32)
        + " [1 0 0xc0.ffee ~ ~] `[["
        + cw.format_hoon_ux("ad" * 32)
        + " 1 0] 900.100]]]]"
    )


def test_custody_entry_poke_renders_sponsor_and_a_bare_hop():
    entry = {
        "txid_hex": "de" * 32,
        "height": 900_200,
        "opening": {
            "internal_key": int("02" + "ab" * 32, 16),
            "snapshot": {"life": 2, "rift": 0, "key": 0xC0FFEE,
                         "sponsor": cw.patp_to_int("~marzod"), "fief": None},
            "spawn_opening": None,     # a state update, not a spawn
        },
    }
    line = cw.format_custody_entry_poke(entry)
    assert "`~marzod" in line
    # an absent blind-opening is the bare ~ that marks a non-spawn hop
    assert line.rstrip().endswith("~]]]")
    assert line.count("[") == line.count("]")


# ---------------------------------------------------------------------------
# The fief is snapshot state (decisions-addendum §2), so it is inside the
# state commitment.  Mainnet vector: C3
# ~ligdes-risbur-folmus-mattyp--firpec-lispec-noddyl-daplyd committed
# fief=[%if .64.227.13.22 34.343] in its life-2 state update, tx
# 8e713009f0c067374deca6069a99076efb2bff3fb1da7f4164f597df1401a4fb, block
# 961.129.  Causeway used to print `~` for this field, producing an %anew
# poke whose state-key could never match the chain — and %anew fails SILENTLY
# (PHASE5B-RESULTS.md finding 6).
# ---------------------------------------------------------------------------

C3_INTERNAL_X = "e129efeba5ace29c3e118634f568ca73ec84d0283695e0d497e9ad9cf9e87703"
C3_TXID = "8e713009f0c067374deca6069a99076efb2bff3fb1da7f4164f597df1401a4fb"
C3_HEIGHT = 961_129
C3_KEY = 0xDF309632F565FDDF5E0CC5A6EA6CC78091056F94E15F12D0E75F70BC7CA43857
# [%if .64.227.13.22 34.343] as a noun: 0x40e30d16 = 64.227.13.22
C3_FIEF = (cw.FIEF_TAGS["if"], (0x40E30D16, 34_343))
C3_STATE_COMMIT = "6f5beb6853dff709fdd333071c2dc002deff8d2f5083893bd811b81a846d2302"
C3_OUTPUT_KEY_Q = "3aea31567982f409f9a3e43b98610c2360c1e135427db948c4e0c56a184ee110"


def _c3_snapshot(fief=C3_FIEF) -> dict:
    return {"life": 2, "rift": 0, "key": C3_KEY, "sponsor": None, "fief": fief}


def _c3_entry(snapshot) -> dict:
    return {
        "txid_hex": C3_TXID,
        "height": C3_HEIGHT,
        "opening": {
            "internal_key": int("02" + C3_INTERNAL_X, 16),
            "snapshot": snapshot,
            "spawn_opening": None,     # a state update, not the spawn
        },
    }


def test_fief_is_inside_the_state_commitment_mainnet_c3():
    """Why the poke must carry it: the commitment is over the JAMMED snapshot,
    so a dropped fief silently yields a different on-chain output key."""
    internal = bytes.fromhex(C3_INTERNAL_X)
    assert cw.state_commit(_c3_snapshot()).hex() == C3_STATE_COMMIT
    assert cw.state_output_key(internal, _c3_snapshot()).hex() == C3_OUTPUT_KEY_Q
    # …and without it, nothing matches the chain.
    assert cw.state_output_key(internal, _c3_snapshot(None)).hex() != C3_OUTPUT_KEY_Q


def test_custody_entry_poke_carries_the_snapshots_fief():
    line = cw.format_custody_entry_poke(_c3_entry(_c3_snapshot()))
    assert "`[%if .64.227.13.22 34.343]" in line
    # exact line — the one a C3 operator must paste into the dojo
    assert line == (
        ":gw-btc &noun [%gw-custody-entry ["
        + cw.format_hoon_ux(C3_TXID)
        + " 961.129 `["
        + cw.format_hoon_ux("02" + C3_INTERNAL_X)
        + " [2 0 "
        + cw.format_hoon_ux(format(C3_KEY, "x"))
        + " ~ `[%if .64.227.13.22 34.343]] ~]]]"
    )
    assert line.count("[") == line.count("]")
    # Hoon needs dot grouping above 999 — 34343 would be a syntax error
    assert re.search(r"(?<![.\dx])\d{4,}", line) is None


def test_custody_entry_poke_still_prints_a_bare_tilde_without_a_fief():
    line = cw.format_custody_entry_poke(_c3_entry(_c3_snapshot(None)))
    assert "~ ~]" in line and "%if" not in line
    assert line != cw.format_custody_entry_poke(_c3_entry(_c3_snapshot()))


def test_fief_survives_the_proof_json_round_trip():
    """`causeway finalize` reads the snapshot straight out of proof.json, where
    the fief noun's tuples have become lists. Both must encode identically."""
    from_json = json.loads(json.dumps(_c3_snapshot(), default=list))
    assert from_json["fief"] == [26217, [1088621846, 34343]]   # not tuples
    assert (cw.format_custody_entry_poke(_c3_entry(from_json))
            == cw.format_custody_entry_poke(_c3_entry(_c3_snapshot())))
    assert cw.state_output_key(
        bytes.fromhex(C3_INTERNAL_X), from_json).hex() == C3_OUTPUT_KEY_Q


def test_format_hoon_fief_renders_every_arm():
    assert cw.format_hoon_fief(None) is None
    assert cw.format_hoon_fief(C3_FIEF) == "[%if .64.227.13.22 34.343]"
    # @is is 8 base-16 groups of 16 bits (+ro-co:co), @if 4 base-10 of 8
    assert cw.format_hoon_fief((cw.FIEF_TAGS["is"], (1, 8_080))) == (
        "[%is .0.0.0.0.0.0.0.1 8.080]"
    )
    turf = (("com", "example"),)   # tld first
    p = 0
    for t in reversed(turf):
        labels = 0
        for lab in reversed(t):
            labels = (int.from_bytes(lab.encode(), "little"), labels)
        p = (labels, p)
    assert cw.format_hoon_fief((cw.FIEF_TAGS["turf"], (p, 80))) == (
        "[%turf ~[~['com' 'example']] 80]"
    )
    with pytest.raises(ValueError):
        cw.format_hoon_fief((0xDEAD, (1, 2)))


# ---------------------------------------------------------------------------
# zig-out path resolution
#
# `zig build` names the install directory after the *resolved* target triple.
# vere/build.zig and comet-miner/build.zig both rewrite a native Linux build to
# musl, so "<arch>-linux-none" is a triple zig never emits and must never be a
# default — that bug made --vere/--miner mandatory on Linux with no hint.
# ---------------------------------------------------------------------------


def test_zig_candidates_never_offer_a_linux_none_triple():
    for machine in ("x86_64", "amd64", "aarch64", "arm64"):
        cands = cw._zig_target_candidates(machine, "Linux")
        assert not any(c.endswith("-linux-none") for c in cands), cands
        assert cands[0].endswith("-linux-musl")
        assert cands[1].endswith("-linux-gnu")


def test_zig_candidates_are_unchanged_on_macos():
    assert cw._zig_target_candidates("arm64", "Darwin") == ["aarch64-macos-none"]
    assert cw._zig_target_candidates("x86_64", "Darwin") == ["x86_64-macos-none"]


def test_zig_out_bin_prefers_the_directory_that_exists(tmp_path, monkeypatch):
    monkeypatch.setattr(cw.platform, "machine", lambda: "x86_64")
    monkeypatch.setattr(cw.platform, "system", lambda: "Linux")
    built = tmp_path / "zig-out" / "x86_64-linux-gnu"
    built.mkdir(parents=True)
    (built / "urbit").write_text("")
    assert cw._zig_out_bin(str(tmp_path), "urbit") == f"{built}/urbit"


def test_zig_out_bin_names_every_path_it_looked_at(tmp_path, monkeypatch):
    monkeypatch.setattr(cw.platform, "machine", lambda: "x86_64")
    monkeypatch.setattr(cw.platform, "system", lambda: "Linux")
    path = cw._zig_out_bin(str(tmp_path), "urbit")
    assert path == f"{tmp_path}/zig-out/x86_64-linux-musl/urbit"
    assert "x86_64-linux-gnu/urbit" in cw._not_found_hint(path)


# ---------------------------------------------------------------------------
# Funded state updates — topping the identity sat up instead of letting it
# decay.  A state update used to spend the identity sat as its ONLY input and
# take the fee out of it, so the sat shrank every time and a comet could be
# priced out of its own identity for good.  A funding input placed AFTER
# input 0 fixes that without touching the protocol.
#
# The verifier imposes no input-count constraint: self-attestation.hoon:674
# reads `(snag-input 0 this)` and :678-680 checks only that input 0 is the
# tracked satpoint; :683-689 reads input 0's prevout for the key-path check;
# The verifier's `continuity` check compares input 0's outpoint against the
# satpoint the log has reached.
# The functions below mirror the Hoon that actually decides where the sat
# lands, so these tests exercise the real rule rather than a paraphrase.
# ---------------------------------------------------------------------------


def _index_to_sont(index: int, out_values: list[int]):
    """Mirror of +index-to-sont (gw-btc/lib/urb-core.hoon:491-503).

    Walks the outputs in order, subtracting each output's value, and returns
    (vout, off) at the first output the running index falls inside. `~` (None)
    means the sat landed in the miner fee.
    """
    vout = 0
    for value in out_values:
        if index < value:
            return (vout, index)
        index -= value
        vout += 1
    return None


def _verify_hop(prev_out_values: list[int], current: tuple, tx) -> dict:
    """Mirror of the per-entry custody checks in +run-checks
    (self-attestation.hoon:674-700), for one hop.

    `current` is the tracked satpoint (txid_hex, vout, off). Returns the named
    checks plus the derived next satpoint, exactly as the Hoon computes them.
    """
    cur_txid, cur_vout, cur_off = current
    checks: dict = {}

    #  :674  =/  inp  (snag-input 0 this)
    checks["input-zero"] = len(tx.vin) > 0
    if not checks["input-zero"]:
        return checks

    #  :678-680  continuity: input 0's outpoint IS the tracked satpoint
    #  embit keeps TransactionInput.txid in display order (it reverses on the
    #  wire), so this is the same hex the proof carries.
    inp0 = tx.vin[0]
    checks["continuity"] = (
        inp0.txid.hex() == cur_txid and int(inp0.vout) == int(cur_vout)
    )

    #  :681-682  prevout-range, then `spent` = the prevout of input 0
    checks["prevout-range"] = cur_vout < len(prev_out_values)
    if not checks["prevout-range"]:
        return checks
    spent_value = prev_out_values[cur_vout]

    #  :691  =/  off-ok=?  (lth off.current value.spent)
    checks["off-range"] = cur_off < spent_value

    #  :695  =/  landed  (index-to-sont:uc off.current os.this)
    #  NOTE the argument: the sat's offset within INPUT 0's prevout is used
    #  directly as the transaction-wide ordinal index.  That identity holds
    #  only while input 0 is first.
    landed = _index_to_sont(cur_off, [o.value for o in tx.vout])
    checks["sat-landed"] = landed is not None
    if landed is not None:
        #  :704  =/  next=sont:ord  [id.this vout.landed off.landed]
        checks["next"] = (tx.txid().hex(), landed[0], landed[1])
    return checks


#  Deliberately NOT byte-palindromic: a txid like "de"*32 reads the same
#  forwards and backwards, so it silently hides display-vs-wire byte-order
#  mistakes in anything that reads an outpoint back off a built tx.
_IDENTITY_TXID = "d232c3282129e9c118aa6752925bb8c32a5e0c5ab40770598d1200d339860130"
_FUND_TXID_A = "9f1c40ab77e5d3b2600891ca4e37f5d80b2e6a1c93445fe7028dc6b1a5730e42"
_FUND_TXID_B = "3ac70e91b58d24f6017ec3a92b6045de8f19cc7302b4a86df15e0937c2481bad"


def _funded_prior(sat_value: int = 754, sat_off: int = 0) -> dict:
    """A prior proof standing in for a decayed identity sat — k1's real 754."""
    proof = _spawn_proof_broadcast()
    proof["commit_txid"] = _IDENTITY_TXID
    proof["sat_value"] = sat_value
    if sat_off:
        proof["sat_off"] = sat_off
    return proof


def _fund(value: int = 20_000, txid: str = _FUND_TXID_A, vout: int = 1) -> dict:
    from embit import ec as _ec
    xonly = _ec.PrivateKey(b"\x77" * 32).get_public_key().xonly()
    return {
        "txid": txid,
        "vout": vout,
        "value": value,
        "script_pubkey": bytes([0x51, 0x20]) + xonly,
        "xonly": xonly,
        "path": "m/86h/0h/0h/0/5",
        "fingerprint": b"\xaa\xbb\xcc\xdd",
    }


# --- the mirror itself, checked against hand-computed ordinals --------------


def test_index_to_sont_mirror_matches_hand_computed_ordinals():
    # Sats are assigned to outputs in order; offset 0 is output 0's first sat.
    assert _index_to_sont(0, [1000, 500]) == (0, 0)
    assert _index_to_sont(999, [1000, 500]) == (0, 999)
    assert _index_to_sont(1000, [1000, 500]) == (1, 0)   # first sat of output 1
    assert _index_to_sont(1499, [1000, 500]) == (1, 499)
    assert _index_to_sont(1500, [1000, 500]) is None     # landed in the fee
    # A zero-value OP_RETURN holds no sats and is skipped, not landed in.
    assert _index_to_sont(0, [0, 1000]) == (1, 0)


# --- the load-bearing ordinal claim ----------------------------------------


@pytest.mark.parametrize("sat_off", [0, 1, 400, 4_000])
def test_trailing_funding_input_does_not_move_the_sat(sat_off):
    """THE load-bearing claim: sats are assigned to outputs in input order, so
    an input placed AFTER input 0 contributes sats strictly behind the tracked
    one and cannot shift its offset.

    Proven by building the same state update with and without a funding input
    and landing the same sat through the verifier's own +index-to-sont.
    """
    prior = _funded_prior(sat_value=5_000, sat_off=sat_off)
    new_snap = dict(prior["snapshot"], life=2, key=0x9999)

    unfunded, _ = cw.build_rekey_psbt(prior_proof=prior, new_snapshot=new_snap,
                                      fee_rate=1)
    funded, _ = cw.build_rekey_psbt(prior_proof=prior, new_snapshot=new_snap,
                                    fee_rate=1, funding_inputs=[_fund(20_000)])

    assert len(unfunded.tx.vin) == 1
    assert len(funded.tx.vin) == 2, "the funding input must actually be there"

    a = _index_to_sont(sat_off, [o.value for o in unfunded.tx.vout])
    b = _index_to_sont(sat_off, [o.value for o in funded.tx.vout])
    assert a is not None and b is not None
    assert a[0] == b[0] == 0, "the sat must land in output 0 either way"
    assert a[1] == b[1] == sat_off, "and at the very same offset"


def test_a_funding_input_ahead_of_the_identity_would_move_the_sat():
    """The counterfactual that makes input-0 ordering load-bearing.

    The verifier feeds +index-to-sont the sat's offset within input 0's prevout
    as if it were the transaction-wide index (self-attestation.hoon:695). Put a
    funding input FIRST and the real index becomes funding_value + off, so the
    sat lands somewhere else entirely — or in the fee. This is why the builder
    refuses to construct that ordering.
    """
    out_values = [50_000, 1_000]
    sat_off = 400
    assert _index_to_sont(sat_off, out_values) == (0, 400)
    # With 20_000 sats of funding ahead of it, the same sat is at index 20_400.
    assert _index_to_sont(20_000 + sat_off, out_values) == (0, 20_400)
    # And with a smaller sat output it leaves output 0 altogether.
    assert _index_to_sont(sat_off, [1_000, 500]) == (0, 400)
    assert _index_to_sont(20_000 + sat_off, [1_000, 500]) is None


# --- a funded state update through the verifier's checks -------------------


def test_funded_state_update_passes_the_verifier_checks():
    prior = _funded_prior(sat_value=754)
    prior_out_values = [754]           # the tx that created the identity output
    current = (prior["commit_txid"], prior["sat_vout"], 0)
    new_snap = dict(prior["snapshot"], life=2, key=0x9999)

    p, proof = cw.build_rekey_psbt(prior_proof=prior, new_snapshot=new_snap,
                                   fee_rate=4, funding_inputs=[_fund(20_000)])
    checks = _verify_hop(prior_out_values, current, p.tx)
    assert checks["input-zero"] is True
    assert checks["continuity"] is True
    assert checks["prevout-range"] is True
    assert checks["off-range"] is True
    assert checks["sat-landed"] is True
    # The tip derivation (+derive-tip, self-attestation.hoon:449) lands the sat
    # in output 0 of the new tx, which is the sat-carrying output.
    assert checks["next"] == (p.tx.txid().hex(), 0, 0)
    assert proof["sat_vout"] == 0


def test_funded_state_update_tip_chains_across_two_hops():
    """+derive-tip walks the chain; a funded hop must not break the walk."""
    prior = _funded_prior(sat_value=754)
    current = (prior["commit_txid"], prior["sat_vout"], 0)
    snap2 = dict(prior["snapshot"], life=2, key=0x9999)

    hop1, proof1 = cw.build_rekey_psbt(prior_proof=prior, new_snapshot=snap2,
                                       fee_rate=4, funding_inputs=[_fund(20_000)])
    c1 = _verify_hop([754], current, hop1.tx)
    assert c1["sat-landed"] and c1["continuity"]

    # Second hop chains off the proof the first one emitted — unfunded this
    # time, spending the now-fat identity output.
    proof1["commit_txid"] = hop1.tx.txid().hex()
    snap3 = dict(snap2, life=3, key=0x7777)
    hop2, _ = cw.build_rekey_psbt(prior_proof=proof1, new_snapshot=snap3, fee_rate=4)
    c2 = _verify_hop([o.value for o in hop1.tx.vout], c1["next"], hop2.tx)
    assert c2["input-zero"] and c2["continuity"] and c2["off-range"]
    assert c2["sat-landed"] is True
    assert c2["next"][1] == 0


# --- the actual point: the sat output can grow -----------------------------


def test_funding_input_lets_the_identity_output_grow():
    """A top-up: output 0 ends up worth MORE than it was, which the unfunded
    builder could never do."""
    prior = _funded_prior(sat_value=754)
    new_snap = dict(prior["snapshot"], life=2)

    shrunk, sp = cw.build_rekey_psbt(prior_proof=prior, new_snapshot=new_snap,
                                     fee_rate=1)
    assert sp["sat_value"] < 754, "unfunded state updates shrink — that's the bug"

    grown, gp = cw.build_rekey_psbt(prior_proof=prior, new_snapshot=new_snap,
                                    fee_rate=4, funding_inputs=[_fund(20_000)])
    assert gp["sat_value"] > 754
    assert grown.tx.vout[0].value == gp["sat_value"]
    assert gp["topped_up_by"] == gp["sat_value"] - 754 > 0
    # Value is conserved: inputs - outputs = fee, and the fee is positive.
    fee = (754 + 20_000) - sum(o.value for o in grown.tx.vout)
    assert fee > 0


def test_sat_target_sizes_the_identity_output_and_returns_change():
    prior = _funded_prior(sat_value=754)
    new_snap = dict(prior["snapshot"], life=2)
    change_spk = bytes([0x51, 0x20]) + b"\x33" * 32
    p, proof = cw.build_rekey_psbt(
        prior_proof=prior, new_snapshot=new_snap, fee_rate=2,
        funding_inputs=[_fund(50_000)], sat_target=10_000,
        change_script_pubkey=change_spk,
    )
    assert p.tx.vout[0].value == 10_000
    assert proof["sat_value"] == 10_000
    # Change is LAST, never ahead of the sat-carrying output 0.
    assert p.tx.vout[-1].script_pubkey.data == change_spk
    assert p.tx.vout[-1].value == proof["change_value"] > 0
    assert (50_000 + 754) - sum(o.value for o in p.tx.vout) > 0   # positive fee
    # And the sat still lands in output 0.
    assert _index_to_sont(0, [o.value for o in p.tx.vout]) == (0, 0)


def test_dust_change_folds_into_the_identity_output():
    prior = _funded_prior(sat_value=754)
    new_snap = dict(prior["snapshot"], life=2)
    fund = _fund(5_000)
    # 754 + 5000 in; a 5_400 target leaves well under the 330-sat dust floor,
    # so the remainder folds into output 0 rather than becoming an unspendable
    # change output — and output 0 ends up ABOVE the target, never below it.
    p, proof = cw.build_rekey_psbt(
        prior_proof=prior, new_snapshot=new_snap, fee_rate=1,
        funding_inputs=[fund], sat_target=5_400,
        change_script_pubkey=bytes([0x51, 0x20]) + b"\x33" * 32,
    )
    assert "change_value" not in proof
    assert len(p.tx.vout) == 1
    assert p.tx.vout[0].value > 5_400


def test_unaffordable_sat_target_is_refused_not_silently_shrunk():
    """A target the inputs cannot cover must fail loudly. Quietly building a
    SMALLER output than asked for is how an identity sat decays by surprise."""
    prior = _funded_prior(sat_value=754)
    with pytest.raises(RuntimeError, match="more than the inputs can pay for"):
        cw.build_rekey_psbt(
            prior_proof=prior, new_snapshot=dict(prior["snapshot"], life=2),
            fee_rate=1, funding_inputs=[_fund(5_000)], sat_target=5_700,
            change_script_pubkey=bytes([0x51, 0x20]) + b"\x33" * 32,
        )


# --- ordering must not be constructible by accident ------------------------


def test_builder_always_puts_the_identity_at_input_zero():
    prior = _funded_prior(sat_value=754)
    new_snap = dict(prior["snapshot"], life=2)
    p, _ = cw.build_rekey_psbt(prior_proof=prior, new_snapshot=new_snap,
                               fee_rate=4,
                               funding_inputs=[_fund(20_000, txid=_FUND_TXID_A),
                                               _fund(9_000, txid=_FUND_TXID_B)])
    assert len(p.tx.vin) == 3
    assert p.tx.vin[0].txid.hex() == prior["commit_txid"]
    assert p.tx.vin[0].vout == prior["sat_vout"]
    # Funding follows, in the order given.
    assert [v.txid.hex() for v in p.tx.vin[1:]] == [_FUND_TXID_A, _FUND_TXID_B]


def test_assert_identity_input_zero_rejects_funding_placed_first():
    """A funding input ahead of the identity must be refused, not merely
    discouraged: the verifier would read the wrong input as the ownership
    proof and +index-to-sont would land the sat in the wrong place."""
    from embit.transaction import (
        Transaction as _Tx, TransactionInput as _TxIn, TransactionOutput as _TxOut)
    prior = _funded_prior(sat_value=754)
    ident = _TxIn(bytes.fromhex(prior["commit_txid"]), 0, sequence=0xFFFFFFFF)
    fund = _TxIn(bytes.fromhex(_FUND_TXID_A), 1, sequence=0xFFFFFFFF)
    out = _TxOut(20_000, cw._script_from_spk(bytes([0x51, 0x20]) + b"\x22" * 32))

    good = _Tx(version=2, vin=[ident, fund], vout=[out], locktime=0)
    cw.assert_identity_input_zero(good, prior["commit_txid"], 0)   # no raise

    bad = _Tx(version=2, vin=[fund, ident], vout=[out], locktime=0)
    with pytest.raises(ValueError, match="input 0 must be the identity satpoint"):
        cw.assert_identity_input_zero(bad, prior["commit_txid"], 0)

    with pytest.raises(ValueError, match="no inputs"):
        cw.assert_identity_input_zero(
            _Tx(version=2, vin=[], vout=[out], locktime=0), prior["commit_txid"], 0)


def test_rekey_refuses_a_funding_input_that_is_the_identity_sat():
    prior = _funded_prior(sat_value=754)
    new_snap = dict(prior["snapshot"], life=2)
    dup = _fund(20_000, txid=prior["commit_txid"], vout=prior["sat_vout"])
    with pytest.raises(ValueError, match="already an input"):
        cw.build_rekey_psbt(prior_proof=prior, new_snapshot=new_snap,
                            funding_inputs=[dup])


def test_rekey_refuses_duplicate_funding_inputs():
    prior = _funded_prior(sat_value=754)
    new_snap = dict(prior["snapshot"], life=2)
    with pytest.raises(ValueError, match="already an input"):
        cw.build_rekey_psbt(prior_proof=prior, new_snapshot=new_snap,
                            funding_inputs=[_fund(20_000), _fund(20_000)])


def test_rekey_refuses_to_shrink_the_output_below_the_sat_offset():
    """Output 0 smaller than the tracked offset would silently move the
    identity out of output 0 — refuse rather than build it."""
    prior = _funded_prior(sat_value=5_000, sat_off=4_000)
    new_snap = dict(prior["snapshot"], life=2)
    with pytest.raises(RuntimeError, match="offset"):
        cw.build_rekey_psbt(prior_proof=prior, new_snapshot=new_snap, fee_rate=2,
                            funding_inputs=[_fund(20_000)], sat_target=1_000,
                            change_script_pubkey=bytes([0x51, 0x20]) + b"\x33" * 32)


def test_sat_target_without_a_change_output_is_refused():
    prior = _funded_prior(sat_value=754)
    new_snap = dict(prior["snapshot"], life=2)
    with pytest.raises(ValueError, match="no change_script_pubkey"):
        cw.build_rekey_psbt(prior_proof=prior, new_snapshot=new_snap, fee_rate=2,
                            funding_inputs=[_fund(50_000)], sat_target=1_000)


# --- PSBT metadata for the funding input -----------------------------------


def test_funding_input_carries_signable_psbt_metadata():
    prior = _funded_prior(sat_value=754)
    new_snap = dict(prior["snapshot"], life=2)
    fund = _fund(20_000)
    p, _ = cw.build_rekey_psbt(prior_proof=prior, new_snapshot=new_snap,
                               fee_rate=4, funding_inputs=[fund])
    fin = p.inputs[1]
    assert fin.witness_utxo.value == 20_000
    assert fin.witness_utxo.script_pubkey.data == fund["script_pubkey"]
    assert fin.taproot_internal_key is not None
    assert len(fin.taproot_bip32_derivations) == 1
    # A plain BIP-86 funding UTXO commits to no script tree: setting a merkle
    # root here would make the signer compute the wrong tweak.
    assert fin.taproot_merkle_root is None
    # Input 0 still carries the state leaf hash it has always carried.
    assert p.inputs[0].taproot_merkle_root == bytes.fromhex(prior["leaf_hash_hex"])
    # Survives a serialization round trip.
    from embit import psbt as _p
    restored = _p.PSBT.from_base64(p.to_base64())
    assert restored.inputs[1].witness_utxo.value == 20_000
    assert len(restored.tx.vin) == 2


def test_normalize_funding_input_accepts_scan_addresses_shape():
    """scan_addresses() emits `scriptpubkey`/`xonly`/`path`; the builder must
    take that dict without the caller renaming anything."""
    from embit import ec as _ec
    xonly = _ec.PrivateKey(b"\x77" * 32).get_public_key().xonly()
    n = cw.normalize_funding_input({
        "txid": _FUND_TXID_A.upper(), "vout": 2, "value": 9_000,
        "scriptpubkey": bytes([0x51, 0x20]) + xonly,
        "xonly": xonly, "path": "m/86h/0h/0h/1/4",
    })
    assert n["txid"] == _FUND_TXID_A       # normalized to lowercase
    assert n["fingerprint"] == b"\x00\x00\x00\x00"
    assert n["path"] == "m/86h/0h/0h/1/4"
    # hex strings work too
    assert cw.normalize_funding_input(
        {"txid": _FUND_TXID_A, "vout": 0, "value": 1,
         "script_pubkey": (bytes([0x51, 0x20]) + xonly).hex(),
         "xonly": xonly.hex()})["xonly"] == xonly


@pytest.mark.parametrize("bad,match", [
    ({"txid": "zz" * 32, "vout": 0, "value": 1}, "64 hex"),
    ({"txid": _FUND_TXID_A, "vout": 0, "value": 0}, "value must be positive"),
    ({"txid": _FUND_TXID_A, "vout": -1, "value": 1}, "vout must be"),
])
def test_normalize_funding_input_rejects_malformed_specs(bad, match):
    with pytest.raises(ValueError, match=match):
        cw.normalize_funding_input(bad)


# --- the unfunded path must be untouched -----------------------------------


def test_unfunded_rekey_is_byte_identical_to_the_old_builder():
    """Regression guard: adding funding must not perturb an ordinary rekey.
    111 vB * fee_rate, one input, one output worth prior - fee."""
    prior = _funded_prior(sat_value=2_000)
    new_snap = dict(prior["snapshot"], life=2)
    p, proof = cw.build_rekey_psbt(prior_proof=prior, new_snapshot=new_snap,
                                   fee_rate=3)
    assert len(p.tx.vin) == 1
    assert len(p.tx.vout) == 1
    assert proof["sat_value"] == 2_000 - 111 * 3
    assert "funding_inputs" not in proof and "topped_up_by" not in proof


def test_unfunded_rekey_still_refuses_an_undersized_sat():
    prior = _funded_prior(sat_value=754)
    new_snap = dict(prior["snapshot"], life=2)
    with pytest.raises(RuntimeError, match="too small"):
        cw.build_rekey_psbt(prior_proof=prior, new_snapshot=new_snap, fee_rate=10)


def test_funding_rescues_a_sat_the_fee_rate_would_have_stranded():
    """The whole point, stated as a test: k1's 754 sats cannot pay a 10 sat/vB
    state update, and a funding input makes the same update buildable."""
    prior = _funded_prior(sat_value=754)
    new_snap = dict(prior["snapshot"], life=2)
    with pytest.raises(RuntimeError):
        cw.build_rekey_psbt(prior_proof=prior, new_snapshot=new_snap, fee_rate=10)
    p, proof = cw.build_rekey_psbt(prior_proof=prior, new_snapshot=new_snap,
                                   fee_rate=10, funding_inputs=[_fund(20_000)])
    assert proof["sat_value"] > 754
    assert p.tx.vin[0].txid.hex() == prior["commit_txid"]


# --- CLI plumbing -----------------------------------------------------------


def test_rekey_funding_flags_reach_the_builder(tmp_path, monkeypatch):
    seen = {}

    def capture(prior_proof, new_snapshot, fee_rate, network, **kw):
        seen.update(kw)
        raise RuntimeError("stop after the builder call")

    monkeypatch.setattr(cw, "build_rekey_psbt", capture)
    monkeypatch.setattr(cw, "_resolve_rekey_funding",
                        lambda **kw: {"funding_inputs": [_fund(20_000)]})
    with pytest.raises(RuntimeError):
        cw._run_rekey_op("~sampel-palnet", _rekey_prior(tmp_path, sponsor=1234), 9,
                         False, 2, "main", str(tmp_path), "stub",
                         fund_xpub="tr(xpub)", sat_target=None)
    assert seen["funding_inputs"][0]["value"] == 20_000


def test_rekey_without_funding_calls_the_builder_exactly_as_before(tmp_path, monkeypatch):
    seen = {}

    def capture(prior_proof, new_snapshot, fee_rate, network, **kw):
        seen["extra"] = kw
        raise RuntimeError("stop")

    monkeypatch.setattr(cw, "build_rekey_psbt", capture)
    with pytest.raises(RuntimeError):
        cw._run_rekey_op("~sampel-palnet", _rekey_prior(tmp_path, sponsor=1234), 9,
                         False, 2, "main", str(tmp_path), "stub")
    assert seen["extra"] == {}, "an unfunded rekey must pass no funding kwargs"


def test_sat_target_without_fund_xpub_is_refused():
    with pytest.raises(SystemExit, match="only means something with a funding input"):
        cw._resolve_rekey_funding(fund_xpub=None, fund_utxo=None, sat_target=5_000,
                                  network="main", mempool_base="stub")


def test_fund_utxo_without_fund_xpub_is_refused():
    with pytest.raises(SystemExit, match="needs --fund-xpub"):
        cw._resolve_rekey_funding(fund_xpub=None, fund_utxo="ab" * 32 + ":0",
                                  sat_target=None, network="main", mempool_base="stub")


def test_no_funding_flags_means_no_builder_kwargs():
    assert cw._resolve_rekey_funding(fund_xpub=None, fund_utxo=None, sat_target=None,
                                     network="main", mempool_base="stub") == {}


def test_rekey_cli_exposes_the_funding_flags():
    names = {o.name for o in cw.cmd_rekey.params}
    assert {"fund_xpub", "fund_utxo", "sat_target"} <= names


# ---------------------------------------------------------------------------
# The ordinal claim from first principles.
#
# +index-to-sont is fed the sat's offset within INPUT 0's prevout and treats it
# as the transaction-wide ordinal index (self-attestation.hoon:695). That is a
# shortcut, and it is only sound while input 0 is first. The simulator below
# does not take the shortcut: it numbers every sat the transaction consumes,
# input by input, then hands them to the outputs in order — the actual ordinals
# assignment rule — and reports where a chosen sat ends up.
# ---------------------------------------------------------------------------


def _ordinal_landing(in_values: list[int], out_values: list[int],
                     in_index: int, in_offset: int):
    """Where does the sat at `in_offset` within input `in_index` land?

    Returns (vout, offset), or None if it fell into the miner fee.
    """
    # Sats are consumed in input order, so the tracked sat's transaction-wide
    # index is everything ahead of its own input plus its offset within it.
    global_index = sum(in_values[:in_index]) + in_offset
    # ...and handed out in output order.
    for vout, value in enumerate(out_values):
        if global_index < value:
            return (vout, global_index)
        global_index -= value
    return None


def test_ordinal_simulator_agrees_with_the_index_to_sont_mirror():
    """With the identity at input 0 the two must agree — that agreement IS the
    shortcut the verifier relies on."""
    import random
    rng = random.Random(20260806)
    for _ in range(500):
        ins = [rng.randint(1, 50_000) for _ in range(rng.randint(1, 4))]
        outs = [rng.randint(0, 40_000) for _ in range(rng.randint(1, 4))]
        off = rng.randrange(ins[0])
        assert _ordinal_landing(ins, outs, 0, off) == _index_to_sont(off, outs)


def test_an_input_after_input_zero_cannot_move_the_tracked_sat():
    """THE claim, stated directly: appending inputs behind the identity leaves
    the tracked sat's landing bit-for-bit unchanged."""
    import random
    rng = random.Random(6408)
    for _ in range(500):
        identity_value = rng.randint(400, 20_000)
        off = rng.randrange(identity_value)
        outs = [rng.randint(330, 60_000) for _ in range(rng.randint(1, 3))]

        alone = _ordinal_landing([identity_value], outs, 0, off)
        trailing = rng.sample([rng.randint(1, 100_000) for _ in range(3)],
                              rng.randint(1, 3))
        with_funding = _ordinal_landing([identity_value] + trailing, outs, 0, off)
        assert alone == with_funding

    # A trailing input can only ADD sats behind the tracked one, so it can also
    # only ever rescue a sat from the fee, never push one into it.
    assert _ordinal_landing([1_000], [500], 0, 700) is None
    assert _ordinal_landing([1_000, 9_000], [9_500], 0, 700) == (0, 700)


def test_an_input_before_input_zero_does_move_the_tracked_sat():
    """The mirror image, and the reason input 0 is not negotiable: a leading
    input shifts the tracked sat by its entire value, so the verifier's
    shortcut lands it somewhere it is not."""
    import random
    rng = random.Random(1189)
    moved = 0
    for _ in range(500):
        identity_value = rng.randint(400, 20_000)
        off = rng.randrange(identity_value)
        outs = [rng.randint(330, 60_000) for _ in range(rng.randint(1, 3))]
        leading = rng.randint(1, 100_000)

        truth = _ordinal_landing([leading, identity_value], outs, 1, off)
        believed = _index_to_sont(off, outs)   # what the verifier would compute
        if truth != believed:
            moved += 1
    assert moved > 400, ("a leading input must generally displace the sat; "
                         f"only {moved}/500 diverged")


def test_the_funded_builder_lands_the_sat_where_the_simulator_says():
    """End to end: build a real funded state update and check the sat's landing
    against the from-first-principles simulator, not just the mirror."""
    for sat_off in (0, 1, 400, 4_000):
        prior = _funded_prior(sat_value=5_000, sat_off=sat_off)
        new_snap = dict(prior["snapshot"], life=2)
        fund = _fund(20_000)
        p, proof = cw.build_rekey_psbt(prior_proof=prior, new_snapshot=new_snap,
                                       fee_rate=4, funding_inputs=[fund])
        in_values = [5_000, 20_000]
        out_values = [o.value for o in p.tx.vout]
        assert _ordinal_landing(in_values, out_values, 0, sat_off) == (0, sat_off)
        # ...and the verifier's own shortcut reaches the same answer.
        assert _verify_hop([5_000], (prior["commit_txid"], 0, sat_off),
                           p.tx)["next"][1:] == (0, sat_off)


def test_verifier_mirror_rejects_a_state_update_with_funding_placed_first():
    """Closing the loop: if such a tx were ever built, the verifier's own
    continuity check is what would reject it."""
    from embit.transaction import (
        Transaction as _Tx, TransactionInput as _TxIn, TransactionOutput as _TxOut)
    ident = _TxIn(bytes.fromhex(_IDENTITY_TXID), 0, sequence=0xFFFFFFFF)
    fund = _TxIn(bytes.fromhex(_FUND_TXID_A), 1, sequence=0xFFFFFFFF)
    out = _TxOut(20_000, cw._script_from_spk(bytes([0x51, 0x20]) + b"\x22" * 32))
    current = (_IDENTITY_TXID, 0, 0)

    ok = _verify_hop([5_000], current,
                     _Tx(version=2, vin=[ident, fund], vout=[out], locktime=0))
    assert ok["input-zero"] and ok["continuity"] and ok["sat-landed"]

    bad = _verify_hop([5_000], current,
                      _Tx(version=2, vin=[fund, ident], vout=[out], locktime=0))
    assert bad["input-zero"] is True      # there IS an input 0...
    assert bad["continuity"] is False     # ...it just is not the identity


# ---------------------------------------------------------------------------
# ops/gwmint.py imports causeway verbatim rather than duplicating encoders, so
# the funding fix must reach it without a second implementation.
# ---------------------------------------------------------------------------


def _gwmint():
    import importlib
    ops = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", "..", "ops"))
    if not os.path.isfile(os.path.join(ops, "gwmint.py")):
        pytest.skip("ops/gwmint.py not found")
    sys.path.insert(0, ops)
    try:
        return importlib.import_module("gwmint")
    except ImportError as e:
        pytest.skip(f"gwmint deps unavailable: {e}")


def test_gwmint_uses_the_same_causeway_builder():
    """No second implementation: gwmint must be calling THIS build_rekey_psbt."""
    g = _gwmint()
    assert g.C.build_rekey_psbt is cw.build_rekey_psbt
    assert g.C.normalize_funding_input is cw.normalize_funding_input
    assert g.C.assert_identity_input_zero is cw.assert_identity_input_zero


def test_gwmint_publish_exposes_funding():
    import inspect
    g = _gwmint()
    params = inspect.signature(g.cmd_publish).parameters
    assert "fund" in params and "sat_target" in params
    assert params["fund"].default is False, "funding must be opt-in"


def test_gwmint_find_funding_utxo_returns_a_builder_ready_spec(monkeypatch):
    """Whatever gwmint picks must drop straight into build_rekey_psbt, and it
    must never hand back the identity sat itself as 'funding'."""
    g = _gwmint()
    from embit import ec as _ec
    xonly = _ec.PrivateKey(b"\x55" * 32).get_public_key().xonly()
    w = {"addr": "bc1p-stub", "spk": bytes([0x51, 0x20]) + xonly,
         "xonly": xonly, "fpr": b"\xaa\xbb\xcc\xdd"}

    utxos = [
        {"txid": _IDENTITY_TXID, "vout": 0, "value": 999_999,
         "status": {"confirmed": True}},                       # the identity sat
        {"txid": _FUND_TXID_A, "vout": 1, "value": 20_000,
         "status": {"confirmed": True}},
        {"txid": _FUND_TXID_B, "vout": 0, "value": 90_000,
         "status": {"confirmed": False}},                      # unconfirmed
    ]

    class _R:
        @staticmethod
        def json():
            return utxos

    monkeypatch.setattr(g.requests, "get", lambda *a, **k: _R())
    fi = g.find_funding_utxo(w, exclude=[(_IDENTITY_TXID, 0)])
    assert fi["txid"] == _FUND_TXID_A, "must skip the identity sat and unconfirmed"
    assert cw.normalize_funding_input(fi)["value"] == 20_000

    # And it really builds a topped-up state update through the shared builder.
    prior = _funded_prior(sat_value=754)
    p, proof = cw.build_rekey_psbt(
        prior_proof=prior, new_snapshot=dict(prior["snapshot"], life=2),
        fee_rate=4, funding_inputs=[fi])
    assert len(p.tx.vin) == 2
    assert p.tx.vin[0].txid.hex() == prior["commit_txid"]
    assert proof["sat_value"] > 754


def test_gwmint_find_funding_utxo_returns_none_when_nothing_is_spendable(monkeypatch):
    g = _gwmint()

    class _R:
        @staticmethod
        def json():
            return [{"txid": _FUND_TXID_A, "vout": 0, "value": 100,
                     "status": {"confirmed": True}}]     # below min_value

    monkeypatch.setattr(g.requests, "get", lambda *a, **k: _R())
    assert g.find_funding_utxo({"addr": "x", "spk": b"", "xonly": b"",
                                "fpr": b""}) is None


# ---------------------------------------------------------------------------
# A top-up nobody can sign is not a fix.  Sign a real two-input state update
# with a real HD root and check the schnorr signatures against the actual
# taproot sighashes.  Nothing here touches the network.
# ---------------------------------------------------------------------------


def _signable_rekey_fixture(sat_value=754, fund_value=20_000):
    from embit import bip32, script as _script
    root = bip32.HDKey.from_seed(b"\x01" * 32)
    fpr = cw.hdkey_fingerprint(root)
    ident_path, fund_path = "m/86h/0h/0h/0/0", "m/86h/0h/0h/0/7"
    ik = root.derive(ident_path).key.get_public_key().xonly()
    fund_pk = root.derive(fund_path).key.get_public_key()

    snap = {"life": 1, "rift": 0, "key": 0xABCD, "sponsor": None, "fief": None}
    q = cw.state_output_key(ik, snap)
    lh = cw.state_leaf_hash(cw.state_leaf_script(cw.state_commit(snap)))
    prior = {
        "version": 2, "protocol": "kelvin-9", "sat_vout": 0,
        "internal_pubkey_hex": ik.hex(), "snapshot": snap,
        "leaf_hash_hex": lh.hex(),
        "sat_script_pubkey_hex": (bytes([0x51, 0x20]) + q).hex(),
        "sat_value": sat_value, "commit_txid": _IDENTITY_TXID, "network": "main",
        "funding": {"txid": "00" * 32, "vout": 0, "value": sat_value,
                    "path": ident_path, "fingerprint_hex": fpr.hex()},
    }
    fund = {
        "txid": _FUND_TXID_A, "vout": 1, "value": fund_value,
        "script_pubkey": _script.p2tr(fund_pk).data,
        "xonly": fund_pk.xonly(), "path": fund_path, "fingerprint": fpr,
    }
    return root, prior, fund


def test_funded_state_update_signs_and_extracts():
    root, prior, fund = _signable_rekey_fixture()
    p, proof = cw.build_rekey_psbt(
        prior_proof=prior, new_snapshot=dict(prior["snapshot"], life=2, key=0x9999),
        fee_rate=4, funding_inputs=[fund])

    assert p.sign_with(root) == 2, "both the identity and the funding input must sign"
    txid, tx_hex = cw._extract_tx_from_psbt(p.to_base64())
    assert len(txid) == 64
    assert proof["sat_value"] > prior["sat_value"], "and it is a real top-up"


def test_both_input_signatures_verify_against_the_taproot_sighashes():
    """The identity input is a key-path spend of the STATE-TWEAKED key; the
    funding input is a plain BIP-86 spend.  Two different tweaks, both of which
    have to come out right or the top-up is unspendable."""
    from embit import ec as _ec
    root, prior, fund = _signable_rekey_fixture()
    p, _ = cw.build_rekey_psbt(
        prior_proof=prior, new_snapshot=dict(prior["snapshot"], life=2, key=0x9999),
        fee_rate=4, funding_inputs=[fund])
    p.sign_with(root)
    _txid, tx_hex = cw._extract_tx_from_psbt(p.to_base64())

    from embit.transaction import Transaction
    tx = Transaction.parse(bytes.fromhex(tx_hex))
    prevouts = [p.inputs[0].witness_utxo, p.inputs[1].witness_utxo]
    spks = [o.script_pubkey.data for o in prevouts]
    values = [o.value for o in prevouts]
    for i in (0, 1):
        sig = _ec.SchnorrSig.parse(tx.vin[i].witness.items[0][:64])
        h = tx.sighash_taproot(i, [o.script_pubkey for o in prevouts], values)
        # The signature must verify against the on-chain output key, i.e. the
        # tweaked key the scriptPubKey actually commits to.
        pk = _ec.PublicKey.from_xonly(spks[i][2:])
        assert pk.schnorr_verify(sig, h), f"input {i} signature does not verify"

    # Input 0's output key is the STATE key: internal key tweaked by the
    # current snapshot's leaf hash.  Input 1's is a plain BIP-86 key.
    assert spks[0][2:].hex() == prior["sat_script_pubkey_hex"][4:]
    assert spks[1] == fund["script_pubkey"]


def test_funded_state_update_fee_estimate_tracks_the_real_vsize():
    """A top-up that underpays its own estimate would sit unconfirmed — the
    thing we are trying to escape.  Check the built fee against actual vsize."""
    root, prior, fund = _signable_rekey_fixture()
    fee_rate = 4
    p, proof = cw.build_rekey_psbt(
        prior_proof=prior, new_snapshot=dict(prior["snapshot"], life=2, key=0x9999),
        fee_rate=fee_rate, funding_inputs=[fund])
    p.sign_with(root)
    _txid, tx_hex = cw._extract_tx_from_psbt(p.to_base64())

    raw = bytes.fromhex(tx_hex)
    from embit.transaction import Transaction
    tx = Transaction.parse(raw)
    stripped = bytearray()
    stripped += tx.version.to_bytes(4, "little")
    stripped += bytes([len(tx.vin)])
    for v in tx.vin:
        stripped += v.txid[::-1] + v.vout.to_bytes(4, "little") + b"\x00"
        stripped += v.sequence.to_bytes(4, "little")
    stripped += bytes([len(tx.vout)])
    for o in tx.vout:
        spk = o.script_pubkey.data
        stripped += o.value.to_bytes(8, "little") + bytes([len(spk)]) + spk
    stripped += tx.locktime.to_bytes(4, "little")
    vsize = (len(stripped) * 3 + len(raw) + 3) // 4

    fee = (prior["sat_value"] + fund["value"]) - sum(o.value for o in tx.vout)
    effective = fee / vsize
    assert effective >= fee_rate * 0.95, (
        f"underpaid: {fee} sat over {vsize} vB = {effective:.2f} sat/vB, "
        f"asked for {fee_rate}")
    assert effective <= fee_rate * 1.15, (
        f"overpaid: {effective:.2f} sat/vB for a requested {fee_rate}")


# ---------------------------------------------------------------------------
# publish — the on-chain declassification, and the four guards on it
#
# A publication's payload is the comet's WHOLE attestation packet: the pass a
# peer would receive over ames, custody log in its xtr, plus the opening for the
# hop the transaction itself performs. The failure mode these tests exist for is
# invisible: publishing the BOOT pass instead produces the same OP_RETURN
# envelope and the same opening, ~200 bytes shorter, and the difference only
# surfaces when a stranger declines to declassify the comet.
# ---------------------------------------------------------------------------

from click.testing import CliRunner  # noqa: E402

_PUB_PASS = int(FULL["pass_empty"], 16)          # a real 108-byte suite-C pass


def _pub_log(n_bare: int = 0) -> list:
    """Entry 0 (spawn, with the dat opening) plus `n_bare` plain custody hops."""
    log = [_full_log()[0]]
    for i in range(n_bare):
        log.append({"txid_hex": f"{i + 0xA0:02x}" * 32, "height": 961_100 + i,
                    "opening": None})
    return log


def _publish_chain(tmp_path, log, *, n_proofs=None, xtr=None, key=None) -> list:
    """A FINALIZED proof chain: one file per hop, the last carrying the baked
    `xtr_hex`, and every `commit_txid` taken from the log — so the log ends
    exactly where the publication's input 0 begins."""
    n = len(log) if n_proofs is None else n_proofs
    snap = {"life": 1, "rift": 0,
            "key": cw.messaging_key_from_pass(_PUB_PASS) if key is None else key,
            "sponsor": cw.patp_to_int(SPONSOR_PATP), "fief": None}
    _psbt_obj, base = cw.build_spawn_psbt(**_fake_utxo(), snapshot=snap)
    paths = []
    for i in range(n):
        proof = dict(base)
        proof["op"] = "spawn" if i == 0 else "rekey"
        proof["patp"] = "~sampel-palnet"
        #  the LAST proof always names the log's last hop, so guard 1 is
        #  satisfied by construction and the other guards can be reached
        proof["commit_txid"] = (log[-1]["txid_hex"] if i == n - 1
                                else log[i]["txid_hex"] if i < len(log)
                                else "ff" * 32)
        if i == 0:
            proof["pass_atom_hex"] = hex(_PUB_PASS)
        if i == n - 1:
            proof["xtr_hex"] = hex(cw.build_xtr_atom(log) if xtr is None else xtr)
        path = tmp_path / f"{i:02d}.proof.json"
        cw.write_proof_json(proof, str(path))
        paths.append(str(path))
    return paths


def _publish(tmp_path, paths, *extra):
    return CliRunner().invoke(
        cw.cli, ["publish", *paths, "--dry-run", "--output-dir", str(tmp_path), *extra])


def _op_return_of(result_dir, tmp_path) -> bytes:
    psbt_files = [p for p in os.listdir(str(tmp_path)) if p.endswith(".psbt")]
    assert psbt_files, "publish wrote no PSBT"
    from embit import psbt as _p
    p = _p.PSBT.from_base64(open(os.path.join(str(tmp_path), psbt_files[0])).read().strip())
    outs = [bytes(o.script_pubkey.data) for o in p.tx.vout]
    pubs = [s for s in outs if cw.parse_publication_script(s) is not None]
    assert len(pubs) == 1
    return pubs[0]


# -- the codec inverses the guards are built out of --------------------------

def test_xtr_of_pass_inverts_pass_with_xtr():
    """Pinned on the golden full-packet vector: the xtr comes back out of the
    500-byte pass exactly as it went in, and a boot pass answers 0."""
    xtr = cw.build_xtr_atom(_full_log())
    assert cw.xtr_of_pass(cw.pass_with_xtr(_PUB_PASS, xtr)) == xtr
    assert cw.xtr_of_pass(_PUB_PASS) == 0
    assert cw.xtr_of_pass(int(FULL["pass_full"], 16)) == xtr


def test_xtr_of_pass_refuses_a_pass_that_is_not_suite_c():
    with pytest.raises(ValueError, match="not a suite-C pass"):
        cw.xtr_of_pass(0xDEAD_BE00)


def test_parse_publication_script_round_trips_the_golden_script():
    script = bytes.fromhex(FULL["op_return_script"])
    kelvin, payload = cw.parse_publication_script(script)
    assert kelvin == cw.KELVIN == 9
    assert payload.hex() == FULL["jam_publication_le"]
    pass_atom, _opening = cw.read_publication_script(script)
    assert pass_atom == int(FULL["pass_full"], 16)


@pytest.mark.parametrize("n", [3, 75, 76, 255, 256, 588])
def test_parse_publication_reads_every_push_form_push_data_emits(n):
    """Direct push, PUSHDATA1 and PUSHDATA2 all come back out."""
    body = bytes([0x6A, 0x03, 0x75, 0x72, 0x62, 0x01, cw.KELVIN])
    payload = bytes(range(256)) * 4
    payload = payload[:n]
    got = cw.parse_publication_script(body + cw.push_data(payload) + payload)
    assert got == (cw.KELVIN, payload)


def test_parse_publication_refuses_what_is_not_a_publication():
    assert cw.parse_publication_script(b"\x6a\x20" + b"\xab" * 32) is None   # a plain OP_RETURN
    assert cw.parse_publication_script(bytes([0x51, 0x20]) + b"\xab" * 32) is None  # P2TR
    assert cw.parse_publication_script(b"") is None
    # a length that does not match the payload actually present
    assert cw.parse_publication_script(
        bytes([0x6A, 0x03, 0x75, 0x72, 0x62, 0x01, cw.KELVIN, 0x20]) + b"\xab" * 4) is None


def test_read_publication_refuses_a_foreign_kelvin():
    """A watcher at another protocol version ignores the output rather than
    mis-parsing it — parse_publication still reports the version it saw."""
    script = bytearray(bytes.fromhex(FULL["op_return_script"]))
    script[6] = cw.KELVIN + 1
    assert cw.parse_publication_script(bytes(script))[0] == cw.KELVIN + 1
    assert cw.read_publication_script(bytes(script)) is None


def test_cue_custody_log_inverts_build_xtr_atom():
    log = _full_log()
    got = cw.cue_custody_log(cw.build_xtr_atom(log))
    assert [e["txid_hex"] for e in got] == [e["txid_hex"] for e in log]
    assert [e["height"] for e in got] == [e["height"] for e in log]
    # exactly one entry — entry 0, the spawn — carries an opening
    assert [e["opening"] is not None for e in got] == [True, False, False, False, False, False]
    assert cw.cue_custody_log(0) == []


# -- GUARD 4: the payload is read back OUT of the script ---------------------

def test_recue_guard_rejects_a_boot_pass_publication():
    """THE ONE THAT MATTERS. A boot-pass publication is indistinguishable from a
    packet publication in a transaction decode; it is the shape that put four
    unverifiable publications on mainnet, and it is caught here."""
    opening = _full_opening(FULL["terminal_opening"])
    xtr = cw.build_xtr_atom(_full_log())
    boot = cw.make_publication_script(_PUB_PASS, opening)
    packet = cw.make_publication_script(cw.pass_with_xtr(_PUB_PASS, xtr), opening)
    # ...and the two really are near-identical on the wire, which is the point
    assert boot[:7] == packet[:7]
    assert len(packet) - len(boot) == 359   # the 358-byte xtr + one length byte

    with pytest.raises(ValueError, match="carries NO custody log"):
        cw.assert_publication_carries_log(boot, xtr=xtr, entries=6)
    cw.assert_publication_carries_log(packet, xtr=xtr, entries=6)   # must not raise


def test_recue_guard_rejects_a_different_log():
    opening = _full_opening(FULL["terminal_opening"])
    mine = cw.build_xtr_atom(_full_log())
    theirs = cw.build_xtr_atom(_full_log()[:3])
    script = cw.make_publication_script(cw.pass_with_xtr(_PUB_PASS, theirs), opening)
    with pytest.raises(ValueError, match="DIFFERENT custody log"):
        cw.assert_publication_carries_log(script, xtr=mine, entries=6)
    with pytest.raises(ValueError, match="3 entries, expected 6"):
        cw.assert_publication_carries_log(script, xtr=theirs, entries=6)


def test_recue_guard_rejects_a_script_that_is_not_a_publication():
    with pytest.raises(ValueError, match="does not read back"):
        cw.assert_publication_carries_log(bytes([0x51, 0x20]) + b"\xab" * 32, xtr=1, entries=1)


# -- the command ------------------------------------------------------------

def test_publish_dry_run_builds_the_whole_packet(tmp_path):
    log = _pub_log(2)
    paths = _publish_chain(tmp_path, log)
    res = _publish(tmp_path, paths)
    assert res.exit_code == 0, res.output
    assert "3 entries" in res.output and "every gate passed" in res.output

    script = _op_return_of(res, tmp_path)
    pass_atom, opening = cw.read_publication_script(script)
    # the pass carries the log finalize baked, and it is NOT the boot pass
    assert cw.xtr_of_pass(pass_atom) == cw.build_xtr_atom(log)
    assert pass_atom != _PUB_PASS
    # GUARD 2 — the terminal opening's blind-opening unit is ~ (0): the dat
    # opening may sit on entry 0 only, and entry 0 is inside the xtr.
    _internal_key, (_snapshot, spawn_opening_unit) = opening
    assert spawn_opening_unit == 0
    # nothing was signed and nothing was broadcast
    assert not [f for f in os.listdir(str(tmp_path)) if f.endswith(".proof.json")
                and "publish" in f]


def test_publish_state_update_shape(tmp_path):
    """Input 0 is the identity sat, output 0 re-commits the snapshot at life+1,
    and the OP_RETURN rides alongside."""
    log = _pub_log(1)
    paths = _publish_chain(tmp_path, log)
    prior = cw.load_proof_json(paths[-1])
    assert _publish(tmp_path, paths).exit_code == 0

    from embit import psbt as _p
    p = _p.PSBT.from_base64(
        open(str(tmp_path / "sampel-palnet-publish.psbt")).read().strip())
    assert p.tx.vin[0].txid.hex() == prior["commit_txid"]
    assert p.tx.vin[0].vout == prior["sat_vout"]
    new_snap = dict(prior["snapshot"], life=prior["snapshot"]["life"] + 1)
    q = cw.state_output_key(bytes.fromhex(prior["internal_pubkey_hex"]), new_snap)
    assert bytes(p.tx.vout[0].script_pubkey.data) == bytes([0x51, 0x20]) + q
    assert p.tx.vout[1].value == 0            # the OP_RETURN carries no value


# -- GUARD 1: the log must END where this transaction BEGINS ----------------

def test_publish_refuses_a_log_that_does_not_end_at_the_spent_outpoint(tmp_path):
    """If the sat has moved since the log was baked, the artifact is short by
    exactly those hops and the packet fails `N-continuity` on chain — after the
    fee is paid. Refuse instead of paying for it."""
    log = _pub_log(2)
    paths = _publish_chain(tmp_path, log)
    moved = cw.load_proof_json(paths[-1])
    moved["commit_txid"] = "de" * 32          # a hop the log does not know about
    cw.write_proof_json(moved, paths[-1])

    res = _publish(tmp_path, paths)
    assert res.exit_code != 0
    assert "N-continuity" in res.output
    assert "custody log ends at" in res.output
    assert not [f for f in os.listdir(str(tmp_path)) if f.endswith(".psbt")]


def test_publish_refuses_an_entry_count_mismatch(tmp_path):
    """A log baked from a different set of hops than the proofs handed in."""
    log = _pub_log(2)
    paths = _publish_chain(tmp_path, log, n_proofs=2)
    res = _publish(tmp_path, paths)
    assert res.exit_code != 0
    assert "3 entries but 2 proofs" in res.output


def test_publish_requires_finalize_to_have_run(tmp_path):
    log = _pub_log(1)
    paths = _publish_chain(tmp_path, log)
    last = cw.load_proof_json(paths[-1])
    del last["xtr_hex"]
    cw.write_proof_json(last, paths[-1])
    res = _publish(tmp_path, paths)
    assert res.exit_code != 0
    assert "causeway finalize" in res.output


# -- GUARD 3: MAX_PUBLICATION, loudly ---------------------------------------

def test_publish_refuses_an_over_cap_payload(tmp_path):
    """~17 hops fit in 1024 bytes; past that the packet no longer fits in one
    transaction and the command says so rather than emitting a bad script."""
    log = _pub_log(30)
    paths = _publish_chain(tmp_path, log)
    res = _publish(tmp_path, paths)
    assert res.exit_code != 0
    assert f"over the {cw.MAX_PUBLICATION}-byte cap" in res.output
    assert "31 custody hops" in res.output
    assert not [f for f in os.listdir(str(tmp_path)) if f.endswith(".psbt")]


def test_publish_reports_the_headroom_it_has_left(tmp_path):
    paths = _publish_chain(tmp_path, _pub_log(1))
    res = _publish(tmp_path, paths)
    assert re.search(r"Payload\s+: \d+ bytes of 1024", res.output)


# -- the pass must be the comet's CURRENT one -------------------------------

def test_publish_refuses_a_pass_that_is_not_the_current_one(tmp_path):
    """A rekey rotates cry. Publishing a stale pass would advertise a key the
    comet no longer uses — and `pass-key` in +run-checks would catch it, after
    the fee."""
    paths = _publish_chain(tmp_path, _pub_log(1), key=0xC0FFEE)
    res = _publish(tmp_path, paths)
    assert res.exit_code != 0
    assert "not the comet's current pass" in res.output


def test_publish_refuses_a_pass_that_is_not_suite_c(tmp_path):
    paths = _publish_chain(tmp_path, _pub_log(1))
    res = _publish(tmp_path, paths, "--pass-hex", "0xdeadbe00")
    assert res.exit_code != 0
    assert "not a suite-C pass" in res.output


def test_publish_pass_hex_accepts_both_hex_conventions(tmp_path):
    """`0x…` is an atom (the proofs' pass_atom_hex); bare hex is a
    little-endian byte dump (rekey's --new-pass-hex)."""
    paths = _publish_chain(tmp_path, _pub_log(1))
    for path in paths:                      # force --pass-hex to be the source
        proof = cw.load_proof_json(path)
        proof.pop("pass_atom_hex", None)
        cw.write_proof_json(proof, path)
    le_dump = _PUB_PASS.to_bytes((_PUB_PASS.bit_length() + 7) // 8, "little").hex()
    for spec in (hex(_PUB_PASS), le_dump):
        assert _publish(tmp_path, paths, "--pass-hex", spec).exit_code == 0


def test_publish_says_where_to_get_the_pass_when_no_proof_has_one(tmp_path):
    paths = _publish_chain(tmp_path, _pub_log(1))
    for path in paths:
        proof = cw.load_proof_json(path)
        proof.pop("pass_atom_hex", None)
        cw.write_proof_json(proof, path)
    res = _publish(tmp_path, paths)
    assert res.exit_code != 0
    assert "--pass-hex" in res.output


# -- funding: a packet publication may cost more than the identity sat holds --

def test_publish_puts_the_funding_input_behind_the_identity_sat(tmp_path, monkeypatch):
    """Ordinals assign sats in input order, so funding ahead of the identity
    would move it. It goes at input 1, and the identity output GROWS."""
    log = _pub_log(1)
    paths = _publish_chain(tmp_path, log)
    prior = cw.load_proof_json(paths[-1])
    monkeypatch.setattr(cw, "_resolve_rekey_funding", lambda **kw: {
        "funding_inputs": [{
            "txid": "ee" * 32, "vout": 3, "value": 20_000,
            "script_pubkey": bytes([0x51, 0x20]) + b"\x22" * 32,
            "xonly": b"\x22" * 32, "path": "m/86h/0h/0h/0/0",
            "fingerprint": b"\xaa\xbb\xcc\xdd",
        }]})
    res = _publish(tmp_path, paths, "--fund-xpub", "xpub-stub")
    assert res.exit_code == 0, res.output

    from embit import psbt as _p
    p = _p.PSBT.from_base64(
        open(str(tmp_path / "sampel-palnet-publish.psbt")).read().strip())
    assert p.tx.vin[0].txid.hex() == prior["commit_txid"]      # identity FIRST
    assert p.tx.vin[1].txid.hex() == "ee" * 32
    assert p.tx.vout[0].value > prior["sat_value"], "identity sat was not topped up"
    assert "TOP-UP" in res.output


# -- rekey now records the pass it rotates to, so publish can find it --------

def test_rekey_records_the_pass_it_rotates_to(tmp_path, monkeypatch):
    """Without this the comet's current pass lives only in the ship, and
    `publish` — which must publish the pass a peer receives — has nowhere to
    read it from."""
    captured = {}

    def capture(prior_proof, new_snapshot, fee_rate, network):
        captured["snap"] = new_snapshot
        raise RuntimeError("stop before the PSBT")

    monkeypatch.setattr(cw, "build_rekey_psbt", capture)
    with pytest.raises(RuntimeError):
        cw._run_rekey_op("~sampel-palnet", _rekey_prior(tmp_path, sponsor=1234),
                         cw.messaging_key_from_pass(_PUB_PASS), False, 2, "main",
                         str(tmp_path), "stub", new_pass=_PUB_PASS)
    assert captured["snap"]["key"] == cw.messaging_key_from_pass(_PUB_PASS)
    assert cw.messaging_key_from_pass(_PUB_PASS) != _PUB_PASS


# --------------------------------------------------------------------------
# --fief on rekey: the only way a comet acquires routing after it is minted.
#
# A comet with neither fief nor sponsor is unreachable by design AND cannot
# learn a route from packets it receives -- ames gates the heard-lane update
# on the sender not being its own sponsor, and an absent sponsor projects to
# self.  Measured on mainnet: four packets delivered, zero answered, across
# 300s.  So a comet minted without routing had no remedy at all until this
# existed: `--fief` was spawn-only and every state-update path hardcoded
# carry-forward.
# --------------------------------------------------------------------------

def test_parse_fief_arg_matches_gwmints_spawn_encoding():
    """The two tools must emit the SAME noun, or a comet that acquires a fief
    by rekey commits something a verifier reads differently from one that got
    it at spawn."""
    ip_s, port = "159.223.141.63", 35364
    got = cw.parse_fief_arg(f"{ip_s}:{port}")
    # exactly what ops/gwmint.py builds at spawn
    gwmint = (
        int.from_bytes(b"if", "little"),
        (int.from_bytes(bytes(int(x) for x in ip_s.split(".")), "big"), port),
    )
    assert got == gwmint
    # and it survives the normaliser every other path runs it through
    assert cw.fief_noun(got) == got


def test_parse_fief_arg_refuses_malformed():
    """Each of these would otherwise commit a wrong route on chain, which is
    permanent and costs a state update to correct."""
    for bad in ("nope", "1.2.3:5", "1.2.3.999:5", "1.2.3.4:0", "1.2.3.4:70000"):
        with pytest.raises(ValueError):
            cw.parse_fief_arg(bad)


def test_parse_fief_arg_none_is_carry_forward():
    """None means "carry the prior snapshot's fief", not "clear it" -- the
    caller distinguishes, and clearing is what --no-route is for."""
    assert cw.parse_fief_arg(None) is None


# ---------------------------------------------------------------------------
# Fief at spawn
#
# `--fief` was a rekey-only option, and the TUI could not set a fief at all
# (its rekey screen hardcoded carry-forward).  So the only way to give a comet
# a static endpoint was to mint it and then spend a SECOND on-chain
# transaction -- which bit hardest on the one identity that always needs one,
# a sponsor, because peers reach a confidential comet through its sponsor.
#
# gwmint.py has been able to do this since the beginning, and the two encoders
# must agree byte for byte or a comet minted by one is unverifiable to a
# verifier fed by the other.  These pin that agreement at the level that
# matters -- the committed bytes, not the Python objects.
# ---------------------------------------------------------------------------

def _gwmint_fief_noun(s):
    """ops/gwmint.py cmd_build, transcribed verbatim (the other encoder)."""
    if s is None:
        return None
    ip_s, port_s = s.rsplit(":", 1)
    ip = int.from_bytes(bytes(int(x) for x in ip_s.split(".")), "big")
    return (int.from_bytes(b"if", "little"), (ip, int(port_s)))


FIEF_CASES = ["1.2.3.4:1234", "64.227.13.22:34343", "203.0.113.7:65535",
              "255.255.255.255:1", None]

# A fixed 256-bit fixture; the exact value is irrelevant, only that both
# sides build their snapshot from the SAME one.
FIEF_PASS_ATOM = 0x5f2a_9c31_08b7_4e6d_a1c0_33f9_7d21_b845_e094_6a17_c2fb_58d3_0e71_9ab4_26cf_8d50


@pytest.mark.parametrize("spec", FIEF_CASES)
def test_fief_parse_matches_gwmint(spec):
    assert cw.parse_fief_arg(spec) == _gwmint_fief_noun(spec)


@pytest.mark.parametrize("spec", FIEF_CASES)
def test_spawn_snapshot_jam_matches_gwmint(spec):
    """The COMMITTED BYTES agree, not merely the Python tuples."""
    pass_atom = FIEF_PASS_ATOM
    key = cw.messaging_key_from_pass(pass_atom)
    sponsor = 42

    ours = cw._initial_snapshot(pass_atom, sponsor, cw.parse_fief_arg(spec))
    theirs = {"life": 1, "rift": 0, "key": key, "sponsor": sponsor,
              "fief": _gwmint_fief_noun(spec)}
    assert (cw.jam_bytes(cw.snapshot_dict_to_noun(ours))
            == cw.jam_bytes(cw.snapshot_dict_to_noun(theirs)))


def test_fief_actually_reaches_the_commitment():
    """A test that cannot pass if --fief is silently dropped.

    Without this, every assertion above still passes when _initial_snapshot
    ignores its fief argument entirely -- both sides would just be building
    the same fief-less snapshot.
    """
    pass_atom = FIEF_PASS_ATOM
    without = cw.jam_bytes(cw.snapshot_dict_to_noun(
        cw._initial_snapshot(pass_atom, 42, None)))
    with_fief = cw.jam_bytes(cw.snapshot_dict_to_noun(
        cw._initial_snapshot(pass_atom, 42, cw.parse_fief_arg("1.2.3.4:1234"))))
    assert without != with_fief
    assert len(with_fief) > len(without)


def test_fief_at_spawn_satisfies_the_routability_guard():
    """A fief alone makes a comet routable -- no sponsor required."""
    snap = cw._initial_snapshot(1234, None, cw.parse_fief_arg("1.2.3.4:1234"))
    assert cw.snapshot_is_routable(snap)
    cw.assert_routable(snap)          # must not raise

    stranded = cw._initial_snapshot(1234, None, None)
    with pytest.raises(Exception):
        cw.assert_routable(stranded)


@pytest.mark.parametrize("bad", ["1.2.3.4", "1.2.3:80", "1.2.3.4.5:80",
                                 "1.2.3.256:80", "1.2.3.4:0", "1.2.3.4:65536",
                                 "1.2.3.4:http"])
def test_fief_rejects_malformed(bad):
    with pytest.raises(ValueError):
        cw.parse_fief_arg(bad)


# ---------------------------------------------------------------------------
# Printed commands must survive being pasted into a shell
#
# An Urbit @p starts with `~`, which a shell reads as a home directory when it
# leads a word.  bash leaves an unknown `~name` alone; ZSH -- the macOS default
# -- fails outright:
#
#     zsh: no such user or named directory: ligdes-risbur-folmus-mattyp-...
#
# So any command we PRINT for a user to copy has to quote its @p.  This was
# found the only way it could be, by a person pasting one and watching it die.
# ---------------------------------------------------------------------------

import re as _re


def _printed_command_lines():
    """Source lines that print a shell command containing a @p placeholder."""
    src = pathlib.Path(cw.__file__).read_text().splitlines()
    for i, line in enumerate(src, 1):
        if "print(" not in line and "echo" not in line:
            continue
        if _re.search(r"--(comet|point|sponsor)\s+[{~]", line):
            yield i, line.strip()


def test_printed_commands_quote_their_patp():
    bad = []
    for lineno, line in _printed_command_lines():
        # acceptable: '{comet}' or '~sampel' -- the @p sits inside single quotes
        if _re.search(r"--(comet|point|sponsor)\s+'", line):
            continue
        bad.append(f"causeway.py:{lineno}: {line}")
    assert not bad, (
        "these print a command whose @p is unquoted; zsh refuses to run it:\n  "
        + "\n  ".join(bad))


def test_boot_sh_quotes_its_patp():
    """boot.sh prints commands too, and is the thing users copy most."""
    boot = pathlib.Path(cw.__file__).parent.parent / "public" / "boot.sh"
    if not boot.exists():           # not present in a packaged release
        pytest.skip("boot.sh not alongside this checkout")
    bad = []
    for lineno, line in enumerate(boot.read_text().splitlines(), 1):
        m = _re.search(r"--(comet|point|sponsor)\s+(\S+)", line)
        if not m:
            continue
        val = m.group(2)
        if val.startswith("<") or val.startswith("'"):
            continue              # a placeholder, or already quoted
        if val.startswith("~") or val.startswith("$"):
            bad.append(f"boot.sh:{lineno}: {line.strip()}")
    assert not bad, (
        "these print a command whose @p is unquoted; zsh refuses to run it:\n  "
        + "\n  ".join(bad))


# ---------------------------------------------------------------------------
# The TUI mint handoff (boot.sh --mint's default face)
#
# boot.sh launches the TUI with CAUSEWAY_* env vars and judges completion by
# DISK: a proof newer than its launch marker, and a feed file derived from the
# proof's name.  Two sides of a filename contract in two languages -- pin it.
# ---------------------------------------------------------------------------

def test_tui_env_prefill(monkeypatch):
    import causeway_tui as tui
    monkeypatch.setenv("CAUSEWAY_SPONSOR", "~sampel-palnet")
    monkeypatch.setenv("CAUSEWAY_FIEF", "1.2.3.4:5678")
    monkeypatch.setenv("CAUSEWAY_OUTPUT_DIR", "/tmp/mintdir")
    monkeypatch.setenv("CAUSEWAY_HANDOFF", "1")
    st = tui.env_prefill(tui.FlowState())
    assert st.sponsor_input == "~sampel-palnet"
    assert st.fief_input == "1.2.3.4:5678"
    assert st.output_dir == "/tmp/mintdir"
    assert st.handoff is True


def test_tui_feed_filename_matches_boot_sh_expectation():
    """causeway_tui writes splitext(proof)[0] + '.feed'; boot.sh derives
    ${proof%.json}.feed.  If either side changes, the mint dies with
    'wrote a proof but no feed file' -- fail here instead."""
    for proof in ("/x/mint/sampel-palnet-spawn.proof.json",           # CLI spelling
                  "/x/mint/sampel-palnet-spawn-0123456789.proof.json"):  # TUI spelling
        python_side = os.path.splitext(proof)[0] + ".feed"
        bash_side = proof[: -len(".json")] + ".feed"     # ${proof%.json}.feed
        assert python_side == bash_side == proof[:-5] + ".feed"
    boot = pathlib.Path(cw.__file__).parent.parent / "public" / "boot.sh"
    if boot.exists():
        src = boot.read_text()
        assert '${proof%.json}.feed' in src
        # the glob must match BOTH spellings, or a successful TUI spawn is
        # reported as "exited without completing"
        assert "-name '*-spawn*.proof.json'" in src


def test_tui_handoff_never_defaults_on():
    import importlib
    import causeway_tui as tui
    for var in ("CAUSEWAY_SPONSOR", "CAUSEWAY_FIEF", "CAUSEWAY_OUTPUT_DIR", "CAUSEWAY_HANDOFF"):
        os.environ.pop(var, None)
    st = tui.env_prefill(tui.FlowState())
    assert st.handoff is False and st.sponsor_input == ""


def test_qr_ascii_renders_an_address():
    """The funding screen's QR: half-block render, rectangular, deterministic,
    and different inputs give different codes (i.e. the data actually lands
    in the matrix -- a constant block would pass a shape-only check)."""
    a = cw.qr_ascii("bc1p20sp5gnlkfavn8ww7cvd9f2unjlhc9udxpav8q7u3mumewytvahqqmxn5e")
    b = cw.qr_ascii("bc1qdifferentaddressxxxxxxxxxxxxxxxxxxxxx")
    lines = a.split("\n")
    assert 10 < len(lines) < 30
    assert all(len(l) == len(lines[0]) for l in lines)
    assert set("".join(lines)) <= set("█▀▄ ")
    assert a == cw.qr_ascii("bc1p20sp5gnlkfavn8ww7cvd9f2unjlhc9udxpav8q7u3mumewytvahqqmxn5e")
    assert a != b


def test_tui_funding_screen_resumes_polling_after_pop():
    """Popping back from "Scan now" must restart the poll worker.

    on_mount fires once per screen instance; the resume path is
    on_screen_resume, and it must be guarded so the success path (utxo
    already picked, MiningScreen pushed) does not restart polling and push a
    second MiningScreen.  Source-level pin: the handler exists, resets
    _superseded, restarts the worker, and checks picked_utxo first.
    """
    import causeway_tui as tui
    import inspect
    src = inspect.getsource(tui.WaitForFundingScreen)
    assert "def on_screen_resume" in src
    resume = src[src.index("def on_screen_resume"):]
    resume = resume[:resume.index("def poll_worker")]
    assert "picked_utxo" in resume          # the success-path guard
    assert "_superseded = False" in resume  # polling can actually restart
    assert "poll_worker()" in resume        # and does


def test_tui_panels_scroll_when_the_window_is_small():
    """Every screen panel must cap at the viewport and scroll internally.

    align: center middle with content taller than the terminal CLIPS THE TOP
    unreachably in Textual, and a fixed-size panel hides its bottom buttons —
    found live when the QR pushed the funding screen past a small window's
    height and "there was no way to scroll down."
    """
    import causeway_tui as tui
    import inspect
    import re
    src = inspect.getsource(tui)
    rules = re.findall(r"#panel \{[^}]*\}", src)
    assert rules, "no #panel rules found"
    for r in rules:
        assert "max-height: 100%" in r and "overflow-y: auto" in r, r


def test_proof_json_is_written_0600(tmp_path):
    """proof.json is half the identity bundle and gets the same 0600 treatment
    as the feed."""
    import stat
    path = str(tmp_path / "x-spawn.proof.json")
    cw.write_proof_json({"op": "spawn", "patp": "~zod"}, path)
    assert stat.S_IMODE(os.stat(path).st_mode) == 0o600
    assert cw.load_proof_json(path)["patp"] == "~zod"




# ---------------------------------------------------------------------------
# dat is the PLAINTEXT spawn satpoint (2026-08-18 reversion to the original)
#
# Jake and Christian's spec put the satpoint in the tweak in cleartext:
# (cat 0 (mat dom) <spawn satpoint>).  A hiding commitment with a blind
# replaced it during the OP_RETURN revision and was reverted: the pass and the
# attestation are one object, so every pass-holder held the opening anyway.
# These pin the reverted shape at the level that matters -- the bytes agree
# with the compiled Hoon codec -- and the property that motivates it.
# ---------------------------------------------------------------------------

def test_dat_is_plaintext_and_round_trips():
    txid = "aa" * 31 + "07"
    dat = cw.build_dat_atom(txid, 3, 0)
    dom, kel, spawn = cw.parse_dat_atom(dat)
    assert (dom, kel) == ("gw-btc", 9)
    assert spawn == cw.spawn_sont_noun(txid, 3, 0)
    # a different satpoint is a different dat -- the @p really commits to it
    assert cw.build_dat_atom(txid, 4, 0) != dat
    # and there is nothing else in it: no seed, no blind, no hash
    assert cw.build_dat_atom(txid, 3, 0) == dat


def test_dat_matches_the_hoon_codec_golden():
    """+make-dat:gw-btc-pass on a fake ship, for the Hoon test fixture
    [txid=0x1234.5678.9abc.def0 vout=1 off=0], printed 0x58.c8d1.59e2.6af3.
    7bc3.b00a.9012.4637.4622.d776.77c0 (167 bits) and parsed back to the same
    satpoint.  Two implementations, one number."""
    txid = format(0x123456789abcdef0, "064x")
    dat = cw.build_dat_atom(txid, 1, 0)
    assert dat == 0x58c8d159e26af37bc3b00a901246374622d77677c0
    assert dat.bit_length() == 167
    assert BASIC["dat"] == "58c8d159e26af37bc3b00a901246374622d77677c0"


def test_dat_rejects_trailing_data():
    txid = "bb" * 32
    dat = cw.build_dat_atom(txid, 0, 0)
    with pytest.raises(ValueError):
        cw.parse_dat_atom((0xab << dat.bit_length()) | dat)


def test_no_blind_machinery_survives():
    """The removal is a subtraction, not a bypass: none of the blind-era
    entry points exist, so nothing can quietly mint an old-format identity."""
    for name in ("make_blind", "derive_blind_seed", "blind_from_mnemonic",
                 "spawn_commit", "obtain_blind_mnemonic", "normalize_blind_mnemonic",
                 "BLIND_DERIV_WALLET_SEED", "BLIND_DERIV_BLIND_MNEMONIC",
                 "BLIND_DERIV_PROOF_FILE"):
        assert not hasattr(cw, name), name
