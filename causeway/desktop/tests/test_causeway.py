"""Pytest suite for causeway.py — kelvin-9 (%gw-btc, OP_RETURN) encoders,
spawn/rekey PSBT builders, proof round-trip, xpub parsing, mnemonyms.

The encoder tests are pinned to the shared golden vectors
(groundwire/vectors/gw-kelvin-9.json), which agree byte-for-byte with the
compiled Hoon lib gw-btc-pass.hoon and its passing test.

Run with: cd causeway/desktop && python -m pytest -q
"""

import json
import os
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
    return {
        "life": s["life"], "rift": s["rift"],
        "key": int(s["key"], 16), "sponsor": sponsor, "fief": None,
    }


# ---------------------------------------------------------------------------
# Golden vectors — hiding dat / blind / d
# ---------------------------------------------------------------------------


def test_golden_blind():
    seed = int(BASIC["seed"], 16)
    assert cw.make_blind(seed).hex() == BASIC["blind"]


def test_golden_jam_spawn():
    sp = BASIC["spawn_sont"]
    noun = cw.spawn_sont_noun(sp["txid"], sp["vout"], sp["off"])
    assert cw.jam_bytes(noun).hex() == BASIC["jam_spawn_le"]


def test_golden_spawn_commit_d():
    # d = H_tag('gw/spawn-commit', jam_spawn_le || blind) — LE jam bytes.
    sp = BASIC["spawn_sont"]
    blind = cw.make_blind(int(BASIC["seed"], 16))
    d = cw.spawn_commit(sp["txid"], sp["vout"], sp["off"], blind)
    assert d.hex() == BASIC["spawn_commit_d"]


def test_golden_dat():
    sp = BASIC["spawn_sont"]
    dat = cw.build_dat_atom(sp["txid"], sp["vout"], sp["off"], int(BASIC["seed"], 16))
    assert dat == int(BASIC["dat"], 16)
    # mat("gw-btc") 59 bits + mat(9) 10 bits + 256 bits d = 325 bits -> 41 bytes.
    assert len(cw.build_dat_bytes(sp["txid"], sp["vout"], sp["off"], int(BASIC["seed"], 16))) == 41


def test_dat_domain_is_rub_extractable():
    # +pass-pki-dom does (rub 0 dat); the head must decode to the domain tag.
    sp = BASIC["spawn_sont"]
    dat = cw.build_dat_atom(sp["txid"], sp["vout"], sp["off"], int(BASIC["seed"], 16))
    _width, dom_atom = cw._hoon_rub(0, dat)
    assert dom_atom.to_bytes(6, "little").decode() == "gw-btc"


def test_dat_expr_is_a_concrete_can():
    sp = BASIC["spawn_sont"]
    expr = cw.make_dat_expr(sp["txid"], sp["vout"], sp["off"], int(BASIC["seed"], 16))
    assert expr.startswith("(can 0 (mat %gw-btc) (mat 9) [256 0x")
    assert expr.endswith("] ~)")


def test_blind_depends_on_seed():
    assert cw.make_blind(1) != cw.make_blind(2)
    # And dat therefore depends on the seed too.
    sp = BASIC["spawn_sont"]
    assert cw.build_dat_atom(sp["txid"], sp["vout"], sp["off"], 1) != \
        cw.build_dat_atom(sp["txid"], sp["vout"], sp["off"], 2)


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
    blind = cw.make_blind(int(BASIC["seed"], 16))
    return {
        "internal_key": int(pub["internal_key"], 16),
        "snapshot": _snap(BASIC),
        "blind_opening": {
            "spawn": {"txid_hex": sp["txid"], "vout": sp["vout"], "off": sp["off"]},
            "start_height": pub["start_height"],
            "blind": blind,
        },
    }


def test_golden_publication_jam():
    pub = BASIC["publication"]
    noun = cw.publication_noun(int(pub["pass"], 16), _basic_opening())
    assert cw.jam_bytes(noun).hex() == pub["jam_publication_le"]


def test_publication_script_shape():
    pub = BASIC["publication"]
    script = cw.make_publication_script(int(pub["pass"], 16), _basic_opening())
    # OP_RETURN PUSH3 'urb' PUSH1 <kelvin=0x09> then pushdata.  (The JSON's
    # `op_return_script` field is illustrative and even drops the 0x09 kelvin
    # byte; the binding golden is jam_publication_le, checked above.)
    assert script[:7] == bytes([0x6A, 0x03, 0x75, 0x72, 0x62, 0x01, 0x09])
    payload = bytes.fromhex(pub["jam_publication_le"])
    assert len(payload) == 69  # <= 75 -> a direct length push, not PUSHDATA1
    assert script[7:] == bytes([len(payload)]) + payload


def test_publication_small_payload_uses_direct_push():
    # A tiny opening (< 75 byte payload) must use a direct length push, no 0x4c.
    opening = {"internal_key": 1, "snapshot": {"life": 1, "rift": 0, "key": 1,
               "sponsor": None, "fief": None}, "blind_opening": None}
    script = cw.make_publication_script(1, opening)
    payload = cw.jam_bytes(cw.publication_noun(1, opening))
    assert len(payload) <= 75
    assert script[7] == len(payload)  # direct push, not 0x4c


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
        "blind_opening": {"spawn": {"txid_hex": u["utxo_txid"], "vout": 0, "off": 0},
                          "start_height": 0, "blind": b"\x11" * 32},
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
    dat = cw.build_dat_atom(sp["txid"], sp["vout"], sp["off"], int(BASIC["seed"], 16))
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
