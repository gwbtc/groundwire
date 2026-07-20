"""Pytest suite for causeway.py — sotx encoders, PSBT builder, proof round-trip, xpub parsing.

Run with: cd causeway && pytest
"""

import os
import sys

import pytest

# Make ../causeway.py importable
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))

import causeway as cw


# ---------------------------------------------------------------------------
# Tweak construction — 3 golden vectors from the web codebase (src/spawn/tweak.ts)
# ---------------------------------------------------------------------------


def test_tweak_bytes_vector_1():
    # Full 32-byte txid (no leading zero display byte) — 32 minimal LE bytes.
    got = cw.build_tweak_bytes(
        txid_hex="ff11223344556677889900112233445566778899001122334455667788990011",
        vout=0,
        off=0,
    )
    assert got[0] == 0x09
    assert b"urb-watcher" in got
    assert b"btc" in got
    assert b"gw" in got
    # txid contributes its minimal LE bytes = reverse of the display bytes.
    txid_le = bytes.fromhex(
        "ff11223344556677889900112233445566778899001122334455667788990011"
    )[::-1]
    assert txid_le in got
    assert got.hex().count(txid_le.hex()) == 1


def test_tweak_bytes_leading_zero_txid_is_minimal():
    # A display txid with a leading zero byte contributes fewer than 32 bytes,
    # matching Hoon's `rap 3` minimal-byte semantics.
    got = cw.build_tweak_bytes("00" * 31 + "01", 0, 0)
    # urbit eval: `@ux`(rap 3 ~[%9 ~tyr %urb-watcher %btc %gw %9 0x1 0 0])
    assert got.hex() == "09997572622d7761746368657262746367770901"


def test_tweak_bytes_different_utxos_produce_different_bytes():
    # Note: the Hoon tweak format is `(rap 3 ~[... txid vout off])`, and `rap 3`
    # emits each atom's raw LE bytes with no length prefix. So (vout=1, off=0) and
    # (vout=0, off=1) collide in bytes by design — we don't test that case.
    a = cw.build_tweak_bytes("00" * 32, 0, 0)
    b = cw.build_tweak_bytes("00" * 32, 1, 0)
    c = cw.build_tweak_bytes("00" * 32, 2, 0)
    d = cw.build_tweak_bytes("ff" * 32, 0, 0)
    assert len({a, b, c, d}) == 4


# ---------------------------------------------------------------------------
# Sotx encoders — per-opcode, deterministic, and well-formed
# ---------------------------------------------------------------------------


def test_spawn_sotx_encodes_deterministically():
    params = dict(
        comet_p=0xAABBCCDDEEFF0011_2233445566778899_AABBCCDDEEFF0011_2233445566778899,
        pass_atom=0x1234,
        spkh=b"\x42" * 32,
        vout=0,
        off=0,
        tej=0,
        fief=None,
    )
    a = cw.encode_spawn_sotx(**params)
    b = cw.encode_spawn_sotx(**params)
    assert a == b
    assert len(a) > 0


def test_spawn_sotx_header_bits():
    out = cw.encode_spawn_sotx(
        comet_p=0,
        pass_atom=1,
        spkh=b"\x00" * 32,
        vout=0,
        off=0,
        tej=0,
        fief=None,
    )
    # First byte: bits 0-1 are en-sig type (=0 → no sig), bit 2-7 are low 6 bits of from-ship (=0).
    # So the first byte should be 0x00.
    assert out[0] == 0x00


def test_keys_sotx_opcode():
    # %keys opcode = 2. After outer [2 0] [128 comet_p], the next 7 bits are opcode = 2.
    out = cw.encode_keys_sotx(comet_p=0, pass_atom=0, breach=False)
    # bits: [sig=2 bits: 00] [from=128 bits: 00..00] [opcode=7 bits: 2 = 0b0000010] [breach=1 bit: 0] [pass-mat]
    # total 2+128+7+1+... = 138+ bits. First 16 bytes are zero (sig+from-ship lower 126 bits),
    # then bits 130-136 contain opcode. Bit position: 130..136 for opcode,
    # byte 16 bit 2..byte 17 bit 0 (LSB-first). Specifically, byte 16 has bits 128..135,
    # bit 2 of byte 16 is bit 130.
    # Opcode 2 = 0b0000010, 7 bits LSB-first means bits 130..136 are 0,1,0,0,0,0,0.
    # So byte 16's bit 2 should be 0, byte 16's bit 3 should be 1.
    # That makes byte 16 = 0b00001000 = 0x08.
    assert out[16] == 0x08


def test_escape_sotx_without_sig_vs_with_sig_differ():
    out_a = cw.encode_escape_sotx(comet_p=1, parent_p=2, escape_sig=None)
    out_b = cw.encode_escape_sotx(comet_p=1, parent_p=2, escape_sig=0xDEADBEEF)
    assert out_a != out_b
    # With sig the encoding is longer (extra 512 bits)
    assert len(out_b) - len(out_a) >= 60


def test_cancel_escape_adopt_reject_detach_differ_only_by_opcode():
    ce = cw.encode_cancel_escape_sotx(comet_p=0, parent_p=1)
    ad = cw.encode_adopt_sotx(sponsor_p=0, child_p=1)
    rj = cw.encode_reject_sotx(sponsor_p=0, child_p=1)
    dt = cw.encode_detach_sotx(sponsor_p=0, child_p=1)
    assert len({ce, ad, rj, dt}) == 4
    # All same length (opcode is a 7-bit field, value differs only within one byte)
    assert len(ce) == len(ad) == len(rj) == len(dt)


def test_fief_sotx_variants():
    none_fief = cw.encode_fief_sotx(comet_p=0, fief=None)
    ipv4 = cw.encode_fief_sotx(comet_p=0, fief=("if", 0x01020304, 31337))
    ipv6 = cw.encode_fief_sotx(comet_p=0, fief=("is", 0x11 << 120, 31337))
    assert len(none_fief) < len(ipv4) < len(ipv6)


# ---------------------------------------------------------------------------
# Confidential commit PSBT builder
# ---------------------------------------------------------------------------


def _fake_utxo():
    """Build a fake UTXO with a REAL curve-valid xonly pubkey (not random bytes)."""
    from embit import ec as _ec
    # Derive a pubkey from a fixed private key; x-only the result.
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


def test_confidential_psbt_round_trip():
    p, proof = cw.build_confidential_commit_psbt(
        **_fake_utxo(),
        attestation_bytes=b"hello-causeway",
    )
    b64 = p.to_base64()
    from embit import psbt as _p
    restored = _p.PSBT.from_base64(b64)
    assert restored.tx.vin[0].vout == 0
    assert len(restored.inputs[0].taproot_bip32_derivations) == 1
    assert restored.inputs[0].taproot_internal_key is not None


def test_proof_self_verify_passes_for_freshly_built_commit():
    p, proof = cw.build_confidential_commit_psbt(
        **_fake_utxo(),
        attestation_bytes=b"verify-me",
    )
    ok, reason = cw.verify_proof_self(proof)
    assert ok, reason


def test_proof_self_verify_fails_on_tampered_leaf():
    p, proof = cw.build_confidential_commit_psbt(
        **_fake_utxo(),
        attestation_bytes=b"original",
    )
    # Flip a bit in the leaf script — recompute should now fail to match the stored spk.
    tampered = bytearray(bytes.fromhex(proof["leaf_script_hex"]))
    tampered[-1] ^= 0x01
    proof["leaf_script_hex"] = bytes(tampered).hex()
    ok, _ = cw.verify_proof_self(proof)
    assert not ok


def test_confidential_psbt_rejects_undersized_utxo():
    u = _fake_utxo()
    u["utxo_value"] = 100  # less than commit+fee minimum
    with pytest.raises(RuntimeError):
        cw.build_confidential_commit_psbt(**u, attestation_bytes=b"x")


# ---------------------------------------------------------------------------
# proof.json — write, load, round-trip
# ---------------------------------------------------------------------------


def test_proof_json_round_trip(tmp_path):
    p, proof = cw.build_confidential_commit_psbt(
        **_fake_utxo(),
        attestation_bytes=b"round-trip-me",
    )
    proof["op"] = "spawn"
    proof["patp"] = "~sampel-palnet"
    proof["commit_txid"] = "de" * 32
    path = tmp_path / "x.proof.json"
    cw.write_proof_json(proof, str(path))
    loaded = cw.load_proof_json(str(path))
    assert loaded == proof


# ---------------------------------------------------------------------------
# Xpub / descriptor parsing
# ---------------------------------------------------------------------------


# BIP-86 test vector, mainnet — any real xpub string works for structural tests.
# Source: https://github.com/bitcoin/bips/blob/master/bip-0086.mediawiki
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
    assert len(spk) == 34  # OP_1 OP_PUSH32 <32 bytes>
    assert spk[:2] == bytes([0x51, 0x20])
    assert len(xonly) == 32
    assert path.endswith("/0/0")


# ---------------------------------------------------------------------------
# Mnemonic generation + key derivation
# ---------------------------------------------------------------------------


def test_generate_new_mnemonic_is_12_words_by_default():
    m = cw.generate_new_mnemonic(strength_bits=128)
    assert len(m.split()) == 12


def test_generate_new_mnemonic_24_words_for_256_bits():
    m = cw.generate_new_mnemonic(strength_bits=256)
    assert len(m.split()) == 24


def test_mnemonic_to_hdkey_and_fingerprint_deterministic():
    # BIP-39 standard test vector
    m = "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about"
    k1 = cw.mnemonic_to_hdkey(m)
    k2 = cw.mnemonic_to_hdkey(m)
    # HDKey has no __eq__ we can rely on; compare xpub strings.
    assert k1.to_base58() == k2.to_base58()
    fp1 = cw.hdkey_fingerprint(k1)
    fp2 = cw.hdkey_fingerprint(k2)
    assert fp1 == fp2
    assert len(fp1) == 4


# ---------------------------------------------------------------------------
# NUMS point sanity
# ---------------------------------------------------------------------------


def test_nums_xonly_constant_is_bip341():
    # The standard BIP-341 NUMS value
    assert cw.NUMS_XONLY.hex() == "50929b74c1a04954b78b4b6035e97a5e078a5a0f28ec96d547bfee9ace803ac0"


def test_build_p2tr_spk_is_34_bytes_op1_push32():
    spk = cw._build_p2tr_spk(cw.NUMS_XONLY, None)
    assert len(spk) == 34
    assert spk[:2] == bytes([0x51, 0x20])


# ---------------------------------------------------------------------------
# Commit output design — internal key is the funding xonly (not NUMS) so the
# point owner can re-spend the sat for the next management op (urb-core's
# ++is-sont-in-input invariant).
# ---------------------------------------------------------------------------


def test_commit_internal_key_equals_funding_xonly_not_nums():
    u = _fake_utxo()
    _, proof = cw.build_confidential_commit_psbt(**u, attestation_bytes=b"x")
    assert proof["internal_pubkey_hex"] == u["funding_internal_xonly"].hex()
    assert proof["internal_pubkey_hex"] != cw.NUMS_XONLY.hex()


def test_commit_proof_has_merkle_root_hex():
    _, proof = cw.build_confidential_commit_psbt(**_fake_utxo(), attestation_bytes=b"x")
    # Single-leaf tree: merkle_root == leaf_hash. Stored for next-op PSBT hint.
    assert "merkle_root_hex" in proof
    assert len(bytes.fromhex(proof["merkle_root_hex"])) == 32


def test_commit_proof_has_funding_provenance():
    u = _fake_utxo()
    _, proof = cw.build_confidential_commit_psbt(**u, attestation_bytes=b"x")
    f = proof["funding"]
    assert f["txid"] == u["utxo_txid"]
    assert f["vout"] == u["utxo_vout"]
    assert f["value"] == u["utxo_value"]
    assert f["path"] == u["funding_path"]


# ---------------------------------------------------------------------------
# Chained commit — the next management op spends the prior commit output.
# ---------------------------------------------------------------------------


def test_chained_commit_input_matches_prior_commit_output():
    u = _fake_utxo()
    _, spawn_proof = cw.build_confidential_commit_psbt(**u, attestation_bytes=b"spawn")
    spawn_proof["commit_txid"] = "de" * 32
    spawn_proof["op"] = "spawn"
    spawn_proof["patp"] = "~sampel-palnet"

    rekey_psbt, rekey_proof = cw.build_chained_commit_psbt(
        prior_proof=spawn_proof,
        new_attestation_bytes=b"rekey-sotx-bytes",
    )
    # Input references prior commit output
    assert rekey_psbt.tx.vin[0].vout == spawn_proof["commit_vout"]
    # PSBT_IN_TAP_MERKLE_ROOT is set so external signers can compute the tweak
    assert rekey_psbt.inputs[0].taproot_merkle_root == bytes.fromhex(spawn_proof["merkle_root_hex"])
    # Same internal key — same point owner
    assert rekey_proof["internal_pubkey_hex"] == spawn_proof["internal_pubkey_hex"]
    # Prior provenance is recorded
    assert rekey_proof["prior_proof"]["commit_txid"] == spawn_proof["commit_txid"]


def test_chained_commit_rejects_unbroadcast_prior():
    u = _fake_utxo()
    _, spawn_proof = cw.build_confidential_commit_psbt(**u, attestation_bytes=b"spawn")
    # No commit_txid — prior hasn't been broadcast yet.
    with pytest.raises(ValueError, match="no commit_txid"):
        cw.build_chained_commit_psbt(prior_proof=spawn_proof, new_attestation_bytes=b"rekey")


def test_add_tap_merkle_root_hint():
    u = _fake_utxo()
    p, _ = cw.build_confidential_commit_psbt(**u, attestation_bytes=b"x")
    fake_root = b"\x01" * 32
    cw.add_tap_merkle_root_hint(p, 0, fake_root)
    assert p.inputs[0].taproot_merkle_root == fake_root


# ---------------------------------------------------------------------------
# cc-draft-2 dat / xtr — golden vectors generated with `urbit eval` against
# the cyc/cc-draft-2 kernel (see the dat section in causeway.py):
#   dat: `@ux`(can 0 (mat %<dom>) [256 txid] (mat vout) (mat off) ~)
#   xtr: `@ux`(jam ~[[txid block internal-key leaf-version wid script] ...])
# ---------------------------------------------------------------------------

_DAT_TXID = "ab12f00d9c330000111122223333444455556666777788889999aaaabbbbcccc"


def _ux(s: str) -> int:
    return int(s.replace(".", ""), 16)


def test_dat_atom_vector_gwbtc_vout1():
    # urbit eval: `@ux`(can 0 (mat %gw-btc) [256 <_DAT_TXID>] (mat 1) (mat 0) ~)
    exp = _ux(
        "7558.9780.6ce1.9800.0088.8911.1199.9a22.22aa.ab33.33bb.bc44"
        ".44cc.cd55.55dd.de66.6637.4622.d776.77c0"
    )
    assert cw.build_dat_atom(_DAT_TXID, 1, 0) == exp
    # dom tag + satpoint; "gw-btc" (6 chars) → 40 bytes.
    assert len(cw.build_dat_bytes(_DAT_TXID, 1, 0)) == 40


def test_dat_atom_vector_gwbtc_vout0():
    exp = _ux(
        "1d58.9780.6ce1.9800.0088.8911.1199.9a22.22aa.ab33.33bb.bc44"
        ".44cc.cd55.55dd.de66.6637.4622.d776.77c0"
    )
    assert cw.build_dat_atom(_DAT_TXID, 0, 0) == exp


def test_dat_atom_vector_urb_watcher_offsets():
    exp = _ux(
        "1112.8680.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000"
        ".0000.0000.0000.0000.0000.0efc.995a.18dd.185d.cb58.9c9d.5780"
    )
    assert cw.build_dat_atom("77", 3, 546, dom="urb-watcher") == exp


def test_dat_expr_matches_atom_semantics():
    # The hoon expression and the local builder must express the same value;
    # the expression is what comet-miner evaluates via u3v_wish.
    expr = cw.make_dat_expr(_DAT_TXID, 1, 0)
    assert expr == (
        "(can 0 (mat %gw-btc) "
        "[256 0xab12.f00d.9c33.0000.1111.2222.3333.4444.5555.6666.7777.8888.9999.aaaa.bbbb.cccc] "
        "(mat 1) (mat 0) ~)"
    )


def test_dat_domain_is_rub_extractable():
    # The kernel's +pass-pki-dom does (rub 0 dat); the head must decode to the domain tag.
    dat = cw.build_dat_atom(_DAT_TXID, 7, 0)
    _width, dom_atom = cw._hoon_rub(0, dat)
    assert dom_atom.to_bytes(6, "little").decode() == "gw-btc"


def test_xtr_jam_vector_single_entry():
    xtr = cw.build_xtr_atom([dict(
        txid_hex="ab12", block_hash_hex="bb44", internal_key_hex="cc55",
        leaf_version=0xC0, leaf_script_hex="dd66",
    )])
    assert xtr == _ux("2.dd66.0812.1c01.0398.aa10.1bb4.4080.d589.0405")


def test_xtr_jam_vector_two_entries():
    xtr = cw.build_xtr_atom([
        dict(txid_hex="ab12", block_hash_hex="bb44", internal_key_hex="cc55",
             leaf_version=0xC0, leaf_script_hex="dd66"),
        dict(txid_hex="ab13", block_hash_hex="bb45", internal_key_hex="cc56",
             leaf_version=0xC0, leaf_script_hex="1234"),
    ])
    assert xtr == _ux(
        "523.4b04.86df.1b98.ac10.1bb4.5080.d589.8405"
        ".dd66.0812.1c01.0398.aa10.1bb4.4080.d589.0405"
    )


def test_ring_xtr_append_and_pass_roundtrip():
    # A miner-fresh ring has no xtr; appending one must leave dat (and hence
    # the @p) unchanged and surface the xtr in the derived pass.
    dat = cw.build_dat_atom(_DAT_TXID, 0, 0)
    w = cw.BitWriter()
    w.write(8, ord("C"))
    w.write(512, 0xDEAD << 496 | 0xBEEF)  # arbitrary 64-byte seed material
    w.write_mat(dat)
    ring0 = w.to_int()
    xtr = cw.build_xtr_atom([dict(
        txid_hex="ab" * 32, block_hash_hex="cd" * 32,
        internal_key_hex="02" + "ef" * 32, leaf_version=0xC0,
        leaf_script_hex="0063037572620102ac",
    )])
    ring1 = cw.append_xtr_to_ring(ring0, xtr)

    bod0, bod1 = ring0 >> 8, ring1 >> 8
    p0, dat0 = cw._hoon_rub(512, bod0)
    p1, dat1 = cw._hoon_rub(512, bod1)
    assert dat0 == dat1 == dat
    assert bod1 >> (512 + p1) == xtr
    assert bod0 >> (512 + p0) == 0

    pass0 = cw.derive_pass_from_ring(cw.encode_uw(ring0))
    pass1 = cw.derive_pass_from_ring(cw.encode_uw(ring1))
    # ugn/cry/dat identical; only the xtr tail differs
    assert pass0 != pass1
    assert pass1 & ((1 << (8 + 256 + 256)) - 1) == pass0 & ((1 << (8 + 256 + 256)) - 1)
    # tweaked fig (the @p) depends only on ugn+dat — recompute both ways
    bodp = pass1 >> 8
    _pw, datp = cw._hoon_rub(512, bodp)
    assert datp == dat


def test_hoon_cue_roundtrips_jam():
    noun = ((2, 0), (0x42CD, (0, ((1, 0xABCDEF0123456789), 0))))
    assert cw.hoon_cue(cw.hoon_jam(noun)) == noun


def test_rebuild_feed_matches_feed_shape():
    ring = 0x43 | (99 << 8)
    feed = cw.rebuild_feed(0x42CD, 0, 1, ring)
    assert cw.hoon_cue(feed) == ((2, 0), (0x42CD, (0, ((1, ring), 0))))


# ---------------------------------------------------------------------------
# Review-fix regressions: breach bit, fingerprint, tap-tree, tx extraction.
# ---------------------------------------------------------------------------

def test_keys_breach_bit_matches_hoon():
    # Hoon full-keys vectors (ship=0x42cd, pass=0xdeadbeef), from `urbit eval`
    # of p:(fax:plot ...): loobean %.y (breach=True) encodes as bit 0.
    def le(b):
        return int.from_bytes(b, "little")
    nb = cw.encode_keys_sotx(comet_p=0x42CD, pass_atom=0xDEADBEEF, breach=False)
    br = cw.encode_keys_sotx(comet_p=0x42CD, pass_atom=0xDEADBEEF, breach=True)
    assert le(nb) == 0x37AB6FBBC1020800000000000000000000000000010B34
    assert le(br) == 0x37AB6FBBC1000800000000000000000000000000010B34


def test_hdkey_fingerprint_no_openssl3_crash():
    root = cw.mnemonic_to_hdkey(cw.generate_new_mnemonic())
    fp = cw.hdkey_fingerprint(root)
    assert isinstance(fp, bytes) and len(fp) == 4


def test_commit_output_has_no_taptree_metadata():
    # The tweaked commit output must NOT claim the wallet's derivation without
    # a tap tree, or strict signers flag a change-substitution. Applies to both
    # the spawn and the chained (management-op) commit builders.
    p, _ = cw.build_confidential_commit_psbt(**_fake_utxo(), attestation_bytes=b"x")
    assert p.outputs[0].taproot_internal_key is None
    assert not p.outputs[0].taproot_bip32_derivations

    _, spawn_proof = cw.build_confidential_commit_psbt(**_fake_utxo(), attestation_bytes=b"spawn")
    spawn_proof["commit_txid"] = "cd" * 32  # pretend it broadcast
    pc, _ = cw.build_chained_commit_psbt(prior_proof=spawn_proof, new_attestation_bytes=b"rekey")
    assert pc.outputs[0].taproot_internal_key is None
    assert not pc.outputs[0].taproot_bip32_derivations


def test_extract_tx_does_not_use_segwit_kwarg():
    # Build a commit, drop in a fake finalized witness, and extract — exercises
    # the txid path that previously crashed on serialize(segwit=False).
    from embit.transaction import Witness
    p, _ = cw.build_confidential_commit_psbt(**_fake_utxo(), attestation_bytes=b"x")
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
    # Identical to the TS port's output (tests/fixtures/mnemonyms.json).
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
    for _ in range(500):
        atom = (1 << 64) + secrets.randbits(64)  # comet range
        assert cw.patp_to_int(cw.int_to_patp(atom)) == atom
    # Known vectors (urbit-ob, non-scrambled comet range)
    assert cw.int_to_patp(0xE7F38742227BF54EC48BE13DBA9242CD) == (
        "~mosnyt-londen-lacrux-rosmeg--mitrev-larder-loddux-daplyd"
    )
    assert cw.int_to_patp(0x42CD) == "~daplyd"
