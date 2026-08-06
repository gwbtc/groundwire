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
# Blind derivation — the blind must NEVER be random: it is the only thing that
# opens the dat commitment, so a comet whose blind is not reproducible from a
# BIP-39 phrase the user holds is an identity that dies with its artifact file.
# ---------------------------------------------------------------------------

# Test vector, pinned so any drift in the (frozen) scheme is loud.  Scheme:
#   blind_seed = int(sha256(bip39_seed_64 || b"gw/spawn-blind-seed"
#                           || txid_be32 || vout_le4), "big")
#   blind      = H_tag("gw/spawn-blind", minimal_LE_bytes(blind_seed))
BLIND_M = "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about"
BLIND_TXID = "00" * 31 + "01"
BLIND_VOUT = 0
BLIND_SEED_PIN = 0x9A38D802E6820AE8E327475C15F7F56E891EAD2174CE3D092A47F7046ED7A814
BLIND_PIN = "cd2085c8c8fb1345ee6a84dde2e1b48df4fea154295eb12ffcff210f5c1adafa"


def _bip39_seed(mnemonic: str) -> bytes:
    from embit import bip39 as _b39
    return _b39.mnemonic_to_seed(mnemonic, "")


def test_derive_blind_seed_matches_frozen_scheme():
    # Recompute from first principles rather than trusting our own helper.
    import hashlib
    seed = _bip39_seed(BLIND_M)
    assert len(seed) == 64
    want = int.from_bytes(hashlib.sha256(
        seed + b"gw/spawn-blind-seed"
        + bytes.fromhex(BLIND_TXID) + BLIND_VOUT.to_bytes(4, "little")
    ).digest(), "big")
    got = cw.derive_blind_seed(seed, BLIND_TXID, BLIND_VOUT)
    assert got == want == BLIND_SEED_PIN


def test_derive_blind_seed_is_deterministic():
    seed = _bip39_seed(BLIND_M)
    a = cw.derive_blind_seed(seed, BLIND_TXID, 3)
    b = cw.derive_blind_seed(seed, BLIND_TXID, 3)
    assert a == b
    assert cw.make_blind(a) == cw.make_blind(b)


def test_derive_blind_seed_is_outpoint_sensitive():
    """One phrase can back several spawns: each outpoint gets its own blind."""
    seed = _bip39_seed(BLIND_M)
    by_vout = {v: cw.make_blind(cw.derive_blind_seed(seed, BLIND_TXID, v)) for v in range(4)}
    assert len(set(by_vout.values())) == 4, "different vout must give a different blind"
    other_txid = "11" * 32
    assert cw.derive_blind_seed(seed, BLIND_TXID, 0) != cw.derive_blind_seed(seed, other_txid, 0)


def test_derive_blind_seed_depends_on_the_phrase():
    other = "absurd amount doctor acoustic avoid letter advice cage absurd amount doctor adjust"
    assert cw.normalize_blind_mnemonic(other) == other  # a real, valid phrase
    assert cw.derive_blind_seed(_bip39_seed(BLIND_M), BLIND_TXID, 0) != \
        cw.derive_blind_seed(_bip39_seed(other), BLIND_TXID, 0)


def test_derive_blind_seed_rejects_malformed_outpoint():
    seed = _bip39_seed(BLIND_M)
    with pytest.raises(ValueError):
        cw.derive_blind_seed(seed, "abcd", 0)          # not 32 bytes
    with pytest.raises(ValueError):
        cw.derive_blind_seed(seed, BLIND_TXID, 2**32)  # vout doesn't fit in 4 bytes


def test_blind_from_mnemonic_round_trips_to_known_blind():
    """The blind-mnemonic path: a known phrase yields a known blind."""
    blind_seed, blind = cw.blind_from_mnemonic(BLIND_M, BLIND_TXID, BLIND_VOUT)
    assert blind_seed == BLIND_SEED_PIN
    assert blind.hex() == BLIND_PIN
    # …and composes exactly as make_blind ∘ derive_blind_seed.
    assert blind == cw.make_blind(cw.derive_blind_seed(_bip39_seed(BLIND_M), BLIND_TXID, BLIND_VOUT))
    # Same phrase again -> same blind (a re-spawn with --blind-mnemonic recovers it).
    assert cw.blind_from_mnemonic(BLIND_M, BLIND_TXID, BLIND_VOUT) == (blind_seed, blind)


def test_recovery_drill_from_phrase_and_satpoint_alone():
    """Lose every artifact; keep the phrase and the (public, on-chain) spawn
    satpoint. The blind, d and dat must all come back."""
    txid = "de" * 32
    vout, off = 2, 0

    # --- spawn time -------------------------------------------------------
    blind_seed, blind = cw.blind_from_mnemonic(BLIND_M, txid, vout)
    dat_at_spawn = cw.build_dat_atom(txid, vout, off, blind_seed)
    d_at_spawn = cw.spawn_commit(txid, vout, off, blind)

    # --- much later: only BLIND_M + the satpoint survive -------------------
    rec_seed = cw.derive_blind_seed(_bip39_seed(BLIND_M), txid, vout)
    rec_blind = cw.make_blind(rec_seed)
    rec_d = cw.spawn_commit(txid, vout, off, rec_blind)
    rec_dat = cw.build_dat_atom(txid, vout, off, rec_seed)

    assert rec_blind == blind
    assert rec_d == d_at_spawn
    assert rec_dat == dat_at_spawn
    # The recovered opening reproduces the miner's tweak expression too, so the
    # @p that was mined from it can be re-derived.
    assert cw.make_dat_expr(txid, vout, off, rec_seed) == cw.make_dat_expr(txid, vout, off, blind_seed)


def test_normalize_blind_mnemonic_accepts_valid_and_rejects_junk():
    assert cw.normalize_blind_mnemonic("  " + BLIND_M.upper() + "  ") == BLIND_M
    with pytest.raises(ValueError):
        cw.normalize_blind_mnemonic("not actually a bip39 phrase at all")
    with pytest.raises(ValueError):
        # Valid words, bad checksum.
        cw.normalize_blind_mnemonic(" ".join(["abandon"] * 12))


def test_finish_spawn_proof_records_blind_and_its_derivation():
    txid, vout = "cd" * 32, 1
    blind_seed, blind = cw.blind_from_mnemonic(BLIND_M, txid, vout)
    proof: dict = {}
    cw._finish_spawn_proof(
        proof, comet="~zod", pass_atom=0x1234, blind=blind, blind_seed=blind_seed,
        utxo={"txid": txid, "vout": vout, "xonly": b"\x00" * 32},
        blind_derivation=cw.BLIND_DERIV_BLIND_MNEMONIC,
    )
    assert proof["blind_hex"] == blind.hex()
    assert int(proof["blind_seed_hex"], 16) == blind_seed
    assert proof["blind_derivation"] == "blind-mnemonic+outpoint"
    assert proof["spawn_sont"] == {"txid_hex": txid, "vout": vout, "off": 0}
    # dat in the proof is the one the phrase reproduces.
    assert int(proof["dat_hex"], 16) == cw.build_dat_atom(txid, vout, 0, blind_seed)


def test_spawn_flows_never_use_a_random_blind():
    """Regression guard for the unrecoverable-blind bug: both CLI spawn flows
    must derive the blind from a BIP-39 phrase, never from secrets.token_bytes."""
    import inspect
    for fn in (cw.run_spawn_connect, cw.run_spawn_generate):
        src = inspect.getsource(fn)
        assert "blind_from_mnemonic" in src, f"{fn.__name__} must derive its blind"
        assert "token_bytes" not in src, f"{fn.__name__} still randomizes something"


def _stub_spawn_io(monkeypatch, captured: dict, txid: str, vout: int):
    """Stub every network/miner touchpoint of the spawn flows, capturing the
    blind seed the miner is handed (it is baked into the @p, so it is the value
    that must be reproducible)."""
    def stub_scan(source, **kw):
        addr, spk, xonly, path = source.derive_address(0, 0)
        return [{"address": addr, "scriptpubkey": spk, "xonly": xonly, "path": path,
                 "change": 0, "index": 0, "txid": txid, "vout": vout,
                 "value": 10_000, "confirmed": True}]

    def stub_mine(_txid, _vout, off=0, seed=0, miner_bin="", dom=cw.PKI_DOM):
        captured["mine_seed"] = seed
        captured["mine_outpoint"] = (_txid, _vout, off)
        return {"comet": "~zod", "feed": "0vfeed", "ring": "0wring"}

    monkeypatch.setattr(cw, "scan_addresses", stub_scan)
    monkeypatch.setattr(cw, "mine_comet_from_utxo", stub_mine)
    monkeypatch.setattr(cw, "derive_pass_from_ring", lambda r, t=None: 0xDEADBEEF)
    monkeypatch.setattr(cw, "_broadcast_tx", lambda tx_hex, mempool_base=None: "bc" * 32)
    monkeypatch.setattr(cw, "_print_boot_oneliner", lambda *a, **k: None)
    monkeypatch.setattr(cw, "pick_utxo_interactive", lambda u, **kw: u[0])
    monkeypatch.setattr(cw, "confirm_seed_saved", lambda m, **kw: None)


def _assert_proof_blind_recoverable(proof: dict, phrase: str, derivation: str, captured: dict):
    """The whole point: phrase + spawn satpoint must rebuild blind / seed / dat."""
    assert proof["blind_derivation"] == derivation
    sp = proof["spawn_sont"]
    seed, blind = cw.blind_from_mnemonic(phrase, sp["txid_hex"], sp["vout"])
    assert proof["blind_hex"] == blind.hex()
    assert int(proof["blind_seed_hex"], 16) == seed
    assert int(proof["dat_hex"], 16) == cw.build_dat_atom(sp["txid_hex"], sp["vout"], 0, seed)
    # And that same seed is what the miner mined the @p under.
    assert captured["mine_seed"] == seed
    assert captured["mine_outpoint"] == (sp["txid_hex"], sp["vout"], 0)


def test_spawn_generate_blind_is_recoverable_from_its_wallet_seed(tmp_path, monkeypatch):
    captured: dict = {}
    _stub_spawn_io(monkeypatch, captured, "ab" * 32, 1)
    real_gen = cw.generate_new_mnemonic
    monkeypatch.setattr(cw, "generate_new_mnemonic",
                        lambda **kw: captured.setdefault("phrase", real_gen(**kw)))

    # no_route: this test is about blind recovery, not routing, so it mints a
    # deliberately outbound-only comet rather than naming a sponsor.
    cw.run_spawn_generate(None, 2, "main", str(tmp_path), "miner", "http://stub", False,
                          no_route=True)

    proof = cw.load_proof_json(str(tmp_path / "zod-spawn.proof.json"))
    _assert_proof_blind_recoverable(proof, captured["phrase"], "wallet-seed+outpoint", captured)


def test_spawn_connect_blind_is_recoverable_from_the_blind_mnemonic(tmp_path, monkeypatch):
    from embit import psbt as _psbt
    captured: dict = {}
    _stub_spawn_io(monkeypatch, captured, "cd" * 32, 0)

    seed_phrase = cw.generate_new_mnemonic()
    root = cw.mnemonic_to_hdkey(seed_phrase)
    acct = root.derive("m/86h/0h/0h").to_public()
    desc = f"tr([{cw.hdkey_fingerprint(root).hex()}/86h/0h/0h]{acct.to_base58()}/0/*)"

    def sign(unsigned_b64, signed_psbt=None):
        p = _psbt.PSBT.from_base64(unsigned_b64)
        p.sign_with(root)
        return p.to_base64()

    monkeypatch.setattr(cw, "_await_signed_psbt", sign)
    blind_phrase = "absurd amount doctor acoustic avoid letter advice cage absurd amount doctor adjust"

    cw.run_spawn_connect(desc, None, 2, "main", str(tmp_path), "miner", "http://stub", False,
                         blind_mnemonic=blind_phrase, no_route=True)

    proof = cw.load_proof_json(str(tmp_path / "zod-spawn.proof.json"))
    _assert_proof_blind_recoverable(proof, blind_phrase, "blind-mnemonic+outpoint", captured)
    # The wallet seed must NOT be what backs the blind on this path.
    assert cw.blind_from_mnemonic(seed_phrase, "cd" * 32, 0)[1].hex() != proof["blind_hex"]


def test_spawn_connect_rejects_an_invalid_blind_mnemonic(tmp_path, monkeypatch):
    captured: dict = {}
    _stub_spawn_io(monkeypatch, captured, "cd" * 32, 0)
    with pytest.raises(SystemExit):
        cw.run_spawn_connect("xpub-unused", None, 2, "main", str(tmp_path), "miner",
                             "http://stub", False, blind_mnemonic="clearly not bip39")
    assert "mine_seed" not in captured, "must bail before mining an unrecoverable comet"


def test_tui_mining_screen_never_uses_a_random_blind():
    pytest.importorskip("textual")
    import inspect
    import causeway_tui as tui
    src = inspect.getsource(tui.MiningScreen.run_mine)
    assert "blind_from_mnemonic" in src
    assert "token_bytes" not in src
    # The connect flow (no wallet seed) must route through the phrase screen.
    assert "BlindPhraseScreen" in inspect.getsource(tui.UtxoPickerScreen.pick_current)


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
        "blind_hex": "cc" * 32,
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
    opening       = [internal-key snapshot blind-opening=(unit blind-opening)]
    blind-opening = [spawn start-height blind]
    """
    log = cw.hoon_cue(xtr)
    entry0 = log[0]
    opening = entry0[1][1][1]          # (unit opening) -> opening
    blind_opening = opening[1][1][1]   # (unit blind-opening) -> blind-opening
    return blind_opening[1][0]


def test_finalize_bakes_the_funding_height_into_the_blind_opening(tmp_path, monkeypatch):
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
        cw.run_spawn_connect("xpub-does-not-matter", None, 2, "main", ".", "miner", "stub")
    assert "neither a sponsor nor a fief" in str(e.value)

    with pytest.raises(Exception) as e:
        cw.run_spawn_generate(None, 2, "main", ".", "miner", "stub")
    assert "neither a sponsor nor a fief" in str(e.value)


def test_spawn_with_a_sponsor_passes_the_routability_gate(monkeypatch):
    """With --sponsor the gate lets the flow proceed; it stops at the first
    real step instead (proving the gate, not the network, was the blocker)."""
    sentinel = RuntimeError("reached parse_key_source")

    def stop(*a, **k):
        raise sentinel

    monkeypatch.setattr(cw, "parse_key_source", stop)
    with pytest.raises(RuntimeError) as e:
        cw.run_spawn_connect("xpub", None, 2, "main", ".", "miner", "stub",
                             sponsor=SPONSOR_PATP)
    assert e.value is sentinel


def test_spawn_no_route_flag_passes_the_gate(monkeypatch):
    sentinel = RuntimeError("reached parse_key_source")

    def stop(*a, **k):
        raise sentinel

    monkeypatch.setattr(cw, "parse_key_source", stop)
    with pytest.raises(RuntimeError) as e:
        cw.run_spawn_connect("xpub", None, 2, "main", ".", "miner", "stub",
                             no_route=True)
    assert e.value is sentinel


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
# the %noun mark ($ingest in groundwire/sur/self-attestation.hoon), so this
# string is the desktop half of that contract.
# ---------------------------------------------------------------------------


def test_custody_entry_poke_shape_for_a_spawn():
    entry = {
        "txid_hex": "de" * 32,
        "height": 900_142,
        "opening": {
            "internal_key": int("02" + "ab" * 32, 16),
            "snapshot": {"life": 1, "rift": 0, "key": 0xC0FFEE, "sponsor": None, "fief": None},
            "blind_opening": {
                "spawn": {"txid_hex": "ad" * 32, "vout": 1, "off": 0},
                "start_height": 900_100,
                "blind": bytes.fromhex("cc" * 32),
            },
        },
    }
    line = cw.format_custody_entry_poke(entry)
    assert line.startswith(":gw-btc &noun [%gw-custody-entry ")
    # entry = [txid height opening]; the opening is PRESENT (a spawn)
    assert " 900.142 `[" in line
    # the blind-opening carries the FUNDING tx's height, not the entry's
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
    # ($ingest in groundwire/sur/self-attestation.hoon) with `urbit eval`.
    assert line == (
        ":gw-btc &noun [%gw-custody-entry ["
        + cw.format_hoon_ux("de" * 32)
        + " 900.142 `["
        + cw.format_hoon_ux("02" + "ab" * 32)
        + " [1 0 0xc0.ffee ~ ~] `[["
        + cw.format_hoon_ux("ad" * 32)
        + " 1 0] 900.100 "
        + cw.format_hoon_ux("cc" * 32)
        + "]]]]"
    )


def test_custody_entry_poke_renders_sponsor_and_a_bare_hop():
    entry = {
        "txid_hex": "de" * 32,
        "height": 900_200,
        "opening": {
            "internal_key": int("02" + "ab" * 32, 16),
            "snapshot": {"life": 2, "rift": 0, "key": 0xC0FFEE,
                         "sponsor": cw.patp_to_int("~marzod"), "fief": None},
            "blind_opening": None,     # a state update, not a spawn
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
            "blind_opening": None,     # a state update, not the spawn
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
