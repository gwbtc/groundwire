"""Invites (a prepaid spawn as one hex code) and next-block fee pricing.

The property that matters most is the last test: a spawn paid for by an
invite is signed by the invite key, but the identity sat it creates is NOT
spendable by that key -- the inviter can pay for a comet without ever being
able to move it.
"""

import os
import sys

import pytest

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))

import causeway as cw  # noqa: E402


# ---------------------------------------------------------------------------
# The code itself
# ---------------------------------------------------------------------------


def test_invite_code_round_trips_and_is_74_hex_chars():
    secret = bytes(range(32))
    code = cw.encode_invite(secret)
    assert len(code) == cw.INVITE_HEX_LEN == 74
    assert all(c in "0123456789abcdef" for c in code)
    assert cw.decode_invite(code) == secret
    assert cw.decode_invite("0x" + code.upper()) == secret     # forgiving input
    assert cw.is_invite_code(code)


def test_invite_code_rejects_a_typo_a_truncation_and_the_faucet_style():
    code = cw.encode_invite(bytes(range(32)))
    flipped = ("f" if code[10] != "f" else "0") + code[11:]
    with pytest.raises(ValueError, match="checksum"):
        cw.decode_invite(code[:10] + flipped)
    with pytest.raises(ValueError, match="74"):
        cw.decode_invite(code[:-2])
    with pytest.raises(ValueError, match="version"):
        cw.decode_invite("02" + code[2:])
    # Whatever the faucet handed out is not an invite code, and must not be
    # mistaken for one: spawn generate keeps the legacy faucet path for it.
    assert not cw.is_invite_code("GW-FAUCET-ABC123")
    assert not cw.is_invite_code(None)
    assert not cw.is_invite_code("")


def test_invite_wallet_is_deterministic_and_raw_seeded():
    """The 32 bytes are the BIP-32 seed (no BIP-39), so the same code always
    lands on the same address, and a different code never does."""
    a_root, a = cw.invite_wallet(bytes(range(32)))
    b_root, b = cw.invite_wallet(bytes(range(32)))
    c_root, c = cw.invite_wallet(bytes(range(1, 33)))
    assert a.derive_address(0, 0) == b.derive_address(0, 0)
    assert a.derive_address(0, 0)[0] != c.derive_address(0, 0)[0]
    assert a.master_fingerprint == b.master_fingerprint != c.master_fingerprint
    assert a.derive_address(0, 0)[0].startswith("bc1p")
    assert a.derive_address(0, 0)[3] == "m/86h/0h/0h/0/0"


def test_new_invites_differ():
    assert cw.encode_invite(cw.new_invite_secret()) != cw.encode_invite(cw.new_invite_secret())


# ---------------------------------------------------------------------------
# Fees: next block plus a little, and honest about the fallback
# ---------------------------------------------------------------------------


def test_recommended_fee_rate_is_next_block_plus_overhead(monkeypatch):
    monkeypatch.setattr(cw, "mempool_get", lambda path, base=None: {"fastestFee": 7, "halfHourFee": 5})
    rate, how = cw.recommended_fee_rate("http://stub")
    assert rate == 7 + cw.FEE_RATE_OVERHEAD
    assert "next block 7" in how


def test_recommended_fee_rate_never_below_the_floor(monkeypatch):
    monkeypatch.setattr(cw, "mempool_get", lambda path, base=None: {"fastestFee": 1})
    rate, _ = cw.recommended_fee_rate("http://stub")
    assert rate == cw.FEE_RATE_FALLBACK


def test_recommended_fee_rate_falls_back_when_the_estimate_is_unreachable(monkeypatch):
    def boom(path, base=None):
        raise RuntimeError("no network")
    monkeypatch.setattr(cw, "mempool_get", boom)
    rate, how = cw.recommended_fee_rate("http://stub")
    assert rate == cw.FEE_RATE_FALLBACK
    assert "unavailable" in how


def test_spawn_fee_matches_what_the_builder_charges():
    """The estimate the inviter is quoted must be the fee build_spawn_psbt
    actually deducts, or the quoted minimum is a lie."""
    from embit import ec as _ec
    xonly = _ec.PrivateKey(b"\x42" * 32).get_public_key().xonly()
    for rate in (1, 2, 9, 40):
        p, _ = cw.build_spawn_psbt(
            utxo_txid="ab" * 32, utxo_vout=0, utxo_value=50_000,
            utxo_script_pubkey=bytes([0x51, 0x20]) + xonly,
            funding_internal_xonly=xonly, funding_path="m/86h/0h/0h/0/0",
            funding_fingerprint=b"\x01\x02\x03\x04",
            snapshot={"life": 1, "rift": 0, "key": 1, "sponsor": None, "fief": None},
            fee_rate=rate,
        )
        assert 50_000 - sum(o.value for o in p.tx.vout) == cw.spawn_fee(rate, change=False)


def test_invite_amounts_grow_with_the_rate_and_leave_headroom():
    lo, hi = cw.invite_amounts(2), cw.invite_amounts(30)
    assert lo["minimum"] == cw.IDENTITY_SAT_VALUE + cw.spawn_fee(2, change=False)
    assert lo["suggested"] > lo["minimum"] + cw.P2TR_DUST      # room for a change output
    assert lo["suggested"] % 100 == 0
    assert hi["minimum"] > lo["minimum"] and hi["suggested"] > lo["suggested"]
    assert lo["headroom_rate"] >= 2 * 3 and hi["headroom_rate"] >= 30 * 3


# ---------------------------------------------------------------------------
# An invite-funded spawn: the inviter pays, the invitee owns
# ---------------------------------------------------------------------------


def _wallet(seed_byte: int):
    """A throwaway owner wallet: (root, KeySource) from a 12-word phrase."""
    mnemonic = cw.bip39.mnemonic_from_bytes(bytes([seed_byte]) * 16)
    root = cw.mnemonic_to_hdkey(mnemonic)
    path = [cw._hardened(86), cw._hardened(0), cw._hardened(0)]
    src = cw.KeySource(xpub=root.derive(cw._path_to_str(path)),
                       master_fingerprint=cw.hdkey_fingerprint(root),
                       account_path=path, network="main")
    return root, src


def _invite_utxo(source, value=5_000):
    addr, spk, xonly, path = source.derive_address(0, 0)
    return {"address": addr, "scriptpubkey": spk, "xonly": xonly, "path": path,
            "change": 0, "index": 0, "txid": "cd" * 32, "vout": 1, "value": value,
            "confirmed": True, "height": 900_000}


SNAP = {"life": 1, "rift": 0, "key": 0xABCD, "sponsor": None, "fief": None}


def test_invite_spawn_puts_the_identity_under_the_invitee_key_not_the_inviter():
    inv_root, inv_src = cw.invite_wallet(bytes(range(32)))
    owner_root, owner_src = _wallet(0x11)
    utxo = _invite_utxo(inv_src)

    p, proof = cw._build_spawn_psbt_and_proof(
        utxo=utxo, source=owner_src, snapshot=SNAP, fee_rate=2, funding_source=inv_src)

    # output 0 commits the snapshot under the OWNER's first receive key
    _a, _s, owner_xonly, owner_path = owner_src.derive_address(0, 0)
    assert proof["internal_pubkey_hex"] == owner_xonly.hex()
    q = cw.state_output_key(owner_xonly, SNAP)
    assert p.tx.vout[0].script_pubkey.data == bytes([0x51, 0x20]) + q
    assert proof["internal_pubkey_hex"] != utxo["xonly"].hex()
    # ... and the proof says where that key lives, for the next rekey
    assert proof["sat_key"] == {"path": owner_path,
                                "fingerprint_hex": owner_src.master_fingerprint.hex()}
    # while `funding` describes the inviter's UTXO, as it should
    assert proof["funding"]["fingerprint_hex"] == inv_src.master_fingerprint.hex()
    assert proof["funding"]["path"] == utxo["path"]
    # change goes to the owner's wallet, and the identity sat is exactly 1000
    _ca, change_spk, _cx, _cp = owner_src.derive_address(1, 0)
    assert p.tx.vout[-1].script_pubkey.data == change_spk
    assert p.tx.vout[0].value == cw.IDENTITY_SAT_VALUE
    assert p.tx.vout[-1].value == 5_000 - cw.IDENTITY_SAT_VALUE - cw.spawn_fee(2, change=True)
    # the change output claims no derivation: that metadata would name the
    # inviter's fingerprint for a key that is the owner's
    assert not p.outputs[-1].taproot_bip32_derivations
    # self-consistency, exactly as a self-funded proof
    ok, why = cw.verify_proof_self(proof)
    assert ok, why


def test_invite_spawn_is_signed_by_the_invite_key_and_only_that_key():
    inv_root, inv_src = cw.invite_wallet(bytes(range(32)))
    owner_root, owner_src = _wallet(0x11)
    p, _ = cw._build_spawn_psbt_and_proof(
        utxo=_invite_utxo(inv_src), source=owner_src, snapshot=SNAP, fee_rate=2,
        funding_source=inv_src)
    from embit import psbt as _p
    fresh = _p.PSBT.from_base64(p.to_base64())
    assert fresh.sign_with(owner_root) == 0        # the owner's seed cannot pay
    assert fresh.sign_with(inv_root) == 1          # the invite key can


def test_the_inviter_cannot_move_the_identity_sat_afterwards():
    """Teeth: a rekey built from an invite-funded proof derives the OWNER's key
    (sat_key), the owner's seed signs it, and the inviter's does not."""
    inv_root, inv_src = cw.invite_wallet(bytes(range(32)))
    owner_root, owner_src = _wallet(0x11)
    _p0, proof = cw._build_spawn_psbt_and_proof(
        utxo=_invite_utxo(inv_src), source=owner_src, snapshot=SNAP, fee_rate=2,
        funding_source=inv_src)
    proof["commit_txid"] = "ef" * 32
    new_snap = dict(SNAP, life=2, key=0xBEEF)
    p, _ = cw.build_rekey_psbt(prior_proof=proof, new_snapshot=new_snap, fee_rate=2)
    from embit import psbt as _p
    unsigned = p.to_base64()
    assert _p.PSBT.from_base64(unsigned).sign_with(inv_root) == 0
    assert _p.PSBT.from_base64(unsigned).sign_with(owner_root) == 1
    # and the PSBT's derivation names the owner's path, not the funding path
    (_pubs, deriv), = p.inputs[0].taproot_bip32_derivations.values()
    assert deriv.fingerprint == owner_src.master_fingerprint


def test_backfill_targets_sat_key_on_an_invite_proof():
    inv_root, inv_src = cw.invite_wallet(bytes(range(32)))
    owner_root, owner_src = _wallet(0x11)
    _p, proof = cw._build_spawn_psbt_and_proof(
        utxo=_invite_utxo(inv_src), source=owner_src, snapshot=SNAP, fee_rate=2,
        funding_source=inv_src)
    proof["sat_key"]["fingerprint_hex"] = "00000000"           # an old proof
    cw._backfill_funding_fingerprint(proof, owner_src)
    assert proof["sat_key"]["fingerprint_hex"] == owner_src.master_fingerprint.hex()
    assert proof["funding"]["fingerprint_hex"] == inv_src.master_fingerprint.hex()  # untouched


def test_self_funded_spawn_is_unchanged():
    """No invite: the funding key IS the identity key, no sat_key is recorded,
    and the change output keeps its derivation metadata."""
    owner_root, owner_src = _wallet(0x11)
    utxo = _invite_utxo(owner_src)
    p, proof = cw._build_spawn_psbt_and_proof(utxo=utxo, source=owner_src, snapshot=SNAP, fee_rate=2)
    assert proof["internal_pubkey_hex"] == utxo["xonly"].hex()
    assert "sat_key" not in proof
    assert p.outputs[-1].taproot_bip32_derivations
    assert cw.build_rekey_psbt(prior_proof=dict(proof, commit_txid="ef" * 32),
                               new_snapshot=dict(SNAP, life=2), fee_rate=2)


def test_invite_spawn_without_room_for_change_folds_into_the_sat():
    inv_root, inv_src = cw.invite_wallet(bytes(range(32)))
    _r, owner_src = _wallet(0x11)
    small = _invite_utxo(inv_src, value=cw.IDENTITY_SAT_VALUE + cw.spawn_fee(2, change=False) + 100)
    p, proof = cw._build_spawn_psbt_and_proof(
        utxo=small, source=owner_src, snapshot=SNAP, fee_rate=2, funding_source=inv_src)
    assert len(p.tx.vout) == 1
    assert p.tx.vout[0].value == small["value"] - cw.spawn_fee(2, change=False)
    assert proof["sat_key"]["fingerprint_hex"] == owner_src.master_fingerprint.hex()
