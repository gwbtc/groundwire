"""The spawn's change, moved into the ship's Wallet (`causeway sweep`).

The properties that matter: the sweep spends exactly the recorded change
outpoint and refuses output 0 and any key that does not spend that script;
a buffer stays with the seed; a change too small to split stays with the
seed whole; and the key file holds the change key alone, never the seed.
"""

import json
import os
import stat
import sys

import pytest
from embit import bip32, ec, script
from embit.transaction import Transaction

sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))

import causeway as cw  # noqa: E402


SEED = bytes(range(64))
ROOT = bip32.HDKey.from_seed(SEED)
CHANGE_PATH = "m/86h/0h/0h/1/0"
CHANGE = ROOT.derive(CHANGE_PATH)
CHANGE_SPK = script.p2tr(CHANGE.key.get_public_key()).data
CHANGE_ADDR = script.p2tr(CHANGE.key.get_public_key()).address()
OTHER = ROOT.derive("m/86h/0h/0h/0/7")
SHIP_ADDR = script.p2tr(OTHER.key.get_public_key()).address()
TXID = "ab" * 32


def _build(value, keep=cw.SWEEP_KEEP, fee_rate=3, to=SHIP_ADDR, spk=CHANGE_SPK, key=CHANGE, vout=1):
    return cw.build_sweep_tx(
        txid=TXID, vout=vout, value=value, script_pubkey_hex=spk.hex(),
        privkey_hex=key.secret.hex(), to_address=to, keep_address=CHANGE_ADDR,
        fee_rate=fee_rate, keep=keep)


def test_sweep_splits_the_change_and_signs_with_the_change_key():
    got = _build(20_000)
    tx = Transaction.parse(bytes.fromhex(got["tx_hex"]))
    assert len(tx.vin) == 1 and tx.vin[0].txid == bytes.fromhex(TXID) and tx.vin[0].vout == 1
    assert [o.value for o in tx.vout] == [got["sent"], got["kept"]]
    assert got["kept"] == cw.SWEEP_KEEP
    assert got["fee"] == cw.SWEEP_VB_TWO * 3
    assert got["sent"] + got["kept"] + got["fee"] == 20_000
    assert tx.vout[0].script_pubkey == script.address_to_scriptpubkey(SHIP_ADDR)
    assert tx.vout[1].script_pubkey.data == CHANGE_SPK
    assert got["txid"] == tx.txid().hex() and len(got["txid"]) == 64
    # the one witness item is a 64-byte schnorr signature by the TWEAKED
    # change key over the taproot sighash, which is what the output key
    # in the script commits to
    sig = ec.SchnorrSig.parse(tx.vin[0].witness.items[0])
    digest = tx.sighash_taproot(0, [script.Script(CHANGE_SPK)], [20_000])
    tweaked = ec.PrivateKey(CHANGE.secret).taproot_tweak(b"").get_public_key()
    assert tweaked.xonly() == CHANGE_SPK[2:]
    assert tweaked.schnorr_verify(sig, digest)


def test_sweep_without_a_buffer_has_one_output_and_the_smaller_fee():
    got = _build(20_000, keep=0, fee_rate=2)
    tx = Transaction.parse(bytes.fromhex(got["tx_hex"]))
    assert len(tx.vout) == 1 and got["kept"] == 0
    assert got["fee"] == cw.SWEEP_VB_ONE * 2
    assert got["sent"] == 20_000 - got["fee"]


def test_a_change_too_small_to_split_stays_with_the_seed():
    with pytest.raises(cw.NothingToSweep):
        _build(5_000 + cw.SWEEP_VB_TWO * 3 + cw.P2TR_DUST - 1)
    # exactly enough goes through
    got = _build(5_000 + cw.SWEEP_VB_TWO * 3 + cw.P2TR_DUST)
    assert got["sent"] == cw.P2TR_DUST


def test_a_buffer_below_dust_is_raised_to_dust():
    got = _build(20_000, keep=100)
    assert got["kept"] == cw.P2TR_DUST


def test_the_identity_sat_is_never_spent():
    with pytest.raises(ValueError, match="output 0"):
        _build(20_000, vout=0)
    with pytest.raises(ValueError, match="output 0"):
        cw.write_change_key("/nonexistent", "~zod", network="main", txid=TXID, vout=0,
                            value=1, address=CHANGE_ADDR, path=CHANGE_PATH, hdkey=CHANGE)


def test_a_key_that_does_not_spend_the_script_is_refused():
    with pytest.raises(ValueError, match="does not spend"):
        _build(20_000, key=OTHER)


def test_find_change_output_skips_output_zero():
    got = _build(20_000)
    # a transaction whose output 0 pays the change script: still not change
    tx = Transaction.parse(bytes.fromhex(got["tx_hex"]))
    tx.vout = [tx.vout[1], tx.vout[0]]
    assert cw.find_change_output(tx.serialize().hex(), CHANGE_SPK) is None
    assert cw.find_change_output(got["tx_hex"], CHANGE_SPK) == (1, got["kept"])
    assert cw.find_change_output(got["tx_hex"], b"\x00" * 34) is None


def test_the_change_key_file_holds_one_key_and_not_the_seed(tmp_path):
    p = cw.write_change_key(str(tmp_path), "~sampel-palnet", network="main", txid=TXID, vout=1,
                            value=12_345, address=CHANGE_ADDR, path=CHANGE_PATH, hdkey=CHANGE)
    assert p == str(tmp_path / "sampel-palnet-change.key.json")
    assert stat.S_IMODE(os.stat(p).st_mode) == 0o600
    doc = json.load(open(p))
    assert doc["privkey_hex"] == CHANGE.secret.hex()
    assert doc["script_pubkey_hex"] == CHANGE_SPK.hex()
    assert doc["txid"] == TXID and doc["vout"] == 1 and doc["value"] == 12_345
    assert "mnemonic" not in json.dumps(doc) and SEED.hex() not in json.dumps(doc)
    # the account key is not derivable from what is written: a hardened
    # ancestor's secret is not in the file
    assert ROOT.derive("m/86h/0h/0h").secret.hex() not in json.dumps(doc)
    loaded = cw.load_change_key(p)
    assert loaded["address"] == CHANGE_ADDR
    # and the loaded file builds the same transaction as the key itself
    got = cw.build_sweep_tx(txid=loaded["txid"], vout=loaded["vout"], value=loaded["value"],
                            script_pubkey_hex=loaded["script_pubkey_hex"],
                            privkey_hex=loaded["privkey_hex"], to_address=SHIP_ADDR,
                            keep_address=loaded["address"], fee_rate=3)
    assert got["sent"] == 12_345 - got["fee"] - cw.SWEEP_KEEP


def test_run_sweep_leaves_the_file_when_the_network_has_not_seen_the_spawn(tmp_path, monkeypatch, capsys):
    p = cw.write_change_key(str(tmp_path), "~zod", network="main", txid=TXID, vout=1,
                            value=20_000, address=CHANGE_ADDR, path=CHANGE_PATH, hdkey=CHANGE)
    monkeypatch.setattr(cw, "mempool_get", lambda *a, **k: (_ for _ in ()).throw(RuntimeError("404")))
    assert cw.run_sweep(p, SHIP_ADDR) == 1
    assert os.path.exists(p)


def test_run_sweep_broadcasts_and_removes_the_file(tmp_path, monkeypatch):
    p = cw.write_change_key(str(tmp_path), "~zod", network="main", txid=TXID, vout=1,
                            value=20_000, address=CHANGE_ADDR, path=CHANGE_PATH, hdkey=CHANGE)
    sent = {}
    monkeypatch.setattr(cw, "mempool_get", lambda path, **k: {"spent": False})
    monkeypatch.setattr(cw, "recommended_fee_rate", lambda *a, **k: (3, "test"))

    def fake_broadcast(tx_hex, **k):
        sent["hex"] = tx_hex
        return Transaction.parse(bytes.fromhex(tx_hex)).txid().hex()
    monkeypatch.setattr(cw, "_broadcast_tx", fake_broadcast)
    assert cw.run_sweep(p, SHIP_ADDR) == 0
    assert not os.path.exists(p)
    receipt = json.load(open(tmp_path / "zod-sweep.json"))
    assert receipt["sent"] + receipt["kept"] + receipt["fee"] == 20_000
    assert receipt["to"] == SHIP_ADDR and receipt["kept_at"] == CHANGE_ADDR
    assert "privkey_hex" not in receipt
    tx = Transaction.parse(bytes.fromhex(sent["hex"]))
    assert tx.txid().hex() == receipt["txid"]


def test_run_sweep_keeps_everything_when_the_change_is_small(tmp_path, monkeypatch):
    p = cw.write_change_key(str(tmp_path), "~zod", network="main", txid=TXID, vout=1,
                            value=4_000, address=CHANGE_ADDR, path=CHANGE_PATH, hdkey=CHANGE)
    monkeypatch.setattr(cw, "mempool_get", lambda path, **k: {"spent": False})
    monkeypatch.setattr(cw, "recommended_fee_rate", lambda *a, **k: (3, "test"))
    monkeypatch.setattr(cw, "_broadcast_tx", lambda *a, **k: pytest.fail("nothing must be broadcast"))
    assert cw.run_sweep(p, SHIP_ADDR) == 0
    assert not os.path.exists(p)
    assert not (tmp_path / "zod-sweep.json").exists()
