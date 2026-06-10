"""bitcoind regtest plumbing tests, run against the real binary.

Boots a throwaway regtest node on an ephemeral port with a tmp datadir,
exercises the harness RPC/wallet/mining helpers, and tears it down.
Skipped if the configured bitcoind binary is missing.
"""

from __future__ import annotations

import dataclasses
import socket
from pathlib import Path

import pytest

from gwharness import btc
from gwharness.config import load_config

try:
    BASE_CFG = load_config()
    BITCOIND = Path(BASE_CFG.bitcoind)
except Exception:  # pragma: no cover
    BASE_CFG = None
    BITCOIND = Path("/opt/homebrew/bin/bitcoind")

pytestmark = pytest.mark.skipif(
    BASE_CFG is None or not BITCOIND.exists(),
    reason=f"bitcoind not found at {BITCOIND}",
)


def _free_port() -> int:
    with socket.socket() as s:
        s.bind(("127.0.0.1", 0))
        return s.getsockname()[1]


@pytest.fixture(scope="module")
def node(tmp_path_factory):
    run = tmp_path_factory.mktemp("btc-run")
    cfg = dataclasses.replace(BASE_CFG, run=run, rpcport=_free_port())
    btc.start_bitcoind(cfg)
    rpc = btc.BitcoinRPC(cfg)
    try:
        yield cfg, rpc
    finally:
        btc.stop_bitcoind(cfg)


def test_regtest_chain(node):
    _, rpc = node
    info = rpc("getblockchaininfo")
    assert info["chain"] == "regtest"


def test_start_is_idempotent(node):
    cfg, _ = node
    btc.start_bitcoind(cfg)  # second call attaches, doesn't crash
    assert btc.is_running(cfg)


def test_wallet_and_maturity(node):
    cfg, rpc = node
    btc.ensure_wallet(rpc, cfg.wallet)
    btc.ensure_wallet(rpc, cfg.wallet)  # idempotent
    assert cfg.wallet in rpc("listwallets")
    btc.ensure_maturity(rpc, cfg.wallet)
    assert rpc("getbalance", wallet=cfg.wallet) >= 50


def test_mine_advances_tip(node):
    cfg, rpc = node
    _, height0 = btc.get_tip(rpc)
    hashes = btc.mine(rpc, cfg.wallet, 3)
    assert len(hashes) == 3
    tip, height1 = btc.get_tip(rpc)
    assert height1 == height0 + 3
    assert tip == hashes[-1]


def test_fund_address(node):
    cfg, rpc = node
    btc.ensure_maturity(rpc, cfg.wallet)
    addr = btc.address(rpc, cfg.wallet, "bech32m")
    assert addr.startswith("bcrt1p")  # regtest taproot
    txid, vout = btc.fund_address(rpc, cfg.wallet, addr, 123_456)
    tx = rpc("getrawtransaction", txid, True)
    out = tx["vout"][vout]
    assert out["scriptPubKey"]["address"] == addr
    assert round(out["value"] * 1e8) == 123_456
    assert tx["confirmations"] >= 2  # fund_address mines 2 conf blocks


def test_reorg_helpers(node):
    cfg, rpc = node
    btc.mine(rpc, cfg.wallet, 2)
    tip, height = btc.get_tip(rpc)
    prev = btc.block_hash_at(rpc, height - 1)
    btc.invalidate(rpc, prev)
    _, h_after = btc.get_tip(rpc)
    assert h_after == height - 2
    btc.reconsider(rpc, prev)
    tip2, h_restored = btc.get_tip(rpc)
    assert h_restored == height and tip2 == tip
