"""bitcoind regtest lifecycle + RPC, on a dedicated datadir.

Never touches the user's ~/Library/Application Support/Bitcoin chainstate: we
run our own regtest node under testnet/run/bitcoind with explicit flags.
"""

from __future__ import annotations

import json
import subprocess
import time
import urllib.request
import urllib.error
from pathlib import Path

from .config import Config


class RpcError(RuntimeError):
    pass


class BitcoinRPC:
    def __init__(self, cfg: Config):
        self.url = f"http://127.0.0.1:{cfg.rpcport}/"
        self._auth = (cfg.rpcuser, cfg.rpcpass)
        self._id = 0

    def __call__(self, method: str, *params, wallet: str | None = None):
        self._id += 1
        url = self.url + (f"wallet/{wallet}" if wallet else "")
        body = json.dumps({
            "jsonrpc": "1.0", "id": self._id, "method": method, "params": list(params),
        }).encode()
        req = urllib.request.Request(url, data=body, method="POST")
        import base64
        tok = base64.b64encode(f"{self._auth[0]}:{self._auth[1]}".encode()).decode()
        req.add_header("Authorization", f"Basic {tok}")
        req.add_header("Content-Type", "text/plain")
        try:
            with urllib.request.urlopen(req, timeout=30) as resp:
                out = json.loads(resp.read())
        except urllib.error.HTTPError as e:
            try:
                out = json.loads(e.read())
            except Exception:
                raise RpcError(f"{method}: HTTP {e.code}")
        if out.get("error"):
            raise RpcError(f"{method}: {out['error']}")
        return out["result"]


def _pid_file(cfg: Config) -> Path:
    return cfg.btc_datadir / "regtest" / "bitcoind.pid"


def is_running(cfg: Config) -> bool:
    try:
        BitcoinRPC(cfg)("getblockchaininfo")
        return True
    except Exception:
        return False


def start_bitcoind(cfg: Config) -> None:
    """Idempotent: start regtest bitcoind (or attach to a running one)."""
    if is_running(cfg):
        chain = BitcoinRPC(cfg)("getblockchaininfo")["chain"]
        if chain != "regtest":
            raise RpcError(f"a non-regtest node ({chain}) is on port {cfg.rpcport}")
        return
    if cfg.attach:
        raise RpcError(f"attach=true but no node on port {cfg.rpcport}")
    cfg.btc_datadir.mkdir(parents=True, exist_ok=True)
    cmd = [
        str(cfg.bitcoind), "-regtest", f"-datadir={cfg.btc_datadir}",
        "-txindex=1", "-fallbackfee=0.0001", "-rpcbind=127.0.0.1",
        f"-rpcport={cfg.rpcport}", f"-rpcuser={cfg.rpcuser}",
        f"-rpcpassword={cfg.rpcpass}", "-rpcallowip=127.0.0.1",
        "-listen=0", "-daemonwait",
    ]
    subprocess.run(cmd, check=True, capture_output=True, timeout=60)
    for _ in range(40):
        if is_running(cfg):
            return
        time.sleep(0.5)
    raise RpcError("bitcoind did not come up")


def stop_bitcoind(cfg: Config) -> None:
    if not is_running(cfg):
        return
    try:
        BitcoinRPC(cfg)("stop")
    except Exception:
        pass
    for _ in range(40):
        if not is_running(cfg):
            return
        time.sleep(0.5)


def ensure_wallet(rpc: BitcoinRPC, name: str) -> None:
    wallets = rpc("listwallets")
    if name in wallets:
        return
    try:
        rpc("createwallet", name, False, False, "", False, True)  # descriptor wallet
    except RpcError:
        rpc("loadwallet", name)


def address(rpc: BitcoinRPC, wallet: str, kind: str = "bech32m") -> str:
    return rpc("getnewaddress", "", kind, wallet=wallet)


def mine(rpc: BitcoinRPC, wallet: str, n: int = 1) -> list[str]:
    addr = address(rpc, wallet, "bech32m")
    return rpc("generatetoaddress", n, addr)


def ensure_maturity(rpc: BitcoinRPC, wallet: str, min_btc: float = 50.0) -> None:
    bal = rpc("getbalance", wallet=wallet)
    if bal < min_btc:
        mine(rpc, wallet, 101)


def confirm(rpc: BitcoinRPC, wallet: str, n: int = 2) -> list[str]:
    """Mine n blocks. The watcher uses block-confirmations=1, so n>=2 makes a
    freshly-broadcast tx visible to it."""
    return mine(rpc, wallet, n)


def fund_address(rpc: BitcoinRPC, wallet: str, addr: str, sats: int,
                 conf: int = 2) -> tuple[str, int]:
    """Send `sats` to addr, mine `conf` blocks, return (txid, vout)."""
    btc = sats / 1e8
    txid = rpc("sendtoaddress", addr, f"{btc:.8f}", wallet=wallet)
    confirm(rpc, wallet, conf)
    tx = rpc("getrawtransaction", txid, True)
    for o in tx["vout"]:
        spk = o.get("scriptPubKey", {})
        if addr in (spk.get("address"), *(spk.get("addresses") or [])):
            return txid, o["n"]
    raise RpcError(f"funded {addr} but no matching output in {txid}")


def get_tip(rpc: BitcoinRPC) -> tuple[str, int]:
    info = rpc("getblockchaininfo")
    return info["bestblockhash"], info["blocks"]


def block_hash_at(rpc: BitcoinRPC, height: int) -> str:
    return rpc("getblockhash", height)


def invalidate(rpc: BitcoinRPC, blockhash: str) -> None:
    rpc("invalidateblock", blockhash)


def reconsider(rpc: BitcoinRPC, blockhash: str) -> None:
    rpc("reconsiderblock", blockhash)
