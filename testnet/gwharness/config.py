"""Harness configuration: load testnet/config.toml into a typed object."""

from __future__ import annotations

import tomllib
from dataclasses import dataclass
from pathlib import Path

HERE = Path(__file__).resolve().parent
TESTNET = HERE.parent
DEFAULT_CONFIG = TESTNET / "config.toml"


@dataclass
class Config:
    # paths
    gw_root: Path
    vere: Path
    comet_miner: Path
    pill: Path
    arvo_dir: str
    bitcoind: Path
    bitcoin_cli: Path
    groundwire: Path
    run: Path
    # bitcoind
    rpcport: int
    rpcuser: str
    rpcpass: str
    wallet: str
    attach: bool
    autominer_secs: int
    # net
    comets: int
    ames_base: int
    http_base: int
    star: str
    install_spv: bool
    # timeouts
    boot_ready: int
    fyrd: int
    verify: int

    # derived run paths
    @property
    def btc_datadir(self) -> Path: return self.run / "bitcoind"
    @property
    def piers_dir(self) -> Path: return self.run / "piers"
    @property
    def comets_dir(self) -> Path: return self.run / "comets"
    @property
    def logs_dir(self) -> Path: return self.run / "logs"
    @property
    def results_dir(self) -> Path: return self.run / "results"
    @property
    def dist_groundwire(self) -> Path: return self.groundwire / "dist-groundwire"
    @property
    def dist_spv(self) -> Path: return self.groundwire / "dist-spv"

    def ames_port(self, i: int) -> int: return self.ames_base + i
    def http_port(self, i: int) -> int: return self.http_base + i

    def ensure_dirs(self) -> None:
        for d in (self.run, self.btc_datadir, self.piers_dir,
                  self.comets_dir, self.logs_dir, self.results_dir):
            d.mkdir(parents=True, exist_ok=True)


def _interp(s: str, gw_root: str) -> str:
    return s.replace("{gw_root}", gw_root)


def load_config(path: Path | str = DEFAULT_CONFIG) -> Config:
    with open(path, "rb") as f:
        raw = tomllib.load(f)
    p, b, n, t = raw["paths"], raw["bitcoind"], raw["net"], raw["timeouts"]
    gw_root = p["gw_root"]

    def pth(key: str) -> Path:
        return Path(_interp(p[key], gw_root))

    return Config(
        gw_root=Path(gw_root),
        vere=pth("vere"),
        comet_miner=pth("comet_miner"),
        pill=pth("pill"),
        arvo_dir=_interp(p.get("arvo_dir", ""), gw_root),
        bitcoind=pth("bitcoind"),
        bitcoin_cli=pth("bitcoin_cli"),
        groundwire=pth("groundwire"),
        run=pth("run"),
        rpcport=b["rpcport"], rpcuser=b["rpcuser"], rpcpass=b["rpcpass"],
        wallet=b["wallet"], attach=b["attach"], autominer_secs=b["autominer_secs"],
        comets=n["comets"], ames_base=n["ames_base"], http_base=n["http_base"],
        star=n["star"], install_spv=n["install_spv"],
        boot_ready=t["boot_ready"], fyrd=t["fyrd"], verify=t["verify"],
    )
