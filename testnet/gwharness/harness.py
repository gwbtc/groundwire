"""Net orchestration: bring up regtest, boot real comets with the groundwire
desk installed and %urb-watcher pointed at the local node, and tear down.
"""

from __future__ import annotations

import subprocess
import time
from dataclasses import dataclass, field

from .config import Config
from . import btc as _btc
from . import chainops, packets
from .ship import Comet, MinedComet
from . import btc


def make_dist(cfg: Config) -> None:
    """Build dist-groundwire / dist-spv from the desk sources."""
    subprocess.run(["make", "build"], cwd=str(cfg.groundwire), check=True,
                   capture_output=True, timeout=120)


@dataclass
class Net:
    cfg: Config
    rpc: btc.BitcoinRPC = field(init=False)
    comets: list[Comet] = field(default_factory=list)

    def __post_init__(self):
        self.rpc = btc.BitcoinRPC(self.cfg)

    # -- lifecycle ----------------------------------------------------------

    def up(self) -> None:
        self.cfg.ensure_dirs()
        _btc.start_bitcoind(self.cfg)
        _btc.ensure_wallet(self.rpc, self.cfg.wallet)
        _btc.ensure_maturity(self.rpc, self.cfg.wallet)
        make_dist(self.cfg)

    def down(self, keep_chain: bool = False) -> None:
        for c in self.comets:
            c.kill()
        if not keep_chain:
            _btc.stop_bitcoind(self.cfg)

    # -- comets -------------------------------------------------------------

    def spawn_identity(self, **kw) -> chainops.Identity:
        return chainops.spawn(self.cfg, self.rpc, **kw)

    def boot_comet(self, ident: chainops.Identity, index: int,
                   install: bool = True, arvo: str | None = None) -> Comet:
        mined = MinedComet(patp=ident.comet, num=_patp_num(ident.comet),
                           feed=ident.feed, ring=ident.ring, seed="")
        c = Comet(cfg=self.cfg, index=index, mined=mined, arvo=arvo)
        self.comets.append(c)
        c.boot()
        if not c.wait_ready():
            raise RuntimeError(f"{c.patp} never became ready")
        if install:
            c.install_desk("groundwire", self.cfg.dist_groundwire)
            self._configure_watcher(c)
        return c

    def _configure_watcher(self, c: Comet) -> None:
        h, height = _btc.get_tip(self.rpc)
        # start a couple of blocks back so the watcher's loop has settled room
        start = max(1, height - 1)
        start_hash = _btc.block_hash_at(self.rpc, start)
        auth = f"{self.cfg.rpcuser}:{self.cfg.rpcpass}"
        url = f"http://127.0.0.1:{self.cfg.rpcport}/"
        # give the agent a moment to be installed/running
        time.sleep(3)
        c.config_watcher(url, auth, start_hash, start)

    def keyfile(self, c: Comet, ident: chainops.Identity) -> object:
        skel = chainops.build_skeleton(self.cfg, ident)
        return packets.poke_keyfile(c, skel)

    def peer(self, c: Comet, ident: chainops.Identity) -> object:
        """Deliver `ident`'s packet to comet `c` as a peer (%self-attestation),
        as Ames would on first contact. On VALID, c's urb-watcher feeds Jael,
        which installs the peer on c's ames (clearing any suite-C attest hold)."""
        skel = chainops.build_skeleton(self.cfg, ident)
        return packets.poke_peer(c, skel)


def _patp_num(patp: str) -> int:
    from . import obphon
    return obphon.patp_to_num(patp)
