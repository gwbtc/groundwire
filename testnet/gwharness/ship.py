"""Comet lifecycle: mine a real suite-C identity, boot a pier, install the
groundwire desk, point %urb-watcher at the regtest node, and drive it.

Everything is real every run (per the harness design): real mining, real piers.
"""

from __future__ import annotations

import re
import shutil
import signal
import subprocess
import time
from dataclasses import dataclass, field
from pathlib import Path

from .config import Config
from .connsock import ConnSock
from . import noun as N
from . import obphon


def make_tweak_expr(txid_hex: str, vout: int, off: int = 0) -> str:
    """The hoon tweak expression comet_miner evaluates (matches urb-core /
    lib/self-attestation): (rap 3 ~[%9 ~tyr %urb-watcher %btc %gw %9 <txid> vout off]).
    txid is the display-order hex as a hoon @ux (dotted every 4 hex digits)."""
    return (
        f"(rap 3 ~[%9 ~tyr %urb-watcher %btc %gw %9 {_hoon_ux(txid_hex)} {vout} {off}])"
    )


def _hoon_ux(hex_str: str) -> str:
    h = hex_str.lower().lstrip("0x") or "0"
    chunks = []
    while len(h) > 4:
        chunks.append(h[-4:]); h = h[:-4]
    chunks.append(h)
    return "0x" + ".".join(reversed(chunks))


@dataclass
class MinedComet:
    patp: str
    num: int
    feed: str
    ring: str
    seed: str


def mine_comet(cfg: Config, tweak_expr: str, star: str | None = None) -> MinedComet:
    star = star or cfg.star
    out = subprocess.run(
        [str(cfg.comet_miner), "-c", "--tweak", tweak_expr, star],
        capture_output=True, text=True, timeout=600,
    )
    if out.returncode != 0:
        raise RuntimeError(f"comet_miner failed: {out.stdout[-500:]}\n{out.stderr[-500:]}")
    fields = {}
    for line in out.stdout.splitlines():
        m = re.match(r"^(seed|ring|feed|comet):\s*(.+)$", line.strip())
        if m:
            fields[m.group(1)] = m.group(2).strip()
    for req in ("feed", "comet", "ring"):
        if req not in fields:
            raise RuntimeError(f"comet_miner produced no '{req}': {out.stdout[-400:]}")
    patp = fields["comet"]
    return MinedComet(patp=patp, num=obphon.patp_to_num(patp),
                      feed=fields["feed"], ring=fields["ring"],
                      seed=fields.get("seed", ""))


@dataclass
class Comet:
    cfg: Config
    index: int
    mined: MinedComet
    proc: subprocess.Popen | None = None
    _conn: ConnSock | None = field(default=None, repr=False)

    @property
    def patp(self) -> str: return self.mined.patp
    @property
    def num(self) -> int: return self.mined.num
    @property
    def pier(self) -> Path: return self.cfg.piers_dir / self.patp.lstrip("~")
    @property
    def log(self) -> Path: return self.cfg.logs_dir / f"{self.patp.lstrip('~')}.log"
    @property
    def ames_port(self) -> int: return self.cfg.ames_port(self.index)
    @property
    def http_port(self) -> int: return self.cfg.http_port(self.index)

    @property
    def conn(self) -> ConnSock:
        if self._conn is None:
            self._conn = ConnSock(self.pier, timeout=self.cfg.fyrd)
        return self._conn

    # -- boot ---------------------------------------------------------------

    def boot(self) -> None:
        if self.pier.exists():
            raise RuntimeError(f"pier already exists: {self.pier}")
        self.pier.parent.mkdir(parents=True, exist_ok=True)
        cmd = [
            str(self.cfg.vere), "-c", str(self.pier), "-G", self.mined.feed,
            "-B", str(self.cfg.pill), "-L", "-p", str(self.ames_port),
            "--http-port", str(self.http_port), "-t",
        ]
        if self.cfg.arvo_dir:
            cmd += ["-A", self.cfg.arvo_dir]
        with open(self.log, "wb") as logf:
            self.proc = subprocess.Popen(cmd, stdout=logf, stderr=subprocess.STDOUT)

    def wait_ready(self) -> bool:
        return self.conn.wait_ready(self.cfg.boot_ready)

    def our(self) -> int:
        """Numeric @p straight from the ship (the source of truth)."""
        r = self.conn.khan_eval(
            "=/  m  (strand ,vase)  ;<  our=@p  bind:m  get-our  (pure:m !>(our))")
        return r

    # -- desk install -------------------------------------------------------

    def install_desk(self, desk: str, dist: Path, kelvin: int = 408) -> None:
        """Merge %base -> desk, mount, replace with `dist`, commit, install."""
        c = self.conn
        c.poke_our("hood", "kiln-merge",
                   f"!>([%{desk} our %base [%da now] %init])")
        # mount and wait for sys.kelvin to appear
        c.poke_our("hood", "kiln-mount",
                   f"!>([(en-beam [[our %{desk} [%da now]] /]) %{desk}])")
        self._await_path(self.pier / desk / "sys.kelvin", 90)
        # replace mounted contents with the built dist
        mount = self.pier / desk
        for child in mount.iterdir():
            if child.is_dir():
                shutil.rmtree(child)
            else:
                child.unlink()
        subprocess.run(["rsync", "-a", f"{dist}/", f"{mount}/"], check=True)
        c.poke_our("hood", "kiln-commit", f"!>([%{desk} %.n])")
        time.sleep(3)
        c.poke_our("hood", "kiln-install", f"!>([%{desk} our %{desk}])")

    def _await_path(self, p: Path, timeout: float) -> None:
        deadline = time.time() + timeout
        while time.time() < deadline:
            if p.exists():
                return
            time.sleep(2)
        raise TimeoutError(f"{p} never appeared")

    # -- watcher config -----------------------------------------------------

    def config_watcher(self, url: str, auth: str, block_hash: str, height: int) -> None:
        """Point %urb-watcher at the regtest node and (re)start its block loop."""
        vase = (f"!>([%watcher-config '{url}' '{auth}' {_hoon_ux(block_hash)} {height}])")
        self.conn.poke_our("urb-watcher", "noun", vase)

    def peek(self, agent: str, spur: list[str]):
        return self.conn.peek_gall(agent, spur)

    # -- teardown -----------------------------------------------------------

    def kill(self) -> None:
        lock = self.pier / ".vere.lock"
        if lock.exists():
            try:
                pid = int(lock.read_text().strip())
                import os
                os.kill(pid, signal.SIGTERM)
            except (ValueError, ProcessLookupError, PermissionError):
                pass
        if self.proc and self.proc.poll() is None:
            try:
                self.proc.terminate()
            except ProcessLookupError:
                pass
