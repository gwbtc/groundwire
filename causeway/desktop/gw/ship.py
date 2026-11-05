"""One ship: its pier, its processes, and the questions we ask it.

Everything the installer used to grep out of the console is a scry here:
%bitcoin-client answers ``/is-synced``, ``/best-block`` and ``/peers``,
%gw-btc answers ``/ready``.  A conn.sock ``%peek`` is an arvo scry, not a
thread, so an agent that is not there answers nothing instead of taking
%spider down with it.
"""

from __future__ import annotations

import os
import re
import shutil
import signal
import subprocess
import time
from pathlib import Path

from . import noun as N
from .connsock import ConnSock, ConnError, GoofError
from .layout import GW_DIR, GwError, ShipConfig, pill_file, piers_dir, var_dir, vere_bin

STRAND_HEAD = (
    "=/  m  (strand ,vase)\n"
    "^-  form:m\n"
    ";<  our=@p   bind:m  get-our\n"
    ";<  now=@da  bind:m  get-time\n"
)


def strand(body: str) -> str:
    """A strand body with ``our`` and ``now`` bound, as gwlib's envelope did."""
    return STRAND_HEAD + body.rstrip("\n") + "\n"


def hoon_hex(h: str) -> str:
    """0x literal, dot-grouped every four hex digits from the right."""
    h = h.lower().lstrip("0") or "0"
    groups = []
    while len(h) > 4:
        groups.insert(0, h[-4:])
        h = h[:-4]
    return "0x" + ".".join([h] + groups)


def ip_hoon(ip: str) -> str:
    a, b, c, d = (int(x) for x in ip.split("."))
    return hoon_hex(f"{(a << 24) | (b << 16) | (c << 8) | d:x}")


class Ship:
    def __init__(self, cfg: ShipConfig):
        self.cfg = cfg
        self.name = cfg.name

    # ------------------------------------------------------------ places --
    @property
    def pier(self) -> Path:
        p = piers_dir() / self.name
        if not p.exists() and (GW_DIR / self.name / ".urb").exists():
            return GW_DIR / self.name          # the pre-2026.8 flat layout
        return p

    @property
    def exists(self) -> bool:
        return (self.pier / ".urb").exists()

    @property
    def log(self) -> Path:
        p = var_dir() / f"{self.name}.log"
        if not p.exists() and (GW_DIR / f"{self.name}.log").exists():
            return GW_DIR / f"{self.name}.log"
        return p

    @property
    def sock(self) -> Path:
        return self.pier / ".urb" / "conn.sock"

    def marker(self, what: str) -> Path:
        return var_dir() / f"{self.name}.{what}"

    # --------------------------------------------------------- processes --
    def _ps(self) -> list[tuple[int, str]]:
        try:
            out = subprocess.run(["ps", "-eo", "pid=,args="], capture_output=True, text=True,
                                 timeout=20).stdout
        except Exception:
            return []
        rows = []
        for line in out.splitlines():
            line = line.strip()
            if not line:
                continue
            pid, _, args = line.partition(" ")
            if pid.isdigit():
                rows.append((int(pid), args))
        return rows

    def king_pid(self) -> int | None:
        pier = str(self.pier)
        for pid, args in self._ps():
            if "gw-vere" in args and "--snap-dir" not in args and pier in args.split():
                return pid
        return None

    def serf_pid(self) -> int | None:
        pier = str(self.pier)
        for pid, args in self._ps():
            parts = args.split()
            if "--snap-dir" in parts:
                i = parts.index("--snap-dir")
                if i + 1 < len(parts) and parts[i + 1] == pier:
                    return pid
        return None

    def running(self) -> bool:
        return self.king_pid() is not None or self.serf_pid() is not None

    def evt_age(self) -> int:
        """Seconds since LMDB last wrote; 99999 when there is no log yet."""
        newest = 0.0
        for f in (self.pier / ".urb" / "log").glob("*/data.mdb"):
            try:
                newest = max(newest, f.stat().st_mtime)
            except OSError:
                pass
        return 99999 if not newest else int(time.time() - newest)

    # ---------------------------------------------------------- vere argv --
    def restart_argv(self) -> list[str]:
        args = [str(vere_bin()), "-t", "--loom", str(self.cfg.loom)]
        if self.cfg.ames_port:
            args += ["-p", str(self.cfg.ames_port)]
        args += ["--http-port", str(self.cfg.http_port), str(self.pier)]
        return args

    def fresh_argv(self, feed: str) -> list[str]:
        args = [str(vere_bin()), "-t", "--loom", str(self.cfg.loom),
                "-c", str(self.pier), "-w", self.name, "-G", feed, "-B", str(pill_file()),
                "--http-port", str(self.cfg.http_port), "--gateway", self.cfg.gateway]
        if self.cfg.ames_port:
            args += ["-p", str(self.cfg.ames_port)]
        return args

    def spawn_detached(self, argv: list[str]) -> int:
        """A plain background process: where there is no service manager."""
        self.log.parent.mkdir(parents=True, exist_ok=True)
        (self.pier / ".vere.lock").unlink(missing_ok=True)
        with open(self.log, "ab") as out:
            p = subprocess.Popen(argv, stdout=out, stderr=subprocess.STDOUT,
                                 stdin=subprocess.DEVNULL, start_new_session=True,
                                 cwd=str(self.pier.parent))
        return p.pid

    def terminate(self, grace: int = 30) -> bool:
        """SIGTERM the king, wait, SIGKILL both.  True when nothing is left.
        A killed vere loses nothing: the event log is durable."""
        k = self.king_pid()
        if k:
            try:
                os.kill(k, signal.SIGTERM)
            except ProcessLookupError:
                pass
        waited = 0
        while self.running() and waited < grace:
            time.sleep(2)
            waited += 2
        if self.running():
            for pid in (self.king_pid(), self.serf_pid()):
                if pid:
                    try:
                        os.kill(pid, signal.SIGKILL)
                    except ProcessLookupError:
                        pass
            w2 = 0
            while self.running() and w2 < 10:
                time.sleep(1)
                w2 += 1
        return not self.running()

    # ------------------------------------------------------ control sock --
    def conn(self, timeout: float = 120.0) -> ConnSock:
        return ConnSock(self.pier, timeout=timeout)

    def khan(self, body: str, timeout: float = 120.0):
        """Run a strand body (``our``/``now`` bound); returns the product noun."""
        return self.conn(timeout).khan_eval(strand(body))

    def poke(self, agent: str, mark: str, vase_expr: str, timeout: float = 600.0) -> None:
        """Poke a local agent.  Waits for the ack: a client that hangs up early
        used to crash the king on runtimes without the conn fix."""
        self.conn(timeout).poke_our(agent, mark, vase_expr)

    def peek(self, agent: str, path: str, timeout: float = 60.0):
        """``/gx/<agent>/<path>`` as an arvo peek.  None when the agent or the
        path is not there; never a crash on the ship."""
        spur = [k for k in path.strip("/").split("/") if k]
        if not spur or spur[-1] != "noun":
            spur.append("noun")
        try:
            return self.conn(timeout).peek_gall(agent, spur)
        except (ConnError, OSError, ValueError):
            return None

    def answers(self, timeout: float = 20.0) -> bool:
        if not self.sock.exists():
            return False
        try:
            return self.conn(timeout).khan_eval(
                "=/  m  (strand ,vase)  (pure:m !>('ok'))") == N.cord("ok")
        except Exception:
            return False

    def our(self, timeout: float = 120.0) -> str:
        try:
            r = self.khan("(pure:m !>((scot %p our)))", timeout)
        except Exception:
            return ""
        return N.from_cord(r) if isinstance(r, int) else ""

    def code(self, timeout: float = 90.0) -> str:
        r = self.khan("(pure:m !>((crip (slag 1 (scow %p .^(@p %j /(scot %p our)/code/"
                      "(scot %da now)/(scot %p our)))))))", timeout)
        s = N.from_cord(r) if isinstance(r, int) else ""
        m = re.search(r"[a-z]{6}(-[a-z]{6}){3}", s)
        return m.group(0) if m else ""

    def desks(self, timeout: float = 120.0) -> list[str]:
        r = self.khan("(pure:m !>(~(tap in .^((set desk) %cd /(scot %p our)/base/(scot %da now)))))",
                      timeout)
        return [N.from_tas(d) for d in N.list_iter(r)]

    def has_desk(self, desk: str) -> bool:
        try:
            return desk in self.desks()
        except Exception:
            return False

    def agent_running(self, dude: str, timeout: float = 120.0) -> bool:
        """gall's %u care with the trailing /$ : a loobean, %.n for an agent
        that is not there at all, never a crash."""
        try:
            r = self.khan(f"(pure:m !>(.^(? %gu /(scot %p our)/{dude}/(scot %da now)/$)))", timeout)
        except Exception:
            return False
        return r == 0

    def wait_agent(self, dude: str, tries: int = 18, every: int = 10) -> bool:
        for _ in range(tries):
            if self.agent_running(dude):
                return True
            time.sleep(every)
        return False

    def wait_up(self, limit: int = 3600, on_tick=None) -> None:
        """Until the ship answers over conn.sock; raises with the log's tail
        when vere died or the feed was refused."""
        waited = 0
        while waited < limit:
            if self.answers(30):
                return
            tail = self._log_tail(400)
            if "dawn: invalid private keys" in tail:
                raise GwError("vere refused the feed: 'dawn: invalid private keys'. The feed must be "
                              f"the @uw (0w...) Causeway finalized. See {self.log}")
            if "bind: address already in use" in tail:
                raise GwError(f"another process holds this ship's UDP port. See {self.log}")
            if waited > 20 and not self.running():
                raise GwError("gw-vere exited during boot. The last lines of "
                              f"{self.log}:\n" + "\n".join(self._log_tail(15).splitlines()[-15:]))
            if on_tick:
                on_tick(waited)
            time.sleep(5)
            waited += 5
        raise GwError(f"the ship did not come up within {limit // 60} minutes. See {self.log}")

    def _log_tail(self, lines: int) -> str:
        try:
            with open(self.log, "rb") as fh:
                fh.seek(0, 2)
                size = fh.tell()
                fh.seek(max(0, size - 256 * lines))
                return fh.read().decode("utf-8", "replace")
        except OSError:
            return ""

    # ---------------------------------------------------------- the facts --
    #  %bitcoin-client answers each peek with a bitcoin-client-update:
    #    [%is-synced ?]   [%best-block %new height hash]   [%peers %all (map ...)]
    def synced(self) -> bool | None:
        r = self.peek("bitcoin-client", "/is-synced")
        if not isinstance(r, tuple) or r[0] != N.tas("is-synced"):
            return None
        return r[1] == 0        # a loobean: 0 is yes

    def best_block(self) -> int | None:
        r = self.peek("bitcoin-client", "/best-block")
        try:
            if r[0] == N.tas("best-block") and r[1][0] == N.tas("new"):
                return int(r[1][1][0])
        except (TypeError, IndexError):
            pass
        return None

    def peer_count(self) -> int | None:
        r = self.peek("bitcoin-client", "/peers")
        try:
            if r[0] == N.tas("peers") and r[1][0] == N.tas("all"):
                return sum(1 for _ in N.map_iter(r[1][1]))
        except (TypeError, IndexError, ValueError):
            pass
        return None

    def ready(self):
        """%gw-btc /x/ready, or None when the agent is not up."""
        if not self.agent_running("gw-btc"):
            return None
        return self.peek("gw-btc", "/ready")

    def domains(self, timeout: float = 120.0) -> list[str]:
        """The turfs eyre holds, as dotted names (TLD last)."""
        try:
            r = self.khan("(pure:m !>(.^(* %e /(scot %p our)/domains/(scot %da now))))", timeout)
        except Exception:
            return []
        out = []
        for turf in N.list_iter(r):
            try:
                parts = [N.from_tas(k) for k in N.list_iter(turf)]
            except Exception:
                continue
            if parts:
                out.append(".".join(reversed(parts)))
        return out

    def status(self) -> dict:
        d: dict = {
            "ship": self.cfg.patp,
            "pier": str(self.pier),
            "running": self.running(),
            "king": self.king_pid(),
            "serf": self.serf_pid(),
            "event_log_age_s": self.evt_age() if self.exists else None,
            "http_port": self.cfg.http_port,
            "release": self.cfg.release,
            "domain": self.cfg.domain or None,
        }
        if d["running"] and self.answers(20):
            d["answers"] = True
            d["identity"] = self.our(60)
            d["synced"] = self.synced()
            d["best_block"] = self.best_block()
            d["peers"] = self.peer_count()
            r = self.ready()
            d["ready"] = _render(r) if r is not None else None
        elif d["running"]:
            d["answers"] = False
        return d


def _render(n, depth: int = 0, budget: list | None = None) -> str:
    """A noun for humans: tas atoms as %words, cells as tuples.  Bounded:
    a whole agent state is thousands of cells deep and the renderer must
    not be the thing that crashes."""
    if budget is None:
        budget = [4000]
    budget[0] -= 1
    if budget[0] < 0:
        return "..."
    if isinstance(n, int):
        s = N.from_tas(n)
        if n and s.isprintable() and all(32 <= ord(c) < 127 for c in s):
            return "%" + s
        return str(n)
    if depth > 40:
        return "[...]"
    return "[" + " ".join(_render(x, depth + 1, budget) for x in _flatten(n)) + "]"


def _flatten(n):
    out = []
    while isinstance(n, tuple):
        out.append(n[0])
        n = n[1]
    out.append(n)
    return out


__all__ = ["Ship", "strand", "hoon_hex", "ip_hoon", "GoofError", "ConnError"]
