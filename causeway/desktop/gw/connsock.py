"""Client for a vere pier's control socket (<pier>/.urb/conn.sock).

Speaks the newt-framed jam protocol documented in vere/pkg/vere/io/conn.c:
  request  = [request-id %fyrd|%peek|%ovum|%peel args]
  response = [request-id tag data]   (tag: %avow/%peek/%news/%peel/%bail)
"""

from __future__ import annotations

import socket
import time
from pathlib import Path

from . import noun as N


SPIDER_BEAMS = N.nlist([N.path("sur", "spider", "hoon"),
                        N.path("lib", "strandio", "hoon")])


class ConnError(RuntimeError):
    pass


class GoofError(ConnError):
    def __init__(self, goof):
        self.goof = goof
        super().__init__(f"thread bailed: {_render_goof(goof)}")


def _render_goof(goof) -> str:
    # goof = [mote=@tas tang]; just surface the mote and any leaf tapes.
    try:
        mote = N.from_tas(goof[0])
        tang = goof[1]
        lines = []
        for tank in N.list_iter(tang):
            # tank = [%leaf tape] = (tas 'leaf', tape) most commonly
            if isinstance(tank, tuple) and tank[0] == N.tas("leaf"):
                lines.append(N.from_tape(tank[1]))
        return f"%{mote}: " + " | ".join(lines)
    except Exception:
        return repr(goof)


class ConnSock:
    def __init__(self, pier: Path, timeout: float = 120.0):
        self.sock_path = Path(pier) / ".urb" / "conn.sock"
        self.timeout = timeout
        self._rid = 0

    def _next_rid(self) -> int:
        self._rid += 1
        return self._rid

    def alive(self) -> bool:
        return self.sock_path.exists()

    def _connect_path(self) -> str:
        """macOS AF_UNIX caps the path at ~104 bytes; pier socket paths blow
        past that. Use the shortest of {absolute, cwd-relative}."""
        import os
        ap = str(self.sock_path)
        try:
            rp = os.path.relpath(self.sock_path)
        except ValueError:
            rp = ap
        path = rp if len(rp) < len(ap) else ap
        if len(path) >= 104:
            raise ConnError(
                f"conn.sock path too long ({len(path)} >= 104): {path}")
        return path

    def _roundtrip(self, request) -> object:
        s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        s.settimeout(self.timeout)
        try:
            s.connect(self._connect_path())
            s.sendall(N.newt_encode(request))
            payload = self._read_frame(s)
        finally:
            s.close()
        return N.cue(N.bytes_to_atom(payload))

    def _read_frame(self, s: socket.socket) -> bytes:
        head = self._recvn(s, 5)
        if head[0] != 0x00:
            raise ConnError(f"bad newt tag {head[0]:#x}")
        length = int.from_bytes(head[1:5], "little")
        return self._recvn(s, length)

    @staticmethod
    def _recvn(s: socket.socket, n: int) -> bytes:
        buf = b""
        while len(buf) < n:
            chunk = s.recv(n - len(buf))
            if not chunk:
                raise ConnError("conn.sock closed mid-message")
            buf += chunk
        return buf

    # -- fyrd / khan-eval ---------------------------------------------------

    def fyrd(self, bear: str, name: str, out_mark: str,
             in_mark: str, in_noun) -> object:
        """Run a thread; return its product noun (the page's data) or raise."""
        rid = self._next_rid()
        cast = (N.tas(out_mark), (N.tas(in_mark), in_noun))
        req = (rid, (N.tas("fyrd"), (N.tas(bear), (N.tas(name), cast))))
        resp = self._roundtrip(req)
        # resp = [rid tag data]
        _, rest = resp
        tag, data = rest
        if tag == N.tas("avow"):
            flag, payload = data            # (each page goof)
            if flag == 0:
                _mark, value = payload      # page = [mark noun]
                return value
            raise GoofError(payload)
        if tag == N.tas("bail"):
            raise ConnError(f"fyrd bail: {data!r}")
        raise ConnError(f"unexpected fyrd response tag {N.from_tas(tag)!r}")

    def khan_eval(self, hoon_body: str) -> object:
        """Eval a strand body (text producing `form:m` for m=(strand ,vase))."""
        page = (N.cord(hoon_body), SPIDER_BEAMS)
        return self.fyrd("base", "khan-eval", "noun", "ted-eval", page)

    def poke_our(self, agent: str, mark: str, vase_expr: str) -> object:
        """Poke a local agent from a transient strand.

        `vase_expr` is a hoon expression producing the poke vase, e.g.
        "!>([%watcher-config ...])". `now`/`our` are in scope.
        """
        body = (
            "=/  m  (strand ,vase)  ^-  form:m\n"
            "  ;<  our=@p   bind:m  get-our\n"
            "  ;<  now=@da  bind:m  get-time\n"
            f"  ;<  ~  bind:m  (poke-our %{agent} %{mark} {vase_expr})\n"
            "  (pure:m !>('ok'))"
        )
        return self.khan_eval(body)

    # -- peek (scry) --------------------------------------------------------

    def peek_gall(self, agent: str, spur: list[str]):
        """Scry /gx/<agent>/<spur...>; return the result noun, or None."""
        rid = self._next_rid()
        tyl = N.nlist(N.tas(k) for k in spur)
        # arvo +peek (arm 22) takes (each path [%once ...] [%beam ...]); the
        # %once form is the %| (right) branch of `each`.
        once = (1, (N.tas("once"), (N.tas("gx"), (N.tas(agent), tyl))))
        req = (rid, (N.tas("peek"), once))
        resp = self._roundtrip(req)
        _, rest = resp
        tag, res = rest
        if tag != N.tas("peek"):
            raise ConnError(f"unexpected peek response tag {N.from_tas(tag)!r}")
        return _unwrap_cask(res)

    # -- ovum (raw event injection) -----------------------------------------

    def ovum(self, tar: str, wire: list[str], card) -> str:
        """Inject [tar wire card]; return 'done' or 'drop'."""
        rid = self._next_rid()
        trel = (N.tas(tar), (N.nlist(N.tas(k) for k in wire), card))
        req = (rid, (N.tas("ovum"), trel))
        resp = self._roundtrip(req)
        _, rest = resp
        tag, data = rest
        if tag == N.tas("news"):
            return N.from_tas(data)
        if tag == N.tas("bail"):
            raise ConnError(f"ovum bail: {data!r}")
        raise ConnError(f"unexpected ovum response tag {N.from_tas(tag)!r}")

    # -- readiness ----------------------------------------------------------

    def wait_ready(self, timeout: float, poll: float = 5.0) -> bool:
        """Poll until the ship answers a trivial thread. Uses a SHORT per-probe
        timeout so a busy boot (gw-base.pill loads a large azimuth snapshot,
        ~minutes unresponsive) is polled responsively instead of blocking the
        full conn timeout each attempt."""
        deadline = time.time() + timeout
        probe = ConnSock(self.sock_path.parent.parent, timeout=20)
        while time.time() < deadline:
            if self.sock_path.exists():
                try:
                    if probe.khan_eval(
                            "=/  m  (strand ,vase)  (pure:m !>('ok'))") == N.cord("ok"):
                        return True
                except Exception:
                    pass
            time.sleep(poll)
        return False


def _unwrap_cask(res):
    """Peel (unit ...) wrappers off a peek result down to the cask's data.

    A successful gall peek is (unit (cask)) = [~ [mark data]]; some paths add
    an extra unit. Marks are nonzero @tas atoms, so we peel while the head is
    0 (the unit `~` tag) and a cell follows, then return the cask's data.
    """
    if res == 0:
        return None
    cur = res
    while isinstance(cur, tuple) and cur[0] == 0 and isinstance(cur[1], tuple):
        cur = cur[1]
    if isinstance(cur, tuple):
        return cur[1]          # cask = [mark data] -> data
    return None
