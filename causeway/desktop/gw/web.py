"""The ship's web server with the owner's login, for what has no poke mark
of its own: the grubbery shell and the Wallet.  The login is +code over the
control socket, exchanged for a cookie kept in memory only."""

from __future__ import annotations

import http.cookiejar
import json
import ssl
import urllib.error
import urllib.parse
import urllib.request

from .ship import Ship


class ShipWeb:
    def __init__(self, ship: Ship):
        self.ship = ship
        self.base = ""
        self.jar = http.cookiejar.CookieJar()
        ctx = ssl.create_default_context()
        ctx.check_hostname = False
        ctx.verify_mode = ssl.CERT_NONE          # it is this machine's own ship
        self.opener = urllib.request.build_opener(
            urllib.request.HTTPCookieProcessor(self.jar),
            urllib.request.HTTPSHandler(context=ctx))

    def login(self) -> bool:
        if self.base:
            return True
        code = self.ship.code(120)
        if not code:
            return False
        for base in (f"http://127.0.0.1:{self.ship.cfg.http_port}",
                     "https://127.0.0.1:443", "https://127.0.0.1:8443"):
            try:
                st, _ = self._req("POST", f"{base}/~/login",
                                  data=urllib.parse.urlencode({"password": code}).encode(),
                                  ctype="application/x-www-form-urlencoded", timeout=20)
            except Exception:
                continue
            if st in (200, 204, 302, 303):
                self.base = base
                return True
        return False

    def _req(self, method: str, url: str, data: bytes | None = None, ctype: str = "",
             timeout: float = 120) -> tuple[int, bytes]:
        req = urllib.request.Request(url, data=data, method=method)
        if ctype:
            req.add_header("content-type", ctype)
        try:
            with self.opener.open(req, timeout=timeout) as resp:
                return resp.status, resp.read()
        except urllib.error.HTTPError as e:
            return e.code, e.read() if e.fp else b""

    def get(self, path: str, timeout: float = 120) -> tuple[int, bytes]:
        return self._req("GET", self.base + path, timeout=timeout)

    def post_json(self, path: str, obj, timeout: float = 120) -> tuple[int, bytes]:
        return self._req("POST", self.base + path, json.dumps(obj).encode(), "application/json", timeout)

    def post_form(self, path: str, fields: dict, timeout: float = 120) -> tuple[int, bytes]:
        return self._req("POST", self.base + path, urllib.parse.urlencode(fields).encode(),
                         "application/x-www-form-urlencoded", timeout)

    def get_json(self, path: str, timeout: float = 120):
        st, body = self.get(path, timeout)
        if st != 200:
            return None
        try:
            return json.loads(body)
        except ValueError:
            return None

    # ------------------------------------------------------------ shell --
    def grub_tool(self, name: str, args: dict, path: str = "") -> str:
        """One MCP tool call into the shell (POST /grubbery/tools)."""
        call = {"tool_name": name, "tool_args": args}
        if path:
            call["path"] = path
        st, body = self.post_json("/grubbery/tools", {
            "jsonrpc": "2.0", "id": 1, "method": "tools/call",
            "params": {"name": "call_tool", "arguments": call}})
        try:
            d = json.loads(body)
        except ValueError:
            return "error: no answer from the shell"
        err = d.get("error")
        if err:
            return "error: %s" % (err.get("message") if isinstance(err, dict) else err)
        r = d.get("result") or {}
        text = " ".join(c.get("text", "") for c in r.get("content", []) if c.get("type") == "text")
        return ("error: " if r.get("isError") else "") + text

    def forge_root(self) -> str:
        d = self.get_json("/apps/grubbery/aliases.json")
        try:
            return (d.get("@forge") or [{}])[0].get("path", "") if isinstance(d, dict) else ""
        except Exception:
            return ""

    def forge_awake(self) -> bool:
        d = self.get_json("/grubbery/ball/sys/link?raw=1")
        try:
            names = [c.get("name") if isinstance(c, dict) else c for c in d.get("children", [])]
            return "forge" in names
        except Exception:
            return False

    def stock_synced(self, desk: str) -> bool:
        d = self.get_json("/grubbery/tiles/desks/stock")
        try:
            return any(x.get("name") == desk and x.get("synced") for x in d)
        except Exception:
            return False

    def wallet_receive_address(self) -> str:
        st, _ = self.get("/groundwire/wallet/simple", timeout=20)
        if st != 200:
            return ""
        st, body = self.post_form("/groundwire/wallet/simple",
                                  {"action": "get-receive-address", "net": "main"}, timeout=20)
        addr = body.decode("utf-8", "replace").strip()
        return addr if addr.startswith(("bc1q", "bc1p")) else ""
