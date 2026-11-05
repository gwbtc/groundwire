"""The post-boot steps, as code.

Each step has a name, a check and an apply, and prints one line.  Steps
run in order on every start; a step that has succeeded once leaves a
marker beside the pier and is skipped from then on (delete the marker to
re-run it).  ``gw populate`` re-runs them by hand; ``--only`` picks some.

Where the step list grows (#174 node defaults, #134 per-app onboarding,
#133 per-agent setup), it grows here.
"""

from __future__ import annotations

import json
import subprocess
import time
from dataclasses import dataclass
from typing import Callable

from .cli import good, info, warn
from .layout import GwError, self_argv, var_dir
from .ship import GoofError, Ship, hoon_hex
from .web import ShipWeb

HI_SHIPS = ("~tyr", "~daplyd")
NOSTR_RELAY = "wss://nos.lol"
GW_GROUP_HOST = "~hodler-lorfeb"
GW_GROUP_NAME = "v769287"
SWEEP_KEEP = 5000
ARMILLARY_REPO = "nisfeb/armillary"
ARMILLARY_REF = "main"


@dataclass
class Context:
    ship: Ship
    proof: dict | None = None          # the spawn proof, on the mint's first boot
    first_boot: bool = False
    web: ShipWeb | None = None

    def sponsor_pass_hex(self) -> str:
        p = self.proof or _load_proof(self.ship)
        v = (p or {}).get("sponsor_pass_hex", "") or ""
        return v[2:] if v.startswith("0x") else v

    def shipweb(self) -> ShipWeb | None:
        if self.web is None:
            w = ShipWeb(self.ship)
            self.web = w if w.login() else None
        return self.web


def _load_proof(ship: Ship) -> dict | None:
    p = var_dir() / f"{ship.name}.proof.json"
    try:
        return json.loads(p.read_text())
    except (OSError, ValueError):
        return None


@dataclass
class Step:
    name: str
    agent: str                                    # must be running first
    apply: Callable[[Context], str | None]        # returns the line to print, raises to fail
    once: bool = True                             # keep a marker after one success
    first_boot_only: bool = False
    skip: Callable[[Context], bool] | None = None  # nothing to do: decided BEFORE waiting on the ship


def _poke(ctx: Context, agent: str, mark: str, vase: str) -> None:
    ctx.ship.poke(agent, mark, vase, timeout=600)


# ---------------------------------------------------------------- steps --

def peer_discovery(ctx: Context) -> str:
    if not ctx.ship.cfg.peer_discovery:
        return "peer discovery: left off (the mint unticked it)"
    _poke(ctx, "gevulot", "noun", "!>([%set-receive %.y])")
    return "peer discovery: on (%gevulot accepts the sponsor's pushes)"


def sponsor_attestation(ctx: Context) -> str:
    h = ctx.sponsor_pass_hex()
    if not h:
        return "sponsor: no attestation in the proof; the Gevulot app can take one later"
    _poke(ctx, "gevulot", "noun", f"!>([%ingest-peer {hoon_hex(h)}])")
    _poke(ctx, "gevulot", "noun", "!>([%distribute ~])")
    _poke(ctx, "gw-btc", "noun", "!>([%index-origin %from-spawn 0 %causeway])")
    return "sponsor: attestation installed, broadcast asked for, public index from our spawn block"


def hi_ships(ctx: Context) -> str:
    for who in HI_SHIPS:
        _poke(ctx, "hood", "helm-send-hi", f"!>([{who} ''])")
    return "hi: sent to " + ", ".join(HI_SHIPS)


def nostr_relay(ctx: Context) -> str:
    j = json.dumps({"rela": {"add": NOSTR_RELAY}})
    _poke(ctx, "nostrill", "json", f"!>((need (de:json:html '{j}')))")
    return f"nostr: relay {NOSTR_RELAY}"


def gw_group(ctx: Context) -> str:
    _poke(ctx, "groups", "group-join", f"!>([[{GW_GROUP_HOST} %{GW_GROUP_NAME}] &])")
    return f"groups: joining {GW_GROUP_HOST}/{GW_GROUP_NAME}"


def _shell_desks(ctx: Context) -> ShipWeb:
    """The shell's forge named and its stock desks (the hatchery holds the
    Wallet) synced.  Raises when it is not there yet."""
    web = ctx.shipweb()
    if web is None:
        raise RuntimeError("no web login to the ship yet")
    forge = ""
    for _ in range(30):
        forge = web.forge_root()
        if forge:
            break
        time.sleep(10)
    if not forge:
        raise RuntimeError("the shell's forge is not up after five minutes")
    if not web.forge_awake():
        web.post_json("/grubbery/tiles/permits/refresh", {"full": True})
        for _ in range(24):
            if web.forge_awake():
                break
            time.sleep(5)
        else:
            raise RuntimeError("the shell's forge still has no name after the registry rebuild")
    if web.stock_synced("hatchery"):
        return web
    web._req("POST", web.base + "/grubbery/tiles/desks/sync-defaults")
    for _ in range(60):
        if web.stock_synced("hatchery"):
            return web
        time.sleep(10)
    raise RuntimeError("the hatchery has not synced in ten minutes")


def wallet_sweep(ctx: Context) -> str | None:
    key = var_dir() / "mint" / f"{ctx.ship.name}-change.key.json"
    if not key.exists() or key.stat().st_size == 0:
        return None                              # nothing to move: not a mint, or done
    web = _shell_desks(ctx)
    addr = ""
    for _ in range(3):
        addr = web.wallet_receive_address()
        if addr:
            break
        time.sleep(10)
    if not addr:
        raise RuntimeError("the Wallet gave no address yet; the leftover stays with the phrase")
    r = subprocess.run(self_argv() + ["causeway", "sweep", "--key-file", str(key), "--to", addr,
                                      "--keep", str(SWEEP_KEEP)],
                       capture_output=True, text=True, timeout=600)
    tail = (r.stdout + r.stderr).strip().splitlines()[-1:] or [""]
    if r.returncode == 0 and not key.exists():
        return f"wallet: leftover sats moved to the ship's Wallet ({addr})"
    raise RuntimeError(f"the sats did not move this time: {tail[0]}")


def armillary(ctx: Context) -> str | None:
    if not ctx.ship.cfg.armillary:
        return None
    web = _shell_desks(ctx)
    forge = web.forge_root()
    res = web.grub_tool("forge_create_repo",
                        {"name": "armillary", "remote": ARMILLARY_REPO, "ref": ARMILLARY_REF},
                        f"{forge}/tools")
    if res.startswith("error:") and "already" not in res:
        raise RuntimeError(f"forge_create_repo: {res}")
    web.post_json("/grubbery/forge/api/config",
                  {"repo": "armillary.git_repo", "origin": ARMILLARY_REPO, "ref": ARMILLARY_REF, "poll": 15})
    pulled = ""
    for _ in range(30):
        pulled = web.grub_tool("git_cmd", {"path": f"{forge}/repos/armillary.git_repo", "command": "pull"},
                               f"{forge}/tools")
        if any(k in pulled for k in ("up to date", "loned", "new object", "Fast-forward")):
            break
        time.sleep(10)
    res = web.grub_tool("create_desk",
                        {"name": "armillary", "source": f"{forge}/repos/armillary.git_repo/data/tree/code"})
    if res.startswith("error:") and "already" not in res:
        st, body = web.get("/grubbery/ball/apps/shell/desks?raw=1")
        if b'"armillary.desk"' not in body:
            raise RuntimeError(f"create_desk: {res}")
    web.post_json("/grubbery/api/poke/apps/shell/desks/armillary.desk/source.json?blot=/json",
                  {"code": "@forge/repos/armillary.git_repo/data/tree/code"})
    app = None
    for _ in range(60):
        asks = web.get_json("/apps/grubbery/asks.json") or []
        for a in asks if isinstance(asks, list) else []:
            if str(a.get("app", "")).endswith("/armillary.armillary_app"):
                app = a
                break
        if app:
            break
        time.sleep(10)
    if not app:
        return "armillary: installed; approve its permission ask from the shell's home page"
    web.post_json("/apps/grubbery/permits", {
        "action": "approve-weir", "app": app["app"], "picks": {},
        "granted": {k: app.get(k, []) for k in ("poke", "peek", "make")}})
    web.post_json("/apps/grubbery/permits/refresh", {"full": True})
    web.post_json("/apps/grubbery/permits/reload", {"app": app["app"]})
    for _ in range(18):
        st, _ = web.get("/apps/armillary")
        if st == 200:
            return "armillary: on the ship at /apps/armillary"
        time.sleep(10)
    return "armillary: installed and granted; /apps/armillary does not answer yet"


def _no_change_key(ctx: Context) -> bool:
    key = var_dir() / "mint" / f"{ctx.ship.name}-change.key.json"
    return not key.exists() or key.stat().st_size == 0


STEPS: list[Step] = [
    Step("peer-discovery", "gevulot", peer_discovery, first_boot_only=True),
    Step("sponsor", "gevulot", sponsor_attestation, once=False),
    Step("hi", "hood", hi_ships, once=False),
    Step("nostr-relay", "nostrill", nostr_relay),
    Step("gw-group", "groups", gw_group),
    Step("wallet-sweep", "gw-btc", wallet_sweep, once=False, skip=_no_change_key),
    Step("armillary", "gw-btc", armillary, skip=lambda ctx: not ctx.ship.cfg.armillary),
]


def run(ctx: Context, only: set[str] | None = None) -> None:
    for s in STEPS:
        if only and s.name not in only:
            continue
        if s.first_boot_only and not ctx.first_boot:
            continue
        marker = ctx.ship.marker(s.name)
        if s.once and marker.exists():
            continue
        if s.skip and s.skip(ctx):
            continue
        if not ctx.ship.wait_agent(s.agent, tries=18, every=10):
            warn(f"{s.name}: %{s.agent} is not running after 3 minutes; next start")
            continue
        try:
            line = s.apply(ctx)
        except GoofError as e:
            warn(f"{s.name}: {e}; next start")
            continue
        except Exception as e:                       # noqa: BLE001 - every step is optional
            warn(f"{s.name}: {e}; next start")
            continue
        if line:
            info(line)
        if s.once:
            marker.touch()
