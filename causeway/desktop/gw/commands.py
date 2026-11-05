"""The subcommands.  Each prints one line per step, one line per failure
with the command to run next, and exits."""

from __future__ import annotations

import json
import os
import subprocess
import sys
import time
import urllib.request
from pathlib import Path

from . import domain, peers, populate, release, service
from . import noun as N
from .cli import die, good, info, step, warn
from .layout import (GW_DIR, GwError, ShipConfig, ensure_layout, list_ships, mint_dir, pill_file,
                     release_info, self_argv, the_ship, validate_patp, var_dir, vere_bin)
from .ship import GoofError, Ship, strand

MIN_FREE_GB = 20


# ------------------------------------------------------------------ install --

def cmd_install(args) -> int:
    ensure_layout()
    step("Installing Groundwire")
    _preflight(new_pier=False)
    tag = release.install(args.tag, args.sha256, args.from_file, args.redownload)
    info(release.on_path_hint())
    info("next:  gw mint          (a new comet)")
    info("       gw start ~name   (a ship you already have)")
    return 0


def _preflight(new_pier: bool) -> None:
    free = _free_gb(GW_DIR)
    if free is not None:
        info(f"free disk     {free} GB")
        if new_pier and free < MIN_FREE_GB:
            raise GwError(f"only {free} GB free where {GW_DIR} lives; {MIN_FREE_GB} GB is the minimum "
                          "(a synced pier is ~2.2 GB and a full disk wedges the ship mid-write)")


def _free_gb(p: Path) -> int | None:
    while not p.exists() and p != p.parent:
        p = p.parent
    try:
        st = os.statvfs(p)
    except OSError:
        return None
    return int(st.f_bavail * st.f_frsize / (1 << 30))


def _need_release() -> None:
    if not vere_bin().exists() or not pill_file().exists():
        raise GwError(f"no release under {GW_DIR}; run `gw install` first")


# --------------------------------------------------------------------- mint --

def cmd_mint(args) -> int:
    ensure_layout()
    if args.tag or not vere_bin().exists():
        step("Installing the release")
        release.install(args.tag or "latest")
    _need_release()
    _preflight(new_pier=True)
    from .mint import mint
    return mint(args)


# ---------------------------------------------------------- boot and start --

def first_boot(cfg: ShipConfig, feed: str, proof: dict | None, wait: bool) -> int:
    """Boot a new pier from the pill and the feed, then do what every start does."""
    ship = Ship(cfg)
    if ship.exists:
        raise GwError(f"{ship.pier} already exists; `gw start {cfg.patp}` restarts it")
    step(f"Booting {cfg.patp}")
    cfg.http_port = _free_http_port(cfg.http_port)
    cfg.save()
    ship.log.parent.mkdir(parents=True, exist_ok=True)
    ship.log.write_text("")
    info(f"gw-vere -t --loom {cfg.loom} -c {ship.pier} -w {cfg.name} -G <feed> -B <pill> "
         f"--http-port {cfg.http_port} --gateway {cfg.gateway}")
    # the first boot is a plain process: the service unit takes over once the
    # pier exists, with the restart form (no -c/-w/-G/-B)
    ship.spawn_detached(ship.fresh_argv(feed))
    _wait_and_check(ship)
    kind = service.kind()
    if kind != "detached":
        # hand the running pier to the service manager: stop the bootstrap
        # process, then let the unit start it (one short replay)
        info(f"handing the ship to {kind}")
        if not ship.terminate():
            raise GwError("the bootstrap vere did not exit; leaving the ship as it is. "
                          f"Run `gw start {cfg.patp}` later to put it under {kind}.")
        cfg.supervised = service.start(ship)
        _wait_and_check(ship)
    else:
        cfg.supervised = "detached"
    cfg.save()
    return _after_start(ship, proof=proof, first_boot=True, wait=wait)


def cmd_start(args) -> int:
    ensure_layout()
    _need_release()
    name = validate_patp(args.comet) if args.comet else None
    cfg = the_ship(name)
    ship = Ship(cfg)
    if args.port:
        cfg.http_port = args.port
    if args.ames_port:
        cfg.ames_port = args.ames_port
    if args.vps:
        cfg.vps = True
    if not ship.exists:
        if not args.feed_file:
            raise GwError(f"no pier for {cfg.patp} under {GW_DIR}; a first boot needs --feed-file "
                          "(the baked feed from `gw causeway finalize`)")
        feed = Path(args.feed_file).read_text().strip()
        if not feed.startswith("0w"):
            raise GwError("the feed must be a @uw atom (0w...), the one `causeway finalize` wrote")
        proof = None
        if args.proof:
            proof = json.loads(Path(args.proof).read_text())
            (var_dir() / f"{cfg.name}.proof.json").write_text(json.dumps(proof))
        cfg.release = cfg.release or release_info().get("tag", "")
        cfg.save()
        return first_boot(cfg, feed, proof, wait=not args.no_wait)
    step(f"Starting {cfg.patp}")
    if ship.running():
        info(f"already running (pid {ship.king_pid() or ship.serf_pid()})")
    else:
        cfg.http_port = _free_http_port(cfg.http_port)
        if args.detached:
            ship.spawn_detached(ship.restart_argv())
            cfg.supervised = "detached"
        else:
            cfg.supervised = service.start(ship)
        info(f"under {cfg.supervised}")
    cfg.save()
    _wait_and_check(ship)
    return _after_start(ship, proof=None, first_boot=False, wait=not args.no_wait)


def _wait_and_check(ship: Ship) -> None:
    print("    waiting for the ship", end="", flush=True)
    ship.wait_up(on_tick=lambda w: print(".", end="", flush=True))
    print()
    our = ship.our(120)
    if not our:
        warn("could not read the ship's identity back over the control socket")
    elif our != ship.cfg.patp:
        raise GwError(f"THE SHIP THAT CAME UP IS NOT THE ONE YOU ASKED FOR.\n    asked for : {ship.cfg.patp}"
                      f"\n    booted as : {our}\n    The pier at {ship.pier} holds the wrong identity; "
                      "nothing here will delete it for you.")
    else:
        good(f"up as {our}")


def _after_start(ship: Ship, proof: dict | None, first_boot: bool, wait: bool) -> int:
    cfg = ship.cfg
    step("Setting up")
    ctx = populate.Context(ship=ship, proof=proof, first_boot=first_boot)
    populate.run(ctx, only={"peer-discovery", "sponsor", "hi"})
    if cfg.bitcoin:
        if not ship.has_desk("node"):
            warn("this release has no %node desk: the ship is up, but it cannot reach Bitcoin")
        else:
            step("Seeding peers")
            have = ship.peer_count() if not first_boot else 0
            if have and have >= 8:
                # a restart keeps its peers; an address handed over twice is
                # wasted (the client blacklists a burned seed for days)
                info(f"the light client already has {have} peers; not re-seeding")
            else:
                try:
                    n = peers.seed(ship, log=info)
                    good(f"handed {n} peer addresses to the light client")
                except Exception as e:                       # noqa: BLE001
                    warn(f"peer seeding: {e}")
    if cfg.vps:
        step(f"A name for {cfg.patp}")
        dom = domain.ensure_domain(ship)
        if dom:
            cfg.domain = dom
            cfg.save()
            good(f"name: https://{dom}  (the certificate follows within minutes; plain http then redirects)")
    populate.run(ctx, only={"nostr-relay", "gw-group", "wallet-sweep", "armillary"})
    _summary(ship)
    if wait and cfg.bitcoin:
        return watch_sync(ship)
    return 0


def _free_http_port(port: int) -> int:
    import socket
    for p in range(port, port + 50):
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
            try:
                s.bind(("127.0.0.1", p))
            except OSError:
                continue
            if p != port:
                info(f"port {port} is in use; using {p} for HTTP")
            return p
    raise GwError(f"no free TCP port in {port}-{port + 49} for the ship's web server")


def _summary(ship: Ship) -> None:
    cfg = ship.cfg
    step("Where things are")
    info(f"pier       {ship.pier}")
    info(f"ship log   {ship.log}")
    info(f"settings   {cfg.path}")
    info(f"runs under {cfg.supervised or service.kind()}")
    info(f"web        http://127.0.0.1:{cfg.http_port}" + (f"   or  https://{cfg.domain}" if cfg.domain else ""))
    info(f"login      gw code {cfg.patp}")
    info(f"status     gw status {cfg.patp}")
    info(f"stop       gw stop {cfg.patp}")
    (GW_DIR / "README").write_text(
        f"Your Groundwire ship: {cfg.patp}\n\n  pier:      {ship.pier}\n  ship log:  {ship.log}\n"
        f"  settings:  {cfg.path}\n  web:       http://127.0.0.1:{cfg.http_port}\n"
        + (f"  address:   https://{cfg.domain}\n" if cfg.domain else "")
        + f"\n  gw status {cfg.patp}\n  gw code {cfg.patp}\n  gw stop {cfg.patp}\n  gw start {cfg.patp}\n\n"
        f"The ship runs under {cfg.supervised or service.kind()}"
        + (", which restarts it after a crash or a reboot.\n" if cfg.supervised in ("systemd", "launchd")
           else "; nothing restarts it after a reboot.\n")
        + "Identity custody: the proof and feed files in var/mint. Back those up.\n")


# --------------------------------------------------------------------- stop --

def cmd_stop(args) -> int:
    cfg = the_ship(validate_patp(args.comet) if args.comet else None)
    ship = Ship(cfg)
    step(f"Stopping {cfg.patp}")
    if not ship.running():
        service.stop(ship)
        info("not running")
        return 0
    if service.stop(ship):
        good("stopped; the pier is untouched")
        return 0
    raise GwError(f"vere (pid {ship.king_pid()}) is still running and holds the pier; "
                  "do not start another ship on it")


def cmd_restart(args) -> int:
    cmd_stop(args)
    args.feed_file = ""
    args.proof = ""
    args.port = 0
    args.ames_port = 0
    args.vps = False
    args.no_wait = True
    args.detached = False
    return cmd_start(args)


# ------------------------------------------------------------------- status --

def cmd_status(args) -> int:
    cfg = the_ship(validate_patp(args.comet) if args.comet else None)
    ship = Ship(cfg)
    d = ship.status()
    d["service"] = service.state(ship)
    if args.json:
        print(json.dumps(d, indent=2))
        return 0
    step(f"Status of {cfg.patp}")
    info(f"release     {cfg.release or release_info().get('tag', '?')}")
    info(f"pier        {ship.pier}")
    info(f"vere        {'pid %s' % (d['king'] or d['serf']) if d['running'] else 'not running'}")
    info(f"service     {d['service']}")
    if d.get("event_log_age_s") is not None:
        info(f"event log   {d['event_log_age_s']}s since the last write")
    if d["running"] and d.get("answers"):
        info(f"identity    {d.get('identity') or '?'}")
        s = d.get("synced")
        info(f"synced      {'yes' if s else 'no' if s is False else 'unknown (no %bitcoin-client answer)'}")
        if d.get("best_block") is not None:
            info(f"best block  {d['best_block']}")
        if d.get("peers") is not None:
            info(f"peers       {d['peers']}")
        if d.get("ready") is not None:
            info(f"gw-btc      {d['ready']}")
    elif d["running"]:
        info("the ship is up but not answering its control socket yet (booting, or very busy)")
    info(f"web         http://127.0.0.1:{cfg.http_port}" + (f"   https://{cfg.domain}" if cfg.domain else ""))
    if args.watch and d["running"]:
        return watch_sync(ship)
    return 0


def watch_sync(ship: Ship) -> int:
    step("Syncing the light client")
    info("block headers first, then filter headers; synced means both are at the tip.")
    info("Safe to Ctrl-C: the ship keeps running.  gw status --watch picks this up again.")
    tip = _chain_tip()
    if tip:
        info(f"chain tip is {tip} (mempool.space)")
    start = time.time()
    prev = prev_t = 0
    while True:
        synced = ship.synced()
        height = ship.best_block()
        npeers = ship.peer_count()
        el = int(time.time() - start)
        rate = ""
        if height and prev and height > prev and time.time() > prev_t:
            per_min = (height - prev) * 60 / (time.time() - prev_t)
            rate = f"  {per_min:.0f}/min"
            if tip and per_min > 0 and tip > height:
                rate += f"  eta ~{int((tip - height) / per_min)}m"
        if height:
            prev, prev_t = height, time.time()
        print(f"    [{el // 3600:02d}:{el % 3600 // 60:02d}:{el % 60:02d}] best block {height or '?'}  "
              f"peers {npeers if npeers is not None else '?'}  synced {'yes' if synced else 'no'}{rate}",
              flush=True)
        if synced:
            good("synced: block headers and filter headers are both at the tip")
            return 0
        if el > 300 and npeers == 0:
            warn("no live peers after five minutes; `gw peers` hands the light client more addresses")
        time.sleep(60)


def _chain_tip() -> int | None:
    try:
        with urllib.request.urlopen("https://mempool.space/api/blocks/tip/height", timeout=6) as r:
            return int(r.read().decode().strip())
    except Exception:
        return None


# --------------------------------------------------------------- code, logs --

def cmd_code(args) -> int:
    cfg = the_ship(validate_patp(args.comet) if args.comet else None)
    ship = Ship(cfg)
    if not ship.running():
        raise GwError(f"the ship is not running; start it first:  gw start {cfg.patp}")
    code = ship.code(90)
    if not code:
        raise GwError("could not read the code from the ship (it may still be booting); try again in a minute")
    print(code)
    return 0


def cmd_logs(args) -> int:
    cfg = the_ship(validate_patp(args.comet) if args.comet else None)
    ship = Ship(cfg)
    if not ship.log.exists():
        raise GwError(f"no log at {ship.log}")
    cmd = ["tail", "-n", str(args.n)] + (["-f"] if args.follow else []) + [str(ship.log)]
    return subprocess.call(cmd)


def cmd_ships(args) -> int:
    for s in list_ships():
        cfg = ShipConfig.load(s)
        ship = Ship(cfg)
        print(f"~{s}  {'running' if ship.running() else 'stopped'}  port {cfg.http_port}"
              + (f"  https://{cfg.domain}" if cfg.domain else ""))
    return 0


def cmd_version(args) -> int:
    print(release_info().get("tag", "no release installed"))
    return 0


# ---------------------------------------------------------------------- ctl --

def cmd_ctl(args) -> int:
    cfg = the_ship(validate_patp(args.ship) if args.ship else None)
    ship = Ship(cfg)
    if not args.ctl_cmd:
        raise GwError("gw ctl [-s @p] scry|poke|eval|our ...")
    try:
        if args.ctl_cmd == "our":
            print(ship.our())
        elif args.ctl_cmd == "scry":
            r = ship.peek(args.agent, args.path)
            print(json.dumps(_jsonable(r)) if args.json else _show(r))
        elif args.ctl_cmd == "poke":
            ship.poke(args.agent, args.mark, args.vase)
            print("ok")
        elif args.ctl_cmd == "eval":
            r = ship.khan(args.hoon, timeout=600)
            print(json.dumps(_jsonable(r)) if args.json else _show(r))
    except GoofError as e:
        raise GwError(str(e)) from e
    return 0


def _jsonable(n):
    if isinstance(n, int):
        return n
    if isinstance(n, tuple):
        return [_jsonable(n[0]), _jsonable(n[1])]
    return n


def _show(n) -> str:
    from .ship import _render
    return "~" if n is None else _render(n)


# -------------------------------------------------------- name, peers, populate --

def cmd_name(args) -> int:
    cfg = the_ship(validate_patp(args.comet) if args.comet else None)
    ship = Ship(cfg)
    if not ship.running():
        raise GwError(f"the ship is not running; start it first:  gw start {cfg.patp}")
    step(f"A name for {cfg.patp}")
    dom = domain.ensure_domain(ship)
    if dom:
        cfg.domain = dom
        cfg.vps = True
        cfg.save()
        good(f"name: https://{dom}")
        return 0
    return 1


def cmd_peers(args) -> int:
    cfg = the_ship(validate_patp(args.comet) if args.comet else None)
    ship = Ship(cfg)
    step("Seeding peers")
    n = peers.seed(ship, batches=1, size=args.count, log=info)
    good(f"handed {n} addresses to the light client; `gw status` shows the live count")
    return 0


def cmd_populate(args) -> int:
    cfg = the_ship(validate_patp(args.comet) if args.comet else None)
    ship = Ship(cfg)
    if not ship.running():
        raise GwError(f"the ship is not running; start it first:  gw start {cfg.patp}")
    only = {s.strip() for s in args.only.split(",") if s.strip()} or None
    step("Setup steps")
    for s in populate.STEPS:
        if only and s.name not in only:
            continue
        if s.once and ship.marker(s.name).exists():
            ship.marker(s.name).unlink()
    populate.run(populate.Context(ship=ship), only=only)
    return 0


# ------------------------------------------------------------------- update --

def cmd_update(args) -> int:
    ensure_layout()
    cfg = the_ship(validate_patp(args.comet) if args.comet else None)
    ship = Ship(cfg)
    step("Fetching the release")
    tag = release.install(args.tag, "", args.from_file)
    if cfg.release == tag and not args.from_file:
        info("already on it")
    was_running = ship.running()
    if was_running:
        step(f"Restarting {cfg.patp} on {tag}")
        if not service.stop(ship):
            raise GwError("the ship did not stop; not swapping the runtime under it")
        cfg.supervised = service.start(ship)
        _wait_and_check(ship)
    cfg.release = tag
    cfg.save()
    good(f"{cfg.patp} is on {tag}; its desks follow the sponsor by OTA")
    return 0


# ------------------------------------------------------------ invite, mine --

def cmd_invite(args) -> int:
    _need_release()
    if args.invite_cmd == "create":
        cmd = ["invite", "create"] + (["--no-wait"] if args.no_wait else [])
    elif args.invite_cmd == "show":
        cmd = ["invite", "show", args.code]
    else:
        raise GwError("gw invite create | gw invite show CODE")
    return cmd_causeway("causeway", cmd)


def cmd_mine(args) -> int:
    info("A comet's name commits to the coin that funds it, so names are mined inside the mint,")
    info("after the funding is chosen.  Ask for them there:")
    info("")
    info("  gw mint --candidates 10          mine ten names, pick one (the time of one mint)")
    info("  gw mint --prefix abc             first syllable (about 256 tries; minutes)")
    info("  gw mint --prefix abcdef          both leading syllables (65,536 tries on average)")
    if args.prefix:
        p = args.prefix.lower()
        if len(p) not in (3, 6) or not p.isalpha():
            raise GwError("a prefix is 3 or 6 lowercase letters")
        info("")
        info(f"  gw mint --prefix {p}")
    return 0


# ----------------------------------------------------------------- causeway --

def cmd_causeway(which: str, rest: list[str]) -> int:
    """Hand the process to Causeway's click CLI or its TUI, in this interpreter."""
    here = str(Path(__file__).resolve().parent.parent)
    if here not in sys.path:
        sys.path.insert(0, here)
    if which == "causeway-tui":
        import causeway_tui
        sys.argv = ["causeway-tui"] + rest
        causeway_tui.main()
        return 0
    import causeway
    sys.argv = ["causeway"] + rest
    try:
        causeway.cli.main(args=rest, prog_name="gw causeway", standalone_mode=True)
    except SystemExit as e:
        return int(e.code or 0)
    return 0
