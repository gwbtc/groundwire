"""Milestone drivers — end-to-end flows that double as smoke tests."""

from __future__ import annotations

import time

from .config import Config
from .harness import Net
from . import noun as N
from . import obphon


def run_m1(cfg: Config) -> bool:
    """Single comet, self-keyfile verified by its own %urb-watcher."""
    net = Net(cfg)
    ok = False
    try:
        print("[m1] net up (regtest + wallet + desk build)...", flush=True)
        net.up()

        print("[m1] spawn identity (fund -> mine -> commit -> proof)...", flush=True)
        ident = net.spawn_identity()
        print(f"[m1]   comet = {ident.comet}", flush=True)

        print("[m1] boot pier + install groundwire desk + config watcher...", flush=True)
        c = net.boot_comet(ident, 0)
        print(f"[m1]   ready; our = {obphon.num_to_patp(c.our())}", flush=True)

        bid = c.peek("urb-watcher", ["x", "block-id"])
        print(f"[m1]   /x/block-id = {bid}", flush=True)
        if bid is None:
            print("[m1] FAIL: urb-watcher not answering /x/block-id", flush=True)
            return False

        print("[m1] poke own keyfile...", flush=True)
        res = net.keyfile(c, ident)
        print(f"[m1]   keyfile thread -> {N.from_tas(res) if isinstance(res,int) else res}", flush=True)

        target = c.num
        print(f"[m1] polling /x/conf for {ident.comet} ...", flush=True)
        for _ in range(40):
            conf = c.peek("urb-watcher", ["x", "conf"])
            if conf:
                keys = [k for k, _ in N.map_iter(conf)]
                if target in keys:
                    print(f"[m1]   CONF registered: {ident.comet} self-verified!", flush=True)
                    ok = True
                    break
            time.sleep(6)
        if not ok:
            print("[m1] FAIL: comet never appeared in /x/conf "
                  "(check pier log for report:lsa check lines)", flush=True)
            print(f"[m1]   pier log: {c.log}", flush=True)
    finally:
        print("[m1] tearing down...", flush=True)
        net.down()
    print(f"[m1] {'PASS' if ok else 'FAIL'}", flush=True)
    return ok
