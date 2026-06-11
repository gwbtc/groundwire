"""Milestone drivers — end-to-end flows that double as smoke tests.

Assertions read the pier log (the watcher slogs its report:lsa verdict there);
gall scries via conn return ~ in this fork, so log-parsing is the assertion
channel (also what the scenario suite uses).
"""

from __future__ import annotations

import re
import time
from pathlib import Path

from .config import Config
from .harness import Net


def _tail_log(path: Path, pattern: str) -> str | None:
    if not path.exists():
        return None
    rx = re.compile(pattern)
    for line in path.read_text(errors="replace").splitlines():
        if rx.search(line):
            return line.strip()
    return None


def _await_log(path: Path, pattern: str, timeout: float, poll: float = 5.0) -> str | None:
    deadline = time.time() + timeout
    while time.time() < deadline:
        hit = _tail_log(path, pattern)
        if hit:
            return hit
        time.sleep(poll)
    return None


def run_m1(cfg: Config) -> bool:
    """Single comet: its own %urb-watcher verifies its keyfile (VALID)."""
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
        print(f"[m1]   ready; pier = {c.pier}", flush=True)

        cfg_hit = _await_log(c.log, "reconfigured to", 30)
        print(f"[m1]   {cfg_hit or 'WARN: watcher not reconfigured'}", flush=True)

        print("[m1] poke own keyfile...", flush=True)
        net.keyfile(c, ident)

        who = re.escape(ident.comet)
        verdict = _await_log(c.log, f"attestation for {who} is (VALID|INVALID)", cfg.verify)
        if verdict:
            print(f"[m1]   {verdict}", flush=True)
            ok = "VALID" in verdict and "INVALID" not in verdict
            # surface the per-check breakdown either way
            for line in c.log.read_text(errors="replace").splitlines():
                if re.search(r"\[(ok|XX)\]", line):
                    print(f"[m1]     {line.strip()}", flush=True)
        else:
            print("[m1]   FAIL: no verdict slogged (verify thread may have crashed)", flush=True)
            print(f"[m1]   pier log: {c.log}", flush=True)
    finally:
        print("[m1] tearing down...", flush=True)
        net.down()
    print(f"[m1] {'PASS' if ok else 'FAIL'}", flush=True)
    return ok
