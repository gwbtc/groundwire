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
from . import chainops, lanes, packets


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


def _verdict_ok(line: str | None) -> bool:
    return bool(line) and "VALID" in line and "INVALID" not in line


def run_m2(cfg: Config) -> bool:
    """Two comets, first contact: each verifies the other's packet (so jael ->
    ames installs the peer), then we inject lanes and prove a live ames
    round-trip with `|hi` both ways."""
    net = Net(cfg)
    ok = False
    try:
        print("[m2] net up...", flush=True)
        net.up()

        print("[m2] spawn two identities...", flush=True)
        a_id = net.spawn_identity()
        b_id = net.spawn_identity()
        print(f"[m2]   A = {a_id.comet}\n[m2]   B = {b_id.comet}", flush=True)

        print("[m2] boot both comets (install desk + config watcher)...", flush=True)
        A = net.boot_comet(a_id, 0)
        B = net.boot_comet(b_id, 1)
        for c in (A, B):
            _await_log(c.log, "reconfigured to", 30)

        print("[m2] each comet verifies its OWN keyfile (serve + track self)...", flush=True)
        net.keyfile(A, a_id)
        net.keyfile(B, b_id)

        print("[m2] cross-verify packets (A learns B, B learns A)...", flush=True)
        skel_a = chainops.build_skeleton(cfg, a_id)
        skel_b = chainops.build_skeleton(cfg, b_id)
        packets.poke_peer(A, skel_b)
        packets.poke_peer(B, skel_a)
        vb = _await_log(A.log, f"attestation for {re.escape(b_id.comet)} is (VALID|INVALID)", cfg.verify)
        va = _await_log(B.log, f"attestation for {re.escape(a_id.comet)} is (VALID|INVALID)", cfg.verify)
        print(f"[m2]   A's verdict on B: {vb or 'NONE'}", flush=True)
        print(f"[m2]   B's verdict on A: {va or 'NONE'}", flush=True)
        if not (_verdict_ok(vb) and _verdict_ok(va)):
            print("[m2]   FAIL: cross-verification did not both pass", flush=True)
            return False

        print("[m2] inject lanes (peer is now %known) + probe with |hi...", flush=True)
        print(f"[m2]   A->B dear: {lanes.inject_lane(A, B.num, B.ames_port)}", flush=True)
        print(f"[m2]   B->A dear: {lanes.inject_lane(B, A.num, A.ames_port)}", flush=True)
        time.sleep(3)
        rab = _try_probe(A, b_id.comet)
        rba = _try_probe(B, a_id.comet)
        print(f"[m2]   A |hi B: {rab}", flush=True)
        print(f"[m2]   B |hi A: {rba}", flush=True)
        ok = ("hi-ok" in str(rab)) and ("hi-ok" in str(rba))
    finally:
        print("[m2] tearing down...", flush=True)
        net.down()
    print(f"[m2] {'PASS' if ok else 'FAIL'}", flush=True)
    return ok


def _try_probe(ship, peer_patp: str) -> object:
    try:
        return lanes.hi_probe(ship, peer_patp)
    except Exception as e:                                    # noqa: BLE001
        return f"ERROR: {e}"
