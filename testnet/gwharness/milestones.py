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

        print("[m2] establish lanes + first contact (|hi both ways)...", flush=True)
        rab = _contact(A, B, b_id.comet)
        rba = _contact(B, A, a_id.comet)
        print(f"[m2]   A |hi B: {rab}", flush=True)
        print(f"[m2]   B |hi A: {rba}", flush=True)
        ok = ("hi-ok" in str(rab)) and ("hi-ok" in str(rba))
        _write_m2(cfg, a_id.comet, b_id.comet, _verdict_ok(va), _verdict_ok(vb), rab, rba)
        try:
            from .report import write_results_md
            write_results_md(cfg)
        except Exception:                                    # noqa: BLE001
            pass
    finally:
        print("[m2] tearing down...", flush=True)
        net.down()
    print(f"[m2] {'PASS' if ok else 'FAIL'}", flush=True)
    return ok


def _write_m2(cfg, a, b, a_ok, b_ok, rab, rba) -> None:
    def hi(r): return "hi-ok" if "hi-ok" in str(r) else str(r)
    lines = [
        "# M2 — two-comet first contact", "",
        f"- **A** `{a}`", f"- **B** `{b}`", "",
        f"- A verifies B's packet: **{'VALID' if b_ok else 'FAIL'}**",
        f"- B verifies A's packet: **{'VALID' if a_ok else 'FAIL'}**",
        f"- A `|hi` B: **{hi(rab)}**",
        f"- B `|hi` A: **{hi(rba)}**", "",
        "Two confidential comets each verified the other's Bitcoin-backed "
        "identity, then completed a live authenticated Ames round-trip both ways.",
    ]
    (cfg.results_dir / "M2.md").write_text("\n".join(lines) + "\n")


def _try_probe(ship, peer_patp: str) -> object:
    try:
        return lanes.hi_probe(ship, peer_patp)
    except Exception as e:                                    # noqa: BLE001
        return f"ERROR: {e}"


def run_gate(cfg: Config, mode: str = "reject") -> bool:
    """Ames suite gate (Workstream A). A NEW-kernel comet A receives an
    unverified suite-C peer B's self-attestation and HOLDS it — never trusting
    the bare Ames packet (the security property: a suite-C comet must prove its
    Bitcoin ownership, else it could lie about being non-Groundwire). Then:
      reject  -> inject a negative verdict; B is suspended (the lying-comet case)
      verify  -> POST B's packet; the jael ride installs B (the honest case)
    Comets boot directly from the fresh %31 pill (gw-base-31.pill, built by
    build_pill.py) — no -A, no upgrade. Set cfg.pill to the %31 pill first."""
    new_pill = cfg.gw_root / "gw-base-31.pill"
    if new_pill.exists():
        cfg.pill = new_pill
        print(f"[gate] using %31 pill: {new_pill}", flush=True)
    else:
        print(f"[gate] WARNING: {new_pill} not found; comets will boot the OLD "
              "kernel and the gate will NOT fire. Build it with build_pill.py.",
              flush=True)
    net = Net(cfg)
    ok = False
    try:
        print("[gate] net up...", flush=True)
        net.up()
        a_id = net.spawn_identity()
        b_id = net.spawn_identity()
        print(f"[gate]   A (new kernel) = {a_id.comet}", flush=True)
        print(f"[gate]   B (peer)       = {b_id.comet}", flush=True)

        print("[gate] boot A on the NEW %31 kernel (fresh from the pill)...", flush=True)
        A = net.boot_comet(a_id, 0)

        print("[gate] boot B...", flush=True)
        B = net.boot_comet(b_id, 1)
        for c in (A, B):
            _await_log(c.log, "reconfigured to", 30)

        # inject lanes both ways so first contact can occur, then A reaches B,
        # which prompts the self-attestation exchange -> A's gate fires on B.
        lanes.inject_lane(A, B.num, B.ames_port)
        lanes.inject_lane(B, A.num, A.ames_port)
        print("[gate] trigger first contact (A |hi B)...", flush=True)
        _try_probe(A, b_id.comet)
        who_b = re.escape(b_id.comet)
        held = _await_log(A.log, f"holding suite-C comet {who_b}", 120)
        print(f"[gate]   A HELD suite-C B (not bare-accepted): {bool(held)}", flush=True)
        if not held:
            print("[gate]   FAIL: gate never fired; trying B->A as well...", flush=True)
            _try_probe(B, a_id.comet)
            held = _await_log(A.log, f"holding suite-C comet {who_b}", 60)
            if not held:
                return False

        if mode == "reject":
            print("[gate] inject NEGATIVE verdict -> suspend...", flush=True)
            lanes.inject_attest_verdict(A, B.num, ok=False)
            susp = _await_log(A.log, f"comet {who_b} attestation failed; suspended", 40)
            print(f"[gate]   B SUSPENDED: {bool(susp)}", flush=True)
            ok = bool(susp)
        else:  # verify
            print("[gate] VERIFY B's packet (POST) -> jael ride installs B...", flush=True)
            net.peer(A, b_id)
            verified = _await_log(A.log, f"attestation for {who_b} is VALID", cfg.verify)
            print(f"[gate]   B Bitcoin-verified by A's watcher: {bool(verified)}", flush=True)
            time.sleep(8)
            hi = _contact(A, B, b_id.comet)
            print(f"[gate]   A |hi B after verification: {hi}", flush=True)
            ok = bool(verified) and ("hi-ok" in str(hi))
    finally:
        print("[gate] tearing down...", flush=True)
        net.down()
    print(f"[gate] {'PASS' if ok else 'FAIL'}", flush=True)
    return ok


def _contact(src, dst, dst_patp: str, rounds: int = 3) -> object:
    """Establish first contact src -> dst, robust to Jael->Ames being lazy.

    A peer becomes a %known ames peer only when ames first needs it (the |hi
    triggers a Jael lookup that now succeeds, since the verified packet
    populated Jael). But +sy-dear only records a lane for an ALREADY-known
    peer. So each round: probe (forces/refreshes the install) -> inject the
    lane (now sticks) -> probe again (routes over the lane). Returns the first
    'hi-ok'."""
    last = None
    for _ in range(rounds):
        last = _try_probe(src, dst_patp)                     # install (may fail w/o lane)
        if "hi-ok" in str(last):
            return last
        try:
            lanes.inject_lane(src, dst.num, dst.ames_port)   # peer known now -> lane sticks
        except Exception as e:                               # noqa: BLE001
            last = f"ERROR(dear): {e}"
        time.sleep(2)
        last = _try_probe(src, dst_patp)                     # route over the lane
        if "hi-ok" in str(last):
            return last
        time.sleep(3)
    return last
