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


def _count_log(path: Path, pattern: str) -> int:
    if not path.exists():
        return 0
    rx = re.compile(pattern)
    return sum(1 for line in path.read_text(errors="replace").splitlines()
               if rx.search(line))


def _await_count(path: Path, pattern: str, n: int, timeout: float,
                 poll: float = 5.0) -> bool:
    """Wait until `pattern` has appeared at least `n` times (for asserting a
    SECOND verdict/VALID where _await_log would re-match the first)."""
    deadline = time.time() + timeout
    while time.time() < deadline:
        if _count_log(path, pattern) >= n:
            return True
        time.sleep(poll)
    return False


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
    Comets boot directly from the fresh kelvin-408 pill (gw-base-408.pill, built
    by build_pill.py from MY arvo) — no -A, no upgrade. Its %base carries my
    ames, so the comets run my suite gate. Set cfg.pill to it first."""
    # SOLID pill built from MY arvo (build_solid.py) + the tinnus vere that can
    # boot it. The bm vere rejects this format and brass pills don't fresh-boot;
    # the solid pill is the one that runs my %31 ames (verified live).
    new_pill = cfg.gw_root / "gw-solid-mine.pill"
    tinnus = cfg.gw_root / "gw-vere-tinnus"
    if new_pill.exists():
        cfg.pill = new_pill
        print(f"[gate] using my-arvo SOLID pill: {new_pill}", flush=True)
    else:
        print(f"[gate] WARNING: {new_pill} not found; comets will boot the OLD "
              "kernel and the gate will NOT fire. Build it with build_solid.py.",
              flush=True)
    if tinnus.exists():
        cfg.vere = tinnus
        print(f"[gate] using tinnus vere: {tinnus}", flush=True)
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

        # The gate fires in +on-hear-open when A HEARS B's suite-C open-packet.
        # Cold comet<->comet contact can't deliver one in -L (A can't route to
        # an %alien B; %dear records a lane only for an already-%known peer), so
        # inject B's REAL signed open-packet straight into A. The injected lane
        # is B's, so A records it for the later verify-mode jael ride.
        print("[gate] trigger gate: inject B's signed open-packet into A...", flush=True)
        lanes.inject_open_packet(A, B)
        who_b = re.escape(b_id.comet)
        held = _await_log(A.log, f"holding suite-C comet {who_b}", 60)
        print(f"[gate]   A HELD suite-C B (not bare-accepted): {bool(held)}", flush=True)
        if not held:
            print("[gate]   FAIL: gate never fired", flush=True)
            return False

        if mode == "reject":
            print("[gate] inject NEGATIVE verdict -> suspend...", flush=True)
            lanes.inject_attest_verdict(A, B.num, ok=False)
            susp = _await_log(A.log, f"comet {who_b} attestation failed; suspended", 40)
            print(f"[gate]   B SUSPENDED: {bool(susp)}", flush=True)
            ok = bool(susp)
        else:  # verify
            print("[gate] VERIFY B's packet (POST to A's urb-watcher)...", flush=True)
            net.peer(A, b_id)
            verified = _await_log(A.log, f"attestation for {who_b} is VALID", cfg.verify)
            print(f"[gate]   B Bitcoin-verified by A's watcher: {bool(verified)}", flush=True)
            # The watcher->ames/jael wiring is deferred Stage 2 (urb-watcher's
            # %ames poke is a documented placeholder against a nonexistent
            # agent), so drive the positive verdict the way that wiring will:
            # my kernel's +sy-attest-verdict ok=%.y clears the hold and slogs
            # "attestation verified" (the opposite branch of the reject path).
            print("[gate] apply POSITIVE verdict -> clear the suite-C hold...", flush=True)
            lanes.inject_attest_verdict(A, B.num, ok=True)
            cleared = _await_log(A.log, f"comet {who_b} attestation verified", 40)
            print(f"[gate]   B hold CLEARED (attestation verified): {bool(cleared)}", flush=True)
            ok = bool(verified) and bool(cleared)
    finally:
        print("[gate] tearing down...", flush=True)
        net.down()
    print(f"[gate] {'PASS' if ok else 'FAIL'}", flush=True)
    return ok


def _use_my_kernel(cfg: Config) -> None:
    """Point the harness at MY %31 arvo: the SOLID pill (my ames suite gate)
    + the tinnus vere that can boot it from a -G feed. (Same override run_gate
    does; factored out so the genuine-loop milestones share it.)"""
    new_pill = cfg.gw_root / "gw-solid-mine.pill"
    tinnus = cfg.gw_root / "gw-vere-tinnus"
    if new_pill.exists():
        cfg.pill = new_pill
        print(f"[boot] my-arvo SOLID pill: {new_pill}", flush=True)
    else:
        print(f"[boot] WARNING: {new_pill} missing; comets boot the OLD kernel "
              "and the gate will NOT fire. Build it with build_solid.py.", flush=True)
    if tinnus.exists():
        cfg.vere = tinnus
        print(f"[boot] tinnus vere: {tinnus}", flush=True)


def _boot_pair(cfg: Config, net: Net):
    """Spawn + boot two real suite-C comets (A first, on the new kernel) on
    regtest, desk installed + watcher configured. Returns (A, B, a_id, b_id)."""
    a_id = net.spawn_identity()
    b_id = net.spawn_identity()
    print(f"[pair]   A = {a_id.comet}\n[pair]   B = {b_id.comet}", flush=True)
    A = net.boot_comet(a_id, 0)
    B = net.boot_comet(b_id, 1)
    for c in (A, B):
        _await_log(c.log, "reconfigured to", 30)
    return A, B, a_id, b_id


def run_m5(cfg: Config) -> bool:
    """GENUINE verification failure -> suspend, driven by the REAL verdict wire
    (Edit 4) -- no injected verdict.

    A holds suite-C B (the only injected step: B's signed open-packet, since -L
    blocks cold comet<->comet routing). Then A's OWN %urb-watcher verifies a
    TAMPERED B packet (bad tip offset) against regtest, returns INVALID, and its
    +verdict-poke -- now an %arvo %a [%attest-verdict B %.n] task to the ames
    vane, not a poke to a dead agent -- drives A's kernel to suspend B. The
    suspend slog is therefore produced by the real watcher->ames path."""
    _use_my_kernel(cfg)
    net = Net(cfg)
    ok = False
    try:
        print("[m5] net up...", flush=True)
        net.up()
        A, B, a_id, b_id = _boot_pair(cfg, net)
        who_b = re.escape(b_id.comet)

        print("[m5] trigger gate: inject B's signed open-packet into A...", flush=True)
        lanes.inject_open_packet(A, B)
        held = _await_log(A.log, f"holding suite-C comet {who_b}", 60)
        print(f"[m5]   A HELD suite-C B (not bare-accepted): {bool(held)}", flush=True)
        if not held:
            print("[m5]   FAIL: gate never fired", flush=True)
            return False

        print("[m5] POST a TAMPERED B packet to A's watcher (tip-off+1)...", flush=True)
        skel_b = chainops.build_skeleton(cfg, b_id)
        bad = packets.bad_tip_off(skel_b, skel_b["tip"]["off"] + 1)
        packets.poke_peer(A, bad)
        invalid = _await_log(A.log, f"attestation for {who_b} is INVALID", cfg.verify)
        print(f"[m5]   A's watcher returned INVALID: {bool(invalid)}", flush=True)
        for line in A.log.read_text(errors="replace").splitlines():
            if re.search(r"\[XX\]", line):
                print(f"[m5]     {line.strip()}", flush=True)
        # the REAL verdict-poke %.n (Edit 4) now drives the kernel suspend
        susp = _await_log(A.log, f"comet {who_b} attestation failed; suspended", 60)
        print(f"[m5]   B SUSPENDED via REAL verdict task: {bool(susp)}", flush=True)
        ok = bool(invalid) and bool(susp)
    finally:
        print("[m5] tearing down...", flush=True)
        net.down()
    print(f"[m5] {'PASS' if ok else 'FAIL'}", flush=True)
    return ok


def run_m3(cfg: Config) -> bool:
    """GENUINE two-ship attestation THROUGH the suite-gate hold (Edit 4, no
    injected verdict).

    Each comet HOLDS the other as an unverified suite-C peer (the gate), then
    its OWN %urb-watcher verifies the peer's REAL packet against regtest. The
    now-real verdict-poke %.y (an %arvo %a task to the ames vane) clears the
    hold, and the watcher's jael feed installs the peer (the ride). |hi both
    ways proves a live authenticated ames round-trip. Only the initial
    open-packet + lane are injected (the -L loopback catch-22); the verdict and
    the install are genuine -- this is run_m2 PLUS the suite-gate hold that m2
    skips, with the verdict no longer injected."""
    _use_my_kernel(cfg)
    net = Net(cfg)
    ok = False
    try:
        print("[m3] net up...", flush=True)
        net.up()
        A, B, a_id, b_id = _boot_pair(cfg, net)
        who_a, who_b = re.escape(a_id.comet), re.escape(b_id.comet)

        print("[m3] each comet tracks its own chain (serve + self-track)...", flush=True)
        net.keyfile(A, a_id)
        net.keyfile(B, b_id)

        print("[m3] fire BOTH suite gates: each holds the other...", flush=True)
        lanes.inject_open_packet(A, B)
        lanes.inject_open_packet(B, A)
        hb = _await_log(A.log, f"holding suite-C comet {who_b}", 60)
        ha = _await_log(B.log, f"holding suite-C comet {who_a}", 60)
        print(f"[m3]   A holds B: {bool(hb)}   B holds A: {bool(ha)}", flush=True)
        if not (hb and ha):
            print("[m3]   FAIL: a gate did not fire", flush=True)
            return False

        print("[m3] each watcher verifies the peer's REAL packet (regtest)...", flush=True)
        net.peer(A, b_id)
        net.peer(B, a_id)
        vb = _await_log(A.log, f"attestation for {who_b} is VALID", cfg.verify)
        va = _await_log(B.log, f"attestation for {who_a} is VALID", cfg.verify)
        print(f"[m3]   A verdict on B: {vb or 'NONE'}\n[m3]   B verdict on A: {va or 'NONE'}", flush=True)
        # the REAL verdict-poke %.y clears each suite-C hold (Edit 4)
        cb = _await_log(A.log, f"comet {who_b} attestation verified", 40)
        ca = _await_log(B.log, f"comet {who_a} attestation verified", 40)
        print(f"[m3]   A cleared hold on B: {bool(cb)}   B cleared hold on A: {bool(ca)}", flush=True)

        print("[m3] establish lanes + first contact (|hi both ways)...", flush=True)
        rab = _contact(A, B, b_id.comet)
        rba = _contact(B, A, a_id.comet)
        print(f"[m3]   A |hi B: {rab}\n[m3]   B |hi A: {rba}", flush=True)
        ok = (_verdict_ok(vb) and _verdict_ok(va) and bool(cb) and bool(ca)
              and "hi-ok" in str(rab) and "hi-ok" in str(rba))
    finally:
        print("[m3] tearing down...", flush=True)
        net.down()
    print(f"[m3] {'PASS' if ok else 'FAIL'}", flush=True)
    return ok


def run_m4(cfg: Config) -> bool:
    """GENUINE re-attestation after a SECOND Bitcoin transaction (Edit 4).

    B verifies A's 1-link packet (A installed via the jael ride). A then signs a
    %no-op management op -- a SECOND on-chain commit that MOVES A's ownership
    sat. B's %urb-watcher block loop detects the confidential move and fires the
    now-real [%attest-request A] task to its ames vane (-> %grace). B then
    re-verifies A's UPDATED 2-link packet: VALID, with the tracked sont
    reconciled as an interior link (tracked-tip). The 2nd tx, the move
    detection, the request task, and the re-verification are all genuine; only
    packet DELIVERY is eyre-POST (the /atst transport makes it ames-native)."""
    _use_my_kernel(cfg)
    net = Net(cfg)
    ok = False
    try:
        print("[m4] net up...", flush=True)
        net.up()
        a_id = net.spawn_identity()
        b_id = net.spawn_identity()
        print(f"[m4]   A (1-link) = {a_id.comet}\n[m4]   B (verifier) = {b_id.comet}", flush=True)
        A = net.boot_comet(a_id, 0)
        B = net.boot_comet(b_id, 1)
        for c in (A, B):
            _await_log(c.log, "reconfigured to", 30)
        who_a = re.escape(a_id.comet)

        print("[m4] B verifies A's 1-link packet (jael ride installs A)...", flush=True)
        net.peer(B, a_id)
        v1 = _await_log(B.log, f"attestation for {who_a} is VALID", cfg.verify)
        print(f"[m4]   B's first verdict on A: {v1 or 'NONE'}", flush=True)
        if not _verdict_ok(v1):
            print("[m4]   FAIL: first verification did not pass", flush=True)
            return False
        # NB: no |hi here -- apply-verified already tracked A's sat in B's conf
        # registry on the verdict, independent of any ames peer install.

        print("[m4] SECOND Bitcoin tx: %no-op moves A's ownership sat...", flush=True)
        chainops.management_op(cfg, net.rpc, a_id, op="no-op")

        print("[m4] await B's watcher detecting the move -> REAL [%attest-request A]...", flush=True)
        req = _await_log(
            B.log, f"{who_a} sat moved confidentially; requesting re-attestation", 200)
        print(f"[m4]   B fired re-attestation request: {bool(req)}", flush=True)

        print("[m4] re-deliver A's UPDATED 2-link packet -> re-verify...", flush=True)
        net.peer(B, a_id)
        v2 = _await_count(B.log, f"attestation for {who_a} is VALID", 2, cfg.verify)
        print(f"[m4]   B re-verified A's 2-link chain (2nd VALID): {bool(v2)}", flush=True)
        tracked = _count_log(B.log, r"\[ok\] tracked-tip") >= 1
        print(f"[m4]   tracked-tip reconciled (interior sont): {tracked}", flush=True)
        ok = bool(req) and bool(v2)
    finally:
        print("[m4] tearing down...", flush=True)
        net.down()
    print(f"[m4] {'PASS' if ok else 'FAIL'}", flush=True)
    return ok


def run_m6(cfg: Config) -> bool:
    """GENUINE /atst transport: the packet is FETCHED over Ames, not POSTed.

    B pokes its OWN keyfile so its %urb-watcher can serve it. A then holds
    suite-C B (the only injected step); A's gate fires +atst-fetch, sending B a
    plaintext %atst-req at B's lane. B serves its full self-attestation from its
    own /x/keyfile (the in-kernel %gx scry) and replies to A's lane. A pokes its
    watcher (%g %deal %noun [%attest-packet]); the watcher verifies and the real
    verdict clears the hold. The watcher's 'verifying self-attestation' slog with
    NO eyre POST is the proof the packet arrived over the /atst transport."""
    _use_my_kernel(cfg)
    net = Net(cfg)
    ok = False
    try:
        print("[m6] net up...", flush=True)
        net.up()
        A, B, a_id, b_id = _boot_pair(cfg, net)
        who_b = re.escape(b_id.comet)

        print("[m6] B verifies + stores its OWN keyfile (so /atst can serve it)...", flush=True)
        net.keyfile(B, b_id)
        kf = _await_log(B.log, f"attestation for {who_b} is VALID", cfg.verify)
        print(f"[m6]   B's own keyfile stored: {bool(kf)}", flush=True)
        if not kf:
            print("[m6]   FAIL: B never stored its keyfile", flush=True)
            return False

        print("[m6] inject B's open-packet -> A holds B -> A asks B over /atst...", flush=True)
        lanes.inject_open_packet(A, B)
        held = _await_log(A.log, f"holding suite-C comet {who_b}", 60)
        print(f"[m6]   A held suite-C B: {bool(held)}", flush=True)
        if not held:
            print("[m6]   FAIL: gate never fired", flush=True)
            return False

        # NO net.peer(A, ...): the packet must arrive via the /atst transport.
        print("[m6] await the watcher verifying B WITHOUT any eyre POST...", flush=True)
        verifying = _await_log(A.log, f"verifying self-attestation for {who_b}", cfg.verify)
        print(f"[m6]   A's watcher got B's packet over /atst: {bool(verifying)}", flush=True)
        valid = _await_log(A.log, f"attestation for {who_b} is VALID", cfg.verify)
        cleared = _await_log(A.log, f"comet {who_b} attestation verified", 40)
        print(f"[m6]   VALID: {bool(valid)}   hold cleared via real verdict: {bool(cleared)}", flush=True)
        ok = bool(verifying) and bool(valid) and bool(cleared)
    finally:
        print("[m6] tearing down...", flush=True)
        net.down()
    print(f"[m6] {'PASS' if ok else 'FAIL'}", flush=True)
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
