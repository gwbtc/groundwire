"""Adversarial scenario suite for the CC 2.0 keyfile verifier.

Each scenario POSTs a (possibly tampered) keyfile skeleton to a live comet's
own %urb-watcher and asserts the verdict slogged to its pier log. The security
property under test is simple and strong:

    a VALID keyfile is accepted with the expected checks;
    every tampered/oversized/misaddressed keyfile is REJECTED — never accepted
    as VALID — and, where the rejection is deterministic, for the RIGHT reason
    (a specific failed check, or a specific refusal message).

All scenarios run against ONE booted comet (one @p), re-poking its own keyfile
endpoint. The watcher serializes verifications (one in-flight at a time) and a
bad keyfile only reports — it never mutates PKI state — so the adversarial
cases are order-independent; we run them first (no tracked sont), then apply
the genuine 2-link keyfile last.

Outcomes (parsed from the pier log region after each poke):
    VALID    report:lsa verdict, ok=%.y     (acceptance)
    INVALID  report:lsa verdict, ok=%.n     (clean rejection, per-check break)
    REFUSED  on-poke guard fired (too long / not-us / in-flight / public)
    CRASH    verify thread bailed (e.g. a lying blockhash fails the fetch)
    TIMEOUT  nothing slogged in time
Only VALID is acceptance; the other four all satisfy a REJECT expectation.
"""

from __future__ import annotations

import re
import time
from dataclasses import dataclass, field
from pathlib import Path

from .config import Config
from . import chainops, packets
from .harness import Net


# -- log outcome parsing ----------------------------------------------------

@dataclass
class Outcome:
    kind: str                       # VALID | INVALID | REFUSED | CRASH | TIMEOUT
    checks: dict[str, bool] = field(default_factory=dict)
    message: str = ""               # refusal/crash slog line, when relevant
    region: str = ""                # the raw new log text (for diagnostics)

    @property
    def accepted(self) -> bool:
        return self.kind == "VALID"

    def failed_checks(self) -> list[str]:
        return [n for n, ok in self.checks.items() if not ok]


_VERDICT = re.compile(r"attestation for (\S+) is (VALID|INVALID)")
_CHECK = re.compile(r"\[(ok|XX)\]\s+(\S+)")
_REFUSE = re.compile(
    r"(chain (?:for \S+ )?too long; refused"
    r"|keyfile chain too long; refused"
    r"|keyfile is for \S+, not us; refused"
    r"|already in flight; refused"
    r"|public on-chain[^\n]*refused"
    r"|we are public on-chain; keyfile refused)")
_CRASH = re.compile(r"(verify thread for \S+ crashed"
                    r"|verification thread crashed"
                    r"|re-verification failed: unspecified behavior"
                    r"|bad skeleton POST)")


def _parse_region(text: str) -> Outcome | None:
    """Classify the newest log region. Returns None if no terminal outcome yet."""
    mv = _VERDICT.search(text)
    if mv:
        checks = {name: (tag == "ok") for tag, name in _CHECK.findall(text)}
        return Outcome(kind=mv.group(2), checks=checks, region=text)
    mr = _REFUSE.search(text)
    if mr:
        return Outcome(kind="REFUSED", message=mr.group(1), region=text)
    mc = _CRASH.search(text)
    if mc:
        return Outcome(kind="CRASH", message=mc.group(1), region=text)
    return None


def _await_outcome(log: Path, offset: int, timeout: float,
                   poll: float = 3.0) -> Outcome:
    deadline = time.time() + timeout
    while time.time() < deadline:
        if log.exists():
            text = log.read_text(errors="replace")[offset:]
            out = _parse_region(text)
            if out is not None:
                return out
        time.sleep(poll)
    text = log.read_text(errors="replace")[offset:] if log.exists() else ""
    return Outcome(kind="TIMEOUT", region=text)


def _log_size(log: Path) -> int:
    return log.stat().st_size if log.exists() else 0


# -- scenario model ---------------------------------------------------------

@dataclass
class Scenario:
    name: str
    desc: str
    build: object                   # (good_skel: dict) -> dict   (the skeleton to POST)
    expect: str                     # "VALID" | "REJECT"
    endpoint: str = "keyfile"       # keyfile | peer
    want_check_xx: str | None = None   # for REJECT: this check must be present & XX
    want_refuse: str | None = None     # for REJECT: refusal message substring


@dataclass
class Result:
    scenario: Scenario
    outcome: Outcome
    passed: bool
    note: str = ""


def _judge(sc: Scenario, out: Outcome) -> Result:
    if sc.expect == "VALID":
        if not out.accepted:
            return Result(sc, out, False, f"expected VALID, got {out.kind} {out.message}")
        bad = out.failed_checks()
        if bad:
            return Result(sc, out, False, f"VALID but checks XX: {bad}")
        return Result(sc, out, True, f"VALID ({len(out.checks)} checks ok)")
    # expect REJECT
    if out.accepted:
        return Result(sc, out, False, "SECURITY: tampered keyfile was ACCEPTED")
    note = out.kind
    if out.kind == "TIMEOUT":
        return Result(sc, out, False, "no outcome slogged (timeout)")
    if sc.want_check_xx is not None:
        if out.kind != "INVALID":
            return Result(sc, out, False,
                          f"wanted check {sc.want_check_xx!r} XX but outcome was {out.kind}")
        if sc.want_check_xx not in out.checks:
            return Result(sc, out, False,
                          f"check {sc.want_check_xx!r} not in verdict ({list(out.checks)})")
        if out.checks[sc.want_check_xx]:
            return Result(sc, out, False, f"check {sc.want_check_xx!r} unexpectedly ok")
        note = f"INVALID, {sc.want_check_xx} XX (correctly)"
    if sc.want_refuse is not None:
        if sc.want_refuse not in out.message:
            return Result(sc, out, False,
                          f"wanted refusal ~{sc.want_refuse!r}, got {out.kind} {out.message!r}")
        note = f"REFUSED: {out.message}"
    return Result(sc, out, True, note)


# -- tamper builders --------------------------------------------------------

def _flip_internal_key(skel: dict, i: int = 0) -> dict:
    import copy
    s = copy.deepcopy(skel)
    h = s["links"][i]["internal_key_hex"]
    # flip the low nibble of the last byte of the 33-byte compressed key
    last = int(h[-1], 16) ^ 0x1
    s["links"][i]["internal_key_hex"] = h[:-1] + f"{last:x}"
    return s


def _swap_links(skel: dict, a: int, b: int) -> dict:
    """Swap both txid AND block of two links (so each still fetches a real tx,
    but at the other link's position) — testing that you cannot substitute /
    reorder the ownership chain: each output now commits to the OTHER link's
    leaf, so the commitment + spawn-position checks fail."""
    import copy
    s = copy.deepcopy(skel)
    for k in ("txid", "block"):
        s["links"][a][k], s["links"][b][k] = s["links"][b][k], s["links"][a][k]
    return s


def _corrupt_precommit_block(skel: dict) -> dict:
    import copy
    s = copy.deepcopy(skel)
    h = s["precommit"]["block"]
    if h:
        last = int(h[-1], 16) ^ 0x1
        s["precommit"]["block"] = h[:-1] + f"{last:x}"
    return s


def _wrong_who(skel: dict) -> dict:
    import copy
    from . import obphon
    s = copy.deepcopy(skel)
    # a different, real comet @p (128-bit, in the comet range) that is NOT our
    # ship, so the watcher's "keyfile is for X, not us" guard fires on a
    # well-formed packet rather than the client choking on a bogus @p.
    other = obphon.num_to_patp((1 << 64) + 0xDEAD_BEEF_CAFE_F00D)
    if other == s["who"]:                                    # astronomically unlikely
        other = obphon.num_to_patp((1 << 64) + 0x1234_5678_9ABC_DEF0)
    s["who"] = other
    return s


# -- the suite --------------------------------------------------------------

def build_suite() -> list[Scenario]:
    """Single-comet keyfile scenarios. `build` receives the genuine 2-link
    skeleton (spawn + no-op) and returns the skeleton to POST."""
    ident = lambda s: s                                       # noqa: E731
    return [
        Scenario(
            "wrong-who-refused",
            "keyfile claims a different @p than the ship -> on-poke refusal",
            build=_wrong_who, expect="REJECT",
            want_refuse="not us"),
        Scenario(
            "chain-cap-refused",
            "keyfile padded past the 1024-link cap -> DoS guard refuses",
            build=lambda s: packets.pad_links(s, 1025), expect="REJECT",
            want_refuse="too long"),
        Scenario(
            "tamper-tip-off",
            "tip claims the wrong sat offset -> tip-sont fails",
            build=lambda s: packets.bad_tip_off(s, s["tip"]["off"] + 1),
            expect="REJECT", want_check_xx="tip-sont"),
        Scenario(
            "tamper-internal-key",
            "link-0 internal key flipped -> recomputed Q != on-chain output key",
            build=lambda s: _flip_internal_key(s, 0),
            expect="REJECT", want_check_xx="link-0-commitment"),
        Scenario(
            "tamper-swap-links",
            "link-0 and link-1 swapped (txid+block) -> each output commits to "
            "the other's leaf; the spawn is no longer first",
            build=lambda s: _swap_links(s, 0, 1),
            expect="REJECT"),
        Scenario(
            "tamper-precommit-block",
            "lying blockhash on the precommit -> getrawtransaction fetch fails "
            "cleanly (fetch:precommit-tx), no crash",
            build=_corrupt_precommit_block, expect="REJECT",
            want_check_xx="fetch:precommit-tx"),
        Scenario(
            "tamper-truncate-to-spawn",
            "drop the no-op link, keep tip at the spent spawn output -> "
            "tip-unspent fails (the no-op already spent it)",
            build=lambda s: packets.truncate(s, 1),
            expect="REJECT", want_check_xx="tip-unspent"),
        Scenario(
            "noop-reattest-valid",
            "genuine 2-link keyfile (spawn -> no-op) -> VALID, link-1 verified",
            build=ident, expect="VALID"),
    ]


# -- runner -----------------------------------------------------------------

def run_scenarios(cfg: Config, only: list[str] | None = None) -> bool:
    net = Net(cfg)
    results: list[Result] = []
    try:
        print("[scn] net up (regtest + wallet + desk build)...", flush=True)
        net.up()

        print("[scn] spawn identity + chain a %no-op (2-link keyfile)...", flush=True)
        ident = net.spawn_identity()
        chainops.management_op(cfg, net.rpc, ident, op="no-op")
        print(f"[scn]   comet = {ident.comet}  ({len(ident.proofs)} proofs)", flush=True)

        print("[scn] boot pier + install desk + config watcher...", flush=True)
        c = net.boot_comet(ident, 0)
        from .milestones import _await_log
        cfg_hit = _await_log(c.log, "reconfigured to", 30)
        print(f"[scn]   {cfg_hit or 'WARN: watcher not reconfigured'}", flush=True)

        good = chainops.build_skeleton(cfg, ident)             # genuine 2-link
        suite = build_suite()
        if only:
            suite = [s for s in suite if s.name in only]

        print(f"[scn] running {len(suite)} scenarios against {ident.comet}\n", flush=True)
        for sc in suite:
            skel = sc.build(good)
            off = _log_size(c.log)
            try:
                status = packets._post(c, skel, sc.endpoint)
            except Exception as e:                             # noqa: BLE001
                # a 400 (e.g. unparseable skeleton) is itself a rejection
                out = Outcome(kind="REFUSED", message=f"http {e}")
                results.append(_judge(sc, out))
                _print_result(results[-1])
                continue
            out = _await_outcome(c.log, off, cfg.verify)
            res = _judge(sc, out)
            results.append(res)
            _print_result(res, status)
    finally:
        print("\n[scn] tearing down...", flush=True)
        net.down()

    _write_results(cfg, results)
    try:
        from .report import write_results_md
        write_results_md(cfg)
    except Exception:                                        # noqa: BLE001
        pass
    npass = sum(r.passed for r in results)
    print(f"\n[scn] {npass}/{len(results)} scenarios passed", flush=True)
    return npass == len(results) and len(results) > 0


def _print_result(res: Result, status: int | None = None) -> None:
    tag = "PASS" if res.passed else "FAIL"
    mark = "✓" if res.passed else "✗"
    print(f"  [{tag}] {mark} {res.scenario.name:24} {res.note}", flush=True)
    if not res.passed:
        # dump the offending log region for debugging
        for line in res.outcome.region.splitlines():
            if re.search(r"urb-watcher|\[(ok|XX)\]", line):
                print(f"          | {line.strip()}", flush=True)


def _write_results(cfg: Config, results: list[Result]) -> None:
    out = cfg.results_dir / "SCENARIOS.md"
    lines = ["# CC 2.0 keyfile scenario results", ""]
    npass = sum(r.passed for r in results)
    lines.append(f"**{npass}/{len(results)} passed**")
    lines.append("")
    lines.append("| scenario | expect | outcome | result | note |")
    lines.append("|---|---|---|---|---|")
    for r in results:
        oc = r.outcome.kind
        if r.outcome.failed_checks():
            oc += f" ({','.join(r.outcome.failed_checks())})"
        lines.append(
            f"| {r.scenario.name} | {r.scenario.expect} | {oc} | "
            f"{'PASS' if r.passed else 'FAIL'} | {r.note} |")
    lines.append("")
    lines.append("## descriptions")
    lines.append("")
    for r in results:
        lines.append(f"- **{r.scenario.name}** — {r.scenario.desc}")
    out.write_text("\n".join(lines) + "\n")
    print(f"[scn] wrote {out}", flush=True)
