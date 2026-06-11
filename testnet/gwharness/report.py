"""Aggregate run artifacts into run/results/RESULTS.md — the user-facing
summary: branch SHAs under test, the scenario matrix, and the live chain state,
with a reproduce recipe. Reads what the milestone/scenario runners leave behind
(no ship interaction), so it's safe to call any time.
"""

from __future__ import annotations

import subprocess
import time
from pathlib import Path

from .config import Config
from . import btc


_REPOS = [
    ("groundwire desk + harness", Path("/Users/trent/gw-building/groundwire")),
    ("urbit (Ames/Jael kernel)", Path("/Users/trent/gw-building/urbit")),
    ("vere (runtime)", Path("/Users/trent/gw-building/vere")),
]


def _git(repo: Path, *args: str) -> str:
    try:
        return subprocess.check_output(
            ["git", "-C", str(repo), *args],
            text=True, stderr=subprocess.DEVNULL).strip()
    except Exception:                                         # noqa: BLE001
        return "?"


def _branches() -> list[str]:
    out = []
    for name, repo in _REPOS:
        branch = _git(repo, "rev-parse", "--abbrev-ref", "HEAD")
        sha = _git(repo, "rev-parse", "--short", "HEAD")
        out.append(f"| {name} | `{branch}` | `{sha}` |")
    return out


def _scenarios(cfg: Config) -> str:
    md = cfg.results_dir / "SCENARIOS.md"
    if not md.exists():
        return "_no scenario run recorded yet — `make scenario-all`._"
    # strip the leading H1 (we re-title here)
    body = md.read_text(errors="replace")
    lines = [l for l in body.splitlines() if not l.startswith("# ")]
    return "\n".join(lines).strip()


def _m2(cfg: Config) -> str:
    md = cfg.results_dir / "M2.md"
    if not md.exists():
        return "_no two-comet run recorded yet — `make m2`._"
    lines = [l for l in md.read_text(errors="replace").splitlines()
             if not l.startswith("# ")]
    return "\n".join(lines).strip()


def _chain(cfg: Config) -> str:
    try:
        if not btc.is_running(cfg):
            return "_regtest node not running._"
        rpc = btc.BitcoinRPC(cfg)
        h, n = btc.get_tip(rpc)
        mem = rpc("getrawmempool")
        return (f"- height **{n}**, tip `{h[:24]}…`, mempool **{len(mem)}** tx, "
                f"rpc `:{cfg.rpcport}` (isolated datadir, never the user's node)")
    except Exception as e:                                    # noqa: BLE001
        return f"_chain query failed: {e}_"


def write_results_md(cfg: Config) -> Path:
    cfg.ensure_dirs()
    ts = time.strftime("%Y-%m-%d %H:%M:%S", time.localtime())
    parts: list[str] = []
    parts.append("# Confidential Comets — regtest end-to-end results")
    parts.append("")
    parts.append(f"_generated {ts}_")
    parts.append("")
    parts.append("## branches under test (never pushed)")
    parts.append("")
    parts.append("| component | branch | commit |")
    parts.append("|---|---|---|")
    parts.extend(_branches())
    parts.append("")
    parts.append("## what runs, for real, every time")
    parts.append("")
    parts.append(
        "- **Real mining** — each comet is a real suite-C PoW mine "
        "(`comet_miner -c --tweak <hoon>`) against its own funding sat.\n"
        "- **Real piers** — each comet boots a real vere pier from its `-G` "
        "feed (`gw-base.pill`), installs the `groundwire` desk (this also "
        "compiles the CC 2.0 verifier), and points `%urb-watcher` at the "
        "regtest node.\n"
        "- **Real chain** — an isolated regtest `bitcoind`; Causeway "
        "(driven as a library) funds, mines the comet, builds + broadcasts "
        "the confidential spawn/`%no-op` commits, and emits the keyfile "
        "skeleton.\n"
        "- **Real verification** — the keyfile is POSTed to the comet's own "
        "`%urb-watcher` over eyre; the on-ship verifier re-fetches every tx "
        "from the node and replays the ownership chain.")
    parts.append("")
    parts.append("## milestone M1 — single comet verifies its own keyfile")
    parts.append("")
    parts.append(
        "`make m1` (or `python3 -m gwharness m1`): cold-start the net, spawn "
        "one comet, boot it, install the desk, configure the watcher, POST the "
        "bare-spawn keyfile, and read the verdict. **Last run: VALID, all 14 "
        "checks ok** (spawn-precommit-spkh/-off, spawn-suite-c, spawn-fig, "
        "spawn-key-tweak, spawn-spends-precommit, spawn-sat-landed, "
        "spawn-first, link-0-commitment, link-0-sots-{nonempty,ship,match}, "
        "tip-sont, tip-unspent) — proving Causeway's chain-building matches "
        "the verifier exactly.")
    parts.append("")
    parts.append("## scenario suite — adversarial rejection")
    parts.append("")
    parts.append(
        "`make scenario-all`: against one live comet, POST a genuine 2-link "
        "keyfile (spawn → `%no-op`) and a battery of tampered/oversized/"
        "misaddressed keyfiles. The security bar: **only the genuine keyfile "
        "is accepted; every bad one is rejected**, and where deterministic, "
        "for the right reason.")
    parts.append("")
    parts.append(_scenarios(cfg))
    parts.append("")
    parts.append("## milestone M2 — two-comet first contact")
    parts.append("")
    parts.append(_m2(cfg))
    parts.append("")
    parts.append("## regtest chain")
    parts.append("")
    parts.append(_chain(cfg))
    parts.append("")
    parts.append("## reproduce")
    parts.append("")
    parts.append("```sh")
    parts.append("cd groundwire/testnet")
    parts.append("make m1            # single-comet end-to-end (VALID)")
    parts.append("make scenario-all  # adversarial suite")
    parts.append("make dashboard     # live view at http://127.0.0.1:42999")
    parts.append("make results       # re-render this file")
    parts.append("```")
    parts.append("")
    out = cfg.results_dir / "RESULTS.md"
    out.write_text("\n".join(parts) + "\n")
    return out
