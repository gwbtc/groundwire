"""gwharness CLI.

    python3 -m gwharness selftest          # offline plumbing checks
    python3 -m gwharness net-up            # start regtest bitcoind + wallet
    python3 -m gwharness net-down          # stop bitcoind
    python3 -m gwharness mine N=1          # mine N regtest blocks
    python3 -m gwharness info              # chain tip + running piers

Comet / scenario subcommands land in later milestones.
"""

from __future__ import annotations

import argparse
import sys

from .config import load_config
from . import btc
from pathlib import Path as _P
HERE = _P(__file__).resolve().parent


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(prog="gwharness")
    ap.add_argument("--config", default=None)
    sub = ap.add_subparsers(dest="cmd", required=True)
    sub.add_parser("selftest")
    sub.add_parser("net-up")
    sub.add_parser("net-down")
    p_mine = sub.add_parser("mine")
    p_mine.add_argument("n", type=int, nargs="?", default=1)
    sub.add_parser("info")
    sub.add_parser("m1")
    sub.add_parser("m2")
    p_scn = sub.add_parser("scenarios")
    p_scn.add_argument("--only", default=None,
                       help="comma-separated scenario names to run")
    sub.add_parser("scenario-all")
    p_dash = sub.add_parser("dashboard")
    p_dash.add_argument("--port", type=int, default=42999)
    sub.add_parser("results")
    args = ap.parse_args(argv)

    if args.cmd == "selftest":
        import subprocess
        return subprocess.call(
            [sys.executable, "-m", "pytest", "-q",
             str(HERE / "tests"), "-k", "not live"])

    cfg = load_config(args.config) if args.config else load_config()
    cfg.ensure_dirs()

    if args.cmd == "net-up":
        btc.start_bitcoind(cfg)
        rpc = btc.BitcoinRPC(cfg)
        btc.ensure_wallet(rpc, cfg.wallet)
        btc.ensure_maturity(rpc, cfg.wallet)
        h, n = btc.get_tip(rpc)
        bal = rpc("getbalance", wallet=cfg.wallet)
        print(f"regtest up: height {n}, tip {h[:16]}…, wallet balance {bal} BTC")
        return 0

    if args.cmd == "net-down":
        btc.stop_bitcoind(cfg)
        print("regtest stopped")
        return 0

    if args.cmd == "mine":
        rpc = btc.BitcoinRPC(cfg)
        hashes = btc.mine(rpc, cfg.wallet, args.n)
        print(f"mined {len(hashes)} block(s); tip {hashes[-1][:16]}…")
        return 0

    if args.cmd == "m1":
        from .milestones import run_m1
        return 0 if run_m1(cfg) else 1

    if args.cmd == "m2":
        from .milestones import run_m2
        return 0 if run_m2(cfg) else 1

    if args.cmd == "scenarios":
        from .scenarios import run_scenarios
        only = args.only.split(",") if args.only else None
        return 0 if run_scenarios(cfg, only=only) else 1

    if args.cmd == "scenario-all":
        from .scenarios import run_scenarios
        return 0 if run_scenarios(cfg) else 1

    if args.cmd == "dashboard":
        from .dashboard import serve
        serve(cfg, port=args.port)
        return 0

    if args.cmd == "results":
        from .report import write_results_md
        path = write_results_md(cfg)
        print(f"wrote {path}")
        return 0

    if args.cmd == "info":
        rpc = btc.BitcoinRPC(cfg)
        if btc.is_running(cfg):
            h, n = btc.get_tip(rpc)
            mem = rpc("getrawmempool")
            print(f"bitcoind: height {n}, tip {h[:16]}…, mempool {len(mem)}")
        else:
            print("bitcoind: not running")
        piers = sorted(cfg.piers_dir.glob("*/.urb/conn.sock")) if cfg.piers_dir.exists() else []
        print(f"piers: {len(piers)} with a live conn.sock")
        for p in piers:
            print(f"  {p.parent.parent.name}")
        return 0

    return 2


if __name__ == "__main__":
    sys.exit(main())
