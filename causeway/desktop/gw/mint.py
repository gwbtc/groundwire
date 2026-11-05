"""From nothing to a running, verifiable confidential comet.

The order is forced by the protocol: install the release (Causeway needs
comet_miner from it); spawn (pick a funding UTXO, THEN mine, because the @p
commits that outpoint); wait for the confirmation; finalize (bake the
custody log into the feed: the first bootable feed); boot THAT feed.

There is one boot.  The ship comes up under the service manager, gets its
setup pokes, its peers and (on a public server) its name, and stays up.
"""

from __future__ import annotations

import json
import os
import re
import subprocess
import sys
import time
import urllib.request
from pathlib import Path

from . import peers, populate, service
from .cli import good, info, step, warn
from .layout import (GwError, MEMPOOL_API, ShipConfig, ensure_layout, list_ships, miner_bin,
                     mint_dir, self_argv, validate_patp, var_dir)
from .ship import Ship


def _tty() -> bool:
    return sys.stdin.isatty() and sys.stdout.isatty()


def mint(args) -> int:
    ensure_layout()
    md = mint_dir()
    md.mkdir(parents=True, exist_ok=True)
    os.chmod(md, 0o700)
    raw = md / "spawn.feed"
    baked = md / "boot.feed"

    if args.skip_attestation:
        name, feed = _mine_unattested(raw)
        proof = {}
    else:
        proof_path = _spawn(args, md, raw)
        proof = json.loads(Path(proof_path).read_text())
        name = validate_patp(proof.get("patp", ""))
        good(f"minted ~{name}")
        step("Baking the custody log into the boot feed")
        info("instant if Causeway already saw the confirmation; otherwise this waits for it.")
        r = subprocess.run(self_argv() + ["causeway", "finalize", str(proof_path),
                                          "--feed-file", str(raw), "--out-feed", str(baked)],
                           stdin=None if _tty() else subprocess.DEVNULL)
        if r.returncode != 0 or not baked.exists() or baked.stat().st_size == 0:
            raise GwError("causeway finalize failed. Your comet is minted and on chain; nothing is lost. "
                          f"Re-run:\n      gw causeway finalize {proof_path} --feed-file {raw} --out-feed {baked}\n"
                          f"    then  gw start ~{name} --feed-file {baked} --proof {proof_path}")
        good("custody log baked")
        feed = baked.read_text().strip()
        # keep the proof beside the pier: every start reads the sponsor's attestation from it
        (var_dir() / f"{name}.proof.json").write_text(json.dumps(proof))

    cfg = ShipConfig(name=name)
    cfg.release = _installed_tag()
    cfg.vps = bool(args.vps)
    cfg.armillary = not args.no_armillary
    cfg.bitcoin = not args.no_bitcoin
    cfg.peer_discovery = not (proof.get("peer_discovery") is False)
    if args.sponsor:
        cfg.sponsor = args.sponsor
    if args.port:
        cfg.http_port = args.port
    if args.ames_port:
        cfg.ames_port = args.ames_port
    elif args.fief:
        cfg.ames_port = int(args.fief.rsplit(":", 1)[1])
    if args.loom:
        cfg.loom = args.loom
    if args.gateway:
        cfg.gateway = args.gateway
    cfg.created = time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())
    cfg.save()

    from .commands import first_boot
    return first_boot(cfg, feed, proof, wait=not args.no_wait)


def _installed_tag() -> str:
    from .layout import release_info
    return release_info().get("tag", "")


def _spawn(args, md: Path, raw: Path) -> Path:
    """Run Causeway's spawn (TUI or prompts); return the proof path."""
    ui = "cli" if (args.headless or args.invite or args.resume or args.assume_saved
                   or args.phrase_file or not _tty()) else "tui"
    if ui == "tui":
        step("Minting a confidential comet (Causeway)")
        info("complete the spawn in the interface; gw resumes when you quit.")
        marker = md / ".mint-start"
        marker.touch()
        time.sleep(1)
        env = dict(os.environ, CAUSEWAY_SPONSOR=args.sponsor or "", CAUSEWAY_FIEF=args.fief or "",
                   CAUSEWAY_OUTPUT_DIR=str(md), CAUSEWAY_HANDOFF="1")
        subprocess.run(self_argv() + ["causeway-tui"], env=env)
        cands = sorted((p for p in md.glob("*-spawn*.proof.json") if p.stat().st_mtime > marker.stat().st_mtime),
                       key=lambda p: p.stat().st_mtime, reverse=True)
        proof = None
        for cand in cands:
            feedf = cand.with_suffix(".feed")
            if not feedf.exists() or feedf.stat().st_size == 0:
                continue
            txid = _proof_txid(cand)
            if txid and _tx_seen(txid):
                proof = cand
                break
            proof = proof or cand
        if proof is None:
            raise GwError("the interface exited without completing a spawn. Nothing was booted; "
                          "run `gw mint` again, or `gw mint --headless` for the prompt flow.")
        if not _tx_seen(_proof_txid(proof) or ""):
            raise GwError("the spawn transaction was built but the network has not seen it:\n"
                          f"      {_proof_txid(proof)}\n    Nothing was spent and nothing was booted. If your "
                          "wallet did broadcast it, wait a minute and finish by hand:\n"
                          f"      gw causeway finalize {proof} --feed-file {proof.with_suffix('.feed')} "
                          f"--out-feed {raw.parent / 'boot.feed'}")
        feedf = proof.with_suffix(".feed")
        raw.write_bytes(feedf.read_bytes())
        os.chmod(raw, 0o600)
        return proof

    step("Minting a confidential comet with Causeway")
    cmd = self_argv() + ["causeway", "spawn", "generate", "--output-dir", str(md), "--out-feed", str(raw),
                         "--miner", str(miner_bin())]
    if args.invite:
        cmd += ["--invite", args.invite]
    if args.sponsor:
        cmd += ["--sponsor", args.sponsor]
    if args.fief:
        cmd += ["--fief", args.fief]
    if args.resume:
        cmd += ["--resume"]
    if args.assume_saved or args.phrase_file:
        cmd += ["--assume-saved"]
    if args.phrase_file:
        cmd += ["--phrase-file", args.phrase_file]
    if args.prefix:
        cmd += ["--prefix", args.prefix]
    if args.candidates and args.candidates > 1:
        cmd += ["--candidates", str(args.candidates)]
        if args.pick:
            cmd += ["--pick", str(args.pick)]
    info("this asks you to fund an address and to write down a recovery phrase. Do not skip the phrase.")
    r = subprocess.run(cmd, stdin=None if _tty() else subprocess.DEVNULL)
    if r.returncode != 0:
        raise GwError("causeway spawn failed or was cancelled")
    if not raw.exists() or raw.stat().st_size == 0:
        raise GwError(f"causeway spawn did not write a feed to {raw}. Nothing was booted; if the "
                      f"transaction broadcast, the proof is in {md} and `gw causeway finalize` finishes it.")
    proofs = sorted(md.glob("*-spawn.proof.json"), key=lambda p: p.stat().st_mtime, reverse=True)
    if not proofs:
        raise GwError(f"no spawn proof found in {md}")
    return proofs[0]


def _proof_txid(p: Path) -> str:
    try:
        return str(json.loads(p.read_text()).get("commit_txid", ""))
    except Exception:
        return ""


def _tx_seen(txid: str) -> bool:
    if not txid:
        return False
    try:
        with urllib.request.urlopen(f"{MEMPOOL_API}/tx/{txid}/status", timeout=10) as r:
            return r.status == 200
    except Exception:
        return False


def _mine_unattested(out: Path) -> tuple[str, str]:
    """A comet against a zeroed satpoint: the right @p shape, nothing on
    chain, no custody log.  For testing only."""
    miner = miner_bin()
    if not miner.exists():
        raise GwError(f"no comet_miner at {miner}; run `gw install` first")
    step("Mining an unattested comet (--skip-attestation)")
    info("no funding and no spawn transaction: this comet commits to a zeroed satpoint")
    info("and no peer can ever verify it. For testing only. Mining takes a few minutes...")
    tweak = "(can 0 (mat %gw-btc) (mat 9) (mat (jam [txid=0x0 vout=0 off=0])) ~)"
    r = subprocess.run([str(miner), "-c", "--tweak", tweak, "daplyd"], capture_output=True, text=True)
    if r.returncode != 0:
        raise GwError("comet_miner failed:\n" + "\n".join(r.stdout.splitlines()[-8:]))
    comet = feed = ""
    for line in r.stdout.splitlines():
        if line.startswith("comet:"):
            comet = line.split(":", 1)[1].strip()
        elif line.startswith("feed:"):
            feed = line.split(":", 1)[1].strip()
    if not comet or not feed:
        raise GwError("could not read a comet and a feed out of comet_miner's output")
    with open(out, "w", opener=lambda p, f: os.open(p, f, 0o600)) as fh:
        fh.write(feed + "\n")
    good(f"mined {comet}")
    return validate_patp(comet), feed
