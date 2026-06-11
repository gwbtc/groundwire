"""Drive the desktop Causeway as a library to build confidential chains on
regtest. Replicates Causeway's spawn/management flow non-interactively (no seed
prompts, no faucet), with the harness funding the derived address.
"""

from __future__ import annotations

import sys
from dataclasses import dataclass, field
from pathlib import Path

from .config import Config
from . import btc as _btc


def _causeway(cfg: Config):
    """Import the desktop causeway module (added to sys.path once)."""
    d = str(cfg.groundwire / "causeway" / "desktop")
    if d not in sys.path:
        sys.path.insert(0, d)
    import causeway  # noqa: E402
    return causeway


def _backend(cw, cfg: Config):
    return cw.make_backend(
        "regtest",
        rpc_url=f"http://127.0.0.1:{cfg.rpcport}",
        rpc_user=cfg.rpcuser, rpc_pass=cfg.rpcpass,
    )


@dataclass
class Identity:
    """A confidential comet's wallet + accumulating proof chain."""
    comet: str
    feed: str
    ring: str
    mnemonic: str
    proofs: list[dict] = field(default_factory=list)
    paths: list[Path] = field(default_factory=list)


def spawn(cfg: Config, rpc, fund_sats: int = 20_000, fee_rate: int = 2,
          mnemonic: str | None = None) -> Identity:
    """Fund a fresh wallet, mine a suite-C comet against the funding sat, build
    + broadcast the spawn commit, and emit its proof.json."""
    cw = _causeway(cfg)
    net = "regtest"
    mnemonic = mnemonic or cw.generate_new_mnemonic(strength_bits=128)
    root = cw.mnemonic_to_hdkey(mnemonic, network=net)
    account_path = [cw._hardened(86), cw._hardened(1), cw._hardened(0)]
    account_xpub = root.derive(cw._path_to_str(account_path))
    source = cw.KeySource(
        xpub=account_xpub, master_fingerprint=cw.hdkey_fingerprint(root),
        account_path=account_path, network=net)

    first_addr = source.derive_address(0, 0)[0]
    _btc.fund_address(rpc, cfg.wallet, first_addr, fund_sats, conf=2)

    backend = _backend(cw, cfg)
    utxos = [u for u in cw.scan_addresses(source, n_receive=5, n_change=2,
                                          backend=backend) if u["confirmed"]]
    if not utxos:
        raise RuntimeError(f"funding for {first_addr} not visible to causeway")
    utxo = max(utxos, key=lambda u: u["value"])

    mined = cw.mine_comet_from_utxo(utxo["txid"], utxo["vout"], 0, str(cfg.comet_miner))
    comet, feed, ring = mined["comet"], mined["feed"], mined.get("ring", "")

    tweak = cw.build_tweak_bytes(utxo["txid"], utxo["vout"], 0)
    pass_atom = cw.derive_pass_from_ring(ring, tweak)
    attestation = cw.encode_spawn_sotx(
        comet_p=cw.patp_to_int(comet), pass_atom=pass_atom,
        spkh=cw.compute_spkh(utxo["address"], utxo["value"]),
        vout=utxo["vout"], off=0, tej=0, fief=None)

    psbt_obj, proof = cw._build_spawn_psbt_and_proof(
        utxo=utxo, source=source, attestation_bytes=attestation, fee_rate=fee_rate)
    proof.update(op="spawn", patp=comet, pass_atom_hex=hex(pass_atom))
    _record_block(cw, proof.setdefault("funding", {}), utxo["txid"], backend, key="block_hex")

    psbt_obj.sign_with(root)
    _local_txid, tx_hex = cw._extract_tx_from_psbt(psbt_obj.to_base64())
    # sendrawtransaction returns the node-canonical txid; trust it.
    commit_txid = cw._broadcast_tx(tx_hex, backend=backend)
    proof["commit_txid"] = commit_txid
    _btc.confirm(rpc, cfg.wallet, 2)
    _record_block(cw, proof, commit_txid, backend, key="commit_block_hex")

    ident = Identity(comet=comet, feed=feed, ring=ring, mnemonic=mnemonic)
    _save(cfg, ident, proof, "spawn")
    return ident


def management_op(cfg: Config, rpc, ident: Identity, op: str = "no-op",
                  fee_rate: int = 2) -> Identity:
    """Chain a management op onto an identity's latest commit. Replicates
    Causeway's _run_management_op non-interactively: spend the point's current
    sont (the prior op's commit output), build a new confidential commit for
    `op`'s sotx, broadcast, and append the proof. A %no-op produces a second
    link with no PKI change — under the latest-sotx-disclosed rule the keyfile
    now carries two links and the verifier validates the new tip."""
    cw = _causeway(cfg)
    backend = _backend(cw, cfg)
    prior = ident.proofs[-1]
    comet_p = cw.patp_to_int(ident.comet)
    if op == "no-op":
        attestation = cw.encode_no_op_sotx(comet_p=comet_p)
    else:
        raise ValueError(f"unsupported management op: {op!r}")

    psbt_obj, proof = cw.build_chained_commit_psbt(
        prior_proof=prior, new_attestation_bytes=attestation,
        fee_rate=fee_rate, network="regtest")
    proof.update(op=op, patp=ident.comet)
    # The funding tx for a management op IS the prior commit; carry its block
    # hash forward (the verifier feeds it to getrawtransaction's blockhash arg).
    prior_block = prior.get("commit_block_hex", "")
    if prior_block:
        proof.setdefault("funding", {})["block_hex"] = prior_block

    root = cw.mnemonic_to_hdkey(ident.mnemonic, network="regtest")
    psbt_obj.sign_with(root)
    _local_txid, tx_hex = cw._extract_tx_from_psbt(psbt_obj.to_base64())
    commit_txid = cw._broadcast_tx(tx_hex, backend=backend)
    proof["commit_txid"] = commit_txid
    _btc.confirm(rpc, cfg.wallet, 2)
    _record_block(cw, proof, commit_txid, backend, key="commit_block_hex")

    _save(cfg, ident, proof, op)
    return ident


def _record_block(cw, target: dict, txid: str, backend, key: str) -> None:
    try:
        h = cw.fetch_block_hex(txid, backend, quiet=True)
        if h:
            target[key] = h
    except Exception:
        pass


def _save(cfg: Config, ident: Identity, proof: dict, op: str) -> None:
    cw = _causeway(cfg)
    outdir = cfg.comets_dir / ident.comet.lstrip("~")
    outdir.mkdir(parents=True, exist_ok=True)
    path = outdir / f"{ident.comet.lstrip('~')}-{len(ident.proofs)}-{op}.proof.json"
    cw.write_proof_json(proof, str(path))
    ident.proofs.append(proof)
    ident.paths.append(path)


def build_skeleton(cfg: Config, ident: Identity) -> dict:
    """The 2.0 keyfile skeleton for this identity's current proof chain."""
    cw = _causeway(cfg)
    return cw.build_packet_skeleton(ident.proofs)
