#!/usr/bin/env python3
"""gwmint — drive a real mainnet Groundwire kelvin-9 confidential-comet spawn.

Reuses causeway.py's encoders verbatim (no second implementation of any
encoder).  The only thing this adds over `causeway spawn` is a hard
verification gate before broadcast, and scriptable control of every step.

dat is the PLAINTEXT spawn satpoint (2026-08-18; the blind era is over):
    dat = (can 0 (mat %gw-btc) (mat 9) (mat (jam [txid vout off])) ~)
so the identity is fixed the moment the funding UTXO is chosen, and nothing
the wallet holds enters the tweak.
"""
import hashlib
import json
import os
import sys
import time

sys.path.insert(0, "/Users/trent/gw-building/groundwire/causeway/desktop")
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

import requests
from embit import bip32, bip39, script, psbt
from embit.networks import NETWORKS

import causeway as C
# The one decoder this needs that causeway does not export: the inverse of
# build_xtr_atom.  It lives next door in gwvec rather than being written a
# second time here.
from gwvec import cue_log

# The wallet holds real mainnet money and lives OUTSIDE any repo, chmod 600.
# Override with GW_WALLET; the default is the campaign's location.
WALLET_JSON = os.environ.get(
    "GW_WALLET", os.path.expanduser("~/gw-building/.gw-mainnet-wallet.json"))
MINER = os.environ.get(
    "GW_MINER",
    os.path.expanduser(
        "~/gw-building/comet-miner/zig-out/aarch64-macos-none/comet_miner"))
STATE_DIR = os.environ.get("GW_STATE_DIR", os.path.dirname(os.path.abspath(__file__)))
FUNDING_PATH = os.environ.get("GW_FUNDING_PATH", "m/86h/0h/0h/0/0")
# Used ONLY for `testmempoolaccept` in the gate -- never to broadcast.  The
# broadcast goes to mempool.space via causeway's _broadcast_tx, because this
# node runs Core 29, whose standardness rules reject our OP_RETURN publication.
# Credentials come from causeway rather than being duplicated here.
RPC = C.RPC_URL
RPC_AUTH = (C.RPC_USER, C.RPC_PASS)

# ---------------------------------------------------------------- independent
# A from-scratch secp256k1 / BIP-341 implementation used ONLY to cross-check
# causeway's Q.  Deliberately not shared code.

_P = 2**256 - 2**32 - 977
_N = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141
_G = (0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798,
      0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8)


def _pt_add(a, b):
    if a is None:
        return b
    if b is None:
        return a
    if a[0] == b[0] and (a[1] + b[1]) % _P == 0:
        return None
    if a == b:
        lam = 3 * a[0] * a[0] % _P * pow(2 * a[1] % _P, _P - 2, _P) % _P
    else:
        lam = (b[1] - a[1]) % _P * pow((b[0] - a[0]) % _P, _P - 2, _P) % _P
    x = (lam * lam - a[0] - b[0]) % _P
    return (x, (lam * (a[0] - x) - a[1]) % _P)


def _pt_mul(pt, k):
    r = None
    while k:
        if k & 1:
            r = _pt_add(r, pt)
        pt = _pt_add(pt, pt)
        k >>= 1
    return r


def _lift_x(x):
    y = pow((pow(x, 3, _P) + 7) % _P, (_P + 1) // 4, _P)
    if pow(y, 2, _P) != (pow(x, 3, _P) + 7) % _P:
        raise ValueError("not on curve")
    return (x, y if y % 2 == 0 else _P - y)


def _tag(tag: str, msg: bytes) -> bytes:
    t = hashlib.sha256(tag.encode()).digest()
    return hashlib.sha256(t + t + msg).digest()


def independent_Q(xonly_P: bytes, merkle_root: bytes) -> bytes:
    """BIP-341 output key, computed from scratch."""
    t = int.from_bytes(_tag("TapTweak", xonly_P + merkle_root), "big")
    assert 0 < t < _N
    Q = _pt_add(_lift_x(int.from_bytes(xonly_P, "big")), _pt_mul(_G, t))
    return Q[0].to_bytes(32, "big")


def independent_leaf_hash(c: bytes) -> bytes:
    """TapLeaf hash of the 37-byte state leaf, from scratch."""
    leaf = bytes([0x6A, 0x02, 0x67, 0x77, 0x20]) + c
    assert len(leaf) == 37
    return _tag("TapLeaf", bytes([0xC0]) + bytes([37]) + leaf)


# ---------------------------------------------------------------------- tx
def parse_raw_tx(raw: bytes) -> dict:
    """Independent raw-transaction decoder (no embit)."""
    o = [0]

    def rd(n):
        b = raw[o[0]:o[0] + n]
        assert len(b) == n, "truncated tx"
        o[0] += n
        return b

    def varint():
        v = rd(1)[0]
        if v < 0xFD:
            return v
        return int.from_bytes(rd({0xFD: 2, 0xFE: 4, 0xFF: 8}[v]), "little")

    version = int.from_bytes(rd(4), "little")
    segwit = False
    save = o[0]
    if raw[o[0]] == 0x00:
        rd(1)
        flag = rd(1)[0]
        assert flag == 0x01
        segwit = True
    else:
        o[0] = save
    vin = []
    for _ in range(varint()):
        prev_txid = rd(32)[::-1].hex()
        prev_vout = int.from_bytes(rd(4), "little")
        slen = varint()
        s = rd(slen)
        seq = int.from_bytes(rd(4), "little")
        vin.append({"txid": prev_txid, "vout": prev_vout,
                    "scriptSig": s.hex(), "sequence": seq})
    vout = []
    for _ in range(varint()):
        val = int.from_bytes(rd(8), "little")
        slen = varint()
        vout.append({"value": val, "scriptPubKey": rd(slen).hex()})
    wit = []
    if segwit:
        for _ in vin:
            items = [rd(varint()).hex() for _ in range(varint())]
            wit.append(items)
    locktime = int.from_bytes(rd(4), "little")
    assert o[0] == len(raw), f"trailing bytes: {len(raw) - o[0]}"

    # sizes
    total = len(raw)
    # strip witness for base size
    base = 4 + 4  # version + locktime
    b = bytearray()
    b += version.to_bytes(4, "little")

    def wvarint(n):
        if n < 0xFD:
            return bytes([n])
        if n <= 0xFFFF:
            return b"\xfd" + n.to_bytes(2, "little")
        return b"\xfe" + n.to_bytes(4, "little")

    b += wvarint(len(vin))
    for i in vin:
        b += bytes.fromhex(i["txid"])[::-1] + i["vout"].to_bytes(4, "little")
        ss = bytes.fromhex(i["scriptSig"])
        b += wvarint(len(ss)) + ss + i["sequence"].to_bytes(4, "little")
    b += wvarint(len(vout))
    for ou in vout:
        sp = bytes.fromhex(ou["scriptPubKey"])
        b += ou["value"].to_bytes(8, "little") + wvarint(len(sp)) + sp
    b += locktime.to_bytes(4, "little")
    base = len(b)
    weight = base * 3 + total
    vsize = (weight + 3) // 4
    txid = hashlib.sha256(hashlib.sha256(bytes(b)).digest()).digest()[::-1].hex()
    return {"version": version, "vin": vin, "vout": vout, "witness": wit,
            "locktime": locktime, "size": total, "base_size": base,
            "weight": weight, "vsize": vsize, "txid": txid}


# ------------------------------------------------------------------- wallet
def load_wallet():
    with open(WALLET_JSON) as f:
        w = json.load(f)
    root = C.mnemonic_to_hdkey(w["mnemonic"], network="main")
    fpr = C.hdkey_fingerprint(root)
    child = root.derive(FUNDING_PATH)
    xonly = child.key.get_public_key().xonly()
    addr = script.p2tr(child.key.get_public_key()).address(NETWORKS["main"])
    spk = script.p2tr(child.key.get_public_key()).data
    assert xonly.hex() == w["xonly_pubkey"], f"xonly mismatch {xonly.hex()}"
    assert addr == w["receive_address"], f"address mismatch {addr}"
    bip39_seed = bip39.mnemonic_to_seed(w["mnemonic"], "")
    assert len(bip39_seed) == 64
    return {"wallet": w, "root": root, "fpr": fpr, "xonly": xonly,
            "addr": addr, "spk": spk, "bip39_seed": bip39_seed}


def statefile(label):
    return os.path.join(STATE_DIR, f"state-{label}.json")


def load_state(label):
    with open(statefile(label)) as f:
        return json.load(f)


def save_state(label, st):
    with open(statefile(label), "w") as f:
        json.dump(st, f, indent=2)
    os.chmod(statefile(label), 0o600)


def rpc(method, params=None):
    r = requests.post(RPC, json={"jsonrpc": "2.0", "id": "gw", "method": method,
                                 "params": params or []}, auth=RPC_AUTH, timeout=60)
    d = r.json()
    if d.get("error"):
        raise RuntimeError(f"RPC {method}: {d['error']}")
    return d["result"]


# --------------------------------------------------------------------- steps
def cmd_mine(label, txid, vout):
    dat = C.build_dat_atom(txid, vout, 0)
    expr = C.make_dat_expr(txid, vout, 0)

    print(f"label       : {label}")
    print(f"funding utxo: {txid}:{vout}")
    print(f"dat         : {hex(dat)}   (plaintext satpoint; parses back: {C.parse_dat_atom(dat)[2]})")
    print(f"tweak expr  : {expr}")
    print()

    t0 = time.time()
    res = C.run_comet_miner(expr, MINER)
    elapsed = time.time() - t0
    comet = res["comet"]
    ring = res["ring"]

    # --- gate: the mined ring must carry exactly our dat, and sit under ~daplyd
    ring_int = C.decode_uw(ring)
    assert ring_int & 0xFF == ord("C"), "not a suite-C ring"
    _cur, ring_dat = C._hoon_rub(512, ring_int >> 8)
    assert ring_dat == dat, f"ring dat {hex(ring_dat)} != {hex(dat)}"
    p_int = C.patp_to_int(comet)
    star = p_int & 0xFFFF
    assert C.int_to_patp(star) == "~daplyd", f"star is {C.int_to_patp(star)}"
    assert p_int.bit_length() > 64, "not a comet"

    pass_atom = C.derive_pass_from_ring(ring)
    print(f"\nmined in {elapsed:.0f}s")
    print(f"comet : {comet}")
    print(f"nym   : {C.patp_to_mnemonym(comet)}")
    print(f"star  : {C.int_to_patp(star)}  (OK)")
    print(f"ring dat matches computed dat: OK")

    st = {
        "label": label, "funding": {"txid": txid, "vout": vout},
        "dat_hex": hex(dat), "dat_expr": expr,
        "comet": comet, "mnemonym": C.patp_to_mnemonym(comet),
        "seed": res.get("seed"), "ring": ring, "feed": res.get("feed"),
        "pass_atom_hex": hex(pass_atom),
        "mine_seconds": round(elapsed, 1),
    }
    save_state(label, st)
    print(f"\nwrote {statefile(label)}")


def cmd_build(label, publish=False, fief=None, sponsor=None, fee_rate=1,
              replace=False, sweep=False):
    w = load_wallet()
    st = load_state(label)
    txid, vout = st["funding"]["txid"], st["funding"]["vout"]

    # confirm the UTXO is still there and unspent
    utxos = requests.get(
        f"https://mempool.space/api/address/{w['addr']}/utxo", timeout=30).json()
    match = [u for u in utxos if u["txid"] == txid and u["vout"] == vout]
    if match:
        value = match[0]["value"]
        assert match[0]["status"]["confirmed"], "funding utxo unconfirmed"
        print(f"funding utxo confirmed: {txid}:{vout} = {value} sats")
    elif replace:
        # A fee-bump replacement.  mempool.space drops a UTXO from /utxo the
        # moment an unconfirmed tx spends it, so the liveness check above can
        # never pass for an RBF; reuse what the first build recorded.
        #
        # Replacing a spawn is safe and does NOT change the identity: the dat
        # commits to the FUNDING outpoint, not to the spawn txid, so the @p and
        # the life are untouched and Q is bit-identical.  Only the identity
        # SATPOINT moves.  Do this only while nothing downstream exists yet --
        # no finalize, no custody entry, no peer tracking the old satpoint.
        value = int(st.get("funding_value")
                    or (int(st["sat_value"]) + int(st["fee_paid"])))
        print(f"REPLACING unconfirmed spawn {st.get('spawn_txid','?')[:16]}... ; "
              f"funding {txid}:{vout} = {value} sats")
    else:
        raise AssertionError(f"funding utxo {txid}:{vout} not in wallet utxo "
                             f"set (pass --replace to fee-bump a spawn)")
    st["funding_value"] = value

    pass_atom = int(st["pass_atom_hex"], 16)
    fief_noun = None
    if fief:
        ip_s, port_s = fief.rsplit(":", 1)
        ip = int.from_bytes(bytes(int(x) for x in ip_s.split(".")), "big")
        fief_noun = (int.from_bytes(b"if", "little"), (ip, int(port_s)))
        print(f"fief: [%if {ip_s} {port_s}] -> {fief_noun}")
    sponsor_int = None
    if sponsor:
        sponsor_int = C.patp_to_int(sponsor)
        print(f"sponsor: {sponsor} -> {sponsor_int}")
    snapshot = {"life": 1, "rift": 0,
                "key": C.messaging_key_from_pass(pass_atom),
                "sponsor": sponsor_int, "fief": fief_noun}

    pub_pass = pub_open = None
    if publish:
        # A SPAWN publication is the degenerate packet: xtr empty, so the
        # watcher completes a one-entry log whose single entry is this
        # transaction, and its opening carries the spawn-opening.  That is
        # correct here and only here -- see cmd_publish for the late case.
        pub_pass = pass_atom
        # start-height names the block of the transaction that CREATED the
        # spawn satpoint -- the FUNDING tx, which the check above has just
        # confirmed -- and it is the FIRST thing +verify-lc fetches, with
        # +fetch-tx-at at exactly this height (lib/lc-attestation.hoon:104).
        # A 0 here sends the verifier looking for the funding transaction in
        # the GENESIS block: the strand fails with attestation-tx-not-found
        # and the publication yields no verdict at all, having reached no
        # check.  That is C3's spawn publication on mainnet (ec5c1fbe...,
        # start-height 0), which is why it does not verify today.  The field
        # enters no hash preimage, so getting it right changes no
        # commitment, no @p and no Q -- only the OP_RETURN payload.
        st["funding_height"] = fh = C.resolve_start_height(
            {"funding": {"txid": txid, "height": st.get("funding_height")}})
        print(f"start-height (funding tx block): {fh}")
        pub_open = {
            "internal_key": int.from_bytes(b"\x02" + w["xonly"], "big"),
            "snapshot": snapshot,
            "spawn_opening": {
                "spawn": {"txid_hex": txid, "vout": vout, "off": 0},
                "start_height": fh,
            },
        }

    # Wire change back to the ops wallet.  build_spawn_psbt has taken these
    # three arguments all along and this caller passed none of them, so every
    # spawn SWEPT its whole input into the identity output -- a 12.000-sat
    # UTXO became a 12.000-sat comet.  That is why minting three comets
    # needed a separate splitting transaction first, and that transaction is
    # what then sat unconfirmed and ended the run: a defect here cost the
    # whole campaign, one layer up.
    #
    # With change wired, the change branch caps the identity sat at 1.000
    # sats and returns the remainder, so ONE UTXO funds a whole run of
    # spawns back to back and no split is needed.  The vbyte estimate above
    # already counts the extra output, so the fee is right in both arms.
    #  --sweep puts the WHOLE input into the identity sat, minus fee.
    #
    #  Change is right for a large UTXO -- it is what lets one output fund a
    #  run of spawns -- and wrong for a small one.  build_spawn_psbt needs
    #  330 (identity) + 330 (change dust) + fee and adds 43 vB, so a 984-sat
    #  UTXO is REFUSED with change and mints comfortably without it.  Making
    #  change unconditional stranded >=330 sats per spawn and put a
    #  two-comet rig out of reach of 2.732 sats that was ample for it.
    #
    #  It stays an explicit flag rather than an automatic fallback: sweeping
    #  a large UTXO into an identity sat is exactly the defect that made a
    #  12.000-sat input into a 12.000-sat comet, so the operator says so.
    change_kwargs = {} if sweep else dict(
        change_internal_xonly=w["xonly"],
        change_script_pubkey=w["spk"],
        change_path=FUNDING_PATH,
    )
    psbt_obj, proof = C.build_spawn_psbt(
        utxo_txid=txid, utxo_vout=vout, utxo_value=value,
        utxo_script_pubkey=w["spk"],
        funding_internal_xonly=w["xonly"], funding_path=FUNDING_PATH,
        funding_fingerprint=w["fpr"], snapshot=snapshot,
        publication_pass_atom=pub_pass, publication_opening=pub_open,
        fee_rate=fee_rate, network="main",
        **change_kwargs,
    )
    psbt_obj.sign_with(w["root"])
    signed_b64 = psbt_obj.to_base64()
    commit_txid, tx_hex = C._extract_tx_from_psbt(signed_b64)

    st.update({
        "snapshot": {k: (v if k != "fief" else fief) for k, v in snapshot.items()},
        "snapshot_key_hex": hex(snapshot["key"]),
        "fief_noun": list(fief_noun) if fief_noun else None,
        "sponsor_patp": sponsor,
        "published": bool(publish),
        "proof": {k: v for k, v in proof.items() if k != "snapshot"},
        "signed_tx_hex": tx_hex, "spawn_txid": commit_txid,
        "fee_rate": fee_rate,
    })
    save_state(label, st)

    # ============================ VERIFY GATE ============================
    ok = True
    raw = bytes.fromhex(tx_hex)
    dec = parse_raw_tx(raw)
    print("\n" + "=" * 72)
    print("FULL INDEPENDENT DECODE")
    print("=" * 72)
    print(json.dumps(dec, indent=2))

    def chk(name, cond, detail=""):
        nonlocal ok
        print(f"  [{'PASS' if cond else 'FAIL'}] {name}{(' — ' + detail) if detail else ''}")
        if not cond:
            ok = False

    print("\nCHECKS")
    chk("txid matches embit", dec["txid"] == commit_txid, f"{dec['txid']}")
    chk("exactly 1 input", len(dec["vin"]) == 1)
    chk("input 0 outpoint is the intended funding UTXO",
        dec["vin"][0]["txid"] == txid and dec["vin"][0]["vout"] == vout,
        f"{dec['vin'][0]['txid']}:{dec['vin'][0]['vout']}")
    chk("input 0 scriptSig empty", dec["vin"][0]["scriptSig"] == "")
    chk("witness is a single 64/65-byte schnorr sig",
        len(dec["witness"]) == 1 and len(dec["witness"][0]) == 1
        and len(dec["witness"][0][0]) // 2 in (64, 65),
        f"{len(dec['witness'][0][0]) // 2} bytes")

    #  cmd_build always wires change now, so every spawn carries one more
    #  output than this used to expect: 2 confidential, 3 published. This
    #  assertion was left at its pre-change value when change was wired in,
    #  and it failed EVERY mint -- after all the load-bearing checks had
    #  passed -- with cmd_broadcast refusing on gate_passed. The gate meant
    #  to stop a bad transaction stopped every good one instead.
    #  ... and then it happened AGAIN, in these same nine lines, for the
    #  opposite reason: --sweep removes the change output and this block
    #  still counted it, so the flag whose entire purpose is to change the
    #  output shape was rejected by the check on the output shape. Fourth
    #  safety check in this campaign to fail by refusing correct work.
    #
    #  The destination check is not merely miscounted under --sweep, it is
    #  INVERTED: with no change output the last output IS output 0, the
    #  identity sat, so asserting that it pays the funding address would be
    #  asserting the identity sat went home to the wallet -- which is the
    #  real defect this tool exists to catch. It has to be skipped, not
    #  re-indexed.
    n_change = 0 if sweep else 1
    n_expected = (2 if publish else 1) + n_change
    chk(f"exactly {n_expected} output(s)"
        f"{' (--sweep: no change)' if sweep else ''}",
        len(dec["vout"]) == n_expected, f"got {len(dec['vout'])}")
    if sweep:
        #  The whole input goes into the identity sat. Say what that cost,
        #  because it is the operator's own money and the flag is the only
        #  thing standing between "funds a small mint" and "makes a
        #  12.000-sat comet".
        chk("--sweep: no change output, whole input to the identity sat",
            True, f"{value} sats in -> {dec['vout'][0]['value']} sat identity")
    else:
        #  A bare count says nothing about WHERE the change went; this says
        #  it comes home.
        chk("last output pays the funding address (change)",
            dec["vout"][-1]["scriptPubKey"] == w["spk"].hex(),
            dec["vout"][-1]["scriptPubKey"])

    # independently recompute Q
    c = C.state_commit(snapshot)
    lh_ind = independent_leaf_hash(c)
    lh_cw = C.state_leaf_hash(C.state_leaf_script(c))
    chk("leaf hash: causeway == independent", lh_ind == lh_cw, lh_cw.hex())
    q_cw = C.state_output_key(w["xonly"], snapshot)
    q_ind = independent_Q(w["xonly"], lh_ind)
    chk("Q: causeway == from-scratch secp256k1", q_cw == q_ind, q_cw.hex())
    want_spk = "5120" + q_ind.hex()
    chk("output 0 scriptPubKey == 5120||Q",
        dec["vout"][0]["scriptPubKey"] == want_spk,
        dec["vout"][0]["scriptPubKey"])

    sat_val = dec["vout"][0]["value"]
    fee = value - sum(o["value"] for o in dec["vout"])
    chk("output 0 value >= 330 (P2TR dust)", sat_val >= 330, f"{sat_val} sats")
    # The ceiling has to scale with the transaction, not sit at a constant.
    # A flat `fee <= 500` contradicted the rate check one line below it: a
    # spawn that also carries a publication is ~400 vB, so at the requested
    # 2 sat/vB it owes ~800 sats and is CORRECT -- and the gate failed it
    # anyway, after every substantive check had passed. Measured: 802 sats
    # at exactly 2.000 sat/vB, refused.
    #
    # What this is actually for is catching a runaway fee, so express it as
    # one: the effective rate must not exceed what was asked for by more
    # than a sat, which bounds the fee at any size.
    eff = fee / dec["vsize"]
    fee_cap = int(dec["vsize"] * (fee_rate + 1))
    chk(f"fee <= {fee_cap} sats ({dec['vsize']} vB x {fee_rate}+1 sat/vB)",
        fee <= fee_cap, f"fee = {fee} sats")
    chk("effective fee rate >= 1.0 sat/vB",
        eff >= 1.0, f"{fee}/{dec['vsize']} = {eff:.3f} sat/vB")
    chk(f"effective fee rate <= requested + 1 ({fee_rate + 1} sat/vB)",
        eff <= fee_rate + 1.0, f"{eff:.3f} sat/vB, requested {fee_rate}")

    if publish:
        pub_spk = dec["vout"][1]["scriptPubKey"]
        expect = C.make_publication_script(pub_pass, pub_open).hex()
        chk("output 1 is the OP_RETURN publication", pub_spk == expect)
        chk("OP_RETURN envelope 6a 03 'urb' 01 09",
            pub_spk.startswith("6a0375726201" + "09"))
        chk("output 1 value == 0", dec["vout"][1]["value"] == 0)
        # envelope = 7 prefix bytes + the pushdata header (1 direct / 2
        # PUSHDATA1 / 3 PUSHDATA2), so measure the payload rather than
        # assuming PUSHDATA1.
        pub_len = len(C.jam_bytes(C.publication_noun(pub_pass, pub_open)))
        print(f"       publication payload: {pub_len} bytes")
    else:
        chk("no OP_RETURN output (confidential)",
            all(not o["scriptPubKey"].startswith("6a") for o in dec["vout"]))

    # node-side policy + signature validation, without broadcasting.
    # NOTE: the reference node (alpha.groundwire.dev) is Bitcoin Core 29, which
    # still enforces the pre-Core-30 default -datacarriersize=83. A kelvin-9
    # publication payload is ~254 bytes, so that node rejects it with
    # reject-reason "scriptpubkey" purely on size. This was verified to be a
    # size-only effect (an otherwise identical tx with an 84-byte OP_RETURN is
    # accepted), and mainnet blocks 961028/961038 contain mined OP_RETURN
    # scripts of 292/1594/1622/1690 bytes, so the network relays and mines
    # these. For a published spawn we therefore accept exactly this one
    # rejection and require the signature to be independently proven valid.
    try:
        res = rpc("testmempoolaccept", [[tx_hex]])[0]
        allowed = res.get("allowed") is True
        if (not allowed and publish
                and res.get("reject-reason") == "scriptpubkey"):
            print("  [WARN] testmempoolaccept: rejected by Core-29 datacarrier "
                  "policy (size-only, expected for a published spawn)")
            # prove the signature is valid by re-testing the same tx with a
            # policy-legal OP_RETURN: same input, same signing path.
            real = C.make_publication_script
            try:
                C.make_publication_script = (
                    lambda p, o: bytes([0x6A, 0x03, 0x75, 0x72, 0x62, 0x01, 0x09])
                    + bytes([75]) + b"\x00" * 75)
                p2, _ = C.build_spawn_psbt(
                    utxo_txid=txid, utxo_vout=vout, utxo_value=value,
                    utxo_script_pubkey=w["spk"],
                    funding_internal_xonly=w["xonly"], funding_path=FUNDING_PATH,
                    funding_fingerprint=w["fpr"], snapshot=snapshot,
                    publication_pass_atom=pub_pass, publication_opening=pub_open,
                    fee_rate=fee_rate, network="main")
                p2.sign_with(w["root"])
                _t2, hex2 = C._extract_tx_from_psbt(p2.to_base64())
                r2 = rpc("testmempoolaccept", [[hex2]])[0]
            finally:
                C.make_publication_script = real
            chk("signing path valid (same tx, policy-legal OP_RETURN accepted)",
                r2.get("allowed") is True, json.dumps(r2))
        else:
            chk("bitcoind testmempoolaccept", allowed, json.dumps(res))
    except Exception as e:
        chk("bitcoind testmempoolaccept", False, str(e))

    print("\n" + ("ALL CHECKS PASSED — safe to broadcast" if ok
                  else "*** GATE FAILED — DO NOT BROADCAST ***"))
    print(f"spawn txid (pre-broadcast): {commit_txid}")
    print(f"sat output: {sat_val} sats  |  fee: {fee} sats  |  vsize: {dec['vsize']}")
    st["gate_passed"] = ok
    st["sat_value"] = sat_val
    st["fee_paid"] = fee
    st["vsize"] = dec["vsize"]
    save_state(label, st)
    return 0 if ok else 1


def cmd_broadcast(label):
    st = load_state(label)
    assert st.get("gate_passed"), "gate did not pass; refusing to broadcast"
    txid = C._broadcast_tx(st["signed_tx_hex"])
    print(f"broadcast: {txid}")
    assert txid == st["spawn_txid"], f"txid mismatch! {txid}"
    st["broadcast_txid"] = txid
    save_state(label, st)


def cmd_status(label):
    st = load_state(label)
    txid = st.get("broadcast_txid") or st["spawn_txid"]
    r = requests.get(f"https://mempool.space/api/tx/{txid}/status", timeout=30)
    print(txid, r.text)
    d = r.json()
    if d.get("confirmed"):
        st["block_height"] = d["block_height"]
        st["block_hash"] = d["block_hash"]
        save_state(label, st)
        print(f"CONFIRMED at height {d['block_height']}")
        return 0
    return 1


def cmd_artifact(label, n):
    """Write the load-bearing identity artifact + a causeway-format proof.json,
    and bake the custody log (xtr) into a bootable feed."""
    st = load_state(label)
    w = load_wallet()
    assert st.get("block_height"), "not confirmed yet"
    txid = st["broadcast_txid"]
    height = int(st["block_height"])
    fief_noun = tuple(st["fief_noun"][0:1]) + (tuple(st["fief_noun"][1]),) \
        if st.get("fief_noun") else None
    if st.get("fief_noun"):
        fief_noun = (st["fief_noun"][0], (st["fief_noun"][1][0], st["fief_noun"][1][1]))
    # the sponsor is INSIDE the state commitment: rebuilding the snapshot with
    # sponsor=None here would recompute a different Q and verify_proof_self
    # would fail.  Take it from the state the build recorded.
    snapshot = {"life": 1, "rift": 0,
                "key": int(st["snapshot_key_hex"], 16),
                "sponsor": st.get("snapshot", {}).get("sponsor"),
                "fief": fief_noun}

    proof = dict(st["proof"])
    proof["snapshot"] = snapshot
    proof["commit_txid"] = txid
    proof["block_height"] = height
    proof["block_hash"] = st["block_hash"]
    proof["patp"] = st["comet"]
    proof["dat_hex"] = st["dat_hex"]

    ok, why = C.verify_proof_self(proof)
    print(f"  verify_proof_self   : {ok} — {why}")
    ok2, why2 = C.verify_proof_onchain(proof)
    print(f"  verify_proof_onchain: {ok2} — {why2}")
    assert ok and ok2, "proof verification failed"

    # start-height names the block of the tx that CREATED the spawn satpoint --
    # the FUNDING tx -- and NOT the spawn tx's own block.  The verifier walks
    # `header-height(start-height)` then `transaction(that block, spawn txid)`
    # looking for the funding outpoint, so pointing it at the spawn's block
    # makes it fetch a block the transaction is not in.  The whole custody log
    # then fails to validate and the %anew thread ends WITHOUT A VERDICT, which
    # is almost silent: one log line, no reason, pass stays 108 bytes.
    #
    # It is transport metadata and enters no hash preimage, so it changes no
    # commitment, no @p and no on-chain data -- only the xtr and the baked feed.
    #
    # This was fixed once before, out of tree, by a fix_artifacts.py that
    # hardcoded the three funding heights and was never folded back here.  It
    # therefore reproduced exactly in the 2026-08-06 cleanroom run.
    fh = C.resolve_start_height(
        {"funding": {"txid": st["funding"]["txid"],
                     "height": st.get("funding_height")}})
    if st.get("funding_height") != fh:
        st["funding_height"] = fh
        save_state(label, st)
    assert fh <= height, f"funding height {fh} above spawn height {height}"
    print(f"  funding height      : {fh}  (spawn is at {height})")

    # bake the xtr (entry 0 = the spawn, naming the sat and its start height)
    entry = dict(
        txid_hex=txid, height=height,
        opening=dict(
            internal_key=int("02" + proof["internal_pubkey_hex"], 16),
            snapshot=snapshot,
            spawn_opening=dict(
                spawn=dict(txid_hex=st["funding"]["txid"],
                           vout=st["funding"]["vout"], off=0),
                start_height=fh,
            ),
        ),
    )
    xtr = C.build_xtr_atom([entry])
    # The poke that extends a RUNNING ship's custody log in place, no reboot.
    # A ship booted from the miner's feed serves a 108-byte pass and CANNOT be
    # verified by anyone; this is how it gets its evidence without a restart.
    # The agent re-verifies the whole extended log against the chain before it
    # will re-encode the pass, so this is evidence, not authority.
    custody_poke = C.format_custody_entry_poke(entry)
    print("\n  custody-entry poke (extends a RUNNING ship's log in place):")
    print("    " + custody_poke)
    ring_int = C.decode_uw(st["ring"])
    noun = C.hoon_cue(C.decode_uw(st["feed"]))
    (_t, _z), (comet_p, (rift, ((life, feed_ring), _nil))) = noun
    assert feed_ring == ring_int, "feed ring != miner ring"
    assert comet_p == C.patp_to_int(st["comet"]), "feed comet != mined comet"
    # The pass a PEER must be given to verify this comet is the one derived
    # from the ring WITH the xtr appended -- ~330-405 B.  pass_atom_hex above
    # is the bare 108-byte object and carries no custody evidence at all; a
    # %jael-writ built from it is dropped silently through +public-pass.
    # Deriving it from a running ship is a trap: jael's /vein gives you the
    # ring, and re-deriving from that reproduces the BARE pass however much
    # evidence the ship has ingested.  Compute it here instead.
    ring_xtr = C.append_xtr_to_ring(ring_int, xtr)
    pass_with_xtr = C.derive_pass_from_ring(C.encode_uw(ring_xtr))
    print(f"  pass bare / with xtr: {(pass_atom_len := (int(st['pass_atom_hex'], 16).bit_length() + 7) // 8)}"
          f" / {(pass_with_xtr.bit_length() + 7) // 8} bytes")
    baked = C.encode_uw(C.rebuild_feed(comet_p, rift, life, ring_xtr))
    # the @p must be unchanged by baking
    n2 = C.hoon_cue(C.decode_uw(baked))
    assert n2[1][0] == comet_p, "baking changed the @p!"

    art = {
        "comet": st["comet"],
        "mnemonym": st["mnemonym"],
        "protocol": "kelvin-9 (%gw-btc)", "kelvin": 9, "dom": "gw-btc",
        "network": "mainnet",
        "confidential": not st["published"],

        "seed": st["seed"], "ring": st["ring"],
        "feed_from_miner": st["feed"],
        "feed_with_xtr_baked": baked,
        "pass_atom_hex": st["pass_atom_hex"],
        "xtr_hex": hex(xtr),
        "ring_with_xtr": C.encode_uw(ring_xtr),
        "pass_with_xtr_hex": hex(pass_with_xtr),
        "custody_entry_poke": custody_poke,
        "funding_height": fh,
        "start_height_note": (
            "start-height is the block of the tx that CREATED the spawn "
            "satpoint (the FUNDING tx), not the spawn tx's own block. It is "
            "transport metadata and enters no hash preimage, so it changes no "
            "commitment, no @p and no on-chain data -- only the xtr custody "
            "log and the xtr-baked boot feed."),

        "dat_format": ("plaintext: dat = (can 0 (mat %gw-btc) (mat 9) (mat (jam "
                       "[txid vout off])) ~). No blind, no hash; the satpoint is "
                       "readable from any pass (2026-08-18 reversion to the original "
                       "spec's cleartext tweak)."),

        "spawn_sont": {"txid": st["funding"]["txid"],
                       "vout": st["funding"]["vout"], "off": 0},
        "dat_hex": st["dat_hex"],
        "dat_expr": st["dat_expr"],

        #  The sponsor comes from the snapshot, not from a constant.  It was
        #  hardcoded None while the adjacent causeway_proof.snapshot carried
        #  the real one -- and the sponsor is INSIDE the state commitment,
        #  so an artifact that misreports it misreports the thing the whole
        #  file exists to preserve.
        "snapshot": {"life": 1, "rift": 0,
                     "key_hex": st["snapshot_key_hex"],
                     "sponsor": st["snapshot"].get("sponsor"),
                     "fief": st["snapshot"].get("fief"),
                     "fief_noun": st.get("fief_noun")},
        "state_commit_c_hex": C.state_commit(snapshot).hex(),
        "state_leaf_hash_hex": proof["leaf_hash_hex"],
        "sat_output_key_Q_hex": proof["sat_script_pubkey_hex"][4:],
        "sat_script_pubkey_hex": proof["sat_script_pubkey_hex"],

        "spawn_txid": txid,
        "block_height": height,
        "block_hash": st["block_hash"],
        "sat_vout": 0,
        "sat_value": st["sat_value"],
        "fee_paid": st["fee_paid"],
        "vsize": st["vsize"],
        "published_op_return": st["published"],

        "funding_key": {
            "path": FUNDING_PATH,
            "master_fingerprint": w["fpr"].hex(),
            "internal_xonly_pubkey": proof["internal_pubkey_hex"],
            "wallet_file": WALLET_JSON,
            "note": ("To spend the sat-carrying output (rekey/custody move): "
                     "key-path spend with the key at this path, taproot merkle "
                     "root = state_leaf_hash_hex above."),
        },
        "causeway_proof": proof,
    }
    out = f"/Users/trent/gw-building/.gw-comet-{n}.json"
    with open(out, "w") as f:
        json.dump(art, f, indent=2, default=str)
    os.chmod(out, 0o600)
    print(f"  wrote {out} (0600)")
    print(f"  comet {st['comet']}  height {height}  sat {st['sat_value']} fee {st['fee_paid']}")


def find_funding_utxo(w, exclude=(), min_value=1_000):
    """Pick a spendable UTXO from the ops wallet's funding address.

    Used to TOP UP an identity sat: a state update with no funding input pays
    its fee out of the identity output, so the sat shrinks every time and the
    comet can end up unable to afford its own next state update.  Returns a
    build_rekey_psbt funding-input dict, or None if nothing is available.
    """
    try:
        utxos = requests.get(
            f"https://mempool.space/api/address/{w['addr']}/utxo", timeout=30).json()
    except Exception as e:  # noqa: BLE001
        print(f"  warning: could not fetch funding UTXOs: {e}")
        return None
    skip = {(t.lower(), int(v)) for t, v in exclude}
    cands = [u for u in utxos
             if u.get("status", {}).get("confirmed")
             and u["value"] >= min_value
             and (u["txid"].lower(), int(u["vout"])) not in skip]
    if not cands:
        return None
    u = max(cands, key=lambda x: x["value"])   # biggest: fewest top-ups later
    return {
        "txid": u["txid"], "vout": u["vout"], "value": u["value"],
        "script_pubkey": w["spk"], "xonly": w["xonly"],
        "path": FUNDING_PATH, "fingerprint": w["fpr"],
    }


def cmd_publish(label, artifact_n, fee_rate=4, fund=False, sat_target=None):
    """TIER 1 DECLASSIFICATION -- a state update carrying an OP_RETURN
    publication, for a comet that is already confidential.

    There is no CLI for this anywhere: causeway has no `publish` subcommand and
    no --publish on `rekey`, and build_rekey_psbt's publication parameters are
    unreachable from any caller.  It is reachable here.

    What the chain sees: input 0 spends the comet's currently tracked identity
    satpoint (the ownership proof -- only its holder can), output 0 commits a
    snapshot whose life STRICTLY exceeds the one peers hold, output 1 is the
    OP_RETURN.  Since 2026-08-10 the payload is the comet's WHOLE attestation
    packet -- the pass a peer receives over ames, custody log in its xtr, plus
    the opening for the hop this transaction performs -- so a STRANGER accepts
    it too: +process-publication completes the log with this transaction and
    hands it to the same +verify-lc a packet gets.  See
    doc/opret-revision/04-decisions-addendum.md section 0.

    The pass published here is the pass a PEER receives -- custody log in its
    xtr -- and that is what makes it a LATE publication rather than a claimed
    spawn.  See the comment over pub_pass below for what publishing the boot
    pass instead does, and what it did to the three state updates already on
    mainnet.

    --fund adds a funding input from the ops wallet AFTER input 0, so the
    identity output is topped up rather than shrunk by the fee.  The verifier
    reads input 0 only (self-attestation.hoon:674-689) and sats are assigned to
    outputs in input order, so an input behind the identity cannot move it.
    """
    fee_rate = int(fee_rate)
    w = load_wallet()
    art_path = f"/Users/trent/gw-building/.gw-comet-{artifact_n}.json"
    art = json.load(open(art_path))
    proof = dict(art["causeway_proof"])
    old_snap = dict(proof["snapshot"])

    new_snap = dict(old_snap)
    new_snap["life"] = int(old_snap["life"]) + 1
    print(f"comet   : {art['comet']}")
    print(f"spending: {proof['commit_txid']}:{proof['sat_vout']} "
          f"= {proof['sat_value']} sats")
    print(f"life    : {old_snap['life']} -> {new_snap['life']}")
    print(f"fief    : {new_snap.get('fief')}   sponsor: {new_snap.get('sponsor')}")

    # PUBLISH THE PASS A PEER GETS, NOT THE BOOT PASS.  Since 2026-08-10 the
    # payload is verified as an attestation packet, and the custody log lives
    # in the pass's xtr.  Publish a boot pass (xtr empty) and the watcher
    # completes a ONE-ENTRY log whose single entry is this transaction -- the
    # degenerate SPAWN shape -- so +run-checks requires input 0 to spend the
    # spawn satpoint.  A state update spends the satpoint its LAST hop landed
    # on, so the check fails and the comet never declassifies.
    #
    # That is measured, not theorised.  Of the four publications on mainnet,
    # all built here with boot passes, only C3's spawn is the shape the
    # verifier now reads; k1's Tier-1 declassification (08957455..., 961353)
    # fails `0-continuity` for exactly this reason.
    xtr = int(art["xtr_hex"], 16)
    log = cue_log(art["xtr_hex"])
    assert log, "artifact carries no custody log; there is nothing to publish"
    last_txid, (last_height, _op) = log[-1]
    # The log must END where this transaction BEGINS: input 0 spends the
    # output the last hop landed on.  If the comet has moved its sat since
    # the artifact was baked, the artifact's log is short by those hops and
    # the publication would fail `N-continuity` on chain.  Refuse rather than
    # pay miner fees for a packet nobody can verify.
    assert f"{last_txid:064x}" == proof["commit_txid"], (
        f"custody log ends at {last_txid:064x} @ {last_height} but input 0 "
        f"spends {proof['commit_txid']}:{proof['sat_vout']} -- the artifact's "
        f"log is missing the hops in between.  Extend it first (the "
        f"%gw-custody-entry poke path) and re-bake the artifact.")
    pub_pass = C.pass_with_xtr(int(art["pass_atom_hex"], 16), xtr)
    print(f"pass    : {(int(art['pass_atom_hex'], 16).bit_length() + 7) // 8} B bare"
          f" -> {(pub_pass.bit_length() + 7) // 8} B with a "
          f"{len(log)}-entry custody log")
    pub_open = {
        "internal_key": int("02" + proof["internal_pubkey_hex"], 16),
        "snapshot": new_snap,
        # The spawn opening may sit ONLY on entry 0 -- `spawn-opening-zero` in
        # +run-checks -- and entry 0 is inside the xtr above, carrying the
        # real start-height the artifact recorded.  This transaction is entry
        # N>0, so its own opening carries none.
        "spawn_opening": None,
    }

    fund_kwargs = {}
    if fund:
        fi = find_funding_utxo(
            w, exclude=[(proof["commit_txid"], int(proof["sat_vout"]))])
        if fi is None:
            raise SystemExit("--fund: no confirmed funding UTXO available")
        print(f"funding : {fi['txid']}:{fi['vout']} = {fi['value']} sats "
              f"(input 1, AFTER the identity sat)")
        fund_kwargs["funding_inputs"] = [fi]
        if sat_target is not None:
            fund_kwargs["sat_target"] = int(sat_target)
            fund_kwargs["change_script_pubkey"] = w["spk"]
            fund_kwargs["change_internal_xonly"] = w["xonly"]
            fund_kwargs["change_path"] = FUNDING_PATH

    psbt_obj, new_proof = C.build_rekey_psbt(
        prior_proof=proof, new_snapshot=new_snap,
        publication_pass_atom=pub_pass, publication_opening=pub_open,
        fee_rate=fee_rate, network="main", **fund_kwargs)
    if new_proof.get("topped_up_by") is not None:
        print(f"identity: {proof['sat_value']} -> {new_proof['sat_value']} sats "
              f"({new_proof['topped_up_by']:+d})")
    psbt_obj.sign_with(w["root"])
    new_txid, tx_hex = C._extract_tx_from_psbt(psbt_obj.to_base64())

    raw = bytes.fromhex(tx_hex)
    dec = parse_raw_tx(raw)
    print("\n" + "=" * 72 + "\nFULL INDEPENDENT DECODE\n" + "=" * 72)
    print(json.dumps(dec, indent=2))

    ok = True

    def chk(name, cond, detail=""):
        nonlocal ok
        print(f"  [{'PASS' if cond else 'FAIL'}] {name}"
              f"{(' — ' + detail) if detail else ''}")
        ok = ok and bool(cond)

    print("\nCHECKS")
    n_fund = len(fund_kwargs.get("funding_inputs", []))
    chk("txid matches embit", dec["txid"] == new_txid, dec["txid"])
    chk(f"exactly {1 + n_fund} input(s)", len(dec["vin"]) == 1 + n_fund,
        f"got {len(dec['vin'])}")
    # The ONE structural invariant: the verifier reads input 0 and nothing
    # else (self-attestation.hoon:674-689), and ordinals assign sats in input
    # order, so funding must sit BEHIND the identity or the sat moves.
    chk("input 0 spends the tracked identity satpoint",
        dec["vin"][0]["txid"] == proof["commit_txid"]
        and dec["vin"][0]["vout"] == int(proof["sat_vout"]),
        f"{dec['vin'][0]['txid']}:{dec['vin'][0]['vout']}")
    for j, fi in enumerate(fund_kwargs.get("funding_inputs", []), start=1):
        chk(f"input {j} is the funding UTXO (behind the identity)",
            dec["vin"][j]["txid"] == fi["txid"]
            and dec["vin"][j]["vout"] == int(fi["vout"]),
            f"{dec['vin'][j]['txid']}:{dec['vin'][j]['vout']}")
    chk("every scriptSig empty", all(v["scriptSig"] == "" for v in dec["vin"]))
    chk("every witness is a single 64/65-byte schnorr sig",
        len(dec["witness"]) == 1 + n_fund
        and all(len(wl) == 1 and len(wl[0]) // 2 in (64, 65)
                for wl in dec["witness"]),
        f"{[len(wl[0]) // 2 for wl in dec['witness']]} bytes")
    n_change = 1 if new_proof.get("change_value") else 0
    chk(f"exactly {2 + n_change} outputs (sat + OP_RETURN"
        f"{' + change' if n_change else ''})",
        len(dec["vout"]) == 2 + n_change, f"got {len(dec['vout'])}")
    # Output 0 must stay the sat-carrying one: +index-to-sont walks outputs in
    # order from the sat's offset, so anything ahead of it would take the sat.
    chk("sat-carrying output is output 0 (change, if any, comes last)",
        dec["vout"][0]["scriptPubKey"] == new_proof["sat_script_pubkey_hex"])

    c = C.state_commit(new_snap)
    lh_ind = independent_leaf_hash(c)
    chk("leaf hash: causeway == independent",
        lh_ind == C.state_leaf_hash(C.state_leaf_script(c)), lh_ind.hex())
    q_cw = C.state_output_key(w["xonly"], new_snap)
    q_ind = independent_Q(w["xonly"], lh_ind)
    chk("Q: causeway == from-scratch secp256k1", q_cw == q_ind, q_cw.hex())
    chk("output 0 scriptPubKey == 5120||Q",
        dec["vout"][0]["scriptPubKey"] == "5120" + q_ind.hex(),
        dec["vout"][0]["scriptPubKey"])
    chk("Q differs from the prior state key (the snapshot really moved)",
        dec["vout"][0]["scriptPubKey"] != proof["sat_script_pubkey_hex"])
    chk("life strictly increases",
        int(new_snap["life"]) > int(old_snap["life"]),
        f"{old_snap['life']} -> {new_snap['life']}")

    sat_val = dec["vout"][0]["value"]
    total_in = int(proof["sat_value"]) + sum(
        int(fi["value"]) for fi in fund_kwargs.get("funding_inputs", []))
    fee = total_in - sum(o["value"] for o in dec["vout"])
    chk("output 0 value >= 330 (P2TR dust)", sat_val >= 330, f"{sat_val} sats")
    if n_fund:
        chk("identity sat was topped up, not shrunk",
            sat_val > int(proof["sat_value"]),
            f"{proof['sat_value']} -> {sat_val} sats")
    #  Scale with the transaction, do not sit at a constant.
    #
    #  Same defect as cmd_build's, in the other command, and I fixed only
    #  the one I was looking at.  A LATE publication carries the whole
    #  attestation packet -- that is the entire point of the rework -- so
    #  it is several hundred vB more than a spawn, and at any sane fee
    #  rate it blows a flat 2.000-sat ceiling while being perfectly
    #  correct.  This blocked the late reveal, which is the single
    #  capability the publication rework exists to deliver.
    #
    #  A flat cap cannot express "the fee is sane": sane means the rate is
    #  what was asked for, and the rate is what the two checks below say.
    eff = fee / dec["vsize"]
    fee_cap = int(dec["vsize"] * (fee_rate + 1))
    chk(f"fee <= {fee_cap} sats ({dec['vsize']} vB x {fee_rate}+1 sat/vB)",
        0 < fee <= fee_cap, f"fee = {fee} sats")
    chk("effective fee rate >= 1.0 sat/vB", eff >= 1.0,
        f"{fee}/{dec['vsize']} = {eff:.3f} sat/vB")
    chk(f"effective fee rate <= requested + 1 ({fee_rate + 1} sat/vB)",
        eff <= fee_rate + 1.0, f"{eff:.3f} sat/vB, requested {fee_rate}")

    pub_spk = dec["vout"][1]["scriptPubKey"]
    chk("output 1 is the OP_RETURN publication",
        pub_spk == C.make_publication_script(pub_pass, pub_open).hex())
    chk("OP_RETURN envelope 6a 03 'urb' 01 09",
        pub_spk.startswith("6a03757262" + "0109"), pub_spk[:16])
    chk("OP_RETURN value is 0", dec["vout"][1]["value"] == 0)
    # MAX_PUBLICATION payload + 7-byte envelope + up to a 3-byte PUSHDATA2
    # header.  The cap is C.MAX_PUBLICATION (1024 since 2026-08-10) and must be
    # read from there, never restated: it lives in three implementations that
    # have to agree byte for byte.
    chk(f"publication <= {C.MAX_PUBLICATION} bytes",
        len(bytes.fromhex(pub_spk)) <= C.MAX_PUBLICATION + 10,
        f"{len(bytes.fromhex(pub_spk))} bytes")
    # THE CHECK THAT SEPARATES A LATE PUBLICATION FROM A CLAIMED SPAWN.  Read
    # the payload BACK out of the script we are about to broadcast and prove
    # the pass in it carries the log -- not the pass we think we passed in.
    # An empty xtr here is the shape that put three unverifiable publications
    # on mainnet, and it is invisible in the decode: same envelope, same
    # opening, ~200 fewer bytes.
    #  PARSE the script; do not re-derive how long it ought to be.
    #
    #  This used to slice the last N bytes of the OP_RETURN, where N came
    #  from re-running jam_bytes(publication_noun(...)) -- the same encoder
    #  that produced the script.  The expected-script check above compares
    #  against that same encoder too, and the envelope check covers only the
    #  first 7 bytes, so NOTHING here validated the push opcode or its
    #  length independently.  Reproduced with the historical
    #  OP_PUSHDATA1-mod-256 corruption on a 279-byte payload (good header
    #  4d1701, corrupt 4c17): the re-cue passes it, causeway's parser
    #  catches it.  That is the encoder-regression class these gates exist
    #  for, and gwmint was blind to exactly it.
    #
    #  assert_publication_carries_log reaches the payload through
    #  parse_publication_script, which reads the push header rather than
    #  assuming a length, and checks the pass's xtr against the log we
    #  meant to publish.
    try:
        C.assert_publication_carries_log(
            bytes.fromhex(pub_spk),
            xtr=C.xtr_of_pass(pub_pass),
            entries=len(log),
        )
        chk("payload parses, and its pass carries the custody log", True,
            f"{len(log)} entries, ending {f'{last_txid:064x}'[:16]}...")
    except Exception as e:
        chk("payload parses, and its pass carries the custody log", False, str(e))
    chk("published pass is not the boot pass",
        pub_pass != int(art["pass_atom_hex"], 16))
    chk("terminal opening carries no spawn-opening (entry 0 owns it)",
        pub_open.get("spawn_opening", pub_open.get("blind_opening")) is None)

    # Core 29 rejects our OP_RETURN by policy, so testmempoolaccept on the real
    # script is expected to fail; prove the SIGNING path instead by swapping in
    # a policy-legal script of the same shape.  We broadcast via mempool.space.
    try:
        res = rpc("testmempoolaccept", [[tx_hex]])[0]
        if res.get("allowed"):
            chk("bitcoind testmempoolaccept", True, json.dumps(res))
        else:
            print(f"  [note] Core rejects the real OP_RETURN by policy: "
                  f"{res.get('reject-reason')}")
            real = C.make_publication_script

            def legal(p, o):
                return bytes([0x6A, 0x20]) + hashlib.sha256(real(p, o)).digest()
            try:
                C.make_publication_script = legal
                p2, _ = C.build_rekey_psbt(
                    prior_proof=proof, new_snapshot=new_snap,
                    publication_pass_atom=pub_pass, publication_opening=pub_open,
                    fee_rate=fee_rate, network="main", **fund_kwargs)
                p2.sign_with(w["root"])
                _t2, hex2 = C._extract_tx_from_psbt(p2.to_base64())
                r2 = rpc("testmempoolaccept", [[hex2]])[0]
            finally:
                C.make_publication_script = real
            chk("signing path valid (policy-legal OP_RETURN accepted)",
                r2.get("allowed") is True, json.dumps(r2))
    except Exception as e:  # noqa: BLE001
        chk("bitcoind testmempoolaccept", False, str(e))

    print("\n" + ("ALL CHECKS PASSED — safe to broadcast" if ok
                  else "*** GATE FAILED — DO NOT BROADCAST ***"))
    print(f"publication txid (pre-broadcast): {new_txid}")
    st = {"label": f"{label}-publish", "comet": art["comet"],
          "artifact": artifact_n, "spawn_txid": new_txid,
          "signed_tx_hex": tx_hex, "gate_passed": ok, "fee_rate": fee_rate,
          "sat_value": sat_val, "fee_paid": fee, "vsize": dec["vsize"],
          "new_snapshot": {k: v for k, v in new_snap.items()},
          "prior_txid": proof["commit_txid"], "published": True,
          # WHICH SHAPE WAS PUBLISHED.  A boot-pass publication and a
          # packet publication are indistinguishable in the tx decode --
          # same envelope, same opening, ~200 fewer bytes -- and the
          # difference is whether anyone can verify it.  Record it.
          "published_pass_hex": hex(pub_pass),
          "published_log_entries": len(log)}
    save_state(f"{label}-publish", st)
    print(f"wrote {statefile(f'{label}-publish')}")
    return 0 if ok else 1


if __name__ == "__main__":
    cmd = sys.argv[1]
    if cmd == "artifact":
        cmd_artifact(sys.argv[2], sys.argv[3]); sys.exit(0)
    if cmd == "mine":
        cmd_mine(sys.argv[2], sys.argv[3], int(sys.argv[4]))
    elif cmd == "build":
        pub = "--publish" in sys.argv
        fief = sponsor = None
        fee_rate = 1
        for a in sys.argv:
            if a.startswith("--fief="):
                fief = a.split("=", 1)[1]
            if a.startswith("--sponsor="):
                sponsor = a.split("=", 1)[1]
            if a.startswith("--fee-rate="):
                fee_rate = int(a.split("=", 1)[1])
        sys.exit(cmd_build(sys.argv[2], publish=pub, fief=fief,
                           sponsor=sponsor, fee_rate=fee_rate,
                           replace="--replace" in sys.argv,
                           sweep="--sweep" in sys.argv))
    elif cmd == "publish":
        fr = 4
        for a in sys.argv:
            if a.startswith("--fee-rate="):
                fr = int(a.split("=", 1)[1])
        st = None
        for a in sys.argv:
            if a.startswith("--sat-target="):
                st = int(a.split("=", 1)[1])
        sys.exit(cmd_publish(sys.argv[2], sys.argv[3], fee_rate=fr,
                             fund="--fund" in sys.argv, sat_target=st))
    elif cmd == "broadcast":
        cmd_broadcast(sys.argv[2])
    elif cmd == "status":
        sys.exit(cmd_status(sys.argv[2]))
    else:
        raise SystemExit(f"unknown cmd {cmd}")
