"""Self-attestation packet delivery.

Causeway emits a JSON *skeleton* (on-chain-derivable data only — see
`causeway packet build`); this module converts it to the noun the on-ship
`ted/conf/attest.hoon` thread expects and pokes it in over conn.sock. The
thread re-derives each link's `sots` from its leaf script, so the skeleton
never encodes sotx.

Skeleton JSON shape:
    {"who": "~patp",
     "precommit": {"txid": <hex>, "block": <hex>},
     "links": [{"txid":<hex>,"block":<hex>,"internal_key_hex":<hex>,
                "leaf_version":<int>,"leaf_script_hex":<hex>}, ...],
     "tip": {"txid":<hex>,"vout":<int>,"off":<int>}}
"""

from __future__ import annotations

import copy
import json
import urllib.request
from pathlib import Path

from . import noun as N
from . import obphon


def load_skeleton(path: Path | str) -> dict:
    with open(path) as f:
        return json.load(f)


def _ux(h: str) -> int:
    return int(h, 16) if h else 0


def skeleton_noun(skel: dict) -> object:
    """dict -> the +skeleton:self-attestation noun [who precommit links tip].

    skel-link = [txid block internal-key version [wid dat]]."""
    who = obphon.patp_to_num(skel["who"])
    pre = (_ux(skel["precommit"]["txid"]), _ux(skel["precommit"]["block"]))

    def link_noun(l: dict) -> object:
        wid = len(l["leaf_script_hex"]) // 2
        dat = _ux(l["leaf_script_hex"])
        ikey = _ux(l["internal_key_hex"])
        return (_ux(l["txid"]),
                (_ux(l["block"]), (ikey, (l["leaf_version"], (wid, dat)))))

    links = N.nlist(link_noun(l) for l in skel["links"])
    tip = (_ux(skel["tip"]["txid"]), (skel["tip"]["vout"], skel["tip"]["off"]))
    return (who, (pre, (links, tip)))


def _post(ship, skel: dict, endpoint: str) -> int:
    """POST the jammed skeleton to %urb-watcher's eyre endpoint. Returns the
    HTTP status (urb-watcher cues it, builds the packet, and self-pokes)."""
    body = N.jam(skeleton_noun(skel))                      # bytes, LSB-first
    url = f"http://127.0.0.1:{ship.http_port}/apps/urb-watcher/{endpoint}"
    req = urllib.request.Request(url, data=body, method="POST")
    with urllib.request.urlopen(req, timeout=30) as r:
        return r.status


def poke_keyfile(ship, skel: dict) -> int:
    """Deliver our own chain as the attestation keyfile (%attestation-keyfile)."""
    return _post(ship, skel, "keyfile")


def poke_peer(ship, skel: dict) -> int:
    """Deliver a peer's packet (%self-attestation), simulating Ames."""
    return _post(ship, skel, "peer")


# -- tamper helpers (adversarial scenarios) ---------------------------------

def truncate(skel: dict, n_links: int) -> dict:
    s = copy.deepcopy(skel)
    s["links"] = s["links"][:n_links]
    if s["links"]:
        last = s["links"][-1]
        s["tip"] = {"txid": last["txid"], "vout": 0, "off": 0}
    return s


def swap_txid(skel: dict, i: int, txid_hex: str) -> dict:
    s = copy.deepcopy(skel)
    s["links"][i]["txid"] = txid_hex
    return s


def bad_tip_off(skel: dict, off: int) -> dict:
    s = copy.deepcopy(skel)
    s["tip"]["off"] = off
    return s


def pad_links(skel: dict, total: int) -> dict:
    """Pad to `total` links with copies (for the chain-cap DoS scenario)."""
    s = copy.deepcopy(skel)
    if not s["links"]:
        return s
    while len(s["links"]) < total:
        s["links"].append(copy.deepcopy(s["links"][-1]))
    return s
