#!/usr/bin/env python3
"""mempool-stub.py PORT -- a fake mempool.space API for onboard-e2e.sh.

Serves just the surface Causeway touches, so the ENTIRE spawn -> finalize ->
boot pipeline can run with zero real bitcoin:

    GET  /address/{addr}/utxo   first address ever queried gets one confirmed
                                20,000-sat UTXO on its second poll (the first
                                returns [], so the polling loop itself runs)
    POST /tx                    accepts a raw tx, PARSES it (embit), stores its
                                outputs, returns the real txid
    GET  /tx/{txid}             funding tx -> status with a block height (what
                                resolve_start_height needs); broadcast tx ->
                                status + a REAL vout array from the parsed tx,
                                confirmed on the second ask (so finalize's
                                wait loop runs too)

The vout array is parsed from the bytes Causeway actually broadcast, not
fabricated -- so verify_proof_onchain's scriptpubkey comparison is a genuine
check of the commitment that was built, and a Causeway that commits the wrong
key FAILS here the way it would fail against the real chain.

State is in-memory; one run per process. Not a bitcoin node, not a test of
consensus -- a test of every line of OUR code on both sides of the network.
"""
import hashlib
import json
import re
import sys
from http.server import BaseHTTPRequestHandler, HTTPServer

from embit.transaction import Transaction

FUNDING_TXID = "aa" * 32
FUNDING_HEIGHT = 900_100
FUNDING_BLOCKHASH = "0b" * 32
SPAWN_HEIGHT = 900_105
SPAWN_BLOCKHASH = "0c" * 32
FUND_SATS = 20_000

STATE = {
    "funded_addr": None,      # first address ever asked about
    "addr_polls": 0,
    "txs": {},                # txid -> {"hex":..., "vout":[...]}
    "tx_polls": {},           # txid -> count
}


def parse_outputs(tx_hex: str):
    tx = Transaction.from_string(tx_hex)
    txid = tx.txid().hex()
    vout = [{"scriptpubkey": o.script_pubkey.data.hex(), "value": o.value}
            for o in tx.vout]
    return txid, vout


class H(BaseHTTPRequestHandler):
    def log_message(self, fmt, *args):  # quiet; the harness prints the story
        sys.stderr.write("  stub: %s\n" % (fmt % args))

    def _json(self, obj, code=200):
        body = json.dumps(obj).encode()
        self.send_response(code)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def _text(self, txt, code=200):
        body = txt.encode()
        self.send_response(code)
        self.send_header("Content-Type", "text/plain")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def do_GET(self):
        m = re.match(r"^/address/([^/]+)/utxo$", self.path)
        if m:
            addr = m.group(1)
            if STATE["funded_addr"] is None:
                STATE["funded_addr"] = addr
            if addr != STATE["funded_addr"]:
                return self._json([])
            STATE["addr_polls"] += 1
            if STATE["addr_polls"] < 2:
                return self._json([])        # make the polling loop actually loop
            return self._json([{
                "txid": FUNDING_TXID, "vout": 1, "value": FUND_SATS,
                "status": {"confirmed": True,
                           "block_height": FUNDING_HEIGHT,
                           "block_hash": FUNDING_BLOCKHASH},
            }])

        m = re.match(r"^/tx/([0-9a-fA-F]{64})$", self.path)
        if m:
            txid = m.group(1).lower()
            if txid == FUNDING_TXID:
                return self._json({
                    "txid": txid,
                    "status": {"confirmed": True,
                               "block_height": FUNDING_HEIGHT,
                               "block_hash": FUNDING_BLOCKHASH},
                })
            rec = STATE["txs"].get(txid)
            if rec is None:
                return self._json({"error": "not found"}, 404)
            n = STATE["tx_polls"][txid] = STATE["tx_polls"].get(txid, 0) + 1
            status = ({"confirmed": True, "block_height": SPAWN_HEIGHT,
                       "block_hash": SPAWN_BLOCKHASH}
                      if n >= 2 else {"confirmed": False})
            return self._json({"txid": txid, "vout": rec["vout"], "status": status})

        return self._json({"error": "unhandled path " + self.path}, 404)

    def do_POST(self):
        if self.path != "/tx":
            return self._json({"error": "unhandled path " + self.path}, 404)
        n = int(self.headers.get("Content-Length", "0"))
        tx_hex = self.rfile.read(n).decode().strip()
        try:
            txid, vout = parse_outputs(tx_hex)
        except Exception as e:  # noqa: BLE001
            return self._text(f"stub could not parse tx: {e}", 400)
        STATE["txs"][txid] = {"hex": tx_hex, "vout": vout}
        sys.stderr.write(f"  stub: accepted tx {txid} ({len(vout)} outputs)\n")
        return self._text(txid)


if __name__ == "__main__":
    port = int(sys.argv[1])
    # A tx whose fingerprint we log at start, so the harness can prove the
    # stub it is talking to is this process and not a stale one.
    sys.stderr.write(f"  stub: listening on 127.0.0.1:{port}\n")
    HTTPServer(("127.0.0.1", port), H).serve_forever()
