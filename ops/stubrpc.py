#!/usr/bin/env python3
"""Minimal JSON-RPC stub on :8332 for %gw-btc's legacy block scanner.

Answers getblockcount with a fixed height (default = the height gw-btc was
started at), so the block thread completes each cycle with zero work instead
of failing and retrying every 30s.  No mainnet node is involved and nothing
is broadcast.  Any other method returns a JSON-RPC error.
"""
import json, sys
from http.server import BaseHTTPRequestHandler, HTTPServer

HEIGHT = int(sys.argv[1]) if len(sys.argv) > 1 else 961058

class H(BaseHTTPRequestHandler):
    def log_message(self, *a): pass
    def do_POST(self):
        n = int(self.headers.get('Content-Length', 0))
        body = self.rfile.read(n)
        try:
            reqs = json.loads(body)
        except Exception:
            reqs = []
        if isinstance(reqs, dict):
            reqs = [reqs]
        out = []
        for r in reqs:
            m = r.get('method'); i = r.get('id', '0')
            if m == 'getblockcount':
                out.append({"jsonrpc": "2.0", "id": i, "result": HEIGHT})
            else:
                out.append({"jsonrpc": "2.0", "id": i, "result": None,
                            "error": {"code": -32601, "message": "stub: %s" % m}})
        b = json.dumps(out).encode()
        self.send_response(200)
        self.send_header('Content-Type', 'application/json')
        self.send_header('Content-Length', str(len(b)))
        self.end_headers()
        self.wfile.write(b)

HTTPServer(('127.0.0.1', 8332), H).serve_forever()
