"""Live dashboard for the regtest comet net — stdlib only (no aiohttp).

Serves a single auto-refreshing page on :42999 that polls /api/state. State is
assembled from what actually works in this fork: the regtest node's tip/mempool
over RPC, and each pier's latest verifier verdict parsed from its slog (gall
peeks return ~ here, so the log is the assertion channel — same as the scenario
suite). The scenario matrix is read from run/results/SCENARIOS.md.

    python3 -m gwharness dashboard        # http://127.0.0.1:42999
"""

from __future__ import annotations

import json
import re
import time
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path

from .config import Config
from . import btc


_VERDICT = re.compile(r"attestation for (\S+) is (VALID|INVALID)")
_CHECK = re.compile(r"\[(ok|XX)\]\s+(\S+)")
_CONFIG = re.compile(r"reconfigured to (\S+) from block (\d+)")
_VERIFYING = re.compile(r"verifying (?:attestation keyfile|self-attestation[^(]*)\((\d+) links")


def _pier_state(log: Path) -> dict:
    """Latest verifier activity for one pier, parsed from its slog."""
    who = log.stem
    st = {"pier": who, "verdict": None, "checks": [], "configured": None,
          "links": None, "last_line": ""}
    if not log.exists():
        return st
    text = log.read_text(errors="replace")
    lines = text.splitlines()
    for line in lines:
        mc = _CONFIG.search(line)
        if mc:
            st["configured"] = {"url": mc.group(1), "block": int(mc.group(2))}
        mver = _VERIFYING.search(line)
        if mver:
            st["links"] = int(mver.group(1))
    # walk backwards for the most recent verdict block
    last_v = None
    for i, line in enumerate(lines):
        mv = _VERDICT.search(line)
        if mv:
            last_v = (i, mv)
    if last_v is not None:
        i, mv = last_v
        checks = []
        for line in lines[i:i + 40]:
            m = _CHECK.search(line)
            if m:
                checks.append({"name": m.group(2), "ok": m.group(1) == "ok"})
            elif _VERDICT.search(line) and line is not lines[i]:
                break
        st["verdict"] = {"who": mv.group(1), "ok": mv.group(2) == "VALID"}
        st["checks"] = checks
    if lines:
        st["last_line"] = lines[-1].strip()[:160]
    return st


def _scenario_table(results_dir: Path) -> str:
    md = results_dir / "SCENARIOS.md"
    return md.read_text(errors="replace") if md.exists() else ""


def collect_state(cfg: Config) -> dict:
    state = {"ts": int(time.time()), "chain": None, "piers": [], "scenarios": ""}
    try:
        if btc.is_running(cfg):
            rpc = btc.BitcoinRPC(cfg)
            h, n = btc.get_tip(rpc)
            mem = rpc("getrawmempool")
            state["chain"] = {"height": n, "tip": h[:16], "mempool": len(mem),
                              "port": cfg.rpcport}
    except Exception as e:                                    # noqa: BLE001
        state["chain"] = {"error": str(e)}
    logs = sorted(cfg.logs_dir.glob("*.log")) if cfg.logs_dir.exists() else []
    state["piers"] = [_pier_state(p) for p in logs]
    state["scenarios"] = _scenario_table(cfg.results_dir)
    return state


_PAGE = """<!doctype html><html><head><meta charset=utf-8>
<title>groundwire · confidential comets regtest</title>
<style>
 body{background:#0b0d10;color:#cdd6e0;font:13px/1.5 ui-monospace,Menlo,monospace;margin:0;padding:18px}
 h1{font-size:15px;color:#7fd6c2;margin:0 0 4px;letter-spacing:.5px}
 .sub{color:#5d6b7a;margin-bottom:16px}
 .grid{display:grid;grid-template-columns:repeat(auto-fill,minmax(360px,1fr));gap:14px}
 .card{background:#12161b;border:1px solid #1f2630;border-radius:8px;padding:12px 14px}
 .card h2{font-size:13px;margin:0 0 8px;color:#9fb3c8;word-break:break-all}
 .ok{color:#5fd38a}.bad{color:#ff6b6b}.dim{color:#5d6b7a}
 .pill{display:inline-block;padding:1px 8px;border-radius:10px;font-size:11px;font-weight:600}
 .pill.ok{background:#10331f;color:#5fd38a}.pill.bad{background:#3a1414;color:#ff8585}
 .pill.idle{background:#1c2330;color:#7e8ea0}
 .checks{display:flex;flex-wrap:wrap;gap:3px;margin-top:6px}
 .chk{font-size:10px;padding:1px 5px;border-radius:4px;background:#16202a}
 .chk.ok{color:#5fd38a}.chk.bad{background:#3a1414;color:#ff8585}
 .chain{display:flex;gap:22px;margin-bottom:16px;flex-wrap:wrap}
 .chain b{color:#e6c07b}
 pre{white-space:pre-wrap;background:#0d1116;border:1px solid #1f2630;border-radius:6px;padding:10px;overflow:auto;font-size:12px}
 table{border-collapse:collapse;width:100%}td,th{border-bottom:1px solid #1f2630;padding:3px 8px;text-align:left;font-size:12px}
 .last{color:#586675;font-size:11px;margin-top:6px;word-break:break-all}
</style></head><body>
<h1>◆ GROUNDWIRE · CONFIDENTIAL COMETS · regtest testnet</h1>
<div class=sub id=ts>connecting…</div>
<div class=chain id=chain></div>
<div class=grid id=piers></div>
<h2 style="color:#9fb3c8;margin-top:22px">scenario matrix</h2>
<div id=scen class=dim>—</div>
<script>
function mdtable(md){
 if(!md) return '<span class=dim>no scenario run yet — <code>make scenario-all</code></span>';
 let rows=md.split('\\n').filter(l=>l.trim().startsWith('|'));
 if(!rows.length) return '<pre>'+md.replace(/[<>]/g,'')+'</pre>';
 let html='<table>';
 rows.forEach((r,i)=>{ if(/^\\|[-\\s|]+\\|$/.test(r))return;
   let cells=r.split('|').slice(1,-1).map(c=>c.trim());
   let tag=i==0?'th':'td';
   let cls=cells.some(c=>c=='PASS')?'ok':cells.some(c=>c=='FAIL')?'bad':'';
   html+='<tr class='+cls+'>'+cells.map(c=>'<'+tag+'>'+c.replace(/[<>]/g,'')+'</'+tag+'>').join('')+'</tr>';
 });
 return html+'</table>';
}
async function tick(){
 try{
  let s=await (await fetch('/api/state')).json();
  document.getElementById('ts').textContent='updated '+new Date(s.ts*1000).toLocaleTimeString();
  let c=s.chain;
  document.getElementById('chain').innerHTML = c? (c.error?'<span class=bad>bitcoind: '+c.error+'</span>':
    'chain <b>'+c.height+'</b> blocks · tip <b>'+c.tip+'…</b> · mempool <b>'+c.mempool+'</b> · :'+c.port)
    : '<span class=dim>bitcoind not running</span>';
  document.getElementById('piers').innerHTML = s.piers.length? s.piers.map(p=>{
   let v=p.verdict, pill = !v?'<span class="pill idle">no verdict</span>'
     : v.ok?'<span class="pill ok">VALID</span>':'<span class="pill bad">INVALID</span>';
   let checks=(p.checks||[]).map(k=>'<span class="chk '+(k.ok?'ok':'bad')+'">'+(k.ok?'✓':'✗')+' '+k.name+'</span>').join('');
   let cfg=p.configured?'<span class=dim>watch '+p.configured.url+' @'+p.configured.block+'</span>':'<span class=dim>not configured</span>';
   return '<div class=card><h2>~'+p.pier+'</h2>'+pill+' '+cfg+
     (p.links?' <span class=dim>· '+p.links+' links</span>':'')+
     '<div class=checks>'+checks+'</div>'+
     '<div class=last>'+(p.last_line||'').replace(/[<>]/g,'')+'</div></div>';
  }).join('') : '<span class=dim>no piers booted</span>';
  document.getElementById('scen').innerHTML = mdtable(s.scenarios);
 }catch(e){ document.getElementById('ts').textContent='poll error: '+e; }
}
tick(); setInterval(tick, 2500);
</script></body></html>"""


def serve(cfg: Config, port: int = 42999) -> None:
    class Handler(BaseHTTPRequestHandler):
        def log_message(self, *a):                            # silence access log
            pass

        def _send(self, code: int, body: bytes, ctype: str) -> None:
            self.send_response(code)
            self.send_header("Content-Type", ctype)
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)

        def do_GET(self):                                     # noqa: N802
            if self.path.startswith("/api/state"):
                body = json.dumps(collect_state(cfg)).encode()
                self._send(200, body, "application/json")
            else:
                self._send(200, _PAGE.encode(), "text/html; charset=utf-8")

    srv = ThreadingHTTPServer(("127.0.0.1", port), Handler)
    print(f"[dash] http://127.0.0.1:{port}  (Ctrl-C to stop)", flush=True)
    try:
        srv.serve_forever()
    except KeyboardInterrupt:
        print("\n[dash] stopped", flush=True)
        srv.shutdown()
