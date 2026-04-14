// Minimal hash-routed UI. The signing flow is paste-based (not camera QR) so
// the whole app is testable locally in two browser tabs without a HW wallet.
//
// Routes:
//   #/login           - enter a @p, fetch snapshot, kick off login sign
//   #/dash            - after login, show point info + op links
//   #/op/<name>       - build a SignRequest for op <name>

import { Mempool } from "../chain/mempool.js";
import { fetchSnapshot } from "../oracle/snapshot.js";
import { lookupPoint, resolveAuthPubkey } from "../oracle/point.js";
import type { UrbState } from "../oracle/state.js";
import { atomToPatp, isPatp, patpToAtom } from "../protocol/patp.js";
import { encodeSignRequest, decodeSignResponse } from "../signing/qr-protocol.js";
import type { SignRequest } from "../signing/qr-protocol.js";
import { verifyResponse } from "../signing/verify.js";
import { ops } from "../ops/index.js";
import type { OpCtx } from "../ops/types.js";

interface Session {
  patpAtom: bigint;
  state: UrbState;
  authPubkey: Uint8Array;
  mp: Mempool;
}

let session: Session | null = null;

function el<K extends keyof HTMLElementTagNameMap>(
  tag: K, attrs: Record<string, string> = {}, text?: string,
): HTMLElementTagNameMap[K] {
  const e = document.createElement(tag);
  for (const [k, v] of Object.entries(attrs)) e.setAttribute(k, v);
  if (text !== undefined) e.textContent = text;
  return e;
}

function h(html: string): DocumentFragment {
  const t = document.createElement("template");
  t.innerHTML = html;
  return t.content;
}

function go(hash: string): void { window.location.hash = hash; }

async function renderLogin(root: HTMLElement): Promise<void> {
  root.innerHTML = "";
  root.appendChild(h(`
    <h1>Causeway</h1>
    <p>Air-gapped comet identity bridge.</p>
    <form id="loginForm">
      <label>Comet @p: <input name="patp" value="~" style="font:14px monospace;width:360px"></label>
      <button type="submit">Continue</button>
    </form>
    <pre id="out"></pre>
  `));
  const form = root.querySelector<HTMLFormElement>("#loginForm")!;
  const out = root.querySelector<HTMLElement>("#out")!;
  form.addEventListener("submit", async (ev) => {
    ev.preventDefault();
    out.textContent = "fetching snapshot…";
    try {
      const data = new FormData(form);
      const patp = String(data.get("patp") ?? "").trim();
      if (!isPatp(patp)) throw new Error(`invalid @p: ${patp}`);
      const patpAtom = patpToAtom(patp);
      const state = await fetchSnapshot();
      if (!lookupPoint(state, patpAtom)) throw new Error(`@p not in snapshot: ${patp}`);
      const mp = new Mempool();
      const auth = await resolveAuthPubkey(state, patpAtom, mp);
      session = { patpAtom, state, authPubkey: auth.tweaked, mp };
      go("#/dash");
    } catch (err: any) {
      out.textContent = `error: ${err.message ?? err}`;
    }
  });
}

function renderDash(root: HTMLElement): void {
  if (!session) { go("#/login"); return; }
  const s = session;
  const point = lookupPoint(s.state, s.patpAtom);
  root.innerHTML = "";
  root.appendChild(h(`
    <h1>Dashboard — ${atomToPatp(s.patpAtom)}</h1>
    <pre style="background:#f0f0f0;padding:8px">${
      point ? JSON.stringify({
        rift: point.net.rift,
        life: point.net.life,
        sponsor: atomToPatp(point.net.sponsor.who),
        escape: point.net.escape ? atomToPatp(point.net.escape) : null,
      }, null, 2) : "no point"
    }</pre>
    <h2>Operations</h2>
    <ul>
      ${Object.keys(ops).filter((k) => k !== "login").map((k) =>
        `<li><a href="#/op/${k}">${k}</a></li>`).join("")}
    </ul>
  `));
}

async function renderOp(root: HTMLElement, opName: string): Promise<void> {
  if (!session) { go("#/login"); return; }
  if (!(opName in ops)) { root.textContent = `unknown op: ${opName}`; return; }
  root.innerHTML = "";
  root.appendChild(h(`
    <h1>${opName}</h1>
    <p>Fill args below, build the SignRequest, copy to the HW stub, paste signed response back.</p>
    <textarea id="args" rows="6" cols="80" placeholder="JSON args for this op"></textarea><br>
    <button id="build">Build SignRequest</button>
    <h3>SignRequest</h3>
    <pre id="req" style="background:#111;color:#9f9;padding:8px;white-space:pre-wrap;word-break:break-all"></pre>
    <h3>Paste SignResponse</h3>
    <textarea id="resp" rows="4" cols="80"></textarea><br>
    <button id="verify">Verify &amp; finalize</button>
    <pre id="out"></pre>
  `));
  const argsArea = root.querySelector<HTMLTextAreaElement>("#args")!;
  const reqPre = root.querySelector<HTMLElement>("#req")!;
  const respArea = root.querySelector<HTMLTextAreaElement>("#resp")!;
  const out = root.querySelector<HTMLElement>("#out")!;

  const ctx: OpCtx = {
    state: session.state,
    patpAtom: session.patpAtom,
    authPubkey: session.authPubkey,
    feeRate: 2,
    mp: session.mp,
  };

  let currentReq: SignRequest | null = null;

  root.querySelector<HTMLButtonElement>("#build")!.addEventListener("click", async () => {
    try {
      const mod = (ops as any)[opName];
      const args = argsArea.value.trim() ? JSON.parse(argsArea.value) : undefined;
      currentReq = await mod.buildRequest(args, ctx);
      reqPre.textContent = encodeSignRequest(currentReq!);
    } catch (err: any) {
      out.textContent = `error: ${err.message ?? err}`;
    }
  });

  root.querySelector<HTMLButtonElement>("#verify")!.addEventListener("click", async () => {
    try {
      if (!currentReq) throw new Error("build a request first");
      const res = decodeSignResponse(respArea.value);
      verifyResponse(currentReq, res);
      const mod = (ops as any)[opName];
      const result = await mod.finalize(currentReq, res.sigs, ctx);
      out.textContent = `ok: ${JSON.stringify(result)}`;
    } catch (err: any) {
      out.textContent = `error: ${err.message ?? err}`;
    }
  });
}

async function route(): Promise<void> {
  const root = document.getElementById("app");
  if (!root) return;
  const hash = window.location.hash || "#/login";
  if (hash === "#/login") { await renderLogin(root); return; }
  if (hash === "#/dash")  { renderDash(root); return; }
  if (hash.startsWith("#/op/")) { await renderOp(root, hash.slice(5)); return; }
  root.textContent = `unknown route: ${hash}`;
}

window.addEventListener("hashchange", () => { route().catch(console.error); });
route().catch(console.error);

export {};
