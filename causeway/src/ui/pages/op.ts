import { base64 } from "@scure/base";
import { el, clearAndAppend, banner, copyButton, kvList } from "../components.js";
import { go, getSession } from "../state.js";
import { atomToPatp, patpToAtom, isPatp } from "../../protocol/patp.js";
import { ops } from "../../ops/index.js";
import type { OpCtx } from "../../ops/types.js";
import type { Fief } from "../../protocol/types.js";
import { deriveKeyInfo } from "../../keys/xpub.js";
import { animateUR } from "../../signing/qr-render.js";
import { encodePsbtUR } from "../../signing/qr-ur.js";
import { scanPsbtFromCamera } from "../../signing/qr-scan.js";
import QrScanner from "qr-scanner";

type FormSpec = Array<{
  name: string;
  label: string;
  kind: "text" | "patp" | "number" | "fief" | "checkbox";
  placeholder?: string;
  required?: boolean;
}>;

const OP_FORMS: Record<string, FormSpec> = {
  "rekey": [
    { name: "newPassHex", label: "New networking pass (hex atom)", kind: "text", required: true },
    { name: "breach", label: "Breach (discontinuity)", kind: "checkbox" },
  ],
  "escape": [
    { name: "newSponsor", label: "New sponsor @p", kind: "patp", required: true },
    { name: "sponsorSigHex", label: "Sponsor's off-chain signature (hex, optional)", kind: "text" },
  ],
  "cancel-escape": [
    { name: "pendingSponsor", label: "Pending sponsor @p", kind: "patp", required: true },
  ],
  "adopt": [{ name: "child", label: "Child @p", kind: "patp", required: true }],
  "reject": [{ name: "child", label: "Child @p", kind: "patp", required: true }],
  "detach": [{ name: "child", label: "Child @p", kind: "patp", required: true }],
  "fief": [
    { name: "ip", label: "IPv4 address (e.g. 1.2.3.4) — leave empty to clear", kind: "text" },
    { name: "port", label: "Port", kind: "number", placeholder: "31337" },
  ],
  "set-mang": [
    { name: "passHex", label: "Management pass (hex) — leave empty to clear", kind: "text" },
  ],
};

function parseIPv4ToInt(s: string): number {
  const parts = s.trim().split(".").map(Number);
  if (parts.length !== 4 || parts.some((n) => !Number.isInteger(n) || n < 0 || n > 255)) {
    throw new Error("invalid IPv4");
  }
  return ((parts[0]! << 24) | (parts[1]! << 16) | (parts[2]! << 8) | parts[3]!) >>> 0;
}

function buildArgs(opName: string, form: HTMLFormElement): any {
  const data = new FormData(form);
  const get = (k: string): string => String(data.get(k) ?? "").trim();
  switch (opName) {
    case "rekey":
      return {
        newPass: BigInt("0x" + get("newPassHex").replace(/^0x/, "")),
        breach: data.has("breach"),
      };
    case "escape": {
      const sigHex = get("sponsorSigHex");
      return {
        newSponsor: patpToAtom(get("newSponsor")),
        sponsorSig: sigHex ? BigInt("0x" + sigHex.replace(/^0x/, "")) : null,
      };
    }
    case "cancel-escape":
      return { pendingSponsor: patpToAtom(get("pendingSponsor")) };
    case "adopt": case "reject": case "detach":
      return { child: patpToAtom(get("child")) };
    case "fief": {
      const ipStr = get("ip"); const portStr = get("port");
      if (!ipStr && !portStr) return { fief: null };
      return { fief: { type: "if", ip: parseIPv4ToInt(ipStr), port: Number(portStr) } as Fief };
    }
    case "set-mang": {
      const h = get("passHex").replace(/^0x/, "");
      if (!h) return { mang: null };
      return { mang: { type: "pass", pass: BigInt("0x" + h) } };
    }
    default: return {};
  }
}

// Build a signed-PSBT input block: shows a scan-with-camera button (if the
// device has a camera) and a paste fallback. Writes the accepted PSBT bytes
// into `setState` when successful.
function signedPsbtInput(
  title: string,
  setState: (bytes: Uint8Array) => void,
): HTMLElement {
  const wrap = el("section", { class: "card" });
  wrap.append(
    el("h3", {}, title),
    el("p", {}, "After signing on your device, point Causeway at the signed PSBT:"),
  );

  const mode = el("div", { class: "row" });
  const scanBtn = el("button", { class: "btn primary", type: "button" }, "Scan with camera");
  const pasteBtn = el("button", { class: "btn secondary", type: "button" }, "Paste base64");
  mode.append(scanBtn, pasteBtn);
  wrap.appendChild(mode);

  const scanArea = el("div", { style: "margin-top:1rem;display:none;" });
  const scanVideo = el("div");
  const scanMeta = el("div", { class: "qr-meta" }, "");
  const scanStop = el("button", { class: "btn secondary", type: "button" }, "cancel scan");
  const scanErr = el("div");
  scanArea.append(scanVideo, scanMeta, scanStop, scanErr);
  wrap.appendChild(scanArea);

  const pasteArea = el("div", { style: "margin-top:1rem;display:none;" });
  const textarea = el("textarea", {
    rows: "4",
    placeholder: "signed PSBT (base64)",
  }) as HTMLTextAreaElement;
  const pasteAccept = el("button", { class: "btn primary", type: "button" }, "Accept");
  const pasteErr = el("div");
  pasteArea.append(textarea, pasteAccept, pasteErr);
  wrap.appendChild(pasteArea);

  const ok = el("div", { class: "banner ok", style: "display:none;" }, "PSBT accepted ✓");
  wrap.appendChild(ok);

  let activeScanStop: (() => void) | null = null;

  // Disable scan button if no camera on this device.
  QrScanner.hasCamera().then((has) => {
    if (!has) {
      scanBtn.setAttribute("disabled", "true");
      scanBtn.title = "no camera detected";
    }
  }).catch(() => {});

  scanBtn.addEventListener("click", async () => {
    scanArea.style.display = "";
    pasteArea.style.display = "none";
    scanErr.innerHTML = "";
    scanMeta.textContent = "requesting camera…";
    try {
      const { handle, psbt } = await scanPsbtFromCamera(scanVideo, (p) => {
        scanMeta.textContent = p.expected > 1
          ? `frames ${p.received} / ${p.expected} (${p.percent}%)`
          : `scanning…`;
      });
      activeScanStop = () => handle.stop();
      const bytes = await psbt;
      activeScanStop = null;
      setState(bytes);
      scanArea.style.display = "none";
      ok.style.display = "";
    } catch (err: any) {
      scanErr.innerHTML = "";
      scanErr.appendChild(banner("err", `scan error: ${err.message ?? err}`));
    }
  });

  pasteBtn.addEventListener("click", () => {
    scanArea.style.display = "none";
    pasteArea.style.display = "";
    if (activeScanStop) { activeScanStop(); activeScanStop = null; }
  });

  scanStop.addEventListener("click", () => {
    if (activeScanStop) { activeScanStop(); activeScanStop = null; }
    scanArea.style.display = "none";
  });

  pasteAccept.addEventListener("click", () => {
    pasteErr.innerHTML = "";
    try {
      const bytes = base64.decode(textarea.value.trim());
      if (bytes.length < 10) throw new Error("too short");
      setState(bytes);
      pasteArea.style.display = "none";
      ok.style.display = "";
    } catch (err: any) {
      pasteErr.innerHTML = "";
      pasteErr.appendChild(banner("err", `invalid: ${err.message ?? err}`));
    }
  });

  return wrap;
}

export function renderOp(root: HTMLElement, opName: string): void {
  const s = getSession();
  if (!s || s.patpAtom === undefined || !s.auth) { go("#/"); return; }
  if (!(opName in ops)) { root.textContent = `unknown op: ${opName}`; return; }
  if (!s.keys) { go("#/keys"); return; }

  const patpAtom = s.patpAtom;
  const auth = s.auth;

  const card = el("section", { class: "card" });
  card.append(
    el("h1", {}, opName),
    el("p", {}, `Build a commit + reveal PSBT pair for ${atomToPatp(patpAtom)}.`),
  );

  const form = el("form", { id: "opForm" });
  const spec = OP_FORMS[opName] ?? [];
  for (const f of spec) {
    const lbl = el("label", { for: f.name }, f.label);
    let input: HTMLElement;
    if (f.kind === "checkbox") {
      input = el("input", { type: "checkbox", name: f.name, id: f.name });
    } else {
      input = el("input", {
        type: f.kind === "number" ? "number" : "text",
        name: f.name, id: f.name,
        placeholder: f.placeholder ?? "",
      });
    }
    form.append(lbl, input);
  }

  const buildBtn = el("button", { class: "btn primary", type: "submit" }, "Build PSBTs");
  form.appendChild(buildBtn);
  card.appendChild(form);

  const buildStatus = el("div");
  card.appendChild(buildStatus);

  const psbtCard = el("section", { class: "card", style: "display:none;" });
  psbtCard.appendChild(el("h2", {}, "Sign on your hardware wallet"));

  const commitQr = el("div", { class: "qr-pane" });
  const commitLeft = el("div");
  const commitRight = el("div");
  commitRight.appendChild(el("h3", {}, "Commit PSBT"));
  commitRight.appendChild(el("p", {},
    "Scan this animated QR on your hardware wallet. Sign, then bring the signed PSBT back."));
  const commitCopy = el("div");
  commitRight.appendChild(commitCopy);
  commitQr.append(commitLeft, commitRight);
  psbtCard.appendChild(commitQr);

  const revealQr = el("div", { class: "qr-pane", style: "margin-top:2rem;" });
  const revealLeft = el("div");
  const revealRight = el("div");
  revealRight.appendChild(el("h3", {}, "Reveal PSBT"));
  revealRight.appendChild(el("p", {},
    "Sign this second PSBT on your device. Causeway broadcasts commit first, then reveal."));
  const revealCopy = el("div");
  revealRight.appendChild(revealCopy);
  revealQr.append(revealLeft, revealRight);
  psbtCard.appendChild(revealQr);

  let signedCommit: Uint8Array | null = null;
  let signedReveal: Uint8Array | null = null;

  psbtCard.appendChild(signedPsbtInput("Signed commit PSBT", (b) => { signedCommit = b; }));
  psbtCard.appendChild(signedPsbtInput("Signed reveal PSBT", (b) => { signedReveal = b; }));

  const bcastBtn = el("button", { class: "btn primary", type: "button" }, "Verify & broadcast");
  const bcastRow = el("div", { class: "row" });
  bcastRow.appendChild(bcastBtn);
  psbtCard.appendChild(bcastRow);

  const status = el("div");
  psbtCard.appendChild(status);
  const resultCard = el("section", { class: "card", style: "display:none;" });

  clearAndAppend(root, card, psbtCard, resultCard);

  let ctx: OpCtx | null = null;
  let commitPsbt: Uint8Array | null = null;
  let revealPsbt: Uint8Array | null = null;

  form.addEventListener("submit", async (ev) => {
    ev.preventDefault();
    buildStatus.innerHTML = "";
    try {
      const args = buildArgs(opName, form);
      const session = s!;
      const keys = session.keys!;

      ctx = {
        state: session.snapshot,
        patpAtom,
        inscriptionUtxo: {
          txid: new Uint8Array(32),
          vout: auth.utxo.vout,
          value: BigInt(auth.utxo.value),
          scriptPubKey: auth.utxo.scriptPubKey,
        },
        fundingKey: deriveKeyInfo(keys, 0, 0),
        commitKey: deriveKeyInfo(keys, 0, 1),
        destKey: deriveKeyInfo(keys, 0, 2),
        feeRate: 2,
        mp: session.mp,
      };
      const txidHex = auth.utxo.txid;
      const txidBytes = new Uint8Array(32);
      for (let i = 0; i < 32; i++) {
        txidBytes[31 - i] = parseInt(txidHex.slice(i * 2, i * 2 + 2), 16);
      }
      ctx.inscriptionUtxo.txid = txidBytes;

      const mod = (ops as any)[opName];
      const pair = await mod.buildPsbts(args, ctx);
      commitPsbt = pair.commitPsbt;
      revealPsbt = pair.revealPsbt;

      psbtCard.style.display = "";

      const commitStream = encodePsbtUR(commitPsbt!);
      animateUR(commitLeft, commitStream, { fps: 4, size: 360 });
      commitCopy.innerHTML = "";
      commitCopy.appendChild(copyButton(() => base64.encode(commitPsbt!), "copy base64"));

      const revealStream = encodePsbtUR(revealPsbt!);
      animateUR(revealLeft, revealStream, { fps: 4, size: 360 });
      revealCopy.innerHTML = "";
      revealCopy.appendChild(copyButton(() => base64.encode(revealPsbt!), "copy base64"));
    } catch (err: any) {
      buildStatus.innerHTML = "";
      buildStatus.appendChild(banner("err", `build error: ${err.message ?? err}`));
    }
  });

  bcastBtn.addEventListener("click", async () => {
    status.innerHTML = "";
    try {
      if (!ctx) throw new Error("build PSBTs first");
      if (!signedCommit) throw new Error("need signed commit PSBT");
      if (!signedReveal) throw new Error("need signed reveal PSBT");
      const mod = (ops as any)[opName];
      status.appendChild(banner("warn", "broadcasting commit, then reveal…"));
      const result = await mod.broadcast(signedCommit, signedReveal, ctx);
      resultCard.style.display = "";
      resultCard.innerHTML = "";
      resultCard.appendChild(el("h2", {}, "Broadcast ✓"));
      resultCard.appendChild(kvList(result.txids.map((t: string, i: number) =>
        [i === 0 ? "commit txid" : "reveal txid", t])));
      status.innerHTML = "";
    } catch (err: any) {
      status.innerHTML = "";
      status.appendChild(banner("err", `broadcast error: ${err.message ?? err}`));
    }
  });
}
