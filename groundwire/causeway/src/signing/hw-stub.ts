// DEV-ONLY software HW wallet. Opens at /causeway/hw-stub.html.
// User pastes a SignRequest JSON, picks a derivation index, and either
// pastes a dev mnemonic or a raw @q-style seed. The page produces a
// SignResponse JSON to copy back into Causeway.

import { schnorr } from "@noble/curves/secp256k1";
import { HDKey } from "@scure/bip32";
import { mnemonicToSeedSync } from "@scure/bip39";
import { sha256 } from "@noble/hashes/sha256";
import {
  decodeSignRequest,
  encodeSignResponse,
  type SignRequest,
} from "./qr-protocol.js";

function el(tag: string, attrs: Record<string, string> = {}, text?: string): HTMLElement {
  const e = document.createElement(tag);
  for (const [k, v] of Object.entries(attrs)) e.setAttribute(k, v);
  if (text !== undefined) e.textContent = text;
  return e;
}

function bytesToHex(b: Uint8Array): string {
  return Array.from(b, (x) => x.toString(16).padStart(2, "0")).join("");
}

function seedForMode(mode: "mnemonic" | "qseed", value: string): Uint8Array {
  if (mode === "mnemonic") return mnemonicToSeedSync(value.trim());
  // @q seed: raw atom bytes. Accept hex here.
  const hex = value.trim().replace(/^0x/, "");
  const out = new Uint8Array(hex.length / 2);
  for (let i = 0; i < out.length; i++) out[i] = parseInt(hex.slice(i * 2, i * 2 + 2), 16);
  return out;
}

function signAt(
  seed: Uint8Array,
  path: string,
  message: Uint8Array,
  auxRand: Uint8Array,
): Uint8Array {
  const root = HDKey.fromMasterSeed(seed);
  const node = root.derive(path);
  if (!node.privateKey) throw new Error("no private key at path");
  return schnorr.sign(message, node.privateKey, auxRand);
}

function render(root: HTMLElement): void {
  root.innerHTML = "";
  const banner = el("div", {
    style:
      "background:#c00;color:#fff;padding:12px 16px;font:600 14px system-ui;margin-bottom:16px;",
  }, "DEV ONLY — software HW wallet. Never paste a real mnemonic or seed.");
  root.appendChild(banner);

  const form = el("div", { style: "font:14px system-ui;max-width:720px;padding:0 16px;" });
  form.appendChild(el("h2", {}, "Sign request"));

  const reqArea = el("textarea", {
    rows: "10", cols: "80",
    placeholder: "Paste SignRequest JSON here…",
    style: "width:100%;font:12px/1.4 monospace;",
  }) as HTMLTextAreaElement;
  form.appendChild(reqArea);

  const seedRow = el("div", { style: "margin-top:12px;" });
  const modeSel = el("select") as HTMLSelectElement;
  modeSel.appendChild(el("option", { value: "mnemonic" }, "BIP-39 mnemonic"));
  modeSel.appendChild(el("option", { value: "qseed" }, "@q seed (hex bytes)"));
  seedRow.appendChild(el("label", {}, "Seed type: "));
  seedRow.appendChild(modeSel);
  form.appendChild(seedRow);

  const seedArea = el("textarea", {
    rows: "2", cols: "80",
    placeholder: "Mnemonic or seed hex",
    style: "width:100%;font:12px/1.4 monospace;margin-top:6px;",
  }) as HTMLTextAreaElement;
  form.appendChild(seedArea);

  const pathRow = el("div", { style: "margin-top:12px;" });
  pathRow.appendChild(el("label", {}, "Derivation path: "));
  const pathInp = el("input", {
    type: "text",
    value: "m/86'/1'/0'/0/0",
    style: "font:12px monospace;width:260px;",
  }) as HTMLInputElement;
  pathRow.appendChild(pathInp);
  form.appendChild(pathRow);

  const btn = el("button", {
    style: "margin-top:16px;padding:8px 20px;font:14px system-ui;cursor:pointer;",
  }, "Review & sign") as HTMLButtonElement;
  form.appendChild(btn);

  const out = el("pre", {
    style:
      "margin-top:16px;background:#111;color:#9f9;padding:12px;font:12px monospace;white-space:pre-wrap;word-break:break-all;",
  });
  form.appendChild(out);

  btn.addEventListener("click", () => {
    out.textContent = "";
    try {
      const req: SignRequest = decodeSignRequest(reqArea.value);
      const seed = seedForMode(modeSel.value as "mnemonic" | "qseed", seedArea.value);
      const summary =
        `op: ${req.op}\n` +
        `patp: ${req.patp}\n` +
        `authKey: ${bytesToHex(req.authKey)}\n` +
        `summary: ${req.summary}\n` +
        `sighashes: ${req.sighashes.length}\n`;
      if (!confirm(summary + "\nSign?")) { out.textContent = "cancelled"; return; }
      const auxRand = new Uint8Array(32);
      crypto.getRandomValues(auxRand);
      const sigs = req.sighashes.map((sh) => signAt(seed, pathInp.value, sh.hash, auxRand));
      const res = encodeSignResponse({ v: 1, nonce: req.nonce, sigs });
      out.textContent = res;
    } catch (err: any) {
      out.textContent = `error: ${err.message ?? err}`;
    }
  });

  root.appendChild(form);
}

const root = document.getElementById("app");
if (root) render(root);

export {};
