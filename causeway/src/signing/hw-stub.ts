// DEV-ONLY software PSBT signer. Opens at /causeway/hw-stub.html.
//
// Primary purpose: sign Causeway-produced PSBTs (commit + reveal for the
// spawn flow) when external wallets refuse. Sparrow in particular won't
// sign Taproot script-path spends whose leaf isn't part of its declared
// wallet policy (tr(BIP39) is key-path-only) — a known limitation for
// ordinal-style custom leaves. This stub uses @scure/btc-signer's signIdx
// which DOES sign arbitrary script-path PSBTs given the private key.
//
// Output is a fully-finalized raw tx hex, ready for Sparrow's
// "Broadcast Transaction" tool or `bitcoin-cli sendrawtransaction`.

import { Transaction } from "@scure/btc-signer";
import { HDKey } from "@scure/bip32";
import { mnemonicToSeedSync } from "@scure/bip39";
import { base64, hex as hexCoder } from "@scure/base";

function el<K extends keyof HTMLElementTagNameMap>(
  tag: K, attrs: Record<string, string> = {}, text?: string,
): HTMLElementTagNameMap[K] {
  const e = document.createElement(tag);
  for (const [k, v] of Object.entries(attrs)) e.setAttribute(k, v);
  if (text !== undefined) e.textContent = text;
  return e;
}

function seedForMode(mode: "mnemonic" | "qseed", value: string): Uint8Array {
  if (mode === "mnemonic") return mnemonicToSeedSync(value.trim());
  const hex = value.trim().replace(/^0x/, "");
  const out = new Uint8Array(hex.length / 2);
  for (let i = 0; i < out.length; i++) out[i] = parseInt(hex.slice(i * 2, i * 2 + 2), 16);
  return out;
}

// Derivation paths to try: mainnet (BIP-86 coin=0) and testnet (coin=1),
// receive (change=0) and change-address (change=1), indices 0..29.
function* candidatePaths(): IterableIterator<string> {
  for (const coin of [0, 1]) {
    for (const change of [0, 1]) {
      for (let i = 0; i < 30; i++) {
        yield `m/86'/${coin}'/0'/${change}/${i}`;
      }
    }
  }
}

function render(root: HTMLElement): void {
  root.innerHTML = "";

  const container = el("div", { class: "container" });

  container.appendChild(el("h1", { style: "font-family: 'Instrument Serif', Georgia, serif; margin-top: 1.5rem;" },
    "Causeway HW stub"));
  container.appendChild(el("p", { class: "lead" },
    "Dev-only PSBT signer for Taproot script-path spends that Sparrow refuses. "
    + "Paste your unsigned PSBT and BIP-39 mnemonic; get back a ready-to-broadcast "
    + "raw tx hex. Only use with test seeds or on a machine you trust."));

  container.appendChild(el("h2", {}, "Unsigned PSBT (base64)"));
  const psbtArea = el("textarea", {
    rows: "6",
    placeholder: "Paste unsigned PSBT base64 here…",
    spellcheck: "false", autocomplete: "off",
  }) as HTMLTextAreaElement;
  container.appendChild(psbtArea);

  container.appendChild(el("h3", {}, "Seed"));
  const modeRow = el("div", { class: "row" });
  const modeSel = el("select") as HTMLSelectElement;
  modeSel.appendChild(el("option", { value: "mnemonic" }, "BIP-39 mnemonic (12/24 words)"));
  modeSel.appendChild(el("option", { value: "qseed" }, "@q seed hex bytes"));
  modeRow.appendChild(modeSel);
  container.appendChild(modeRow);

  const seedArea = el("textarea", {
    rows: "3",
    placeholder: "Mnemonic words separated by spaces, or seed hex",
    spellcheck: "false", autocomplete: "off",
  }) as HTMLTextAreaElement;
  container.appendChild(seedArea);

  container.appendChild(el("p", { class: "qr-meta", style: "margin-top:0.5rem;" },
    "Tries BIP-86 derivations m/86'/{0,1}'/0'/{0,1}/0..29 — "
    + "covers mainnet/testnet, receive/change, first 30 indices."));

  const btnRow = el("div", { class: "row" });
  const signBtn = el("button", { class: "btn primary", type: "button" }, "Sign & finalize");
  btnRow.appendChild(signBtn);
  container.appendChild(btnRow);

  container.appendChild(el("h3", {}, "Signed PSBT (base64)"));
  const signedPsbtOut = el("pre", { class: "code", style: "min-height:2.5rem;" });
  container.appendChild(signedPsbtOut);
  const signedPsbtCopyRow = el("div", { class: "row" });
  const copyPsbtBtn = el("button", { class: "btn secondary", type: "button" }, "copy signed PSBT");
  signedPsbtCopyRow.appendChild(copyPsbtBtn);
  container.appendChild(signedPsbtCopyRow);

  container.appendChild(el("h3", {}, "Raw tx hex (ready to broadcast)"));
  container.appendChild(el("p", { class: "qr-meta" },
    "Paste this into Sparrow → Tools → Broadcast Transaction, "
    + "or run `bitcoin-cli sendrawtransaction <hex>` from a node."));
  const rawHexOut = el("pre", { class: "code", style: "min-height:2.5rem;" });
  container.appendChild(rawHexOut);
  const rawRow = el("div", { class: "row" });
  const copyHexBtn = el("button", { class: "btn secondary", type: "button" }, "copy raw tx hex");
  rawRow.appendChild(copyHexBtn);
  container.appendChild(rawRow);

  const summary = el("pre", { class: "qr-meta" });
  container.appendChild(summary);

  signBtn.addEventListener("click", () => {
    signedPsbtOut.textContent = "";
    rawHexOut.textContent = "";
    summary.textContent = "";
    try {
      const psbtBytes = base64.decode(psbtArea.value.trim());
      const tx = Transaction.fromPSBT(psbtBytes);
      const seed = seedForMode(modeSel.value as "mnemonic" | "qseed", seedArea.value);
      const root = HDKey.fromMasterSeed(seed);

      const attempts: string[] = [];
      let signedCount = 0;

      for (let idx = 0; idx < tx.inputsLength; idx++) {
        let signedThis = false;
        for (const path of candidatePaths()) {
          try {
            const node = root.derive(path);
            if (!node.privateKey) continue;
            if (tx.signIdx(node.privateKey, idx)) {
              signedCount++;
              signedThis = true;
              attempts.push(`  input ${idx}: signed via ${path}`);
              break;
            }
          } catch {
            // Wrong key for this input — keep trying
          }
        }
        if (!signedThis) {
          attempts.push(`  input ${idx}: NO KEY FOUND in first 30 paths across both coin types`);
        }
      }

      // Emit signed PSBT regardless of whether all inputs signed.
      const signedPsbtBytes = tx.toPSBT(0);
      signedPsbtOut.textContent = base64.encode(signedPsbtBytes);

      // Attempt to finalize + extract. May fail if not all inputs signed.
      try {
        tx.finalize();
        const raw = tx.extract();
        rawHexOut.textContent = hexCoder.encode(raw);
      } catch (err: any) {
        rawHexOut.textContent = `(can't finalize: ${err.message ?? err})`;
      }

      summary.textContent =
        `inputs: ${tx.inputsLength} · signed: ${signedCount}\n`
        + attempts.join("\n");
    } catch (err: any) {
      signedPsbtOut.textContent = `error: ${err.message ?? err}`;
    }
  });

  copyPsbtBtn.addEventListener("click", async () => {
    await navigator.clipboard.writeText(signedPsbtOut.textContent ?? "");
    copyPsbtBtn.textContent = "copied ✓";
    setTimeout(() => { copyPsbtBtn.textContent = "copy signed PSBT"; }, 1200);
  });
  copyHexBtn.addEventListener("click", async () => {
    await navigator.clipboard.writeText(rawHexOut.textContent ?? "");
    copyHexBtn.textContent = "copied ✓";
    setTimeout(() => { copyHexBtn.textContent = "copy raw tx hex"; }, 1200);
  });

  root.appendChild(container);
}

const app = document.getElementById("app");
if (app) render(app);

export {};
