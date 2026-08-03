// Management op — kelvin-9 rekey (state update).
//
// Under the OP_RETURN revision the only surviving on-chain management op is a
// state update: spend the sat-carrying UTXO key-path and re-commit a new
// snapshot in output 0. Sponsorship / escape / adopt / reject / detach / fief /
// set-mang are off-chain now (spec §8), so this page handles only `rekey`
// (rotate the messaging key, optional breach) as a SINGLE PSBT — no
// commit/reveal pair.

import { base64 } from "@scure/base";
import { el, clearAndAppend, banner, copyButton, kvList } from "../components.js";
import { go, getSession } from "../state.js";
import { atomToMnemonym, abridgeMnemonym } from "../../protocol/mnemonym.js";
import { ops } from "../../ops/index.js";
import type { StateUpdateCtx } from "../../ops/types.js";
import type { Snapshot } from "../../spawn/snapshot.js";
import { lookupPoint } from "../../oracle/point.js";
import { findKeyForScript } from "../../keys/xpub.js";
import { animateUR } from "../../signing/qr-render.js";
import { encodePsbtUR } from "../../signing/qr-ur.js";
import { scanPsbtFromCamera } from "../../signing/qr-scan.js";
import QrScanner from "qr-scanner";

// Build a signed-PSBT input block: scan-with-camera (if available) + paste
// fallback. Writes the accepted PSBT bytes into `setState`.
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
  if (opName !== "rekey") { root.textContent = `unknown op: ${opName}`; return; }
  if (!s.keys) { go("#/keys"); return; }

  const patpAtom = s.patpAtom;
  const auth = s.auth;
  const keys = s.keys;

  const card = el("section", { class: "card" });
  card.append(
    el("h1", {}, "rekey"),
    el("p", {},
      `Rotate the messaging key for ${abridgeMnemonym(atomToMnemonym(patpAtom))} — a `
      + `single state-update transaction that re-commits a new snapshot in the `
      + `sat-carrying output. No commit/reveal pair.`),
  );

  const form = el("form", { id: "opForm" });
  form.append(
    el("label", { for: "newKeyHex" }, "New messaging key (cry.pub, hex atom)"),
    el("input", { type: "text", name: "newKeyHex", id: "newKeyHex", placeholder: "0x…" }),
    el("label", { for: "breach", style: "display:block;margin-top:0.6rem;" }, "Breach (rift increment)"),
    el("input", { type: "checkbox", name: "breach", id: "breach" }),
  );
  const buildBtn = el("button", { class: "btn primary", type: "submit", style: "margin-top:0.8rem;" }, "Build PSBT");
  form.appendChild(buildBtn);
  card.appendChild(form);

  const buildStatus = el("div");
  card.appendChild(buildStatus);

  const psbtCard = el("section", { class: "card", style: "display:none;" });
  psbtCard.appendChild(el("h2", {}, "Sign the state update"));
  const qrPane = el("div", { class: "qr-pane" });
  const qrLeft = el("div");
  const qrRight = el("div");
  qrRight.appendChild(el("h3", {}, "Rekey PSBT"));
  qrRight.appendChild(el("p", {},
    "Scan the animated QR on your wallet (or copy the base64). Sign, then bring "
    + "the signed PSBT back."));
  const qrCopy = el("div");
  qrRight.appendChild(qrCopy);
  qrPane.append(qrLeft, qrRight);
  psbtCard.appendChild(qrPane);

  let signedPsbt: Uint8Array | null = null;
  psbtCard.appendChild(signedPsbtInput("Signed rekey PSBT", (b) => { signedPsbt = b; }));

  const bcastBtn = el("button", { class: "btn primary", type: "button" }, "Verify & broadcast");
  const bcastRow = el("div", { class: "row" });
  bcastRow.appendChild(bcastBtn);
  psbtCard.appendChild(bcastRow);

  const status = el("div");
  psbtCard.appendChild(status);
  const resultCard = el("section", { class: "card", style: "display:none;" });

  clearAndAppend(root, card, psbtCard, resultCard);

  let ctx: StateUpdateCtx | null = null;

  form.addEventListener("submit", (ev) => {
    ev.preventDefault();
    buildStatus.innerHTML = "";
    try {
      const data = new FormData(form);
      const newKeyHex = String(data.get("newKeyHex") ?? "").trim().replace(/^0x/, "");
      if (!/^[0-9a-fA-F]+$/.test(newKeyHex)) throw new Error("new key must be a hex atom");
      const breach = data.has("breach");

      const point = lookupPoint(s.snapshot, patpAtom);
      if (!point) throw new Error("point not found in the current snapshot");

      // The key that controls the current sat-carrying UTXO. A kelvin-9 output
      // is P2TR(Q) tweaked by its state leaf; the internal key is the same
      // BIP-86 child that owned the funding UTXO at spawn.
      const owner = findKeyForScript(keys, auth.utxo.scriptPubKey);
      if (!owner) {
        throw new Error(
          "couldn't find the key controlling your ownership UTXO in the first 40 "
          + "derivations of either chain — is this the wallet that holds the @p?",
        );
      }
      const internalKey33 = new Uint8Array(33);
      internalKey33[0] = 0x02;
      internalKey33.set(owner.internalKey, 1);

      // Current committed snapshot, mapped from the oracle point. (The kelvin-9
      // state oracle is not yet wired; life/rift/sponsor carry over, and the
      // current messaging key comes from the point's pass.)
      const currentSnapshot: Snapshot = {
        life: point.net.life,
        rift: point.net.rift,
        key: point.net.pass,
        sponsor: point.net.sponsor.who,
        fief: null,
      };

      const txidBytes = new Uint8Array(32);
      for (let i = 0; i < 32; i++) {
        txidBytes[i] = parseInt(auth.utxo.txid.slice(i * 2, i * 2 + 2), 16);
      }

      ctx = {
        current: {
          txid: txidBytes,
          vout: auth.utxo.vout,
          value: BigInt(auth.utxo.value),
          scriptPubKey: auth.utxo.scriptPubKey,
        },
        ownerKey: owner,
        internalKey33,
        currentSnapshot,
        feeRate: 2,
        mp: s.mp,
      };

      const built = ops.rekey.build({ newKey: BigInt("0x" + newKeyHex), breach }, ctx);

      psbtCard.style.display = "";
      const stream = encodePsbtUR(built.psbt);
      animateUR(qrLeft, stream, { fps: 4, size: 360 });
      qrCopy.innerHTML = "";
      qrCopy.appendChild(copyButton(() => base64.encode(built.psbt), "copy base64"));
      buildStatus.appendChild(banner("ok",
        `built rekey → life ${built.newSnapshot.life}, rift ${built.newSnapshot.rift} `
        + `(predicted txid ${built.txidHex.slice(0, 8)}…)`));
    } catch (err: any) {
      buildStatus.innerHTML = "";
      buildStatus.appendChild(banner("err", `build error: ${err.message ?? err}`));
    }
  });

  bcastBtn.addEventListener("click", async () => {
    status.innerHTML = "";
    try {
      if (!ctx) throw new Error("build the PSBT first");
      if (!signedPsbt) throw new Error("need the signed rekey PSBT");
      status.appendChild(banner("warn", "broadcasting state update…"));
      const result = await ops.rekey.broadcast(signedPsbt, ctx);
      resultCard.style.display = "";
      resultCard.innerHTML = "";
      resultCard.appendChild(el("h2", {}, "Broadcast ✓"));
      resultCard.appendChild(kvList(result.txids.map((t) => ["txid", t] as [string, string])));
      status.innerHTML = "";
    } catch (err: any) {
      status.innerHTML = "";
      status.appendChild(banner("err", `broadcast error: ${err.message ?? err}`));
    }
  });
}
