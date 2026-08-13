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
import type { BuiltStateUpdate, StateUpdateCtx } from "../../ops/types.js";
import type { Snapshot } from "../../spawn/snapshot.js";
import { messagingKeyFromPass } from "../../spawn/mine-c.js";
import { lookupPoint } from "../../oracle/point.js";
import { patpToAtom } from "../../protocol/patp.js";
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
    el("label", { for: "sponsorPatp", style: "display:block;margin-top:0.6rem;" },
      "Sponsor @p (blank = keep the current one)"),
    el("input", { type: "text", name: "sponsorPatp", id: "sponsorPatp", placeholder: "~sampel-palnet" }),
    el("label", { for: "noRoute", style: "display:block;margin-top:0.6rem;" },
      "No-route: clear the sponsor and permit an unroutable result — outbound-only. "
      + "(The comet's fief, if it has one, always carries forward.)"),
    el("input", { type: "checkbox", name: "noRoute", id: "noRoute" }),
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
  // Held across the two handlers so the broadcast can prove it is sending
  // the transaction the build produced. A rebuild replaces it, so a stale
  // signed PSBT pasted after a rebuild is caught rather than broadcast.
  let built: BuiltStateUpdate | null = null;

  form.addEventListener("submit", (ev) => {
    ev.preventDefault();
    buildStatus.innerHTML = "";
    try {
      const data = new FormData(form);
      const newKeyHex = String(data.get("newKeyHex") ?? "").trim().replace(/^0x/, "");
      if (!/^[0-9a-fA-F]+$/.test(newKeyHex)) throw new Error("new key must be a hex atom");
      // This field wants `cry.pub` — a bare 32-byte messaging key — and it
      // used to accept anything hex-shaped. The desktop's --new-pass-hex
      // wants the WHOLE PASS and reads it little-endian, so the same string
      // means two different things in the two tools, and the value users
      // actually have to hand is the whole pass: Causeway writes
      // `pass_atom_hex` with Python's hex(), big-endian, and this field is
      // labelled "hex atom" with a 0x… placeholder.
      //
      // Getting it wrong is not a build error, it is a burnt identity.
      // Nothing downstream checks the width — measured: the build succeeds
      // with an 862-bit key, a 4096-bit key, and zero — and the comet then
      // fails `pass-key` in +verify-lc, which is FRAUD class: a permanent,
      // never-expiring, one-way snub on every transport. Worse, the fix is
      // not another rekey through this tool, because it would rebuild the
      // prior snapshot from the CORRECT cry and the merkle root would not
      // match the UTXO.
      const newKeyAtom = BigInt("0x" + newKeyHex);
      if (newKeyAtom >> 256n !== 0n) {
        throw new Error(
          (newKeyAtom & 0xffn) === 0x63n
            ? "that looks like a whole suite-C pass, not cry.pub — this field "
              + "wants the 32-byte messaging key alone. (The desktop's "
              + "--new-pass-hex takes the whole pass; this one does not.)"
            : "messaging key must be at most 32 bytes (64 hex characters)",
        );
      }
      const breach = data.has("breach");
      const noRoute = data.has("noRoute");
      const sponsorText = String(data.get("sponsorPatp") ?? "").trim();
      // `undefined` = carry the current sponsor forward; an explicit @p sets
      // it. Clearing a sponsor is only reachable via the no-route tick.
      let sponsor: bigint | null | undefined =
        sponsorText ? patpToAtom(sponsorText) : undefined;
      if (!sponsorText && noRoute) sponsor = null;

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
      //
      // NB: `point.net.sponsor` is the Jael projection, where an ABSENT
      // on-chain sponsor is rendered as self-sponsorship (has=%.n, who=the
      // ship itself). Copying `.who` unconditionally would silently turn "no
      // sponsor" into "sponsored by itself" and defeat the routability guard
      // below, so honour `.has`.
      //
      // NB2: the fief must come from the point too, NOT be hardcoded to null.
      // It rides the snapshot into the state commitment, so dropping it makes
      // the INPUT merkle root (over currentSnapshot) wrong — the PSBT's
      // PSBT_IN_TAP_MERKLE_ROOT no longer matches the UTXO being spent and the
      // signer cannot produce a valid key-path signature — and re-commits a
      // state-key with the comet's fief silently erased. `ops.rekey` carries
      // `currentSnapshot.fief` forward verbatim.
      //
      // NB3: `key` is the MESSAGING HALF of the pass (cry.pub, 32 bytes), not
      // the pass. `point.net.pass` is the whole ~108-byte suite-C pass, so it
      // must be split — the same hazard as NB2 and with the same consequence:
      // a snapshot whose key is a pass hashes to a `c` no verifier computes,
      // so the input merkle root is wrong and the output commits a Q nothing
      // can reconstruct. The Hoon verifier compares precisely
      // `cry.pub` of the carried pass against `key.snap` (+verify-lc in
      // lib/self-attestation.hoon), and the desktop tool does the same split
      // in messaging_key_from_pass().
      const currentSnapshot: Snapshot = {
        life: point.net.life,
        rift: point.net.rift,
        key: messagingKeyFromPass(point.net.pass),
        sponsor: point.net.sponsor.has ? point.net.sponsor.who : null,
        fief: point.net.fief,
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
        noRoute,
      };

      const b = ops.rekey.build(
        {
          newKey: newKeyAtom,
          breach,
          ...(sponsor === undefined ? {} : { sponsor }),
        },
        ctx,
      );
      built = b;

      psbtCard.style.display = "";
      const stream = encodePsbtUR(b.psbt);
      animateUR(qrLeft, stream, { fps: 4, size: 360 });
      qrCopy.innerHTML = "";
      qrCopy.appendChild(copyButton(() => base64.encode(b.psbt), "copy base64"));
      buildStatus.appendChild(banner("ok",
        `built rekey → life ${b.newSnapshot.life}, rift ${b.newSnapshot.rift} `
        + `(predicted txid ${b.txidHex.slice(0, 8)}…)`));
    } catch (err: any) {
      buildStatus.innerHTML = "";
      buildStatus.appendChild(banner("err", `build error: ${err.message ?? err}`));
    }
  });

  bcastBtn.addEventListener("click", async () => {
    status.innerHTML = "";
    try {
      if (!ctx || !built) throw new Error("build the PSBT first");
      if (!signedPsbt) throw new Error("need the signed rekey PSBT");
      // Captured before the await: these are mutable outer bindings, so
      // the narrowing above does not survive the suspension point — and a
      // rebuild during the broadcast really could swap them.
      const bcastCtx = ctx;
      const bcastBuilt = built;
      status.appendChild(banner("warn", "broadcasting state update…"));
      const result = await ops.rekey.broadcast(signedPsbt, bcastCtx, bcastBuilt);
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
