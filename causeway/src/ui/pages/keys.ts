import { el, clearAndAppend, banner, kvList } from "../components.js";
import { go, getSession, ensureSession } from "../state.js";
import {
  parseKeySource, defaultAccountPath, formatAccountPath,
} from "../../keys/xpub.js";
import { discoverUtxos } from "../../chain/discover.js";
import { atomToPatp } from "../../protocol/patp.js";
import { scanXpubFromCamera } from "../../signing/qr-xpub.js";
import QrScanner from "qr-scanner";

// Keys page supports a ?then= query for post-import routing:
//   #/keys           → go to dashboard after import
//   #/keys?then=spawn → go to spawn page after import
function parseThenParam(): "dash" | "spawn" {
  const raw = window.location.hash.split("?")[1] ?? "";
  const params = new URLSearchParams(raw);
  return params.get("then") === "spawn" ? "spawn" : "dash";
}

export async function renderKeys(root: HTMLElement): Promise<void> {
  const s = getSession();
  const then = parseThenParam();
  // Manage-existing flow requires a @p-backed session; if absent, bounce home.
  if (!s && then !== "spawn") { go("#/"); return; }

  const card = el("section", { class: "card" });
  const heading = s && s.patpAtom !== undefined
    ? `Import your key — ${atomToPatp(s.patpAtom)}`
    : then === "spawn" ? "Import your key (for new comet)" : "Import your key";
  card.append(
    el("h1", {}, heading),
    el("p", { class: "lead" },
      "Point Causeway at your hardware wallet's BIP-86 taproot account. "
      + "Most wallets can display this as a QR code — scan it directly, or paste "
      + "the output descriptor / xpub below."),
  );

  // -- Network --
  const netLabel = el("label", {}, "Network");
  const netSel = el("select");
  netSel.append(
    el("option", { value: "main", selected: "true" }, "Bitcoin mainnet"),
    el("option", { value: "testnet" }, "Testnet"),
  );

  // -- QR scan pane --
  const scanRow = el("div", { class: "row", style: "margin-top:1rem;" });
  const scanBtn = el("button", { class: "btn primary", type: "button" }, "Scan QR from HW wallet");
  const pasteBtn = el("button", { class: "btn secondary", type: "button" }, "Or paste manually");
  scanRow.append(scanBtn, pasteBtn);

  const scanArea = el("section", { class: "card", style: "display:none;" });
  const scanVideo = el("div");
  const scanMeta = el("div", { class: "qr-meta" }, "");
  const scanCancel = el("button", { class: "btn secondary", type: "button" }, "cancel");
  const scanHelp = el("p", { class: "qr-meta" },
    "Export your BIP-86 account from your wallet: "
    + "Passport → Account Info; Keystone → Menu → Watch-Only Wallet; "
    + "Coldcard → Advanced → Export → Generic JSON. Most also expose an "
    + "animated UR crypto-account / crypto-hdkey.");
  scanArea.append(el("h3", {}, "Point your camera at the QR"), scanVideo, scanMeta, scanCancel, scanHelp);

  // -- Paste pane (hidden until "Paste manually" or scan completes) --
  const pasteArea = el("section", { class: "card", style: "display:none;" });
  pasteArea.appendChild(el("h3", {}, "Descriptor / xpub"));
  const descArea = el("textarea", {
    rows: "4",
    placeholder: "tr([a0b1c2d3/86'/0'/0']xpub6...) — from your HW wallet export",
    spellcheck: "false", autocomplete: "off",
  }) as HTMLTextAreaElement;
  pasteArea.appendChild(descArea);

  const advToggle = el("details");
  const advSummary = el("summary", { style: "cursor:pointer;color:var(--muted);margin:1rem 0 0.5rem;" },
    "Advanced: origin fields for bare xpubs");
  const fpLabel = el("label", {}, "Master fingerprint (required for bare xpub imports — 8 hex chars)");
  const fpInp = el("input", {
    type: "text",
    placeholder: "a0b1c2d3",
    value: "",  // empty by default — users MUST enter their real fingerprint
    pattern: "[0-9a-fA-F]{8}",
    maxlength: "8",
  }) as HTMLInputElement;
  const fpHelp = el("div", { class: "qr-meta", style: "margin:0.25rem 0 0.75rem;color:var(--muted);" },
    "Not the placeholder 00000000 — Sparrow, BlueWallet, Coldcard etc. will refuse to sign "
    + "if the PSBT's fingerprint doesn't match your wallet's real seed fingerprint. "
    + "Sparrow shows it at the top of the wallet tab. BlueWallet: wallet settings → Show xpub.") as HTMLDivElement;
  const pathLabel = el("label", {}, "Account derivation path (only if pasting a bare xpub)");
  const pathInp = el("input", { type: "text", value: formatAccountPath(defaultAccountPath("main")) }) as HTMLInputElement;
  advToggle.append(advSummary, fpLabel, fpInp, fpHelp, pathLabel, pathInp);
  pasteArea.appendChild(advToggle);

  netSel.addEventListener("change", () => {
    pathInp.value = formatAccountPath(defaultAccountPath(netSel.value as "main" | "testnet"));
  });

  const importBtn = el("button", { class: "btn primary", type: "button", style: "margin-top:1rem;" },
    "Import & discover UTXOs");
  pasteArea.appendChild(importBtn);

  // -- Assemble --
  card.append(netLabel, netSel, scanRow);
  const status = el("div");
  const result = el("div");
  clearAndAppend(root, card, scanArea, pasteArea, status, result);

  // Disable scan button when no camera is available.
  QrScanner.hasCamera().then((has) => {
    if (!has) {
      scanBtn.setAttribute("disabled", "true");
      scanBtn.title = "no camera detected — use 'paste manually'";
      pasteArea.style.display = "";
    }
  }).catch(() => {});

  let activeScanStop: (() => void) | null = null;

  scanBtn.addEventListener("click", async () => {
    status.innerHTML = "";
    scanArea.style.display = "";
    pasteArea.style.display = "none";
    scanMeta.textContent = "requesting camera…";
    try {
      const handle = await scanXpubFromCamera(scanVideo, (p) => {
        scanMeta.textContent = p.expected > 1
          ? `frames ${p.received} / ${p.expected}`
          : "scanning…";
      });
      activeScanStop = () => handle.stop();
      const scanned = await handle.result;
      activeScanStop = null;
      scanArea.style.display = "none";
      // Populate the paste area fields so the user can verify what was read.
      pasteArea.style.display = "";
      descArea.value = scanned.text;
      if (scanned.masterFingerprint) fpInp.value = scanned.masterFingerprint;
      if (scanned.accountPath) pathInp.value = scanned.accountPath;
      status.appendChild(banner("ok",
        `got ${scanned.kind}: ${scanned.text.slice(0, 44)}…`));
      // Auto-import.
      importBtn.click();
    } catch (err: any) {
      scanArea.style.display = "none";
      status.innerHTML = "";
      status.appendChild(banner("err", `scan error: ${err.message ?? err}`));
    }
  });

  pasteBtn.addEventListener("click", () => {
    scanArea.style.display = "none";
    pasteArea.style.display = "";
    if (activeScanStop) { activeScanStop(); activeScanStop = null; }
  });

  scanCancel.addEventListener("click", () => {
    if (activeScanStop) { activeScanStop(); activeScanStop = null; }
    scanArea.style.display = "none";
  });

  importBtn.addEventListener("click", async () => {
    status.innerHTML = ""; result.innerHTML = "";
    status.appendChild(banner("warn", "parsing descriptor & discovering UTXOs…"));
    try {
      const descText = descArea.value.trim();
      const isDescriptor = descText.startsWith("tr(");
      const fp = fpInp.value.trim();
      // If the user pasted a bare xpub, the fingerprint field must be the
      // real master fingerprint — Sparrow, BlueWallet, Coldcard etc. all
      // reject PSBTs whose tapBip32Derivation fingerprint is a placeholder
      // with "No open wallets can sign" (the match against their own
      // wallet's fingerprint fails).
      if (!isDescriptor && (!fp || fp === "00000000" || !/^[0-9a-fA-F]{8}$/.test(fp))) {
        throw new Error(
          "Master fingerprint is required for bare xpub imports and must be "
          + "your wallet's real 8-hex-char fingerprint (not 00000000). "
          + "Find it in Sparrow: top of the wallet tab. In BlueWallet: wallet "
          + "settings → Show xpub → fingerprint is shown above the xpub. "
          + "Or paste a full output descriptor — tr([FP/86'/0'/0']xpub...) "
          + "— which carries the fingerprint automatically.",
        );
      }

      const session = await ensureSession();
      const src = parseKeySource(descText, {
        masterFingerprint: fp || "00000000",
        accountPath: pathInp.value.trim() || formatAccountPath(defaultAccountPath(netSel.value as any)),
        network: netSel.value as "main" | "testnet",
      });
      session.keys = src;

      const discovery = await discoverUtxos({
        src, mp: session.mp, sontMap: session.snapshot.sontMap, maxIndex: 10,
      });
      session.discovery = discovery;

      status.innerHTML = "";
      status.appendChild(banner("ok",
        then === "spawn" ? "imported — preparing spawn" : "imported — opening dashboard"));
      result.appendChild(kvList([
        ["Addresses scanned", `${discovery.addresses.length}`],
        ["Funding UTXOs", `${discovery.fundingUtxos.length} (${discovery.totalFundingSats} sats)`],
        ["Inscription UTXOs (protected)", `${discovery.inscriptionUtxos.length}`],
      ]));

      setTimeout(() => go(then === "spawn" ? "#/spawn" : "#/dash"), 600);
    } catch (err: any) {
      status.innerHTML = "";
      status.appendChild(banner("err", `error: ${err.message ?? err}`));
    }
  });
}

