// Spawn — mine a ~daplyd comet, assemble commit+reveal PSBTs, show each as
// an animated QR for the user's wallet to sign and broadcast. Causeway
// predicts both txids (segwit txids are deterministic from the unsigned tx)
// and polls mempool.space automatically. Once the reveal confirms, the boot
// command appears.
//
// State is persisted to localStorage after assembly, so an accidental refresh
// between broadcasting the commit and the reveal doesn't orphan sats.

import { el, clearAndAppend, banner, kvList, copyButton } from "../components.js";
import { go, getSession } from "../state.js";
import { miner } from "../../spawn/miner.js";
import { formatBootCommand } from "../../spawn/boot-cmd.js";
import { assembleSpawn } from "../../spawn/assemble.js";
import { buildTweakBytes } from "../../spawn/tweak.js";
import { atomToPatp } from "../../protocol/patp.js";
import type { DiscoveredUtxo } from "../../chain/discover.js";
import { encodePsbtUR } from "../../signing/qr-ur.js";
import { animateUR } from "../../signing/qr-render.js";
import { requestEscapeSig, ESCAPE_SPONSOR } from "../../chain/sponsor.js";
import {
  savePendingSpawn, loadPendingSpawn, clearPendingSpawn, updatePhase,
  b64Encode, b64Decode, bytesToHex, hexToBytes,
  type PersistedSpawn,
} from "../../spawn/persist.js";
import { formatAccountPath } from "../../keys/xpub.js";

const REVEAL_CONFIRMATIONS = 2;
const POLL_INTERVAL_MS = 15_000;

// Txids are stored display-order throughout Causeway; hex-encode directly.
function bytesToDisplayHex(displayBytes: Uint8Array): string {
  return Array.from(displayBytes, (b) => b.toString(16).padStart(2, "0")).join("");
}
function shortTxid(hex: string): string { return `${hex.slice(0, 8)}…${hex.slice(-6)}`; }

// Trigger a file download for a Uint8Array with a given filename.
function downloadBytes(bytes: Uint8Array, filename: string, mime = "application/octet-stream"): void {
  // Slice to detach from any shared buffer so Blob's TS types are happy.
  const blob = new Blob([bytes.slice().buffer as ArrayBuffer], { type: mime });
  const url = URL.createObjectURL(blob);
  const a = document.createElement("a");
  a.href = url;
  a.download = filename;
  document.body.appendChild(a);
  a.click();
  setTimeout(() => {
    document.body.removeChild(a);
    URL.revokeObjectURL(url);
  }, 100);
}

function downloadButton(getBytes: () => Uint8Array, filename: string): HTMLButtonElement {
  const btn = el("button", { class: "btn secondary", type: "button" }, `download ${filename}`);
  btn.addEventListener("click", () => downloadBytes(getBytes(), filename));
  return btn;
}

// Copy describing which wallets the user should actually use for signing.
const SIGNER_ADVICE_HTML = `
  <strong>Signing requirements.</strong>
  The commit tx is a standard BIP-86 key-path spend — any modern taproot wallet
  should handle it. The reveal tx is a taproot <em>script-path</em> spend, which
  is only reliably supported in a few places today. In order of recommendation:
  <ul style="margin-top:0.5rem;padding-left:1.2rem;">
    <li><a href="https://sparrowwallet.com" target="_blank" rel="noopener">Sparrow Wallet</a> (desktop, free, open-source) — the reference signer for
        both steps. Load each PSBT via <em>File → Load Transaction → From Text</em>
        (paste the base64) or <em>From QR Code</em> (scan Causeway's QR with your webcam).</li>
    <li><strong>Coldcard Q</strong> with EDGE firmware — the only hardware wallet with
        confirmed script-path taproot signing, via animated QR.</li>
    <li><strong>Bitcoin Core</strong> via <code>walletprocesspsbt</code> / <code>finalizepsbt</code> — if you're running a node.</li>
  </ul>
  Wallets that <em>will not</em> work end-to-end today: BlueWallet, Electrum mobile,
  Phoenix, Muun, Ledger, Trezor, Jade.
`.trim();

export function renderSpawn(root: HTMLElement): void {
  const session = getSession();

  // Check for a pending spawn before demanding a full session — a returning
  // user whose tab just refreshed may hit this page first.
  const pending = loadPendingSpawn();

  if (!session) {
    if (pending) {
      renderResumeOnly(root, pending);
      return;
    }
    go("#/"); return;
  }
  if (!session.keys || !session.discovery) {
    if (pending) {
      renderResumeOnly(root, pending);
      return;
    }
    go("#/keys?then=spawn"); return;
  }

  const s = session;
  const discovery = session.discovery;
  const keys = session.keys;
  const confirmedFunding = discovery.fundingUtxos.filter((u) => u.confirmed);

  // ---- Page chrome ----
  const resumeCard = el("section", { class: "card", style: "display:none;" });
  const intro = el("section", { class: "card" });
  const status = el("div");
  const mineCard = el("section", { class: "card", style: "display:none;" });
  const commitCard = el("section", { class: "card", style: "display:none;" });
  const revealCard = el("section", { class: "card", style: "display:none;" });
  const bootCard = el("section", { class: "card", style: "display:none;" });

  intro.append(
    el("h1", {}, "Spawn a comet"),
    el("p", { class: "lead" },
      "Causeway mines a comet @p under the ~daplyd star, assembles commit + "
      + "reveal transactions, and hands each to your wallet as a QR or "
      + "downloadable PSBT file. We watch mempool.space for the predicted "
      + "txids; once the reveal confirms, the boot command appears."),
  );

  // Signer advice card — shown up front so users pick the right tool before investing sats.
  const advice = el("section", { class: "card" });
  advice.innerHTML = SIGNER_ADVICE_HTML;

  if (confirmedFunding.length === 0) {
    // A very common way to land here with an empty UTXO set: the user already
    // broadcast a commit tx in a previous session, which spent their funding
    // UTXO. mempool.space no longer lists it, so discovery comes back empty.
    // In that case a pending spawn needs to be resumable — so we add all the
    // placeholder cards to the DOM (hidden), identical to the main path, and
    // let the Resume button populate them.
    const reason = pending
      ? "Your funding UTXO isn't in the mempool's UTXO set right now — most "
        + "likely because you already broadcast the commit tx for your pending "
        + "spawn. Click Resume above to continue where you left off."
      : "No confirmed funding UTXOs at your account's first 20 addresses. "
        + "Send at least ~1,000 sats to one of your BIP-86 addresses and come back.";
    intro.appendChild(banner("warn", reason));
    clearAndAppend(root, resumeCard, intro, advice, status, mineCard, commitCard, revealCard, bootCard);
    if (pending) renderResumeCard(resumeCard, pending);
    return;
  }

  intro.appendChild(el("h2", {}, "Precommit UTXO"));
  intro.appendChild(el("label", {}, "Your confirmed UTXOs (smallest-first)"));
  const pickSel = el("select") as HTMLSelectElement;
  for (const [i, u] of confirmedFunding.entries()) {
    const displayHex = bytesToDisplayHex(u.txid);
    pickSel.appendChild(el("option", { value: `${i}` },
      `${u.value} sats · ${shortTxid(displayHex)}:${u.vout} · ${u.address.slice(0, 12)}…`));
  }
  intro.appendChild(pickSel);

  const startBtn = el("button", {
    class: "btn primary", type: "button", style: "margin-top:1rem;",
  }, "Mine & build transactions");
  intro.appendChild(startBtn);

  clearAndAppend(root, resumeCard, intro, advice, status, mineCard, commitCard, revealCard, bootCard);
  if (pending) renderResumeCard(resumeCard, pending);

  let pollTimer: ReturnType<typeof setInterval> | null = null;

  async function runFlow(picked: DiscoveredUtxo): Promise<void> {
    status.innerHTML = "";
    mineCard.style.display = "none";
    commitCard.style.display = "none";
    revealCard.style.display = "none";
    bootCard.style.display = "none";
    if (pollTimer) { clearInterval(pollTimer); pollTimer = null; }

    const txidDisplay = bytesToDisplayHex(picked.txid);
    const tweak = buildTweakBytes({ txidHex: txidDisplay, vout: picked.vout, off: 0 });

    status.appendChild(banner("warn", "mining under ~daplyd — ~65k iterations…"));
    const progressLine = el("div", { class: "qr-meta", style: "margin-top:0.3rem;" }, "0 tries");
    status.appendChild(progressLine);

    const t0 = performance.now();
    let mined: Awaited<ReturnType<typeof miner.mine>>;
    try {
      mined = await miner.mine({
        tweakExpr: tweak,
        onProgress: (tries) => { progressLine.textContent = `${tries.toLocaleString()} tries…`; },
      });
    } catch (err: any) {
      status.innerHTML = "";
      status.appendChild(banner("err", `mine error: ${err.message ?? err}`));
      return;
    }
    const elapsed = Math.round(performance.now() - t0);
    status.innerHTML = "";
    status.appendChild(banner("ok",
      `mined in ${(elapsed / 1000).toFixed(1)}s (${mined.tries.toLocaleString()} tries)`));

    const cometPatp = atomToPatp(mined.comet);

    status.appendChild(banner("warn",
      `requesting escape-sig from ${ESCAPE_SPONSOR.slice(0, 14)}…`));
    let sponsorSig;
    try {
      sponsorSig = await requestEscapeSig(cometPatp);
    } catch (err: any) {
      status.innerHTML = "";
      status.appendChild(banner("err",
        `sponsor-signer error: ${err.message ?? err}. `
        + `A fresh comet without a sponsor won't route — aborting.`));
      return;
    }

    const assembled = assembleSpawn({
      mined, picked, keys, sponsorSig: sponsorSig.sig, feeRate: 2,
    });

    // ---- Persist everything we'd need to resume from a page refresh ----
    const persisted: PersistedSpawn = {
      version: 1,
      createdAt: Date.now(),
      network: keys.network,
      descriptor: "", // may be empty if user pasted bare xpub; we'd need to thread the raw input through — best-effort
      accountPath: formatAccountPath(keys.accountPath),
      masterFingerprint: keys.masterFingerprint.toString(16).padStart(8, "0"),
      picked: {
        txidHex: bytesToDisplayHex(picked.txid),
        vout: picked.vout,
        value: picked.value.toString(),
        scriptPubKeyHex: bytesToHex(picked.scriptPubKey),
        change: picked.change,
        index: picked.index,
        address: picked.address,
      },
      mined: {
        comet: mined.comet.toString(),
        pass: mined.pass.toString(),
        feedHex: bytesToHex(mined.feed),
        tries: mined.tries,
      },
      sponsor: {
        sig: sponsorSig.sig.toString(),
        height: sponsorSig.height,
      },
      commitPsbtB64: b64Encode(assembled.commitPsbt),
      revealPsbtB64: b64Encode(assembled.revealPsbt),
      commitTxidHex: assembled.commitTxidHex,
      revealTxidHex: assembled.revealTxidHex,
      phase: "assembled",
    };
    savePendingSpawn(persisted);

    renderMineCard(mineCard, {
      cometPatp,
      pickedSummary: `${shortTxid(bytesToDisplayHex(picked.txid))}:${picked.vout} (${picked.value} sats)`,
      tries: mined.tries,
      sponsorHeight: sponsorSig.height,
      commitTxidHex: assembled.commitTxidHex,
      revealTxidHex: assembled.revealTxidHex,
    });

    renderCommitCard(
      commitCard, assembled.commitPsbt, assembled.commitTxidHex,
      assembled.revealPsbt,
    );

    await pollForTx(assembled.commitTxidHex,
      commitCard.querySelector<HTMLElement>(".qr-meta")!,
      { minConfs: 0 });
    commitCard.querySelector<HTMLElement>(".qr-meta")!.textContent = "commit seen in mempool ✓";
    updatePhase("commit-broadcast");

    renderRevealCard(revealCard, assembled.revealPsbt, assembled.revealTxidHex);

    await pollForTx(assembled.revealTxidHex,
      revealCard.querySelector<HTMLElement>(".qr-meta")!,
      { minConfs: REVEAL_CONFIRMATIONS });
    updatePhase("reveal-confirmed");

    renderBootCard(bootCard, cometPatp, mined.feed);

    // Success terminal — we can clear persistence now.
    clearPendingSpawn();
  }

  async function pollForTx(
    txidHex: string, metaEl: HTMLElement, opts: { minConfs: number },
  ): Promise<void> {
    return new Promise((resolve) => {
      const tick = async (): Promise<void> => {
        try {
          const tx = await s.mp.tx(txidHex).catch(() => null);
          if (!tx) {
            metaEl.textContent = `waiting for ${shortTxid(txidHex)} in mempool…`;
            return;
          }
          if (!tx.status.confirmed) {
            metaEl.textContent = `in mempool (0 confs) · ${shortTxid(txidHex)}`;
            if (opts.minConfs === 0) {
              clearInterval(pollTimer!); pollTimer = null;
              resolve();
            }
            return;
          }
          const tip = await s.mp.tipHeight();
          const confs = tip - (tx.status.block_height ?? tip) + 1;
          metaEl.textContent = `${confs} / ${opts.minConfs} confirmations`;
          if (confs >= opts.minConfs) {
            clearInterval(pollTimer!); pollTimer = null;
            resolve();
          }
        } catch (err: any) {
          metaEl.textContent = `poll error: ${err.message ?? err} (retrying)`;
        }
      };
      tick();
      pollTimer = setInterval(tick, POLL_INTERVAL_MS);
    });
  }

  startBtn.addEventListener("click", () => {
    const idx = Number(pickSel.value);
    const picked = confirmedFunding[idx];
    if (!picked) { status.appendChild(banner("err", "no UTXO selected")); return; }
    runFlow(picked).catch((err) => {
      status.appendChild(banner("err", `unexpected: ${err.message ?? err}`));
    });
  });

  // ---- Resume-from-pending card ---------------------------------------

  function renderResumeCard(el_: HTMLElement, p: PersistedSpawn): void {
    el_.innerHTML = "";
    el_.style.display = "";
    const cometName = atomToPatp(BigInt(p.mined.comet));
    const ageMin = Math.round((Date.now() - p.createdAt) / 60_000);
    el_.append(
      el("h2", {}, "Pending spawn in progress"),
      el("p", {},
        `There's an unfinished spawn for ${cometName} (started ${ageMin} min ago, `
        + `phase: ${p.phase}). Resume it or discard.`),
      kvList([
        ["Comet", cometName],
        ["Precommit UTXO", `${shortTxid(p.picked.txidHex)}:${p.picked.vout}`],
        ["Commit txid", p.commitTxidHex],
        ["Reveal txid", p.revealTxidHex],
        ["Sponsor sig block", `${p.sponsor.height}`],
      ]),
    );
    const row = el("div", { class: "row" });
    const resume = el("button", { class: "btn primary", type: "button" }, "Resume");
    const discard = el("button", { class: "btn secondary", type: "button" }, "Discard");
    row.append(resume, discard);
    el_.appendChild(row);

    resume.addEventListener("click", () => {
      resumeFromPersisted(p).catch((err) => {
        status.appendChild(banner("err", `resume error: ${err.message ?? err}`));
      });
    });
    discard.addEventListener("click", () => {
      if (!confirm(
        "Discard the pending spawn? If you've already broadcast the commit, "
        + "those sats become unreachable.",
      )) return;
      clearPendingSpawn();
      el_.style.display = "none";
    });
  }

  async function resumeFromPersisted(p: PersistedSpawn): Promise<void> {
    console.log("[causeway] resume clicked, phase=", p.phase,
      " commit=", p.commitTxidHex.slice(0, 12),
      " reveal=", p.revealTxidHex.slice(0, 12));

    // Hide the "start a fresh spawn" UI so the resumed flow is the only
    // thing competing for the user's attention.
    resumeCard.style.display = "none";
    intro.style.display = "none";
    advice.style.display = "none";

    status.innerHTML = "";
    status.appendChild(banner("warn",
      `resuming ${atomToPatp(BigInt(p.mined.comet))} from phase: ${p.phase}…`));
    mineCard.style.display = "none";
    commitCard.style.display = "none";
    revealCard.style.display = "none";
    bootCard.style.display = "none";
    if (pollTimer) { clearInterval(pollTimer); pollTimer = null; }

    let commitPsbt: Uint8Array;
    let revealPsbt: Uint8Array;
    let feed: Uint8Array;
    let cometPatp: string;
    try {
      commitPsbt = b64Decode(p.commitPsbtB64);
      revealPsbt = b64Decode(p.revealPsbtB64);
      feed = hexToBytes(p.mined.feedHex);
      cometPatp = atomToPatp(BigInt(p.mined.comet));
    } catch (err: any) {
      status.innerHTML = "";
      status.appendChild(banner("err",
        `couldn't decode saved spawn data: ${err.message ?? err}. `
        + `You may need to discard this pending spawn and start fresh.`));
      return;
    }

    try {
      renderMineCard(mineCard, {
        cometPatp,
        pickedSummary: `${shortTxid(p.picked.txidHex)}:${p.picked.vout} (${p.picked.value} sats)`,
        tries: p.mined.tries,
        sponsorHeight: p.sponsor.height,
        commitTxidHex: p.commitTxidHex,
        revealTxidHex: p.revealTxidHex,
      });

      renderCommitCard(commitCard, commitPsbt, p.commitTxidHex, revealPsbt);
    } catch (err: any) {
      status.innerHTML = "";
      status.appendChild(banner("err", `render error: ${err.message ?? err}`));
      console.error("[causeway] resume render failed:", err);
      return;
    }

    // Scroll the resumed content into view so "Resume" feels obviously active.
    mineCard.scrollIntoView({ behavior: "smooth", block: "start" });
    status.innerHTML = "";

    await pollForTx(p.commitTxidHex,
      commitCard.querySelector<HTMLElement>(".qr-meta")!,
      { minConfs: 0 });
    commitCard.querySelector<HTMLElement>(".qr-meta")!.textContent = "commit seen in mempool ✓";
    updatePhase("commit-broadcast");

    renderRevealCard(revealCard, revealPsbt, p.revealTxidHex);
    revealCard.scrollIntoView({ behavior: "smooth", block: "start" });

    await pollForTx(p.revealTxidHex,
      revealCard.querySelector<HTMLElement>(".qr-meta")!,
      { minConfs: REVEAL_CONFIRMATIONS });
    updatePhase("reveal-confirmed");

    renderBootCard(bootCard, cometPatp, feed);
    bootCard.scrollIntoView({ behavior: "smooth", block: "start" });
    clearPendingSpawn();
  }
}

// If there's no session at all but a pending spawn exists, offer just the
// resume option — the user may not need to re-authenticate.
function renderResumeOnly(root: HTMLElement, p: PersistedSpawn): void {
  const card = el("section", { class: "card" });
  const cometName = atomToPatp(BigInt(p.mined.comet));
  card.append(
    el("h1", {}, "Pending spawn in progress"),
    el("p", { class: "lead" },
      `There's an unfinished spawn for ${cometName} saved in this browser. `
      + `To continue, first re-import your xpub so Causeway can attach to your `
      + `wallet's address set — then come back here.`),
  );
  const row = el("div", { class: "row" });
  const importBtn = el("a", { class: "btn primary", href: "#/keys?then=spawn" }, "Import xpub");
  const discard = el("button", { class: "btn secondary", type: "button" }, "Discard pending");
  row.append(importBtn, discard);
  card.appendChild(row);
  discard.addEventListener("click", () => {
    if (!confirm(
      "Discard the pending spawn? If you've already broadcast the commit, "
      + "those sats become unreachable.",
    )) return;
    clearPendingSpawn();
    window.location.href = "#/";
  });
  clearAndAppend(root, card);
}

// ---- Card renderers (shared by run-fresh and resume paths) ----

function renderMineCard(
  card: HTMLElement,
  info: {
    cometPatp: string;
    pickedSummary: string;
    tries: number;
    sponsorHeight: number;
    commitTxidHex: string;
    revealTxidHex: string;
  },
): void {
  card.innerHTML = "";
  card.style.display = "";
  card.append(
    el("h2", {}, "Mined ✓"),
    kvList([
      ["Comet", info.cometPatp],
      ["Funding UTXO", info.pickedSummary],
      ["Mining tries", info.tries.toLocaleString()],
      ["Sponsor escape sig", `valid near block ${info.sponsorHeight}`],
      ["Commit txid (predicted)", info.commitTxidHex],
      ["Reveal txid (predicted)", info.revealTxidHex],
    ]),
  );
}

function renderCommitCard(
  card: HTMLElement,
  commitPsbt: Uint8Array,
  commitTxidHex: string,
  revealPsbt: Uint8Array,
): void {
  card.innerHTML = "";
  card.style.display = "";
  const left = el("div");
  const right = el("div");
  right.appendChild(el("h2", {}, "1. Sign & broadcast commit"));
  right.appendChild(el("p", {},
    "Paste this into Sparrow (File → Load Transaction → From Text), or scan "
    + "the QR with a wallet that supports UR PSBT imports. Sign the commit "
    + `input, then broadcast. Causeway is watching for ${shortTxid(commitTxidHex)}.`));

  // Prominent warning — save the reveal PSBT BEFORE broadcasting.
  right.appendChild(el("div", { class: "banner warn" },
    "Download both PSBTs before you broadcast the commit. "
    + "If this page reloads and the reveal can't be rebuilt from the commit "
    + "tx alone, those sats are stranded on-chain."));

  const meta = el("div", { class: "qr-meta" }, "waiting for commit in mempool…");
  right.appendChild(meta);

  const row = el("div", { class: "row" });
  row.appendChild(copyButton(() => toBase64(commitPsbt), "copy PSBT base64"));
  row.appendChild(downloadButton(() => commitPsbt, "commit.psbt"));
  row.appendChild(downloadButton(() => revealPsbt, "reveal.psbt"));
  right.appendChild(row);

  const pane = el("div", { class: "qr-pane" });
  pane.append(left, right);
  card.appendChild(pane);

  const stream = encodePsbtUR(commitPsbt);
  animateUR(left, stream, { fps: 4, size: 340 });
}

function renderRevealCard(
  card: HTMLElement,
  revealPsbt: Uint8Array,
  revealTxidHex: string,
): void {
  card.innerHTML = "";
  card.style.display = "";
  const left = el("div");
  const right = el("div");
  right.appendChild(el("h2", {}, "2. Sign & broadcast reveal"));
  right.appendChild(el("p", {},
    "This is a taproot script-path spend. Use Sparrow Wallet (desktop): "
    + "File → Load Transaction → From Text (paste the base64). Mobile hot "
    + "wallets don't reliably sign script-path spends yet. Once broadcast, "
    + `Causeway polls for ${shortTxid(revealTxidHex)}.`));
  const meta = el("div", { class: "qr-meta" }, "waiting for reveal in mempool…");
  right.appendChild(meta);
  const row = el("div", { class: "row" });
  row.appendChild(copyButton(() => toBase64(revealPsbt), "copy PSBT base64"));
  row.appendChild(downloadButton(() => revealPsbt, "reveal.psbt"));
  right.appendChild(row);
  const pane = el("div", { class: "qr-pane" });
  pane.append(left, right);
  card.appendChild(pane);
  const stream = encodePsbtUR(revealPsbt);
  animateUR(left, stream, { fps: 4, size: 340 });
}

function renderBootCard(card: HTMLElement, cometPatp: string, feed: Uint8Array): void {
  const bootCmd = formatBootCommand({ comet: cometPatp, feed });
  card.innerHTML = "";
  card.style.display = "";
  card.append(
    el("h2", {}, "Boot your comet"),
    el("p", {},
      "Reveal confirmed. Run this on your own machine (macOS or Linux) to "
      + "download the Groundwire runtime and launch your ship."),
    el("pre", { class: "code" }, bootCmd),
    (() => { const row = el("div", { class: "row" });
      row.appendChild(copyButton(() => bootCmd, "copy command")); return row; })(),
  );
}

function toBase64(b: Uint8Array): string {
  let s = "";
  for (const x of b) s += String.fromCharCode(x);
  return btoa(s);
}
