// Spawn — cc-draft-2 CONFIDENTIAL comet spawn (at parity with desktop Causeway).
//
// PROTOCOL: this page mines a suite-%c comet whose Schnorr tweak is the
// cc-draft-2 `dat` (mat-encoded %gw-btc PKI domain + spawn satpoint, see
// spawn/dat.ts), then builds a single COMMIT-ONLY taproot transaction whose
// output commits the %spawn attestation in a NUMS-keyed tapleaf. The
// attestation is NEVER revealed on-chain (no reveal PSBT); it is disclosed
// off-chain to the %gw-btc verifier via the xtr reveal log baked into the boot
// feed once the commit confirms. This mirrors desktop/causeway.py's
// build_confidential_commit_psbt + run_spawn_connect + `causeway finalize`.
//
// The legacy PUBLIC flow (v9 rap-3 tweak + on-chain commit/reveal) has been
// retired; see git history / the removed spawn/tweak.ts.
//
// State is persisted to localStorage after assembly, so an accidental refresh
// between broadcasting the commit and finalizing doesn't orphan sats.

import { el, clearAndAppend, banner, kvList, copyButton } from "../components.js";
import { go, getSession } from "../state.js";
import { miner } from "../../spawn/miner.js";
import {
  formatBootCommand, formatBootCommandFromUw, atomToUw, bytesToAtomLE, uwToAtom,
} from "../../spawn/boot-cmd.js";
import { assembleSpawn } from "../../spawn/assemble.js";
import { buildDatBytes } from "../../spawn/dat.js";
import { buildXtrAtom, bakeXtrIntoFeedAtom } from "../../spawn/reveal-log.js";
import { atomToPatp } from "../../protocol/patp.js";
import { atomToMnemonym, abridgeMnemonym } from "../../protocol/mnemonym.js";
import type { DiscoveredUtxo } from "../../chain/discover.js";
import { encodePsbtUR } from "../../signing/qr-ur.js";
import { animateUR } from "../../signing/qr-render.js";
import {
  savePendingSpawn, loadPendingSpawn, clearPendingSpawn, updatePhase,
  b64Encode, b64Decode, bytesToHex,
  type PersistedSpawn, type PersistedXtrInputs,
} from "../../spawn/persist.js";
import { formatAccountPath } from "../../keys/xpub.js";

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

// Copy describing which wallets the user should actually use for signing. The
// confidential commit is a plain BIP-86 key-path spend — every taproot wallet
// handles it, and there is NO awkward script-path reveal step anymore.
const SIGNER_ADVICE_HTML = `
  <strong>Signing requirements.</strong>
  The confidential commit is a standard BIP-86 taproot key-path spend — any
  modern taproot wallet signs it. There is no on-chain reveal, so you do not
  need the script-path signers the old public flow required.
  <ul style="margin-top:0.5rem;padding-left:1.2rem;">
    <li><a href="https://sparrowwallet.com" target="_blank" rel="noopener">Sparrow Wallet</a>,
        <strong>BlueWallet</strong>, <strong>Coldcard</strong>, <strong>Keystone</strong>,
        <strong>Passport</strong>, or Bitcoin Core via <code>walletprocesspsbt</code> all work.
        Load the PSBT via <em>File → Load Transaction → From Text</em> (paste base64)
        or scan Causeway's animated QR.</li>
  </ul>
  The attestation lives in the taproot output's committed tapleaf and is never
  published on-chain; your comet discloses it off-chain via the reveal log baked
  into the boot feed.
`.trim();

export function renderSpawn(root: HTMLElement): void {
  const session = getSession();

  // Check for a pending spawn before demanding a full session — a returning
  // user whose tab just refreshed may hit this page first.
  const pending = loadPendingSpawn();

  if (!session) {
    if (pending) { renderResumeOnly(root, pending); return; }
    go("#/"); return;
  }
  if (!session.keys || !session.discovery) {
    if (pending) { renderResumeOnly(root, pending); return; }
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
  const finalizeCard = el("section", { class: "card", style: "display:none;" });
  const bootCard = el("section", { class: "card", style: "display:none;" });

  intro.append(
    el("h1", {}, "Spawn a confidential comet"),
    el("p", { class: "lead" },
      "Causeway mines a comet @p under the ~daplyd star with the cc-draft-2 "
      + "confidential tweak, then assembles ONE commit transaction whose taproot "
      + "output commits your %spawn attestation. Nothing is revealed on-chain — "
      + "we watch mempool.space for the commit, and once it confirms we bake the "
      + "off-chain reveal log (xtr) into your boot feed."),
  );

  // Signer advice card — shown up front so users pick the right tool.
  const advice = el("section", { class: "card" });
  advice.innerHTML = SIGNER_ADVICE_HTML;

  if (confirmedFunding.length === 0) {
    // A very common way to land here with an empty UTXO set: the user already
    // broadcast a commit tx in a previous session, which spent their funding
    // UTXO. mempool.space no longer lists it, so discovery comes back empty.
    const reason = pending
      ? "Your funding UTXO isn't in the mempool's UTXO set right now — most "
        + "likely because you already broadcast the commit tx for your pending "
        + "spawn. Click Resume above to continue where you left off."
      : "No confirmed funding UTXOs at your account's first 20 addresses. "
        + "Send at least ~1,000 sats to one of your BIP-86 addresses and come back.";
    intro.appendChild(banner("warn", reason));
    clearAndAppend(root, resumeCard, intro, advice, status, mineCard, commitCard, finalizeCard, bootCard);
    if (pending) renderResumeCard(resumeCard, pending);
    return;
  }

  intro.appendChild(el("h2", {}, "Spawn UTXO"));
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
  }, "Mine & build commit");
  intro.appendChild(startBtn);

  clearAndAppend(root, resumeCard, intro, advice, status, mineCard, commitCard, finalizeCard, bootCard);
  if (pending) renderResumeCard(resumeCard, pending);

  let pollTimer: ReturnType<typeof setInterval> | null = null;

  async function runFlow(picked: DiscoveredUtxo): Promise<void> {
    status.innerHTML = "";
    for (const c of [mineCard, commitCard, finalizeCard, bootCard]) c.style.display = "none";
    if (pollTimer) { clearInterval(pollTimer); pollTimer = null; }

    const txidDisplay = bytesToDisplayHex(picked.txid);
    // cc-draft-2 `dat` — the name-committing confidential tweak (mat(%gw-btc) +
    // spawn satpoint), replacing the legacy v9 rap-3 tweak.
    const dat = buildDatBytes({ txidHex: txidDisplay, vout: picked.vout, off: 0 });

    status.appendChild(banner("warn", "mining under ~daplyd — ~65k iterations…"));
    const progressLine = el("div", { class: "qr-meta", style: "margin-top:0.3rem;" }, "0 tries");
    status.appendChild(progressLine);

    const t0 = performance.now();
    let mined: Awaited<ReturnType<typeof miner.mine>>;
    try {
      mined = await miner.mine({
        tweakExpr: dat,
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

    // The @p is the machine identity (feed filename, boot command); the
    // mnemonym is what we show the user.
    const cometPatp = atomToPatp(mined.comet);
    const cometMnemo = atomToMnemonym(mined.comet);

    const assembled = assembleSpawn({ mined, picked, keys, feeRate: 2 });

    // ---- Persist everything we'd need to resume from a page refresh ----
    // NB: the feed (which embeds the private seed) is intentionally NOT
    // persisted; the user must save it from the download below.
    const xtrInputs: PersistedXtrInputs = {
      internalKeyHex: assembled.internalKeyHex,
      leafScriptHex: assembled.leafScriptHex,
      leafVersion: assembled.leafVersion,
    };
    const persisted: PersistedSpawn = {
      version: 3,
      createdAt: Date.now(),
      network: keys.network,
      descriptor: "", // best-effort; may be empty if user pasted a bare xpub
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
        tries: mined.tries,
      },
      commitPsbtB64: b64Encode(assembled.commitPsbt),
      commitTxidHex: assembled.commitTxidHex,
      commitValue: assembled.commitOutputValue.toString(),
      xtr: xtrInputs,
      phase: "assembled",
    };
    savePendingSpawn(persisted);

    renderMineCard(mineCard, {
      cometMnemo,
      cometPatp,
      pickedSummary: `${shortTxid(bytesToDisplayHex(picked.txid))}:${picked.vout} (${picked.value} sats)`,
      tries: mined.tries,
      commitTxidHex: assembled.commitTxidHex,
    });

    // The feed is the comet's private key and is deliberately NOT stored in
    // this browser. Download it now (as the 0w… atom) so the user can boot —
    // and resume if the tab is refreshed before the boot step.
    const feedUw = atomToUw(bytesToAtomLE(mined.feed));
    downloadBytes(new TextEncoder().encode(feedUw), `${cometPatp}.feed.txt`, "text/plain");
    mineCard.appendChild(banner("warn",
      "Downloaded your comet key (feed) as a .txt — KEEP IT SAFE. It is your "
      + "comet's private key and is not saved anywhere in this browser. You "
      + "need it to boot, and to resume this spawn if the page reloads."));

    renderCommitCard(commitCard, assembled.commitPsbt, assembled.commitTxidHex);

    await pollForTx(assembled.commitTxidHex,
      commitCard.querySelector<HTMLElement>(".qr-meta")!,
      { minConfs: 0 });
    commitCard.querySelector<HTMLElement>(".qr-meta")!.textContent = "commit seen in mempool ✓";
    updatePhase("commit-broadcast");

    // Boot now (raw feed) + finalize (bake xtr once the commit confirms).
    renderBootCard(bootCard, cometMnemo, cometPatp, mined.feed);
    renderFinalizeCard(finalizeCard, {
      commitTxidHex: assembled.commitTxidHex,
      xtr: xtrInputs,
      cometPatp,
      getFeedAtom: () => bytesToAtomLE(mined.feed),
    });
    finalizeCard.scrollIntoView({ behavior: "smooth", block: "start" });
  }

  // Poll for a tx reaching the mempool (minConfs 0) — resolves once seen.
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
          resolve();
        } catch (err: any) {
          metaEl.textContent = `poll error: ${err.message ?? err} (retrying)`;
        }
      };
      tick();
      pollTimer = setInterval(tick, POLL_INTERVAL_MS);
    });
  }

  // Poll until the tx confirms; resolve with its block height (needed to build
  // the xtr reveal-log entry — the twin of desktop `causeway finalize`).
  async function pollForConfirmation(txidHex: string, metaEl: HTMLElement): Promise<number> {
    return new Promise((resolve) => {
      const tick = async (): Promise<void> => {
        try {
          const tx = await s.mp.tx(txidHex).catch(() => null);
          if (!tx) { metaEl.textContent = `waiting for ${shortTxid(txidHex)}…`; return; }
          if (!tx.status.confirmed || tx.status.block_height === undefined) {
            metaEl.textContent = `in mempool (0 confs) · waiting for a block…`;
            return;
          }
          if (pollTimer) { clearInterval(pollTimer); pollTimer = null; }
          resolve(tx.status.block_height);
        } catch (err: any) {
          metaEl.textContent = `poll error: ${err.message ?? err} (retrying)`;
        }
      };
      tick();
      pollTimer = setInterval(tick, POLL_INTERVAL_MS);
    });
  }

  // Render the finalize card: poll the commit to confirmation, then bake the
  // xtr reveal log into a feed and present the finalized boot command.
  function renderFinalizeCard(
    card: HTMLElement,
    opts: {
      commitTxidHex: string;
      xtr: PersistedXtrInputs;
      cometPatp: string;
      getFeedAtom: () => bigint | null;
    },
  ): void {
    card.innerHTML = "";
    card.style.display = "";
    card.append(
      el("h2", {}, "Finalize reveal log (xtr)"),
      el("p", {},
        "Once your commit confirms in a block, Causeway bakes the off-chain "
        + "reveal log (xtr) into your boot feed so your booted ship serves its "
        + "own attestation to the %gw-btc verifier. Until then you can boot with "
        + "the plain feed above; it works, but serves an empty reveal log until "
        + "an %anew round-trip refreshes it."),
    );
    const meta = el("div", { class: "qr-meta" }, "waiting for commit to confirm…");
    card.appendChild(meta);
    const out = el("div");
    card.appendChild(out);

    pollForConfirmation(opts.commitTxidHex, meta)
      .then((blockHeight) => {
        updatePhase("commit-confirmed");
        meta.textContent = `commit confirmed in block ${blockHeight} ✓`;
        const xtr = buildXtrAtom([{
          txidHex: opts.commitTxidHex,
          blockHeight,
          reveal: {
            // 33-byte compressed internal key, even parity — matching desktop
            // finalize's "02" + internal_pubkey_hex.
            internalKeyHex: "02" + opts.xtr.internalKeyHex,
            leafVersion: opts.xtr.leafVersion,
            leafScriptHex: opts.xtr.leafScriptHex,
          },
        }]);
        const baseFeed = opts.getFeedAtom();
        if (baseFeed === null) {
          renderFinalizeNeedsFeed(out, opts.cometPatp, xtr);
        } else {
          const feedBytes = bakeXtrIntoFeedAtom(baseFeed, xtr);
          renderFinalizedBoot(out, opts.cometPatp, feedBytes);
          clearPendingSpawn();
        }
      })
      .catch((err) => {
        out.innerHTML = "";
        out.appendChild(banner("err", `finalize error: ${err.message ?? err}`));
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
    const cometName = atomToMnemonym(BigInt(p.mined.comet));
    const ageMin = Math.round((Date.now() - p.createdAt) / 60_000);
    el_.append(
      el("h2", {}, "Pending spawn in progress"),
      el("p", {},
        `There's an unfinished confidential spawn for ${abridgeMnemonym(cometName)} `
        + `(started ${ageMin} min ago, phase: ${p.phase}). Resume it or discard.`),
      kvList([
        ["Comet", cometName],
        ["Spawn UTXO", `${shortTxid(p.picked.txidHex)}:${p.picked.vout}`],
        ["Commit txid", p.commitTxidHex],
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
    resumeCard.style.display = "none";
    intro.style.display = "none";
    advice.style.display = "none";

    status.innerHTML = "";
    status.appendChild(banner("warn",
      `resuming ${abridgeMnemonym(atomToMnemonym(BigInt(p.mined.comet)))} from phase: ${p.phase}…`));
    for (const c of [mineCard, commitCard, finalizeCard, bootCard]) c.style.display = "none";
    if (pollTimer) { clearInterval(pollTimer); pollTimer = null; }

    let commitPsbt: Uint8Array;
    let cometPatp: string;
    let cometMnemo: string;
    try {
      commitPsbt = b64Decode(p.commitPsbtB64);
      cometPatp = atomToPatp(BigInt(p.mined.comet));
      cometMnemo = atomToMnemonym(BigInt(p.mined.comet));
    } catch (err: any) {
      status.innerHTML = "";
      status.appendChild(banner("err",
        `couldn't decode saved spawn data: ${err.message ?? err}. `
        + `You may need to discard this pending spawn and start fresh.`));
      return;
    }

    renderMineCard(mineCard, {
      cometMnemo,
      cometPatp,
      pickedSummary: `${shortTxid(p.picked.txidHex)}:${p.picked.vout} (${p.picked.value} sats)`,
      tries: p.mined.tries,
      commitTxidHex: p.commitTxidHex,
    });
    renderCommitCard(commitCard, commitPsbt, p.commitTxidHex);
    mineCard.scrollIntoView({ behavior: "smooth", block: "start" });
    status.innerHTML = "";

    await pollForTx(p.commitTxidHex,
      commitCard.querySelector<HTMLElement>(".qr-meta")!,
      { minConfs: 0 });
    commitCard.querySelector<HTMLElement>(".qr-meta")!.textContent = "commit seen in mempool ✓";
    updatePhase("commit-broadcast");

    // The feed isn't persisted (it's the private key), so on a resumed flow the
    // user re-supplies the copy they saved at mine time. renderBootCard's paste
    // UI captures it; finalize then bakes xtr into that re-supplied feed.
    let resumedFeedAtom: bigint | null = null;
    renderBootCard(bootCard, cometMnemo, cometPatp, null, (uw) => { resumedFeedAtom = uwToAtom(uw); });
    renderFinalizeCard(finalizeCard, {
      commitTxidHex: p.commitTxidHex,
      xtr: p.xtr,
      cometPatp,
      getFeedAtom: () => resumedFeedAtom,
    });
    finalizeCard.scrollIntoView({ behavior: "smooth", block: "start" });
  }
}

// If there's no session at all but a pending spawn exists, offer just the
// resume option — the user may not need to re-authenticate.
function renderResumeOnly(root: HTMLElement, p: PersistedSpawn): void {
  const card = el("section", { class: "card" });
  const cometName = abridgeMnemonym(atomToMnemonym(BigInt(p.mined.comet)));
  card.append(
    el("h1", {}, "Pending spawn in progress"),
    el("p", { class: "lead" },
      `There's an unfinished confidential spawn for ${cometName} saved in this browser. `
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
    cometMnemo: string;
    cometPatp: string;
    pickedSummary: string;
    tries: number;
    commitTxidHex: string;
  },
): void {
  card.innerHTML = "";
  card.style.display = "";
  card.append(
    el("h2", {}, "Mined ✓"),
    kvList([
      ["Comet", info.cometMnemo],
      ["@p", info.cometPatp],
      ["Spawn UTXO", info.pickedSummary],
      ["Mining tries", info.tries.toLocaleString()],
      ["Commit txid (predicted)", info.commitTxidHex],
    ]),
  );
}

function renderCommitCard(
  card: HTMLElement,
  commitPsbt: Uint8Array,
  commitTxidHex: string,
): void {
  card.innerHTML = "";
  card.style.display = "";
  const left = el("div");
  const right = el("div");
  right.appendChild(el("h2", {}, "Sign & broadcast the confidential commit"));
  right.appendChild(el("p", {},
    "Paste this into your wallet (Sparrow: File → Load Transaction → From Text), "
    + "or scan the QR. It's a plain BIP-86 taproot key-path spend — sign it and "
    + `broadcast. There is NO reveal step. Causeway is watching for ${shortTxid(commitTxidHex)}.`));
  right.appendChild(banner("ok",
    "The %spawn attestation is committed in this transaction's taproot output "
    + "and is never published on-chain — this is the confidential flow."));

  const meta = el("div", { class: "qr-meta" }, "waiting for commit in mempool…");
  right.appendChild(meta);

  const row = el("div", { class: "row" });
  row.appendChild(copyButton(() => toBase64(commitPsbt), "copy PSBT base64"));
  row.appendChild(downloadButton(() => commitPsbt, "commit.psbt"));
  right.appendChild(row);

  const pane = el("div", { class: "qr-pane" });
  pane.append(left, right);
  card.appendChild(pane);

  const stream = encodePsbtUR(commitPsbt);
  animateUR(left, stream, { fps: 4, size: 340 });
}

// The boot card. In the fresh flow `feed` is in memory; on a resumed flow it is
// null (the feed is never persisted — it embeds the private seed), so we ask
// the user for the copy they saved at mine time. `onFeedUw` (resume only) is
// called with the pasted @uw so the finalize step can bake xtr into it.
function renderBootCard(
  card: HTMLElement,
  cometMnemo: string,
  cometPatp: string,
  feed: Uint8Array | null,
  onFeedUw?: (uw: string) => void,
): void {
  card.innerHTML = "";
  card.style.display = "";
  card.append(
    el("h2", {}, "Boot your comet"),
    el("div", { class: "mnemonym" }, cometMnemo),
    el("p", {},
      "Run this on your own machine (macOS or Linux) to download the Groundwire "
      + "runtime and launch your ship. You can boot as soon as the commit is in "
      + "the mempool; the Finalize step below upgrades this to include your "
      + `off-chain reveal log once the commit confirms. Uses your @p (${cometPatp}).`),
  );

  const cmdPre = el("pre", { class: "code" });
  const cmdRow = el("div", { class: "row" });

  const showCmd = (cmd: string): void => {
    cmdPre.textContent = cmd;
    cmdRow.innerHTML = "";
    cmdRow.appendChild(copyButton(() => cmd, "copy command"));
  };

  if (feed) {
    showCmd(formatBootCommand({ comet: cometPatp, feed }));
    card.append(cmdPre, cmdRow);
  } else {
    card.append(banner("warn",
      "Your feed (the comet's key) wasn't saved in this browser — for your "
      + "security it is never stored. Paste the feed you downloaded at mine "
      + "time (the 0w… string) to reproduce your boot command."));
    const input = el("textarea", { rows: "3", placeholder: "0w…" }) as HTMLTextAreaElement;
    const goBtn = el("button", { class: "btn primary", type: "button" }, "Show boot command");
    const err = el("div");
    goBtn.addEventListener("click", () => {
      err.innerHTML = "";
      const uw = input.value.trim();
      if (!/^0w[0-9a-zA-Z.~-]+$/.test(uw)) {
        err.appendChild(banner("err", "that doesn't look like a 0w… feed atom"));
        return;
      }
      showCmd(formatBootCommandFromUw({ comet: cometPatp, feedUw: uw }));
      onFeedUw?.(uw);
    });
    card.append(input, goBtn, err, cmdPre, cmdRow);
  }
}

// The finalized boot command, using the xtr-baked feed.
function renderFinalizedBoot(out: HTMLElement, cometPatp: string, feedBytes: Uint8Array): void {
  out.innerHTML = "";
  out.appendChild(banner("ok",
    "Reveal log baked into your feed. Use THIS boot command — it serves your "
    + "attestation to the %gw-btc verifier."));
  const cmd = formatBootCommand({ comet: cometPatp, feed: feedBytes });
  const cmdPre = el("pre", { class: "code" }, cmd);
  const row = el("div", { class: "row" });
  row.appendChild(copyButton(() => cmd, "copy finalized command"));
  const feedUw = atomToUw(bytesToAtomLE(feedBytes));
  row.appendChild(downloadButton(
    () => new TextEncoder().encode(feedUw), `${cometPatp}.feed.xtr.txt`,
  ));
  out.append(cmdPre, row);
}

// On resume, the finalize step needs the user's feed to bake xtr in. If they
// haven't pasted it into the boot card yet, show the raw xtr and instructions.
function renderFinalizeNeedsFeed(out: HTMLElement, cometPatp: string, xtr: bigint): void {
  out.innerHTML = "";
  out.appendChild(banner("warn",
    "Paste your saved feed (0w…) into the Boot card above and click “Show boot "
    + "command”, then reload this page — Causeway will bake the reveal log below "
    + "into it. Your reveal log (xtr) atom, for reference:"));
  out.appendChild(el("pre", { class: "code" }, "0x" + xtr.toString(16)));
  out.appendChild(el("p", { class: "qr-meta" },
    `(@p ${cometPatp} — the reveal log commits to your spawn commit + block.)`));
}

function toBase64(b: Uint8Array): string {
  let s = "";
  for (const x of b) s += String.fromCharCode(x);
  return btoa(s);
}
