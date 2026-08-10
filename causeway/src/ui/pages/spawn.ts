// Spawn — kelvin-9 %gw-btc confidential (or public) comet spawn.
//
// PROTOCOL: this page mines a suite-%c comet whose Schnorr tweak is the
// kelvin-9 hiding `dat` (mat(%gw-btc) + mat(9) + a hiding commitment to the
// spawn satpoint, blinded by the seed — see spawn/dat.ts), then builds ONE
// spawn transaction: input 0 = the chosen funding UTXO (the spawn satpoint),
// output 0 = the sat-carrying P2TR output whose key Q commits the initial
// snapshot. There is NO commit/reveal pair and NO on-chain attestation leaf.
//
// A CONFIDENTIAL spawn (default) carries no on-chain payload; the opening is
// disclosed off-chain via the xtr custody log baked into the boot feed once the
// spawn tx confirms. A PUBLIC spawn additionally adds one OP_RETURN publication
// output revealing pass + snapshot + blind-opening.
//
// State is persisted to localStorage after assembly, so an accidental refresh
// between broadcasting the spawn and finalizing doesn't orphan sats. The feed
// (the comet's private seed) is NEVER persisted.

import { el, clearAndAppend, banner, kvList, copyButton } from "../components.js";
import { go, getSession } from "../state.js";
import { miner } from "../../spawn/miner.js";
import {
  formatBootCommand, formatBootCommandFromUw, atomToUw, bytesToAtomLE, uwToAtom,
} from "../../spawn/boot-cmd.js";
import { assembleSpawn } from "../../spawn/assemble.js";
import { buildXtrAtom, bakeXtrIntoFeedAtom } from "../../spawn/reveal-log.js";
import { resolveStartHeight } from "../../spawn/start-height.js";
import type { Opening } from "../../spawn/publication.js";
import { atomToPatp, patpToAtom } from "../../protocol/patp.js";
import { atomToMnemonym, abridgeMnemonym } from "../../protocol/mnemonym.js";
import type { DiscoveredUtxo } from "../../chain/discover.js";
import { encodePsbtUR } from "../../signing/qr-ur.js";
import { animateUR } from "../../signing/qr-render.js";
import {
  savePendingSpawn, loadPendingSpawn, clearPendingSpawn, updatePhase,
  b64Encode, b64Decode, bytesToHex,
  type PersistedSpawn, type PersistedOpening,
} from "../../spawn/persist.js";
import { formatAccountPath } from "../../keys/xpub.js";

const POLL_INTERVAL_MS = 15_000;

function bytesToDisplayHex(displayBytes: Uint8Array): string {
  return Array.from(displayBytes, (b) => b.toString(16).padStart(2, "0")).join("");
}
function shortTxid(hex: string): string { return `${hex.slice(0, 8)}…${hex.slice(-6)}`; }

function downloadBytes(bytes: Uint8Array, filename: string, mime = "application/octet-stream"): void {
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

const SIGNER_ADVICE_HTML = `
  <strong>Signing requirements.</strong>
  The spawn is a standard BIP-86 taproot key-path spend — any modern taproot
  wallet signs it. There is no commit/reveal step and no script-path signing.
  <ul style="margin-top:0.5rem;padding-left:1.2rem;">
    <li><a href="https://sparrowwallet.com" target="_blank" rel="noopener">Sparrow Wallet</a>,
        <strong>BlueWallet</strong>, <strong>Coldcard</strong>, <strong>Keystone</strong>,
        <strong>Passport</strong>, or Bitcoin Core via <code>walletprocesspsbt</code> all work.
        Load the PSBT via <em>File → Load Transaction → From Text</em> (paste base64)
        or scan Causeway's animated QR.</li>
  </ul>
  The state is committed in the sat-carrying output's taproot key; a confidential
  comet discloses its opening off-chain via the reveal log baked into the boot feed.
`.trim();

export function renderSpawn(root: HTMLElement): void {
  const session = getSession();
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

  const resumeCard = el("section", { class: "card", style: "display:none;" });
  const intro = el("section", { class: "card" });
  const status = el("div");
  const mineCard = el("section", { class: "card", style: "display:none;" });
  const spawnCard = el("section", { class: "card", style: "display:none;" });
  const finalizeCard = el("section", { class: "card", style: "display:none;" });
  const bootCard = el("section", { class: "card", style: "display:none;" });

  intro.append(
    el("h1", {}, "Spawn a confidential comet"),
    el("p", { class: "lead" },
      "Causeway mines a comet @p under the ~daplyd star with the kelvin-9 "
      + "hiding tweak, then assembles ONE spawn transaction whose taproot "
      + "output commits your initial state. Nothing is revealed on-chain by "
      + "default — we watch mempool.space for the spawn tx, and once it confirms "
      + "we bake the off-chain reveal log (xtr) into your boot feed."),
  );

  const advice = el("section", { class: "card" });
  advice.innerHTML = SIGNER_ADVICE_HTML;

  if (confirmedFunding.length === 0) {
    const reason = pending
      ? "Your funding UTXO isn't in the mempool's UTXO set right now — most "
        + "likely because you already broadcast the spawn tx for your pending "
        + "spawn. Click Resume above to continue where you left off."
      : "No confirmed funding UTXOs at your account's first 20 addresses. "
        + "Send at least ~1,000 sats to one of your BIP-86 addresses and come back.";
    intro.appendChild(banner("warn", reason));
    clearAndAppend(root, resumeCard, intro, advice, status, mineCard, spawnCard, finalizeCard, bootCard);
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

  const publicRow = el("label", { style: "display:block;margin-top:0.8rem;" });
  const publicChk = el("input", { type: "checkbox", id: "publicSpawn" }) as HTMLInputElement;
  publicRow.append(publicChk, document.createTextNode(
    " Public spawn (add an OP_RETURN publication revealing the opening on-chain)"));
  intro.appendChild(publicRow);

  // Routing. A comet with neither a sponsor nor a fief cannot be
  // cold-contacted: the verifier projects an absent sponsor to SELF, so no
  // peer that has forgotten it can ever find it again. Causeway refuses to
  // mint one unless the operator deliberately ticks the override.
  intro.appendChild(el("h2", { style: "margin-top:1rem;" }, "Routing"));
  intro.appendChild(el("p", { class: "qr-meta" },
    "Confidential comets have no fixed address, so peers reach them through a "
    + "sponsor committed in the on-chain snapshot. Without one, nothing can "
    + "cold-contact your comet."));
  intro.appendChild(el("label", { for: "sponsorPatp" }, "Sponsor @p (e.g. ~daplyd)"));
  const sponsorInput = el("input", {
    type: "text", id: "sponsorPatp", placeholder: "~sampel-palnet",
  }) as HTMLInputElement;
  intro.appendChild(sponsorInput);
  const noRouteRow = el("label", { style: "display:block;margin-top:0.6rem;" });
  const noRouteChk = el("input", { type: "checkbox", id: "noRoute" }) as HTMLInputElement;
  noRouteRow.append(noRouteChk, document.createTextNode(
    " Unroutable (no-route): mint with no sponsor and no fief. Outbound-only — "
    + "no peer will ever be able to contact this comet first."));
  intro.appendChild(noRouteRow);

  const startBtn = el("button", {
    class: "btn primary", type: "button", style: "margin-top:1rem;",
  }, "Mine & build spawn");
  intro.appendChild(startBtn);

  clearAndAppend(root, resumeCard, intro, advice, status, mineCard, spawnCard, finalizeCard, bootCard);
  if (pending) renderResumeCard(resumeCard, pending);

  let pollTimer: ReturnType<typeof setInterval> | null = null;

  async function runFlow(picked: DiscoveredUtxo): Promise<void> {
    status.innerHTML = "";
    for (const c of [mineCard, spawnCard, finalizeCard, bootCard]) c.style.display = "none";
    if (pollTimer) { clearInterval(pollTimer); pollTimer = null; }

    const txidDisplay = bytesToDisplayHex(picked.txid);
    const isPublic = publicChk.checked;
    const noRoute = noRouteChk.checked;

    // Resolve the sponsor BEFORE mining: an unroutable spawn must be refused
    // up front, not after ~65k proof-of-work iterations.
    let sponsor: bigint | null = null;
    const sponsorText = sponsorInput.value.trim();
    if (sponsorText) {
      try {
        sponsor = patpToAtom(sponsorText);
      } catch (err: any) {
        status.appendChild(banner("err", `sponsor: ${err.message ?? err}`));
        return;
      }
    }
    if (sponsor === null && !noRoute) {
      status.appendChild(banner("err",
        "no sponsor given. A comet with neither a sponsor nor a fief cannot be "
        + "cold-contacted — an absent sponsor projects to self-sponsorship, so "
        + "once a peer drops its state the identity is unreachable forever. "
        + "Enter a sponsor @p, or tick “Unroutable (no-route)” on purpose."));
      return;
    }

    status.appendChild(banner("warn", "mining under ~daplyd — ~65k iterations…"));
    const progressLine = el("div", { class: "qr-meta", style: "margin-top:0.3rem;" }, "0 tries");
    status.appendChild(progressLine);

    const t0 = performance.now();
    let mined: Awaited<ReturnType<typeof miner.mine>>;
    try {
      // The kelvin-9 dat is a hiding commitment to the spawn satpoint; the miner
      // recomputes it per candidate seed (blind = H_tag('gw/spawn-blind', seed)).
      mined = await miner.mine({
        spawn: { txidHex: txidDisplay, vout: picked.vout, off: 0 },
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
    const cometMnemo = atomToMnemonym(mined.comet);

    let assembled: ReturnType<typeof assembleSpawn>;
    try {
      assembled = assembleSpawn({
        mined, picked, keys, feeRate: 2, publish: isPublic, sponsor, noRoute,
        ...(picked.blockHeight ? { startHeight: picked.blockHeight } : {}),
      });
    } catch (err: any) {
      status.innerHTML = "";
      status.appendChild(banner("err", `assemble error: ${err.message ?? err}`));
      return;
    }

    // PersistedSnapshot has no `fief` field, and openingFromPersisted rebuilds
    // the snapshot from this record — a dropped fief would silently change the
    // state-key and make the xtr opening unverifiable. assembleSpawn cannot
    // currently mint a fief-bearing spawn (there is no fief input at spawn, in
    // the web app or in the desktop tool), so this is unreachable; if that ever
    // changes, persist the fief (and bump SCHEMA_VERSION) rather than deleting
    // this guard.
    if (assembled.snapshot.fief !== null) {
      status.innerHTML = "";
      status.appendChild(banner("err",
        "internal error: this spawn's snapshot carries a fief, which the saved "
        + "spawn record cannot store. Refusing to persist a record that would "
        + "rebuild a DIFFERENT state-key on resume."));
      return;
    }

    const opening: PersistedOpening = {
      internalKeyHex: assembled.internalKeyHex,
      snapshot: {
        life: assembled.snapshot.life,
        rift: assembled.snapshot.rift,
        keyHex: assembled.snapshot.key.toString(16),
        sponsor: assembled.snapshot.sponsor === null ? null : assembled.snapshot.sponsor.toString(),
      },
      spawnTxidHex: assembled.spawnSont.txidHex,
      spawnVout: assembled.spawnSont.vout,
      spawnOff: Number(assembled.spawnSont.off ?? 0),
      blindHex: assembled.blindHex,
      // The FUNDING tx's block, recorded from the UTXO scan. This — never the
      // spawn tx's height — is the blind-opening's start-height.
      ...(picked.blockHeight ? { fundingHeight: picked.blockHeight } : {}),
    };
    const persisted: PersistedSpawn = {
      version: 4,
      createdAt: Date.now(),
      network: keys.network,
      descriptor: "",
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
      spawnPsbtB64: b64Encode(assembled.spawnPsbt),
      spawnTxidHex: assembled.spawnTxidHex,
      outputValue: assembled.outputValue.toString(),
      opening,
      phase: "assembled",
    };
    savePendingSpawn(persisted);

    renderMineCard(mineCard, {
      cometMnemo,
      cometPatp,
      pickedSummary: `${shortTxid(bytesToDisplayHex(picked.txid))}:${picked.vout} (${picked.value} sats)`,
      tries: mined.tries,
      spawnTxidHex: assembled.spawnTxidHex,
      isPublic,
    });

    const feedUw = atomToUw(bytesToAtomLE(mined.feed));
    downloadBytes(new TextEncoder().encode(feedUw), `${cometPatp}.feed.txt`, "text/plain");
    mineCard.appendChild(banner("warn",
      "Downloaded your comet key (feed) as a .txt — KEEP IT SAFE. It is your "
      + "comet's private key and is not saved anywhere in this browser. You "
      + "need it to boot, and to resume this spawn if the page reloads."));

    renderSpawnTxCard(spawnCard, assembled.spawnPsbt, assembled.spawnTxidHex, isPublic);

    await pollForTx(assembled.spawnTxidHex,
      spawnCard.querySelector<HTMLElement>(".qr-meta")!,
      { minConfs: 0 });
    spawnCard.querySelector<HTMLElement>(".qr-meta")!.textContent = "spawn tx seen in mempool ✓";
    updatePhase("spawn-broadcast");

    renderBootCard(bootCard, cometMnemo, cometPatp, mined.feed);
    renderFinalizeCard(finalizeCard, {
      spawnTxidHex: assembled.spawnTxidHex,
      opening,
      cometPatp,
      getFeedAtom: () => bytesToAtomLE(mined.feed),
    });
    finalizeCard.scrollIntoView({ behavior: "smooth", block: "start" });
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
          resolve();
        } catch (err: any) {
          metaEl.textContent = `poll error: ${err.message ?? err} (retrying)`;
        }
      };
      tick();
      pollTimer = setInterval(tick, POLL_INTERVAL_MS);
    });
  }

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

  function renderFinalizeCard(
    card: HTMLElement,
    opts: {
      spawnTxidHex: string;
      opening: PersistedOpening;
      cometPatp: string;
      getFeedAtom: () => bigint | null;
    },
  ): void {
    card.innerHTML = "";
    card.style.display = "";
    card.append(
      el("h2", {}, "Finalize reveal log (xtr)"),
      el("p", {},
        "Once your spawn tx confirms in a block, Causeway bakes the off-chain "
        + "custody log (xtr) into your boot feed so your booted ship serves its "
        + "own attestation to the %gw-btc verifier. Until then you can boot with "
        + "the plain feed above; it serves an empty log until an %anew round-trip."),
    );
    const meta = el("div", { class: "qr-meta" }, "waiting for spawn tx to confirm…");
    card.appendChild(meta);
    const out = el("div");
    card.appendChild(out);

    pollForConfirmation(opts.spawnTxidHex, meta)
      .then(async (blockHeight) => {
        updatePhase("spawn-confirmed");
        meta.textContent = `spawn tx confirmed in block ${blockHeight} ✓`;
        // The xtr ENTRY's height is the spawn tx's block (that is the tx the
        // entry names). The blind-opening's START-HEIGHT is a different block
        // entirely: the FUNDING tx's, i.e. the block of
        // `opts.opening.spawnTxidHex`. Passing `blockHeight` here made every
        // browser-minted comet unverifiable — the verifier fetches the funding
        // tx by [height txid] and got `attestation-tx-not-found`.
        const startHeight = await resolveStartHeight(opts.opening, s.mp);
        meta.textContent =
          `spawn tx confirmed in block ${blockHeight} ✓ · funding block ${startHeight}`;
        const opening = openingFromPersisted(opts.opening, startHeight);
        const xtr = buildXtrAtom([{ txidHex: opts.spawnTxidHex, blockHeight, opening }]);
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
        `There's an unfinished spawn for ${abridgeMnemonym(cometName)} `
        + `(started ${ageMin} min ago, phase: ${p.phase}). Resume it or discard.`),
      kvList([
        ["Comet", cometName],
        ["Spawn UTXO", `${shortTxid(p.picked.txidHex)}:${p.picked.vout}`],
        ["Spawn txid", p.spawnTxidHex],
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
        "Discard the pending spawn? If you've already broadcast the spawn tx, "
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
    for (const c of [mineCard, spawnCard, finalizeCard, bootCard]) c.style.display = "none";
    if (pollTimer) { clearInterval(pollTimer); pollTimer = null; }

    let spawnPsbt: Uint8Array;
    let cometPatp: string;
    let cometMnemo: string;
    try {
      spawnPsbt = b64Decode(p.spawnPsbtB64);
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
      spawnTxidHex: p.spawnTxidHex,
      isPublic: false,
    });
    renderSpawnTxCard(spawnCard, spawnPsbt, p.spawnTxidHex, false);
    mineCard.scrollIntoView({ behavior: "smooth", block: "start" });
    status.innerHTML = "";

    await pollForTx(p.spawnTxidHex,
      spawnCard.querySelector<HTMLElement>(".qr-meta")!,
      { minConfs: 0 });
    spawnCard.querySelector<HTMLElement>(".qr-meta")!.textContent = "spawn tx seen in mempool ✓";
    updatePhase("spawn-broadcast");

    let resumedFeedAtom: bigint | null = null;
    renderBootCard(bootCard, cometMnemo, cometPatp, null, (uw) => { resumedFeedAtom = uwToAtom(uw); });
    renderFinalizeCard(finalizeCard, {
      spawnTxidHex: p.spawnTxidHex,
      opening: p.opening,
      cometPatp,
      getFeedAtom: () => resumedFeedAtom,
    });
    finalizeCard.scrollIntoView({ behavior: "smooth", block: "start" });
  }
}

// Rebuild the xtr Opening from persisted opening data + the resolved
// start-height (the FUNDING tx's block — see spawn/start-height.ts, and NEVER
// the spawn tx's own height).
function openingFromPersisted(o: PersistedOpening, startHeight: number): Opening {
  return {
    internalKey: BigInt("0x" + o.internalKeyHex),
    snapshot: {
      life: o.snapshot.life,
      rift: o.snapshot.rift,
      key: BigInt("0x" + o.snapshot.keyHex),
      sponsor: o.snapshot.sponsor === null ? null : BigInt(o.snapshot.sponsor),
      // Always null, and provably so: the persist site above refuses to write a
      // record whose snapshot carries a fief, precisely because this rebuild
      // could not restore it.
      fief: null,
    },
    blindOpening: {
      spawnSont: { txidHex: o.spawnTxidHex, vout: o.spawnVout, off: o.spawnOff },
      startHeight,
      blind: BigInt("0x" + o.blindHex),
    },
  };
}

function renderResumeOnly(root: HTMLElement, p: PersistedSpawn): void {
  const card = el("section", { class: "card" });
  const cometName = abridgeMnemonym(atomToMnemonym(BigInt(p.mined.comet)));
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
      "Discard the pending spawn? If you've already broadcast the spawn tx, "
      + "those sats become unreachable.",
    )) return;
    clearPendingSpawn();
    window.location.href = "#/";
  });
  clearAndAppend(root, card);
}

// ---- Card renderers ----

function renderMineCard(
  card: HTMLElement,
  info: {
    cometMnemo: string;
    cometPatp: string;
    pickedSummary: string;
    tries: number;
    spawnTxidHex: string;
    isPublic: boolean;
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
      ["Spawn txid (predicted)", info.spawnTxidHex],
      ["Mode", info.isPublic ? "public (OP_RETURN publication)" : "confidential"],
    ]),
  );
}

function renderSpawnTxCard(
  card: HTMLElement,
  spawnPsbt: Uint8Array,
  spawnTxidHex: string,
  isPublic: boolean,
): void {
  card.innerHTML = "";
  card.style.display = "";
  const left = el("div");
  const right = el("div");
  right.appendChild(el("h2", {}, "Sign & broadcast the spawn transaction"));
  right.appendChild(el("p", {},
    "Paste this into your wallet (Sparrow: File → Load Transaction → From Text), "
    + "or scan the QR. It's a plain BIP-86 taproot key-path spend — sign it and "
    + `broadcast. Causeway is watching for ${shortTxid(spawnTxidHex)}.`));
  right.appendChild(banner("ok", isPublic
    ? "This PUBLIC spawn carries an OP_RETURN publication revealing your opening."
    : "Your initial state is committed in this transaction's taproot output; "
      + "nothing is published on-chain — this is the confidential flow."));

  const meta = el("div", { class: "qr-meta" }, "waiting for spawn tx in mempool…");
  right.appendChild(meta);

  const row = el("div", { class: "row" });
  row.appendChild(copyButton(() => toBase64(spawnPsbt), "copy PSBT base64"));
  row.appendChild(downloadButton(() => spawnPsbt, "spawn.psbt"));
  right.appendChild(row);

  const pane = el("div", { class: "qr-pane" });
  pane.append(left, right);
  card.appendChild(pane);

  const stream = encodePsbtUR(spawnPsbt);
  animateUR(left, stream, { fps: 4, size: 340 });
}

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
      + "runtime and launch your ship. You can boot as soon as the spawn tx is in "
      + "the mempool; the Finalize step below upgrades this to include your "
      + `off-chain reveal log once the tx confirms. Uses your @p (${cometPatp}).`),
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

function renderFinalizeNeedsFeed(out: HTMLElement, cometPatp: string, xtr: bigint): void {
  out.innerHTML = "";
  out.appendChild(banner("warn",
    "Paste your saved feed (0w…) into the Boot card above and click “Show boot "
    + "command”, then reload this page — Causeway will bake the reveal log below "
    + "into it. Your reveal log (xtr) atom, for reference:"));
  out.appendChild(el("pre", { class: "code" }, "0x" + xtr.toString(16)));
  out.appendChild(el("p", { class: "qr-meta" },
    `(@p ${cometPatp} — the reveal log commits to your spawn tx + block.)`));
}

function toBase64(b: Uint8Array): string {
  let s = "";
  for (const x of b) s += String.fromCharCode(x);
  return btoa(s);
}
