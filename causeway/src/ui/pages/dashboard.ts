import { el, clearAndAppend, banner, kvList, atomToHex } from "../components.js";
import { go, getSession } from "../state.js";
import { atomToPatp } from "../../protocol/patp.js";
import { atomToMnemonym, displayId } from "../../protocol/mnemonym.js";
import { lookupPoint } from "../../oracle/point.js";

// Under the kelvin-9 OP_RETURN revision the only surviving on-chain management
// op is a state update (rekey). Sponsorship / escape / adopt / reject / detach /
// fief / set-mang are off-chain now (spec §8).
const OP_LABELS: Array<[string, string, string]> = [
  ["rekey", "Rotate messaging key", "Re-commit a new snapshot; optional breach."],
];

export function renderDashboard(root: HTMLElement): void {
  const s = getSession();
  if (!s || s.patpAtom === undefined || !s.auth) { go("#/"); return; }
  const patpAtom = s.patpAtom;
  const auth = s.auth;
  const point = lookupPoint(s.snapshot, patpAtom);
  if (!point) { go("#/"); return; }

  const card = el("section", { class: "card" });
  // The comet's mnemonym is its human name; the @p is kept as a muted
  // reference (it's what the boot command and other machine tokens use).
  card.append(
    el("h1", { class: "mnemonym" }, atomToMnemonym(patpAtom)),
    el("div", { class: "patp-ref" }, atomToPatp(patpAtom)),
  );

  const meta = kvList([
    ["Block", `${s.snapshot.blockId.num}`],
    ["Rift", `${point.net.rift}`],
    ["Life", `${point.net.life}`],
    ["Sponsor", displayId(point.net.sponsor.who)],
    ["Pending escape", point.net.escape ? displayId(point.net.escape) : "—"],
    ["Fief", point.net.fief
      ? (point.net.fief.type === "if"
        ? `${((point.net.fief.ip >>> 24) & 0xff)}.${((point.net.fief.ip >>> 16) & 0xff)}.${((point.net.fief.ip >>> 8) & 0xff)}.${(point.net.fief.ip & 0xff)}:${point.net.fief.port}`
        : point.net.fief.type)
      : "—"],
    ["Networking pass", "0x" + atomToHex(point.net.pass).slice(0, 48) + "…"],
    ["Inscription UTXO", `${auth.utxo.txid.slice(0, 12)}…:${auth.utxo.vout}`],
    ["Auth key source", auth.source === "witness" ? "reveal witness control block" : "tweaked (fallback)"],
  ]);
  card.appendChild(meta);

  if (auth.source === "tweaked-fallback") {
    card.appendChild(banner("warn",
      "Could not find a reveal witness for this inscription's current UTXO. " +
      "Reveal-path signing will fall back to the tweaked output key — " +
      "this typically means your point is still at its spawn commit."));
  }

  if (s.discovery) {
    const d = s.discovery;
    card.appendChild(el("h2", {}, "Your balance"));
    card.appendChild(kvList([
      ["Funding UTXOs", `${d.fundingUtxos.length}`],
      ["Spendable sats", `${d.totalFundingSats}`],
      ["Inscription UTXOs (protected)", `${d.inscriptionUtxos.length}`],
    ]));
  }

  const opsCard = el("section", { class: "card" });
  opsCard.appendChild(el("h2", {}, "Operations"));
  const ul = el("ul", { class: "op-list" });
  for (const [key, label, desc] of OP_LABELS) {
    const li = el("li");
    const a = el("a", { href: `#/op/${key}` });
    a.append(el("div", {}, label), el("div", { class: "desc" }, desc));
    li.appendChild(a);
    ul.appendChild(li);
  }
  opsCard.appendChild(ul);

  const spawnCard = el("section", { class: "card" });
  spawnCard.append(
    el("h2", {}, "Spawn another"),
    el("p", {}, "Use a separate Bitcoin UTXO to mine and spawn a second comet under the same hardware wallet."),
  );
  const spawnBtn = el("a", { class: "btn primary", href: "#/spawn" }, "Start a spawn");
  spawnCard.appendChild(spawnBtn);

  clearAndAppend(root, card, opsCard, spawnCard);
}
