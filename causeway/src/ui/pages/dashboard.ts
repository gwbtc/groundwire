import { el, clearAndAppend, banner, kvList, atomToHex } from "../components.js";
import { go, getSession } from "../state.js";
import { atomToPatp } from "../../protocol/patp.js";
import { lookupPoint } from "../../oracle/point.js";

const OP_LABELS: Array<[string, string, string]> = [
  ["rekey", "Rotate networking key", "Deploy a new networking key; optional breach."],
  ["escape", "Escape to new sponsor", "Request a new sponsor; they must sign off-chain."],
  ["cancel-escape", "Cancel pending escape", "Withdraw an in-flight escape request."],
  ["adopt", "Adopt a child", "Accept a child's escape to you."],
  ["reject", "Reject a child", "Refuse a child's escape request."],
  ["detach", "Detach a child", "Disown a current child."],
  ["fief", "Set fief (static IP)", "Pin your networking endpoint on-chain."],
  ["set-mang", "Set management proxy", "Delegate sponsorship via a management key."],
];

export function renderDashboard(root: HTMLElement): void {
  const s = getSession();
  if (!s || s.patpAtom === undefined || !s.auth) { go("#/"); return; }
  const patpAtom = s.patpAtom;
  const auth = s.auth;
  const point = lookupPoint(s.snapshot, patpAtom);
  if (!point) { go("#/"); return; }

  const card = el("section", { class: "card" });
  card.append(el("h1", {}, atomToPatp(patpAtom)));

  const meta = kvList([
    ["Block", `${s.snapshot.blockId.num}`],
    ["Rift", `${point.net.rift}`],
    ["Life", `${point.net.life}`],
    ["Sponsor", atomToPatp(point.net.sponsor.who)],
    ["Pending escape", point.net.escape ? atomToPatp(point.net.escape) : "—"],
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
