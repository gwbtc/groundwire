import { el, clearAndAppend, banner } from "../components.js";
import { fetchSnapshot } from "../../oracle/snapshot.js";
import { lookupPoint, resolveAuthPubkey } from "../../oracle/point.js";
import { isPatp, patpToAtom } from "../../protocol/patp.js";
import { Mempool } from "../../chain/mempool.js";
import { go, setSession, clearManageSession } from "../state.js";

export async function renderLanding(root: HTMLElement): Promise<void> {
  // ---- Hero ----
  const hero = el("section", { class: "hero" });
  hero.append(
    el("h1", {}, "Spawn a Groundwire comet"),
    el("p", { class: "tagline" },
      "Mint a new on-chain identity, tied to a satpoint you control, "
      + "signed by your own hardware wallet."),
    (() => {
      // Any prior manage-existing state (patpAtom/auth) is explicitly dropped
      // when entering the spawn flow, so the keys page doesn't mislabel the
      // header with a point we're no longer acting on.
      const btn = el("a", { class: "btn primary cta", href: "#/keys?then=spawn" },
        "Start spawn →");
      btn.addEventListener("click", () => { clearManageSession(); });
      return btn;
    })(),
  );

  const manageLink = el("span", { class: "manage-link" }, "I already have a comet — manage it");
  const manageWrap = el("div", { style: "text-align:center;" });
  manageWrap.appendChild(manageLink);
  hero.appendChild(manageWrap);

  // ---- Modal (hidden by default) ----
  const backdrop = el("div", { class: "modal-backdrop" });
  const modal = el("div", { class: "modal" });
  const modalWrap = el("div", { class: "wrap" });
  const closeBtn = el("button", { class: "modal-close", type: "button", "aria-label": "close" }, "×");
  modalWrap.appendChild(closeBtn);
  modalWrap.append(
    el("h2", {}, "Manage an existing comet"),
    el("p", {}, "Enter the @p of the comet you control. We'll fetch its on-chain record and, after you import your taproot xpub, let you sign identity operations from your hardware wallet."),
  );

  const form = el("form", { id: "loginForm" });
  form.append(
    el("label", { for: "patp" }, "Comet @p"),
    el("input", {
      type: "text", id: "patp", name: "patp", value: "~",
      autocomplete: "off", spellcheck: "false",
      placeholder: "~sampel-palnet",
    }),
  );
  const submitRow = el("div", { class: "row" });
  submitRow.appendChild(el("button", { class: "btn primary", type: "submit" }, "Look up"));
  form.appendChild(submitRow);
  modalWrap.appendChild(form);
  const status = el("div", { id: "status" });
  modalWrap.appendChild(status);

  modal.appendChild(modalWrap);
  backdrop.appendChild(modal);

  clearAndAppend(root, hero, backdrop);

  // ---- Behavior ----
  const openModal = (): void => {
    backdrop.classList.add("open");
    setTimeout(() => {
      const inp = form.querySelector<HTMLInputElement>("#patp");
      inp?.focus();
      inp?.setSelectionRange(1, 1); // after the tilde
    }, 50);
  };
  const closeModal = (): void => {
    backdrop.classList.remove("open");
    status.innerHTML = "";
  };

  manageLink.addEventListener("click", openModal);
  closeBtn.addEventListener("click", closeModal);
  backdrop.addEventListener("click", (e) => {
    if (e.target === backdrop) closeModal();
  });
  document.addEventListener("keydown", (e) => {
    if (e.key === "Escape" && backdrop.classList.contains("open")) closeModal();
  }, { once: false });

  form.addEventListener("submit", async (ev) => {
    ev.preventDefault();
    status.innerHTML = "";
    status.appendChild(banner("warn", "fetching snapshot…"));
    try {
      const inp = form.querySelector<HTMLInputElement>("#patp");
      const patp = (inp?.value ?? "").trim();
      if (!isPatp(patp)) throw new Error(`invalid @p: ${patp}`);
      const patpAtom = patpToAtom(patp);
      const snapshot = await fetchSnapshot();
      if (!lookupPoint(snapshot, patpAtom)) {
        throw new Error(`@p not in snapshot (block ${snapshot.blockId.num})`);
      }
      const mp = new Mempool();
      const auth = await resolveAuthPubkey(snapshot, patpAtom, mp);
      setSession({ patpAtom, snapshot, auth, mp });
      go("#/keys");
    } catch (err: any) {
      status.innerHTML = "";
      status.appendChild(banner("err", `error: ${err.message ?? err}`));
    }
  });
}
