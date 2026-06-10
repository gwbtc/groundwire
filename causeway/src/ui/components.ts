// Small DOM utility helpers shared across pages.

export function el<K extends keyof HTMLElementTagNameMap>(
  tag: K,
  attrs: Record<string, string> = {},
  text?: string,
): HTMLElementTagNameMap[K] {
  const e = document.createElement(tag);
  for (const [k, v] of Object.entries(attrs)) {
    if (k === "class") e.className = v;
    else if (k === "html") e.innerHTML = v;
    else e.setAttribute(k, v);
  }
  if (text !== undefined) e.textContent = text;
  return e;
}

export function h(html: string): DocumentFragment {
  const t = document.createElement("template");
  t.innerHTML = html;
  return t.content;
}

export function clearAndAppend(root: HTMLElement, ...children: Node[]): void {
  root.innerHTML = "";
  for (const c of children) root.appendChild(c);
}

export function kv(label: string, value: string): HTMLElement {
  const row = el("div", { class: "kv-row", style: "display:contents;" });
  row.append(
    el("dt", {}, label),
    el("dd", {}, value),
  );
  return row;
}

export function kvList(pairs: Array<[string, string]>): HTMLElement {
  const dl = el("dl", { class: "kv" });
  for (const [k, v] of pairs) {
    dl.appendChild(el("dt", {}, k));
    dl.appendChild(el("dd", {}, v));
  }
  return dl;
}

export function banner(kind: "ok" | "warn" | "err", message: string): HTMLElement {
  return el("div", { class: `banner ${kind}` }, message);
}

export function copyButton(getText: () => string, label = "copy"): HTMLButtonElement {
  const btn = el("button", { class: "btn secondary", type: "button" }, label);
  btn.addEventListener("click", async () => {
    try {
      await navigator.clipboard.writeText(getText());
      const original = btn.textContent;
      btn.textContent = "copied ✓";
      setTimeout(() => { btn.textContent = original; }, 1200);
    } catch (err) {
      btn.textContent = "copy failed";
    }
  });
  return btn;
}

export function shortHex(hex: string, head = 6, tail = 4): string {
  if (hex.length <= head + tail + 3) return hex;
  return `${hex.slice(0, head)}…${hex.slice(-tail)}`;
}

export function bytesToHex(b: Uint8Array): string {
  return Array.from(b, (x) => x.toString(16).padStart(2, "0")).join("");
}

export function atomToHex(a: bigint, bytes?: number): string {
  const hex = a.toString(16);
  if (bytes) return hex.padStart(bytes * 2, "0");
  return hex.length % 2 ? "0" + hex : hex;
}
