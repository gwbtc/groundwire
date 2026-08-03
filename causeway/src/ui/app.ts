import { renderLanding } from "./pages/landing.js";
import { renderKeys } from "./pages/keys.js";
import { renderDashboard } from "./pages/dashboard.js";
import { renderOp } from "./pages/op.js";
import { renderSpawn } from "./pages/spawn.js";

async function route(): Promise<void> {
  const root = document.getElementById("app");
  if (!root) return;
  // Normalize: strip optional query-string portion before matching.
  const fullHash = window.location.hash || "#/";
  const hash = fullHash.split("?")[0]!;

  try {
    if (hash === "#/" || hash === "#/login") { await renderLanding(root); return; }
    if (hash === "#/keys") { await renderKeys(root); return; }
    if (hash === "#/dash") { renderDashboard(root); return; }
    if (hash === "#/spawn") { renderSpawn(root); return; }
    if (hash.startsWith("#/op/")) { renderOp(root, hash.slice(5)); return; }
    root.textContent = `unknown route: ${hash}`;
  } catch (err: any) {
    root.innerHTML = `<section class="card"><h1>Error</h1><p class="lead">${err.message ?? err}</p></section>`;
    console.error(err);
  }
}

window.addEventListener("hashchange", () => { route().catch(console.error); });
route().catch(console.error);

export {};
