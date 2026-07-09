import "./ui/styles.css";
import { initUrbCore } from "./wasm/urb-core.js";
import urbWasmUrl from "./wasm/urb_wasm.wasm?url";

// Load the shared urb/ wasm core before the app boots, so the consensus-critical
// %spawn encoding runs through the same Rust the Passport signs with (see
// protocol/encoder.ts). If it can't load, encoding falls back to the TS path.
try {
  const bytes = await (await fetch(urbWasmUrl)).arrayBuffer();
  await initUrbCore(bytes);
} catch (err) {
  console.warn("urb wasm core unavailable; using TS encoder fallback", err);
}

await import("./ui/app.js");
