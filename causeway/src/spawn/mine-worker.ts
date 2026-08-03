// Dedicated mining Web Worker. Keeps the ~65k-iteration ~daplyd search off the
// main thread so the UI stays responsive.
//
// Protocol: parent posts `{ spawn, prefix }`, worker posts `{type:"progress"}`
// periodically and eventually `{type:"result"}` or `{type:"error"}`.

import { mineSuiteC } from "./mine-c.js";
import type { SpawnSont } from "./dat.js";

interface MineMessage {
  spawn: SpawnSont;
  dom?: string;
  prefix: number | null;
}

self.onmessage = async (ev: MessageEvent<MineMessage>) => {
  const { spawn, dom, prefix } = ev.data;
  try {
    const res = await mineSuiteC({
      spawn,
      ...(dom ? { dom } : {}),
      prefix,
      onProgress: (tries) => {
        (self as unknown as Worker).postMessage({ type: "progress", tries });
      },
      // Yield more often so progress reaches the main thread smoothly.
      yieldEveryTries: 5000,
    });
    (self as unknown as Worker).postMessage({ type: "result", res });
  } catch (err: any) {
    (self as unknown as Worker).postMessage({ type: "error", error: String(err.message ?? err) });
  }
};

// Needed so the worker is treated as a module.
export {};
