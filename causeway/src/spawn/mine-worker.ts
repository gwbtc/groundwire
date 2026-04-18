// Dedicated mining Web Worker. Keeps the ~65k-iteration ~daplyd search off the
// main thread so the UI stays responsive.
//
// Protocol: parent posts `{ tweak, prefix }`, worker posts `{type:"progress"}`
// periodically and eventually `{type:"result"}` or `{type:"error"}`.

import { mineSuiteC } from "./mine-c.js";

interface MineMessage {
  tweak: Uint8Array;
  prefix: number | null;
}

self.onmessage = async (ev: MessageEvent<MineMessage>) => {
  const { tweak, prefix } = ev.data;
  try {
    const res = await mineSuiteC({
      tweak,
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
