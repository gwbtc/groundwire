// Comet miner interface + default Web-Worker-backed implementation.
//
// The star constraint (~daplyd) makes mining a ~65k-iteration search; running
// it on the main thread would freeze the UI for 5-20s. We spawn a dedicated
// worker per mine() call instead.

import { mineSuiteC } from "./mine-c.js";
import type { MineResult as RawResult } from "./mine-c.js";

export interface MineParams {
  tweakExpr: Uint8Array;
  parent?: bigint;
  prefix?: number | null;
  onProgress?: (tries: number) => void;
}

export interface MineResult {
  comet: bigint;
  feed: Uint8Array;
  ring: Uint8Array;
  seed: Uint8Array;
  pass: bigint;
  sPub: Uint8Array;
  cPub: Uint8Array;
  tries: number;
}

export interface CometMiner {
  mine(params: MineParams): Promise<MineResult>;
}

function bytesToAtomLE(b: Uint8Array): bigint {
  let x = 0n;
  for (let i = b.length - 1; i >= 0; i--) x = (x << 8n) | BigInt(b[i]!);
  return x;
}

function toPublicResult(r: RawResult): MineResult {
  return {
    comet: bytesToAtomLE(r.comet),
    feed: r.feed,
    ring: r.ringAtomBytes,
    seed: r.seed,
    pass: r.pass,
    sPub: r.sPub,
    cPub: r.cPub,
    tries: r.tries,
  };
}

// Worker-backed miner. Preferred in the browser.
const workerMiner: CometMiner = {
  async mine({ tweakExpr, prefix, onProgress }) {
    // Vite resolves the worker URL at build time via import.meta.url.
    const worker = new Worker(new URL("./mine-worker.ts", import.meta.url), {
      type: "module",
    });
    return new Promise<MineResult>((resolve, reject) => {
      worker.onmessage = (ev: MessageEvent<any>) => {
        const msg = ev.data;
        if (msg.type === "progress") {
          onProgress?.(msg.tries);
        } else if (msg.type === "result") {
          worker.terminate();
          resolve(toPublicResult(msg.res));
        } else if (msg.type === "error") {
          worker.terminate();
          reject(new Error(msg.error));
        }
      };
      worker.onerror = (err) => {
        worker.terminate();
        reject(err);
      };
      worker.postMessage({ tweak: tweakExpr, prefix: prefix ?? null });
    });
  },
};

// Main-thread fallback — used in tests and environments without Worker.
const mainThreadMiner: CometMiner = {
  async mine({ tweakExpr, prefix, onProgress }) {
    const res = await mineSuiteC({
      tweak: tweakExpr,
      prefix: prefix ?? null,
      ...(onProgress ? { onProgress } : {}),
    });
    return toPublicResult(res);
  },
};

const canUseWorker =
  typeof Worker !== "undefined" &&
  typeof window !== "undefined";

let current: CometMiner = canUseWorker ? workerMiner : mainThreadMiner;

export const miner: CometMiner = {
  mine: (p) => current.mine(p),
};

export function setMiner(m: CometMiner): void { current = m; }
