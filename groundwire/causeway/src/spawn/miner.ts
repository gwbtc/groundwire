// Native comet-miner interface. The real implementation is a native helper
// (C/Zig) provided out-of-band; this module just defines the interface and a
// stub the UI can surface until the helper is installed.

export interface MineParams {
  tweakExpr: Uint8Array;
  parent?: bigint;
}

export interface MineResult {
  comet: bigint;
  feed: Uint8Array;
  ring: Uint8Array;
  seed: Uint8Array;
}

export interface CometMiner {
  mine(params: MineParams): Promise<MineResult>;
}

const stub: CometMiner = {
  async mine() {
    throw new Error(
      "comet-miner not installed. Causeway needs a native helper to spawn new comets.",
    );
  },
};

let current: CometMiner = stub;

export const miner: CometMiner = {
  mine: (p) => current.mine(p),
};

export function setMiner(m: CometMiner): void {
  current = m;
}
