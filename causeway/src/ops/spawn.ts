// Spawn: bring a new @p into existence.
//
// Flow:
//   1. User picks a precommit UTXO (txid/vout/off).
//   2. We compute the tweak bytes (spawn/tweak.ts) — these identify the sat
//      that will own the new comet.
//   3. Miner produces {comet, pass, feed, …}. The pass encodes the tweak, so
//      urb-core will accept the @p only if it matches this tweak.
//   4. We emit a spawn sotx (optionally wrapped in a batch with %escape), build
//      commit+reveal PSBTs, and the user signs them like any other op.

import type { Fief, SkimSotx } from "../protocol/types.js";
import { miner } from "../spawn/miner.js";
import { buildTweakBytes } from "../spawn/tweak.js";
import { buildPsbtsForSkim, broadcastPair } from "./_common.js";
import type { BroadcastResult, OpCtx, OpModule, PsbtPair } from "./types.js";

export interface SpawnArgs {
  precommit: {
    txidHex: string;     // display-order hex
    vout: number;
    off: number;
  };
  spkh: Uint8Array;
  tej: bigint;
  fief: Fief | null;
  escapeSponsor?: bigint;
  escapeSponsorSig?: bigint;
  prefix?: number | null;
  onProgress?: (tries: number) => void;
}

export interface SpawnResult {
  comet: bigint;
  feed: Uint8Array;
  pass: bigint;
  pair: PsbtPair;
  tries: number;
}

export const spawnOp: OpModule<SpawnArgs> & {
  spawn(args: SpawnArgs, ctx: OpCtx): Promise<SpawnResult>;
} = {
  name: "spawn",

  async buildPsbts(args, ctx): Promise<PsbtPair> {
    return (await this.spawn(args, ctx)).pair;
  },

  async spawn(args, ctx): Promise<SpawnResult> {
    const tweak = buildTweakBytes({
      txidHex: args.precommit.txidHex,
      vout: args.precommit.vout,
      off: args.precommit.off,
    });

    const mined = await miner.mine({
      tweakExpr: tweak,
      prefix: args.prefix ?? null,
      ...(args.onProgress ? { onProgress: args.onProgress } : {}),
    });

    const spawnSingle = {
      op: "spawn" as const,
      pass: mined.pass,
      fief: args.fief,
      to: {
        spkh: args.spkh,
        off: BigInt(args.precommit.off),
        tej: args.tej,
        vout: args.precommit.vout === 0 ? null : BigInt(args.precommit.vout),
      },
    };

    const skim: SkimSotx = args.escapeSponsor !== undefined
      ? { op: "batch", items: [
          spawnSingle,
          { op: "escape", parent: args.escapeSponsor, sig: args.escapeSponsorSig ?? null },
        ] }
      : spawnSingle;

    const pair = buildPsbtsForSkim(skim, ctx.inscriptionUtxo, ctx);
    return { comet: mined.comet, feed: mined.feed, pass: mined.pass, pair, tries: mined.tries };
  },

  async broadcast(c, r, ctx): Promise<BroadcastResult> { return broadcastPair(c, r, ctx.mp); },
};
