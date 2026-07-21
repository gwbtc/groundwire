import type { Single } from "../protocol/types.js";
import { buildPsbtsForSkim, broadcastPair } from "./_common.js";
import type { BroadcastResult, OpCtx, OpModule, PsbtPair } from "./types.js";

export interface RekeyArgs { newPass: bigint; breach: boolean; }

export const rekeyOp: OpModule<RekeyArgs> = {
  name: "rekey",
  async buildPsbts(args, ctx): Promise<PsbtPair> {
    const skim: Single = { op: "keys", pass: args.newPass, breach: args.breach };
    return buildPsbtsForSkim(skim, ctx.inscriptionUtxo, ctx);
  },
  async broadcast(c, r, ctx): Promise<BroadcastResult> { return broadcastPair(c, r, ctx.mp); },
};
