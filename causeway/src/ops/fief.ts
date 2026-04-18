import type { Fief, Single } from "../protocol/types.js";
import { buildPsbtsForSkim, broadcastPair } from "./_common.js";
import type { BroadcastResult, OpCtx, OpModule, PsbtPair } from "./types.js";

export interface FiefArgs { fief: Fief | null; }

export const fiefOp: OpModule<FiefArgs> = {
  name: "fief",
  async buildPsbts(args, ctx): Promise<PsbtPair> {
    const skim: Single = { op: "fief", fief: args.fief };
    return buildPsbtsForSkim(skim, ctx.inscriptionUtxo, ctx);
  },
  async broadcast(c, r, ctx): Promise<BroadcastResult> { return broadcastPair(c, r, ctx.mp); },
};
