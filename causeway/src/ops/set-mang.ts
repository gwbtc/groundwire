import type { Mang, Single } from "../protocol/types.js";
import { buildPsbtsForSkim, broadcastPair } from "./_common.js";
import type { BroadcastResult, OpCtx, OpModule, PsbtPair } from "./types.js";

export interface SetMangArgs { mang: Mang | null; }

export const setMangOp: OpModule<SetMangArgs> = {
  name: "set-mang",
  async buildPsbts(args, ctx): Promise<PsbtPair> {
    const skim: Single = { op: "set-mang", mang: args.mang };
    return buildPsbtsForSkim(skim, ctx.inscriptionUtxo, ctx);
  },
  async broadcast(c, r, ctx): Promise<BroadcastResult> { return broadcastPair(c, r, ctx.mp); },
};
