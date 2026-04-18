import type { Single } from "../protocol/types.js";
import { buildPsbtsForSkim, broadcastPair } from "./_common.js";
import type { BroadcastResult, OpCtx, OpModule, PsbtPair } from "./types.js";

export interface EscapeArgs { newSponsor: bigint; sponsorSig: bigint | null; }

export const escapeOp: OpModule<EscapeArgs> = {
  name: "escape",
  async buildPsbts(args, ctx): Promise<PsbtPair> {
    const skim: Single = { op: "escape", parent: args.newSponsor, sig: args.sponsorSig };
    return buildPsbtsForSkim(skim, ctx.inscriptionUtxo, ctx);
  },
  async broadcast(c, r, ctx): Promise<BroadcastResult> { return broadcastPair(c, r, ctx.mp); },
};
