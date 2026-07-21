import type { Single } from "../protocol/types.js";
import { buildPsbtsForSkim, broadcastPair } from "./_common.js";
import type { BroadcastResult, OpCtx, OpModule, PsbtPair } from "./types.js";

export interface CancelEscapeArgs { pendingSponsor: bigint; }

export const cancelEscapeOp: OpModule<CancelEscapeArgs> = {
  name: "cancel-escape",
  async buildPsbts(args, ctx): Promise<PsbtPair> {
    const skim: Single = { op: "cancel-escape", parent: args.pendingSponsor };
    return buildPsbtsForSkim(skim, ctx.inscriptionUtxo, ctx);
  },
  async broadcast(c, r, ctx): Promise<BroadcastResult> { return broadcastPair(c, r, ctx.mp); },
};
