import type { Single } from "../protocol/types.js";
import { buildPsbtsForSkim, broadcastPair } from "./_common.js";
import type { BroadcastResult, OpCtx, OpModule, PsbtPair } from "./types.js";

export interface RejectArgs { child: bigint; }

export const rejectOp: OpModule<RejectArgs> = {
  name: "reject",
  async buildPsbts(args, ctx): Promise<PsbtPair> {
    const skim: Single = { op: "reject", ship: args.child };
    return buildPsbtsForSkim(skim, ctx.inscriptionUtxo, ctx);
  },
  async broadcast(c, r, ctx): Promise<BroadcastResult> { return broadcastPair(c, r, ctx.mp); },
};
