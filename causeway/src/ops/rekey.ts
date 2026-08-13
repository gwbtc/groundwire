// rekey — rotate the networking (messaging) key, optionally with a breach.
//
// The signing key that fixes the @p is immutable; only the encryption half
// (snapshot.key = cry.pub) rotates. Every snapshot change increments `life`; a
// breach also increments `rift` (decisions-addendum §2). This is the sole
// on-chain management op that survives under the OP_RETURN revision.

import { buildStateUpdate, broadcastStateUpdate } from "./_common.js";
import type { OpModule, StateUpdateCtx, BuiltStateUpdate, BroadcastResult } from "./types.js";
import type { Snapshot } from "../spawn/snapshot.js";

export interface RekeyArgs {
  newKey: bigint;   // new messaging key (cry.pub of the new suite-C pass)
  breach: boolean;
  // Sponsor for the NEW snapshot. `undefined` carries the current one
  // forward; an explicit `bigint` sets it; explicit `null` clears it (which
  // needs ctx.noRoute unless a fief is present).
  sponsor?: bigint | null;
}

export const rekeyOp: OpModule<RekeyArgs> = {
  name: "rekey",
  build(args, ctx): BuiltStateUpdate {
    const newSnapshot: Snapshot = {
      life: ctx.currentSnapshot.life + 1,
      rift: ctx.currentSnapshot.rift + (args.breach ? 1 : 0),
      key: args.newKey,
      sponsor: args.sponsor === undefined ? ctx.currentSnapshot.sponsor : args.sponsor,
      fief: ctx.currentSnapshot.fief,
    };
    return buildStateUpdate(newSnapshot, ctx);
  },
  async broadcast(signedPsbt, ctx, built): Promise<BroadcastResult> {
    return broadcastStateUpdate(signedPsbt, ctx.mp, built.txidHex);
  },
};
