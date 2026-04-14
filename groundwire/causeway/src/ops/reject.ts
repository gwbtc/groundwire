import { atomToPatp } from "../protocol/patp.js";
import { newNonce } from "../signing/qr-protocol.js";
import type { SignRequest } from "../signing/qr-protocol.js";
import type { Single } from "../protocol/types.js";
import { buildAttestationLeaf, notImplementedFinalize } from "./_common.js";
import type { FinalizeResult, OpCtx, OpModule } from "./types.js";

export interface RejectArgs { child: bigint; }

export const rejectOp: OpModule<RejectArgs> = {
  name: "reject",
  async buildRequest(args, ctx): Promise<SignRequest> {
    const skim: Single = { op: "reject", ship: args.child };
    buildAttestationLeaf(skim, ctx.authPubkey);
    return {
      v: 1, nonce: newNonce(), patp: ctx.patpAtom, op: "reject",
      summary: `${atomToPatp(ctx.patpAtom)} rejects ${atomToPatp(args.child)}`,
      authKey: ctx.authPubkey, sighashes: [],
    };
  },
  async finalize(_r, _s, _c): Promise<FinalizeResult> { return notImplementedFinalize(); },
};
