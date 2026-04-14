import { atomToPatp } from "../protocol/patp.js";
import { newNonce } from "../signing/qr-protocol.js";
import type { SignRequest } from "../signing/qr-protocol.js";
import type { Single } from "../protocol/types.js";
import { buildAttestationLeaf, notImplementedFinalize } from "./_common.js";
import type { FinalizeResult, OpCtx, OpModule } from "./types.js";

export interface CancelEscapeArgs {
  pendingSponsor: bigint;
}

export const cancelEscapeOp: OpModule<CancelEscapeArgs> = {
  name: "cancel-escape",

  async buildRequest(args, ctx): Promise<SignRequest> {
    const skim: Single = { op: "cancel-escape", parent: args.pendingSponsor };
    buildAttestationLeaf(skim, ctx.authPubkey);
    return {
      v: 1,
      nonce: newNonce(),
      patp: ctx.patpAtom,
      op: "cancel-escape",
      summary: `Cancel escape of ${atomToPatp(ctx.patpAtom)} to ${atomToPatp(args.pendingSponsor)}`,
      authKey: ctx.authPubkey,
      sighashes: [],
    };
  },

  async finalize(_req, _sigs, _ctx): Promise<FinalizeResult> {
    return notImplementedFinalize();
  },
};
