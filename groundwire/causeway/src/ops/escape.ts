import { atomToPatp } from "../protocol/patp.js";
import { newNonce } from "../signing/qr-protocol.js";
import type { SignRequest } from "../signing/qr-protocol.js";
import type { Single } from "../protocol/types.js";
import { buildAttestationLeaf, notImplementedFinalize } from "./_common.js";
import type { FinalizeResult, OpCtx, OpModule } from "./types.js";

export interface EscapeArgs {
  newSponsor: bigint;          // @p of the new sponsor
  sponsorSig: bigint | null;   // optional 512-bit sponsor signature
}

export const escapeOp: OpModule<EscapeArgs> = {
  name: "escape",

  async buildRequest(args, ctx): Promise<SignRequest> {
    const skim: Single = { op: "escape", parent: args.newSponsor, sig: args.sponsorSig };
    buildAttestationLeaf(skim, ctx.authPubkey);
    return {
      v: 1,
      nonce: newNonce(),
      patp: ctx.patpAtom,
      op: "escape",
      summary: `Escape ${atomToPatp(ctx.patpAtom)} → ${atomToPatp(args.newSponsor)}`,
      authKey: ctx.authPubkey,
      sighashes: [],
    };
  },

  async finalize(_req, _sigs, _ctx): Promise<FinalizeResult> {
    return notImplementedFinalize();
  },
};
