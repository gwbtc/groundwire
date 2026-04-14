import { atomToPatp } from "../protocol/patp.js";
import { newNonce } from "../signing/qr-protocol.js";
import type { SignRequest } from "../signing/qr-protocol.js";
import type { Single } from "../protocol/types.js";
import { buildAttestationLeaf, notImplementedFinalize } from "./_common.js";
import type { FinalizeResult, OpCtx, OpModule } from "./types.js";

export interface RekeyArgs {
  newPass: bigint;
  breach: boolean;
}

export const rekeyOp: OpModule<RekeyArgs> = {
  name: "rekey",

  async buildRequest(args, ctx): Promise<SignRequest> {
    const skim: Single = { op: "keys", pass: args.newPass, breach: args.breach };
    buildAttestationLeaf(skim, ctx.authPubkey);
    // TODO: plug in commit/reveal builders to produce actual sighashes.
    return {
      v: 1,
      nonce: newNonce(),
      patp: ctx.patpAtom,
      op: "rekey",
      summary: `Rekey ${atomToPatp(ctx.patpAtom)}${args.breach ? " (breach)" : ""}`,
      authKey: ctx.authPubkey,
      sighashes: [],
    };
  },

  async finalize(_req, _sigs, _ctx): Promise<FinalizeResult> {
    return notImplementedFinalize();
  },
};
