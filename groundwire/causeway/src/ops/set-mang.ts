import { atomToPatp } from "../protocol/patp.js";
import { newNonce } from "../signing/qr-protocol.js";
import type { SignRequest } from "../signing/qr-protocol.js";
import type { Mang, Single } from "../protocol/types.js";
import { buildAttestationLeaf, notImplementedFinalize } from "./_common.js";
import type { FinalizeResult, OpCtx, OpModule } from "./types.js";

export interface SetMangArgs { mang: Mang | null; }

export const setMangOp: OpModule<SetMangArgs> = {
  name: "set-mang",
  async buildRequest(args, ctx): Promise<SignRequest> {
    const skim: Single = { op: "set-mang", mang: args.mang };
    buildAttestationLeaf(skim, ctx.authPubkey);
    const desc = args.mang === null ? "clear" : `type=${args.mang.type}`;
    return {
      v: 1, nonce: newNonce(), patp: ctx.patpAtom, op: "set-mang",
      summary: `Set-mang for ${atomToPatp(ctx.patpAtom)}: ${desc}`,
      authKey: ctx.authPubkey, sighashes: [],
    };
  },
  async finalize(_r, _s, _c): Promise<FinalizeResult> { return notImplementedFinalize(); },
};
