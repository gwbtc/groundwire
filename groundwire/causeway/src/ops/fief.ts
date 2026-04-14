import { atomToPatp } from "../protocol/patp.js";
import { newNonce } from "../signing/qr-protocol.js";
import type { SignRequest } from "../signing/qr-protocol.js";
import type { Fief, Single } from "../protocol/types.js";
import { buildAttestationLeaf, notImplementedFinalize } from "./_common.js";
import type { FinalizeResult, OpCtx, OpModule } from "./types.js";

export interface FiefArgs { fief: Fief | null; }

export const fiefOp: OpModule<FiefArgs> = {
  name: "fief",
  async buildRequest(args, ctx): Promise<SignRequest> {
    const skim: Single = { op: "fief", fief: args.fief };
    buildAttestationLeaf(skim, ctx.authPubkey);
    const where = args.fief === null ? "clear" :
      args.fief.type === "if" ? `${args.fief.ip}:${args.fief.port}` :
      args.fief.type === "is" ? `[${args.fief.ip}]:${args.fief.port}` : "turf";
    return {
      v: 1, nonce: newNonce(), patp: ctx.patpAtom, op: "fief",
      summary: `Set fief for ${atomToPatp(ctx.patpAtom)}: ${where}`,
      authKey: ctx.authPubkey, sighashes: [],
    };
  },
  async finalize(_r, _s, _c): Promise<FinalizeResult> { return notImplementedFinalize(); },
};
