// Login is a challenge-response: sign sha256("causeway-login/v1" || nonce ||
// patp || block-id-hax) with the point's current taproot key.
//
// Not an on-chain op — no tx, no broadcast. Pure proof of key control.

import { sha256 } from "@noble/hashes/sha256";
import { concatBytes } from "@noble/hashes/utils";
import { newNonce } from "../signing/qr-protocol.js";
import type { SignRequest } from "../signing/qr-protocol.js";
import { atomToPatp } from "../protocol/patp.js";
import type { OpCtx, OpModule, FinalizeResult } from "./types.js";

const TAG = new TextEncoder().encode("causeway-login/v1");

function u128LE(a: bigint): Uint8Array {
  const out = new Uint8Array(16);
  for (let i = 0; i < 16; i++) out[i] = Number((a >> BigInt(8 * i)) & 0xffn);
  return out;
}

function u256LE(a: bigint): Uint8Array {
  const out = new Uint8Array(32);
  for (let i = 0; i < 32; i++) out[i] = Number((a >> BigInt(8 * i)) & 0xffn);
  return out;
}

export function loginChallenge(nonce: Uint8Array, patpAtom: bigint, blockHax: bigint): Uint8Array {
  return sha256(concatBytes(TAG, nonce, u128LE(patpAtom), u256LE(blockHax)));
}

export const loginOp: OpModule<undefined> = {
  name: "login",

  async buildRequest(_args, ctx): Promise<SignRequest> {
    const nonce = newNonce();
    const hash = loginChallenge(nonce, ctx.patpAtom, ctx.state.blockId.hax);
    return {
      v: 1,
      nonce,
      patp: ctx.patpAtom,
      op: "login",
      summary: `Log in as ${atomToPatp(ctx.patpAtom)}`,
      authKey: ctx.authPubkey,
      sighashes: [
        { kind: "login", hash, txBrief: { inputs: [], outputs: [], lockTime: 0 } },
      ],
    };
  },

  async finalize(_req, _sigs, _ctx): Promise<FinalizeResult> {
    return { txids: [] };
  },
};
