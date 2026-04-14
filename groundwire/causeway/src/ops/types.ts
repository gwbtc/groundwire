import type { Mempool } from "../chain/mempool.js";
import type { Rpc } from "../chain/rpc.js";
import type { UrbState } from "../oracle/state.js";
import type { OpName, SignRequest } from "../signing/qr-protocol.js";

export interface OpCtx {
  state: UrbState;
  patpAtom: bigint;
  authPubkey: Uint8Array;  // tweaked xonly for key-path verification
  feeRate: number;         // sat/vB
  mp: Mempool;
  rpc?: Rpc;
}

export interface FinalizeResult {
  txids: string[];
}

export interface OpModule<Args> {
  readonly name: OpName;
  buildRequest(args: Args, ctx: OpCtx): Promise<SignRequest>;
  finalize(req: SignRequest, sigs: Uint8Array[], ctx: OpCtx): Promise<FinalizeResult>;
}
