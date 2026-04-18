import type { Mempool } from "../chain/mempool.js";
import type { Rpc } from "../chain/rpc.js";
import type { UrbState } from "../oracle/state.js";
import type { KeyInfo, Utxo } from "../signing/psbt.js";

export type OpName =
  | "spawn" | "rekey" | "escape" | "cancel-escape"
  | "adopt" | "reject" | "detach" | "fief" | "set-mang";

export interface OpCtx {
  state: UrbState;
  patpAtom: bigint;
  inscriptionUtxo: Utxo;  // UTXO currently holding the ownership sat
  fundingKey: KeyInfo;     // key at index 0 — signs commit input (= inscription UTXO)
  commitKey: KeyInfo;      // key at index 1 — internal key for commit output, signs reveal
  destKey: KeyInfo;        // key at index 2 — internal key for reveal destination
  feeRate: number;
  mp: Mempool;
  rpc?: Rpc;
}

export interface PsbtPair {
  commitPsbt: Uint8Array;
  revealPsbt: Uint8Array;
}

export interface BroadcastResult {
  txids: string[];
}

export interface OpModule<Args> {
  readonly name: OpName;
  buildPsbts(args: Args, ctx: OpCtx): Promise<PsbtPair>;
  broadcast(signedCommit: Uint8Array, signedReveal: Uint8Array, ctx: OpCtx): Promise<BroadcastResult>;
}
