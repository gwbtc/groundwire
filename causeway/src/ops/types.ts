// kelvin-9 on-chain management. Under the OP_RETURN revision, sponsorship /
// escape / adopt / reject / detach / fief / set-mang all leave the consensus
// layer (they are off-chain, wire-borne state — spec §8). The ONLY on-chain
// management op that survives is a state UPDATE: a rekey/breach that spends the
// sat-carrying UTXO key-path and re-commits a new snapshot in output 0.

import type { Mempool } from "../chain/mempool.js";
import type { KeyInfo, Utxo } from "../signing/psbt.js";
import type { Snapshot } from "../spawn/snapshot.js";

export type OpName = "rekey";

export interface StateUpdateCtx {
  current: Utxo;                 // the sat-carrying UTXO to spend (input 0)
  ownerKey: KeyInfo;             // key controlling `current` (signs input 0)
  internalKey33: Uint8Array;     // 33-byte compressed internal key P
  currentSnapshot: Snapshot;     // committed in `current` (gives the input merkle root)
  feeRate: number;
  mp: Mempool;
  // Deliberately allow a state update whose new snapshot has neither a
  // sponsor nor a fief (an outbound-only identity). Default off: Causeway
  // refuses to build one, because nothing can cold-contact the result.
  noRoute?: boolean;
}

export interface BuiltStateUpdate {
  psbt: Uint8Array;
  txidHex: string;               // predicted display-order txid
  newSnapshot: Snapshot;
}

export interface BroadcastResult {
  txids: string[];
}

export interface OpModule<Args> {
  readonly name: OpName;
  build(args: Args, ctx: StateUpdateCtx): BuiltStateUpdate;
  // `built` is REQUIRED, not optional, so that a caller cannot broadcast
  // without saying what it thinks it is broadcasting. A segwit txid does
  // not change under signing, so built.txidHex must equal the signed
  // transaction's txid; if it does not, the signer returned a different
  // transaction and the identity sat is going somewhere we did not choose.
  broadcast(
    signedPsbt: Uint8Array, ctx: StateUpdateCtx, built: BuiltStateUpdate,
  ): Promise<BroadcastResult>;
}
