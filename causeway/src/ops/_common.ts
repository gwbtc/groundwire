// Shared helpers for kelvin-9 on-chain state updates.
//
// A state update is ONE transaction: spend the current sat-carrying UTXO
// key-path (BIP-371: tapInternalKey + tapMerkleRoot = the CURRENT state leaf
// hash) and send output 0 to the new sat-carrying P2TR key Q committing the new
// snapshot. No commit/reveal pair, no attestation leaf on-chain.

import { buildSpawnPsbt, extractTx, rawTxid } from "../signing/psbt.js";
import {
  assertRoutable, stateMerkleRoot, stateOutputKey, stateOutputScript, type Snapshot,
} from "../spawn/snapshot.js";
import type { Mempool } from "../chain/mempool.js";
import type { StateUpdateCtx, BuiltStateUpdate, BroadcastResult } from "./types.js";

export function buildStateUpdate(newSnapshot: Snapshot, ctx: StateUpdateCtx): BuiltStateUpdate {
  // A state update that leaves the comet with neither a sponsor nor a fief
  // makes it unroutable from that life onward. Refuse unless opted out.
  assertRoutable(newSnapshot, ctx.noRoute ?? false);
  const inputMerkleRoot = stateMerkleRoot(ctx.currentSnapshot);
  const outputScript = stateOutputScript(stateOutputKey(ctx.internalKey33, newSnapshot));
  const built = buildSpawnPsbt({
    funding: ctx.current,
    fundingKey: ctx.ownerKey,
    outputScript,
    inputMerkleRoot,
    feeRate: ctx.feeRate,
  });
  return { psbt: built.psbt, txidHex: built.txidHex, newSnapshot };
}

function bytesToHex(b: Uint8Array): string {
  return Array.from(b, (x) => x.toString(16).padStart(2, "0")).join("");
}

export async function broadcastStateUpdate(
  signedPsbt: Uint8Array, mp: Mempool, expectTxidHex: string,
): Promise<BroadcastResult> {
  const raw = extractTx(signedPsbt);
  const hex = bytesToHex(raw);
  const txid = rawTxid(raw);
  // Refuse to broadcast a transaction we did not build.
  //
  // This txid was already being computed — for the idempotent-retry path
  // below — and never compared to anything. A segwit txid commits to
  // every input and output but NOT the witness, so signing cannot change
  // it; a mismatch means the signer returned a different transaction,
  // spending or paying somewhere we did not choose.
  if (txid !== expectTxidHex) {
    throw new Error(
      `refusing to broadcast: signed transaction is not the one built `
      + `(built ${expectTxidHex}, signed ${txid}). A segwit txid does not `
      + `change when a transaction is signed, so these differ only if the `
      + `inputs or outputs differ.`,
    );
  }
  // Idempotent broadcast: a retry of an already-known tx must not fail.
  let broadcastTxid: string;
  try {
    broadcastTxid = await mp.broadcast(hex);
  } catch (err) {
    const known = await mp.tx(txid).then(() => true).catch(() => false);
    if (!known) throw err;
    broadcastTxid = txid;
  }
  return { txids: [broadcastTxid] };
}
