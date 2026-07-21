// Shared helpers for on-chain operation modules.
//
// All on-chain ops follow the same pattern:
//   1. Build a skim-sotx for the op (encoder.ts)
//   2. Encode to bytes
//   3. Build commit + reveal PSBTs (commit.ts / reveal.ts)
//   4. Present both PSBTs to HW wallet for signing (two QR round-trips)
//   5. Extract finalized txs and broadcast

import { encodeFull } from "../protocol/encoder.js";
import type { SkimSotx } from "../protocol/types.js";
import { buildCommit } from "../chain/commit.js";
import { buildReveal } from "../chain/reveal.js";
import { extractVerifiedPair, rawTxid } from "../signing/psbt.js";
import type { Mempool } from "../chain/mempool.js";
import type { Utxo } from "../signing/psbt.js";
import type { OpCtx, PsbtPair, BroadcastResult } from "./types.js";

export function buildPsbtsForSkim(
  skim: SkimSotx,
  funding: Utxo,
  ctx: OpCtx,
): PsbtPair {
  // On-chain unvs carry the full sotx: a [sig ship] header (sig none; the
  // point is the from-ship) before the skim. urb-core's parse-roll consumes
  // the header before the opcode, so a bare skim is unparseable and can wedge
  // urb-watcher's block processing.
  const encoded = encodeFull([{ ship: ctx.patpAtom, sig: null, skim }]);

  const commit = buildCommit({
    funding,
    fundingKey: ctx.fundingKey,
    commitKey: ctx.commitKey,
    attestationData: encoded,
    feeRate: ctx.feeRate,
  });

  const reveal = buildReveal({
    commitTxid: commit.commitTxid,
    commitOutputValue: commit.commitOutputValue,
    commitOutputScript: commit.commitOutputScript,
    commitKey: ctx.commitKey,
    destKey: ctx.destKey,
    attestationData: encoded,
    feeRate: ctx.feeRate,
  });

  return {
    commitPsbt: commit.psbt,
    revealPsbt: reveal.psbt,
  };
}

export async function broadcastPair(
  signedCommit: Uint8Array,
  signedReveal: Uint8Array,
  mp: Mempool,
): Promise<BroadcastResult> {
  // Verify the pair BEFORE broadcasting anything: the reveal must be a
  // script-path spend of the commit's output-0 (so the attestation is
  // actually published). If this throws, no commit is broadcast and no sat
  // is stranded.
  const { commitHex, revealHex, commitTxid } = extractVerifiedPair(signedCommit, signedReveal);

  // Broadcast both idempotently: a retry (e.g. after a transient reveal
  // failure, or after the whole pair already landed) must not abort just
  // because a tx is already in the mempool / already confirmed. If a broadcast
  // fails but the tx is already known to the network, treat it as success.
  const broadcastIdempotent = async (hex: string, txid: string): Promise<string> => {
    try {
      return await mp.broadcast(hex);
    } catch (err) {
      const known = await mp.tx(txid).then(() => true).catch(() => false);
      if (known) return txid;
      throw err; // genuine failure — surface it
    }
  };

  const commitBroadcastTxid = await broadcastIdempotent(commitHex, commitTxid);
  const revealTxid = await broadcastIdempotent(revealHex, revealTx(revealHex));
  return { txids: [commitBroadcastTxid, revealTxid] };
}

// The display-order txid of a raw tx hex (for the idempotent reveal check).
function revealTx(hex: string): string {
  const bytes = new Uint8Array(hex.length / 2);
  for (let i = 0; i < bytes.length; i++) bytes[i] = parseInt(hex.slice(i * 2, i * 2 + 2), 16);
  return rawTxid(bytes);
}
