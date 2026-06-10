// Shared helpers for on-chain operation modules.
//
// All on-chain ops follow the same pattern:
//   1. Build a skim-sotx for the op (encoder.ts)
//   2. Encode to bytes
//   3. Build commit + reveal PSBTs (commit.ts / reveal.ts)
//   4. Present both PSBTs to HW wallet for signing (two QR round-trips)
//   5. Extract finalized txs and broadcast

import { encodeSkim } from "../protocol/encoder.js";
import type { SkimSotx } from "../protocol/types.js";
import { buildCommit } from "../chain/commit.js";
import { buildReveal } from "../chain/reveal.js";
import { extractTxHex } from "../signing/psbt.js";
import type { Mempool } from "../chain/mempool.js";
import type { Utxo } from "../signing/psbt.js";
import type { OpCtx, PsbtPair, BroadcastResult } from "./types.js";

export function buildPsbtsForSkim(
  skim: SkimSotx,
  funding: Utxo,
  ctx: OpCtx,
): PsbtPair {
  const encoded = encodeSkim(skim);

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
  const commitHex = extractTxHex(signedCommit);
  const commitTxid = await mp.broadcast(commitHex);

  const revealHex = extractTxHex(signedReveal);
  const revealTxid = await mp.broadcast(revealHex);

  return { txids: [commitTxid, revealTxid] };
}
