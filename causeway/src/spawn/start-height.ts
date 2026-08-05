// The `start-height` of a spawn's blind-opening — the web twin of the
// desktop's `resolve_start_height()` in causeway/desktop/causeway.py.
//
// sur/self-attestation.hoon defines start-height as "the block containing the
// transaction that CREATED the spawn satpoint" — i.e. the FUNDING tx, the one
// whose output the spawn transaction spends. It is the FIRST transaction the
// verifier fetches (`+verify-lc` in lib/lc-attestation), and the light client
// can only address a transaction by [height txid] — there is no lookup by bare
// txid. So a wrong height is not cosmetic: the fetch fails with
// `attestation-tx-not-found` and the attestation dies with no verdict at all.
//
// It is emphatically NOT the spawn transaction's own height. The web spawn
// page used to pass exactly that (the height `pollForConfirmation` returned for
// `spawnTxidHex`), which made EVERY comet minted through the browser
// unverifiable — the same bug the desktop had before `resolve_start_height`.
// The two fields are confusingly both called "spawn txid" at different nesting
// levels: `PersistedSpawn.spawnTxidHex` is the spawn TX, while
// `PersistedSpawn.opening.spawnTxidHex` is the spawn SATPOINT's txid, i.e. the
// funding outpoint. This module always uses the latter.
//
// Order of preference, mirroring the desktop exactly:
//   1. an explicit `startHeight` recorded on the opening
//   2. the `fundingHeight` the UTXO scan recorded at spawn time
//   3. a mempool.space lookup of the funding txid
//   4. hard error — never a silent default, never the spawn height
// `0` is the pre-broadcast placeholder used by on-chain PUBLICATIONS (see
// decisions-addendum §4b) and is treated as unset, never as a real height.

import type { PersistedOpening } from "./persist.js";

// Just enough of the Mempool client to look a transaction up; keeps this
// module testable without a network or a DOM.
export interface TxStatusSource {
  tx(txid: string): Promise<{ status: { confirmed: boolean; block_height?: number } }>;
}

export type StartHeightInput = Pick<
  PersistedOpening,
  "startHeight" | "fundingHeight" | "spawnTxidHex"
>;

function recorded(candidate: number | undefined | null): number | null {
  if (candidate === undefined || candidate === null) return null;
  if (!Number.isFinite(candidate) || candidate <= 0) return null;  // 0 = unset
  return Math.trunc(candidate);
}

export async function resolveStartHeight(
  o: StartHeightInput,
  mp: TxStatusSource,
): Promise<number> {
  for (const candidate of [o.startHeight, o.fundingHeight]) {
    const h = recorded(candidate);
    if (h !== null) return h;
  }
  const txid = (o.spawnTxidHex ?? "").replace(/^0x/, "");
  if (!txid) {
    throw new Error(
      "opening carries no funding txid, so the spawn's start-height "
      + "(the funding tx's block) cannot be determined",
    );
  }
  let status: { confirmed: boolean; block_height?: number };
  try {
    status = (await mp.tx(txid)).status;
  } catch (err: any) {
    throw new Error(
      `could not look up funding tx ${txid} to establish the spawn's `
      + `start-height: ${err?.message ?? err}`,
    );
  }
  const h = recorded(status?.block_height);
  if (!status?.confirmed || h === null) {
    throw new Error(
      `funding tx ${txid} is not confirmed; its block height is the `
      + "blind-opening's start-height and must not be guessed",
    );
  }
  return h;
}
