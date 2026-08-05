// The web SPA's blind-opening start-height, and the bug it exists to prevent.
//
// `start-height` is the block of the transaction that CREATED the spawn
// satpoint — the FUNDING tx. The web spawn page used to hand
// `openingFromPersisted` the block height that `pollForConfirmation` returned
// for the SPAWN tx, which made every browser-minted comet unverifiable
// (`attestation-tx-not-found`, exactly as the desktop's were before
// `resolve_start_height()`). The two txids are both called "spawn txid" at
// different nesting levels, which is how the bug survived review.

import { describe, expect, test } from "vitest";
import { resolveStartHeight, type TxStatusSource } from "../src/spawn/start-height.js";

const FUNDING_TXID = "aa".repeat(32);
const SPAWN_TXID = "bb".repeat(32);
const FUNDING_HEIGHT = 900_100;
const SPAWN_HEIGHT = 900_142;   // strictly later — the value the bug used

// A mempool stand-in that knows both transactions. Asking it for the SPAWN
// txid is itself the bug, so the fake makes that answer distinguishable.
function fakeMempool(overrides: Record<string, { confirmed: boolean; block_height?: number }> = {}): TxStatusSource & { asked: string[] } {
  const table: Record<string, { confirmed: boolean; block_height?: number }> = {
    [FUNDING_TXID]: { confirmed: true, block_height: FUNDING_HEIGHT },
    [SPAWN_TXID]: { confirmed: true, block_height: SPAWN_HEIGHT },
    ...overrides,
  };
  const asked: string[] = [];
  return {
    asked,
    async tx(txid: string) {
      asked.push(txid);
      const status = table[txid];
      if (!status) throw new Error(`mempool tx ${txid}: 404`);
      return { status };
    },
  };
}

// The persisted opening as the spawn page writes it. `spawnTxidHex` here is
// the spawn SATPOINT's txid, i.e. the FUNDING outpoint.
function opening(extra: Record<string, unknown> = {}) {
  return { spawnTxidHex: FUNDING_TXID, ...extra } as any;
}

describe("resolveStartHeight", () => {
  test("looks up the FUNDING tx, never the spawn tx", async () => {
    const mp = fakeMempool();
    const h = await resolveStartHeight(opening(), mp);
    expect(h).toBe(FUNDING_HEIGHT);
    expect(h).not.toBe(SPAWN_HEIGHT);
    expect(mp.asked).toEqual([FUNDING_TXID]);
  });

  test("prefers a recorded funding height over a network lookup", async () => {
    const mp = fakeMempool();
    const h = await resolveStartHeight(opening({ fundingHeight: FUNDING_HEIGHT }), mp);
    expect(h).toBe(FUNDING_HEIGHT);
    expect(mp.asked).toEqual([]);
  });

  test("an explicit startHeight wins over everything", async () => {
    const mp = fakeMempool();
    const h = await resolveStartHeight(
      opening({ startHeight: 12_345, fundingHeight: FUNDING_HEIGHT }), mp,
    );
    expect(h).toBe(12_345);
    expect(mp.asked).toEqual([]);
  });

  test("0 is the publication placeholder, not a height", async () => {
    const mp = fakeMempool();
    const h = await resolveStartHeight(
      opening({ startHeight: 0, fundingHeight: 0 }), mp,
    );
    expect(h).toBe(FUNDING_HEIGHT);
    expect(mp.asked).toEqual([FUNDING_TXID]);
  });

  test("refuses to guess when the funding tx is unconfirmed", async () => {
    const mp = fakeMempool({ [FUNDING_TXID]: { confirmed: false } });
    await expect(resolveStartHeight(opening(), mp)).rejects.toThrow(/not confirmed/);
  });

  test("refuses to guess when the funding tx is unknown", async () => {
    const mp = fakeMempool({});
    delete (mp as any).nothing;
    const empty: TxStatusSource = { async tx() { throw new Error("404"); } };
    await expect(resolveStartHeight(opening(), empty)).rejects.toThrow(/start-height/);
  });

  test("refuses to guess with no funding txid at all", async () => {
    const mp = fakeMempool();
    await expect(resolveStartHeight(opening({ spawnTxidHex: "" }), mp))
      .rejects.toThrow(/no funding txid/);
  });
});
