import { describe, expect, test } from "vitest";
import {
  collectSontMapKeys, mempoolTxidToAtomHex, addressToScriptPubKey, networkFor,
} from "../src/chain/select.js";
import type { Noun } from "../src/oracle/noun.js";

// Build a synthetic sont-map treap: [[[txid vout] value] [left right]]
function makeSontMapEntry(txid: bigint, vout: bigint, value: bigint, left: Noun, right: Noun): Noun {
  return [[[txid, vout], value], [left, right]];
}

describe("select", () => {
  test("collectSontMapKeys walks treap", () => {
    const sontMap: Noun = makeSontMapEntry(
      0x1234n, 0n, 1000n,
      makeSontMapEntry(0xabcdn, 1n, 2000n, 0n, 0n),
      makeSontMapEntry(0xffeen, 2n, 3000n, 0n, 0n),
    );
    const keys = collectSontMapKeys(sontMap);
    expect(keys.has("1234:0")).toBe(true);
    expect(keys.has("abcd:1")).toBe(true);
    expect(keys.has("ffee:2")).toBe(true);
    expect(keys.size).toBe(3);
  });

  test("empty sont-map produces empty set", () => {
    const keys = collectSontMapKeys(0n);
    expect(keys.size).toBe(0);
  });

  test("mempoolTxidToAtomHex matches Hoon display-order txid atom", () => {
    // Hoon stores txids as display-order numerics, so the atom value equals the
    // display hex read big-endian: "01020304" → 0x01020304.
    expect(mempoolTxidToAtomHex("01020304")).toBe("1020304");
    // Real 32-byte txid with a leading zero byte stays lossless.
    expect(mempoolTxidToAtomHex("00" + "ab".repeat(31))).toBe("ab".repeat(31));
  });

  test("addressToScriptPubKey for P2TR (main)", () => {
    // Known BIP-86 vector
    const addr = "bc1p5cyxnuxmeuwuvkwfem96lqzszd02n6xdcjrs20cac6yqjjwudpxqkedrcr";
    const spk = addressToScriptPubKey(addr, networkFor("main"));
    expect(spk.length).toBe(34);
    expect(spk[0]).toBe(0x51);  // OP_1
    expect(spk[1]).toBe(0x20);  // push 32
  });
});
