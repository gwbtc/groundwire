import { describe, expect, test } from "vitest";
import { readFileSync } from "node:fs";
import { resolve } from "node:path";
import { cue } from "../src/oracle/cue.js";
import { decodeState } from "../src/oracle/state.js";
import { bytesToAtomLE } from "../src/oracle/snapshot.js";

const SNAPSHOT_PATH = resolve(__dirname, "fixtures/snapshot.jam");

describe("live snapshot decode", () => {
  test("cue + decodeState round-trip", () => {
    const bytes = new Uint8Array(readFileSync(SNAPSHOT_PATH));
    const atom = bytesToAtomLE(bytes);
    const noun = cue(atom);
    const state = decodeState(noun);

    expect(state.blockId.num).toBeGreaterThan(0);
    expect(state.unvIds.size).toBeGreaterThan(0);

    // First point sanity-check
    for (const [patpAtom, point] of state.unvIds) {
      expect(patpAtom).toBeGreaterThan(0n);
      expect(point.net.life).toBeGreaterThanOrEqual(1);
      expect(point.net.pass).toBeGreaterThan(0n);
      expect(point.net.sponsor.who).toBeGreaterThan(0n);
      expect(point.own.sont.txid).toBeInstanceOf(Uint8Array);
      expect(point.own.sont.txid.length).toBe(32);
      break;
    }
  });

  test("has 45 unv-ids at block 945375", () => {
    const bytes = new Uint8Array(readFileSync(SNAPSHOT_PATH));
    const state = decodeState(cue(bytesToAtomLE(bytes)));
    expect(state.unvIds.size).toBe(45);
    expect(state.blockId.num).toBe(945375);
  });
});
