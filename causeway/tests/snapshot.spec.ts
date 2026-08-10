import { describe, expect, test } from "vitest";
import { readFileSync } from "node:fs";
import { resolve } from "node:path";
import { cue } from "../src/oracle/cue.js";
import { decodePoint, decodeState } from "../src/oracle/state.js";
import type { Noun } from "../src/oracle/noun.js";
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

// A $point gained `seen` -- the block the indexer most recently observed it in
// (sur/urb.hoon). Both shapes are in circulation: the pinned fixture above was
// written before the field, a current indexer writes [own net seen]. The
// decoder must read both, and must tell them apart EXACTLY rather than by
// guessing -- in the old shape the tail IS net, whose head `rift` is an atom;
// in the new one the tail is [net seen], whose head is the net cell.
describe("point provenance", () => {
  const own: Noun = [[0x1234n, [0n, 0n]], 0n];
  const net: Noun = [0n, [1n, [0xdeadn, [[1n, 0x100n], [0n, 0n]]]]];

  test("the pinned legacy snapshot decodes with seen = null", () => {
    const bytes = new Uint8Array(readFileSync(SNAPSHOT_PATH));
    const state = decodeState(cue(bytesToAtomLE(bytes)));
    for (const [, point] of state.unvIds) {
      expect(point.seen).toBeNull();
    }
  });

  test("a point carrying a block hash decodes it, and one without does not", () => {
    const withSeen = decodePoint([own, [net, [0n, 0xb10cn]]]);
    const noSeen = decodePoint([own, [net, 0n]]);
    const legacy = decodePoint([own, net]);
    expect(withSeen.seen).toBe(0xb10cn);
    // an explicit ~ and an absent field are the same statement: no provenance
    expect(noSeen.seen).toBeNull();
    expect(legacy.seen).toBeNull();
    // and nothing else moved under either shape
    for (const p of [withSeen, noSeen, legacy]) {
      expect(p.net.life).toBe(1);
      expect(p.net.pass).toBe(0xdeadn);
      expect(p.net.sponsor.has).toBe(true);
      expect(p.net.sponsor.who).toBe(0x100n);
    }
  });
});
