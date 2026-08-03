// kelvin-9 web parity: the finalize step that bakes the xtr custody log into
// the boot feed (mirrors desktop causeway.py append_xtr_to_ring + rebuild_feed,
// exercised by `causeway finalize --feed` and ui/pages/spawn.ts renderFinalizeCard).

import { describe, it, expect } from "vitest";
import { buildDatAtom, type SpawnSont } from "../src/spawn/dat.js";
import { buildXtrAtom, bakeXtrIntoFeedAtom, type XtrEntry } from "../src/spawn/reveal-log.js";
import type { Opening } from "../src/spawn/publication.js";
import {
  buildRingAtomBytes, appendXtrToRing, bakeXtrIntoFeed, jamFeed,
} from "../src/spawn/mine-c.js";
import { cue } from "../src/oracle/cue.js";
import { head, tail, asAtom } from "../src/oracle/noun.js";
import { bytesToAtomLE } from "../src/protocol/bitwriter.js";
import { atomToUw, uwToAtom } from "../src/spawn/boot-cmd.js";

const SPAWN: SpawnSont = {
  txidHex: "ab12f00d9c330000111122223333444455556666777788889999aaaabbbbcccc",
  vout: 0,
  off: 0,
};
const SEED = 0xdeadbeefn;
const COMET = 0x1234_42cdn; // arbitrary comet atom (LE low bytes are the ~daplyd star)

function cometBytesLE(a: bigint): Uint8Array {
  const out = new Uint8Array(16);
  let x = a;
  for (let i = 0; i < 16; i++) { out[i] = Number(x & 0xffn); x >>= 8n; }
  return out;
}

const OPENING: Opening = {
  internalKey: BigInt("0x02" + "ef".repeat(32)),
  snapshot: { life: 1, rift: 0, key: 0xabcdn, sponsor: null, fief: null },
  blindOpening: { spawnSont: SPAWN, startHeight: 100, blind: 0xdeadn },
};
const XTR_ENTRY: XtrEntry = { txidHex: SPAWN.txidHex, blockHeight: 123, opening: OPENING };

describe("kelvin-9 feed finalize", () => {
  it("bakeXtrIntoFeed appends xtr to the ring and keeps the [[2 0] comet rift [[life ring] 0]] shape", () => {
    const dat = buildDatAtom(SPAWN, SEED);
    const seed = new Uint8Array(64).fill(9);
    const ring0 = buildRingAtomBytes(seed, dat);
    const xtr = buildXtrAtom([XTR_ENTRY]);

    const feed1 = bakeXtrIntoFeed(COMET, ring0, xtr, 0, 1);
    const noun = cue(bytesToAtomLE(feed1));
    // [[2 0] [comet [rift [[life ring] 0]]]]
    const [tag0, tag1] = [asAtom(head(head(noun))), asAtom(tail(head(noun)))];
    expect([tag0, tag1]).toEqual([2n, 0n]);
    const body = tail(noun);
    expect(asAtom(head(body))).toBe(COMET);
    const rest = tail(body);
    expect(asAtom(head(rest))).toBe(0n); // rift
    const lifeRing = head(tail(rest));
    expect(asAtom(head(lifeRing))).toBe(1n); // life
    const ringAtom = asAtom(tail(lifeRing));
    expect(ringAtom).toBe(bytesToAtomLE(appendXtrToRing(ring0, xtr)));
  });

  it("bakeXtrIntoFeedAtom (cue-based, resume path) matches bakeXtrIntoFeed (fresh path)", () => {
    const dat = buildDatAtom(SPAWN, SEED);
    const seed = new Uint8Array(64).fill(4);
    const ring0 = buildRingAtomBytes(seed, dat);
    const xtr = buildXtrAtom([XTR_ENTRY]);

    const feedFresh = bakeXtrIntoFeed(COMET, ring0, xtr, 0, 1);
    // The miner-fresh feed (xtr-less) the user would re-supply on resume:
    const feed0 = jamFeed(cometBytesLE(COMET), 0, 1, ring0);
    const feedResume = bakeXtrIntoFeedAtom(bytesToAtomLE(feed0), xtr);
    expect(feedResume).toEqual(feedFresh);
  });

  it("uwToAtom inverts atomToUw", () => {
    for (const x of [0n, 1n, 63n, 64n, 0xdead_beefn, 1n << 300n, 0x42cdn]) {
      expect(uwToAtom(atomToUw(x))).toBe(x);
    }
  });
});
