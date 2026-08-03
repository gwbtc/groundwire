import { describe, expect, test } from "vitest";
import {
  mineSuiteC, buildPassAtom, buildRingAtomBytes, jamFeed, REQUIRED_STAR,
} from "../src/spawn/mine-c.js";
import { miner } from "../src/spawn/miner.js";
import { buildDatAtom, datPkiDom, type SpawnSont } from "../src/spawn/dat.js";

// The kelvin-9 dat is now a hiding commitment to the spawn satpoint, blinded by
// the (random) seed and recomputed per mining iteration — so the tweak is no
// longer a fixed input. The miner takes the spawn satpoint instead.
const SPAWN: SpawnSont = {
  txidHex: "ab12f00d9c330000111122223333444455556666777788889999aaaabbbbcccc",
  vout: 1,
  off: 0,
};

describe("suite-C miner (~daplyd star)", () => {
  test("mined comet starts with REQUIRED_STAR bytes", async () => {
    const res = await mineSuiteC({ spawn: SPAWN });
    const starS = (res.comet[0]! | (res.comet[1]! << 8)) & 0xffff;
    expect(starS).toBe(REQUIRED_STAR);
  }, 120_000);

  test("REQUIRED_STAR equals ~daplyd (0x42cd)", () => {
    expect(REQUIRED_STAR).toBe(0x42cd);
  });

  test("output shape is well-formed; dat carries %gw-btc at bit 0", async () => {
    const res = await mineSuiteC({ spawn: SPAWN });
    expect(res.seed.length).toBe(64);
    expect(res.ringMaterial.length).toBe(64);
    expect(res.sPub.length).toBe(32);
    expect(res.cPub.length).toBe(32);
    expect(res.tweakedSPub.length).toBe(32);
    expect(res.comet.length).toBe(16);
    expect(res.pass).toBeGreaterThan(0n);
    expect(res.blind.length).toBe(32);
    expect(res.ringAtomBytes.length).toBeGreaterThan(64);
    expect(res.feed.length).toBeGreaterThan(0);
    expect(res.tries).toBeGreaterThanOrEqual(1);
    // The winning dat is exactly buildDatAtom(spawn, seed), and the domain is
    // rub-extractable at bit 0.
    expect(res.dat).toBe(buildDatAtom(SPAWN, res.seed));
    expect(datPkiDom(res.dat)).toBe("gw-btc");
  }, 120_000);

  test("ring bytes begin with 'C'", async () => {
    const res = await mineSuiteC({ spawn: SPAWN });
    expect(res.ringAtomBytes[0]).toBe(0x43);
  }, 120_000);

  test("pass atom begins with 'c' (byte 0x63)", async () => {
    const res = await mineSuiteC({ spawn: SPAWN });
    expect(Number(res.pass & 0xffn)).toBe(0x63);
  }, 120_000);

  test("high-level miner returns bigint comet under ~daplyd", async () => {
    // No Worker in Node, so this uses the main-thread fallback.
    const res = await miner.mine({ spawn: SPAWN });
    expect(res.comet).toBeGreaterThan(0n);
    expect(res.feed.length).toBeGreaterThan(0);
    expect(res.pass).toBeGreaterThan(0n);
    expect(res.dat).toBeGreaterThan(0n);
    expect(res.blind.length).toBe(32);
    // Atom is LE; low 2 bytes should equal REQUIRED_STAR.
    expect(Number(res.comet & 0xffffn)).toBe(REQUIRED_STAR);
  }, 120_000);

  test("ring/pass/feed constructors are deterministic", () => {
    const fakeSeed = new Uint8Array(64).fill(0xab);
    const fakePub = new Uint8Array(32).fill(0xcd);
    const tweakAtom = BigInt("0x07f6e5d4c3b2a109");
    const ring = buildRingAtomBytes(fakeSeed, tweakAtom);
    expect(ring[0]).toBe(0x43);
    const pass = buildPassAtom(fakePub, fakePub, tweakAtom);
    expect(Number(pass & 0xffn)).toBe(0x63);
  });

  test("jamFeed encodes non-empty bytes", () => {
    const comet = new Uint8Array(16).fill(0x11);
    const ring = new Uint8Array([0x43, ...new Array(64).fill(0), 0x01]);
    const feed = jamFeed(comet, 0, 1, ring);
    expect(feed.length).toBeGreaterThan(0);
  });
});
