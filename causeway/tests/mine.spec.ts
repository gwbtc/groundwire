import { describe, expect, test } from "vitest";
import {
  mineSuiteC, buildPassAtom, buildRingAtomBytes, jamFeed, REQUIRED_STAR,
} from "../src/spawn/mine-c.js";
import { miner } from "../src/spawn/miner.js";

const TWEAK = new Uint8Array([0x9, 0xa1, 0xb2, 0xc3, 0xd4, 0xe5, 0xf6, 0x07]);

describe("suite-C miner (~daplyd star)", () => {
  test("mined comet starts with REQUIRED_STAR bytes", async () => {
    const res = await mineSuiteC({ tweak: TWEAK });
    const starS = (res.comet[0]! | (res.comet[1]! << 8)) & 0xffff;
    expect(starS).toBe(REQUIRED_STAR);
  }, 120_000);

  test("REQUIRED_STAR equals ~daplyd (0x42cd)", () => {
    expect(REQUIRED_STAR).toBe(0x42cd);
  });

  test("output shape is well-formed", async () => {
    const res = await mineSuiteC({ tweak: TWEAK });
    expect(res.seed.length).toBe(64);
    expect(res.ringMaterial.length).toBe(64);
    expect(res.sPub.length).toBe(32);
    expect(res.cPub.length).toBe(32);
    expect(res.tweakedSPub.length).toBe(32);
    expect(res.comet.length).toBe(16);
    expect(res.pass).toBeGreaterThan(0n);
    expect(res.ringAtomBytes.length).toBeGreaterThan(64);
    expect(res.feed.length).toBeGreaterThan(0);
    expect(res.tries).toBeGreaterThanOrEqual(1);
  }, 120_000);

  test("ring bytes begin with 'C'", async () => {
    const res = await mineSuiteC({ tweak: TWEAK });
    expect(res.ringAtomBytes[0]).toBe(0x43);
  }, 120_000);

  test("pass atom begins with 'c' (byte 0x63)", async () => {
    const res = await mineSuiteC({ tweak: TWEAK });
    expect(Number(res.pass & 0xffn)).toBe(0x63);
  }, 120_000);

  test("high-level miner returns bigint comet under ~daplyd", async () => {
    // No Worker in Node, so this uses the main-thread fallback.
    const res = await miner.mine({ tweakExpr: TWEAK });
    expect(res.comet).toBeGreaterThan(0n);
    expect(res.feed.length).toBeGreaterThan(0);
    expect(res.pass).toBeGreaterThan(0n);
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
