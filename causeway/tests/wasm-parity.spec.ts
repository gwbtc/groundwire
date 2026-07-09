import { describe, expect, test, beforeAll } from "vitest";
import { readFileSync } from "node:fs";
import { resolve } from "node:path";
import { encodeSkim } from "../src/protocol/encoder.js";
import { initUrbCore, urbCore, UrbCore } from "../src/wasm/urb-core.js";
import type { Fief, SkimSotx } from "../src/protocol/types.js";

// The wasm core is the Passport's `src/urb/` compiled to wasm32. Once it's
// initialized, `encodeSkim` delegates the consensus-critical %spawn encoding to
// it (see protocol/encoder.ts). This asserts the *integrated* path reproduces
// this repo's own golden vectors byte-for-byte — i.e. the shared engine is a
// drop-in for the TS encoder on the spawn path.

const hex = (b: Uint8Array) => Array.from(b, (x) => x.toString(16).padStart(2, "0")).join("");
const toBig = (v: unknown): bigint =>
  typeof v === "object" && v !== null && "__bigint__" in (v as any)
    ? BigInt((v as { __bigint__: string }).__bigint__)
    : BigInt(v as number);
const leBytes = (n: bigint, len: number): Uint8Array => {
  const o = new Uint8Array(len);
  for (let i = 0; i < len; i++) o[i] = Number((n >> BigInt(8 * i)) & 0xffn);
  return o;
};

let wasmBytes: Uint8Array;
beforeAll(async () => {
  wasmBytes = readFileSync(resolve(__dirname, "../src/wasm/urb_wasm.wasm"));
  await initUrbCore(wasmBytes); // after this, encodeSkim() routes %spawn -> wasm
});

type Fix = { name: string; kind: string; input: any; output_hex: string };

function toSpawnSkim(inp: any): SkimSotx {
  const fief: Fief | null =
    inp.fief === null
      ? null
      : inp.fief.type === "if"
        ? { type: "if", ip: Number(inp.fief.ip), port: Number(inp.fief.port) }
        : { type: "is", ip: toBig(inp.fief.ip), port: Number(inp.fief.port) };
  return {
    op: "spawn",
    pass: toBig(inp.pass),
    fief,
    to: {
      spkh: leBytes(toBig(inp.to.spkh), 32),
      off: toBig(inp.to.off),
      tej: toBig(inp.to.tej),
      vout: inp.to.vout === null ? null : toBig(inp.to.vout),
    },
  };
}

describe("encodeSkim routes %spawn through the wasm core", () => {
  const all: Fix[] = JSON.parse(
    readFileSync(resolve(__dirname, "fixtures/encoder-vectors.json"), "utf8"),
  );
  const spawns = all.filter((v) => v.kind === "skim" && v.input?.op === "spawn");

  test("core initialized (so encodeSkim will delegate)", () => {
    expect(spawns.length).toBeGreaterThan(0);
    expect(urbCore()).not.toBeNull();
  });

  test.each(spawns)("encodeSkim %spawn: $name", (v) => {
    expect(hex(encodeSkim(toSpawnSkim(v.input)))).toBe(v.output_hex);
  });

  test("direct binding: tweak + patp + self-check", async () => {
    const core = await UrbCore.load(wasmBytes);
    expect(hex(core.tweak("00".repeat(31) + "01", 0, 0))).toBe(
      "09997572622d776174636865726274636777090100000000000000000000000000000000000000000000000000000000000000",
    );
    expect(core.patp(new Uint8Array([0, 2]))).toBe("~binzod");
    expect(core.nym(new Uint8Array(16))).toBe("..abducts"); // 128-bit zero comet
    expect(core.selfTest()).toContain("6/6");
  });
});
