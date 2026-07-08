import { describe, expect, test, beforeAll } from "vitest";
import { readFileSync } from "node:fs";
import { resolve } from "node:path";
import { UrbCore } from "../src/wasm/urb-core.js";

// Proves the Passport's `urb/` core, compiled to wasm32, reproduces Causeway's
// own golden vectors byte-for-byte — the parity that lets the two share one
// engine. The device is spawn-only, so we exercise the `%spawn` encoder vectors
// plus the tweak and @p paths.

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
const minLE = (n: bigint): Uint8Array => {
  const o: number[] = [];
  while (n > 0n) { o.push(Number(n & 0xffn)); n >>= 8n; }
  return new Uint8Array(o);
};

let core: UrbCore;
beforeAll(async () => {
  const bytes = readFileSync(resolve(__dirname, "../src/wasm/urb_wasm.wasm"));
  core = await UrbCore.load(bytes);
});

type Fix = { name: string; kind: string; input: any; output_hex: string };

describe("wasm urb/ core ⇄ Causeway golden vectors", () => {
  const all: Fix[] = JSON.parse(
    readFileSync(resolve(__dirname, "fixtures/encoder-vectors.json"), "utf8"),
  );
  const spawns = all.filter((v) => v.kind === "skim" && v.input?.op === "spawn");

  test("has %spawn vectors to check", () => {
    expect(spawns.length).toBeGreaterThan(0);
  });

  test.each(spawns)("encode_spawn: $name", (v) => {
    const inp = v.input;
    const fief =
      inp.fief === null
        ? null
        : inp.fief.type === "if"
          ? { type: "if" as const, ip: Number(inp.fief.ip), port: Number(inp.fief.port) }
          : { type: "is" as const, ip: toBig(inp.fief.ip), port: Number(inp.fief.port) };
    const out = core.encodeSpawn({
      pass: minLE(toBig(inp.pass)),
      spkh: leBytes(toBig(inp.to.spkh), 32),
      off: toBig(inp.to.off),
      tej: toBig(inp.to.tej),
      vout: inp.to.vout === null ? null : toBig(inp.to.vout),
      fief,
    });
    expect(hex(out)).toBe(v.output_hex);
  });

  test("build_tweak_bytes (txid=1, vout=0, off=0)", () => {
    expect(hex(core.tweak("00".repeat(31) + "01", 0, 0))).toBe(
      "09997572622d776174636865726274636777090100000000000000000000000000000000000000000000000000000000000000",
    );
  });

  test("to_patp comet range (512 → ~binzod)", () => {
    expect(core.patp(new Uint8Array([0, 2]))).toBe("~binzod");
  });

  test("self-check reports all vectors reproduced", () => {
    expect(core.selfTest()).toContain("5/5");
  });
});
