import { describe, expect, test } from "vitest";
import { readFileSync } from "node:fs";
import { resolve } from "node:path";
import { encodeSkim, encodeFull } from "../src/protocol/encoder.js";
import type {
  Fief, Mang, Single, SkimSotx, Sont, Sotx, SpawnTo,
} from "../src/protocol/types.js";

const VECTORS_PATH = resolve(__dirname, "fixtures/encoder-vectors.json");

type Fix = { name: string; kind: "skim" | "full"; input: unknown; output_hex: string };

function toBig(v: unknown): bigint {
  if (v === null || v === undefined) throw new Error("toBig: null");
  if (typeof v === "bigint") return v;
  if (typeof v === "number") return BigInt(v);
  if (typeof v === "object" && v !== null && "__bigint__" in (v as any)) {
    return BigInt((v as { __bigint__: string }).__bigint__);
  }
  throw new Error(`toBig: unrecognized ${JSON.stringify(v)}`);
}

function toBigOrNull(v: unknown): bigint | null {
  return v === null ? null : toBig(v);
}

function atomToBytesLE(a: bigint, byteLen: number): Uint8Array {
  const out = new Uint8Array(byteLen);
  for (let i = 0; i < byteLen; i++) {
    out[i] = Number((a >> BigInt(8 * i)) & 0xffn);
  }
  return out;
}

function reviveFief(v: any): Fief | null {
  if (v === null) return null;
  if (v.type === "if") return { type: "if", ip: Number(v.ip), port: Number(v.port) };
  if (v.type === "is") return { type: "is", ip: toBig(v.ip), port: Number(v.port) };
  throw new Error(`reviveFief: unknown ${v.type}`);
}

function reviveSpawnTo(v: any): SpawnTo {
  return {
    spkh: atomToBytesLE(toBig(v.spkh), 32),
    off: toBig(v.off),
    tej: toBig(v.tej),
    vout: v.vout === null ? null : toBig(v.vout),
  };
}

function reviveSont(v: any): Sont {
  return {
    txid: atomToBytesLE(toBig(v.txid), 32),
    vout: toBig(v.vout),
    off: toBig(v.off),
  };
}

function reviveMang(v: any): Mang | null {
  if (v === null) return null;
  if (v.type === "sont") return { type: "sont", sont: reviveSont(v.sont) };
  return { type: "pass", pass: toBig(v.pass) };
}

function reviveSingle(v: any): Single {
  switch (v.op) {
    case "spawn":
      return { op: "spawn", pass: toBig(v.pass), fief: reviveFief(v.fief), to: reviveSpawnTo(v.to) };
    case "keys":
      return { op: "keys", pass: toBig(v.pass), breach: Boolean(v.breach) };
    case "escape":
      return { op: "escape", parent: toBig(v.parent), sig: toBigOrNull(v.sig) };
    case "cancel-escape":
      return { op: "cancel-escape", parent: toBig(v.parent) };
    case "adopt":
      return { op: "adopt", ship: toBig(v.ship) };
    case "reject":
      return { op: "reject", ship: toBig(v.ship) };
    case "detach":
      return { op: "detach", ship: toBig(v.ship) };
    case "fief":
      return { op: "fief", fief: reviveFief(v.fief) };
    case "set-mang":
      return { op: "set-mang", mang: reviveMang(v.mang) };
    default:
      throw new Error(`reviveSingle: ${v.op}`);
  }
}

function reviveSkim(v: any): SkimSotx {
  if (v.op === "batch") return { op: "batch", items: v.items.map(reviveSingle) };
  return reviveSingle(v);
}

function reviveFull(v: any[]): Sotx[] {
  return v.map((e) => ({ ship: toBig(e.ship), sig: toBigOrNull(e.sig), skim: reviveSkim(e.skim) }));
}

function bytesToHex(b: Uint8Array): string {
  return Array.from(b, (x) => x.toString(16).padStart(2, "0")).join("");
}

const fixtures: Fix[] = JSON.parse(readFileSync(VECTORS_PATH, "utf8"));

describe("encoder golden vectors", () => {
  for (const f of fixtures) {
    test(f.name, () => {
      const actual =
        f.kind === "skim"
          ? encodeSkim(reviveSkim(f.input))
          : encodeFull(reviveFull(f.input as any[]));
      expect(bytesToHex(actual)).toBe(f.output_hex);
    });
  }
});
