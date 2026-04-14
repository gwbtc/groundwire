import { describe, expect, test } from "vitest";
import { readFileSync } from "node:fs";
import { resolve } from "node:path";
import { cue } from "../src/oracle/cue.js";
import type { Noun } from "../src/oracle/noun.js";
import { isAtom, isCell } from "../src/oracle/noun.js";

type JsonNoun = { __atom__: string } | [JsonNoun, JsonNoun];
type Case = { name: string; noun: JsonNoun; jam_hex: string; jam_dec: string };

function reviveExpected(v: JsonNoun): Noun {
  if (Array.isArray(v)) return [reviveExpected(v[0]), reviveExpected(v[1])];
  return BigInt(v.__atom__);
}

function eq(a: Noun, b: Noun): boolean {
  if (isAtom(a) && isAtom(b)) return a === b;
  if (isCell(a) && isCell(b)) return eq(a[0], b[0]) && eq(a[1], b[1]);
  return false;
}

function show(n: Noun): string {
  if (isAtom(n)) return n.toString();
  return `[${show(n[0])} ${show(n[1])}]`;
}

const cases: Case[] = JSON.parse(
  readFileSync(resolve(__dirname, "fixtures/jam-vectors.json"), "utf8"),
);

describe("cue / jam round-trip", () => {
  for (const c of cases) {
    test(c.name, () => {
      const jam = BigInt(c.jam_dec);
      const decoded = cue(jam);
      const expected = reviveExpected(c.noun);
      if (!eq(decoded, expected)) {
        throw new Error(`mismatch\n  expected: ${show(expected)}\n  actual:   ${show(decoded)}`);
      }
    });
  }
});
