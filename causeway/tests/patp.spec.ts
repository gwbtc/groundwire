import { describe, expect, test } from "vitest";
import { atomToPatp, patpToAtom, isPatp } from "../src/protocol/patp.js";

describe("patp codec", () => {
  const cases: Array<[string, bigint]> = [
    ["~zod", 0n],
    ["~nec", 1n],
    ["~bud", 2n],
    ["~marzod", 256n],
    ["~sampel-palnet", 1624961343n],
  ];

  for (const [p, a] of cases) {
    test(`${p} <-> ${a}`, () => {
      expect(patpToAtom(p)).toBe(a);
      expect(atomToPatp(a)).toBe(p);
    });
  }

  test("isPatp accepts with or without tilde", () => {
    expect(isPatp("~zod")).toBe(true);
    expect(isPatp("zod")).toBe(true);
    expect(isPatp("garbage")).toBe(false);
  });

  test("rejects invalid", () => {
    expect(() => patpToAtom("~notaship")).toThrow();
  });
});
