// Hoon ++jam — serialize a noun to its minimal little-endian byte string,
// with back-references (the standard jam, matching dump_jam_vectors.py and the
// kelvin-9 golden vectors).
//
// A noun is an atom (bigint) or a cell [head, tail]; cells right-associate,
// so [a b c] == [a [b c]].

import { BitWriter } from "./bitwriter.js";

export type Noun = bigint | [Noun, Noun];

export function jam(noun: Noun): Uint8Array {
  const w = new BitWriter();
  const refs = new Map<string, number>();

  function key(n: Noun): string {
    if (typeof n === "bigint") return `a:${n.toString(16)}`;
    return `c:(${key(n[0])}|${key(n[1])})`;
  }

  function encode(n: Noun): void {
    const start = w.bitLength;
    const k = key(n);
    const existing = refs.get(k);

    if (Array.isArray(n)) {
      if (existing !== undefined) {
        w.write(1, 1); w.write(1, 1);
        w.writeMat(BigInt(existing));
      } else {
        refs.set(k, start);
        w.write(1, 1); w.write(1, 0);
        encode(n[0]);
        encode(n[1]);
      }
    } else {
      const a = n as bigint;
      if (existing !== undefined) {
        const aBits = a === 0n ? 0 : a.toString(2).length;
        const rBits = existing === 0 ? 1 : Math.floor(Math.log2(existing)) + 1;
        if (aBits <= rBits) {
          w.write(1, 0); w.writeMat(a);
        } else {
          w.write(1, 1); w.write(1, 1); w.writeMat(BigInt(existing));
        }
      } else {
        refs.set(k, start);
        w.write(1, 0); w.writeMat(a);
      }
    }
  }

  encode(noun);
  return w.toBytes();
}
