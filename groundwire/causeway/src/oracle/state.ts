import type { Fief, Mang, Sont } from "../protocol/types.js";
import {
  asAtom,
  asMap,
  asT2,
  asT3,
  asT6,
  asT4,
  asUnit,
  head,
  isAtom,
  isCell,
  tail,
} from "./noun.js";
import type { Noun } from "./noun.js";

export interface BlockId {
  hax: bigint;
  num: number;
}

export interface Sponsor {
  has: boolean;
  who: bigint;
}

export interface Point {
  own: {
    sont: Sont;
    mang: Mang | null;
  };
  net: {
    rift: number;
    life: number;
    pass: bigint;
    sponsor: Sponsor;
    escape: bigint | null;
    fief: Fief | null;
  };
}

export interface UrbState {
  blockId: BlockId;
  sontMap: Noun;
  inscIds: Noun;
  unvIds: Map<bigint, Point>;
}

function atomToBytesLE(a: bigint, len: number): Uint8Array {
  const out = new Uint8Array(len);
  for (let i = 0; i < len; i++) {
    out[i] = Number((a >> BigInt(8 * i)) & 0xffn);
  }
  return out;
}

function decodeFief(n: Noun): Fief {
  // Hoon fief: head = tag atom (%turf | %if | %is), tail depends.
  // As a tagged union noun: [tag p q]
  const [tag, p, q] = asT3(n);
  const tagA = asAtom(tag);
  // %if = 0x6669 ('if' LE), %is = 0x7369, %turf = 0x66727574 ('turf' LE)
  if (tagA === 0x6669n) {
    return { type: "if", ip: Number(asAtom(p)), port: Number(asAtom(q)) };
  }
  if (tagA === 0x7369n) {
    return { type: "is", ip: asAtom(p), port: Number(asAtom(q)) };
  }
  if (tagA === 0x66727574n) {
    // %turf: p = (list turf), q = port. Leave domains unparsed for now.
    return { type: "turf", domains: [], port: Number(asAtom(q)) };
  }
  throw new Error(`decodeFief: unknown tag 0x${tagA.toString(16)}`);
}

function decodeSont(n: Noun): Sont {
  const [txid, vout, off] = asT3(n);
  return {
    txid: atomToBytesLE(asAtom(txid), 32),
    vout: asAtom(vout),
    off: asAtom(off),
  };
}

function decodeMang(n: Noun): Mang {
  // [%sont sont] or [%pass pass]
  const [tag, rest] = asT2(n);
  const tagA = asAtom(tag);
  // %sont = 0x746e6f73 (little-endian "sont"), %pass = 0x73736170 ("pass")
  if (tagA === 0x746e6f73n) return { type: "sont", sont: decodeSont(rest) };
  if (tagA === 0x73736170n) return { type: "pass", pass: asAtom(rest) };
  throw new Error(`decodeMang: unknown tag 0x${tagA.toString(16)}`);
}

function decodeSponsor(n: Noun): Sponsor {
  const [has, who] = asT2(n);
  return { has: asAtom(has) !== 0n, who: asAtom(who) };
}

function decodePoint(n: Noun): Point {
  // point = [own net] where own = [sont mang-unit] and
  // net = [rift life pass sponsor escape fief]
  const [ownN, netN] = asT2(n);
  const [sontN, mangN] = asT2(ownN);
  const [rift, life, pass, sponsor, escape, fief] = asT6(netN);
  return {
    own: {
      sont: decodeSont(sontN),
      mang: asUnit(mangN, decodeMang),
    },
    net: {
      rift: Number(asAtom(rift)),
      life: Number(asAtom(life)),
      pass: asAtom(pass),
      sponsor: decodeSponsor(sponsor),
      escape: asUnit(escape, (v) => asAtom(v)),
      fief: asUnit(fief, decodeFief),
    },
  };
}

function decodeBlockId(n: Noun): BlockId {
  const [hax, num] = asT2(n);
  return { hax: asAtom(hax), num: Number(asAtom(num)) };
}

export function decodeState(n: Noun): UrbState {
  const [blockN, sontMap, inscIds, unvIds] = asT4(n);
  return {
    blockId: decodeBlockId(blockN),
    sontMap,
    inscIds,
    unvIds: asMap(unvIds, (k) => asAtom(k), decodePoint),
  };
}

// Convenience re-exports for callers that just want the noun helpers.
export { isAtom, isCell, head, tail };
