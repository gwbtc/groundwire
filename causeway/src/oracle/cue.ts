import type { Noun } from "./noun.js";

function rub(pos: number, a: bigint, totalBits: number): { p: number; q: bigint } {
  let c = 0;
  while (pos + c < totalBits && ((a >> BigInt(pos + c)) & 1n) === 0n) c++;
  if (c === 0) return { p: 1, q: 0n };
  const d = pos + c + 1;
  const lenLow = c === 1 ? 0n : (a >> BigInt(d)) & ((1n << BigInt(c - 1)) - 1n);
  const e = Number((1n << BigInt(c - 1)) + lenLow);
  const value = e === 0 ? 0n : (a >> BigInt(d + c - 1)) & ((1n << BigInt(e)) - 1n);
  return { p: 2 * c + e, q: value };
}

type Cont =
  | { kind: "head"; startPos: number }
  | { kind: "tail"; startPos: number; head: Noun };

// Hoon ++cue — iterative single-pass, no double-walk.
export function cue(a: bigint): Noun {
  const totalBits = a === 0n ? 1 : a.toString(2).length;
  const backrefs = new Map<number, Noun>();
  const conts: Cont[] = [];
  let pos = 0;
  let result: Noun = 0n;

  while (true) {
    const startPos = pos;
    const tag0 = Number((a >> BigInt(pos)) & 1n);
    if (tag0 === 0) {
      const { p, q } = rub(pos + 1, a, totalBits);
      pos += 1 + p;
      backrefs.set(startPos, q);
      result = q;
    } else {
      const tag1 = Number((a >> BigInt(pos + 1)) & 1n);
      if (tag1 === 0) {
        // Cell: descend into head, then tail, then combine.
        pos += 2;
        conts.push({ kind: "head", startPos });
        continue;
      }
      // Back-ref
      const { p, q } = rub(pos + 2, a, totalBits);
      pos += 2 + p;
      const target = backrefs.get(Number(q));
      if (target === undefined) throw new Error(`cue: missing backref @${q}`);
      result = target;
    }

    // Resolve pending continuations
    while (conts.length > 0) {
      const c = conts[conts.length - 1]!;
      if (c.kind === "head") {
        conts.pop();
        conts.push({ kind: "tail", startPos: c.startPos, head: result });
        break; // parse the tail next
      }
      // tail
      conts.pop();
      const cell: Noun = [c.head, result];
      backrefs.set(c.startPos, cell);
      result = cell;
    }
    if (conts.length === 0) return result;
  }
}
