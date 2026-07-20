export function bitLength(n: bigint): number {
  if (n < 0n) throw new Error("mat: negative atom");
  if (n === 0n) return 0;
  return n.toString(2).length;
}

export function mat(a: bigint): { p: number; q: bigint } {
  if (a === 0n) return { p: 1, q: 1n };
  const b = bitLength(a);
  const c = bitLength(BigInt(b));
  const p = 2 * c + b;
  const lowB = c > 1 ? BigInt(b) & ((1n << BigInt(c - 1)) - 1n) : 0n;
  const shiftedA = c > 1 ? a << BigInt(c - 1) : a;
  const mixed = lowB ^ shiftedA;
  const q = (1n << BigInt(c)) | (mixed << BigInt(c + 1));
  return { p, q };
}

// Hoon ++rub: decode a mat at bit offset `pos` of atom `b`.
// Returns { p: bits consumed, q: the decoded atom }.
export function rub(pos: number, b: bigint): { p: number; q: bigint } {
  let c = 0;
  while (((b >> BigInt(pos + c)) & 1n) === 0n) {
    c++;
    if (c > 4096) throw new Error("rub: too many leading zeros");
  }
  if (c === 0) return { p: 1, q: 0n };
  const d = pos + c + 1;
  const low = c > 1 ? (b >> BigInt(d)) & ((1n << BigInt(c - 1)) - 1n) : 0n;
  const e = (1n << BigInt(c - 1)) + low;
  const q = (b >> BigInt(d + c - 1)) & ((1n << e) - 1n);
  return { p: 2 * c + Number(e), q };
}
