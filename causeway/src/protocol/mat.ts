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
