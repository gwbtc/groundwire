import { mat } from "./mat.js";

export class BitWriter {
  private bits = 0n;
  private pos = 0;

  write(width: number, value: bigint | number): void {
    if (width < 0) throw new Error("BitWriter.write: negative width");
    if (width === 0) return;
    const v = typeof value === "bigint" ? value : BigInt(value);
    const mask = (1n << BigInt(width)) - 1n;
    this.bits |= (v & mask) << BigInt(this.pos);
    this.pos += width;
  }

  writeMat(value: bigint | number): void {
    const v = typeof value === "bigint" ? value : BigInt(value);
    const { p, q } = mat(v);
    this.write(p, q);
  }

  get bitLength(): number { return this.pos; }

  toInt(): bigint { return this.bits; }

  toBytes(): Uint8Array {
    const n = (this.pos + 7) >> 3;
    const out = new Uint8Array(n);
    let b = this.bits;
    for (let i = 0; i < n; i++) {
      out[i] = Number(b & 0xffn);
      b >>= 8n;
    }
    return out;
  }
}

export function bytesToAtomLE(b: Uint8Array): bigint {
  let x = 0n;
  for (let i = b.length - 1; i >= 0; i--) {
    x = (x << 8n) | BigInt(b[i]!);
  }
  return x;
}
