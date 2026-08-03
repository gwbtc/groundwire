// BIP-340 tagged hash and the byte helpers the kelvin-9 %gw-btc encoders share.
//
//   H_tag(tag, msg) = sha256( sha256(tag) || sha256(tag) || msg )
//
// `tag` is the ASCII tag string; `msg` is a raw byte string; the result is 32
// big-endian bytes. This is +tagged-hash:taproot in the Hoon desk.
//
// A jammed noun enters a hash message as its minimal little-endian byte dump
// (the ordinary serialization of a jam). In Hoon this is +jam-octs
// (`[wid (rev 3 wid jm)]` fed to a big-endian tagged hash), which nets out to
// exactly the jam's natural LE byte string — pinned by the shared golden
// vectors (groundwire/vectors/gw-kelvin-9.json, tests/lib/gw-btc-pass.hoon).

import { sha256 } from "@noble/hashes/sha2";

export function concatBytes(...arrs: Uint8Array[]): Uint8Array {
  const total = arrs.reduce((s, a) => s + a.length, 0);
  const out = new Uint8Array(total);
  let off = 0;
  for (const a of arrs) { out.set(a, off); off += a.length; }
  return out;
}

export function hTag(tag: string, msg: Uint8Array): Uint8Array {
  const t = sha256(new TextEncoder().encode(tag));
  return sha256(concatBytes(t, t, msg));
}

// Minimal little-endian byte dump of an atom (Hoon `met 3` bytes; LSB first).
// The zero atom is the empty byte string.
export function minimalLEBytes(a: bigint): Uint8Array {
  if (a < 0n) throw new Error("minimalLEBytes: negative atom");
  const out: number[] = [];
  while (a > 0n) { out.push(Number(a & 0xffn)); a >>= 8n; }
  return new Uint8Array(out);
}

export function bytesToHex(b: Uint8Array): string {
  return Array.from(b, (x) => x.toString(16).padStart(2, "0")).join("");
}

export function hexToBytes(h: string): Uint8Array {
  const clean = h.replace(/^0x/, "");
  const out = new Uint8Array(clean.length / 2);
  for (let i = 0; i < out.length; i++) out[i] = parseInt(clean.slice(i * 2, i * 2 + 2), 16);
  return out;
}

// The big-endian numeric value of a byte string, as a Hoon @ux atom. A 32-byte
// hash (e.g. blind, d, c) becomes the atom the Hoon nouns carry.
export function bytesToAtomBE(b: Uint8Array): bigint {
  return b.length ? BigInt("0x" + bytesToHex(b)) : 0n;
}
