// Pure-JS port of the suite-C comet miner (comet-miner/pkg/vere/comet_miner.c _mine_c).
//
// Algorithm per iteration:
//   1. seed = 64 bytes random
//   2. ringMaterial = SHA-512(seed)                          (urcrypt_shal)
//   3. sPub = ed25519.pubkey(ringMaterial[0..32])            (urcrypt_ed_luck)
//   4. twSca = SHA-256(sPub || tweak)                        (urcrypt_shay)
//   5. tweakedSPub = sPub + twSca·G                          (urcrypt_ed_add_scalar_public)
//   6. comet = shaf("cfig", tweakedSPub)                     (16-byte fuzzy hash)
//   7. if (optional prefix/star checks pass) → done
//
// With no prefix/star constraints (the default), iteration 1 wins. With a
// 16-bit prefix constraint, expect ~65k iterations.

import { ed25519 } from "@noble/curves/ed25519";
import { sha256, sha512 } from "@noble/hashes/sha2";
import { BitWriter, bytesToAtomLE } from "../protocol/bitwriter.js";

// ed25519 curve order L
const ED_L = 2n ** 252n + 27742317777372353535851937790883648493n;

// Hoon shas: out = SHA-256(salt XOR SHA-256(msg)), with salt-padding rules
// from urcrypt_shas.
function shas(salt: Uint8Array, msg: Uint8Array): Uint8Array {
  const mid = sha256(msg);
  if (salt.length > 32) {
    const padded = new Uint8Array(salt.length);
    padded.set(salt);
    for (let i = 0; i < 32; i++) padded[i] = padded[i]! ^ mid[i]!;
    return sha256(padded);
  }
  const tmp = new Uint8Array(32);
  tmp.set(mid);
  for (let i = 0; i < salt.length; i++) tmp[i] = tmp[i]! ^ salt[i]!;
  return sha256(tmp);
}

// Hoon shaf: 16-byte fuzzy hash = halves XOR of shas.
function shaf(salt: Uint8Array, msg: Uint8Array): Uint8Array {
  const h = shas(salt, msg);
  const out = new Uint8Array(16);
  for (let i = 0; i < 16; i++) out[i] = h[i]! ^ h[i + 16]!;
  return out;
}

// ed25519 keypair-from-seed. Matches ed25519_create_keypair (orlp): produces
// the same pubkey as RFC 8032 getPublicKey.
function edLuck(seed32: Uint8Array): Uint8Array {
  return ed25519.getPublicKey(seed32);
}

// P' = P + s·G where s is a 32-byte scalar interpreted little-endian and
// reduced mod L.
function edAddScalarPublic(pub: Uint8Array, scalarBytes: Uint8Array): Uint8Array {
  let s = 0n;
  for (let i = scalarBytes.length - 1; i >= 0; i--) {
    s = (s << 8n) | BigInt(scalarBytes[i]!);
  }
  s = s % ED_L;
  const sG = ed25519.ExtendedPoint.BASE.multiply(s === 0n ? 1n : s);
  // Subtract if s==0 — never happens in practice (SHA-256 output is effectively never 0).
  const P = ed25519.ExtendedPoint.fromHex(pub);
  const sum = P.add(sG);
  if (s === 0n) return sum.subtract(ed25519.ExtendedPoint.BASE).toRawBytes();
  return sum.toRawBytes();
}

const SALT_CFIG = (() => {
  const s = new Uint8Array(32);
  s[0] = 0x63; // 'c'
  s[1] = 0x66; // 'f'
  s[2] = 0x69; // 'i'
  s[3] = 0x67; // 'g'
  return s;
})();

// Groundwire currently only sponsors comets whose @p falls under ~daplyd (a
// star, 16-bit atom = 0x42cd). Enforced in the miner: the first 2 bytes of
// comet (little-endian u16) must equal this value. This is the upstream-
// dictated constraint — changing it requires coordination with the
// sponsor-signer service and urb-watcher.
export const REQUIRED_STAR = 0x42cd; // patpToAtom("~daplyd")

// Build the suite-C ring atom bytes: 'C' || ringMaterial || mat(tweakAtom).
export function buildRingAtomBytes(ringMaterial: Uint8Array, tweakAtom: bigint): Uint8Array {
  const w = new BitWriter();
  w.write(8, 0x43); // 'C'
  w.write(512, bytesToAtomLE(ringMaterial));
  w.writeMat(tweakAtom);
  return w.toBytes();
}

// Build the on-chain pass atom: [tag='c' ugn=sPub cry=cPub dat=mat(tweak)]
export function buildPassAtom(sPub: Uint8Array, cPub: Uint8Array, tweakAtom: bigint): bigint {
  const w = new BitWriter();
  w.write(8, 0x63); // 'c'
  w.write(256, bytesToAtomLE(sPub));
  w.write(256, bytesToAtomLE(cPub));
  w.writeMat(tweakAtom);
  return w.toInt();
}

export interface MineOpts {
  tweak: Uint8Array;           // raw tweak bytes (LE atom bytes)
  life?: number;               // default 1
  rift?: number;               // default 0
  // Optional vanity: 16-bit match on last 2 bytes of comet. Combined with
  // the mandatory ~daplyd star constraint this becomes a 32-bit search,
  // which takes ~4B iterations and is impractical in-browser. Intended for
  // off-line use with the native miner.
  prefix?: number | null;
  maxTries?: number;           // default 10M
  onProgress?: (tries: number) => void;
  yieldEveryTries?: number;    // yield to event loop for progress (default 2000)
}

export interface MineResult {
  seed: Uint8Array;           // 64 random bytes (entropy)
  ringMaterial: Uint8Array;   // 64 bytes = SHA-512(seed)
  sPub: Uint8Array;
  cPub: Uint8Array;
  tweakedSPub: Uint8Array;
  comet: Uint8Array;          // 16 bytes — the raw comet @p atom
  pass: bigint;               // on-chain pass atom for %spawn sotx
  ringAtomBytes: Uint8Array;
  feed: Uint8Array;           // jam bytes; encode as @uw for vere -G
  tries: number;
}

export async function mineSuiteC(opts: MineOpts): Promise<MineResult> {
  const prefix = opts.prefix ?? null;
  const maxTries = opts.maxTries ?? 10_000_000;
  const yieldEvery = opts.yieldEveryTries ?? 2000;
  const tweakAtom = bytesToAtomLE(opts.tweak);

  let tries = 0;
  while (tries < maxTries) {
    tries++;

    const seed = new Uint8Array(64);
    crypto.getRandomValues(seed);
    const ringMaterial = sha512(seed);              // 64 bytes
    const sSeed = ringMaterial.slice(0, 32);
    const sPub = edLuck(sSeed);
    const twScaData = new Uint8Array(32 + opts.tweak.length);
    twScaData.set(sPub, 0);
    twScaData.set(opts.tweak, 32);
    const twSca = sha256(twScaData);
    const tweakedSPub = edAddScalarPublic(sPub, twSca);
    const comet = shaf(SALT_CFIG, tweakedSPub);

    // Mandatory: comet's first 2 bytes (LE u16) must == ~daplyd (0x42cd).
    const starS = (comet[0]! | (comet[1]! << 8)) & 0xffff;
    if (starS !== REQUIRED_STAR) {
      if (opts.onProgress && tries % 1000 === 0) opts.onProgress(tries);
      if (tries % yieldEvery === 0) {
        await new Promise((r) => setTimeout(r, 0)); // yield to event loop
      }
      continue;
    }

    // Optional additional vanity: last 2 bytes prefix match.
    if (prefix !== null) {
      const fepS = (comet[14]! | (comet[15]! << 8)) & 0xffff;
      if (fepS !== prefix) {
        if (opts.onProgress && tries % 1000 === 0) opts.onProgress(tries);
        if (tries % yieldEvery === 0) {
          await new Promise((r) => setTimeout(r, 0));
        }
        continue;
      }
    }

    // Matched. Compute cPub and finalize.
    const cSeed = ringMaterial.slice(32, 64);
    const cPub = edLuck(cSeed);
    const pass = buildPassAtom(sPub, cPub, tweakAtom);
    const ringAtomBytes = buildRingAtomBytes(ringMaterial, tweakAtom);
    const feed = jamFeed(comet, opts.rift ?? 0, opts.life ?? 1, ringAtomBytes);

    return {
      seed, ringMaterial, sPub, cPub, tweakedSPub, comet,
      pass, ringAtomBytes, feed, tries,
    };
  }
  throw new Error(`miner: exceeded ${maxTries} tries`);
}

// Jam the feed noun: [[2 0] comet-atom rift [[life ring-atom] 0]]
// Mirrors hoon_jam in dump_jam_vectors.py.
export function jamFeed(
  cometBytes: Uint8Array,
  rift: number,
  life: number,
  ringAtomBytes: Uint8Array,
): Uint8Array {
  const cometAtom = bytesToAtomLE(cometBytes);
  const ringAtom = bytesToAtomLE(ringAtomBytes);

  type Noun = bigint | [Noun, Noun];
  const noun: Noun = [
    [2n, 0n],
    [cometAtom, [BigInt(rift), [[BigInt(life), ringAtom], 0n]]],
  ];
  return jam(noun);
}

// Minimal jam (Hoon ++jam) with back-references.
function jam(noun: bigint | [any, any]): Uint8Array {
  const w = new BitWriter();
  const refs = new Map<string, number>();

  function key(n: any): string {
    if (typeof n === "bigint") return `a:${n.toString(16)}`;
    return `c:(${key(n[0])}|${key(n[1])})`;
  }

  function encode(n: any): void {
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
