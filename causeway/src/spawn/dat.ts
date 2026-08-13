// kelvin-9 %gw-btc `dat` — the HIDING, name-committing tweak data for
// suite-C confidential comets.
//
// Per the OP_RETURN revision (groundwire/doc/opret-revision, and the Hoon
// codec groundwire/lib/gw-btc-pass.hoon):
//
//     dat   = (can 0 (mat %gw-btc) (mat 9) [256 d] ~)
//     d     = H_tag("gw/spawn-commit", (jam spawn-sont) || blind)
//     blind = H_tag("gw/spawn-blind", seed)
//
// The domain tag is the leading +mat item at bit 0 — Ames extracts it with
// (rub 0 dat) and routes the pass to %gw-btc; nothing else in the kernel
// changes. The Kelvin (9) is plaintext, so any holder of a pass can read a
// comet's mint version without an opening. `d` is a HIDING commitment: the
// spawn satpoint is learned only from an explicit blind-opening (in the xtr,
// or in a public OP_RETURN publication), never parsed out of dat — this
// replaces the legacy clear-satpoint `can`/`mat` dat. Decoders reject trailing
// data: the bit-width is exactly p:(mat %gw-btc) + p:(mat 9) + 256.
//
// `dat` is hashed into the signing key, so the comet's @p commits to the
// domain tag, the Kelvin, and the hidden spawn commitment forever.
//
// `blind` derives from the ship's seed alone, which makes `dat` depend on the
// seed, so the miner recomputes it per candidate seed (see mine-c.ts).
//
// THAT DOES NOT MAKE THE OPENING RECOVERABLE IN THIS CLIENT, and this comment
// used to claim it did. The seed the blind derives from is 64 bytes of
// crypto.getRandomValues, and it is DISCARDED after mining: the ring carries
// sha512(seed), not the seed, and persist.ts deliberately stores no feed. So
// the blind survives only in the artifacts the user keeps — the downloaded
// feed file, and the xtr baked into the finalized feed — never in anything
// re-derivable from a wallet.
//
// The desktop is different: derive_blind_seed hangs the blind off a BIP-39
// phrase and the funding outpoint, so it genuinely is recoverable there.
// Porting that here is the real fix; see the task board.
//
// Golden vectors: groundwire/vectors/gw-kelvin-9.json + tests/kelvin9.spec.ts,
// pinned to the authoritative Hoon test tests/lib/gw-btc-pass.hoon.

import { BitWriter } from "../protocol/bitwriter.js";
import { rub } from "../protocol/mat.js";
import { jam, type Noun } from "../protocol/jam.js";
import { hTag, concatBytes, minimalLEBytes, bytesToAtomBE } from "../protocol/tagged-hash.js";

export const DEFAULT_PKI_DOM = "gw-btc";
export const KELVIN = 9;

export interface SpawnSont {
  txidHex: string;          // 64-char display hex of the spawn satpoint's txid
  vout: number;             // output index the sat sits at pre-spawn
  off?: number | bigint;    // sat offset within that output (default 0)
}

// Pack an ASCII term into its Hoon atom (LE bytes).
export function cordToAtom(s: string): bigint {
  let x = 0n;
  for (let i = s.length - 1; i >= 0; i--) {
    x = (x << 8n) | BigInt(s.charCodeAt(i));
  }
  return x;
}

// The spawn satpoint as a jammable noun: [txid [vout off]], txid = the numeric
// value of its display hex. jam(spawnNoun) is the `jam_spawn_le` vector.
export function spawnNoun(s: SpawnSont): Noun {
  const clean = s.txidHex.replace(/^0x/, "").toLowerCase();
  if (clean.length !== 64) {
    throw new Error(`dat: expected 64 hex chars of txid, got ${clean.length}`);
  }
  return [BigInt("0x" + clean), [BigInt(s.vout), BigInt(s.off ?? 0)]];
}

// blind = H_tag("gw/spawn-blind", seed_le_bytes). `seed` is the ship's master
// seed as an atom (bigint) or its minimal LE byte string; both hash the same
// minimal LE bytes.
export function makeBlind(seed: bigint | Uint8Array): Uint8Array {
  const atom = typeof seed === "bigint" ? seed : bytesToAtomBEofLE(seed);
  return hTag("gw/spawn-blind", minimalLEBytes(atom));
}

// A Uint8Array seed is an atom in LE byte order (LSB first), matching how the
// miner draws 64 random bytes and how Hoon reads `seed`.
function bytesToAtomBEofLE(b: Uint8Array): bigint {
  let x = 0n;
  for (let i = b.length - 1; i >= 0; i--) x = (x << 8n) | BigInt(b[i]!);
  return x;
}

// d = H_tag("gw/spawn-commit", jam(spawn-sont) || blind). `blind` is the raw
// 32 bytes.
export function spawnCommit(s: SpawnSont, blind: Uint8Array): Uint8Array {
  return spawnCommitFromJam(jam(spawnNoun(s)), blind);
}

// Same, from a precomputed jam(spawn-sont) — the miner reuses this across the
// whole search since the spawn satpoint is fixed and only `blind` varies.
export function spawnCommitFromJam(jamSpawn: Uint8Array, blind: Uint8Array): Uint8Array {
  return hTag("gw/spawn-commit", concatBytes(jamSpawn, blind));
}

// dat atom = can(0, [mat(dom), mat(9), [256, d]]).
export function datFromCommit(d: Uint8Array, dom = DEFAULT_PKI_DOM): bigint {
  const w = new BitWriter();
  w.writeMat(cordToAtom(dom));
  w.writeMat(BigInt(KELVIN));
  w.write(256, bytesToAtomBE(d));
  return w.toInt();
}

export function buildDatAtom(s: SpawnSont, seed: bigint | Uint8Array, dom = DEFAULT_PKI_DOM): bigint {
  return datFromCommit(spawnCommit(s, makeBlind(seed)), dom);
}

// Minimal LE atom bytes — the tweak the miner hashes into the signing key and
// what `comet-miner` consumes via u3r_bytes_all. dat always ends in a mat's
// 1-bit, so ceil(bits/8) is the minimal byte length.
export function buildDatBytes(s: SpawnSont, seed: bigint | Uint8Array, dom = DEFAULT_PKI_DOM): Uint8Array {
  return minimalLEBytes(buildDatAtom(s, seed, dom));
}

// The domain tag a receiving kernel would extract via +pass-pki-dom's
// (rub 0 dat). Round-trip check for callers and tests.
export function datPkiDom(dat: bigint): string {
  const { q } = rub(0, dat);
  let x = q;
  let s = "";
  while (x > 0n) {
    s += String.fromCharCode(Number(x & 0xffn));
    x >>= 8n;
  }
  return s;
}
