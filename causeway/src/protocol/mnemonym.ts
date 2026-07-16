// Mnemonyms — a BIP-39-style, human-memorable rendering of a Groundwire ID.
//
// A Groundwire comet's @p is a 128-bit atom (the shaf fingerprint of its
// tweaked networking key). Instead of the scrambled @p (~mosnyt-londen-…), we
// render that atom as a "mnemonym": the 128-bit value split into 11-bit groups,
// each indexing a 2048-word list, with a SHA-256 checksum appended (exactly
// BIP-39), then dot-joined. A leading "." marks a tweaked (Groundwire) ID; ".."
// an untweaked one. Leading all-zero words are dropped.
//
// Ported 1:1 from gwbtc/mnemonyms (libraries/hoon/lib/mnemonyms.hoon +
// libraries/python/mnemonyms.py); cross-checked bit-for-bit against that
// reference's shared test-vectors in tests/mnemonym.spec.ts.
//
// The @p remains the canonical machine identity (boot command, pier name,
// proof.json, sotx encoding); the mnemonym is a display/entry representation.

import { sha256 } from "@noble/hashes/sha256";
import { WORDLIST } from "./mnemonym-wordlist.js";
import { patpToAtom, atomToPatp } from "./patp.js";

// Groundwire comet @p's are 128-bit, tweaked (suite-C) identities.
export const COMET_STRENGTH = 128;
export const COMET_TWEAKED = true;

export interface MnemonymOpts {
  tweaked?: boolean;   // default true (Groundwire ids are tweaked)
  strength?: number;   // bit length, multiple of 32; default 128
}

function totalWords(strength: number): number {
  return (strength + strength / 32) / 11;
}

// Big-endian bit string of an atom padded to `bits` bits.
function bitsOf(value: bigint, bits: number): string {
  let s = value.toString(2);
  if (s.length > bits) throw new Error(`mnemonym: value exceeds ${bits} bits`);
  return s.padStart(bits, "0");
}

// Encode raw entropy bytes (big-endian, length === strength/8) as a mnemonym.
export function bytesToMnemonym(data: Uint8Array, opts: MnemonymOpts = {}): string {
  const tweaked = opts.tweaked ?? COMET_TWEAKED;
  const strength = opts.strength ?? COMET_STRENGTH;
  if (data.length !== strength / 8) {
    throw new Error(`mnemonym: expected ${strength / 8} bytes, got ${data.length}`);
  }
  const digest = sha256(data);
  let entropy = 0n;
  for (const b of data) entropy = (entropy << 8n) | BigInt(b);
  let checksum = 0n;
  for (const b of digest) checksum = (checksum << 8n) | BigInt(b);

  const csBits = strength / 32;
  const bits = bitsOf(entropy, strength)
    + bitsOf(checksum, 256).slice(0, csBits);

  const indices: number[] = [];
  for (let i = 0; i < bits.length / 11; i++) {
    indices.push(parseInt(bits.slice(i * 11, i * 11 + 11), 2));
  }
  while (indices.length && indices[0] === 0) indices.shift();

  const prefix = tweaked ? "." : "..";
  return prefix + indices.map((idx) => WORDLIST[idx]).join(".");
}

// Render a Groundwire ID atom (e.g. a comet @p) as a mnemonym.
export function atomToMnemonym(atom: bigint, opts: MnemonymOpts = {}): string {
  const strength = opts.strength ?? COMET_STRENGTH;
  const bytes = new Uint8Array(strength / 8);
  let a = atom;
  for (let i = bytes.length - 1; i >= 0; i--) { bytes[i] = Number(a & 0xffn); a >>= 8n; }
  if (a !== 0n) throw new Error(`mnemonym: atom exceeds ${strength} bits`);
  return bytesToMnemonym(bytes, opts);
}

// Map each word to its FIRST index. The reference wordlist contains one
// duplicate ("alike" at 128 and 129), and both the Python (list.index) and
// Hoon (find) references resolve a word to its first occurrence — so a
// mnemonym containing "alike" always decodes to 128. (Value 129 still encodes
// to "alike", so it is not round-trippable — a quirk we mirror exactly.)
const WORD_INDEX: ReadonlyMap<string, number> = (() => {
  const m = new Map<string, number>();
  WORDLIST.forEach((w, i) => { if (!m.has(w)) m.set(w, i); });
  return m;
})();

// Decode a mnemonym back to its raw entropy bytes, verifying the checksum.
// Throws on a bad prefix, unknown word, or checksum mismatch.
export function mnemonymToBytes(nym: string, opts: MnemonymOpts = {}): Uint8Array {
  const strength = opts.strength ?? COMET_STRENGTH;
  let body: string;
  if (nym.startsWith("..")) body = nym.slice(2);
  else if (nym.startsWith(".")) body = nym.slice(1);
  else throw new Error("mnemonym: must start with '.' or '..'");
  if (body.length === 0) throw new Error("mnemonym: empty");

  const words = body.split(".");
  const total = totalWords(strength);
  if (words.length > total) {
    throw new Error(`mnemonym: ${words.length} words exceeds ${total} for ${strength}-bit`);
  }
  const indices: number[] = new Array(total - words.length).fill(0);
  for (const w of words) {
    const idx = WORD_INDEX.get(w);
    if (idx === undefined) throw new Error(`mnemonym: unknown word "${w}"`);
    indices.push(idx);
  }
  const bits = indices.map((i) => i.toString(2).padStart(11, "0")).join("");
  const csBits = strength / 32;
  const entropyBits = bits.length - csBits;
  const entropy = BigInt("0b" + bits.slice(0, entropyBits));
  const data = new Uint8Array(strength / 8);
  let e = entropy;
  for (let i = data.length - 1; i >= 0; i--) { data[i] = Number(e & 0xffn); e >>= 8n; }

  const digest = sha256(data);
  let checksum = 0n;
  for (const b of digest) checksum = (checksum << 8n) | BigInt(b);
  const expected = bitsOf(checksum, 256).slice(0, csBits);
  if (bits.slice(entropyBits) !== expected) throw new Error("mnemonym: checksum mismatch");
  return data;
}

// Decode a mnemonym to its Groundwire ID atom (inverse of atomToMnemonym).
export function mnemonymToAtom(nym: string, opts: MnemonymOpts = {}): bigint {
  const data = mnemonymToBytes(nym, opts);
  let a = 0n;
  for (const b of data) a = (a << 8n) | BigInt(b);
  return a;
}

export function isMnemonym(s: string): boolean {
  return s.trim().startsWith(".");
}

// Parse a user-supplied Groundwire ID that may be a mnemonym (".…") or a @p
// ("~…"), returning the atom. Lets input fields accept either form now that
// IDs are shown as mnemonyms.
export function resolveId(input: string): bigint {
  const s = input.trim();
  return isMnemonym(s) ? mnemonymToAtom(s) : patpToAtom(s);
}

// Comets occupy [2^64, 2^128). Only comets are mnemonyms (the scheme is
// 128-bit; the Hoon +name asserts %pawn). Galaxies/stars/planets/moons have no
// comet mnemonym and are shown as @p.
const COMET_MIN = 1n << 64n;

// Clan-aware ID display: a comet renders as a mnemonym, anything else (e.g. a
// star sponsor) as its @p. Use this wherever an ID could be a comet OR a star.
export function displayId(atom: bigint): string {
  return atom >= COMET_MIN ? atomToMnemonym(atom) : atomToPatp(atom);
}

export function validateMnemonym(nym: string, opts: MnemonymOpts = {}): boolean {
  try { mnemonymToBytes(nym.trim(), opts); return true; } catch { return false; }
}

function splitNym(nym: string): { prefix: string; words: string[] } {
  const prefix = nym.startsWith("..") ? ".." : ".";
  return { prefix, words: nym.slice(prefix.length).split(".") };
}

// Compact form for tight UI (dropdowns, table cells): ".first...last".
// Mirrors +abridge in the Hoon reference; returns the full nym if ≤2 words.
export function abridgeMnemonym(nym: string): string {
  const { prefix, words } = splitNym(nym);
  if (words.length <= 2) return nym;
  return `${prefix}${words[0]}...${words[words.length - 1]}`;
}

// Medium-compact form: ".first.second..penultimate.last" (+foreshorten).
export function foreshortenMnemonym(nym: string): string {
  const { prefix, words } = splitNym(nym);
  if (words.length <= 4) return nym;
  const [a, b] = words;
  const pen = words[words.length - 2];
  const last = words[words.length - 1];
  return `${prefix}${a}.${b}..${pen}.${last}`;
}
