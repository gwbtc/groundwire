// kelvin-9 committed state snapshot + its taproot commitment (spec §3, and the
// Hoon codec groundwire/lib/gw-btc-pass.hoon +state-commit / +state-leaf /
// +state-key).
//
// Ongoing networking state is committed by tweaking the taproot output key of
// the sat-bearing output — the sat and its commitment travel together. A chain
// observer sees only the P2TR output key Q, indistinguishable from any other
// taproot key; the snapshot is revealed only to attestation verifiers (via the
// xtr opening) or on-chain when a public comet deliberately publishes.
//
//     snapshot = [life rift key sponsor=(unit @p) fief=(unit fief)]
//     c        = H_tag("gw/state-commit", (jam snapshot))
//     leaf     = OP_RETURN PUSH2 'gw' PUSH32 <c>            (0x6a 02 6777 20 || c)
//     root     = H_TapLeaf(0xc0 || compact_size(leaf) || leaf)   (single-leaf tree)
//     Q        = x( lift_x(P) + H_TapTweak(x(P) || root)*G )
//
// Golden vectors: groundwire/vectors/gw-kelvin-9.json ("state-key-pin") +
// tests/kelvin9.spec.ts, pinned to tests/lib/gw-btc-pass.hoon.

import { secp256k1 } from "@noble/curves/secp256k1";
import { jam, type Noun } from "../protocol/jam.js";
import {
  hTag, concatBytes, bytesToHex, hexToBytes, bytesToAtomBE,
} from "../protocol/tagged-hash.js";

export const TAP_LEAF_VERSION = 0xc0;

export interface Snapshot {
  life: number;
  rift: number;
  key: bigint;              // current messaging key (cry.pub of the suite-C pass)
  sponsor: bigint | null;   // a @p atom, or null (self-sponsorship)
  fief: null;               // (unit fief); always null in v9
}

// A snapshot with NEITHER a sponsor NOR a fief is a one-way identity: the
// verifier projects an absent sponsor to SELF in the Jael udiff
// (+urb-point-to-jael in app/gw-btc.hoon), so nothing can route to it and once
// a peer drops its state the comet can never be re-contacted. It is legal
// protocol (see doc/opret-revision/04-decisions-addendum.md §2 "Fief scope"),
// so the verifier must never reject it — but Causeway refuses to MINT one
// unless the operator deliberately opts out.
export function isRoutable(s: Snapshot): boolean {
  return s.sponsor !== null || s.fief !== null;
}

export const NO_ROUTE_MESSAGE =
  "this snapshot has neither a sponsor nor a fief, so nothing can cold-contact "
  + "the comet: an absent sponsor projects to self-sponsorship, and once a peer "
  + "drops its state the identity is unreachable forever. Pick a sponsor, or "
  + "tick \"unroutable (no-route)\" / pass --no-route if you really want an "
  + "outbound-only identity.";

export function assertRoutable(s: Snapshot, noRoute = false): void {
  if (noRoute || isRoutable(s)) return;
  throw new Error(NO_ROUTE_MESSAGE);
}

// (unit x): null → 0; a full unit [~ v] → [0, v].
function unit(v: bigint | null): Noun {
  return v === null ? 0n : [0n, v];
}

// snapshot as a noun: [life [rift [key [sponsor fief]]]].
export function snapshotToNoun(s: Snapshot): Noun {
  return [BigInt(s.life), [BigInt(s.rift), [s.key, [unit(s.sponsor), unit(s.fief)]]]];
}

// c = H_tag("gw/state-commit", (jam snapshot)) — 32 bytes.
export function stateCommit(s: Snapshot): Uint8Array {
  return hTag("gw/state-commit", jam(snapshotToNoun(s)));
}

// The unspendable commitment tapleaf script: OP_RETURN PUSH2 'gw' PUSH32 <c>,
// 37 bytes. Execution fails at OP_RETURN, so the script path is provably
// unspendable and every custody spend is key-path by construction.
export function stateLeafScript(c: Uint8Array): Uint8Array {
  if (c.length !== 32) throw new Error("stateLeafScript: c must be 32 bytes");
  return concatBytes(Uint8Array.of(0x6a, 0x02, 0x67, 0x77, 0x20), c);
}

// Bitcoin compact-size (varint) encoding.
function compactSize(n: number): Uint8Array {
  if (n < 0xfd) return Uint8Array.of(n);
  if (n <= 0xffff) return Uint8Array.of(0xfd, n & 0xff, (n >> 8) & 0xff);
  return Uint8Array.of(0xfe, n & 0xff, (n >> 8) & 0xff, (n >> 16) & 0xff, (n >> 24) & 0xff);
}

// TapLeaf hash. For the single-leaf state tree this is also the merkle root.
export function leafHash(script: Uint8Array): Uint8Array {
  return hTag("TapLeaf", concatBytes(Uint8Array.of(TAP_LEAF_VERSION), compactSize(script.length), script));
}

// The merkle root committing `snap` = leaf hash of its state leaf.
export function stateMerkleRoot(snap: Snapshot): Uint8Array {
  return leafHash(stateLeafScript(stateCommit(snap)));
}

// The 32-byte x-only P2TR output key Q committing `snap` under internal key P
// (33-byte compressed). Single-leaf tree, so root == leaf hash. The
// sat-carrying output's scriptPubKey is OP_1 PUSH32 Q (0x51 0x20 || Q).
export function stateOutputKey(internalKey33: Uint8Array, snap: Snapshot): Uint8Array {
  if (internalKey33.length !== 33) throw new Error("stateOutputKey: internal key must be 33-byte compressed");
  return outputPubkey(internalKey33, stateMerkleRoot(snap));
}

// BIP-341 output-key tweak: Q = lift_x(x(P)) + H_TapTweak(x(P) || root)*G.
export function outputPubkey(internalKey33: Uint8Array, merkleRoot: Uint8Array): Uint8Array {
  const x = internalKey33.slice(1); // 32-byte x-only
  // lift_x: the unique even-y point with this x-coordinate.
  const pEven = secp256k1.Point.fromHex("02" + bytesToHex(x));
  const t = bytesToAtomBE(hTag("TapTweak", concatBytes(x, merkleRoot)));
  const q = pEven.add(secp256k1.Point.BASE.multiply(t));
  return hexToBytes(q.x.toString(16).padStart(64, "0"));
}

// scriptPubKey for the sat-carrying output: OP_1 PUSH32 Q.
export function stateOutputScript(q: Uint8Array): Uint8Array {
  if (q.length !== 32) throw new Error("stateOutputScript: Q must be 32 bytes");
  return concatBytes(Uint8Array.of(0x51, 0x20), q);
}
