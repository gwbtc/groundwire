// kelvin-9 OP_RETURN publication output (spec §6, decisions-addendum §4, and
// the Hoon codec groundwire/lib/gw-btc-pass.hoon +publication-script /
// +make-publication).
//
// Publication is DEFAULT OFF: a confidential custody transaction carries no
// on-chain payload at all. When a PUBLIC comet deliberately publishes, it adds
// one OP_RETURN output to its custody (spawn or state-update) transaction:
//
//     scriptPubKey = OP_RETURN PUSH3 'urb' PUSH1 <kelvin=0x09> <pushdata payload>
//     payload      = (jam publication)
//     publication  = [pass opening]
//     opening      = [internal-key snapshot blind-opening]
//     blind-opening = (unit [spawn-sont start-height blind])
//
// The payload is the comet's FULL ATTESTATION PACKET. `pass` is byte-for-byte
// the pass it hands a peer over ames — name, hiding `dat` commitment, and the
// whole custody log in its xtr (see passWithXtr in ./mine-c.ts) — and a watcher
// runs it through the same verification a mailed attestation gets.
//
// `opening` is the one hop the packet cannot contain: a publication rides the
// comet's own custody transaction, whose txid does not exist until it is
// signed, so the log in xtr ends at the satpoint this transaction SPENDS and
// `opening` reveals the state it commits. The watcher is reading the block, so
// it completes the log itself with [txid height opening].
//
// A spawn publication is the degenerate case, not a special one: xtr is empty
// and the completed log is the single entry whose blind-opening opens `dat`.
//
// Golden vectors: groundwire/vectors/gw-kelvin-9.json ("basic".publication,
// "full-packet") + tests/kelvin9.spec.ts.

import { jam, type Noun } from "../protocol/jam.js";
import { concatBytes } from "../protocol/tagged-hash.js";
import { spawnNoun, type SpawnSont, KELVIN } from "./dat.js";
import { snapshotToNoun, type Snapshot } from "./snapshot.js";

// The byte cap on a publication payload.  1024, and the number is not
// arbitrary: it is THE PACKET BOUND.  Decisions addendum §6 fixes a complete
// jammed first-contact attestation at one Mesa fragment (~1 KiB), and a
// publication carries that same packet — a payload this codec would accept but
// ames could never carry would describe an identity that can be published and
// then never attest.
//
// A pass core is ~108 B, entry 0's opening ~120 B and the terminal opening
// ~100 B, so the floor is ~330 B and each further custody hop adds ~40 B: 1024
// is ~17 hops, against the four that 512 allowed.  An OP_RETURN is all
// non-witness data, so payload bytes convert ~1:1 into vbytes — ~1160 vB,
// ~2320 sats at 2 sat/vB.  The hard ceiling is MAX_SCRIPT_SIZE (10000).
//
// MUST equal +max-publication:gw-btc-pass and MAX_PUBLICATION in
// causeway/desktop/causeway.py, byte for byte.
export const MAX_PUBLICATION = 1024;

export interface BlindOpening {
  spawnSont: SpawnSont;
  startHeight: number;   // block height of the spawn tx (transport metadata; not hashed)
  blind: bigint;         // the 32-byte blind as a big-endian @ux atom
}

export interface Opening {
  internalKey: bigint;             // 33-byte compressed internal key as a @ux atom
  snapshot: Snapshot;
  blindOpening?: BlindOpening | null;  // present marks a spawn; absent a state update
}

// opening = [internal-key [snapshot blind-opening]].
export function openingNoun(o: Opening): Noun {
  const bo: Noun = o.blindOpening == null
    ? 0n
    : [0n, [spawnNoun(o.blindOpening.spawnSont), [BigInt(o.blindOpening.startHeight), o.blindOpening.blind]]];
  return [o.internalKey, [snapshotToNoun(o.snapshot), bo]];
}

// publication = [pass opening].
export function publicationNoun(pass: bigint, o: Opening): Noun {
  return [pass, openingNoun(o)];
}

// The minimal Bitcoin push opcode(s) for `payload`.
//
// A direct push (opcode = length) reaches 75. PUSHDATA1 (0x4c) carries ONE
// length byte and so stops at 255 — far below this codec's own 1024-byte cap,
// so a fief-carrying publication (265–269 bytes in practice) already needs
// PUSHDATA2 (0x4d) and its TWO-byte LITTLE-ENDIAN length, and a full-packet one
// is never anything else. Byte-for-byte identical to
// +push-data:gw-btc-pass and push_data() in causeway/desktop/causeway.py.
//
// Note Uint8Array.of(0x4c, n) SILENTLY takes n mod 256 — which is exactly how a
// >255-byte payload used to produce a corrupt script here. Anything this cannot
// express throws.
export function pushData(payload: Uint8Array): Uint8Array {
  const n = payload.length;
  if (n <= 75) return Uint8Array.of(n);
  if (n <= 0xff) return Uint8Array.of(0x4c, n);
  if (n <= 0xffff) return Uint8Array.of(0x4d, n & 0xff, (n >>> 8) & 0xff);
  throw new Error(`publication: push ${n} bytes is too big for OP_PUSHDATA2`);
}

// The full OP_RETURN scriptPubKey for a publication. Payloads over 75 bytes use
// PUSHDATA1 (0x4c len), over 255 PUSHDATA2 (0x4d len-lo len-hi); payloads over
// MAX_PUBLICATION are rejected.
export function buildPublicationScript(pass: bigint, o: Opening): Uint8Array {
  const payload = jam(publicationNoun(pass, o));
  if (payload.length > MAX_PUBLICATION) {
    throw new Error(`publication: payload ${payload.length} > ${MAX_PUBLICATION} bytes`);
  }
  // 6a 03 'urb'(75 72 62) 01 <kelvin> <pushdata> <payload>
  const prefix = Uint8Array.of(0x6a, 0x03, 0x75, 0x72, 0x62, 0x01, KELVIN);
  return concatBytes(prefix, pushData(payload), payload);
}
