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
// The publication is the on-chain twin of a confidential xtr entry: `pass`
// binds the name (who = fig(pass)) and `opening` reveals the state committed in
// the transaction's sat-carrying output. A PRESENT blind-opening marks a spawn
// (and opens the hiding `dat` commitment); an ABSENT one marks a state update
// (rekey/breach) of an already-tracked comet.
//
// Golden vectors: groundwire/vectors/gw-kelvin-9.json ("basic".publication) +
// tests/kelvin9.spec.ts.

import { jam, type Noun } from "../protocol/jam.js";
import { concatBytes } from "../protocol/tagged-hash.js";
import { spawnNoun, type SpawnSont, KELVIN } from "./dat.js";
import { snapshotToNoun, type Snapshot } from "./snapshot.js";

export const MAX_PUBLICATION = 512;

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
// length byte and so stops at 255 — below this codec's own 512-byte cap, so a
// fief-carrying publication (265–269 bytes in practice) needs PUSHDATA2 (0x4d)
// and its TWO-byte LITTLE-ENDIAN length. Byte-for-byte identical to
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
