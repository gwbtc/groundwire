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

// The full OP_RETURN scriptPubKey for a publication. Payloads over 75 bytes use
// PUSHDATA1 (0x4c len); payloads over MAX_PUBLICATION are rejected.
export function buildPublicationScript(pass: bigint, o: Opening): Uint8Array {
  const payload = jam(publicationNoun(pass, o));
  if (payload.length > MAX_PUBLICATION) {
    throw new Error(`publication: payload ${payload.length} > ${MAX_PUBLICATION} bytes`);
  }
  const psh = payload.length <= 75
    ? Uint8Array.of(payload.length)
    : Uint8Array.of(0x4c, payload.length);
  // 6a 03 'urb'(75 72 62) 01 <kelvin> <pushdata> <payload>
  const prefix = Uint8Array.of(0x6a, 0x03, 0x75, 0x72, 0x62, 0x01, KELVIN);
  return concatBytes(prefix, psh, payload);
}
