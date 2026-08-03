// kelvin-9 `xtr` — the off-chain custody/attestation log riding in a
// confidential comet's pass (spec §5, decisions-addendum §2.5).
//
// xtr is NOT hashed into the key (the @p commits to ugn+dat only, see
// ./dat.ts), so it grows over the comet's lifetime without changing the name;
// peers' domain agents parse it to verify the on-chain ownership chain, and
// the kernel's %anew flow refreshes it when the sat moves.
//
//     xtr     = (jam log)
//     log     = (list entry)                 :: oldest first, terminates in ~
//     entry   = [txid=@ux height=@ud opening=(unit opening)]
//     opening = [internal-key snapshot blind-opening=(unit [spawn blind])]
//
// An entry that carries an opening lets the verifier recompute the state-key Q
// and compare it against the transaction's sat-carrying output. A bare entry
// (opening omitted) is a pure custody transfer that only proves the sat moved;
// only the LATEST state-bearing entry is authoritative. The spawn entry
// (normally entry 0) carries a blind-opening, which also opens the hiding
// `dat` commitment.
//
// Golden vectors: groundwire/vectors/gw-kelvin-9.json ("basic".xtr) +
// tests/kelvin9.spec.ts.

import { jam, type Noun } from "../protocol/jam.js";
import { bakeXtrIntoFeed } from "./mine-c.js";
import { bytesToAtomLE } from "../protocol/bitwriter.js";
import { openingNoun, type Opening } from "./publication.js";
import { cue } from "../oracle/cue.js";
import { asAtom, head, tail } from "../oracle/noun.js";

export interface XtrEntry {
  txidHex: string;          // display hex of the (spawn / update / transfer) tx
  blockHeight: number;      // height of its containing block
  opening?: Opening | null; // omit for a pure custody-transfer hop
}

// entry = [txid [height opening=(unit opening)]].
function entryNoun(e: XtrEntry): Noun {
  const ou: Noun = e.opening == null ? 0n : [0n, openingNoun(e.opening)];
  return [BigInt("0x" + e.txidHex.replace(/^0x/, "")), [BigInt(e.blockHeight), ou]];
}

// xtr = (jam log), log oldest-first, terminating in ~. Returned as the atom
// (bytesToAtomLE of the jam) so it can ride in the ring/pass.
export function buildXtrAtom(entries: XtrEntry[]): bigint {
  let log: Noun = 0n;
  for (let i = entries.length - 1; i >= 0; i--) {
    log = [entryNoun(entries[i]!), log];
  }
  return bytesToAtomLE(jam(log));
}

// Re-bake a boot-feed ATOM (e.g. cued from a user's saved 0w… feed) around an
// xtr-appended ring — the web twin of desktop `causeway finalize --feed`. It
// cues the feed [[2 0] comet rift [[life ring] 0]], appends xtr to the ring,
// and re-jams, returning the new feed bytes. Used by the resume path, where
// `mined` (with its ring/comet) is gone from memory and the feed is re-supplied.
export function bakeXtrIntoFeedAtom(feedAtom: bigint, xtr: bigint): Uint8Array {
  const feed = cue(feedAtom);
  // [[2 0] [comet [rift [[life ring] 0]]]]
  const body = tail(feed);
  const comet = asAtom(head(body));
  const rest = tail(body);
  const rift = Number(asAtom(head(rest)));
  const lifeRing = head(tail(rest)); // [life ring]
  const life = Number(asAtom(head(lifeRing)));
  const ringAtom = asAtom(tail(lifeRing));
  const ringBytes = atomToBytesLE(ringAtom);
  return bakeXtrIntoFeed(comet, ringBytes, xtr, rift, life);
}

function atomToBytesLE(a: bigint): Uint8Array {
  const out: number[] = [];
  let x = a;
  while (x > 0n) {
    out.push(Number(x & 0xffn));
    x >>= 8n;
  }
  return new Uint8Array(out);
}
