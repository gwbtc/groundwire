// cc-draft-2 `xtr` — the off-chain reveal log riding in a confidential
// comet's pass.
//
// xtr is NOT hashed into the key (the @p commits to ugn+dat only, see
// ./dat.ts), so it grows over the comet's lifetime without changing the
// name; peers' domain agents parse it to verify the on-chain ownership
// chain, and the kernel's %anew flow refreshes it when the sat moves.
//
// This encoding is now PINNED to the %gw-btc agent's decoder
// (groundwire lib/gw-verify `$custody-log`, branch hd/gw-btc): the
// spec's fetch-based "variant B" (§8) with STATE COMMITMENTS (§2.1) —
//
//     xtr = (jam log)
//     log = (list [txid=@ux block-height=@ud reveal=(unit reveal)])
//     reveal = [internal-key=@ux leaf-version=@ux leaf-script=[wid=@ud dat=@ux]]
//
// oldest-first: entry 0 is the spawn commit. `block-height` is the
// containing block's HEIGHT (the agent resolves it to a hash and fetches
// the tx in-block, so no -txindex is required). A `reveal` is present on
// every entry that re-attests networking state (spawn / rekey / escape /
// …); a bare entry (`reveal` omitted) is a pure custody transfer that only
// proves the sat moved. Only the LATEST state-bearing entry is
// authoritative. txid / key / script atoms are the numeric values of their
// display hex (hexb:bitcoin convention).
//
// The Python twin is build_xtr_atom in desktop/causeway.py; both are
// pinned to the same `urbit eval` golden vectors (tests/dat.spec.ts).

import { jam, bakeXtrIntoFeed } from "./mine-c.js";
import { bytesToAtomLE } from "../protocol/bitwriter.js";
import { cue } from "../oracle/cue.js";
import { asAtom, head, tail } from "../oracle/noun.js";

export interface XtrReveal {
  internalKeyHex: string;   // 33-byte compressed internal pubkey P (0x02||xonly)
  leafVersion: number;      // BIP-342 leaf version, 0xc0
  leafScriptHex: string;    // the committed `urb` tapleaf script bytes
}

export interface XtrEntry {
  txidHex: string;          // display hex of the (commit / op / transfer) tx
  blockHeight: number;      // height of its containing block
  reveal?: XtrReveal;       // omit for a pure custody-transfer hop
}

type Noun = bigint | [Noun, Noun];

function revealNoun(r: XtrReveal | undefined): Noun {
  if (r === undefined) return 0n; // ~
  const tapleaf: Noun = [
    BigInt(r.leafVersion),
    [BigInt(r.leafScriptHex.length / 2), BigInt("0x" + r.leafScriptHex)],
  ];
  // [~ [internal-key leaf-version leaf-script]] — the (unit reveal) some-case
  return [0n, [BigInt("0x" + r.internalKeyHex), tapleaf]];
}

export function buildXtrAtom(entries: XtrEntry[]): bigint {
  let log: Noun = 0n;
  for (let i = entries.length - 1; i >= 0; i--) {
    const e = entries[i]!;
    const node: Noun = [
      BigInt("0x" + e.txidHex),
      [BigInt(e.blockHeight), revealNoun(e.reveal)],
    ];
    log = [node, log];
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
