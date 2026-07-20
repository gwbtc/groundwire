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

import { jam } from "./mine-c.js";
import { bytesToAtomLE } from "../protocol/bitwriter.js";

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
