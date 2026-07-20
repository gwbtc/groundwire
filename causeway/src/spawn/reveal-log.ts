// cc-draft-2 `xtr` — the off-chain reveal log riding in a confidential
// comet's pass.
//
// xtr is NOT hashed into the key (the @p commits to ugn+dat only, see
// ./dat.ts), so it grows over the comet's lifetime without changing the
// name; peers' domain agents parse it to verify the on-chain ownership
// chain, and the kernel's %anew flow refreshes it when the sat moves.
//
// The kernel treats xtr as opaque bits and the agent-side decoder is not
// yet pinned (spec §4 item 3), so this encoding is Causeway's PROPOSAL —
// the spec's fetch-based "variant B" (§8) married to the protocol-2.0
// +link shape (sur/self-attestation.hoon on hd/cc-e2e, minus `sots`,
// which verifiers re-derive from the leaf script):
//
//     xtr = (jam log)
//     log = (list [txid=@ux block=@ux internal-key=@ux tapleaf=[version=@ux script=octs]])
//     octs = [wid=@ud dat=@ux]
//
// oldest-first: entry 0 is the spawn commit. txid / block-hash / key /
// script atoms are the numeric values of their display hex (hexb:bitcoin
// convention). `block` is the containing block's HASH so verifiers can
// getrawtransaction without -txindex, exactly like protocol 2.0.
//
// The Python twin is build_xtr_atom in desktop/causeway.py; both are
// pinned to the same `urbit eval` golden vectors.

import { jam } from "./mine-c.js";
import { bytesToAtomLE } from "../protocol/bitwriter.js";

export interface RevealEntry {
  txidHex: string;          // display hex of the commit tx
  blockHashHex: string;     // display hex of its containing block's hash
  internalKeyHex: string;   // 33-byte compressed internal pubkey P (0x02||xonly)
  leafVersion: number;      // BIP-342 leaf version, 0xc0
  leafScriptHex: string;    // the committed `urb` tapleaf script bytes
}

type Noun = bigint | [Noun, Noun];

export function buildXtrAtom(entries: RevealEntry[]): bigint {
  let log: Noun = 0n;
  for (let i = entries.length - 1; i >= 0; i--) {
    const e = entries[i]!;
    const node: Noun = [
      BigInt("0x" + e.txidHex),
      [
        BigInt("0x" + e.blockHashHex),
        [
          BigInt("0x" + e.internalKeyHex),
          [
            BigInt(e.leafVersion),
            [BigInt(e.leafScriptHex.length / 2), BigInt("0x" + e.leafScriptHex)],
          ],
        ],
      ],
    ];
    log = [node, log];
  }
  return bytesToAtomLE(jam(log));
}
