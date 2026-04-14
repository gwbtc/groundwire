// Build the commit tx for the urb attestation reveal pattern.
//
// Porting reference: gw-onboard.py lines ~1024-1117 (build_commit_tx).
// The commit tx:
//   - spends a BIP-86 taproot UTXO (key-path)
//   - sends to a fresh P2TR address whose output key is tweaked from the
//     attestation leaf script's merkle root
// Fees are calculated at 2 sat/vB to match gw-onboard.
//
// TODO: full implementation once reveal.ts is ready. This file currently
// defines the shape we want to emit so the QR protocol layer can be built.

import { Transaction } from "@scure/btc-signer";

export interface CommitInputs {
  funding: { txid: string; vout: number; value: bigint; scriptPubKey: Uint8Array };
  internalXOnly: Uint8Array;          // BIP-86 key at m/86'/1'/0'/0/0
  leafScript: Uint8Array;             // urb-tagged tapscript from tapscript.ts
  commitInternalXOnly: Uint8Array;    // key at m/86'/1'/0'/0/1 (reveal internal)
  feeRate: number;                    // sat/vB
}

export interface BuiltCommit {
  tx: Transaction;
  commitOutputValue: bigint;
  commitOutputIndex: number;
}

export function buildCommit(_inputs: CommitInputs): BuiltCommit {
  throw new Error("buildCommit: not yet implemented — see gw-onboard.py ~1024-1117");
}
