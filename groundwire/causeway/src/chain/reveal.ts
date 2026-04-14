// Build the reveal tx that spends the commit output via script-path,
// exposing the urb attestation leaf in the witness.
//
// Porting reference: gw-onboard.py lines ~1120-1168 (build_reveal_tx).
// Witness layout: [<schnorr_sig>, <leafScript>, <controlBlock>]
// Control block: (leafVersion | parity) || internalXOnly || merkleProof…
//
// TODO: full implementation.

import type { Transaction } from "@scure/btc-signer";

export interface RevealInputs {
  commit: { txid: string; vout: number; value: bigint; scriptPubKey: Uint8Array };
  leafScript: Uint8Array;
  commitInternalXOnly: Uint8Array;
  destScriptPubKey: Uint8Array;       // P2TR to the user's spend destination
  feeRate: number;
}

export interface BuiltReveal {
  tx: Transaction;
  sighash: Uint8Array;                // script-path sighash to be signed
}

export function buildReveal(_inputs: RevealInputs): BuiltReveal {
  throw new Error("buildReveal: not yet implemented — see gw-onboard.py ~1120-1168");
}
