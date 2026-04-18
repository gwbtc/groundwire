// Build the commit PSBT for the urb attestation reveal pattern.
//
// Porting reference: gw-onboard.py lines ~1024-1117.
// The commit tx spends a funding UTXO (key-path) and sends to a P2TR
// address whose script tree contains the attestation leaf.

import {
  buildCommitPsbt,
  type CommitResult,
  type KeyInfo,
  type Utxo,
} from "../signing/psbt.js";
import { urbLeafScript } from "./tapscript.js";

export interface CommitInputs {
  funding: Utxo;
  fundingKey: KeyInfo;
  commitKey: KeyInfo;
  attestationData: Uint8Array;
  feeRate: number;
}

export function buildCommit(inputs: CommitInputs): CommitResult {
  const leafScript = urbLeafScript(inputs.attestationData, inputs.commitKey.internalKey);
  return buildCommitPsbt({
    funding: inputs.funding,
    fundingKey: inputs.fundingKey,
    commitKey: inputs.commitKey,
    leafScript,
    feeRate: inputs.feeRate,
  });
}
