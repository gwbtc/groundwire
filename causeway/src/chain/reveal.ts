// Build the reveal PSBT that spends the commit output via script-path,
// exposing the urb attestation leaf in the witness.
//
// Porting reference: gw-onboard.py lines ~1125-1168.
// The inscription sat lands at offset 0 of the first output.

import {
  buildRevealPsbt,
  type KeyInfo,
  type RevealResult,
} from "../signing/psbt.js";
import { urbLeafScript } from "./tapscript.js";

export interface RevealInputs {
  commitTxid: Uint8Array;
  commitOutputValue: bigint;
  commitOutputScript: Uint8Array;
  commitKey: KeyInfo;
  destKey: KeyInfo;
  attestationData: Uint8Array;
  feeRate: number;
}

export function buildReveal(inputs: RevealInputs): RevealResult {
  const leafScript = urbLeafScript(inputs.attestationData, inputs.commitKey.internalKey);
  return buildRevealPsbt({
    commitTxid: inputs.commitTxid,
    commitOutputValue: inputs.commitOutputValue,
    commitOutputScript: inputs.commitOutputScript,
    commitKey: inputs.commitKey,
    leafScript,
    destKey: inputs.destKey,
    feeRate: inputs.feeRate,
  });
}
