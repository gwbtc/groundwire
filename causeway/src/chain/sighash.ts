// Thin wrappers around @scure/btc-signer for computing BIP-341 taproot
// sighashes. See @scure/btc-signer's preimageWitnessV1:
//   preimageWitnessV1(idx, prevOutScripts, hashType, amounts,
//                     codeSep?, leafScript?, leafVer?, annex?)

import type { Transaction } from "@scure/btc-signer";

const SIGHASH_DEFAULT = 0;
const LEAF_VER_TAPSCRIPT = 0xc0;

export function keyPathSighash(
  tx: Transaction,
  idx: number,
  prevOutScripts: Uint8Array[],
  amounts: bigint[],
): Uint8Array {
  return tx.preimageWitnessV1(idx, prevOutScripts, SIGHASH_DEFAULT, amounts);
}

export function scriptPathSighash(
  tx: Transaction,
  idx: number,
  prevOutScripts: Uint8Array[],
  amounts: bigint[],
  leafScript: Uint8Array,
): Uint8Array {
  return tx.preimageWitnessV1(
    idx,
    prevOutScripts,
    SIGHASH_DEFAULT,
    amounts,
    undefined,
    leafScript,
    LEAF_VER_TAPSCRIPT,
  );
}
