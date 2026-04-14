import { schnorr, secp256k1 } from "@noble/curves/secp256k1";
import { sha256 } from "@noble/hashes/sha256";
import { concatBytes } from "@noble/hashes/utils";

// BIP-341: t = tagged_hash("TapTweak", P || merkleRoot?)
// Returns the tweaked xonly output key.
const TAP_TWEAK_TAG = "TapTweak";

function taggedHash(tag: string, ...msgs: Uint8Array[]): Uint8Array {
  const tagHash = sha256(new TextEncoder().encode(tag));
  return sha256(concatBytes(tagHash, tagHash, ...msgs));
}

export function tweakXOnly(internalXOnly: Uint8Array, merkleRoot?: Uint8Array): {
  output: Uint8Array;
  parity: 0 | 1;
} {
  if (internalXOnly.length !== 32) throw new Error("internal key must be 32 bytes");
  const t = merkleRoot
    ? taggedHash(TAP_TWEAK_TAG, internalXOnly, merkleRoot)
    : taggedHash(TAP_TWEAK_TAG, internalXOnly);
  const P = secp256k1.ProjectivePoint.fromHex(
    `02${Array.from(internalXOnly, (b) => b.toString(16).padStart(2, "0")).join("")}`,
  );
  const Q = P.add(secp256k1.ProjectivePoint.BASE.multiply(
    BigInt("0x" + Array.from(t, (b) => b.toString(16).padStart(2, "0")).join("")),
  ));
  const affine = Q.toAffine();
  const xBytes = Q.toRawBytes(true).slice(1);
  const parity: 0 | 1 = affine.y % 2n === 0n ? 0 : 1;
  return { output: xBytes, parity };
}

export { schnorr };
