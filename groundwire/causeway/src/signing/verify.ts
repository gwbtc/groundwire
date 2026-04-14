import { schnorr } from "@noble/curves/secp256k1";
import type { SignRequest, SignResponse } from "./qr-protocol.js";

export function verifyResponse(req: SignRequest, res: SignResponse): void {
  if (res.v !== 1) throw new Error("verify: bad version");
  if (res.nonce.length !== req.nonce.length) throw new Error("verify: nonce length");
  for (let i = 0; i < res.nonce.length; i++) {
    if (res.nonce[i] !== req.nonce[i]) throw new Error("verify: nonce mismatch");
  }
  if (res.sigs.length !== req.sighashes.length) {
    throw new Error(`verify: expected ${req.sighashes.length} sigs, got ${res.sigs.length}`);
  }
  for (let i = 0; i < res.sigs.length; i++) {
    const sig = res.sigs[i]!;
    const sh = req.sighashes[i]!;
    if (sig.length !== 64) throw new Error(`verify: sig[${i}] length != 64`);
    if (!schnorr.verify(sig, sh.hash, req.authKey)) {
      throw new Error(`verify: sig[${i}] schnorr verify failed`);
    }
  }
}
