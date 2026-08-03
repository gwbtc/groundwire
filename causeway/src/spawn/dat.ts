// cc-draft-2 `dat` — the name-committing tweak data for suite-C
// confidential comets.
//
// Per the kernel spec (gwbtc/urbit cyc/cc-draft-2,
// doc/spec/confidential-comets.md + sur/stealth.hoon), the Schnorr tweak
// data hashed into a confidential comet's signing key is:
//
//     dat = (can 0 (mat dom) [256 txid] (mat vout) (mat off) ~)
//
// i.e. the +mat-encoded PKI domain tag at bit 0 — the kernel extracts it
// with (rub 0 dat) in +pass-pki-dom — followed by the spawn satpoint (the
// sat's location BEFORE the spawn commit spends it), laid out the way
// lib/urb-encoder's +en-sont writes satpoints: a fixed 256-bit txid (the
// numeric value of the display hex a block explorer shows), then
// mat(vout), then mat(off).
//
// The domain tag is 1:1 with the verifier agent registered with Jael via
// %anex — %gw-btc, the Groundwire Bitcoin-PKI domain agent (renamed from
// %urb-watcher / %groundwire). dat is hashed into the signing key, so the
// comet's @p commits to the tag forever — it is consensus-critical.
//
// Unlike the legacy v9 rap-3 tweak (see ./tweak.ts), the fixed-width txid
// field means a txid with leading zero bytes cannot shift the encoding,
// and the trailing mat always ends in a 1-bit, so the atom's byte length
// is unambiguous.
//
// Golden vectors: tests/dat.spec.ts, generated with `urbit eval` against
// the cyc/cc-draft-2 kernel.

import { BitWriter } from "../protocol/bitwriter.js";
import { rub } from "../protocol/mat.js";

export const DEFAULT_PKI_DOM = "gw-btc";

// Pack an ASCII term into its Hoon atom (LE bytes).
export function cordToAtom(s: string): bigint {
  let x = 0n;
  for (let i = s.length - 1; i >= 0; i--) {
    x = (x << 8n) | BigInt(s.charCodeAt(i));
  }
  return x;
}

export interface DatInputs {
  txidHex: string;          // 64-char display hex of the spawn satpoint's txid
  vout: number;             // output index the sat sits at pre-spawn
  off?: number | bigint;    // sat offset within that output (default 0)
  dom?: string;             // PKI domain tag (default %groundwire)
}

export function buildDatAtom(inputs: DatInputs): bigint {
  const clean = inputs.txidHex.replace(/^0x/, "").toLowerCase();
  if (clean.length !== 64) {
    throw new Error(`dat: expected 64 hex chars of txid, got ${clean.length}`);
  }
  const w = new BitWriter();
  w.writeMat(cordToAtom(inputs.dom ?? DEFAULT_PKI_DOM));
  w.write(256, BigInt("0x" + clean));
  w.writeMat(BigInt(inputs.vout));
  w.writeMat(BigInt(inputs.off ?? 0));
  return w.toInt();
}

// Minimal LE atom bytes — what the tweak hash (shax(ugn || dat)) and the
// miner consume. dat always ends in a mat's 1-bit, so ceil(bits/8) is the
// minimal byte length.
export function buildDatBytes(inputs: DatInputs): Uint8Array {
  let a = buildDatAtom(inputs);
  const out: number[] = [];
  while (a > 0n) {
    out.push(Number(a & 0xffn));
    a >>= 8n;
  }
  return new Uint8Array(out);
}

// The domain tag a receiving kernel would extract via +pass-pki-dom's
// (rub 0 dat). Round-trip check for callers and tests.
export function datPkiDom(dat: bigint): string {
  const { q } = rub(0, dat);
  let x = q;
  let s = "";
  while (x > 0n) {
    s += String.fromCharCode(Number(x & 0xffn));
    x >>= 8n;
  }
  return s;
}
