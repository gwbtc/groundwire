// LEGACY (v9) tweak — the atom bytes lib/urb-core uses to validate an
// ON-CHAIN (public, revealed) %spawn's networking key. New confidential
// comets use the cc-draft-2 `dat` instead (see ./dat.ts): the kernel's
// +pass-pki-dom requires a mat-encoded domain tag at bit 0, which this
// format does not provide. Keep using v9 only for the public spawn flow,
// which today's chain watcher verifies against this exact format.
//
// NB: `rap 3` concatenates each atom's MINIMAL LE bytes — a txid whose
// display hex has leading zero bytes contributes fewer than 32 bytes in
// Hoon, which the fixed 32-byte txidHexToAtomBytes below does not
// reproduce (~1/256 of txids). The cc-draft-2 dat encoding has no such
// hazard (fixed 256-bit field). See the review notes before relying on
// this for fresh mainnet spawns.
//
// Hoon source (lib/urb-core.hoon:363-375):
//
//   =/  tweak
//     %+  rap 3
//     :~  %9            :: atom 9 (version tag)
//         ~tyr          :: galaxy 153
//         %urb-watcher  :: cord bytes LE
//         %btc          :: cord bytes LE
//         %gw           :: cord bytes LE
//         %9            :: atom 9 (fixed sotx-set version)
//         txid          :: @ux — 32-byte tx hash as LE atom
//         vout          :: @ud — output index
//         off           :: @ud — byte offset within output
//     ==
//
// `rap 3` concatenates each atom's minimal LE byte representation at bloq 3
// (byte width). Atoms whose bit-length is 0 (value 0) contribute 0 bytes.
//
// Reference: gw-onboard.py:862-886.

// Convert display-order txid hex (big-endian display) to the Hoon @ux atom's
// MINIMAL little-endian bytes — what `rap 3` actually concatenates ((met 3 txid)
// bytes). A fixed 32 bytes is wrong when the display hex has leading zero bytes
// (~1/256 txids): those become trailing zero LE bytes that `met` drops, so a
// fixed-width encoding shifts every following element and mismatches urb-core.
function txidHexToAtomBytes(displayHex: string): Uint8Array {
  const clean = displayHex.replace(/^0x/, "").toLowerCase();
  if (clean.length !== 64) throw new Error(`expected 64 hex chars, got ${clean.length}`);
  // Display hex 0x0123...  →  atom bytes LE = bytes of display hex reversed
  const full = new Uint8Array(32);
  for (let i = 0; i < 32; i++) {
    full[i] = parseInt(clean.slice((31 - i) * 2, (31 - i) * 2 + 2), 16);
  }
  let end = 32;
  while (end > 0 && full[end - 1] === 0) end--; // minimal LE bytes (met 3)
  return full.subarray(0, end);
}

// Minimum-length LE bytes of a non-negative integer. Returns empty array for 0
// (matching Hoon's (met 3 0) = 0).
function uintToMinLEBytes(n: number | bigint): Uint8Array {
  let x = BigInt(n);
  if (x < 0n) throw new Error("tweak: negative value");
  if (x === 0n) return new Uint8Array();
  const bytes: number[] = [];
  while (x > 0n) {
    bytes.push(Number(x & 0xffn));
    x >>= 8n;
  }
  return new Uint8Array(bytes);
}

function concat(...parts: Uint8Array[]): Uint8Array {
  const n = parts.reduce((s, p) => s + p.length, 0);
  const out = new Uint8Array(n);
  let off = 0;
  for (const p of parts) { out.set(p, off); off += p.length; }
  return out;
}

// Pack a short ASCII "cord" into its Hoon atom bytes (LE). The minimal-byte
// representation matches rap-3 semantics: no trailing zero bytes beyond the
// last non-null character.
function cordBytes(s: string): Uint8Array {
  const raw = new TextEncoder().encode(s);
  // Trim trailing zeros (shouldn't occur for our cords but be safe).
  let end = raw.length;
  while (end > 0 && raw[end - 1] === 0) end--;
  return raw.subarray(0, end);
}

export interface TweakInputs {
  txidHex: string;   // 64-char display hex (canonical txid as shown by explorers)
  vout: number;
  off?: number;      // default 0
}

export function buildTweakBytes(inputs: TweakInputs): Uint8Array {
  const off = inputs.off ?? 0;
  return concat(
    Uint8Array.of(0x09),                     // %9
    Uint8Array.of(0x99),                     // ~tyr (galaxy 153)
    cordBytes("urb-watcher"),                // %urb-watcher
    cordBytes("btc"),                        // %btc
    cordBytes("gw"),                         // %gw
    Uint8Array.of(0x09),                     // %9
    txidHexToAtomBytes(inputs.txidHex),      // txid (32 bytes LE)
    uintToMinLEBytes(inputs.vout),           // vout (omitted if 0)
    uintToMinLEBytes(off),                   // off (omitted if 0)
  );
}
