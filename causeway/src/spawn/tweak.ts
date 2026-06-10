// Build the tweak atom bytes that urb-core uses to validate a %spawn's
// networking key.
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

// Convert display-order txid hex (big-endian display) to the Hoon @ux atom
// bytes (little-endian wire order, full 32 bytes).
function txidHexToAtomBytes(displayHex: string): Uint8Array {
  const clean = displayHex.replace(/^0x/, "").toLowerCase();
  if (clean.length !== 64) throw new Error(`expected 64 hex chars, got ${clean.length}`);
  // Display hex 0x0123...  →  atom bytes LE = bytes of display hex reversed
  const out = new Uint8Array(32);
  for (let i = 0; i < 32; i++) {
    out[i] = parseInt(clean.slice((31 - i) * 2, (31 - i) * 2 + 2), 16);
  }
  return out;
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
