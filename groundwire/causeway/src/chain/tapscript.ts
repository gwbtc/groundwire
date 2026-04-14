// Build the urb-tagged Taproot script that wraps attestation data:
//
//   OP_0 OP_IF "urb" <pushdata chunks for dat> OP_ENDIF <xonly> OP_CHECKSIG
//
// Matches ++unv-to-script in lib/urb-encoder.hoon.

const OP_0 = 0x00;
const OP_IF = 0x63;
const OP_ENDIF = 0x68;
const OP_CHECKSIG = 0xac;
const OP_PUSHDATA1 = 0x4c;
const OP_PUSHDATA2 = 0x4d;
const OP_PUSHDATA4 = 0x4e;

function pushData(data: Uint8Array): Uint8Array {
  const n = data.length;
  if (n < 0x4c) {
    const out = new Uint8Array(1 + n);
    out[0] = n;
    out.set(data, 1);
    return out;
  }
  if (n <= 0xff) {
    const out = new Uint8Array(2 + n);
    out[0] = OP_PUSHDATA1;
    out[1] = n;
    out.set(data, 2);
    return out;
  }
  if (n <= 0xffff) {
    const out = new Uint8Array(3 + n);
    out[0] = OP_PUSHDATA2;
    out[1] = n & 0xff;
    out[2] = (n >> 8) & 0xff;
    out.set(data, 3);
    return out;
  }
  const out = new Uint8Array(5 + n);
  out[0] = OP_PUSHDATA4;
  out[1] = n & 0xff;
  out[2] = (n >> 8) & 0xff;
  out[3] = (n >> 16) & 0xff;
  out[4] = (n >> 24) & 0xff;
  out.set(data, 5);
  return out;
}

function concat(...parts: Uint8Array[]): Uint8Array {
  const total = parts.reduce((s, p) => s + p.length, 0);
  const out = new Uint8Array(total);
  let off = 0;
  for (const p of parts) {
    out.set(p, off);
    off += p.length;
  }
  return out;
}

export function urbLeafScript(dat: Uint8Array, xOnlyKey: Uint8Array): Uint8Array {
  if (xOnlyKey.length !== 32) throw new Error("xonly must be 32 bytes");
  const urb = new TextEncoder().encode("urb");
  // Split dat into 520-byte chunks (bitcoin PUSHDATA max per op)
  const CHUNK = 520;
  const chunks: Uint8Array[] = [];
  for (let i = 0; i < dat.length; i += CHUNK) {
    chunks.push(pushData(dat.subarray(i, i + CHUNK)));
  }
  return concat(
    Uint8Array.of(OP_0, OP_IF),
    pushData(urb),
    ...chunks,
    Uint8Array.of(OP_ENDIF),
    pushData(xOnlyKey),
    Uint8Array.of(OP_CHECKSIG),
  );
}
