// UR2.0 (Blockchain Commons Uniform Resources) codec for PSBTs.
//
// Outbound: wrap raw PSBT bytes as CBOR byte string, tag as ur:psbt, produce
// a sequence of fountain-coded frames suitable for animated QR display.
//
// Inbound: accept frames one at a time from a camera scanner; the UR decoder
// signals completion once enough frames have accumulated.
//
// Reference: https://github.com/BlockchainCommons/Research/blob/master/papers/bcr-2020-006-urtypes.md

import { UR, UREncoder, URDecoder } from "@ngraveio/bc-ur";
import { Buffer } from "buffer";

const UR_TYPE_PSBT = "crypto-psbt";

// Encode bytes as a CBOR byte-string: 0x58 <len> <bytes> for len<256,
// 0x59 <lenHi><lenLo> <bytes> for len<65536, 0x5A <4 bytes len> for larger.
function cborByteString(data: Uint8Array): Uint8Array {
  const len = data.length;
  let header: Uint8Array;
  if (len < 24) {
    header = Uint8Array.of(0x40 | len);
  } else if (len < 0x100) {
    header = Uint8Array.of(0x58, len);
  } else if (len < 0x10000) {
    header = Uint8Array.of(0x59, (len >> 8) & 0xff, len & 0xff);
  } else {
    header = Uint8Array.of(
      0x5a,
      (len >>> 24) & 0xff,
      (len >>> 16) & 0xff,
      (len >>> 8) & 0xff,
      len & 0xff,
    );
  }
  const out = new Uint8Array(header.length + len);
  out.set(header, 0);
  out.set(data, header.length);
  return out;
}

// Strip the CBOR byte-string wrapper.
function cborByteStringDecode(data: Uint8Array): Uint8Array {
  const first = data[0]!;
  let offset: number;
  let len: number;
  if (first >= 0x40 && first < 0x58) {
    len = first & 0x1f;
    offset = 1;
  } else if (first === 0x58) {
    len = data[1]!;
    offset = 2;
  } else if (first === 0x59) {
    len = (data[1]! << 8) | data[2]!;
    offset = 3;
  } else if (first === 0x5a) {
    len = ((data[1]! << 24) | (data[2]! << 16) | (data[3]! << 8) | data[4]!) >>> 0;
    offset = 5;
  } else {
    throw new Error(`cbor: expected byte-string, got 0x${first.toString(16)}`);
  }
  if (data.length !== offset + len) {
    throw new Error(`cbor: length mismatch (expected ${offset + len}, got ${data.length})`);
  }
  return data.subarray(offset);
}

export interface URStream {
  frames: string[];              // all possible frames (pre-rendered)
  totalFragments: number;         // minimum frames required to decode
  encoder: UREncoder;             // for pulling additional frames
  nextFrame(): string;            // advance and return the next frame
}

// Create an animated-QR frame stream for a PSBT.
// maxFragmentLength defaults to 300 bytes — comfortable QR density at v20.
export function encodePsbtUR(
  psbt: Uint8Array,
  maxFragmentLength = 300,
): URStream {
  const cbor = cborByteString(psbt);
  const ur = new UR(Buffer.from(cbor), UR_TYPE_PSBT);
  const encoder = new UREncoder(ur, maxFragmentLength);
  const total = encoder.fragmentsLength;

  // For ≤1 fragment, still emit as a sequenced single part for consistency.
  const frames: string[] = [];
  for (let i = 0; i < Math.max(total, 1); i++) frames.push(encoder.nextPart());

  return {
    frames,
    totalFragments: total,
    encoder,
    nextFrame: () => encoder.nextPart(),
  };
}

// Accept frames one-at-a-time. Returns decoded PSBT bytes, or null if more
// frames are needed. Throws on malformed/invalid inputs.
export class URPsbtDecoder {
  private decoder = new URDecoder();

  receive(frame: string): Uint8Array | null {
    this.decoder.receivePart(frame);
    if (this.decoder.isError()) {
      throw new Error(`UR decode error: ${this.decoder.resultError()}`);
    }
    if (!this.decoder.isComplete()) return null;
    const ur = this.decoder.resultUR();
    if (ur.type !== UR_TYPE_PSBT) {
      throw new Error(`UR type mismatch: expected ${UR_TYPE_PSBT}, got ${ur.type}`);
    }
    return cborByteStringDecode(new Uint8Array(ur.cbor));
  }

  progress(): number { return this.decoder.estimatedPercentComplete(); }
  expectedFragments(): number { return this.decoder.expectedPartCount(); }
  receivedFragments(): number[] { return this.decoder.receivedPartIndexes(); }
}

// Convenience: synchronously decode an array of frames.
export function decodePsbtURFrames(frames: string[]): Uint8Array {
  const d = new URPsbtDecoder();
  for (const f of frames) {
    const res = d.receive(f);
    if (res) return res;
  }
  throw new Error("UR decode: not enough frames");
}
