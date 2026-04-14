// QR payload schema for Causeway <-> HW wallet.
//
// v0 serialization: JSON with Uint8Array/bigint expressed as hex/decimal
// strings. Easy to debug, language-agnostic. Encoding into a QR code
// (base45 / BBQR multi-frame) is orthogonal and happens in the UI layer.

export type OpName =
  | "spawn"
  | "rekey"
  | "escape"
  | "cancel-escape"
  | "adopt"
  | "reject"
  | "detach"
  | "fief"
  | "set-mang"
  | "login";

export type SighashKind = "key-path" | "script-path" | "login";

export interface TxInputBrief {
  txid: Uint8Array;
  vout: number;
  value: bigint;
  script: Uint8Array;
}

export interface TxOutputBrief {
  value: bigint;
  script: Uint8Array;
}

export interface TxBrief {
  inputs: TxInputBrief[];
  outputs: TxOutputBrief[];
  lockTime: number;
  leafScript?: Uint8Array;
}

export interface SignRequestSighash {
  kind: SighashKind;
  hash: Uint8Array;
  txBrief: TxBrief;
}

export interface SignRequest {
  v: 1;
  nonce: Uint8Array;
  patp: bigint;
  op: OpName;
  summary: string;
  authKey: Uint8Array;
  sighashes: SignRequestSighash[];
}

export interface SignResponse {
  v: 1;
  nonce: Uint8Array;
  sigs: Uint8Array[];
}

// -- Serialization -----------------------------------------------------------

function bytesToHex(b: Uint8Array): string {
  return Array.from(b, (x) => x.toString(16).padStart(2, "0")).join("");
}

function hexToBytes(s: string): Uint8Array {
  if (s.length % 2 !== 0) throw new Error("hex: odd length");
  const out = new Uint8Array(s.length / 2);
  for (let i = 0; i < out.length; i++) {
    out[i] = parseInt(s.slice(i * 2, i * 2 + 2), 16);
  }
  return out;
}

export function encodeSignRequest(req: SignRequest): string {
  return JSON.stringify({
    v: req.v,
    nonce: bytesToHex(req.nonce),
    patp: req.patp.toString(),
    op: req.op,
    summary: req.summary,
    authKey: bytesToHex(req.authKey),
    sighashes: req.sighashes.map((sh) => ({
      kind: sh.kind,
      hash: bytesToHex(sh.hash),
      txBrief: {
        inputs: sh.txBrief.inputs.map((i) => ({
          txid: bytesToHex(i.txid),
          vout: i.vout,
          value: i.value.toString(),
          script: bytesToHex(i.script),
        })),
        outputs: sh.txBrief.outputs.map((o) => ({
          value: o.value.toString(),
          script: bytesToHex(o.script),
        })),
        lockTime: sh.txBrief.lockTime,
        leafScript: sh.txBrief.leafScript ? bytesToHex(sh.txBrief.leafScript) : undefined,
      },
    })),
  });
}

export function decodeSignRequest(json: string): SignRequest {
  const j = JSON.parse(json);
  if (j.v !== 1) throw new Error(`unsupported version ${j.v}`);
  return {
    v: 1,
    nonce: hexToBytes(j.nonce),
    patp: BigInt(j.patp),
    op: j.op,
    summary: j.summary,
    authKey: hexToBytes(j.authKey),
    sighashes: j.sighashes.map((sh: any) => ({
      kind: sh.kind,
      hash: hexToBytes(sh.hash),
      txBrief: {
        inputs: sh.txBrief.inputs.map((i: any) => ({
          txid: hexToBytes(i.txid),
          vout: i.vout,
          value: BigInt(i.value),
          script: hexToBytes(i.script),
        })),
        outputs: sh.txBrief.outputs.map((o: any) => ({
          value: BigInt(o.value),
          script: hexToBytes(o.script),
        })),
        lockTime: sh.txBrief.lockTime,
        leafScript: sh.txBrief.leafScript ? hexToBytes(sh.txBrief.leafScript) : undefined,
      },
    })),
  };
}

export function encodeSignResponse(res: SignResponse): string {
  return JSON.stringify({
    v: res.v,
    nonce: bytesToHex(res.nonce),
    sigs: res.sigs.map(bytesToHex),
  });
}

export function decodeSignResponse(json: string): SignResponse {
  const j = JSON.parse(json);
  if (j.v !== 1) throw new Error(`unsupported version ${j.v}`);
  return {
    v: 1,
    nonce: hexToBytes(j.nonce),
    sigs: (j.sigs as string[]).map(hexToBytes),
  };
}

// -- Nonce generation --------------------------------------------------------

export function newNonce(): Uint8Array {
  const out = new Uint8Array(16);
  crypto.getRandomValues(out);
  return out;
}
