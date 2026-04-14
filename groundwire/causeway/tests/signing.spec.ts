import { describe, expect, test } from "vitest";
import { schnorr } from "@noble/curves/secp256k1";
import { sha256 } from "@noble/hashes/sha256";
import {
  decodeSignRequest, decodeSignResponse,
  encodeSignRequest, encodeSignResponse,
  newNonce,
} from "../src/signing/qr-protocol.js";
import type { SignRequest, SignResponse } from "../src/signing/qr-protocol.js";
import { verifyResponse } from "../src/signing/verify.js";

const priv = new Uint8Array(32).fill(7);
const authKey = schnorr.getPublicKey(priv);

function makeReq(): SignRequest {
  const msg1 = sha256(new Uint8Array([1, 2, 3]));
  const msg2 = sha256(new Uint8Array([4, 5, 6]));
  return {
    v: 1,
    nonce: newNonce(),
    patp: 1624961343n,
    op: "login",
    summary: "test login",
    authKey,
    sighashes: [
      { kind: "login", hash: msg1, txBrief: { inputs: [], outputs: [], lockTime: 0 } },
      { kind: "key-path", hash: msg2, txBrief: { inputs: [], outputs: [], lockTime: 0 } },
    ],
  };
}

describe("qr-protocol", () => {
  test("round-trips request", () => {
    const req = makeReq();
    const json = encodeSignRequest(req);
    const back = decodeSignRequest(json);
    expect(back.patp).toBe(req.patp);
    expect(back.op).toBe(req.op);
    expect(back.sighashes.length).toBe(2);
    expect(back.authKey).toEqual(req.authKey);
  });

  test("round-trips response", () => {
    const sigs = [new Uint8Array(64).fill(0xab), new Uint8Array(64).fill(0xcd)];
    const res: SignResponse = { v: 1, nonce: new Uint8Array(16), sigs };
    const json = encodeSignResponse(res);
    const back = decodeSignResponse(json);
    expect(back.sigs).toEqual(sigs);
  });
});

describe("verifyResponse", () => {
  test("accepts valid sigs", () => {
    const req = makeReq();
    const aux = new Uint8Array(32);
    const sigs = req.sighashes.map((sh) => schnorr.sign(sh.hash, priv, aux));
    const res: SignResponse = { v: 1, nonce: req.nonce, sigs };
    expect(() => verifyResponse(req, res)).not.toThrow();
  });

  test("rejects nonce mismatch", () => {
    const req = makeReq();
    const sigs = req.sighashes.map((sh) => schnorr.sign(sh.hash, priv, new Uint8Array(32)));
    const res: SignResponse = { v: 1, nonce: new Uint8Array(16), sigs };
    expect(() => verifyResponse(req, res)).toThrow(/nonce/);
  });

  test("rejects wrong count", () => {
    const req = makeReq();
    const res: SignResponse = { v: 1, nonce: req.nonce, sigs: [new Uint8Array(64)] };
    expect(() => verifyResponse(req, res)).toThrow(/expected 2/);
  });

  test("rejects bad sig", () => {
    const req = makeReq();
    const res: SignResponse = { v: 1, nonce: req.nonce, sigs: [new Uint8Array(64), new Uint8Array(64)] };
    expect(() => verifyResponse(req, res)).toThrow(/verify/);
  });
});
