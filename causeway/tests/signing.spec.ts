// PSBT construction — kelvin-9 single-transaction spawn / state-update.
//
// A spawn (and every custody move) is ONE key-path P2TR spend: input 0 = the
// current sat-carrying UTXO, output 0 = the new sat-carrying P2TR key Q. There
// is no commit/reveal pair. A state UPDATE additionally carries the current
// state leaf's merkle root on the input so the signer can key-path-spend the
// tweaked output (BIP-371).

import { describe, expect, test } from "vitest";
import { Transaction, p2tr } from "@scure/btc-signer";
import { HDKey } from "@scure/bip32";
import { sha256 } from "@noble/hashes/sha256";
import {
  buildSpawnPsbt, extractTx, rawTxid, bip86Path, type KeyInfo,
} from "../src/signing/psbt.js";
import {
  stateOutputKey, stateOutputScript, stateMerkleRoot, type Snapshot,
} from "../src/spawn/snapshot.js";

const SEED = sha256(new TextEncoder().encode("causeway-test-seed-do-not-use"));
const root = HDKey.fromMasterSeed(SEED);

function keyAt(index: number): KeyInfo {
  const node = root.derive(`m/86'/1'/0'/0/${index}`);
  return {
    internalKey: node.publicKey!.slice(1), // drop 02/03 prefix → xonly
    masterFingerprint: root.fingerprint,
    derivationPath: bip86Path(0, 0, index),
  };
}

const fundingKey = keyAt(0);
const internalKey33 = (() => {
  const k = new Uint8Array(33);
  k[0] = 0x02;
  k.set(fundingKey.internalKey, 1);
  return k;
})();

const initialSnapshot: Snapshot = { life: 1, rift: 0, key: 0xabcdn, sponsor: null, fief: null };

const fundingTxid = sha256(new TextEncoder().encode("funding-txid"));
const funding = {
  txid: fundingTxid,
  vout: 0,
  value: 10000n,
  scriptPubKey: p2tr(fundingKey.internalKey).script,
};

describe("kelvin-9 PSBT construction", () => {
  test("buildSpawnPsbt produces a one-in one-out P2TR(Q) spend", () => {
    const outputScript = stateOutputScript(stateOutputKey(internalKey33, initialSnapshot));
    const result = buildSpawnPsbt({ funding, fundingKey, outputScript, feeRate: 2 });

    expect(result.psbt.length).toBeGreaterThan(0);
    expect(result.txid.length).toBe(32);
    expect(result.outputValue).toBeGreaterThan(0n);

    const tx = Transaction.fromPSBT(result.psbt);
    expect(tx.inputsLength).toBe(1);
    expect(tx.outputsLength).toBe(1);
    // output 0 is a 34-byte P2TR scriptPubKey (5120 || Q).
    const out0 = tx.getOutput(0).script!;
    expect(out0.length).toBe(34);
    expect(out0[0]).toBe(0x51);
    expect(out0[1]).toBe(0x20);
    expect(Array.from(out0)).toEqual(Array.from(outputScript));
  });

  test("spawn + sign + finalize round-trip (BIP-86 key-path)", () => {
    const outputScript = stateOutputScript(stateOutputKey(internalKey33, initialSnapshot));
    const built = buildSpawnPsbt({ funding, fundingKey, outputScript, feeRate: 2 });
    const tx = Transaction.fromPSBT(built.psbt);
    const privKey = root.derive("m/86'/1'/0'/0/0").privateKey!;
    expect(tx.signIdx(privKey, 0)).toBe(true);
    tx.finalizeIdx(0);
    const raw = tx.extract();
    expect(raw.length).toBeGreaterThan(0);
    // The predicted (pre-sign) txid matches the finalized non-witness txid.
    expect(built.txidHex).toBe(rawTxid(raw));
  });

  test("public spawn adds an OP_RETURN publication output", () => {
    const outputScript = stateOutputScript(stateOutputKey(internalKey33, initialSnapshot));
    const publicationScript = Uint8Array.from([0x6a, 0x03, 0x75, 0x72, 0x62, 0x01, 0x09, 0x02, 0xca, 0xfe]);
    const built = buildSpawnPsbt({ funding, fundingKey, outputScript, publicationScript, feeRate: 2 });
    const tx = Transaction.fromPSBT(built.psbt, { allowUnknownOutputs: true });
    expect(tx.outputsLength).toBe(2);
    const op = tx.getOutput(1).script!;
    expect(op[0]).toBe(0x6a); // OP_RETURN
    expect(tx.getOutput(1).amount).toBe(0n);
  });

  test("state update carries the current leaf merkle root on the input", () => {
    // Spend a sat-carrying output committing `initialSnapshot`, re-commit a new
    // snapshot (rekey: life+1, new key).
    const currentQ = stateOutputKey(internalKey33, initialSnapshot);
    const current = {
      txid: sha256(new TextEncoder().encode("state-utxo")),
      vout: 0,
      value: 8000n,
      scriptPubKey: stateOutputScript(currentQ),
    };
    const newSnapshot: Snapshot = { ...initialSnapshot, life: 2, key: 0x1234n };
    const outputScript = stateOutputScript(stateOutputKey(internalKey33, newSnapshot));
    const built = buildSpawnPsbt({
      funding: current, fundingKey, outputScript,
      inputMerkleRoot: stateMerkleRoot(initialSnapshot), feeRate: 2,
    });
    const tx = Transaction.fromPSBT(built.psbt);
    const in0 = tx.getInput(0);
    expect(in0.tapMerkleRoot).toBeDefined();
    expect(Array.from(in0.tapMerkleRoot!)).toEqual(Array.from(stateMerkleRoot(initialSnapshot)));
    expect(Array.from(in0.tapInternalKey!)).toEqual(Array.from(fundingKey.internalKey));
  });

  test("extractTx finalizes a signed spawn PSBT", () => {
    const outputScript = stateOutputScript(stateOutputKey(internalKey33, initialSnapshot));
    const built = buildSpawnPsbt({ funding, fundingKey, outputScript, feeRate: 2 });
    const tx = Transaction.fromPSBT(built.psbt);
    tx.signIdx(root.derive("m/86'/1'/0'/0/0").privateKey!, 0);
    const raw = extractTx(tx.toPSBT(0));
    expect(raw.length).toBeGreaterThan(0);
    expect(rawTxid(raw)).toBe(built.txidHex);
  });
});
