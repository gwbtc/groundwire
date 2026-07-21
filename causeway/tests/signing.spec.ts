import { describe, expect, test } from "vitest";
import { Transaction, p2tr } from "@scure/btc-signer";
import { HDKey } from "@scure/bip32";
import { sha256 } from "@noble/hashes/sha256";
import {
  buildCommitPsbt,
  buildRevealPsbt,
  extractTx,
  assertRevealsLeaf,
  extractVerifiedPair,
  rawTxid,
  bip86Path,
  type KeyInfo,
} from "../src/signing/psbt.js";
import { urbLeafScript } from "../src/chain/tapscript.js";

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
const commitKey = keyAt(1);
const destKey = keyAt(2);

const dummyAttestation = new Uint8Array([0xde, 0xad, 0xbe, 0xef]);
const leafScript = urbLeafScript(dummyAttestation, commitKey.internalKey);

const fundingTxid = sha256(new TextEncoder().encode("funding-txid"));
const funding = {
  txid: fundingTxid,
  vout: 0,
  value: 10000n,
  scriptPubKey: p2tr(fundingKey.internalKey).script,
};

describe("PSBT construction", () => {
  test("buildCommitPsbt produces valid PSBT", () => {
    const result = buildCommitPsbt({
      funding,
      fundingKey,
      commitKey,
      leafScript,
      feeRate: 2,
    });
    expect(result.psbt.length).toBeGreaterThan(0);
    expect(result.commitTxid.length).toBe(32);
    expect(result.commitOutputValue).toBeGreaterThan(0n);
    expect(result.commitOutputScript.length).toBeGreaterThan(0);

    const tx = Transaction.fromPSBT(result.psbt);
    expect(tx.inputsLength).toBe(1);
    expect(tx.outputsLength).toBe(1);
  });

  test("buildRevealPsbt produces valid PSBT", () => {
    const commit = buildCommitPsbt({ funding, fundingKey, commitKey, leafScript, feeRate: 2 });
    const reveal = buildRevealPsbt({
      commitTxid: commit.commitTxid,
      commitOutputValue: commit.commitOutputValue,
      commitOutputScript: commit.commitOutputScript,
      commitKey,
      leafScript,
      destKey,
      feeRate: 2,
    });
    expect(reveal.psbt.length).toBeGreaterThan(0);
    expect(reveal.revealOutputValue).toBeGreaterThan(0n);

    const tx = Transaction.fromPSBT(reveal.psbt);
    expect(tx.inputsLength).toBe(1);
    expect(tx.outputsLength).toBe(1);
  });

  test("commit + sign + finalize round-trip", () => {
    const commit = buildCommitPsbt({ funding, fundingKey, commitKey, leafScript, feeRate: 2 });
    const tx = Transaction.fromPSBT(commit.psbt);
    const privKey = root.derive("m/86'/1'/0'/0/0").privateKey!;
    tx.signIdx(privKey, 0);
    tx.finalizeIdx(0);
    const raw = tx.extract();
    expect(raw.length).toBeGreaterThan(0);
  });

  // The reveal MUST spend the commit output via the script path so the urb
  // attestation leaf is published on-chain. Regression guard for the
  // key-path-preferred finalize bug (the leaf was never revealed).
  test("reveal signs script-path and publishes the leaf", () => {
    const commit = buildCommitPsbt({ funding, fundingKey, commitKey, leafScript, feeRate: 2 });
    const reveal = buildRevealPsbt({
      commitTxid: commit.commitTxid,
      commitOutputValue: commit.commitOutputValue,
      commitOutputScript: commit.commitOutputScript,
      commitKey, leafScript, destKey, feeRate: 2,
    });

    // commitKey is at derivation index 1; it is the key inside the urb leaf.
    const tx = Transaction.fromPSBT(reveal.psbt, { allowUnknownInputs: true });
    const commitPriv = root.derive("m/86'/1'/0'/0/1").privateKey!;
    expect(tx.signIdx(commitPriv, 0)).toBe(true);

    // No tapInternalKey means no key-path sig is possible — only a script sig.
    const signed = tx.toPSBT(0);
    assertRevealsLeaf(signed, leafScript); // throws if key-path or missing leaf

    // Finalize via the shipping path and confirm the script-path witness.
    tx.finalizeIdx(0);
    const witness = tx.getInput(0).finalScriptWitness!;
    expect(witness.length).toBe(3); // [sig, leafScript, controlBlock]
    expect(Array.from(witness[1]!)).toEqual(Array.from(leafScript));
    expect(extractTx(signed).length).toBeGreaterThan(0);
  });

  // extractVerifiedPair must return the real TXID and confirm the reveal spends
  // the commit output-0. Regression guard: rawTxid must hash the NON-witness
  // serialization (a wtxid here would make the outpoint check always fail).
  test("extractVerifiedPair returns the txid and verifies the reveal chains", () => {
    const commit = buildCommitPsbt({ funding, fundingKey, commitKey, leafScript, feeRate: 2 });
    const reveal = buildRevealPsbt({
      commitTxid: commit.commitTxid,
      commitOutputValue: commit.commitOutputValue,
      commitOutputScript: commit.commitOutputScript,
      commitKey, leafScript, destKey, feeRate: 2,
    });
    const ctx = Transaction.fromPSBT(commit.psbt);
    ctx.signIdx(root.derive("m/86'/1'/0'/0/0").privateKey!, 0);
    const signedCommit = ctx.toPSBT(0);
    const rtx = Transaction.fromPSBT(reveal.psbt, { allowUnknownInputs: true });
    rtx.signIdx(root.derive("m/86'/1'/0'/0/1").privateKey!, 0);
    const signedReveal = rtx.toPSBT(0);

    // Passing at all proves the reveal's input-0 outpoint matched the commit
    // txid (extractVerifiedPair throws otherwise).
    const { commitTxid, commitHex, revealHex } = extractVerifiedPair(signedCommit, signedReveal);
    // The reported txid must be the real (non-witness) txid — not the wtxid.
    ctx.finalizeIdx(0);
    expect(commitTxid).toBe(rawTxid(ctx.extract()));
    expect(commitHex.length).toBeGreaterThan(0);
    expect(revealHex.length).toBeGreaterThan(0);
  });

  test("a key-path-only reveal PSBT is rejected by assertRevealsLeaf", () => {
    // Simulate the old dangerous shape: a reveal input that carries the
    // internal key + merkle root, letting a signer key-path spend it.
    const commit = buildCommitPsbt({ funding, fundingKey, commitKey, leafScript, feeRate: 2 });
    const commitPayment = p2tr(commitKey.internalKey, { script: leafScript, leafVersion: 0xc0 }, undefined, true);
    const tx = new Transaction();
    tx.addInput({
      txid: commit.commitTxid, index: 0,
      witnessUtxo: { script: commit.commitOutputScript, amount: commit.commitOutputValue },
      tapInternalKey: commitKey.internalKey,
      tapMerkleRoot: commitPayment.tapMerkleRoot,
    });
    tx.addOutput({ script: p2tr(destKey.internalKey).script, amount: commit.commitOutputValue - 500n });
    const commitPriv = root.derive("m/86'/1'/0'/0/1").privateKey!;
    tx.signIdx(commitPriv, 0);
    expect(() => assertRevealsLeaf(tx.toPSBT(0), leafScript)).toThrow(/key-path/);
  });
});
