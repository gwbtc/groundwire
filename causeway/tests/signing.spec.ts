import { describe, expect, test } from "vitest";
import { Transaction, p2tr } from "@scure/btc-signer";
import { HDKey } from "@scure/bip32";
import { sha256 } from "@noble/hashes/sha256";
import {
  buildCommitPsbt,
  buildRevealPsbt,
  extractTx,
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
});
