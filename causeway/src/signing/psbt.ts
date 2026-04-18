// PSBT construction with BIP-371 taproot fields.
//
// Builds unsigned PSBTs for the commit/reveal attestation pattern.
// The HW wallet signs them; Causeway extracts the witness and broadcasts.
//
// Uses @scure/btc-signer's Transaction class which supports full BIP-371.

import { Transaction, p2tr } from "@scure/btc-signer";
import type { TaprootNode } from "@scure/btc-signer/payment";
import { sha256 } from "@noble/hashes/sha256";

const TAP_LEAF_VERSION = 0xc0;

export interface Utxo {
  txid: Uint8Array;
  vout: number;
  value: bigint;
  scriptPubKey: Uint8Array;
}

export interface KeyInfo {
  internalKey: Uint8Array;        // 32-byte xonly
  masterFingerprint: number;      // 4-byte big-endian
  derivationPath: number[];       // e.g., [86 | 0x80000000, 1 | 0x80000000, 0 | 0x80000000, 0, 0]
}

export interface CommitParams {
  funding: Utxo;
  fundingKey: KeyInfo;            // key at index 0 — signs the commit input
  commitKey: KeyInfo;             // key at index 1 — internal key for the commit output
  leafScript: Uint8Array;         // urb-tagged attestation script
  feeRate: number;                // sat/vB
}

export interface CommitResult {
  psbt: Uint8Array;               // serialized unsigned PSBT
  commitTxid: Uint8Array;         // 32-byte txid (deterministic since segwit)
  commitOutputValue: bigint;
  commitOutputScript: Uint8Array;
}

export function buildCommitPsbt(params: CommitParams): CommitResult {
  const { funding, fundingKey, commitKey, leafScript, feeRate } = params;

  const scriptTree: TaprootNode = { script: leafScript, leafVersion: TAP_LEAF_VERSION };
  const commitPayment = p2tr(commitKey.internalKey, scriptTree, undefined, true);

  // Fee estimate: P2TR key-path input ~58 vB, P2TR output ~43 vB, overhead ~11 vB
  const commitVbytes = 11 + 58 + 43;
  const commitFee = BigInt(commitVbytes * feeRate);
  const commitValue = funding.value - commitFee;
  if (commitValue <= 0n) throw new Error("psbt: funding UTXO too small for commit");

  const tx = new Transaction();

  tx.addInput({
    txid: funding.txid,
    index: funding.vout,
    witnessUtxo: { script: funding.scriptPubKey, amount: funding.value },
    tapInternalKey: fundingKey.internalKey,
    tapBip32Derivation: [[
      fundingKey.internalKey,
      { hashes: [], der: { fingerprint: fundingKey.masterFingerprint, path: fundingKey.derivationPath } },
    ]],
  });

  tx.addOutput({ script: commitPayment.script, amount: commitValue });

  // Emit PSBT v0 (BIP-174) for maximum wallet compatibility — PSBT v2
  // (BIP-370) is still unevenly supported outside of new hardware signers.
  const psbt = tx.toPSBT(0);

  // Compute commit txid from the unsigned tx (segwit txid excludes witness)
  const commitTxid = computeTxid(tx);

  return {
    psbt,
    commitTxid,
    commitOutputValue: commitValue,
    commitOutputScript: commitPayment.script,
  };
}

export interface RevealParams {
  commitTxid: Uint8Array;
  commitOutputValue: bigint;
  commitOutputScript: Uint8Array;
  commitKey: KeyInfo;             // key at index 1 — signs the reveal input (script-path)
  leafScript: Uint8Array;
  destKey: KeyInfo;               // key at index 2 — internal key for reveal destination
  feeRate: number;
}

export interface RevealResult {
  psbt: Uint8Array;
  revealOutputValue: bigint;
}

export function buildRevealPsbt(params: RevealParams): RevealResult {
  const {
    commitTxid, commitOutputValue, commitOutputScript,
    commitKey, leafScript, destKey, feeRate,
  } = params;

  const scriptTree: TaprootNode = { script: leafScript, leafVersion: TAP_LEAF_VERSION };
  const commitPayment = p2tr(commitKey.internalKey, scriptTree, undefined, true);

  // Script-path input weight: ~265 + script_len (from gw-onboard.py:1127)
  const inputWeight = 265 + leafScript.length;
  const inputVb = Math.ceil(inputWeight / 4);
  const revealVbytes = 11 + inputVb + 43;
  const revealFee = BigInt(revealVbytes * feeRate);
  const revealValue = commitOutputValue - revealFee;
  if (revealValue < 330n) throw new Error("psbt: commit output too small for reveal");

  const destPayment = p2tr(destKey.internalKey);

  const tx = new Transaction();

  tx.addInput({
    txid: commitTxid,
    index: 0,
    witnessUtxo: { script: commitOutputScript, amount: commitOutputValue },
    tapInternalKey: commitKey.internalKey,
    tapMerkleRoot: commitPayment.tapMerkleRoot,
    ...(commitPayment.tapLeafScript ? { tapLeafScript: commitPayment.tapLeafScript } : {}),
    tapBip32Derivation: [[
      commitKey.internalKey,
      {
        hashes: commitPayment.leaves?.map((l) => l.hash) ?? [],
        der: { fingerprint: commitKey.masterFingerprint, path: commitKey.derivationPath },
      },
    ]],
  });

  tx.addOutput({ script: destPayment.script, amount: revealValue });

  return { psbt: tx.toPSBT(0), revealOutputValue: revealValue };
}

// Extract a finalized (signed) tx from a PSBT.
export function extractTx(signedPsbt: Uint8Array): Uint8Array {
  const tx = Transaction.fromPSBT(signedPsbt);
  tx.finalize();
  return tx.extract();
}

export function extractTxHex(signedPsbt: Uint8Array): string {
  return bytesToHex(extractTx(signedPsbt));
}

// Compute the txid in DISPLAY byte order (matches block explorers, mempool.space,
// Sparrow, etc.). `sha256(sha256(raw))` gives internal/wire bytes; we reverse
// so callers can pass the result straight back into @scure's addInput (which
// expects display-order bytes) for the reveal's dependency on the commit.
function computeTxid(tx: Transaction): Uint8Array {
  const wire = sha256(sha256(tx.unsignedTx));
  const display = new Uint8Array(wire.length);
  for (let i = 0; i < wire.length; i++) display[i] = wire[wire.length - 1 - i]!;
  return display;
}

function bytesToHex(b: Uint8Array): string {
  return Array.from(b, (x) => x.toString(16).padStart(2, "0")).join("");
}

function hexToBytes(s: string): Uint8Array {
  const out = new Uint8Array(s.length / 2);
  for (let i = 0; i < out.length; i++) out[i] = parseInt(s.slice(i * 2, i * 2 + 2), 16);
  return out;
}

// Helper: build a BIP-86 derivation path as an array of uint32 with hardened flags.
export function bip86Path(account: number, change: number, index: number): number[] {
  // Use addition, not bitwise OR — JS bitwise ops coerce to signed int32.
  const H = 0x80000000;
  return [86 + H, 1 + H, account + H, change, index];
}
