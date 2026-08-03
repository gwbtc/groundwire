// PSBT construction with BIP-341/BIP-371 taproot fields for the kelvin-9
// OP_RETURN protocol.
//
// A spawn (and every custody move) is ONE transaction — no commit/reveal pair:
//   * input 0  — the current sat-carrying UTXO, spent key-path.
//   * output 0 — the new sat-carrying P2TR output whose key Q commits the
//                snapshot (5120 || Q); the sat travels here at offset 0.
//   * optional OP_RETURN publication output (public comets only).
//   * optional change output.
//
// For a SPAWN, input 0 is an ordinary BIP-86 UTXO (no tapMerkleRoot). For a
// state UPDATE (rekey/breach), input 0 is the previous sat-carrying output,
// so the signer is given tapInternalKey + tapMerkleRoot (the leaf hash of the
// CURRENT state leaf) to key-path-spend it (BIP-371, PSBT_IN_TAP_LEAF_SCRIPT).
//
// Uses @scure/btc-signer's Transaction class (full BIP-371 support). The HW
// wallet signs; Causeway extracts the witness and broadcasts.

import { Transaction } from "@scure/btc-signer";
import { sha256 } from "@noble/hashes/sha256";

export interface Utxo {
  txid: Uint8Array;               // display order (block-explorer form)
  vout: number;
  value: bigint;
  scriptPubKey: Uint8Array;
}

export interface KeyInfo {
  internalKey: Uint8Array;        // 32-byte xonly
  masterFingerprint: number;      // 4-byte big-endian
  derivationPath: number[];       // e.g. [86|H, 1|H, 0|H, 0, 0]
}

export interface SpawnPsbtParams {
  funding: Utxo;                  // input 0 — the sat-carrying UTXO to spend
  fundingKey: KeyInfo;            // key controlling `funding` (signs input 0)
  outputScript: Uint8Array;       // output 0 scriptPubKey = 5120 || Q
  // For a state UPDATE the current output is P2TR-tweaked by its state leaf;
  // present → the signer key-path-spends the tweaked output. Absent → a plain
  // BIP-86 spawn input.
  inputMerkleRoot?: Uint8Array;
  publicationScript?: Uint8Array; // optional OP_RETURN publication output
  changeScript?: Uint8Array;      // optional change output scriptPubKey
  postageValue?: bigint;          // value for output 0 when a change output is used
  feeRate: number;                // sat/vB
}

export interface SpawnPsbtResult {
  psbt: Uint8Array;               // serialized unsigned PSBT (v0 / BIP-174)
  txid: Uint8Array;               // 32-byte display-order txid (segwit — witness-excluded)
  txidHex: string;
  outputValue: bigint;            // value of the sat-carrying output 0
}

// vbyte estimates (conservative): overhead 11, P2TR key-path input ~58,
// P2TR output ~43, OP_RETURN output ~ 9 + scriptLen.
function estimateVbytes(p: SpawnPsbtParams): number {
  let vb = 11 + 58 + 43;
  if (p.publicationScript) vb += 9 + p.publicationScript.length;
  if (p.changeScript) vb += 43;
  return vb;
}

export function buildSpawnPsbt(params: SpawnPsbtParams): SpawnPsbtResult {
  const { funding, fundingKey, outputScript, feeRate } = params;

  const fee = BigInt(estimateVbytes(params) * feeRate);
  const usePostage = params.changeScript !== undefined && params.postageValue !== undefined;
  const outputValue = usePostage ? params.postageValue! : funding.value - fee;
  if (outputValue < 330n) throw new Error("psbt: sat-carrying output below dust");

  const changeValue = usePostage ? funding.value - fee - outputValue : 0n;
  if (usePostage && changeValue < 0n) throw new Error("psbt: funding UTXO too small for postage + fee");

  const tx = new Transaction({ allowUnknownOutputs: true });

  tx.addInput({
    txid: funding.txid,
    index: funding.vout,
    witnessUtxo: { script: funding.scriptPubKey, amount: funding.value },
    tapInternalKey: fundingKey.internalKey,
    ...(params.inputMerkleRoot ? { tapMerkleRoot: params.inputMerkleRoot } : {}),
    tapBip32Derivation: [[
      fundingKey.internalKey,
      { hashes: [], der: { fingerprint: fundingKey.masterFingerprint, path: fundingKey.derivationPath } },
    ]],
  });

  // Output 0 — the sat-carrying P2TR output.
  tx.addOutput({ script: outputScript, amount: outputValue });
  // Optional OP_RETURN publication (public comets).
  if (params.publicationScript) tx.addOutput({ script: params.publicationScript, amount: 0n });
  // Optional change.
  if (usePostage && changeValue >= 330n) tx.addOutput({ script: params.changeScript!, amount: changeValue });

  const psbt = tx.toPSBT(0);
  const txid = computeTxid(tx);
  return { psbt, txid, txidHex: bytesToHex(txid), outputValue };
}

// Extract a finalized (signed) tx from a PSBT. `allowUnknownInputs`/outputs let
// @scure finalize a tx with a bare OP_RETURN publication output. Already-
// finalized inputs (e.g. one Sparrow finalized itself) are skipped.
export function extractTx(signedPsbt: Uint8Array): Uint8Array {
  const tx = Transaction.fromPSBT(signedPsbt, { allowUnknownInputs: true, allowUnknownOutputs: true });
  for (let i = 0; i < tx.inputsLength; i++) {
    const wit = tx.getInput(i).finalScriptWitness;
    if (wit && wit.length) continue;
    tx.finalizeIdx(i);
  }
  return tx.extract();
}

export function extractTxHex(signedPsbt: Uint8Array): string {
  return bytesToHex(extractTx(signedPsbt));
}

// The display-order txid of a raw (finalized) tx. Hash the NON-witness
// serialization (toBytes(true) omits the witness) — double-SHAing the full
// witness-inclusive bytes yields the wtxid, not the txid.
export function rawTxid(rawTx: Uint8Array): string {
  return Transaction.fromRaw(rawTx, { allowUnknownInputs: true, allowUnknownOutputs: true }).id;
}

// Compute the txid in DISPLAY byte order from an unsigned tx (segwit txid
// excludes the witness, so it is stable before signing).
function computeTxid(tx: Transaction): Uint8Array {
  const wire = sha256(sha256(tx.unsignedTx));
  const display = new Uint8Array(wire.length);
  for (let i = 0; i < wire.length; i++) display[i] = wire[wire.length - 1 - i]!;
  return display;
}

function bytesToHex(b: Uint8Array): string {
  return Array.from(b, (x) => x.toString(16).padStart(2, "0")).join("");
}

// Build a BIP-86 derivation path as an array of uint32 with hardened flags.
export function bip86Path(account: number, change: number, index: number): number[] {
  const H = 0x80000000;
  return [86 + H, 1 + H, account + H, change, index];
}
