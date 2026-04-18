// Inscription-aware UTXO selection.
//
// Fetches UTXOs for a funding address, filters out any that appear in the
// snapshot's sont-map (which tracks inscription-bearing satpoints), and
// returns a list of candidates. Callers can then pick the best one (usually
// smallest-sufficient) for their fee-funding needs.

import { Address, NETWORK, TEST_NETWORK, OutScript } from "@scure/btc-signer";
import type { BTC_NETWORK } from "@scure/btc-signer/utils";
import type { Mempool } from "./mempool.js";
import type { Noun } from "../oracle/noun.js";
import { isAtom, isCell, head, tail, asT2 } from "../oracle/noun.js";
import type { Utxo } from "../signing/psbt.js";

function hexToBytes(s: string): Uint8Array {
  const out = new Uint8Array(s.length / 2);
  for (let i = 0; i < out.length; i++) out[i] = parseInt(s.slice(i * 2, i * 2 + 2), 16);
  return out;
}

// Derive the scriptPubKey bytes for a known address (any type) using
// @scure/btc-signer's bech32/base58 decoder.
export function addressToScriptPubKey(address: string, network: BTC_NETWORK): Uint8Array {
  const decoded = Address(network).decode(address);
  return OutScript.encode(decoded);
}

export function networkFor(kind: "main" | "testnet"): BTC_NETWORK {
  return kind === "main" ? NETWORK : TEST_NETWORK;
}

// Walk a Hoon sont-map (treap) and collect every [txid, vout] key.
// sont-map = (map [txid vout] vout-map); key shape is a 2-cell of atoms.
export function collectSontMapKeys(sontMap: Noun): Set<string> {
  const keys = new Set<string>();
  const walk = (n: Noun): void => {
    if (isAtom(n)) return; // ~ (null)
    const kv = head(n);
    const lr = tail(n);
    const k = head(kv);
    if (isCell(k)) {
      const [txid, vout] = asT2(k);
      if (isAtom(txid) && isAtom(vout)) {
        keys.add(`${txid.toString(16)}:${vout}`);
      }
    }
    if (isCell(lr)) { walk(head(lr)); walk(tail(lr)); }
  };
  walk(sontMap);
  return keys;
}

// Mempool txid (display-order hex) → Hoon @ux atom hex string, for comparison
// against sont-map keys (which hold the Hoon atom).
export function mempoolTxidToAtomHex(displayHex: string): string {
  const bytes = hexToBytes(displayHex);
  let val = 0n;
  for (let i = bytes.length - 1; i >= 0; i--) val = (val << 8n) | BigInt(bytes[i]!);
  return val.toString(16);
}

export interface CandidateUtxo extends Utxo {
  address: string;   // the address this UTXO was found at (for provenance)
  confirmed: boolean;
}

// Enumerate UTXOs at `address` that are *not* inscription-bearing. Returns
// them in smallest-first order (good for selecting minimal funding UTXO).
export async function listFundingUtxos(
  address: string,
  mp: Mempool,
  sontMap: Noun,
  network: BTC_NETWORK,
): Promise<CandidateUtxo[]> {
  const utxos = await mp.utxo(address);
  const inscriptionKeys = collectSontMapKeys(sontMap);
  const script = addressToScriptPubKey(address, network);

  return utxos
    .filter((u) => {
      const key = `${mempoolTxidToAtomHex(u.txid)}:${u.vout}`;
      return !inscriptionKeys.has(key);
    })
    .sort((a, b) => a.value - b.value)
    .map((u) => ({
      // Display-order bytes (@scure/btc-signer reverses when serializing).
      txid: hexToBytes(u.txid),
      vout: u.vout,
      value: BigInt(u.value),
      scriptPubKey: script,
      address,
      confirmed: u.status.confirmed,
    }));
}

// Pick the smallest-sufficient funding UTXO for `neededSats`. Returns null if
// nothing large enough.
export async function selectFundingUtxo(
  address: string,
  mp: Mempool,
  sontMap: Noun,
  neededSats: bigint,
  network: BTC_NETWORK,
): Promise<CandidateUtxo | null> {
  const candidates = await listFundingUtxos(address, mp, sontMap, network);
  return candidates.find((u) => u.value >= neededSats && u.confirmed) ?? null;
}
