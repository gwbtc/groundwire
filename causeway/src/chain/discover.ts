// Discover UTXOs for a user's account.
//
// Scans the first N (default 20) addresses on change=0 (receive) and change=1
// (change), classifying each UTXO as either:
//   - inscriptionUtxo: matches an entry in sont-map. Never spend from Causeway.
//   - fundingUtxo: everything else. Available to pay fees.
//
// Returns both sets with provenance (which address / derivation index).

import type { KeySource } from "../keys/xpub.js";
import { deriveP2TRAddress } from "../keys/xpub.js";
import type { Mempool } from "./mempool.js";
import type { Noun } from "../oracle/noun.js";
import {
  collectSontMapKeys,
  mempoolTxidToAtomHex,
  addressToScriptPubKey,
  networkFor,
} from "./select.js";
import type { CandidateUtxo } from "./select.js";

export interface DiscoveredUtxo extends CandidateUtxo {
  change: 0 | 1;
  index: number;
}

export interface Discovery {
  fundingUtxos: DiscoveredUtxo[];
  inscriptionUtxos: DiscoveredUtxo[];
  addresses: Array<{ change: 0 | 1; index: number; address: string }>;
  totalFundingSats: bigint;
}

export interface DiscoverOpts {
  src: KeySource;
  mp: Mempool;
  sontMap: Noun;
  maxIndex?: number;  // default 20
}

function hexToBytes(s: string): Uint8Array {
  const out = new Uint8Array(s.length / 2);
  for (let i = 0; i < out.length; i++) out[i] = parseInt(s.slice(i * 2, i * 2 + 2), 16);
  return out;
}

export async function discoverUtxos(opts: DiscoverOpts): Promise<Discovery> {
  const max = opts.maxIndex ?? 20;
  const inscriptionKeys = collectSontMapKeys(opts.sontMap);
  const net = networkFor(opts.src.network);

  const fundingUtxos: DiscoveredUtxo[] = [];
  const inscriptionUtxos: DiscoveredUtxo[] = [];
  const addresses: Array<{ change: 0 | 1; index: number; address: string }> = [];

  // Fire all address-UTXO fetches in parallel for responsiveness.
  const chains: Array<{ change: 0 | 1; index: number; address: string }> = [];
  for (const change of [0 as const, 1 as const]) {
    for (let i = 0; i < max; i++) {
      chains.push({ change, index: i, address: deriveP2TRAddress(opts.src, change, i) });
    }
  }

  const results = await Promise.all(
    chains.map(async (c) => ({ ...c, utxos: await opts.mp.utxo(c.address).catch(() => []) })),
  );

  for (const { change, index, address, utxos } of results) {
    addresses.push({ change, index, address });
    const script = addressToScriptPubKey(address, net);
    for (const u of utxos) {
      const utxo: DiscoveredUtxo = {
        // Store txid bytes in display order — @scure/btc-signer's addInput
        // takes display bytes and reverses them itself when serializing.
        txid: hexToBytes(u.txid),
        vout: u.vout,
        value: BigInt(u.value),
        scriptPubKey: script,
        address,
        confirmed: u.status.confirmed,
        change,
        index,
      };
      const key = `${mempoolTxidToAtomHex(u.txid)}:${u.vout}`;
      if (inscriptionKeys.has(key)) inscriptionUtxos.push(utxo);
      else fundingUtxos.push(utxo);
    }
  }

  // Smallest-sufficient-first order for funding
  fundingUtxos.sort((a, b) => Number(a.value - b.value));

  const totalFundingSats = fundingUtxos
    .filter((u) => u.confirmed)
    .reduce((s, u) => s + u.value, 0n);

  return { fundingUtxos, inscriptionUtxos, addresses, totalFundingSats };
}
