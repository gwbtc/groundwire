import type { Mempool } from "../chain/mempool.js";
import type { Point, UrbState } from "./state.js";

export function lookupPoint(state: UrbState, patpAtom: bigint): Point | null {
  return state.unvIds.get(patpAtom) ?? null;
}

export interface AuthPubkey {
  tweaked: Uint8Array;   // P2TR output key
  internal: Uint8Array;  // untweaked internal key, for script-path
  utxo: { txid: string; vout: number; value: number; scriptPubKey: Uint8Array };
  source: "witness" | "tweaked-fallback";
}

function hexToBytes(hex: string): Uint8Array {
  const clean = hex.startsWith("0x") ? hex.slice(2) : hex;
  const out = new Uint8Array(clean.length / 2);
  for (let i = 0; i < out.length; i++) {
    out[i] = parseInt(clean.slice(i * 2, i * 2 + 2), 16);
  }
  return out;
}

function bytesToHex(b: Uint8Array): string {
  return Array.from(b, (x) => x.toString(16).padStart(2, "0")).join("");
}

// scriptPubKey for P2TR is: OP_1 <32-byte xonly>. Extract the xonly.
function extractP2trXOnly(scriptHex: string): Uint8Array {
  const s = hexToBytes(scriptHex);
  if (s.length !== 34 || s[0] !== 0x51 || s[1] !== 0x20) {
    throw new Error(`not P2TR: ${scriptHex}`);
  }
  return s.subarray(2);
}

// Txid atoms are stored as little-endian bytes in Hoon (matching wire order).
// Display hex is wire order reversed.
function txidAtomToDisplayHex(atomBytesLE: Uint8Array): string {
  const reversed = new Uint8Array(atomBytesLE.length);
  for (let i = 0; i < atomBytesLE.length; i++) {
    reversed[i] = atomBytesLE[atomBytesLE.length - 1 - i]!;
  }
  return bytesToHex(reversed);
}

// Extract the internal (untweaked) xonly key from a script-path spend's
// witness. BIP-341 control block layout:
//   byte 0: (leaf_version | parity)
//   bytes 1..33: internal xonly pubkey
//   bytes 33+: merkle path (32*n bytes)
// The control block is the *last* witness element.
function extractInternalFromWitness(witness: string[]): Uint8Array | null {
  if (!witness || witness.length < 3) return null;
  const cbHex = witness[witness.length - 1]!;
  const cb = hexToBytes(cbHex);
  if (cb.length < 33) return null;
  if ((cb.length - 1) % 32 !== 0) return null;
  return cb.subarray(1, 33);
}

// Resolve the on-chain P2TR key(s) that currently control a point's inscription.
// The "tweaked" key is the P2TR output key (directly from scriptPubKey).
// The "internal" key is pulled from the reveal tx's script-path witness so
// future reveals (script-path spends) can be signed correctly.
export async function resolveAuthPubkey(
  state: UrbState,
  patpAtom: bigint,
  mp: Mempool,
): Promise<AuthPubkey> {
  const point = lookupPoint(state, patpAtom);
  if (!point) throw new Error("resolveAuthPubkey: point not in snapshot");

  const txidHex = txidAtomToDisplayHex(point.own.sont.txid);
  const voutIdx = Number(point.own.sont.vout);
  const tx = await mp.tx(txidHex);
  const out = tx.vout[voutIdx];
  if (!out) throw new Error(`resolveAuthPubkey: vout ${voutIdx} missing in ${txidHex}`);
  const tweaked = extractP2trXOnly(out.scriptpubkey);

  // The tx at point.own.sont.txid IS the most recent reveal (the tx that
  // produced the current inscription UTXO). Its input 0's witness should be
  // a script-path spend containing the attestation leaf and a control block.
  let internal: Uint8Array | null = null;
  const input0 = tx.vin[0];
  if (input0 && input0.witness) {
    internal = extractInternalFromWitness(input0.witness);
  }

  return {
    tweaked,
    internal: internal ?? tweaked,
    source: internal ? "witness" : "tweaked-fallback",
    utxo: {
      txid: txidHex,
      vout: voutIdx,
      value: out.value,
      scriptPubKey: hexToBytes(out.scriptpubkey),
    },
  };
}
