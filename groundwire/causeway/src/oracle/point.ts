import type { Mempool } from "../chain/mempool.js";
import type { Point, UrbState } from "./state.js";

export function lookupPoint(state: UrbState, patpAtom: bigint): Point | null {
  return state.unvIds.get(patpAtom) ?? null;
}

export interface AuthPubkey {
  tweaked: Uint8Array;   // P2TR output key — use for key-path sig verify
  internal: Uint8Array;  // untweaked internal key — use for script-path sig verify
  utxo: { txid: string; vout: number; value: number; scriptPubKey: Uint8Array };
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

// Bitcoin txids on the wire are little-endian of the hash; display hex is
// reversed. Given a txid atom stored internally as display-order bytes, convert
// to the canonical display hex string (reverse of raw-byte hex).
function txidAtomToDisplayHex(atomBytesLE: Uint8Array): string {
  // atomBytesLE came from atomToBytesLE(..., 32). Display-hex = LE-bytes reversed.
  const reversed = new Uint8Array(atomBytesLE.length);
  for (let i = 0; i < atomBytesLE.length; i++) {
    reversed[i] = atomBytesLE[atomBytesLE.length - 1 - i]!;
  }
  return bytesToHex(reversed);
}

// Resolve the on-chain P2TR key(s) that currently control a point's inscription.
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

  // Internal key comes from the reveal witness' control block. For points
  // whose current inscription was spent further downstream we'd walk outspends;
  // for now we assume the inscription hasn't moved past the genesis reveal,
  // and reuse the tweaked key as a stand-in until reveal parsing lands.
  // TODO(reveal-witness-parse): extract internal from control block.
  const internal = tweaked;

  return {
    tweaked,
    internal,
    utxo: {
      txid: txidHex,
      vout: voutIdx,
      value: out.value,
      scriptPubKey: hexToBytes(out.scriptpubkey),
    },
  };
}
