// cc-draft-2 CONFIDENTIAL spawn assembler.
//
// Given a mined result + the user's picked spawn UTXO + derivation keys, build
// the single COMMIT-ONLY PSBT (no on-chain reveal) plus the deterministic
// (segwit, witness-excluded) commit txid Causeway can poll for.
//
// This is the web twin of desktop/causeway.py's build_confidential_commit_psbt
// + run_spawn_connect:
//   * input 0  — the spawn (funding) UTXO, spent via P2TR key-path.
//   * output 0 — P2TR(internal = funding xonly, merkle_root = tapleaf(%spawn)).
//     The %spawn attestation is COMMITTED in the taproot output but never
//     revealed on-chain: the leaf's OP_CHECKSIG uses the BIP-341 NUMS key
//     (nobody can sign it), so the script path is dead and the sat stays
//     key-path spendable by the point owner for any future management op.
//
// The attestation itself (the %spawn sotx) is disclosed off-chain to the
// %gw-btc verifier via the xtr reveal log baked into the boot feed (see
// reveal-log.ts + mine-c.ts bakeXtrIntoFeed, the twin of `causeway finalize`).
//
// Unlike the retired legacy public flow, there is NO reveal PSBT and NO
// on-chain %escape — the leaf commits a bare %spawn, matching the desktop.

import { sha256 } from "@noble/hashes/sha256";
import { encodeFull } from "../protocol/encoder.js";
import type { SkimSotx } from "../protocol/types.js";
import type { DiscoveredUtxo } from "../chain/discover.js";
import { deriveKeyInfo } from "../keys/xpub.js";
import type { KeySource } from "../keys/xpub.js";
import { urbLeafScript, NUMS_XONLY } from "../chain/tapscript.js";
import { buildCommitPsbt, type KeyInfo, type Utxo } from "../signing/psbt.js";
import type { MineResult } from "./miner.js";

const TAP_LEAF_VERSION = 0xc0;

// spkh = shay((can 3 script-pubkey 8^value ~)) per boot.hoon's
// extract-spawn-fields / urb-core's calc-precommit-sont. script-pubkey is a
// big-endian hexb; `can 3` lays its atom out LSB-first, so the hashed byte
// stream is the script bytes REVERSED, then the 8 little-endian value bytes.
// Hashing the script in natural (wire) order — as this did — makes the spkh
// mismatch urb-core for every spawn, silently dropping it after fees are spent.
export function computeSpkh(scriptPubKey: Uint8Array, valueSats: bigint): Uint8Array {
  const buf = new Uint8Array(scriptPubKey.length + 8);
  for (let i = 0; i < scriptPubKey.length; i++) {
    buf[i] = scriptPubKey[scriptPubKey.length - 1 - i]!; // reverse script bytes
  }
  let v = valueSats;
  for (let i = 0; i < 8; i++) {
    buf[scriptPubKey.length + i] = Number(v & 0xffn);
    v >>= 8n;
  }
  return sha256(buf);
}

export interface AssembleArgs {
  mined: MineResult;
  picked: DiscoveredUtxo;         // the spawn (funding) UTXO
  keys: KeySource;
  feeRate?: number;               // default 2 sat/vB
}

export interface AssembledSpawn {
  commitPsbt: Uint8Array;
  commitTxid: Uint8Array;         // 32 bytes, display order (LE of wire hash)
  commitTxidHex: string;          // display hex
  commitOutputValue: bigint;
  fundingKey: KeyInfo;
  // Fields the xtr reveal log needs once the commit confirms (mirrors the
  // proof.json a `causeway finalize` reads: internal key + leaf).
  internalKeyHex: string;         // 32-byte funding xonly hex; xtr uses 02||xonly
  leafScript: Uint8Array;         // NUMS-wrapped %spawn attestation leaf
  leafScriptHex: string;
  leafVersion: number;            // 0xc0
  attestationHex: string;         // encoded sotx bytes — for inspection
}

// Txids are stored in display byte order throughout Causeway (what block
// explorers, mempool.space, Sparrow show — and what @scure/btc-signer's
// addInput expects). So we just hex-encode.
function bytesToDisplayHex(displayBytes: Uint8Array): string {
  return Array.from(displayBytes, (b) => b.toString(16).padStart(2, "0")).join("");
}

function bytesToHex(b: Uint8Array): string {
  return Array.from(b, (x) => x.toString(16).padStart(2, "0")).join("");
}

export function assembleSpawn(args: AssembleArgs): AssembledSpawn {
  const feeRate = args.feeRate ?? 2;
  const { mined, picked, keys } = args;

  // Derive the key that controls the funding UTXO at its own (change, index).
  const fundingKey = deriveKeyInfo(keys, picked.change, picked.index);
  // The commit output's internal key IS the funding xonly, so the sat at
  // output 0 stays key-path spendable by the point owner (load-bearing for
  // urb-core's sont chain: the next management op must key-path spend it).
  const commitKey = fundingKey;

  const spkh = computeSpkh(picked.scriptPubKey, picked.value);

  // A bare %spawn sotx — matching desktop encode_spawn_sotx. urb-core's
  // calc-precommit-sont treats a null vout as undefined behavior and fails; the
  // vout unit must always be some, including vout 0 (the common case).
  const spawnSingle = {
    op: "spawn" as const,
    pass: mined.pass,
    fief: null,
    to: {
      spkh,
      off: 0n,
      tej: 0n,
      vout: BigInt(picked.vout),
    },
  };
  const sotx: SkimSotx = spawnSingle;

  // On-chain unvs carry the full sotx: a [sig ship] header (sig none here;
  // the comet is the from-ship) precedes the skim. urb-core's parse-roll
  // consumes that header before the opcode, so a bare skim is unparseable.
  const attestation = encodeFull([{ ship: mined.comet, sig: null, skim: sotx }]);
  // CONFIDENTIAL: the leaf's OP_CHECKSIG uses the NUMS key so the script path
  // is unspendable — the attestation is committed but can never be revealed on
  // chain. (The legacy public flow used commitKey.internalKey here to make the
  // reveal spendable.)
  const leafScript = urbLeafScript(attestation, NUMS_XONLY);

  const funding: Utxo = {
    txid: picked.txid,              // already display-order
    vout: picked.vout,
    value: picked.value,
    scriptPubKey: picked.scriptPubKey,
  };

  const commit = buildCommitPsbt({
    funding, fundingKey, commitKey, leafScript, feeRate,
  });

  return {
    commitPsbt: commit.psbt,
    commitTxid: commit.commitTxid,
    commitTxidHex: bytesToDisplayHex(commit.commitTxid),
    commitOutputValue: commit.commitOutputValue,
    fundingKey,
    internalKeyHex: bytesToHex(commitKey.internalKey),
    leafScript,
    leafScriptHex: bytesToHex(leafScript),
    leafVersion: TAP_LEAF_VERSION,
    attestationHex: bytesToHex(attestation),
  };
}
