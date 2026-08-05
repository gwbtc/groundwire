// kelvin-9 CONFIDENTIAL (or public) spawn assembler.
//
// Given a mined result + the user's picked spawn UTXO + derivation keys, build
// the SINGLE spawn transaction (no commit/reveal pair, no NUMS leaf):
//   * input 0  — the spawn (funding) UTXO, spent BIP-86 key-path.
//   * output 0 — the sat-carrying P2TR output whose key Q commits the initial
//                snapshot (5120 || Q). The sat travels here at offset 0.
//   * optional OP_RETURN publication output — PUBLIC spawns only; the
//                confidential flow omits it entirely (nothing to grep on chain).
//
// The state is committed on-chain in Q for everyone; only a PUBLIC spawn also
// reveals the opening (pass + snapshot + blind-opening) in the OP_RETURN. A
// confidential comet discloses its opening off-chain, via the xtr custody log
// baked into the boot feed once the spawn tx confirms (reveal-log.ts +
// mine-c.ts bakeXtrIntoFeed).

import type { DiscoveredUtxo } from "../chain/discover.js";
import { deriveKeyInfo } from "../keys/xpub.js";
import type { KeySource } from "../keys/xpub.js";
import { buildSpawnPsbt, type KeyInfo, type Utxo } from "../signing/psbt.js";
import {
  assertRoutable, stateOutputKey, stateOutputScript, type Snapshot,
} from "./snapshot.js";
import { buildPublicationScript, type Opening } from "./publication.js";
import type { SpawnSont } from "./dat.js";
import type { MineResult } from "./miner.js";
import { bytesToAtomLE } from "../protocol/bitwriter.js";
import { bytesToHex, bytesToAtomBE } from "../protocol/tagged-hash.js";

export interface AssembleArgs {
  mined: MineResult;
  picked: DiscoveredUtxo;         // the spawn (funding) UTXO — the spawn satpoint
  keys: KeySource;
  feeRate?: number;               // default 2 sat/vB
  publish?: boolean;              // PUBLIC spawn (add OP_RETURN); default confidential
  startHeight?: number;           // blind-opening height for a public spawn (default 0)
  sponsor?: bigint | null;        // sponsor @p committed in the initial snapshot
  noRoute?: boolean;              // deliberately mint an UNROUTABLE comet (no sponsor, no fief)
  dom?: string;
}

export interface AssembledSpawn {
  spawnPsbt: Uint8Array;
  spawnTxid: Uint8Array;          // 32 bytes, display order
  spawnTxidHex: string;           // display hex
  outputValue: bigint;            // sat-carrying output value
  fundingKey: KeyInfo;
  // Data the xtr opening / finalize needs (mirrors the desktop proof.json):
  internalKeyHex: string;         // 33-byte compressed internal key (02||xonly)
  snapshot: Snapshot;             // the initial committed snapshot
  spawnSont: SpawnSont;           // for the blind-opening
  blindHex: string;               // 32-byte blind (BE hex) — opens the dat commitment
  publicationScriptHex?: string;  // present iff public
}

// Txids are stored in display byte order throughout Causeway.
function bytesToDisplayHex(displayBytes: Uint8Array): string {
  return Array.from(displayBytes, (b) => b.toString(16).padStart(2, "0")).join("");
}

export function assembleSpawn(args: AssembleArgs): AssembledSpawn {
  const feeRate = args.feeRate ?? 2;
  const { mined, picked, keys } = args;

  // Key that controls the funding UTXO at its own (change, index). Its xonly is
  // the internal key P for the sat-carrying output, so the point owner can
  // later key-path spend it (with the state leaf's merkle root) for a rekey.
  const fundingKey = deriveKeyInfo(keys, picked.change, picked.index);
  const internalKey33 = new Uint8Array(33);
  internalKey33[0] = 0x02; // BIP-341 lifts to even-y, so the parity byte is nominal
  internalKey33.set(fundingKey.internalKey, 1);

  const spawnSont: SpawnSont = { txidHex: bytesToDisplayHex(picked.txid), vout: picked.vout, off: 0 };

  // Fresh-spawn snapshot: life 1, rift 0, messaging key = cry.pub (mined.cPub),
  // the operator's chosen sponsor (absent = self-sponsored), no fief.
  const snapshot: Snapshot = {
    life: 1,
    rift: 0,
    key: bytesToAtomLE(mined.cPub),
    sponsor: args.sponsor ?? null,
    fief: null,
  };
  // Refuse to mint an identity nothing can route to, unless deliberately
  // opted out. This is a Causeway policy, NOT a protocol validity rule.
  assertRoutable(snapshot, args.noRoute ?? false);

  const q = stateOutputKey(internalKey33, snapshot);
  const outputScript = stateOutputScript(q);

  // Public spawn: reveal the opening (pass + snapshot + blind-opening) on chain.
  let publicationScript: Uint8Array | undefined;
  if (args.publish) {
    const opening: Opening = {
      internalKey: bytesToAtomBE(internalKey33),
      snapshot,
      blindOpening: {
        spawnSont,
        startHeight: args.startHeight ?? 0,
        blind: bytesToAtomBE(mined.blind),
      },
    };
    publicationScript = buildPublicationScript(mined.pass, opening);
  }

  const funding: Utxo = {
    txid: picked.txid,
    vout: picked.vout,
    value: picked.value,
    scriptPubKey: picked.scriptPubKey,
  };

  const built = buildSpawnPsbt({
    funding,
    fundingKey,
    outputScript,
    ...(publicationScript ? { publicationScript } : {}),
    feeRate,
  });

  return {
    spawnPsbt: built.psbt,
    spawnTxid: built.txid,
    spawnTxidHex: built.txidHex,
    outputValue: built.outputValue,
    fundingKey,
    internalKeyHex: bytesToHex(internalKey33),
    snapshot,
    spawnSont,
    blindHex: bytesToHex(mined.blind),
    ...(publicationScript ? { publicationScriptHex: bytesToHex(publicationScript) } : {}),
  };
}
