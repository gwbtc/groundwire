// Given a mined result + the user's picked precommit UTXO + derivation keys
// + a sponsor signature, build the full commit + reveal PSBT pair plus the
// deterministic (segwit, witness-excluded) txids Causeway can poll for.

import { sha256 } from "@noble/hashes/sha256";
import { encodeSkim } from "../protocol/encoder.js";
import type { SkimSotx } from "../protocol/types.js";
import type { DiscoveredUtxo } from "../chain/discover.js";
import { deriveKeyInfo } from "../keys/xpub.js";
import type { KeySource } from "../keys/xpub.js";
import { urbLeafScript } from "../chain/tapscript.js";
import {
  buildCommitPsbt, buildRevealPsbt, type KeyInfo, type Utxo,
} from "../signing/psbt.js";
import type { MineResult } from "./miner.js";
import { patpToAtom } from "../protocol/patp.js";
import { ESCAPE_SPONSOR } from "../chain/sponsor.js";

// spkh = sha256(scriptPubKey || u64le(value))
// Matches boot.hoon's extract-spawn-fields and urb-core's calc-precommit-sont.
export function computeSpkh(scriptPubKey: Uint8Array, valueSats: bigint): Uint8Array {
  const buf = new Uint8Array(scriptPubKey.length + 8);
  buf.set(scriptPubKey, 0);
  let v = valueSats;
  for (let i = 0; i < 8; i++) {
    buf[scriptPubKey.length + i] = Number(v & 0xffn);
    v >>= 8n;
  }
  return sha256(buf);
}

// Historically these were set to high unused indices (1000, 1001) to keep
// Causeway's attestation keys out of the user's regular receive set, but that
// meant signing wallets like Sparrow couldn't locate them within their gap
// limit. We now reuse the funding key — see comment inline below.

export interface AssembleArgs {
  mined: MineResult;
  picked: DiscoveredUtxo;         // the precommit UTXO
  keys: KeySource;
  sponsorSig: bigint | null;      // null = spawn without escape (comet will self-sponsor)
  feeRate?: number;               // default 2 sat/vB
}

export interface AssembledSpawn {
  commitPsbt: Uint8Array;
  commitTxid: Uint8Array;         // 32 bytes, wire order (LE of display hex)
  commitTxidHex: string;          // display hex
  commitOutputValue: bigint;
  revealPsbt: Uint8Array;
  revealTxidHex: string;
  revealOutputValue: bigint;
  fundingKey: KeyInfo;
  commitKey: KeyInfo;
  destKey: KeyInfo;
  attestationHex: string;         // for debugging / inspection
}

// Txids are stored in display byte order throughout Causeway (what blocks
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
  const { mined, picked, keys, sponsorSig } = args;

  // Derive the key that controls the funding UTXO at its own (change, index).
  const fundingKey = deriveKeyInfo(keys, picked.change, picked.index);
  // Reuse the funding key for the commit internal key. Wallets like Sparrow
  // only derive within their gap limit (~20); using a high unused index
  // means they can't locate the key for script-path signing. The funding
  // key is always present in the wallet (that's how the UTXO was spotted),
  // and the commit's merkle-root tweak makes the commit output's P2TR
  // address distinct from the funding address, so there's no visual reuse.
  const commitKey = fundingKey;
  // Destination can be any key the user controls. Reusing funding means the
  // inscription sat lands back at the funding address — trackable as a
  // normal incoming UTXO in any wallet.
  const destKey = fundingKey;

  const spkh = computeSpkh(picked.scriptPubKey, picked.value);

  const spawnSingle = {
    op: "spawn" as const,
    pass: mined.pass,
    fief: null,
    to: {
      spkh,
      off: 0n,
      tej: 0n,
      vout: picked.vout === 0 ? null : BigInt(picked.vout),
    },
  };

  const sotx: SkimSotx = sponsorSig !== null
    ? {
        op: "batch",
        items: [
          spawnSingle,
          { op: "escape", parent: patpToAtom(ESCAPE_SPONSOR), sig: sponsorSig },
        ],
      }
    : spawnSingle;

  const attestation = encodeSkim(sotx);
  const leafScript = urbLeafScript(attestation, commitKey.internalKey);

  const funding: Utxo = {
    txid: picked.txid,              // already wire-order LE
    vout: picked.vout,
    value: picked.value,
    scriptPubKey: picked.scriptPubKey,
  };

  const commit = buildCommitPsbt({
    funding, fundingKey, commitKey, leafScript, feeRate,
  });

  const reveal = buildRevealPsbt({
    commitTxid: commit.commitTxid,
    commitOutputValue: commit.commitOutputValue,
    commitOutputScript: commit.commitOutputScript,
    commitKey,
    leafScript,
    destKey,
    feeRate,
  });

  // Reveal txid — we need it too. Compute from reveal's unsigned tx bytes.
  // buildRevealPsbt doesn't expose txid today, so derive it here.
  const revealTxidHex = computeRevealTxid(reveal.psbt);

  return {
    commitPsbt: commit.psbt,
    commitTxid: commit.commitTxid,
    commitTxidHex: bytesToDisplayHex(commit.commitTxid),
    commitOutputValue: commit.commitOutputValue,
    revealPsbt: reveal.psbt,
    revealTxidHex,
    revealOutputValue: reveal.revealOutputValue,
    fundingKey,
    commitKey,
    destKey,
    attestationHex: bytesToHex(attestation),
  };
}

// Compute txid from a PSBT by deserializing and hashing the unsigned tx.
// Returns DISPLAY-order hex (reverse of internal double-SHA output).
import { Transaction } from "@scure/btc-signer";
function computeRevealTxid(psbt: Uint8Array): string {
  const tx = Transaction.fromPSBT(psbt);
  const wire = sha256(sha256(tx.unsignedTx));
  const display = new Uint8Array(wire.length);
  for (let i = 0; i < wire.length; i++) display[i] = wire[wire.length - 1 - i]!;
  return bytesToDisplayHex(display);
}
