// Spawn is distinct from other ops: it's how a new @p comes into existence.
// Requires the native comet-miner helper to produce {comet, feed, ring, seed}
// with a `pass` that matches the tweak derived from the funding UTXO.
//
// In fief mode (direct static IP), spawn alone is sufficient.
// In normal mode, the spawn is wrapped in a batch with %escape so the new
// comet gets a networking sponsor right away.

import { atomToPatp } from "../protocol/patp.js";
import { newNonce } from "../signing/qr-protocol.js";
import type { SignRequest } from "../signing/qr-protocol.js";
import type { Fief, SkimSotx } from "../protocol/types.js";
import { miner } from "../spawn/miner.js";
import { buildAttestationLeaf, notImplementedFinalize } from "./_common.js";
import type { FinalizeResult, OpCtx, OpModule } from "./types.js";

export interface SpawnArgs {
  funding: { txid: string; vout: number; valueSats: bigint };
  spkh: Uint8Array;         // 32 bytes
  off: bigint;
  tej: bigint;
  fief: Fief | null;        // if set and no escape, fief-mode spawn
  escapeSponsor?: bigint;   // if provided, batch with %escape
  escapeSponsorSig?: bigint;
}

export const spawnOp: OpModule<SpawnArgs> = {
  name: "spawn",

  async buildRequest(args, ctx): Promise<SignRequest> {
    // Mine the comet. tweakExpr construction is out of scope for this stub —
    // the native helper computes it from the funding UTXO.
    const tweakExpr = new TextEncoder().encode(
      `(rap 3 ~[%9 ~tyr %urb-watcher %btc %gw %9 ${args.funding.txid} ${args.funding.vout} ${args.off}])`,
    );
    const mined = await miner.mine({ tweakExpr });

    const spawnSingle = {
      op: "spawn" as const,
      pass: bytesToBigIntLE(mined.ring),  // best-effort; real pass derivation is in gw-onboard
      fief: args.fief,
      to: { spkh: args.spkh, off: args.off, tej: args.tej, vout: args.funding.vout === 0 ? null : BigInt(args.funding.vout) },
    };

    const skim: SkimSotx = args.escapeSponsor !== undefined
      ? {
          op: "batch",
          items: [
            spawnSingle,
            {
              op: "escape",
              parent: args.escapeSponsor,
              sig: args.escapeSponsorSig ?? null,
            },
          ],
        }
      : spawnSingle;

    buildAttestationLeaf(skim, ctx.authPubkey);

    return {
      v: 1,
      nonce: newNonce(),
      patp: mined.comet,
      op: "spawn",
      summary: `Spawn ${atomToPatp(mined.comet)}`,
      authKey: ctx.authPubkey,
      sighashes: [],
    };
  },

  async finalize(_r, _s, _c): Promise<FinalizeResult> { return notImplementedFinalize(); },
};

function bytesToBigIntLE(b: Uint8Array): bigint {
  let x = 0n;
  for (let i = b.length - 1; i >= 0; i--) x = (x << 8n) | BigInt(b[i]!);
  return x;
}
