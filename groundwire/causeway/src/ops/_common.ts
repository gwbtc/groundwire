// Shared helpers for on-chain operation modules.
//
// All on-chain ops follow the same pattern:
//   1. Build a skim-sotx for the op (encoder.ts)
//   2. Wrap in urb-tagged Taproot script (tapscript.ts)
//   3. Build commit + reveal txs (commit.ts / reveal.ts — not yet impl)
//   4. Emit a SignRequest with both sighashes
//   5. On finalize, attach sigs as witnesses and broadcast

import { encodeSkim } from "../protocol/encoder.js";
import type { SkimSotx } from "../protocol/types.js";
import { urbLeafScript } from "../chain/tapscript.js";

export function buildAttestationLeaf(skim: SkimSotx, xonly: Uint8Array): {
  encoded: Uint8Array;
  leafScript: Uint8Array;
} {
  const encoded = encodeSkim(skim);
  const leafScript = urbLeafScript(encoded, xonly);
  return { encoded, leafScript };
}

export function notImplementedFinalize(): never {
  throw new Error(
    "finalize not implemented — commit/reveal builders in src/chain/ are stubs. " +
    "See plan /Users/trent/.claude/plans/sharded-kindling-parasol.md",
  );
}
