import { rekeyOp } from "./rekey.js";

// Under the kelvin-9 OP_RETURN revision, the on-chain sotx opcodes
// (spawn/keys/escape/adopt/reject/detach/fief/set-mang) are removed. Spawn has
// its own dedicated page (ui/pages/spawn.ts); sponsorship/escape are off-chain
// (spec §8). The only surviving on-chain management op is a state update —
// rekey (with optional breach).
export const ops = {
  rekey: rekeyOp,
} as const;

export type { OpName } from "./types.js";
