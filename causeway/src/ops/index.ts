import { adoptOp } from "./adopt.js";
import { cancelEscapeOp } from "./cancel-escape.js";
import { detachOp } from "./detach.js";
import { escapeOp } from "./escape.js";
import { fiefOp } from "./fief.js";
import { rejectOp } from "./reject.js";
import { rekeyOp } from "./rekey.js";
import { setMangOp } from "./set-mang.js";

// NB: spawn is NOT a management op — it has its own dedicated confidential page
// (ui/pages/spawn.ts, the cc-draft-2 flow). The legacy public spawnOp (v9 tweak
// + on-chain reveal via ops/_common) has been retired along with spawn/tweak.ts;
// this registry only carries the post-spawn management ops.
export const ops = {
  adopt: adoptOp,
  "cancel-escape": cancelEscapeOp,
  detach: detachOp,
  escape: escapeOp,
  fief: fiefOp,
  reject: rejectOp,
  rekey: rekeyOp,
  "set-mang": setMangOp,
} as const;

export type { OpName } from "./types.js";
