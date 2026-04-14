import { adoptOp } from "./adopt.js";
import { cancelEscapeOp } from "./cancel-escape.js";
import { detachOp } from "./detach.js";
import { escapeOp } from "./escape.js";
import { fiefOp } from "./fief.js";
import { loginOp } from "./login.js";
import { rejectOp } from "./reject.js";
import { rekeyOp } from "./rekey.js";
import { setMangOp } from "./set-mang.js";
import { spawnOp } from "./spawn.js";

export const ops = {
  adopt: adoptOp,
  "cancel-escape": cancelEscapeOp,
  detach: detachOp,
  escape: escapeOp,
  fief: fiefOp,
  login: loginOp,
  reject: rejectOp,
  rekey: rekeyOp,
  "set-mang": setMangOp,
  spawn: spawnOp,
} as const;

export type OpName = keyof typeof ops;
