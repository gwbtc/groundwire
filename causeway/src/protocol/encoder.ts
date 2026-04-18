import { BitWriter, bytesToAtomLE } from "./bitwriter.js";
import { OP } from "./opcodes.js";
import type { Fief, Single, SkimSotx, Sont, Sotx } from "./types.js";

function encodeSig(w: BitWriter, sig: bigint | null): void {
  if (sig === null) {
    w.write(2, 0);
  } else {
    w.write(2, 1);
    w.write(512, sig);
  }
}

function encodeFiefInline(w: BitWriter, fief: Fief | null): void {
  if (fief === null) { w.write(2, 0); return; }
  switch (fief.type) {
    case "turf":
      throw new Error("encoder: %turf fief not supported (matches Hoon !!)");
    case "if":
      w.write(2, 2);
      w.write(32, fief.ip);
      w.write(16, fief.port);
      return;
    case "is":
      w.write(2, 3);
      w.write(128, fief.ip);
      w.write(16, fief.port);
      return;
  }
}

function encodeSont(w: BitWriter, sont: Sont): void {
  w.write(1, 0);
  w.write(256, bytesToAtomLE(sont.txid));
  w.writeMat(sont.vout);
  w.writeMat(sont.off);
}

function encodeSingle(w: BitWriter, s: Single): void {
  switch (s.op) {
    case "spawn": {
      w.write(7, OP.spawn);
      w.write(1, 0);
      w.writeMat(s.pass);
      encodeFiefInline(w, s.fief);
      w.write(256, bytesToAtomLE(s.to.spkh));
      w.writeMat(s.to.off);
      w.writeMat(s.to.tej);
      if (s.to.vout === null) {
        w.write(2, 0);
      } else {
        w.write(2, 1);
        w.writeMat(s.to.vout);
      }
      return;
    }
    case "keys":
      w.write(7, OP.keys);
      w.write(1, s.breach ? 1 : 0);
      w.writeMat(s.pass);
      return;
    case "escape":
      w.write(7, OP.escape);
      w.write(1, 0);
      w.write(128, s.parent);
      encodeSig(w, s.sig);
      return;
    case "cancel-escape":
      w.write(7, OP["cancel-escape"]);
      w.write(1, 0);
      w.write(128, s.parent);
      return;
    case "adopt":
      w.write(7, OP.adopt);
      w.write(1, 0);
      w.write(128, s.ship);
      return;
    case "reject":
      w.write(7, OP.reject);
      w.write(1, 0);
      w.write(128, s.ship);
      return;
    case "detach":
      w.write(7, OP.detach);
      w.write(1, 0);
      w.write(128, s.ship);
      return;
    case "fief":
      w.write(7, OP.fief);
      w.write(1, 0);
      encodeFiefInline(w, s.fief);
      return;
    case "set-mang":
      w.write(7, OP["set-mang"]);
      if (s.mang === null) { w.write(2, 0); return; }
      if (s.mang.type === "sont") {
        w.write(2, 1);
        encodeSont(w, s.mang.sont);
      } else {
        w.write(2, 2);
        w.write(256, s.mang.pass);
      }
      return;
  }
}

function writeSkim(w: BitWriter, sot: SkimSotx): void {
  if (sot.op === "batch") {
    const l = sot.items.length;
    if (l <= 1) throw new Error("encoder: batch requires >1 items");
    w.write(7, OP.batch);
    w.writeMat(l);
    for (const item of sot.items) encodeSingle(w, item);
    return;
  }
  encodeSingle(w, sot);
}

export function encodeSkim(sot: SkimSotx): Uint8Array {
  const w = new BitWriter();
  writeSkim(w, sot);
  return w.toBytes();
}

export function encodeFull(sots: Sotx[]): Uint8Array {
  const w = new BitWriter();
  for (const s of sots) {
    encodeSig(w, s.sig);
    w.write(128, s.ship);
    writeSkim(w, s.skim);
  }
  return w.toBytes();
}
