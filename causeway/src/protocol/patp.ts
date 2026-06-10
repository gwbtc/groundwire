import { isValidPatp, patp as dec2patp, patp2dec } from "urbit-ob";

export function patpToAtom(patp: string): bigint {
  const normalized = patp.startsWith("~") ? patp : `~${patp}`;
  if (!isValidPatp(normalized)) throw new Error(`invalid @p: ${patp}`);
  return BigInt(patp2dec(normalized));
}

export function atomToPatp(a: bigint): string {
  return dec2patp(a.toString());
}

export function isPatp(s: string): boolean {
  const normalized = s.startsWith("~") ? s : `~${s}`;
  return isValidPatp(normalized);
}
