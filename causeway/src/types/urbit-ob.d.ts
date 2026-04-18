declare module "urbit-ob" {
  export function patp(dec: string | number): string;
  export function patp2dec(patp: string): string;
  export function patq(dec: string | number): string;
  export function patq2dec(patq: string): string;
  export function hex2patp(hex: string): string;
  export function hex2patq(hex: string): string;
  export function patp2hex(patp: string): string;
  export function patq2hex(patq: string): string;
  export function isValidPatp(s: string): boolean;
  export function isValidPatq(s: string): boolean;
  export function isValidPat(s: string): boolean;
  export function eqPatq(a: string, b: string): boolean;
  export function clan(patp: string): "galaxy" | "star" | "planet" | "moon" | "comet";
  export function sein(patp: string): string;
}
