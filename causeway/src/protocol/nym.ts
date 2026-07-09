import { urbCore } from "../wasm/urb-core.js";

/** The mined comet's groundwire mnemonym (gwbtc/mnemonyms). Comets are 128-bit,
 *  so we render exactly 16 little-endian bytes through the shared urb core — the
 *  same Rust the Passport runs. Comet-only: mnemonyms don't name stars/planets,
 *  which keep their `@p` (see `atomToPatp`). */
export function atomToNym(a: bigint): string {
  const core = urbCore();
  if (!core) throw new Error("urb core not initialized — call initUrbCore() at startup");
  const le = new Uint8Array(16);
  for (let i = 0; i < 16; i++) le[i] = Number((a >> BigInt(8 * i)) & 0xffn);
  return core.nym(le);
}
