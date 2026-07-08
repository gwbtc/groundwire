// Dependency-free binding to the Passport's `urb/` protocol core, compiled to
// wasm32 (gwbtc/passport-prime, crate `wasm/`). This is the *same* vector-pinned
// Rust the device runs — encoder, tweak, comet miner, taproot tx-builder — so
// Causeway can share one audited engine instead of maintaining a parallel TS
// reimplementation.
//
// ABI: byte buffers cross as a packed u64 = (ptr<<32)|len into the module's
// exported linear memory. We `__alloc` inputs and `__dealloc` every returned
// buffer. No wasm-bindgen, no runtime deps.

export type FiefIn =
  | { type: "if"; ip: number; port: number }
  | { type: "is"; ip: bigint; port: number }
  | null;

export interface SpawnInput {
  pass: Uint8Array; // little-endian atom bytes
  spkh: Uint8Array; // 32 little-endian bytes
  off: bigint;
  tej: bigint;
  vout: bigint | null;
  fief: FiefIn;
}

interface Exports {
  memory: WebAssembly.Memory;
  __alloc(len: number): number;
  __dealloc(ptr: number, len: number): void;
  tweak(ptr: number, len: number, vout: bigint, off: bigint): bigint;
  patp(ptr: number, len: number): bigint;
  uw(ptr: number, len: number): bigint;
  encode_spawn(
    passP: number, passL: number,
    spkhP: number, spkhL: number,
    off: bigint, tej: bigint, vout: bigint,
    fiefKind: number, ipP: number, ipL: number, port: number,
  ): bigint;
  self_test(): bigint;
}

function leBytes(n: bigint, len: number): Uint8Array {
  const out = new Uint8Array(len);
  for (let i = 0; i < len; i++) out[i] = Number((n >> BigInt(8 * i)) & 0xffn);
  return out;
}

export class UrbCore {
  private constructor(private readonly w: Exports) {}

  /** Instantiate from the wasm bytes (Node: readFileSync; browser: fetch()). */
  static async load(wasm: BufferSource): Promise<UrbCore> {
    const { instance } = await WebAssembly.instantiate(wasm, {});
    return new UrbCore(instance.exports as unknown as Exports);
  }

  private mem(): Uint8Array {
    return new Uint8Array(this.w.memory.buffer);
  }
  private put(bytes: Uint8Array): [number, number] {
    const ptr = this.w.__alloc(bytes.length);
    this.mem().set(bytes, ptr);
    return [ptr, bytes.length];
  }
  private take(packed: bigint): Uint8Array {
    const ptr = Number(packed >> 32n);
    const len = Number(packed & 0xffffffffn);
    const out = this.mem().slice(ptr, ptr + len);
    this.w.__dealloc(ptr, len);
    return out;
  }

  /** `build_tweak_bytes(txidHex, vout, off)` → raw tweak bytes. */
  tweak(txidHex: string, vout: bigint | number, off: bigint | number): Uint8Array {
    const [p, l] = this.put(new TextEncoder().encode(txidHex));
    const out = this.take(this.w.tweak(p, l, BigInt(vout), BigInt(off)));
    this.w.__dealloc(p, l);
    return out;
  }

  /** `to_patp(atomLE)` → the comet `~name`. */
  patp(atomLE: Uint8Array): string {
    const [p, l] = this.put(atomLE);
    const out = new TextDecoder().decode(this.take(this.w.patp(p, l)));
    this.w.__dealloc(p, l);
    return out;
  }

  /** `atom_to_uw(le)` → the `0v…` boot-feed rendering. */
  uw(atomLE: Uint8Array): string {
    const [p, l] = this.put(atomLE);
    const out = new TextDecoder().decode(this.take(this.w.uw(p, l)));
    this.w.__dealloc(p, l);
    return out;
  }

  /** `encode_spawn(..)` → the bit-exact `%spawn` sotx bytes. */
  encodeSpawn(s: SpawnInput): Uint8Array {
    const [pp, pl] = this.put(s.pass);
    const [sp, sl] = this.put(s.spkh);
    let fiefKind = 0;
    let ip = new Uint8Array(0);
    let port = 0;
    if (s.fief) {
      if (s.fief.type === "if") {
        fiefKind = 2;
        ip = leBytes(BigInt(s.fief.ip), 4);
        port = s.fief.port;
      } else {
        fiefKind = 3;
        ip = leBytes(s.fief.ip, 16);
        port = s.fief.port;
      }
    }
    const [ipp, ipl] = this.put(ip);
    const out = this.take(
      this.w.encode_spawn(pp, pl, sp, sl, s.off, s.tej, s.vout ?? -1n, fiefKind, ipp, ipl, port),
    );
    this.w.__dealloc(pp, pl);
    this.w.__dealloc(sp, sl);
    this.w.__dealloc(ipp, ipl);
    return out;
  }

  /** Runs the built-in golden-vector self-check; returns its text report. */
  selfTest(): string {
    return new TextDecoder().decode(this.take(this.w.self_test()));
  }
}
