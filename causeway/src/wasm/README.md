# urb/ core (wasm)

`urb_wasm.wasm` is the Passport's `src/urb/` protocol core — the *same*
vector-pinned Rust the device runs (encoder, tweak, ed25519 comet miner,
BIP-341/342 taproot tx-builder+signer, jam/`@uw`, `@p`) — compiled to `wasm32`.
`urb-core.ts` is a dependency-free binding.

The point: Causeway can share one audited engine instead of maintaining a
parallel TypeScript reimplementation of the consensus-critical encoding. See
`tests/wasm-parity.spec.ts` — the wasm reproduces this repo's own
`encoder-vectors.json` (`%spawn`), tweak, and `@p` vectors byte-for-byte.

## Provenance / rebuild

Built from `gwbtc/passport-prime`'s `wasm/` crate:

```sh
cd wasm
RUSTFLAGS="-C target-feature=-reference-types,-sign-ext" \
  cargo +stable build --release --lib --target wasm32-unknown-unknown
cp target/wasm32-unknown-unknown/release/urb_wasm.wasm \
   <causeway>/src/wasm/urb_wasm.wasm
```

(The `-reference-types,-sign-ext` flags keep the module loadable on older
engines; drop them if you only target current browsers/Node ≥18.)

## ABI

No wasm-bindgen. Byte buffers cross as a packed `u64` = `(ptr<<32)|len` into the
exported `memory`; `__alloc` inputs, `__dealloc` every returned buffer. Exports:
`tweak`, `patp`, `uw`, `encode_spawn`, `self_test`. `urb-core.ts` hides all of it
behind typed methods. It is spawn-only, matching the device.
