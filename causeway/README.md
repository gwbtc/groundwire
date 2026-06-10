# Causeway

Bring-your-own-Bitcoin identity bridge for [Groundwire](https://groundwire.io)
comets. Causeway is the equivalent of Urbit's Azimuth bridge, but for comets
whose identity lives on Bitcoin rather than Ethereum.

It is a pure static SPA: no backend, no custody, no accounts. Every on-chain
operation is signed air-gapped on your hardware wallet using standard
**PSBT + UR2.0 animated QR**, which means it works with Coldcard Q, Foundation
Passport, Keystone, SeedSigner, and any other wallet that speaks
[BCR-2020-006](https://github.com/BlockchainCommons/Research/blob/master/papers/bcr-2020-006-urtypes.md).

## What it does

- **Spawn** a new Groundwire comet by mining a suite-C networking key in the
  browser, building commit + reveal PSBTs for you to sign, and handing you a
  one-line shell command to boot the comet afterwards.
- **Manage** an existing comet: rotate its networking key (`rekey`), change
  sponsor (`escape`/`cancel-escape`/`adopt`/`reject`/`detach`), pin a static
  endpoint (`fief`), or delegate management (`set-mang`).

Every operation:

1. Causeway fetches the urb-watcher snapshot and resolves your point.
2. It filters out inscription-bearing UTXOs from your coin selection so you
   can't accidentally burn your own @p by paying fees with it.
3. It builds a **commit** PSBT (spends the inscription UTXO, parks the sat
   at a P2TR address with the attestation script tree) and a **reveal** PSBT
   (spends the commit output via script-path, exposing the attestation).
4. You scan each PSBT into your hardware wallet and scan the signed response
   back into Causeway.
5. Causeway broadcasts commit first, then reveal, via mempool.space.

## Dev

```bash
npm install
npm run dev       # http://localhost:5173/causeway/
npm test          # 70 tests, no network
npm run build     # emits ../../../website/static/causeway/
```

The companion page at `/causeway/hw-stub.html` is a **dev-only** software PSBT
signer: paste a base64 PSBT, enter a BIP-39 mnemonic, get a signed PSBT back.
Useful for end-to-end testing in two browser tabs. **Never use a real mnemonic
on the HW stub page.**

A Vite dev proxy forwards `/_proxy/urbwatcher/*` → urb-watcher and
`/_proxy/mempool/*` → mempool.space, so the app works from localhost despite
the upstream servers not sending CORS headers. Production deploys must provide
an equivalent reverse proxy.

## Architecture

```
groundwire/groundwire/causeway/
  src/
    oracle/          snapshot fetch + jam decode + point lookup
    protocol/        urb-encoder.hoon port (20 golden vectors)
    chain/           taproot, mempool/rpc clients, inscription-aware selection
    keys/            xpub/descriptor parsing + BIP-86 derivation
    signing/         PSBT construction, UR2.0 codec, QR render + scan
    spawn/           pure-JS suite-C miner + boot-command formatter
    ops/             one module per urb operation
    ui/              hash-routed SPA
  tests/             70 tests: encoder, cue, snapshot, xpub, ur, mine, …
```

## How to use

1. **Export your xpub** from your hardware wallet as a BIP-380 output descriptor
   (most wallets call this "Descriptor" or "BIP-86 Taproot Account"). It looks
   like `tr([a0b1c2d3/86'/0'/0']xpub6...)/0/*`.
2. **Enter your @p** on the landing page. Causeway fetches the urb-watcher
   snapshot, locates your point, and pulls your inscription UTXO's auth key.
3. **Paste your descriptor.** Causeway derives the first 10 receive and 10
   change addresses on BIP-86 and looks for UTXOs at each. Inscription UTXOs
   are excluded from fee funding.
4. **Pick an operation** from the dashboard. Causeway builds commit + reveal
   PSBTs and renders each as an animated UR QR.
5. **Sign.** Scan each QR on your hardware wallet, confirm the output, sign.
   Return to Causeway and either scan (via camera) or paste the signed PSBTs.
6. **Broadcast.** Causeway verifies the signatures, extracts the signed txs,
   and broadcasts commit → reveal via mempool.space.

### Spawn flow

The spawn page mines a comet entirely in-browser using `@noble/curves` (ed25519)
and `@noble/hashes` (SHA-256/512). The algorithm matches `_mine_c` in
`comet-miner/pkg/vere/comet_miner.c`. With no prefix constraint the miner
terminates on the first iteration (there is no PoW hardness — the "mining" is
just aesthetic constraint selection).

After a successful spawn and reveal broadcast, you see a shell one-liner like:

```bash
curl -fsSL https://groundwire.io/install.sh | sh
~/.groundwire/gw-vere -c ~sampel-palnet -G 0vABC.DEF01.234...
```

which installs the Groundwire runtime and boots your new comet.

## Status

Verified end-to-end:
- Snapshot decode from the live urb-watcher (45 comets at block 945,375).
- BIP-86 test vectors round-trip (mainnet `bc1p...` addresses match spec).
- UR2.0 PSBT codec round-trips a 1KB blob.
- Suite-C miner produces valid ring/pass atoms.
- Production build succeeds; bundle ~120KB gzipped.

Known limitations:
- **Multi-hop reveal parsing:** `oracle/point.ts` extracts the internal key
  from the reveal witness at the inscription's current UTXO. Points that have
  rekeyed/escaped many times and then had additional unrelated spends layered
  may need outspend-walking to find the canonical reveal. When the witness
  cannot be parsed, Causeway flags it and falls back to the tweaked output key.
- **Spawn tweak expression:** `src/ui/pages/spawn.ts`'s `buildTweakBytes` is
  currently a best-effort stub. The exact bit-for-bit `(rap 3 ~[%9 ~tyr ...])`
  atom needs to be re-derived to match urb-core's verification (see
  `urb-core.hoon:363-375`). Easy to finish but requires another encoding pass.
- **Camera QR:** `signing/qr-scan.ts` wraps `qr-scanner` but the op page
  currently uses paste-based input for signed PSBTs. The camera path is
  already wired and just needs a UI toggle.
- **BBQR:** Coldcard Q prefers BBQR; we emit UR2.0 only. Coldcard Q also
  accepts UR2.0, so nothing is blocked — this is a nice-to-have.

## Testing

```bash
npm test                        # 70 tests, ~2s
npx tsc --noEmit                # strict typecheck
npx vite build                  # production bundle
```

Fixtures are committed:
- `tests/fixtures/snapshot.jam` — live urb-watcher snapshot for decode tests.
- `tests/fixtures/encoder-vectors.json` — 20 bit-exact encoder vectors.
- `tests/fixtures/jam-vectors.json` — 14 cue round-trip vectors.

Regenerate after protocol changes:
```bash
python3 tests/dump_vectors.py
python3 tests/dump_jam_vectors.py
curl http://143.198.70.9:8081/apps/urb-watcher/snapshot > tests/fixtures/snapshot.jam
```
