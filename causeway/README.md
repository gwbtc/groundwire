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
  browser (kelvin-9 confidential), building a single commit-only PSBT for you
  to sign, and handing you a one-line shell command to boot the comet afterwards.
  The attestation is committed on-chain but never revealed; the custody log is
  baked into the boot feed off-chain.
- **Manage** an existing comet: rotate its networking key (`rekey`, optionally
  breaching). **That is the only on-chain management op.** Under the kelvin-9
  OP_RETURN revision the other sotx opcodes — `escape`, `cancel-escape`,
  `adopt`, `reject`, `detach`, `fief`, `set-mang` — were removed from the
  chain; sponsorship and escape are off-chain (spec §8), and a static endpoint
  (`fief`) is now a field of the committed snapshot rather than an operation.
  `src/ops/index.ts` exports exactly one op, `rekey`.

Groundwire IDs are shown as **mnemonyms** — a BIP-39-style word rendering of the
comet's 128-bit @p (e.g. `.routine.inhale.regimes.…`) — rather than the scrambled
`~mosnyt-londen-…` @p. The encoder + wordlist are ported from
[gwbtc/mnemonyms](https://github.com/gwbtc/mnemonyms) (`src/protocol/mnemonym.ts`,
cross-checked against the reference vectors). The @p remains the machine identity
(boot `--comet`, pier name, proof.json, sotx); input fields accept either form.

Every operation:

1. Causeway fetches the point snapshot and resolves your point.
2. It filters out the sat-carrying UTXO from your fee coin selection so you
   can't accidentally burn your own @p by paying fees with it.
3. It builds a **single** PSBT. There is no commit+reveal pair: the new
   snapshot is committed in the **taproot tweak** of the sat-carrying output,
   so on-chain the transaction is a plain P2TR key-path spend and nothing is
   revealed. (`--publish`/public spawns additionally carry an OP_RETURN
   publication output that opens the commitment deliberately.)
4. You scan the PSBT into your hardware wallet and scan the signed response
   back into Causeway.
5. Causeway broadcasts it via mempool.space. Peers verify from the off-chain
   `xtr` custody log, which is baked into the boot feed after the tx confirms.

## Dev

```bash
npm install
npm run dev       # http://localhost:5173/causeway/
npm test          # 108 tests in 14 files, no network (~55s; the miner suite is slow)
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
groundwire/causeway/
  src/
    oracle/          snapshot fetch + jam decode + point lookup
    protocol/        jam/mat/bitwriter, patp + mnemonym, tagged hashes
    chain/           taproot, mempool/rpc clients, sat-aware coin selection
    keys/            xpub/descriptor parsing + BIP-86 derivation
    signing/         PSBT construction, UR2.0 codec, QR render + scan
    spawn/           kelvin-9 dat/xtr encoders, pure-JS suite-C miner,
                     feed finalize, boot-command formatter
    ops/             on-chain management ops — rekey, and only rekey
    ui/              hash-routed SPA
  tests/             108 tests: kelvin9, cue, snapshot, xpub, ur, mine, fief,
                     feed-finalize, patp, mnemonym, routable, select,
                     signing, start-height
```

## How to use

1. **Export your xpub** from your hardware wallet as a BIP-380 output descriptor
   (most wallets call this "Descriptor" or "BIP-86 Taproot Account"). It looks
   like `tr([a0b1c2d3/86'/0'/0']xpub6...)/0/*`.
2. **Enter your mnemonym** (or @p) on the landing page. Causeway fetches the
   point snapshot, locates your point, and pulls its sat-carrying UTXO.
3. **Paste your descriptor.** Causeway derives the first 10 receive and 10
   change addresses on BIP-86 and looks for UTXOs at each. The sat-carrying
   UTXO is excluded from fee funding.
4. **Pick an operation** from the dashboard — `rekey` is the only one — and
   Causeway builds a single PSBT and renders it as an animated UR QR.
5. **Sign.** Scan the QR on your hardware wallet, confirm the output, sign.
   Return to Causeway and either scan (via camera) or paste the signed PSBT.
6. **Broadcast.** Causeway verifies the signature, extracts the signed tx, and
   broadcasts it via mempool.space.

### Spawn flow

The spawn page mines a comet entirely in-browser using `@noble/curves` (ed25519)
and `@noble/hashes` (SHA-256/512). The algorithm matches `_mine_c` in
`comet-miner/pkg/vere/comet_miner.c`, tweaked with the kelvin-9 `dat`. The
`~daplyd` star constraint makes it a ~65k-iteration search.

The spawn is **confidential**: Causeway builds ONE transaction
(`src/spawn/assemble.ts`) — a plain BIP-86 key-path spend, signable by any
taproot wallet — whose sat-carrying P2TR output key `Q` commits the initial
snapshot. There is no commit/reveal pair and no NUMS leaf; a chain observer
sees an ordinary taproot payment. Once it confirms, Causeway bakes the
off-chain `xtr` custody log into the boot feed. A `--publish`-style public
spawn adds one OP_RETURN output carrying the opening; the confidential flow
omits it entirely.

After the transaction is broadcast, you see a shell one-liner like:

```bash
curl -fsSL https://groundwire.io/install.sh | sh
~/.groundwire/gw-vere -c ~sampel-palnet -G 0vABC.DEF01.234...
```

which installs the Groundwire runtime and boots your new comet.

## Protocol status (kelvin-9)

The confidential-comets kernel spec moved the attestation into the pass itself
and, in the kelvin-9 OP_RETURN revision, moved on-chain state into the taproot
tweak. **Both front ends run the same protocol**:

- **Web app (this SPA)** — the spawn page (`src/ui/pages/spawn.ts`) mines with
  the kelvin-9 `dat` (`can 0 (mat %gw-btc) (mat 9) [256 d]`, `src/spawn/dat.ts`),
  builds one taproot transaction whose sat-carrying output key commits the
  initial snapshot (`src/spawn/assemble.ts`), and bakes the off-chain custody
  log into the boot feed once it confirms (`src/spawn/reveal-log.ts`
  `bakeXtrIntoFeedAtom`, the twin of `causeway finalize`). The legacy public
  flow (v9 rap-3 tweak + on-chain commit/reveal, formerly `src/spawn/tweak.ts`)
  has been **retired**.
- **Desktop app (`desktop/`)** — the primary front end; same `dat`, and
  `causeway finalize` bakes `xtr` into the boot feed. There is no
  `--legacy-tweak` escape hatch any more — v9 is gone from the desktop CLI too.
  See `desktop/docs/CONFIDENTIAL-COMETS.md` for the full protocol note and
  compatibility matrix.

The desktop app remains the primary, production front end and the web app may
not be used in production — but **the web app is maintained going forward for
correctness and comprehensiveness**, kept at protocol parity with the desktop
twin. When the two disagree, the `urbit eval` golden vectors win.

Both `dat` and `xtr` encoders are pinned to those golden vectors in
`tests/kelvin9.spec.ts` (JS) and `desktop/tests/test_causeway.py` (Python) —
there is no `tests/dat.spec.ts`; the `dat` assertions live in `kelvin9.spec.ts`
alongside the snapshot/leaf/publication vectors. The confidential feed-finalize
step is covered by `tests/feed-finalize.spec.ts`.

## Status

Verified end-to-end:
- Snapshot decode from the live snapshot feed (45 comets at block 945,375).
- BIP-86 test vectors round-trip (mainnet `bc1p...` addresses match spec).
- UR2.0 PSBT codec round-trips a 1KB blob.
- Suite-C miner produces valid ring/pass atoms.
- Production build succeeds; bundle ~120KB gzipped.

Known limitations:
- **Multi-hop reveal parsing:** `oracle/point.ts` extracts the internal key
  from the reveal witness at the point's current UTXO. Points that have
  rekeyed many times and then had additional unrelated spends layered may need
  outspend-walking to find the canonical reveal. When the witness cannot be
  parsed, Causeway flags it and falls back to the tweaked output key.
- **Spawn tweak:** the spawn page uses the kelvin-9 `dat` (`src/spawn/dat.ts`),
  pinned bit-for-bit to `urbit eval` golden vectors in `tests/kelvin9.spec.ts`.
  The legacy v9 `(rap 3 ~[%9 ~tyr ...])` tweak has been retired from the web
  spawn path.
- **Camera QR:** `signing/qr-scan.ts` wraps `qr-scanner` but the op page
  currently uses paste-based input for signed PSBTs. The camera path is
  already wired and just needs a UI toggle.
- **BBQR:** Coldcard Q prefers BBQR; we emit UR2.0 only. Coldcard Q also
  accepts UR2.0, so nothing is blocked — this is a nice-to-have.

## Testing

```bash
npm test                        # 108 tests in 14 files, ~55s
npx tsc --noEmit                # strict typecheck, clean
npx vite build                  # production bundle
```

Most of the wall-clock is `tests/mine.spec.ts` (8 tests, ~52s): it actually
mines suite-C comets rather than asserting against a fixture.

Fixtures are committed:
- `tests/fixtures/snapshot.jam` — a live point snapshot, read by `snapshot.spec.ts`.
- `tests/fixtures/jam-vectors.json` — 14 cue round-trip vectors, read by `cue.spec.ts`.
- `tests/fixtures/mnemonyms.json` — read by `mnemonym.spec.ts`.
- `tests/fixtures/encoder-vectors.json` — **dead**: sotx encoder vectors for the
  retired opcodes. No spec reads it, and its generator `tests/dump_vectors.py`
  emits ops (`escape`, `detach`, `set-mang`, …) that no longer exist on chain.
  Kept only for archaeology; do not treat it as authoritative.

The kelvin-9 vectors are not fixtures here at all — `kelvin9.spec.ts` reads the
shared `groundwire/vectors/gw-kelvin-9.json`, and where that JSON has gone stale
the spec asserts against the authoritative Hoon-test values instead (see the
header comment in that file).

Regenerate after protocol changes:
```bash
python3 tests/dump_jam_vectors.py
curl http://143.198.70.9:8081/apps/urb-watcher/snapshot > tests/fixtures/snapshot.jam
```
