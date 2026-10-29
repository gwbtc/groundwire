# Groundwire Comet Onboarding

> **This is the legacy onboarding path.** The current, production way to mint
> and boot a confidential comet is Causeway (`causeway/desktop/`), and the
> end-to-end bring-up procedure — runtime, pill, identity, desks, light
> client — is [`ops/doc/OPERATIONS.md`](../../ops/doc/OPERATIONS.md).
> `gw-onboard.py` predates the kelvin-9 OP_RETURN protocol and still mines the
> legacy v9 `(rap 3 ~[%9 ~tyr …])` tweak. Keep it for reference; do not treat
> it as the supported flow.

`gw-onboard.py` is a Python script that wraps the Groundwire boot process. It generates a master ticket, derives a taproot address, funds it via the faucet, mines a comet, and boots it.

## Prerequisites

Clone and build these Groundwire repos into this directory:

- gwbtc/groundwire
- gwbtc/spv-wallet
- gwbtc/urbit
- gwbtc/vere
- gwbtc/urcrypt
- gwbtc/comet-miner

You'll have to ask ~tinnus-napbus, ~bonbud-macryg, ~hanfel-dovned, and ~niblyx-malnus what the latest branches are. Make sure you're on the correct branches!

The end goal is to have these files relative to this directory:

- `./comet-miner/zig-out/<platform>/comet_miner`
- `./vere/zig-out/<platform>/urbit`
- `./gw-base.pill`

`<platform>` is zig's **resolved target triple**, auto-detected by the script
and probed on disk. It is not `<arch>-<os>-none` everywhere:

| host | directory zig actually writes |
| --- | --- |
| Apple Silicon macOS | `aarch64-macos-none` |
| Intel macOS | `x86_64-macos-none` |
| x86-64 Linux | `x86_64-linux-musl` |
| aarch64 Linux | `aarch64-linux-musl` |

Both `vere/build.zig` and `comet-miner/build.zig` rewrite a *native* Linux
build's ABI to musl (`build.zig:252-269` in vere), so even a bare `zig build`
on a glibc host emits `…-linux-musl`. **`x86_64-linux-none` is a triple zig
never emits** — an `-Dasan`/`-Dubsan` build is the only one that escapes the
rewrite, and it lands in `…-linux-gnu`. The script tries musl then gnu and, if
neither exists, names both paths in its error. You can always override with
`--miner` and `--vere`.

Install Python dependencies:

```
pip install -r requirements.txt
```

## Building

Zig **0.15.2** is the pinned version for both trees — see `vere/INSTALL.md`,
and CI, which uses `mlugg/setup-zig@v2` with `version: 0.15.2`. Other versions
are not supported; the build uses 0.15-era `std.Build` APIs.

1. Build comet miner:
   ```
   zig build -Doptimize=ReleaseFast -Dprogram=comet_miner
   ```

2. Build Vere (requires urcrypt in this directory):
   ```
   zig build
   ```

   `vere/build.zig` opens `.git/logs/HEAD` unconditionally to stamp the
   version, so an exported tarball with no reflog hard-fails.

3. Get the Groundwire pill — **from CI, not by hand.**

   The authoritative recipe is the `build-pill` job of
   `.github/workflows/groundwire-build.yml` in `gwbtc/urbit`. It builds a
   **brass** pill on a fake `~zod` via `fyrd`/`khan-eval` (`brass:pill`),
   baking in `%gw-base`, `%gw-btc`, `%mcp` and `%vitriol`. The pill is
   written into Clay at `/pill/pill` on `%base` — not into `.urb/put/` — and
   CI copies it out as `gw-base.pill`.

   ```
   gh workflow run groundwire-build.yml --repo gwbtc/urbit --ref gw/next/kelvin/408
   # then download the `gw-base-pill` artifact -> ./gw-base.pill
   ```

   > **Known broken: the hand-run `+pill/solid` route.** The old recipe here
   > was: boot a stock fake ship, `|new-desk %gw-base`, `|mount %gw-base`,
   > copy `gwbtc-urbit/pkg/arvo/*` in, `|commit %gw-base`,
   > `.gw-base/pill +pill/solid %gw-base`. Against the current kernel that
   > fails to compile with `mint-vain` in `/sys/vane/ames/hoon`, and no pill
   > is ever written. There is no local pill build in this repo; use CI.
   > (A `fyrd (solid:pill …)` build from a *builder fakezod already booted on
   >  a Groundwire pill* has worked — see
   >  `ops/doc/live-tests/PHASE67-RESULTS.md` — but that is a kernel-test
   >  procedure, not the bring-up path, and it needs a Groundwire pill to start
   >  from.)

## Running

```
python3 gw-onboard.py
```

The script will:
1. Generate a master ticket (save this!)
2. Derive a **mainnet** taproot funding address (`bc1p…`; `NETWORKS["main"]`
   throughout, and it watches mainnet mempool.space — the old "signet" wording
   here was stale)
3. Request sats from the faucet and wait for confirmation (10-30 minutes typical)
4. Mine your comet identity
5. Boot the comet

The script prints login and setup instructions before booting. Once running, open `http://localhost:8080/spv-wallet` in your browser.

If the script crashes after funding, you can resume without losing your UTXO:
```
python3 gw-onboard.py --master-ticket '~your-ticket-here'
```

## Post-boot

1. Install the Groundwire and SPV Wallet desks to your new ship. Both installations and initial chain syncs will take a while.

2. After a few block confirmations, you should see your comet attestation come
   in within the `%gw-btc` agent's output. (`%urb-watcher` is the old name for
   this agent; `desk.bill` ships `%gw-btc` and `%urb-snapshot`.)

## Development

Run tests (needs `pip install -r requirements.txt` first — the suite imports
`gw-onboard.py`, which pulls in `bitstring` via `pynoun`):
```
python3 -m unittest test_gw_onboard -v
```

Lint and format:
```
ruff check
ruff format --check
```

Both are enforced by CI on all PRs targeting `main`.

## Caveat

The number of moving parts this script depends on means it can go out of date
quickly: the Python script, the `%gw-btc` agent, and the SPV wallet all have to
agree on the same Bitcoin network and the same attestation protocol. They
currently do not — `gw-onboard.py` mines the retired v9 tweak while the desk
runs kelvin-9 — which is why this path is legacy and Causeway is the supported
one. See [`ops/doc/OPERATIONS.md`](../../ops/doc/OPERATIONS.md).
