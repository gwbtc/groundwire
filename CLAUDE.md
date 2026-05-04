# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

Groundwire is a Bitcoin-based alternative to Azimuth for Urbit comet generation and bootstrap. This monorepo contains two Urbit desks written in Hoon:

- **`%groundwire`** — runs a background Gall agent (`%urb-watcher`) that scans Bitcoin for comet attestations and updates the ship's Jael state
- **`%spv-wallet`** — an SPV wallet Gall app used for Bitcoin wallet management and post-boot comet attestations

The `vendor/` directory holds shared libraries (base Urbit libs, BTC wallet structures, test utilities) that get copied into each desk at build time.

## Build Commands

```bash
make build   # generates dist-groundwire/ and dist-spv/ by merging desk sources with vendor deps
make clean   # removes dist directories
```

There is no linter or formatter. There is no standalone test runner — tests run inside an Urbit ship via the Hoon test library.

### Running Tests (inside Urbit)

Tests live in `groundwire/tests/` and `spv-wallet/tests/`. They use `/+  *test` (standard Urbit test library). To run them, commit the desk to a dev ship and invoke `-test` threads from the Dojo.

### Deploying to a Dev Ship

```
|new-desk %groundwire
|mount %groundwire
make build
$ cp -r dist-groundwire/* path/to/zod/groundwire/
|commit %groundwire
|install our %groundwire
```

Repeat analogously for `%spv-wallet`.

## Architecture

### Desk Structure (standard Urbit layout)

```
app/      Gall agents
sur/      structure/type definitions
lib/      libraries
gen/      generators (CLI commands)
mar/      marks (data format handlers)
ted/      threads (async tasks)
tests/    unit tests
```

### Key Agents

- `groundwire/app/urb-watcher.hoon` — polls Bitcoin RPC for new blocks, decodes URB attestations from OP_RETURN outputs, and pokes Jael to update ship keys/sponsors. `new-rpc`, `start-height`, and `block-confirmations` are the main config knobs.
- `spv-wallet/app/spv-wallet.hoon` — UI-facing wallet agent; handles key management, PSBT construction, and submitting `%spawn`/`%escape` attestations to Bitcoin.
- `spv-wallet/app/indexer.hoon` — Bitcoin address indexer used by the wallet.

### Key Libraries

| Library | Location | Purpose |
|---|---|---|
| `urb-core.hoon` | `groundwire/lib/` | Core URB protocol logic (encode/decode attestations) |
| `urb-encoder.hoon` | `groundwire/lib/` | URB script encoding/decoding |
| `groundwire.hoon` | `groundwire/lib/` | Main URB structures and operations |
| `bitcoin.hoon` | `groundwire/lib/` | Transaction structures, encoding/decoding |
| `btc-script.hoon` | `groundwire/lib/` | Script building and parsing |
| `psbt.hoon` | `groundwire/lib/` | BIP174 PSBT support |
| `taproot.hoon` | `spv-wallet/lib/` | BIP86 taproot addresses and script trees |
| `bitcoin-spv.hoon` | `spv-wallet/lib/` | SPV chain header verification |

### Vendor Dependencies

`vendor/base-dev/` provides standard Urbit libs (bip32, bip39, default-agent, strand, etc.). `vendor/btc-wall/` provides BTC wallet structures. The `Makefile` defines exactly which vendor files each desk needs — edit it there when adding/removing shared deps.

### spv-wallet Dependency on groundwire

`spv-wallet` copies several files directly from `groundwire/` at build time (see `GW_FILES_FOR_SPV` in the Makefile). Changes to those groundwire libs affect both desks.

## CI / Deployment

Pushes to `main` trigger `.github/workflows/deploy-desk.yml`, which runs `make build`, SCPs the dist directories to the distribution ship at `143.198.70.9`, and commits both desks via a tmux session named `desk-distributor`.

## Onboarding Script

`onboarding/booting/gw-onboard.py` is a Python CLI that automates comet generation: derives a taproot address, mines a comet with the correct key tweak, submits `%spawn`/`%escape` attestations to Bitcoin, and boots the ship. It depends on `embit`, `pyNaCl`, and `requests`.

### Development

Run from `onboarding/booting/`:

```bash
pip install -r requirements.txt
python3 gw-onboard.py

# Tests
python3 -m unittest test_gw_onboard -v

# Lint / format (enforced by CI)
ruff check
ruff format --check
```

### Building a Frozen Binary with PyInstaller

The script has first-class frozen-build support (`sys.frozen` guards throughout). Build from `onboarding/booting/`:

```bash
pip install pyinstaller
pyinstaller --onefile \
  --hidden-import requests \
  --hidden-import nacl.bindings \
  --hidden-import embit.util.secp256k1 \
  --hidden-import _cffi_backend \
  gw-onboard.py
```

This produces `dist/gw-onboard` (or `dist/gw-onboard.exe` on Windows). The script deliberately does **not** bundle `certifi` — it searches standard system CA paths at runtime instead.

In frozen mode the script resolves external binaries relative to `sys.executable`, so place these files alongside the output executable before distributing:

- `comet_miner` — built from `gwbtc/comet-miner` with `zig build -Doptimize=ReleaseFast -Dprogram=comet_miner`
- `gw-vere` — built from `gwbtc/vere` with `zig build`
- `gw-base.pill` — Groundwire Arvo pill (see `onboarding/booting/README.md` for how to generate it)
