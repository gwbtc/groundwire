# Groundwire Comet Onboarding

`gw-onboard.py` is a Python script that wraps the Groundwire boot process. It generates a master ticket, derives a signet taproot address, funds it via the faucet, mines a comet, and boots it.

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

Where `<platform>` is auto-detected (e.g. `aarch64-macos-none`, `x86_64-linux-none`). You can override the paths with `--miner` and `--vere` flags.

Install Python dependencies:

```
pip install -r requirements.txt
```

## Building

1. Build comet miner using Zig 0.14.1:
   ```
   zig build -Doptimize=ReleaseFast -Dprogram=comet_miner
   ```

2. Build Vere using Zig 0.14.1 (requires urcrypt in this directory):
   ```
   zig build
   ```

3. Build the Groundwire pill:
   - Boot a normal fake ship with a non-Groundwire Urbit binary.
   - `|new-desk %gw-base`, `|mount %gw-base`
   - `cp -r gwbtc-urbit/pkg/arvo/* zod/gw-base`
   - `|commit %gw-base`
   - `.gw-base/pill +pill/solid %gw-base`
   - Copy the pill out of the ship's `.urb` directory into this directory.

## Running

```
python3 gw-onboard.py
```

The script will:
1. Generate a master ticket (save this!)
2. Derive a signet funding address
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

2. After a few block confirmations, you should see your comet attestation come in within `%urb-watcher` output.

## Development

Run tests:
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

The number of moving parts this script depends on means it can go out of date quickly. Details like the Python script, `%urb-watcher`, and the SPV wallet all need to be pointed at signet. Things will break when these get out of sync.
