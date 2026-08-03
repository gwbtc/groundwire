# Causeway (Desktop)

CLI + TUI for Groundwire comet spawning and management on the **kelvin-9**
(`%gw-btc`, OP_RETURN) confidential-comets protocol, for air-gapped signing
compatibility.

## What it does

- **Spawn a comet** from an air-gapped environment by signing a single standard P2TR transaction in any Bitcoin wallet (Sparrow, BlueWallet, Passport, Keystone, Coldcard, Ledger, etc).
- **Rekey a comet** (rotate the messaging key, optionally breach) via the same signing path.
- Emit an off-chain **attestation proof** (`comet.proof.json`) alongside the `gw-vere` boot feed.

Confidential = no on-chain payload. The comet's `@p` commits to a **hiding**
`dat` (the spawn satpoint behind a seed-derived blind), and its per-identity
state (life/rift/messaging-key/sponsor) is committed in the **taproot tweak of
the sat-carrying output** — a chain observer sees only a plain P2TR key. Peers
validate the identity from the off-chain `xtr` custody log (or a deliberate
public OP_RETURN publication). Sponsorship and escape are off-chain.

## Install

```bash
pip install -e .
```

Then:

```bash
causeway --help           # CLI
causeway-tui              # Textual terminal UI
```

## Quickstart (Generate New Wallet)

```bash
causeway spawn generate --invite <FAUCET_CODE>
```

Prints a fresh 12-word seed phrase, derives a P2TR address, requests 1000 sats from the faucet, waits for confirmation, mines a comet under `~daplyd`, signs + broadcasts a single spawn tx (confidential by default; pass `--publish` for a public on-chain publication), then prints the boot command.

## Quickstart (Connect Wallet)

```bash
causeway spawn connect --xpub <YOUR_XPUB>
```

Same flow, but you sign the PSBT externally (scan a UR animated QR into Passport/Keystone, or load the `.psbt` file in Sparrow/BlueWallet).

## Management (rekey)

Rekey is the only on-chain management op in kelvin-9; sponsorship and escape
are off-chain. A rekey spends the point's current sat-carrying output key-path
and commits a new snapshot (life+1, rift+1 on `--breach`, rotated messaging
key), chaining off the point's prior proof:

```bash
causeway rekey --point ~sampel-palnet --prior-proof ~sampel-palnet-spawn.proof.json \
  --new-pass-hex <NEW_PASS_HEX> [--breach]
```

`--point` accepts a **mnemonym** (`.routine.inhale.…`) or a @p; the CLI and TUI
render comet IDs as mnemonyms (the @p is shown alongside as the machine form and
used for filenames). The encoder + wordlist are vendored from
[gwbtc/mnemonyms](https://github.com/gwbtc/mnemonyms) under `desktop/vendor/`.

It emits `<patp>-rekey-<txid>.proof.json`. After it confirms, hand the new `xtr`
entry + opening to your ship's `%gw-btc` agent (the `%anew` poke) so peers can
re-verify you.

## Finalize — bake the custody log into your boot feed

Once the spawn tx confirms, bake the off-chain custody log (`xtr`) into the feed
so the booted ship's pass carries its own attestation. Entry 0 (the spawn)
additionally opens the hiding `dat` commitment via its blind-opening:

```bash
causeway finalize ~sampel-palnet-spawn.proof.json --feed 0vABC...
# after a rekey, pass every proof for the point, oldest first:
causeway finalize spawn.proof.json rekey.proof.json --feed 0vABC...
```

This records `block_hash`/`block_height`/`xtr_hex` in the proof(s) and
prints an updated boot one-liner. Booting with the miner's original feed
also works — the ship just serves an empty log until an `%anew`
round-trip (or a re-boot with the finalized feed) supplies it.

## Why confidential?

See [docs/CONFIDENTIAL-COMETS.md](docs/CONFIDENTIAL-COMETS.md).
