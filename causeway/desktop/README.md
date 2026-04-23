# Causeway (Desktop)

CLI + TUI for Groundwire comet spawning and management, using **confidential comets** for air-gapped signing compatibility.

## What it does

- **Spawn a comet** from an air-gapped environment by signing a single standard P2TR commit transaction in any Bitcoin wallet (Sparrow, BlueWallet, Passport, Keystone, Coldcard, Ledger, etc).
- **Manage a comet** (rekey, escape, cancel-escape, set-fief) via the same signing path.
- Emit an off-chain **attestation proof** (`comet.proof.json`) alongside the `gw-vere` boot feed.

Confidential = no reveal transaction. The urb attestation sits inside a committed-but-never-revealed taproot leaf; peers validate it later via the proof over Ames (once runtime support lands).

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

Prints a fresh 12-word seed phrase, derives a P2TR address, requests 1000 sats from the faucet, waits for confirmation, mines a comet under `~daplyd`, signs + broadcasts a single confidential commit tx, then prints the boot command.

## Quickstart (Connect Wallet)

```bash
causeway spawn connect --xpub <YOUR_XPUB>
```

Same flow, but you sign the PSBT externally (scan a UR animated QR into Passport/Keystone, or load the `.psbt` file in Sparrow/BlueWallet).

## Management ops

```bash
causeway rekey --point ~sampel-palnet --xpub <XPUB>
causeway escape --point ~sampel-palnet --parent ~daplyd --xpub <XPUB>
causeway cancel-escape --point ~sampel-palnet --parent ~daplyd --xpub <XPUB>
causeway fief --point ~sampel-palnet --xpub <XPUB> --ip 1.2.3.4 --port 31337
```

Each emits `<patp>-<op>-<txid>.proof.json`. To apply the attestation inside your ship, poke the `%spv-wallet` agent:

```
:spv-wallet +import-proof /path/to/proof.json
```

(Note: the runtime side of proof consumption is still a work-in-progress. The proof is stored for future use.)

## Why confidential?

See [docs/CONFIDENTIAL-COMETS.md](docs/CONFIDENTIAL-COMETS.md).
