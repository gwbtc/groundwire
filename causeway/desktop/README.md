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

## The whole CLI

| command | what it does |
| --- | --- |
| `causeway spawn generate` | mint a wallet, fund it, spawn a comet, print the boot one-liner |
| `causeway spawn connect --xpub …` | same, but you sign the PSBT externally |
| `causeway rekey` | rotate the messaging key on an existing point (the only on-chain management op) |
| `causeway finalize <proof…>` | bake the `xtr` custody log into the proofs and, with `--feed`, into the boot feed |
| `causeway proof show <path>` | pretty-print a `proof.json` |
| `causeway proof verify <path>` | check a `proof.json` for internal consistency and (by default) against its on-chain tx |
| `causeway-tui` | Textual terminal UI over the same flows |

There is **no `causeway mine`** — mining happens inside `spawn generate` /
`spawn connect`, via the external `comet_miner` binary (see `--miner`).

`--miner` defaults to `./comet-miner/zig-out/<zig-target-triple>/comet_miner`,
resolved by probing the triples zig actually emits: `aarch64-macos-none` /
`x86_64-macos-none` on macOS, and `x86_64-linux-musl` (then `…-linux-gnu`) on
Linux, because `comet-miner/build.zig` rewrites a native Linux build to musl.
If neither exists, Causeway names both paths it looked at. Pass `--miner`
explicitly to override.

## Quickstart (Generate New Wallet)

```bash
causeway spawn generate --invite <FAUCET_CODE>
```

Prints a fresh 12-word seed phrase, derives a P2TR address, requests 1000 sats from the faucet, waits for confirmation, mines a comet under `~daplyd`, signs + broadcasts a single spawn tx (confidential by default; pass `--publish` for a public on-chain publication), then prints the boot command.

That seed phrase is a **complete** backup: the comet's blind is derived from it (see [Recovery](#recovery--keep-the-phrase-not-just-the-file)).

## Quickstart (Connect Wallet)

```bash
causeway spawn connect --xpub <YOUR_XPUB>
```

Same flow, but you sign the PSBT externally (scan a UR animated QR into Passport/Keystone, or load the `.psbt` file in Sparrow/BlueWallet).

Because your wallet never hands over its seed, this path prints a **separate
12-word blind recovery phrase** and makes you write it back before mining. Keep
it with the same care as your wallet seed — see below. To re-derive a blind you
already hold a phrase for, pass `--blind-mnemonic "<12 words>"`.

### Spawn flags

Common to both spawn commands: `--invite` (faucet code), `--fee-rate`
(sat/vB, default 2), `--network main|testnet`, `--output-dir`, `--miner`,
`--mempool-base`, `--publish` (public spawn: add an OP_RETURN publication
output opening the `dat`; default is confidential), `--sponsor` (@p or
mnemonym committed in the initial snapshot) and `--no-route` (deliberately
mint an unroutable, outbound-only comet).

`spawn connect` additionally takes `--xpub` (required), `--blind-mnemonic`,
`--utxo` and `--signed-psbt`.

### Scripting a spawn

Causeway aborts rather than looping when a prompt cannot be answered, so a
headless run must pre-answer every one of them. Three flags do that:

| flag | replaces the prompt for |
| --- | --- |
| `--utxo TXID:VOUT` | "which UTXO do you want to spend?" — must be one the xpub scan found (`spawn connect`) |
| `--signed-psbt PATH\|-` | "paste the signed PSBT". A named pipe works: the unsigned PSBT is written to `<patp>-spawn.psbt` first (`spawn connect`, also `rekey`) |
| `--assume-saved` | the seed / blind-phrase read-back. **The phrase is printed nowhere else** — a scripted run MUST capture stdout or the comet is unrecoverable (both spawn commands) |

`causeway spawn generate` with no funded UTXO and no `--invite` still waits
indefinitely for funding; there is no timeout.

## Recovery — keep the phrase, not just the file

A comet's `@p` commits to a hiding `dat` whose spawn satpoint sits behind a
32-byte `blind`. The `blind` is what lets you *open* that commitment: without it
you can never prove or re-attest the identity, even holding the wallet seed and
the coins. So Causeway never picks it randomly — it is always derived from a
BIP-39 phrase you hold, plus the funding outpoint:

```
blind_seed = sha256(bip39_seed || "gw/spawn-blind-seed" || txid_be32 || vout_le4)
blind      = H_tag("gw/spawn-blind", minimal_LE_bytes(blind_seed))
d          = H_tag("gw/spawn-commit", jam(spawn-sont) || blind)
dat        = (can 0 (mat %gw-btc) (mat 9) [256 d] ~)
```

| flow | phrase the blind comes from | `blind_derivation` in the proof |
| --- | --- | --- |
| `spawn generate` | the wallet seed phrase it printed | `wallet-seed+outpoint` |
| `spawn connect` | the separate blind recovery phrase | `blind-mnemonic+outpoint` |

Folding the outpoint in means one phrase can safely back several spawns. Each
`proof.json` still records `blind_hex` and `blind_seed_hex` verbatim
(belt-and-braces) plus `blind_derivation`, which names *which* phrase
regenerates them. **The phrase plus the spawn satpoint — which is public,
on-chain, and in the proof — is enough to rebuild `blind`, `d` and `dat` from
nothing.** (`tests/test_causeway.py::test_recovery_drill_from_phrase_and_satpoint_alone`
is that drill.)

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

Other rekey flags: `--fee-rate`, `--network`, `--output-dir`, `--mempool-base`,
`--sponsor`, `--no-route`, and `--signed-psbt PATH|-` for an unattended run.

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

## Inspecting and checking a proof

```bash
causeway proof show <path>              # pretty-print the proof.json
causeway proof verify <path>            # internal consistency + on-chain check
causeway proof verify <path> --offline  # skip the network; consistency only
```

`proof verify` first recomputes the commitment from the proof's own fields
(`verify_proof_self`) and then, unless `--offline` is given (or the proof has
no `commit_txid`), fetches the tx from mempool.space and checks that the
sat-carrying output's `scriptPubKey` on chain equals the one in the proof, and
reports whether it has confirmed. It prints `OK — <reason>` or `FAIL —
<reason>` and exits non-zero on failure, so it works as a gate in a script.
`--mempool-base` points it at a different API.

## Why confidential?

See [docs/CONFIDENTIAL-COMETS.md](docs/CONFIDENTIAL-COMETS.md).
