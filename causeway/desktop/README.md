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
