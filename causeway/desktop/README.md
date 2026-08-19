# Causeway (Desktop)

CLI + TUI for Groundwire comet spawning and management on the **kelvin-9**
(`%gw-btc`, OP_RETURN) confidential-comets protocol, for air-gapped signing
compatibility.

## What it does

- **Spawn a comet** from an air-gapped environment by signing a single standard P2TR transaction in any Bitcoin wallet (Sparrow, BlueWallet, Passport, Keystone, Coldcard, Ledger, etc).
- **Rekey a comet** (rotate the messaging key, optionally breach) via the same signing path.
- Emit an off-chain **attestation proof** (`comet.proof.json`) alongside the `gw-vere` boot feed.

Confidential = no on-chain payload. The comet's `@p` commits to its `dat`
(the spawn satpoint, in plaintext, plus domain and kelvin), and its per-identity
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
| `causeway publish <proof…>` | declassify: put the comet's whole attestation packet on chain in an OP_RETURN |
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

That seed phrase controls the coins. The comet's identity is the proof file + the feed (see [Custody](#custody-the-identity-bundle)).

## Quickstart (Connect Wallet)

```bash
causeway spawn connect --xpub <YOUR_XPUB>
```

Same flow, but you sign the PSBT externally (scan a UR animated QR into Passport/Keystone, or load the `.psbt` file in Sparrow/BlueWallet) — and **broadcast it from that wallet too**. Causeway already knows the transaction's txid (a segwit txid is fixed before signing) and just watches the chain; it moves on by itself when the network has it. Nothing needs to be pasted back. If your wallet can only sign, paste the signed PSBT or signed transaction (or the path to the file it saved) into the prompt / the TUI's box instead, and Causeway broadcasts it. Either way the proof and the feed are on disk before your wallet is asked to sign, so nothing is lost if the wallet broadcasts and Causeway is closed.

Your wallet never hands over its seed, and Causeway needs no secret from it:
the identity is the proof file + the feed (see [Custody](#custody-the-identity-bundle)).

### Spawn flags

Common to both spawn commands: `--invite` (faucet code), `--fee-rate`
(sat/vB, default 2), `--network main|testnet`, `--output-dir`, `--miner`,
`--mempool-base`, `--publish` (public spawn: add an OP_RETURN publication
output opening the `dat`; default is confidential), `--sponsor` (@p or
mnemonym committed in the initial snapshot), `--fief IP:PORT` (a static
endpoint committed in the initial snapshot) and `--no-route` (deliberately
mint an unroutable, outbound-only comet).

A comet needs a sponsor **or** a fief to be reachable at all; Causeway refuses
to mint one with neither unless you pass `--no-route`, and it refuses *before*
the faucet call, the UTXO scan and the proof-of-work, so a stranded comet never
costs you a mine. A comet that other comets will name as their **sponsor** needs
a fief of its own, because that is how peers reach it — so a sponsor is normally
minted with `--fief`. If you set one, the ship must actually bind that port
(`boot.sh --ames-port`), or the fief is a promise it cannot keep.

`spawn connect` additionally takes `--xpub` (required),
`--utxo` and `--signed-psbt`.

### Custody: the identity bundle

Your comet's identity is **two files**: the proof (`<patp>-spawn.proof.json`
and any rekey proofs after it) and the feed. Back both up like a wallet.

- The **feed** holds the ring — the ship's networking key. Mining is seeded
  from system entropy, so no phrase regenerates it, ever.
- The **proof chain** holds the committed snapshot of every hop — sponsor,
  fief, life, rift. Choices, not derivations; nothing regenerates them.

Both are written 0600. The wallet seed phrase (generate flow) or your own
wallet (connect flow) controls the coins and nothing else: there is no blind
and no second phrase. The spawn satpoint sits in the pass's `dat` in plaintext.

### The TUI

`causeway-tui` (or `~/.groundwire/causeway tui`) is the screen-based flow, and
it is the **default face of `boot.sh --mint`** at a terminal: the installer
launches it with the sponsor/fief/work-dir passed through `CAUSEWAY_*`
environment variables, you complete the spawn in the interface, and when you
quit, the installer picks the proof and feed off disk and carries on with
finalize + boot. `--headless`, `--resume`, `--xpub`, or the absence of a tty
all fall back to the prompt-based CLI flow below.

### Headless / agent use

Every flow runs without a terminal. The contract:

```bash
# mint (non-interactive: fund the printed address out-of-band, or use --invite)
causeway spawn generate --assume-saved \
  --sponsor '~host-ship' --out-feed ./raw.feed --output-dir ./work

# resume a mint that died after funding (phrase from a file, never an argument)
causeway spawn generate --mnemonic-file ./phrase.txt --out-feed ./raw.feed

# once the spawn tx confirms: bake the custody log, write the bootable feed
causeway finalize ./work/<patp>-spawn.proof.json \
  --feed-file ./raw.feed --out-feed ./boot.feed

# boot (feed by FILE — a feed is the ship's private key)
boot.sh --comet '<patp>' --feed-file ./boot.feed
```

Secrets never ride the command line: seed phrases come from files or prompts,
feeds move through 0600 files. `--assume-saved` skips the read-back prompts,
so a script MUST capture stdout — the phrase is printed nowhere else.
`ops/onboard-e2e.sh` exercises this exact sequence against a stub mempool.

### Scripting a spawn

Causeway aborts rather than looping when a prompt cannot be answered, so a
headless run must pre-answer every one of them. Three flags do that:

| flag | replaces the prompt for |
| --- | --- |
| `--utxo TXID:VOUT` | "which UTXO do you want to spend?" — must be one the xpub scan found (`spawn connect`) |
| `--signed-psbt PATH\|-` | "paste the signed PSBT". A named pipe works: the unsigned PSBT is written to `<patp>-spawn.psbt` first (`spawn connect`, also `rekey`) |
| `--assume-saved` | the wallet-seed read-back (`spawn generate`). **The phrase is printed nowhere else** — a scripted run MUST capture stdout or the coins are unrecoverable. No-op on `spawn connect` |

`causeway spawn generate` with no funded UTXO and no `--invite` still waits
indefinitely for funding; there is no timeout.

## `dat`, and why there is nothing to recover

A comet's `@p` commits to `dat = (can 0 (mat %gw-btc) (mat 9) (mat (jam
spawn-sont)) ~)`: the domain, the kelvin, and the spawn satpoint **in
plaintext**. Anyone holding a pass can read the satpoint out of it, and the
verifier requires entry 0's spawn-opening to name the same one.

Until 2026-08-18 the satpoint sat behind a 32-byte blind in a hiding
commitment, and this section was about never losing that blind. The blind was
removed: the pass and the attestation are one object, so every pass-holder held
the opening anyway, and the blind protected nobody while being one more secret
to lose. There is now **no per-identity secret outside the feed** — see
[Custody](#custody-the-identity-bundle).

## Management (rekey)

Rekey is the only on-chain management op in kelvin-9; sponsorship and escape
are off-chain. A rekey spends the point's current sat-carrying output key-path
and commits a new snapshot (life+1, rift+1 on `--breach`, rotated messaging
key), chaining off the point's prior proof:

```bash
causeway rekey --point '~sampel-palnet' --prior-proof ~sampel-palnet-spawn.proof.json \
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
`--sponsor`, `--fief IP:PORT`, `--no-route`, and `--signed-psbt PATH|-` for an
unattended run. `--sponsor` and `--fief` are set-or-carry: pass one to change
it, omit it to keep whatever the prior snapshot committed.

## Finalize — bake the custody log into your boot feed

Once the spawn tx confirms, bake the off-chain custody log (`xtr`) into the feed
so the booted ship's pass carries its own attestation. Entry 0 (the spawn)
additionally names the spawn sat and its funding block via its spawn-opening:

```bash
causeway finalize ~sampel-palnet-spawn.proof.json --feed 0vABC...
# after a rekey, pass every proof for the point, oldest first:
causeway finalize spawn.proof.json rekey.proof.json --feed 0vABC...
```

This records `block_hash`/`block_height`/`xtr_hex` in the proof(s) and
prints an updated boot one-liner. Booting with the miner's original feed
also works — the ship just serves an empty log until an `%anew`
round-trip (or a re-boot with the finalized feed) supplies it.

## Publish — declassify the comet on chain

`publish` takes exactly what `finalize` takes — every proof for the point,
oldest first — because the thing it publishes is the custody log `finalize`
bakes onto the last one. Run `finalize` first.

```bash
causeway publish spawn.proof.json rekey.proof.json --fee-rate 2
# see what would go on chain without signing or broadcasting anything:
causeway publish spawn.proof.json rekey.proof.json --dry-run
# scripted, and topping the identity sat up instead of shrinking it:
causeway publish spawn.proof.json --fund-xpub "$XPUB" --fund-utxo TXID:VOUT \
                 --signed-psbt /tmp/signed.fifo
```

**Publishing is one way.** The packet names the comet, its spawn satpoint and
every custody hop since, in public, forever.

What goes on chain is one transaction, and it is a state update: input 0 spends
the identity sat (only its holder can, so the publication is the *owner's*
consent to declassify), output 0 re-commits the snapshot at `life+1`, and an
OP_RETURN carries the comet's **whole attestation packet** — the pass a peer
would receive over ames, custody log in its `xtr`, plus the opening for the hop
this very transaction performs. That last opening is the one thing the packet
cannot contain, because the transaction's txid does not exist until it is
signed; the watcher completes the log from the block it is reading and runs the
same `+run-checks` a mailed attestation gets. So a **stranger** can verify it,
and a comet can publish **late**.

Four things it refuses to do, all before any fee is paid:

* publish a log that does not **end** at the outpoint input 0 spends — the
  artifact would be short by the hops in between and the packet would fail
  `N-continuity` on chain;
* put a spawn-opening on the terminal opening — it may sit on entry 0 only
  (`spawn-opening-zero`), and entry 0 is inside the `xtr`;
* emit a payload over `MAX_PUBLICATION` (1024 bytes, ~17 hops);
* broadcast a payload whose pass does **not** carry the log. That last check
  re-reads the payload back out of the script itself, before signing and again
  after, because a boot-pass publication and a packet publication are
  indistinguishable in a transaction decode — same envelope, same opening,
  ~200 bytes shorter — and the difference is only whether anyone can verify it.

A packet publication runs ~400 vB, so ~1,000 sats at 2 sat/vB. An identity sat
may not cover that; `--fund-xpub` adds a funding input **after** input 0 (sats
are assigned to outputs in input order, so an input behind the identity cannot
move it) and the identity output is topped up rather than shrunk.

Once it confirms, run `finalize` again with the publish proof appended: a
publication is a custody move like any other, and the next hop's log must
carry it.

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
