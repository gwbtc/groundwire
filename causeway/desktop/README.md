# Causeway (Desktop)

CLI + TUI for Groundwire comet spawning and management on the **kelvin-9**
(`%gw-btc`, OP_RETURN) confidential-comets protocol.

Causeway **generates and holds the wallet itself** and signs every transaction
in-process. External wallets are not part of the signing path: a comet's
sat-carrying output commits state in a taproot key-path tweak, and no consumer
or hardware wallet will sign that (only Bitcoin Core and Keystone honour the
merkle-root PSBT field, and neither is worth a dependency) — so the seed phrase
Causeway prints at spawn is the one and only custody of the identity sat.
**Write it down.** External wallets are still fine for *funding* — you send BTC
to the address Causeway shows you — they just never sign.

## What it does

- **Spawn a comet**: Causeway mines it, generates a BIP-39 wallet, shows you an
  address to fund, then signs + broadcasts a single standard P2TR transaction
  with the generated seed.
- **Rekey a comet** (rotate the messaging key, optionally breach) — signed
  in-process with the same seed phrase (`--mnemonic-file`, or a hidden prompt).
- Emit an off-chain **attestation proof** (`comet.proof.json`) alongside the
  `gw-vere` boot feed.

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
| `causeway spawn generate` | mint a wallet, fund it (or spend an invite), spawn a comet, print the boot one-liner |
| `causeway invite create` / `invite show <code>` | prepay someone else's spawn as one hex code; see what a code holds |
| `causeway rekey` | rotate the messaging key on an existing point (the only on-chain management op) |
| `causeway finalize <proof…>` | bake the `xtr` custody log into the proofs and, with `--feed`, into the boot feed |
| `causeway publish <proof…>` | declassify: put the comet's whole attestation packet on chain in an OP_RETURN |
| `causeway proof show <path>` | pretty-print a `proof.json` |
| `causeway proof verify <path>` | check a `proof.json` for internal consistency and (by default) against its on-chain tx |
| `causeway-tui` | Textual terminal UI over the same flows |

There is **no `causeway mine`** — mining happens inside `spawn generate`,
via the external `comet_miner` binary (see `--miner`).

`--miner` defaults to `./comet-miner/zig-out/<zig-target-triple>/comet_miner`,
resolved by probing the triples zig actually emits: `aarch64-macos-none` /
`x86_64-macos-none` on macOS, and `x86_64-linux-musl` (then `…-linux-gnu`) on
Linux, because `comet-miner/build.zig` rewrites a native Linux build to musl.
If neither exists, Causeway names both paths it looked at. Pass `--miner`
explicitly to override.

## Quickstart (Generate New Wallet)

```bash
causeway spawn generate
```

Prints a fresh 12-word seed phrase, derives a P2TR address, waits for you to
fund it and for that to confirm, mines a comet, signs + broadcasts a single
spawn tx (confidential by default; pass `--publish` for a public on-chain
publication), then prints the boot command.

That seed phrase controls the coins. The comet's identity is the proof file + the feed (see [Custody](#custody-the-identity-bundle)).

### Invites: paying for someone else's spawn

One person funds, another spawns, and the funder never holds the comet.

```bash
# the inviter
causeway invite create
#   -> an address to send sats to, how many (priced at today's fees), and
#      a 74-character hex INVITE CODE; then it waits for the funding to confirm

# the invitee
causeway spawn generate --invite <CODE>
```

The code is 32 random bytes with a version byte and a checksum. Those bytes
seed a BIP-32 wallet (raw seed, the way `%spv-wallet` treats a `%q` seed)
whose first BIP-86 address the inviter funds, so **the code is the sats:
anyone holding it can spend them**. Pass it over a channel you would pass
money over. The invitee's Causeway derives the same wallet, spends that UTXO
as the spawn's input and signs the input with the invite key — but the
identity sat it creates is tweaked from the **invitee's** own wallet key
(recorded in the proof as `sat_key`, which the next `rekey` derives), and
any change goes to the invitee's wallet at `m/86'/0'/0'/1/0`. Import the
same 12 words into the ship's Wallet app (`%spv-wallet`, a BIP-39 `%t`
seed) and the change shows up there. The inviter is left with nothing: not
the identity, not the remainder.

`causeway invite show <CODE>` reports what an invite's address holds.
`invite create` prints the code *before* it waits for funding, because a run
that dies while waiting must not take the only key to the sats with it.

### Fees

Every transaction Causeway builds is a P2TR key-path spend — SegWit v1 — so
fees are sat per **virtual** byte: a spawn is ~111 vB with one output and
~154 vB with change. By default the rate is the mempool's next-block
estimate (`/v1/fees/recommended`, `fastestFee`) plus 1 sat/vB, never below
2; `--fee-rate` overrides it. `invite create` quotes two amounts from the
same estimate: the minimum for a spawn today, and a suggestion with fee
headroom for the days an invite may sit unused, since whatever the fee does
not consume reaches the invitee as change. (The old flow priced everything
at a flat 1000 sats, which at a busy mempool meant hours in the queue.)

### Spawn flags

Common to both spawn commands: `--invite` (an invite code; anything else is
treated as a legacy faucet code), `--fee-rate` (sat/vB; default = next block
+ 1, see [Fees](#fees)), `--network main|testnet`, `--output-dir`, `--miner`,
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

### Custody: the identity bundle

Your comet's identity is **two files**: the proof (`<patp>-spawn.proof.json`
and any rekey proofs after it) and the feed. Back both up like a wallet.

- The **feed** holds the ring — the ship's networking key. Mining is seeded
  from system entropy, so no phrase regenerates it, ever.
- The **proof chain** holds the committed snapshot of every hop — sponsor,
  fief, life, rift. Choices, not derivations; nothing regenerates them.

Both are written 0600. The wallet seed phrase controls the coins and nothing
else: there is no blind and no second phrase. The spawn satpoint sits in the pass's `dat` in plaintext.

### The TUI

`causeway-tui` (or `~/.groundwire/causeway tui`) is the screen-based flow, and
it is the **default face of `boot.sh --mint`** at a terminal: the installer
launches it with the sponsor/fief/work-dir passed through `CAUSEWAY_*`
environment variables, you complete the spawn in the interface, and when you
quit, the installer picks the proof and feed off disk and carries on with
finalize + boot. `--headless`, `--resume`, or the absence of a tty
all fall back to the prompt-based CLI flow below.

### Headless / agent use

Every flow runs without a terminal. The contract:

```bash
# mint (non-interactive: fund the printed address out-of-band, or spend an
# invite code someone made with `causeway invite create`)
causeway spawn generate --assume-saved --invite <CODE> \
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
| `--mnemonic-file PATH` | the wallet-seed prompt (`rekey`, `publish`; also resumes `spawn generate`). The file holds the BIP-39 phrase; a phrase on a command line lands in shell history |
| `--assume-saved` | the wallet-seed read-back (`spawn generate`). **The phrase is printed nowhere else** — a scripted run MUST capture stdout or the coins are unrecoverable |

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
`--sponsor`, `--fief IP:PORT`, `--no-route`, `--mnemonic-file PATH` for an
unattended run, and `--top-up` to add a funding input from the same wallet so
the identity output grows instead of shrinking by the fee. `--sponsor` and `--fief` are set-or-carry: pass one to change
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
causeway publish spawn.proof.json --top-up --fund-utxo TXID:VOUT \
                 --mnemonic-file ~/comet-seed.txt
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
may not cover that; `--top-up` adds a funding input **after** input 0 (sats
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
