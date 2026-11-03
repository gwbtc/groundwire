# Causeway

Bring-your-own-Bitcoin identity bridge for [Groundwire](https://groundwire.io)
comets. Causeway is the equivalent of Urbit's Azimuth bridge, but for comets
whose identity lives on Bitcoin rather than Ethereum.

No backend, no custody, no accounts. Every on-chain operation is a single
standard P2TR transaction that you sign in whatever wallet you already trust —
Sparrow, BlueWallet, Passport, Keystone, Coldcard, Ledger — via a PSBT that
Causeway hands you and never signs itself.

## Layout

| path | what it is |
| --- | --- |
| `desktop/` | **the front end** — CLI (`causeway`) and TUI (`causeway-tui`). See [`desktop/README.md`](desktop/README.md) for the full command surface. |
| `public/boot.sh` | the installer a minted comet's owner runs; served at `https://groundwire.io/causeway/boot.sh` |

There is no web app. One lived at `causeway/src/` and was removed — see
"Why there is no web app" below.

## What it does

- **Spawn** a new Groundwire comet: mine a suite-C networking key, build a
  single commit-only PSBT for you to sign, and hand you a one-line shell
  command to boot the comet afterwards. The attestation is committed on-chain
  but never revealed; the custody log is baked into the boot feed off-chain.
- **Rekey** an existing comet — rotate its networking key, optionally
  breaching. **That is the only on-chain management op.** Under the kelvin-9
  OP_RETURN revision the other sotx opcodes — `escape`, `cancel-escape`,
  `adopt`, `reject`, `detach`, `set-mang` — left the chain entirely;
  sponsorship and escape are off-chain (spec §8), and a static endpoint
  (`fief`) is a field of the committed snapshot rather than an operation.

Groundwire IDs are shown as **mnemonyms** — a BIP-39-style word rendering of
the comet's 128-bit @p (e.g. `.routine.inhale.regimes.…`) — rather than the
scrambled `~mosnyt-londen-…` @p. The encoder and wordlist are vendored from
[gwbtc/mnemonyms](https://github.com/gwbtc/mnemonyms). The @p remains the
machine identity (boot `--comet`, pier name, `proof.json`); input fields accept
either form.

## Every operation

1. Causeway fetches the point snapshot and resolves your point.
2. It filters the sat-carrying UTXO out of fee coin selection, so you cannot
   accidentally burn your own @p by paying fees with it.
3. It builds a **single** PSBT. There is no commit+reveal pair: the new
   snapshot is committed in the **taproot tweak** of the sat-carrying output,
   so on-chain it is a plain P2TR key-path spend and nothing is revealed.
   (`--publish` additionally carries an OP_RETURN publication output that
   opens the commitment deliberately.)
4. You sign it in your own wallet.
5. Causeway broadcasts via mempool.space. Peers verify from the off-chain `xtr`
   custody log, baked into the boot feed once the transaction confirms.

## Why there is no web app

A browser SPA (`causeway/src/`, ~7,500 lines with its tests) implemented the
same protocol in TypeScript and was kept at parity with the desktop front end.
It was deleted, deliberately.

The protocol has one wire format, and every front end that speaks it is another
implementation of the same encoders that must agree **byte for byte** — with
each other and with the Hoon. Three implementations meant every protocol change
landed three times, and the third one is where the expensive bugs lived: the
SPA baked `fief: null` into its `Snapshot` type, so a web rekey of any
fief-carrying comet built an unsignable PSBT; and it copied
`point.net.sponsor.who` unconditionally, turning Jael's projection of *no
sponsor* back into a real one and defeating the routability guard outright.
Neither had a counterpart in the Python.

Nothing depended on it. The CLI prints the boot one-liner, and `boot.sh` is a
static file that needs no application to host it.

If a browser front end is ever wanted again, the thing to reach for is a
**thin** one over the desktop's encoders, not a second full implementation of
the protocol.

## Protocol status (kelvin-9)

The confidential-comets spec moved the attestation into the pass itself and, in
the OP_RETURN revision, moved on-chain state into the taproot tweak. The comet
mines against the kelvin-9 `dat` (`can 0 (mat %gw-btc) (mat 9) [256 d]`),
builds one taproot transaction whose sat-carrying output key commits the
initial snapshot, and `causeway finalize` bakes the off-chain custody log into
the boot feed once it confirms. The legacy v9 `(rap 3 ~[%9 ~tyr …])` tweak and
the on-chain commit/reveal pair are both retired.

`dat` and `xtr` are pinned to golden vectors in `desktop/tests/test_causeway.py`,
generated from `urbit eval` and shared with the Hoon at
`groundwire/vectors/gw-kelvin-9.json`. **When an implementation and the vectors
disagree, the vectors win.**

See [`desktop/docs/CONFIDENTIAL-COMETS.md`](desktop/docs/CONFIDENTIAL-COMETS.md)
for the protocol note and compatibility matrix, and
[`../ops/doc/OPERATIONS.md`](../ops/doc/OPERATIONS.md) for the end-to-end
runbook.
