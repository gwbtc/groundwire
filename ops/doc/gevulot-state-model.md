# Gevulot as the ship's state of record (design, 2026-09-10)

Gevulot replaces `%urb-snapshot`. A comet syncs its sponsor's world through
attestation pushes (each carrying its own on-chain proof) and indexes the
chain forward from a recent height, instead of downloading a monolithic
index or scanning every block since the epoch. That removes one mechanism
and adds several *modes*, so the control pane must make the state of the
system legible in a solid-state way: every setting shows its value, **who
set it** (Causeway at mint, the user, a default), and **when**; and nothing
expensive or irreversible happens without a human choosing it.

Principle: **the system proposes, the user disposes.** When the ship cannot
determine a safe default (no sponsor known, discovery off, no proof), it
parks in a *pending decision* state and says so. It never falls back to the
epoch scan on its own. (On the first real mints Causeway forgot to give
comets their sponsor at boot; an automatic fallback would have committed
every one of them to a day of pinned CPU because of a tooling bug.)

## 1. The two syncs, named apart

| | Light client (`%bitcoin-client`) | Public index (`%gw-btc` scanner) |
|---|---|---|
| What it downloads | Block headers + BIP-158 compact filter headers/filters | Full blocks |
| What it is for | Verifying *specific* identities: targeted tx fetches, and the liveness walk over filters ("has this sat moved?") | Discovering publications by names you do not know yet, and following known names' moves without contact |
| Cost | ~1.5 h from genesis once; seconds per new block | Tens of seconds of Hoon per block; **~a day from the epoch**; cheap when following the tip |
| Pane row | **light client**: `synced — block N` / `not synced — block N` / `waiting for peers` | **public index**: see §2 |

The pane must never collapse these into one word. "Synced" is the light
client; the index has its own row, its own start block, and its own cursor.

## 2. Public index: a small state machine

`%gw-btc` persists `index-origin`:

```
[mode=?(%unstarted %pending %from-spawn %from-height %from-epoch)
 start=@ud
 decided-by=?(%causeway %user %default)
 at=@da]
```

plus the existing cursor (`num.block-id.urb-state`).

- `%unstarted` — no decision yet and no sponsor/proof to derive one from.
  The pane shows a **pending decision** card (below), not a spinner.
- `%from-spawn` (the default the system *proposes*) — start at the comet's
  own spawn height, which Causeway knows and writes into the proof; history
  before it arrives as sponsor pushes. Chosen automatically only when the
  proof names a sponsor **and** peer discovery is on **and** the sponsor's
  attestation was ingested — i.e. when the pushes that make it complete are
  actually going to arrive. Otherwise it is proposed, not applied.
- `%from-height` — a user-chosen start (operator recovery).
- `%from-epoch` — the full scan from block 963,104. Roots (a comet with no
  sponsor, e.g. `~barmul`) get this proposed; sponsees only ever get it by
  pressing the scary button.

Pane row, always present:

```
public index   from-spawn since block 966.012 (set by Causeway at mint, 2026-09-10 00:41)
               scanning block 966.301 of 966.415 — 114 to go
```
or `complete at block N; M identities indexed`, or **`pending: no sponsor
attestation was installed at boot — choose: [index from my spawn] [index
from the epoch] [do nothing yet]`**.

## 3. Sponsor & discovery: value + provenance

| row | shows |
|---|---|
| sponsor | the @p committed in our pass (from the custody log), or "none committed" |
| sponsor reachable | `installed (Causeway, at boot)` / `installed (pasted by you, time)` / **`not installed — ames cannot reach it`** with the paste box right there |
| peer discovery (receive) | `ON (default; set by Causeway at mint)` / `ON (you)` / `OFF (you)` — the toggle stays |
| broadcast (distribute) | `announced to ~sponsor at time` / `not announced` — button stays |
| distribution service (serving) | as today |

"Sponsorship is ON by default" is stated in the row, not implied.

## 4. Verification: queue, progress, memory

Per installed peer: `verified on chain` / `verifying now — liveness walk
block A → B (N filters, clean through H)` / `queued behind ~x` / `trusted —
awaiting sync` / `unconfirmed (3 tries)`. "Clean through H" is the new
persisted per-ship `liveness-clean-to`: the walk resumes from it, so every
attempt makes durable progress and a re-check costs blocks-since-last-check.
The index scanner yields while a verification holds the slot.

## 5. Actions — all manual, each with its consequence spelled out

- **Install a peer / the sponsor by hand** (paste) — as today.
- **Ask my sponsor to broadcast me** — as today.
- **Index from my spawn** / **Index from block N** — only from `%pending`.
- **Re-index from the epoch (slow)** — the scary button. Confirmation text
  states exactly what it does: *rewinds the scanner to block 963,104 and
  re-reads every block (about a day at full CPU). Keeps every verified
  identity, every trusted install, and everything in Jael. Does not touch
  your keys.* Implemented as a cursor rewind; the wholesale-replace poke
  (`%gw-index-from` as it exists today) is not reachable from the pane.
- **Re-check all on chain now** — as today, but resumes from `clean-to`.

Nothing on the pane, and nothing in `boot.sh`, may trigger the epoch scan,
forget a peer, or replace the index without one of these being pressed.

## 6. How Causeway initializes it, and what re-running does

The spawn proof carries: sponsor @p, `sponsor_pass_hex`, and (new)
`spawn_height`. `boot.sh` pokes, on **every** boot, idempotently:
`%set-receive %.y` (mint only — the opt-in is the user's box in Causeway),
`%ingest-peer <sponsor pass>`, `%distribute`, and (new) `%index-origin
[%from-spawn h %causeway now]` — which `%gw-btc` accepts only while
`%unstarted`/`%pending`; once an index exists the poke is a no-op that logs
"already indexing from …". Re-running `boot.sh` therefore never changes an
index decision; it only refreshes the sponsor install and the announcement.
Each poke's effect shows on the pane with `set by Causeway at <time>`.

If any of the three inputs is missing (a proof without the sponsor pass, as
on the first mints), the index parks in `%pending` and the pane says which
input was missing. The ship still boots, syncs its light client, and serves
its dojo; only discovery and the index wait for a human.

## 7. Roots

A comet with no committed sponsor (`~barmul`) is an index authority: the
pane proposes `%from-epoch` in its pending card, and its "As a sponsor" card
gains: **push my whole public index to sponsees** (default ON), so a
sponsee's `%from-spawn` index is complete, not just social.

## 8. Removal

`%urb-snapshot` (216 lines: publishes `urb-snapshot.jam` over HTTP) is
deleted with this change, together with its `desk.bill` entry and the
`%gw-btc` subscription hook that feeds it.
