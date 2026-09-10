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

## 9. Invariants: Jael, `urb-state`, and Gevulot must not drift

There are three stores, and "urb-state and Jael becoming disjoint" is the
bug this design must make impossible, not merely unlikely:

| store | is the truth about | may be written by |
|---|---|---|
| **Jael** (`pos.zim.pki`) | which keys this ship will talk to, and at what life | `%verdict` facts from `%gw-btc` (add/update); the forget path (`%stale-notice` today, `%snob-notice` under gwbtc/urbit#72); trusted installs from Gevulot (`%gw-trusted-peer`) |
| **`%gw-btc` `urb-state`** | what the chain says: verified points (`unv-ids`), the sat index, the scan cursor | the scanner (batches are all-or-nothing) and `+apply-verified` — **never a poke that replaces it** |
| **Gevulot** | *intent and provenance only*: receive/serving, the broadcast roster (passes it must relay), and per-ship annotations (how a peer was learned, when, tries) | the user and Causeway |

Rules:

1. **Gevulot keeps no peer list of its own.** The "installed peers" section
   is *derived* on every render from Jael's known set and `%gw-btc`'s
   verified set; Gevulot's map only annotates ships that appear there.
   (Today's `installed` map is a third copy and is removed.)
2. **Every identity is classified, and every class is shown:**
   `trusted` = in Jael, not in `urb-state` (a Gevulot install awaiting
   verification — the one *intended*, temporary disjointness, and it says
   so); `verified` = in both; `indexed-only` = in `urb-state`, not in Jael
   (should not persist: a verdict is emitted in the same event that indexes
   a point; if the pane ever shows one, that is a bug to report, and the
   pane says so).
3. **No single-store mutation is reachable from the pane or from
   `boot.sh`.** "Forget" becomes one operation that removes the annotation,
   runs `+forget-points`, and sends the forget to Jael — or it is not
   offered. `%gw-index-from` (wholesale replace) is not reachable; re-index
   is a cursor rewind that keeps `unv-ids`, so the sponsor-existence set
   and Jael stay aligned throughout.
4. **Crash safety is Arvo's, and we rely on it:** a scan batch, its
   `urb-state` change, and the `%verdict` cards it emits are one event;
   Jael's application of those cards is later events in the same log.
   Replay reproduces the same sequence. No store is updated outside an
   event.
5. **A self-check the pane can run** (and shows in the debug card): count
   the three classes and any `indexed-only` identity, so drift is visible
   the moment it exists rather than discovered a day later in a log.
