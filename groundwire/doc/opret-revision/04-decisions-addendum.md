# OP_RETURN revision — decisions addendum

Status: adopted 2026-08-03. This document records the architect decisions
resolving the open questions of `01-spec-revision.md` §9 and fixing the scope
of the first implementation ("v9" below). It amends `01-spec-revision.md`
where they differ; everything not mentioned here stands as written there.

The guiding scope rule: rely on economic sybil resistance (on-chain cost)
rather than mechanical DOS resistance. Urbit as a whole is not DOS-resistant;
piecemeal hardening here buys little and costs complexity. A kernel-wide DOS
pass is a separate, later effort.

## 1. Protocol version: a Kelvin number in `dat`

The domain `dat` carries an explicit protocol version, Kelvin-style
(counting down), starting at **9**:

```
dat = (can 0 (mat %gw-btc) (mat 9) [256 d] ~)
d   = H_tag("gw/spawn-commit", (jam spawn-sont) || blind)
```

- The domain tag remains the first `+mat` item at bit 0 — Ames reads only
  that and nothing else changes in the kernel.
- The Kelvin is plaintext (not inside the hiding commitment) so any holder
  of a pass can read a comet's mint version without an opening.
- `dat` is hashed into the name, so a comet's mint Kelvin is immutable.
  A Kelvin decrement changes what new comets mint under; verifiers advertise
  the set of Kelvins they accept, and retiring a Kelvin is a deliberate
  (breach-class) network decision.
- One number governs interpretation of everything downstream for that comet:
  snapshot mold, xtr grammar, tag-string semantics, publication payload.
  Consequently (resolving §9 Q1): the tagged-hash strings
  (`gw/spawn-commit`, `gw/state-commit`, `gw/spawn-blind`) are themselves
  **unversioned**; the OP_RETURN publication envelope's version byte **is
  the Kelvin** (`0x09`). No separate registry is needed.
- Decoders MUST reject trailing data: the bit-width of `dat` is exactly
  `p:(mat %gw-btc) + p:(mat 9) + 256`.
- **A pass at a Kelvin we do not implement gets SILENCE, never a negative
  verdict.** A negative verdict is a Jael `%fail`, which Ames turns into a
  *snub*. If a verifier condemned foreign Kelvins, then across a Kelvin
  bump every old ship would snub every new one and vice versa — a network
  partition on upgrade, produced by the very mechanism meant to make
  versioning orderly. Retiring a Kelvin is a deliberate breach-class
  decision (above); it is not something a verifier does by accident to
  every peer it cannot parse. Treated exactly like the `xtr = 0`
  public-onboarding packet: no verdict at all.

## 2. Snapshot mold (resolves §9 Q2)

On-chain committed snapshots carry sponsorship and routing state:

```
+$  snapshot
  $:  life=@ud
      rift=@ud
      key=@                    ::  current messaging key (cry.pub)
      sponsor=(unit @p)
      fief=(unit fief)
  ==
```

- `rift` stays explicit on-chain (not derived from key discontinuity).
  Revisit removal only once the codebase is settled.
- **Every snapshot change increments `life`; a `rift` increment implies a
  `life` increment.** This makes life comparison a total order over a ship's
  states, so the kernel's existing life-based intake triage is sufficient
  and no same-life re-verification path is needed.
- Sponsor semantics at verification: a named sponsor must exist as a public
  point (existence check only — **no consent signature in v9**; consent
  material, sponsor-chain evidence, multi-sponsor fallback, and all other
  off-chain sponsorship coordination are one deferred workstream). An absent
  sponsor projects to self-sponsorship in the Jael udiff, and is never
  treated as fraud.
- **Sponsorship flow (v9, adopted 2026-08-04).** Requesting sponsorship IS
  attesting to the sponsor: a comet that commits `sponsor=S` in its
  snapshot connects to S and self-attests like any peer. The moment S's
  verifier sees a VALID attestation whose snapshot names S itself as
  sponsor is the accept/reject decision point: on accept (the v9 default —
  a policy hook with no rejection mechanism yet) S records the sponsee and
  forwards for it; a future rejection is expressed by declining the
  sponsorship role, NEVER by a `%fail` verdict or snub (the attestation
  itself is valid). Peers route to a confidential comet via the sponsor
  committed in its verified snapshot; a failed/unconsented sponsorship is
  self-announcing (the sponsor does not forward). The old on-chain
  escape/adopt handshake and the sponsor-signer desk are retired.
- **A comet with neither `sponsor` nor `fief` is a one-way identity — clients
  must refuse to mint one.** An absent sponsor projects to *self* in
  `+urb-point-to-jael`, so nothing can route to such a comet: it can still
  initiate outbound and answer on a reply lane, but once a peer drops its
  state (any `%stale`, any restart) it can never be re-contacted. Confirmed
  live — such a comet received zero attestations across a full test run.
  Deliberately NOT enforced in the `snapshot` mold or as a verifier validity
  rule: `(each fief sponsor)` would forbid holding BOTH, which is legitimate,
  and rejecting such snapshots would turn a self-harming choice into a
  consensus rule while breaking comets that are legitimately outbound-only.
  Causeway refuses by default with an explicit opt-out; `%gw-btc` slogs a
  warning so hand-rolled transactions stay visible.
- **Self-rescue.** An unreachable comet is not permanently lost, because its
  owner can still act on-chain (spending the sat needs a wallet, not a
  reachable ship). Two paths, with a real trade-off: **pairwise** — the comet
  initiates to each peer and hands over its attestation, preserving
  confidentiality but requiring a personal introduction to every future peer;
  or **publication** — one state update that adds a route AND carries an
  OP_RETURN, after which every scanner learns the route from chain alone,
  at the permanent cost of confidentiality. So: *an unreachable confidential
  comet can always rescue itself, but only by sacrificing either scalability
  or confidentiality.*

  > **⚠️ CONTRADICTED ON MAINNET, 2026-08-05 — neither path worked.**
  > See `doc/live-tests/PHASE5B-RESULTS.md` findings 4 and 10, and §2b below
  > for the adopted resolution.
  > *Publication*: a correctly formed, fully gated publication (tx
  > `0cca2561…`, block 961 217) was indexed by nobody. **Partly fixed**
  > (§2b): a scanner that already tracks the comet now accepts it and
  > declassifies. A scanner that has never seen the comet still cannot, and
  > that half is specified-but-unimplemented.
  > *Pairwise*: still unimplemented. Ames has no unsolicited self-attestation
  > push — all three `+attestation-packet` call sites are reactive, and the
  > only broadcast one (`+sy-priv`'s rekey blast) iterates peers that are
  > **already** `%known`.
  > The attempt was also actively harmful: moving the sat made the ship's
  > un-refreshed attestation fail `tip-unspent` at its peers, which produced a
  > negative verdict rather than `%stale`, so the comet got **snubbed** by its
  > own sponsor. **Fixed** — see §3.

### 2b. The confidential → public transition (adopted 2026-08-06)

What actually failed on mainnet is narrower, and different, from what the
Phase-5b report concluded, so the record is corrected first.

`+apply-state:urb-core`'s `unv-ids` guard is a barrier only for a **stranger**
scanner. `unv-ids` holds confidential points too — `+apply-verified` puts one
there on every positive verdict, which is exactly how `+detect-stale` follows
a confidential comet's sat. C1 and C3 had both verified C2, so for them
`+apply-state` found the point, input 0 spent the tip they were tracking, the
`state-key` commitment matched, and life advanced 1 → 2. Every *loud* failure
in that arm slogs, and no `%urb-core:` line appears in C1's log at block
961 217. The publication was almost certainly **accepted**.

It was then lost twice over, in `app/gw-btc.hoon`:

- the ship was still in `.confidential`, so `+filtered-udiffs` dropped every
  udiff `+index-point` had just produced;
- and the publication's own sat move then looked exactly like staleness to
  `+detect-stale`, which dropped the point altogether and told jael.

So the missing piece was never the guard. It was that **nothing could ever
move a comet out of `.confidential`.** `+public-spawns` — the only exit — keys
off an `%owner` effect, which only a *spawn* emits.

**Adopted design, in two tiers.**

**Tier 1 — continuity publication (implemented).** A publication whose subject
the scanner already tracks is a **state update**, whatever the shape of its
opening. Input-0 continuity from the sat we already follow is the ownership
proof; a `blind-opening`, when present, adds nothing to it and is checked only
for consistency against `dat`. `+index-point` now emits `[%point who %public ~]`
for every accepted publication, and `%gw-btc`'s `+published-comets` uses it to
move the ship out of `.confidential`. Only the holder of the tracked identity
sat can build such a transaction, so the publication *is* the owner's consent
to declassify — irreversible, and slogged as such.

This delivers the rescue for every peer that has ever verified the comet, which
is the population §2's self-rescue was written for: a comet becomes unreachable
precisely because peers that *did* know it dropped its point.

**Tier 2 — publication as an on-chain self-attestation (specified, NOT
implemented).** A stranger scanner still cannot admit a state-update
publication, and must not: the `pass`↔`dat` binding names the **spawn**
satpoint, and nothing in a single publication connects that satpoint to the one
this transaction spends. Walking that custody chain is the confidential
verifier's job, not a publication's.

The resolution is to stop inventing a second proof system and reuse the first:
**a publication that must convince a stranger carries the comet's custody log
in the published pass's own `xtr`** (`+with-xtr:gw-btc-pass` already builds
exactly that pass; the payload type does not change), and the scanner hands it
to the same `+verify-lc` walk a packet attestation goes through. A publication
then *is* a self-attestation, published on chain instead of mailed.

Consequences to pin when this lands:

- Cost is bounded by evidence the publisher paid for on chain, and is *lower*
  than a packet attestation's: the scanner is reading the block that contains
  the tip transaction, so the BIP-158 tip scan — the dominant cost (§9) — is
  ~zero blocks wide.
- The 512-byte payload cap (§4) bounds the log: pass ≈ 108 B plus opening
  ≈ 120 B leaves room for roughly seven `[txid height]` entries. A comet that
  has moved its sat more often than that cannot publish, and that is a real
  limitation, stated rather than discovered.
- §4b needs one refinement. `blind-opening.start-height` is unfillable only
  when the publication rides the spawn transaction itself. A **late**
  publication can and MUST carry a true `start-height`: the funding
  transaction is no longer the parent of the transaction in hand, so the
  scanner cannot recover it from the block it is reading.
- Work must be launched asynchronously (a new `effect:urb` the agent turns
  into a `+verify-lc` job), never inline in the block thread, and must reuse
  the single-flight `inflight` slot so one publication per ship per batch is
  the most anyone can buy.

**Rejected alternatives.**

- *Scanner discovers the chain itself*, filter-scanning forward from the spawn
  satpoint hop by hop. It needs no new payload, and that is its whole appeal.
  But the work is O(hops × blocks-since-mint), unbounded in dormancy, and it
  is triggered by a **broadcast**: for one ~345-sat transaction anyone can put
  a publication with a plausible blind-opening in a block and force every
  scanner on the network into a multi-minute walk. It also cannot live where
  it would have to — the block scanner is a forward-only streaming thread with
  no way to launch or await per-comet historical work.
- *Publication as a routing hint, confirmed by a later attestation.* Does not
  deliver "from chain alone" — it is the pairwise path with an extra
  unconfirmed-hint state to carry, and pairwise has no push primitive anyway.

**Both silent refusals now slog**, along with every other early return in
`+apply-spawn` and `+apply-state`. Their silence is why this cost a mainnet
transaction to find instead of a log line, and it is the direct answer to
test 6.7: *no*, it was not diagnosable from logs alone. `%gw-btc` also gained
`/x/inflight`, `/x/publicizing`, `/x/confidential` and `/x/attested` scries for
the same reason.
- **Fief scope.** A fief is for ships with STATIC addresses — sponsors and
  other infrastructure (its `%turf` form is literally DNS). A reliable
  sponsor should commit a fief on-chain. Roaming/confidential comets set
  `fief=~` and are reached via their sponsor plus dynamically learned
  lanes; their transient addresses never touch the chain. A comet with
  neither sponsor nor fief is not cold-reachable (reply-lane peers only) —
  legal, but clients should warn.

### 2a. Unroutable snapshots: how the refusal is implemented

This is the mechanics of the one-way-identity bullet above; the decision
itself (refuse at mint time, never in the mold and never as a verifier
validity rule) is stated there.

- **Causeway refuses by default**, in all three front ends —
  `causeway/desktop/causeway.py` (`spawn connect`, `spawn generate`,
  `rekey`), `causeway/desktop/causeway_tui.py`, and the web SPA
  (`causeway/src/spawn/assemble.ts`, `causeway/src/ops/_common.ts`). Pass
  `--sponsor <@p|mnemonym>` to name one; pass `--no-route` (a checkbox in
  the TUI and the SPA) to mint an unroutable identity **on purpose**. The
  refusal fires *before* any faucet call, UTXO scan, or proof-of-work, so
  an operator never burns a mine on a stranded comet. A state update
  (rekey) that would *drop* the last sponsor is refused on the same rule.

- **`%gw-btc` only warns.** It slogs when it verifies a confidential comet
  or indexes a public spawn whose committed state has neither
  (`+unroutable-point` / `+unroutable-points` in `app/gw-btc.hoon`,
  `+routable` in `lib/self-attestation.hoon`). It **must never** become a
  validity rule: a negative verdict is a Jael `%fail`, which Ames turns
  into a *snub*, and snubbing an honest ship for an allowed-by-spec
  snapshot would be a self-inflicted partition. The warning is how a
  hand-rolled transaction that never touched Causeway stays visible.

One live bug fell out of writing this down: the web rekey page copied
`point.net.sponsor.who` unconditionally, which turns the Jael *self*
projection back into a real sponsor and would have made every unrouted
comet look routed. It now honours `.has`.

## 3. Stale attestations: demote to alien, never snub

When `%gw-btc` observes a confidential comet's tip outpoint spent (conf
registry), the identity's attestation is stale. This is not fraud and MUST
NOT produce `%fail` (a snub would block the replacement packet).

**This applies to the PACKET path identically** (adopted 2026-08-06). The same
physical fact reaches a verifier two ways: the scanner watches a tracked tip
get spent, or the owner moves the sat and the ship keeps sending the pass it
booted with, so the spent tip arrives in a packet. In the second case the
spentness is still established by *our own* filter scan against the chain, not
by anything the peer claimed — it is the same fact, discovered by the inbox
instead of the scanner, and it MUST take the same route. It did not: `%gw-btc`
emitted a negative `%verdict`, and on mainnet 2026-08-05 that snubbed an
honest comet permanently, from its own sponsor, for a correctly formed state
update — with the snub then blocking the replacement packet exactly as this
section predicts.

`+stale-verdict:self-attestation` is the discriminator. A failed verdict is
staleness, and routes to `%stale`, only when **every** failing check is one of

| check | meaning |
|---|---|
| `tip-unspent` | our filter scan positively PROVED the log's tip outpoint spent. A scan that could not be evaluated at all is `tip-scanned`, below — a different class |
| `tracked-tip` | our own tracker holds this comet's sat somewhere this log never reaches |
| `life-monotonic` | the log's latest life is below one we already hold: an older copy |
| `tracked-lag` | this log is a hop-for-hop **prefix** of the one we already verified for this comet — the same log with its last entries missing, not a different one. Added 2026-08-07; see the staleness-by-degree rule below |

A third class sits beside staleness: checks the verifier could not EVALUATE.
`+unknown-verdict:self-attestation` is its discriminator, and a failing verdict
in this class emits **no verdict at all** — not `%fail`, not `%stale`, silence.
Ignorance is not evidence, and a negative verdict here would snub the peer over
a gap in our own knowledge, blocking the very packet that would close it.

| check | meaning |
|---|---|
| `sponsor-known` | the snapshot names a sponsor we cannot see as a public point. Our public index is a *window* on the chain — it begins at an operator-chosen height and ends wherever our scanner has reached — so "not in it" never distinguishes *no such comet* from *we have not looked there yet*. A verifier that has not indexed the sponsor cannot judge, and must not |
| `tip-scanned` | the BIP-158 liveness scan could not be evaluated: an unavailable filter or block, an inconsistent answer, or a degenerate (empty) scan range. `tip-unspent` passes vacuously in that case, which is not a fail-open — the verdict is already not `ok`, so no point is installed; all it decides is which of the two non-fraud outcomes we take |

Everything else stays `%fail`: `spawn-commit` (the log is not bound to this
name), `entry-N-commitment` (a snapshot never committed on chain),
`entry-N-continuity`/`-key-path`/`-sat-landed`/`-txid` (a custody hop that did
not happen), `entry-N-life-order` (a log that contradicts itself),
`tracked-prefix` (a log we cannot reconcile with the one we already verified —
a fork, or a lag too big to be our own scanner), `pass-key`, and every
structural check.

The classes are ordered, not summed: a verdict is `%fail` if **any** failing
check is fraud-class, otherwise `%unknown` if any is unknown-class, otherwise
`%stale`. Fraud beats everything — a peer does not get to launder bad evidence
by also being out of date, or by being unknowable. Unknown then beats stale:
`%stale` is a positive finding about the peer's evidence, and we are not
entitled to make it while some of our own machinery came back blank.

A failing check name is not the only thing that has to be classified: a
verification can end before it runs a single check (§3a), and one that passed
every check can still be declined (§3b). Both resolve into the same three
classes.

**Staleness is forgiven by degree** (owner decision, 2026-08-07; implemented in
`cdaf7cf`). `tracked-prefix` used to answer "does this log reconcile with ours"
with a loobean, and so gave the same `%.n` to a log that *diverges* from ours
and to one that *is* ours with its last entries missing. Fraud and staleness,
reported as one bit — and a fraud-class bit, so a sticky snub. The relation is
now a closed union, `$log-relation` (`%same` / `%extends by=n` / `%behind by=n`
/ `%fork at=i`), judged in `+anchor-ok`:

- a **fork at any position** is fraud, regardless of length;
- **behind by exactly one** custody entry is **forgiven** — we may simply be the
  one who is out of date, and a one-entry gap is exactly the gap our own
  ingestion path opens. The verdict comes out `%stale`, which demotes to a fresh
  `%alien` and lets the replacement packet through;
- **behind by two or more** is fraud.

`.by` / `.at` are counted in custody-log **entries**, not lives and not blocks.
The comparison keys on hop *identity* (`+hop-id` / `+spawn-id`, which strip
`height` and `start-height`), because heights are reorg-unstable and sit inside
the log the pass commits to — the same reason the accepted-but-unimplemented
block-hash rollback proposal (§11b) is right. `tracked-lag` is emitted for every
`%behind`, forgiven or not, so a forgiveness is recorded in the report rather
than being silent.

Emitting `%stale` for a ship jael never verified under this domain is a no-op
there (it checks `hep` first), so this is also the "stay silent" case without a
second code path.

- `writ-result` gains a variant: `[%stale dom=@tas =ship]`.
- On `%stale`, Jael drops the stored point (the `%lyfe` scry reports the
  ship unknown again) and Ames **demotes the peer to `%alien`**: channel
  state is dropped; queued/pending outbound accumulates as ordinary alien
  todos.
- The ship's next open packet re-enters the normal suite-C `%writ` path.
  On the eventual `%full` verdict the alien is promoted and pending state
  drains through the existing promotion machinery. No new request/response
  API: sat moves are owner-initiated, so the moving ship refreshes its own
  pass (`%anew`, §5) and re-handshakes of its own accord.

### 3a. Early aborts: classifying a verification that ran no checks

The three classes of §3 have three doorways, and only one of them is a check
name:

- `+verify-lc` (`lib/lc-attestation.hoon`) — the light-client adapter that
  fetches the evidence and hands it to the pure `++run-checks` — has four
  early returns that never reach the checker at all. These are `$abort`, and
  are the subject of this section.
- `++run-checks` can answer `ok=%.y` and `%gw-btc` still decline to install
  the point. These are `$refusal` (§3b).
- and a `%jael-writ` can be disposed of before any verification is launched.
  These are `$writ-drop`. They resolve through `+writ-drop-fate` to `$writ-fate`
  rather than to `$verdict-class` — four values, because "declined on purpose"
  and "could not evaluate it yet" emit identical cards (none) and must not read
  identically to an operator. Only its `%fail` reaches the same negative verdict
  the other two doorways can. Not enumerated here, but one of its answers is a
  **standing open decision**: a custody log over
  the 1,024-entry cap (`%log-too-long`) is classed fraud, on the ground that
  walking it is a denial of service and that no amount of catching up changes
  the answer. That condemns a peer for exceeding a **protocol bound** rather
  than for anything it forged. The intended remedy is a protocol-version change
  that raises or removes the bound, not a reclassification, and it has not
  landed.

Each doorway is a **closed union** switched on with `?-`, so a new member
cannot reach any branch — least of all the destructive one — until somebody
has assigned it a class. That property, and not the particular answers below,
is the requirement.

`$abort` (`sur/self-attestation.hoon`) is classified by `+abort-class`, and
`+fail-result` will not build a verdict from a name outside the union, so an
early return cannot be added as a free-form cord belonging to no class. The
rule that decides them:

> **An abort is fraud when the evidence was fully obtained and it is the
> peer's claim about that evidence that fails. It is unknown when the failure
> is about us rather than about them.**

| abort | class | why |
|---|---|---|
| `empty-chain` | **fraud** | The pass decodes to an empty custody log. Purely structural — computed from the peer's own `xtr` before the first watch card, so there was no evidence to obtain. A suite-C `%gw-btc` pass asserts a confidential identity and an empty log offers nothing whatever for it |
| `spawn-opening` | **fraud** | Entry 0 carries no `blind-opening`, so the log never binds itself to the name the pass fingerprints to. Again structural, again reached with no fetch |
| `derive-tip` | **fraud** | `+derive-tip` could not walk the satpoint from the spawn to the tip. This one sits *after* the fetches and needs the argument below |
| `tip-vout-range` | **unknown** | The derived tip names an output index outside the last fetched transaction. Unreachable: reaching it means our own arithmetic contradicted itself. The **fourth** unknown-class outcome, and the only one that is not a check name |

A second rule constrains the first two, and is the reason they cannot be
softened: **aborting early must never be more forgiving than failing late.**
`++run-checks` has a check for each of those two conditions — `chain-nonempty`
and `spawn-opening`, the latter under the identical name — and both are
fraud-class there. If the abort were classed softer, then reaching the same
condition sooner would be an escape hatch from a judgement that arriving later
would have earned.

**`derive-tip` is fraud because a fetch failure never gets there.** "A fetch
went wrong" would be unknown, not fraud, and `+derive-tip` runs after every
fetch — so the classification turns entirely on what can still be true at that
point. Every transaction `+derive-tip` sees came through `+fetch-tx-at`, which
**strand-fails and never returns** if the block-info height disagrees, if the
txid disagrees, or if the transaction is unknown to the light client; and a
failed strand emits no verdict at all (§9). So by the time `+derive-tip` runs,
every transaction in the log is confirmed on the main chain, at the height the
peer claimed, under the txid the peer claimed. The evidence was fully obtained.

What is left for `+derive-tip` to reject is exactly the peer's claim *about*
that evidence: a vout that does not exist in the previous transaction, a sat
offset past the output's value, an input 0 that does not spend the outpoint the
log says it spends, or a hop that drops the sat into fees. `++run-checks` names
those same conditions `entry-N-prevout-range` / `-off-range` / `-continuity` /
`-sat-landed` and calls every one of them fraud. `+derive-tip` is only a
pre-pass to recover the tip scriptPubKey for the liveness scan, so it adds no
judgement of its own — it duplicates those checks. Classing it softer than the
checks it duplicates is exactly the "fail early, be forgiven" escape hatch the
rule above closes.

(A byzantine light client returning a well-formed but *wrong* transaction could
make this fire for an honest peer. That is equally true of `entry-N-continuity`
and of every other fraud verdict this desk emits — the node is the trust root
for all of them, which is a property of the deployment, not a reason to treat
this abort differently.)

**`tip-vout-range` is unknown because it accuses us, not the peer.**
`+derive-tip`'s last step takes the tip vout from `+index-to-sont:urb-core`,
which only ever returns an index at which an output actually exists, over the
very output list this bound then re-checks. The bound is kept only because
`+snag` would crash without it. So if it ever fires, the peer has not been
caught at anything — our own ordinal arithmetic has contradicted itself — and
the honest report is that we could not evaluate the attestation. Snubbing a peer
over a bug in this desk is precisely the outcome the classification exists to
prevent, and this is the case that makes the point without ambiguity: there is
no reading of a `tip-vout-range` abort under which the peer did anything.

### 3b. Local refusals: declining a verdict that passed

`++run-checks` can return `ok=%.y` and `%gw-btc` still refuse to install the
point, for reasons that are about our own state rather than the peer's evidence.
`+local-refusal` (`app/gw-btc.hoon`) names them, as the closed union `$refusal`;
`+refusal-class` classes them. **All four are unknown-class**, so a refusal emits
zero cards: no `%fail`, no `%stale`, silence, and another look on the next
retransmission.

When there is a refusal, its class **replaces** the verdict's own — the verdict
passed every check, so it has nothing to say about the peer, and the only
question left is what the refusal entitles us to. A peer that submitted a
cryptographically perfect attestation must never be snubbed for any of these.

| refusal | why it is not a finding about the peer |
|---|---|
| `who-mismatch` | The verdict names a different ship than the writ did. The peer never supplies `who.verdict`: it is `who.sat`, which `+pass-attestation` set from the ship named in the writ *after* checking that the pass fingerprints to it. A mismatch is our own in-flight bookkeeping |
| `no-point` | `ok=%.y` with no point. `++run-checks` builds the point whenever `ok` holds (`ok` implies `state-resolve`, which implies a resolved snapshot), so this is a contradiction inside the verifier |
| `pass-mismatch` | The rebuilt pass's key disagrees with the pass jael forwarded — but those are the same pass: `+pass-attestation` builds the attestation *from* the forwarded pass and `++run-checks` copies it into the point. Another internal contradiction |
| `tip-owned` | Our sat index already attributes the proven tip satpoint to a **different** comet. The one genuinely reachable refusal; see below |

The first three are internal-consistency guards, unreachable unless this desk
contradicts itself. If one ever fires it is a bug report, and there is nothing
in it to attribute to anybody.

**`tip-owned` is a conflict between two views, and we cannot rank them.** Two
chain-valid custody logs cannot both end at the same satpoint — a satpoint has
one owner — so a conflict here is never two truths. It is one stale view, and
the whole question is whose.

The two views are not comparable. Ours is a **forward-only scanner over an
operator-chosen window**: it begins at whatever height the operator configured,
it ends wherever the scanner has reached, and what it holds for a satpoint
reflects only the blocks it has actually processed. It is routinely behind. The
peer's is a **cryptographic proof against the chain**, hop by hop, which we have
just verified in full. Neither is authority over the other: a proof does not
overrule an index by being a proof, and an index that is admittedly behind does
not overrule a proof by being ours.

So we do neither of the things a decision would license. **We refuse the point
and we refuse the snub.** We do not overwrite the other comet's attribution of
that sat on the strength of evidence we cannot rank against our own index, and
we do not condemn a peer whose attestation passed every check — the conflict may
be entirely our lag. The outcome is `%unknown`-class: no card at all, the peer
untouched, and the next retransmission judged against a scanner that has moved
on.

This is deliberately *not* symmetric with `tracked-prefix`, which compares a
peer's log against our own index and **can** be fraud. That comparison is
against a log we verified **for that same comet**, where a divergence is the
comet contradicting itself. `tip-owned` is a disagreement between two *different*
peers' claims to one sat, adjudicated by an index that may be behind either of
them.

## 4. Publication payload + public scanning (resolves §9 Q4)

A **public** comet publishes on-chain by adding one OP_RETURN output to
its custody transaction (spawn or state update). The confidential path
never publishes; there is nothing to grep for a confidential comet.

```
scriptPubKey = OP_RETURN OP_PUSH3 "urb" OP_PUSH1 <kelvin=0x09> OP_PUSHDATA <payload>
payload      = (jam publication)
publication  = [=pass =opening]        :: sur/self-attestation
opening      = [internal-key=@ux snapshot blind-opening=(unit [spawn blind])]
```

The publication is the on-chain twin of a confidential `xtr` entry: the
`pass` binds the name (`who = fig(pass)`) and the `opening` reveals the
state committed in the transaction's sat-carrying output. A **present**
`blind-opening` marks a **spawn** (it also opens the hiding `dat`
commitment); an **absent** one marks a **state update** (rekey/breach)
of an already-tracked comet.

Maximum payload size: **512 bytes**. Confidential custody transactions
carry no publication output at all. Reachability-restoration and
advertisement payloads (01 §6.2–6.3) are deferred (§7).

### 4a. Byte order of the payload

The payload is the jam's **ordinary little-endian byte serialization** —
the same `+jam-octs` convention every hash preimage in this protocol uses,
and the same bytes Causeway's `jam_bytes` produces. It is *not* the jam
atom read as a big-endian byte string. An encoder/decoder pair can be
self-consistent in the wrong order and still be unable to exchange a
single publication with any other implementation; the pair shipped that
way once, so this is now pinned by a test that decodes a real mainnet
OP_RETURN.

### 4b. A published spawn's `start-height` is `0`, on purpose

`blind-opening` carries `start-height`, defined as the block containing
the transaction that **created** the spawn satpoint (i.e. the funding
tx). In a *packet* the field is load-bearing: the verifier has no block
to start from, and the light client can only address a transaction by
`[height txid]`, so a wrong or missing height means the fetch fails and
the attestation gets no verdict.

In a **publication** it is not, and it cannot be filled in: the
publication output is built and broadcast inside the very transaction it
describes, so at write time nothing knows what block it will land in.
Published spawns therefore carry `start-height = 0` permanently. This is
correct and requires no format change — the public scanner is *reading a
block* when it finds the publication, so it already knows the height, and
the funding tx is the direct parent of the transaction in hand. A scanner
must ignore the field on a publication rather than trust it, and must
never treat `0` as a real height (see `resolve_start_height` in
`causeway.py`, which refuses `0` as an answer for the packet path).

`start-height` is transport metadata and enters no hash preimage, so
correcting it in a transport artifact never invalidates a minted comet.

The scanner (`lib/urb-core`) discovers public comets by grepping
transaction **outputs** for the `OP_RETURN "urb"` prefix — never by
parsing witnesses. Per publication it runs the same verification the
confidential walk does for one hop: for a spawn, the `pass`↔`dat`
hiding-commitment binding, the input-0 funding spend, the `state-key`
output commitment, and sat-occupancy; for a state update, input-0
custody continuity, the `state-key` commitment, and a life advance.
Sat movement (with or without a publication) is followed by the
ordinal tracker. The pre-OP_RETURN precommit/commit/reveal machinery
and the on-chain sotx opcodes (spawn/keys/escape/adopt/reject/detach/
fief/set-mang) are removed; sponsorship is a snapshot field, resolved
off-chain (§2, and 01 §8).

## 5. `%anew` / custody-log extension (resolves §9 Q7)

Owner-driven, no auto-detection in v9:

1. The owner performs a custody transaction (e.g. rotation) via Causeway.
2. After confirmation, Causeway hands `%gw-btc` the new xtr entry and its
   opening (a poke, same channel as proof ingestion).
3. `%gw-btc` validates the entry against the chain (light client), extends
   the log, rebuilds the pass, and answers Jael's `%anew` with
   `%anew-response`; Ames installs the refreshed pass via the existing path.

Known accepted gap (unchanged): the refreshed pass is not written back to
the boot keyfile, so a reboot re-derives from the feed and needs another
`%anew` round-trip.

### 5a. As implemented

The poke is `[%gw-custody-entry entry=custody-entry]` (`$ingest` in
`sur/self-attestation.hoon`), carried on the **`%noun` mark** so neither
side needs a mark file; `%gw-btc` dispatches on the head tag before the
`$jael-poke` mold-cast. It cannot live beside `$jael-poke` in `sur/urb.hoon`
because `sur/self-attestation` already imports `sur/urb`, and naming
`$custody-entry` from there would be a Clay import cycle.

The poke is **evidence, not authority**. `%gw-btc` appends the entry to the
log it already holds (idempotently — re-poking the current tip is a
re-validation request, not a second hop: `+extend-log`) and runs the
**whole extended log** through `+verify-lc`, the *same* light-client walk a
peer's attestation goes through. So before a pass is rebuilt:

- every entry's transaction is fetched by `[height txid]`, and the light
  client itself is made to agree the txid is in the block at that height;
- input 0 of each entry spends exactly the current tip outpoint
  (`continuity`), the sat lands by ordinal arithmetic in a real output
  (`sat-landed`, `off-range`), and every spend after the first hop is
  key-path over a P2TR prevout (`key-path`);
- each opening recomputes `state-key(internal-key, snapshot)` and must equal
  the on-chain P2TR output key of the sat-carrying output that entry created
  (`entry-N-commitment`);
- entry 0's `blind-opening` must open the pass's hiding `dat` commitment
  (`spawn-commit`), which is what binds the log to *our name*;
- `life`/`rift` never regress, only entry 0 may open `dat`, heights are
  monotone;
- the tip is still unspent by a BIP-158 filter scan to the chain tip, failing
  closed on any unavailable filter or block (`tip-unspent`);
- and the candidate pass's messaging key equals the latest custody-proven
  snapshot's `key` (`pass-key`).

The pass is built **once, before validation**, from the ring Jael holds for
our *current life* plus `(jam candidate-log)` (`+with-xtr` in
`lib/gw-btc-pass.hoon`, which copies `ugn`/`cry`/`dat` verbatim so `fig` —
our @p — is unchanged by construction, and is checked anyway). The
`%anew-response` republishes that byte-identical pass, so what Ames installs
is exactly what the light client verified. Because the base comes from
Jael's *current* ring, a kernel `%rekey` is picked up automatically: the
refreshed pass carries both the rotated messaging key and the extended log.

Two consequences worth stating:

- **A failed validation is silence**, never a verdict and never a pass. The
  previously stored log stays in place. `%jael-anew` therefore also
  *re-validates* rather than answering from memory — the identity sat can be
  spent at any time, and a stale pass is worse than none.
- **The sponsor-existence check is relaxed for our own log only.**
  `+run-checks` fails a snapshot naming a sponsor the verifier cannot see as
  a public point. That check belongs to the *peer*; applied to ourselves it
  would leave a comet whose node does not index the public chain unable ever
  to refresh its own pass. `+self-known-public` admits our own
  owner-chosen sponsors; peers still check them.

Validation is single-flight (one job at a time) with the same `~h2`
resource-leak backstop as peer verification, which emits no verdict.
`/x/custody` scries the validated log.

## 6. Packet bound: one fragment (resolves §9 Q3 scope)

- A complete jammed first-contact attestation MUST fit **one Mesa fragment**
  (~1 KiB). No two-fragment bound, no anonymous-reassembly subsystem.
- Under this revision that is comfortable: no inline raw transaction, no key
  history; entries are ~40 bytes, openings only on spawn and tip. Golden
  fixtures MUST include a maximal single-fragment packet to pin the
  achievable log length; packet-local checks reject oversized logs.
- Fetch-heavy verification is the accepted trade: the verifier fetches
  per-entry evidence itself (§8). Packet-carried inclusion proofs /
  progressive merkle hydration are a deferred workstream.
- Entry 0 MUST open the spawn commitment (strict rule retained). Re-anchoring
  and the degraded tier (01 §7.5) are deferred (§7).

## 7. Deferred workstreams (out of scope for v9)

- Sponsorship wire layer: routing records, sponsor-chain hints, multi-sponsor
  fallback, consent tokens/signatures, liveness-enforced rejection.
- Stranger discovery, reachability restoration, sponsor advertisement.
- Degraded-tier (plain `tr(P)`) ships and any-entry re-anchoring.
- Two-fragment packets / bounded anonymous reassembly.
- Packet-carried inclusion evidence and merkle trust hydration.
- Jael domain tombstones (`%bane` permanence) — requires a proper Jael state
  version bump and migration; not an in-place `%5` edit.
- Public-index reorg repair: the block-hash-per-point design of §11b. What
  ships is the halt of §11a.
- Kernel-wide DOS pass.

## 8. Verification backend: `%light-client` = gwbtc/node `%bitcoin-client`

The fetch layer targets the update API of `desk/app/bitcoin-client.hoon`
(gwbtc/node, `develop`): watch paths `/best-block` (with `%reorg-rollback`),
`/block-header/{hash,height}/…`, `/block-filter/{hash,height}/…`,
`/block/{hash,height}/…`, `/transaction/<block-hash>/<txid>`; all facts carry
`block-info` (height, hash, confirmations, chainwork).

- xtr entries name `[txid height]`; fetching is
  `/block-header/height` → hash → `/transaction/<hash>/<txid>`.
- **Tip liveness has no `gettxout` analogue.** Spentness is determined by
  filter scan: walk BIP-158 filters from the tip's height to best-block for
  the tip output's scriptPubKey; on filter match fetch the block and check
  for an input spending the outpoint. Any unavailable filter/block in the
  range → undeterminable → fail closed (never a negative verdict). Reorg
  (`%reorg-rollback`) re-checks tip liveness at apply time.
- Development shim: until `%bitcoin-client` runs in the harness (it lacks
  `%regtest` network parameters and requires the `%tcp` sidecar), a shim
  agent implements the identical watch paths over Bitcoin Core RPC. The
  verifier is agnostic between them.

Two properties of that API are load-bearing and were each fatal when got
wrong against a real node:

- **`/best-block` is a *persistent* subscription**, not a request. It gives
  a fact for every new block and never kicks, unlike every other endpoint
  above (which end their response with `end (kick sub)`). It must never be
  read with a fact-then-kick one-shot from a strand — that blocks forever,
  so tip liveness is never determined and *no* confidential attestation can
  ever produce a verdict. `%gw-btc` holds the subscription and passes the
  chain tip into the verification thread as an argument.
- **The node's `$hexb` is byte-reversed relative to the desk's.** The node
  puts the first wire byte in the low byte of `dat`; the desk (and
  `+p2tr-xonly`, `+state-key`, the BIP-340 code) put it in the high byte.
  The two molds are structurally identical, so a missing conversion type-
  checks perfectly and fails only against real chain data. Note the flip
  has two halves: the node's own GCS matcher consumes filter targets in the
  *node's* order, so converting transaction bytes without converting the
  filter target back makes the filter stop matching — and a spent tip then
  reports **unspent**, which is fail-open. Change both or neither.

## 9. Verifier concurrency (descope of the queue economy)

- **Single-flight per ship**: at most one verification job per ship;
  a duplicate `%jael-writ` for the same `[ship life]` while a job is
  pending, or after a terminal verdict for that exact pass, is dropped
  silently. No pending queue, no inflight-slot economy, no global
  chain-epoch invalidation, no probe pools, no policy/receipt/economic-
  reservation machinery.
- Retained correctness invariants (these are not DOS machinery): a verdict
  applies only if it matches the pass currently under verification for that
  ship; apply-time re-checks against live state (known-public, sat-owner);
  crashes/timeouts/undeterminable fetches never produce a negative verdict;
  bounded log length and per-entry size checks at parse time.
- **No wall-clock deadline decides a verification.** Verification is fully
  asynchronous — the peer sits as an `%alien` until a verdict arrives and
  nothing in the kernel blocks on it — while its cost is dominated by the
  filter scan, which is O(blocks since the comet last moved its sat). A
  fixed timeout would therefore make long-dormant comets arbitrarily
  unverifiable as a function of a magic number, silently converting "slow"
  into "no verdict". A slow-but-progressing verification runs to
  completion. The one remaining timer (`+stuck-job-guard`, `~h2`) is a
  resource-leak backstop, not a policy: its only job is to stop a job that
  died silently from holding a ship's single-flight slot forever. It emits
  no verdict.
- Measured cost, mainnet, 2-vCPU droplet, comet spawned ~20 blocks back:
  **100–110 s** per verification, ~11 filter fetches plus 2 block
  downloads. Scan cost grows with dormancy. A light-client-side primitive
  (batched filter fetches, or a "has this scriptPubKey been spent since
  height H" query) would collapse most of it; that is a feature request for
  gwbtc/node, not a change to the verifier's shape.
- Kernel suspension machinery (`%gost`/`%ghul`/`%bane` + Clay `%tire`
  wiring) is retained as-is, with one fix: an additive
  `[%snub ?(%add %del %set) …]` Ames task so domain suspension does not
  clobber manually curated snub lists.

## 10. New kernel surface (both items small)

- **Jael scry: domain of a ship.** New care (`%dome`):
  `.^((unit @tas) %j /=dome=/<ship>)` — looks up the ship's point, extracts
  the leading `+mat` domain tag from the latest life's pass. Requires
  hoisting `+pass-pki-dom` from Ames's packet core into lull. Returns `~`
  for non-suite-C ships and unknown ships.
- **Udiff source authorization**: Jael accepts generic `%azimuth-udiffs`
  facts only from a live registered domain agent or an explicitly configured
  legacy source. (The authorization guard from the `%bane` prototype branch,
  landed without the unsafe state edit.)

## 11. Chain reorganisation

Two different things in this system are reorg-sensitive, and only one of them
is handled. Attestation *verification* was made reorg-tolerant by `cdaf7cf`
(§3): the tracked-log comparison keys on hop identity rather than on heights,
so a custody transaction re-mined one block over is no longer a divergence.
The **public block index** has no such tolerance. What ships for it today is a
halt, and this section records it, because the spec has so far described only
the replacement.

### 11a. What ships today: `$reorg-stop` and `%gw-reorg-resume`

`%bitcoin-client` reports a reorg as a `%reorg-rollback` on `/best-block`,
carrying the height and hash the chain rolled back to. `%gw-btc` compares that
height against its own scan cursor, `num.block-id.urb-state`
(`app/gw-btc.hoon:1386-1431`), and there are two cases:

- **rollback above the cursor.** The orphaned blocks are ones the scanner had
  not reached, so nothing we hold came from them. The agent slogs a line, moves
  `.best`, and carries on; the winning chain gets walked normally.
- **rollback at or below the cursor.** The index may contain facts derived from
  blocks that no longer exist. **The scanner stops**, and `.reorg-halt` is set
  to a `$reorg-stop` (`app/gw-btc.hoon:92-121`) recording `at` (the height
  rolled back to), `cursor` (where we had scanned to), and `since`.

A halt is not silent, deliberately. It slogs a capitalised block at the moment
it happens; the `/timer` arm keeps re-arming itself every `~m5` and reprints
the halt on every poll (`app/gw-btc.hoon:818-827`), on the principle that a
stopped scanner which stops mentioning it is indistinguishable from a working
one; and `.reorg-halt` is the fourth field of the `/x/ready` scry, alongside
`synced`, `tip` and `indexing`, so a monitor sees it without reading logs.

**Confidential verification deliberately keeps running.** It reads the chain
through the light client, which does its own reorg handling (and re-checks tip
liveness at apply time, §8); its answers do not come from this index at all.
Only the public index is frozen.

**Why a halt and not a repair.** The index cannot be rewound, because a
`$point` records no provenance: not the height, not the block it was indexed
out of. There is therefore no way to tell which entries of `.unv-ids` came from
the losing chain, and rewinding the cursor to rescan would replay the winning
chain *on top of* a corrupted index rather than instead of it — a second bug,
not a fix. A halted index is a liveness failure that announces itself; a
silently forked one is a correctness failure that does not.

`%gw-reorg-resume` (`app/gw-btc.hoon:530-569`, `our`-only) therefore does not
claim to repair anything. It exists so an operator chooses deliberately and on
the record, rather than having the agent guess:

- `~` — resume from the current cursor, accepting that facts from orphaned
  blocks may still be in `.unv-ids` and that the winning chain's replacements
  in the skipped range were never seen.
- `[~ height]` — rewind the cursor first (stored as `height - 1`, since
  `.block-id` is the *last* block scanned) and then resume, so the winning
  chain from there is scanned. This **adds** the correct facts; it cannot
  remove the wrong ones.

Neither is a repair. The only true repair is to rebootstrap the public index,
which needs a nuke — `%urb-start-indexing` is one-shot by design.

### 11b. The agreed replacement (team decision 2026-08-10) — NOT IMPLEMENTED

Recorded here as the accepted design. None of it is in the code; §11a is what
runs.

- **`$point` gains a block hash** in our userspace point type, **refreshed on
  every observation** — attestation, rotation and so on — rather than fixed at
  index time. That is a refinement on the original proposal: the hash tracks
  the most recent evidence for a point rather than its origin.
- **Robin supplies the list of orphaned blocks** on a reorg, so the index can
  be *filtered* against those hashes instead of being nuked or frozen.
- **An orphaned point is forgotten, not snubbed.** The peer drops to `%alien`
  and is asked to re-attest via `%sybl`; it still knows where to find us. This
  is the same shape as `%stale` (§3) and right for the same reason: **a reorg
  is not fraud**, and a snub is permanent on every transport (`b0a8e962ff`), so
  spending one on a chain event would be unrecoverable without an operator.
- **Migration.** Every point already in `unv-ids` carries no hash, so on the
  first reorg after the upgrade they cannot be filtered. "Forget and go
  `%alien`" is a safe default for them — conservative, and the peer re-attests
  — so this may need no special handling at all; that should be confirmed as
  the intent rather than discovered.
- **Future work:** two peers both reorged and unable to find each other. It
  may already be covered by public sponsor fallback.
