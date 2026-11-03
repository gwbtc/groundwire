# Confidential Comets: TODO and Architect Decisions

Updated: **2026-08-03**

This is an ordered cross-repository work list. Read
[confidential-comets-state.md](confidential-comets-state.md) first, and the
kernel specification `gwbtc/urbit/pkg/arvo/doc/spec/pluggable-comet-pki.md` for
normative kernel behavior and
[confidential-comets.md](confidential-comets.md) for the `%gw-btc` protocol. Items marked
**decision** must not be silently resolved in implementation. Items marked
**deferred** are not release blockers.

> **2026-08-03 amendment.** The OP_RETURN revision and its decisions addendum
> (`gwbtc/groundwire/groundwire/doc/opret-revision/`, files `01` and `04`)
> resolve or descope many items below; where they conflict with this list,
> they win. In particular: §§5–9 economic admission, probe pools, receipts,
> reservations and scheduling are **descoped** (single-flight-per-ship
> dedup in the agent replaces them; on-chain cost is the sybil resistance);
> §11's two-fragment reassembly path is **descoped** (one-fragment bound);
> §1's `dat` layout is superseded by the Kelvin-9 hiding commitment; §13a's
> udiff authorization is landed (tombstone still deferred pending a state
> migration); §14's stale-vs-`%fail` and re-attestation items are resolved
> by the `%stale` flow; §15's additive `%snub` is landed; §17's life-2 hang is
> diagnosed and fixed and the Aqua fixtures are regenerated against the
> kelvin-9 formats. The remaining live kernel items are Mesa scenario
> reruns and the shared golden vectors.

## Working rules

1. Preserve the distinction between merged target, dirty worktree, selected
   next design, and open option.
2. Harmonize wire molds with the other Groundwire codebases before changing
   producers or consumers. Do not create an Urbit-only encoding.
3. Keep reorg/chaintip logic in `%light-client`, economic thresholds in policy,
   historical verification in `%gw-btc`, and anonymous packet bounds in Ames.
4. Stage documentation and code by explicit path. Never include the untracked
   `dev/` mounted desk.
5. A test double may model an asynchronous dependency; it must not turn a
   light-client assertion into protocol proof.

## Active pull-request map

All heads start from `agent/confidential-comets-waypoint` except the Aqua
prototype, which is intentionally stacked on the compatibility head.

| PR / branch | Base | State | Merge condition |
|---|---|---|---|
| [#59](https://github.com/gwbtc/urbit/pull/59) / `agent/cc-ames-malformed-domain` | `agent/confidential-comets-waypoint` | ready | review guarded parsing and the downstream timeout/no-crash regression limitation |
| [#60](https://github.com/gwbtc/urbit/pull/60) / `agent/cc-jael-anex-rewatch` | `agent/confidential-comets-waypoint` | ready | review exact-duplicate re-watch; add lifecycle integration separately |
| [#61](https://github.com/gwbtc/urbit/pull/61) / `agent/cc-jael-bane-auth` | `agent/confidential-comets-waypoint` | draft | choose retirement semantics, add a real `%5 -> %6` migration, recovery, source matrix, and lifecycle tests |
| [#62](https://github.com/gwbtc/urbit/pull/62) / `agent/cc-test-compat` | `agent/confidential-comets-waypoint` | ready | review provenance and focused passes with the disclosed baseline-red suites |
| [#63](https://github.com/gwbtc/urbit/pull/63) / `agent/cc-aqua-attestation` | `agent/cc-test-compat` | draft | diagnose the life-2 second-handshake hang, rerun Mesa, and replace prototype identities/jammed satpoint/boolean verdicts with canonical vectors and deterministic `%light-client` facts |

Do not merge the draft Jael branch merely because it compiles. Before it can
be ready, choose whether `%bane` is permanent, design recovery if needed,
introduce a valid new Jael state version/migration for any persisted field,
and test unregistered denial, registered/live allowance, configured legacy
allowance, suspended denial, queued facts, and the real Gall/Clay lifecycle.

Do not promote the Aqua draft by polishing the obsolete vectors. Retain its
asynchronous Ames -> Jael -> Gall -> Jael -> Ames shape, but regenerate
identities from the harmonized direct `dat` and replace boolean `xtr` verdicts
with deterministic packet and `%light-client` test vectors. Add same-life
state sequence, public/confidential arrival orders, bounded scheduling,
deadline, stale callback, and cancellation coverage after those contracts are
frozen.

## P0: freeze the revision-2 wire contract

### 1. Harmonize immutable `dat`

- [ ] Locate the canonical encoders and decoders in every participating
  Groundwire repository.
- [ ] Replace jammed-satpoint variants with exactly:

  ```hoon
  (can 0 (mat %gw-btc) [256 txid] (mat vout) (mat off) ~)
  ```

- [ ] Specify endianness, fixed txid width, accepted `vout`/`off` ranges,
  canonical `+mat` decoding, and rejection of trailing data.
- [ ] Publish shared positive and negative golden vectors and make both Urbit
  and Groundwire consume them.

Justification: `dat` determines the comet name and cannot be migrated in place.
Even equivalent-looking noun encodings are distinct identities.

### 2. Freeze the full-state commitment mold

- [ ] Define “full Jael/urb snapshot” precisely: sequence, rift, life, complete
  relevant keys/current key, sponsor, fief/other selected routing state, and
  suite-C pass normalization.
- [ ] Require the pass inside committed state to have `xtr=0`; document the
  comparison that ignores only the outer pass's mutable `xtr`.
- [ ] Define state-sequence monotonicity and overflow/canonical-number rules.
- [ ] Confirm combined genesis encoding and how the immutable spawn satpoint is
  related to its funding and first custody transactions.
- [ ] Define conversion to Jael point/udiff, including the accepted rule that a
  missing sponsor uses the ship itself as the conversion fallback.
- [ ] Define explicit-public-sponsor handling: an unknown explicit sponsor is
  deferred or policy-ineligible until public, never silently rewritten to self
  and never cached as cryptographic fraud.
- [ ] Update `sur/stealth.hoon` or remove it as a competing design authority.

**Decision — unchanged-state checkpoint sequence.** A bare custody move must be
followed by a state-bearing current tip before first-contact admission. Choose:

- **A. Sequence numbers semantic state changes.** Recommitting identical state
  after a custody move retains its sequence; Bitcoin custody order disambiguates
  checkpoints. This keeps the sequence about Jael meaning.
- **B. Sequence numbers every state commitment.** Every checkpoint increments,
  even if the state is unchanged. This gives one simple total order but mixes
  identity-state revision with chain placement.

Record the choice in the spec and shared vectors before implementation.

### 3. Freeze the `xtr` envelope and profiles

- [ ] Reuse the common organization tag/version conventions; wire versioning is
  permitted and distinct from the explicitly rejected `%gw-btc` persisted-state
  migration/versioning.
- [ ] Require one canonical ordering, fixed-width txids, canonical heights,
  unique entries, and `xtr == jam(decoded-xtr)`.
- [ ] Require a full raw current-tip transaction and full current-state reveal.
- [ ] Derive the head txid and unique commitment `vout`; do not transmit
  redundant trusted values.
- [ ] Bound historical `[txid height reveal]` references and require reveals
  for every state-bearing commitment that policy or custody verification uses.
- [ ] Define the origin proof: first funding transaction, first custody/spawn
  transaction, or an authenticated funding-prevout fact from `%light-client`.

**Decision — short-log origin profile.** Measure first, then choose a
deterministic rule:

- **A. Inline genesis-side evidence when `log-length <= K`.** Best balance if a
  stable `K` fits all permitted state sizes; rejects more origin fraud without a
  fetch.
- **B. Always inline it.** Simplest verification and strongest endpoint
  self-containment, but may crowd out large full-state snapshots.
- **C. Never inline it.** Smallest packet and simplest producer, but loses the
  cheap second endpoint check motivating the two-fragment revision.

Recommended direction is A only if exact worst-case fixtures establish a
deterministic `K`; otherwise use an exact byte/profile rule rather than the
informal “about five entries.” A sender must not choose a weaker form ad hoc.

### 4. Measure the complete packet

- [ ] Generate canonical complete open responses for 1, 2, and 5 key lives;
  no sponsor, public sponsor, and fief cases; short and long custody logs; and
  minimum/maximum permitted transactions.
- [ ] Measure the complete jammed response, not only `xtr`.
- [ ] Set explicit maxima for state, transaction, entry count, and total 2,048
  bytes.
- [ ] Make oversize behavior deterministic and test it in both codebases.

Justification: the old 5–15-entry estimates assumed delta reveals. Full key
history may dominate the packet.

## P0: implement economic admission before the heavy queue

### 5. Packet-local preflight in `%gw-btc`

- [ ] Parse/canonicalize the envelope, full state, and raw head transaction
  without entering the historical queue.
- [ ] Bind `who`, normalized current key, and outer self-attestation signer.
- [ ] Reconstruct the combined single-leaf Taproot key and require exactly one
  matching output.
- [ ] Derive txid/`vout`, read nominal value, and reject malformed or obviously
  sub-policy heads immediately.
- [ ] Distinguish `invalid`, `stale`, `policy-ineligible`, and `defer` results.

Never claim this step proves inclusion, unspentness, actual witness, or fee.

### 6. Atomic `%light-client` admission fact

- [ ] Agree a minimal request/response contract with `%light-client` for
  canonical block identity/height, confirmations, exact tx output, current
  unspentness, and a chain-view identifier.
- [ ] Decide whether authenticated fee is available. If so, state how prevout
  values and witness bytes are authenticated.
- [ ] Return one atomic chain view rather than composing observations that can
  race a tip change.
- [ ] Keep the probe pool small, constant-size, short-lived, deduplicated, and
  negatively cached.

**Decision — inclusion evidence in the packet.** An optional Merkle branch can
prove txid inclusion against cached headers but still cannot prove unspentness
or witness bytes. Omit it from revision 2 unless measurements show that saving
one inclusion lookup justifies the bytes and complexity.

### 7. Policy fact/result boundary

- [ ] Define an authenticated fact record containing at least ship, state
  sequence/hash, `[txid vout]`, value, height, confirmations, chain view,
  optional total/allocated fee, and credited distinct commitment heights.
- [ ] Define policy results `eligible`, `ineligible`, and `defer`, with receipt
  expiry and policy-version identity.
- [ ] Reserve an eligible outpoint atomically for one active job at a receiver.
- [ ] Recheck the outpoint and chain view before historical launch and before
  applying the verdict.

**Decision — thresholds.** Minimum value, confirmations, fee, and commitment
depth are deployment policy and should be bracketed from this implementation.
Ship safe defaults separately after economic modeling.

**Decision — batched fee attribution.** Do not credit the full transaction fee
to every ship output. Options are one ship per fee-bearing transaction, one
receiver-side claimant per txid, or conservative allocation across eligible
outputs. Conservative allocation is the preferred future-compatible direction
because it permits legitimate batching without creating fee credit.

**Decision — one commitment per block.** Initially prefer “at most one state
commitment per ship per authenticated height receives policy credit.” Making a
second same-block commitment protocol-invalid is stronger but unnecessarily
ossifies custody construction. Record the architect choice.

## P0: make asynchronous scheduling race-safe

### 8. Separate pools and queues

- [ ] Implement three explicit stages: bounded anonymous/preflight state,
  bounded light-client probes, and economically eligible historical jobs.
- [ ] Keep hard eligible/in-flight limits and deadlines even after economic
  gating; rich attackers and policy mistakes still exist.
- [ ] Use FIFO within capped policy classes or another explicit scheduler. Do
  not depend on Hoon map order and do not create an uncapped wealth auction.
- [ ] Reserve a modest continuation lane for previously verified peers whose
  prefix can be reused, without bypassing current-head economic policy.

### 9. Job identity and epochs

- [ ] Audit the current token/context/chain-epoch guards, preserve the useful
  pieces, and key the revised job by ship, state sequence/hash, economic
  head/outpoint, policy receipt, and captured chain view.
- [ ] Apply a callback only when its job identity is still current.
- [ ] Allow one active job and at most one independently admitted successor per
  ship.
- [ ] Do not let an unqualified replacement cancel paid in-flight work.
- [ ] Cache terminal results and cooldowns by economic token so alternate tails
  cannot reopen unlimited work behind one head.
- [ ] Reuse verified custody prefixes and validate only the suffix.

### 10. Public/confidential race behavior

- [ ] Compare candidates by custody-proven state/sequence rather than admission
  source.
- [ ] Ensure a public-index update cannot cancel a qualified confidential job
  merely because it arrived on a privileged path.
- [ ] Ensure a confidential packet cannot replace a newer verified public
  state.
- [ ] Retain source/lane only for free fragment/probe fairness and diagnostics.

Accepted rule: latest custody-proven state wins regardless of visibility.

## P0: kernel work

### 11. Bounded two-fragment alien Mesa path

- [ ] Design a dedicated at-most-two-fragment first-contact assembly path; the
  normal known-peer Mesa assembler cannot simply be reused by relaxing the
  assertion.
- [ ] Enforce at most 2,048 complete bytes, two data fragments, a fixed small
  pool, TTL, LRU eviction, and isolation from established peers.
- [ ] Authenticate root metadata, fetch fragments in order, and cue/verify only
  after complete assembly.
- [ ] Leave the legacy Ames open path single-fragment.
- [ ] Test missing fragment, duplicate fragment, wrong root, oversize response,
  LRU eviction, timeout, malformed cue, bad pass/name, and valid two-fragment
  promotion.

### 12. Land malformed suite-C intake as a focused fix

Tracking PR: [#59](https://github.com/gwbtc/urbit/pull/59), branch
`agent/cc-ames-malformed-domain` (ready for review).

- [x] Extract the guarded `+rub`/`+pass-pki-dom` change from the dirty tree.
- [x] Keep legacy intake fail-closed before alien insertion, and Mesa intake
  fail-closed before Jael lookup/cards inside its already-created `al-core`.
- [x] Publish the malformed-attestation regression independently of revision 2.
- [ ] Land it after review.

This is a separate Ames fail-closed bug fix. It may share a focused kernel PR
with the Jael `%base` work if review prefers, but it is not the same bug and
must remain a separately testable change. Neither belongs in the Groundwire
userspace PR.

### 13. Land the selected `%anex` restart fix

Tracking PR: [#60](https://github.com/gwbtc/urbit/pull/60), branch
`agent/cc-jael-anex-rewatch` (ready for review).

The merged target rejects every duplicate registration. The selected follow-up
accepts an exact duplicate from the same agent and path as an idempotent request
to re-emit the retained Gall watch; a changed path remains a hard conflict. The
focused implementation is published in #60, not yet landed.

- [ ] Reproduce pause/revive and true nuke/restart separately.
- [ ] Observe Gall kick, Jael re-watch, watch acknowledgements, desk `%tire`, and
  whether the restarted app can publish `/writs` without manual action.
- [ ] Verify exact duplicates repair the negative-watch-ack gap without
  duplicating watches or resetting domain state.
- [ ] Verify a different path and a different agent cannot take over the
  registration.
- [ ] Land this in the separate Jael `%base` PR, with its focused tests.

### 13a. Decide Jael domain retirement and generic udiff authorization

Tracking PR: [#61](https://github.com/gwbtc/urbit/pull/61), branch
`agent/cc-jael-bane-auth` (draft only; unsafe for live `%5` upgrades).

- [ ] Decide whether `%bane` permanently tombstones a domain or permits an
  explicit architected recovery path.
- [ ] If a tombstone is persisted, introduce a new Jael state version and a
  valid migration from existing `%5`; never add the field directly to `%5`.
- [ ] Decide whether `%bane` explicitly leaves the Gall watch or whether Gall
  lifecycle ownership should close it through another path.
- [ ] Review the proposed ingress rule: generic udiffs are accepted only from
  a live registered domain or an explicitly configured legacy source, with a
  tombstone overriding a dual-role legacy registration.
- [ ] Add positive and negative authorization tests and exercise queued facts,
  suspension/revival, app nuke/restart, and watch acknowledgements.

### 14. Decide app-nuke recovery and re-attestation

The merged `%anew` refreshes the local ship's own pass. It does not ask a
remote peer to replace a stale packet, replay prior `%writ`s, or reconstruct a
nuked `%gw-btc` private index.

Options for a true nuke:

- **A. Lazy recovery.** Restore the verifier watch and let later suite-C traffic
  trigger new `%writ`s. Minimal kernel work, slower recovery.
- **B. Eager peer re-attestation.** Jael retains the domain's `hep` set and Ames
  requests a fresh attestation from every peer after the verifier returns.
  Recovers promptly but needs a peer-directed kernel flow and a bounded fanout.
- **C. Persist/recover the private index.** Avoids network replay but changes the
  confidentiality and persistence design.

- [ ] Choose lazy versus bounded eager behavior for this release.
- [ ] Specify a peer-directed stale verdict/request/response path, including
  retry bounds and correlation with job epochs, if eager or verifier-triggered
  refresh is selected.
- [ ] Keep stale/defer distinct from `%fail` so re-attestation does not create a
  sticky snub.
- [ ] Fix the known-peer/equal-life intake gap: a suite-C state checkpoint can
  advance at the same Ames life, so life comparison alone must not bypass
  `%gw-btc`. Define bounded deduplication and when a changed `xtr`/head is sent
  through `%writ` again.
- [ ] Decide whether refreshed local `xtr` persistence to the boot keyfile is
  deferred or required. No `%gw-btc` state migration is implied.

### 15. Decide `%bane` and additive snub semantics

- [ ] Architect-review the draft #61 durable domain tombstone. The merged `%bane`
  deregisters/destructively breaches but does not permanently reserve the
  domain name.
- [ ] Decide whether `%bane` must leave the Gall watch explicitly and how queued
  facts are rejected after the ban event.
- [ ] Add `[%snub ?(%add %del %set) form ships]` or an equivalent additive Ames
  task so `%gost/%ghul/%bane` do not clobber manual blocklists.
- [ ] Preserve the already merged additive behavior for a single failed alien.

## P1: verification

### 16. Shared pure vectors

- [ ] Canonical/harmonic `dat` and `xtr` encodings.
- [ ] Full-state sequence, unchanged-state checkpoint, life/key rotation, public
  sponsor, no-sponsor ship fallback, and combined genesis.
- [ ] Inline-head txid/script/value/key binding, zero/multiple matching outputs,
  malformed/oversized state, and noncanonical transaction.
- [ ] Economic facts: below-minimum value, unconfirmed, spent, unknown, stale
  chain view, reused outpoint, batched fee allocation, and same-block depth.
- [ ] Custody failures after admission: gap, wrong sat offset, bad witness,
  omitted state reveal, forked history, and bad final recheck.

### 17. Aqua without external Bitcoin

Tracking draft: [#63](https://github.com/gwbtc/urbit/pull/63), stacked on
[#62](https://github.com/gwbtc/urbit/pull/62).

- [x] Run the prototype Ames accept and reject paths after repairing the suite
  inventory; both reach `done` without `%aqua-crash`.
- [x] Diagnose the life-2 second-handshake hang. Three independent causes, all
  fixed: (a) `+get-keys:aqua-azimuth` derived each life from a different seed,
  so the suite-C signing half — and therefore the comet's `@p` — changed on
  rekey; the seed is now split, with only the messaging half rotating.
  (b) `+on-hear-packet:ames` routed a comet's packet to `+on-hear-open` only
  while the comet was *not* `%known`, so the re-attestation that follows a
  rekey was fed to the SIV decrypter; authentication failure there is a `%evil`
  bail, which under Aqua killed the host event and blocked the behn queue (this
  also broke plain `attestation-hi`, whose comet re-attests while the verifier
  is still deciding). Ames now recognises an attestation by shape
  (`+is-open-packet`). (c) Nothing made a rekeyed comet re-attest at all;
  `+sy-priv` now sends a fresh self-attestation to every known peer when our
  own life changes, which is the only signal a confidential comet's peers can
  get. The `known-filter` workaround in `ted/aqua/ames.hoon`, which dropped
  attestations to `%known` receivers to dodge (b), is removed.
- [ ] Rerun all four scenarios under Mesa.
- [x] Preserve the current async success, rejection, malformed, and life-2
  scenarios while replacing old delta/current packet fixtures. Fixtures are now
  kelvin-9: `dat` is the hiding spawn commitment and `xtr` a real
  `$custody-log`. Both are checked into `pkg/arvo/lib/aqua-azimuth.hoon` as
  opaque atoms; base arvo carries **no** `%gw-btc` codec. Regenerate them with
  `+groundwire!aqua-fixtures`, whose output is produced by this desk's
  `lib/gw-btc-pass.hoon` — the one authoritative implementation, pinned in turn
  by `vectors/gw-kelvin-9.json`. Note that the simulation does **not** run the
  `%gw-btc` domain: base arvo may not name a vendor, so the fixtures commit to
  `+aqua-domain` (`%test-pki`), which must equal `+cc-domain:aqua-azimuth`.
  The fake verifier agent in `pkg/arvo/lib/ph/cc/util.hoon` is a lookup table
  over those fixtures, not a verifier; real verification is tested here, in
  `tests/lib/gw-btc-pass.hoon` and `tests/lib/self-attestation.hoon`.
- [ ] Inject delayed and out-of-order `%light-client` answers to exercise job
  epochs, successor handling, timeout, and cancellation rules.
- [ ] Cover public/confidential arrival in both orders and confirm that verified
  state—not source—wins.
- [ ] Fill and drain the fragment, probe, eligible, and in-flight bounds.
- [ ] Confirm suite-B Ames and Mesa tests remain unchanged.

### 18. Regtest E2E — deferred

- [ ] Run the real `%light-client`/`%spv-wallet` stack with a sidecar Bitcoin
  regtest node.
- [ ] Mine confirmation-depth fixtures, spend/recommit the tracked sat, exercise
  actual witness retrieval, fee facts, and chain reorganization.
- [ ] Assert that `%gw-btc` only observes stale/new light-client facts and does
  not grow an independent reorg state machine.

## Release acceptance checklist

- [ ] Spec, state, TODO, Groundwire spec, and code agree on state semantics and
  canonical `dat`.
- [ ] Exact complete-packet fixtures fit the selected two-fragment bounds.
- [ ] No historical job exists without a live reserved economic outpoint.
- [ ] Late callbacks cannot overwrite a newer state or public snapshot.
- [ ] Public and confidential candidates converge on the latest custody-proven
  state.
- [ ] Remote stale behavior is explicit and non-sticky, even if automatic
  re-attestation is deferred.
- [ ] All deterministic Urbit/Groundwire tests pass; suite-B regressions pass.
- [ ] Dirty Jael choices are either separately approved/landed or removed from
  the implementation branch.
- [ ] Reorg/chaintip behavior remains owned by `%light-client`.

## Explicitly out of scope for this release

- `%urb-watcher` migration or versioned `%gw-btc` persisted-state migration;
- private/non-public sponsors and a general sponsor-revocation protocol;
- complex Taptrees or per-receiver on-chain commitments;
- a Groundwire-owned chaintip/reorg engine;
- production policy tuning beyond the authenticated fact/result boundary; and
- external-node regtest E2E, provided the deterministic adapter boundary is
  complete and the deferred work stays recorded above.
