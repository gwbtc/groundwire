# `%gw-btc` confidential-comets TODO

This is the implementation queue for the 2026-07-29 waypoint. It absorbs the
Groundwire-side open items previously scattered through the old spec and code
comments. Cross-repository kernel work remains explicitly assigned to Urbit.

Read [SPEC](confidential-comets-agent.md) and
[STATE](confidential-comets-agent-state.md) first.

## 1. Release gates requiring architect decisions

Do not silently settle these in code.

- [ ] Freeze state-sequence semantics for identical state checkpoints:
  increment every commitment, or increment only on semantic Jael-state change.
- [ ] Decide whether naming a public non-self sponsor is sufficient, or whether
  relationship-specific domain-separated consent is required.
- [ ] Freeze the full committed-state mold, including ship binding, sequence,
  normalized key/pass representation, sponsor, and fief.
- [ ] Freeze explicit-public-sponsor handling: absent means self; an explicit
  sponsor not yet in the authoritative public snapshot is deferred or
  policy-ineligible, never silently rewritten to self and never sticky fraud.
- [ ] Freeze the tagged v1 `xtr` Hoon mold and exact field ordering with
  Causeway and Urbit.
- [ ] Measure and select the deterministic short-log inline-origin rule. Do not
  encode the provisional `~5` estimate as protocol.
- [ ] Decide the minimal authenticated fee fact and fee-allocation rule, or
  explicitly defer fee policy and ship with UTXO-value admission facts only.
- [ ] Confirm that “one commitment per ship per block” is initially a policy
  credit cap rather than a protocol-invalid condition.
- [ ] Decide how internal `%invalid`, `%ineligible`, `%defer`, and `%stale`
  results map to the existing Jael verdict API without caching policy misses or
  infrastructure failures as fraud.

Recommended minimal choices for this release are: semantic sequences with an
explicit same-sequence/same-state rule; public-existence-only sponsorship unless
consent is affirmatively specified; UTXO-value admission with fee facts
optional; and one depth credit per ship per authenticated height.

## 2. Establish the implementation branch and evidence

- [ ] Fetch the target and record the exact fresh base hash. The target observed
  at the waypoint was `2e2407c1225d8d564d403d7ca09ce66820b58e8f`.
- [ ] Preserve the adversarial waypoint
  `0fc1493180c6db9e6275a189fe9639c6201db430`; do not rebase it onto the target.
- [ ] Keep this documentation commit on both the adversarial line and the fresh
  target-based working line.
- [ ] Record the exact Urbit companion hashes/spec revision used to freeze
  shared interfaces.
- [ ] Before code changes, run and record the target's compile/unit/Aqua baseline
  with exact commands and artifacts.
- [ ] Inventory tests from `0fc1493` and classify each as port unchanged, adapt
  to state semantics, superseded, or future policy coverage.

## 3. Freeze shared codecs and vectors

### Canonical `dat`

- [ ] Replace any `(mat domain) + jam(sont)` codec with:

  ```hoon
  (can 0 (mat %gw-btc) [256 txid] (mat vout) (mat off) ~)
  ```

- [ ] Share txid byte-order rules with Causeway and `%light-client`.
- [ ] Reject trailing bits and noncanonical `mat` encodings.
- [ ] Add cross-language/cross-repository golden vectors for minimum and maximum
  representative `vout`/`off` values.

### Complete state

- [ ] Add the full normalized Jael-state-plus-sequence mold.
- [ ] Name the Taproot opening separately from the complete state decoded from
  its leaf script; do not reuse the old compact `$reveal`/`$gw-state` names as
  though they were the v1 state mold.
- [ ] Normalize every suite-C pass embedded in the snapshot to `xtr=0`.
- [ ] Add a helper that compares the outer current pass after the same
  normalization and then materializes the actual outer pass on success. Compare
  the complete normalized suite-C pass, including its attestation signing key;
  do not retain the earlier `cry`-only shortcut.
- [ ] Require self-enactment and ship binding for `%spawn` and `%state`.
- [ ] Reject state regression and same-sequence/different-state conflicts.
- [ ] Remove confidential verification's dependency on replaying event-shaped
  `%keys`, `%escape`, `%adopt`, `%detach`, `%set-mang`, or related deltas.
- [ ] Keep the public block indexer's event semantics isolated from this codec.

### Combined genesis and Taproot

- [ ] Confirm that `dat` names the funding outpoint and that the combined spawn
  transaction spends it.
- [ ] Require a single combined state leaf for this release.
- [ ] Derive the landing satpoint and verify the combined genesis output.
- [ ] Preserve strict Tapscript leaf-version and canonical script parsing.
- [ ] Derive rather than trust the head txid and matching `vout`; require a
  unique matching P2TR output.

### Versioned `xtr`

- [ ] Replace the untagged oldest-first locator with the tagged v1 envelope.
- [ ] Encode the mandatory inline, state-bearing current head.
- [ ] Order bounded middle references newest-to-oldest, with unique txids.
- [ ] Encode combined-genesis evidence exactly once.
- [ ] Implement the selected deterministic inline-origin profile.
- [ ] Reject sender-selected equivalent alternate forms.
- [ ] Enforce canonical `xtr == jam(decoded-envelope)`.
- [ ] Add complete-response size fixtures, including pass/cage/vase/open-packet
  overhead and one-, two-, and multi-life state snapshots.

## 4. Define the local `%light-client` contract

- [ ] Keep `%light-client` as the only Bitcoin-data source for confidential
  verification. Do not port the target's direct remote RPC path.
- [ ] Define an atomic admission response containing chain view, canonical
  height, confirmations, exact output, value, and current unspentness.
- [ ] Decide whether the same response includes authenticated total fee or fee
  credit; document required prevout authentication.
- [ ] Define transaction-by-height responses for the later custody walk,
  including authenticated witness data.
- [ ] Distinguish `%unknown`, `%not-found`, `%spent`, `%stale-view`, temporary
  adapter failure, and malformed response.
- [ ] Give every receipt an expiry or validation method.
- [ ] Treat ordinary best-tip advancement as a wake/recheck opportunity, not a
  reason to increment a global invalidation epoch.
- [ ] Remove `%gw-btc` reorg interpretation; consume `%light-client`'s chain-view
  and stale results instead.

## 5. Implement staged admission

### Packet-local preflight

- [ ] Enforce complete packet, log, state, transaction, and reveal size bounds.
- [ ] Validate suite, domain, name, canonical `dat`, canonical `xtr`, and entry
  uniqueness before allocation.
- [ ] Parse and re-encode the inline head transaction.
- [ ] Parse the full state and bind its current key to the outer pass.
- [ ] Reconstruct the unique Taproot commitment output and candidate value.
- [ ] Reject below-floor nominal values before a light-client request when a
  configured floor is available.

### Admission-probe pool

- [ ] Add a small pool separate from pending historical jobs.
- [ ] Bound concurrency, total records, record size, and TTL.
- [ ] Deduplicate by txid/outpoint and add short negative caching.
- [ ] Request the atomic inclusion/UTXO fact from `%light-client`.
- [ ] Construct the short-lived policy receipt.
- [ ] Permit source/lane quotas only here and in Ames pre-auth reassembly; do not
  make them protocol authority.

### Policy seam and reservation

- [ ] Define an `admission-facts` input and structured policy result without
  hardcoding monetary policy into the verifier.
- [ ] Make current UTXO value the first mandatory economic fact.
- [ ] If fee credit is enabled, prevent one batched fee from being credited in
  full to multiple identities.
- [ ] Count at most one authenticated commitment height per ship per block for
  depth policy.
- [ ] Reserve an eligible outpoint atomically before queuing historical work.
- [ ] Prevent one outpoint/economic token from backing multiple jobs.
- [ ] Release the reservation on completion, expiry, stale view, or cancellation.

## 6. Port the safe scheduler properties

Use the adversarial branch as design/test input, not as a patch to apply whole.

- [ ] Retain hard pending, inflight, and deadline bounds.
- [ ] Key requests by `[ship tip-outpoint state-hash]`, not ship alone.
- [ ] Give every launch a monotonic job id and immutable request token.
- [ ] Capture the public-index/application context needed to reject stale
  callbacks without treating every chaintip as a new epoch.
- [ ] Allow at most one active job and one independently eligible successor per
  ship.
- [ ] Do not let an unproven or policy-ineligible replacement cancel active paid
  work.
- [ ] Use explicit FIFO/age order within any capped policy-score buckets; do not
  use map traversal order as scheduling policy.
- [ ] Cache completed/failed work by economic head to stop mutable-tail replay.
- [ ] Reserve capacity or priority for verified-prefix suffix checks without
  bypassing the current-head economic requirement.
- [ ] Recheck the receipt/current outpoint immediately before applying a result.

## 7. Implement the state custody walk

- [ ] Validate inline combined genesis according to the selected origin profile.
- [ ] Fetch only missing references after economic eligibility.
- [ ] Stop at the first failed link.
- [ ] Authenticate heights, transactions, and witnesses through `%light-client`.
- [ ] Track the exact sat from the `dat` funding output to the admitted head.
- [ ] Accept reveal-less custody moves internally while requiring a state-bearing
  current head for first-contact eligibility.
- [ ] Validate every revealed Taproot commitment and full state.
- [ ] Enforce sequence/state conflict rules.
- [ ] Cache a verified prefix and verify only a new suffix when possible.
- [ ] Project the latest complete normalized state to Jael and insert the actual
  incoming outer pass as the current key.
- [ ] Preserve the ship-itself no-sponsor fallback in udiff/point conversion.

## 8. Resolve public/confidential concurrency

- [ ] Replace source-priority logic with custody/state reconciliation.
- [ ] Capture block-scanner base context for each relevant job.
- [ ] When a public spawn races a confidential insert, compare their common
  origin and head rather than automatically declassifying or discarding either.
- [ ] Let the latest custody-proven state win regardless of visibility.
- [ ] Keep confidential points and private verification metadata out of public
  snapshots and scries.
- [ ] Ensure a public sponsor becoming known can retry a previously deferred
  sponsor check without corrupting an active job.
- [ ] Add adversarial callback-order tests for every race branch.

## 9. Jael and kernel integration

Groundwire tasks:

- [ ] Send `%anex /writs` on initialization and accept Jael pokes only from the
  local vane source.
- [ ] Do not add `%urb-watcher` migration or versioned `%gw-btc` state.
- [ ] Persist enough private state and job metadata to preserve safe Gall
  save/load behavior for the supported lifecycle.
- [ ] Keep `%jael-anew` silent unless the local pass can be proven current.
- [ ] Separate local pass construction from remote-peer re-attestation requests.

Urbit-owned dependencies, to be implemented in separate changes:

- [x] Publish malformed-domain hardening as ready
  [urbit#59](https://github.com/gwbtc/urbit/pull/59); merge remains subject to
  review.
- [x] Publish the exact-duplicate `%anex` re-watch as ready
  [urbit#60](https://github.com/gwbtc/urbit/pull/60); retain full app-nuke and
  Gall/Clay lifecycle coverage as follow-up.
- [x] Publish the `%bane`/udiff-authorization experiment as draft
  [urbit#61](https://github.com/gwbtc/urbit/pull/61); do not ready it without a
  real new Jael state version/migration, recovery decision, source matrix, and
  lifecycle tests.
- [x] Publish test compatibility provenance as ready
  [urbit#62](https://github.com/gwbtc/urbit/pull/62), with existing baseline-red
  suites explicitly recorded.
- [x] Publish the async-attestation scaffold as draft
  [urbit#63](https://github.com/gwbtc/urbit/pull/63), stacked on #62.
- [ ] Replace #63's jammed-satpoint identities and boolean verdicts with the
  frozen revision-2 packet and deterministic `%light-client` facts; rerun its
  full Ames/Mesa matrix after every fixture-inventory change.
- [ ] Add bounded two-data-fragment alien Mesa reassembly in Ames.
- [ ] Add or confirm the Jael/Ames path that asks a remote peer to re-attest
  after its recorded outpoint moves.
- [ ] Merge the reviewed Jael `%anex` same-agent/same-path lifecycle fix while
  continuing to reject conflicting paths.
- [ ] If needed, extend the Jael verdict API so invalid, policy-ineligible,
  deferred, and stale outcomes do not collapse into one sticky failure.

## 10. Test plan

### Pure and golden tests

- [ ] Causeway-compatible `dat` vectors and malformed/trailing encodings.
- [ ] Tagged `xtr` canonicality, ordering, duplicates, and profile selection.
- [ ] Full-state normalization and outer-key equality.
- [ ] Combined genesis, self-enactment, sequence regression, and sequence
  equivocation.
- [ ] Inline txid/output derivation and ambiguous/missing matching outputs.
- [ ] Realistic complete-response sizes for both one- and two-fragment cases.
- [ ] Pure custody moves followed by required state checkpoint.
- [ ] Cached prefix plus new suffix verification.
- [ ] UTXO, fee allocation, and one-credit-per-height policy facts.

### Light-client adapter vectors

- [ ] Confirmed eligible output.
- [ ] Fabricated transaction, txid mismatch, output mismatch, and insufficient
  confirmations/value.
- [ ] Spent, unknown, stale-view, delayed, kicked, and timed-out probes.
- [ ] Authenticated witness differing from packet-supplied witness.
- [ ] Historical missing transaction distinguished from transient backend
  failure.
- [ ] Ordinary tip advance that does not invalidate an otherwise current job.

### Agent tests

- [ ] Probe, eligible-pending, and inflight caps independently.
- [ ] Outpoint reservation and duplicate fleet submissions.
- [ ] Unqualified replacement cannot cancel qualified inflight work.
- [ ] One qualified successor per ship.
- [ ] Stale job id, request token, public context, receipt, and save/load callback.
- [ ] Public-first, confidential-first, and interleaved scanner/verifier results.
- [ ] Snapshot/scry filtering and self-sponsor fallback.
- [ ] Timeout and infrastructure failure release slots without a fraud verdict.

### Aqua

- [ ] Build suite-C Groundwire comet fixtures with the canonical shared codec.
- [ ] Exercise real Ames -> Jael -> `%gw-btc` -> Jael -> Ames flow.
- [ ] Inject delayed asynchronous light-client vectors; do not require an
  external Bitcoin node in this pass.
- [ ] Cover eligible promotion, protocol-invalid rejection, policy ineligibility,
  transient deferral/retry, stale callback, and public/confidential race cases.
- [ ] Record exact commands, runtime/pill, commits, and results in the PR.

Future E2E:

- [ ] Run the real `%light-client` with a sidecar `bitcoind` in regtest mode.
- [ ] Test real confirmations, spends, reorgs, and remote re-attestation there,
  not by adding reorg machinery to `%gw-btc`.

## 11. Explicitly out of scope for this pass

- Production monetary thresholds or a general policy language.
- Complex Taptrees.
- Confidential sponsors.
- General sponsorship revocation.
- A Bitcoin full-node client or reorg engine inside `%gw-btc`.
- `%urb-watcher` migration or versioned state migration.
- Guaranteed recovery after a true agent nuke.
- Sidecar-regtest E2E.

## 12. Definition of done for the userspace PR

- [ ] Shared state and packet codecs are frozen and vector-compatible with
  Causeway and the companion Urbit spec.
- [ ] Only an economically authenticated, reserved current head enters the
  historical queue.
- [ ] Full state—not update replay—drives the Jael result.
- [ ] Local-light-client facts are the only Bitcoin truth consumed by the
  confidential verifier.
- [ ] Queue bounds, deadlines, deduplication, successor behavior, and stale
  callback rejection pass adversarial tests.
- [ ] Public/confidential races resolve by custody-proven state.
- [ ] Vector-backed Aqua covers the real async kernel/userspace path.
- [ ] Deferred kernel work and policy choices remain clearly scoped rather than
  being approximated inside the agent.
- [ ] The branch compiles and every existing relevant unit/Aqua suite passes,
  with reproducible evidence recorded.
