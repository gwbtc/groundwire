# Action plan synthesis

Sequencing for the OP_RETURN / snapshot / off-chain-sponsorship revision,
across the four workstreams (this desk, kernel, Causeway, onboarding). The
guiding constraint: **formats first, once, together** — the PR-126/127 cycle
showed that letting each implementation carry its own encoding produces
three-way divergence that reviews then have to catch after the fact.

## Phase 0 — settle the branch state (now)

1. **Land PR #127 into `cyc/groundwire-agent`.** The hardening is wanted; the
   two spec-level review objections (dat-format divergence, deltas-vs-%state)
   are both superseded by this revision rather than needing resolution inside
   that PR. Resolve the review with pointers to `doc/opret-revision/`; do not
   block on fixing `gw-onboard.py`'s rap-3 tweak there (it is deleted in
   Phase 2 anyway).
2. Keep PR #125 in draft; it becomes the trunk that receives the revision
   work. This branch (`cyc/opret-spec-revision`) carries the working docs and
   is the base for Phase 1+ implementation branches.
3. Ops: rotate the historical RPC credential (independent of everything).

## Phase 1 — spec decisions (cyc; small, blocking)

Resolve the open questions in `01-spec-revision.md` §9 — chiefly: tag
registry + envelope version byte, snapshot field set, opening placement
rule, spawn payload grammar, N/depth caps. Output: a short addendum commit
to `01-spec-revision.md`. Everything downstream keys off this; it is the
only serialized step.

## Phase 2 — format layer + golden vectors (first code, all repos together)

- Author the shared golden-vector artifact (one JSON: seed → blind → dat →
  pass → snapshot → leaf → root → Q → sample custody log / xtr), generated
  once, checked into all three codebases, with a generator script whose
  output is reproducible.
- Implement codecs against the vectors in the same change set:
  `[hoon]` `gw-btc-pass` + `taproot` leaf constructor;
  `[cw]` Causeway encoder + blind derivation;
  `[py]` `gw-onboard.py` tweak builder (rap-3 deleted, including the test
  that pins it).
- Exit criterion: all three suites green against the *same file*. No
  implementation-specific vectors for consensus formats from here on.

## Phase 3 — verifier + scanner (this desk; parallel after Phase 2)

- 3a. `self-attestation`: snapshot resolution replaces event replay; type
  changes in `sur/`; test rewrite on the new vectors. The lc-attestation
  fetcher and the agent's async machinery are untouched apart from types.
- 3b. `urb-core`/`urb-encoder`: OP_RETURN scanner, single-tx spawn,
  deletion of on-chain sponsorship handlers and inscription indexing.
- 3a and 3b are independent of each other; both merge into the trunk behind
  the existing test harness. GPT-style adversarial review passes both after
  they land (same playbook as PR #127, much smaller surface).

## Phase 4 — sponsorship wire layer (kernel + desk)

- Kernel: signed routing-record ingestion with `(life, seq)` freshness,
  sponsor-chain hint verification (name/key/domain per hop, depth/acyclicity
  caps), ordered multi-sponsor fallback, consent-token verify-if-present.
- Desk: emit/refresh own routing record; interim self-sponsor default
  removed in favor of wire-derived sponsor in `urb-point-to-jael`.
- Aqua coverage: attestation → promotion with sponsor chain; stale-record
  replay rejected; sponsor-loss → restoration publication (6b) → peer
  re-acquisition via filter hit.

## Phase 5 — Causeway wallet flows + onboarding

- Causeway: isolation flow, spawn PSBT, update PSBT with BIP-371 fields,
  frozen-UTXO guardrail, degraded tier; the signer-compatibility matrix
  task decides UX prominence of the degraded tier.
- `gw-onboard.py`: single-tx spawn end-to-end on regtest against the Phase 3
  scanner.

## Phase 6 — integration debt (unchanged from PR #127's deferrals)

`%light-client` implementation, public-scanner reorg/checkpoint model, full
regtest e2e including reorgs, sybil queue-starvation mitigation. These were
deferred before the pivot and remain deferred; the pivot does not enlarge
them (it shrinks the scanner's parsing surface they must cover).

## Later (explicitly parked)

- Spec synthesis: final canonical spec documents + spec-history idea-maze
  document (per cyc — after the revision stabilizes).
- Stranger-discovery indexing / advertisement protocol (out of scope even at
  spec level; formats reserved so it can be added without a break).
- Multi-sponsor as a kernel concept (only if packet-level hints prove
  insufficient in operation).

## Dependency sketch

```
Phase 0 (merge 127, rotate cred)
   └─ Phase 1 (spec decisions)  ── the only serialized step
        └─ Phase 2 (formats + shared vectors, all repos)
             ├─ Phase 3a (verifier)  ─┐
             ├─ Phase 3b (scanner)   ─┼─ Phase 5 (causeway/onboarding e2e)
             └─ Phase 4 (kernel wire layer, overlaps 3)
                                      └─ Phase 6 (integration debt)
```
