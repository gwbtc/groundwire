# `%gw-btc` confidential-comets waypoint state

Date: **2026-07-29**

This document is the factual handoff between existing code and the next
revision. Read it with the normative
[SPEC](confidential-comets-agent.md) and actionable
[TODO](confidential-comets-agent-todo.md). When code and SPEC disagree, this
document explains the disagreement; it does not silently make the code
normative.

## 1. Git waypoint

The Groundwire worktree was clean when this waypoint was audited.

| Role | Branch/ref | Commit |
|---|---|---|
| Original first-pass base | `cyc/groundwire-agent` at the merge waypoint | `17453457a7b17b444e9831d7ca39c90154569990` |
| Adversarial implementation | `agent/gw-btc-adversarial` | `0fc1493180c6db9e6275a189fe9639c6201db430` |
| Target observed after nine follow-up fixes | `origin/cyc/groundwire-agent` | `2e2407c1225d8d564d403d7ca09ce66820b58e8f` |
| Common ancestor of the two current lines | — | `17453457a7b17b444e9831d7ca39c90154569990` |

At the audit, the adversarial line contained one commit absent from the target,
and the observed target contained nine commits absent from the adversarial
line. A `git fetch origin --prune` completed on 2026-07-29 and left the target
ref at `2e2407c`. A fresh implementation branch must still record its actual
base rather than assuming a historical waypoint is remote HEAD.

The documentation waypoint should exist identically on the adversarial branch
and on a fresh branch from the fetched target. It is a planning bridge, not a
request to rebase the adversarial implementation. The paired fresh branch is
`agent/confidential-comets-waypoint`.

## 2. Revision map

### 2.1 Previous: first working target base

Commit `1745345` merged the first working `%gw-btc` confidential verifier. Its
model was:

- direct `btcio` transaction fetches;
- an indexed synchronous shortcut plus an asynchronous confidential custody
  walk;
- an untagged oldest-first `[txid height reveal]` locator log;
- complete but compact `%state [life key sponsor]` commitments; and
- limited queue/lifecycle protection.

This established the basic Ames/Jael/Gall path and custody-walk shape, but left
several security and concurrency properties implicit.

### 2.2 Current target: first pass plus team fixes

The observed target `2e2407c` adds nine focused fixes:

1. current sync-start height;
2. only public state sponsors are accepted;
3. confidential comets default to self-sponsorship;
4. sponsor representation becomes `(unit @p)`;
5. spawn/state commitments must be self-enacted;
6. life regression is rejected;
7. genesis correctly spends the funding satpoint named by `dat`;
8. verified confidential tip sats are watched for movement; and
9. the move-log handler's slog is corrected.

Strengths to retain:

- state commitments rather than update replay;
- canonical fieldwise `dat` parsing;
- combined-genesis direction;
- self-enactment and nonregression checks;
- correct self-sponsor projection; and
- recognition that confidential tips need freshness monitoring.

Known limitations relative to the next SPEC:

- it still uses direct RPC rather than local `%light-client`;
- its state is versioned (`state-0/state-1`) despite the breach deployment;
- every accepted writ can launch a Khan walk without the staged economic gate;
- it fetches the complete history before checking the current economic head;
- the committed state is not yet the full normalized Jael state plus sequence;
- tip movement is logged, but no remote re-attestation path exists;
- the state document/spec on that branch predates all nine fixes; and
- sponsor consent text and sponsor-validation code have diverged.

### 2.3 Current adversarial: concurrency and verifier hardening

Commit `0fc1493` replaced the target verifier with a local `%light-client`
adapter and added substantial pure, adapter, and agent tests. It introduced:

- readiness gates for public snapshot and light-client state;
- a 1,024-entry pending bound and 16-job active bound;
- monotonic jobs, request tokens, context/chain epochs, and deadlines;
- canonical decode/re-encode checks;
- private confidential indexing and snapshot filtering;
- public/confidential block-result reconciliation; and
- explicit transient handling for timeouts and missing facts.

Strengths to carry into the next implementation:

- local `%light-client` boundary and vector-test architecture;
- hard admission/concurrency bounds;
- job identity, supersession, deadlines, and stale-callback rejection;
- separation of transient infrastructure failures from fraud verdicts;
- private/public snapshot filtering; and
- the expanded adversarial test inventory.

Superseded or incorrect choices:

- `dat` became `(mat domain) + jam(sont)`, diverging from Causeway and the
  target's canonical field encoding;
- confidential state is reconstructed by replaying event/update-shaped leaves;
- the old untagged oldest-first `xtr` format remains;
- packets enter `pending` before proving an economically qualified current
  head;
- the adapter fetches the start and every log transaction before `/tx-out`;
- a global chain epoch invalidates jobs on ordinary best-tip advances, pulling
  reorg/chaintip policy into `%gw-btc`; and
- latest-request-wins by ship can let a cheap replacement interfere with more
  expensive admitted work.

The adversarial commit should therefore be mined for safety properties and
tests, not rebased wholesale onto the target.

### 2.4 Next: state-based economic admission

The next revision combines the target's correct state/genesis semantics with
the adversarial line's local-light-client, queue, and async hardening, then
changes the evaluation order:

```text
bounded Ames reassembly
        -> packet-local state-bearing-head preflight
        -> constant-size light-client economic probe
        -> local policy decision and outpoint reservation
        -> bounded historical custody queue
        -> final receipt/UTXO recheck
        -> Jael result
```

The expensive queue no longer contains arbitrary self-attestations. It contains
only economically authenticated heads.

## 3. Accepted decisions

These are settled for the next revision.

### Identity, lifecycle, and persistence

- The agent and PKI domain are `%gw-btc`.
- The network breach removes any need for `%urb-watcher` migration.
- Persisted `%gw-btc` state is unversioned for this release.
- A true nuke may lose private state; operators own reboot/snapshot handling.
- The duplicate `%anex`/Jael `%base` issue is a separate Urbit PR, not a reason
  to add userspace migration machinery.

### Bitcoin and state semantics

- `dat` uses the canonical Causeway field encoding, including explicit 256-bit
  txid and `mat`-encoded `vout`/`off`.
- Commitments represent complete states, not updates.
- A state contains the full normalized Jael state plus a sequence number.
- Suite-C passes inside a committed state have `xtr=0`; the outer pass is
  compared after the same normalization.
- Genesis is combined: the spawn commitment spends the `dat` funding satpoint
  and commits the initial state.
- The current first-contact head must be state-bearing.
- The release uses one combined Taproot leaf; complex Taptrees are later work.
- A missing explicit sponsor projects to the ship itself.
- Only public sponsors are supported initially; confidential sponsorship and
  generalized revocation are out of scope.

### Verification and economics

- `%gw-btc` relies on a local `%light-client`; it does not own full-node query
  or Bitcoin-reorg algorithms.
- The mandatory inline head transaction is checked before historical fetches.
- Its state must attest the exact current key used by the self-attestation.
- The unique matching P2TR output supplies the candidate outpoint and value.
- Inclusion, confirmations, exact output, and unspentness come from an atomic
  light-client fact scoped to a chain view.
- Economic thresholds belong to a separate policy engine, but wire shape and
  evaluation order must permit UTXO-, fee-, and commitment-depth policies.
- Only a policy-qualified receipt may reserve an outpoint and enter the
  historical queue.
- Source/lane metadata is transport defense-in-depth, not correctness or
  admission authority.

### Scheduling and visibility

- Hard queue/concurrency limits and deadlines remain necessary.
- Job ids and captured context remain necessary for async callback safety.
- Ordinary chaintip movement must not globally invalidate every active job.
- The latest custody-proven state wins whether first observed publicly or
  confidentially.
- An unqualified new packet cannot cancel a qualified active job.
- Test vectors simulate asynchronous `%light-client` fetching in this pass.
- `%light-client` plus sidecar-regtest E2E is future work.

## 4. Why economic admission changes the queue recommendation

The original 1,024-entry bound limited memory but did not make filling it
expensive. A fleet of syntactically valid fake packets could occupy pending
work and cause transaction fetches before any scarce resource was proved.

The state-bearing head reverses that asymmetry. Packet-local work derives the
head transaction, complete state, current signing key, exact commitment output,
and nominal value. One short light-client lookup then proves that output is
confirmed and unspent. Only after this proof is the job allowed to reserve a
historical slot.

If local policy requires minimum value `V`, 1,024 simultaneous jobs require
1,024 distinct live outputs and approximately `1,024 * V` locked capital. Fee
requirements can add burned cost, but only when `%light-client` authenticates
prevout values and policy avoids crediting one batched fee many times.

The small admission-probe pool still needs bounds: attackers can fabricate raw
transactions and force negative lookups. Its records are constant-size and
short-lived, however, unlike a two-minute custody walk. This is the remaining
place for LRU, TTL, txid/outpoint deduplication, and optional transport-lane
fairness.

“Zero queue/bandwidth cost” therefore means no historical-queue occupancy and
no raw-head transaction fetch. It does not mean zero parsing, reassembly, CPU,
or light-client lookup cost.

## 5. Public/confidential races

The race is not “public traffic versus private traffic”; it is two asynchronous
claims about one custody history.

A public scanner may establish a spawn while a confidential job is in flight.
A confidential packet may also extend a state newer than the public scanner has
processed. Granting unconditional priority to either source is wrong:

- public provenance does not prove every later state; and
- a private packet is not authoritative until its custody and current outpoint
  are verified.

The next scheduler captures the public-state context at launch and keys the job
by ship, head outpoint, and state hash. Completion rechecks that context and
reconciles both claims through their common custody prefix. It does not replace
an active job because a newer-shaped packet arrived. At most one independently
eligible successor may wait.

The “epoch” retained from the adversarial work is thus an application-context
generation for stale async callbacks, not a locally invented Bitcoin-reorg
epoch. Chain-view validity comes from `%light-client` receipts.

## 6. Open architectural options

These options are not license for an implementer to choose silently. Their
decision tasks are in the TODO.

### Sequence behavior for identical checkpoints

- **Option A:** increment sequence for every state-bearing checkpoint. This
  gives simple strict ordering but treats an unchanged-state custody checkpoint
  as a semantic revision.
- **Option B:** increment only when Jael state changes; identical checkpoints
  retain the sequence and are ordered by custody. This keeps sequence semantic
  but requires an explicit same-sequence/same-state rule.

The SPEC currently permits both safely: sequence may not regress, and differing
states at one sequence are invalid.

### Public sponsor consent

- **Public-existence-only:** a ship may name any publicly known sponsor; that
  sponsor may ignore routing. This matches the current target and has the
  smallest wire format.
- **Relationship consent:** a non-self sponsor signs a message binding protocol
  version/domain, sponsor, child, and state or sequence. “Domain-separated”
  means the signature cannot be replayed as another protocol message;
  “external sponsorship” means `sponsor != child`.

If consent is selected, exact message bytes, key suite, expiry/reuse, and
placement must be shared cross-repository. Explicit revocation remains out of
scope either way.

### Short evidence profile

- Always include only the full current head and reference genesis.
- Require the full spawn commitment transaction when the log has at most a
  measured threshold.
- For completely fetch-free genesis, additionally supply or authenticate the
  funding transaction/output named by `dat`.

The recommendation is a deterministic short profile measured against the full
2,048-byte response budget. A hardcoded `~5` threshold is premature because
full-state/key-history size is variable. The working sizing hypothesis is that
ordinary one-life states can fit an inline head transaction, an inline spawn
transaction, and roughly three or four compact `[txid height reveal]`
references. Golden encodings must confirm or reject that hypothesis before it
becomes a profile rule.

### Fee attribution

- Conservatively divide a transaction's authenticated fee among qualifying
  outputs.
- Reserve the fee-bearing txid for one identity.
- Make current UTXO value mandatory and defer fee policy until the light-client
  interface exposes an adequate authenticated fact.

The third option is the minimal release path. Full fee credit per batched output
is never acceptable.

### One commitment per block

- Treat additional same-height commitments as protocol-invalid.
- Accept them but grant policy depth credit to at most one per ship per height.

The second option is recommended initially because it gives the Sybil-resistance
benefit without unnecessarily ossifying valid custody behavior.

### Policy-result mapping to Jael

`%gw-btc` needs distinct invalid, ineligible, defer, and stale results. The
existing Jael interface may not express every distinction without making a
policy miss look like permanent fraud. The userspace result algebra can be
implemented first, but its vane mapping is a cross-repository release decision.

## 7. Kernel dependencies and ownership

### Ames

Two-fragment first contact requires bounded anonymous Mesa reassembly. It must
cap total response size, fragments, concurrent assemblies, and lifetime before
the packet reaches `%gw-btc`. Source metadata is useful only at this pre-proof
layer.

### Jael/Ames re-attestation

When a watched confidential outpoint moves, `%gw-btc` can identify the old pass
as stale but cannot discover the remote successor by itself. A remote
re-attestation request needs a modest kernel path from Jael to Ames and back to
the existing `%writ` flow. This is separate from `%jael-anew`, which refreshes
the local ship's own pass.

### Jael `%anex`

The desired lifecycle is idempotent restoration of the same agent/path
registration without allowing a conflicting path. The earlier local `%base`
fix is not part of the Groundwire PR and must be handled in a separate Urbit
change. Groundwire documentation must not describe it as already merged.

### Published companion Urbit split

The previously uncommitted kernel and Aqua work is now separated into five
reviewable Urbit PRs. This ledger is descriptive; none is merged merely because
it is published.

| PR | Branch / base | State | Groundwire relevance and current evidence |
|---|---|---|---|
| [urbit#59](https://github.com/gwbtc/urbit/pull/59) | `agent/cc-ames-malformed-domain` -> `agent/confidential-comets-waypoint` | ready | fail-closed malformed suite-C domain parsing; fresh kernel and Ames/Mesa malformed cases pass without observed `%aqua-crash` |
| [urbit#60](https://github.com/gwbtc/urbit/pull/60) | `agent/cc-jael-anex-rewatch` -> `agent/confidential-comets-waypoint` | ready | exact duplicate `%anex` restores the retained watch; both focused Jael tests pass |
| [urbit#61](https://github.com/gwbtc/urbit/pull/61) | `agent/cc-jael-bane-auth` -> `agent/confidential-comets-waypoint` | draft | revocation/authorization experiment; its direct persisted `%5` mold edit is unsafe, and recovery/lifecycle/source semantics remain open |
| [urbit#62](https://github.com/gwbtc/urbit/pull/62) | `agent/cc-test-compat` -> `agent/confidential-comets-waypoint` | ready | fixture/API provenance is recorded; Dawn and representative legacy Aqua pass, with baseline-red Ames/full-suite failures disclosed |
| [urbit#63](https://github.com/gwbtc/urbit/pull/63) | `agent/cc-aqua-attestation` -> `agent/cc-test-compat` | draft | real Ames -> Jael -> fake `%gw-btc` -> Behn -> Jael -> Ames scaffold; packet/proof vectors are intentionally obsolete pending revision 2 |

The first #63 audit caught a mismatched comet/suite/seed inventory before the
attestation path; `cd96be4e84` supplies the missing suite-C entries. Its
post-fix matrix is still validation evidence for a draft, not a reason to treat
jammed-satpoint or boolean verdict fixtures as protocol authority.

The archived development pier is outside both repositories at
`/Users/armitage/r/gwbtc/s/dev`. It is an operational artifact, not a source or
test fixture.

## 8. Test state

The adversarial branch contains a large useful suite for its own implementation:
pure verifier tests, local-light-client adapter tests, and agent queue/race
tests. Those tests are design input, not proof that the next state-based packet
or current target passes.

The target's nine follow-up commits changed verifier semantics without changing
its stale protocol document. This waypoint makes no fresh compile/Aqua claim
for either diverged line. Before implementation resumes, record exact commands,
runtime/pill, commit hashes, and results in the TODO or PR description.

For the present phase, deterministic vectors are the intended external boundary.
The future E2E stack includes `%light-client` and a sidecar Bitcoin regtest node.

## 9. Fresh-agent reading order

1. Read the normative SPEC in this directory.
2. Read this STATE document to understand branch divergence.
3. Read the TODO and do only the selected phase.
4. Read the companion Urbit SPEC/STATE/TODO before changing shared molds or
   Jael/Ames interactions.
5. Inspect both `0fc1493` and the fetched target; port individual invariants and
   tests rather than rebasing one implementation over the other.
6. Do not implement policy thresholds, reorg handling, sponsor consent, or a
   new kernel task unless the corresponding open option has been resolved.
