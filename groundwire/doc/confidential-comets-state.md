# Confidential Comets: Implementation State

Updated: **2026-07-29**

This is the handoff document for humans and fresh agents. It records what is
merged, what was preserved in the legacy source stash, and what protocol revision
has been selected next. It is descriptive; normative behavior lives in the
kernel specification `gwbtc/urbit/pkg/arvo/doc/spec/pluggable-comet-pki.md` and
the protocol specification [confidential-comets.md](confidential-comets.md), and
unfinished work lives in
[confidential-comets-todo.md](confidential-comets-todo.md).

> This document and its companion TODO used to live beside that specification in
> `gwbtc/urbit/pkg/arvo/doc/spec/`. They describe the Groundwire `%gw-btc`
> domain, which base arvo does not implement and must not document as its own,
> so they now live here in the Groundwire repository.

## Status vocabulary

- **Merged** means reachable from the named remote target commit.
- **Dirty** means present only as an unstaged worktree change. It may be a good
  fix, but it is neither reviewed nor part of the target.
- **Current userspace** means the working `%gw-btc` implementation in the
  Groundwire repository, not the final wire protocol.
- **Selected next** means architecturally chosen for the next revision but not
  yet implemented or mergeable.
- **Open** means an architect decision or measured fact is still required.

## Git waypoint

Repository: `https://github.com/gwbtc/urbit`

| Item | Value at this waypoint |
|---|---|
| legacy source checkout | `cyc/cc-draft` |
| legacy source commit | `e523267ea2d6986a7f3cff442bab2e78c48e72a5` |
| legacy source upstream | none |
| paired fresh branch | `agent/confidential-comets-waypoint` |
| merged pull request | [#57, Cyc/cc draft](https://github.com/gwbtc/urbit/pull/57) |
| pull-request target | `gw/next/kelvin/408` |
| remote-tracking target after 2026-07-29 fetch | `802b386a4387126d8a2b14fd674329613b5db460` |
| target parents | `1a24aa9c94be9f48489f0d794147463d3e3529f0`, `e523267ea2d6986a7f3cff442bab2e78c48e72a5` |

The target is the merge commit whose second parent is the legacy source tip. A
fresh continuation branch must start from
`origin/gw/next/kelvin/408` at `802b386a43`, not from the stale local
`gw/next/kelvin/408` branch, which pointed at `0fa01fa1a2` during this audit.
The paired documentation/implementation waypoint branch is
`agent/confidential-comets-waypoint`.

At the beginning of this documentation waypoint there were no staged changes,
15 modified tracked files, and six untracked status entries. `dev/` is an
untracked development pier, including a mounted `%base` mirror. On 2026-07-29
it was archived intact outside the repository at `/Users/armitage/r/gwbtc/s/dev`;
its binaries, LMDB state, logs, and mounted files are not authoritative and
must not be committed. Documentation commits must stage their paths explicitly.

After the split PRs were published, the remaining `cyc/cc-draft` source changes
were preserved in the named stash
`6db5a3f0c9c582822d57a0999d66e6f6bb60d4ba` (`stash@{0}` when created):
`pre-split confidential-comets worktree; extracted to PRs 59-63`. This stash is
local recovery provenance, not another candidate branch or protocol authority.

## Follow-up branch and pull-request split

The follow-up was deliberately split instead of rebased or landed as one
patch. Every independent kernel branch below starts from this waypoint; the
Aqua prototype is stacked only on the compatibility branch.

| Workstream | PR / head | Pull-request base | Review state | Scope and checked status |
|---|---|---|---|---|
| Ames malformed suite-C domain | [#59](https://github.com/gwbtc/urbit/pull/59) / `agent/cc-ames-malformed-domain` | `agent/confidential-comets-waypoint` | ready | guarded domain parse; fresh kernel boot and malformed Ames/Mesa cases pass without `%aqua-crash` |
| Jael duplicate registration | [#60](https://github.com/gwbtc/urbit/pull/60) / `agent/cc-jael-anex-rewatch` | `agent/confidential-comets-waypoint` | ready | exact same-agent/same-path `%anex` re-watch; both focused Jael tests pass |
| Jael domain retirement/authorization | [#61](https://github.com/gwbtc/urbit/pull/61) / `agent/cc-jael-bane-auth` | `agent/confidential-comets-waypoint` | draft | prototype tests pass, but the in-place `%5` mold change is unsafe and recovery/lifecycle policy is unresolved |
| Test-suite compatibility | [#62](https://github.com/gwbtc/urbit/pull/62) / `agent/cc-test-compat` | `agent/confidential-comets-waypoint` | ready | Dawn and representative legacy Aqua pass; existing full-suite/Ames expectation failures are disclosed in the PR |
| Async attestation Aqua prototype | [#63](https://github.com/gwbtc/urbit/pull/63) / `agent/cc-aqua-attestation` | `agent/cc-test-compat` | draft | Ames accept/reject pass; life-2 hangs before a second verifier request; Mesa rerun and canonical vectors remain open |

The malformed-attestation Aqua regression travels with the Ames fix rather
than being duplicated in the stacked Aqua prototype. The compatibility branch
is the sole base of the Aqua branch; the three kernel branches remain
independent so each kernel issue can be reviewed and merged separately.

The draft Jael retirement branch is intentionally not represented as safe to
merge. Its current tombstone adds a field directly to persisted Jael `%5`
state without a `%5` to `%6` migration, has no unban/recovery task, and lacks
positive authorization and live Gall/Clay lifecycle coverage. The direct
state edit is incorrect for existing `%5` snapshots. The earlier decision not
to version fresh `%gw-btc` state does not apply to Jael's already-persisted
kernel state.

The draft Aqua branch preserves a useful end-to-end control-path scaffold,
not a revision-2 protocol fixture. Its current valid vectors use the
superseded jam-based satpoint encoding and a boolean synthetic `xtr`; the
hard-coded comet identities must be regenerated after the direct `dat` and
state/economic envelope are frozen. It does not yet model `%light-client`
facts, public/confidential ordering, economic admission, queue bounds, epochs,
or callback cancellation.

The first independent Aqua audit of #63 found that its two new comet names and
seeds had no corresponding suite entries, causing `+zip` to fail before the
attestation path. Commit `cd96be4e84` adds the two `%c` entries. This is useful
evidence for retaining cross-list length checks. After that fix, Ames accept
and reject reach `done` without `%aqua-crash`; life-2 establishes life 1, then
hangs with `%fine-mismatch our=[0 2] her=[comet 0 1]` and never emits a second
fake-verifier request. Mesa was not rerun under the synchronous time bound.
These results and the canonical-vector replacement keep #63 explicitly draft.

## Revision 0: merged kernel integration

Urbit PR #57 merged the following sequence:

| Commit | Content |
|---|---|
| `694b097032` | verified initial Jael/Ames confidential-comet kernel draft |
| `7cebc27879` | Clay `%tire` liveness, one-to-one domains, and `%anew` refresh plumbing |
| `e523267ea2` | variant-B fetch locators and the state-commitment design |
| `802b386a43` | merge of the above into `gw/next/kelvin/408` |

Merged behavior includes:

- suite `%c` recognition while retaining the vanilla suite `%b` path;
- domain extraction from the head of immutable pass tweak data;
- forwarding unknown/higher-life suite-C attestations from Ames through Jael
  to a same-named registered Gall verifier;
- asynchronous `%writ-response` facts and `%sybl` gifts;
- promotion through the ordinary Jael/Ames public-key path on success and
  additive snubbing of failed alien peers;
- Jael state `%5`, including the `%4` to `%5` migration, domain registry,
  verifier watch, and `%sybl` subscribers;
- `%anex`, `%writ`, `%sybl`, `%gost`, `%ghul`, `%bane`, and `%anew` task/gift
  plumbing;
- desk suspension/revival through Clay `%tire`;
- suite-C higher-life handling in both Mesa and legacy Ames open paths; and
- a manual local self-attestation refresh path through `%anew`.

The merged design document described complete state commitments and a compact
fetch-based `[txid height reveal]` locator log. The userspace implementation
that followed did not preserve every one of those semantics, which is why the
next revision is a deliberate protocol waypoint rather than a small cleanup.

## Revision 1: diverged userspace checkpoints

There are two current Groundwire lines, neither of which is the selected final
protocol.

The refreshed target `origin/cyc/groundwire-agent` at `2e2407c` is the team's
working first pass. It uses direct `btcio` fetches, an untagged
`[txid height (unit reveal)]` locator list, combined genesis, and compact
state-snapshot commitments. Its nine post-base fixes add public-sponsor
checking, self-sponsor fallback, self-enacted `%spawn`/`%state` leaves, life
nonregression, correct funding-satpoint genesis, and confidential-tip movement
watching. It retains versioned agent state, lacks the selected staged economic
gate, and fetches history before checking the live head.

The adversarial branch `agent/gw-btc-adversarial` at `0fc1493` is one commit
from the older common base and nine target commits behind. It replaces direct
RPC with a local `%light-client`, bounds pending and active work, adds deadlines
and job/context/chain epochs, maintains a private filtered index, and supplies
substantial pure, adapter, and Gall tests. However, it diverges from Causeway's
canonical `dat`, reconstructs confidential state by replaying event/delta
leaves, and treats ordinary best-tip changes as agent-wide invalidation.

The target supplies the state/genesis correctness baseline. The adversarial
line supplies safety properties and test designs to port selectively. It does
not make sense to rebase or transplant the adversarial commit wholesale.

The archived Urbit source stash contains Aqua fixtures that drive the real
Ames -> Jael -> Gall -> Jael -> Ames asynchronous route without `bitcoind`.
Production `%light-client` plus regtest E2E remains future work.

## Archived source Urbit worktree

The following changes originated in the now-stashed `cyc/cc-draft` worktree and
are separate from merged PR #57. The branch split above is their actual
publication disposition; this section records provenance rather than implying
that all source hunks belong together.

### Jael candidates

The stashed `pkg/arvo/sys/vane/jael.hoon` source contained:

- exact duplicate `%anex` as an idempotent request to re-emit the retained
  Gall watch, while rejecting a changed path;
- a durable `%bane` tombstone;
- explicit leave of the registered Gall watch during `%bane`; and
- rejection of unauthorized, post-bane, or already-queued udiff facts.

`tests/sys/vane/jael.hoon` was untracked and tests those choices. Exact-duplicate
idempotent re-watch is selected for a separate Jael `%base` PR; the tombstone
still requires architect approval. Neither may be described as target behavior
merely because tests exist.

### Ames hardening candidate

`pkg/arvo/sys/vane/ames.hoon` changes domain extraction from a crashing direct
`+rub` to a guarded parse and rejects a malformed suite-C tweak before it
creates alien peer state or sends a Jael `%writ`. The change is mirrored in
Mesa and legacy Ames intake. This is a concrete fail-closed bug fix, but it is
still unmerged.

### Aqua and compatibility work

The source stash also contains:

- a deterministic test `%gw-btc` agent and suite-C fixtures;
- new `attestation-hi`, `attestation-reject`, `attestation-life-2`, and
  `attestation-malformed` Aqua scenarios (formerly untracked);
- route-table and synthetic-Azimuth fixture updates so legacy tests use suite-B
  comets and the new tests use prototype suite-C identities;
- Jael-point `fief` propagation and Dawn/test shape updates; and
- supporting Pharos file-copy/start/snub assertions.

These edits were split by purpose into #59–#63. The stale `sur/stealth.hoon`
sketch and any superseded combined hunks remain only in the recovery stash.

## Revision 2: selected next architecture

The next revision has the following accepted direction.

### Identity and state

- The immutable tweak is the harmonized direct encoding
  `(can 0 (mat %gw-btc) [256 txid] (mat vout) (mat off) ~)`.
- On-chain leaves commit complete per-identity Jael/`urb-snapshot` state plus a
  sequence number, not deltas.
- The suite-C pass inside committed state is normalized with `xtr=0`.
- Genesis is a combined custody and initial-state commitment.
- Only public sponsors and a single combined Taproot leaf are in scope.
- A no-sponsor state converts to an udiff using the ship itself as the fallback.
- The latest custody-proven state wins regardless of whether it arrived from a
  public index or confidential first contact.
- A first-contact attestation has a state-bearing current unspent tip whose
  key is the self-attestation signer.

### Packet and admission

- The complete first-contact response is bounded to two 1 KiB Mesa data
  fragments.
- The current-tip transaction and current full-state reveal are mandatory
  inline evidence.
- Historical entries remain bounded compact references; a deterministic short
  profile may inline genesis-side transaction evidence after size measurement.
- Packet-local work derives the txid, state commitment output, `vout`, key
  binding, and nominal value before any historical fetch.
- A small bounded `%light-client` probe then authenticates inclusion,
  confirmations, the exact output, unspentness, and chain view.
- Only a policy-qualified, uniquely reserved outpoint enters the expensive
  custody queue. UTXO value is the primary concurrent-capital defense; safely
  attributable fee and block-depth facts may strengthen it.
- One commitment per ship per block is at least a policy-credit limit. Whether
  it is a wire-validity rule remains open.

### Async correctness and ownership

- The existing token/epoch machinery is retained and extended so verification
  identities are tied to ship, state, economic outpoint, policy receipt, and
  chain view; stale callbacks cannot overwrite newer state.
- One active job and at most one independently qualified successor exist per
  ship; alternate locator tails cannot repeatedly buy work with the same head.
- `%light-client` owns reorg and chaintip interpretation. `%gw-btc` consumes
  atomic facts, detects a stale receipt, and defers/retries.
- Economic thresholds and fee allocation are policy-engine scope. Wire code
  supplies authenticated facts and safety caps.
- Admission source/lane is not identity authority. It is useful only for the
  bounded anonymous fragment/probe pools and observability.
- Jael will accept an exact same-agent/same-path `%anex` as an idempotent
  re-watch and will continue to reject a conflicting path; this lands as a
  separate kernel fix.

## Legacy TODO disposition

The deleted `pkg/arvo/todo.md` has been fully absorbed as follows.

| Former item | Disposition |
|---|---|
| `%sybl`, `%gost`, `%ghul`, `%bane` tasks | merged in revision 0 |
| `%hand` registration update | deliberately removed by one-domain/one-agent design |
| `%writ-response` fact contract | merged in revision 0 |
| Gall liveness affordance | replaced by existing Clay `%tire` for desk liveness; app-nuke semantics remain open |
| additive `%snub` task | still open; retained in the new TODO |
| Ames boot `%sybl` subscription | merged in revision 0 |
| Mesa intake hardening and registration flow | merged baseline; malformed-domain fail-closed fix is ready in #59 but unmerged |
| mirror confidential handling into legacy Ames | merged in revision 0 |
| `%full`/`%fail`/`%lost` result handling | merged in revision 0 |
| choose pass payload size | superseded by the selected bounded hybrid/economic-head revision |
| kernel attestation-request entry point | local/manual `%anew` merged; remote stale-peer request remains open |
| `%urb-watcher` handler | renamed `%gw-btc` and implemented in Groundwire |
| causal liveness via `%tire` | merged; nuke versus suspend is still distinct |
| one-to-one domain/agent and removal of `%hand` | merged in revision 0 |
| immutable `dat` plus mutable `xtr` anatomy | retained, with `dat` encoding corrected and harmonized in revision 2 |
| `%anew` response flow | merged but log extension/automatic triggering remains open |
| variant-B locator decision | retained as compact history, augmented by an inline economic head |
| state commitments | reaffirmed for revision 2; current userspace delta encoding must change |
| per-agent nuke affordance | open architect decision |
| automatic re-attestation | open and expanded in the new TODO |
| refreshed-pass boot-key persistence | deferred/open for a later release decision |

## Handoff cautions

- Do not conflate full on-chain identity snapshots with Gall save-state
  snapshots.
- Do not infer that a supplied raw transaction proves inclusion, unspentness,
  fee, or witness authenticity.
- Do not implement reorg handling in `%gw-btc` merely to make tests pass.
- Do not use map traversal order as queue scheduling policy.
- Do not restore the archived pier into a source tree or treat the recovery
  stash as a branch to merge wholesale.
- Do not reintroduce userspace migration/versioned `%gw-btc` state; this
  network revision begins after a planned breach.
