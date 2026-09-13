# `%gw-btc` confidential-comet verification

Status: **next-revision normative specification**. The implementation branches
described in [confidential-comets-agent-state.md](confidential-comets-agent-state.md)
are waypoints, not authorities where they disagree with this document.

This is the userspace companion to the generic kernel specification in
`gwbtc/urbit/pkg/arvo/doc/spec/pluggable-comet-pki.md` and to the `%gw-btc`
protocol specification in [confidential-comets.md](confidential-comets.md). The desk is
`%groundwire`; the Gall agent and PKI-domain tag are both `%gw-btc`.

The words **MUST**, **MUST NOT**, **SHOULD**, and **MAY** describe protocol or
release requirements. Unresolved architectural choices are kept out of the
normative path and recorded in
[confidential-comets-agent-todo.md](confidential-comets-agent-todo.md).

## Revision history

- **2026-07-29 — state-and-economic-admission waypoint.** Full state snapshots,
  rather than event replay, become normative. The immutable tweak is restored
  to the canonical Causeway field encoding. A state-bearing current transaction
  is evaluated before historical fetches, and only a light-client-authenticated,
  policy-qualified economic head may enter the expensive custody queue. Reorg
  interpretation belongs to `%light-client`. Two-fragment first contact is an
  explicit kernel dependency. This is the first entry in the compact normative
  history; earlier implementation history is preserved in the STATE document.

## 1. Scope and deployment assumptions

This protocol verifies a Groundwire comet whose Bitcoin state is intentionally
not present in the receiver's public PKI index. Ames verifies the packet's
self-attestation signature, extracts `%gw-btc` from the suite-C tweak, and asks
Jael to route the pass to the local `%gw-btc` agent. `%gw-btc` verifies the
Bitcoin-backed state asynchronously and returns a Jael point.

The deployment starts after a comet network breach. Consequently:

- the agent is named `%gw-btc`; there is no `%urb-watcher` migration;
- `%gw-btc` persisted state is unversioned for this release;
- the operators will reboot and snapshot the default sponsor; and
- a true Gall-agent nuke may lose the private confidential index. Recovery from
  that exceptional event is re-attestation or a future private-state recovery
  mechanism, not a migration layer in this agent.

For this release `%gw-btc` MUST use a local `%light-client` agent. That agent is
also expected to serve `%spv-wallet` and is responsible for obtaining and
authenticating Bitcoin data from full nodes. Direct full-node RPC and Bitcoin
reorg logic are not part of `%gw-btc`'s confidential-verification boundary.

## 2. Trust boundaries and principal invariants

There are four distinct boundaries:

1. **Ames** bounds and authenticates the self-attestation transport.
2. **`%gw-btc`** validates Groundwire encodings, state, custody, and scheduling.
3. **`%light-client`** authenticates Bitcoin inclusion, UTXO, transaction, and
   chain-view facts.
4. **The local policy engine** decides the economic thresholds required to
   consume an expensive verifier slot.

The next revision is organized around these invariants:

- Every state commitment is a complete normalized Jael state, not an update or
  a Groundwire-event delta.
- The latest custody transaction in an admissible first-contact packet is
  state-bearing and commits the key that authenticated that packet.
- One expensive verification job is backed by one distinct, confirmed,
  current, identity-bound, policy-qualified Bitcoin outpoint.
- Public indexing and confidential self-attestation are evidence sources, not
  precedence classes. The latest custody-proven state wins.
- `%gw-btc` rejects stale asynchronous results by job identity and captured
  context. It does not independently interpret Bitcoin reorganizations.

## 3. Suite-C pass and immutable tweak

A Groundwire comet uses crypto suite `%c`. The immutable `dat` is:

```hoon
(can 0 (mat %gw-btc) [256 txid] (mat vout) (mat off) ~)
```

Here `[txid vout off]` is the funding satpoint consumed by the combined spawn
commitment transaction. It is not the output created by that transaction: a
transaction cannot commit its own txid without a recursive hash.

The `dat` codec MUST:

- use the shared Causeway/Groundwire fieldwise encoding above, not a jammed
  `$sont` tail;
- use the same txid byte order as Causeway, `%light-client`, and the Bitcoin
  transaction codec;
- require the canonical `mat` encodings and consume the complete atom; and
- be pinned by shared cross-repository golden vectors.

`dat` is part of the suite-C key tweak, permanently binding the comet name to
the `%gw-btc` domain and initial satpoint. `xtr` is not part of that tweak and
may change without changing the comet name.

The `xtr` atom MUST be the canonical jam of the decoded versioned Groundwire
attestation envelope. Appended data, alternate noun shapes, noncanonical txid
widths, or noncanonical re-encodings MUST fail before asynchronous work begins.

## 4. State commitments

### 4.1 Complete state, not updates

A confidential state commitment contains:

- the attesting ship;
- a sequence number; and
- every field required to reconstruct that ship's Jael point, including rift,
  current life, complete key state, effective sponsor, and fief.

The exact shared Hoon mold remains a codec-freeze task, but its semantics are a
snapshot. Verifiers MUST NOT derive a confidential point by replaying `%keys`,
`%escape`, `%adopt`, `%detach`, or other event-shaped Groundwire mutations.
Those operations may remain meaningful to the public indexer, but they are not
the confidential state wire contract.

Every suite-C pass stored inside a committed snapshot is normalized with
`xtr=0`. This prevents a state from recursively committing the packet that
reveals it. The current committed key MUST equal the **entire** outer
self-attestation pass after applying the same normalization, including the key
that authenticated the self-attestation; comparing only the messaging `cry` is
insufficient. On success the Jael result may carry the actual submitted outer
pass as the current key.

State sequence numbers MUST NOT regress along an authenticated custody history.
Two different normalized states at the same sequence are conflicting and MUST
fail closed. An identical state MAY be checkpointed again at the same or a
higher sequence; transaction order remains authoritative between identical
states. Whether policy grants economic-depth credit to such a checkpoint is a
separate decision. The state sequence is distinct from Jael life and rift.

### 4.2 Combined genesis and current head

The genesis commitment is combined: the first custody transaction spends the
funding satpoint named by `dat` and commits the initial complete state in its
single Groundwire Taproot leaf. A separate state-update phase is not required.

This release uses a single combined Tapscript leaf. More complex Taptrees are a
future protocol revision.

Pure custody moves with no reveal remain legal inside a history. They do not
change state. However, after a pure move, the ship cannot use the new output for
first-contact admission until it publishes a new state-bearing checkpoint at
the current tip. Thus an admissible packet's final transaction MUST:

- be the current custody tip;
- carry a complete state reveal;
- commit the current self-attestation key; and
- create the outpoint used as the economic admission token.

### 4.3 Sponsor semantics

Only public sponsors are supported initially. A committed sponsor MUST be the
ship itself or a ship already established through the receiver's public PKI
view. An absent explicit sponsor projects to the ship itself, not to its
structural sponsor.

An explicit sponsor that is not yet present in the receiver's authoritative
public snapshot MUST NOT be silently rewritten to self. The check is deferred
while public-index readiness or freshness is indeterminate; once evaluated
against the current snapshot, the state is unsupported/policy-ineligible until
that sponsor becomes public. This is not a cryptographic-fraud verdict.

Confidential sponsors and generalized revocation are out of scope. A public
sponsor may stop routing for an abandoned child. Whether a non-self, public
sponsor must also provide relationship-specific cryptographic consent is an
architectural choice recorded in the TODO; the implementation MUST NOT invent
a consent format locally before that choice is made.

## 5. Version-one `xtr` evidence envelope

The next wire envelope is tagged and versioned. Its semantic shape is:

```hoon
+$  gw-xtr-v1
  $:  tag=%gw-btc
      version=%1
      head=head-evidence
      history=(list custody-ref)
      origin=origin-evidence
  ==

+$  head-evidence
  [height=@ud raw-tx=octs opening=taproot-opening]

+$  taproot-opening
  [internal-key=@ux leaf=[version=@ux script=octs]]

+$  custody-ref
  [txid=@ux height=@ud opening=(unit taproot-opening)]

+$  origin-evidence
  $%  [%head]
      [%ref txid=@ux height=@ud opening=taproot-opening]
      [%inline height=@ud raw-tx=octs opening=taproot-opening]
  ==
```

This is a semantic mold: the final field names/order and the exact transaction
`octs` mold MUST be frozen together with Causeway and shared golden vectors.
Implementations MUST NOT ship a locally convenient incompatible variant.
`taproot-opening` is the internal key plus Taproot leaf opening; the complete
v1 state is decoded from the Groundwire state record inside its leaf script.
It is not the previous compact `%state [life key sponsor]` mold.

The envelope obeys these rules:

- `head` is mandatory, inline, state-bearing, and names the current tip.
- The head txid is derived from `raw-tx`; a redundant claimed txid is forbidden.
- The state encoded in the head opening is self-enacted by the attesting ship.
- The verifier reconstructs the single-leaf Taproot output and requires exactly
  one matching output. Its `vout` and value are derived, not trusted fields.
- `history` is newest-to-oldest, excludes the head and origin, has bounded
  length, contains unique txids, and has structurally monotonic claimed heights
  that are authenticated before they receive custody or policy credit.
- `origin` always includes the combined spawn reveal. A deterministic short-log
  profile additionally requires the complete spawn commitment transaction.
- If head and origin are the same transaction, `origin=[%head]` is mandatory so
  the transaction and opening are encoded exactly once.
- Sender-selected mixtures of equivalent inline and referenced evidence are
  forbidden. The profile is determined by protocol rules and total packet size.
- There is no compatibility requirement for the previous untagged envelope
  after the network breach.

The short-log threshold is intentionally not guessed from the current `~5`
estimate. It MUST be selected from measured, canonical fixtures containing the
complete pass/open-packet overhead and realistic one-, two-, and multi-life
state snapshots. The complete serialized first-contact response, not merely
`xtr`, must fit the kernel limit.

The supplied transaction's non-witness serialization authenticates its txid,
inputs, outputs, scripts, and values. Its witness is not authenticated by txid.
The full custody verifier MUST use `%light-client`-authenticated witness data
rather than trusting a packet-supplied witness.

## 6. Ordered verification and economic admission

Verification is deliberately staged so unqualified packets cannot occupy the
historical-walk queue.

### 6.1 Ames transport preflight

Ames authenticates the pass signature and bounds anonymous reassembly before
delivering `%writ` to Jael. Two-fragment first contact requires the kernel work
described in section 10; until that dependency lands, a two-fragment v1 packet
is not deployable.

### 6.2 Packet-local preflight

Before entering any asynchronous queue, `%gw-btc` MUST:

1. Enforce the complete byte, entry, and reveal bounds.
2. Decode and re-encode `dat` and `xtr` canonically.
3. Require suite `%c`, domain `%gw-btc`, and `fig(pass) == who`.
4. Require a nonempty, uniquely ordered evidence log and a state-bearing head.
5. Decode and canonically re-encode the inline head transaction.
6. Derive its txid and parse the complete normalized state.
7. Bind the state's ship and current key to the outer self-attestation pass.
8. Reconstruct the single-leaf Taproot output and derive its unique `vout`.
9. Read the candidate output value.
10. Reject any locally impossible or nominally below-floor candidate before
    requesting historical transaction data.

This stage uses only the bounded packet. A fabricated high-value transaction
can reach the next constant-size probe, but cannot enter the expensive queue.

### 6.3 Light-client economic probe

`%gw-btc` next asks the local `%light-client` for one atomic admission fact at
a named chain view:

- canonical inclusion and authenticated block height;
- confirmation count;
- the exact output script and amount;
- current unspentness; and
- optionally, an authenticated transaction fee or conservatively allocated fee
  credit.

The returned output MUST equal the inline transaction output exactly. Raw
transaction bytes alone cannot prove inclusion, current unspentness, or fee;
input values live in prevouts. A packet Merkle branch would prove inclusion
against headers but still not unspentness, so it is not required in v1.

Successful probing produces a short-lived receipt containing at least:

```text
policy-version, chain-view, ship, state-sequence, state-hash,
tip=[txid vout], value, confirmations, optional fee-credit, expiry
```

The preflight/probe pool MUST be separate from the historical-verification
queue, small, concurrency-bounded, TTL-bounded, deduplicated by transaction or
outpoint, and equipped with a short negative cache. This bounds fake txid and
fake high-value-transaction probes.

### 6.4 Policy evaluation

The protocol produces authenticated facts; a separately scoped local policy
engine sets thresholds. It may require:

- a minimum current UTXO value;
- a minimum confirmation count;
- a minimum uniquely attributable fee expenditure; and
- a minimum number or span of distinct authenticated commitment heights.

Only a policy-eligible receipt reserves its outpoint and enters the expensive
custody queue. Protocol-invalid, policy-ineligible, transient/deferred, and
stale outcomes are distinct internal results. A policy miss or unavailable
light client is not cryptographic fraud and MUST NOT be cached as such.

### 6.5 Historical custody verification

For an eligible receipt, `%gw-btc`:

1. validates the combined genesis endpoint;
2. fetches only evidence not supplied inline;
3. authenticates each asserted height and transaction through `%light-client`;
4. walks the custody sat from the `dat` funding outpoint to the admitted head;
5. checks key-path custody transitions and every disclosed commitment;
6. enforces state self-enactment, complete-state validity, and sequence rules;
7. reconciles any previously verified prefix rather than redoing it; and
8. rechecks the receipt/current outpoint before applying the Jael result.

The walk stops on its first failed link. For an already verified ship, only the
new suffix should be fetched and checked when the cached prefix still matches.

## 7. Economic accounting

### 7.1 UTXO value

The mandatory v1 economic fact is a distinct current unspent output. If policy
requires value `V`, 1,024 concurrently admitted identities require 1,024
distinct reserved outpoints and approximately `1,024 * V` locked value. The
value is recoverable rather than burned, but cannot be reused concurrently at
one receiver.

Reservations are keyed by outpoint and identity-bound state. A single outpoint
MUST NOT underwrite multiple jobs. A transaction may create outputs for several
ships, but each ship needs its own independently qualifying state-committing
output. If the admitted output is spent during verification, the job becomes
stale and is evicted or retried only after a newly eligible head is presented.

Cross-receiver reuse of the same prepared fleet is an acknowledged limitation;
receiver-specific on-chain commitments would prevent synchronous first contact.

### 7.2 Fees

Fees are burned cost but are not derivable from a raw latest transaction without
authenticated prevout values. When `%light-client` supplies a fee, one batched
transaction's full fee MUST NOT be credited independently to every output.
Policy may allocate fee conservatively per output, or reserve a fee-bearing txid
for one identity. The exact allocation is a policy decision.

Historical UTXO values MUST NOT be added as if they were independent cost; the
same capital can roll forward. Authenticated, nonduplicated fees and the current
reserved UTXO are the meaningful economic quantities.

### 7.3 Commitment depth

Policy SHOULD count at most one commitment per ship per authenticated block
height. This creates real coordination-time depth and prevents same-block churn
from manufacturing age. It does not serialize different ships: an attacker may
advance many identities in parallel, so additive resistance across identities
still comes principally from distinct UTXOs and fee allocation.

For v1 this is a policy-credit rule, not a Bitcoin-validity rule. Hidden or
omitted commitments receive no depth credit. Claimed sequence gaps and claimed
heights receive no credit without authenticated distinct transactions.

## 8. Scheduling, stale callbacks, and source races

There are three bounded stages: admission probes, economically eligible pending
jobs, and active historical walks. Existing numerical caps such as 1,024
pending and 16 active are deployment parameters, not wire constants.

The scheduler MUST:

- reserve one economic token per pending or active job;
- identify work by `[ship tip-outpoint state-hash]` plus a monotonic job id;
- keep at most one active job and one independently eligible successor per ship;
- never let an unproven replacement cancel an economically admitted active job;
- reject callbacks whose job id, request token, captured public-state context,
  or receipt is no longer current;
- deduplicate completed and negative work by economic head;
- use explicit FIFO/age ordering within any bounded policy-score classes, not
  Hoon map iteration order; and
- retain hard caps and deadlines even when economic policy is enabled.

The public index and confidential verifier can observe the same ship in either
order. Arrival source is not authority. A public observation MUST NOT cancel a
paid confidential walk merely because it is public, and an unverified private
tail MUST NOT overwrite a public or confidential point. The implementation
reconciles both against custody, state sequence, and head outpoint; the latest
custody-proven state wins regardless of visibility.

Ephemeral admission-source metadata MAY be used for Ames fragment fairness,
probe-pool quotas, and diagnostics. It MUST NOT be persisted into Jael state or
used to decide protocol truth.

## 9. `%light-client` and reorganization boundary

`%light-client` owns header validation, canonical-chain selection, transaction
authentication, UTXO interpretation, and reorganization handling. Every
admission receipt is scoped to its returned chain view and expiry.

`%gw-btc` MUST NOT invalidate every active job merely because the best-block id
advanced. It may ask `%light-client` whether a receipt is still usable, and it
must handle a stale/indeterminate answer by releasing or retrying work without
issuing a fraud verdict. Reorg-specific state machines belong in the light
client, not this agent.

Watching a verified tip for a later spend is useful for freshness but does not
by itself reveal the successor transaction or pass. Triggering a remote peer to
re-attest requires a Jael/Ames kernel path and is a separate dependency.

## 10. Ames two-fragment dependency

Mesa data fragments are 1 KiB. Supporting a complete Groundwire attestation of
up to two data fragments is not a one-line relaxation of the present
first-contact assertion: normal Mesa reassembly assumes a known peer.

The kernel dependency MUST provide a narrowly bounded alien-attestation
reassembly path with:

- at most two data fragments and at most 2,048 bytes of complete serialized
  response;
- a fixed small anonymous reassembly pool;
- short TTL and LRU eviction;
- bounded root/auth state and ordered fragment requests;
- no cue until complete assembly; and
- the existing name, pass, signature, and root checks after assembly.

The 2,048-byte cap applies to the complete jammed response, including
open-packet/pass/cage/vase overhead, not just `xtr`. The legacy plaintext
single-shot path remains single-fragment unless separately redesigned.

## 11. Jael interaction and lifecycle

On initialization `%gw-btc` sends `[%anex /writs]` to Jael and watches the
local `%light-client` readiness interface. It accepts `%jael-writ` only from the
local vane path and emits `%verdict` only for the matching current job.

The agent does not need an explicit rename migration or versioned load path.
The exact duplicate-`%anex` behavior after desk/Gall lifecycle transitions is a
kernel `%base` concern. This document does not claim that the required Jael fix
is already merged; it is intentionally tracked as a separate Urbit PR.

`%jael-anew` is the local ship's refresh dual. Returning an old pass whose head
is no longer current is forbidden. Discovering the local successor, extending
the evidence envelope, and asking a remote peer to re-attest are separate
operations and must not be conflated.

## 12. Required test boundary

The release uses deterministic vectors instead of a Bitcoin sidecar.

Pure tests MUST cover:

- canonical `dat` and versioned `xtr` vectors shared with Causeway;
- complete-state normalization, sequence conflicts, combined genesis, and
  state-bearing-head enforcement;
- inline transaction parsing, txid derivation, unique output reconstruction,
  and realistic complete packet sizes;
- custody continuity, malformed witnesses/reveals, bare moves, and cached
  prefix/suffix verification; and
- economic fact and fee-allocation edge cases.

Adapter tests MUST drive the asynchronous `%light-client` interface with fixed
watch acknowledgements, facts, failures, delays, and stale chain-view receipts.

Agent tests MUST cover bounded admission and historical queues, outpoint
reservation, deduplication, successor behavior, deadlines, stale callbacks,
public/confidential races, snapshot filtering, and the self-sponsor fallback.

Aqua MUST exercise the real Ames -> Jael -> Gall -> Jael -> Ames asynchronous
path using suite-C Groundwire comet fixtures and delayed deterministic
light-client vectors. A later end-to-end layer will use the real `%light-client`
and a sidecar `bitcoind` in regtest mode.

## 13. Deliberately out of scope

The following are not release blockers for this userspace pass unless promoted
in the TODO:

- a production economic-policy implementation or fixed monetary thresholds;
- complex Taptrees;
- confidential sponsors;
- generalized sponsorship revocation;
- a full-node or Bitcoin-reorg implementation inside `%gw-btc`;
- migration from `%urb-watcher` or versioned `%gw-btc` state;
- private-index recovery after a true app nuke; and
- regtest/sidecar E2E coverage.
