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
- **Fief scope.** A fief is for ships with STATIC addresses — sponsors and
  other infrastructure (its `%turf` form is literally DNS). A reliable
  sponsor should commit a fief on-chain. Roaming/confidential comets set
  `fief=~` and are reached via their sponsor plus dynamically learned
  lanes; their transient addresses never touch the chain. A comet with
  neither sponsor nor fief is not cold-reachable (reply-lane peers only) —
  legal, but clients should warn.

## 3. Stale attestations: demote to alien, never snub

When `%gw-btc` observes a confidential comet's tip outpoint spent (conf
registry), the identity's attestation is stale. This is not fraud and MUST
NOT produce `%fail` (a snub would block the replacement packet).

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
the boot keyfile.

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
