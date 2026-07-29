# Implications for the current implementation, and TODOs

Baseline: `agent/gw-btc-adversarial` (PR #127 head), i.e. the adversarially
hardened tree. Each item is tagged with where the work lands:
`[hoon]` this desk, `[kernel]` gwbtc/urbit, `[cw]` Causeway, `[py]`
onboarding/comet-miner, `[spec]` decision needed first.

## 1. What survives unchanged (keep, do not rewrite)

- **Pure-verify / fetch split** (`lib/self-attestation` + `lib/lc-attestation`
  + `sur/light-client`). The trust boundary, the deterministic tx-vector
  interface, fail-closed `%unknown` tx-out handling, and the strand structure
  all carry over. Only the per-entry semantics change (§3 below).
- **Agent async machinery** in `app/gw-btc`: pending/inflight bounds,
  request/context/epoch tokens, per-job Behn timeout backstopping the strand
  timeout, latest-request-wins, no-sticky-negative-verdict discipline, the
  `reconcile-block` three-way merge and `publicizing` guard, snapshot/scry
  filtering of confidential points. All of this is protocol-agnostic
  concurrency correctness and is retained.
- **`lib/taproot`** hardening (lift_x, tweak < n, infinity check). Gains one
  arm: canonical construction of the `gw` OP_RETURN leaf from a snapshot
  commitment (§3 of the spec revision), so verifier and wallet build
  byte-identical leaves.
- **`lib/gw-btc-pass`** as the single home of pass/dat codecs — the shape
  survives, the encoding inside changes (hiding `d`, §2 below).
- Bounds and constants (1,024-entry log, max-pending/inflight, timeout),
  height-ordering and tracked-prefix checks, tip-liveness requirement.
- Credential removal, Makefile desk-root packaging, test harness structure
  (deterministic vectors, light-client mock endpoint tests, agent tests).

## 2. Format layer — `dat`, pass, tweak

- **One dat format** (`mat %gw-btc` ++ 32-byte hiding commitment) replaces all
  three current encodings. Concretely:
  - `[hoon]` `+make-dat`/`+parse-dat` in `lib/gw-btc-pass` become
    commit/verify against an opening (they can no longer *decode* a satpoint
    from `dat`; parsing yields `[dom=@tas d=@ux]`). Everything that currently
    pattern-matches a spawn satpoint out of `dat` must take the opening from
    the attestation instead: `+from-xtr`, `+check-spawn`, `+public-pass`,
    `+pass-attestation`, and `urb-core`'s spawn tweak check.
  - `[py]` `gw-onboard.py` `make_tweak_expr` / `build_tweak_bytes` are still
    emitting the legacy rap-3 tweak (only renamed in PR #127; flagged in
    review). They get the real migration now, plus blind derivation. The new
    Python test pinning the rap-3 bytes is deleted with it.
  - `[cw]` Causeway's dat encoder (currently `can`/`mat` layout) migrates to
    the hiding commitment; Causeway stores/derives the blind alongside the
    seed.
  - `[all]` **One shared golden-vector file** (JSON: seed, spawn-sont, blind,
    dat, pass, leaf, root, Q, sample xtr) consumed by Hoon, TS, and Python
    test suites. The PR-126-era cross-impl vector regressed in PR #127; this
    reinstates it as a first-class artifact.
- `[hoon]` `sur/urb` custody types change: `entry` gains the
  `opening`/`blind-opening` structure; the free-form
  `reveal=[internal-key tapleaf]` shape (arbitrary script) is replaced by the
  canonical snapshot opening — the verifier constructs the leaf itself and
  never parses attacker-supplied script bytes at all.

## 3. Verifier — `lib/self-attestation`

- **Delete the event-replay machinery**: `+replay-chain`, `+replay-singles`
  (%keys/%fief/%escape/%cancel-escape/%adopt/%reject/%detach/%set-mang
  branches), the signed-%escape verification (its domain-separation and
  sponsor-availability problems die with it), the `sponsors` map argument,
  and `+parse-leaf`/`+singles`/`+find-spawn`/`+spawn-first-ok` (no committed
  sotx streams to parse).
- **Replace with snapshot resolution**: walk hops exactly as now (continuity,
  key-path, offsets — all retained); for each entry with an opening,
  reconstruct leaf/root/Q and compare to the sat-carrying output's
  scriptPubKey; require exactly one blind-opening matching `dat`; final state
  = latest opening's snapshot projected onto `point` with `sont.own` = derived
  tip. Net effect: the library shrinks substantially and no longer contains a
  state machine.
- Sponsor fields in the projected point come from wire records (§8.2 of the
  revision), not from the walk. Interim default: self-sponsor (matches the
  existing `urb-point-to-jael` fallback in the adversarial branch).
- `+derive-tip`, `+prefix-chain`, `+tracked-ok`, the check-report structure,
  and `lc-attestation`'s fetch loop survive with type-level adjustments only.

## 4. Block scanner — `lib/urb-core`, `lib/urb-encoder`, `lib/ord`

- `[hoon]` The scanner stops parsing witnesses entirely. Discovery = grep
  transaction **outputs** for the `OP_RETURN "urb"` envelope prefix. The
  envelope codec in `urb-encoder` is rewritten for the output script (the
  `btc-script` OP_IF descriptor round-trip machinery is no longer part of
  consensus parsing). The three crash sites hardened in PR #127 disappear
  rather than needing hardening.
- `[hoon]` The precommit → commit → reveal three-step collapses: a public
  spawn is **one transaction** (input 0 spends the committed satpoint's UTXO;
  output 0 carries the sat under the initial snapshot commitment; an
  OP_RETURN output carries pass + dat opening). `+find-precommits`, the
  commit/precommit fetch dance, and `+calc-precommit-sont`'s multi-tx flow
  reduce to single-tx checks mirroring the verifier's `+check-spawn`.
- `[hoon]` On-chain sotx semantics shrink to what remains on-chain: spawn and
  (public-path) snapshot updates. The `%escape`/`%adopt`/`%reject`/`%detach`/
  `%fief`/`%set-mang` on-chain handlers in `+handle-block` are deleted along
  with their opcodes in `urb-encoder`. (Note: PR #127's `%detach`
  self-sponsor fix and `%set-mang` no-crash fix land first and are then
  deleted with the branch — that is fine; merge order in the action plan.)
- `[hoon]` `lib/ord`: with inscriptions gone, the protocol no longer indexes
  third-party inscriptions at all. `insc`/`insc-ids`, co-located-inscription
  carry logic (including the `moved-ins` handling the adversarial branch
  added in `+apply-verified` / `+strip-private-sat` / `+graft-private`), and
  inscription parsing go away; `sont-map` keeps only comet occupancy. The
  `[txid off]`→`[txid vout]` deletion-keying bugfix from PR #127 is retained
  (the map survives, smaller).
- `[hoon]` `app/gw-btc`: scanner-side simplifications flow through
  (`public-spawns` unchanged in shape; `apply-verified` loses inscription
  handling). The RPC full-block path remains the public indexer's transport
  for now; its reorg/checkpoint model remains the known deferred gap from
  PR #127 pending `%light-client` integration.

## 5. Sponsorship / kernel — gwbtc/urbit (`cyc/cc-draft-2`)

- `[kernel]` Remove consent from the writ-verification contract; `%gw-btc`'s
  `%writ-response` continues to carry a `point`, whose sponsor field is now
  advisory (wire-derived or self).
- `[kernel]` Jael/Ames ingestion of signed routing records (`gw/fief`), with
  the `(life, seq)` freshness rule; packet-level sponsor-chain hints and
  ordered fallback across up to N claimed sponsors. `point:jael` is **not**
  widened; canonical sponsor = first listed.
- `[kernel]` `%anew` flow unchanged at the interface; custody-log extension
  design (open question 7) unblocks the agent-side TODO.
- `[spec]` Consent-token and routing-record grammars are in the revision;
  kernel enforcement is verify-if-present only.

## 6. Causeway `[cw]` and onboarding `[py]`

- PSBT construction per §7 of the revision: isolation send, child-key export,
  spawn PSBT (with BIP-371 `TAP_INTERNAL_KEY`/`TAP_LEAF_SCRIPT` on update
  spends), degraded plain-`tr(P)` tier, frozen-UTXO guardrail (normative).
- Blind derivation from seed; hiding-dat encoder; shared golden vectors.
- `gw-onboard.py`: single-tx spawn builder with OP_RETURN payload; drop the
  inscription/commit-reveal path.
- Causeway wallet-integration research task: matrix of PSBT signers
  (Sparrow, Core, Coldcard, Ledger/BIP-388) against the one-leaf tree —
  determines how prominent the degraded tier is in UX.

## 7. Explicitly deferred (unchanged from PR #127) or out of scope

- `%light-client` implementation itself (only `sur/` exists in-repo) and
  end-to-end regtest coverage; public-scanner reorg model.
- Stranger-discovery indexing / advertisement lookup: **out of scope even at
  spec level** (per cyc). The OP_RETURN payload formats are fixed so this can
  be added later without a format break.
- Migration of any existing on-chain names: none; breach/bootstrap.
- Sybil queue-starvation mitigation (review finding on `max-pending`): carry
  as a known residual into the new agent work, unchanged priority.
- Rotation of the leaked RPC credential (git history): operational, still
  open, unaffected by this revision.

## 8. Consolidated TODO list

| # | tag | item |
|---|-----|------|
| 1 | spec | Resolve §9 open questions 1–7 (tags registry, snapshot fields, opening placement, spawn payload, N/depth, restoration payload, anew/extension) |
| 2 | all | Shared golden-vector artifact for dat/pass/leaf/Q/xtr |
| 3 | hoon | `gw-btc-pass`: hiding dat commit/verify + blind handling |
| 4 | hoon | `taproot`: canonical `gw` leaf constructor |
| 5 | hoon | `sur/urb` + `sur/self-attestation`: snapshot/opening types |
| 6 | hoon | `self-attestation`: replace replay with snapshot resolution |
| 7 | hoon | `urb-encoder`: OP_RETURN envelope codec; shrink opcode set |
| 8 | hoon | `urb-core`: output-grep scanner; single-tx spawn; delete sponsorship handlers + inscription indexing |
| 9 | hoon | `app/gw-btc`: type flow-through; wire-record ingestion stub |
| 10 | py | `gw-onboard.py`: new tweak + single-tx spawn + OP_RETURN; delete rap-3 (and its pinning test) |
| 11 | cw | Causeway: hiding dat, PSBT flows, frozen-UTXO guardrail, signer matrix |
| 12 | kernel | routing records + sponsor-chain hints + consent-if-present; spec docs |
| 13 | hoon | Test migration: keep harness, regenerate vectors, rewrite walk tests for snapshot model |
| 14 | ops | Rotate the historical RPC credential |
