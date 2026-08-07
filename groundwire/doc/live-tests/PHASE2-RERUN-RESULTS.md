# Phase 2 adversarial matrix + Phase 5 rekey/`%stale`, re-run on current code

Date: 2026-08-07. Live mainnet, real comets, **no transaction broadcast, zero
sats spent.**

The suite re-run of 2026-08-06
([`SUITE-RERUN-RESULTS.md`](SUITE-RERUN-RESULTS.md)) was targeted — 14 of 72
numbered tests — and explicitly did not attempt 2.2–2.16 or 5.1–5.5. But
`bf90840` changed **verdict classification**: which failures are fraud (snub),
which are stale (demote), and which are undetermined (no verdict at all).
Phase 2's adversarial matrix is the live-fire suite for exactly that surface,
and it had not run since Phase 2 itself, against much older code. This run
closes that.

Companion to [`PHASE2-RESULTS.md`](PHASE2-RESULTS.md) (2026-08-04, the original
matrix); every divergence from it is flagged and reasoned about.

---

# Headline — three classifications changed, all three for the better

| # | case | Phase 2 (2026-08-04) | this run | is the change right? |
|---|---|---|---|---|
| **2.8** | tip already spent | **INVALID**, `[XX] tip-unspent`, snub | **STALE**, `[..] tip-unspent`, `%stale-card`, **no snub** | **Yes.** A moved identity sat is not fraud. Snubbing it blocked the replacement attestation — the exact failure that cost a comet its sponsor on 2026-08-05. |
| **2.9** | foreign protocol kelvin | negative verdict → **snub** | **SILENCE** — no verdict, no snub, one named log line | **Yes.** Phase 2 flagged this as a design question and recommended silence; `+foreign-kelvin` now implements it. A kelvin bump no longer partitions the network by mutual snub. |
| **2.11** | snapshot names an unknown sponsor | **INVALID**, `[XX] sponsor-known` + 2 more, snub | **UNDETERMINED**, `[??] sponsor-known` **sole** failure of 44, no verdict, no snub | **Yes**, and proved retryable: the *identical packet* that read UNDETERMINED on k3 read **VALID** five minutes later once k3 had learned the sponsor. Ignorance is not evidence. |

Everything else held its class. **Nine cases are fraud-class and snub, four are
non-destructive, and no case moved in the wrong direction.**

Two new findings, both about classification, neither previously recorded:

- **A comet replaying its own genuine but OLDER attestation is classed FRAUD
  and snubbed** — `[XX] tracked-prefix` — even though the three other failing
  checks in the same verdict (`tip-unspent`, `tracked-tip`, `life-monotonic`)
  all say *stale*. Reproduced on two verifiers, two subjects and both desk
  versions. **This is reachable by an honest comet with no attacker involved**
  (§Finding 1). **Fixed since, in `cdaf7cf`** — staleness is now forgiven by
  degree; see §Finding 1.
- **`sponsor-known` is satisfied by a CONFIDENTIALLY verified sponsor**, not
  only by a publicly indexed one. `OPERATIONS.md` §5.2 and `ops/README.md` both
  assert in bold that it is not. The agent carries two different definitions of
  "known-public" and the sponsor check uses the broader one (§Finding 2).

---

## What was under test, and on which desk

| | |
|---|---|
| verifiers | **k2** `~namfyn-…` (N2 `159.223.141.63`), **k3** `~hacsut-…` (N3 `206.189.188.16`) |
| subjects | **C1** `~havnyl-…`, **C3** `~ligdes-…` — real mainnet comets, ships stopped, identities on chain since block 961.055/961.059 |
| desk, most of the run | `hd/cc-landing@bf90840` — what the previous run deployed and what was on all three ships |
| desk, k3 for the last block of tests | `hd/cc-landing@167e143` (i.e. **including `25f0a1d`**), deployed here |
| kernel | `hd/cc-kernel@6d9b3a4643`, `gw-cc-kernel-solid-DOS.pill`, unchanged |
| chain tip | 961.370 → 961.378 during the run |
| spent | **0 sats.** No transaction was built, signed or broadcast. |

### The desk the rig was running was not the branch tip, and that matters exactly once

The ships were carrying `bf90840`. The branch tip `167e143` contains one later
desk commit, **`25f0a1d`** (*"a decision nobody can see is indistinguishable
from a wedge"*), which is about **reporting**, not classification. Verified
arm-by-arm rather than asserted:

```
stale-checks  unknown-checks  abort-of  abort-class  check-class
classify      unknown-verdict stale-verdict  fail-result  run-checks
prefix-chain  tracked-ok      report      derive-tip   spawn-of
      -- all IDENTICAL between bf90840 and 167e143
lib/lc-attestation.hoon -- no arm changed at all
```

(`refusal-class` reads as changed only because the extractor swallows the
comment block that follows it; the arm body is identical.)

So **every verdict class below holds for the branch tip.** What `25f0a1d`
changes is that four `%jael-writ` exits which were completely silent now
announce themselves — and my run had already measured all four as silent
before I discovered the commit existed. So the tip desk was deployed to k3 and
those five cases were re-run against it (§"The four silences, closed").

---

# Phase 2 — the adversarial matrix

**How each vector was built.** Every packet is a real suite-C pass rebuilt
around a mutated custody log, using causeway's own encoders
(`build_xtr_atom` → `append_xtr_to_ring` → `derive_pass_from_ring`) — no
encoder was reimplemented. Every txid, height, blind and snapshot comes out of
the comets' **own on-chain xtr**, cued from the artifacts. Each rebuilt pass
was checked on-ship to fingerprint to the intended `@p` before use. The
builder is committed as [`ops/gwvec.py`](../../../ops/gwvec.py) so this is not
lost again — Phase 2's `genvec.py` was.

**Why C1 and C3 rather than k1/k2/k3.** A verifier that already holds an
anchor for a subject runs three extra `tracked-*` checks, and any mutation of
entry 0 breaks them too — which is why half of Phase 2's results carry an
extra unexplained `[XX] tracked-prefix`. C1 and C3 had never been verified by
k2 or k3, so **most rows below fail for exactly one named reason**, which is
what makes them readable. C3 also carries `sponsor=~` at every life, so
`sponsor-known` is not in the way.

Legend: **E2E** = packet poked into the live `%gw-btc`, verified against that
ship's own from-genesis light client. Verifier is k3 unless stated.

| # | what was fed | verdict class | snub | failing checks | vs Phase 2 |
|---|---|---|---|---|---|
| 2.1 | *(genuine attestation — re-run 2026-08-06, six ordered pairs VALID)* | VALID | no | — | held |
| 2.2 | C3's log + **C1's real spawn tx** (`2c66c654…` @961.055) appended as a custody hop | **INVALID** (fraud) | **YES** | `[XX] derive-tip` — a one-check verdict, aborted before the scan | same class |
| 2.3 | C3's **genuine** pass poked under **C1's** `@p` | **INVALID** (fraud) | **YES** | agent gate, no report | same class |
| 2.4 | C3 entry-0 life 1→2, entry-1 life 2→1 | **INVALID** (fraud) | **YES** | `[XX] entry-0-commitment` `[XX] entry-1-commitment` `[XX] entry-1-life-order` | same |
| 2.5 | C3 entry-1 `rift` 0→1 (state key no longer matches the on-chain output; life still monotonic) | **INVALID** (fraud) | **YES** | `[XX] entry-1-commitment` — **sole** failure | same |
| 2.6 | one bit flipped in C3 entry-0's `blind` | **INVALID** (fraud) | **YES** | `[XX] spawn-commit` — **sole** failure | same |
| 2.7 | 1.025-entry custody log | **INVALID** (fraud) | **YES** | bound gate, before the thread | same |
| **2.8** | **C1 truncated to entry 0**: life 1, sponsor absent, tip `2c66c654…:0` — **provably spent at 961.130** | **STALE** | **NO** | `[..] tip-unspent` — **sole** failure | **CHANGED — fraud → stale** |
| **2.9** | identical pass, `dat` carries protocol **kelvin 8** (→ a different `@p`, `~dopled-…`) | **SILENCE** — no verdict at all | **NO** | none | **CHANGED — fraud → silence** |
| 2.10 | continuity not through input 0 | *not reachable E2E* | — | subsumed by `%derive-tip`: it rejects the identical condition first, and `+abort-class` classes it fraud **because** it duplicates `entry-N-continuity` | Phase 2 ran it PURE and got `[XX] entry-1-continuity`; same class |
| **2.11** | **C1's genuine 3-entry log**, whose snapshot names sponsor `~ligdes-…`, unknown to k3 | **UNDETERMINED** — no verdict emitted | **NO** | `[??] sponsor-known` — **sole** failure, 43 of 44 `[ok]` | **CHANGED — fraud → unknown** |
| 2.12 | `xtr = (jam ~)` — a present but canonically empty chain | **INVALID** (fraud) | **YES** | agent gate | same |
| 2.13 | `xtr = 0` — the public onboarding packet | **SILENCE** | **NO** | none | same, and still cleanly distinct from 2.12 |
| 2.14 | `dat = 0`, not a decodable `+mat` | **dropped in ames**, before jael | n/a | `+pass-pki-dom` → `~` | same |
| 2.15 | three identical writs, 2 s apart (verifier **k2**) | one verdict only | no | two `dropped: a verification is already in flight`, `/x/inflight` = 1 throughout | same |
| 2.16 | does the liveness scan start **at** the tip's own block? | — | — | `%gw-btc-lc-scan-clean … from=961.129`, and 961.129 is the block containing the tip transaction. A same-block spend is therefore visible | mechanism confirmed; see note |

### 2.8 in full — the classification change that matters most

C1's own life-1 attestation, every byte of it true when it was minted:

```
%gw-btc-lc-scan-spent  tip 0x2c66.c654…:0  height=961.130
%gw-btc: attestation for ~havnyl-… is STALE (out of date, not fraud)
  [ok] chain-nonempty … [ok] entry-0-life-order … [ok] tip-scanned
  [..] tip-unspent
  [ok] tip-p2tr  [ok] pass-key  [ok] sponsor-known  [ok] tracked-prefix
%gw-btc: attestation for ~havnyl-… is STALE, not invalid
  (its identity sat has moved; demoting to alien, never snubbing)
```

Note `[ok] tip-scanned` beside `[..] tip-unspent`: the three-valued split is
doing its job. The scan **succeeded** and **proved the outpoint spent** — which
is staleness — as distinct from a scan that could not be evaluated, which
would be `[??] tip-scanned` and silence. Phase 2's `2.8b` recorded
`VERDICT INVALID / [XX] tip-unspent`, and `2.8c` (undeterminable) recorded the
*same* INVALID. Those two outcomes are now three, and only the middle one is a
finding about the peer.

### 2.11 in full, and the retry that proves it

44 checks, 43 `[ok]`, one `[??] sponsor-known`, no verdict, no snub. Then, on
the same verifier, with nothing changed but k3's knowledge:

| step | on k3 | C1's verdict |
|---|---|---|
| 1 | k3 has never seen `~ligdes-…` | **UNDETERMINED**, `[??] sponsor-known` |
| 2 | verify `~ligdes-…` on k3 → VALID, installed at life 2 | — |
| 3 | **re-poke the byte-identical C1 packet** | **VALID**, point installed at life 3, `dome=[~ %gw-btc]` |

That is the scenario in `+unknown-checks`' own comment ("the same attestation
from the same peer verified VALID two hours later with nothing changed but the
scan position") reproduced deliberately, in five minutes, on mainnet. Under the
old classification step 1 would have snubbed C1 *permanently*, and the snub
would have blocked the packet that step 3 accepted.

### 2.16 — the scan range, honestly

The property is that the BIP-158 scan starts at `tip-height`, **not**
`tip-height + 1`, so an output created and spent inside one block is caught.
Confirmed live: the scan of C3's tip (`8e713009…`, confirmed in **961.129**)
logged `from=961.129`. Phase 2 additionally scanned a real mainnet outpoint
created and spent inside block 961.055 and saw it detected; that specific scan
was **not** repeated here, because no comet in the rig has a same-block-spent
identity sat and `+scan-liveness`' range logic is byte-identical to what Phase 2
tested.

---

# The four silences, closed — `25f0a1d` verified live on the branch tip

Before I knew `25f0a1d` existed, this run measured on `bf90840`: **2.3, 2.7 and
2.12 each emit a sticky ames snub with ZERO log output**, and 2.9/2.13 are
equally mute but harmless. Determining snub-or-not required clearing the snub
set and re-poking one case at a time, because the log said nothing at all.

The branch tip was then built, staged (105 files, md5-verified identical to the
local build), deployed to k3, and confirmed in clay (`app/gw-btc.hoon` 106.186 B
= HEAD; agent state survived the upgrade). The five cases were re-run:

| # | on `bf90840` | on `167e143` | class | snub |
|---|---|---|---|---|
| 2.13 | **0 lines** | `dropped: public onboarding packet (no xtr); the block scanner resolves it, so no verdict` | silence | no |
| 2.9 | **0 lines** | `dropped: pass minted under a FOREIGN protocol kelvin` + why | silence | no |
| 2.12 | **0 lines** | `REFUSED: its pass decodes to an EMPTY custody log` + "a NEGATIVE verdict: jael %fails and ames SNUBS, stickily" + how to undo | fraud | **YES** |
| 2.7 | **0 lines** | `REFUSED: custody log of 1.025 entries is over the 1.024 cap` + idem | fraud | **YES** |
| 2.3 | **0 lines** | `REFUSED: its pass is not a readable %gw-btc attestation` + idem | fraud | **YES** |

**Classes and snubs are identical on both desks; only the diagnosability
differs.** The `dropped` / `REFUSED` split is the operator-visible half of the
same discipline the verdict classes give the peer, and it is the difference
between "this ship is idle" and "this ship just blacklisted your comet".

---

# Phase 5 — rekey and `%stale`

**Constraint honoured: nothing was spent.** k1's identity sat is down to 754
sats and the comets' sats are the only funds. Where a test as written needs a
chain operation it is marked SKIPPED and the reason is stated, and where an
*already-confirmed* on-chain state update supplies the same evidence, that was
used instead.

| # | test | result |
|---|---|---|
| 5.1 | Rekey C1 (state-update tx, life++) | **SKIPPED (chain op).** The verification half was run against C3's **real, already-confirmed** on-chain rekey (life 1→2, tx `8e713009…`, block 961.129, adds `fief`): verified from chain and installed at **life 2** on both k2 and k3, all 38 checks `[ok]`, `ames: lamp ~ligdes-… static ip .64.227.13.22 port 34343`. |
| 5.2 | Peers re-verify at the new life | **PARTIAL.** A post-rekey snapshot is fetched from chain, verified and installed at the new life (above, ×2 verifiers). Re-verification *across a live rekey* needs a spend. The previous run already covered the on-chain-state-update → peers-re-verify path end to end: k1's life 1→2 declassification was accepted and applied by k2 and k3 unprompted. |
| 5.3 | Move C1's identity sat → detected | **PASS**, on a real move: `%gw-btc-lc-scan-spent tip 0x2c66.c654…:0 height=961.130`. C1's sat really was spent in 961.130; the verifier's own filter scan found it. |
| 5.4 | `%stale` demotes to a fresh `%alien`, **no snub** | **Classifier half PASS** — see 2.8: STALE, `%stale-card`, no snub, `never snubbing` in the log. **Demotion of an already-installed point could not be produced without a spend** (below). And see **Finding 1**: the one way an installed point *can* be presented with a moved sat today produces a snub. |
| 5.5 | C1 re-attests after the move → promoted again | **PASS.** After the negative verdict of Finding 1, re-poking the correct (life-2) log on k2 returned **VALID**, 38/38 `[ok]`, point retained at life 2, `dome=[~ %gw-btc]`. **But the snub was not cleared** (below). |

### Why the anchored `%stale` demotion is not reachable without a spend

The demotion needs a point installed from a log whose tip *later* becomes
spent. Every comet in the rig is at its final on-chain life, so any log with a
spent tip is one the verifier can never have installed in the first place —
`tip-unspent` fails on the way in. The only way to create the state is to spend
an identity sat, which this run is forbidden to do. What *is* testable — the
classification of a proven-spent tip, and that it emits `%stale-card` and no
snub — was tested (2.8) and passes.

---

# Findings

## Finding 1 (real, product) — an honest comet replaying its own older attestation is classed FRAUD and snubbed

> **Fixed since this run, in `cdaf7cf`.** `+prefix-chain` is replaced by
> `$log-relation` (`%same` / `%extends` / `%behind by=n` / `%fork at=i`) and
> judged by `+anchor-ok`: a fork at any position is still fraud, **behind by
> exactly one custody entry is forgiven** and comes out `%stale` (demote to a
> fresh `%alien`, never a snub), behind by two or more is fraud. The comparison
> keys on hop *identity* (`+hop-id` / `+spawn-id`, which strip `height` and
> `start-height`), which also removes the reorg variant recorded in
> [`MATRIX-COMPLETION-RESULTS.md`](MATRIX-COMPLETION-RESULTS.md). A new
> stale-class check `tracked-lag` fires on every `%behind` so the forgiveness is
> never silent. The record below is left as it was written.

**Reproduced on two verifiers, two subjects, and both desk versions.**

Install a comet from its current log, then present an **earlier, genuine,
fully on-chain-verifiable** log for the same comet:

```
%gw-btc-lc-scan-spent  tip 0x2c66.c654…:0  height=961.130
%gw-btc: attestation for ~havnyl-… is INVALID
  [..] tip-unspent        <- stale: its sat moved
  [..] tracked-tip        <- stale: we are ahead of this log
  [XX] tracked-prefix     <- FRAUD
  [..] life-monotonic     <- stale: this is an older copy of a log we know
%gw-btc: SNUBBING ~havnyl-… on a negative %gw-btc verdict
```

| run | verifier | desk | subject | outcome |
|---|---|---|---|---|
| A | k2 | `bf90840` | C3 installed at life 2, then its life-1 log | INVALID + snub |
| B | k3 | `167e143` | C1 installed at life 3, then its life-1 log | INVALID + snub |

Three of the four failing checks are stale-class and say precisely *"this is an
old copy"*. The fourth decides the verdict, because `+classify` lets fraud beat
everything. And `tracked-prefix` cannot be right here:

```hoon
++  prefix-chain
  |=  [old=custody-log:sa new=custody-log:sa]
  |-  ?~  old  %.y
      ?~  new  %.n              ::  <- new ran out first
      ?.  =(i.old i.new)  %.n   ::  <- they diverged
      $(old t.old, new t.new)
```

The arm collapses two different facts into one `%.n`: **a fork** (the logs
disagree at some index — genuinely fraud, exactly as `+stale-checks`' comment
says) and **a strictly shorter identical prefix** (an old copy — exactly what
that comment says is *not* fraud). The check has the information to tell them
apart and does not use it: if `(prefix-chain new old)` also holds, the new log
is a prefix of the one we already verified, which is staleness, not a fork.

**This is reachable with no attacker.** `OPERATIONS.md` §6 records that a
refreshed pass "is not written back to the boot keyfile, so a reboot re-derives
from the feed and needs another `%anew` round-trip". A comet that rekeys after
minting and then restarts serves the shorter, feed-baked log — and every peer
that had already installed it at the newer life snubs it, stickily, for
presenting truthful evidence. That is the same class of failure `bf90840` was
written to eliminate, one arm further in.

Severity is bounded by how the snub then behaves — see Finding 3.

## Finding 2 (real, spec/code divergence) — `sponsor-known` accepts a *confidential* sponsor

The agent holds two different definitions of "known-public":

```hoon
++  known-public                        ::  app/gw-btc.hoon:2068
  |=  who=ship
  ?&  (~(has by unv-ids.urb-state) who)
      !(~(has in confidential) who)     ::  <- confidential comets excluded
  ==

++  verify-cards                        ::  app/gw-btc.hoon:1758
  …  (verify-lc:lca sat.req (tracked-anchor …) ~(key by unv-ids.urb-state) …)
                                        ::  ^ raw, NOT excluded
```

The peer-facing `sponsor-ok` check reads the second one, so a sponsor the
verifier has only ever verified **confidentially** satisfies `sponsor-known`.
`lib/self-attestation.hoon:617` says "A named sponsor must exist as a **public**
point"; `OPERATIONS.md` §5.2 and `ops/README.md` both say, in bold, that naming
a confidential comet as sponsor leaves the sponsee permanently UNDETERMINED and
"silently unreachable forever". Live, it does not.

Discovered as a natural experiment and then confirmed deliberately: the same C1
packet was UNDETERMINED on k3 (which did not know C3) and VALID on k2 (which
had just verified C3 **confidentially** — `~ligdes-…` is in k2's
`/x/confidential` and **not** in `/x/points`).

Which behaviour is wanted is a design call. That one agent disagrees with
itself about it, and that the runbook documents the opposite of what ships, is
not.

## Finding 3 (real, kernel; bounded) — a positive verdict does not clear a snub

> **Fixed since this run, in the kernel's `f68a547b2b`:** `+sy-sybl`'s `%full`
> branch now calls `(sy-snub %deny %del ~[her])`, so a positive verdict lifts an
> existing snub. Read it as a safety net, not as self-healing: `%hear` still
> drops a snubbed peer's packets, so the positive verdict must arrive by a route
> the snub does not block — the block scanner learning a publication, an
> operator-poked `%jael-writ`, or the mesa `%page` path, which has no snub gate
> at all and is a separate open question. The record below is left as it was
> written.

After Finding 1's snub, the correct attestation verified **VALID** and the
point was installed — and `.^(/snubbed)` still contained the peer.
`+sy-sybl`'s `%full` branch (`sys/vane/ames.hoon:11351`) funnels into `+sy-publ`
and never touches `ships.snub.ames-state`; only `%fail` writes it, and only an
operator `%snub %deny %del` clears it.

This is consistent with the design the agent's own log states ("a snub is
sticky and blocks the packet that would correct it"), so it is not a
regression. It is what makes Finding 1 unrecoverable in the field rather than
merely wrong: in this run the correcting attestation only got through because
it was **poked locally**. Over the wire it would have been dropped by the snub
it caused.

## Not a finding — 2.9 remains an open design question, as briefed

A foreign-kelvin pass is now correctly parsed as foreign, refused without a
verdict, and named in the log. What is still undecided is *which* kelvins a
verifier should advertise and accept, and how retiring one is announced
(decisions addendum §1). Recorded, not touched.

## Harness

- `ops/unsnub.py` (recovered from N1) has never worked: it calls
  `send-raw-card:strandio`, and in a `khan-eval` thread strandio's arms are
  already in the subject, so the wing resolves to nothing and the thread dies
  with a bare `%thread-fail:` and an empty tang. The bare `send-raw-card` form
  works. Since clearing a snub is the *only* recovery from a wrong negative
  verdict, a broken unsnub tool is worse than none.
- Determining "was a snub emitted" for a silent disposition requires an empty
  snub set beforehand — an already-snubbed subject makes the observation
  vacuous. That cost one full pass of 2.7/2.12/2.13 and is why they were re-run
  one at a time with `unsnub` in between. `25f0a1d` removes the need entirely.
- **`gwsup.sh`'s singleton lock can be inherited by a fork that outlives its
  purpose.** `ensure_sidecar` starts the sidecar as
  `( cd "$PIER" && … setsid nohup /opt/gw/bin/tcp-sidecar . & )`. The lock is
  taken with `exec 9>` and is therefore *not* close-on-exec, so the subshell
  inherits fd 9. On k2, after the supervisor restarted the sidecar at 04:47Z, a
  second `/bin/bash ops/gwsup.sh k2 35354` (ppid 1, the sidecar's parent)
  remains, and `/proc/318115/fd/9 -> /opt/gw/.gwsup-k2.lock`. A BSD `flock` is
  held by the *open file description*, so the lock survives the real
  supervisor's exit for as long as that subshell lives. Consequence, if it ever
  fires: `gwsup.sh k2` refuses to start with "a supervisor for k2 is already
  running" while **no** supervisor is running — the failure mode the flock was
  added to prevent, wearing the flock's own error message. Not induced here
  (that would mean stopping a live supervisor); reasoned from the inherited fd,
  which is directly observable. Distinct from the SUITE-RERUN note about
  `wedge_remote.sh` duplicating a supervisor: this one is `gwsup.sh`'s own
  subshell. Fix is one line — close fd 9 in the subshell. Until then: if a
  supervisor refuses to start, check `ls -l /proc/*/fd/9` for an orphan holding
  `/opt/gw/.gwsup-<pier>.lock` before believing the message.

## Infrastructure

- **k2 took one `loom: external fault`** (vere SIGSEGV) mid-run at 04:47Z. The
  supervisor detected VERE-DOWN, relaunched, restarted the sidecar, ran the
  wedge recovery and re-seeded — all unattended, no state loss, and the
  verification in flight completed on replay. Same class and same outcome as
  the previous run's k1 incident.
- **k1 was found with its light client stalled**: `synced=%.n`,
  `live-earth-peers 0`, 26 blocks behind, and it had been that way since before
  this run started. Fail-closed and correct (it would have *held* every
  attestation rather than misjudge one), but k1 could not have verified anyone.
  The documented recovery — `&kill-peer-connections` then re-seed 2×25 from the
  `x49.` pool — restored it to `is-synced %.y` at the tip within minutes.
- Stray, unrelated to Groundwire: k3's log carries an Ethereum JSON-RPC error
  (`logs by range`, `range 100000 exceeds limit of 10000`) from `%urb-snapshot`.
  Noted only so it is not mistaken for a `%gw-btc` fault.

---

# Rig left in this state

| | k1 | k2 | k3 |
|---|---|---|---|
| ship | up, life 1 | up, life 1 (replayed clean after its SIGSEGV) | up, life 1 |
| light client | **synced** (was stalled; recovered) | synced | synced |
| desk | `bf90840` (98.404 B) | `bf90840` (98.404 B) | **`167e143` (106.186 B)** |
| `/x/points` | `~` | `{~talryg}` | `{~talryg}` |
| `/x/confidential` | `{~namfyn, ~hacsut}` | `{~havnyl, ~ligdes, ~hacsut}` | `{~namfyn, ~havnyl, ~ligdes}` |
| snubbed | **`~`** | **`~`** | **`~`** |
| desks mounted | none | none | none |

Every snub this run caused was cleared. Two real comets (C1, C3) are now
verified peers of k2 and k3 — that is a true fact about the chain, arrived at
through the normal path, and was left in place.

C1/C2/C3's own piers (`r1`/`r2`/`r3`) were never started. **k1, k2 and k3 are
now on different desk revisions**; if the rig is to be uniform again, k1 and k2
want `167e143` staged the same way k3 got it (`ops/gwctl.py desks`,
`/opt/gw/desks/gw-tip`, ~90 s per ship, state survives).

## Money

Nothing was spent. No transaction was built, signed or broadcast. k1's
identity sat is still 754 sats, C2's 1.544, C3's 1.288, and the wallet's
~17.822-sat change UTXO was not touched.
