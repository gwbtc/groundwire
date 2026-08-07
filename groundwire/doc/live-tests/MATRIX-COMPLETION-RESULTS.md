# Matrix completion — every numbered test not yet exercised on the current desk

Date: 2026-08-07. Live mainnet, real comets. **No transaction was broadcast;
zero sats spent.** No comet's identity sat was touched.

The two previous runs ([`SUITE-RERUN-RESULTS.md`](SUITE-RERUN-RESULTS.md),
[`PHASE2-RERUN-RESULTS.md`](PHASE2-RERUN-RESULTS.md)) had covered 34 of the 72
numbered tests on the current code. This run takes the remainder: 1.2–1.5,
3.2/3.3/3.5/3.6, 4.1–4.3, phase 5b, 6.3–6.6, all of phase 7, and an honest
re-score of 6.7.

---

# Headline

**1. Test 6.7 re-scores as PARTIALLY — "better, and still not yes".** It was
"still no" before `25f0a1d`. That commit is a real, measurable improvement: the
three formerly-silent snubbing exits now each print cause, consequence *and*
remedy, and the `%anew` refusal now carries a reason and a job id. Judged
blind — a reader given the log and forbidden from opening the source — **two of
six substantive failure classes are fully diagnosable, one is half-diagnosable,
and three are not diagnosable at all.** The blocking gaps are structural, not
wording: **no timestamps anywhere**, no correlation id on the peer verification
path, `[..]` never defined, and `REFUSED` (snubs) vs `refused` (harmless)
distinguished **only by letter case**. Details in §6.7.

**2. Phase 7 still passes against the changed Causeway, and 7.1 has gone from
FAIL to PASS.** `c534cba`'s rewritten `build_rekey_psbt` was exercised both
ways against the live encoders: the unfunded path is unchanged (1 input, 1
output, 111 vB) and the funded path puts the identity sat at input 0, funding
strictly behind it, and **grows** output 0 to the requested target with change
last. `Q` is byte-identical with and without the funding input — the ordinal
claim the commit rests on, confirmed observationally. **7.4, the seed-loss
demonstration, is intact and was re-proved on the deployed verifier**, not just
in Python.

**3. One thing the orchestrator should know about money:** the ~17,822-sat
"wallet change UTXO" named in the brief is **not spendable with any key
material on this machine.** Its address is not derivable from
`~/gw-building/.gw-mainnet-wallet.json` under `m/86h/0h/{0,1,2}h/{0,1}/0..49`,
and the documented funding wallet `bc1ptv5pf62…` has **0 UTXOs** (funded 12000,
spent 12000). The pre-authorised throwaway mint was therefore not fundable —
and, as it turned out, not needed (§7.4).

| | |
|---|---|
| desk | `groundwire@hd/cc-landing` `9331a8d` on all three ships |
| kernel | `hd/cc-kernel@6d9b3a4643`, `gw-cc-kernel-solid-DOS.pill`, unchanged |
| chain tip | 961.445 → 961.455 during the run |
| spent | **0 sats.** Nothing was signed for broadcast or broadcast. |

---

# Deploy — all three ships to branch tip

`gwctl.py desks` to k1 and k2; k3 needed nothing.

**A correction to `PHASE2-RERUN-RESULTS.md`.** That doc records k3 as running
`hd/cc-landing@167e143` "i.e. **including `25f0a1d`**". `25f0a1d` is a *child*
of `167e143`, so that cannot be true as written — and the byte lengths settle
it: `167e143`'s `app/gw-btc.hoon` is 98.404 B, `25f0a1d`'s is 106.186 B, and
k3 was serving **106.186**. k3 was on `25f0a1d`'s desk content all along; only
the label was wrong. Desk content is identical between `25f0a1d` and the tip
`9331a8d` (the latter adds only a doc), so k3 was already at tip.

| | k1 | k2 | k3 |
|---|---|---|---|
| before (`app/gw-btc.hoon` in clay) | 98.404 | 98.404 | 106.186 |
| after | **106.186** | **106.186** | 106.186 |
| `lib/self-attestation` / `urb-core` / `gw-btc-pass` | 43.744 / 19.885 / 11.029 | idem | idem |

The desk built from the working tree is **md5 `7f237306f84a2151588c5f64020ada9b`
over 105 files**, byte-identical to what k3 already had staged — which is the
cross-check that the local build and the deployed tree are the same object.

Pre-flight, all three: `synced`, tip 961.445, `snub=[%deny ~]`, `/x/inflight`
empty, **no desks mounted**. State survived the upgrade on both ships.

---

# Phase 7 — Causeway

## 7.1 Headless spawn over SSH — **PASS** (was **FAIL**)

The Phase 6/7 run recorded this as failing badly: four blocking `input()` calls
with no flags, two of them `continue`-on-EOF, producing a pinned core writing
~110 MB/min of `  > ` until the disk filled — 11,453,252 lines / 57 MB in 31 s.

Re-measured on the current tree, with **no TTY, no `TERM`, no `DISPLAY`, stdin
at `/dev/null`**:

| case | old | now |
|---|---|---|
| `spawn generate`, no flags | infinite loop, 57 MB / 31 s | **exit 2, 1.417 bytes** |
| `spawn connect`, no `--utxo` | uncaught `EOFError` traceback | **exit 2, 549 bytes** |

and each names the flag that supplies the missing answer:

```
causeway: cannot read the seed phrase confirmation: no interactive terminal on stdin
causeway: pass --assume-saved to supply it non-interactively
causeway: cannot read a UTXO choice: no interactive terminal on stdin
causeway: pass --utxo <txid>:<vout> to supply it non-interactively
```

Then the positive case — a **complete** headless spawn, exit 0: `--xpub` as a
BIP-380 descriptor with a real master fingerprint, `--utxo`, `--signed-psbt`
pointed at a **named pipe** (the documented form), `--assume-saved`,
`--sponsor`, and the real `comet_miner`. Causeway mined a real suite-C comet
(`~navhec-fadnyl-…-daplyd`), wrote the unsigned PSBT, an external signer
(playing the hardware wallet — causeway never saw the seed) signed it and fed
the pipe, causeway extracted, wrote the proof, and called broadcast.

**Nothing was broadcast.** The broadcast POST went to a local capture stub that
proxies every GET to the real mempool.space API and *records* POSTs instead of
relaying them. The funding prevout was a real, **already-spent** historical
output of the real funding address — so the transaction is a double-spend of a
confirmed input and could not confirm even if it leaked. The captured tx
decodes correctly:

```
1 input   3046729429a1…:1
1 output  1778 sat  5120d354803b72febc08cab6864ced03c5d84263a82e80f32cb1d681a025f9ec35df
          == the proof's sat_script_pubkey_hex, 5120||Q, 32 bytes
no OP_RETURN (confidential spawn, as asked)
witness   1 item, 64 bytes -> key-path schnorr
fee       222 sat over 111 vB = 2.00 sat/vB
```

*Honest scope:* the only fact simulated is "this prevout is still unspent". Every
byte of key derivation, mining, PSBT construction, signing and encoding is real.

## `c534cba`'s rewritten `build_rekey_psbt` — **verified both ways**

The commit that the brief flagged as untested-by-Phase-7. Driven headlessly
against the throwaway comet's own spawn proof, both modes exit 0:

| | unfunded (`plain`) | funded (`--fund-xpub/--fund-utxo/--sat-target 3000`) |
|---|---|---|
| inputs | 1 — the identity sat | 2 — identity sat **first**, funding second |
| outputs | 1 | 2 — sat-carrying first, change (354 sat) **last** |
| output 0 value | 1778 → **1556** (shrinks by the fee) | 1778 → **3000** (grown to target) |
| raw size | 162 B | 312 B |
| fee rate | 2.00 sat/vB | 2.00 sat/vB |
| signatures | 1 × 64 B key-path | 2 × 64 B key-path |

Both pass the full pre-broadcast gate, with `Q` recomputed from
`[internal-key, new snapshot]` by an independent secp256k1 / BIP-341
implementation (not by calling causeway's own helper):

```
[ok] input 0 is the intended identity outpoint
[ok] funding sits AFTER input 0            [ok] no stray outputs
[ok] output 0 is exactly 5120||<32 bytes>  [ok] output 0 value >= 330
[ok] output 0's Q == independently recomputed state-key(P, new snapshot)
[ok] proof's leaf_hash matches my independent TapLeaf
[ok] life strictly increases  (1 -> 2)
```

**The load-bearing observation:** `Q` is `5701073b1a04…2667` in *both* modes.
The trailing funding input does not perturb the identity commitment — which is
the ordinal argument the commit rests on, confirmed against real encoders
rather than only in the commit's own simulator.

## 7.2 Causeway → `%gw-btc` — **PARTIAL, not re-driven from empty**

The agent-side half was observed live on the current desk: k2 logged
`%gw-btc: custody log verified (1 entries); refreshing our pass` after a
`%anew`, which is the same `+begin-anew` re-walk a Causeway
`%gw-custody-entry` poke drives. The Causeway-side half (does `finalize` emit
the right poke line) was **not** re-run here; it last passed ×3 on `bf90840`,
and `app/gw-btc.hoon` and `lib/gw-btc-pass.hoon` have both changed since. A
clean re-drive needs a ship whose `/x/custody` is empty, which on this rig
means the same agent nuke that 1.5 needs (below). **Carried forward.**

## 7.3 Causeway ↔ ship state synchronisation — **PASS ×6**

`causeway proof verify --onchain`, headless, against live mainnet, on all six
comet proofs (the previous run did three):

```
C1 C2 C3 k1 k2 k3  ->  OK — OK (confirmed)   exit 0
```

## 7.4 Destroy the seed, attempt recovery — **PASS, both halves**

**No mint was needed.** The destructive half only requires *mining* a comet
under a hiding `dat` — no transaction, no chain, no sats. That is what the
previous run did too, and it is why the pre-authorised 2,500-sat budget went
unused.

**(a) Confidential — unrecoverable.** A fresh 12-word phrase was generated
inside a single process; `blind_seed`, `blind` and `d` derived; a **real**
suite-C comet mined under that `dat` with the real `comet_miner`
(`~hidtyr-ladrut-fontyc-hinfet--forlur-locser-tirdef-daplyd`,
`d = b6d94c28…4b5b1a2c`). The phrase, the BIP-39 seed, the blind seed and the
blind were never printed and never written to disk; only `sha256(blind)` was
kept, as a checker that can confirm a recovery but cannot shortcut one.

```
2,132,000 candidate blinds in 60s (35,503/s)   found: False
expected time to exhaust the 2^256 blind space: ~1.0e+65 years
structured guesses (zero / ff / sha256(txid) / sha256(d) / d itself): all False
```

And — the part that makes it a demonstration rather than a re-implementation —
**the same conclusion from the deployed verifier**. `lib/gw-btc-pass` was built
out of k3's own clay (care `%a`) at branch tip and called directly. Its
`+parse-dat` accepts the reconstructed dat as `[%gw-btc kelvin 9 d]`, and then:

| forged blind | `+verify-dat` | `+spawn-commit == d` |
|---|---|---|
| zero / one / all-ff / `d` itself / txid-as-blind | **%.n** ×5 | **%.n** ×5 |

**(b) Public — recoverable from the chain alone.** C3's real published spawn
OP_RETURN (block 961.059) carries its own blind. Fed to *the same*
`+verify-dat` on the same ship:

```
C3's REAL published blind    +verify-dat: %.y  OPENS
a forged blind (control)     +verify-dat: %.n  refused
```

The positive control matters: it shows the predicate **discriminates** rather
than merely always refusing. Independently, a from-scratch reimplementation
recomputed `spawn-commit(spawn_sont, blind)` from **only** the OP_RETURN's own
bytes and got `a68fa950…dd6414`, exactly the `d` inside that same publication —
with no secret material read at any point.

---

# 6.7 — are failures diagnosable from the logs alone? **PARTIALLY**

## Method

Ten failures were induced on k2 (a live verifier at branch tip) against **C2
(`~barpyx-…`)**, chosen because no ship in the rig tracked it, so failures fail
for one named reason instead of dragging three `tracked-*` checks along. Snubs
were cleared between cases so each case's own disposition was observable.
Vectors were built by `ops/gwvec.py` from C2's **own on-chain custody log**.

Then the discipline the brief asked for: the resulting 209-line log slice was
handed to **an independent reader with no access to the source code**, the
repo, the docs, or any other file, and asked to write an incident report. What
follows is that reader's verdict, not mine.

## What `25f0a1d` demonstrably fixed

Three lines are, in the reader's words, "better than most production logging
anywhere" — cause, operational consequence, the trap, and the exact undo:

```
%gw-btc: writ from ~barpyx-… REFUSED: its pass decodes to an EMPTY custody log
  (a suite-C pass asserts a confidential identity and this offers no
   evidence whatsoever for it)
  (a NEGATIVE verdict: jael %fails and ames SNUBS, stickily, and the snub
   then blocks the very packet that would correct it)
  (inspect with .^(/snubbed) and undo with %snub %deny %del)
```

```
%gw-btc: writ from ~barpyx-… REFUSED: custody log of 1.025 entries is over the 1.024 cap
  (walking it is a denial of service, and the cap is protocol rather
   than readiness -- no amount of catching up changes this answer)
```

and the `%anew` timeout is no longer reasonless — it names the reason, the
slot, **and a job id**:

```
%gw-btc: %anew for ~namfyn-… refused: a self-validation is already in flight (job 9)
  (single-flight; /x/pending-own shows the slot, and it is released
   either by the thread's answer or by the ~h2 leak guard)
```

The reader could act on all of these without ever opening source. That is a
genuine advance on "still no".

## What still blocks a YES

1. **No timestamps anywhere in the log.** Nothing can be ordered against the
   outside world or against a suspected cause; no durations, no rates. The
   reader called this fatal and worked the whole incident off the scanner
   heights as a surrogate clock.
2. **No correlation id on the peer path — though there is one on the self
   path.** Four `dropped: a verification is already in flight` lines and three
   verdicts for the same ship could not be mapped to one another. `%anew` says
   `(job 9)`; the writ path says nothing, and points at no scry, where its
   sibling points at `/x/pending-own`.
3. **`[..]` is never defined.** It marks three of the four failing checks in
   the INVALID verdict. Line 179 explains `[XX]`; nothing explains `[..]`, so
   whether it is a failure, a skip or a not-yet-run is a guess.
4. **`REFUSED` vs `refused` — a snub versus a no-op — differ only by letter
   case.** Five verbs (`dropped`, `REFUSED`, `refused`, `declined`, `holding`)
   cover the whole disposition space with no legend.
5. **The check list silently changes shape between runs.** The VALID verdict
   ends `pass-key / sponsor-known / tracked-prefix`; the INVALID one has
   `tracked-tip` and `life-monotonic` as well. Whether they were checked and
   passed, or never run, is not determinable — so the reader could not assert
   the VALID verdict was as thorough as the INVALID one.
6. **The decisive fact is present but never said.** C2 was VALID with a
   2-entry chain and tip `0xcca.2561…`, then INVALID with a 1-entry chain and
   tip `0xf7b1.2cc9…`. The reader found this by diffing two thirty-line blocks
   by eye. No line says "chain length changed 2→1" or "tip differs from the
   one previously accepted".
7. **`sponsorship of ~… declined by operator`** has no cause, no consequence
   and no remedy — the most information-free line in the file.
8. **Only prose predicts state mutations; nothing records them.** There is no
   "snub added", no "snub list now N", no "pass accepted into jael", no "route
   torn down". A positive verdict announces nothing at all about its effect —
   the only evidence the VALID verdict did anything is an unrelated `ames: lamp`
   line.
9. **Two self-contradictions** an operator cannot resolve without source:
   `light client is NOT synced; holding all attestations (no verdicts)`
   immediately followed by a full verdict and a snub (in-flight work is exempt,
   and the message overstates); and a "sticky" snub followed by more traffic
   from the snubbed ship. *Honesty note: the second is partly my harness — I
   cleared snubs out of band between cases with `gwsnub.py`. But the reader's
   underlying point stands and is not a harness artefact: **snub add/remove is
   never logged**, so the operator has no way to tell an out-of-band clear from
   a snub that never applied.*
10. **21% of the log (44 of 209 lines) is `eth-watcher` noise** from
    `%urb-snapshot` — an Ethereum JSON-RPC range error, dumped twice as a
    20-line noun. Identifiable from its first line only; lines 2–20 of each
    dump carry no agent tag at all. It is the largest visual block in the file.

## Verdict

> **PARTIALLY.** Of six substantive `%gw-btc` failure classes: two fully
> diagnosable, one half-diagnosable (the class and the failing check's *name*,
> but not what `tracked-prefix` means or why it flipped), three not diagnosable.

"Better, still not yes" — and the remaining work is structural (timestamps,
correlation ids, a legend, state-mutation events), not more prose.

---

# The rest of the matrix

## Phase 1 — mint identities

Closed by read-only chain forensics: every spawn re-decoded from raw hex, and
`state-key(P, snapshot)` recomputed by a from-scratch secp256k1 / BIP-341 /
`jam` implementation written against the Hoon spec — not by calling causeway
and comparing it with itself.

| | spawn txid | blk | in | out | OP_RETURN | out[0] | state-key |
|---|---|---|---|---|---|---|---|
| C1 | `2c66c654…` | 961055 | 1 | 1 | no | 1889 | **MATCH** |
| C2 | `f7b12cc9…` | 961056 | 1 | 1 | no | 1889 | **MATCH** |
| C3 | `ec5c1fbe…` | 961059 | 1 | 2 | **yes**, `0x4c` PUSHDATA1, 254 = 254 | 1615 | **MATCH** |
| k1 | `1653a2ce…` | 961324 | 1 | 1 | no | 1556 | **MATCH** |
| k2 | `8fc1f950…` | 961324 | 1 | 1 | no | 1556 | **MATCH** |
| k3 | `ceaa3fd7…` | 961324 | 1 | 1 | no | 1556 | **MATCH** |

- **1.2 PASS.** C1 and C2's spawns are single-input, single-output, zero
  OP_RETURN.
- **1.3 PASS, with a named gap.** C3's spawn publication is well-formed
  (`6a 03 'urb' 01 09 4c fe <254 bytes>`, declared length exact) and **opens its
  own `dat`** using only its own bytes. The gap: all three publications on chain
  are 196/214/254 payload bytes, so only the **PUSHDATA1** branch of
  `+push-data` has ever met a real node. The `0x4d` PUSHDATA2 two-byte-LE path —
  the one `194e56d` was written for, and the one k1's 279-byte declassification
  used — is exercised by that transaction but **a public *spawn* carrying a
  fief, which is what the code comment predicts at 265–269 B, has never been
  broadcast.**
- **1.4 PASS ×6.**
- **1.5 BLOCKED — not run.** See below.

**A trap worth recording.** C1/C2/C3's artifacts carry their *latest
state-update* proof in `causeway_proof`, not their spawn proof, so their
top-level `sat_output_key_Q_hex` / `sat_script_pubkey_hex` / `sat_value`
describe the **current tip sat, not the spawn output**. Checking 1.4 by
comparing those fields against the spawn transaction produces a spurious
mismatch (C1: artifact `bb0cad2e…` vs spawn out[0] `be3a7444…`). k1/k2/k3's
artifacts do not have this problem. Also noted: C1's life-3 update bumped
`life` and changed nothing else — the committed `key` is byte-identical across
lives 1, 2 and 3.

## Test 1.5 — **BLOCKED, and the reason is structural**

1.5 needs a scanner that covers a **spawn** publication for a comet it does
**not** track. On this rig that is uniquely constrained:

- The only spawn publication on chain is **C3's, at 961.059**. Every other
  publication (C2 @961217, C3 @961129, k1 @961353) is a *state-update*
  publication with no blind-opening, which `+process-publication` routes to
  `+apply-state` and which a scanner refuses outright for a comet it does not
  track (the documented Tier-2 gap).
- k2 and k3 both already track C3 confidentially, so for them C3 routes to
  `+apply-state` and never reaches `+apply-spawn`.
- **k1 is the only viable verifier** — empty index, no point for C3, synced
  light client.

`%gw-index-from` refuses while `unv-ids` is non-empty, and k1's holds 2 (k2 and
k3, verified confidentially). Confirmed live, non-destructively:

```
%gw-btc: refusing %gw-index-from: an index already exists
  (cursor 0, 2 points;
   rebootstrapping means nuking the agent, deliberately)
```

The guard is correct and well-diagnosed — it even reports the cursor and the
point count, and says what getting past it costs. Getting past it requires
`kiln-nuke` on a live mainnet ship's `%gw-btc`. **I did not do that**: the
safety gate declined the operation, and I judged it not worth forcing for one
test. It is cheap for someone who wants it — jael's points survive a gall nuke,
and the answer arrives within 4 blocks of the 961.055 start — and it would
close **7.2** in the same pass. Recommend running the two together.

## Phase 3 — kernel gating and real networking

| # | result |
|---|---|
| 3.3 | **PARTIAL — verifier half PASS** |
| 3.5 | **PASS (verifier half)** — the key safety invariant holds |
| 3.2, 3.6 | **NOT RUN** — blocked, see below |

The `badiv` fixture (a pier that boots as C3's `@p` from a **corrupted**
xtr-baked feed, live pass 294 B) was booted on N1 and aimed at k1, which holds
no point for C3.

**Over the wire, nothing arrived, and that is not a verifier result.** `|hi`
from badiv to k1 produced no ack in 150 s, k1's log was completely empty, and
k1's view of C3 was unchanged. But the cause is routing, not judgement:

```
badiv's forward-lane to k1 : [%& ~<star 153>]   -- via a star
k1's ames peer entry for C3: ~                  -- nothing ever arrived
```

Mainnet stars do not relay comet↔comet packets — the finding Phase 3 recorded
and Phase 4 exists to fix. An **unverified** comet has no lane to a verifier,
because the lane (`ames: lamp`) only appears *after* verification installs the
on-chain fief. That is a genuine chicken-and-egg, and it is why every
verification in this entire campaign — including the headline 3.1 — has been
driven by the local `%jael-writ` poke rather than by first contact over the
wire.

**Driven through that documented poke path instead, the verifier half is
clean:**

```
%gw-btc: writ from ~ligdes-… REFUSED: its pass is not a readable %gw-btc attestation
  (a NEGATIVE verdict: jael %fails and ames SNUBS, stickily, …)
```

and k1's resulting state is the 3.5 invariant, exactly:

| | |
|---|---|
| `lyfe` | `~` — no point installed |
| `dome` | `~` — **not** `%gw-btc`, and **not** vanilla comet PKI either |
| `/x/points`, `/x/confidential`, `/x/attested` | absent from all three |
| snub | C3 added |

**3.5 holds: an invalid attestation did not silently degrade to vanilla-comet
networking.** It produced no registration of any kind.

*The snub was cleared afterwards* — badiv was impersonating the real C3, and
leaving it would have blacklisted an honest comet.

**3.2 and 3.6 not run.** 3.2 (unattested comet held pending verdict) needs the
same first-contact-over-the-wire path that does not exist without sponsorship.
3.6 (attested comet ↔ vanilla suite-B comet) needs the `van` fixture booted and
a route to it, and hits the same wall.

## Phase 4 — sponsorship — **4.1, 4.2, 4.3 NOT RUN**

All three need a sponsor ship (C3) running and reachable *and* a sponsee (C1)
attesting to it. The rig can host that — C3's committed fief is on N1 and C1's
on-chain `sponsor=~ligdes-…` is already confirmed — but the two ships would
each need their light client brought forward and, more importantly, this run's
measurements above show the sponsee→sponsor leg is the very leg that has never
worked over the wire. Standing that up honestly is a campaign of its own, not a
tail-end task. 4.4–4.8 already passed on this desk in the suite re-run.

## Phase 5b

- **5b.1 — dropped** by owner decision.
- **5b.2 — done** (Tier-1 declassification, block 961.353).
- **5b.3 — NOT RUN.** It needs a **live rekey**: C1 rekeys, C2 *receives* the
  updated attestation over the wire and re-verifies at the new life. That is a
  chain operation, and the receiving half needs the comet↔comet path above. The
  static half is already covered — `PHASE2-RERUN` verified a post-rekey
  snapshot from chain at the new life on two verifiers.

## 6.3 Network partition between two comets, then heal — **PASS**

k1 ↔ k3, dropping only UDP so each ship's light client (TCP 8333) stayed up —
a peer partition, not a chain outage. A detached dead-man's timer removed the
rules unconditionally, so the box could not be left partitioned.

| phase | k1→k3 | k3→k1 |
|---|---|---|
| baseline | ACKED 0.4 s | ACKED 0.3 s |
| partitioned | **no ack, 100 s** | **no ack, 100 s** |
| healed | ACKED **63.2 s** | ACKED 0.6 s |

**No false verdicts and no snubs.** Both ships' view of the other was
byte-identical before and after: k1 held k3 at `lyfe=[~ 1] dome=[~ %gw-btc]`
confidential+attested; k3 held k1 at `lyfe=[~ 2] dome=[~ %gw-btc]` public;
`snub=[%deny ~]` on both throughout. Recovery needed no intervention. The
63.2 s on the first direction is ames retry backoff, not a failure — worth
knowing before someone calls a healed partition broken at t+30 s.

## 6.4 Prolonged offline, then rejoin — **NOT RUN**

Passed on the Phase 6/7 run ("PASS, with supervision"). Re-running it means
stopping a live mainnet ship *and* its supervisor for a prolonged period; it
was the riskiest remaining operation with the least new information, and I ran
out of budget before it. Not blocked — just not done.

## 6.5 Chain reorg — **CHARACTERISED, not passed**

A reorg cannot be staged on mainnet, so this is derived from the shipped code
at `9331a8d`. **`PHASE67-RESULTS.md` §6.5 is now stale** — it describes a
`?- -.upd` that treated `%reorg-rollback` identically to `%new`, and that code
is gone.

**Shipped behaviour.** A `%reorg-rollback` at or below the scan cursor sets
`reorg-halt [at cursor since]` (`app/gw-btc.hoon:1427-1431`), stops the block
scanner, and re-warns every ~5 minutes. Above the cursor it logs and continues
(`:1409-1414`). The halt log line is excellent — it states that the index may
hold facts from orphaned blocks, that the ship *cannot tell which*, and both
resume forms. Four unit tests pin the mechanics
(`tests/app/gw-btc.hoon:775,795,812,827`).

**Confidential verification genuinely keeps working** during a halt — the
runbook's claim is correct; `reorg-halt` appears nowhere in the `%jael-writ`
path. But it is not *unaffected*: it reads the frozen index for `sponsor-known`
and `tracked-anchor`, so a sponsor publishing after the halt is invisible and
every comet naming it reads UNDETERMINED indefinitely.

**No fact in the index records the height it was indexed at** — confirmed
against `sur/urb.hoon:70-84` (`$point`), `sur/ord.hoon:37-40`. The single
cursor is the only height in the structure. So after a reorg nothing can
identify which facts are wrong beyond the `(at, cursor]` range.

Three things a real reorg would break that **nothing currently detects**:

1. **A `/blocks` return straddling the rollback is applied in full after the
   halt.** `app/gw-btc.hoon:1167-1319` has no `reorg-halt` guard; blocks are
   fetched by bare height (`:1691`) with no prev-hash linkage check (`:1695`),
   so one batch can mix both chains into the index — and then `at`/`cursor`
   bound the wrong range.
2. **A second, deeper rollback while already halted is completely silent**
   (`:1415-1416`) and does not update the halt record, so `at` stays at the
   first, shallowest value and `%gw-reorg-resume [~ at]` rewinds too little.
3. **Rewind-and-rescan is not "add-only".** `urb-core.hoon:417` already
   *deleted* the spent `sont-map` entry, so the winning chain's transaction
   finds nothing and is a no-op (`:415`); `+apply-state` then fails with
   "does not spend our tracked tip" (`:352-354`) and `+apply-spawn` refuses a
   comet already in `unv-ids` (`:320-322`). `OPERATIONS.md:1118-1122`'s "adds
   correct facts but cannot remove wrong ones" overstates what the rewind
   achieves.

Also: **`/x/ready` reads healthy on three of four fields while halted** —
`synced=%.y`, `indexing=%.y`, a live `tip`, and only `reorg-halt` dissenting.
And `block-confirmations` is **1** (`app/gw-btc.hoon:1603`, "1 for alpha"), so
the ordinary single-block mainnet reorg lands inside the reorged range
essentially every time.

## 6.6 Two peers disagree about a third — **OBSERVED; they do, and it does not reconcile**

What the three ships actually believe, measured on all six subjects (`0`/`~`
= no point; `conf`/`att` = confidential set / attested map):

| subject | k1 believes | k2 believes | k3 believes |
|---|---|---|---|
| k1 `~talryg` | life **1**, not public | life **2**, **PUBLIC** | life **2**, **PUBLIC** |
| k2 `~namfyn` | life 1, conf+att | *(self)* | life 1, conf+att |
| k3 `~hacsut` | life 1, conf+att | life 1, conf+att | *(self)* |
| C1 `~havnyl` | **nothing** | life 3, conf+att | life 3, conf+att |
| C2 `~barpyx` | **nothing** | nothing → later life 2, conf+att | nothing |
| C3 `~ligdes` | **nothing** | life 2, conf+att | life 2, conf+att |

`dome=[~ %gw-btc]` wherever a point exists; `snub=[%deny ~]` everywhere.

**k2 and k3 agree with each other on all six subjects. k1 disagrees with both
about four of them, including about itself.** k1 believes it is life 1 and
confidential; both peers believe it is life 2 and public — correctly, because
k1's declassification confirmed in block 961.353 and an on-chain state update
does not rekey a running ship. k1 cannot learn this because **k1 does not
index** (cursor 0, `indexing=%.n`), so its own publication is invisible to it.

Two things worth stating plainly:

- **The disagreement is invisible.** Nothing logs it, nothing scries it, and no
  ship is wrong in a way it could detect. An operator comparing two ships has
  to do it by hand, as here.
- **It does not reconcile and there is no mechanism by which it would.** It is
  also benign *today* — k1↔k2 and k1↔k3 traffic works throughout, because the
  publication advanced only `life` and carried the same networking key forward.
  A rekey that actually changed the key would not be benign.

Also observed, unprompted: during the 6.7 run k2 verified **C2** for the first
time and installed it, and C2's on-chain fief became a runtime lane
(`ames: lamp ~barpyx-… static ip .206.189.188.16 port 49818`). That is a true
fact about the chain, arrived at through the normal path, and was left in place.

---

# Findings

## Known-open bugs, re-encountered (not re-reported as new)

- **[14]** — over-cap custody log snubs as fraud. Reproduced again
  (`REFUSED: custody log of 1.025 entries is over the 1.024 cap` + snub).
- **[15]** — a comet replaying its own genuine older attestation is snubbed via
  `[XX] tracked-prefix`, with the three sibling checks all saying *stale*.
  Reproduced again on k2 against C2.
- **[16]** — a positive verdict never clears an existing snub. Consistent with
  everything seen here.

## New consequence of [15] — worth reporting

**`tracked-prefix` is height-sensitive, so an ordinary Bitcoin reorg can snub an
honest comet.** Custody entries are `[txid height opening]`
(`sur/self-attestation.hoon:68-72`) and `$blind-opening` carries `start-height`
(`:57`) — heights are *inside* the log the pass commits to. If a reorg re-mines
a comet's custody transaction one block over, its old attestation stops
fetching (safe: silence). It re-finalizes with corrected heights and re-attests
— every walk check passes, `spawn-commit` still passes because `start-height` is
explicitly not in the commitment preimage — and then
`lib/self-attestation.hoon:950-957` compares whole entries, the heights differ,
`prefix-chain` returns `%.n`, `tracked-prefix` is fraud-class, and the honest
comet is **snubbed stickily**. `+detect-stale`, which would clear the stale
anchor, runs only from the scanner path (`app/gw-btc.hoon:1285`) — which the
reorg has halted. This is [15]'s mechanism reached without any replay and
without any attacker.

## Real bugs (product) found this run

**None new.** Every product behaviour observed matched its specification, apart
from the known-open items above.

## Documentation defects

1. **`PHASE2-RERUN-RESULTS.md` mislabels k3's desk** as `167e143` "including
   `25f0a1d`". It was `25f0a1d`. Corrected in this doc's Deploy section; the
   run's conclusions are unaffected because the arm-by-arm comparison it
   reports was against the right *content*.
2. **`PHASE67-RESULTS.md` §6.5 describes code that no longer exists.**
3. **`OPERATIONS.md:1118-1122` overstates what `%gw-reorg-resume [~ height]`
   repairs** (see 6.5, point 3).
4. **`ops/gwctl.py pass`'s docstring says it returns the BARE ~108 B pass and
   that "the xtr pass is in the artifact".** Measured on badiv it returned
   **294 B** — the xtr-bearing pass — because a ship booted from an xtr-baked
   feed has the xtr in jael's ring. The docstring's warning is wrong for
   exactly the ships this campaign runs, and it prints the caveat every time.

## Harness / tooling

- **`ops/gwvec.py` and `ops/gwsnub.py` were not on the droplets** and
  `gwvec.py` cannot run there anyway — it imports `causeway`, which needs
  `embit`, which is not installed. Vectors have to be built on the Mac and the
  pass hexes shipped. Worth a line in `ops/README.md`.
- **`gwctl.py writ` and `gwctl.py pass` still do not compose** (dot-grouped
  output, `int(x,16)` input) — previously recorded, still true.
- The `/x/point/<ship>` scry **crashes a thread** for any ship that is not a
  public point (`[~ ~]` in a `.^` is a crash, not a `~`). Use `/x/points` and
  test membership.

## Infrastructure

- **The light-client `synced` flag flaps.** Four full
  `NOT synced` → `SYNCED` cycles on k2 inside ~15 minutes, each immediately
  preceded by a `%headers-from` line. Fail-closed and harmless — but it makes
  `synced` a poor health signal, and at the end of this run k1 and k3 read
  `synced=%.n` while k2 read `%.y`, all three at the same tip. Consistent with
  `OPERATIONS.md` §5.7's "a quiet ship may read `%.n` until something asks it
  to care"; noted because the log gives no quantity (no height, no lag, no
  peer count) to act on.
- k3 continues to log the unrelated `%urb-snapshot` Ethereum JSON-RPC error.
  It is now 21% of a `%gw-btc` log slice by line count.
- Zero vere SIGSEGVs, zero sidecar SIGSEGVs, zero supervisor interventions.

---

# Money

| | |
|---|---|
| spent | **0 sats.** Nothing was signed for broadcast or broadcast. |
| k1 identity sat | 754 — untouched |
| C2 / C3 | 1.544 / 1.288 — untouched |
| the pre-authorised throwaway mint | **not used, and not needed** — 7.4's destructive half only mines |
| the ~17.822-sat "wallet change" UTXO | **not spendable from this machine** (§Headline) |

Two transactions were *constructed and signed* during Phase 7 — a spawn and two
rekeys for a throwaway comet — against an already-spent prevout, and delivered
to a local capture stub. They are double-spends of a confirmed input and cannot
confirm.

---

# Rig left in this state

| | k1 | k2 | k3 |
|---|---|---|---|
| desk | **`9331a8d`** (106.186 B) | **`9331a8d`** | **`9331a8d`** |
| ship | up, life 1 | up, life 1 | up, life 1 |
| light client | synced, tip 961.455 | synced, tip 961.455 | synced, tip 961.455 |
| snubbed | **`~`** | **`~`** | **`~`** |
| desks mounted | none | none | none |
| public index | still empty (cursor 0) | following tip | following tip |

**All three ships are now on one revision** — the first time in this campaign.
Every snub caused by this run was cleared. `badiv` was stopped and its pier left
intact. The 6.3 iptables rules are gone (verified: 0 UDP rules). All scratch
files and every copy of a secret-bearing artifact were removed from the
droplets; the local scratchpad went from 1.7 GB to 208 MB.

**One state change was left deliberately:** k2 now holds C2 (`~barpyx-…`) as a
verified confidential peer at life 2, with a runtime lane from its on-chain
fief. It arrived through the normal path and is true.

---

# What is left, and what it costs

| test | why not run | cost to close |
|---|---|---|
| **1.5** + **7.2** | needs `kiln-nuke %gw-btc` on k1 to clear a non-empty `unv-ids` | one nuke + `%gw-index-from 961.055`; the 1.5 answer lands 4 blocks in. **Do these together.** |
| **3.2**, **3.6** | no comet↔comet first contact over the wire without sponsorship | blocked on Phase 4 |
| **4.1–4.3** | needs C1 and C3's ships up with current light clients | a campaign, not a task |
| **5b.3** | needs a live rekey (chain op) + the receiving half of Phase 4 | chain budget + Phase 4 |
| **6.4** | ran out of budget; passed previously | ~30 min, stop/wait/restart k1 |
| **6.5** | cannot be staged on mainnet | stage on regtest — `testnet/` has no `invalidateblock` support today |
