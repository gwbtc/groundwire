# Phase 2 — adversarial verification matrix (live mainnet)

Date: 2026-08-04. Operator: Claude (agent).
Rig: **N1** (`64.227.13.22`), fake `~zod` at `/opt/gw/piers/smoke`, kelvin-408
CC kernel pill, gwbtc/node `%bitcoin-client` **fully synced to mainnet tip**
(961.044 → 961.075 during the run, `is-synced %.y`, 8–10 live peers), plus the
`%groundwire` desk carrying `%gw-btc`.
No Bitcoin transaction was broadcast. Nothing was committed or pushed.

Real assets under test: **C1** `~havnyl-lonpub-botben-hidleb--lomper-marryc-lanmec-daplyd`
(spawn `2c66c654…` @961.055, funding `394f3678…:1` @961.044),
**C2** `~barpyx-…-daplyd` (spawn `f7b12cc9…` @961.056, funding `44eb0d9f…:1` @961.045),
**C3** `~ligdes-…-daplyd` (public, spawn `ec5c1fbe…` @961.059, funding `72340acb…:1` @961.045).

---

## LEAD: six real bugs, four of them individually fatal to the release

| # | Bug | Where | Severity | Class |
|---|---|---|---|---|
| **B1** | `+scan-liveness` hangs **forever** on its first step | `groundwire/lib/lc-attestation.hoon` | **CRITICAL** | real verifier bug |
| **B2** | Light-client `hexb` is byte-reversed relative to the desk's; scriptPubKeys are never converted | `groundwire/lib/lc-attestation.hoon` | **CRITICAL** | real verifier bug |
| **B3** | Causeway writes the **wrong `start-height`** into every blind-opening | `causeway/desktop/causeway.py` | **CRITICAL** | real client bug |
| **B4** | OP_RETURN publication payload byte order is inverted vs. Causeway and vs. `+jam-octs` | `groundwire/lib/gw-btc-pass.hoon` | **CRITICAL** (public path) | real bug |
| **B5** | `%gw-btc` watches agent `%light-client`; the node desk installs `%bitcoin-client` | `groundwire/app/gw-btc.hoon`, `lib/lc-attestation.hoon` | HIGH | packaging |
| **B6** | The `%groundwire` desk ships no `mar/` files for the light client's fact marks | `groundwire/mar/` | HIGH | packaging |

Plus three lower-severity findings (B7–B9) and one design-question result (2.9)
in the sections below.

### B1 — `+scan-liveness` deadlocks on `/best-block` (CRITICAL)

`+scan-liveness:lc-attestation` opens its scan with

```hoon
;<  best-cage=cage  bind:m  (watch-best our)
```

and `+watch-best` is `watch-one:strandio`, i.e. **watch → take-fact → take-kick**.
But `%bitcoin-client`'s `/best-block` is a *persistent* subscription:

```hoon
++  best-block                       :: make-update, app/bitcoin-client.hoon
  |=  dat=best-block:update
  ^-  card
  =/  paf  /best-block
  %+  fact  paf  [%best-block !>(dat)]     ::  <-- fact only, NO kick
```

Every other node endpoint (`block-header-by-*`, `block-filter-by-*`,
`block-by-*`, `transaction`) has `++ end (kick sub)` in its `res`; `/best-block`
does not. `take-kick:strandio` answers `[%wait ~]`/`[%skip ~]` for anything that
is not a `%kick`, so the strand blocks forever.

Observed: the verification strand sat idle for >12 minutes (host load average
0.14, `pending-block-height-reqs {}`, `%filters` never grew — **no filter
request was ever issued**), then died on the app-level timeout with
`%gw-btc: verification thread for … ended without a verdict / timeout`.

**Consequence: with the shipped code, tip liveness can never be determined, so
NO confidential attestation can ever produce a verdict. Every peer is met with
silence, forever.** It fails closed (correct direction) but the feature is
100 % non-functional.

Fix direction: `/best-block` should be read with a fact-only watch + `%leave`
(what I patched in), or better, `%gw-btc` should pass its already-subscribed
`best` (it holds one in `gw-state`) into the thread instead of re-fetching.

### B2 — scriptPubKey byte order is never converted (CRITICAL)

`+common-out-to-bc` / `+common-in-to-bc` pass `script-pubkey`, `script-sig` and
witness items straight through from `$hexb:bitcoin-common` to `$hexb:bitcoin`.
The two are structurally identical (`[wid=@ dat=@ux]`) so it type-checks, but
the node stores its byte strings with the **first wire byte in the low byte**
while the desk's `+p2tr-xonly` / `+state-key` / BIP-340 code are **big-endian**.

Proved directly by dumping C1's spawn transaction through the verifier's own
fetch path (`ted/gw-tx`):

```
GWTX outs=~[[script-pubkey=[wid=34 dat=0x4183.f66a.7d36.16d3.…2051] value=1.889]]
```

The real scriptPubKey is `5120be3a…8341`; the verifier sees it exactly
byte-reversed. `+p2tr-xonly` then reads `0x2051` instead of `0x5120`.

Observed verdict on the genuine C1 attestation before the fix:

```
%gw-btc: attestation for ~havnyl-… is INVALID
  [XX] entry-0-commitment
  [XX] tip-p2tr
```

**Consequence: every `entry-N-commitment` and every `tip-p2tr` check fails on
real chain data — no genuine attestation can ever be VALID.** Note the two bugs
partially mask each other: `+scan-liveness` casts the (already node-order)
`tip-spk` *back* to `hexb:bcm` for the GCS matcher, so a fix to B2 must also
flip in `+scan-liveness` (as I did) or the filter scan silently stops matching —
which would turn a spent tip into a **false `unspent`**, i.e. fail-open.

### B3 — Causeway bakes the wrong `start-height` into every blind-opening (CRITICAL)

`sur/self-attestation.hoon` defines `start-height` as "the block containing the
transaction that **CREATED** the spawn satpoint" — i.e. the *funding* tx's block.
`causeway finalize` writes the *spawn* tx's block instead:

```python
# causeway.py:2463, inside cmd_finalize
start_height=int(proof.get("start_height", height)),   # height = proof["block_height"]
```

and no proof file ever carries a `start_height` key (verified on all three
artifacts), so the default always wins. All three minted comets are affected:

| comet | baked start-height | funding tx's real height |
|---|---|---|
| C1 | 961.055 | **961.044** |
| C2 | 961.056 | **961.045** |
| C3 | 961.059 | **961.045** |

Observed with the packet exactly as `causeway finalize` produced it:

```
%gw-btc: verification thread for … ended without a verdict
attestation-tx-not-found
[961.055 0x394f.3678.9ed2.f0f1.…b076.a24c]
```

**Consequence: every comet Causeway has ever minted is unverifiable.** (The
`spawn generate/connect` publication path has the same defect frozen on-chain —
see B4/B7: C3's published blind-opening says `start-height = 0`.)

### B4 — OP_RETURN publication payload byte order is inverted (CRITICAL, public path)

`+make-publication:gw-btc-pass` builds the payload as `[(met 3 jm) jm]` — the
raw jam atom used as a **big-endian** `hexb` — and `+read-publication` inverts
that with `(cue dat.payload)`. But the file's own `+jam-octs` (and every hash
preimage, and Causeway's `jam_bytes`) uses the jam's **little-endian** byte dump,
which is the ordinary serialization of a `jam`. So Hoon's encoder/decoder pair
are self-consistent with each other and **inconsistent with everything else**.

Run against C3's real on-chain OP_RETURN (263-byte scriptPubKey, 254-byte payload):

```
"envelope kelvin      = 9  payload = 254 bytes"
"shipped +read-publication decodes  = %.n"
"byte-reversed payload decodes      = %.y"
```

**Consequence: the block scanner can never decode a publication written by
Causeway, and a publication written by the Hoon encoder could never be decoded
by Causeway.** The public onboarding path is broken at the codec.

Once the payload is read in the right order, the *semantics* are completely
correct against real mainnet data (see test 1.5 below): the published pass
fingerprints to C3's real `@p`, its blind-opening opens the `dat` commitment,
and the recomputed `state-key` equals the on-chain P2TR output key exactly.

### B5 — agent name mismatch: `%light-client` vs `%bitcoin-client` (HIGH)

`app/gw-btc.hoon` and `lib/lc-attestation.hoon` hardcode `[our %light-client]`
in 8 places. gwbtc/node's `desk/desk.bill` is `:~ %bitcoin-client ==` and there
is no `%light-client` agent anywhere. As shipped, `%gw-btc`'s `/best-block`
watch is nacked, `best` stays `~`, and **every `%jael-writ` is dropped silently**
by the `?: ?|(=(| ready) ?=(~ best))` guard. Harness fix: `sed` the name.

### B6 — the `%groundwire` desk ships no `mar/` files for the node's fact marks (HIGH)

Neither desk defines `mar/block-header-by-height.hoon`, `mar/transaction.hoon`,
`mar/block-filter-by-height.hoon`, `mar/block-by-height.hoon`, … Because the
verification strand runs in the `%groundwire` desk, Clay must build the mark
there to deliver the fact:

```
clay: no files match /mar/block-header-by-height/hoon
[%error-building-mark %block-header-by-height]
spider crashed, killing all strands: %fact
```

I added nine trivial pass-through marks. **Note the failure mode: it took
spider down entirely (`spider crashed, killing all strands`), which is a
DOS-adjacent hazard — one unbuildable mark on an incoming fact kills every
concurrent thread on the ship.**

### B7 — `start-height` is a placeholder `0` in published spawns (MEDIUM)

`_spawn_publication_opening` in `causeway.py` sets `"start_height": 0,
# transport metadata; filled/refined at finalize`, but the publication output is
built and broadcast at spawn time, so it is never refined. C3's on-chain
publication permanently claims `start-height = 0`. Confirmed by decoding the
real OP_RETURN: `"start-height claimed = 0"`.

### B8 — `%gw-btc`'s block scanner still uses the legacy Bitcoin-Core RPC (MEDIUM)

`on-init` pins `rpc = ['http://localhost:8332' ~]` (no auth) and `+get-blocks`
drives `getblockcount` / `getblockhash` / `getblock` / `getrawtransaction`
against it. The confidential path was ported to `%light-client`; **the public
scanner was not**. A light-client-only deployment therefore cannot index public
comets at all, which is why test 1.5's scanner half is not runnable here (no
mainnet `-txindex` node exists on the Mac or on the droplets). Also `ready`
is gated behind a one-shot `%urb-start-indexing` poke, and `rpc` can never be
reconfigured after `on-init`.

### B9 — cosmetic: the ship prints as a raw atom in the failure slog (LOW)

`%gw-btc: verification thread for 235.146.299.140.078.997.037.850.464.130.958.181.069
ended without a verdict` — `who` comes from `(slav %p …)` typed `@`, so
`{<who>}` prints decimal instead of `~havnyl-…`. Makes log triage harder.

---

## Test matrix

Legend for "how": **E2E** = packet poked into the live `%gw-btc` agent, verified
against the real mainnet light client. **PURE** = `+run-checks:self-attestation`
driven directly on the ship with synthetic transactions (used only where the
case needs a multi-hop custody chain, which no real comet has yet).
**SCAN** = the real BIP-158 `+scan-liveness` against mainnet.

All E2E results are *after* applying harness fixes for B1, B2, B5, B6 — without
them nothing verifies at all.

| id | what I did | expected | actual | verdict |
|---|---|---|---|---|
| 2.1a | C1 packet **exactly as `causeway finalize` produced it** (E2E) | VALID | `attestation-tx-not-found [961.055 0x394f3678…]`, **no verdict** (silence) | **FAIL — B3.** The fail-closed behaviour is correct, the packet is not |
| 2.1 | C1 packet with `start-height` corrected to 961.044 (E2E) | VALID, point installed | `%gw-btc: attestation for ~havnyl-… is VALID`; `WRITWATCH %verdict … VERDICT=FULL(valid)`; `.^((unit @ud) %j /=lyfe=/~havnyl-…)` → `[~ 1]` | **PASS** (110 s) |
| 2.1b | same for **C2** (E2E) | VALID | `attestation for ~barpyx-… is VALID`, `VERDICT=FULL(valid)` | **PASS** (100 s) |
| 2.1c | same for **C3** (the public comet's confidential packet) (E2E) | VALID | `attestation for ~ligdes-… is VALID`, `VERDICT=FULL(valid)` | **PASS** (80 s) |
| 2.2 | append a real mainnet tx (C2's spawn) as a bogus custody hop (E2E) | INVALID, tip/sont | `INVALID` / `[XX] derive-tip` / `VERDICT=FAIL(negative)` | **PASS** |
| 2.3 | genuine C1 pass poked under **C2's `@p`** (E2E) | INVALID | `WRITWATCH … who=~barpyx-… VERDICT=FAIL(negative)`, no `+report` block (rejected in `+pass-attestation` before any fetch) | **PASS** |
| 2.4 | two openings, life 2 → 1 (PURE) | INVALID, life-order | `VERDICT INVALID` / `[XX] entry-1-life-order` | **PASS** |
| 2.4b | rift regresses while life advances (PURE) | INVALID | `[XX] entry-1-life-order` (same check covers rift) | **PASS** |
| 2.5 | snapshot `life` 1→2 in the opening; on-chain Q unchanged (E2E) | INVALID, commitment | `INVALID` / `[XX] entry-0-commitment` (+ `[XX] tracked-prefix`, expected: C1 was already anchored by 2.1) | **PASS** |
| 2.6 | one bit flipped in the `blind` (E2E) | INVALID, spawn-commit | `INVALID` / `[XX] spawn-commit` — the **only** failing check | **PASS** |
| 2.7 | custody log of 1.025 entries (E2E) | rejected on the bound | `VERDICT=FAIL(negative)`, no `+report` (dropped by `(gth (lent chain) 1.024)` before the thread) | **PASS** |
| 2.7b | 1.025-entry log at the pure boundary (PURE) | `chain-bounded` | `VERDICT INVALID` / `[XX] chain-bounded` `[XX] fetch-count` | **PASS** |
| 2.8 | **real spent outpoint** — C1's own funding outpoint `394f3678…:1`, created @961.044 and spent by C1's spawn @961.055 (SCAN, scanning 961.044 → tip) | spend detected → `%.n` | `[%gw-btc-lc-scan-spent tip=[…394f3678… vout=1 off=0] height=961.055]` → `GWSCAN RESULT [~ %.n] = SPENT` (≈4 min: 11 filters + 2 real block downloads) | **PASS** |
| 2.8b | `tip-unspent = [~ %.n]` at the pure boundary (PURE) | INVALID | `VERDICT INVALID` / `[XX] tip-unspent` — sole failure | **PASS** |
| 2.8c | `tip-unspent = ~` (undeterminable) (PURE) | fail closed, INVALID | `VERDICT INVALID` / `[XX] tip-unspent` | **PASS** |
| 2.9 | identical pass, `dat` carries **kelvin 8** (E2E) | ignored as a foreign version, NOT mis-parsed | not mis-parsed (rejected in `+pass-attestation`, no fetch, no report) **but a NEGATIVE verdict is emitted**: `WRITWATCH … who=~ritlex-… VERDICT=FAIL(negative)` | **PARTIAL — see note** |
| 2.10 | entry 1 spends the sat through **input 1**, not input 0 (PURE) | INVALID | `VERDICT INVALID` / `[XX] entry-1-continuity` (`entry-1-input-zero` is `[ok]` — input 0 exists, it just isn't the sat) | **PASS** |
| 2.10b | post-first-hop spend with a script-path-shaped witness (PURE) | INVALID | `[XX] entry-1-key-path` | **PASS** |
| 2.11 | snapshot names `sponsor=~marzod`, unknown to the verifier (E2E) | `sponsor-known` fails | `INVALID` / `[XX] entry-0-commitment` `[XX] sponsor-known` `[XX] tracked-prefix` | **PASS** (see note) |
| 2.11b | clean `sponsor-known` with the commitment intact (PURE) | INVALID, only `sponsor-known` | `VERDICT INVALID` / `[XX] sponsor-known` — sole failure | **PASS** |
| 2.11c | same sponsor, but present in `known-public` (PURE) | VALID | `VERDICT VALID` | **PASS** |
| 2.12 | `xtr = (jam ~)` — canonically-empty chain (E2E) | negative verdict | `WRITWATCH %verdict … VERDICT=FAIL(negative)` | **PASS** |
| 2.13 | `xtr = 0` — public onboarding packet (E2E) | **SILENCE**, no verdict | **no `%verdict` at all** — the `+public-pass` branch returns `` `this `` | **PASS** — and cleanly distinct from 2.12 |
| 2.14 | malformed non-`mat` `dat` (`dat = 0`) on an otherwise valid suite-C pass | dropped in ames before Jael | ran the exact `+pass-pki-dom:ames` body on each real pass: `nonmatdat` → **`~`**, `genuine` → `[~ [p=59 q=109.351.514.503.015]]` (= `%gw-btc`), `kelvin8` → `%gw-btc` too. `~` ⇒ `+on-hear-open`'s `?: &(?=([%c *] cek) ?=(~ dom)) event-core` drops the packet before creating alien state or passing `%writ`. Poked *directly* into `%gw-btc` (bypassing ames) it yields `VERDICT=FAIL(negative)` | **PASS at the arm + code-path level**; the ames drop itself needs Phase 3 networking |
| 2.15 | **three** identical C1 writs poked 2 s apart (E2E) | duplicates dropped silently | `~(wyt by inflight)` → `1` while running; exactly **one** `+report` block and **one** `%verdict VERDICT=FULL(valid)` | **PASS** |
| 2.16 | **real same-block spend** — outpoint `5f7203ed…:0` (P2TR, 1149 sat) created *and* spent inside block 961.055 (SCAN) | detected; scan starts at tip height | `[%gw-btc-lc-scan-spent tip=[…5f7203ed… vout=0 off=0] height=961.055]` → `GWSCAN RESULT [~ %.n] = SPENT` | **PASS** |
| 1.5 | does the public path work from the on-chain OP_RETURN alone? | scanner indexes C3 with no packet | **decoder half FAILS (B4)**; with the byte order corrected everything else is exact — see below | **FAIL (B4) / semantics PASS** |

### 1.5 in full — C3's real on-chain publication

Decoded straight out of the 263-byte OP_RETURN scriptPubKey of `ec5c1fbe…`
(block 961.059) with the desk's own `+parse-publication` / `+read-publication`:

```
"envelope kelvin      = 9  payload = 254 bytes"
"shipped +read-publication decodes  = %.n"       <-- B4
"byte-reversed payload decodes      = %.y"
"who = fig(pass)      = ~ligdes-risbur-folmus-mattyp--firpec-lispec-noddyl-daplyd"
"blind-opening present= %.y"
"dat opens to spawn   = %.y"
"spawn satpoint       = [txid=0x7234.0acb.…bc54 vout=1 off=0]"
"start-height claimed = 0"                       <-- B7
"snapshot             = [life=1 rift=0 key=100.951.…343 sponsor=~ fief=~]"
"recomputed state-key = 0xca2.828c.764a.…0bee"
"matches on-chain Q   = %.y"
```

So the publication binds the right name, opens the hiding `dat`, and its
`state-key` reproduces the on-chain P2TR output exactly. Only the payload byte
order (B4) and the placeholder `start-height` (B7) are wrong. The **scanner**
half of 1.5 (`+find-publication` / `+apply-spawn` walking live blocks) could not
be exercised at all because of B8 — there is no mainnet `-txindex` Bitcoin Core
anywhere in this rig, and the scanner has not been ported to the light client.

### Test 2.9 — the kelvin question

The plan's expectation is "ignored as a foreign protocol version, NOT
mis-parsed". Half is confirmed: a kelvin-8 `dat` is parsed correctly as
kelvin 8 (`+parse-dat` reads both `+mat` items and the trailing-data bound
holds; `(mat 8)` and `(mat 9)` have identical width, so nothing shifts) and
rejected before any chain fetch. But the rejection path is
`+pass-attestation` → `~`, then `+public-pass` → `%.n`, then
`~[(writ-card dom who ~)]` — **a negative verdict**, which Jael turns into
`[%give %sybl %fail …]` and Ames turns into a **snub**.

That means a comet minted under a future (or past) kelvin is not "ignored", it
is actively blacklisted by every kelvin-9 verifier. Per addendum §1 verifiers
are supposed to "advertise the set of Kelvins they accept" and retiring a
kelvin is meant to be a deliberate breach-class decision; snubbing looks like
the wrong default. Recommendation: treat an unrecognised kelvin like the
`xtr = 0` public-onboarding case — silence, not `%fail`. Flagging as a design
decision to confirm rather than an outright bug.

### Note on 2.5 / 2.11 and the extra `tracked-prefix` failure

C1 was successfully attested in 2.1, so `%gw-btc` holds an anchor for it and
every later C1 packet additionally runs `tracked-tip` / `tracked-prefix` /
`life-monotonic`. Mutating the entry-0 opening necessarily breaks the
prefix relation with the stored chain, so `tracked-prefix` also fails. The
*named reason under test* is present in both cases; 2.11b/2.11c re-run the
sponsor case cleanly at the pure boundary with no anchor.

---

## Harness fixes applied (deployed copy only — nothing committed)

The `%groundwire` desk was rsynced to a scratch copy, patched, and installed on
N1. The user's git working tree was not modified. Patches, all marked
`TEST-HARNESS PATCH` in the source:

1. `%light-client` → `%bitcoin-client` in `app/gw-btc.hoon` (3 sites) and
   `lib/lc-attestation.hoon` (5 sites)  — B5.
2. Nine pass-through `mar/*.hoon` files for the node's fact marks — B6.
3. `+watch-best` rewritten as watch → take-fact → leave — B1.
4. `+flip-hexb` added; applied in `+common-out-to-bc` / `+common-in-to-bc`,
   and the inverse flip added in `+scan-liveness`'s GCS target — B2.
5. `verify-timeout` `~m2` → `~m20`, purely so a stalled strand could be
   observed. **Note: a genuine verification takes 100–110 s on this rig, which
   is uncomfortably close to the shipped 2-minute budget** — a single slow
   block fetch would turn a valid peer into silence.
6. `desk.bill` trimmed to `%gw-btc` + a new `%writ-watch` observer, and
   `app/reg-tester.hoon` (which points at regtest port 18443) removed.

New test-only files in the deployed desk: `lib/gw-vec.hoon` (packet vectors
generated from the real artifacts by `scratchpad/genvec.py`, which imports
`causeway.py`'s encoders — no encoder was reimplemented), `lib/gw-pure.hoon`
(pure-boundary vectors), `lib/gw-pub.hoon` (C3's on-chain publication),
`ted/gw-scan.hoon`, `ted/gw-tx.hoon`, `app/writ-watch.hoon`.

`%urb-start-indexing` was poked with a start block-id of 961.058 and a stub
JSON-RPC server on `127.0.0.1:8332` (`/opt/gw/stubrpc.py`) answers
`getblockcount` with 961.058 so the legacy block thread completes each cycle
with zero work instead of erroring every 30 s. It never touches a real node and
cannot broadcast.

## Infrastructure notes

* The tcp sidecar had dropped again before this run; the documented recovery
  (`&kill-peer-connections ~` then one `&add-earth-peer`) restored 0 → 10 peers
  in a few minutes. `is-synced %.y` was confirmed before every verdict.
* `%bitcoin-client`'s `+get-some-peer` picks a random handshake-done peer with
  no service filter at the call site; combined with heavy peer churn
  (`blacklist` reached 775 entries) filter/block fetches are latency-variable.
  A single block fetch on this 2-vCPU droplet is tens of seconds.
* `.^((unit @tas) %j /=dome=/…)` returns `~` on this ship — **not a bug**:
  the `%dome` scry short-circuits on `fak.own.pki.lex`, and the smoke ship is a
  fakezod. `%dome` is therefore untestable on a fakeship. (Also note the
  addendum §10 asked for `+pass-pki-dom` to be *hoisted* into lull; it was
  instead duplicated — jael.hoon:1818 carries a hand-copied "mirror of
  +pass-pki-dom:ames". Two copies of a consensus-critical parser.)

## Final state (left running on N1)

```
[%is-synced %.y]  [%best-block block-height=961.077]  [%live-earth-peers 9]
.^((unit @ud) %j /=lyfe=/~havnyl-…)  ->  [~ 1]     (C1 installed)
.^((unit @ud) %j /=lyfe=/~barpyx-…)  ->  [~ 1]     (C2 installed)
.^((unit @ud) %j /=lyfe=/~ligdes-…)  ->  [~ 1]     (C3 installed)
.^((unit @ud) %j /=lyfe=/~ritlex-…)  ->  ~         (kelvin-8 impostor: never installed)
```

tmux session `smoke` on N1 still holds the live dojo with `v` (packet vectors),
`gp` (pure vectors) and `gb` (C3 publication) bound. Helper scripts:
`/opt/gw/run.sh`, `/opt/gw/t.sh`, `/opt/gw/slow.sh`, `/opt/gw/scan.sh`,
`/opt/gw/pure.sh`, `/opt/gw/stubrpc.py`. Patched desk source:
`<scratchpad>/gwdesk/`; vector generator `<scratchpad>/genvec.py`.

## Summary

24 of 26 executed checks PASS. Two do not:

* **2.1a FAILS** — the packet Causeway actually produces is unverifiable (B3).
* **1.5 FAILS at the codec** — the shipped publication decoder cannot read a
  real on-chain publication (B4); the scanner half is untestable (B8).
* **2.9 PARTIAL** — correct parsing, but a foreign kelvin gets snubbed rather
  than ignored; needs a design ruling.

Every adversarial case that was supposed to fail failed **for its own named
reason**, and the two "subtle" distinctions the plan called out held exactly:
`xtr = 0` is SILENCE while `(jam ~)` is a NEGATIVE verdict, and the filter scan
does start at the tip height (a same-block spend of a real mainnet P2TR output
was detected). Fail-closed discipline was also correct everywhere it was
exercised: fetch failures, timeouts and undeterminable liveness all produced
silence or a `tip-unspent` failure, never a false positive.

**The headline is that with the code as it stands in the repo, none of this
matrix runs at all**: B5 drops every writ, B6 kills spider, B1 hangs every
verification and B2 fails every commitment. Four independent single-point
failures, each sufficient on its own to make confidential comets 100 %
non-functional, all of them invisible to unit tests and all of them found in
the first hour against real infrastructure.
