# Phase 6 + 7 — resilience and Causeway, on the fixed kernel (live mainnet)

Date: 2026-08-06 (UTC). Operator: Claude (agent).
Repos: `/Users/trent/gw-building/groundwire` @ `hd/cc-landing` **`a3460e9`**,
`/Users/trent/gw-building/urbit` @ `hd/cc-kernel` **`efe63a546e`**.
Nothing pushed.

---

## STEP 1 — kernel deploy

### The pill

Built a fresh solid pill from `hd/cc-kernel@efe63a546e` by the prescribed
recipe: boot a builder fakezod (`~zod`, pier `builder3`) from
`gw-cc-kernel-solid-FIXED.pill`, `kiln-merge` a `%gw-base` desk off `%base`,
`rsync -aL` the whole of `pkg/arvo` plus `tests/` into it, `|commit %.n`,
unmount, `fyrd (solid:pill …)`.

```
out    <scratchpad>/kerneltest/gw-cc-kernel-solid-P67.pill
size   11,749,129 bytes
sha256 0973a76c8cb6e62c65150bbbbef3af559cc7c613c4e3ebe832c601608fbf3dfd
```

`|commit` was poked with **`%.n`**, not `%.y` — `%.y` arms a repeating 1 Hz
`%dirk` timer that unmounting does not cancel. The commit needed one re-poke
(the mount→rsync→commit race the Phase-5b assets warn about); the script
re-pokes until a clay scry of the committed file matches the byte length on
disk, which it did.

### The pill contains both fixes — verified in the built artifact

A `~bus` fakeship booted from the new pill, then scried for its own
`%base` `sys/vane/ames.hoon`:

| probe | result |
|---|---|
| `[zuse hoon-version]` | `[408 135]` |
| `ames.hoon` byte length in the pill's clay | **556,396** = exactly the file at `efe63a546e` |
| `++  open-jam-shaped` | present, offset 17,187 |
| `(open-jam-shaped content.shot)` (the guard call in `+is-open-packet`) | present, offset 19,617 |
| `%give %fief (my [ship fief.point]~)` (the `+on-publ-full` push) | present, offset 445,015 |
| `++  on-publ-full` | present, offset 443,142 |
| `jael.hoon` byte length | 63,920 — unchanged, as the commit claims |

### …and the guard actually works, on the real bomb

Not just "the source is in there": the arm was lifted verbatim out of the
pill's own committed `ames.hoon` and run inside that kernel against the four
`$shut-packet` `.content` atoms captured off the mainnet wire during Phase 5b.

**First, the premise reproduced.** Evaluating the unguarded
`(mole |.((cue a)))` — the exact expression `+is-open-packet` used before
this commit — on the captured `C1_83B` atom **killed the event**. The other
three survived. So `%meme` really does escape `+mole`, and `C1_83B` really is
the bomb that livelocked C1↔C3 for six hours.

| test | result |
|---|---|
| guard verdict on all 4 captured mainnet shut packets | `%.n` ×4 — all rejected, bomb included |
| unguarded `(mole \|.((cue a)))` per atom | `C1_83B` **EVENT DIED**; other 3 survivable |
| genuine `(jam [signature=@ signed=@])` | accepted, and round-trips through `+cue` |
| 200 attestation-shaped jams of assorted sizes | **200/200 accepted** |
| 4,000 random ciphertext-shaped atoms (32/64/81/96 B) | **0 accepted** |
| ship alive afterwards | yes |

So the guard is not merely conservative — it admits every real attestation
shape tried and no random ciphertext at all.

### Choosing each comet's boot feed — the part that could have gone wrong

`-w` and `-G` were given together (a bare `-G` self-mines). But
`.gw-comet-N.json` carries up to **three** feeds, and they are not
interchangeable: each is a `$dawn-1`
`[[%2 ~] who=ship ryf=rift kyz=(list [lyf=life key=ring])]`, and jael's
`%dawn` sets `lyf.own.pki` from `i.kyz`. Booting the wrong one gives the
right `@p` at the **wrong life**.

Every candidate was decoded through a real ship's own `(slaw %uw)` + `+cue`
and gated on `fig:ex` reproducing the artifact's `@p` before anything was
stopped:

| comet | feed used | life | pass | why |
|---|---|---|---|---|
| **C1** | `feed_with_xtr_baked` | 3 | 405 B | confidential — the custody log must ride in the pass |
| **C2** | `feed_with_xtr_baked` | **2** | 371 B | see below |
| **C3** | `feed_public_no_xtr` | 2 | 108 B | public — its opening is on chain; this is byte-identical to what the old pier was actually running (confirmed from the live `-G` argument in `ps`) |

**C2 is a deliberate, documented change.** It was running at kernel life 1 on
a 293-byte pass — the pre-rescue, confidential, life-1 xtr — while its
on-chain snapshot has been at **life 2** since block 961,217. That mismatch is
Phase 5b's leading explanation for the 44 attestations C2 received and never
acted on. Booting it on the life-2 feed costs nothing (identical `@p`,
identical `cry` key `0x6f7415dc…`, no transaction) and is exactly the kernel
`%rekey` Phase 5b said was needed but could not perform. The alternative,
`feed_public_no_xtr` at life 2, was rejected because `a3460e9` implements
declassification only for a scanner that *already tracks* the comet — a
freshly-booted peer is a stranger to C2 and cannot learn it from the chain, so
C2 still needs the xtr-bearing pass on the packet path.

### Deploy

Piers renamed aside as backups (`q1.pre-p67` 2.2 G, `q2.pre-p67` 2.2 G,
`q3.pre-p67` 1.7 G — nothing deleted), new piers `r1`/`r2`/`r3` booted from
the P67 pill with the feeds above. C3 is on N1 at `-p 34343` as required.

C3 did not answer SIGTERM within 120 s and was SIGKILLed; its pier is
crash-safe and was preserved intact as a backup regardless.

**All three came back at the right `@p`:**

| ship | `@p` | life | pass | port | droplet |
|---|---|---|---|---|---|
| C1 | `~havnyl-lonpub-botben-hidleb--lomper-marryc-lanmec-daplyd` | 3 | 405 B | 47908 | N2 |
| C2 | `~barpyx-tirmur-sovrex-dolfet--rivsyx-pidtud-ronmeb-daplyd` | 2 | 371 B | 49818 | N3 |
| C3 | `~ligdes-risbur-folmus-mattyp--firpec-lispec-noddyl-daplyd` | 2 | 108 B | 34343 | N1 |

`fig:ex` of each booted ship's live ring reproduces its artifact `@p` exactly
(C1 `235146299140078997037850464130958181069`, C2
`244023470154023229697843905006977303245`, C3
`148665118248982927679614113796747313869`) — the same three integers the
pre-deploy ships reported.

The `%gw-btc` / `%bitcoin-client` / `%tcp` desks were reinstalled from
`/opt/gw/desks/gw-p4`, whose 72 tracked files hash **identically** to the
repo working tree at `a3460e9` (aggregate md5 `a1bac519…` on both sides), so
the desk under test is the branch tip.

### The fix is live on all three mainnet ships

Each ship was asked to read its **own** `%base` `sys/vane/ames.hoon` out of
clay after boot:

```
                       C1                C2                C3
who                    ~havnyl-…         ~barpyx-…         ~ligdes-…
life                   3                 2                 2
kelvin [zuse hoon]     [408 135]         [408 135]         [408 135]
ames.hoon bytes        556396            556396            556396
++open-jam-shaped   @  17187             17187             17187
guard call          @  19617             19617             19617
fief push           @  445015            445015            445015
jael.hoon bytes        63920             63920             63920
```

Byte-for-byte the source at `efe63a546e`, on every live ship.

---

## STEP 3 — Phase 7 (Causeway)

Reported before Phase 6 because it is the part that finished; Phase 6's
network tests wait on a light client and are below.

### 7.1 Headless spawn over SSH — **PARTIAL, and it fails badly**

Causeway's CLI itself is fine headless. Run with no TTY, no `DISPLAY`, no
`TERM`, stdin at `/dev/null`:

- `causeway --help` — fine.
- `causeway proof verify --onchain` — fine (see 7.3).
- `causeway spawn connect --xpub <real account xpub> --sponsor ~ligdes-…` —
  parses the key source, warns correctly about the missing master
  fingerprint, scans for UTXOs, and exits cleanly with
  `No UTXOs found. Fund one of your xpub's addresses and try again.`
  That is the true state of the world: the funding wallet
  `bc1ptv5pf62…` has `funded 6000 / spent 6000`, **0 UTXOs** — every sat is
  already inside C1/C2/C3's identity outputs, which must not be spent.

Beyond that point the flow is **not** headless, and this is a real defect
rather than a missing feature. `causeway.py` has four blocking `input()`
calls with no flag to supply the answer:

| line | prompt | behaviour on EOF (i.e. headless) |
|---|---|---|
| 123 | `confirm_master_ticket` | `except EOFError: print(); continue` → **infinite busy loop** |
| 2211 | `Pick a UTXO number:` | `EOFError` **uncaught** → traceback |
| 2304 | `confirm_seed_saved` | `except EOFError: print(); continue` → **infinite busy loop** |
| 2786 | `Signed PSBT (base64) >` | `sys.exit(1)` — the only one that behaves |

The two `continue`-on-EOF loops are not a graceful refusal. Measured, on
`causeway spawn generate --sponsor ~ligdes-… < /dev/null`:

```
t+21s   100.0% CPU   7,696,787 lines of "  > " written
t+31s   100.0% CPU  11,453,252 lines   (57 MB)
```

≈110 MB/min of prompt spam at a pinned core, forever. On a small VM that is
a disk-exhaustion bug, reached by the single most obvious way to script the
tool. `--blind-mnemonic` avoids the seed-confirm prompt; there is **no**
equivalent for UTXO selection or for handing back a signed PSBT.

**Verdict: 7.1 FAILS as specified.** A fully headless spawn is not possible
today, and attempting one can fill the disk. Fixes are small and obvious:
make the EOF branches exit non-zero, and add `--utxo <txid:vout>` and
`--signed-psbt <file|->`.

### 7.2 Causeway pokes an attestation through to `%gw-btc` — **PASS end-to-end**

`causeway.py:format_custody_entry_poke` is the function Phase 5b finding 6
condemned for hardcoding the snapshot's fief to `~`, which made the printed
`%anew` line unable to match the on-chain state-key commitment for any comet
with a fief — and `%anew`'s failure path is silence, so the operator saw a
poke that did nothing, forever.

**Fixed and verified.** All **7** custody entries across the three live
comets were rendered by Causeway and, independently, by a from-scratch
re-implementation, and compared character by character:

| comet | entries | Causeway == correct? |
|---|---|---|
| C1 | 3 (life 1, 2, 3) | yes, yes, yes |
| C2 | 2 (life 1, 2) | yes, yes — including `` `[%if .206.189.188.16 49.818]`` |
| C3 | 2 (life 1, 2) | yes, yes — including `` `[%if .64.227.13.22 34.343]`` |

i.e. the two entries that finding 6 got wrong are now right, and nothing
that was right regressed.

### 7.3 Causeway ↔ ship state synchronisation

`causeway proof verify --onchain`, headless, against live mainnet, on the
proofs stored in the three comet artifacts:

```
C1 proof (state-update, life 3)  ->  OK — OK (confirmed)
C2 proof (state-update, life 2)  ->  OK — OK (confirmed)
C3 proof (state-update, life 2)  ->  OK — OK (confirmed)
```

Causeway's view, the chain, and the artifacts agree.

### 7.4 Destroy the seed and attempt recovery — **the finale**

Two halves, both done for real. No mainnet sats were spent: the funding
wallet is empty and C1/C2/C3's sats are their identities, so the destructive
half used a **throwaway comet minted for this test** and the recoverable half
used **C3's real, already-published mainnet spawn**.

#### (a) Confidential — genuinely unrecoverable

A fresh 12-word BIP-39 phrase was generated *inside a single process*,
`blind_seed = derive_blind_seed(seed, funding_txid, vout)`,
`blind = H_tag('gw/spawn-blind', blind_seed)`,
`d = H_tag('gw/spawn-commit', jam(spawn-sont) || blind)` — and then a **real
suite-C comet was mined under that `dat`** with the real `comet_miner`,
through Causeway's own `run_comet_miner`:

```
comet  ~forput-racfyl-dirrem-hinsym--mottep-patryc-latlud-daplyd
d      802f7f6c39d46eb28e6c8a9ba615f8f6d357373deeb0a740b50a8543c9c4338a
```

The phrase, the BIP-39 seed, the blind seed and the blind were **never
printed and never written to disk**; only `sha256(blind)` was kept, as a
checker that can confirm a recovery but cannot shortcut one. When the process
exited they ceased to exist.

*(The first run of this drill printed the blind "for the record" and thereby
leaked it into a log — the exact mistake the property is about. That run was
discarded and its artifacts shredded; the numbers above are from a clean
re-run.)*

Recovery attempt, holding everything a real owner who lost their phrase would
still have — the funding outpoint (public, on chain) and `d` (baked into the
`@p`):

```
2,000,000 candidate blinds tried in 60s (33,449/s)   found: False
expected time to exhaust the 2^256 blind space: ~1.1e65 years
```

And the same conclusion from the **ship's own verifier** rather than Python.
`lib/gw-btc-pass` was built out of the live C3's clay and run directly:

| probe | result |
|---|---|
| forged blind `0x0` opens the throwaway's `d` | `%.n` |
| forged blind = C3's real blind opens it | `%.n` |
| forged blind `(shas %guess 1)` / `(shas %guess 2)` | `%.n`, `%.n` |
| `+verify-dat` with a forged blind | `%.n` |

`+verify-dat`'s `=(d.u.psd (spawn-commit spawn.open blind.open))` is the same
predicate that gates `+apply-spawn` (`urb-core:258`) and `+run-checks`
(`self-attestation:294`). It cannot be satisfied without the blind, so **no
peer will ever accept an attestation for that `@p` again**. The identity is
gone. That is the property the hiding commitment exists to provide, and it
holds.

#### (b) Public — survives, from the chain alone

C3 published its spawn. Its OP_RETURN at block **961,059** (263-byte script,
`6a 03 'urb' 01 09 4c fe <254-byte jam>`) was fetched from mempool.space and
cued with **no secret material whatsoever**:

```
pass          108 bytes
internal-key  0x2e129efeba5ace29c3e118634f568ca73ec84d0283695e0d497e9ad9cf9e87703
snapshot      life 1  rift 0  key 0xdf309632f565fddf…  sponsor ~  fief ~
blind-opening spawn 72340acb1b4216f3e1ff0da2232279e43326cd53083331fbf2ee7774daa9bc54:1 off 0
              blind ebaf37a76c4d7e4a029a3e50c7c81714e36eeb0af660534e083bad2976118409
```

Recomputing the commitment from that blind, twice independently:

```
python  spawn_commit(...)  a68fa950d20fb4f097a8b537df31ec6a4ed3093843830d3b7e76f56548dd6414
hoon    +spawn-commit:cc   0xa68fa950d20fb4f097a8b537df31ec6a4ed3093843830d3b7e76f56548dd6414
C3's committed d           a68fa950d20fb4f097a8b537df31ec6a4ed3093843830d3b7e76f56548dd6414   MATCH
```

`+verify-dat` with that blind: `%.y`.

And the counterpart, checked on chain: **C1's spawn tx (961,055) has exactly
one output and no OP_RETURN**, as does C2's (961,056). There is nothing
anywhere on the chain that could ever open their `dat`.

**7.4 PASSES, demonstrated rather than asserted.** Publication is not a
convenience: it is the difference between an identity whose opening lives on
the most durable database in the world and one whose opening lives only in
twelve words.

---

## STEP 2 — Phase 6 (resilience)

### 6.1 Attestation arrives while the light client is still syncing — **FAIL**

Expected: *fail-closed / silence; **NEVER** a false negative verdict.*
Observed: **a false negative verdict, and a snub.**

This was run as a natural experiment rather than a contrivance. Straight
after the three ships were rebooted onto the new kernel and the desks
installed — light clients at height 0, no peers seeded yet — C1's real live
405-byte pass was poked into C3 as a `%jael-writ`, twice.

The first minutes looked exactly right, and the new `/x/inflight` scry is
what made them readable at all:

```
gw-blockid   (0, 0)          <- light client at genesis
INFLIGHT     ~havnyl-…       <- writ 1 accepted, a job is running
             (writ 2 dropped by the single-flight guard)
snubbed      ('deny', ~)     <- nothing
ATTESTED     ~
[log]        (nothing at all)
```

and the ship's own light-client log showed the verification strand blocking
on the block it needed:

```
[%pending-block-height-reqs {[p=961.044 q={[%block-header ~]}]}]
```

Silent, fail-closed, no verdict. Correct so far. **Then the block scanner
was started (`%gw-index-from 961.040`) and the light client began serving
blocks, and the job completed — against a chain the client had barely
begun.** The verdict:

```
[%gw-btc-lc-scan-clean tip 0x3330.3356…6038 vout=0 off=0 from=961.196 to=0]
%gw-btc: attestation for ~havnyl-… is INVALID
  [ok] chain-nonempty      [ok] chain-bounded      [ok] fetch-count
  [ok] spawn-opening       [ok] spawn-commit       [ok] start-txid
  … 38 more [ok] …
  [ok] state-resolve       [ok] tip-unspent        [ok] tip-p2tr
  [ok] pass-key
  [XX] sponsor-known                                   <-- the only failure
  [ok] tracked-prefix
```

**43 of 44 checks passed. The one that failed was `sponsor-known`, and it
failed because C3 had not yet indexed its own publication.**

C1's snapshot names `~ligdes-…` — *C3 itself* — as its sponsor.
`+run-checks` requires a named sponsor to be in `known-public`, which
`+verify-cards` sources as `~(key by unv-ids.urb-state)`: the scanner's
public index. C3's scanner was at block 961,039 and C3's own public spawn
publication is in block **961,059**. So C3 did not yet know that C3 exists,
and refused an otherwise perfect attestation on that ground.

`sponsor-known` is deliberately classified as **fraud**, not staleness
(`lib/self-attestation:60` — `stale-checks` is only `tip-unspent`,
`tracked-tip`, `life-monotonic`). So the verdict became a Jael `%fail`,
and ames snubbed:

```
/x/inflight  ~                     (job finished)
snubbed      ('deny', ('~havnyl-lonpub-botben-hidleb--…-daplyd', 0))
```

**C3 has snubbed C1 for naming C3 as its sponsor.** This is Phase 5b
finding 10 all over again, one check to the left: `a3460e9` correctly moved
`tip-unspent` out of the fraud class, but `sponsor-known` can fail for
exactly the same reason — *our own view of the chain is not caught up* — and
still produces the maximally destructive outcome. The snub then blocks the
packet that would fix it.

**Root cause, precisely.** The readiness gate in `%jael-writ` is

```hoon
?~  best
  `this
```

i.e. *"have we ever received a `/best-block` update"*, not *"is the light
client usable"*. `%bitcoin-client` answers that subscription immediately
with the **genesis** block, so `best = [<genesis> 0]` and the gate opens
while the client is 961,000 blocks behind. Two consequences, both visible in
the one log line above:

1. `sponsor-known` is evaluated against an empty public index → false fraud
   verdict → snub. *(this test)*
2. `from=961.196 to=0` — the BIP-158 tip-unspent scan ran over an **empty
   range** and returned "clean". `tip-unspent` therefore passed on **zero
   evidence**. Here the tip really is unspent so no harm was done, but a
   spent tip would have read identically. That is a **fail-open** on the one
   check the design most wants to fail closed, and it is the mirror image of
   the same bug.

**Suggested fix**, in order of value:

- Gate on the light client actually being near the tip — e.g. require
  `best-height >= max(entry heights) + block-confirmations`, or simply hold
  the writ (do not drop it, do not judge it) until `is-synced`. Dropping is
  already safe; judging is not.
- Move `sponsor-known` into `stale-checks`, or better, make it
  *undeterminable* rather than false when the scanner has not yet reached
  the height at which the sponsor could have been published. A verifier that
  cannot see the sponsor has no evidence of fraud, only ignorance.
- Make an empty filter-scan range return `~` (undeterminable) rather than
  `[~ %.y]`; `a3460e9` already fails `tip-unspent` closed on an
  undeterminable fetch, so this would slot straight in.

**Severity: high.** It is reachable by any comet rebooting or joining, it
punishes the honest party, the punishment is a snub (which is sticky and
blocks its own remedy), and nothing in the logs names the cause unless you
already know to read the check list.

### 6.4 (runtime layer) Prolonged offline, then rejoin — **PASS, with supervision**

Not staged: C1's vere SIGSEGVed on its own at 04:08:29Z, mid filter-header
sync, immediately after its block scanner indexed C3's public spawn.

```
%gw-btc: WARNING ~ligdes-… commits neither a sponsor nor a fief   <- C3's spawn snapshot, indexed
newt: write failed broken pipe
loom: external fault: 0 (0x200000000 : 0x280000000)
  u3m_fault / sigsegv_handler / u3_king_commence (king.c:1136) / main (main.c:3409)
```

`gwsup.sh` noticed within its 30-second poll and relaunched:

```
2026-08-06T04:08:49Z [r1] INTERVENTION #3 VERE-DOWN (restart #1) -> relaunching
```

**20 seconds down, no manual intervention, no state loss** — the pier
replayed and carried on. This is the same `loom: external fault` class of
vere instability Phase 5 saw; it is a runtime bug, not a Groundwire one, and
the supervisor covers it. Worth recording that the crash came while the ship
was under maximum load (961 k headers, 231 live peers, block scanning and
filter-header sync concurrently).

### 6.5 Chain reorg behaviour — **CHARACTERISED, NOT PASSED** (known gap, and worse than "forward-only")

No reorg occurred during the session (mainnet tip 961,237 → 961,246, all
extensions), so this is a source characterisation backed by the observed
scanner behaviour, and it is stated as such.

`%bitcoin-client` **does** tell `%gw-btc` about reorgs: `/best-block`
carries `best-block:update` which is either `%new` or `%reorg-rollback`,
both with a height and a hash. `%gw-btc` handles them **identically**
(`app/gw-btc.hoon:890-899`):

```hoon
=/  new-best=id:block:bc
  ?-  -.upd
    %new             [block-hash.upd block-height.upd]
    %reorg-rollback  [block-hash.upd block-height.upd]
  ==
`this(best `new-best)
```

Only `best` moves. `block-id.urb-state` — the scanner's own cursor — and
`unv-ids` are untouched. Three consequences:

1. **The cursor never rewinds.** After a rollback the scanner is *ahead* of
   the new tip, so `+scan-again`'s
   `(lth num.block-id.st (sub num.u.tip block-confirmations))` is false and
   it simply idles until the new chain overtakes the old height. The
   orphaned range is **never rescanned**.
2. **Orphaned facts are permanent.** A spawn or state update indexed out of
   a block that later loses the race stays in `unv-ids` forever. There is no
   inverse of `+apply-spawn` / `+apply-state`.
3. **Facts unique to the winning chain are lost.** Anything that appears
   only in the replacement blocks falls in the range the cursor skipped.

`block-confirmations` is **1** (`:943`, commented `1 for alpha`), so a
one-block reorg — the common kind, several per month on mainnet — is enough
to hit this. Raising it does not fix the gap; it only makes it rarer.

The verification path inherits the same exposure from the other side: the
BIP-158 `tip-unspent` scan is evaluated against whatever chain the light
client currently holds, so a reorg that un-spends a tip flips a `%stale`
demotion back to valid only if something re-triggers verification, and a
reorg that spends one is not noticed until the next attestation.

**Recommended, not attempted here:** treat `%reorg-rollback` as a real
event — rewind `block-id.urb-state` to the rollback height and re-derive, or
at minimum log loudly and refuse to advance until an operator intervenes.
Silently continuing forward past a rollback is the one behaviour that cannot
be right.

### 6.7 Are failures diagnosable from logs alone? — **MUCH BETTER, still not yes**

Phase 5b's answer was a flat no, and its top recommendation was
"`/x/inflight` (and `/x/publicizing`) plus a `~&` on the non-obvious drops".
`a3460e9` shipped exactly that, and it **paid for itself within an hour**:

- The 6.1 investigation was only tractable because `/x/inflight` said
  `~havnyl-…` while the log said nothing. Without it, the observable was an
  attestation that produced no output — indistinguishable from the eight
  other silent-drop returns, which is precisely the wall Phase 5b hit.
- The negative verdict prints its **entire 44-check list** with `[ok]`/`[XX]`
  per check. `[XX] sponsor-known` named the root cause of a mainnet snub in
  one line. That is the difference between a two-minute diagnosis and a
  345-sat transaction.

What is still not diagnosable from logs alone:

1. **The snub itself is silent.** C3 snubbed C1 and the log contains no
   `snubbing` line; the only way to see it was `.^(… /snubbed)`. Phase 5b
   *did* see `mesa: …: attestation writ failed; snubbing`, so this line
   appears to be emitted on the packet path but not on an operator `%writ`
   poke. Either way, "who have I snubbed and why" is not in the log.
2. **The light-client readiness gate is invisible.** `?~ best` drops a writ
   with no log line, and nothing exposes `best`. A ship that is quietly
   dropping every attestation because its `/best-block` subscription was
   kicked looks identical to a ship nobody is talking to.
3. **`from=… to=0` was the only hint** that the filter scan had done nothing,
   and it reads like a normal scan line. An empty or degenerate scan range
   should say so.

### 6.6 Two peers disagree about a third — **OBSERVED, and it does not reconcile**

Not staged: it fell out of 6.1. At 04:15Z the three ships held three
mutually inconsistent views of the same two on-chain facts:

| holder | view of C1 | view of C3 |
|---|---|---|
| **C1** | (self, life 3) | jael point, life 1, `dome=%gw-btc`, from its own scanner — *public spawn only*, so `forward-lane` is still the self-projection `[%& ~ligdes-…]` because it has not yet scanned the life-2 fief at 961,129 |
| **C2** | nothing | nothing (scanner behind) |
| **C3** | **SNUBBED** (fraud verdict, 6.1) | (self, life 2) |

Every one of these is a correct application of each ship's own rules to the
part of the chain it has seen. Nothing in the protocol makes them converge:
there is no gossip of verdicts, no expiry on a snub, and no re-verification
trigger other than a fresh packet — which the snub blocks. C1 and C3 are
each other's sponsor/sponsee and neither can tell the other that it is
wrong.

The interesting half is that **the disagreement is a pure function of scan
position**, not of any dispute about evidence. Two honest ships with the
same code and the same chain reach opposite verdicts on the same
attestation depending only on how far each has indexed. That is the same
root cause as 6.1, seen from the network rather than from one ship.

### Infrastructure note: light-client bring-up dominated this run

Rebooting onto a new pill means three fresh piers and three light clients
syncing mainnet **from genesis**. Measured on 2-vCPU droplets:

| stage | rate | wall clock |
|---|---|---|
| block headers 0 → 961,247 | ~16 k/min once ≥100 peers | ~55 min |
| filter headers 0 → 961,2xx | ~10 k/min | ~95 min |

Both are strictly serial: `+continue-syncing-headers` asks **one** peer for
2,000 headers and waits (`bitcoin-client.hoon:999-1034`), so extra peers buy
resilience, not throughput. Phase 0's "~8 min" estimate is off by an order
of magnitude.

Two things bit repeatedly and are worth writing down:

- **Bulk-adding peers kills the tcp-sidecar.** Adding 200–300
  `%add-earth-peer`s at once reliably produced `--- CRASH: signal 11 ---`
  in `/opt/gw/sc-*.log`, after which `live-earth-peers` went to **0** and
  sync stalled until `gwsup.sh` restarted it and re-seeded (with one peer).
  Batches of ~25 are stable. This is the same `gwbtc/node#1` sidecar
  fragility, with a reproducible trigger.
- **A syncing ship's `conn.sock` is effectively unusable.** Scries and pokes
  time out at 120–380 s while filter-header batches are being processed, so
  the operator loses observability exactly when the ship is doing the thing
  they want to watch. Several probes in this run returned nothing for that
  reason and had to be retried later.

### 6.1 (continued) The retry — root cause confirmed, and a second failure mode

Once C3's **block scanner** passed 961,059 — the height of C3's own public
spawn publication — `/x/points` on C3 gained `~ligdes-…`, i.e. C3 finally
knew that C3 exists. The snub was cleared (`snubbed ('deny', ~)`) and the
identical writ was poked again.

```
gw-blockid   (…, 961089)          <- scanner past C3's own publication
gw-points    ~ligdes-…            <- sponsor-known can now succeed
snubbed      ('deny', ~)
INFLIGHT     ~havnyl-…            <- accepted, second attempt
```

This is the retry the test asks for, and it confirms the diagnosis: the
verdict was a pure function of scan position, not of the evidence. But it
also exposed the **second** failure mode, which is starvation:

```
%gw-btc: block thread failed, retrying
```

`+block-fetch-timeout` (`~m5`) fired on the *scanner's* block fetch while
the light client was busy with filter-header sync. The verification strand
is fed by the same `%light-client` `/block/height/<h>` endpoint, so a ship
that is still syncing cannot serve verification and index blocks at the same
time; the job sits in `inflight` and neither succeeds nor fails.

That is fail-closed, which is right. But the single-flight slot is held for
up to `+stuck-job-guard` = **2 hours**, during which *every* further
attestation from that peer is dropped silently. So the honest reading of
6.2's "retry succeeds" is: **a retry cannot even be attempted for two hours
after a starved verification**, and the only reason the retry above was
possible at all is that the *first* job had already been resolved (into the
wrong verdict).

---

## Summary

### Kernel deploy

| item | result |
|---|---|
| pill built from `efe63a546e` | **DONE** — `gw-cc-kernel-solid-P67.pill`, sha256 `0973a76c…` |
| both fixes present in the built pill | **VERIFIED** by scrying the pill's own `sys/vane/ames.hoon` |
| `+open-jam-shaped` rejects the real mainnet bomb | **VERIFIED** — and the unguarded `+cue` still kills the event, proving the premise |
| deployed to N1/N2/N3, piers rebooted | **DONE**, old piers preserved as `q*.pre-p67` |
| each comet back at the right `@p` | **YES** — C1/C2/C3 all reproduce their artifact `fig:ex` |
| C3 on N1 at `-p 34343` | **YES** |
| `bail: meme` | **0 on all three ships** over **3,831** live comet packets (was 378 on the old C3 pier) |

### Phase 6

| # | test | result |
|---|---|---|
| 6.1 | attestation while light client unsynced | **FAIL** — false INVALID + snub; root cause found (`?~ best` readiness gate + `sponsor-known` classed as fraud) |
| 6.2 | kill/restart light client mid-verification | **PARTIAL** — fail-closed confirmed (no verdict, no snub); "retry succeeds" **eventually** (C3 re-judged the same attestation VALID once synced) but **not after a runtime death**, which strands the single-flight slot for 2 h |
| 6.3 | network partition, then heal | **PASS** — 0 packets and 0 verdicts through the cut, immediate automatic recovery on heal |
| 6.4 | prolonged offline, then rejoin | **PASS** — at the runtime layer (unplanned vere SIGSEGV, supervisor recovered it in 20 s, no state loss) and at the protocol layer (C1↔C3 recovered from a 2-hour snubbed/unverified state with no operator action once the light client caught up) |
| 6.5 | chain reorg | **CHARACTERISED** — worse than "forward-only": `%reorg-rollback` is handled identically to `%new`, the scan cursor never rewinds, orphaned index entries are permanent |
| 6.6 | two peers disagree about a third | **OBSERVED** — three ships, three irreconcilable views, all a pure function of scan position |
| 6.7 | diagnosable from logs alone? | **MUCH BETTER** (`a3460e9`'s scries + the printed check list paid for themselves within the hour) but still **no** — the snub itself, the readiness drop, and a degenerate scan range are all silent |

### Phase 7

| # | test | result |
|---|---|---|
| 7.1 | headless spawn over SSH | **FAIL** — four blocking `input()` calls with no flags; two `continue` on EOF, producing a 100 %-CPU infinite loop that wrote 57 MB of `  > ` in 31 s |
| 7.2 | Causeway pokes an attestation to `%gw-btc` | **PASS end-to-end** — all 7 custody entries across 3 comets match a from-scratch re-implementation exactly including both fiefs (Phase 5b finding 6 fixed), and the printed poke drove C2's full chain re-verification: `/x/custody` 0 → 2 entries |
| 7.3 | Causeway ↔ ship state sync | **PASS** — all three proofs verify against live mainnet, headless |
| 7.4 | **destroy the seed, attempt recovery** | **PASS** — confidential identity provably unrecoverable (2 M brute-force attempts, `+verify-dat` refuses every forged blind); public identity fully recoverable from its on-chain OP_RETURN with zero secret material |

---

## The second kernel fix, proven on mainnet — and what it still does not buy

`+on-publ-full` pushing a verdict-installed point's fief to the runtime was
the fix for Phase 5b finding 7 ("a verified fief becomes a jael point but
never a runtime route"). It is now demonstrably live.

C2's real 371-byte life-2 pass was poked into the freshly-synced C1 as a
`%jael-writ`. C1 ran the full chain verification and:

```
[%gw-btc-lc-scan-clean …]
%gw-btc: attestation for ~barpyx-… is VALID
ames: lamp ~barpyx-… static ip .206.189.188.16 port 49818     <-- NEW
```

That `lamp` line is precisely what Phase 5b reported as absent: *"No `lamp`
line for C2 appeared in C1's runtime log, while C3's is right there."* It
appears now, from a `%writ` verdict, with no scanner udiff involved (C2's
spawn carries no OP_RETURN, so C1's scanner cannot index it — `/x/points`
still lists only `~ligdes-…`). C1's state afterwards:

```
CONFIDENTIAL  ~barpyx-…        ATTESTED  ~barpyx-…
lyfe          [~ 2]            dome      [~ %gw-btc]
sein          ~ligdes-…        jael fief ~barpyx-… .206.189.188.16 49818
```

**But C1 still cannot reach C2 directly, and the reason is one level up.**
With `%spew` verbosity on, a `|hi` to C2 produces:

```
ames: ~barpyx-…: send-blob: to ~barpyx-…
ames: ~barpyx-…: no route to:  ~barpyx-…
ames: ~barpyx-…: trying route: ~ligdes-…
ames: ~barpyx-…: dead [[1 0] rto=120.000 … counter=0]
```

`+send-blob-via` (`ames.hoon:6081-6128`) chooses a lane from
`route.u.ship-state` — **the lane learned from a heard packet** — and
nothing else. If that is `~` it prints `no route to` and calls
`+try-next-sponsor`. The fief never enters that decision: it lives only in
the *runtime's* lamp table, which resolves `[%& ship]` lanes, and ames emits
`[%& ship]` only for **sponsors**.

So the fief works exactly when its holder is reached *as a sponsor* — which
is why C3's fief has always worked (C1 routes to C3 as its sponsor,
`trying route: ~ligdes-…`, and the runtime resolves it from C3's lamp) and
why C2's still does not. Verified in the same session: C1→C3 is a live path,
30 UDP packets captured on C3's port 34343 from C1 in a 70-second window.

This is not a regression and not a defect in `efe63a546e` — the commit does
what it claims, and Phase 5b's diagnosis ("the push is missing") was correct
as far as it went. It is a **second, independent gap**: to route to a comet
by its committed fief, ames needs to *offer* the runtime a `[%& her]` lane
for a peer with `fief=^` and no heard route, or `+send-blob-via` needs to
read the fief itself. One line of design, and without it an on-chain fief is
only useful to sponsors.

---

## What to fix, in order

1. **The light-client readiness gate is `?~ best`, and that is not
   readiness** (`app/gw-btc.hoon`, the `%jael-writ` arm). `%bitcoin-client`
   answers `/best-block` with the **genesis** block the moment the
   subscription opens, so a ship 961,000 blocks behind passes the gate and
   judges attestations against a chain it has not seen. This produced a
   false `INVALID` and a **snub of an honest peer by its own sponsor**,
   live, on mainnet, within minutes of a reboot. Gate on `is-synced`, or at
   minimum on `best-height` covering the log's own heights.

2. **`sponsor-known` must not be fraud.** It is currently in the same class
   as a forged commitment (`lib/self-attestation:60`), but it fails whenever
   *our* scanner has not yet reached the block where the sponsor published.
   That is ignorance, not evidence. Either add it to `stale-checks` or make
   it three-valued (`known` / `unknown-because-unscanned` / `absent`). This
   is the same lesson as Phase 5b finding 10, which `a3460e9` fixed for
   `tip-unspent` only.

3. **An empty filter-scan range must be undeterminable, not "clean."**
   `[%gw-btc-lc-scan-clean … from=961.196 to=0]` passed `tip-unspent` having
   examined **zero** blocks. `a3460e9` already fails `tip-unspent` closed on
   an undeterminable fetch; a degenerate range belongs in the same branch.

4. **`+send-blob-via` never consults a peer's fief.** `efe63a546e` correctly
   gets the fief into the runtime (proven: `ames: lamp ~barpyx-… static ip
   .206.189.188.16 port 49818` from a `%writ` verdict), but ames only ever
   offers the runtime a `[%& ship]` lane for **sponsors**, so a comet with a
   committed fief and no prior contact is still routed to its sponsor
   instead. An on-chain fief is currently useful only to sponsors.

5. **A starved verification holds the single-flight slot for 2 hours.**
   `+stuck-job-guard` is `~h2` and `inflight` is only cleared by a verdict
   or that timer, so one attestation that arrives while the light client is
   busy silences that peer for two hours. Release the slot when the strand
   reports the *light client* failed (as distinct from the attestation
   failing), the way `+block-fetch-timeout` already does for the scanner.

6. **`%reorg-rollback` is handled identically to `%new`** — see 6.5. The
   scan cursor never rewinds and orphaned index entries are permanent.

7. **Causeway's headless EOF loops** — `confirm_seed_saved` (2304) and
   `confirm_master_ticket` (123) `continue` on `EOFError`, producing a
   100 %-CPU infinite loop that writes ~110 MB/min. Exit non-zero instead,
   and add `--utxo` and `--signed-psbt` so a scripted spawn is possible at
   all.

8. **Make the snub visible.** C3 snubbed C1 and printed nothing. `/snubbed`
   is the only witness.

---

## Phase 6, the staged tests

### 6.2 Kill and restart the light client mid-verification — **fail-closed YES, "retry succeeds" NO**

Run on C1 once its light client was fully synced (`is-synced %.y`, headers
and filter headers both 961,259). C2's real pass was poked as a `%jael-writ`,
`/x/inflight` confirmed a job was running, and 20 seconds in the light
client's transport was killed:

```
--- KILL sidecar (light client transport) ---
killing 211098
newt: write failed broken pipe
```

Killing the sidecar took the runtime down with it (`newt: write failed`),
`gwsup.sh` relaunched it 106 s later, and the ship replayed and re-pushed
its whole lamp table on `%born` — including, incidentally, a third
confirmation of the fix under test:

```
ames: lamp ~barpyx-… static ip .206.189.188.16 port 49818
```

**The safety half passes cleanly.** After the kill:

```
INFLIGHT   ~barpyx-…      ATTESTED  ~barpyx-…     snubbed  ('deny', ~)
```

No verdict, no `INVALID`, no snub. An infrastructure failure did not become
evidence against the peer. That is exactly the specified behaviour.

**The liveness half fails.** The strand died with the runtime, but
`inflight` is agent state and survived the replay, so the ship now believes
a verification is in progress that no longer exists. Two further writs were
poked:

```
writ 1: ok in 0.25s
writ 2: ok in 0.21s
(no verdict, no log line at all)
INFLIGHT   ~barpyx-…
```

Both were swallowed by the single-flight guard, silently. Nothing will clear
that slot except `+stuck-job-guard`, **two hours** later. So a retry is not
merely unproven — it is **impossible for two hours** after a runtime fault
during verification.

This is the precise reproduction of Phase 5b finding 9's leading hypothesis
("C2's runtime segfaulted during Phase 5 *specifically while running the
confidential verification strand for C1*, which is exactly the shape that
would strand `inflight[~havnyl-…]`"). It is now a demonstrated fact rather
than an inference, and `/x/inflight` is what made it visible.

### 6.3 Network partition between two comets, then heal — **PASS**

C1↔C3 is a live path (C3 is C1's on-chain sponsor and its fief resolves in
the runtime), so the partition was applied at N1's firewall against N2's
address only, leaving both ships and both light clients untouched.

| phase | C3: `received packet` | C3: `got attestation` | C3: verdicts | C3: `bail: meme` |
|---|---|---|---|---|
| baseline (10 `\|hi`) | >0, 8 UDP captured | yes | — | 0 |
| **partitioned** (15 `\|hi`) | **0** | **0** | **0** | 0 |
| healed (15 `\|hi`) | **6** | **6** | — | 0 |

During the cut, C1's pump behaved correctly — exponential backoff to the
`rto=120.000` ceiling, `trying route: ~ligdes-…` repeatedly, no state
corruption:

```
ames: ~ligdes-…: dead [[1 0] rto=120.000 rtt=1.000 rttvar=1.000
                       ssthresh=1 cwnd=1 num-live=1 counter=0]
```

**No false verdicts on either side, and recovery on heal was immediate and
automatic** — the first `|hi` after `iptables -D` arrived. No operator
action beyond removing the rule.

(Note for anyone repeating this: `tcpdump` taps below `iptables`, so it
still shows the packets arriving on the wire during the cut. The ship-level
counters are the real measurement.)

---

## Did `bail: meme` stop? — **YES, on 3,831 live mainnet comet packets**

The old C3 pier logged **378** `bail: meme`, every one immediately preceded
by `ames: ~havnyl-…: received packet`, and the livelock never cleared in six
hours. On the new kernel:

Once C1 and C2 had mutually verified, `helm-send-hi` was driven in both
directions with a per-message counter in the payload — AES-SIV is
deterministic, so a repeated payload would produce one ciphertext tested
many times rather than many ciphertexts tested once.

| round | messages | packets received by C1 | `bail: meme` | any `bail` |
|---|---|---|---|---|
| 1 | 200 (C2→C1) | 201 | **0** | 0 |
| 2 | 900 each way | 1,810 | **0** | 0 |
| 3 | 900 each way | 1,820 | **0** | 0 |
| **total** | | **3,831** | **0** | **0** |

`+open-jam-shaped` runs on every one of those, since `+is-open-packet` is
called on every `$shot` from a comet. The commit puts the bomb rate at
roughly **1 packet in 500** (root tag `%11` × ≥7 leading zeros in the `+mat`
index), so 3,831 packets is an expectation of ~7.7 bombs and
P(zero by luck) ≈ **0.05 %**. On the old kernel each of those would have
been a dead event and an unacked packet.

C2 and C3 also logged 0 `bail: meme` for the whole session.

Combined with the direct test — the captured `C1_83B` atom still kills an
event when handed to the unguarded `(mole |.((cue a)))` inside this very
kernel, and is rejected by the guard — **the `%meme` livelock is fixed.**

---

## 7.2, run end-to-end against the chain

The encoder check above is only half the test. The dojo line Causeway printed
for **C2's real life-2 custody entry** was then poked into the live C2 —
whose `/x/custody` was empty — through `%gw-btc`'s `%noun` mark, and the
agent did the whole owner-driven refresh:

```
[%gw-btc-lc-scan-clean …]
%gw-btc: attestation for ~barpyx-… is VALID
%gw-btc: custody log verified (2 entries); refreshing our pass
```

`/x/custody` went from **0 entries** to the full 2-entry log, each with the
snapshot its transaction committed. That is `+verify-lc` refetching every
transaction by `[height txid]`, rewalking continuity through input 0,
recomputing both `state-key` commitments, running a BIP-158 filter scan to
prove the tip unspent, rebuilding the pass with `+with-xtr` and confirming it
still hashes to `~barpyx-…`. Zero sats. **The wallet → CLI → ship → chain →
verifier loop closes.**

### …but `%anew` has the same stranded-slot hazard as `inflight`

The identical poke on **C1** produced nothing, twice, and `/x/custody` stayed
empty. Every precondition of `+begin-anew` that can be observed from outside
was reproduced against C1's and C2's own `lib/gw-btc-pass` and all pass:

```
dom %gw-btc   kel 9   xtr-entries 2   xtr-canonical %.y
with-xtr rebuilds 371 B, byte-identical to the original, fig still ~barpyx-…
```

and C1's scanner is advancing block-by-block with `settled-tip`, which proves
`best` is set. That leaves exactly one of `+begin-anew`'s **seven** silent
`state early returns: `pending.own` still holds the job the first poke
started, whose strand never returned. Like `inflight`, it is cleared only by
a verdict or by a two-hour leak backstop — and unlike `inflight`, **there is
no scry for it**. `a3460e9` added `/x/inflight` and `/x/publicizing`; it
should add `/x/pending-own` and a `~&` on `+begin-anew`'s seven refusals for
the same reason.

---

## Reliability over the session

≈5 h 40 m, three droplets, three live mainnet comets.

| event | count | covered by |
|---|---|---|
| vere SIGSEGV (`loom: external fault`) | 1 (C1) | `gwsup.sh`, 20 s down, no state loss |
| tcp-sidecar SIGSEGV | 9 (C1), 6+ (C2), 9 (C3) | `gwsup.sh` restart + reseed; **triggered reproducibly by bulk `%add-earth-peer`** |
| `spider crashed … %arvo-response` | 38 (C3) | pre-existing; did not strand a slot this run |
| `bail: meme` | **0 across all three ships** | the fix under test |
| transactions broadcast | **0** | nothing needed one; C1 1445 / C2 1544 / C3 1288 sats untouched |
| disk (droplets) | 87–98 GB free throughout | |
| disk (Mac) | 29 GB free; one 57 MB runaway log from 7.1, removed | |

Two Phase-5b hazards did NOT recur: no `%not-mounted` storm (every
`kiln-commit` used `%.n`) and no desk left mounted.

A third is now understood rather than feared: `%gw-btc: verification thread
for … ended without a verdict` **does** release the `inflight` slot — the
`[%verify ship job]` wire deletes the entry before dispatching on the sign
(`app/gw-btc.hoon:602-609`). The slot is stranded only when the **runtime
dies**, because then no `%khan %arow` ever arrives. That is a sharper
statement than Phase 5b could make, and it narrows the fix: the leak is
specific to runtime faults, not to strand failures.

## Reproduction assets

Session scratchpad, `p67/`:

- `gwstat.py` — status probe including `a3460e9`'s four new scries.
- `mypass.py` — a ship's live jael pass (not the artifact's `pass_atom_hex`).
- `kernelcheck.py` — proves a live ship is running the fixed ames.
- `seedloss.py` — the 7.4 drill: `mint` (real miner, phrase destroyed) and
  `attack` (brute force). `throwaway.json` holds only public values.
- `hoonseed.py` — 7.4 through the ship's own `lib/gw-btc-pass`.
- `mkanew.py` / `anew-c{1,2,3}-e*.hoon` / `anewpoke.py` — 7.2.
- `hiflood.py` — counter-varied `|hi` traffic for the `%meme` test.
- `partition.sh` — 6.3, tagged iptables rules with a safe `heal`.
- `mark.sh`, `phase6.sh`, `syncwatch.sh`, `feeder.sh` — operations.
- `kerneltest/`: `build_solid_p67.py`, `verify_pill_p67.py`,
  `guardtest2.py`, `feeddecode2.py`, `bootp67.sh`, `stopq.sh`.
- On the droplets: `/opt/gw/{bootp67.sh,stopq.sh,gwstat.py,mypass.py,
  kernelcheck.py,hiflood.py,partition.sh,anewpoke.py}` and
  `/opt/gw/pills/gw-cc-kernel-solid-P67.pill`.

Pier backups (nothing deleted): `/opt/gw/piers/q{1,2,3}.pre-p67`.
---

## 6.1's root cause, confirmed by the control

The claim in 6.1 is that C3's `INVALID` was a function of *scan position*,
not of the evidence. The controls make that airtight: **the very same
attestations verified VALID on the two ships whose light clients were
caught up.**

| verifier | light client | scanner | attestation | verdict |
|---|---|---|---|---|
| C3 | height 0 → mid-sync | 961,039 (before C3's own publication at 961,059) | C1's life-3 pass | **INVALID**, `[XX] sponsor-known`, → snub |
| C1 | `is-synced %.y`, 961,259 | 961,257 | C2's life-2 pass | **VALID** + fief lamp |
| C2 | `is-synced %.y`, 961,265 | 961,263 | C1's life-3 pass | **VALID**, point at life 3 |

Same code, same chain, same evidence — opposite verdicts, decided entirely
by how far the verifier had indexed. And the one that was behind did not
merely abstain: it snubbed.

**And then the control closed on C3 itself.** While its filter-header sync
finished, C3 kept cycling verification attempts against a light client that
could not yet serve blocks (`%gw-btc: block thread failed, retrying`,
`verification thread for ~havnyl-… ended without a verdict` — both correct
fail-closed behaviour, neither producing a further false verdict or snub).
At 08:08Z C3 reached `is-synced %.y`, and the **same attestation from the
same peer on the same ship** was re-judged:

```
[%gw-btc-lc-scan-clean …]
%gw-btc: attestation for ~barpyx-… is VALID
ames: lamp ~barpyx-… static ip .206.189.188.16 port 49818
%gw-btc: attestation for ~havnyl-… is VALID      <-- INVALID two hours earlier
```

INVALID → VALID with **nothing changed but the light client catching up**,
and no operator action: C1's own retransmitted attestation packets drove the
retry. That is simultaneously

- the proof of 6.1's root cause (scan position, not evidence),
- the "retry succeeds" half of **6.2**, once the ship is not starved, and
- **6.4** proper — a long-unreachable peer relationship recovered on its own.

Final state, all three comets mutually verified on the fixed kernel:

| ship | life | verified peers (`ATTESTED`) | snubbed | `bail: meme` |
|---|---|---|---|---|
| C1 | 3 | `~barpyx-…` | none | **0** (3,838 packets received) |
| C2 | 2 | `~havnyl-…` | none | **0** |
| C3 | 2 | `~barpyx-…`, `~havnyl-…` | none | **0** |

C3 holds C1 at `lyfe [~ 3]`, `dome %gw-btc`, with a direct `%if` forward
lane. It also pushed C2's fief lamp on its verdict — the **second
independent confirmation** of `+on-publ-full`'s fix, on a different ship.

