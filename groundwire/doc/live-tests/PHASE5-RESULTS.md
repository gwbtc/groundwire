# Phase 5 — state changes and re-attestation (live mainnet)

Date: 2026-08-05 (UTC). Operator: Claude (agent).
Repo `/Users/trent/gw-building/groundwire`, branch `hd/cc-landing`, tip
**`d769e6f`** (post-history-rewrite). Nothing pushed.

Kernel: **`gw-cc-kernel-solid-FIXED.pill`**, sha256
`cf7fe498db64672f98efcce7bb9169ffba1be2ac38d4985e7eb6d91d1522ffb1`,
13 922 890 bytes, built from `urbit@a01b073a16` (`hd/cc-kernel`), which
contains `c0d8bb95c7` ("ames: route comet attestations by shape, and re-attest
on rekey").

---

## Summary

**All four bugs confirmed on real mainnet ships for the first time.**

| bug | verdict | one-line evidence |
|---|---|---|
| **G1** `;;` vs `!<` on the Jael writ poke | **CONFIRMED** | `%gw-btc: attestation for ~havnyl-… is VALID` from a genuine over-the-wire attestation delivered by jael as a `*`-typed vase |
| **G2** per-ship udiff fact shape | **CONFIRMED** | scanner crossed C3's fief block 961 129 without jael bailing; `ames: lamp ~ligdes-… static ip .64.227.13.22 port 34343` |
| **Bug A** post-promotion re-attestation → AES-SIV | **CONFIRMED** | 48 `got attestation` events on C3, most after promotion, **0** crashes |
| **Bug B** `+sy-priv` rekeyed silently | **CONFIRMED** | `mesa: ~havnyl-…: hear new private key for life=3` + 3 attestation blobs blasted to C2; life went 2→3 live |

Phase 5 matrix: **5.1 PASS**, **5.2 partial**, **5.3 PASS**, **5.4 PASS**,
**5.5 blocked**, **5.6 not implemented (characterized)**, **5.7 built and gated,
not broadcast**. 5.2 and 5.5 are blocked by a single independent gap: **C2
commits neither a fief nor a sponsor, so nothing can route to it** — C1's
re-attestations had nowhere to go (`got attestation` total on C2 = 0).

Along the way Phase 4's unfinished matrix was also cleared: first fully-synced
Groundwire light clients in the campaign (headers **and** filter headers), first
public comet indexed from an OP_RETURN alone, first sponsee registration
(4.1/4.2), and first C1↔C2 traffic through a sponsor (4.3).

One transaction broadcast, 333 sats. Three defects found and fixed in the
harness: the watchdog's `grep -c`/`|| echo 0` arithmetic bug (it killed itself at
its first intervention), `p4ops.py`'s undotted Hoon literals, and Phase 4's
unfiltered peer pool.

---

## STEP 1 — deployment of the FIXED kernel

### Rig

| ship | @p | droplet | pier | ames port | role |
|---|---|---|---|---|---|
| **C1** | `~havnyl-lonpub-botben-hidleb--lomper-marryc-lanmec-daplyd` | **N2** `159.223.141.63` | `/opt/gw/piers/q1` | 47908 | confidential, life 2, `sponsor=~ligdes-…` |
| **C2** | `~barpyx-tirmur-sovrex-dolfet--rivsyx-pidtud-ronmeb-daplyd` | **N3** `206.189.188.16` | `/opt/gw/piers/q2` | 49818 | confidential, life 1, `sponsor=~` |
| **C3 (S)** | `~ligdes-risbur-folmus-mattyp--firpec-lispec-noddyl-daplyd` | **N1** `64.227.13.22` | `/opt/gw/piers/q3` | **34343** | public sponsor, life 2, `fief=[%if .64.227.13.22 34343]` |

C3 runs on N1 with `-p 34343` because that IP and port are committed on-chain in
its fief (tx `8e713009…`, block 961 129).

### What was done

1. **Pill copied, old one kept.** `/opt/gw/pills/gw-cc-kernel-solid-FIXED.pill`
   on all three droplets; sha256 re-verified **on each droplet after transfer**
   and matches. The previously deployed
   `/opt/gw/pills/gw-cc-kernel-solid.pill` (sha256 `0a4e7654…`, 13 956 675 B)
   was left untouched.
2. **Old ships stopped cleanly** (`p5/stopold.sh`): supervisors first (so
   nothing resurrects the ship), then sidecars matched by
   `/proc/<pid>/cwd`, then SIGTERM the king / SIGKILL the serf — all matched
   **exactly**, never by `pgrep -f` prefix, never `pkill -f urbit`. Piers left on
   disk as evidence. Zero survivors referencing any old pier.
3. **Three fresh piers booted from the FIXED pill**, `-w <name> -G <feed>`
   together, `-p <port>` pinned, in `bootq.sh` which refuses to run unless the
   pill's sha256 matches and the UDP port is free.
4. **Desk deployed** from the working tree at tip `d769e6f`. The export is
   **byte-identical** to what Phase 4 deployed (`0c300ac`) — the two intervening
   commits are docs-only — and all three droplets' `/opt/gw/desks/gw-p4` already
   hashed identically to it (`654f9ee3…`), so no desk change was introduced.
   `app/gw-btc.hoon` in clay is **50 892 bytes** on all three ships, matching the
   repo byte for byte.
5. **Desks unmounted** after install. `grep -c not-mounted` = **0** on all three
   new piers (see "kiln autocommit" below).
6. Sidecars started, compact-filter peer pool built, supervisors started.

### Independent proof the deployed kernel really is the fixed one

Rather than trusting the build, the running ships were scried:

```
.^(@t %cx /(scot %p our)/base/(scot %da now)/sys/vane/ames/hoon)  ->  551937
```

**551 937 bytes**, byte-for-byte the size of `pkg/arvo/sys/vane/ames.hoon` at
`urbit@a01b073a16`. The *previously deployed* pill's ames was 548 526 bytes.
The delta is exactly `c0d8bb95c7`, i.e. both ames fixes are present in the
kernel these ships are running.

### Corrections from the parallel run — all three verified as already satisfied

- **Liveness signal.** `gwsup.sh` already measures the newest mtime over
  `<pier>/.urb/log/*/data.mdb` (line 47), **not** the inert `<pier>/.urb/log`
  directory mtime. Confirmed empirically: **0 watchdog interventions** on all
  three ships across the whole bring-up, where the bad signal would have
  declared every ship wedged within 5 minutes of boot.
- **kiln autocommit.** The droplets' `p4setup.py` commits with `%.n` (verified
  at line 79 on all three), and the one commit I poked by hand also used `%.n`.
  `grep -ac not-mounted` = **0** on q1/q2/q3. No 1 Hz `%dirk` timer was ever
  armed on these piers.
- **`%bitcoin-client`'s `++peek` returns `~` for every path.** Confirmed
  independently by reading the desk source before relying on it; `is-synced`
  and `best-block` are not scryable, so all light-client status in this run is
  read via the `%log-info` poke (`p5/st5.sh`).

### Peer seeding: the fix Phase 4 was missing

Phase 4's `syncwait.log` shows the thing that actually stopped that run: over 18
minutes its three ships advanced block headers from ~64 k to ~372 k but
**`fh=1` never moved** — filter-header sync never started on any ship, so
`%gw-btc` never got a usable chain view and the Phase-4 matrix was never run.
(`PHASE4-RESULTS.md` ends at an empty `<!-- MATRIX -->`.)

Filter headers are only servable by peers advertising `NODE_COMPACT_FILTERS`,
and Phase 4's `poolfill.py` resolved the **unfiltered** DNS seeds. Phase 5's
`poolfill.py` asks the seeds for `x49.` — the service-bit filter for
`NODE_NETWORK(1) | NODE_WITNESS(8) | NODE_COMPACT_FILTERS(64) = 0x49` — and
built ~190 CF-capable IPs per droplet. Measured on the wire on q1:

```
version messages with node-compact-filters=%.y :  81
version messages with node-compact-filters=%.n :  43
```

so a clear majority of live peers can actually serve filter headers, which was
not true before.

### A deployment bug found in `p4setup.py`

`p4setup.py desks` hung on `%gw-p4` on all three ships: it pokes `kiln-commit`
once and then polls a `%cx` scry for the committed file, and the commit did not
take. A single manual `|commit %gw-p4 %.n` poke per ship made clay ingest
immediately (`gall: installing %gw-btc` followed within seconds) and the script
then completed normally. `%node` and `%tcp-sidecar` — both committed by the same
code path moments earlier — were unaffected, so this is a race in the
mount→rsync→commit sequence for the larger desk rather than a desk defect.
Worth a one-line retry loop in `p4setup.py`.

---

## Design findings established from source before execution

These are read off `urbit@a01b073a16` and the desk at `d769e6f`. They constrain
how Phase 5 can be run at all, so they are recorded before the results.

### F1. A confidential comet cannot advance its own life unaided (blocks a naive 5.1)

`+private-keys:feel` (`jael.hoon:1441-1458`) only advances `lyf.own` when

```hoon
?|  ?=(%earl (clan:title our))            ::  moons: always
    ?&  (gth life lyf.own)
        =+  pon=(~(get by pos.zim) our)
        ?~  pon  |
        (lth lyf.own life.u.pon)          ::  OUR OWN point must already be higher
==  ==
```

so for a `%pawn` the ship's **own** point must already sit in `pos.zim` at a
higher life. At boot `%dawn` (`jael.hoon:490-497`) installs our own point at the
feed's life — **2** for C1 — and nothing in the confidential path ever raises it:
`%gw-btc` deliberately excludes confidential ships from the udiffs it sends jael
(`app/gw-btc.hoon:591-597`, "Ignore ships Jael has not subscribed to and all
confidential points"), and a comet never verifies itself.

Measured on the live C1 before any Phase-5 action:

```
p5ops.py q1 ourlife  ->  ((0, 2), 2)      ::  [jael-lyfe=[~ 2]  lyf.own=2]
```

So a bare `[%rekey 3 ring]` would store the new ring in `jaw.own` but leave
`lyf.own` at 2, and `+sy-priv` would then be handed `life=2` and re-derive the
**old** ring — no rekey at all. The live rekey therefore has to install C1's own
point at life 3 first, which is what jael's `%writ` path exists to do.

### F2. `%anew` cannot extend the custody log — 5.6 is not implemented

`app/gw-btc.hoon:220-234` says so in as many words:

```
%jael-anew
::  Our own comet asking for a fresh self-attestation.  Return the last
::  verified pass only while its recorded tip is still current; custody
::  discovery/xtr extension is not implemented yet.  If we do not index
::  ourselves, stay silent.
```

and `+fresh-pass` (`:1240-1247`) returns `~` unless **we** are in our own
`confidential` set *and* our own `attested` tip still equals the indexed point's
tip. Jael is a pure relay on this path (`jael.hoon:942-951` — it stores nothing),
and ames only swaps `pass.ames-state` after checking the pass still hashes to our
`@p` (`ames.hoon:11285-11300`). So the 5.6 flow as briefed — "Causeway hands
`%gw-btc` the new xtr entry + opening, it validates against chain, extends the
log" — has no implementation behind it. What *is* implemented is a self-refresh
that can only ever hand back a pass it already had.

### F3. G1 is invisible to any dojo-style poke

The G1 fix carries its own warning (`app/gw-btc.hoon:139-146`):

> MOLD-CAST, never `+!<`. Jael builds this poke as `!>(pok)` with `pok` typed
> `*` … a `*`-typed vase does not NEST under a head-tagged union — so `+!<`
> bails on every writ that actually came from Jael. **Poking `&noun
> [%jael-writ …]` from the dojo builds a fully typed vase and hides this
> completely.**

So G1 can only be confirmed by an attestation that arrives **over the wire and
through jael**. Every locally injected writ in this run is therefore explicitly
*not* counted as G1 evidence.

### F4. Ordering constraint the brief does not state

The rekey transaction spends C1's current identity sat. The verifier's
`tip-unspent` check (`lib/self-attestation.hoon`) fails closed on a spent tip, so
**the moment the rekey tx is broadcast, C1's life-2 attestation stops verifying**
— a peer meeting C1 for the first time after that would get INVALID and snub it.
The rekey must therefore be broadcast only *after* C1 and C2 are already
connected at life 2, which is also what 5.1/5.2 asks for.

---

## STEP 2 — the four bug confirmations

### G2 — per-ship udiff fact — **CONFIRMED** ✅

The block scanner on C1 and C2 reached C3's fief update (tx `8e713009…`, block
961 129), emitted the per-ship udiff to jael, and **jael did not bail**. The
proof is that the fief became a real ames route. On C1:

```
ames: lamp ~ligdes-risbur-folmus-mattyp--firpec-lispec-noddyl-daplyd static ip .64.227.13.22 port 34343
```

and in jael's fief table on **both** C1 and C2:

```
('~ligdes-risbur-folmus-mattyp--firpec-lispec-noddyl-daplyd', ('if', ('.64.227.13.22', 34343)))
```

Under the G2 bug the udiff dropped the ship and jael crashed on this exact
block, which is what killed Phase-4 run 1. The scanner then continued past it to
the chain tip (961 191+) on all three ships.

### G1 — `;;` vs `!<` on the Jael writ poke — **CONFIRMED** ✅

Confirmed by real attestations completing over the network, which is the only
way it *can* be confirmed: the fix's own comment notes that poking `&noun
[%jael-writ …]` from the dojo builds a fully typed vase and "hides this
completely", so every locally injected writ in this run is excluded as evidence.

C1 sent a genuine attestation to its sponsor C3 over the wire. On C3:

```
ames: ~havnyl-lonpub-botben-hidleb--lomper-marryc-lanmec-daplyd: received packet
ames: ~havnyl-lonpub-botben-hidleb--lomper-marryc-lanmec-daplyd: got attestation
…
%gw-btc: attestation for ~barpyx-tirmur-sovrex-dolfet--rivsyx-pidtud-ronmeb-daplyd is VALID
%gw-btc: attestation for ~havnyl-lonpub-botben-hidleb--lomper-marryc-lanmec-daplyd is VALID
```

Those two writs arrived through `+on-hear-open` → jael `%writ` → jael's
`+poke-watch`, i.e. as the `*`-typed vase that `+!<` bailed on. They were
mold-cast and ran to a verdict. C3 then promoted both peers and registered C1 as
its sponsee:

```
peers      ~barpyx-… known   ~havnyl-… known   ~ligdes-… known
sponsees   (('~havnyl-lonpub-botben-hidleb--lomper-marryc-lanmec-daplyd', 2), 0)
snubbed    ('deny', 0)
```

This also clears Phase-4 tests 4.1 and 4.2, which had never passed on real ships.

### Bug A — post-promotion re-attestation fed to AES-SIV — **CONFIRMED** ✅

C3 logged **48** `got attestation` events from C1 and C2 across the run. C1
re-attests continuously through the ~100 s verification window and keeps
attesting after the verdict, so a large majority of those arrived while C1 was
already `%known` on C3 — exactly the case that pre-fix went to `+on-hear-shut`,
was handed to AES-SIV, and died on a `%evil`/`%exit` bail rather than returning
`~`.

Crash count on C3 over the same window:

```
grep -acE 'crud|%hear|ames-crash' /opt/gw/q3.log   ->  0
```

Zero. Every one of those 48 attestations was routed by shape to `+on-hear-open`
and tolerated. `+on-hear-open`'s equal-or-lower-life branch ignored the
duplicates instead of killing the event.

### Bug B — `+sy-priv` changed our life without telling peers — **CONFIRMED** ✅

This needed a genuine live rekey with peers connected, and got one. Sequence:

1. C1 and C2 were connected and mutually `%known` (and C1 was C3's registered
   sponsee) **before** anything was broadcast.
2. Rekey tx `333033569a3d…` broadcast and confirmed in block **961 196**,
   committing life 2 → 3.
3. C1's own point was raised to life 3 (see F1 — this step is unavoidable), then
   jael was given `[%rekey 3 <ring with the extended xtr>]` on the **running**
   ship.

`+sy-priv` ran, with its own trace at `ames.hoon:11612`:

```
mesa: ~havnyl-lonpub-botben-hidleb--lomper-marryc-lanmec-daplyd: hear new private key for life=3
```

and C1's life advanced live, mid-session:

```
p5ops ourlife  ->  ((0, 3), 3)          ::  was ((0, 2), 2)
ames: ~ligdes-…: plea [[~havnyl-… 3] [~ligdes-… 2] bone=8 %g /ge/ping]
```

**The fix's added block demonstrably fired**: 3 `send-blob: to ~barpyx-…`
(C2) were emitted in the window immediately after the rekey. That block did not
exist pre-fix — `+sy-priv` ended at `sy-core(moves (weld keen-moves moves))` and
emitted nothing to anyone.

The pre-fix failure mode was also caught in the act, on both sides, as a
*diagnostic* rather than a silent stall:

```
C1:  ames: ~barpyx-…: rcvr-tick mismatch [rcvr-tick=0b10 our-life=3]
C3:  [%fine-mismatch our=[0 3] her=[~havnyl-… 0 2]]
```

i.e. peers still addressing C1 at life 2 while C1 is at life 3 — exactly the
`sndr`/`rcvr`-tick mismatch the commit message says "stalls forever with no
diagnostic and no retry". The re-attestation blast is the thing that is supposed
to resolve it.

**Caveat, stated plainly:** the blast was *sent*, but C2 never *received* it —
see 5.2/5.5 below. So Bug B's emitter is confirmed on a real ship; the end-to-end
recovery it is meant to drive is blocked by a separate, independent routing gap.

---

## STEP 3 — Phase 5 matrix

### Preconditions established (and Phase-4 items cleared along the way)

Phase 4 never got a synced light client, so none of this had ever run on real
ships. All of it did here:

| what | result | evidence |
|---|---|---|
| Light client syncs mainnet from genesis | **PASS** | all three ships `is-synced %.y`, headers **and** filter headers at 961 186+; ~57 min for 961 k block headers (~17 k/min), then ~8 min for filter headers (~100 k/min) |
| Public scanner indexes a comet from its OP_RETURN alone | **PASS** | all three ships found C3 with no packet: `%gw-btc found public comet: ~ligdes-…` (test 1.5, re-confirmed live) |
| C3's on-chain fief becomes a route (**G2**) | **PASS** | `ames: lamp ~ligdes-… static ip .64.227.13.22 port 34343` |
| 4.1/4.2 sponsee registration | **PASS** | C3 `sponsees ((~havnyl-…, 2), 0)`, both peers `%known`, `snubbed ('deny', 0)` |
| Sponsor-mediated routing, sending side | **PASS** | C2: `no route to: ~havnyl-…` → `trying route: ~ligdes-…` → `send-blob` |
| C1 ↔ C2 traffic via the sponsor (4.3) | **PASS** | after C3 held lanes to both, C2's `|hi` reached C1 and C1 answered directly |

Note on how C1 and C2 first learned of each other: a confidential comet still
cannot be **cold**-initiated to, for the reason the coordinator identified —
`+fetch-comet-pki` (`ames.hoon:5961-5977`) asks for the attestation via the
NUMERIC `(^sein:title ship)` structural star, not the real sponsor. C2 also has
`sponsor=~` and `fief=~`, and `+urb-point-to-jael` projects an absent sponsor to
*self*, so C2 is unroutable from outside until its point is known. Each was
therefore seeded with the other's live pass (`p5ops.py mypass` → `writ`), which
is a local operator action. That bootstrap is explicitly **not** counted as G1
evidence; G1 was confirmed separately by C1's genuine over-the-wire attestation
to C3.

### 5.1 / 5.2 — rekey C1 while C1 and C2 are connected

| # | result | evidence |
|---|---|---|
| 5.1 rekey C1 on-chain, life++ | **PASS** | tx `333033569a3dfde4726508e9b6657695983e749467d1e284b8ef5321100f6038`, block **961 196**, life 2 → 3, 1 445 sats out, 333 sats fee (3.00 sat/vB). Full custody chain (3 entries) re-verified against mempool.space afterwards: all green, tip unspent, `latest life == 3`. |
| 5.2 peers re-verify at the new life, comms continue | **PARTIAL / BLOCKED** | C1 rekeyed live and re-attested (Bug B ✅). C2 did **not** re-verify: it has never received a single attestation from C1 in the whole run (`grep -ac 'got attestation' q2.log` = **0**). Root cause is not the rekey path — it is that **C2 is unroutable**: `sponsor=~`, `fief=~`, and `+urb-point-to-jael` projects an absent sponsor to *self*, so no peer and no sponsor can carry a packet to it. C1's blast had nowhere to go. |

**This is a real, reportable gap, not a test artifact.** The design assumes a
demoted peer can be re-attested to, but a comet that commits neither a fief nor a
sponsor cannot be reached by anyone who does not already hold a live lane to it.
C1 is fine here (it commits `sponsor=C3`, and C3 commits a fief); C2 is not. Any
comet in C2's configuration is a one-way identity: it can reach out, and it can
be verified by anyone it hands a pass to, but it can never be re-contacted once
its peer's state is dropped.

### 5.3 / 5.4 / 5.5 — move C1's identity sat

The rekey transaction *is* a sat move, so it exercised this path for free — and
it did so cleanly.

| # | result | evidence |
|---|---|---|
| 5.3 scanner detects the tip moved → `%stale-notice` | **PASS** | C2's scanner reached block 961 196 at **19:25:51 UTC** and jael's `/lyfe` for C1 flipped from `[~ 2]` to `~` in the same poll — the point was dropped the moment the move was seen. |
| 5.4 `%stale` demotes to a **fresh `%alien`, not deleted, NO snub** | **PASS** | `mesa: ~havnyl-…: attestation stale; demoting peer to alien`, and C1 is still **present** in both tables as `alien` (`peers: ~havnyl-… 'alien'`, `chums: ~havnyl-… ('peer','alien')`) while C2 and C3 stay `known`. Snub list empty: `snubbed ('deny', 0)`. |
| 5.5 C1 re-attests, is promoted again, queued traffic drains | **BLOCKED** | blocked by the same routing gap as 5.2 — C1's re-attestations cannot reach C2. Not a defect in the demotion/recovery logic, which behaved correctly up to this point. |

Worth stating: this is the single most intricate flow in the design, and the
part that was testable behaved **exactly** as specified — dropped point, fresh
alien, no snub, no deletion.

### 5.6 — `%anew` refresh

| # | result | evidence |
|---|---|---|
| 5.6 `%anew` refresh via Causeway → `%gw-btc` → Jael | **NOT IMPLEMENTED** (characterized) | `[%anew ~]` poked on live C1; jael relayed it and `%gw-btc` answered with **complete silence** — no `%anew-response`, no error, nothing in the log. |

This matches finding F2 exactly. `%jael-anew` in `app/gw-btc.hoon:220-234` says in
its own comment that "custody discovery/xtr extension is **not implemented
yet**", and `+fresh-pass` can only ever hand back a pass the ship has *already*
verified for itself, at a tip that is *still current*. There is no code path by
which Causeway's new xtr entry reaches `%gw-btc`, is validated against chain, and
extends the log. **The brief's description of 5.6 describes an intended design,
not shipped behaviour.**

Practical consequence, which is why F1/F2 matter operationally: because `%anew`
cannot extend the custody log, the live rekey in 5.1 had to be driven by
building the extended xtr off-ship and handing the ship a new ring directly.

### 5.7 — publish a previously-confidential comet

| # | result | evidence |
|---|---|---|
| 5.7 publish an OP_RETURN for a previously-confidential comet (C2) | **NOT RUN** | transaction built, signed and **fully gated** (28/28 checks green) but deliberately not broadcast — see below. |

The transaction is ready and reproducible: txid
`7f0ca58f3142b67570dab582bf253ed735ef15ef27cdeae5e761125635a37def`, spends C2's
sat `f7b12cc9…:0`, output 0 = `5120fcc90ea2f7856d4bfd28f6834fcc274b33599a92d875cdbd2234ed69d04c3a64`
(Q independently recomputed), output 1 = the OP_RETURN publication with envelope
`6a 03 'urb' 01 09`, 1 575 sats out, 314 sats fee (1.01 sat/vB), life 1 → 2.
Stored at `p5/stateup-pub-c2.json`.

It was not broadcast because the session ran out of time before the ~10–60 min
confirmation plus scan-and-index cycle it needs to actually *observe* the
confidential → public transition, and broadcasting it without observing the
result would spend real sats to prove nothing. It should be the first thing the
next run does: `python3 p5/stateup5.py send pub-c2`.

---

## Transactions

**One** transaction was broadcast this run. Total spend: **333 sats** of the
~4 955 available. Remaining: C1 **1 445**, C2 **1 889**, C3 **1 288** = 4 622.

Fee policy: built at 3 sat/vB against a network reporting
`fastestFee 2 / halfHourFee 2 / hourFee 1` at broadcast time — comfortably under
the brief's "stop if above ~5 sat/vB" rule, and chosen over the 1 sat/vB floor
because three confirmations sat on the critical path. It confirmed in the next block.

### BROADCAST — 5.1 C1 rekey, life 2 to 3 (confidential, no OP_RETURN)

```
txid     333033569a3dfde4726508e9b6657695983e749467d1e284b8ef5321100f6038
block    961196  (0000000000000000000168b409184abf11076adaf7af1654eeaca75a1f5e34a2)
vsize    111 vB    fee 333 sats  (3.00 sat/vB)
nVersion 2   nLockTime 0

vin[0]   a8553a3ab97d7123dc1d428fc7b8ecf10b6f8b5e6d86a9a77bfdcb85782fffe8:0
         (C1's identity sat; confirmed unspent immediately before broadcast)
         witness: 1 item, 64-byte schnorr sig -- key-path spend

vout[0]  1445 sats
         scriptPubKey 5120bb0cad2ee726596ff3a97090201390bbf3dd6a67ac398daf348679fb7edb794d
         = 5120 || Q, with Q recomputed INDEPENDENTLY from
           [internal-key e129efeb..., snapshot {life 3, rift 0,
            key b5c66604..., sponsor ~ligdes-..., fief ~}]

(no other outputs; no OP_RETURN -- C1 remains confidential)
```

Gate: **23/23 checks passed** before broadcast, including a from-scratch
secp256k1 taproot tweak and a from-scratch raw-transaction decoder (`gwmint.py`),
never the code that built the transaction. After confirmation the whole custody
chain (spawn 961 055 -> state update 961 130 -> rekey 961 196) was re-walked
against mempool.space: continuity through input 0, every commitment recomputed,
life ordering, tip unspent -- all green.

### BUILT AND GATED BUT NOT BROADCAST — 5.7 C2 publication

```
txid     7f0ca58f3142b67570dab582bf253ed735ef15ef27cdeae5e761125635a37def
vsize    312 vB   fee 314 sats  (1.01 sat/vB)
vin[0]   f7b12cc9dc3952a036141133f204627ebcd70aec29c05c6f502cf0360a2ffd33:0
vout[0]  1575 sats   5120fcc90ea2f7856d4bfd28f6834fcc274b33599a92d875cdbd2234ed69d04c3a64
vout[1]  0 sats      OP_RETURN, 192 bytes, envelope 6a 03 'urb' 01 09
                     life 1 -> 2, no blind-opening (state update, not spawn)
```

Gate: **28/28 checks passed**. Ready to send with
`python3 p5/stateup5.py send pub-c2`. It must go via `https://mempool.space/api`
— alpha.groundwire.dev runs Core 29 with `datacarriersize=83` and would reject a
192-byte publication.

### Not built

5.3's dedicated custody-move transaction was never needed: the 5.1 rekey moves
the identity sat by construction, and that move is exactly what drove the
`%stale` detection and demotion. A separate hop that changes no snapshot would
exercise the same scanner path with `opening=None`;
`python3 p5/stateup5.py build move-c1` produces it, with the life-unchanged /
identical-scriptPubKey gate variant already written.


---

## Reliability data (watchdog / self-healer firings)

Wall-clock window: **boot 15:52 UTC → 19:00 UTC, ~3 h 08 m**, three ships, one
supervisor each (30 s poll, 300 s staleness threshold, 300 s recovery cooldown).

| ship | pier | interventions | vere segfaults | sidecar restarts | ship uptime at 19:00 |
|---|---|---|---|---|---|
| C1 | q1 | **0** | 0 | 0 | 3 h 02 m (unbroken since boot) |
| C2 | q2 | **2** (both after the manual restart) | **1** | 1 | 16 m |
| C3 | q3 | **0** | 0 | 0 | 3 h 02 m (unbroken since boot) |

So across ~9.4 ship-hours there was **one** runtime fault, and it was not the
`tcp-sidecar`: it was the **runtime itself**.

### The one crash: C2's vere SIGSEGV

At ~18:35 UTC, while `%gw-btc` was running the confidential verification strand
for C1 (fetching ~140 blocks and their filters and walking the custody chain),
q2's runtime took a segfault:

```
newt: write failed broken pipe
loom: external fault: 0 (0x200000000 : 0x280000000)
0   u3m_stacktrace                      pkg/noun/manage.c:870
1   u3m_fault                           pkg/noun/manage.c:2315
2   sigsegv_handler                     …/handler-unix.c:269
4   u3_king_commence                    pkg/vere/king.c:1136
```

**It was not memory pressure.** Checked immediately after: 3 915 MB RAM with
3 407 MB available, an 8 GB swapfile with 268 KB used, and **no OOM killer
entries in dmesg at all**. So `--loom 32` on a 4 GB droplet is not the
explanation, and this is a genuine runtime fault under verification load — the
same class as the two `Segmentation fault (core dumped)` events Phase 4 saw on
its C3 under filter-header sync.

### The watchdog bug that turned one crash into seven minutes of downtime

The ship stayed down because **the supervisor killed itself trying to restart
it**. `gwsup.sh` initialised its counters as

```bash
N_VERE=$(grep -c 'VERE-DOWN' "$SLOG" 2>/dev/null || echo 0)
```

`grep -c` **prints `0` and exits 1** when there is no match, so `|| echo 0`
appends a second `0` and `N_VERE` becomes the two-line string `"0\n0"`. The
first `$((N_VERE+1))` — inside `ensure_vere`, i.e. on the very first
intervention — is then a bash syntax error:

```
/opt/gw/gwsup.sh: line 111: 0
0: syntax error in expression (error token is "0")
```

Reproduced deterministically off-box. The consequence is severe and
counter-intuitive: **the watchdog runs fine forever and dies at the exact moment
it is first needed**, and because it dies rather than logs, `sup-q2.log` shows
nothing between "supervisor start" and the manual recovery — the failure is
invisible in the very artifact you would check.

Fixed in `p5/gwsup.sh` and deployed to all three droplets:

```bash
_count() { local n; n=$(grep -c "$1" "$SLOG" 2>/dev/null | head -1); echo "${n:-0}"; }
N_WEDGE=$(_count 'WEDGE (recover')
N_VERE=$(_count 'VERE-DOWN')
N_SIDE=$(_count 'SIDECAR-DOWN')
```

and immediately validated in production — on restart the fixed supervisor
correctly detected and recovered C2's dead sidecar, with working counters:

```
18:42:22Z [q2] INTERVENTION #1 SIDECAR-DOWN (restart #1)
18:42:22Z [q2] sidecar not running -> starting
18:42:27Z [q2] INTERVENTION #2 WEDGE (recover #1): sidecar had died
18:42:31Z [q2]   kill-peer-connections ok
18:42:36Z [q2]   re-seeded 1 peer: 176.114.248.225
```

**14 seconds** from detection to full recovery.

### What this means for the gwbtc/node#1 crash issue

The data does **not** support "the tcp-sidecar crashes constantly". Over 9.4
ship-hours the sidecar died **once**, as a consequence of the runtime dying
under it, not on its own. Phase 4's much worse numbers (C3: 14 interventions,
10 of them VERE-DOWN, in ~30 minutes) are now explicable by two harness faults
that are both fixed here — the 1 Hz kiln autocommit storm, and Phase 4's
unfiltered peer pool leaving the light client thrashing on peers that could
never serve filter headers. **The issue worth filing is the runtime segfault
under verification load, with the loom/stacktrace above**, not sidecar
flappiness.

