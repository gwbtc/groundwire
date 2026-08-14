# Phase 5b — self-rescue, and the loop Phase 5 only half-proved (live mainnet)

Date: 2026-08-05 (UTC), 22:04–…Z. Operator: Claude (agent).
Repo `/Users/trent/gw-building/groundwire`, branch `hd/cc-landing`, tip
**`8ef95e9`** at session start. Nothing pushed.

Rig unchanged from Phase 5: three DigitalOcean droplets, mainnet BTC,
kernel `gw-cc-kernel-solid-FIXED.pill` (`urbit@a01b073a16`).

| ship | @p | droplet | pier | ames port |
|---|---|---|---|---|
| **C1** | `~havnyl-lonpub-botben-hidleb--lomper-marryc-lanmec-daplyd` | N2 `159.223.141.63` | `/opt/gw/piers/q1` | 47908 |
| **C2** | `~barpyx-tirmur-sovrex-dolfet--rivsyx-pidtud-ronmeb-daplyd` | N3 `206.189.188.16` | `/opt/gw/piers/q2` | 49818 |
| **C3** | `~ligdes-risbur-folmus-mattyp--firpec-lispec-noddyl-daplyd` | N1 `64.227.13.22` | `/opt/gw/piers/q3` | 34343 |

**Desk upgraded this run.** Phase 5 ran the desk at `d769e6f`, in which `%anew`
was *not implemented*. All three ships were moved to the working tree at
`8ef95e9`, i.e. **including `7d8ba8b` ("gw-btc: implement `%anew` in band")** —
`app/gw-btc.hoon` 50 892 → **64 745 bytes**, md5 `3dd45a46…`, identical on all
three droplets and identical to the repo. Each ship logged exactly one
`gall: reloading %gw-btc`, no crash, no `not-mounted`.

---

## The two questions the brief asked first

**Did a peer finally RECEIVE and verify a post-rekey attestation?**
**Yes — on C3.** `got attestation` → `%gw-btc: attestation for ~havnyl-… is
VALID` → C3's jael now holds C1 at **life 3**. C1 is confidential and
`%gw-btc` excludes confidential ships from its udiffs, so a life-3 point on C3
can only have come from a verified attestation. C2 also **received** 44
attestations from C1 (55 by end of session) — the first it has ever
received, against 0 for all of Phase 5 — but produced no verdict on them
(finding 9). And the pair does
*not* keep talking: C3 memory-bails on every C1 packet from the moment of that
promotion onward (finding 5).

**Did `%anew` work against a real chain?**
**Yes.** First run ever, on live C1: the full 3-entry custody log refetched and
re-walked through `+verify-lc` against mainnet, tip proven unspent by a BIP-158
filter scan, pass rebuilt and re-checked to still hash to C1's `@p`,
`%anew-response` emitted, `/x/custody` populated. Zero sats. Its failure path is
correct too — on C2, whose log went stale mid-run, `%anew` refused and stayed
silent exactly as specified.

**And the thing nobody asked, which matters most:** 5b.2 **fails** — a
confidential comet can never become public (finding 4) — and attempting it got
C2 **snubbed by its own sponsor** (finding 10).

## Headlines

### 1. `%anew` works against a real chain — first time ever

`7d8ba8b` had never been run against mainnet. It was, on live C1, and it
completed the full owner-driven refresh:

```
[%gw-btc-lc-scan-clean
   tip 0x3330.3356.9a3d.fde4.7265.08e9.b665.7695.983e.7494.67d1.e284.b8ef.5321.100f.6038
   vout=0 off=0 from=961.196 to=961.215]
%gw-btc: attestation for ~havnyl-lonpub-botben-hidleb--lomper-marryc-lanmec-daplyd is VALID
%gw-btc: custody log verified (3 entries); refreshing our pass
```

`/x/custody` — empty before the poke — now returns the **whole 3-entry
custody log**, each entry with the snapshot that transaction committed:

| entry | height | life | sponsor | blind-opening |
|---|---|---|---|---|
| 0 (spawn) | 961 055 | 1 | ~ | present, `start-height` 961 044 |
| 1 | 961 130 | 2 | `~ligdes-…` | ~ |
| 2 | 961 196 | 3 | `~ligdes-…` | ~ |

That is not a replay from memory: `+verify-lc` refetched every transaction by
`[height txid]`, rewalked continuity through input 0, recomputed every
`state-key` commitment, and ran a BIP-158 filter scan from 961 196 to the chain
tip 961 215 to prove the tip still unspent. Then `+with-xtr` rebuilt the pass,
`?.  =(our fig:ex:…)` confirmed it still hashes to C1's `@p`, and the
`%anew-response` went to jael and on to ames.

Cost: **zero sats**. `%anew` is idempotent on the current tip by design
("re-poking the current tip is a re-validation request, not a second hop"), so
the entire path could be exercised without moving the identity sat.

### 2. The `%vein` crash risk in the brief is a FALSE ALARM

The brief flagged `+own-pass`'s `life > 1` branch as a likely agent-killer:
`%vein` is not in jael's `lyc` care whitelist and scries cannot be
`+mole`-wrapped. **It does not crash, and it cannot.** Jael's gate is

```hoon
?.  ?|  =([~ ~] lyc)                       ::  <-- this disjunct
        ?=($?(%lyfe %life %rift %ryft %deed %sein %saxo %turf
              %fief %pont %pynt %sponsors %lamp %dome) syd)
    ==
  ~
```

and gall runs every agent scry through `(look rof [~ ~] /gall/[agent-name])`
(`gall.hoon:2190`, `:2201`). `lyc` is therefore *always* `[~ ~]` for an agent,
which short-circuits the whitelist before `syd` is ever examined. The whitelist
only constrains **remote** (`gang`-qualified) scries.

Confirmed empirically on live C1 (life 3, so the branch is reachable):

```
life=3   vein-answered=%.y   ring-bytes=405   pass-bytes=405
fig=235146299140078997037850464130958181069   ==  ~havnyl-…   ✅
```

and `%gw-btc` itself ran `+own-pass` through that branch during the `%anew`
above with **0** `bail`/`crud` events on C1.

### 3. 5b.3(b) — a peer HAS now received and verified a post-rekey attestation

Phase 5 could only show C1 *emitting* attestation blobs on rekey; `got
attestation` on C2 was 0 all run. **C3 received one and verified it.** From
C3's log, in order:

```
>   [%gw-btc-scanned from=961.195 to=961.195 settled-tip=961.195]
ames: ~havnyl-…: got attestation
%gw-btc: attestation for ~havnyl-… is VALID
>   [%gw-btc-scanned from=961.196 to=961.196 settled-tip=961.196]
```

and C3's jael now reports C1 at **life 3** (`/lyfe` → `[~ 3]`), up from 2.
C1 is confidential, and `%gw-btc` deliberately excludes confidential ships from
the udiffs it hands jael, so a life-3 point on C3 can *only* have come from a
verified attestation. The loop closes: rekey → attestation → peer's `%gw-btc`
fetches the new custody entry from chain → re-verify at the new life →
point installed at the new life.

**But see finding 5: immediately after that promotion, C3 began memory-bailing
on every packet from C1.** So the verification half of the loop is proven and
the "and they keep talking" half is not.

---

### 4. 5b.2 FAILS — a confidential comet can never become public

This is the most important result of the run, it cost a real mainnet
transaction to find, and it invalidates a documented design property.

The rescue transaction was built exactly as the plan asks: one C2 state update
that **both** adds a route (`fief=[%if .206.189.188.16 49818]` — C2's real
droplet and its pinned ames port — **and** `sponsor=~ligdes-…`) **and** carries
the OP_RETURN publication. 31/31 build-gate checks green, plus a 9/9 gate on
the payload **as decoded by a ship's own Hoon `+cue`** (see Transactions).
Broadcast at 1.01 sat/vB, confirmed in block **961 217**.

All three scanners reached 961 217. What they did with it:

```
mesa: ~barpyx-…: attestation stale; demoting peer to alien        (C1 and C3)
```

…and **nothing else**. No `%gw-btc found public comet: ~barpyx-…`, no udiff, no
`lamp`. C1's and C3's view of C2 went from

```
POINT  rift=0 life=1 sponsor=<self> fief=~        (before)
NO-POINT                                          (after)
```

and `/x/points` on both still lists exactly one public comet, `~ligdes-…`.

**Root cause, in the scanner's own source** (`lib/urb-core.hoon`). There are
exactly two ways into the public index `unv-ids`, and neither can admit a
previously-confidential comet:

```hoon
++  apply-spawn                                   ::  the ONLY writer of a new id
  |=  [who=ship =pass d=@ux op=opening:sa bo=blind-opening:sa]
  ?^  (~(get by unv-ids) who)  cor                ::  refuses if already known
  ?.  =(d (spawn-commit:cc spawn.bo blind.bo))  cor
  ?.  =([txid vout]:spawn.bo [txid pos]:i.inputs)  cor   ::  <-- must spend the
  ...                                                    ::      FUNDING satpoint

++  apply-state
  |=  [who=ship =pass op=opening:sa]
  ?~  pt=(~(get by unv-ids) who)  cor             ::  <-- refuses if NOT known
  ...
```

- A **state-update** publication (absent `blind-opening`) is dropped unless the
  comet is *already* publicly indexed. C2 was confidential, so it never was.
- A **spawn** publication (present `blind-opening`) is the only thing that
  creates an index entry, and it requires input 0 of that very transaction to
  spend the comet's original **funding** satpoint. For C2 that was
  `44eb0d9f…:1`, spent at block 961 055 and unrepeatable. Nor can the
  blind-opening be re-pointed: it must open the immutable hiding commitment
  `d = H_tag("gw/spawn-commit", jam(spawn-sont) || blind)` baked into the `@p`.

So the confidential → public transition has **no implementation**. Addendum §2's
promised escape hatch — "*publication* — one state update that adds a route AND
carries an OP_RETURN, after which every scanner learns the route from chain
alone" — describes an intended design, not shipped behaviour, exactly as
Phase 5 found for `%anew`. Test-plan 5.7 and 5b.2 are both unachievable as
written, and Phase 5's pre-built `pub-c2` transaction would have failed the
same way for the same reason (the test plan was right to say "do not send it
as-is", for a different reason than anyone knew).

**Both refusals are completely silent** — no `~&`, no slog — while *every other*
failure in those two arms carries a `~&  >>>` diagnostic ("spawn state
commitment mismatch", "state commitment mismatch", "spawn sat already
occupied"). That silence is precisely why this cost a 345-sat mainnet
transaction to discover instead of a log line. It is also a Phase-6.7 answer:
**no, this failure was not diagnosable from logs alone.**

**And it left C2 strictly worse off.** The transaction did move the sat, so
`%stale` fired correctly on both peers and they dropped C2's point altogether.
Before the rescue C1 held C2 at life 1 with an unusable route; after it, C1
holds nothing at all. A comet attempting this self-rescue in the field would
pay a fee, lose confidentiality *permanently* (the publication is on-chain
forever), and end up **less** reachable than it started.

### 5. C1 ↔ C3 are permanently livelocked by an unbounded `%ahoy` storm

New this run, and unrelated to Groundwire — this is kernel ames. C3 has logged
**182** `bail: meme` (loom exhaustion), and *every single one* is immediately
preceded by `ames: ~havnyl-…: received packet`. C1 and C2 have **0**.

The onset is exact. C3's last healthy exchange with C1 was the very
post-rekey attestation of headline 3; from the next block onward, every packet
from C1 bails:

```
>   [%gw-btc-scanned from=961.195 to=961.195 …]
ames: ~havnyl-…: got attestation
%gw-btc: attestation for ~havnyl-… is VALID           <- promotion to life 3
>   [%gw-btc-scanned from=961.196 to=961.196 …]
>   [%gw-btc-scanned from=961.197 to=961.197 …]
ames: ~havnyl-…: received packet
bail: meme                                            <- and forever after
```

The mechanism is visible in the trace. On hearing a shut-packet from a peer it
has not yet migrated, ames emits a *fresh* `%ahoy` migration plea
(`ames.hoon:6224`, `(poke-send-ahoy duct our her force-test=%.y)`) — with **no
dedupe and no rate limit**, once per heard packet:

```
ames: ~havnyl-…: is online; enqueue %ahoy $plea on bone=9
ames: ~havnyl-…: hear ~havnyl-… seq=136 1kb
```

C3 emitted **147** such pleas (bone=1 … bone=9) across **138** heard messages,
then began memory-bailing. C1, for its part, is stuck retransmitting forever:

```
ames: ~ligdes-…: dead [[137 0] rto=120.000 rtt=82 rttvar=9 ssthresh=1 cwnd=1
                       num-live=1 counter=136]
```

`rto` has saturated at 120 s and `counter` has not moved since seq 136. So: C1
retransmits every 120 s, C3 memory-bails on each one, C1 never gets an ack, and
neither side ever recovers. **Two live, healthy, mutually-attested ships that
can no longer exchange a single packet, with no diagnostic beyond a bare
`bail: meme`.** C3's `%gw-btc`, its light client and its scanner are entirely
unaffected and stayed current at the chain tip throughout, and the desk reload
did not clear it — this is ames state, not agent state.

### 6. Causeway prints an `%anew` poke that cannot verify, for any comet with a fief

`causeway.py:format_custody_entry_poke` — the function whose whole job is to
print the `:gw-btc &noun [%gw-custody-entry …]` dojo line — hardcodes the
snapshot's fief:

```python
# v9 snapshots carry no fief (a fief is for static-address ships).
f'{_hoon_unit(None)}]'
```

That comment is simply wrong: C3 has committed `fief=[%if .64.227.13.22 34343]`
on-chain since block 961 129, and C2 now commits one too. For C3's life-2 entry
the printed line and the correct line differ in exactly that field:

```
CAUSEWAY: … [2 0 0xdf30.9632.… ~ ~] ~]]]
CORRECT : … [2 0 0xdf30.9632.… ~ `[%if .64.227.13.22 34.343]] ~]]]
```

`state-key(internal-key, snapshot)` is taken over the *jammed snapshot*, so
dropping the fief changes the commitment and the `entry-N-commitment` check
cannot match the on-chain output. `%anew` then fails and, by design, stays
**silent** — the operator sees a poke that does nothing, forever, with no error.

---

## Test matrix

| # | test | result | evidence |
|---|---|---|---|
| **5b.1** | pairwise rescue: the unreachable comet initiates and hands over its attestation | **NOT ACHIEVABLE as written** (see below) | ames has no unsolicited self-attestation push; the only two senders of `+attestation-packet` are a reply to a `sendkeys` request and `+sy-priv`'s rekey blast, and the blast only reaches peers *already* `%known` |
| **5b.2** | publication rescue: scanners learn the route from chain alone | **FAIL** | headline 4. tx `0cca2561…` confirmed in 961 217; `apply-state` dropped it silently; `/x/points` unchanged; C2's point *deleted* on both peers |
| **5b.3(a)** | C1 rekeys live, on-chain, life++ | **PASS** (Phase 5, re-verified) | tx `333033569a3d…` block 961 196, life 2→3; the full 3-entry log re-walked against chain this run by `%anew` |
| **5b.3(b)** | **a peer RECEIVES the updated attestation and re-verifies at the new life** | **PASS on C3** | headline 3: `got attestation` → `is VALID` → C3's jael `/lyfe` for C1 = `[~ 3]`. C1 is confidential, so a life-3 point can only come from a verified attestation |
| **5b.3(b′)** | …and the two keep talking | **FAIL** | headline 5: C3 memory-bails on every C1 packet from the moment of that promotion onward |
| **5b.3(c)** | `%stale` → demote-to-alien on a real sat move | **PASS ×2** | C2's sat moved in 961 217 → `mesa: ~barpyx-…: attestation stale; demoting peer to alien` on **both** C1 and C3, no snub (`snubbed ('deny', 0)`) |
| **5b.3(d)** | **re-attest via `%anew`** | **PASS** | headline 1 — first `%anew` ever run against a real chain; 3-entry log re-verified through `+verify-lc`, pass rebuilt, `%anew-response` emitted |
| — | the `%vein` crash risk | **FALSE ALARM** | headline 2 — proven from gall's source *and* empirically |
| — | staleness must demote, never snub | **FAIL on the packet path** | finding 10 — C3 snubbed C2 for an honest state update; `snubbed ('deny', (~barpyx-…, 0))` |

### 5b.1, stated precisely

C2 could not initiate, and the reason is structural rather than incidental. To
send *anything* encrypted, C2 needs C1's point; it had none (its own `%stale`
demotion had dropped it). To obtain one it calls `+fetch-comet-pki`, which asks
via the **numeric** `(^sein:title ship)` structural star — a ship that does not
exist on mainnet. Its own attestation it cannot push either: `+attestation-packet`
has exactly three call sites, and all three are reactive (answer a `sendkeys`
request; resend on first-message timeout to a peer already `%known`; the
`+sy-priv` rekey blast, which iterates only `%known` peers). **There is no
"introduce myself" primitive**, so pairwise rescue has no mechanism behind it
either. Both of the addendum's two self-rescue paths are therefore unimplemented:
publication is dropped by the scanner, and pairwise has no packet to send.

### The one-way-identity property, measured

Phase 5 inferred C1 could not reach C2. This run measured it. C1 held C2 as
`%known` with a point, and `/peers/<c2>/forward-lane` answered

```
[[%& ~barpyx-tirmur-sovrex-dolfet--rivsyx-pidtud-ronmeb-daplyd] ~]
```

— the *self*-projection, encoded as a `[%& ship]` lane. Ames dutifully logs
`send-blob: to ~barpyx-…` and `trying route: ~barpyx-…` and hands it to the
runtime, which can only resolve `[%& ship]` lanes for **galaxies** and drops it.
A 35-second `tcpdump -n udp port 49818` on C2's droplet, taken while C1 was
poked to `|hi` C2, captured **0 packets**. C1's own pump then reports
`dead [[1 0] … num-live=1 counter=0]`. So the failure is silent at every layer:
ames believes it sent, the runtime drops without a trace, and only the pump
timeout ever hints at it.

---

## After the failure: what a manual rescue reached, and where it stopped

With publication dead, C2's life-2 attestation was handed to C1 by the operator
`%writ` path instead (explicitly **not** "from chain alone" — recorded as such).
That produced three further results, each a distinct gap.

### 7. A verified fief becomes a jael point but never a runtime route

C1 verified C2's life-2 attestation against the chain
(`%gw-btc: attestation for ~barpyx-… is VALID`) and jael installed the point
with the route intact:

```
POINT  rift=0  life=2
       sponsor  ~ligdes-risbur-folmus-mattyp--firpec-lispec-noddyl-daplyd
       fief     'if .206.189.188.16 port 49.818'
```

Ames, however, still said `no route to: ~barpyx-…`, and
`/peers/<c2>/forward-lane` answered `[%& ~ligdes-…]` — the *sponsor*, not the
fief. No `lamp` line for C2 appeared in C1's runtime log, while C3's
(`ames: lamp ~ligdes-… static ip .64.227.13.22 port 34343`) is right there.

The fief table that actually routes packets lives in **vere**, fed by
`[unix-duct %give %fief …]`. Ames only sends that on
`[%diff @ %fief *]` (`+on-publ-fief`) or on `%born` (`sy-born` pushes jael's
whole `fes.zim`). A Groundwire verdict arrives as `%sybl` → `sy-publ /sybl
[%full …]` → `+on-publ-full`, which touches neither. Restarting C1's runtime to
force a `%born` re-pushed `%turf` (the galaxy lamps reappeared) but still
produced no comet fief lamp.

So: **the only fief that has ever become a real route in this campaign is one
that arrived through the block scanner's `%fief` *udiff*.** A fief learned from
a verified attestation is stored, reported by `/pynt`, and inert. C3's fief
worked for exactly that reason and nobody noticed the difference until a second
comet committed one.

### 8. C2 received an attestation — 55 of them — for the first time in the campaign

Phase 5's headline gap was `got attestation` = **0** on C2 across a whole run.
After C1 gained a (sponsor-mediated) path, `tcpdump -n udp port 49818` on C2's
droplet captured real inbound traffic, and C2's log filled with

```
ames: ~havnyl-…: received packet
ames: ~havnyl-…: got attestation          x44 in the observed window, 55 total
```

So the delivery half of 5b.3(b) now works on C2 as well as C3.

### 9. …but C2 produced no verdict, and I could not determine why

None of the 44 attestations, and not C2's own self-`%writ` of its life-2
xtr-bearing pass, produced any `%gw-btc` output — no `is VALID`, no `is
INVALID`, no log line at all — while C2's scanner stayed healthy and current at
the chain tip (961 219, so the `/best-block` subscription is live and `best` is
set). C2's own life stayed at 1 and `/x/custody` stayed empty.

**Established:**
- C2 is not memory-bailing (`bail: meme` = **0**, against 206 on C3), so this is
  not finding 5.
- `best` is set (the scanner cannot advance without it), so the "light client
  not ready" drop is excluded.
- `%gw-btc` on C2 *is* alive and running verification jobs: the `%anew` poked at
  23:13 ran to completion and printed its verdict, using the same light client
  and the same `+verify-lc`. So the agent is not wedged as a whole — only the
  per-ship `inflight` path produced nothing.
- **Not** established, and worth chasing: every ship carries a large number of
  `spider crashed, killing all strands: %arvo-response` events — C1 **113**,
  C2 **131**, C3 **131**, all with that same reason. On C2 they are clustered
  early (last at log line 3 904 of 12 632), so they did not cause *this*
  silence. But a spider crash kills whatever verification strand is running,
  and `%gw-btc` only clears `inflight` on a verdict — so any strand killed this
  way leaves that ship's single-flight slot occupied **forever**. C2's runtime
  segfaulted during Phase 5 *specifically while running the confidential
  verification strand for C1*, which is exactly the shape that would strand
  `inflight[~havnyl-…]`. That is my leading explanation for the 44/55 ignored
  attestations, and it is untestable from outside precisely because of the
  missing scry below.
- The one silence I *can* explain is C2's `%anew`, and it is **correct
  behaviour**: C2's boot-feed xtr is a one-entry log whose tip is the sat that
  the 5b.2 transaction just spent, so `tip-unspent` fails closed and `%anew`
  stays silent exactly as specified. That is a fail-closed pass, not a bug.

**Leading hypothesis for the 44 attestations, not confirmed:** a life mismatch.
C1 builds `+attestation-packet` with `rcvr-life` = the life *it* believes C2
holds, which after finding 7 is the **on-chain** life 2, while C2's *kernel*
life is still 1 (its snapshot advanced on-chain; its ship was never rekeyed).
`+sift-open-packet` is handed `life.ames-state`, so the packet would not
authenticate and `+on-hear-open` would drop it at
`?: &(?=([%c *] cek) ?=(~ dom))` — in **ames**, before jael or `%gw-btc` is
ever involved, which matches the total absence of `%gw-btc` output. Breaking
that cycle needs a kernel `%rekey` on C2, which needs C2's own point at life 2
first (Phase 5's finding F1), which needs the self-`%writ` — which is also
silent, and *that* one is unexplained.

**Why I could not close it, which is itself the finding.** `%jael-writ` has
**nine** distinct silent-drop returns — `publicizing`, `inflight`, `declined`,
public-pass, foreign-kelvin, empty chain, over-bound log, `known-public`,
and `best` — and not one of them logs anything. **No scry exposes `inflight`
or `publicizing`**, and `+on-load` carries `inflight` forward verbatim across
an agent reload, so a slot left occupied by (say) the verification C2 was
running when its runtime segfaulted mid-Phase-5 would block that ship forever
with no symptom and no way to clear it short of nuking the agent. I could not
distinguish these nine cases from outside.

That is the concrete answer to Phase 6.7 — **no, this was not diagnosable from
logs alone** — and the cheapest high-value fix in this entire report is a
`/x/inflight` (and `/x/publicizing`) scry plus a `~&` on the non-obvious drops.
Three of this run's four failures (4, 7, 9) are silent returns that took source
reading, a runtime restart, a tcpdump and a mainnet transaction to localise.

### 10. An honest on-chain state update got the comet SNUBBED by its own sponsor

This is the most serious safety finding of the run, and it contradicts an
explicit design rule.

C2 performed a legitimate, correctly-formed, fully-gated state update. Its ship
kept sending the attestation it booted with — the **life-1** one, whose tip is
the sat that update just spent. C3 verified that packet:

```
[ %gw-btc-lc-scan-spent
    tip 0xf7b1.2cc9.dc39.52a0.3614.1133.f204.627e.bcd7.0aec.29c0.5c6f.502c.f036.0a2f.fd33
    vout=0 off=0  height=961.217 ]
%gw-btc: attestation for ~barpyx-… is INVALID
mesa: ~barpyx-…: attestation writ failed; snubbing
mesa: ~barpyx-…: snubbed          (and on every packet since)
```

and C3's snub list, empty all campaign, is now

```
snubbed   ('deny', ('~barpyx-tirmur-sovrex-dolfet--rivsyx-pidtud-ronmeb-daplyd', 0))
```

Addendum §3 states the rule this breaks, in as many words:

> When `%gw-btc` observes a confidential comet's tip outpoint spent … the
> identity's attestation is stale. **This is not fraud and MUST NOT produce
> `%fail`** (a snub would block the replacement packet).

The `%stale` path honours that — when the **scanner** sees a tracked comet's tip
spent it demotes to alien and does not snub, which is exactly what C1 and C3 did
for C2 at block 961 217 (test 5b.3(c), PASS). But the **packet** path does not:
when a peer's attestation arrives carrying a now-spent tip, `+run-checks`'s
`tip-unspent` fails, and that failure is a *negative verdict* → Jael `%fail` →
Ames snub. **The same physical fact produces demote-and-forgive down one path
and snub down the other**, and which one you get depends only on whether your
scanner or your inbox saw it first.

The consequence is the one §3 predicts: the snub blocks the replacement packet.
C2 can no longer reach C3 at all, so it can never deliver the refreshed
attestation that would fix it. Any comet performing a routine rekey is exposed
to this in the window between "sat moves" and "ship's pass refreshed" — a
window that is at minimum one block, and which for C2 is *permanent*, because:

- `%anew` is the mechanism meant to close that window, and it correctly
  **refuses** here: C2's boot-feed log is the stale one-entry log, so
  `tip-unspent` fails and `%anew` stays silent (`our own custody log did not
  verify; %anew stays silent` — fail-closed, exactly as specified, finding 9);
- and the manual route out (self-`%writ` the new pass → own point at life 2 →
  kernel `%rekey`) is blocked by findings 7 and 9.

So a correct implementation of one rule (`%anew` must never hand back a stale
pass) plus a violation of another (staleness must never snub) combine into an
unrecoverable state for an honest ship. **Fixing this is more urgent than
anything else in this report**: `tip-unspent` failing on a peer's packet should
produce `%stale`, not `%fail`.

---

## Phase 6 (partial)

| # | test | result |
|---|---|---|
| 6.2 | kill/restart mid-verification | **partially covered, accidentally**: C1's runtime was restarted deliberately (finding 7) and C2's had segfaulted during a Phase-5 verification. Neither produced a false verdict — the failure mode is *silence*, which is fail-closed and correct. But neither retry succeeded either, so "retry succeeds" is **unproven**. |
| 6.4 | prolonged offline, then rejoin | **FAIL for C1↔C3**: both ships have been continuously up for 6 h+ and cannot exchange a packet (finding 5). Recovery does not happen on its own. |
| 6.5 | reorg | not exercised; no reorg occurred (forward-only remains a known gap) |
| 6.6 | two peers disagree about a third's state | **observed**: C3 has snubbed C2 while C1 has not (C1 simply has no point for it). The two hold irreconcilable views of C2 and nothing reconciles them. |
| — | spider stability | **113–131 `spider crashed, killing all strands: %arvo-response` per ship.** Each one kills any in-flight verification, and `inflight` is only cleared by a verdict — so every such crash can strand a ship's single-flight slot permanently. |
| 6.7 | diagnosable from logs alone? | **NO** — findings 4, 7 and 9 are each a silent drop with no log line. Three of this run's four failures were invisible until source was read. |

Reliability over the session (≈1 h 15 m, three ships): **0** sidecar deaths,
**0** watchdog interventions, **0** vere segfaults, disk 92–99 GB free on all
three droplets. The only runtime fault was C3's ongoing `bail: meme` livelock,
which is an event-level bail, not a process death.

---

## Transactions

Fee policy: `mempool.space/api/v1/fees/recommended` reported
`fastestFee 1 / halfHourFee 1 / hourFee 1 / economyFee 1` — rechecked
immediately before broadcast, far under the brief's 5 sat/vB stop rule. One
transaction broadcast, **345 sats**. Remaining: C1 **1 445**, C2 **1 544**,
C3 **1 288**.

### BROADCAST — 5b.2 C2 self-rescue by publication (life 1 → 2)

```
txid     0cca2561703912be64e56d1516bc8204008343bde0b582d3ac790d178bd3cce8
block    961217  (00000000000000000000d8c6277e27aa8af1cb57dcfa86ea91aa98d4e7a2b28b)
vsize    343 vB   size 394   base 326   weight 1372
fee      345 sats  (1.01 sat/vB)
nVersion 2   nLockTime 0

vin[0]   f7b12cc9dc3952a036141133f204627ebcd70aec29c05c6f502cf0360a2ffd33:0
         (C2's identity sat; confirmed unspent immediately before broadcast)
         witness: 1 item, 64-byte schnorr sig -- key-path spend
         9f64deb248f44a357bc796a79029a521216bafa743fa337f21535d199d66ebaf
         aa00b6a21e45e49faa96f4321e81e036ffa0207fc7ddd7564c45e02b7edc0392

vout[0]  1544 sats
         scriptPubKey 5120e8e8c353d5832e99ba8a153f5489befcc1a2c92b23f90a58c5d64408e0752712
         = 5120 || Q, Q recomputed INDEPENDENTLY from
           [internal-key 02e129efeb...,
            snapshot {life 2, rift 0, key 6f7415dc...,
                      sponsor ~ligdes-risbur-folmus-mattyp--firpec-lispec-noddyl-daplyd,
                      fief [%if .206.189.188.16 49818]}]
         leaf hash caca9fd0c9950891b5722563c324678d87559de1b4be71843f010e40a3878df7

vout[1]  0 sats   OP_RETURN publication, 214-byte payload
         6a 03 'urb' 01 09 4c d6 || (jam [pass opening])
         6a0375726201094cd601a0d7b1476e250947d0bb5d2c0d5123d188515b79cb19
         c59fd71c2c77f98a8d3f7b9b8152d5ad93382ea1a1eaa59665599d6eca79a79a
         7921f52d694171ad56ee0aba37008980efecae458c6e8c64188aad3224386779
         9a9542f44b38b9bad93f14c6d71bc67559504a0bab2aa833000a0cdca1e773b6
         a65f528357daa04013b2cf29a3d5d31846f8708ab396aebfa7845bc80cc07fa9
         ead6491c97d050f552cbb2ac4e37e5bc53cdbc90fa96b4a0b8562b7705ddbb04
         f8370b9921f227f0a7f6f6776df0705fbf33f069e60001046faf3310348505

(no other outputs)
```

**Gate: 31/31 build checks + 9/9 payload checks.** The build side used
causeway's encoders; every check was recomputed from scratch (independent
secp256k1 taproot tweak, independent tapleaf, independent raw-tx decoder in
`gwmint.py`).

The fief encoding was validated *before* building, by re-deriving C3's
**known-good, live, on-chain** fief from its dotted string alone and confirming
it reproduces C3's committed values exactly:

```
recomputed state_commit c : 6f5beb6853dff709fdd333071c2dc002deff8d2f5083893bd811b81a846d2302
artifact  state_commit_c  : 6f5beb6853dff709fdd333071c2dc002deff8d2f5083893bd811b81a846d2302
recomputed Q  : 51203aea31567982f409f9a3e43b98610c2360c1e135427db948c4e0c56a184ee110
ON-CHAIN spk  : 51203aea31567982f409f9a3e43b98610c2360c1e135427db948c4e0c56a184ee110
```

The OP_RETURN payload was then decoded by **a ship's own Hoon `+cue`** (not by
any Python), on C3, and gated on the result:

```
life 2  rift 0  key 0x6f7415dc…  sponsor ~ligdes-…  fief [%if .206.189.188.16 49818]
blind-opening ABSENT (state update, not spawn)   pass 108 bytes   payload 214 B (<= 512)
```

Reachability of the committed fief was verified on the droplet before
broadcast — `gw-vere` bound to `0.0.0.0:49818`, `iptables INPUT` policy ACCEPT,
public IP exactly `206.189.188.16` — so the fief is truthful, not a guess.

After confirmation the artifact `/Users/trent/gw-building/.gw-comet-2.json` was
updated and the whole comet re-verified end-to-end against mempool.space:
**36/36 checks, no failures**, including entry-1 continuity through input 0,
both `state-key` commitments, life ordering, tip-unspent, and
`pass-key: boot ring's cry == latest snapshot key`. C2 is now
`confidential: false`, `published_op_return: true`, life 2, sat
`0cca2561…:0` worth 1 544 sats.

### BUILT AND GATED, NOT BROADCAST — 5b.3(a) second C1 rekey (life 3 → 4)

```
txid     9fda06bd2611807b9bce15c0c35f93929664401528e0103d571f7d320ca5f859
vsize    111 vB   fee 111 sats  (1.00 sat/vB)
vin[0]   333033569a3dfde4726508e9b6657695983e749467d1e284b8ef5321100f6038:0
vout[0]  1334 sats  5120 3c3c98b238d49f591b1cb787f563352e8d55b35051e4fd299bfa4d524f9bbef1
         life 3 -> 4, key/sponsor/fief carried forward, no OP_RETURN
```

**Gate: 25/25.** Stored at `p5b/stateup-rekey2-c1.json`, ready for
`python3 stateup5b.py send rekey2-c1`.

Deliberately **not** broadcast. Phase 5's finding F4 applies: the instant this
is broadcast, C1's life-3 tip is spent and its current attestation stops
verifying (`tip-unspent` fails closed), so any peer meeting C1 for the first
time afterwards gets INVALID and **snubs** it. It must only go out once C1 and
C2 are actually connected at life 3 — which findings 7–9 prevented. Sending it
now would have burned the one remaining clean rekey without a peer able to
observe the result.


---

## What to fix, in order

1. **`tip-unspent` on a peer's packet must produce `%stale`, not `%fail`**
   (finding 10). Today an honest rekey gets the rekeying ship snubbed by
   everyone who re-verifies it in the window, and the snub then blocks the
   packet that would fix it. This is a live, reproduced, unrecoverable state on
   mainnet right now.

2. **Implement the confidential → public transition, or delete the claim**
   (finding 4). `apply-spawn` is the only writer of `unv-ids` and it requires
   spending the original funding satpoint; `apply-state` refuses anything not
   already indexed. Either add a third entry point (a state-update publication
   carrying enough of the log to bootstrap an index entry) or strike the
   "publication" self-rescue path from addendum §2 and tests 5.7 / 5b.2. As it
   stands the documented escape hatch costs a fee, costs confidentiality
   permanently, and leaves the comet *less* reachable.

3. **Make the silent drops visible.** Add `~&` (or `%slog`) to
   `apply-spawn`/`apply-state`'s two `unv-ids` guards, and add `/x/inflight`
   and `/x/publicizing` scries to `%gw-btc`. Three of this run's four failures
   were silent returns; one of them cost a mainnet transaction to find.

4. **Route a fief learned from a `%writ` verdict** (finding 7). `+on-publ-full`
   should push the point's fief to the runtime the way `+on-publ-fief` does,
   otherwise an on-chain fief only works if it happens to arrive as a scanner
   udiff.

5. **Bound the `%ahoy` migration storm** (finding 5). `ames.hoon:6224` emits a
   fresh migration plea per heard packet with no dedupe; 147 of them wedged a
   healthy ship into permanent `bail: meme` on every packet from that peer.

6. **Fix `causeway.py:format_custody_entry_poke`** (finding 6): stop hardcoding
   the snapshot's fief to `~`. The printed `%anew` dojo line is unusable for
   any comet that commits a fief, and it fails silently.

7. **`%anew` cannot advance the kernel life.** It refreshes the pass and the
   log, but `lyf.own` still needs a jael `%rekey`, which still needs our own
   point installed at the higher life first (Phase 5's F1). Worth stating in
   addendum §5 so nobody expects `%anew` alone to complete a rekey.

## Reproduction assets

Everything is in the session scratchpad under `p5b/`:

- `stateup5b.py` — adds the `rescue-c2` op (fief + sponsor + publication) and
  `rekey2-c1`; `stateup-rescue-c2.json` and `stateup-rekey2-c1.json` hold the
  signed transactions and their full decodes.
- `update5b.py` — folds a confirmed op into the artifact and re-verifies the
  comet end-to-end against mempool.space.
- `mkanew.py` — prints both causeway's `%anew` dojo line and the correct one,
  demonstrating finding 6.
- `pt.sh` / `pynt.sh` / `gw.sh` / `mark.sh` / `dep.sh` — point inspection, log
  marking, desk redeploy (with the manual `|commit` that the mount→rsync→commit
  race requires for a desk this size — same race Phase 5 hit).
- `/opt/gw/{vein,cust,pt,cueship,spew,gwhi}.py` on all three droplets.

Artifact backup before the rescue: `p5b/gw-comet-2.json.pre-rescue`.
