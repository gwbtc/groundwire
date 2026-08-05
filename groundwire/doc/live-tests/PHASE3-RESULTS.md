# Phase 3 — kernel gating and real networking between real comets (live mainnet)

Date: 2026-08-05 (UTC). Operator: Claude (agent).
Desk pinned to **`a9f2d22`** on `hd/cc-landing` (deployed via `git archive a9f2d22 groundwire`).
The repo working tree moved to `c885a60` under me mid-run (the concurrent public
block-scanner port). **Nothing from those commits was deployed**, and every source
citation below is read out of `a9f2d22`. Nothing was committed or pushed.
No Bitcoin transaction was broadcast.

## Rig

| ship | @p | where | role |
|---|---|---|---|
| **C1** | `~havnyl-lonpub-botben-hidleb--lomper-marryc-lanmec-daplyd` | **N2** `159.223.141.63`, new pier `/opt/gw/piers/c1` | attested comet A |
| **C2** | `~barpyx-tirmur-sovrex-dolfet--rivsyx-pidtud-ronmeb-daplyd` | **N3** `206.189.188.16`, new pier `/opt/gw/piers/c2` | attested comet B |
| **BAD** | `~ligdes-risbur-folmus-mattyp--firpec-lispec-noddyl-daplyd` | **N1** `64.227.13.22`, piers `badnf` / `badiv` | adversarial suite-C |
| **VAN** | `~hapryg-pinnec-rolwyt-ramtyr--figsun-hatbet-fidtyd-samzod` | **N1**, pier `van` | vanilla suite-B comet |

Both C1 and C2 were booted `-w <name> -G <feed_with_xtr_baked>`, each with its own
`%bitcoin-client` light client synced to mainnet tip (`is-synced %.y`, headers and
filter-headers at 961.11x) and its own `%gw-btc` + `%writ-watch`, `%anex`-registered
for domain `%gw-btc`. **N1's `/opt/gw/piers/smoke` was never touched.**

BAD is C3's *real* @p with a **tampered `xtr`**: the ship name commits to `ugn+dat`
only (`append_xtr_to_ring` in `causeway.py`), so replacing the custody log leaves
the @p intact. Two variants, built by `<scratchpad>/mkbad.py` from C3's artifact
(the script first *reproduces C3's real feed bit-for-bit* — `rebuilt good feed ==
artifact feed_with_xtr_baked: True` — so only the intended field differs):

* `badnf` — entry-0 names txid `de…de` (32 bytes of 0xde), a transaction that does
  not exist. Nothing was broadcast; the txid is a deterministic fake.
* `badiv` — one bit flipped in the spawn `blind`, so the blind-opening no longer
  opens the on-chain `dat` commitment.

---

# LEAD

## BUG G1 (CRITICAL, new, release-blocking): every live `%writ` crashes `%gw-btc`

**Confidential comets are 100 % non-functional over real networking with the code
as it stands.** Jael hands the verifier its writ through:

```hoon
++  poke-watch                                   :: sys/vane/jael.hoon:271-274
  |=  [hen=duct app=term pok=*]                  :: <-- pok is typed  *
  %-  emit
  [hen %pass /[app]/poke %g %deal [our our /jael] app %poke %noun !>(pok)]
```

`!>(pok)` therefore builds a vase whose **type is `*`**. `%gw-btc` unpacks it with

```hoon
    =/  poke  !<(jael-poke:urb vase)             :: app/gw-btc.hoon:114 @ a9f2d22
```

`!<` nest-checks, `*` does not nest under a head-tagged union, so the poke **bails
every single time**. Observed on C2 the instant the first real attestation crossed
the wire:

```
ames: ~havnyl-lonpub-botben-hidleb--lomper-marryc-lanmec-daplyd: got attestation
jael-bad-coup
nest-fail
-have.*
-need.?([%jael-anew dom=@tas] [%jael-writ dom=@tas who=@p pass=@])
/app/gw-btc/hoon:<[114 15].[114 37]>
/app/gw-btc/hoon:<[114 5].[198 7]>
/app/gw-btc/hoon:<[106 3].[237 5]>
/sys/vane/gall/hoon:<[1.842 9].[1.842 37]>
```

and again for every 30 s retry, forever.

**Why Phase 2 could not have caught it.** Phase 2 drove the verifier with
`:gw-btc &noun [%jael-writ …]` typed at the dojo, where dojo builds a fully-typed
vase that `!<` happily accepts. The defect lives exactly on the kernel→agent
boundary that only real networking exercises. It is also *silent* in the sense
that matters: jael's `%poke-ack` handler only slogs `jael-bad-coup`
(`jael.hoon:892-896`) — it does **not** synthesise a `%sybl %fail`. So a verifier
that crashes on every writ produces **silence, never a false negative**. The
fail-closed direction is right; the feature is simply dead.

**Fix**: the agent must not trust the vase's type —

```hoon
    =/  poke  ;;(jael-poke:urb q.vase)
```

(Typing it in jael instead would work but `+poke-watch` is deliberately generic
over domains, so the agent side is the right place.) I applied exactly this to the
**deployed copy only**, marked `TEST-HARNESS PATCH` in the source; the git working
tree was not modified. **Everything below is after that patch.**

## 3.5 — THE KEY SAFETY INVARIANT HOLDS

An invalid attestation does **not** silently degrade to vanilla-comet networking.
Full evidence in the 3.5 row below. Nothing in this run produced a channel to a
ship whose attestation failed, in either direction.

## Second finding (infrastructure, blocking): comet↔comet packets never reach each other on the live network

Neither sponsor star relays. Every ship can reach its own sponsor —
`|hi ~daplyd` succeeds from C1 **and** C2, `|hi ~samzod` succeeds from VAN — but a
`%keys` request addressed to another comet is dropped somewhere in between.
`tcpdump` shows C1 emitting its 41-byte attestation request to `35.247.119.159:13337`
(`~zod`) and C2 emitting its to `134.209.161.119:13490` (`~tyr`), one packet per
30 s retry, and **nothing ever arrives at the far comet** (`%msg`/`%rcv` verbosity
on both ends: only "requesting attestion", never "received packet"/"got attestation").
The same is true VAN↔C1 across two different stars.

This is **not** a Groundwire defect — it is transport, and it is precisely what the
`fief` (on-chain IP) in the snapshot exists to solve. All three minted comets carry
`fief=~`, so there is no in-band lane and the ships fall back on sponsor forwarding,
which the live network does not perform for them.

**Harness workaround — transport only.** A DNAT rule per droplet rewrote *only* the
UDP destination address:

```
iptables -t nat -A OUTPUT -p udp --sport <my-ames-port> ! -d <peer-ip> \
         -j DNAT --to-destination <peer-ip>:<peer-ames-port>
```

No packet contents, no ames logic, no kernel and no desk code are touched; the ships
still authenticate each other entirely from packet contents, across two DigitalOcean
regions over the public internet. **Its side effect is documented in "What went
wrong in the harness" at the bottom — it creates a routing cycle that ames turns
into a packet storm, and that storm is what blocked the second half of 3.1.**

---

# Results

| id | verdict | summary |
|---|---|---|
| 3.1 | **PARTIAL** | C2 verified C1 **VALID** against real mainnet and installed the point; C1→C2 blocked by the harness storm (see below) |
| 3.2 | **PASS** | unverifiable suite-C comet → no verdict at all → held, no communication |
| 3.3 | **PASS** | invalid attestation → `[XX] spawn-commit` → `%sybl %fail` → snub |
| 3.4 | **PASS** | `%writ`→`%sybl` fired; `/=dome=/` proves the Groundwire path; vanilla contrast captured |
| 3.5 | **PASS** | no silent degradation to vanilla networking, in either direction |
| 3.6 | **PASS (characterised)** | a ship with **no** Groundwire verifier can never talk to a Groundwire comet — `%sybl %lost`, fail-closed |

## 3.1 — two attested comets exchange `|hi` both ways — **PARTIAL**

**What I did.** Booted C1 and C2 as above on two different droplets, each with its
own synced mainnet light client, then let ames' own alien machinery drive first
contact across the DNAT bridge (plus an explicit `|hi` from each side).

**Expected.** Each verifies the other on-chain, then `|hi` succeeds both ways.

**Actual, C2 verifying C1 — complete success.** C2 received C1's attestation over
the wire, jael passed the `%writ` to `%gw-btc`, and the light-client-backed
verification ran against real mainnet data:

```
ames: ~havnyl-…-daplyd: got attestation
%gw-btc: attestation for ~havnyl-lonpub-botben-hidleb--lomper-marryc-lanmec-daplyd is VALID
  [ok] chain-nonempty      [ok] chain-bounded        [ok] fetch-count
  [ok] spawn-opening       [ok] spawn-commit         [ok] start-txid
  [ok] start-vout-range    [ok] start-off-range      [ok] entry-0-txid
  [ok] entry-0-height-order[ok] entry-0-input-zero   [ok] entry-0-continuity
  [ok] entry-0-key-path    [ok] entry-0-off-range    [ok] entry-0-sat-landed
  [ok] entry-0-commitment  [ok] entry-0-blind-opening-zero
  [ok] entry-0-life-order  [ok] state-resolve        [ok] tip-unspent
  [ok] tip-p2tr            [ok] pass-key             [ok] sponsor-known
  [ok] tracked-prefix
WRITWATCH %writ-response dom=gw-btc who=~havnyl-…-daplyd  VERDICT=FULL(valid)
```

and the point was installed:

```
~barpyx:dojo> .^((unit @ud)  %j /=lyfe=/~havnyl-…-daplyd)   ->  [~ 1]
~barpyx:dojo> .^((unit @tas) %j /=dome=/~havnyl-…-daplyd)   ->  [~ %gw-btc]
```

That is a real confidential comet, minted on mainnet, verified end-to-end against
real Bitcoin by a *different* real comet on a *different* host, purely from a
packet that arrived over the internet. All 24 checks, including the BIP-158
`tip-unspent` filter scan, passed.

**Actual, C1 verifying C2 — never completed.** C1 requests C2's attestation every
30 s (`ames: ~barpyx-…: requesting attestion`), C2 receives the request and logs
`ames: ~havnyl-…: requested attestation` — and then **emits no attestation**.
`tcpdump` on C2 over many minutes with the storm capped shows a stream of 47-byte
packets to C1 and **not one packet in the 150–2000 byte range** (an iptables
`-m length --length 150:2000` counter sat at 0 the whole time), i.e. the
attestation blob is never put on the wire. `.^((unit @ud) %j /=lyfe=/~barpyx-…)`
on C1 is `~`, so `|hi` cannot complete in that direction.

**Verdict: PARTIAL, and I cannot cleanly attribute it.** Two candidate causes, in
order of my confidence:

1. **(most likely) harness.** The DNAT bridge makes each comet look like the route
   to every third party, which is a routing *cycle*; ames' forwarding turned it
   into a 58 000 packet/s storm between the two hosts (measured:
   3 480 586 47-byte packets to C1 in 60 s). That storm **killed both piers once**
   mid-run — C2's vere was found dead having been at 1.7 GB RSS on a 3.9 GB box,
   and C1's died with it. Rate-capping the storm at the firewall keeps the ships
   alive but does not restore the missing attestation.
2. **(hypothesis, NOT confirmed) a real asymmetry in `+on-hear-keys`.** The
   responder branches on whether it already knows the requester:

   ```hoon
   =/  via=@p                                   :: sys/vane/ames.hoon ~4988
     ?.  =(%pawn (clan:title sndr.shot))  sndr.shot
     =/  lyf  (rof … /lyfe/(scot %p sndr.shot))
     ?:  ?=([~ ~ [* * ^]] lyf)  sndr.shot        :: <-- known: send direct
     (^sein:title sndr.shot)                     :: <-- unknown: send via sponsor
   =/  =blob  (attestation-packet sndr.shot 1)
   %-  send-blob-via  [for=| via sndr.shot blob (~(get by peers.ames-state) via)]
   ```

   The one direction that **worked** is exactly the "responder does *not* know the
   requester" branch (C1 did not know C2, so `via = ~daplyd`, a ship that is in the
   classic `peers` map). The direction that **never works** is the "responder
   already knows the requester" branch, where `via = sndr.shot` and the ship-state
   is looked up in `peers.ames-state`. A comet promoted through the Groundwire path
   is installed by `+on-publ-full` → `+sy-put-ship` under whichever table
   `+find-peer` reports; if it lands in the `%mesa` `chums` map, then
   `(~(get by peers.ames-state) via)` is `~` and `+send-blob-via` enqueues the blob
   into an alien agenda and asks jael for the comet's keys — which, for a comet,
   never arrive, so the attestation is never sent. That would mean **an attested
   Groundwire comet can never answer an attestation request from a peer it has
   already verified**, which is a bidirectional-bootstrap deadlock. I could not
   confirm it: the `%ax /peers` scries I tried returned nothing usable on these
   ships and I ran out of runway. **Flagging it for a targeted re-test, not
   claiming it.**

## 3.2 — unattested suite-C comet contacts an attested one — **PASS**

**Method (honest).** `badnf`: C3's real @p with a custody log naming a transaction
that does not exist (`de…de`). This is the "claims the protocol, has no on-chain
anchor" case — the verifier finds nothing to check against.

**Expected.** Held pending a verdict; no communication.

**Actual.** Both directions exercised.

*BAD initiates* (`|hi ~barpyx` from `badnf`): C2 answers with its own attestation,
`badnf` has no `%gw-btc` (it is a bare comet) so jael answers its writ with `%lost`:

```
~ligdes:  ames: ~barpyx-…: got attestation
~ligdes:  mesa: ~barpyx-…: writ for unknown pki domain %gw-btc
```

and C2 is never registered on `badnf`. No channel.

*C2 initiates* (`|hi ~ligdes-…` from C2) — this is the case the plan is really
after, because C2 *does* run the verifier:

```
ames: ~ligdes-…: got attestation
%gw-btc: verification thread for ~ligdes-risbur-folmus-mattyp--firpec-lispec-noddyl-daplyd
         ended without a verdict
attestation-tx-not-found
[961.059 0xdede.dede.dede.dede.dede.dede.dede.dede.dede.dede.dede.dede.dede.dede.dede.dede]
```

**No `%writ-response` fact at all** — `%writ-watch` stayed silent, so this is
SILENCE, not a negative verdict, and it is cleanly distinguishable from 3.3.
The peer is held forever:

```
~barpyx:dojo> .^((unit @ud) %j /=lyfe=/~ligdes-…-daplyd)  ->  ~
```

Retries continue every 30 s and are re-verified every time; nothing is cached as a
failure. **PASS.**

## 3.3 — comet with an invalid attestation — **PASS**

**Method.** `badiv`: one bit flipped in the spawn `blind`, so the blind-opening no
longer opens the on-chain `dat` commitment. Everything else is C3's genuine,
on-chain-anchored packet.

**Expected.** Rejected, suspended, no channel.

**Actual** (C2, verifying against real mainnet):

```
%gw-btc: attestation for ~ligdes-risbur-folmus-mattyp--firpec-lispec-noddyl-daplyd is INVALID
  [XX] spawn-commit
mesa: ~ligdes-…-daplyd: attestation writ failed; snubbing
WRITWATCH %writ-response dom=gw-btc who=~ligdes-…-daplyd  VERDICT=FAIL(negative)
```

Exactly one check failed, and it is the named one. Jael turned it into
`[%give %sybl %fail …]`, ames took the `%fail` branch of `+sy-sybl` — additive snub
plus deletion of the alien state — and no point was stored:

```
.^((unit @ud)  %j /=lyfe=/~ligdes-…)  ->  ~
.^((unit @tas) %j /=dome=/~ligdes-…)  ->  ~
```

**PASS.**

## 3.4 — prove the Groundwire path was actually used — **PASS**

Three independent pieces of evidence, all from live traffic:

1. **`%writ` reached the agent**: the G1 crash trace itself is proof that jael's
   `%writ` → `%g %deal … %poke %noun` arrived at `%gw-btc` (it is the *only* thing
   that pokes that mark from `[our our /jael]`). After the patch the same path
   produced the `+report` block quoted in 3.1.
2. **`%sybl` fired and Jael stored the point**: `WRITWATCH %writ-response …
   VERDICT=FULL(valid)` is `%gw-btc` giving the fact on `/writs`, the path Jael
   registered for at `%anex`; jael then emitted `[%give %sybl %full …]`, and
   `.^((unit @ud) %j /=lyfe=/~havnyl-…)` on C2 became `[~ 1]`.
3. **The domain scry discriminates the path.** These comets are real ships (not
   fakeships), so `%dome` is meaningful here — it was untestable in Phase 2:

   | ship, as seen from | `/=lyfe=/` | `/=dome=/` | how it got there |
   |---|---|---|---|
   | C1 seen by C2 | `[~ 1]` | `[~ %gw-btc]` | **Groundwire**: `%writ`→`%sybl %full` |
   | VAN seen by C1 | `~` | `~` | never registered (see 3.6) |
   | BAD seen by C2 | `~` | `~` | snubbed / held |

   `%dome` reads the suite-C tweak of the stored key (`jael.hoon:1807-1828`); it is
   `~` for anything that is not a suite-C Groundwire pass. A vanilla comet
   registered by `+al-register-comet` could never produce `[~ %gw-btc]`.

The negative half of the contrast is stronger than planned: I could not get a
vanilla comet registered on C1 *at all*, because the two never exchanged
attestations (see the transport finding, and 3.6). **PASS.**

## 3.5 — invalid attestation must NOT degrade to vanilla networking — **PASS**

**What I did.** After the 3.3 snub, drove traffic both ways: `|hi ~barpyx-…` from
`badiv`, and `|hi ~ligdes-…` from C2, and watched both panes for ~3 minutes.

**Expected.** No channel, and specifically no fallback to the suite-B path.

**Actual.**

* C2 logs `ames: ~ligdes-…: requesting attestion` on every retry and **never**
  `got attestation` — the snub drops the ship's packets at ames' door.
* `badiv`'s `|hi ~barpyx-…` is accepted by its dojo (`>=`) and then produces
  nothing at all.
* `.^((unit @ud) %j /=lyfe=/~ligdes-…)` stays `~` and `.^((unit @tas) %j
  /=dome=/~ligdes-…)` stays `~`. The point was never installed by *any* path.

**Why the code cannot degrade.** Both attestation entry points gate on the suite
*before* the vanilla registration:

```hoon
?:  ?&  ?=([%c *] cek)                          :: on-hear-open, ames.hoon ~5040
        ?|(?=(~ lyf) (lth u.lyf sndr-life.open-packet))
    ==
  ?>  ?=(^ dom)
  %-  emil  :~ … %writ u.dom sndr.shot pass.open-packet ==   :: returns here
?^  lyf   (emit … %public-keys …)
?>  =(1 sndr-life.open-packet)                  :: vanilla path unreachable for %c
…  (sy-meet-alien-ship …)
```

A suite-C pass with no jael life *always* takes the `%writ` return; the vanilla
`+sy-meet-alien-ship` / `+al-register-comet` branch is only reachable once the
suite-C branch has been excluded. `+al-take-proof` (the `%mesa` twin,
`ames.hoon:12658-12683`) has the identical shape. There is no third door: a
malformed suite-C tweak (`dom = ~`) is dropped even earlier, before any alien state
is created.

**The invariant holds. No degradation observed and none reachable in the source.**

## 3.6 — attested comet ↔ vanilla suite-B comet — **PASS (characterised)**

**What I did.** Self-mined a vanilla comet with `gw-vere -c /opt/gw/piers/van`
(no `-G`) — `~hapryg-pinnec-rolwyt-ramtyr--figsun-hatbet-fidtyd-samzod`, sponsor
`~samzod`, no light client, no `%gw-btc`. Ran `|hi` in both directions against C1,
with `%msg`/`%rcv`/`%odd` tracing on both ships.

**Actual.** Over the real network the two never exchanged a packet at all (the
transport finding above): each side loops on `requesting attestion` and neither
ever logs `got attestation`. Across the bridged path, however, the same
configuration was exercised directly — `badnf`, which like VAN has no `%gw-btc`,
did receive C2's suite-C attestation and produced:

```
mesa: ~barpyx-…-daplyd: writ for unknown pki domain %gw-btc
```

i.e. jael answered `[%give %sybl %lost %gw-btc ~barpyx]` (no live domain
registered), `+sy-sybl`'s `%lost` branch is a documented no-op, and the ship is
**never registered**. So:

> **A ship that does not run a Groundwire verifier can never open a channel to a
> Groundwire comet, in either direction.** The vanilla comet's own suite-B
> attestation would be accepted by the Groundwire comet (that path is intact), but
> the reverse leg cannot complete, so no session is ever established.

That is fail-closed and consistent with the design, but it is a **deployment
consequence worth stating explicitly**: confidential comets are not reachable from
the general Urbit network until the counterparty runs a `%gw-btc`-equivalent for
domain `%gw-btc`. It also means the "initiator must verify the target first"
ordering makes the bootstrap strictly two-sided. **PASS as a characterisation.**

---

# Classification of every failure seen

| what | class |
|---|---|
| G1 `!<` vs `!>(pok=*)` writ crash | **(a) real bug** — release-blocking |
| `+on-hear-keys` never answers a peer it already knows | **(a) suspected real bug, UNCONFIRMED** — see 3.1 |
| comet↔comet packets not relayed by mainnet stars | **(c) infrastructure** (and the reason `fief` exists) |
| DNAT bridge → ames forwarding cycle → 58 kpps storm → both piers died once | **(b) test-harness**, self-inflicted |
| `tcp-sidecar` `--- CRASH: signal 11 ---`, light client silently stops at 0 live peers | **(c) infrastructure**, recurred 3× (now under a restart supervisor) |
| `%gw-btc` `ready` gated behind `%urb-start-indexing` + a Bitcoin Core RPC | **(a) real, already known** (Phase-2 B8) — *fixed after `a9f2d22`*: `c885a60` splits `ready` into `indexing`/`best`, so a light-client-only node verifies without the public-index poke |
| my C1 tmux session killed by the concurrently-running agent on N2 | **(b) harness/ops collision** |

## Other observations

* **A crashing verifier fails closed.** Jael's `%poke-ack` handler only slogs
  `jael-bad-coup`; it does not synthesise `%sybl %fail`. So G1 produced silence for
  hours, never a snub. That is the right direction, and it is also why G1 is easy
  to miss.
* **`%dome` is testable on real ships.** Phase 2 could not exercise it (fakeship
  short-circuit); here it is the cleanest single discriminator between the
  Groundwire path and vanilla comet registration.
* **`%anex` re-registration on agent reload is clean** — `%gw-btc` was `|commit`ed
  and bumped live twice with no loss of the Jael registration.
* **A genuine verification took ~100 s** on this rig, consistent with Phase 2, and
  the removal of the verification deadline (`b05a7a2`) means the slow path no longer
  turns a valid peer into silence.
* **Two identical @p's booted from different feeds** (C3's real feed vs. the two
  tampered ones) all produce `~ligdes_daplyd`, confirming empirically that the
  ship name commits to `ugn+dat` only and *not* to the custody log.

## What went wrong in the harness (so the next run avoids it)

1. `tmux` session names collide across agents on shared droplets, and
   `tmux send-keys -t gw` **prefix-matches**. Two of my dojo commands landed in
   another agent's `~zod`, and my C1 session was killed out from under me. Use a
   unique session name and always target `<session>:0.0`.
2. A DNAT "bridge" between two ships makes each the apparent route to every third
   party. Ames forwards, and the result is an exponential packet storm that OOMs
   both piers. If this trick is needed again, pair it with a hard egress rate cap
   **from the very first packet**, or (better) give the comets a real `fief`.
3. Driving a dojo with `tmux send-keys` while a background poller does the same
   interleaves characters into one unparseable line, and drum persists that input
   buffer **across pier restarts**. One writer per pane.
4. The Mac's disk filled twice mid-run (a 28 GB scratch pier), which blocks every
   local tool including `Bash` — nothing can be run, not even `df`.

## Final state (left running)

* **C1** N2 `159.223.141.63`, tmux `gwc1x`, pier `/opt/gw/piers/c1`, `-p 47908`,
  light client `is-synced %.y` @ 961.117, `%gw-btc` + `%writ-watch` live,
  sidecar under `/opt/gw/sc1sup.sh` (auto-restart).
* **C2** N3 `206.189.188.16`, tmux `gwc2x`, pier `/opt/gw/piers/c2`, `-p 49818`,
  light client `is-synced %.y` @ 961.118, holds C1's verified point
  (`lyfe [~ 1]`, `dome [~ %gw-btc]`), `~ligdes-…` **snubbed** from 3.3.
* **N1** `64.227.13.22`: `smoke` untouched; `van` (vanilla comet) running;
  `badnf`/`badiv` stopped. `iptables` flushed.
* C1/C2 keep a 20 pkt/s egress cap toward each other so the forwarding storm cannot
  restart unattended (`iptables -L OUTPUT`); all DNAT rules removed.
* Deployed desk = `a9f2d22` + the single G1 patch, at `/opt/gw/desks/groundwire`
  on both hosts (also `<scratchpad>/p3desk/`). Adversarial feed builder:
  `<scratchpad>/mkbad.py`; feeds in `<scratchpad>/bad-feeds.json` (0600).
  Dojo helpers: `<scratchpad>/gwd1.sh` (C1), `gwd2.sh` (C2).
