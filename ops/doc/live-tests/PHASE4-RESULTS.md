# Phase 4 — sponsorship (live mainnet)

> **Read this first — there were TWO concurrent run-2s, and that was an
> orchestration error, not a plan.** The coordinator wrongly believed run 1 had
> died (it had not — see the liveness-signal correction below) and dispatched a
> second agent onto the same three droplets and the same three comet
> identities. Both wrote to this same results path, so earlier revisions of
> this file mix the two. The second agent stood down rather than contest the
> ports. Consequences to keep straight:
>
> - **Run 1 DID complete the matrix.** Its results: 4.1 PASS, 4.2 PASS,
>   **4.3 FAIL**, 4.4 PASS, 4.5 PASS, 4.6 PASS, 4.7 PASS, 4.8 PASS
>   (characterised). Caveat: 4.4/4.5/4.6 were driven by injecting the
>   `[%jael-writ …]` poke rather than by real network re-attestation, because
>   the sponsor already held C1's point at life 2 and `+on-hear-open` only
>   re-issues a writ on a life *increase*. The no-snub half was read from live
>   `/ax/snubbed`. Run 1 also broadcast the two on-chain state updates
>   (`8e713009…` @961129, `a8553a3a…` @961130) and diagnosed the
>   `~hopned-…` anomaly as our own unit-test fixture.
> - **Run 2 did NOT complete the matrix** — it stood down first. Its value is
>   the infrastructure findings below, which are the more durable output:
>   the autocommit diagnosis, the corrected liveness signal, and the
>   watchdog counts.
> - **Neither run tested the fixed kernel.** Both ran a pill predating
>   `c0d8bb95c7`, so kernel bugs A (post-promotion re-attestation dropped)
>   and B (rekey does not re-attest) were PRESENT throughout. Do not quote
>   either run as evidence that the fixed kernel works.
>
> Two corrections that supersede earlier claims, including ones the
> coordinator made confidently:
>
> - **The `<pier>/.urb/log` mtime liveness signal is inert.** That path is a
>   directory; LMDB writes into an already-created `data.mdb` and never
>   touches the dirent — measured 21 h stale on a ship demonstrably
>   processing events. Use the newest mtime across `<pier>/.urb/log/*/data.mdb`
>   (glob: piers roll epochs). Every earlier "the ship is wedged" diagnosis
>   built on the directory mtime is unproven.
> - **The ship-killer was kiln autocommit, not the light client.** `|commit
>   <desk> %.y` arms a 1 Hz repeating timer that unmounting does not cancel
>   (`lib/hood/kiln.hoon:659-670`), measured at 0.4–0.8 events/s/desk forever
>   on a 2-vCPU box. `|cancel-autocommit` stopped it dead. This is a harness
>   bug, and it materially weakens the earlier claim that the light client had
>   wedged us five times: run 2 logged **zero unexplained light-client wedges
>   in ~80 ship-minutes**.

Date: 2026-08-05 (UTC). Operator: Claude (agent). What follows is run 2.

Repo `/Users/trent/gw-building/groundwire`, branch `hd/cc-landing`, desk deployed
from tip **`0c300ac`** (`git archive HEAD groundwire`, `doc/` stripped so a
mounted desk cannot fail to build a `%mime` tube; `tests/` kept). Nothing pushed.
**No Bitcoin transactions were broadcast in this run** — all on-chain state was
already committed and confirmed by run 1.

## Rig

| ship | @p | droplet | pier | ames port | role |
|---|---|---|---|---|---|
| **C1** | `~havnyl-lonpub-botben-hidleb--lomper-marryc-lanmec-daplyd` | **N2** `159.223.141.63` | `/opt/gw/piers/sp1` | 47908 | confidential sponsee, life 2, `sponsor=~ligdes-…` |
| **C2** | `~barpyx-tirmur-sovrex-dolfet--rivsyx-pidtud-ronmeb-daplyd` | **N3** `206.189.188.16` | `/opt/gw/piers/sp2` | 49818 | confidential third party, life 1, `sponsor=~` |
| **C3 (S)** | `~ligdes-risbur-folmus-mattyp--firpec-lispec-noddyl-daplyd` | **N1** `64.227.13.22` | `/opt/gw/piers/sp3` | **34343** | public sponsor, life 2, `fief=[%if .64.227.13.22 34343]` |

C3's on-chain fief commits it to `64.227.13.22:34343`; it is booted on that IP
with `-p 34343` so the fief is true. All three piers are **fresh** (booted
`-w <name> -G <feed>` together, from the kelvin-408 solid pill). No DNAT rule
exists on any droplet; `iptables -t nat -S` is bare policy on all three, and the
Phase-3 rate-limits were already flushed.

Relevant on-chain state, all confirmed before this run:

| | txid | block | change |
|---|---|---|---|
| C3 | `8e713009…` | 961 129 | `fief=[%if .64.227.13.22 34343]`, life 1→2, **with OP_RETURN** |
| C1 | `a8553a3a…` | 961 130 | `sponsor=~ligdes-…`, life 1→2, confidential (rides `xtr`) |

---

# PROVENANCE — what kernel and desk this run actually tested

**Desk:** `hd/cc-landing` tip `0c300ac`, which includes the run-1 fix
"gw-btc: the per-ship udiff fact dropped the ship, crashing jael". Without that
fix no peer can index a public comet's state update, so C3's fief at 961 129
would have been unindexable — run 1 hit exactly that.

**Kernel: the pill was stale, and was repaired in place before any test ran.**
`/opt/gw/pills/gw-cc-kernel-solid.pill` was built 08-04 17:17 UTC, nine hours
before `urbit@c0d8bb95c7` ("ames: route comet attestations by shape, and
re-attest on rekey"). That commit is load-bearing for exactly the paths Phase 4
exercises:

- a comet keeps re-attesting while the receiver's verifier deliberates (~100 s),
  and a suite-%c comet re-attests at every new life; pre-fix, `+on-hear-packet`
  routed such a packet to `+on-hear-shut`, where AES-SIV authentication failure
  is a `%evil` bail rather than a `~`;
- `+sy-priv` never re-attested on our own life change.

I confirmed the gap empirically rather than by build date — `sys/vane/ames.hoon`
in the booted ships' `%base` was **548 526** bytes against **551 937** in
`/Users/trent/gw-building/urbit` (HEAD `a01b073a16`), while `jael.hoon` (63 920)
and `zuse.hoon` (229 044) matched exactly. So the single delta was the ames fix.

**Repair:** `<scratchpad>/p4b/basefix.py` mounted `%base` on each ship, replaced
`sys/vane/ames.hoon` with the repo file, `|commit`-ed, and unmounted. All three
ships then reported `551937` from clay and logged

```
clay: kernel updated
clay: rebuilding %base after kernel update
vane: %ames: 0v5.8og63
```

i.e. the **running** ames vane was reloaded, not merely the source in clay.
**This run therefore tests the current kernel** (`urbit@a01b073a16` ames) on a
kelvin-408 pill, and results should NOT be read as characterising the stale
pill. No result below needs the "kernel predates c0d8bb95c7" caveat.

---

# THE RELIABILITY PROBLEM (gwbtc/node#1) — diagnosis and supervisor

## What actually killed run 1 — two distinct faults, one of them ours

### Fault 1 (ours, and it is the ship-killer): `|commit <desk> %.y`

Run 1's `[%not-mounted %gw-p4]` storm was **not** a clay `%into` loop and not a
`%mime` tube failure. `[%not-mounted <desk>]` is printed by clay's `%dirk`
handler (`sys/vane/clay.hoon:4825-4827`) when a `%dirk` arrives for a mount
point clay no longer holds. The `%dirk`s came from kiln:

```hoon
++  poke-commit                                   :: lib/hood/kiln.hoon:659-670
  |=  [mon=kiln-commit auto=?]
  =.  +>.$  (emit %pass /commit %arvo %c [%dirk mon])
  ?.  auto
    +>.$
  =/  recur  ~s1
  =.  commit-timer  [/kiln/autocommit (add now recur) recur mon]
  (emit %pass way.commit-timer %arvo %b [%wait nex.commit-timer])
```

`p4setup.py` poked `kiln-commit` with **`%.y`** — autocommit — which arms a
**1 Hz** repeating `%dirk` timer that never stops. Unmounting the desk afterwards
does not cancel it; it just makes every tick print `[%not-mounted %gw-p4]`. Run
1's piers were burning a real event every second, forever, per desk, on a
2-vCPU droplet, on top of filter-header sync.

Measured on this run's ships before the fix: `[%not-mounted %base]` +
`[%not-mounted %gw-p4]` accumulating at ~0.4–0.8 lines/s, 1 890 / 2 705 / 2 292
lines respectively.

Fix: five `:hood &kiln-cancel-autocommit ~` pokes per ship (there can be more
than one live timer on `/kiln/autocommit`, and `%rest` cancels one at a time).
After: **0 new lines in 25 s on all three ships.** `p4setup.py` and
`redeploy.py` were patched on all three droplets to commit with `%.n`.

This is a **harness bug, not a kernel bug**, and it explains the "starves the
ship while looking healthy" symptom better than the light client does.

### Fault 2 (real, and it is gwbtc/node#1): sidecar death is not observable

`tcp-sidecar` dies; `%bitcoin-client` keeps its peer table intact and believes
every peer is live; every send returns `no such connection`; the ship never
retries and never self-heals; restarting the sidecar alone does not fix it
because the agent's peer entries still point at connections that no longer
exist. The only recovery is `&kill-peer-connections` followed by re-seeding.

Run 1 also saw the **runtime** itself SIGSEGV twice under filter-header sync
load (`wd-p4c3.out`: `Segmentation fault (core dumped)` at
`gw-vere -t --loom 32 -p 34343 /opt/gw/piers/p4c3`), so a supervisor that only
watches the sidecar is not enough.

## The liveness signal — and a correction to the brief

The brief specified `<pier>/.urb/log` mtime. **That signal is inert**: `.urb/log`
is a directory and LMDB writes into an already-created `data.mdb` inside it, so
the dirent is never touched. Verified against a ship that was demonstrably
processing events at that moment:

```
/opt/gw/piers/smoke/.urb/log               mtime 2026-08-04 17:18:56  (21 h stale)
/opt/gw/piers/smoke/.urb/log/0i0/data.mdb  mtime 2026-08-05 14:55:32  (now)
```

Using the directory would have reported every healthy ship as WEDGED from the
moment it booted. The supervisor uses **the newest mtime over
`<pier>/.urb/log/*/data.mdb`** (one directory per epoch — piers roll epochs, so
it must be a glob, not `0i0`). That is genuine event-commit progress and it does
advance during legitimate header/filter sync.

## The supervisor

`<scratchpad>/p4b/gwsup.sh`, one detached `setsid nohup` loop per ship,
30 s poll, logging every intervention with a UTC timestamp and a running counter
to `/opt/gw/sup-<pier>.log`. Triggers, in order:

1. **VERE-DOWN** — no king (`$NF == <pier>`) and no serf (`--snap-dir <pier>`)
   process: delete `.vere.lock`, relaunch `gw-vere -t --loom 32 -p <port>`.
   Process matching is by exact final argument / exact `--snap-dir` value, never
   a `pgrep -f` prefix — `p4c1` prefix-matches `p4c1b`, and killing the wrong
   pier is worse than killing none.
2. **SIDECAR-DOWN** — no `tcp-sidecar` whose `/proc/<pid>/cwd` is this pier:
   restart it with the prescribed
   `SSL_CERT_FILE=… setsid nohup /opt/gw/bin/tcp-sidecar .`, then **immediately**
   run the wedge recovery, because a dead sidecar *is* the gwbtc/node#1 trigger
   and waiting out the 5-minute staleness window just wastes five minutes.
3. **WEDGE** — event log stale > 300 s: ensure sidecar, poke
   `:bitcoin-client &kill-peer-connections ~`, re-seed **one** fresh peer, and
   let the peer count climb by gossip. 300 s cooldown between recoveries.

Peer pool: `<scratchpad>/p4b/poolfill.py` resolves ten Bitcoin Core DNS seeds
six times over (they return a small random slice per query) into
`/opt/gw/peerpool.txt`, and each ship keeps a `used-<pier>.txt` so an IP is
never handed to the same ship twice. Blacklist expiry inside `%bitcoin-client`
is `~d3`, so seeds are consumed for three days at a time — run 1 needed ~185
distinct IPs to recover 9 peers. First fill produced **208** unique IPs.

## Sanity check — deliberate sidecar kill

```
2026-08-05T15:32:01Z  kill -9 <sidecar pid 97956>   (cwd = /opt/gw/piers/sp2)
2026-08-05T15:32:06Z  [sp2] INTERVENTION #3 SIDECAR-DOWN (restart #2)
2026-08-05T15:32:06Z  [sp2] sidecar not running -> starting
2026-08-05T15:32:11Z  [sp2] INTERVENTION #4 WEDGE (recover #2): sidecar had died
2026-08-05T15:32:14Z  [sp2]   kill-peer-connections ok
2026-08-05T15:32:20Z  [sp2]   re-seeded 1 peer: 172.104.174.58
```

Detected in **5 s**, fully recovered in **19 s**, new sidecar pid 100286.
**PASS.**

## How often the watchdog fired — the hard data

Window: supervisor start (15:15–15:30 UTC, staggered) to 15:52 UTC, when this
run's ships were stopped by a **second concurrent operator** (see "RUN STATUS"
below). ~37, ~22 and ~22 minutes of supervised uptime respectively, all of it
under header-sync load.

| ship | total interventions | SIDECAR-DOWN | VERE-DOWN | WEDGE (recover) |
|---|---|---|---|---|
| **C1** `sp1` | **2** | 1 | 0 | 1 |
| **C2** `sp2` | **4** | 2 | 0 | 2 |
| **C3** `sp3` | **14** | 2 | **10** | 2 |

Reading these honestly:

- **The first SIDECAR-DOWN + WEDGE pair on every ship is bookkeeping, not a
  fault** — the supervisor starts before the sidecar exists, so it starts one
  and runs the recovery once. Subtract one of each per ship.
- **C2's second pair is the deliberate kill test** (above). So in ~37 + ~22
  minutes of real operation, **C1 and C2 each experienced zero spontaneous
  faults**.
- **C3's 10 VERE-DOWN restarts are one incident, not ten.** A stale `p4c3` ship
  from the previous run was resurrected at 15:32 by `/opt/gw/heal.py`, which
  survived my first cleanup pass, and it took udp/34343. Every relaunch then
  died on `mesa: bind: address already in use` and the supervisor retried once a
  minute for ten minutes until I found and killed the squatter. **Root cause was
  a stale supervisor of mine, not the light client.** This is why `gwsup.sh`
  gained `free_port`, and why refusing to boot onto an already-bound port
  (as `bootq.sh` now does) is the better fix.

So: **zero unexplained light-client wedges in ~80 ship-minutes.** That is a
much weaker indictment of gwbtc/node#1 than run 1 suggested, and it is
consistent with the `%.y` autocommit storm — not the sidecar — having been what
actually killed run 1. The sidecar failure mode is real (it is reproducible on
demand, and the recovery is necessary and works), but on this evidence it is
rarer than once per 40 ship-minutes, and run 1's "five times" was probably
counting the autocommit starvation as light-client wedges.

Caveat: 80 ship-minutes is a small sample and none of it reached the
filter-header phase, which is where run 1 saw the runtime SIGSEGV. The
supervisor handles that case (VERE-DOWN) but this run never exercised it
spontaneously.

---

# RUN STATUS — this run did not execute the matrix

**At 15:52 UTC a second agent, working the same brief on the same three
droplets, ran a `stopold.sh` that stopped `sp1`/`sp2`/`sp3` and both of their
supervisors, and booted its own `q1`/`q2`/`q3` on the same three @p and the
same three ames ports.** It is still running as I write this: three live SSH
sessions, `gwsup.sh q1|q2|q3` supervising, ships header-syncing.

It is building on this run's work — `/opt/gw/gwsup.sh` is byte-identical to
mine (6 326 bytes, `free_port` and all), and it is using `poolfill.py`,
`addpeers.py`, `p4ops.py`, `st2.py` and the `%.n` patch to
`p4setup.py`/`redeploy.py` from this run. It has also done something better
than I did: it built a **fixed pill**,
`/opt/gw/pills/gw-cc-kernel-solid-FIXED.pill`
(sha256 `cf7fe498db64672f98efcce7bb9169ffba1be2ac38d4985e7eb6d91d1522ffb1`,
from `urbit@a01b073a16`), instead of repairing `%base` in place.

**I stood down rather than contest the rig.** There is exactly one set of comet
identities with the required on-chain state, and two agents driving the same
three @p on the same three ports would produce garbage — double-booted piers,
divergent event logs, and unattributable ames behaviour. The matrix belongs to
whichever run holds the ports, and that is `q1`/`q2`/`q3`.

State reached before the stop (all three were mid header-sync, none had reached
`is-synced %.y`, so no matrix test was runnable yet):

```
15:51:19 | c1 h=372.001 peers=12 | c2 h=590.001 peers=10 | c3 h=202.001 peers=13
```

A handoff note with everything below was left at
`/opt/gw/HANDOFF-from-sp-run.md` on all three droplets, and the evidence is on
disk: `/opt/gw/piers/sp{1,2,3}`, `/opt/gw/sp{1,2,3}.log`,
`/opt/gw/sup-sp{1,2,3}.log`.

---

# MATRIX

**Not executed by this run.** What follows is source-derived analysis of what
4.1/4.2/4.3 should do and how to read them — it is **prediction, not evidence**,
and is recorded here because it changes how 4.3 must be run to produce a
meaningful verdict.

## 4.1 / 4.2 — C1 attests to C3, C3 accepts

Preconditions that are easy to get wrong:

- **C3 must have indexed its own public spawn.** `+verify-cards` passes
  `known-public = ~(key by unv-ids.urb-state)` (`app/gw-btc.hoon:879`) and
  `+run-checks` fails `sponsor-known` when the attesting snapshot's named
  sponsor is not in that set (`lib/self-attestation.hoon:241-247`). C1's
  snapshot names C3, so **C3 must find C3 in its own index** — that means
  `:gw-btc &gw-index-from` at a height at or below C3's OP_RETURN publication
  (961 059) and the scanner walked past 961 130. A ship with `indexing=|` fails
  `sponsor-known` on every sponsored comet, and the failure looks like a bad
  attestation rather than a missing index.
- C1 must have indexed too, or it has no fief for C3 and cannot send anything.

The decision point is `app/gw-btc.hoon:478-493`:

```hoon
=/  claims-us=?
  ?&  has.sponsor.net.u.verified
      =(our.bowl who.sponsor.net.u.verified)
  ==
?:  ?&(claims-us ?=(%decline (sponsor-policy who life.net.u.verified)))
  %-  (slog leaf+"%gw-btc: declining sponsorship of {<who>}" ~)
  `this(declined (~(put in declined) who))
…
=?  sponsees  claims-us  (~(put by sponsees) who [life.net.u.verified now.bowl])
=?  declined  claims-us  (~(del in declined) who)
```

with `+sponsor-policy` (`:956-960`) accepting everything not already in
`declined`. Evidence to capture on C3: the `%gw-btc: attestation for
~havnyl-… is VALID` report with its `[ok]` check list, `/x/sponsees` containing
`~havnyl-…` at life 2, and jael's `%lyfe` + `%dome` for `~havnyl-…`
(`[~ %gw-btc]` from `%dome` is the single cleanest proof the point arrived via
`%writ`→`%sybl %full` and not via vanilla comet registration).

## 4.3 — and why it must be run in two halves

`+fetch-comet-pki` (`sys/vane/ames.hoon:5961-5977`) requests a comet's
attestation through

```hoon
=/  sponsor=@p  (^sein:title ship)
```

That is the **numeric** `+sein` (`sys/zuse.hoon:5673-5683`): for a `%pawn` it is
`(end 4 who)` — the star formed by the comet's low 16 bits — **not** its
Groundwire sponsor. Contrast two other call sites that do the same conceptual
thing correctly:

- `+on-hear-forward` (`:5006`): `(^^sein:title rof /ames our now sndr.shot)`
- `+zar` inside `+get-forward-lanes` (`:4257-4266`):
  `(^^sein:title rof /ames our now her)`

Both use the jael-scry form, and jael's `+sein` (`sys/vane/jael.hoon:285-297`)
returns the point's `sponsor` when the point is known, falling back to numeric
when it is not.

The consequence is structural: **cold first contact between two confidential
comets cannot use the sponsor.** C2 asking for C1's keys sends the `%keys` blob
to C1's numeric star, which Phase 3 already established does not relay to
comets. Sponsor-mediated routing engages only once C2 *already holds* C1's point
— then `+zar` walks C1 → C3, finds C3's fief lane, and C3's `+on-hear-forward`
(which "performs all forwarding requests without filtering") relays to C1
because C3 holds C1's lane from 4.2.

So 4.3 answers two different questions and should be reported as two:

- **(a) bootstrap** — C2 `|hi ~havnyl-…` cold. Predicted failure, with
  `requesting attestion` in the trace and the blob going to C1's numeric star.
  If it fails, that is a **real design gap**, not a harness problem: a
  confidential comet is undiscoverable by construction, and the sponsor cannot
  bridge that because `+fetch-comet-pki` never asks it.
- **(b) forwarding** — give C2 C1's point the way jael would, by injecting the
  attestation out of band (`:gw-btc &noun [%jael-writ %gw-btc ~havnyl-…
  0x<C1's live pass>]`, which still runs the full ~100 s chain verification),
  then retry. The decisive evidence is, on C2,

  ```hoon
  .^((list lane) %ax /(scot %p our)//(scot %da now)/peers/(scot %p ~havnyl-…)/forward-lane)
  ```

  which should be C3's `[%if .64.227.13.22 34343]` and nothing else.

  Read C1's live pass off C1 itself (`p4ops.py <pier> mypass` → jael `%pynt` →
  `keys` → `pass`). **Do not** use `pass_atom_hex` from `.gw-comet-1.json`: it
  is 108 bytes, jael's `pass` for these ships is ~330, and they are not the same
  object.

Also worth separating when reading lanes: the reverse direction has no sponsor
path at all, because C2's snapshot carries `sponsor=~` and jael's `+sein` for C2
therefore falls back to its numeric star. C1 can still answer C2, but via the
`origin.shot` lane that C3 stamps on the forwarded packet — a direct route
learned through the relay, not sponsor routing. Calling that "4.3 passed in both
directions" would be wrong.

## 4.4 / 4.6 — decline is silence

`%gw-sponsor-decline` emits **no `%verdict` at all**, so ames never sees a
`%fail` and `+sy-sybl`'s additive snub (`sys/vane/ames.hoon:11226-11234`) is
never reached. The check that matters is therefore
`.^([?(%allow %deny) (list @p)] %ax /(scot %p our)//(scot %da now)/snubbed)` on
C3 staying `[%deny ~]` across a declined re-attestation. Note that decline also
deletes the sponsee entry (`sponsees (~(del by sponsees) who)`), and that a
declined ship short-circuits at `app/gw-btc.hoon:167` before any verification,
which is what makes 4.5's repeated retries cheap.

