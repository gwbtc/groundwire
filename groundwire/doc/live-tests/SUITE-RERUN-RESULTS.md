# Suite re-run — current code on the three clean-room comets

Re-deploy of the branch tips onto the same three confidential comets the
clean-room run stood up (`k1`/`k2`/`k3`), and a re-run of the full suite — with
the three things the clean-room run could **not** close as the point of this
run: tests 4.4–4.6/4.8, Bug 2's single-flight wedge, and the pre-auth DoS fix
live. Plus the first-ever **Tier-1 declassification** broadcast.

Companion to [`CLEANROOM-RESULTS.md`](CLEANROOM-RESULTS.md); every divergence
from it is flagged.

---

## Headline

1. **The deploy succeeded.** A solid pill was built from `hd/cc-kernel@6d9b3a4643`,
   **both kernel fixes were verified inside it** by booting a ship from it and
   scrying its own `sys/vane/ames.hoon`, and all three comets came back at the
   right `@p` and life on desk `hd/cc-landing@bf90840`. The *running* ships were
   then re-checked: the executing kernel carries `+on-hear-drop`.
2. **All six ordered pairs re-verify VALID** — 10 VALID, 0 INVALID, 0 snubs,
   `dome=[~ %gw-btc]` on every one, each an on-chain `fief` becoming a runtime
   lane. Every one was also a first contact from an unknown comet.
3. **Tier-1 declassification worked on chain — the first one ever sent.**
   Transaction
   [`08957455…0266`](https://mempool.space/tx/08957455470694feb0c82216635de43a4b43424bf037da544022dd5a062e0266)
   confirmed in **block 961353**: one input (k1's identity sat), output 0 the
   new life-2 snapshot, output 1 a 279-byte OP_RETURN whose push opcode is
   **`0x4d` — OP_PUSHDATA2 with a 269-byte little-endian length**, which
   `OP_PUSHDATA1` cannot express and which the Hoon encoder used to truncate
   silently. **k2 and k3 both accepted it as a state update and declassified
   k1**: out of `.confidential`, into `/x/points`, jael life 1 → 2, each
   logging `published itself on chain; now PUBLIC, permanently`.

Both other target gaps also closed: **4.4/4.5/4.6** ran properly on a current
kernel for the first time (named drop, never a snub, fully reversible), and
**bug 2's wedge was reproduced and behaved correctly** against a long-dormant
comet — slot released, no verdict, no snub, re-poke accepted.

**No product bugs were found.** Cost: 802 sats. C2 and C3 untouched.

| | |
|---|---|
| desk | `groundwire@hd/cc-landing` `bf90840` (was `8878da0` in the clean-room run) |
| kernel | `urbit@hd/cc-kernel` `6d9b3a4643` (was `de3222d36a`) |
| pill | `gw-cc-kernel-solid-DOS.pill`, sha256 `a791ec2a002c0cb98e6f31b1c17b3e401993aac5966b3045d68b29352fd07731` — **built in this run from the kernel tip**; the clean-room pill was `4e0af17b…` from `de3222d36a` |
| vere | `/opt/gw/bin/gw-vere`, reused (built 2026-08-04 from `vere@gw/next/kelvin/408`) |
| sidecar | `/opt/gw/bin/tcp-sidecar`, reused |
| boxes | N1 `64.227.13.22` (k1), N2 `159.223.141.63` (k2), N3 `206.189.188.16` (k3); 3.9 GB / 2 vCPU each |

## What is new in this deploy, vs the clean-room pill/desk

Kernel `de3222d36a → 6d9b3a4643`, exactly **one** kernel commit:

- **`4db0cfe544`** — *ames: drop comet packets we cannot route, instead of
  cueing them.* The **pre-auth DoS fix.** `+on-hear-packet` now classifies a
  comet packet by SHAPE, not peer state: an unknown sender's packet goes
  through `+is-open-packet`, and anything left over (unknown comet, not an
  attestation) hits a new **`+on-hear-drop`** rather than a bare `+cue` in
  `+sift-open-packet`. `+sift-open-packet` itself now asserts
  `+open-jam-shaped`. **This commit had never run live** — the clean-room pill
  predates it.
- `6d9b3a4643` — CI workflow only (ships the tcp-sidecar), no kernel change.

Desk `8878da0 → bf90840`:

- **`194e56d`** — *OP_PUSHDATA2 for publications.* The clean-room run's Gap 1
  blocker: `OP_PUSHDATA1` stops at 255 bytes but a fief-carrying publication is
  265–269 B. Fixed in all three encoders + the parser. **Unblocks Tier-1
  declassification**, which had never been broadcast because it could not be
  encoded.
- **`bf90840`** — *a check we could not evaluate is not a verdict, in every
  path.* Closes the last two default doorways onto the snub path
  (`+verify-lc` early aborts; `+run-checks` ok=%.y with a local refusal), each
  now a structurally-exhaustive `?-` over `$verdict-class`.

---

## Step 1 — deploy: PASS

| step | result |
|---|---|
| `git fetch` both repos | both at the named tips, local == remote (`bf90840`, `6d9b3a4643`) |
| pill built from `hd/cc-kernel@6d9b3a4643` | **PASS** — solid pill via `fyrd (solid:pill …)` on the local `builder3` fakeship; commit verified in clay first (5 probe files byte-exact against disk, `lib/gw-btc-pass.hoon` confirmed absent), sha256 `a791ec2a…` |
| **both kernel fixes present IN the built pill** | **PASS** — booted a fakeship from `a791ec2a…` and scried its own `%base` `sys/vane/ames.hoon`: `met 3 = 558410` (byte-exact to `6d9b3a4643`), and it contains `+open-jam-shaped`, **`+on-hear-drop`**, the `%open-packet-malformed` assertion in `+sift-open-packet`, and the `dropped unroutable packet` trace |
| desk built from `hd/cc-landing@bf90840` | **PASS** — `make groundwire`, `doc/` stripped (no `.md` survives); PUSHDATA2 (`0x4d` + 2-byte LE length) and `refusal-class`/`verdict-class` both confirmed in the built tree |
| pill + desk staged to all three boxes | **PASS** — pill sha256 `a791ec2a…` identical on all four machines; desk tree md5 `0485be6e…` identical on all four |
| ships backed up | **PASS** — each old pier moved aside to `<pier>.pre-dos` (atomic; 981M/1.1G/982M). NB: `ops/stopship.sh` matches the king by trailing argv, which for a `-c…-B<pill>` boot is the *pill*, so it silently missed; and `gw-vere -t` did not die on SIGTERM/SIGINT — only SIGKILL stopped it. Both are harness gaps (below), not ship faults. |
| booted `-w <name> -G <feed>` from the **xtr-baked** feed | **PASS** — each came up as exactly the intended `@p`, life 1, rift 0, and its live jael pass is **byte-identical to the artifact's `pass_with_xtr_hex`** (305/307/307 B, against a 108 B bare pass). The custody log is present with no post-boot poke, which removes the clean-room's finalize-ordering trap: that run booted the **miner** feed and had to grow the pass afterwards |
| three desks installed on each (`%tcp`, `%bitcoin-client`, `%gw-btc`, `%urb-snapshot`) | **PASS** — every commit landed; all unmounted; `/x/ready` answers on all three |
| deployed desk verified **in clay**, not just on disk | **PASS** — `%cx` byte lengths on all three ships: `app/gw-btc.hoon` 98404, `lib/self-attestation.hoon` 30115, `lib/urb-core.hoon` 19073, `lib/gw-btc-pass.hoon` 10274 — byte-exact to the built desk on all three |

The old (`de3222d36a`) piers are preserved at `<pier>.pre-dos` on each box and
were not deleted.

---

## Kernel unit tests, on the exact built kernel (Gap 3, static half)

Ran the vane suites against `gw-base` clay on the builder pier — i.e. compiled
against the **same** `sys/vane/ames.hoon` that became the deployed pill:

**`tests/sys/vane/ames` — 28 OK / 3 FAILED / 3 CRASHED.** The four DoS routing
cases and both structural guards all pass:

| test | result | routing case |
|---|---|---|
| `test-hear-attestation-from-unknown-comet` | **OK** | 1 — onboarding (the regression risk) |
| `test-hear-reattestation-from-known-comet` | **OK** | 2 — known comet re-attests |
| `test-hear-shut-packet-from-known-comet` | **OK** | 3 — known comet, `$shut-packet` |
| `test-hear-drops-unroutable-comet-packet` | **OK** | 4 — the real captured mainnet `%meme` bomb, now dropped |
| `test-is-open-packet-accepts-a-real-attestation` | **OK** | guard |
| `test-is-open-packet-rejects-a-cue-bomb` | **OK** | guard |

The 6 non-OK are **exactly** the six pre-existing failures the commit documents
(`message-flow`, `alien-encounter`, `ames-flow-with-new-rift` FAILED;
`comet-message-flow`, `comet-comet-message-flow`, `comet-sends-mesa` CRASHED) —
**zero new**. `tests/sys/vane/jael` and `tests/lib/vere/dawn`: **all pass**
(aggregate 0).

<!-- LIVE RESULTS APPENDED BELOW AS THEY LAND -->

---

## Harness gaps found and fixed during the deploy (`d0c29b6`)

Both were found the same way: a deploy that believed it had stopped three
ships and had not, then `cp -a`'d three piers whose LMDB was still being
written and reported the backups as good.

1. **`ops/stopship.sh` could not see the ships it exists to stop.** It matched
   the king with `$NF==<pier>` — the pier as the *trailing* argv field. That
   holds only for the RESTART form (`gw-vere -t --loom 32 -p <port> <pier>`).
   Every comet in every campaign is first booted with the CREATE form, which
   ends `… -G <feed> -B <pill>`, so the trailing field is the **pill** and the
   matcher returned nothing. Verified against the three live ships: old matcher
   empty, standalone-FIELD match returns the king. `gwsup.sh`'s `king_pid` had
   the identical bug — it never mis-fired a relaunch (VERE-DOWN needs king AND
   serf empty, and `serf_pid` keys off `--snap-dir`), but it was silently
   useless on exactly the ships the supervisor supervises.

2. **`gw-vere -t` does not exit on SIGTERM.** Measured on all three ships:
   TERM plus a 40 s wait left every king alive with its original PID; SIGINT
   likewise; only SIGKILL stopped them. `stopship.sh` sent one TERM, printed
   the residue, and returned **0 regardless** — so a caller reading the exit
   code saw success while the ship kept processing events. It now escalates
   TERM → INT → KILL, re-checks after each, and exits non-zero if anything
   survives.

This is the OPERATIONS.md §5.10 incident class again — ships handed over as
"stopped" that were running — in a new form: not a missing procedure, but a
procedure that could not see its target and could not tell it had failed.

*Classification: **harness bug**, both fixed in `d0c29b6`.*

**Backups for this run** were therefore taken by *moving* each stopped pier to
`<pier>.pre-dos` (atomic, instant, consistent) rather than copying a live one.
The clean-room `de3222d36a` piers are preserved there on each box.

---

## Pre-verification baseline (captured before any attestation)

All three piers are fresh, so **every peer is unknown on every verifier**. All
six ordered pairs read identically:

```
lyfe = ~            no point installed
dome = ~            not via the Groundwire path (nor any other)
snub = [%deny ~]    empty
```

and `/x/sponsees` and `/x/declined` are both `~` on all three.

This is the precondition that makes two of this run's three target gaps
testable at all:

- **DoS routing case 1** (unknown comet + valid attestation → `+on-hear-open`)
  is the *onboarding* path and the regression a careless fix breaks. Because
  every peer is unknown here, **every one of the six verifications below is a
  genuine first contact from an unknown comet** — the case is exercised six
  times over, on real wire packets, not simulated.
- **Test 4.8** (self-announcing registration). k1 holds no point for k2 or k3
  even though both are real, funded, on-chain, currently-running comets whose
  identities are committed in confirmed Bitcoin transactions. Registration is
  never inferred — from the chain, from a sponsor, or from anything else. It
  requires the subject's own attestation.

### Identity-sat economics, measured

A fief-carrying publication is **401 vB** (1 input, 1 P2TR output, 1 OP_RETURN
of 279 B). A state update takes its fee **out of the identity sat** — one
input, one output of `prior − fee`, no change — so the sat bounds the fee rate
the comet can ever pay:

| comet | sat | max sat/vB (absolute) | max sat/vB (staying ≥330 dust) |
|---|---|---|---|
| k1 / k2 / k3 | 1556 | 3.88 | **3.06** |
| C1 | 1445 | 3.60 | 2.78 |
| C3 | 1288 | 3.21 | **2.39** |

Two consequences worth stating, neither of which is a bug:

- **4 sat/vB is not "expensive" for these comets, it is impossible** — the fee
  would exceed the whole sat. The usual advice "bid above what recent blocks
  cleared to" has a hard ceiling here, and on 2026-08-06 block 961343 cleared
  at 4.01 sat/vB, *above k1's absolute ceiling*. A comet can be priced out of
  its own identity.
- **Publication is close to a one-way door at these sizes.** k1 publishing at
  2 sat/vB leaves 754 sats, and `754 < 330 + 401` — so k1 can never fund
  another *publication* at any fee rate. Plain rekeys (~111 vB) remain
  affordable for a while. Identity sats shrink monotonically and this path
  cannot top them up.

---

# THE HEADLINE — six pairs re-verified, and Tier-1 declassification broadcast

## Step 1 result: all six ordered pairs VALID on the new pill

Full bring-up from genesis took **2 h 17 m** (block headers ~65 min, filter
headers ~7 min). Every pair driven by the documented `%jael-writ` path against
the verifier's own from-genesis light client, and — because all three piers are
fresh — **every one is a first contact from an unknown comet**:

| verifier → subject | verdict | point state |
|---|---|---|
| k1 → k2 | **VALID** | `lyfe=[~ 1]  dome=[~ %gw-btc]  snub=[%deny ~]` |
| k1 → k3 | **VALID** | idem |
| k2 → k1 | **VALID** | idem |
| k2 → k3 | **VALID** | idem |
| k3 → k1 | **VALID** | idem |
| k3 → k2 | **VALID** | idem |

Each emitted `[%gw-btc-lc-scan-clean …]`, then
`%gw-btc: attestation for ~… is VALID`, then
`ames: lamp ~… static ip … port …` — the on-chain `fief` becoming a runtime
lane (**test 4.7**). `dome=[~ %gw-btc]` on all six is the clean discriminator
that the Groundwire path was used and not vanilla comet PKI (**test 3.4**).

**Tally across the three ships: 9 VALID** (2 peers + 1 self-`%anew` each),
**0 INVALID, 0 STALE, 0 UNDETERMINED, 0 snubs, 6 fief lamps, 0 `bail: meme`,
0 vere SIGSEGVs, 0 sidecar SIGSEGVs, 0 supervisor interventions.**

**Tests 5.6 / 7.2 PASS ×3** — the Causeway-built `%gw-custody-entry` was
ingested on all three; each logged
`custody log verified (1 entries); refreshing our pass` and `/x/custody` went
from empty to 1 entry.

> **A distinction the runbook does not draw.** Booting the xtr-baked feed gives
> jael a correct 305–307 B pass immediately — that is what peers verify — but
> `%gw-btc`'s **own** `chain.own` (`/x/custody`) stays **empty** until a custody
> entry is poked in. The two are independent stores. A ship can therefore be
> perfectly verifiable by everyone while its own `/x/custody` reads `~`, which
> looks alarming and is not. Only the poke populates `chain.own`, and it needs a
> synced light client because `+begin-anew` re-walks the whole log against the
> chain.

## Tests 4.4 / 4.5 / 4.6 — sponsor decline and clear: **PASS**

The clean-room could not close these; they last "passed" on a stale kernel via
poke injection. Run properly here on the live DoS kernel, k2 as verifier and k3
as subject, **after** k3 was already an installed point on k2 — so the only
variable is the decline gate.

| # | action | result |
|---|---|---|
| 4.4 | `%gw-sponsor-decline ~k3` on k2 | `/x/declined` = `{~k3}` (atom `231321342428198689666916463453877977805`, k3's `@p` exactly) |
| 4.4 | k3 re-attests | `writ from ~hacsut-… dropped: sponsorship declined by operator` — **a named drop, not silence** |
| 4.5 | k3 re-attests again | same named line; short-circuits **before** verification (no light-client work — cheap retries) |
| — | snub state throughout | `snub=[%deny ~]` — **never a snub**, and the existing point survived untouched at `lyfe=[~ 1] dome=[~ %gw-btc]` |
| 4.6 | `%gw-sponsor-clear ~k3`, k3 re-attests | `/x/declined` = `~`, then `%gw-btc-lc-scan-clean` → **VALID** again |

This is the designed behaviour exactly: declining produces *silence toward the
peer* and a *named line for the operator*, costs nothing to re-refuse, never
snubs, and is fully reversible.

## Test 4.8 — registration is self-announcing: **PASS**

Before any attestation, k1 held **no point** for k2 or k3 (`lyfe=~`, `dome=~`)
even though both are real, funded, currently-running comets whose identities
are committed in confirmed Bitcoin transactions, and k1's own light client was
fully synced over those blocks. `/x/sponsees` and `/x/declined` were empty on
all three. A point appears **only** after the subject's own attestation is
verified. Registration is never inferred — not from the chain, not from a
sponsor, not from liveness.

---

# TIER-1 DECLASSIFICATION — the first ever, and it WORKED

No Tier-1 declassification had ever been broadcast. It was impossible before
`194e56d` (a fief-carrying publication is 265–269 B and `OP_PUSHDATA1` stops at
255) and merely unproven after it. It is now proven on mainnet.

**Transaction `08957455470694feb0c82216635de43a4b43424bf037da544022dd5a062e0266`,
confirmed in block 961353.**

| | |
|---|---|
| subject | k1 `~talryg-widseg-nisdex-mipryd--dibweb-narzod-tortes-daplyd` |
| input 0 | `1653a2ce…:0` — k1's tracked identity satpoint, 1556 sats (the ownership proof: only its holder can spend it) |
| output 0 | `5120ad601c4846924a3f82c19c8f49933cd9416d25ec68fe270db6e19447bf58631b`, **754 sats** — `5120‖Q`, `Q` recomputed from `[internal-key, new-snapshot]` by a from-scratch secp256k1/BIP-341 implementation and compared against causeway's |
| output 1 | OP_RETURN, 0 sats, **279 bytes** — the publication rider |
| life | **1 → 2**, strictly increasing |
| fee | 802 sats, 401 vB, **2.000 sat/vB** (blocks around it cleared at 0.62–2.16) |
| outputs | exactly 2 — no change, no stray |

The OP_RETURN, decoded independently from the *signed* transaction bytes:

```
6a                 OP_RETURN
03 'urb'           the Groundwire envelope
01 09              protocol kelvin 9
4d 0d 01           OP_PUSHDATA2, length 0x010d = 269 LITTLE-ENDIAN
<269 bytes>        jammed [pass [internal-key snapshot blind-opening]]
```

**That `4d` is the whole point.** `OP_PUSHDATA1` carries one length byte and
cannot express 269; the Hoon encoder used to truncate it silently to
`269 mod 256`. This transaction is only encodable because of `194e56d`, and it
is the first one ever broadcast.

Re-encoding the intended publication from the artifact reproduces the on-chain
script **byte-for-byte**, and `spawn-commit(funding satpoint, blind)` equals the
`dat` commitment `2532d042cb…` — which is precisely the check the verifiers run.

## Both peers accepted it and declassified k1

`block-confirmations = 1`, so each scanner processed 961353 once the tip reached
961354. Both k2 and k3 logged, unprompted:

```
%gw-btc: ~talryg-widseg-nisdex-mipryd--dibweb-narzod-tortes-daplyd
         published itself on chain; now PUBLIC, permanently
```

and their state moved exactly as `+apply-state` → `+index-point` specifies:

| | k2 | k3 |
|---|---|---|
| in `/x/confidential` | **`%.n`** — left the set | **`%.n`** |
| in `/x/points` | **`%.y`** — now a public point | **`%.y`** |
| jael `/lyfe/` | **`[~ 2]`** — advanced 1 → 2 | **`[~ 2]`** |
| jael `/dome/` | `[~ %gw-btc]` | `[~ %gw-btc]` |

This closes the clean-room's Gap 1 and test 5.7 / 5b.2 — **"shipped but never
broadcast" is now "broadcast, confirmed, and accepted by every peer that tracked
it."** It also confirms the mechanism the runbook §10 describes and the
superseded Phase-5b conclusion denied: a confidential comet **can** become
public, because `+process-publication` routes a comet already in `unv-ids` to
`+apply-state` and never reaches `+apply-spawn`'s funding-satpoint guard.

---

# GAP 2 — the single-flight wedge: **REPRODUCED AND CLEAN**

The clean-room could not construct this: a freshly-spawned comet's entire
verification is servable from local light-client state, so cutting the transport
changed nothing and the job simply completed. The fix was to pick a
**long-dormant** subject — **C1** (`~havnyl-…`), whose sat last moved at height
**961196**, ~157 blocks back, so its BIP-158 liveness scan must genuinely fetch.

Method (`ops`-style, all on the droplet to avoid round-trip latency): poke the
writ, and 6 s later `iptables -I OUTPUT -p tcp --dport 8333 -j DROP` — leaving
the **sidecar alive**, because killing it trips `%gw-btc`'s readiness gate and
no job ever starts (fail-closed, and the reason the wedge is hard to build).

It stranded, exactly as intended:

```
[+6s]  DROP installed; sidecar 238507/238509 alive; serf alive; newt errors: 0
[26s]  inflight = (~havnyl-…)
[46s]  inflight = (~havnyl-…)
 …     unchanged through
[126s] inflight = (~havnyl-…)
```

No runtime death, no `%kick`, zero `newt: write failed` — the job was genuinely
blocked on fetches that could never arrive. Then:

```
[200s] %gw-btc: verification thread for ~havnyl-… ended without a verdict
5. inflight after: 0
6. re-poke: 27503 ('ok')   -- NOT "dropped: a verification is already in flight"
```

**All four properties hold:**

1. `+lc-fetch-timeout` fired and the strand failed.
2. **The slot was released** — `/x/inflight` went from `{~havnyl-…}` to empty.
   (`inflight` is deleted *before* the `?+ sign-arvo` switch, so even the crash
   path releases it.)
3. **No verdict was emitted** — no VALID, no INVALID, and **no snub**. A
   generic strand crash is correctly treated as retryable/indeterminate.
4. **The slot is reusable** — the re-poked writ was accepted, not rejected as
   already-in-flight.

This is `app/gw-btc.hoon:894-900` — the `[%khan %arow %.n …]` branch on a
`/verify/<ship>/<job>` wire — **observed firing for the first time.** The
clean-room recorded that "no test anywhere injects it" and it "remains
untested"; it is now exercised live, on mainnet, against a real dormant comet.

*Classification: **no defect**. The fix behaves exactly as designed under the
one condition that can actually produce the wedge.*

---

# GAP 3 — the pre-auth DoS fix, live

`4db0cfe544` had never run anywhere. It does now, on all three ships:

**The running kernel is the fixed kernel.** Scried from each *live* ship's own
`%base`, not from the pill: `sys/vane/ames.hoon` is **558410 bytes** with
`+open-jam-shaped` @17654, **`+on-hear-drop` @170840**, the
`dropped unroutable packet` trace @170972 and the `%open-packet-malformed`
assertion in `+sift-open-packet` @15069 — byte-identical offsets to the pill.

## Test 3.1 — two attested comets exchange `|hi` both ways: **PASS**

All six ordered pairs, over real ames packets:

| | k1→k2 | k2→k1 | k1→k3 | k3→k1 | k2→k3 | k3→k2 |
|---|---|---|---|---|---|---|
| result | ACKED 1s | ACKED 2s | ACKED 1s | ACKED 1s | ACKED 2s | ACKED 1s |

> **How the receiver is proved, since the obvious signal is a trap.** `+poke-hi`
> emits through `flog %text`, which goes to **dill's terminal** — and a `-t`
> ship has none, so the message never reaches the log file and grepping for it
> proves nothing. The real signal is the **ack**: strandio's `+poke` blocks
> until the remote `%poke-ack`, so a returned `'sent'` *is* the round trip.
> Negative control: the identical poke aimed at a comet that does not exist
> **never returned** (timed out at 65 s), while every real pair returned in
> 1–2 s.

## The four routing cases

| case | sender | payload | arm | how covered |
|---|---|---|---|---|
| 1 | unknown comet | valid attestation | `+on-hear-open` | **the onboarding path.** All six verifications ran against peers whose `lyfe` and `dome` were both `~` — every one a first contact from an unknown comet, all six VALID. Unit-covered on this exact kernel by `test-hear-attestation-from-unknown-comet` |
| 2 | known comet | re-attestation | `+on-hear-open` | exercised by the 4.6 re-attestation after clear (k3 already `%known` on k2) → VALID. Unit: `test-hear-reattestation-from-known-comet` |
| 3 | known comet | `$shut-packet` | `+on-hear-shut` | **the six `|hi` round-trips above**, all acked. Unit: `test-hear-shut-packet-from-known-comet` |
| 4 | unknown comet | garbage | **`+on-hear-drop`** | unit-covered on this exact kernel by `test-hear-drops-unroutable-comet-packet`, using **the real captured mainnet `%meme` bomb**. Deliberately **not** spoofed at a production ship — see below |

**`bail: meme` = 0 on all three ships across the entire campaign** — through
2 h 17 m of light-client sync, ten verifications, six `|hi` round-trips, a
transport partition, a vere SIGSEGV and a full replay. The old kernel logged
378 and never recovered.

Two honest limits on the live half:

- **Case 4 was not induced on a live ship.** Sending a spoofed-`sndr` `%meme`
  bomb at a production mainnet comet is an attack; it is covered by a unit test
  against the real captured packet on the byte-identical kernel, which is the
  right place for it. `+on-hear-drop` therefore never fired in production —
  correctly, because nobody attacked us.
- **`dropped unroutable packet` is `(ev-trace rcv.veb …)`,** so it only prints
  with `%rcv` ames verbosity on. A count of 0 is consistent with "no
  unroutable packets arrived" *and* with "the trace is off", and this run does
  not distinguish them. The load-bearing live claim is the negative one — the
  fix did not break case 1, 2 or 3 — and that is established six times over.

*Classification: **no regression**. Onboarding, re-attestation and steady-state
comet traffic all work on the fixed kernel; the structural guard is present in
the running image; zero `%meme` events.*

---

# Stability

| | Phase 6/7 (~5 h 40 m) | clean-room | this run (~3 h) |
|---|---|---|---|
| vere SIGSEGV | 1 | 0 | **1** (k1, during the transport partition; supervisor restarted it and it replayed events 67437–76813 with no state loss, back in ~20 s) |
| tcp-sidecar SIGSEGV | 24 | 0 | **0** |
| `bail: meme` | 0 (fixed kernel) | 0 | **0** |
| supervisor interventions | many | 0 | **1** (the above — and it worked) |
| peak RAM | — | — | ~700 MB of 3915, swap ~0 |
| synced pier | ~2.2 GB doc'd | 0.96 GB | **~0.84 GB** |

Timings against §8: block headers 0→tip **~65 min** (§8 says ~55), filter
headers 0→tip **~7 min** (§8 says ~95 — off by an order of magnitude, as the
clean-room also found), full bring-up **2 h 17 m**, one confidential
verification **70–150 s** (§8 says 100–110).

## Money

| | |
|---|---|
| spent | **802 sats**, the publication fee, taken out of k1's identity sat |
| k1 identity sat | 1556 → **754 sats** (new outpoint `08957455…:0`) |
| C2 | `0cca2561…` **1544 sats — untouched** |
| C3 | `8e713009…` **1288 sats — untouched** |
| wallet change | ~17,822 sats — **not needed and not touched**; the publication is self-funded from the identity sat |

Nothing else was broadcast. C1's sat was read for the wedge test but never spent.

---

# Matrix versus `CLEANROOM-RESULTS.md`

| # | test | clean-room | **this run** | divergence |
|---|---|---|---|---|
| 0 | bring-up | PASS on a `de3222d36a` pill | **PASS** on a pill built from `6d9b3a4643` | first pill carrying the DoS fix |
| 1.1–1.4 | mint + confirm + `5120\|\|Q` | PASS ×3 | *n/a* — same three identities reused, nothing minted | — |
| 2.1 | genuine attestation | PASS ×6 | **PASS ×6** | held |
| 3.1 | `\|hi` both ways | not attempted | **PASS ×6**, acked 1–2 s | **NEW — closed** |
| 3.4 | prove the Groundwire path was used | PASS ×6 | **PASS ×6** (`dome=[~ %gw-btc]`) | held |
| 4.4 | decline, subject re-attests | last "passed" on a stale kernel via poke injection | **PASS** — named drop, no snub | **TARGET GAP — closed** |
| 4.5 | declined subject re-attests repeatedly | ditto | **PASS** — short-circuits before verification | **TARGET GAP — closed** |
| 4.6 | clear, back to normal | ditto | **PASS** — VALID again | **TARGET GAP — closed** |
| 4.7 | on-chain fief reaches the holder | PASS ×6 | **PASS ×6** (`ames: lamp`) | held |
| 4.8 | a comet that never attests never registers | not attempted | **PASS** | **NEW — closed** |
| 5.6 / 7.2 | custody entry → `%gw-btc` → jael | PASS ×3 | **PASS ×3** | held |
| 5.7 / 5b.2 | **confidential → public (Tier 1)** | **BLOCKED** — `PUSHDATA1` bug | **PASS — broadcast, confirmed in 961353, both peers declassified k1** | **THE HEADLINE — closed** |
| 6.1 | attestation while light client unsynced | PASS | **PASS** — observed again during the wedge (`light client is NOT synced; holding all attestations`) | held |
| 6.2 | kill transport mid-verification | **not reproduced** | **PASS — reproduced and clean** | **TARGET GAP — closed** |
| 6.7 | diagnosable from logs alone | "still no" | **better, still qualified** — see below | improved |
| DoS | four `+on-hear-packet` routing cases | *postdates the run* | **cases 1–3 live, case 4 unit-only; 0 `bail: meme`** | **TARGET GAP — closed as far as is safe** |
| — | ames/jael/dawn vane suites on the built kernel | — | **28 OK / 6 pre-existing failures / 0 new**; jael + dawn green | new evidence |

**Not attempted, and why:** 2.2–2.16 (the adversarial matrix — unchanged code,
pinned by unit tests); 4.1–4.3 (sponsorship topology — needs an on-chain
`sponsor=` state update, and k2/k3 would each have to spend their identity
sats); 5.1–5.5 (rekey/stale/re-attest — each needs its own spend); 6.3–6.6;
7.1/7.3/7.4 (Causeway-side, unchanged since the clean-room proved them).

## 6.7 — are failures diagnosable from logs alone?

**Better than the clean-room found, and still not "yes".** Improvements seen
directly this run: the decline path now names itself
(`dropped: sponsorship declined by operator`), the readiness gate names itself
(`light client is NOT synced; holding all attestations`), the wedge names
itself (`verification thread for ~… ended without a verdict`), and the custody
ingest names itself (`custody log verified (1 entries)`).

Two things still cost real time here and would cost an operator more:

- **`+poke-hi` writes to dill, which a `-t` ship does not have.** The delivery
  succeeded and the log said nothing at all; the only proof is the ack. A
  reader who greps for the message concludes traffic is broken. This is not a
  `%gw-btc` issue but it sits squarely on the main "did it work" path.
- **`/x/custody` reads `~` on a perfectly healthy, fully verifiable ship**
  until a custody entry is poked in, because jael's pass and `%gw-btc`'s
  `chain.own` are independent stores. Nothing says so.

# Classification of every finding

**Real bugs (product): none found this run.** Every product behaviour observed
matched its specification, including the two paths that had never executed
(Tier-1 declassification and the `%arow %.n` wedge branch).

**Latent issue, worth fixing before Tier 2:** `ops/gwmint.py cmd_publish` sets
the blind-opening's `start_height` from `proof["block_height"]` — the **spawn**
block (961324) — while the opening's satpoint is the **funding** outpoint,
created in block 961302 (`funding_height`). It did not matter here and could
not: `+process-publication` routes a *tracked* comet to `+apply-state`, whose
only opening check is `spawn-commit(spawn, blind) == dat`, and `start_height`
is not an input to it (verified: the value matches the artifact's
`spawn_commit_d_hex` exactly). It **would** matter on any path that walks from
the spawn — Tier 2, or a stranger's verification — where the verifier fetches
the block at `start_height` and looks for the funding transaction there. This
is the same `start_height` bug the clean-room fixed in `cmd_artifact`, still
present in `cmd_publish`.

**Harness / tooling (fixed here, `d0c29b6` + `aaf3baf`):** `stopship.sh`
matched the king by trailing argv and so could not see a `-c…-B<pill>` boot;
`gw-vere -t` ignores SIGTERM/SIGINT and needs SIGKILL, and the old script
returned 0 anyway; `gwsup.sh`'s `king_pid` had the same matcher bug;
`ops/state-*.json` (a signed mainnet transaction) was not git-ignored.
Also: `gwctl.py pass` prints dot-grouped Hoon `@ux` but `gwctl.py writ` does
`int(pass,16)`, which rejects the dots — the two do not compose, and the
docstring does not say the `<patp>` argument needs its leading `~`.

**Infrastructure:** one vere `loom: external fault` on k1 during the transport
partition, recovered automatically by the supervisor with a full replay and no
state loss. Zero sidecar crashes. Separately, the **build host** ran out of
disk mid-run (a 28 GB stale aqua pier in a scratch directory), which broke
every tool including `df`; cleared, 32 GB reclaimed.

**Documentation:** the runbook held up well this time. Two additions earned:
the jael-pass-vs-`chain.own` distinction (§6), and that `flog`/`|hi` output is
invisible on a `-t` ship so the ack is the only delivery proof (§6).

---

# Rig left in this state

| | k1 | k2 | k3 |
|---|---|---|---|
| ship | up, life 1, replayed clean after its SIGSEGV | up, life 1 | up, life 1 |
| supervisor | 1 | 1 | 1 |
| sidecar | connected | connected | connected |
| desks mounted | **none** | **none** | **none** |
| light client | synced to tip | synced, scanner following | synced, scanner following |
| clean-room pier | preserved at `k1.pre-dos` (981M) | `k2.pre-dos` (1.1G) | `k3.pre-dos` (982M) |
| iptables | clean (the wedge's DROP was removed) | — | — |

**k1's on-chain identity is now life 2 and PUBLIC, while the running k1 ship is
still life 1.** That divergence is expected — an on-chain state update does not
rekey a running ship — and it demonstrably does **not** break traffic: k1↔k2 and
k1↔k3 `|hi` both acked in 1–2 s afterwards, because the publication advanced
only `life` and carried the same networking key forward. A future rekey that
actually changes the key would need the ship rebooted onto the new life.

## Two residual harness notes

- **A supervisor was duplicated during the wedge test.** `wedge_remote.sh`
  restarts one unconditionally in its `EXIT` trap, and the pier briefly had
  two. `gwsup.sh`'s `flock` guard is **sound** — tested directly on the box, a
  third start printed `a supervisor for k1 is already running; refusing` — so
  this is the scratch script's fault, not the supervisor's. Cleaned up; one per
  pier now.
- **`ops/gwctl.py` argument conventions do not compose.** `gwctl.py pass`
  prints a dot-grouped Hoon `@ux` (`0x2.e37a.b201…`) but `gwctl.py writ` does
  `int(pass_hex, 16)`, which rejects the dots; and every command that takes a
  `<patp>` needs the leading `~` because it interpolates into `` `@p`<patp> ``,
  where a bare name parses as a wing. Neither is in the docstring. Both were
  caught before they could corrupt a result, but both would stop a new operator
  cold.
