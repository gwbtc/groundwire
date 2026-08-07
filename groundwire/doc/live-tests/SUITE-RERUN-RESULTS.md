# Suite re-run — current code on the three clean-room comets

Re-deploy of the branch tips onto the same three confidential comets the
clean-room run stood up (`k1`/`k2`/`k3`), and a re-run of the full suite — with
the three things the clean-room run could **not** close as the point of this
run: tests 4.4–4.6/4.8, Bug 2's single-flight wedge, and the pre-auth DoS fix
live. Plus the first-ever **Tier-1 declassification** broadcast.

Companion to [`CLEANROOM-RESULTS.md`](CLEANROOM-RESULTS.md); every divergence
from it is flagged.

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
| booted `-w <name> -G <feed>` from the **xtr-baked** feed | **PASS** — each came up as exactly the intended `@p`, life 1, rift 0; live jael pass 380–383 B (≫ 108 B), so the custody log is present with no post-boot poke — booting the xtr feed removes the clean-room's finalize-ordering trap |
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
