# Cleanroom run — three brand-new confidential comets

Independent re-run of the whole confidential-comet procedure from scratch, on
three identities with **no history**, following
[`../OPERATIONS.md`](../OPERATIONS.md) as the procedure and treating every
place the runbook misled a first-time operator as a finding.

Started 2026-08-06 ~16:40 UTC. Real mainnet BTC, real ships.

| | |
|---|---|
| desk | `groundwire@hd/cc-landing` `8878da0` |
| kernel | `urbit@hd/cc-kernel` `de3222d36a` |
| pill | `gw-cc-kernel-solid-TIP.pill`, sha256 `4e0af17b937abe2efcdcaeb965bfbb9013270285837c9a47bfd249edd55e6851` — **built in this run from the kernel tip** |
| vere | `/opt/gw/bin/gw-vere`, reused, built 2026-08-04 from `vere@gw/next/kelvin/408` |
| sidecar | `/opt/gw/bin/tcp-sidecar`, reused |

## The three comets

All minted confidential (no OP_RETURN), all at life 1, each committing a
`fief` naming the exact IP and UDP port its ship is pinned to.

| | comet | box | pier | fief | funding | spawn |
|---|---|---|---|---|---|---|
| **k1** | `~talryg-widseg-nisdex-mipryd--dibweb-narzod-tortes-daplyd` | N1 `64.227.13.22` | `/opt/gw/piers/k1` | `.64.227.13.22:35353` | `012f4a40…:1` | `52ef7801…` |
| **k2** | `~namfyn-fonnyt-roctes-pintev--hidtyv-wacwyx-faddyr-daplyd` | N2 `159.223.141.63` | `/opt/gw/piers/k2` | `.159.223.141.63:35354` | `30467294…:1` | `bd9bb2d3…` |
| **k3** | `~hacsut-ribmel-fonfun-sibbyr--sapted-fonnex-dolfen-daplyd` | N3 `206.189.188.16` | `/opt/gw/piers/k3` | `.206.189.188.16:35355` | `c80d1739…:1` | `de0685c5…` |

Each spawn: 1 input (the intended 2000-sat funding UTXO), **one** output of
1889 sats at `5120||Q`, 111 sats fee, 111 vB, exactly 1.000 sat/vB. No change
output — a 2000-sat input does not trip causeway's `> 2000` change branch.

Artifacts at `~/gw-building/.gw-comet-r{1,2,3}.json` (0600). The three
pre-existing comets' artifacts (`.gw-comet-{1,2,3}.json`) were not touched and
their identity sats were not spent.

### Verification gate, per transaction, before any broadcast

`ops/gwmint.py build` refuses to set `gate_passed` — and `broadcast` refuses to
run without it — unless every check passes. All three printed:

```
[PASS] txid matches embit
[PASS] exactly 1 input
[PASS] input 0 outpoint is the intended funding UTXO
[PASS] input 0 scriptSig empty
[PASS] witness is a single 64/65-byte schnorr sig — 64 bytes
[PASS] exactly 1 output(s) — got 1
[PASS] leaf hash: causeway == independent
[PASS] Q: causeway == from-scratch secp256k1
[PASS] output 0 scriptPubKey == 5120||Q
[PASS] output 0 value >= 330 (P2TR dust) — 1889 sats
[PASS] fee <= 500 sats — fee = 111 sats
[PASS] effective fee rate >= 1.0 sat/vB — 111/111 = 1.000 sat/vB
[PASS] no OP_RETURN output (confidential)
[PASS] bitcoind testmempoolaccept — allowed: true
```

`Q` is recomputed by a from-scratch secp256k1 / BIP-341 implementation inside
`gwmint.py` and compared against causeway's, so a bug in causeway's encoder
cannot pass the gate. Fees never exceeded 5 sat/vB during the run
(`fastestFee` peaked at 5, `hourFee` stayed at 1); nothing was broadcast above
1 sat/vB.

---

## Infrastructure: what was NOT validated

The three droplets were already tuned from the previous campaign, so **the
infrastructure half of the runbook did not run and remains unvalidated by this
test.** Explicitly, these steps were skipped because the box already satisfied
them:

| runbook step | why skipped |
|---|---|
| §2 apt packages (`tmux rsync build-essential pkg-config libssl-dev`) | already installed |
| §2 8 GB swapfile | already present (8191 MB swap on each box) |
| §2 UDP inbound open, outbound TCP 8333 | already open; the previous campaign's fiefs worked |
| §3.1 build vere (zig 0.15.2, sibling `urcrypt`, `-Dunsafe-dawn`) | **reused the 2026-08-04 binary.** Not rebuilt, not re-verified. The zig-version pin, the `urcrypt`-must-be-a-sibling rule and the `.git/logs/HEAD` requirement are all untested here |
| §3.3 build the tcp-sidecar | reused the existing binary |
| §5.1 provision `/opt/gw/{bin,pills,piers,desks}` | already existed |
| §5.6 first peer-pool build | reused `peerpool.txt` (831 IPs); `poolfill.py` added 34 new `x49.` IPs on top |
| the `%node` and `%tcp-sidecar` desks | reused the staged copies at `/opt/gw/desks/` |

Also note the boxes are **3.9 GB RAM / 2 vCPU**, not the "4 GB + 8 GB swap"
of §2 — close enough that nothing was adjusted, but the sizing table is
describing this rig, not a spec.

What this run *did* exercise end to end: the pill build from a kernel branch,
the `%groundwire` desk build and deploy, minting, boot, desk install, sidecar
start, peer seeding, supervision, light-client sync from genesis, and
verification.

---

## Documentation findings

The runbook's accuracy was under test. These are the places it failed a
first-time operator, in descending order of cost. All are fixed in
`cadc9ef`.

### 1. "There is no local pill build" — false, and expensive (§3.2, §11)

§3.2 said the pill is "not built by this repo, and not by hand", pointed at a
CI job in `gwbtc/urbit` on branch `gw/next/kelvin/408`, and marked the local
route known-broken. §11 repeated it.

This is wrong, and it was wrong when written: the Phase 6/7 results this repo
ships were produced on `gw-cc-kernel-solid-P67.pill`, **built locally** by
`fyrd`-ing `solid:pill` on a fakeship. The genuinely broken thing is the
`+pill/solid` *dojo generator*; `fyrd`-ing the `solid:pill` *gate* is a
different route entirely.

Following the runbook would have sent this run to a 45-minute CI job on a
branch that is **not the one under review** — and the kernel is the thing under
review. Instead the pill was rebuilt locally from `hd/cc-kernel@de3222d36a` in
about six minutes, and all three comets booted from it.

*Classification: **documentation**, high severity.*

### 2. "A confidential comet cannot become public" — false (§10, §5.2)

Stated as a permanent design property, in a section titled *Things that are
deliberately not repairable*, with a reason attached:

> `+apply-spawn` is the only writer of the public index and requires spending
> the original funding satpoint.

Both halves are wrong. `+apply-state` writes the index via `+index-point`
(`lib/urb-core.hoon:353` → `:376`) and the confidential verifier writes it
directly (`app/gw-btc.hoon:1919`) — which is *why* a confidential comet is in
`unv-ids` at all. The funding-satpoint guard (`lib/urb-core.hoon:311`) belongs
to `+apply-spawn`, which a tracked comet never reaches, because
`+process-publication` (`:256-262`) routes anything already in `unv-ids` to
`+apply-state` first.

Declassification (Tier 1) shipped in `d63e28a`; the runbook commit landed
*after* it and reproduced the superseded Phase-5b conclusion verbatim.

*Classification: **documentation**, high severity — it tells you not to attempt
something the code supports, in the section you'd check first.*

### 3. The boot procedure does not match any real deployment (§5.3)

§5.3 said: run inside tmux, **do not** pass `-t`, drive the dojo with
`send-keys`. No live ship has ever run that way. Every campaign ship, and all
three in this run, runs `-t` under a supervisor and is driven over
`<pier>/.urb/conn.sock`.

The consequence is not stylistic. A light-client bring-up is ~2.5 h and both
vere and the sidecar die under load (§5.9) — a ship that exists only inside
your tmux pane cannot be supervised. Worse, **every `>` line in the runbook is
notation**: `> :gw-btc &noun [%jael-writ …]` is not typeable on a `-t` ship,
and a scry written `.^(* %gx /…/ready/noun)` has to be wrapped in a
`(strand ,vase)` thread and handed to khan. Nothing said so. `ops/gwctl.py` is
now that wrapping and the runbook says so.

*Classification: **documentation**, high severity.*

### 4. There was no shutdown procedure at all — and it caused an incident

The runbook has ~1000 lines on bringing a ship up and **not one line on
stopping one**. That gap produced a concrete failure *in the handover to this
run*: the three control ships were believed stopped, were described as stopped
in this run's own brief, and were in fact **running** — because stopping a
supervised ship without stopping its supervisor first is a no-op. `gwsup.sh`
notices VERE-DOWN within one 30 s poll and relaunches.

Found all three boxes running their control ships, `r3`'s serf at 1.76 GB RSS
on a 3.9 GB box — which would have contended with the fresh comet's sync. Added
as §5.10 and `ops/stopship.sh`.

*Classification: **documentation** (missing procedure) with a real
operational consequence.*

### 5. Naming a confidential comet as a sponsor is fatal, and undocumented

`sponsor-ok` (`lib/self-attestation.hoon:456-459`) is
`(~(has in known-public) u.sponsor…)` — the sponsor must be a **public** point.
Name a confidential one and `sponsor-known` fails; the check is classified
unevaluable, so **every** verification of the sponsee returns UNDETERMINED, no
verdict is emitted, and no point is ever installed. Nothing logs "your sponsor
is confidential".

This run's original topology was going to be "k1 is the sponsor, k2 and k3
name it" — the proven Phase 4 shape. That would have made k2 and k3
permanently unverifiable, and the failure mode is silence. Caught by reading
the check before spending, not by any warning in the runbook. §5.8 mentions
that a *public* sponsor must be indexed; it never states the converse.

*Classification: **documentation**, high severity. The code behaviour is
correct and deliberate.*

### 6. A 108-byte pass is the tell for a missing custody log

A ship booted from the miner's feed rather than the xtr-baked one serves a
108-byte pass and looks entirely healthy: right `@p`, right life, agents
installed, `/x/ready` fine. It simply cannot be verified by anybody, because
there is no custody evidence to check. After the custody log lands the same
scry returns ~330–405 B.

§6 warned that the artifact's `pass_atom_hex` is "a different, shorter object
(108 B)" but never connected that number to the failure mode, and §5.2's
`finalize` note ("without this the ship boots fine but serves an empty log")
does not mention that booting before the spawn confirms puts you there. Added
to §6 as an explicit first-check.

*Classification: **documentation**.*

### 7. `gwsup.sh` had no singleton guard

All three droplets were found running **two** supervisors per pier. Two
supervisors both see VERE-DOWN, both relaunch, and the loser dies on
`mesa: bind: address already in use` — which reads exactly like a crash loop,
and §9 documents that symptom without naming this cause. §5.9 specified the
supervisor's triggers but never said it must be a singleton.

Fixed with an `flock` in `ops/gwsup.sh`; verified that a second start now
refuses.

*Classification: **harness bug**, now fixed.*

### 8. Causeway cannot build the transactions the test plan needs

Not a runbook error so much as a gap it never mentions:

- **No `--fief`, anywhere.** `rekey` carries the prior fief forward and offers
  no way to set one. The Phase 4 sponsorship topology *requires* the sponsor to
  commit a real fief, so that transaction cannot come from Causeway.
- **No `--publish` on `rekey`, and no `publish` subcommand.**
  `build_rekey_psbt` accepts `publication_pass_atom` / `publication_opening`
  and builds the OP_RETURN correctly, but **every caller passes neither**
  (`causeway.py:2669-2674`, `causeway_tui.py:803-808`). The Tier 1
  declassification path therefore has no CLI at all.

Both are why `ops/gwmint.py` exists and why the Phase 5b run needed an
out-of-tree `stateup5b.py` that was never committed.

*Classification: **real gap in the tool**, documented in §11.*

### 9. Smaller things

- §1's repo table names `gwbtc/*` on `gw/next/kelvin/408`; the branches under
  review are `hd/cc-landing` and `hd/cc-kernel`, and the local `urcrypt` is on
  `zig-build`, not `gw/cmp-pub`.
- §5.2's headless flag list is incomplete in a way that costs money:
  **`--assume-saved` is mandatory**, because without it the blind-phrase
  read-back fires a second time *after the transaction is broadcast* and the
  run exits 2 with mainnet money spent. Piping stdin does not help — the
  `isatty()` check fires before `input()`, so every prompt must be
  pre-answered by a flag.
- The campaign tooling `gwsup.sh` / `poolfill.py` / `p4setup.py` was described
  in §11 as "not in this repo… must be rewritten". It was not lost — it was on
  the droplets. Recovered and committed (`5928283`). Meanwhile
  `testnet/gwharness/`'s Python sources **had** already been lost from this
  checkout (only `__pycache__` survived) and were recovered from N1 in the same
  sweep. Single-copy-on-a-droplet is not a storage strategy.
- Two of the recovered tools were **dead code against a real node**:
  `p4setup.py`'s `sync`/`info` and `peercount.py` scry `%bitcoin-client`, whose
  `++peek` is literally `~` for every path (`app/bitcoin-client.hoon:181-184`).
  The runbook is right about this (§5.7); the tooling never was. Not carried
  forward in that shape.

---

## `+groundwire!aqua-fixtures` — first execution

The generator had never been run, only syntax-checked. Run on a local fakeship
booted from the tip pill with `%groundwire` installed:

```
comet-ok  = ~tirdyn-hocpes-ribtyl-fitfyr--winrus-dabdyl-nardev-dapryc
comet-fail= ~hodwyn-topmul-sogfun-hatfeb--riblug-nomnep-fidben-macfun
domain    = %test-pki
```

Cross-checked every emitted literal against the kernel's checked-in fixtures in
`urbit/pkg/arvo/lib/aqua-azimuth.hoon`:

| fixture | bits | result |
|---|---|---|
| `dat-tail-ok` | 259 | MATCH |
| `dat-tail-fail` | 266 | MATCH |
| `xtr-ok-life-1` | 1061 | MATCH |
| `xtr-ok-life-2` | 1473 | MATCH |
| `xtr-fail-life-1` | 1067 | MATCH |
| `xtr-fail-life-2` | 1474 | MATCH |
| `domain` | — | MATCH (`%test-pki` both sides) |

**The generator runs and its output matches the kernel byte-for-byte.** The
checked-in Aqua fixtures are not stale.

One residual: the generator's *printed* comet names are produced by the desk's
`lib/gw-btc-pass` codec, while arvo computes `cc-comet-ok`/`cc-comet-fail` from
the same atoms with its own generic `cric` at compile time
(`aqua-azimuth.hoon:266-267`). Executing arvo's computation to compare the two
names directly was not completed: dojo state does not persist across `lens`
calls, and the `khan`-with-beams route fails because `aqua-azimuth` pulls in
`/- dice, *aquarium` and `/+ ethereum, azimuth` transitively. Since both sides
derive from identical atoms under one shared `cric`, and the scenarios refer to
the comets symbolically rather than by literal name, nothing functional depends
on it — but the generator's own docstring asks for that comparison and it
remains unperformed.

*Classification: **pass**, with one unperformed sub-check.*

---

## Phase 0 — bring-up

| step | result |
|---|---|
| pill built from `hd/cc-kernel@de3222d36a` | **PASS** — ~6 min, sha256 `4e0af17b…`; commit verified in clay before compiling (all four probe files byte-exact, `lib/gw-btc-pass.hoon` confirmed *absent*) |
| desk built from `hd/cc-landing@8878da0` | **PASS** — `make groundwire`, `doc/` stripped, no `.md` survives; deployed to all three hosts, `sha256` of the whole tree identical on all four machines (`aa93ed50…`) |
| three comets mined | **PASS** — all three in under 60 s, all under `~daplyd`, each ring's `dat` matching the independently computed `dat` |
| three spawns broadcast | **PASS** — gate above; see the RBF note below |
| three ships booted | **PASS** — each came up as exactly the intended `@p` at life 1, rift 0 |
| three desks installed on each | **PASS** — `%groundwire`, `%node`, `%tcp-sidecar`; every commit landed on the **first** poke, verified by `%cx` byte-length against disk; all unmounted immediately |
| sidecars connected | **PASS** — `lick: connected` on all three |
| peers seeded | **PASS** — `x49.`-filtered pool, batches of 25 |
| supervisors | **PASS** — one per pier, singleton guard verified by attempting a duplicate |

### The vanes are byte-identical to the pill Phase 6/7 ran on

Worth stating because it bounds what a fresh pill could have changed: neither
commit between `efe63a546e` (the P67 pill) and `de3222d36a` (the tip) touches
`sys/vane/ames.hoon` or `sys/vane/jael.hoon`. The delta is docs, `tests/`,
`ted/ph/cc/*`, `lib/ph/*`, `lib/aqua-azimuth.hoon`, and the deletion of
`lib/gw-btc-pass.hoon`. The pill was rebuilt anyway so the run validates the
tip under review rather than something near it.

### The spawns had to be fee-bumped

Broadcast at 1 sat/vB (111 sats). They sat unconfirmed for ~1 h 40 m through
two blocks whose own minimums were **2.1 and 3.0 sat/vB**, while
`mempool.space`'s `hourFee` reported 1 throughout. Diagnosed by measuring the
mempool directly — only ~1.3 blocks of vsize sat above 1.0 sat/vB, but blocks
were not clearing that far down.

Replaced all three at 4 sat/vB as full-RBF. **The identity is unaffected**:
`Q` and the leaf hash were bit-identical across the replacement for all three
comets, because the `dat` commits to the *funding* outpoint, not to the spawn
txid. Only the identity satpoint moved. All three confirmed together in block
**961324**.

| | replaced txid | sat |
|---|---|---|
| k1 | `1653a2cebf0ef447764e41a2b8e20a04f8dc8fe7bea4d571914f77c520733d35` | 1556 |
| k2 | `8fc1f950758a440fc4000e16c3594d8450c98c1037c89e846016f1840cce9fe7` | 1556 |
| k3 | `ceaa3fd752b5d58291dc299c9f71139d762ed527b30f529775b751a9dd907482` | 1556 |

Both `verify_proof_self` and `verify_proof_onchain` returned OK for all three.

*This is a documentation finding, not a bug — see §5.2's new fee-rate
guidance. It cost ~2 h of the run's critical path.*

### Stability, against the Phase 6/7 baseline

Over the bring-up and sync, across three hosts:

| | Phase 6/7 (~5 h 40 m) | this run |
|---|---|---|
| vere SIGSEGV | 1 | **0** |
| tcp-sidecar SIGSEGV | 24 | **0** |
| supervisor interventions | many | **0** |
| `bail: meme` | 0 (on the fixed kernel) | **0** |

Peers were seeded in batches of 25 from the very first batch, which is the
documented mitigation for `gwbtc/node#1`. The previous campaign discovered that
rule partway through. Zero sidecar crashes is consistent with the batch size
being the real trigger, though one clean run is not proof.

## The readiness gate, observed live

Poking `%gw-custody-entry` into k1 while its light client was still syncing:

```
%gw-btc: %anew refused: light client not synced (tip 554.000)
```

and the pass stayed at 108 bytes, with state untouched. This is the Phase 7.2
work — "SEVEN silent refusals used to live here… every one of them now says
which it was" (`app/gw-btc.hoon:1691-1695`) — confirmed on a live ship rather
than a fixture. Re-poking after sync is the intended flow and is safe.

**Ordering constraint the runbook does not state:** the custody entry cannot be
ingested until the light client is synced, because `+begin-anew` re-verifies
the whole extended log against the chain before it will re-encode the pass. So
the real order is *spawn confirms → light client syncs → custody entry → pass
grows → peers can verify*, and the 2.5 h sync is on that path, not parallel to
it.

## Re-test of 7.1 — headless spawn (was a Phase 6/7 **FAIL**)

Phase 6/7 recorded: *"four blocking `input()` calls with no flags; two
`continue` on EOF, producing a 100 %-CPU infinite loop that wrote 57 MB of
`  > ` in 31 s"*.

Re-run against piped (non-TTY) stdin, no money at risk:

| case | result |
|---|---|
| no `--sponsor` and no `--no-route` | **exit 2**, immediately, 599 bytes of output |
| `--no-route`, no fundable UTXO | **exit 1**, bounded, 357 bytes |

Neither hangs, neither loops, and the first names both remedies explicitly:

> `Error: this snapshot has neither a sponsor nor a fief, so nothing can
> cold-contact the comet: the verifier projects an absent sponsor to SELF
> (+urb-point-to-jael) … Pass --sponsor <@p or mnemonym>, or pass --no-route
> if you really do want an outbound-only identity.`

**7.1: PASS.** Also note what that message proves about finding 8 — `--sponsor`
and `--no-route` are the *only* two ways out, because the CLI has no way to set
a fief at all.

## A troubleshooting entry that fires on every healthy run

At the halfway mark k1 reported:

```
[%headers 608.001]
[%filter-headers 1]
[%live-earth-peers 139]
```

which is verbatim §9's *"Filter-header height never leaves 1, block headers
advance normally → your peers cannot serve compact filters… this killed an
entire test run."*

Nothing was wrong. `+continue-syncing-headers`
(`bitcoin-client.hoon:1015-1032`) does not request a single filter header until
`block-headers-are-synced`, so this is the expected state for the entire
~55-minute block-header phase. Following the runbook would have meant tearing
down a healthy 139-peer set for no reason.

The discriminator, now in §9: the symptom is real only once `%headers` has
**reached the tip**; and a healthy `[%live-earth-peers N]` already proves the
pool is CF-capable, because `++peer-services-are-sufficient` requires
`node-compact-filters` before a peer can be live at all.

*Classification: **documentation**, and the most likely of all these findings
to waste a new operator's time, because it fires mid-run on a healthy system
and the remedy is destructive.*

---

# THE HEADLINE — three fresh comets, mutually verified

All six ordered pairs, driven by the documented `%jael-writ` path, each against
the verifier's own from-genesis light client:

| verifier → subject | verdict | time | fief installed |
|---|---|---|---|
| k1 → k2 | **VALID** | 81 s | ✓ `.159.223.141.63:35354` |
| k1 → k3 | **VALID** | 148 s | ✓ `.206.189.188.16:35355` |
| k2 → k1 | **VALID** | 70 s | ✓ `.64.227.13.22:35353` |
| k2 → k3 | **VALID** | 81 s | ✓ |
| k3 → k1 | **VALID** | 69 s | ✓ |
| k3 → k2 | **VALID** | 81 s | ✓ |

Every one produced `[%gw-btc-lc-scan-clean …]` then
`%gw-btc: attestation for ~… is VALID`, followed by
`ames: lamp ~… static ip … port …` — the on-chain `fief` becoming a runtime
lamp. Point state on every verifier, for every subject:

```
lyfe = [~ 1]            point installed at life 1
dome = [~ %gw-btc]      via the Groundwire path, NOT vanilla comet PKI
snub = [%deny ~]        empty
```

`dome` is the clean discriminator (§6) and it reads `%gw-btc` in all six.

**Final tally across the three ships: 13 VALID, 0 INVALID, 0 snubs, 0 vere
SIGSEGVs, 0 sidecar SIGSEGVs, 0 supervisor interventions.** Verifications ran
69–148 s against a documented 100–110 s.

Timings, measured, against §8's table:

| stage | §8 says | measured |
|---|---|---|
| block headers 0 → tip | ~55 min | ~65 min |
| filter headers 0 → tip | ~95 min | **~12 min** |
| full light-client bring-up | ~2.5 h | **~1 h 20 m** |
| synced pier on disk | ~2.2 GB | **~0.96 GB** |
| one confidential verification | 100–110 s | 69–148 s |

§8's filter-header figure is off by ~8× and the pier size by ~2.3×. Both are
better than documented, but "anything materially slower than the table is a
symptom" cuts both ways — an operator using §8 to judge progress would have
concluded something was wrong.

---

# Gap 1 — confidential → public (Tier 1): **BLOCKED BY A NEW BUG**

The precondition was in place: k2 and k3 both track k1, which is what Tier 1
requires. Building the state-update-with-publication transaction for k1 fails
before it can be signed:

```
File "causeway.py", line 887, in make_publication_script
    push = b"\x4c" + bytes([len(payload)])
ValueError: bytes must be in range(0, 256)
```

**`OP_PUSHDATA1` carries a single length byte, so it cannot express a payload
longer than 255 — but the protocol's own `max-publication` is 512.** Measured
payloads for these comets:

| shape | payload |
|---|---|
| spawn publication, `start_height=0` | **265 B** |
| spawn-shaped at life 1 | **268 B** |
| late publication at life 2 (Tier 1) | **269 B** |

All three are legal by the spec and none can be encoded.

This is not just Causeway. The **Hoon encoder has the identical defect**, and
the **Hoon parser cannot read a `PUSHDATA2` script either**:

```hoon
++  publication-script                          :: lib/gw-btc-pass.hoon:201
  ?>  (lte wid.payload max-publication)         :: guards at 512 …
  =/  psh=hexb:btc
    ?:  (lte wid.payload 75)  [1 wid.payload]
    (cat:byt:bcu ~[[1 0x4c] [1 wid.payload]])   :: … but only ever emits PUSHDATA1

++  parse-publication                           :: :238
  =?  rst  =(0x4c dat:(take:byt:bcu 1 rst))     :: only 0x4c
    (drop:byt:bcu 1 rst)
  =/  len=@ud  dat:(take:byt:bcu 1 rst)         :: one length byte
```

Python raises; **Hoon does not** — `[1 wid.payload]` with `wid.payload > 255`
is a one-byte field holding a value that does not fit, so the Hoon encoder
silently emits a corrupt script. The `(lte … max-publication)` guard at 512 is
a lie in all three implementations; the real ceiling is 255.

**Why this was never hit before:** the previously published comets (C2, C3)
were published *at spawn*, when their snapshots carried **no fief**. Every
comet in this run commits a fief at spawn, which pushes the payload past 255.
So the bug is invisible to a comet with no fief and fatal to one with — and the
Phase 4 topology *requires* the sponsor to commit a fief.

**Consequences:** `causeway spawn connect --publish` is broken for any comet
that commits a fief, and Tier 1 declassification is unreachable for all of
them. This closes out "shipped but never broadcast": it could not have been.

**The fix** (three sites, same shape) — accept `OP_PUSHDATA2`:

```
len <= 75    -> [len]
len <= 255   -> 0x4c, len
otherwise    -> 0x4d, len as 2 bytes LITTLE-ENDIAN
```

and teach `+parse-publication` the `0x4d` case. This is consensus-relevant —
it changes bytes that enter `+read-publication` — so it wants review and a
shared vector in `vectors/`, not a hurried patch at the end of a test run. Not
attempted here.

*Classification: **real bug**, high severity, previously unfound. Found only
because these comets commit a fief at spawn.*

> **Fixed after this run.** `OP_PUSHDATA2` support landed in all three
> encoders (`+push-data:gw-btc-pass`, `causeway.push_data`,
> `publication.ts pushData`) and in `+parse-publication`, with a shared
> golden vector `pushdata2-fief` in `vectors/gw-kelvin-9.json` pinning a
> 269-byte fief-carrying publication byte-for-byte across Hoon, Python and
> TypeScript. The Hoon encoder now crashes `%exit` with a named reason on
> anything it cannot express instead of truncating a length byte. The
> **Tier-1 declassification path itself is still untested end to end** — the
> encoding blocker is gone, but nothing has been broadcast.

---

# Gap 2 — the residual single-flight wedge: **INDUCED, NOT TRIGGERED**

The residual window is the transport dying *after* a job starts and *before*
its fetches are served. Two methods, both instructive:

**Method 1 — kill the tcp-sidecar. Does not reproduce it.** `%bitcoin-client`
loses its peers, emits an `/is-synced` transition, and `%gw-btc`'s readiness
gate then *holds* the writ, so no job ever starts:

```
%gw-btc: light client is NOT synced; holding all attestations (no verdicts)
%gw-btc: writ from ~talryg-… held: light client NOT synced
```

Fail-closed and correct — and it is *why* the wedge is hard to construct. Note
this contradicts §9's "the sidecar died and the agent does not know": for the
`synced` computation the agent notices promptly.

**Method 2 — leave the sidecar alive and DROP outbound tcp/8333.** This does
strand a job. With the sidecar processes alive, the serf alive and **zero**
`newt: write failed`, `/x/inflight` held the peer continuously:

```
[26s]  inflight = (~talryg-…, 0)
[126s] inflight = (~talryg-…, 0)
```

So the wedge state was genuinely induced — no runtime death, no `%kick`.

**But `+lc-fetch-timeout` never fired, because the job did not need the
network.** At ~200 s the verification simply *completed*:

```
%gw-btc: attestation for ~talryg-… is VALID
```

**This is the finding.** A comet spawned ~5 blocks ago has a liveness-scan
range of ~5 blocks, and the light client already holds those filters and
headers locally — block headers are served from local state by design
(`lib/lc-attestation.hoon`, the `/block-header/height` path needs no peer). So
for a *recently active* comet the whole verification is servable offline, and
cutting the transport changes nothing. The previous campaign could not
construct this either; now there is a reason why.

To exercise `+lc-fetch-timeout` you must force a fetch the light client cannot
answer locally — verify a comet **dormant for a long span** (a large BIP-158
scan), or evict the filter cache first. That is a cheap follow-up and it is the
one thing here still pinned only by
`test-every-request-is-timeout-bounded`, which asserts merely that the timer is
*armed* on the strand's first move — never that it fires, never that the strand
fails, never that the slot is released. No test anywhere injects
`[%khan %arow %.n …]` on a `/verify/<ship>/<job>` wire, so
`app/gw-btc.hoon:895-899` remains untested.

Positive results from the attempt: `/x/inflight` is accurate and useful; the
`~h2` `+stuck-job-guard` never had to fire (`releasing stuck` count: **0**
across all three ships); and a re-poked writ was **held**, not
`dropped: a verification is already in flight`, so no slot was ever stranded.

*Classification: **not reproduced**, with a concrete reason and a concrete
recipe for next time. No defect observed.*

---

# Gap 3 — the missing campaign tooling: **DONE**

Recovered from droplet N1 (it was never lost, just unbacked-up) and committed
under `ops/` — see `ops/README.md`. `gwsup.sh`, `poolfill.py`, `addpeers.py`,
plus `gwctl.py` (replacing a dozen one-off scripts), `gwmint.py`,
`bootcomet.sh` and a new `stopship.sh`. `testnet/gwharness/`'s Python sources
had *already* been lost from this checkout — only `__pycache__` survived — and
were recovered in the same sweep.

Corrections made rather than carrying the tools forward as found:

- `gwsup.sh` is now an `flock` singleton (all three boxes were running two).
- `p4setup.py`'s `sync`/`info` and `peercount.py` were **dead code against a
  real node** — they scry `%bitcoin-client`, whose `++peek` is literally `~`.
  Not carried forward; `gwctl.py ready` reads `%gw-btc`'s `/x/ready` instead.
- `gwmint.py`'s `start_height` bug (below) fixed at source.

## Two bugs that had been fixed out-of-tree and came back

Both were previously corrected by throwaway scripts in an operator scratchpad
and never folded into the tool, so both **reproduced exactly**:

1. **`start_height` pointed at the spawn block, not the funding block.** The
   verifier fetches the block at `start_height` and looks for the funding
   transaction there, so it fetched a block the transaction is not in and the
   whole custody log failed to validate. The only symptom is one reasonless
   line: `%anew self-validation ended without a verdict`. Previously patched by
   a `fix_artifacts.py` that hardcoded three funding heights.
2. **`pass_with_xtr_hex` was not computed at all.** This is the ~305–405 B
   object a peer must be handed; the artifact carried only the bare 108 B
   `pass_atom_hex`, and a `%jael-writ` built from that is dropped silently.

Both now produced by `ops/gwmint.py artifact`, with `funding_height` recorded.

## An ad-hoc diagnostic scry can abort unrelated in-flight work

Worth stating on its own. A `.^` on jael's `/deed` that jael declines returns
`arvo: scry-lost` → `bail: 4` → `spider crashed, killing all strands`. That
killed the `%anew` job it was trying to measure, on two ships, and the only
trace was the same reasonless `%anew self-validation ended without a verdict`.
An operator debugging a stuck verification with an improvised scry can destroy
the thing they are measuring and mistake it for a product bug. It cost about an
hour here.


---

# Matrix versus the recorded Phase 6/7 results

| # | test | Phase 6/7 | this run |
|---|---|---|---|
| 0 | bring-up | done | **PASS**, on a pill built from the kernel tip |
| 1.1–1.4 | mint + confirm + `5120\|\|Q` | done | **PASS** ×3, Q recomputed from scratch |
| 2.1 | genuine attestation | PASS | **PASS** ×6 (all ordered pairs) |
| 3.4 | prove the Groundwire path was used | PASS | **PASS** — `dome=[~ %gw-btc]` ×6 |
| 4.7 | on-chain fief used to reach the holder | partial | **PASS** — `ames: lamp` ×6 from the committed fief |
| 5.6 | `%anew` refresh via Causeway → `%gw-btc` → jael | PASS | **PASS** ×3 (`custody log verified (1 entries)`) |
| 5.7 / 5b.2 | confidential → public | FAIL (pre-fix) | **BLOCKED** — new `PUSHDATA1` bug, see Gap 1 |
| 6.1 | attestation while light client unsynced | **FAIL** (false INVALID + snub) | **PASS** — held with a named reason, 0 snubs |
| 6.2 | kill the transport mid-verification | PARTIAL | **not reproduced**, see Gap 2 |
| 6.7 | diagnosable from logs alone? | "much better, still no" | **still no** — four `%jael-writ` drops silent, `%anew` failures reasonless |
| 7.1 | headless spawn | **FAIL** (100 % CPU, 57 MB) | **PASS** — bounded non-zero exits |
| 7.2 | Causeway pokes an attestation into `%gw-btc` | PASS | **PASS** ×3 |
| 7.3 | Causeway ↔ ship state sync | PASS | **PASS** — `verify_proof_self` + `_onchain` OK ×3 |
| — | `+groundwire!aqua-fixtures` | never executed | **PASS**, first execution, matches the kernel |

**Divergences from Phase 6/7:** 6.1 and 7.1 both moved FAIL → PASS, confirming
those fixes on fresh identities. 5.7 moved from "impossible by design" (the old
§10 claim) to "possible in principle, blocked by an encoder bug" — a different
and much more actionable answer. Everything else held.

**Not attempted** (time, and each needs its own on-chain spend): 2.2–2.16 (the
adversarial matrix — unchanged code, already pinned by unit tests), 3.1–3.6
(`|hi` and kernel gating over real packets), 4.1–4.8 (sponsorship — needs a
*public* sponsor, which needs Gap 1 fixed first), 5.1–5.5, 5b, 6.3–6.6, 7.4.

# Classification of every finding

**Real bugs (product):**
1. `PUSHDATA1`-only publication encoding in Causeway *and* Hoon, plus a parser
   that cannot read `PUSHDATA2`, against a spec that permits 512 bytes. Blocks
   all publication for any comet committing a fief. Hoon fails *silently*.
2. Four `%jael-writ` drops emit no log line while the code comment claims
   "EVERY drop below is announced".
3. `%anew` failures report no reason at all — one line, no tang.

**Harness / tooling (all fixed here):** duplicate supervisors; `gwmint`
`start_height`; missing `pass_with_xtr_hex`; `p4setup`/`peercount` scrying an
agent whose `++peek` is `~`; no shutdown procedure or script.

**Infrastructure:** none. Zero crashes, zero interventions, zero OOM across
~4 h on three 3.9 GB boxes.

**Documentation:** nine findings, all fixed in `cadc9ef`, `0962873`, `570342a`
and `f5baf3d`. The three that would cost a first-time operator the most are the
pill (§3.2 sends you to the wrong branch on CI for a job you can do locally in
six minutes), the filter-header troubleshooting entry (§9 fires on every
healthy run and its remedy is destructive), and the missing shutdown procedure
(§5.10 — which is why this run was handed three "stopped" ships that were
running).
