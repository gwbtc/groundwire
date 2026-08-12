# Freeze validation — attempted full-stack run (live mainnet)

2026-08-12, UTC. Operator: automated agent session.

| | |
|---|---|
| desk | `groundwire@hd/cc-landing` **`b10536e4`** (fetched; identical to `origin`) |
| kernel | `urbit@hd/cc-kernel` **`819e4c5f`** (fetched; identical to `origin`) |
| pill | `gw-cc-kernel-FREEZE.pill`, sha256 `b632e311…f8709` — **CI run 31635958376, `headSha=819e4c5f`**, the exact branch tip |
| vere | `gw-vere-freeze`, sha256 `2a90d0ca…1b2fa`, same CI run (`urbit 4.4-58b77ce`) |
| sidecar | `tcp-sidecar-freeze`, sha256 `9abc34ff…e78ed`, same CI run |
| miner | `comet_miner` macos-aarch64, same CI run |
| funding | `bc1ptv5pf6…az5slmrajl` = `m/86h/0h/0h/0/0`, 12,000 sats, one UTXO `2f724c4a…87c3:1` @962197 |

## Lead: what this run did and did not establish

**Three fresh comets did NOT mint on chain, and therefore did not mutually
verify.** Three identities were mined on current code and three ships were
booted from the freeze pill at the correct `@p`, but the funding-split
transaction that precedes the three spawns **had not confirmed when the run
ended** (2.01 sat/vB, broadcast 22:0x UTC, still in mempool across blocks
962199–962200). No spawn transaction was ever broadcast.

**The late reveal did not happen.** It is downstream of a confirmed spawn plus
a confirmed sat move; neither exists.

**Nothing regressed that this run could observe** — but the observation
surface was small, because the desk never finished installing on any ship (see
Finding 1) and the light clients therefore never synced. The nine
never-live-tested changes remain never-live-tested.

What *was* established, and is worth keeping:

1. **The pill is provably the branch tip.** Not by marker grep — by hashing
   the pill's *own* kernel sources after boot and comparing to the checkout.
   All three match byte-for-byte (below). This is the strongest form of the
   "verify the fixes are in the built pill" gate and it passed cleanly.
2. **All three landmines were real, and all three are cleared**, with the
   flock fix verified against live supervisors rather than assumed.
3. **The pre-broadcast gate earned its keep**: it caught a byte-order bug in
   the funding-split transaction that would have broadcast a tx spending a
   non-existent outpoint.
4. **Five tooling defects** were found, two of which are why this run stopped.

## Pill verification — PASS (the one unambiguous success)

A fake `~zod` was booted from the pill **without `-A`**, so `%base` came from
the pill itself and nothing local could contaminate it. Its own kernel sources
were then scried out and hashed:

```
> (met 3 amesrc)                     562.912          ← pkg/arvo/sys/vane/ames.hoon = 562912
> [(met 3 jaelsrc) (met 3 lullsrc)]  [63.908 168.831] ← jael.hoon = 63908, lull.hoon = 168831
> (shax amesrc)
  0xe27940bf67b597f8011a88c46ee035d8228734abbb01f5758d8e74ad46bdbd4b
> [(shax jaelsrc) (shax lullsrc)]
  0x49dfc4c7494d0b3997ee2fb5fce54e6459359826311a506ac7abe692a072a55c
  0xe4f0068864fb0e0bb1f4c36967b4bec7827cacb64a95236033c209c61724386e
```

Every value equals the sha256 of the corresponding file at `819e4c5f`
(byte-reversed, which is Hoon's atom order). So the `%verdict` rename, the
`%page` snub gate, `+fief-route`, `+open-jam-shaped`, the `%stale` writ-result
and the 3-field `%snub` are all in the shipped pill **by construction**, not by
inference. The pill also boots and installs `%bitcoin-client` and `%tcp`, so
`%node` and `%tcp-sidecar` really are baked in and start.

This does not prove any of those code paths *runs correctly* on a live ship.
It proves only that the artifact contains the source under test.

## Landmines — all three cleared

**1. `gwsup.sh` flock leak — confirmed live on all three, fixed, and the fix
verified.** Before: fd 9 on `/opt/gw/.gwsup-kN.lock` was held by the
supervisor, an orphaned second `gwsup.sh`, **both `gw-vere` processes**, and
the `tcp-sidecar` — exactly the described leak. The droplets' `gwsup.sh`
(md5 `27f28055`) lacked the `exec 9>&-` at line 105; the repo's (md5
`600ec401`) has it at line 114. Repo version deployed to all three, old kept as
`gwsup.sh.pre-freeze`. Ships stopped with the repo `stopship.sh`, residual
holders reaped, and `flock -n` then reported `LOCK IS FREE` on every box.

After the new supervisors were started on the new ships, fd 9 is held only by
the real supervisor and its transient `sleep 30` poll child — **the sidecar and
the sidecar subshell no longer hold it**. The fix works.

**2. `/opt/gw/stubrpc.py` recovered.** Byte-identical on all three (md5
`67d93f84fd391b3de1bafcba71c13ab3`, 40 lines, 1530 bytes), running
`python3 /opt/gw/stubrpc.py 961058` with ~8 days uptime and in no repository.
Recovered verbatim to `ops/stubrpc.py` in this commit. Note its pinned height
961058 is now **1,142 blocks stale** against tip 962200; it is a silence stub
for the legacy RPC scanner, not a data source, but the pin should be an
argument the boot path sets rather than a number frozen in a file nobody had.

**3. k1's `gw-tcp-sidecar.service` disabled.** It was `enabled` + `activating`,
pointed at `WorkingDirectory=/opt/gw/piers/smoke` — the wrong pier — and had
**`NRestarts=85771`**, failing with `status=1/FAILURE` every 5 seconds. Stopped
and disabled; N2/N3 never had the unit.

## Identities minted (off chain only)

All three mined on the CI `comet_miner` from `819e4c5f`, each against its own
intended funding outpoint, each verified suite-C under `~daplyd` with the mined
ring's `dat` equal to the independently computed `dat`.

| | comet | box | pier | port | funding outpoint (intended) |
|---|---|---|---|---|---|
| f1 | `~napdes-lorhet-wistus-mitled--fontux-balwer-ridrel-daplyd` | N1 `64.227.13.22` | `/opt/gw/piers/f1` | 35363 | `fcb70af0…614f:0` |
| f2 | `~noplup-rilsul-lapmug-pagrum--forleg-sansyl-mirned-daplyd` | N2 `159.223.141.63` | `/opt/gw/piers/f2` | 35364 | `fcb70af0…614f:1` |
| f3 | `~narmul-halmec-mallux-sablen--mittep-larbep-solsed-daplyd` | N3 `206.189.188.16` | `/opt/gw/piers/f3` | 35365 | `fcb70af0…614f:2` |

Artifacts are in the session scratchpad as `state-f{1,2,3}.json`. They were
**not** written to `.gw-comet-f{1,2,3}.json` because those files are specified
to hold a *spawned* comet's artifact and no spawn exists; writing them would
assert an on-chain identity that is not on chain. `.gw-comet-{1,2,3}.json` and
`.gw-comet-r{1,2,3}.json` were not touched.

All three ships booted from the freeze pill and report the right identity:

```
~napdes-lorhet-wistus-mitled--fontux-balwer-ridrel-daplyd  life=1 rift=0
~noplup-rilsul-lapmug-pagrum--forleg-sansyl-mirned-daplyd  life=1 rift=0
~narmul-halmec-mallux-sablen--mittep-larbep-solsed-daplyd  life=1 rift=0
```

Booted with `-w <name> -G <feed>` together, from the raw miner feed, so each
carries a 108-byte pass with no `xtr` — deliberately, because the custody entry
was to be poked in later as tests 5.6/7.2. That poke never happened.

## The intended design, recorded so it is not re-derived

The three priority tests drove the topology, and the reasoning is worth keeping
even though it went unexecuted:

- **`sponsor-known` is in the unevaluable class** (`lib/self-attestation.hoon:161`,
  `:1180-1183`): a named sponsor must be in `known-public`, and failure yields
  *no verdict*, not a negative one. So "conventionally sponsored" cannot mean a
  real Azimuth star — naming `~daplyd` would leave the sponsee permanently
  UNDETERMINED. It has to mean **a Groundwire comet that is itself public**.
- Hence: **f1 public at spawn** (so it is a `known-public` point and can be a
  sponsor), **f2 confidential naming f1 as sponsor plus a fief** — that is the
  `+fief-route` case, because f2's sponsor is a different ship whose relay is
  the fallback the fief is supposed to replace — and **f3 confidential** as the
  late-reveal subject and the peer that never tracked f2.
- An absent sponsor projects to *self* (`:672-689`), which is why the prior
  self-sponsoring comets proved nothing about `+fief-route`.

**PUSHDATA2 is fixed**, contrary to the cleanroom finding: `lib/gw-btc-pass.hoon`
emits `0x4d` with a two-byte LE length above 255 (`:262`) and parses it
(`:336-337`), `lib/btc-script.hoon` likewise, and the cap is now 1,024 bytes.
A published spawn carrying a fief encoded correctly at 269 payload bytes.

## Findings

### Real bugs (product / tooling), in the order they cost time

**1. `gwctl.py desks` destroys the ship it is verifying — this is what stopped
the run.** Its commit-verification step scries `%cx …/groundwire/app/gw-btc/hoon`
to compare the committed byte length against disk. If the commit has not landed,
that file does not exist, and the scry does not return `~` — it takes the vane
down:

```
bail: 4
bail: 2
spider crashed, killing all strands: %arvo-response
! / cx ~napdes-… groundwire ~2026.08.12..22.58.46..92c2 app gw-btc hoon
```

The loop then retries, crashing spider again, and logs `clay=None disk=118139`
**59 times** without ever succeeding. Because it kills *all* strands, it also
kills every other khan thread on the ship — which is why the supervisor's peer
work started failing at the same moment:

```
GoofError: thread bailed: %thread-fail:
[f1]   kill-peer-connections FAILED (conn.sock unresponsive)
[f1]   re-seed FAILED (1.160.182.9)
```

So one unguarded probe took out the desk install *and* the light-client peer
seeding on all three ships. This is the same class as the runbook's own warning
that an ad-hoc diagnostic scry killed the `%anew` job it was measuring — but
here it is in the committed operator tool, on the default path, and it is not a
diagnostic, it is the success check. The scry needs a `mule`/`mole` guard.

**2. Two desk test files do not build, and they crash the whole test thread, so
`gwtest.py` cannot produce a tally at all.** Against the freeze pill:

```
clay: no files match /sur/urb-watcher/hoon
FAILED  /tests/app/urb-watcher/hoon (build)
…
-need  ?([%escape parent=@p sig=u(@)] …)
-have  [%escape @p]
nest-fail
FAILED  /tests/lib/urb-core/hoon (build)
thread failed: %crash
```

`tests/app/urb-watcher.hoon` imports `/sur/urb-watcher`, which does not exist in
the desk (`sur/` has no `urb-watcher.hoon`). `tests/lib/urb-core.hoon` fails a
nest against `$effect:urb`, whose `%escape` gained a `sig=(unit @)` third field
(`sur/urb.hoon:57`) that the constructed value does not have. `urb-core.hoon`
was last touched by `8055221` — the `$point.seen` / `+anchor-ok` lag commit — so
this is plausibly fallout from that change rather than ancient rot. Either way
it is exactly the failure `gwtest.py` was written to stop hiding: **a whole file
of assertions vanishes and the surviving lines still all say OK**. The desk test
suite is currently untallyable.

**3. `gwmint.py cmd_build`'s gate fails every published spawn on a hardcoded
sat cap.** The gate asserts `fee <= 500 sats` immediately after asserting
`effective fee rate >= 1.0`. A published spawn is ~401 vB, so at the policy
minimum of 2 sat/vB the fee is 802 sats and the gate prints
`*** GATE FAILED — DO NOT BROADCAST ***` on a transaction all of whose
substantive checks passed:

```
[PASS] input 0 outpoint is the intended funding UTXO
[PASS] Q: causeway == from-scratch secp256k1
[PASS] output 0 scriptPubKey == 5120||Q
[PASS] output 0 value >= 330 (P2TR dust) — 11198 sats
[FAIL] fee <= 500 sats — fee = 802 sats
[PASS] effective fee rate >= 1.0 sat/vB — 802/401 = 2.000 sat/vB
```

An absolute-sat cap and a rate check cannot both be the policy. The cap blocks
the publication path this release exists to ship.

**4. `gwmint.py cmd_build` never wires change, so one UTXO cannot fund a
campaign.** `build_spawn_psbt` takes `change_internal_xonly` /
`change_script_pubkey` / `change_path` (`causeway.py:1739-1741`) and Causeway's
own flow passes them (`:3134-3137`), but `cmd_build` omits them entirely — so a
spawn sweeps the *whole* funding UTXO into the identity sat. With a single
12,000-sat UTXO the first spawn would have created one comet holding 11,198
sats and left nothing for the other two. The brief's premise that
"`build_spawn_psbt` chains through change, so spend it serially" is true of the
function and false of the tool that calls it. Worked around with a separate
4-output split transaction.

**5. `ops/bootcomet.sh` cannot boot any pill but one.** Both the path
(`gw-cc-kernel-solid-TIP.pill`) and its sha256 are hardcoded, with a `REFUSING:`
guard on the hash. Booting the freeze pill required a parallel script. The hash
guard is good; the hardcoded target should be an argument.

### Harness / operational

- `ssh host '<cmd> &'` still hangs the channel even with the documented
  `setsid nohup … </dev/null` form when the command also writes to stdout;
  add `ssh -n` and redirect *all* streams. Two multi-minute stalls.
- The dojo input buffer survives an aborted multi-line expression; `C-c` and
  `C-u` do not clear it, and the next `tmux send-keys` is appended to the
  residue, producing a nonsense command. Reboot the fakeship instead.
- `gwtest.py commit` reports `committed %groundwire` even when the transcript
  carries `%no-cast-between %mime %md` from `doc/*.md` — the desk ships `doc/`
  but no `%md` mark, which is the documented permanent-clay-crash hazard.

### Infrastructure

Clean. Three droplets, ~8 days uptime, no OOM, no vere or sidecar SIGSEGV
observed in this run. Disk 84–91 GB free per box, RAM ~3.4 GB available after
the k-ships were stopped. Peer pools built fine (913 / 638 / 905
`NODE_COMPACT_FILTERS` peers from `x49.`-filtered seeds).

### Chain

The funding split `fcb70af0…614f` (1 in, 4 out, 482 sats fee, 2.01 sat/vB,
`testmempoolaccept` allowed, broadcast successfully) did not confirm within the
run. Fees never exceeded the 5 sat/vB stop threshold — `fastestFee` was 1–2
throughout — but the next projected block's *median* was 2.58, so 2.01 sat/vB
sat just below the cut. **Cost so far: 482 sats.** The remaining 11,518 sats are
in four outputs at the funding address once it confirms; nothing is lost.

Pre-broadcast gate output, which is the reason this is a fee story and not a
loss story:

```
[PASS] input 0 is the intended funding outpoint
[PASS] exactly 1 input, 4 outputs, no strays
[PASS] every output pays our own funding address
[PASS] all outputs >= 330 dust
[PASS] fee rate 2.01 <= 5 sat/vB
[PASS] node accepts to mempool
GATE PASSED
```

An earlier build of the same transaction **failed** that gate with
`reject-reason: missing-inputs` because the input txid had been byte-reversed
(`bytes.fromhex(txid)[::-1]`; Causeway uses `bytes.fromhex(txid)` unreversed,
`causeway.py:1811`). Had the gate not run a real `decoderawtransaction` +
`testmempoolaccept`, that transaction would have been broadcast and rejected.

## Matrix

Every phase 0–7 test is **NOT RUN** unless listed below. This is not a matrix
with gaps; it is a matrix that was not reached, because the desk never installed
and the light clients never synced.

| # | test | result |
|---|---|---|
| 0 | build a solid pill from kernel HEAD | **PASS** — CI run 31635958376 at `headSha=819e4c5f` |
| 0 | verify the fixes are in the pill, not just the source | **PASS** — pill's own `ames`/`jael`/`lull` sha256 identical to the checkout |
| 0 | pill boots; `%node` + `%tcp-sidecar` baked | **PASS** — fake `~zod` boots, installs `%bitcoin-client` and `%tcp` |
| 0 | deploy pill + vere + sidecar to N1/N2/N3 | **PASS** — byte-identical sha256 on all three |
| 0 | stop k1/k2/k3, preserve piers and feeds | **PASS** — clean stop, feeds md5-identical before and after |
| 0 | landmine 1 — flock leak | **PASS** — fixed and re-verified against live supervisors |
| 0 | landmine 2 — `stubrpc.py` | **PASS** — recovered into `ops/` |
| 0 | landmine 3 — k1 sidecar unit | **PASS** — disabled after 85,771 restarts |
| 0 | light-client sync | **FAIL** — peer seeding killed by Finding 1 |
| 0 | desk install on the three ships | **FAIL** — Finding 1 |
| — | desk test suite tally | **BLOCKED** — Finding 2; no tally is producible |
| 1.1 | mine three suite-C comets under the kelvin-9 hiding `dat` | **PASS** — 3/3, ring `dat` == computed `dat`, star `~daplyd` |
| 1.2 | confidential spawns | **NOT RUN** — blocked on the split confirming |
| 1.3 | public spawn with OP_RETURN publication | **NOT RUN** — built and decoded, never broadcast (Finding 3 blocked the gate) |
| 1.4 | output 0 == `5120‖Q` with `Q` independently recomputed | **PASS (pre-broadcast)** — verified on the built f1 spawn against a from-scratch secp256k1 |
| 1.5 | public scanner indexes from OP_RETURN alone | **NOT RUN** |
| 2.1–2.16 | verification correctness | **NOT RUN** |
| 3.1–3.6 | kernel gating and real networking | **NOT RUN** |
| 4.1–4.8 | sponsorship, incl. 4.7 fief routing | **NOT RUN** |
| 5.1–5.7, 5b.1–5b.3 | state changes, re-attestation, self-rescue | **NOT RUN** |
| 6.1–6.7 | resilience | **NOT RUN** |
| 7.1–7.4 | Causeway | **NOT RUN** |
| — | **late reveal on chain** | **NOT RUN** — the headline capability is still unproven on chain |
| — | **fief routing for a conventionally sponsored comet** | **NOT RUN** — design settled (above), never executed |
| — | **`%verdict` round trip across the kernel/desk boundary** | **NOT RUN** — present in the pill by hash, never exercised |

### Divergences from prior runs

Only one behavioural divergence is claimable, and it is a fix confirming: the
`gwsup.sh` flock leak that the cleanroom run diagnosed is now demonstrably
absent on live supervisors. Everything else that would constitute a divergence
sits behind the desk install.

The PUSHDATA1 ship-blocker recorded in `CLEANROOM-RESULTS.md` ("publication is
impossible for any comet with a fief") is **fixed in source** — `OP_PUSHDATA2`
is emitted and parsed on both sides and the cap is 1,024 — and a published spawn
with a fief encoded to a well-formed 269-byte payload here. It has still never
been broadcast.

## What this run cannot prove, stated so no reader infers otherwise

These were out of scope by construction, independent of the blockers:

- **Reorg repair cannot be staged on mainnet.** The reorg integration is in the
  pill and in the desk; nothing here exercised it, and nothing here could.
- **The installer cannot be tested against a real release until urbit#67
  merges.**
- **The web SPA has never broadcast a transaction**, and did not here.
- **The `%mesa` path is opt-in** and its Aqua suite cannot complete.
- **The `$gw-state` migration is deliberately skipped** — fresh comets boot
  straight into current state, so no migration path was exercised and none was
  intended to be.

Additionally, and specific to this run: the pill hash match proves *containment*,
not *behaviour*. None of the nine never-live-tested changes — the publication
rework, the `%verdict` rename on either side, the reorg integration,
`$point.seen`, the `+anchor-ok` lag change, the snapshot-key fix, the `%page`
snub gate, fief routing — has now run on a live ship. Only the pre-auth DoS fix
remains previously deployed.

## State left behind

- k1/k2/k3 stopped cleanly; piers and `/opt/gw/feed-k{1,2,3}.txt` preserved and
  checksum-verified. C1/C2/C3 untouched.
- f1/f2/f3 booted, correct `@p`, desks left **mounted** with a crashed spider —
  they should be stopped with `ops/stopship.sh` before reuse, and the mounted
  desk is the documented clay hazard.
- Freeze artifacts on all three droplets:
  `/opt/gw/pills/gw-cc-kernel-FREEZE.pill`, `/opt/gw/bin/gw-vere-freeze`,
  `/opt/gw/bin/tcp-sidecar-freeze`, `/opt/gw/ops/bootfreeze.sh`,
  `/opt/gw/desks/gw-freeze`.
- Old supervisors kept as `/opt/gw/ops/gwsup.sh.pre-freeze`.

## What to fix, in order

1. **Guard the scry in `gwctl.py desks`** (Finding 1). Nothing else can be
   tested until an operator can install a desk without killing the ship.
2. **Fix or quarantine `tests/lib/urb-core.hoon` and
   `tests/app/urb-watcher.hoon`** (Finding 2) so `gwtest.py` can tally again.
3. **Replace the `fee <= 500` cap with a rate-and-vsize bound** (Finding 3).
4. **Wire change through `cmd_build`** (Finding 4), or document the split step.
5. **Parameterise `bootcomet.sh`** (Finding 5) and fold `stubrpc.py`'s height
   into its caller.
6. Re-run this campaign from the top. The split UTXOs will still be there.
