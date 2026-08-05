# Phase 4 — sponsorship (live mainnet), run 2

Date: 2026-08-05 (UTC). Operator: Claude (agent). Second attempt: run 1 completed
setup, then all three ships wedged and produced no matrix results.

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

<!-- WATCHDOG-COUNTS -->

---

# MATRIX

<!-- MATRIX -->
