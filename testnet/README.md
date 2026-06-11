# gwharness — local regtest testnet for Confidential Comets

A self-contained rig that boots **real** Groundwire comets against a **real**
regtest Bitcoin node and drives the full Confidential Comets 2.0 flow — mine an
identity, spawn it on-chain, boot its pier, install the `groundwire` desk,
verify its self-attestation keyfile, and adversarially try to break the
verifier. Nothing is faked or templated; every run mines real PoW, broadcasts
real transactions, and boots real piers.

> All mutable state lives under `run/` (gitignored). The node runs on an
> **isolated datadir + port 18549** — it never touches a user's own regtest
> node on 18443.

## Quick start

```sh
make m1            # single comet verifies its own keyfile end-to-end (VALID)
make scenario-all  # the adversarial suite (genuine keyfile accepted; tampered ones rejected)
make dashboard     # live view at http://127.0.0.1:42999
make tmux          # tmux session: dashboard + chain + per-pier slog tails
make results       # (re)generate run/results/RESULTS.md
make clean         # kill piers, stop bitcoind, wipe run/
```

`make m1` is the de-risking milestone; `make scenario-all` is the proof.
Each takes ~10–12 min (most of it a real PoW mine + a real pier boot that
loads a 143k-point azimuth snapshot).

## What each piece is

| module | role |
|---|---|
| `btc.py` | isolated regtest `bitcoind` (port 18549), wallet, maturity, funding, confirms, fork ops |
| `noun.py` | jam/cue/newt in pure Python (validated byte-for-byte against `urbit eval`) |
| `obphon.py` | `@p` codec — comets are **big-endian** (validated against `scot`/`slav %p`) |
| `connsock.py` | drives `<pier>/.urb/conn.sock`: `khan_eval`, `poke_our`, `ovum` (lane injection) |
| `chainops.py` | drives the desktop **Causeway** as a library: spawn + `%no-op` chained commits + keyfile skeleton |
| `ship.py` | comet lifecycle: mine → boot → desk install → watcher config → teardown |
| `packets.py` | skeleton → noun, POST to `%urb-watcher`'s eyre endpoint, tamper helpers |
| `scenarios.py` | the adversarial suite + log-parsed verdict assertions |
| `lanes.py` | `%dear` lane injection + `\|hi` reachability probe (two-comet contact) |
| `dashboard.py` | stdlib live web view (chain + per-pier verdicts + scenario matrix) |
| `report.py` | aggregates `run/results/RESULTS.md` (branch SHAs, M1, scenarios, chain) |

## Milestones

- **M1 — single comet** (`make m1`): cold-start the net, mine + spawn one comet,
  boot it, install the desk (this also compiles the CC 2.0 verifier), point its
  `%urb-watcher` at the node, POST the bare-spawn keyfile, read the verdict.
  Passes when the verdict is **VALID** with all 14 checks `ok`.

- **M3 — adversarial suite** (`make scenario-all`): one live comet, a genuine
  2-link keyfile (spawn → `%no-op`) plus seven tampered/oversized/misaddressed
  variants. Bar: only the genuine keyfile is **accepted**; every bad one is
  **rejected**, and where deterministic, for the right reason. See the table in
  `run/results/SCENARIOS.md`.

- **M2 — two-comet first contact** (`make m2`): boot two comets, have each
  verify the other's packet (so Jael → Ames installs the peer), inject lanes,
  and prove a live Ames round-trip with `|hi` both ways.

## Assertions

Gall scries over `conn.sock` return `~` on this fork, so the assertion channel
is the pier slog: `%urb-watcher` prints its `report:lsa` verdict
(`attestation for ~ship is VALID/INVALID` + a `[ok]/[XX]` line per check). The
scenario runner snapshots each pier log's size before a POST and parses only the
new region for the verdict — so repeated pokes on one comet stay disambiguated.

## Notes / gotchas

- conn paths must be **cwd-relative** (AF_UNIX 104-char limit) — run from
  `testnet/`. The Makefile and CLI do this.
- Don't `pkill -f urbit` — it hits any urbit process. The harness kills only the
  `urbit -c <run/piers/...>` it spawned (`ship.kill` is pid-scoped, SIGTERM then
  SIGKILL after a grace period).
- The branches under test are never pushed; see `run/results/RESULTS.md` for the
  exact SHAs.
