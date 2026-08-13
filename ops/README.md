# `ops/` — the Groundwire campaign tooling

Everything here was previously described in prose in `groundwire/doc/OPERATIONS.md`
and in the `PHASE*-RESULTS.md` docs, but existed only on the droplets at
`/opt/gw/` and in operator scratchpads. `OPERATIONS.md` §11 said so plainly:

> **No supervisor in this repo.** `gwsup.sh` is specified in §5.9 and must be
> written. **No peer-pool tool in this repo.** `poolfill.py` is specified in §5.6.

That was true and it was a liability: the tools were recovered from a single
droplet during the cleanroom run, and `testnet/gwharness/`'s Python sources had
already been lost locally (only `__pycache__` survived). They are checked in now.

**These run ON a droplet**, not on your laptop — every one of them talks to a
pier's Lick socket at `<pier>/.urb/conn.sock`, which is a unix socket. They
expect `gwharness` importable from `/opt/gw` (`testnet/gwharness/`).
`gwtest.py` is the one exception: it drives a throwaway fakeship through tmux
and needs nothing but vere, a kernel checkout and a solid pill, so it runs on
a laptop.

| tool | what it does |
|---|---|
| `gwtest.py` | boot a fakeship, commit a desk into it, `-test` it, and tally the result honestly |
| `gwctl.py` | the operator surface: identity, desks, readiness, peers, verification pokes |
| `gwmint.py` | mint a comet on mainnet, with an independent pre-broadcast verification gate |
| `bootcomet.sh` | full bringup of a minted comet: boot, install `%groundwire`, supervise, seed peers, report readiness |
| `pcap.sh` | start a packet capture — run at BOTH ends of a networking test |
| `pcapsum.sh` | summarise a capture BY DIRECTION ("N out, M back") |
| `pushfile.sh` | copy one file to a droplet and prove it arrived — sha256 + byte count |
| `stopship.sh` | stop a *supervised* ship — supervisor first, then runtime, then sidecar |
| `gwsup.sh` | per-ship supervisor: restarts vere and the sidecar, unwedges the light client |
| `poolfill.py` | refill the peer pool from `x49.`-filtered DNS seeds |
| `addpeers.py` | `%bitcoin-client-connect-peer` a batch of IPs in one strand |
| `killpeers.py` | disconnect every earth peer — the unwedge step, one strand |
| `gwvec.py` | build the Phase-2 adversarial attestation vectors from a comet artifact |
| `gwsnub.py` | read and clear a ship's ames blocklist — one of the two ways to undo a snub |

## The things that cost hours

**Ships run with `-t`, so there is no dojo.** `OPERATIONS.md` §5.3 tells you to
run inside tmux without `-t` and drive the dojo with `send-keys`. Every ship in
every live campaign has actually run with `-t` under a supervisor, driven over
`conn.sock`. The runbook's `> :gw-btc &noun [...]` lines are *notation*: a scry
written there as `.^(* %gx /...)` has to be wrapped in a `(strand ,vase)` thread
and handed to khan. `gwctl.py` is that wrapping.

**`%bitcoin-client` COULD not be scried, and now can.** Its `++peek` was
literally `~` for every path, which is why the recovered `p4setup.py
sync`/`info` and `peercount.py` were dead code against a real node and are not
carried forward. At `node@063720b9` it serves `/x/network`, `/x/is-synced`,
`/x/best-block`, `/x/peers` and the block-header paths — `killpeers.py` depends
on `/x/peers` being real. Readiness is still best read from `%gw-btc`'s
`/x/ready` (`gwctl.py ready`), because that is the verifier's own view and it
is what gates verdicts; `gwctl.py peers` pokes `&log-info` for detail.

**Liveness is `<pier>/.urb/log/*/data.mdb`, never `<pier>/.urb/log`.** The
directory is a dirent; LMDB writes into an already-created file. Measured 21 h
stale on a ship demonstrably processing events. Using the directory declares
every healthy ship wedged from the moment it boots.

**Match processes exactly.** `p4c1` prefix-matches `p4c1b`. Never
`pkill -f urbit`. `gwsup.sh` matches the king by exact final argv, the serf by
the field after `--snap-dir`, and the sidecar by `/proc/<pid>/cwd` — it has no
port and no distinctive argv.

Two traps in that, both sprung live. **`stopship.sh` silently stops nothing for
a ship booted in CREATE form**: it matches the king by `$NF==pier`, but
`-c PIER … -B PILL` ends its argv with the *pill*. And **`pgrep -f` on a remote
host matches the ssh wrapper's own command line**, which contains the pattern as
literal text — one such kill took out its own shell before the next command,
with no output and no error, looking exactly like a clean no-op. Kill by
explicit PID and confirm with `ps -p`.

**`gwsup.sh` is now a singleton.** It takes an flock. Before that guard, all
three droplets were found running *two* supervisors per pier; both saw
VERE-DOWN, both relaunched, and the loser died on
`mesa: bind: address already in use`. It is also why ships that had been
stopped were found running again — nothing stops a supervised ship except
`stopship.sh`, and `OPERATIONS.md` documents no shutdown procedure at all.

**Seed peers ~25 at a time.** Bulk peer-adds reliably SIGSEGV the
tcp-sidecar (`gwbtc/node#1`). Filter headers are only servable by peers
advertising `NODE_COMPACT_FILTERS`, hence the `x49.` DNS prefix in
`poolfill.py`; seeding from unfiltered seeds leaves filter sync at height 1
forever and killed an entire test run.

**node renamed its whole interface at `063720b9`.** That is the ref CI pins,
and the one we need for `.stale-branch`. Every mark moved under
`mar/bitcoin-client/` and was renamed with it: `%add-earth-peer` became
`%bitcoin-client-connect-peer`, `kill-peer-connections` ceased to exist in
favour of a per-address `%bitcoin-client-disconnect-peer`, and every *fact*
mark the desk subscribes to gained the same prefix. Only `%log-info` and
`%broadcast-transaction` kept their bare names.

Nothing catches this class of break. Both desks compile independently and
each is internally consistent, so CI is green and a code review has nothing
to see — it shows up only on a running ship. It cost two validation runs:
the first crash-looped 136,539 times on the fact marks, and the second came
up with a healthy-looking agent, zero peers, and a cursor that never moved,
because peer seeding was still poking a mark that no longer existed. A ship
with no peers receives no facts, so "no unexpected subscription update" was
vacuously true. **When bumping `NODE_REF`, diff `desk/mar/` and the agent's
poke arms, not just `sur/`.**

**A positive verdict lifts a snub — but no transport can deliver one.** The kernel's
`f68a547b2b` made `+sy-sybl`'s `%full` branch the exact inverse of its `%fail`
branch — it calls `(sy-snub %deny %del ~[her])` — so a later `%full` really does
un-snub a ship. That is a **safety net, not self-healing**, because a snubbed
ship cannot deliver the attestation that would earn the `%full`: classic ames
`+pe-hear` tests `ships.snub` the moment it has a `$shot`, before it classifies
the packet at all, and since `b0a8e962ff` mesa's `+pe-heer` gates its `%page`
branch on `her.name` as well. That branch was the last transport a snubbed comet
could re-attest over, and it is closed. So the only `%full` that reaches a
snubbed ship is one an **operator asked for**, by re-poking a `%jael-writ`
(`gwctl.py writ`) — jael forwards a `%writ` to the domain agent unconditionally,
so an already-snubbed ship is verified again from scratch and the snub is lifted
as a *consequence* of the verdict.

Two undos, and they are not equivalent. Prefer `gwctl.py writ` whenever the
attestation is expected to pass now — the usual case, where the snub was our own
ignorance and the verifier has since caught up — because it leaves the ship
un-snubbed **and** verified, with the reason on the record. Reach for
`gwsnub.py del` when you need packets flowing regardless of what the chain says:
it sends `%snub %deny %del` straight at ames, edits the blocklist and nothing
else, and leaves jael's opinion of the ship exactly as it was.

**Snubs never expire**, and that is a decision rather than an omission: a
per-ship expiry timer is durable state an attacker can make us allocate. Nothing
decays on its own — a wrong snub is undone by one of the two paths above, or not
at all.

## Running the desk's tests

```sh
gwtest.py boot   /tmp/zod --arvo ~/urbit/pkg/arvo --pill ~/urbit/bin/solid.pill \
                          --urbit ~/vere/zig-out/*/urbit
make build                                     # -> dist-groundwire/
gwtest.py commit /tmp/zod groundwire dist-groundwire
gwtest.py run    /tmp/zod --desk groundwire    # exits non-zero on any bad result
gwtest.py tally  /tmp/zod.transcript           # re-tally a saved transcript
```

There was no test tooling here until 2026-08-10, and the cost of that was
measured: every agent wrote its own `grep | wc -l` over the `-test` output and
they all made the same mistake, because `ted/test.hoon` prints **four**
outcomes and only one of them is `OK`:

    OK      /tests/lib/foo/test-bar     the arm ran and asserted true
    FAILED  /tests/lib/foo/test-bar     the arm ran and asserted false
    CRASHED /tests/lib/foo/test-bar     the arm never ran
    FAILED  /tests/lib/foo (build)      no arm in this FILE ran

A tally that counts only `OK` lines reports a crashing suite as perfectly
green — and did, as `164 OK / 0 fail`, for a week, while
`test-real-mainnet-publication-indexes-c3` was crashing against behaviour
`4ae85b8` had removed. A crash is strictly worse than a failure (the arm never
ran, so its assertions are untested, not merely false), and a build failure is
worse still — the file's arms vanish from the denominator, so the transcript
looks *healthier* the more of it is missing. `gwtest.py` counts the four
apart, names every non-OK arm, cross-checks its count against the `ok=%.y` the
thread itself returns, and refuses to call an empty transcript a pass.

Two traps `boot` exists to dodge, both of which cost hours before:

- **`pkg/arvo` is a tree of symlinks into `pkg/base-dev`.** Copy it without
  dereferencing and you get a ship that boots fine and then silently commits
  nothing to `%base`. `boot` copies with `-L`.
- **You cannot `urbit … | tee log`.** A pipe is not a tty, so vere refuses
  outright; `-t` fixes that and costs you the dojo, which is the only way in
  without a conn.sock client. `boot` runs vere under tmux and captures with
  `tmux pipe-pane`. Note `pipe-pane` **toggles** — running it a second time
  turns the capture off, and a detached capture reads as a clean run to
  anything that only counts `OK` lines. `run` fails loudly if the transcript
  stops growing.

## Minting

`gwmint.py` exists because `causeway spawn` cannot do two things the campaign
needs:

1. **a wallet-seed-derived blind.** causeway's CLI uses `secrets.token_bytes`,
   which is not recoverable from the wallet. `gwmint.py` derives
   `blind_seed = sha256(bip39_seed || "gw/spawn-blind-seed" || txid_be32 || vout_le4)`,
   so phrase + satpoint alone rebuild the identity.
2. **a pre-broadcast verification gate.** causeway broadcasts the moment it has
   a signed PSBT. `gwmint.py` splits `build` from `broadcast` and refuses to
   broadcast unless `build` set `gate_passed`. The gate recomputes `Q` from
   `[internal-key, snapshot]` with its own secp256k1 point arithmetic and
   compares against causeway's, so a bug in causeway's encoder cannot pass.

It also sets a `fief` and a `sponsor` at spawn; the causeway CLI has no
`--fief` anywhere. (It *can* publish late, via `causeway publish` — that
half of the gap closed.)

**gwmint's paths are absolute and point at one machine.** `gwmint.py:730`
writes `/Users/trent/gw-building/.gw-comet-<n>.json`, and `sys.path` is
pinned the same way. This file says these tools run ON A DROPLET, where
that path does not exist — so gwmint is currently laptop-only, whatever the
table above implies. Fix the paths or run it where they resolve; do not
assume `~/` expands to the same place.

```sh
gwmint.py mine  <label> <funding-txid> <vout>
gwmint.py build <label> [--fief=IP:PORT] [--sponsor=~patp] [--publish] [--fee-rate=N] [--sweep]
gwmint.py broadcast <label>          # refuses unless the gate passed
gwmint.py status    <label>          # poll for confirmation
gwmint.py artifact  <label> <n>      # -> /Users/trent/gw-building/.gw-comet-<n>.json (0600)
gwmint.py publish <label> <n> [--fee-rate=N] [--fund] [--sat-target=S]
```

`publish` is the LATE publication — Tier-1 declassification for a comet that
is already confidential. It was the only builder of one until `causeway
publish` landed; the two now agree byte for byte, because both call the same
`causeway` encoders and `gwmint` imports them verbatim. Prefer `causeway
publish` unless you need gwmint's ops wallet: it takes the whole ordered proof
chain rather than one artifact, so it can refuse a stale log, and it re-reads
the payload out of the script both before signing and again before broadcast.
Since 2026-08-10 the OP_RETURN payload is the comet's whole attestation
packet, so what goes in it is the pass a **peer** receives: the custody log in
its `xtr` (`pass_with_xtr`, from the artifact's baked `xtr_hex`), and an
opening with **no** blind-opening, because the dat opening may sit only on
entry 0 and entry 0 is inside that log. Publish the 108-byte boot pass
instead and the watcher completes a one-entry log whose single entry is this
transaction — the degenerate *spawn* shape — so `+run-checks` demands that
input 0 spend the spawn satpoint, which a state update never does. The build
refuses if the artifact's log does not end at the outpoint input 0 spends.

That is exactly how the one Tier-1 declassification already on chain was
built. k1's `08957455…` (block 961 353) verified against the code of the day
and **does not verify against the current verifier** — `entry-0-continuity`,
because the empty `xtr` makes the completed log claim input 0 spends the spawn
satpoint. It is historical evidence, not a live publication, and it is not
being rebuilt: k1 holds 754 sats and a packet publication costs ~1,000. The
other three publications on mainnet fail too; see `OPERATIONS.md` §10 for the
table.

> **A comet naming another comet as its sponsor is only as verifiable as the
> sponsor is *known*.** `sponsor-ok` (`lib/self-attestation.hoon`) is
> `(~(has in known-public) u.sponsor...)`, and `known-public` is whatever
> `+verify-cards` hands the thread — which is `~(key by unv-ids.urb-state)`,
> **raw**. A verifier that has never seen the sponsor fails the check, and
> since 2026-08-06 that is classified *unevaluable*, so the sponsee reads
> UNDETERMINED, no verdict is emitted and no point is installed. It is not
> permanent: the same packet verifies VALID the moment the verifier learns the
> sponsor (measured live, 2026-08-07).
>
> This paragraph used to say the sponsor had to be **public**. Measured live,
> it does not: `unv-ids` also holds comets this verifier has verified
> **confidentially**, and one of those satisfies `sponsor-known` — even though
> the agent's own `+known-public` predicate, used two arms away for the writ
> gate, explicitly subtracts `.confidential`. Two definitions, one agent. See
> `doc/live-tests/PHASE2-RERUN-RESULTS.md`, Finding 2. An absent sponsor is
> fine either way: it projects to self and `sponsor-ok` is `%.y`.

## Testing verification, without spending anything

`gwvec.py` builds the Phase-2 adversarial matrix
(`doc/opret-revision/05-live-test-plan.md`, tests 2.2–2.16) out of a comet's
**own on-chain custody log**: truncate it and the tip is a satpoint the next
entry already spent; flip a bit in the blind and `spawn-commit` cannot open
`dat`; append a real foreign transaction and `derive-tip` breaks. No
transaction is built and nothing is broadcast.

```sh
gwvec.py show  ~/gw-building/.gw-comet-3.json          # the real custody log
gwvec.py build ~/gw-building/.gw-comet-3.json \
               --foreign ~/gw-building/.gw-comet-1.json -o vectors.json
gwctl.py writ  <pier> <patp> <pass_hex>                # feed one to a verifier
```

Three things learned the hard way, all in
`doc/live-tests/PHASE2-RERUN-RESULTS.md`:

- **Pick a subject the verifier has never verified.** An anchored subject adds
  `tracked-tip` / `tracked-prefix` / `life-monotonic` to every verdict — and
  `tracked-lag` too, whenever the log you feed it is a prefix of the one the
  verifier already holds — and any mutation of entry 0 breaks them too, so
  nothing fails for one reason and nothing is readable.
- **Clear the snub set first.** Several cases emit a snub, and nothing lifts one
  on its own (above); if the subject is already snubbed you cannot tell whether
  this case snubbed it. `gwsnub.py show` / `gwsnub.py del` between cases —
  `del` rather than `gwctl.py writ` here, because you want the blocklist reset
  without a verdict muddying the next case's reading.
- **A verification is `O(blocks since the comet last moved its sat)`.** A comet
  dormant for ~250 blocks costs ~5 minutes per case on a 2-vCPU droplet. Two
  *different* subjects run concurrently on one verifier (single-flight is per
  ship); the same subject does not.
