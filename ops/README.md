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

| tool | what it does |
|---|---|
| `gwctl.py` | the operator surface: identity, desks, readiness, peers, verification pokes |
| `gwmint.py` | mint a comet on mainnet, with an independent pre-broadcast verification gate |
| `bootcomet.sh` | first boot of a minted comet from a pinned pill |
| `stopship.sh` | stop a *supervised* ship — supervisor first, then runtime, then sidecar |
| `gwsup.sh` | per-ship supervisor: restarts vere and the sidecar, unwedges the light client |
| `poolfill.py` | refill the peer pool from `x49.`-filtered DNS seeds |
| `addpeers.py` | `%add-earth-peer` a batch of IPs in one strand |

## The things that cost hours

**Ships run with `-t`, so there is no dojo.** `OPERATIONS.md` §5.3 tells you to
run inside tmux without `-t` and drive the dojo with `send-keys`. Every ship in
every live campaign has actually run with `-t` under a supervisor, driven over
`conn.sock`. The runbook's `> :gw-btc &noun [...]` lines are *notation*: a scry
written there as `.^(* %gx /...)` has to be wrapped in a `(strand ,vase)` thread
and handed to khan. `gwctl.py` is that wrapping.

**`%bitcoin-client` cannot be scried.** Its `++peek` is literally `~` for every
path (`app/bitcoin-client.hoon:181-184`). Any tool that scries `/is-synced`,
`/best-block` or `/peers` gets nothing and always did — the recovered
`p4setup.py sync`/`info` and `peercount.py` were dead code against a real node,
which is why they are not carried forward in that form. Read readiness from
`%gw-btc`'s `/x/ready` (`gwctl.py ready`), and detail from the ship's log after
`gwctl.py peers` pokes `&log-info`.

**Liveness is `<pier>/.urb/log/*/data.mdb`, never `<pier>/.urb/log`.** The
directory is a dirent; LMDB writes into an already-created file. Measured 21 h
stale on a ship demonstrably processing events. Using the directory declares
every healthy ship wedged from the moment it boots.

**Match processes exactly.** `p4c1` prefix-matches `p4c1b`. Never
`pkill -f urbit`. `gwsup.sh` matches the king by exact final argv, the serf by
the field after `--snap-dir`, and the sidecar by `/proc/<pid>/cwd` — it has no
port and no distinctive argv.

**`gwsup.sh` is now a singleton.** It takes an flock. Before that guard, all
three droplets were found running *two* supervisors per pier; both saw
VERE-DOWN, both relaunched, and the loser died on
`mesa: bind: address already in use`. It is also why ships that had been
stopped were found running again — nothing stops a supervised ship except
`stopship.sh`, and `OPERATIONS.md` documents no shutdown procedure at all.

**Seed peers ~25 at a time.** Bulk `%add-earth-peer` reliably SIGSEGVs the
tcp-sidecar (`gwbtc/node#1`). Filter headers are only servable by peers
advertising `NODE_COMPACT_FILTERS`, hence the `x49.` DNS prefix in
`poolfill.py`; seeding from unfiltered seeds leaves filter sync at height 1
forever and killed an entire test run.

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

It also sets a `fief` and a `sponsor` at spawn, neither of which the causeway
CLI can do (`--publish` exists only on `spawn`, and `rekey` has no `--fief`;
`build_rekey_psbt`'s publication parameters are unreachable from any caller).

```sh
gwmint.py mine  <label> <funding-txid> <vout>
gwmint.py build <label> [--fief=IP:PORT] [--sponsor=~patp] [--publish] [--fee-rate=N]
gwmint.py broadcast <label>          # refuses unless the gate passed
gwmint.py status    <label>          # poll for confirmation
gwmint.py artifact  <label> <n>      # -> ~/gw-building/.gw-comet-<n>.json (0600)
```

> **A comet must not name a *confidential* comet as its sponsor.**
> `sponsor-ok` in `lib/self-attestation.hoon:456-459` is
> `(~(has in known-public) u.sponsor...)` — it requires the sponsor to be a
> **public** point. Naming a confidential one makes `sponsor-known` fail, which
> is classified unevaluable, so every verification of the sponsee returns
> UNDETERMINED and no point is ever installed. An absent sponsor is fine: it
> projects to self and `sponsor-ok` is `%.y`.
