# Groundwire confidential comets — operations runbook

How to stand up a Groundwire confidential comet from nothing: build the
runtime and the pill, mint an identity, boot the ship, install the desks,
sync a Bitcoin light client, and verify a peer end to end.

This is the bring-up procedure. For *results* — what has been tested and
what is broken — see `doc/live-tests/PHASE*-RESULTS.md`. For the protocol,
see `doc/opret-revision/`.

Every command below was checked against the source in this repo (or the
sibling repos named in §1) at the time of writing. Where a results doc and
the code disagree, the code wins and the disagreement is recorded in §12.

Placeholders: `<HOST>` a droplet address, `<PIER>` an absolute pier path,
`<COMET>` a `~sampel-…` comet name, `<FEED>` an `0v…` boot feed atom,
`<PORT>` an ames UDP port, `<SESSION>` a tmux session name.

---

## 1. What lives where

**Nothing but the `%groundwire` desk is in this repo.** A working comet
needs four checkouts:

| repo | provides | notes |
|---|---|---|
| this repo | the `%groundwire` desk: agents `%gw-btc` (verifier + block scanner) and `%urb-snapshot`; `causeway/` (minting); `onboarding/` | `desk.bill`, `sys.kelvin` `[%zuse 410/409/408]` |
| `gwbtc/vere` | the runtime (`urbit`, deployed as `gw-vere`) | branch `gw/next/kelvin/408` |
| `gwbtc/urbit` | the kernel, and the CI job that builds the pill | branch `gw/next/kelvin/408` |
| `gwbtc/urcrypt` | crypto, built into vere | branch `gw/cmp-pub`; must sit **next to** the vere checkout as `../urcrypt` |
| `gwbtc/node` | the `%node` desk: agent `%bitcoin-client`, the BIP-157/158 light client | `desk.bill` is `:~ %bitcoin-client ==` |
| `gwbtc/tcp-sidecar` | the `%tcp-sidecar` desk (agent `%tcp`) and the C `tcp-sidecar` binary | the light client's only transport |

`%gw-btc` never names a light client directly. It reads
`++light-client-agent` from `lib/lc-attestation.hoon:50`, which is
`%bitcoin-client`. **There is no `%light-client` agent.** Older docs that
say otherwise describe a bug that was fixed (Phase 2 finding B5).

`%gw-btc` has **no** Bitcoin endpoint, RPC or credential poke. Every byte
of chain data comes from the local `%bitcoin-client` agent over the
`/best-block` and `/is-synced` subscriptions. Endpoint configuration lives
on that agent's desk, and there is not much of it: `%bitcoin-client` pins
`network %mainnet` and its required peer services in `++init` and they
cannot be changed by poke.

---

## 2. Machine prerequisites and sizing

Verified on the live campaign's rig: three DigitalOcean droplets, Ubuntu,
**2 vCPU / 3.9 GB RAM**, one comet each.

| resource | requirement | why |
|---|---|---|
| vCPU | 2 minimum | header/filter sync is CPU-bound on this box; 1 vCPU has not been tried |
| RAM | 4 GB + an **8 GB swapfile** | vere runs `--loom 32` (4 GB address space). Observed peak RSS 1.7 GB. The one measured SIGSEGV was *not* OOM (3.4 GB available, 268 KB swap in use, no dmesg OOM entries) |
| disk | **≥ 20 GB free per host, and watch it** | a synced pier is ~2.2 GB; the campaign ran with 87–98 GB free. A full disk blocks *every* tool on the box, including `df` |
| UDP inbound | one port per ship, open | if the comet commits a `fief`, this must be the exact IP:port it committed |
| outbound TCP 8333 | required | the light client's peer connections |

Packages. Vere itself needs **no apt packages** — zig builds every C
dependency from `ext/`. What you do need:

```sh
# build host (can be your laptop; vere cross-compiles)
#   zig 0.15.2  — see below, the version is pinned
# droplet
sudo apt-get update
sudo apt-get install -y tmux rsync build-essential pkg-config libssl-dev
```

`libssl-dev` and `pkg-config` are for the `tcp-sidecar` build only; if you
cross-compile the sidecar elsewhere you can drop them.

Swap, on a 4 GB droplet:

```sh
sudo fallocate -l 8G /swapfile && sudo chmod 600 /swapfile
sudo mkswap /swapfile && sudo swapon /swapfile
echo '/swapfile none swap sw 0 0' | sudo tee -a /etc/fstab
```

**Two operators must never share a rig.** Two agents driving the same
three `@p` on the same three ports produces double-booted piers, divergent
event logs and unattributable ames behaviour. One operator per host.

---

## 3. Build the artifacts

### 3.1 vere

Prerequisites: **zig 0.15.2** (pinned — CI uses `mlugg/setup-zig@v2` with
`version: 0.15.2`), a sibling `urcrypt` checkout, and a real `.git/logs/HEAD`
in the vere tree (`build.zig:207` opens it unconditionally, so an exported
tarball with no reflog hard-fails).

```sh
git clone git@github.com:gwbtc/vere.git       # branch gw/next/kelvin/408
git clone -b gw/cmp-pub git@github.com:gwbtc/urcrypt.git gwbtc-urcrypt
# urcrypt must NOT look like a zig package to the vere build:
rm -f gwbtc-urcrypt/build.zig gwbtc-urcrypt/build.zig.zon
ln -sfn "$PWD/gwbtc-urcrypt" "$PWD/urcrypt"

cd vere
zig build -Doptimize=ReleaseFast -Dunsafe-dawn=true -Dtarget=x86_64-linux-musl
```

The binary lands at **`zig-out/x86_64-linux-musl/urbit`**. Copy it to the
droplet as `/opt/gw/bin/gw-vere`.

- The output directory is zig's target triple (`build.zig:546-560`). On an
  Apple Silicon laptop a native build lands at `zig-out/aarch64-macos-none/urbit`.
- On Linux, `build.zig:252-269` rewrites a *native* build's ABI to `musl`,
  so even a bare `zig build` on a glibc host emits `x86_64-linux-musl`.
  There is no `x86_64-linux-gnu` output unless you ask for it explicitly.
- `-Dunsafe-dawn=true` is the Groundwire flag (`build.zig:173-176`). It
  compiles out the PKI-gateway lookups in `pkg/vere/dawn.c` and derives the
  boot point from the feed alone. Without it, boot tries to fetch a
  sponsorship chain from `http://143.198.70.9:8080` for a ship that no
  gateway knows about.

### 3.2 the pill

**Not built by this repo, and not by hand.** The authoritative recipe is
the `build-pill` job of `.github/workflows/groundwire-build.yml` in
`gwbtc/urbit`. It builds a **brass** pill on a fake `~zod` via
`fyrd`/`khan-eval`, baking in `%gw-base` (the kernel), `%groundwire` (this
repo's `make groundwire` output), `%mcp` and `%vitriol`. The pill is
written into Clay at `/pill/pill` on `%base` and surfaces in the mounted
desk; CI copies it out as `gw-base.pill`.

To get one:

```sh
gh workflow run groundwire-build.yml --repo gwbtc/urbit --ref gw/next/kelvin/408
# then download the `gw-base-pill` artifact -> gw-base.pill
```

Push to this repo's `main` triggers the same workflow
(`.github/workflows/trigger-urbit-build.yml`).

Record the pill's sha256 and re-verify it **after** transferring it to each
droplet; a truncated pill fails late and confusingly.

```sh
sha256sum gw-base.pill
```

> Not automated: there is no local pill build. `onboarding/booting/README.md`
> documents a manual `+pill/solid` route; it is stale (see §12) and the one
> attempt in `testnet/run/pillbuild.log` failed with `mint-vain`.

### 3.3 the tcp-sidecar

```sh
git clone git@github.com:gwbtc/tcp-sidecar.git
cd tcp-sidecar/sidecar && make          # needs pkg-config + libssl-dev
```

Produces `sidecar/tcp-sidecar`. Copy to `/opt/gw/bin/tcp-sidecar`. It takes
exactly one argument, the pier path, and opens **no listening port** — it
is a Unix-domain client onto the pier's Lick socket at
`<PIER>/.urb/dev/tcp/tcp`. Outbound connections use whatever port the
caller asks for (8333, for Bitcoin).

### 3.4 the desks

```sh
cd <this repo>
make groundwire        # -> dist-groundwire/ : groundwire/* + vendor deps
```

`make groundwire` is a pure copy step. `make build` additionally creates a
venv and pip-installs pyinstaller for `gw-onboard`, which you do not need
here — use `make groundwire`.

**Strip `doc/` before you ship the desk.** `make groundwire` copies
`groundwire/*` wholesale, including this file. A desk builds marks from its
own `/mar`, and there is no `mar/md.hoon` in this desk or in
`vendor/base-dev/mar/` (`%base` has one; that does not help), so clay
cannot build a `%mime` tube for a `.md` file. See §5.4 — this is a
ship-killer.

```sh
rm -rf dist-groundwire/doc
```

The alternative is to give the desk an `md` mark — copy `mar/md.hoon` from
the kernel's `pkg/arvo/mar/` and add it to `VENDOR_BASE_DEV_GW` in the
`Makefile`. Until someone does that, strip `doc/`.

The other two desks ship as-is: `gwbtc/node/desk/` and
`gwbtc/tcp-sidecar/desk/`.

---

## 4. Order of operations

Light-client sync from genesis is **~2.5 hours** and it is the critical
path. Everything else fits inside it.

1. Build vere, the pill, the sidecar, the desks. *(once, on your laptop)*
2. Provision all hosts and push the artifacts. *(parallel)*
3. Mint each comet with Causeway; wait for confirmations. *(parallel)*
4. Boot each ship. *(parallel)*
5. Install the three desks on each ship. *(parallel)*
6. **Start the sidecar and seed peers on all hosts at once.** From here the
   clock runs; do not serialise it.
7. While syncing: bootstrap the public index, set up the supervisor, write
   down each ship's `@p`/port/pier.
8. When every host reports `is-synced %.y`: verify peers.

---

## 5. Per-host bring-up

### 5.1 Provision

```sh
ssh <HOST> 'sudo mkdir -p /opt/gw/{bin,pills,piers,desks} && sudo chown -R $USER /opt/gw'
scp gw-vere            <HOST>:/opt/gw/bin/gw-vere
scp tcp-sidecar        <HOST>:/opt/gw/bin/tcp-sidecar
scp gw-base.pill       <HOST>:/opt/gw/pills/gw-base.pill
ssh <HOST> 'chmod +x /opt/gw/bin/*'
rsync -a --delete dist-groundwire/  <HOST>:/opt/gw/desks/groundwire/
rsync -a --delete node/desk/        <HOST>:/opt/gw/desks/node/
rsync -a --delete tcp-sidecar/desk/ <HOST>:/opt/gw/desks/tcp-sidecar/
ssh <HOST> 'sha256sum /opt/gw/pills/gw-base.pill'   # compare to the build host
```

### 5.2 Mint the comet

Causeway, from `causeway/desktop/`:

```sh
cd causeway/desktop && pip install -e .      # python >= 3.10
```

Two spawn modes. Both mine a suite-C comet under a kelvin-9 hiding `dat`
by shelling out to the external `comet_miner` binary (`--miner`), then
build and broadcast a single commit transaction. **There is no
`causeway mine` subcommand.**

```sh
# you hold the wallet:
causeway spawn connect --xpub <XPUB_OR_DESCRIPTOR> \
                       [--sponsor <SPONSOR_PATP>] \
                       [--publish] \
                       [--utxo <TXID>:<VOUT>] \
                       [--signed-psbt <PATH|->] \
                       [--assume-saved] \
                       [--miner /path/to/comet_miner]

# causeway generates the wallet:
causeway spawn generate [--invite <FAUCET_CODE>] [--sponsor <SPONSOR_PATP>] …
```

- **Omit `--publish` for a confidential comet.** `--publish` adds the
  OP_RETURN publication output, which is permanent and irreversible: it is
  the difference between an identity whose opening lives on chain and one
  whose opening lives only in twelve words. A confidential comet **cannot
  later become public** — `+apply-spawn` in `lib/urb-core.hoon` is the only
  writer of the public index and it requires spending the original funding
  satpoint, which is already spent.
- Full headless operation needs `--utxo`, `--signed-psbt` and
  `--assume-saved`; without them the flow prompts. `prompt()` now aborts
  non-zero on any non-TTY stdin naming the flag to pass, so a piped run
  fails loudly rather than hanging.
- `spawn generate` with no funding still **waits forever**. Not automated.
- Requires ≥ 1000 sats in the funding UTXO (`REQUIRED_SATS`), default fee
  rate 2 sat/vB, `BLOCK_CONFIRMATIONS = 2`.

Then, once the spawn tx confirms, bake the custody log into the boot feed:

```sh
causeway finalize <COMET>-spawn.proof.json --feed <FEED>
```

`finalize` waits for each proof's tx to confirm, records the block, builds
the xtr custody log, and re-encodes the feed with xtr baked into the ring.
**Without this the ship boots fine but serves an empty log**, so no peer
can verify it. Pass every proof for the point, oldest first.

`finalize` also prints a `:gw-btc &noun [%gw-custody-entry …]` dojo line
that extends a **running** ship's log in place, no reboot required.

Sanity-check before you boot:

```sh
causeway proof verify <COMET>-spawn.proof.json --onchain
causeway proof show   <COMET>-spawn.proof.json
```

### 5.3 Boot the ship

**Run the ship inside tmux, and do not pass `-t`.** `-t/--no-tty` disables
terminal assumptions (`main.c:905`), which is exactly the dojo you need to
drive the ship. `-d/--daemon` implies `-t`. A tmux pane gives you a live
dojo you can drive with `send-keys` and read with `capture-pane`.

```sh
ssh <HOST>
tmux new-session -d -s <SESSION>
tmux send-keys -t <SESSION>:0.0 \
  '/opt/gw/bin/gw-vere -c <PIER> -w <COMET_NO_TILDE> -G <FEED> -p <PORT> --loom 32' C-m
```

Rules that have each cost hours:

- **Use a distinctive session name and always target `<SESSION>:0.0`.**
  `tmux send-keys -t <name>` **prefix-matches**. During Phase 3 two dojo
  commands landed in another agent's `~zod` and a live session was killed
  out from under the operator. `-t gwc1x:0.0`, never `-t gw`.
- **One writer per pane.** Driving a dojo with `send-keys` while a
  background poller does the same interleaves characters into one
  unparseable line, and drum persists that input buffer **across pier
  restarts**.
- **`-w` and `-G` must be given together.** `_boothack_doom()`
  (`king.c:810`) branches on `who_c` — the `-w` name. With `-G` alone and
  no `-w`, it falls through to `[%come ~]` and **self-mines a different
  comet**. With both, it builds `[%dawn seed]` and `_boothack_key()`
  cross-checks the seed's ship against `-w`, so `-w` is also your guard
  against booting the wrong feed.
- **Pick the right feed.** A Causeway artifact can carry more than one
  feed. Each is a `$dawn-1` `[[%2 ~] who=ship ryf=rift kyz=(list [lyf key])]`
  and jael's `%dawn` sets `lyf.own.pki` from `i.kyz` — so the wrong feed
  gives you the **right `@p` at the wrong life**, which is silent and
  breaks packet authentication later. Use the xtr-baked feed for a
  confidential comet. Gate on the `@p` before you commit to it.
- **Pin `-p <PORT>` if the comet commits a `fief`.** The fief names an
  exact IP and port; the ship must be reachable there or the fief is a lie.
- `-c <PIER>` creates the pier. On a restart, drop `-c`, `-w`, `-G` and
  `-B` and pass the pier as the trailing argument.

Confirm the identity before going further:

```
> our
> (scot %p fig:ex:cub:crypto <ring>)   ::  must reproduce the artifact @p
```

Restart form (after a crash, or from a supervisor):

```sh
rm -f <PIER>/.vere.lock
/opt/gw/bin/gw-vere -t --loom 32 -p <PORT> <PIER>
```

`-t` is correct *here* — a supervised restart has no terminal. You lose the
dojo; drive that ship over `<PIER>/.urb/conn.sock` instead.

### 5.4 Install the desks

Three desks: `%tcp-sidecar` (agent `%tcp`), `%node` (agent
`%bitcoin-client`), `%groundwire` (agents `%gw-btc`, `%urb-snapshot`).

For each, in the dojo:

```
> |new-desk %<desk>
> |mount %<desk>
```

then on the host:

```sh
rsync -a --delete /opt/gw/desks/<desk>/ <PIER>/<desk>/
```

then back in the dojo:

```
> |commit %<desk>
> |unmount %<desk>
> |install our %<desk>
```

Four rules here, all load-bearing:

- **Never `|commit <desk> %.y`.** The generator's `auto` argument defaults
  to `%.n`, so a bare `|commit %<desk>` is safe — but a raw
  `:hood &kiln-commit [%<desk> %.y]` (which is what a naive deploy script
  writes) arms a **1 Hz repeating `%dirk` timer**
  (`lib/hood/kiln.hoon:659-670`). Unmounting does **not** cancel it; it
  just converts every tick into a `[%not-mounted %<desk>]` line. Measured
  at 0.4–0.8 events/s/desk, forever, on a 2-vCPU box — it starves the ship
  while everything looks healthy. If you hit it:

  ```
  > :hood &kiln-cancel-autocommit ~
  ```

  `%rest` cancels **one** timer at a time and there can be several. Poke it
  five times, then confirm zero new `not-mounted` lines in 25 s.

- **Never leave a desk mounted.** A mounted desk syncs every file to unix,
  which needs a `%mime` tube per file. There is no `%md` mark anywhere in
  the desk or in `vendor/base-dev/mar/`, so any `doc/*.md` in a mounted
  desk cannot build one and the ship enters a permanent clay crash loop.
  Strip `doc/` from `dist-groundwire/` (§3.4) **and** unmount immediately
  after the rsync. Mount only for the length of the copy.

- **The commit can race the rsync on a large desk.** `%groundwire` is big
  enough that a single `kiln-commit` poke sometimes does not take. Re-poke
  `|commit` until a `%cx` scry of a committed file matches the byte length
  on disk. `gall: installing %gw-btc` in the log is the success signal.

- `%gw-btc` registers itself with jael automatically —
  `+on-init` emits `[%pass /anex %arvo %j %anex /writs]` (`gw-btc.hoon:248`).
  There is nothing to configure. Agent reload preserves the registration.

Verify the desk you deployed is the desk you meant to:

```sh
ssh <HOST> 'cd /opt/gw/desks/groundwire && find . -type f | sort | xargs md5sum | md5sum'
# compare against the same command on your working tree
```

### 5.5 Start the sidecar

The sidecar must be running **before** the light client can do anything.

```sh
ssh <HOST> 'cd <PIER> && SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt \
   setsid nohup /opt/gw/bin/tcp-sidecar . > /opt/gw/sc-<name>.log 2>&1 &'
```

The trailing `.` is the pier path; cwd is the pier. That matters: the
supervisor identifies the right sidecar by `/proc/<pid>/cwd`, not by port
(it has none) and never by `pgrep -f` prefix.

`SSL_CERT_FILE` matters because `main.c:618` calls
`SSL_CTX_set_default_verify_paths` with `SSL_VERIFY_PEER`.

The sidecar has no internal recovery: `SIGSEGV`/`SIGBUS`/`SIGABRT` print
`--- CRASH: signal N ---` plus a backtrace and `_exit(1)`. A dropped Lick
socket is a hard exit too. **Run it under a supervisor** (§5.9).

### 5.6 Seed peers

`%bitcoin-client` has **no DNS seeding and no hardcoded peer list**. It
connects only to addresses you poke in, then self-sustains by `getaddr`
gossip. Cold start with no `%add-earth-peer` never connects.

**Filter headers are only servable by peers advertising
`NODE_COMPACT_FILTERS`.** `++init` (`bitcoin-client.hoon:1781-1800`) sets
`node-network`, `node-witness` and `node-compact-filters` as *required*
peer services, and `++peer-services-are-sufficient` drops everything else.
Seed from an unfiltered DNS seed and filter sync never leaves height 1 —
this is the single biggest time sink in the whole procedure, and it is what
killed the entire Phase 4 run.

Ask the seeds for the service-bit filter `x49` —
`NODE_NETWORK(1) | NODE_WITNESS(8) | NODE_COMPACT_FILTERS(64) = 0x49`:

```sh
for s in seed.bitcoin.sipa.be dnsseed.bluematt.me seed.bitcoinstats.com \
         seed.bitcoin.jonasschnelli.ch seed.btc.petertodd.net \
         seed.bitcoin.sprovoost.nl dnsseed.emzy.de seed.bitcoin.wiz.biz \
         seed.mainnet.achownodes.xyz \
         dnsseed.bitcoin.dashjr-list-of-p2p-nodes.us ; do
  for i in 1 2 3 4 5 6 ; do dig +short A "x49.$s" ; done
done | grep '^[0-9]' | sort -u > peerpool.txt
```

All ten seeds honour the `x49.` prefix and each returns a small random
slice (~20–25 addresses) per query, hence the six repeats. Expect ~200
unique CF-capable IPs. Keep a per-pier `used-<pier>.txt`: an IP handed to
the same ship twice is wasted, and `%bitcoin-client`'s blacklist
expiration is **`~d3`**, so a burned seed stays burned for three days.

Then poke them in, **~25 at a time**:

```
> :bitcoin-client &add-earth-peer [%ipv4 .<a.b.c.d> 8.333]
```

The vase is a bare `earth-address` = `[net-id=?(%ipv4 %ipv6) address=@ux port=@ud]`
(`bitcoin-client.hoon:27-31`). `net-id` must be `%ipv4` or `%ipv6`; a Tor
or I2P variant `!!`s.

> **Bulk-adding peers SIGSEGVs the sidecar, reproducibly.** Adding 200–300
> at once reliably produced `--- CRASH: signal 11 ---`, after which
> `live-earth-peers` went to **0** and sync stalled until the supervisor
> restarted the sidecar and re-seeded. Batches of ~25 are stable — ~450
> peer additions across three ships with no crash. This is the
> `gwbtc/node#1` fragility with a known trigger.

Do not over-seed chasing throughput. `+continue-syncing-headers` asks
**one** peer for 2,000 headers and waits (`bitcoin-client.hoon:999-1034`),
so header sync is strictly serial: extra peers buy resilience, not speed.
`target-peers` is 10.

There are no dojo mark files for these pokes (`node/desk/mar/` has only
`txt json hoon mime noun bill kelvin`), and that is fine: dojo's `&mark`
form falls through to a bare retag when the mark is missing
(`app/dojo.hoon`, the `%as` arm — `?. has-mark :: yolo`).

### 5.7 Tell when sync is genuinely complete

**`%bitcoin-client`'s `++peek` returns `~` for every path**
(`bitcoin-client.hoon:181-184` — the arm is literally `~`). There is
nothing to scry. Read the logs:

```
> :bitcoin-client &log-info ~
```

which prints, among others:

```
[%is-synced %.y]
[%best-block [block-height=961.280 …]]
[%best-filter-header [961.280 …]]
[%headers 961.281]
[%filter-headers 961.281]
[%live-earth-peers 72]
[%blacklist 14]
```

**"Started" is not "complete".** `++is-fully-synced` is
`?&(block-headers-are-synced filter-headers-are-synced)`, and
`++filter-headers-are-synced` is `.=(block-hash.best-block block-hash.best-filter-header)`
— an *equality*, not a height. So:

- `[%is-synced %.y]` is the only authoritative answer, and
- **both** `%headers` and `%filter-headers` must be at the tip. A ship with
  961k block headers and `filter-headers 1` is not synced, is not close to
  synced, and will produce no verdicts.

Confirm the same thing from `%gw-btc`'s side:

```
> .^([synced=? tip=(unit @ud) indexing=? halt=(unit [at=@ud cursor=@ud since=@da])] \
      %gx /(scot %p our)/gw-btc/(scot %da now)/ready/noun)
```

`synced=%.y` and a `tip` at the chain tip. Note that `%gw-btc` learns
`synced` from a `/is-synced` subscription that fires on the initial watch
and thereafter **only on `%bitcoin-client`'s transitions** — and that emit
coverage is incomplete (losing the last peer is announced; recovering one
is not). `%gw-btc` compensates with `+refresh-synced`, which drops and
re-establishes the subscription on exactly the paths that were held for
lack of readiness. It is demand-driven, not timed: a retransmitting peer is
the poll. A quiet ship may read `synced=%.n` until something asks it to
care.

Do not be alarmed when `conn.sock` stops answering during filter-header
sync. Scries and pokes have been measured timing out at **120–380 s** while
batches are processed. Retry later; the ship is working.

### 5.8 Bootstrap the public index (optional, and one-shot)

The public block scanner is only needed to index **public** comets — a
light-client-only ship verifies confidential attestations without it. But
`sponsor-known` reads the public index, so if any comet in your set names a
**public** sponsor, every verifier must have indexed that sponsor's
publication block before it can confirm the sponsor exists.

```
> :gw-btc &gw-index-from 961.040
```

or, to start from the desk's built-in default snapshot:

```
> :gw-btc &urb-start-indexing ~
```

Choose a height at or below the sponsor's publication block. Both pokes are
guarded against destroying an existing index: they refuse whenever the
cursor has moved or `unv-ids` is non-empty, logging
`%gw-btc: refusing %gw-index-from: an index already exists`. The guard reads
**the index**, not a flag — the `.indexing` flag on any pier upgraded
before 2026-08-06 was reset to `%.n` on every `+on-load` and is not a
record of anything.

`%gw-index-from 0` is a silent no-op.

### 5.9 Supervisor

Both vere and the sidecar die under load. Over one ~5h40m three-droplet
session: 1 vere SIGSEGV (`loom: external fault`), 24 sidecar SIGSEGVs. Run
a supervisor per ship. Three triggers, in this order:

1. **VERE-DOWN** — no king process whose final argument is `<PIER>` and no
   serf whose `--snap-dir` is `<PIER>`: delete `<PIER>/.vere.lock`, relaunch
   `gw-vere -t --loom 32 -p <PORT> <PIER>`.
2. **SIDECAR-DOWN** — no `tcp-sidecar` whose `/proc/<pid>/cwd` is `<PIER>`:
   restart it, then **immediately** run the wedge recovery. A dead sidecar
   *is* the wedge trigger; waiting out the staleness window wastes 5 minutes.
3. **WEDGE** — event log stale > 300 s: ensure the sidecar, then
   `:bitcoin-client &kill-peer-connections ~`, then re-seed **one** peer and
   let gossip refill. 300 s cooldown between recoveries.

Two rules the supervisor must follow:

- **Liveness is the newest mtime across `<PIER>/.urb/log/*/data.mdb`.**
  The `.urb/log` *directory* mtime is inert: it is a dirent, LMDB writes
  into an already-created `data.mdb`, and it has been measured **21 hours
  stale on a ship demonstrably processing events**. Using it declares every
  healthy ship wedged from the moment it boots. Glob the epoch directories
  — piers roll epochs, so it is not always `0i0`.

  ```sh
  find <PIER>/.urb/log -name data.mdb -printf '%T@\n' | sort -rn | head -1
  ```

- **Match processes exactly, never by `pgrep -f` prefix.** `p4c1`
  prefix-matches `p4c1b`; killing the wrong pier is worse than killing none.

A restart *is* recovery: measured 20 s down for a vere SIGSEGV with no state
loss (the pier replays), and 19 s for a sidecar kill.

> Not automated: `gwsup.sh`, `poolfill.py` and the other campaign scripts
> lived in an operator scratchpad and on the droplets at `/opt/gw/`. They
> are **not in this repo** and must be rewritten from the spec above.

---

## 6. Verify a peer end to end

Preconditions on the verifier: `/x/ready` reports `synced=%.y`, and its tip
covers the peer's evidence.

Get the peer's **live jael pass** — not the artifact's `pass_atom_hex`,
which is a different, shorter object (108 B vs ~330–405 B for the jael
pass). Read it off the running ship via jael's `%pynt` → `keys` → `pass`.

Then, on the verifier:

```
> :gw-btc &noun [%jael-writ %gw-btc ~<peer> 0x<peer's live pass>]
```

Expected sequence in the log:

```
[%gw-btc-lc-scan-clean tip 0x… vout=0 off=0 from=961.196 to=961.280]
%gw-btc: attestation for ~<peer> is VALID
  [ok] chain-nonempty   [ok] chain-bounded   [ok] fetch-count
  … 44 checks …
  [ok] sponsor-known    [ok] tracked-prefix
ames: lamp ~<peer> static ip .<a.b.c.d> port <PORT>     ::  if the peer commits a fief
```

- **A verification takes 100–110 s** on a 2-vCPU droplet for a comet
  spawned ~20 blocks back (~11 filter fetches + 2 block downloads), and
  longer the longer the comet has been dormant. **There is no wall-clock
  deadline, by design**: cost is `O(blocks since the comet last moved its
  sat)`, so a fixed timeout would make long-dormant comets unverifiable as
  a function of a magic number. The only timer is `+stuck-job-guard` (`~h2`),
  a resource-leak backstop that emits no verdict. Individual light-client
  *requests* are bounded at `~m5` (`+lc-fetch-timeout`).
- `+report` names the outcome **`VALID` / `STALE (out of date, not fraud)` /
  `UNDETERMINED (no verdict emitted)` / `INVALID`** and marks each check
  `[ok]` / `[..]` stale / `[??]` unevaluable / `[XX]` fraud
  (`lib/self-attestation.hoon:32-49`). Only `[XX]` produces a snub.
- Single-flight: at most one verification per peer. A duplicate writ while
  one is in flight is dropped — with a log line naming the reason.

Confirm the point landed, and that it came through the Groundwire path:

```
> .^((unit @ud)  %j /=lyfe=/~<peer>)     ::  [~ 3]      — point installed at life 3
> .^((unit @tas) %j /=dome=/~<peer>)     ::  [~ %gw-btc] — via %writ, not vanilla comet PKI
> .^([?(%allow %deny) (list @p)] %ax /(scot %p our)//(scot %da now)/snubbed)
                                          ::  must stay [%deny ~]
```

`/=dome=/` is the single cleanest discriminator between a Groundwire
verdict and ordinary comet registration. It short-circuits on fakeships, so
it is untestable on a fakezod.

Then exchange traffic:

```
> |hi ~<peer>
```

**Known gap:** a verified `fief` becomes a jael point and a runtime lamp,
but `+send-blob-via` chooses a lane only from a route learned from a heard
packet, and ames offers the runtime a `[%& ship]` lane only for *sponsors*.
So an on-chain fief is today useful only when its holder is reached **as a
sponsor**. Two comets that have never exchanged a packet still route via
the sponsor.

---

## 7. Diagnostics reference

### `%gw-btc` scries

All are care `%x`, so `%gx` with a trailing mark. Confirmed against
`app/gw-btc.hoon:635-716`.

| path | mark | value |
|---|---|---|
| `/x/ready` | `noun` | `[synced=? tip=(unit @ud) indexing=? reorg-halt=(unit [at cursor since])]` |
| `/x/inflight` | `noun` | `(set ship)` — peers with a verification running |
| `/x/pending-own` | `noun` | `(unit @ud)` — the `%anew` self-validation slot |
| `/x/publicizing` | `noun` | `(set ship)` |
| `/x/custody` | `noun` | our own verified custody log (the xtr we serve) |
| `/x/attested` | `noun` | `(map ship sont)` — tip each confidential peer last attested to |
| `/x/confidential` | `noun` | `(set ship)` — identities held confidentially |
| `/x/sponsees` | `noun` | `(map ship [life since])` |
| `/x/declined` | `noun` | `(set ship)` |
| `/x/block-id` | `block-id` | the scanner's cursor `[hax num]` |
| `/x/points` | `urb-points` | all **public** indexed points |
| `/x/point/<ship>` | `urb-point` | one point; `[~ ~]` if confidential or unindexed |
| `/x/urb-state` | `noun` | the whole scanner state, confidential points stripped |

```
> .^(* %gx /(scot %p our)/gw-btc/(scot %da now)/ready/noun)
> .^((set @p) %gx /(scot %p our)/gw-btc/(scot %da now)/inflight/noun)
> .^(* %gx /(scot %p our)/gw-btc/(scot %da now)/custody/noun)
> .^(* %gx /(scot %p our)/gw-btc/(scot %da now)/block-id/block-id)
```

`/x/ready`, `/x/inflight` and `/x/pending-own` exist because each of them
was, at some point, an invisible piece of state that cost hours. A ship
quietly refusing every attestation looks exactly like a ship nobody is
talking to; `/x/ready` is the difference.

### `%gw-btc` pokes

| poke | purpose |
|---|---|
| `:gw-btc &noun [%jael-writ %gw-btc ~<ship> <pass>]` | verify a peer (normally arrives from jael) |
| `:gw-btc &noun [%jael-anew %gw-btc]` | re-validate and republish our own pass |
| `:gw-btc &noun [%gw-custody-entry [<txid> <height> <opening>]]` | Causeway ingestion; extends our own log |
| `:gw-btc &gw-sponsor-decline ~<ship>` | refuse to sponsor; produces silence, never a snub |
| `:gw-btc &gw-sponsor-clear ~<ship>` | undo the above |
| `:gw-btc &gw-reorg-resume ~` | resume a reorg-halted scanner from the current cursor |
| `:gw-btc &gw-reorg-resume [~ <height>]` | rewind the cursor first, then resume |
| `:gw-btc &gw-index-from <height>` | bootstrap the public index (one-shot) |
| `:gw-btc &urb-start-indexing ~` | ditto, from the default snapshot |

An unrecognised mark **crashes the poke** (`?+ mark !!`). All pokes are
local-only (`?> =(our src)`).

### `%bitcoin-client`

```
> :bitcoin-client &log-info ~                 ::  the only way to read status
> :bitcoin-client &add-earth-peer [%ipv4 .<a.b.c.d> 8.333]
> :bitcoin-client &kill-peer-connections ~    ::  flush a phantom peer table
> :bitcoin-client &broadcast-transaction <tx>
```

Beware: `%log-info` prints `pending-block-hash-reqs` and
`pending-block-height-reqs` as **whole jugs**, not counts. On a busy node
that is a large dump.

### Kernel scries

```
> .^((unit @ud)  %j /=lyfe=/~<ship>)                 ::  peer's life, ~ if no point
> .^((unit @tas) %j /=dome=/~<ship>)                 ::  [~ %gw-btc] if Groundwire-verified
> .^([?(%allow %deny) (list @p)] %ax /(scot %p our)//(scot %da now)/snubbed)
> .^((list lane:ames) %ax /(scot %p our)//(scot %da now)/peers/(scot %p ~<ship>)/forward-lane)
> .^((map @p ?(%alien %known)) %ax /(scot %p our)//(scot %da now)/peers)
```

### Log lines worth grepping

```
%gw-btc: attestation for ~… is VALID
%gw-btc: attestation for ~… is INVALID
%gw-btc: attestation for ~… is STALE, not invalid
%gw-btc: attestation for ~… is UNDETERMINED; emitting no verdict
%gw-btc: writ from ~… held: light client NOT synced
%gw-btc: writ from ~… held: tip N below evidence height M
%gw-btc: writ from ~… held: no chain tip yet
%gw-btc: writ from ~… dropped: a verification is already in flight
%gw-btc: writ from ~… dropped: sponsorship declined by operator
%gw-btc: writ from ~… dropped: already a public point
%gw-btc: writ from ~… dropped: public-spawn replay in progress
%gw-btc: light client is SYNCED; confidential verification enabled
%gw-btc: light client is NOT synced; holding all attestations (no verdicts)
%gw-btc: SNUBBING ~… on a negative %gw-btc verdict
%gw-btc: CHAIN REORG TO N -- BLOCK SCANNER HALTED
%gw-btc: refusing %gw-index-from: an index already exists
%gw-btc: verification thread for ~… ended without a verdict
%gw-btc: custody log verified (N entries); refreshing our pass
%gw-btc: %anew refused: …                       (seven distinct reasons)
[%gw-btc-lc-scan-clean …] / [%gw-btc-lc-scan-spent …] / [%gw-btc-lc-scan-degenerate …]
ames: ~…: got attestation
ames: lamp ~… static ip .a.b.c.d port N
[%not-mounted %<desk>]                          ::  autocommit storm
--- CRASH: signal 11 ---                        ::  tcp-sidecar
loom: external fault                            ::  vere SIGSEGV
mesa: bind: address already in use              ::  port squatted
bail: meme
```

---

## 8. Known-good timings

Measured on 2-vCPU droplets against mainnet. Use these to tell "slow" from
"stuck".

| stage | rate | wall clock |
|---|---|---|
| vere cross-compile (`ReleaseFast`) | — | a few minutes |
| pill build (CI, `build-pill` job) | — | up to 45 min |
| comet mint → 2 confirmations | — | 20–60 min (chain-dependent) |
| ship boot from pill | — | seconds to a couple of minutes |
| desk install (3 desks) | — | a few minutes |
| **block headers 0 → ~961k** | ~16 k/min once ≥100 peers | **~55 min** |
| **filter headers 0 → ~961k** | ~10 k/min | **~95 min** |
| **full light-client bring-up** | — | **~2.5 h** |
| one confidential verification | ~11 filter fetches + 2 blocks | **100–110 s** |
| BIP-158 liveness scan, ~170 blocks | — | ~4 min |
| single block fetch | — | tens of seconds |
| peer recovery 0 → 10 live peers | gossip | a few minutes |
| sidecar kill → detected | 30 s poll | 5 s |
| sidecar kill → recovered | — | 19 s |
| vere SIGSEGV → back up, replayed | — | 20 s |
| synced pier on disk | — | ~2.2 GB |

An earlier plan (`doc/opret-revision/05-live-test-plan.md`, Phase 0)
estimated "~8 min, ~850 MB pier" for light-client sync. That is wrong by an
order of magnitude in time and by ~2.5× in space; the numbers above are
measured. That doc has been annotated.

Anything materially slower than the table is a symptom, not variance —
start at §9.

---

## 9. Troubleshooting, by symptom

**Filter-header height never leaves 1, block headers advance normally.**
Your peers cannot serve compact filters. `++init` requires
`node-compact-filters`, and unfiltered DNS seeds mostly do not have it.
Re-seed from `x49.`-prefixed seeds (§5.6). This killed an entire test run.

**`live-earth-peers` is 0 and sync stalled, right after seeding a lot of peers.**
The sidecar SIGSEGVed. Check `/opt/gw/sc-*.log` for `--- CRASH: signal 11 ---`.
Restart it, then `:bitcoin-client &kill-peer-connections ~`, then re-seed
**one** peer. Seed in batches of ~25 from now on.

**Peers look fine in `%log-info` but nothing progresses.**
The sidecar died and the agent does not know. Its `earth-peers` table is
never invalidated, so it reports stale live peers and every send returns
`no such connection`. Restarting the sidecar alone does **not** fix it —
you must also `&kill-peer-connections ~` to flush the phantom table.

**The ship prints `[%not-mounted %<desk>]` forever, and is sluggish.**
Kiln autocommit. `:hood &kiln-cancel-autocommit ~`, five times. Then find
whatever poked `%kiln-commit` with `%.y` and fix it.

**The ship is in a clay crash loop right after a mount.**
A file in the mounted desk has no mark that can build a `%mime` tube —
almost always a `doc/*.md`. Remove `doc/` from the desk source and
redeploy. Unmount everything.

**Every ship looks WEDGED to the supervisor from the moment it boots.**
You are reading `<PIER>/.urb/log` (a directory, inert). Use the newest
mtime across `<PIER>/.urb/log/*/data.mdb`.

**Scries and pokes time out at 2–6 minutes.**
The ship is processing filter-header batches. Expected. Retry later.

**Attestations produce no verdict and nothing in the log.**
Check `/x/ready`. If `synced=%.n` the writ is being held, which is correct
— but the log *does* name every drop now, so a genuinely silent drop means
the agent is older than 2026-08-06. Check `/x/inflight`: a stranded
single-flight slot silences that peer for up to `~h2`. Check
`/x/pending-own` for the `%anew` equivalent.

**A peer got `INVALID` on `[XX] sponsor-known`, and is now snubbed.**
Your scanner has not reached the block where that sponsor published. This
is ignorance, not evidence, and as of 2026-08-06 `sponsor-known` is
classified unevaluable, so it yields `UNDETERMINED` and no snub. If you are
on an older desk, this snub is sticky and blocks the packet that would fix
it. Undo with an ames `%snub %deny %del` task, bring the index forward
(§5.8), and re-verify.

**`[%gw-btc-lc-scan-clean … from=N to=0]`.**
A degenerate scan range — the scan examined **zero** blocks and would have
reported an unspent tip identically to a spent one. Current code returns
`~` (undeterminable) and logs `%gw-btc-lc-scan-degenerate`. Seeing
`to=0` means the light client believed the tip was genesis.

**`%gw-btc: CHAIN REORG TO N -- BLOCK SCANNER HALTED`.**
A rollback at or below the scan cursor. Confidential verification is
unaffected (it reads the light client directly). The public index may
contain facts derived from orphaned blocks and there is no way to tell
which — a `$point` does not record the height it was indexed at.
`:gw-btc &gw-reorg-resume ~` accepts that; `[~ <height>]` rewinds first and
rescans, which *adds* correct facts but cannot remove wrong ones. The only
repair is to rebootstrap the index.

**`mesa: bind: address already in use`.**
A stale ship or an old supervisor is squatting the UDP port. Find it by
exact pier match, not `pgrep -f` prefix. Do not boot onto a bound port.

**Two ships booted from the same feed, or the wrong `@p` came up.**
Almost certainly `-G` without `-w` (self-mined a new comet), or the wrong
feed from a multi-feed artifact (right `@p`, wrong life). Check
`fig:ex` against the artifact before doing anything else.

**Dojo commands land in the wrong pane.**
`tmux send-keys -t <name>` prefix-matches. Use `-t <SESSION>:0.0`.

**Everything on the box fails, including `df`.**
The disk is full. Watch disk on every host, always.

**`bail: meme` on every packet from one peer.**
Kernel ames, fixed in `gwbtc/urbit` by the `+open-jam-shaped` guard on
`+is-open-packet`. Verified 0 occurrences over 3,831 live comet packets on
the fixed kernel; the old kernel logged 378 and never recovered. If you see
this, your pill predates the fix.

---

## 10. Things that are deliberately not repairable

- **A confidential comet cannot become public.** `+apply-spawn` is the only
  writer of the public index and requires spending the original funding
  satpoint. Attempting it costs a fee, costs confidentiality permanently,
  and leaves the comet *less* reachable than before.
- **A destroyed blind destroys a confidential identity.** `+verify-dat`'s
  `=(d.u.psd (spawn-commit spawn.open blind.open))` cannot be satisfied
  without the blind; brute force is a 2^256 search. A **public** comet's
  opening is in its OP_RETURN and recovers from the chain alone with no
  secret material. This is the property, not a bug.
- **Reorg recovery is a halt, not a repair.** See §9.

---

## 11. Not automated

Stated plainly so nobody hunts for a script that does not exist:

- **No local pill build.** The pill comes from CI in `gwbtc/urbit`.
- **No supervisor in this repo.** `gwsup.sh` is specified in §5.9 and must
  be written.
- **No peer-pool tool in this repo.** `poolfill.py` is specified in §5.6.
- **No deploy script.** The desk install in §5.4 is manual.
- **No fully headless Causeway spawn without funding.** `spawn generate`
  with no funded UTXO waits forever rather than timing out.
- **`causeway.py` and `gw-onboard.py` cannot find vere or the miner on Linux.**
  Both compute `f"{arch}-{os_name}-none"` for the zig output directory,
  which yields `x86_64-linux-none` — a triple zig never emits. Always pass
  `--vere` and `--miner` explicitly on Linux.

---

## 12. Where the docs and the code disagree

Recorded rather than quietly resolved. In each case this runbook follows
the code.

| claim | where | reality |
|---|---|---|
| light-client sync is "~8 min, ~850 MB pier" | `doc/opret-revision/05-live-test-plan.md` Phase 0 | ~2.5 h, ~2.2 GB. Measured twice. |
| build vere with zig **0.14.1**, output at `zig-out/x86_64-linux-none/urbit` | `onboarding/booting/README.md` | zig **0.15.2** (`vere/INSTALL.md`, CI pin); Linux output is `zig-out/x86_64-linux-musl/urbit`. `x86_64-linux-none` is not a triple zig emits. |
| pill is built by hand with `+pill/solid` on a `%gw-base` desk | `onboarding/booting/README.md` | production is `brass:pill` via `fyrd`/`khan-eval` in CI, with `%groundwire`/`%mcp`/`%vitriol` baked in; output lands in Clay at `/pill/pill`, not `.urb/put/`. The solid route failed locally with `mint-vain`. |
| the repo runs an agent called `%urb-watcher` on a `%groundwire` desk | this repo's `README.md` | the agents are `%gw-btc` and `%urb-snapshot` (`desk.bill`). `%urb-watcher` is the old name. |
| `%gw-btc` scries `%light-client` | scattered comments, older results docs | the agent is `%bitcoin-client`; `%gw-btc` reaches it through `++light-client-agent:lca`. Fixed as Phase 2 finding B5. |
| Causeway supports `escape`, `adopt`, `detach`, `fief`, `set-mang`… | `causeway/README.md`, `causeway/docs/OPERATIONS.md` | `src/ops/index.ts` exports exactly one op, `rekey`. Under the kelvin-9 OP_RETURN revision the other opcodes are removed. `causeway/docs/OPERATIONS.md` describes a retired commit+reveal protocol and a `protocol/encoder.ts` that does not exist — treat it as historical. |
| `causeway` has a `mine` subcommand | folklore | it does not. Mining happens inside `spawn generate`/`spawn connect` via the external `comet_miner` binary (`--miner`). |
| `causeway/desktop/README.md` documents the CLI | itself | it omits `proof show`, `proof verify`, and nearly every flag, including `--assume-saved`, `--utxo` and `--signed-psbt`, which are what make a scripted spawn possible. |
| `causeway/README.md` cites `tests/dat.spec.ts` | itself | that file does not exist. |
| `_print_boot_oneliner` points at `https://groundwire.io/causeway/boot.sh` | `causeway/desktop/causeway.py:3415` | the repo README points users at `groundwire.dev`. One of the two hostnames is wrong; unresolved here. |
| the pier liveness signal is `<PIER>/.urb/log` mtime | early briefs | inert; measured 21 h stale on a live ship. Use `<PIER>/.urb/log/*/data.mdb`. |

---

## See also

- `doc/live-tests/PHASE*-RESULTS.md` — what was actually tested, and every
  failure, with evidence.
- `doc/opret-revision/05-live-test-plan.md` — the test matrix this runbook
  supports.
- `doc/opret-revision/04-decisions-addendum.md` §8 — the `%bitcoin-client`
  API contract, including the two byte-order and subscription traps that
  were each fatal against a real node.
