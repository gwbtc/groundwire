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
| `gwbtc/tcp-sidecar` | the `%tcp-sidecar` desk (agent `%tcp`) and the C `tcp-sidecar` binary | the light client's only transport. Public. The binary now ships prebuilt in the release tarball (§3.3); clone this only for the desk |

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
sudo apt-get install -y tmux rsync build-essential
```

`pkg-config` and `libssl-dev` used to be on that line, for the
`tcp-sidecar` build only. CI now ships a **statically linked** sidecar
(§3.3), so the droplet no longer needs OpenSSL headers. Add them back only
if you are building the sidecar on the droplet by hand.

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

Two routes. **Build locally** when you are testing a kernel branch — which is
the normal case, because the kernel is usually what is under test.

#### Locally, from a kernel branch (the route every live campaign has used)

A **solid** pill, built by `fyrd`-ing `solid:pill` on a throwaway fakeship.
`ops/` does not carry this as a script only because the builder pier is a
long-lived local artifact; the recipe is six steps and is worth understanding:

1. Boot a fakeship builder pier once (`urbit -F zod -B <any pill> -c builder`),
   and thereafter restart it with `urbit -t --loom 32 builder`.
2. `kiln-merge` a `%gw-base` desk off `%base`.
3. Mount it, delete every child, and rsync `pkg/arvo/` and `tests/` from your
   kernel checkout into it.
4. `kiln-commit` with **`%.n`**, in a retry loop, until a `%cx` scry of a
   known file matches its byte length on disk. A single poke does not reliably
   take on a desk this large, and `%.y` arms a 1 Hz `%dirk` timer that
   unmounting does not cancel (§5.4).
5. **Unmount** before compiling.
6. `fyrd` a thread that does
   `(solid:pill /(scot %p our)/gw-base/(scot %da now)/sys ~ | now & ~)` and
   `%ins`-es the result at `/pill/pill` on `%base`; it lands on disk in the
   mounted `%base` desk as `pill.pill`.

Verified in the cleanroom run of 2026-08-06: this produced a working pill from
`hd/cc-kernel@de3222d36a` in about six minutes, and three comets booted from
it. Take the sha256 and pin it in whatever boots from it — `ops/bootcomet.sh`
refuses to boot a pill whose hash it does not expect.

#### From CI

The `build-pill` job of `.github/workflows/groundwire-build.yml` in
`gwbtc/urbit` builds a **brass** pill the same way, baking in `%gw-base`,
`%groundwire`, `%mcp` and `%vitriol`, and copies it out as `gw-base.pill`.

```sh
gh workflow run groundwire-build.yml --repo gwbtc/urbit --ref <branch>
```

Push to this repo's `main` triggers it
(`.github/workflows/trigger-urbit-build.yml`). Use this for release artifacts;
it is up to 45 minutes and it builds whatever branch you name, which is rarely
the one you are debugging.

Record the sha256 and re-verify **after** transferring to each droplet; a
truncated pill fails late and confusingly.

> **Correction (2026-08-06).** This section used to say "not built by this
> repo, and not by hand… there is no local pill build", and §11 repeated it.
> That was wrong, and it was wrong at the time of writing: the Phase 6/7
> campaign ran entirely on `gw-cc-kernel-solid-P67.pill`, built locally by
> exactly the recipe above. What is genuinely broken is the **`+pill/solid`
> dojo generator**; `fyrd`-ing the `solid:pill` *gate* is a different route and
> it works. The distinction cost a cleanroom operator the better part of an
> hour and would have sent them to CI on `gw/next/kelvin/408` — a branch that
> is not the one under review.

### 3.3 the tcp-sidecar

**Do not build this by hand any more.** `tcp-sidecar` ships in the same
release tarball as `gw-vere`, `gw-base.pill`, `comet_miner` and
`gw-onboard`, for all three targets:

```sh
tar xzf groundwire-linux-x86_64.tar.gz    # or -linux-aarch64, -macos-aarch64
# -> tcp-sidecar, alongside gw-vere / gw-base.pill / comet_miner / gw-onboard
```

Both release lanes build it: `groundwire-build.yml` (bleeding-edge,
`groundwire-alpha-*` tags) and `daily-release.yml` (onboarding,
`groundwire-daily-*`), from the SHA pinned in each workflow's
`TCP_SIDECAR_REF`. The Linux binaries are **static musl**, like `gw-vere`,
so a droplet needs no compiler, no `libssl-dev` and no matching glibc.

> **The artifact only exists in releases published after this job first
> ran** — it was added on 2026-08-06 (`gwbtc/urbit` PR #67). Any tarball
> older than that has no `tcp-sidecar` in it; use the manual build below,
> or take a newer release.

CI builds OpenSSL from source with `--openssldir=/etc/ssl` so the
sidecar's `SSL_VERIFY_PEER` finds the host's real CA roots
(`/etc/ssl/certs` on Linux, `/etc/ssl/cert.pem` on macOS). If a host keeps
its roots somewhere else, `SSL_CERT_FILE` / `SSL_CERT_DIR` still override
at runtime. One deliberate loss on Linux: musl has no `backtrace()`, so a
sidecar SIGSEGV still prints its `--- CRASH: signal 11 ---` line — which is
what `ops/gwsup.sh` detects on (§5.9) — but no stack trace after it. The
hand-built binary below keeps the trace.

Building it by hand is still supported, and is what you want if you are
changing the sidecar:

```sh
git clone git@github.com:gwbtc/tcp-sidecar.git
cd tcp-sidecar/sidecar && make          # needs pkg-config + libssl-dev
```

Either way you end up with one binary. Copy it to
`/opt/gw/bin/tcp-sidecar` — **keep that exact name**, because
`ops/gwsup.sh` and `ops/stopship.sh` find the process with
`pgrep -f 'bin/tcp-sidecar'`. It takes exactly one argument, the pier
path, and opens **no listening port** — it is a Unix-domain client onto
the pier's Lick socket at `<PIER>/.urb/dev/tcp/tcp`. Outbound connections
use whatever port the caller asks for (8333, for Bitcoin).

You still need a `gwbtc/tcp-sidecar` checkout for the **desk** (§3.4);
only the binary is in the release.

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

1. Build vere, the pill, the desks; unpack the sidecar from the release
   tarball (§3.3). *(once, on your laptop)*
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
  whose opening lives only in twelve words. A confidential comet **can** be
  published later as a state update (§10), but only its existing peers will
  accept that; publishing at spawn is the only way a stranger can learn it
  from the chain alone.
- **A named sponsor must be one the verifier already knows.** `sponsor-ok`
  (`lib/self-attestation.hoon`) is `(~(has in known-public) u.sponsor…)`, and
  the set handed to the thread by `+verify-cards` is
  `~(key by unv-ids.urb-state)`. A verifier that has not indexed or verified
  the sponsor fails `sponsor-known`; that is classified **unevaluable**, so the
  sponsee reads **UNDETERMINED**, no verdict is emitted, no snub is emitted and
  no point is installed. It is **not permanent**: the identical packet verifies
  VALID the moment the verifier learns the sponsor — measured live on
  2026-08-07, five minutes apart, on the same ship (see
  `doc/live-tests/PHASE2-RERUN-RESULTS.md`).

  This bullet used to say the sponsor had to be **public**, and that naming a
  confidential comet left the sponsee "silently unreachable forever". Measured
  live: false. `unv-ids` also holds comets the verifier has verified
  *confidentially*, and one of those satisfies `sponsor-known` — while the
  agent's own `+known-public` predicate (`app/gw-btc.hoon:2068`), used for the
  writ gate, does subtract `.confidential`. Two definitions of the same name in
  one agent; the peer-facing check uses the broader one. Recorded in §12 as a
  disagreement to settle, not relied on. An **absent** sponsor is fine either
  way — it projects to self and `sponsor-ok` is `%.y`.
- **`--assume-saved` is not optional in a script.** Without it the blind-phrase
  read-back fires a second time *after the transaction has been broadcast*, and
  the run exits 2 with mainnet money already spent. Note also that piping stdin
  does **not** answer prompts: the `isatty()` check fires before `input()`, so
  every prompt must be pre-answered by a flag. One of `--sponsor` / `--no-route`
  is likewise mandatory or the command aborts.
- Full headless operation needs `--utxo`, `--signed-psbt` and
  `--assume-saved`; without them the flow prompts. `prompt()` now aborts
  non-zero on any non-TTY stdin naming the flag to pass, so a piped run
  fails loudly rather than hanging.
- `spawn generate` with no funding still **waits forever**. Not automated.
- Requires ≥ 1000 sats in the funding UTXO (`REQUIRED_SATS`), default fee
  rate 2 sat/vB, `BLOCK_CONFIRMATIONS = 2`.
- **Pick the fee rate against the mempool, not against the default.** A spawn
  is ~111 vB, so the whole fee is 111 × rate and every rate in the plausible
  range is affordable — but the confirmation is on your critical path, because
  the custody log cannot be baked until the spawn confirms and nothing can
  verify you until it is (§6). In the 2026-08-06 run three spawns went out at
  1 sat/vB and sat unconfirmed through two blocks whose own minimums were
  **2.1 and 3.0 sat/vB**; `mempool.space`'s `hourFee` said 1 the whole time.
  Check what recent blocks actually cleared to:

  ```sh
  curl -s https://mempool.space/api/v1/blocks/<tip> \
    | python3 -c 'import sys,json;[print(b["height"],b["extras"]["feeRange"][0]) for b in json.load(sys.stdin)[:5]]'
  ```

  and bid above that. If you are already stuck, `ops/gwmint.py build <label>
  --replace --fee-rate=N` rebuilds the spawn as a full-RBF replacement:
  **the identity is unaffected** — the `dat` commits to the funding outpoint,
  not the spawn txid, so `@p`, life and `Q` are bit-identical and only the
  identity satpoint moves. Safe only before `finalize`, before any custody
  entry, and before any peer has tracked the old satpoint.

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

**Decide first how you will drive the ship**, because it changes the boot
flags and it is not reversible without a restart.

| | tmux, no `-t` | `-t` under a supervisor |
|---|---|---|
| drive it with | dojo, `tmux send-keys` | `conn.sock`, `ops/gwctl.py` |
| survives your ssh session | only while tmux lives | yes |
| supervisable | no | yes |
| use for | poking at one ship by hand | **anything that runs for hours** |

**Use `-t` for a campaign.** Every ship in every live run has. A light-client
bring-up is ~2.5 h and both vere and the sidecar die under load (§5.9), so the
ship has to come back without you; a supervisor cannot restart a ship that only
exists inside your tmux pane. `-t/--no-tty` disables terminal assumptions
(`main.c:905`) and `-d/--daemon` implies it.

```sh
# the campaign form -- see ops/bootcomet.sh, which adds the guards
setsid nohup /opt/gw/bin/gw-vere -t --loom 32 \
  -c <PIER> -p <PORT> -w <COMET_NO_TILDE> -G <FEED> -B <PILL> \
  >> /opt/gw/<name>.log 2>&1 </dev/null &
```

> **Every `>` line in this runbook is notation, not something you can type.**
> A ship booted with `-t` has no dojo. `> :gw-btc &noun [%jael-writ …]` means
> "poke `%gw-btc` with that noun", and a scry written
> `.^(* %gx /(scot %p our)/gw-btc/(scot %da now)/ready/noun)` has to be wrapped
> in a `(strand ,vase)` thread and handed to khan over `<PIER>/.urb/conn.sock`.
> `ops/gwctl.py` is that wrapping, and every `>` line below has a subcommand.

If you do use tmux, the rules that have each cost hours:

```sh
tmux new-session -d -s <SESSION>
tmux send-keys -t <SESSION>:0.0 '/opt/gw/bin/gw-vere -c <PIER> …' C-m
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

- **The supervisor must be a singleton.** Two supervisors on one pier both see
  VERE-DOWN, both relaunch, and the loser's ship dies on
  `mesa: bind: address already in use` — which reads exactly like a crash loop.
  In the 2026-08-06 cleanroom run *all three* droplets were found running two
  `gwsup.sh` instances per pier. `ops/gwsup.sh` now takes an `flock` and a
  second start is a no-op.

`gwsup.sh` and `poolfill.py` are in **`ops/`** (see `ops/README.md`).

### 5.10 Stopping a ship

There was no shutdown procedure in this runbook until 2026-08-06, and its
absence has already caused one incident: three ships were believed stopped,
were handed to a new operator as stopped, and were in fact running — because
**stopping a supervised ship without stopping its supervisor first is a
no-op**. `gwsup.sh` notices VERE-DOWN within one 30 s poll and relaunches.

Order matters. Use `ops/stopship.sh <name>`, which does:

1. **supervisors first** — there may be more than one; match on the exact
   `gwsup.sh <name> <port>` argv, not a prefix.
2. **then the runtime** — `SIGTERM` the king, matched by *exact* final argv
   field. The serf exits with it, the event log is durable, and the pier
   replays on next boot, so this is a clean stop with no state loss.
3. **then the sidecar** — identified by `/proc/<pid>/cwd`, because it has no
   port and no distinctive argv.

Never `pkill -f urbit`, and never match a pier by `pgrep -f` prefix. A stopped
pier is preserved in full; nothing here deletes anything.

---

## 6. Verify a peer end to end

Preconditions on the verifier: `/x/ready` reports `synced=%.y`, and its tip
covers the peer's evidence.

Get the peer's **live jael pass** — not the artifact's `pass_atom_hex`,
which is a different, shorter object (108 B vs ~330–405 B for the jael
pass). Read it off the running ship via jael's `%pynt` → `keys` → `pass`
(`ops/gwctl.py pass <PIER>`).

> **The pass length is your custody-log signal, and it is the cheapest one
> you have.** A ship booted from the miner's feed — the un-baked one — serves
> a **108-byte** pass, because the ring carries no `xtr`. It looks completely
> healthy: right `@p`, right life, agents installed, `/x/ready` fine. But it
> has no custody evidence, so every peer that verifies it gets nothing to
> check and no peer can ever install it. After `causeway finalize` bakes the
> log in, or after the `%gw-custody-entry` poke lands on a running ship and
> logs `custody log verified (N entries); refreshing our pass`, the same scry
> returns ~330–405 B. **If you are about to debug "nobody can verify my
> comet", check the length first.** 108 means you skipped §5.2's finalize
> step, which is exactly what happens if you boot before the spawn confirms.

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

**First, check whether block headers have actually finished.** Filter headers
sitting at 1 while block headers climb is **normal and expected** —
`+continue-syncing-headers` (`bitcoin-client.hoon:1015-1032`) does not request
a single filter header until `block-headers-are-synced` is true. During the
whole ~55-minute block-header phase you will see exactly this:

```
[%headers 608.001]
[%filter-headers 1]
```

and there is nothing wrong. A cleanroom operator following the old wording here
would have torn down a perfectly healthy 139-peer set at the halfway mark.

It is a real symptom **only once `%headers` has reached the chain tip and
`%filter-headers` is still 1.** Then your peers cannot serve compact filters:
`++init` requires `node-compact-filters` and unfiltered DNS seeds mostly do not
have it. Re-seed from `x49.`-prefixed seeds (§5.6). This killed an entire test
run.

A quick way to tell the two apart without waiting: `++peer-services-are-sufficient`
*requires* `node-compact-filters`, so any peer in `[%live-earth-peers N]` has
already advertised it. A healthy N means the pool is fine and you are simply
still in the block-header phase.

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

**Check the peer's pass length first (§6).** If it is 108 bytes the peer has
no custody log, and `%jael-writ` drops it through `+public-pass` —
**silently, with no log line and no verdict**. That is the correct *verdict*
behaviour (a suite-C pass with no xtr is the public-onboarding shape, and
judging it negatively would snub an honest comet the block scanner will
resolve later), but it produces no diagnostic whatsoever, and it is exactly
what you get if you booted before the spawn confirmed.

Then check `/x/ready`. If `synced=%.n` the writ is being *held*, and that one
**is** announced (`writ from … held: light client NOT synced`). Check
`/x/inflight`: a stranded single-flight slot silences that peer for up to
`~h2`. Check `/x/pending-own` for the `%anew` equivalent.

> **"Every drop is announced" became true in `25f0a1d`, and was false before
> it.** On `bf90840` and earlier, four `%jael-writ` exits were completely
> silent — an undecodable pass, a decoded-but-empty custody log, a log over the
> 1.024 cap, and the shared public-onboarding/foreign-kelvin return — and
> **three of those four emit a sticky ames snub**. An operator watching a peer
> get blacklisted saw exactly what an idle ship looks like. Measured live
> 2026-08-06 (a 108-byte pass: zero log lines) and again 2026-08-07, where
> determining snub-or-not for each of them required clearing the snub set and
> re-poking one case at a time.
>
> Since `25f0a1d` every exit goes through `+drop-writ`, which derives the line
> and the cards from one `$writ-drop` value, and the verb says which it is:
> **`dropped`** (harmless), **`held`** (readiness, clears by itself),
> **`REFUSED`** (a negative verdict — jael `%fail`s and ames snubs). All five
> re-verified live on 2026-08-07. If a writ vanishes with no line at all, the
> desk predates `25f0a1d`.

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

- **A destroyed blind destroys a confidential identity.** See below.
- **A destroyed blind destroys a confidential identity.** `+verify-dat`'s
  `=(d.u.psd (spawn-commit spawn.open blind.open))` cannot be satisfied
  without the blind; brute force is a 2^256 search. A **public** comet's
  opening is in its OP_RETURN and recovers from the chain alone with no
  secret material. This is the property, not a bug.
- **Reorg recovery is a halt, not a repair.** See §9.

### What *is* repairable, contrary to earlier drafts

**A confidential comet CAN become public.** This section used to assert the
opposite, as a permanent design property, on the grounds that `+apply-spawn`
is the only writer of the public index and requires spending the original
funding satpoint. Both halves are wrong:

- `+apply-spawn` is **not** the only writer. `+apply-state` writes the index
  through `+index-point` (`lib/urb-core.hoon:353` → `:376`), and the
  confidential verifier writes it directly (`app/gw-btc.hoon:1919`) — which is
  precisely why a confidential comet is *in* `unv-ids` at all.
- The funding-satpoint guard (`lib/urb-core.hoon:311`) is real but belongs to
  `+apply-spawn`, which a tracked comet never reaches:
  `+process-publication` (`lib/urb-core.hoon:256-262`) routes any ship already
  in `unv-ids` to `+apply-state` first.

This is **Tier 1** in `doc/opret-revision/04-decisions-addendum.md:154`: a
publication whose subject the scanner already tracks is a *state update*,
whatever the shape of its opening. Input-0 continuity from the sat we already
follow is the ownership proof. It shipped in `d63e28a`; the runbook commit
landed after it and reproduced the superseded Phase-5b conclusion.

The transaction is one input and (at least) two outputs: input 0 spends the
comet's currently tracked identity satpoint; the sat-carrying output commits a
snapshot whose `life` **strictly** exceeds the life peers hold; and an
OP_RETURN carries `[pass opening]` jammed behind `6a 03 'urb' 01 09`, ≤512
bytes. Every peer that has ever verified this comet accepts it, emits
`[%point who %public ~]`, drops it from `.confidential`, and logs
`published itself on chain; now PUBLIC, permanently`.

Two caveats the test plan does not state:

- **Only peers that already track the comet.** A scanner that has never
  verified it is a stranger and refuses at `lib/urb-core.hoon:273`
  (`state-update publication for a comet we do not track`). Admitting a
  publication from a stranger is Tier 2, which is specified and **not
  implemented**.
- **Causeway cannot build it.** There is no `publish` subcommand and no
  `--publish` on `rekey`; `build_rekey_psbt` accepts
  `publication_pass_atom`/`publication_opening` but every caller passes
  neither. Use `ops/gwmint.py`.

Watch `/x/confidential` for the ship leaving the set — **not**
`/x/publicizing`, which is an unrelated re-entrancy latch for the public-spawn
replay race and never mentions declassification.

---

## 11. Not automated

Stated plainly so nobody hunts for a script that does not exist:

- **No deploy script.** The desk install in §5.4 is manual (but see
  `ops/gwctl.py desks`, which does it).
- **No fully headless Causeway spawn without funding.** `spawn generate`
  with no funded UTXO waits forever rather than timing out.
- **Causeway cannot commit a `fief`, and cannot publish anything except at
  spawn.** No `--fief` anywhere; `rekey` carries the prior fief forward and
  offers no way to set one. Since the sponsorship topology of Phase 4 requires
  the sponsor to commit a real fief, that transaction has to come from
  `ops/gwmint.py`.
- **No CI build of the `%tcp-sidecar` and `%node` desks.** Only the sidecar
  *binary* is released (§3.3). Both desks are checked-in Hoon needing no
  build step, but you still clone `gwbtc/tcp-sidecar` and `gwbtc/node` to get
  them, and `gwbtc/node` is **private**, so an outsider cannot complete an
  install from public artifacts alone. Making it public is in progress and is
  not gated on anything here.

**No longer true — these are now in `ops/`** (see `ops/README.md`): the
supervisor `gwsup.sh`, the peer-pool tool `poolfill.py`, the minting path
`gwmint.py`, the operator surface `gwctl.py`, and a local pill build (§3.2).

**No longer true — the sidecar binary is now a release artifact.** §3.3 used
to be a mandatory hand-compile on every host, which is why `libssl-dev` and
`pkg-config` were droplet prerequisites in §2. `gwbtc/urbit` PR #67 added a
`build-sidecar` job to both `groundwire-build.yml` and `daily-release.yml`,
so `tcp-sidecar` now ships statically linked in every
`groundwire-<platform>.tar.gz` — for releases cut after 2026-08-06.

**Fixed since this runbook was written:** `causeway.py` and `gw-onboard.py`
used to compute `f"{arch}-{os_name}-none"` for the zig output directory,
yielding `x86_64-linux-none` — a triple zig never emits — so on Linux the
defaults could never resolve and `--vere`/`--miner` were mandatory with no
hint. Both now probe the triples zig actually emits (`…-linux-musl`, then
`…-linux-gnu`; `…-macos-none` on Darwin) and, on a miss, print every path
they looked at. The flags still override.

---

## 12. Where the docs and the code disagree

Recorded rather than quietly resolved. In each case this runbook follows
the code.

| claim | where | reality |
|---|---|---|
| light-client sync is "~8 min, ~850 MB pier" | `doc/opret-revision/05-live-test-plan.md` Phase 0 | ~2.5 h, ~2.2 GB. Measured twice. |
| `%gw-btc` scries `%light-client` | scattered comments, older results docs | the agent is `%bitcoin-client`; `%gw-btc` reaches it through `++light-client-agent:lca`. Fixed as Phase 2 finding B5. |
| `causeway` has a `mine` subcommand | folklore | it does not. Mining happens inside `spawn generate`/`spawn connect` via the external `comet_miner` binary (`--miner`). |
| the pier liveness signal is `<PIER>/.urb/log` mtime | early briefs | inert; measured 21 h stale on a live ship. Use `<PIER>/.urb/log/*/data.mdb`. |
| "there is no local pill build" | **this runbook**, §3.2 and §11 | false, and false when written: every campaign has run on a locally built solid pill. `+pill/solid` the *generator* is broken; `fyrd`-ing the `solid:pill` *gate* is not. Rebuilt from `hd/cc-kernel@de3222d36a` in ~6 min on 2026-08-06. §3.2 rewritten. |
| "a confidential comet cannot become public" | **this runbook**, §10 and §5.2 | false since `d63e28a`. `+apply-state` and `+apply-verified` both write the index; `+process-publication` routes a tracked comet to `+apply-state` and never reaches the funding-satpoint guard. §10 rewritten. |
| boot into tmux and drive the dojo | **this runbook**, §5.3 | no live ship has ever run that way. Campaign ships run `-t` under a supervisor and are driven over `conn.sock`; the `>` lines are notation for a khan-eval. §5.3 rewritten, `ops/gwctl.py` added. |
| — (nothing said) | **this runbook**, everywhere | there was **no shutdown procedure at all**, and stopping a supervised ship without stopping its supervisor first is a no-op. Directly caused an incident in which three running ships were handed over as "stopped". Added as §5.10. |
| naming a **confidential** comet as a sponsor makes the sponsee permanently UNDETERMINED | **this runbook**, §5.2 (added 2026-08-06) | false, measured live 2026-08-07. `+verify-cards` hands `+run-checks` the **raw** `~(key by unv-ids.urb-state)`, which includes confidentially-verified comets, while `+known-public:gw-btc` — same name, same agent, two arms away — subtracts `.confidential`. A confidential sponsor satisfies `sponsor-known`. And UNDETERMINED is never permanent: it clears the moment the verifier learns the sponsor. §5.2 rewritten; the two definitions still need reconciling. |
| a snub is undone by a later positive verdict | folklore | it is not. `+sy-sybl`'s `%full` branch (`sys/vane/ames.hoon`) installs the point and never touches `ships.snub`; only `%fail` writes it. Verified live 2026-08-07: a comet re-verified VALID with its snub intact. Only an ames `%snub %deny %del` clears it — `ops/gwsnub.py`. |
| `tracked-prefix` failing means "a fork, not an old copy" | `lib/self-attestation.hoon`, `+stale-checks`' comment | `+prefix-chain` cannot tell the two apart: it answers `%.n` both when the logs *diverge* (a fork) and when the new log is a **strictly shorter identical prefix** (an old copy). So a comet replaying its own genuine earlier attestation — which a reboot from the un-refreshed boot feed produces — is classed fraud and snubbed, alongside three stale-class checks in the same verdict that all say "old copy". Reproduced on two verifiers 2026-08-07. |
| `rekey` is the only on-chain management op | §12, below | true, and it is not enough: Causeway can commit **no** `fief` at all, and can publish only at spawn. The Phase 4 sponsorship topology therefore cannot be built with Causeway. `ops/gwmint.py` does both. |

### Reconciled since

These rows were live disagreements when this runbook was written; the docs
have since been corrected to match the code, and are kept here only so the
old claims are recognisable if they resurface.

| former claim | now says |
|---|---|
| build vere with zig **0.14.1**, output at `zig-out/x86_64-linux-none/urbit` (`onboarding/booting/README.md`) | zig **0.15.2**, and a per-host table of the triples zig really emits (`…-macos-none`, `…-linux-musl`). |
| pill is built by hand with `+pill/solid` (`onboarding/booting/README.md`) | the `+pill/solid` route is marked known-broken (`mint-vain` in `/sys/vane/ames/hoon`); the documented route is `brass:pill` via `fyrd` in `gwbtc/urbit` CI, landing in Clay at `/pill/pill`. |
| the repo runs an agent called `%urb-watcher` (repo `README.md`) | `%gw-btc` (verifier + block scanner) and `%urb-snapshot`, per `desk.bill`, with `%urb-watcher` named as the retired alias. |
| Causeway supports `escape`, `adopt`, `detach`, `fief`, `set-mang` (`causeway/README.md`, `causeway/docs/OPERATIONS.md`) | `rekey` is the only on-chain management op. `causeway/docs/OPERATIONS.md`, which described the retired commit+reveal protocol and a non-existent `src/protocol/encoder.ts`, has been **deleted** in favour of this runbook. |
| `causeway/desktop/README.md` documents the CLI | it now lists every subcommand, including `proof show` / `proof verify`, and the `--utxo` / `--signed-psbt` / `--assume-saved` flags that make a scripted spawn possible. |
| `causeway/README.md` cites `tests/dat.spec.ts` | it cites `tests/kelvin9.spec.ts`, where the `dat` vectors actually live. |
| the repo `README.md` sent users to `groundwire.dev` while the code printed `https://groundwire.io/causeway/boot.sh` (`causeway/desktop/causeway.py`) | **`groundwire.io` is correct** (owner, 2026-08-06). The README now says `groundwire.io`. Note that `alpha.groundwire.dev` is a different, live endpoint — the RPC proxy and faucet — and is unaffected. |

---

## See also

- `doc/live-tests/PHASE*-RESULTS.md` — what was actually tested, and every
  failure, with evidence.
- `doc/opret-revision/05-live-test-plan.md` — the test matrix this runbook
  supports.
- `doc/opret-revision/04-decisions-addendum.md` §8 — the `%bitcoin-client`
  API contract, including the two byte-order and subscription traps that
  were each fatal against a real node.
