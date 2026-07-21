# `%gw-btc`: confidential-comet verification

Status: **draft implementation** (branch `cyc/groundwire-agent`)

This is the userspace companion to
`gwbtc/urbit/pkg/arvo/doc/spec/confidential-comets.md`. The desk is
`%groundwire`; the Gall agent and PKI-domain tag are both `%gw-btc`.

This deployment starts after a network breach, so `%gw-btc` has no
`%urb-watcher` state migration. Its state is deliberately unversioned. The
team operating the network is responsible for rebooting and snapshotting the
default sponsor.

## 1. Packet format

A Groundwire comet uses crypto suite `%c`. Its pass has two independent
payloads:

```hoon
dat  = (cat 0 q:(mat %gw-btc) (jam spawn-sont))
xtr  = (jam custody-log)

+$  custody-entry
  $:  txid=@ux
      height=@ud
      reveal=(unit reveal)
  ==

+$  reveal
  $:  internal-key=@ux
      tapleaf=[version=@ux script=hexb]
  ==
```

The current wire protocol requires `tapleaf.version == 0xc0` (BIP-342
Tapscript). Other leaf-version bytes are rejected even when their algebraic
TapTweak matches the claimed output key.

`dat` is included in the suite-C key tweak. It permanently binds the comet's
name to `%gw-btc` and to the spawn satpoint. `xtr` is not part of the tweak, so
the custody locator log can grow without changing the name. A reveal-less log
entry is a pure custody move.

The current Groundwire encoding inside a revealed Taproot leaf is the same
event/delta-shaped encoding consumed by `lib/urb-core`. Verification replays
those deltas in custody order. This differs from the earlier kernel-spec draft,
which described every reveal as a complete state; the wire implementation is
authoritative for this pass, and unsupported deltas fail closed.

## 2. Kernel interaction

On initialization `%gw-btc`:

1. sends `[%anex /writs]` to Jael, registering the sending app name
   (`%gw-btc`) as its PKI domain;
2. watches local `%light-client` at `/best-block`; and
3. waits for the initial `%urb-start-indexing` snapshot.

Both the snapshot and the first light-client best-block fact are readiness
gates. A `%jael-writ` received before either one is available is retained and
launched later.

```text
suite-C open packet
  -> Ames extracts %gw-btc from dat
  -> Jael %writ
  -> %gw-btc %jael-writ poke
  -> asynchronous %light-client reads + pure verification
  -> %writ-response fact on /writs
  -> Jael stores point and gives %sybl %full/%fail
  -> Ames promotes or snubs the peer
```

An exact duplicate `%anex` from the same app and path is idempotent and
re-emits Jael's retained watch. This matters after a true app nuke: Gall kicks
the old watch, the attempted re-watch can receive a negative acknowledgement
while the app is absent, and the restarted app's `+on-init` then restores it.
A duplicate that changes the path is still rejected as a conflicting
registration. Jael does not replay old `%writ`s or request new attestations
from every peer. A true nuke also deletes `%gw-btc`'s private confidential
index, which public snapshots intentionally omit; recovery therefore requires
peers to re-attest (or a future private-state recovery mechanism).

Holding and reviving a desk is different from nuking an app. Clay's retained
`%tire` subscription maps those transitions to Jael's `%gost`/`%ghul`
liveness behavior, and the Gall watch remains installed; `%tire %live` must
therefore not create a duplicate watch. `%ghul` only unsnubs the domain's known
peers. A new suite-C packet is what starts another `%writ`.

## 3. Verification

Before starting asynchronous work the app:

- requires domain `%gw-btc` and a canonical suite-C `dat`/`xtr` decode;
- recomputes the comet name from the pass;
- rejects custody logs longer than 1,024 entries;
- rejects a point already known through the public indexing path; and
- applies bounded queues (1,024 pending, 16 concurrent), with the latest pass
  winning for each ship.

`lib/lc-attestation` resolves the vector through the local `%light-client`
interface:

| Path | Use |
|---|---|
| `/transaction/<txid>` | fetch the raw transaction containing the immutable spawn satpoint |
| `/block-hash-by-height/<height>` | resolve each claimed canonical height |
| `/transaction/<block-hash>/<txid>` | ask the backend for a raw transaction scoped to the asserted block |
| `/tx-out/<txid>/<vout>` | ask the backend whether the exact output is currently unspent and has the expected value/script |
| `/best-block` | readiness and chain-progress feed |

These calls are an adapter trust boundary, not an SPV proof in this desk. In
particular, the block-qualified transaction endpoint supplies no Merkle proof,
Bitcoin transaction IDs do not commit witness data, and `/tx-out` is a backend
UTXO assertion. Production safety therefore depends on the local
`%light-client` validating its chain view and authenticating these answers; the
test adapter injects fixed vectors at the same boundary.

The pure verifier then checks, in order:

- the first entry reveals the `%spawn` that matches `dat`;
- transaction IDs, input-zero custody continuity, sat offsets, and
  nondecreasing heights;
- key-path witnesses for custody spends after the initial transaction (which
  spends the arbitrary spawn precommit output), and a P2TR previous output for
  each such key-path spend;
- single-leaf Taproot output-key reconstruction for every reveal;
- ordered replay of the revealed Groundwire deltas;
- consistency with any point already tracked by the block watcher;
- a P2TR final output that the backend reports unspent at the current
  light-client view; and
- equality of the submitted and replayed key material while intentionally
  ignoring `xtr`.

Malformed data, unknown UTXO status, unsupported deltas (currently including
`%set-mang`), and mold-valid negative verifier results fail closed. A missing
light-client fact or generic thread crash has no trustworthy permanent/transient
classification yet: an in-strand deadline terminates the Khan computation and
tears down its light-client watches, while an app-level backstop releases the
request and in-flight slot. Neither sends Jael a fraud verdict, allowing a
later packet to retry.

On success the verified point is inserted into the private Groundwire index
and marked confidential. Confidential points are filtered out of classic
`%azimuth-udiffs`, snapshots, and public scries so their state reaches peers
only through self-attestation. Declassification is deliberately narrow: only
an on-chain `%owner` effect that introduces a ship absent from the scanner's
base is treated as a public-spawn race. A later effect for an already-tracked
confidential point is not enough to prove public provenance and does not
declassify it.

## 4. State and `%anew`

The unversioned state contains the existing RPC/index state plus:

- snapshot and light-client readiness;
- latest pending and in-flight verification requests;
- the confidential-ship set; and
- the last attested tip for each verified ship;
- monotonic verification job IDs plus chain/context epochs that reject stale
  asynchronous results; and
- a transient public-spawn retry guard used while a conflicted block batch is
  replayed from a sanitized base.

`%jael-anew` is intentionally conservative. `%gw-btc` answers only when its
own ship is confidential, indexed, and still at the exact tip that was last
verified; the sponsor fallback in the projected Jael point is the ship itself.
The app does not yet discover and append new custody entries for its own pass,
so automatic self-refresh remains follow-up work.

## 5. Test boundary

The deterministic pure-verifier tests inject fetched transactions and final
UTXO status immediately below `lib/lc-attestation`. They cover valid
spawn/custody flows, pure moves, key rotation, malformed and tampered reveals,
height and continuity failures, tracked-tip reconciliation, unknown/spent
tips, and fail-closed replay.

Adapter tests drive the actual `lib/lc-attestation` khan shed, assert each
`%light-client` watch path, and inject fixed watch-ack/fact/kick responses. They
cover the complete successful endpoint sequence, height and transaction-ID
mismatches, and spent/unknown final outputs without a Bitcoin node.

Agent tests cover initialization, registration, unversioned save/load,
readiness queuing, latest-request-wins behavior, bounded concurrency,
timeout/crash slot release, confidential snapshot filtering, successful state
application ordering, `%anew` freshness, and the no-sponsor fallback. They
verify that the light-client strand and app-level deadline are launched
together.

The Aqua tests use suite-C Groundwire comet fixtures and a deterministic
`%gw-btc` verifier stub. The stub delays its `%writ-response` through Behn, so
the test exercises the real Ames -> Jael -> Gall -> Jael -> Ames asynchronous
path without a Bitcoin node. A future end-to-end layer should replace the stub
with `%light-client` and a sidecar `bitcoind` in regtest mode.

## 6. Remaining decisions

- Decide whether committed leaves will remain Groundwire deltas or change to
  full-state attestations; update both specs and encoders together.
- Distinguish permanent missing-transaction answers from transient
  `%light-client` failures.
- Define automatic `%anew` triggering and custody-log extension for the local
  comet.
- Add `%light-client` + Bitcoin-regtest end-to-end coverage, including reorgs.
- Decide whether cross-identity child mutations should ever be supported;
  the current verifier rejects them explicitly and accepts only self-targeted
  `%adopt`, `%reject`, and `%detach` mutations.
