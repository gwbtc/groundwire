# %groundwire: the confidential-comets verifier agent

Status: **draft** (branch `cyc/groundwire-agent`)

Companion to the kernel-side spec in **gwbtc/urbit**,
`pkg/arvo/doc/spec/confidential-comets.md` (branches `cyc/cc-draft` /
PR #57). Read that first — this document only covers the agent half of
the protocol and assumes the kernel task/gift API it defines.

## 1. Role

The kernel treats on-chain identity verification as a pluggable **PKI
domain**: a local Gall agent, registered with Jael, owns all chain
knowledge for one domain. `%groundwire` (renamed from `%urb-watcher`)
is that agent for the `%groundwire` domain on Bitcoin.

Domain and agent are **1:1 by construction** (kernel spec §2.2): the
domain name *is* this agent's name. A confidential comet commits the
tag `%groundwire` in its key tweak; Jael derives the same tag from the
gall duct our `%anex` arrives on. Neither side names the other
directly — they meet at the shared tag.

The agent keeps its pre-existing job (watch Bitcoin blocks on a timer,
parse Groundwire PKI events, feed Jael `%azimuth-udiffs`) and gains
three responsibilities:

1. **Register** the domain with Jael on boot (`%anex`).
2. **Verify** comet self-attestations Jael forwards (`%jael-writ` →
   `%writ-response`).
3. **Refresh** our own comet's attestation on request (`%jael-anew` →
   `%anew-response`).

## 2. Protocol flow

```
confidential comet ~zig          our ship
  |  suite-%c self-attestation      |
  |  (pass tweak commits            |
  |   %groundwire + spawn-satpoint) |
  |-------------------------------->|  ames +on-hear-open / +al-take-proof
  |                                 |    unknown / newer life?
  |                                 |--%writ %groundwire ~zig pass--> jael
  |                                 |         |  dos lookup: %groundwire
  |                                 |         |--%jael-writ poke--> %groundwire
  |                                 |         |                       (this agent)
  |                                 |         |<--%writ-response fact--  verify
  |                                 |         |  store point; %public-keys
  |                                 |<--%sybl (/sybl)--|  give verdict to subs
  |                                 |  +sy-sybl: %full -> promote ~zig
```

`%anex` registers us and makes Jael `%watch` our `/writs` path. We give
three fact marks on that path, and Jael routes on the mark:

| fact mark | meaning |
|---|---|
| `%writ-response` | a verdict for a `%jael-writ` (`[dom ship (unit point)]`) |
| `%anew-response` | a fresh pass for a `%jael-anew` (`[dom pass]`) |
| `%azimuth-udiffs` | ongoing chain updates (unchanged from before) |

## 3. Verification (`+verify-writ`)

`%jael-writ` carries `[dom=%groundwire who=@p pass]`. We:

1. Decode the pass; assert suite `%c`; extract the tweak-committed
   domain (`+rub` over the head of `dat.tw.pub`) and confirm it equals
   `dom`. This mirrors Ames's `+pass-pki-dom` and forbids cross-chain
   double-boot.
2. Re-derive the comet's name from the tweaked key
   (`fig:ex:(com:nu:cric ...)`) and confirm it equals `who`. Ames and
   Jael already checked this; we re-derive rather than trust.
3. Look `who` up in our **own indexed view** of the chain
   (`unv-ids` in `urb-state`, the map our block-watcher maintains).
4. Confirm the attested messaging key is the one we've indexed as
   current (the latest committed state). If so, project the urb
   `$point` onto Jael's `$point` and answer `%full`; otherwise `~`.

### 3.1 Trust model and the indexed-view shortcut

The kernel spec (§7) describes full **variant-B** verification: walk
the comet's off-chain custody log (`[txid block-height reveal]`
entries in the pass's `xtr` data), fetching each tx from a txindexed
Bitcoin node, tracking the ownership sat spawn→tip, and interpreting
the latest committed **state** (commitments carry states, not events —
kernel spec §2.1 / §8). Pure custody transfers carry no reveal and
just advance the sat.

This agent currently takes the **indexed-view shortcut**: rather than
re-walk the log per attestation, it consults the point its
block-watcher already derived from the same chain. This is the same
trust model the spec names ("the agent's own view of the chain") and
is sufficient whenever the attesting comet has already been indexed —
the common case, since sponsors and hubs surface comets on-chain, and
the watcher tracks every protocol sat. It is also exactly what the
Aqua tests drive (§4).

**Not yet wired** (marked `XX` in `on-poke`): the tx-fetch custody walk
for a comet *not yet in our index*, or attesting a state *newer than
we've indexed*. In production this falls back to the full §7 procedure
against the configured node; here it declines (`~` → `%fail`), and the
comet re-attests once the watcher catches up. Wiring the fetch path is
the main follow-up.

## 4. Testing (Aqua)

The existing `ph/gw` Aqua suite (hi, key-cycle, sponsor variants)
injects PKI udiffs *directly into Jael* via the trivial `%gw` feed
agent, so comets are already `%known` before any packet — the
`%writ`/`%sybl` path is never exercised. The confidential-comet test
(branch `cyc/groundwire-aqua`, and the companion tests added to
gwbtc/urbit's harness) instead:

- boots a suite-`%c` comet whose tweak commits `%groundwire` + a
  spawn satpoint,
- registers a `%groundwire` verifier agent on the *receiving* ship and
  seeds its `unv-ids` with the attesting comet's point (simulating a
  prior block-index), **without** telling Jael,
- lets the comet self-attest, so the receiver's Ames sees an unknown
  suite-`%c` comet and drives the real `%writ` → `%groundwire` →
  `%writ-response` → `%sybl` → promotion loop,
- asserts promotion (Jael now holds the point; a `|hi` succeeds), and
  the `%fail` path (a corrupt/mismatched attestation is snubbed).

See that branch's report for results.

## 5. Open items

- Wire the variant-B tx-fetch custody walk (§3.1) for un-indexed /
  newer-than-indexed attestations.
- `%jael-anew` currently returns our indexed pass verbatim; once the
  watcher tracks our own `xtr` reveal log it should extend it per the
  kernel spec's `%anew` dual.
- Decide the watch-path story if a ship hosts more than one PKI domain
  agent (kernel currently tracks one domain per desk via `%tire`).
