# Confidential Comets — live mainnet test plan

Status: executing, 2026-08-04. Real mainnet BTC, real ships, three
DigitalOcean droplets. Each test states what it proves and what a
FAILURE would mean, so a red result is actionable rather than just red.

Legend: **[N1]/[N2]/[N3]** = droplet 1/2/3, **[M]** = the Mac,
**[chain]** = requires an on-chain transaction (costs sats).

## Phase 0 — bringup (prerequisite, not a test)

**The procedure is now written down: [`../OPERATIONS.md`](../OPERATIONS.md).**
Follow that rather than this paragraph — it has the verified commands, the
known-good timings and the traps.

Deploy the cross-compiled x86_64 vere, the kelvin-408 pill, the
groundwire desk, the node light-client desk and the tcp sidecar to all
three droplets; boot a ship on each; sync each light client from
genesis to mainnet tip. Each verifying ship needs its OWN light client.

Exit criteria: three live ships, each reporting mainnet tip height and
a running `%gw-btc`.

*Two corrections to the original text, both measured since:* full
light-client bring-up is **~2.5 h** (~55 min of block headers, ~95 min of
filter headers) and a synced pier is **~2.2 GB** — not "~8 min, ~850 MB";
and the light client is the `%bitcoin-client` agent of the `%node` desk,
not a `%light-client` agent (`%gw-btc` reaches it through
`++light-client-agent:lca`). Start the sync first, in parallel across all
machines.

## Phase 1 — mint identities [chain]

Three funding UTXOs (2000 sats each) → three comet identities.

| # | Test | Notes |
|---|---|---|
| 1.1 | Mine three suite-C comets under the kelvin-9 hiding `dat` | dat recomputed per candidate seed; blind = H_tag('gw/spawn-blind', seed) |
| 1.2 | Confidential spawn for C1, C2 (no OP_RETURN output) | single tx, no reveal |
| 1.3 | **Public** spawn for S (the sponsor), WITH the OP_RETURN publication output | proves the publication encoder against a real node |
| 1.4 | Confirm each spawn: output 0 scriptPubKey == `5120||state-key(P, snapshot)` | the commitment the verifier will recompute |
| 1.5 | The public scanner indexes S from its OP_RETURN alone, with no packet | proves `find-publication`/`apply-spawn` on live chain data |

## Phase 2 — verification correctness (single node, no networking)

The adversarial matrix. Cheap, fast, no networking: feed attestation
packets straight to `%gw-btc` and read the verdict. This is where most
of the safety surface lives.

| # | Test | Expected |
|---|---|---|
| 2.1 | Genuine attestation for C1 | VALID, point installed |
| 2.2 | Tampered tip satpoint | INVALID (tip/sont check) |
| 2.3 | `who` ≠ fig(pass) | INVALID |
| 2.4 | Life regression across openings | INVALID (life-order) |
| 2.5 | State-key commitment doesn't match the on-chain output | INVALID (commitment) |
| 2.6 | blind-opening doesn't open the `dat` commitment | INVALID (spawn-commit) |
| 2.7 | Custody log > 1024 entries | rejected (bound) |
| 2.8 | Tip already spent | tip-unspent fails → INVALID |
| 2.9 | Wrong kelvin byte in the pass | ignored as a foreign protocol version, NOT mis-parsed |
| 2.10 | Continuity not through input 0 | INVALID |
| 2.11 | Snapshot names an unknown sponsor | sponsor-known fails |
| 2.12 | Canonically-empty chain `(jam ~)` | negative verdict |
| 2.13 | `xtr=0` public onboarding packet | SILENCE, not a negative verdict |
| 2.14 | Malformed (non-mat) `dat` | dropped in ames before Jael is involved |
| 2.15 | Duplicate writ while one is in flight | dropped silently (single-flight) |
| 2.16 | Same-block spend of the tip | detected by the filter scan (scan starts at tip height) |

## Phase 3 — kernel gating and real networking [N1][N2]

| # | Test | Expected |
|---|---|---|
| 3.1 | Two attested comets exchange `\|hi` both ways | success — the headline test |
| 3.2 | Unattested suite-C comet contacts an attested one | held pending verdict; NO communication |
| 3.3 | Comet with an invalid attestation | rejected, suspended, no channel |
| 3.4 | Prove the Groundwire path was actually used | `%writ`→`%sybl` fired; compare against a suite-B comet's path |
| 3.5 | Invalid attestation does NOT silently degrade to vanilla-comet networking | **the key safety invariant** |
| 3.6 | Attested comet talks to a vanilla (suite-B) comet | characterize: allowed, and by which path |

## Phase 4 — sponsorship [N1][N2][N3]

**Revised after Phase 3.** Phase 3 established that mainnet stars do **not**
relay comet↔comet packets: each comet reaches its own sponsor, but an
attestation request never arrives at the far comet, and the direct C1↔C2
leg only worked behind a transport-only DNAT hack. That is precisely the
problem sponsorship exists to solve — so Phase 4 is not merely a feature
test, it is the real fix for what Phase 3 had to work around.

Consequences for setup:
- *(Superseded 2026-08-05: C3 now commits `fief=[%if .64.227.13.22 34343]`
  at life 2, tx `8e713009…` block 961129, and C1 commits `sponsor=~ligdes-…`
  at life 2, tx `a8553a3a…` block 961130. The paragraph below describes the
  state that made those two updates necessary.)*
- **All three comets carried `fief=~`**, so none was reachable by
  a peer that doesn't already have a lane. The sponsor must commit a real
  fief. That needs one on-chain state update on C3
  (`fief=[%if <droplet-ip> <ames-port>]`), and C3 must then be booted on
  that exact IP with that exact port pinned (`-p`).
- C1 must name C3 as sponsor, which is committed on-chain — another state
  update (`sponsor=~ligdes-…`). Both are ~111 sats; budget is ample.
- Because life must increase on every snapshot change, each state update
  also exercises re-verification at the new life (Phase 5.1/5.2 for free).

This **subsumes the 3.1 retest**: if C1 and C2 can reach each other via C3
with no DNAT, the unresolved C1→C2 asymmetry was an artifact of the DNAT
routing cycle. If it persists over clean sponsor routing, the
`+on-hear-keys` `via = sndr` / `chums`-vs-`peers` hypothesis is real and
becomes a kernel bug to chase.


| # | Test | Expected |
|---|---|---|
| 4.1 | C1 commits `sponsor=S`, attests to S | S recognizes itself named; policy runs |
| 4.2 | S accepts | C1 in `/x/sponsees`; S installs point + lane |
| 4.3 | C2 routes to C1 **via S** (C1 has no fief) | forwarding works; the sponsor-mediated topology |
| 4.4 | `%gw-sponsor-decline` C1, C1 re-attests | SILENCE — no verdict, no snub, `/x/declined` shows C1 |
| 4.5 | Declined C1 re-attests repeatedly | short-circuits before verification (cheap retries) |
| 4.6 | `%gw-sponsor-clear` C1, C1 re-attests | back to normal, sponsored again |
| 4.7 | S's on-chain fief is used to reach S | fief routing from the committed snapshot |
| 4.8 | A comet naming S that never attests to S | never registers; peers routing via S fail (self-announcing) |

## Phase 5 — state changes and re-attestation [chain]

| # | Test | Expected |
|---|---|---|
| 5.1 | Rekey C1 (state-update tx, life++) | new snapshot committed on-chain |
| 5.2 | Peers re-verify at the new life | new key installed, comms continue |
| 5.3 | Move C1's identity sat | scanner detects → `%stale-notice` |
| 5.4 | `%stale` demotes the peer to a fresh `%alien` (not deleted), NO snub | the demotion design |
| 5.5 | C1 re-attests after the move | promoted again; queued traffic drains |
| 5.6 | `%anew` refresh via Causeway → `%gw-btc` → Jael | the owner-driven pass refresh |
| 5.7 | Publish an OP_RETURN for a previously-confidential comet | confidential → public transition; leaves the conf registry |

## Phase 5b — self-rescue: can an unreachable comet make itself reachable?

Added 2026-08-05, after Phase 5. C2 was minted with **neither `fief` nor
`sponsor`**, and an absent sponsor projects to *self*, so nothing can route
to it: it received zero attestations across the entire Phase 5 run, which
blocked 5.2 and 5.5. Rather than mint a fresh comet, test the edge case we
landed in by accident — it is a question a real user will ask.

The key asymmetry: C2 is unreachable over the *network*, but its owner can
still act *on-chain*, because spending the sat needs a wallet, not a
reachable ship. So recovery is possible; the question is how peers learn.

| # | Test | Chain? | Property under test |
|---|---|---|---|
| 5b.1 | ~~**Pairwise rescue.** C2 *initiates* to C1 and hands over its attestation; C1 verifies and gains a route to C2 from that alone, then reaches C2 unprompted.~~ **DOES NOT WORK — measured 2026-08-13, see below.** | no | Was: recovery preserving confidentiality, scaling terribly. In fact there is no such recovery. |
| 5b.2 | **Publication rescue.** One C2 state update that BOTH adds a `fief` (or `sponsor`) AND carries an OP_RETURN publication. Scanners on C1/C3 learn C2's route from the chain alone, with no contact; C1 then cold-initiates to C2. | yes, ~200–400 sats | The confidential→public transition (this subsumes **5.7**) as the general-purpose escape hatch. Costs confidentiality permanently. |
| 5b.3 | **Rerun 5.2 / 5.5 properly** once C2 is routable: C1 rekeys live, C2 **receives** the updated attestation, `%gw-btc` fetches the new custody entry from chain and re-verifies at the new life, and the two keep talking. Then move C1's sat and drive the full `%stale` → re-attest → promote → drain loop through `%anew`. | reuses 5.1/5.3 | The loop Phase 5 only proved **half** of: C1 emitted three attestation blobs on rekey, but C2 logged `got attestation` **0** times all run, so no peer has ever verified a post-rekey attestation. |

**NOTE on the pre-built 5.7 transaction: do not send it as-is.** It publishes
C2's *current* snapshot, which has neither `fief` nor `sponsor` — broadcasting
it would announce C2 to the world and leave it exactly as unreachable. Rebuild
it as 5b.2 above.

**5b.1 DOES NOT WORK, and the claim that stood here was wrong.** Measured on
mainnet, 2026-08-13, with dual packet captures agreeing to 400 µs: f1 (no
fief, no sponsor) sent f2 four packets — an attestation plus two message
fragments, retried once — and f2 answered with **zero packets across a full
300 s window**. Both ships held each other at `lyfe=[~ 1] dome=[~ %gw-btc]`
and neither was snubbed, so this was neither a PKI gap nor a snub.

The mechanism, traced to the line. An absent on-chain sponsor **projects to
self**, so jael's `sein(f1)` is f1, and `ames.hoon:12630` fires on
`=(ship (sein ship))` — f2 installs a self-referential `[%& f1]` lane
("reach f1 by asking f1"). It is marked `direct=%.y`, so `+get-forward-lanes`
returns it on the main branch and never falls through to `+zar`; no sponsor
relay is attempted, and vere drops it. Zero bytes, matching both captures.

Worse, **the hearer cannot acquire the lane either.** The heard-lane update
at `ames.hoon:5347` is gated on `!=(her.channel (sein … her.channel))`,
which for f1 is `!=(f1 f1)` — false. So f2 may receive unlimited packets
from f1 and will never learn a route from any of them.

The corrected property: **speaking first is not a rescue.** A comet that
committed neither a `fief` nor a `sponsor` has told the network nothing about
where it is, and no amount of it speaking first fixes that — its peers have
nowhere to reply to, and cannot learn one from the packets.

**Publication rescue (5b.2) is unaffected by any of this, and should work.**
Do not read the above as "an unreachable comet is lost" — it is not, and the
two paths fail and succeed for unrelated reasons:

`+fief-route` (`ames.hoon:4348`) does **not** put an address in ames. It
returns `[direct=%.n lane=[%& her]]` — a *ship*-lane. The address goes to the
RUNTIME: ames `%give %fief`s it to the unix duct (`:4849`, and `+on-publ-fief`
for a fief that arrives alone), populating vere's lamp table
(`vere/pkg/vere/io/ames/lamp.c`), and vere resolves `[%& ship]` through
`_ames_lamp_lane` — the same path galaxies use.

So for a self-sponsored comet the self-sponsor rule at `:12630` **does**
overwrite the fief route, and it does not matter: both produce the identical
lane `[%& ship]`, differing only in `direct`. Reachability turns on whether
vere's lamp holds an entry, not on which of the two set the route. A comet
that publishes a fief later reaches jael → `%give %fief` → the lamp, and the
route the peer already had starts resolving. Nothing in ames needs to change.

Confirmed shape from the live rig: `ames: lamp ~noplup-… static ip
.159.223.141.63 port 35364`.

**Untested.** This is traced through ames and vere, not measured — no comet
in this campaign has published a fief after spawn. `causeway rekey --fief`
now exists to build one; before it, nothing could, which is the only reason
this hatch was ever unreachable.

Commit the routing you need at spawn if you can; it is one transaction
cheaper and it is the only thing that works *immediately*.

## Phase 6 — resilience and diagnostics

| # | Test | Expected |
|---|---|---|
| 6.1 | Attestation arrives while the light client is still syncing | fail-closed / silence; NEVER a false negative verdict |
| 6.2 | Kill and restart the light client mid-verification | timeout → silence, no sticky failure; retry succeeds |
| 6.3 | Network partition between two comets (firewall drop) | no false verdicts; recovery on heal |
| 6.4 | Prolonged offline, then rejoin | recovers without manual intervention |
| 6.5 | Chain reorg behaviour | **characterize only** — forward-only is a known gap |
| 6.6 | Two peers disagree about a third's attestation state | characterize |
| 6.7 | Are failures diagnosable from logs alone? | judgement call, recorded |

## Phase 7 — Causeway

| # | Test | Expected |
|---|---|---|
| 7.1 | Full headless spawn over SSH (desktop CLI) | works without a GUI |
| 7.2 | Causeway pokes the attestation into `%gw-btc` | end-to-end from wallet to verifier |
| 7.3 | Causeway ↔ ship state synchronisation | characterize |
| 7.4 | **Destroy the seed, attempt recovery** | identity is UNRECOVERABLE for a confidential comet (the blind is gone); recoverable for a public one (opening is on-chain). Demonstrated, not assumed. |

## Explicitly out of scope (deferred by design)

Sponsor drop mid-session and recovery; sponsor discovery protocol;
consent tokens; multi-sponsor fallback; reachability-restoration
publication; the kernel no-relay gate (decline currently withholds
contact as well as relay). These are the deferred sponsorship
workstream, not gaps in this release.
