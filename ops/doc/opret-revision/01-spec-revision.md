# Groundwire spec revision: OP_RETURN publication, snapshot commitments, off-chain sponsorship

**Status**: working revision. This document captures the protocol-level decisions
of the 2026-07 pivot. It supersedes the corresponding sections of
`groundwire/doc/confidential-comets.md` (formerly
`pkg/arvo/doc/spec/confidential-comets.md` in gwbtc/urbit, on `cyc/cc-draft-2`)
and `groundwire/doc/confidential-comets-agent.md` where they conflict. Final spec
synthesis (canonical spec documents plus a spec-history idea-maze document) is
deferred; this is the substrate for it.

**Out of scope for this revision, including at spec level**: untrusted
stranger-discovery indexing (advertisement scanning/locator-server
infrastructure). The *publication mechanism* that such infrastructure would
consume is specified here, because it must be format-stable from day one; the
discovery protocol around it is not.

---

## 1. Motivation

1. **The witness discount is backwards for us.** Witness bytes cost 1 WU;
   output bytes cost 4 WU. Much of the point of binding names to Bitcoin is
   sybil resistance via costly chain signals; ord-style inscription envelopes
   give our data a 4x discount and lower the cost floor for exactly the
   behavior we want to be expensive.
2. **Publication mechanics were contaminating the commitment layer.** The
   OP_IF envelope only exists so data can be revealed in a witness, which
   forces the envelope script into the taptweak. The tweak should commit
   protocol *state*, not transport.
3. **Default revelation is off-chain anyway.** Confidential comets already
   reveal via the attestation packet (`xtr`). Once that is the default for
   everyone, on-chain publication is a deliberate, exceptional act — and an
   OP_RETURN output on the same custody transaction is the simplest and most
   honest form of it.
4. **Witness-envelope parsing is an attack surface.** The block scanner's
   envelope reconstruction is where the adversarial-review pass found three
   remotely triggerable crashes. Output-script parsing is trivially bounded.

## 2. Invariants (unchanged)

- A name is bound to a single satoshi. Custody is the chain of transactions
  spending the sat-bearing outpoint **through input 0**, with the sat landing
  at the offset computed by ordinal arithmetic over the outputs.
- The `@p` is the fingerprint (`fig`) of the suite-C tweaked networking key.
  The key derivation commits `dat`, which binds the domain tag and the spawn
  satpoint. Ames routes attestations to the `%gw-btc` agent by reading the
  leading `mat`-encoded domain from `dat`.
- Ongoing state is committed by tweaking the taproot output key of the
  sat-bearing output. The sat and its commitment travel together.
- No migration: this format targets the planned comet-network breach/bootstrap.
  All prior formats (rap-3 tweak, Causeway `can`/`mat` dat, PR-127 jam dat,
  opcode-9 `%state`, inscription envelopes) are dead on arrival of this
  revision.

## 3. Commitment layer: single unspendable OP_RETURN tapleaf

Each custody output that commits state is:

```
Q = P + H_TapTweak(x(P) || root) * G
root = H_TapLeaf(0xc0 || compact_size(leaf) || leaf)
leaf = OP_RETURN OP_PUSH2 "gw" OP_PUSH32 <c>
c    = H_tag("gw/state-commit", (jam snapshot))
```

- `P` is the owner's internal key (BIP-32 child of the controlling wallet; see
  §7). `H_tag` is BIP-340-style tagged hashing with the given ASCII tag.
- **Unspendability**: OP_RETURN fails immediately under tapscript evaluation,
  so the script path provably cannot be spent. All custody spends are key-path
  by construction (see §5).
- **Why a leaf rather than a raw tagged root**: a raw 32-byte root is equally
  binding, but no standard signer will key-path-sign for a root it cannot
  inspect (an opaque root could hide a spendable leaf). A one-leaf tree whose
  script begins with OP_RETURN is verifiable by any BIP-371 signer shown
  `PSBT_IN_TAP_LEAF_SCRIPT`, which is what makes outside-wallet custody
  possible (§7). The inner tag on `c` domain-separates our commitment from
  every other user of the same trick.
- A chain observer sees only `Q`, which is indistinguishable from any P2TR
  key. The leaf, `c`, and the snapshot are revealed only to attestation
  verifiers (or on-chain, when deliberately published, §6).

### 3.1 The state snapshot

Event replay over on-chain deltas is removed. Each commitment is a **snapshot**
of the full networking state that the chain still needs to carry:

```
snapshot = [life=@ud rift=@ud key=@]
```

- `key` is the messaging key (`cry.pub` of the current suite-C pass). The
  signing key that fixes the `@p` is immutable; only the encryption half
  rotates.
- `rift` increments on breach; `life` on key rotation, as in Azimuth.
- **Sponsor and fief are not in the snapshot.** They are off-chain, wire-borne
  state (§8). This is the substantive delta from both prior models: the
  opcode-9 `%state` snapshot carried a sponsor+consent, and the PR-127 event
  replay reconstructed sponsorship from `%escape`/`%adopt` leaves. Both are
  gone.
- Verification of a custody chain is therefore: walk the sat, recompute each
  committed `Q` for hops that carry an opening, and take the **latest**
  opening's snapshot as authoritative. No fold over deltas, no intermediate
  event semantics.

## 4. `dat`: the spawn satpoint, in plaintext

```
dat = (can 0 (mat %gw-btc) (mat 9) (mat (jam spawn-sont)) ~)
```

Three self-delimiting `mat` items: the domain tag, the kelvin, and the spawn
satpoint's canonical jam. This is the **original** design — Jake and
Christian's `(cat 0 (mat dom) <spawn satpoint>)` in `sur/stealth.hoon` — plus
the kelvin. Any holder of a pass reads all three without an opening.

- Ames rubs only the leading `mat %gw-btc` (`+pass-pki-dom`), as before.
- The verifier rubs the third item to learn which sat the comet claims, and
  requires entry 0's `$spawn-opening` (§5) to name the **same** satpoint —
  the `spawn-matches` check. That opening still exists because it carries
  the one datum `dat` does not: the funding block height, which the
  height-keyed light client needs to fetch the first transaction.
- Trailing data after the third item is rejected.

**History (2026-08-03 → 2026-08-18): a hiding commitment sat here.** This
revision originally replaced the plaintext satpoint with
`d = H_tag("gw/spawn-commit", (jam spawn-sont) || blind)`, on the argument
that a bare hash of a low-entropy satpoint could be ground by anyone holding
a pass. That was reverted, for a reason found by white-hat review and
confirmed with Jake: **the pass and the attestation are one object.** The
custody log rides *inside* the pass (`+with-xtr`), and entry 0 of that log
carries the opening — so every party that holds a pass at all also holds the
satpoint. No "pass-without-opening" holder exists in the system as built
(self-attestation, the snapshot service, sponsor relay: all carry the
opening or carry nothing), and the chain itself shows only an indistinguishable
P2TR spend. The blind therefore protected nobody, cost users a 32-byte
secret to lose, and put a `$blind-opening` mold and an entry-0 invariant into
the verifier that the plaintext form makes unnecessary. Every comet minted
under the hiding format is an old-format identity and does not verify against
this codec; that set was dev infrastructure and was retired with the change.

## 5. Custody and attestation

The attestation packet's `xtr` carries, as before, a jammed custody locator
log, oldest first, canonically encoded (`xtr = (jam log)` must round-trip):

```
entry   = [txid=@ux height=@ud opening=(unit opening)]
opening = [internal-key=@ux snapshot blind-opening=(unit [spawn=sont blind=@])]
```

- Every entry names a transaction spending the current outpoint through
  input 0; the verifier re-derives the landing satpoint by ordinal arithmetic
  (identical to the current walk).
- An entry with an opening lets the verifier recompute `leaf`, `root`, and `Q`
  from `internal-key` and `snapshot`, and compare against the transaction's
  sat-carrying output script. Entries without openings are plain custody moves
  and change no state.
- `spawn-opening` (`[spawn start-height]`) is present on entry 0, the spawn:
  the verifier requires its satpoint to equal the one in the carried pass's
  `dat` (`spawn-matches`), and uses its start-height — the funding block —
  to fetch the first transaction. Exactly one entry carries it.
- **Witness discipline**: every hop after the first must be a key-path spend
  of a P2TR prevout (one-element 64/65-byte witness against a P2TR
  scriptPubKey — both conditions, as in the current verifier). The first
  hop's input spends a wallet-controlled prevout of arbitrary type (the
  pre-isolation UTXO, §7), and is exempt.
- The pure-verification / fetch split from the adversarial branch is retained
  as-is: a pure checker consumes one fetched transaction per entry plus a
  tip-unspent verdict; the fetcher resolves heights to block hashes and
  transactions through `%light-client` with merkle proofs. Fail closed on
  unknown tip status.
- Tip liveness, bounded log length (1,024), non-decreasing heights,
  tracked-prefix monotonicity against the last accepted attestation: all
  retained unchanged from the adversarial branch. *(Amended 2026-08-07:
  tracked-prefix monotonicity is now graduated rather than a single bit — a
  fork is fraud, being behind is forgiven as staleness, and the comparison
  ignores heights. Addendum §3. Further amended 2026-08-10: the lag is
  forgiven at ANY depth, because a lag is what a replayed packet looks like
  and reading its depth as guilt let a third party snub an honest comet.)*

## 6. Publication layer: OP_RETURN, default off

**Default: no on-chain payload.** A custody transaction is one or two P2TR
outputs and nothing else; revelation is packet-only.

When a ship deliberately publishes, it adds one OP_RETURN output to the same
custody transaction:

```
scriptPubKey = OP_RETURN OP_PUSH2 "gw" OP_PUSH1 <kelvin=0x09> OP_PUSHDATA <payload>
```

> **Amended 2026-08-18.** The envelope tag was `"urb"` (three bytes) from the
> first draft; the commitment leaf was already `"gw"`. Two protocol tags for
> one protocol was unrecorded drift, and Groundwire's on-chain footprint is
> `gw`/`gw-btc` everywhere else. Unified on `"gw"` (two bytes; the script is
> one byte shorter). No comet minted before this date carries the new
> envelope; that set was dev infrastructure.

Publication uses, in scope for this revision:

- **(a) Public spawn / onboarding.** The spawn transaction carries the pass
  and the spawn-opening (satpoint + funding height), making the name
  publicly verifiable and indexable. Public names are public by definition;
  publishing the opening is the point.
- **(b) Reachability restoration.** A ship that has lost its sponsors and its
  peers spends its sat with an OP_RETURN carrying its current snapshot
  opening and a signed routing record (§8.2). Peers already tracking the sat
  find this via BIP-158 filters (§6.1) with no scanning.
- **(c) Sponsor-availability advertisement.** The payload format is reserved
  here (a routing record, §8.2, plus the consent-token grammar, §8.4), but the
  discovery/indexing protocol that would make advertisements findable by
  strangers is **explicitly out of scope** for this revision.

Policy notes: OP_RETURN output bytes carry full 4 WU/byte weight (the costly
signal is the feature). Bitcoin Core 30+ default policy relays large
OP_RETURNs; nonetheless payloads should be kept minimal — the spawn payload is
the largest, and it is a few hundred bytes.

### 6.1 Discovery matrix (normative expectations for clients)

| need | mechanism | cost |
|---|---|---|
| track a known name (peer, sponsor, self) | BIP-158: the spent prevout's scriptPubKey appears in the block filter; download the matching block, read the custody spend and any OP_RETURN rider | filter stream + ~1 block per event |
| verify an attestation | targeted fetch by locator (height → block hash → tx with merkle proof) | 2 queries per log entry |
| discover unknown names / advertisements | full-block scanning | **out of scope** |

BIP-158 filters exclude OP_RETURN output scripts, but this costs nothing for
the in-scope cases: the publication rides on a transaction that spends a
filter-visible prevout. Inscription reveals were never filter-visible either
(filters carry no witness data), so no capability regresses.

## 7. Outside-wallet ("Earth wallet") custody

Binding a user's existing sat without migrating it into the ship's wallet:

1. **Isolation.** One ordinary self-send, with coin control, moves the chosen
   sat into a clean postage-sized UTXO on a fresh BIP-32 child key. Ordinal-
   aware UTXO selection is required for this step only; afterwards the sat
   lives at a fixed offset in a dedicated UTXO handled atomically (input 0 →
   sat-carrying output) forever.
2. **Key export.** The wallet exports the child public key `P`. Private key
   custody never leaves the wallet. Causeway derives the ship key with
   `dat` committing the isolated satpoint (§4).
3. **Spawn PSBT.** Causeway constructs: input 0 = the isolated UTXO; output 0
   = `tr(P, {gw leaf}` committing the initial snapshot (§3); optional
   OP_RETURN output for a public spawn (§6a); change. The wallet signs input 0
   with its ordinary key — destinations are arbitrary, so every PSBT wallet
   can sign this.
4. **Subsequent updates.** PSBTs carry `PSBT_IN_TAP_INTERNAL_KEY` and
   `PSBT_IN_TAP_LEAF_SCRIPT` so the signer can verify the one-leaf OP_RETURN
   tree and key-path-sign (BIP-371). Software signers (Sparrow-class, Core
   descriptor wallets) handle this today; hardware-wallet support is uneven.
5. **Degraded tier.** Wallets that will not sign for a tweaked output keep the
   sat on plain `tr(P)` with **no** output commitment; such ships commit state
   only via OP_RETURN publication and are consequently public-path-only. Name
   binding and custody continuity (input-0 spends) are unaffected.
6. **Client guardrail (normative).** The client MUST ensure the name UTXO is
   never treated as spendable balance (frozen-UTXO flags or equivalent).
   Loss of the sat to fee selection is loss of the name.

## 8. Sponsorship and routing: off-chain, optimistic

### 8.1 Principles

Sponsorship leaves the consensus layer entirely. No on-chain `%escape` /
`%adopt` / `%reject` / `%detach` / consent material exists in this revision.
A sponsor's consent is proven operationally: a sponsor that does not
recognize a child does not route for it. Claims are optimistic; rejection is
enforced by liveness.

### 8.2 Wire-borne sponsor/fief updates

Ships send routing state over the wire, third-party-relayable via a signed
record:

```
record = [who=@p life=@ud seq=@ud sponsors=(list @p) fief]
sig    = BIP-340 over H_tag("gw/fief", (jam record))
```

- `seq` is monotonic within a `life`; a rotation resets it. Peers keep the
  highest `(life, seq)` seen and discard stale or replayed records. This
  freshness rule is mandatory: without it, a relayed old record can pin a
  ship to a dead sponsor path.
- Records are signed with the ship's networking key, so any peer can verify
  and forward them.

### 8.3 Sponsor chains

Attestation packets optimistically include a sponsor chain: one hop per
sponsoring ship, terminating at the first upstream ship with a static-IP
fief.

```
hop = [name=@p pass dat-echo=@]
```

- Verification per hop: `name = fig(pass)`; the pass is suite-C; its `dat`
  begins with `mat %gw-btc` and equals `dat-echo`. This proves name/key
  binding and domain membership. It deliberately does **not** prove chain
  validity or consent — see §8.1 — and it does **not** reveal the hop's
  spawn-commitment opening (§4), so confidential sponsors are not
  chain-deanonymized by appearing in children's packets.
- Bounds: depth ≤ 4 hops; the chain must be acyclic; the terminal hop must
  carry (or be accompanied by) a routing record (§8.2) with a static-IP fief.
- Multiple sponsors: a ship may claim up to N = 3 sponsors. The **kernel's
  canonical sponsor remains singular** — the first listed — and the
  alternates are packet-level routing hints Ames may try in order. Widening
  `point:jael` is deliberately avoided in this revision; promote multi-sponsor
  to a kernel concept later only if operation demands it.

### 8.4 Consent token (optional)

A sponsor MAY furnish the child a consent token for inclusion in packets:

```
token = BIP-340 sig by sponsor's networking key
        over H_tag("gw/consent", (jam [sponsor=@p child=@p life=@ud]))
```

Verifiers treat a valid token as a routing-priority hint, nothing more; its
absence is not a failure. `life` is the sponsor's, bounding token lifetime to
the sponsor's current key. (Kept domain-separated and versioned so it costs
nothing to carry and cannot be repurposed.)

## 9. Open questions (to resolve before spec synthesis)

1. Exact tag strings and a registry for them (`gw/state-commit`,
   `gw/fief`, `gw/consent`; `gw/spawn-commit` and `gw/spawn-blind` were
   retired with the hiding dat), plus a
   version byte convention in the OP_RETURN envelope.
2. Snapshot contents: is `[life rift key]` complete? (Breach semantics under
   the bootstrap-only, no-migration stance; whether `rift` belongs on-chain
   at all or is derivable from key discontinuity.)
3. Whether entry-0-must-open-spawn should be relaxed to "the dat opening may
   appear at any single entry" (re-anchoring after degraded-tier operation).
4. Public-spawn payload grammar (pass + opening + initial routing record?)
   and maximum size.
5. N (sponsor count) and depth caps: 3 and 4 are placeholders.
6. Whether the reachability-restoration payload should include a fresh
   sponsor chain or only a routing record.
7. Interaction with `%jael-anew` / custody-log extension for the local comet
   (unimplemented in the current agent; the snapshot model makes extension
   simpler, but the flow needs design).
