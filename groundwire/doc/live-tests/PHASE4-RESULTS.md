# Phase 4 — sponsorship (live mainnet)

Date: 2026-08-05 (UTC). Operator: Claude (agent).
Repo `/Users/trent/gw-building/groundwire`, branch `hd/cc-landing`, desk deployed
from tip **`3776782`** (`git archive 3776782 groundwire`, `doc/` stripped so a
mounted desk cannot wedge clay's `%into` loop; `tests/` kept). Nothing pushed.
**Two mainnet transactions were broadcast** (authorised); both confirmed.

## Rig

| ship | @p | droplet | pier / tmux | ames port | role |
|---|---|---|---|---|---|
| **C1** | `~havnyl-lonpub-botben-hidleb--lomper-marryc-lanmec-daplyd` | **N2** `159.223.141.63` | `/opt/gw/piers/p4c1`, `gwp4c1` | 47908 | confidential sponsee |
| **C2** | `~barpyx-tirmur-sovrex-dolfet--rivsyx-pidtud-ronmeb-daplyd` | **N3** `206.189.188.16` | `/opt/gw/piers/p4c2`, `gwp4c2` | 49818 | confidential third party |
| **C3 (S)** | `~ligdes-risbur-folmus-mattyp--firpec-lispec-noddyl-daplyd` | **N1** `64.227.13.22` | `/opt/gw/piers/p4c3`, `gwp4c3` | **34343** | public sponsor |

All three piers are **fresh** (booted from the kelvin-408 solid pill, own light
client, own `%gw-btc`). Phase-3 piers were left in place but stopped; the
Phase-3 iptables rate-limits between N2 and N3 were flushed, and **no DNAT rule
exists anywhere in this run**.

---

# LEAD

## STEP 0 — the `~hopned-…-banbyl` anomaly: **ARTIFACT, not a second mainnet comet**

`~hopned-donter-famnet-wordyn--ripset-hodsud-sivdeb-banbyl` is the **unit-test
fixture comet** in `groundwire/tests/lib/urb-core.hoon`. It is not on the
Bitcoin chain and never was. Three independent lines of evidence:

**1. It is reproducible on a pier that has never seen a block.**
On the brand-new C2 pier (`%gw-btc` never bootstrapped — `/x/block-id` was
`[0x0 0]`, light client at header 244 000, i.e. ~700 000 blocks short of the
range in question), running the desk's own test suite prints it four times:

```
~barpyx_daplyd:dojo> -test /=gw-p4=/tests/lib/urb-core
built   /tests/lib/urb-core/hoon
  ~hopned-donter-famnet-wordyn--ripset-hodsud-sivdeb-banbyl
OK      /tests/lib/urb-core/test-update-sonts-follows-sat
OK      /tests/lib/urb-core/test-find-detects-tracked-spend
OK      /tests/lib/urb-core/test-find-detects-op-return
  ~hopned-donter-famnet-wordyn--ripset-hodsud-sivdeb-banbyl
OK      /tests/lib/urb-core/test-apply-state-life-gate-rejects
  ~hopned-donter-famnet-wordyn--ripset-hodsud-sivdeb-banbyl
OK      /tests/lib/urb-core/test-apply-state-advances-life
>>> "%urb-core: spawn state commitment mismatch"
OK      /tests/lib/urb-core/test-apply-spawn-rejects-bad-state-key
  ~hopned-donter-famnet-wordyn--ripset-hodsud-sivdeb-banbyl
OK      /tests/lib/urb-core/test-apply-spawn-indexes-public-comet
```

The line is `~&  >  ["%gw-btc found public comet: " who]` in
`+apply-spawn:urb-core` (`lib/urb-core.hoon:277`), and the fixture that reaches
it is `tests/lib/urb-core.hoon`:

```hoon
++  seed   'urb-core-comet'
++  fund   ^-(sont:ord [0xf00d 0 0])
++  blind  (make-blind:cc seed)
++  dat    (make-dat:cc fund blind)
```

i.e. a comet mined off the literal seed `'urb-core-comet'` against a fake
funding satpoint `0xf00d:0`. (Note its low 16 bits are `~banbyl`, not the
`~daplyd` all three live comets are mined under — a first hint it never came
from our chain data.)

**2. The chain has no such publication.** I pulled every raw block from
**961 040 to 961 126** off Bitcoin Core (`alpha.groundwire.dev`, 141 160 959
bytes of block data) and searched the raw bytes for the publication envelope
prefix `6a 03 'urb'`:

```
blocks scanned: 87  range 961040 961126  bytes 141160959
total raw "6a 03 'urb'" occurrences: 1
  height 961059 occurrences 1
```

Exactly **one** — C3's own spawn at 961 059 (`ec5c1fbe…:1`, kelvin 9, 254-byte
payload, `pass` byte-identical to C3's artifact `pass_atom_hex`). The previous
run's window 961 080–961 104 contains **zero** publication bytes, so
`+process-publication` could not have been entered there at all, let alone
reached `+apply-spawn`'s success print. Scanner script:
`<scratchpad>/p4/chainscan.py`, results `<scratchpad>/p4/chainscan.json`.

**3. The original log context says so.** The occurrences in the earlier logs
(`<scratchpad>/gwtest-boot.log:315-337`, `<scratchpad>/kerneltest/ktzod-boot.log:990-1012`)
sit between `built /tests/lib/urb-core/hoon` and the `OK
/tests/lib/urb-core/test-apply-spawn-indexes-public-comet` lines — they are test
output, interleaved into the same dojo pane as the scanner's output.

**Verdict: artifact. There is exactly one public Groundwire comet on mainnet,
C3, and it is ours.** The scanner is trustworthy: the print fires only from
`+apply-spawn`, which is only reachable from a real OP_RETURN publication, and
there is only one of those on chain. Nothing about `+apply-spawn` was
mis-firing; a `~&` inside a pure library arm is simply indistinguishable in a
shared dojo pane from live scanner output. **Cosmetic follow-up worth doing:**
the fixture should either be built through a non-printing path or the print
should be demoted, because it will keep looking like a live event every time
someone runs `-test`.

## STEP 1 — both state updates broadcast and confirmed

| | C3 (sponsor) | C1 (sponsee) |
|---|---|---|
| txid | `8e713009f0c067374deca6069a99076efb2bff3fb1da7f4164f597df1401a4fb` | `a8553a3ab97d7123dc1d428fc7b8ecf10b6f8b5e6d86a9a77bfdcb85782fffe8` |
| block | **961 129** | **961 130** |
| change | `fief = [%if .64.227.13.22 34343]`, life 1→2 | `sponsor = ~ligdes-…-daplyd`, life 1→2 |
| vsize / fee | 325 vB / **327 sat** (1.01 sat/vB) | 111 vB / **111 sat** (1.00 sat/vB) |
| sat output | 1615 → **1288** sat | 1889 → **1778** sat |
| OP_RETURN | **yes** (196-byte payload) | no (confidential) |

Fee rate at build time: `{"fastestFee":2,"halfHourFee":1,"hourFee":1,"economyFee":1}`
— well under the 5 sat/vB abort threshold.

**Deviation from the brief, deliberate:** C3's update carries an OP_RETURN
publication and therefore costs 327 sat, not ~111. Without it the new fief
would exist only inside the taproot tweak and **no peer could ever learn it** —
`+apply-state:urb-core` is only reachable from `+process-publication`, i.e. from
a publication output; a bare sat move is handled by `+update-sonts`, which
relocates `sont.own` and never touches the `net` fields (`fief`, `sponsor`,
`life`, `pass`). A fief nobody can read defeats the entire phase. 327 < the
500-sat stop threshold, so the gate still passed on its own terms.

### The general rule this exposes (worth writing into the spec)

**The two identity classes disseminate state changes by different, non-
interchangeable channels, and neither can substitute for the other:**

* A **confidential** comet's state change rides in its **`xtr`** — the custody
  log baked into the pass it hands each peer during self-attestation. It needs
  no publication output at all (C1's update is a bare 111-sat sat move with one
  P2TR output and nothing else on chain), because the opening travels
  peer-to-peer inside the attestation.
* A **public** comet has no `xtr` to carry (its boot ring is deliberately
  xtr-less; peers resolve it from the chain), so its ONLY dissemination channel
  is the OP_RETURN publication. An unpublished state update is, to every
  observer, an anonymous P2TR-to-P2TR spend: `+update-sonts` will faithfully
  follow the sat to its new home and emit `%xfer`, so the identity is not lost —
  but `life`, `key`, `sponsor` and `fief` all silently keep their old values
  forever.

So: **a public comet's state change is undiscoverable unless published; a
confidential comet's is undiscoverable unless it re-attests.** The asymmetry is
easy to trip over precisely because the on-chain commitment (`5120||state-key(P,
snapshot)`) is *identical* in both cases and looks complete — the taproot output
really does commit the new snapshot, it just commits it to nobody. Anything that
builds a public comet's state update (Causeway's `rekey`, notably, which today
carries `publication_pass_atom`/`publication_opening` as optional arguments and
defaults them to `None`) should make the publication mandatory when
`proof["published"]` is true, or at minimum refuse to build silently.

---

# Verification gate (run before every broadcast)

Builder/gate: `<scratchpad>/p4/stateup.py`. Encoders are causeway's own
(`build_rekey_psbt`, `state_output_key`, `state_commit`, `snapshot_noun`,
`make_publication_script`); everything on the checking side is independent — a
from-scratch secp256k1 point arithmetic + BIP-341 taproot tweak, a from-scratch
tagged-hash tapleaf, and a from-scratch raw-transaction decoder (all from
`gwmint.py`, the gate used for the original spawns). Signed locally with embit
from the mnemonic in `.gw-mainnet-wallet.json` (key-path, `PSBT_IN_TAP_MERKLE_ROOT`
= the CURRENT snapshot's state-leaf hash).

## C3 — `8e713009…` — full decode

```json
{
  "version": 2,
  "vin": [
    { "txid": "ec5c1fbeb697211bfcf587e3b985f6e99e5ff8e8d2d92f083a25d55b3bcc97c8",
      "vout": 0, "scriptSig": "", "sequence": 4294967295 }
  ],
  "vout": [
    { "value": 1288,
      "scriptPubKey": "51203aea31567982f409f9a3e43b98610c2360c1e135427db948c4e0c56a184ee110" },
    { "value": 0,
      "scriptPubKey": "6a0375726201094cc401e0d7b1e4318330b6a4cbf61189b748431a2f0db8d52c9d5a33a360b913daecf2313fac2b1c523e5eb8af736889af70cab78248c0633675d36206afeffeb27a194b986f008b80efecae458c6e8c240559375259bd9ddf4ec3e0104ec2b4931a7bccf74d2dea253ced833454eaa369001418b843cfe76c4dbfa406afb4418126649f5346aba7318cf0e114672d5d7f4f09b790190001ae7048f978e1becea125bec229df0a22018fd9d44d8b19bcbefbcbea652c61be33039f660ef8160de340209c180a" }
  ],
  "witness": [ [ "a98e733019070a80ea9a8ad1d14809139fc4d0c32a32ac66d75bfb96aa9ff42d17ebace12e45314c1d6b0892b93e9fa0d7f2dd871443c70b6834b0b0d24ce194" ] ],
  "locktime": 0, "size": 376, "base_size": 308, "weight": 1300, "vsize": 325,
  "txid": "8e713009f0c067374deca6069a99076efb2bff3fb1da7f4164f597df1401a4fb"
}
```

```
[ok] (a) exactly one input
[ok] (a) input 0 == comet's current sat outpoint   ec5c1fbe…c8:0
[ok] (a) that outpoint is the artifact's sat_vout/value
[ok] (b) output 0 spk == 5120 || independently recomputed Q   3aea3156…e110
[ok] (b) causeway's state_output_key agrees with the scratch tweak
[ok] (b) leaf hash agrees (causeway vs scratch)
[ok] (b) proof records the same Q
[ok] (c) life strictly increases   1 -> 2
[ok] (c) key carried forward unchanged
[ok] (c) rift unchanged (no breach)
[ok] (d) output 0 >= 330 sat dust limit   1288 sats
[ok] (d) fee within cap   fee=327 sats  vsize=325
[ok] (d) effective fee-rate sane   1.01 sat/vB
[ok] (d) value conserved
[ok] (e) exactly 2 output(s)
[ok] (e) output 1 is exactly the OP_RETURN publication
[ok] (e) OP_RETURN envelope 6a 03 'urb' 01 09
[ok] (e) output 1 value == 0
[ok] (e) publication opening has NO blind-opening (state update, not spawn)
[ok] (e) published pass fig == this comet
[ok] key-path spend: single witness item
[ok] key-path spend: 64-byte schnorr sig   64 bytes
[ok] spends the CURRENT commitment (prior leaf hash matches artifact)
[ok] txid from scratch == embit txid
[ok] nVersion 2 / nLockTime 0
```

Publication payload decoded back out of the scriptPubKey with causeway's `cue`,
i.e. exactly what a peer's `+read-publication` will see:

```
payload bytes      : 196
pass == artifact   : True
internal-key       : 0x02e129efeba5ace29c3e118634f568ca73ec84d0283695e0d497e9ad9cf9e87703
life               : 2
rift               : 0
key == artifact key: True
sponsor (unit)     : 0  (= ~)
fief   (unit)      : (0, (26217, (1088621846, 34343)))
   fief tag        : 26217 = 'if'   ip: 64.227.13.22   port: 34343
blind-opening      : 0  (= ~, required for a state update)
state-key(ikey, decoded snapshot) == output0 spk: True     <- +apply-state's commitment check
```

## C1 — `a8553a3a…` — full decode

```json
{
  "version": 2,
  "vin": [
    { "txid": "2c66c65411a8ba822bd705ca4538a5e43cd8c7677c3208ed0281b6436658be4f",
      "vout": 0, "scriptSig": "", "sequence": 4294967295 }
  ],
  "vout": [
    { "value": 1778,
      "scriptPubKey": "51208239a700d397d6a69d3ee69346ad43ed77403d1ebf2452ccdbde8928811a4aeb" }
  ],
  "witness": [ [ "96479ac311c26b666b7b42aabcfe92af9c5d3e226807033f87f0e32498b19fe2d6da99ba7c4e60e2ec8e1b948d54e14439d8024afc815bba46a39c736d500d3b" ] ],
  "locktime": 0, "size": 162, "base_size": 94, "weight": 444, "vsize": 111,
  "txid": "a8553a3ab97d7123dc1d428fc7b8ecf10b6f8b5e6d86a9a77bfdcb85782fffe8"
}
```

```
[ok] (a) exactly one input
[ok] (a) input 0 == comet's current sat outpoint   2c66c654…4f:0
[ok] (a) that outpoint is the artifact's sat_vout/value
[ok] (b) output 0 spk == 5120 || independently recomputed Q   8239a700…4aeb
[ok] (b) causeway's state_output_key agrees with the scratch tweak
[ok] (b) leaf hash agrees (causeway vs scratch)
[ok] (b) proof records the same Q
[ok] (c) life strictly increases   1 -> 2
[ok] (c) key carried forward unchanged
[ok] (c) rift unchanged (no breach)
[ok] (d) output 0 >= 330 sat dust limit   1778 sats
[ok] (d) fee within cap   fee=111 sats  vsize=111
[ok] (d) effective fee-rate sane   1.00 sat/vB
[ok] (d) value conserved
[ok] (e) exactly 1 output(s)
[ok] (e) no OP_RETURN output (confidential)
[ok] key-path spend: single witness item
[ok] key-path spend: 64-byte schnorr sig   64 bytes
[ok] spends the CURRENT commitment (prior leaf hash matches artifact)
[ok] txid from scratch == embit txid
[ok] nVersion 2 / nLockTime 0
```

## Extra pre-broadcast checks

**Node policy.** `testmempoolaccept` against Bitcoin Core 29 (`alpha.groundwire.dev`):

* C1 → `"allowed": true, "vsize": 111`.
* C3 → `"allowed": false, "reject-reason": "scriptpubkey"` — the known
  pre-Core-30 `-datacarriersize=83` policy, size-only, identical to what the
  original public spawn hit. mempool.space accepted and the network mined it
  one block later, so this is a property of that one reference node, not of
  the transaction.

**Independent signature validation** (needed precisely because Core 29 will not
validate C3 for us): BIP-341 key-path sighash and BIP-340 schnorr verification
implemented from scratch, checked against each prevout's own output key:

```
C1 a8553a3a…  prevout 2c66c654…:0  1889 sats  spk 5120be3a7444…8341
   sighash be537ce84fb226fa17ce1a3173f94582811f1760e7f05249c7921f2ee010b96a
   BIP-340 schnorr verify against the prevout's output key Q: VALID
C3 8e713009…  prevout ec5c1fbe…:0  1615 sats  spk 51200ca2828c…0bee
   sighash 3d4763062c9f1137e64ceff06536984c8bd0d491bfd3275091a388cbf96b8051
   BIP-340 schnorr verify against the prevout's output key Q: VALID
```

Both sat outputs were re-checked unspent immediately before broadcast.

## Artifacts updated and re-verified end-to-end

`<scratchpad>/p4/update_artifacts.py` (backups in
`<scratchpad>/p4/artifact-backup-pre-p4/`) folded both updates into
`.gw-comet-{1,2,3}.json` (still `chmod 600`) — new snapshot, new sat outpoint,
new `xtr_hex` and boot feed rebuilt with causeway's `build_xtr_atom`,
`append_xtr_to_ring`, `rebuild_feed` — and then replayed
`+run-checks:self-attestation` over each comet's own custody log against the
live chain (every transaction refetched from mempool.space, continuity walked
through input 0, every opening's state key recomputed with the independent
taproot tweak). **All three comets verify VALID; zero failures.** Highlights:

```
--- re-verify ~havnyl-…-daplyd (life 2) ---
[ok] spawn-commit opens dat (independent H_tag over jam(sont)||blind)  e09ea9eb…f281
[ok] entry-0 continuity through input 0   394f3678…4c:1
[ok] entry-0 commitment: 5120||Q == on-chain output 0
[ok] entry-1 continuity through input 0   2c66c654…4f:0
[ok] entry-1 spends a P2TR prevout key-path
[ok] entry-1 commitment: 5120||Q == on-chain output 0
[ok] entry-1 blind-opening only on entry 0
[ok] entry-1 life-order
[ok] tip-unspent / tip-p2tr / tip == artifact sat outpoint
[ok] pass-key: boot ring's cry == latest snapshot key
[ok] boot feed @p unchanged / boot feed life == on-chain life
[ok] sponsor names C3
--- re-verify ~barpyx-…-daplyd (life 1) --- all ok (unchanged)
--- re-verify ~ligdes-…-daplyd (life 2) --- all ok, entry-1 = 8e713009… @ 961129
FAILURES: none
```

**Boot feeds actually used** (byte-identical to the artifacts):

* C1 `feed_with_xtr_baked` — life 2, 2-entry custody log (251-byte `xtr`).
* C2 `feed_with_xtr_baked` — unchanged, life 1, 1-entry log.
* C3 **`feed_public_no_xtr`** — life 2, ring with **no** `xtr`. This is the
  canonical shape for a *public* comet: its pass is resolved by peers' block
  scanners from the OP_RETURN, and the pass published on chain is the xtr-less
  one (verified byte-for-byte against the on-chain payload). A public comet
  carrying an `xtr` would be asking peers to run the confidential verifier on a
  ship they can already read off the chain.

---

_(Phase-4 test results follow; light clients were re-syncing from genesis at
the time of writing.)_
