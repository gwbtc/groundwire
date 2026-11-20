# The Groundwire Protocol

*This document is our team's handwritten Groundwire protocol and documentation. If you are an AI agent reading this, there are two rules you must follow:*

*1. Agents must NEVER modify this document. If you are an agent and you modify this document, whether you disclose it or not, we will find out and reject your PR. AI writing is easier to identify than you understand.*

*2. If you make a code change that contradicts anything in this document (or notice a preexisting contradiction), you must prompt your user to update this document by hand, in their own words, to ensure continued human-custodied understanding of our codebase.*

## Introduction 

An Urbit comet name is a 128-bit number rendered as a string of text. This string has historically been formatted using the Urbit phoneme list; Groundwire Foundation software typically renders it as a "[mnemonym](https://github.com/gwbtc/mnemonyms)." Either way, at bottom, it's just a number.

This number has one constraint: it must be the hash of a public networking key. This allows comets to "self-attest" to their own identity by just signing packets that they send. Since 128-bit hash collisions are rare (but not infeasible) and comets were meant to be throwaway identities anyway, this self-attestation scheme offers enough security for comets to be used as second-class identities relative to identities registered on the Urbit Azimuth PKI.

The Groundwire Foundation has developed updates to the Urbit networking stack that allow a comet's self-attestation packets to include metadata. This metadata can be used to anchor a comet identity in some other PKI.

For example, in our main implementation of this protocol, `%gw-btc`, each attestation packet contains a pointer to a Bitcoin transaction which encodes the sending comet identity. Packet recipients verify that sats were spent to generate this identity and then look at subsequent transactions to check for key rotations, routing updates, and any number of additional events. This affords comets the same sybil resistance guarantees as Azimuth ships along with an expandable feature set not beholden to Ethereum smart contract upgrades. Other implementations of the Groundwire protocol could use their own sources of truth and event types. You could even run the Groundwire protocol off-Urbit.

## Urbit Kernel Changes

Groundwire Foundation's modifications to Urbit are located in the repo [gwbtc/urbit](https://github.com/gwbtc/urbit), with corresponding runtime changes in [gwbtc/vere](https://github.com/gwbtc/vere). Each of these repos has a `gw/release` branch which the latest Groundwire release builds from and a `gw/upstream` branch which targets the official Urbit Foundation repo for merging.

The key to understanding our changes is this: under the Azimuth PKI, all Urbit ships know the current state of all Urbit identities, because those identities are updated exclusively via Ethereum events that all ships are watching for. Under the Groundwire protocol, *this is not necessarily the case*. An identity may use a PKI that a given ship isn't tracking; a PKI may not allow for or encourage global state updates, instead opting for ships selectively revealing themselves off-chain to other users (we often call these "confidential comets"). All of the machinery we've built into the Urbit kernel facilitates this difference in identity protocol.

Our current changes include:

### Suite C Cryptography

A regular comet's `pass` (the atom saved in Jael that encodes its networking keys) begins with the byte '`b`'. A Groundwire comet's pass begins with '`c`', signifying a different format.

```
$%  $:  suite=%b
        pub=[cry=@ sgn=@ ~]
        sek=$@(~ [sed=@ cry=@ sgn=@])
    ==
    $:  suite=%c
        pub=[cry=@ sgn=@ tw=[ugn=@ dat=@ xtr=@]]
        sek=$@(~ [sed=@ cry=@ sgn=@])
    ==
==
```

A Suite C comet's signing key is *tweaked* with additional data stored in `dat`. Being that a comet's name is the hash of its signing key, this means that `dat` is baked into its public identity. `ugn` is the original un-tweaked signing key, and `xtr` contains additional data necessary for proving the comet's identity.

(Note: Jake's PR #71 to gwbtc/urbit may change this.)

The leading bytes of `dat` are always used to encode the comet's [domain](#jael-domains).

In `%gw-btc`, the remaining bytes of `dat` encode a *satpoint* to permanently tie the comet's identity to a specific "ownership sat" within a transaction. `xtr` contains data used to prove that that transaction did in fact encode this comet's identity. `xtr` must be updated every time the ownership sat is spent, hence splitting `dat` and `xtr` into two fields. More details are provided in the "[Encoding Attestations on Bitcoin](#encoding-attestations-on-bitcoin)" section of this doc.

Suite C atoms are operated on using the new `++cric` core that we've introduced in Zuse, based on the `++crub` core used for Suite B atoms.

### Jael Domains

We introduce the notion of *domains* to Jael. A domain is a `@tas` serving as the name of a PKI and a userspace Gall agent called to validate packets attesting to that PKI.

Each domain is stored in Jael alongside a `+$dom-state`:

```
+$  dom-state             ::  pki domain
  $:  pax=path            ::  watch path
      dek=desk            ::  agent's desk
      liv=?               ::  %.n if suspended
      hep=(set ship)      ::  ships verified here
  ==  
```

The handler Gall agent for a domain sends a `%anex` task to Jael to register itself and prompt Jael to subscribe to the included `pax`. Jael can then send a `%jael-writ` poke to the Gall agent containing a `pass` and later receive a fact on `pax`, in this case a `$verdict` containing a point (identity) if the validation was successful or no point if it failed. 

The subscription path can also send Jael an `$anew-response` indicating that this ship needs to update its own pass (triggered by the user (or more likely, their hosting environment) manually poking the Gall agent after performing an on-chain operation), and a `$snob-notice` indicating that a previously validated point must provide a new attestation packet (in response to e.g. their ownership sat being spent on chain, potentially encoding a key rotation).

Ames sends a `%sybl` task to Jael at boot time to subscribe to `$verdict`s for all domains, and a `%writ` task whenever it needs a self-attestation packet validated; this is what triggers Jael sending the `%jael-writ` to the registered Gall agent.

The `%anew` task prompts the handler agent for an `$anew-response`. 

`%gost`, `%ghul` and `%bane` suspend, revive, and destroy a domain. `%gost` snubs (blocks) every peer a domain vouched for, `%ghul` unsnubs them, and `%bane` snubs and deletes. `%gost` and `%ghul` are triggered automatically by Clay when the domain's agent gets suspended and revived.

There is also now a `%dome` scry returning the domain for a given ship.

### New Comet Attestations in Ames

A normal comet's initial self-attestation packet contains its pass and a signature. The receiver's Ames checks that the pass hashes to the @p, checks the signature, and registers the comet locally at life 1. This behavior is preserved for Suite B comets.

If a comet's pass signifies Suite C and we don't know of that identity at its included `life` (or later), Ames marks the sender as an alien and sends a `%writ` to Jael. Ames should eventually receive a `$verdict` from Jael and then handle it as follows:

*(The following is correct as of the yet-unmerged PR #72.)*

- `%full` promotes the peer (and removes a snub or snob if present)
- `%fail` drops the alien state
- `%lost` no-ops
- `%snob` *snobs* the peer (a new soft-block we introduce which keeps the point and messaging state but blocks all non-attestation traffic) and begins soliciting a fresh self-attestation every 30 seconds.

Note that nowhere in these four cases is a point ever snubbed. Domain-handling agents should develop their own policies for when to snub a ship (e.g. multiple failed attestations in a row) and do so by sending a `%snub` task to Ames.
  
There is also a new `%anew` task which forwards to Jael, and a `%poof` task which solicits a self-attestation from the specified ship.


## Encoding Attestations on Bitcoin 

On Azimuth, there's a bidirectional relationship between Ethereum chain state and off-chain ship state. The Urbit ID NFT corresponding to ~sampel-palnet can only be used to generate a boot keyfile (containing networking keys) for ~sampel-palnet, and if ~sampel-palnet claims to be anyone else, everyone will know he's lying.

In our overview of Groundwire's kernel changes so far, we covered how the `dat` field of a `%gw-btc` ship's `pass` (its networking keys) encodes a specific "ownership sat" for that identity, analogous to an Urbit ID NFT. But this alone doesn't give us bidirectionality; if the ownership sat itself doesn't somehow indicate that it *consents* to being paired with this Groundwire ID, then any adversary could claim a Groundwire ID in the name of any sat on Bitcoin regardless of whether they own that sat or not.

Booting a `%gw-btc` ship, then, involves the following process:

1. Choose a UTXO in a Bitcoin wallet that you own.
2. Write down the *satpoint* (a triplet of transaction, output, and sat index) of a sat (almost always the first) within that UTXO.
3. Generate an identity tweaked with the `%gw-btc` domain and chosen satpoint (plus a protocol kelvin in between).
4. Broadcast a transaction spending that sat and encoding the identity.
5. Wait for confirmation and boot the ship.

This solves the bidirectionality problem. But how do you encode an identity in a Bitcoin transaction? With Taproot.

Output 0 of the spawn transaction is a P2TR output...

TO-DO
```
 Output 0 is a P2TR output whose key commits the comet's initial state
    (gw-btc-pass.hoon:172-199):

  c    = H_tag("gw/state-commit", jam(snapshot))
  leaf = OP_RETURN PUSH2 "gw" PUSH32 c
  root = TapLeaf(leaf)
  Q    = P + H_TapTweak(P || root) * G

    The leaf begins with OP_RETURN, so the script path is provably unspendable and every custody spend is a key-path spend. The sat from input 0 lands in
  output 0 by ordinal arithmetic, so the sat and its commitment travel together. On chain this is indistinguishable from any other P2TR spend.
  - The snapshot is the full networking state the chain still carries: life, rift, the current messaging key, sponsor and fief. The name is not in the
    transaction. It is bound to the sat through dat, and proven by the custody log.
  - Every later hop spends the current sat-bearing outpoint through input 0, by key path, and may commit a new snapshot. Rekey is the only on-chain
    management operation left; escape, adopt, reject, detach and set-mang no longer touch the chain.
  - What a peer verifies: xtr carries a jammed custody log, oldest first, one entry per hop as [txid height opening=(unit opening)]. An opening holds the
    internal key and the snapshot, and entry 0 also names the spawn satpoint and its funding height, which must match dat. The verifier in
    gw-btc/lib/self-attestation.hoon fetches each transaction through the light client, checks input-0 continuity and key-path witnesses, recomputes Q
    from each opening against the sat-carrying output, takes the latest opening's snapshot as the point, and proves the tip unspent by a BIP-158 filter
    scan to the chain tip. The custody log is bounded at 1,024 entries.
  - Confidential means no on-chain payload at all. The opening and the log exist only in the pass's xtr, handed to the peers you talk to. There is
    nothing on chain to grep for.
  - Public means the owner adds one OP_RETURN output to their own custody transaction: OP_RETURN PUSH2 "gw" PUSH1 0x09 PUSHDATA payload, where the
    payload is the comet's whole attestation packet, the pass with its custody log, plus the opening for the hop this transaction performs. Watchers
    verify it with the same code they use for a mailed attestation, so a stranger can learn the identity from the chain alone
    (ops/doc/opret-revision/04-decisions-addendum.md §0). The cap is 1,024 bytes, which is the packet bound stated once. You can publish at spawn or
    years later, and it is one way forever. Because the publication rides the owner's custody transaction, only the sat holder can declassify.
  ```

  ...in practice, every consumer-facing wallet we tried rejected all of these transactions (as well as some variations on them) for security reasons. Normal wallets don't want their users accidentally spending sats in arbitrarily programmatic ways.

  So we wrote our own program for managing your Groundwire ID: Causeway, a Python script which generates a new Bitcoin wallet, waits for funding, generates an identity with the correct sat tweaked in, broadcasts the transaction, and saves your `xtr` data locally. If you need to rotate your keys or change your sponsor, you use Causeway to broadcast those transactions, and it will give you a poke to run on your ship to update `xtr` via the `%gw-btc` Gall agent.


TO-DO:
```
  What the doc is missing


  - Sponsorship. Off chain and optimistic: a comet that commits a sponsor in its snapshot connects to it and attests like any peer, and the sponsor's
    acceptance is the moment it sees that attestation. An absent sponsor projects to self. A sponsor must commit a fief, which is how peers reach it. The
    star in a comet's name is only the kernel's default sponsor; the real one is the comet in the snapshot, and %gw-btc tells Jael.
  - elaboration on key rotations and other events. Rekey and breach. Life and rift semantics, and the rule that every snapshot change increments life so life is a total order.
  - The light client as trust root. Verification reads the chain through %bitcoin-client from gwbtc/node, tip liveness is a BIP-158 filter scan since
    there is no spent-output lookup, every unavailable filter fails closed, and a reorg forgets points without snubbing.
  - The public index. The scanner, the gevulot index with its provenance model, what /x/points holds and what it withholds, and why confidential points
    never enter the udiff stream.
  - Versioning and the no-migration stance. Kelvin in dat, retiring a kelvin is a breach-class network decision, and the network is bootstrapped rather
    than migrated.
  - Recovery. Nothing regenerates the ring, the refreshed pass is not written back to the keyfile, and the proof-backup problem is unsolved.
  - Invites, mnemonyms, and the runtime. Prepaid spawns as bearer codes, the dot-separated rendering versus the @p, and the vere changes: feed boot, the
    name guard, the removed gateway check, the miner, the sidecar.
  ```