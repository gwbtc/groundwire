# Confidential Comets

## The problem

The original Groundwire spawn protocol is a three-transaction chain:

1. **Precommit** — earmark a sat
2. **Commit** — spend the precommit sat into a taproot output whose internal key is tweaked by the hash of an urb-attestation tapleaf
3. **Reveal** — spend the commit via *script-path*, executing the tapleaf (which publishes the attestation)

Step 3 (the reveal) is a script-path spend of a tapleaf containing arbitrary bytes. Mainstream Bitcoin wallets refuse to sign this:

- **Sparrow, Electrum, BlueWallet** enforce Script Policies (miniscript / descriptor-based) and cannot express "spend this tapleaf that isn't in our wallet policy."
- **Coldcard, Passport, Keystone (current fw), Ledger** similarly will only sign key-path P2TR or tapleaves matching their descriptor.

You can sign script-path tapleaves with Bitcoin Core, a bespoke Node CLI tool, or Coldcard Q EDGE. But for a "pick any Bitcoin wallet" UX, reveals are a non-starter.

## The fix

Skip the reveal. The attestation is still committed on-chain (it's hashed into the commit output's tweaked internal key), but it is never revealed as a tapleaf spend. Instead:

- Causeway emits a `comet.proof.json` file alongside the usual boot feed.
- The proof contains everything a verifier needs to reconstruct the tapleaf hash and verify it hashes into the commit output's taproot key.
- The ship ingests `proof.json` post-boot (via `causeway import-proof` or the in-ship `%spv-wallet` agent) and stores it in Ames self-attestation state.
- When a peer contacts the ship for the first time, the ship presents `(attestation_bytes, proof)`. The peer verifies: `TapTweak(internal_pubkey, merkle_root) == commit_output_xonly_key`, then checks the commit tx is confirmed on Bitcoin.

The signer only ever sees a standard P2TR key-path spend. Every Bitcoin wallet can sign that.

## Sat-chain invariant and commit-output design

urb-core's `++is-sont-in-input` (`lib/urb-core.hoon` around L595) requires each management op's commit tx to spend the point's **current sont** — the single specific satoshi identifying the point on-chain. After spawn, that sont lives at `(spawn_commit_txid, 0)`. The next management op (rekey, escape, fief, …) MUST spend that UTXO as its input.

This means the commit output's taproot internal key cannot be something unspendable like the NUMS point — it has to be a key the point owner can sign with. Causeway uses the **funding UTXO's own xonly** as the commit output's internal key: the point owner already holds the private key (from the xpub derivation), so they can key-path-spend the commit output for the next op.

The attestation is still committed: the commit output's tweaked key is `internal_pubkey + TapTweak(internal_pubkey, merkle_root)·G`, and `merkle_root` is the hash of the tapleaf containing the encoded sotx. A verifier recomputes the tweak to confirm.

The tapleaf script itself is `OP_0 OP_IF "urb" <push-data…> OP_ENDIF <NUMS-xonly> OP_CHECKSIG` — the trailing `OP_CHECKSIG` uses NUMS so the tapleaf is script-path-*unspendable*, even though the key-path remains spendable. This prevents anyone (including the point owner) from revealing the attestation via script-path spend; it must be delivered off-chain via `proof.json`.

When the user performs the next management op on the point, their PSBT's input references `(prior_commit_txid, 0)` and sets `PSBT_IN_TAP_MERKLE_ROOT = merkle_root` so the external signer (Sparrow ≥ 1.8, etc.) can compute the taproot tweak and produce a valid key-path signature. `build_chained_commit_psbt` in `causeway.py` does this.

## proof.json schema (v1)

```json
{
  "version": 1,
  "op": "spawn",
  "patp": "~sampel-palnet",

  "commit_txid": "abcd…",
  "commit_vout": 0,
  "commit_value": 1000,
  "commit_script_pubkey_hex": "5120…",

  "internal_pubkey_hex": "…",
  "leaf_version": 192,
  "leaf_script_hex": "0063037572620c…ac",
  "merkle_path": [],
  "merkle_root_hex": "…",

  "attestation_bytes_hex": "…",
  "pass_atom_hex": "0x…",

  "network": "main",
  "funding": {
    "txid": "…",
    "vout": 0,
    "value": 3400,
    "path": "m/86'/0'/0'/0/3",
    "fingerprint_hex": "abcd1234"
  },
  "prior_proof": {
    "commit_txid": "…",
    "commit_vout": 0
  },
  "sponsor_sig": null
}
```

Field rules:

| Field | Type | Required | Notes |
| --- | --- | --- | --- |
| `version` | int | yes | Schema version. Currently `1`. Bumped on any breaking change. |
| `op` | string | yes | `"spawn"`, `"keys"`, `"escape"`, `"cancel-escape"`, `"adopt"`, `"reject"`, `"detach"`, `"fief"`. |
| `patp` | string | yes | The `@p` this proof is about. Leading `~` included. |
| `commit_txid` | hex string | after broadcast | Display-order txid (same form as block explorers). Empty/absent until the signed PSBT has been broadcast; verifiers MUST reject proofs without it. |
| `commit_vout` | int | yes | Output index of the commit output. Currently always `0`. |
| `commit_value` | int | yes | Value of the commit output in satoshis. |
| `commit_script_pubkey_hex` | hex string | yes | The tweaked P2TR `scriptPubKey` (`OP_1 PUSH_32 <xonly>`). Verifiers recompute from `internal_pubkey_hex` + `merkle_root_hex` and check match. |
| `internal_pubkey_hex` | hex string (32 B) | yes | The commit output's taproot internal x-only pubkey. Equals the funding UTXO's xonly under the current spendable-output design; the point owner holds the private key. |
| `leaf_version` | int | yes | BIP-342 leaf version byte. Currently always `0xc0` (192). |
| `leaf_script_hex` | hex string | yes | Tapleaf script bytes. `OP_0 OP_IF "urb" <push-data…> OP_ENDIF <NUMS> OP_CHECKSIG`. |
| `merkle_path` | array of hex strings | yes | Sibling hashes along the path from `leaf_script_hex` to the merkle root. Empty list for a single-leaf tree. Each sibling is folded in as `TaggedHash("TapBranch", min(a,b) \|\| max(a,b))`. |
| `merkle_root_hex` | hex string (32 B) | yes | Final taproot merkle root. For a single-leaf tree this equals `TapLeafHash(leaf_version, leaf_script)`. Used by the next management op's PSBT as `PSBT_IN_TAP_MERKLE_ROOT`. |
| `attestation_bytes_hex` | hex string | yes | Raw urb sotx bytes. Redundant with `leaf_script_hex` but convenient. |
| `pass_atom_hex` | hex string | spawn only | Networking pubkey (pass) derived from the mined ring. Absent for management ops. |
| `network` | string | yes | `"main"` or `"testnet"`. |
| `funding` | object | yes | Provenance of the sat entering this commit: `txid`, `vout`, `value`, `path` (BIP-32), `fingerprint_hex`. Spawn: the user's funding UTXO. Management: the prior commit's output. |
| `prior_proof` | object | management only | Back-pointer: `{commit_txid, commit_vout}` of the prior proof whose commit output this op spent. Verifiers walk this chain to confirm the sont hasn't forked. |
| `sponsor_sig` | hex string \| null | optional | For `%escape` with off-chain sponsor pre-sig (sponsor ed25519-signs `shaz(jam([sponsee, block_height]))`). |

Versioning rules:

- **Additive, backwards-compatible** changes (new optional fields, new `op` values) don't bump `version`.
- **Structural changes** (renamed / removed required fields, changed meaning) bump `version`. Consumers MUST reject unknown versions.

Verification (off-chain):

1. Parse, check `version == 1`.
2. Recompute `leaf_hash = TaggedHash("TapLeaf", leaf_version \|\| compact_size(leaf_script) \|\| leaf_script)`.
3. Walk `merkle_path` from `leaf_hash`, folding with `TaggedHash("TapBranch", min(a,b) \|\| max(a,b))`; result should equal `merkle_root_hex`.
4. Compute `output_key = internal_pubkey + TapTweak(internal_pubkey, merkle_root)·G`.
5. Check `commit_script_pubkey_hex[:2] == 5120` and `commit_script_pubkey_hex[2:]` equals `x_only(output_key)`.
6. (On-chain) Fetch `commit_txid` and verify it's confirmed and its `commit_vout` output's scriptPubKey matches.
7. (Chain) For management ops, fetch the commit tx and check `vin[0] == (prior_proof.commit_txid, prior_proof.commit_vout)`, then recurse into `prior_proof`.

## What's actually implemented in this pass

- Desktop Causeway generates the proof file and saves it locally.
- `gw-vere` accepts a `--proof` CLI flag that saves the file into `~/.groundwire/<patp>.proof.json` (no further consumption yet).
- `%spv-wallet` has a new `+import-proof` generator that stashes the proof in agent state.

**Not yet implemented** (out of scope for this pass):

- Runtime code that validates the proof on boot and injects it into Ames self-attestation state.
- Ames ships that request `(attestation, proof)` from unknown peers.
- `%urb-watcher` changes to index commit-only attestations given out-of-band proof.

Until that lands, confidential comets are locally valid but not network-discoverable. The feed atom alone is enough for the runtime to boot and claim the identity; the proof is preserved for later.

## Sponsor attestations (reveal still required)

Sponsor-side ops (a star adopting a fief, rejecting an escape, detaching a child) still require a reveal: other ships need to discover the attestation from an arbitrary indexer (urb-watcher) without a prior peer connection. Desktop Causeway does **not** support these. Use the in-ship `%spv-wallet` app instead, which has the seed in hot-wallet state and can sign script-path spends.

## The proof-backup problem

If you delete your pier, you lose your Ames state — including the proofs of every confidential attestation you ever made. Without the proofs, peers can't verify your comet identity. 

Christian's proposal: encrypt the proof bundle with a key derived from the commit-input's xpub, publish the ciphertext on-chain (e.g. in an `OP_RETURN` or a later key-path spend). Recovery: re-derive the xpub from the seed, locate the commit tx, decrypt the bundle. Out of scope for this pass — captured here so it's not forgotten.
