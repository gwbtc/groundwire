# Groundwire Onboarding

`%urb-watcher` is Groundwire's equivalent of `%azimuth` and `%eth-watcher`. It regularly polls Bitcoin for comet "attestations" and updates its ship's Jael state accordingly.

Generating and booting a Groundwire ship happens in the following steps:

1. A Bitcoin UTXO is identified and used as a key tweak when mining a new comet. This binds the comet to the first sat of this UTXO in the same way that Azimuth ships are bound to their corresponding NFTs.
2. After mining the comet but before booting it, a `%spawn` attestation is submitted to Bitcoin that includes the comet's public networking key, notifying all Groundwire ships running `%urb-watcher`. 
3. We bundle an `%escape` attestation alongside the `%spawn`. Groundwire allows sponsors to sign a potential sponsee's `%escape` transaction off-chain, so that when the `%escape` transaction is submitted everyone knows that the sponsor consents without needing to wait for an `%adopt` attestation. For ease of onboarding, the Groundwire Foundation runs a sponsor which exposes an HTTP endpoint that any onboarding comet can query for a signature. `sponsor-sign-desk` contains this code.
4. After these transactions have been confirmed on-chain, we can boot the ship. At boot time, Vere performs a one-time check against a remote "gateway" ship's Jael state to ensure that the user isn't attempting to boot with a bad keyfile. Groundwire Foundation runs this gateway ship as well. Users who want to boot entirely trustlessly can skip this check with the `-Dunsafe-dawn=true` flag.
5. After booting, the new ship's `%urb-watcher` agent will take some time to get synced to the chain. (Eventually we'll make this more performant with snapshotting.)

Groundwire's current onboarding script automatically generates a new Bitcoin address, queries a testnet faucet for a funding UTXO, mines a comet, submits the `%spawn` and `%escape` transactions (requesting a signature from the hardcoded sponsor for the `%escape`), waits a few blocks, and then boots the ship.