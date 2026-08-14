# Groundwire

This repo contains two Urbit desks: `%gw-btc` and `%spv-wallet`.

The `%gw-btc` desk was called `%groundwire` until it was renamed to match
the one PKI domain it serves. A ship installed from a release older than
that rename keeps an inert `%groundwire` desk; it can be uninstalled.


The `%gw-btc` desk runs two Gall agents (`gw-btc/desk.bill`):

- **`%gw-btc`** — scans Bitcoin for comet attestations and updates the ship's
  Jael accordingly. It reads the chain through the `%bitcoin-client` agent
  (via `++light-client-agent:lca`), not `%light-client`. This agent used to be
  called `%urb-watcher`; that name is retired and survives only in older docs
  and in the legacy `onboarding/booting/` tooling.
- **`%urb-snapshot`** — publishes the point snapshot (`urb-snapshot.jam`) to
  an S3 bucket so clients can fetch it without talking to the ship.

The `%spv-wallet` desk contains an SPV Wallet Gall app. We currently use this app for performing post-boot comet attestations, though this may change soon.

## Installation

If you're interesting in running a Groundwire ship, go to [groundwire.io](https://groundwire.io/) and use the one-line command.

To install these desks for development purposes, you'll need to be running a ship using versions of Arvo and Vere with Groundwire's modifications. Until these modifications get merged into the official distros, setting a ship up in this way is a bit of an esoteric process. 

See the onboarding directory for elaboration on the boot process and additional tooling we use.

To bring up a Groundwire confidential comet from scratch — build the runtime and the pill, mint an identity with Causeway, boot the ship, install the desks, sync a Bitcoin light client and verify a peer — follow [`ops/doc/OPERATIONS.md`](ops/doc/OPERATIONS.md). It carries the known-good timings and the failure modes that have cost the most time.

This repo also contains a `vendor` directory containing shared libraries between the `%gw-btc` and `%spv-wallet` desks. To install the two desks, you'll need to run `make build` to copy in `vendor`'s dependencies and generate a `dist` directory for each one, like so:

1. `|new-desk %gw-btc`
2. `|mount %gw-btc`
3. `make build`
4. `$ cp -r dist-gw-btc/* path/to/zod/gw-btc/`
5. `|commit %gw-btc`
6. `|install our %gw-btc`