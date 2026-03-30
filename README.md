# Groundwire

This repo contains two Urbit desks: `%groundwire` and `%spv-wallet`. 

The `%groundwire` desk runs a background Gall agent called `%urb-watcher` which scans Bitcoin for comet attestations and updates the ship's Jael accordingly.

The `%spv-wallet` desk contains an SPV Wallet Gall app. We currently use this app for performing post-boot comet attestations, though this may change soon.

## Installation

If you're interesting in running a Groundwire ship, go to [groundwire.dev](https://groundwire.dev/) and use the one-line command.

To install these desks for development purposes, you'll need to be running a ship using versions of Arvo and Vere with Groundwire's modifications. Until these modifications get merged into the official distros, setting a ship up in this way is a bit of an esoteric process. 

See the onboarding directory for elaboration on the boot process and additional tooling we use.

This repo also contains a `vendor` directory containing shared libraries between the `%groundwire` and `%spv-wallet` desks. To install the two desks, you'll need to run `make build` to copy in `vendor`'s dependencies and generate a `dist` directory for each one, like so:

1. `|new-desk %groundwire`
2. `|mount %groundwire`
3. `make build`
4. `$ cp -r dist-groundwire/* path/to/zod/groundwire/`
5. `|commit %groundwire`
6. `|install our %groundwire`