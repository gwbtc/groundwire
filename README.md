# Groundwire

This repo contains the `%gw-btc` desk, the Causeway minting tool (`causeway/`), the installer users run (`causeway/public/boot.sh`) and the onboarding guides (`onboarding/`). The `%spv-wallet` desk in `spv-wallet/` is no longer shipped; the Wallet a ship runs comes with grubbery.

The `%gw-btc` desk was called `%groundwire` until it was renamed to match
the one PKI domain it serves. A ship installed from a release older than
that rename keeps an inert `%groundwire` desk; it can be uninstalled.


The `%gw-btc` desk runs two Gall agents (`gw-btc/desk.bill`):

- **`%gw-btc`** — scans Bitcoin for comet attestations and updates the ship's
  Jael accordingly. It reads the chain through the `%bitcoin-client` agent
  (via `++light-client-agent:lca`), not `%light-client`. This agent used to be
  called `%urb-watcher`; that name is retired and survives only in older docs
  and in the legacy `onboarding/booting/` tooling.
- **`%gevulot`** — the identity control pane. Sponsor-mediated peer
  discovery, the public index's start and provenance, and the derived view
  of every identity the ship holds (`ops/doc/gevulot-state-model.md`). It
  replaced `%urb-snapshot`, which published a monolithic index snapshot to
  S3: a sponsor now pushes attestations, each carrying its own proof.

## Branches and releases

- **`gw/release`** is the default and the only development branch: pull requests merge straight into it. `hd/cc-landing` was its name until 2026-11-03.
- gwbtc/urbit and gwbtc/vere work the same way, each with a `gw/release`, plus a `gw/upstream` that lags it and is what we offer the urbit organization's repos.
- **Releases are cut from [gwbtc/release](https://github.com/gwbtc/release)**, not from here: that repo's workflow pins every input of a release by commit (this repo's `gw/release` commit among them), builds the pill and the runtime, boots the pill, and publishes a prerelease on every merge. To ship a change from this repo, merge it here, then move the `GROUNDWIRE_BRANCH` pin there (`./bump GROUNDWIRE_BRANCH gw/release`) in a pull request. A merge here also deploys `%gw-btc` to the distribution ship for over-the-air updates.
- `boot.sh` installs the newest prerelease; a release promoted by clearing its prerelease flag is what the website's installer follows.

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