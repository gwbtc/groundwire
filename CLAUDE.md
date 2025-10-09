# CLAUDE.md

## Overview

This is the userspace desk for Groundwire, a project which modifies Urbit’s Arvo, Vere, and UrCrypt components to support PKIs other than Azimuth. Groundwire also establishes a Bitcoin PKI using [ordinal inscriptions](https://docs.ordinals.com/inscriptions.html).

The entire codebase is written in Hoon, with a `build.sh` script and `peru.yaml` file at the root of the repo. The build script creates a `/dist` folder, which contains everything in `/desk` along with dependencies from other remote desks declared in `peru.yaml`.

## State of the codebase

This desk contains the `%ord-watcher` agent, supporting libraries, and unit tests. `%ord-watcher` queries a Bitcoin RPC node and extracts unvault transactions that reveal attestations to Urbit PKI changes. `%ord-watcher` converts the attested changes into `$udiff`s which Jael (Arvo’s PKI module) uses to diff its state and notify subscribers who may be other Urbit ships.

The supporting libraries in this desk are imported as dependencies by other Groundwire projects like [`gwbtc/spv-wallet`](https://github.com/gwbtc/spv-wallet), so this repo may evolve into something of a monorepo used to build various desks.

### %ord-watcher data flow

1. The `+get-blocks` thread in `%ord-watcher` sequentially retrieves blocks from the Bitcoin RPC node configured in `%ord-watcher`’s `.req-to`.
2. The `+elab-block` arm in that thread extracts Urbit attestations from the witness data in that block’s transactions.
3. `+find-block-deps` identifies data dependencies by finding transaction inputs referencing outputs that contain Urbit attestations.
4. `+handle-block` calls `+handle-tx` for each transaction in the block, updates `+ord-core` state accordingly.
5. `+handle-tx` handles relevant Bitcoin transactions by calling `+process-unv` for Urbit attestations and then `+sont-track-input` to handle sat ownership transfers. (Note the commented-out `+check-for-insc`.)
6. `+process-unv` parses and validates Urbit attestations from witness data. This handles PKI changes like spawning new points, key rotations, sponsorships changes, etc.
7. `%ord-watcher` receives new `$effect`s and `$state` from `+ord-core` and sends `$udiff` updates to Jael and other subscribers.

### Issues and unanswered design questions

The main issues and unanswered questions in this codebase are as follows:
* The libraries are imported from a few projects that integrated Bitcoin and Urbit over the years, and as a result there are a lot of overlapping/redundant type definitions and other bits of functionality.
* The `/lib/ord` library is original to Groundwire and contains the heart of `%ord-watcher`, the `+ord-core` engine. This core has some missing pieces and open design questions that Groundwire’s original developers didn’t resolve, such as:
  * How to set management proxies for Urbit ships.
    * Urbit’s `naive.hoon` has Hoon logic for setting management proxies but it’s not clear how much of that pertains to Groundwire’s non-hierarchical, “wild-west” sponsorship chains.
  * Incomplete `%fief` handling for domain updates, parsing `$turf` updates.
    * The `%fief` updates pertain to the `$fief` type that Groundwire adds to Jael. This genericizes our particular use-case, which is solely to support Bitcoin ordinal inscriptions as a store of PKI updates.
    * `%fief` updates may be onchain attestations to {domain/turf}, IPv4 or IPv6 addresses.
  * Handling ordinal inscriptions in the `+check-for-insc` arm.
  * Handling signatures for key rotation updates.
  * Incomplete `+spend-point` functionality for verifying who can spend the Bitcoin UTXO that represents ownership of an Urbit ID. The question is how to adapt Azimuth’s proxy model on Ethereum to Bitcoin UTXOs.

## Tutorials

You can use an Urbit ship’s Dojo using [Clurd](https://github.com/niblyx-malnus/clurd). If asked to use the Dojo or Clurd, look for a `/clurd` git repo on the local machine (if it's not in or beneath the parent directory of this repo, ask the user for a filepath) and follow the `README.md` and `config.json` files in there.

### Building the desk

To build code changes, follow these steps:
* Make sure there’s a local fakeship with a `/groundwire` directory. (Local fakeships are probably in or beneath the parent directory of this repo. There may be several fakeships, so consult the Clurd config for the right one.)
  * If no `/groundwire` directory exists (run `+vats` in the Dojo to check) but there is an existing fakeship configured in the Clurd repo, run these commands in the Dojo to create that desk and expose it to the host machine’s filesystem:
    * `|new-desk %groundwire`
    * `|mount %groundwire`
* Run `./build.sh -p /path/to/fakeship/groundwire`.
* In the fakeship’s Dojo using Clurd, run...
    * `|commit %groundwire` to commit code changes
    * `|install our %groundwire` to boot agents if this desk was just created
* Watch for Dojo output, which will either be a list of changes or the stack trace for a compile-time error.

### Running tests

This desk has integration tests for agents and unit tests for libraries.

To run the integration test for agents in `/desk/app`, run:

```dojo
-test /=groundwire=/tests/app
```

To run the unit tests for libraries, run:

```dojo
-test /=groundwire=/tests/lib
```

The command above could take something like 40-60 seconds to execute.

If you ever want to run a specific arm in a `/tests` file, specify the arm like so

If necessary, you can also run specific testing arms. For example, to run `+test-foo-bar` in a `/tests/lib/example.hoon` file, you'd run:

```dojo
-test /=groundwire=/tests/lib/example/test-foo-bar
```

There’s also a file `/desk/tests/unv.hoon` which is for populating a Bitcoin testnet with Groundwire transactions. You should ignore this for now until we have better tooling for declaratively controlling the Bitcoin testnet environment from within the desk.

