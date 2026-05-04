# SPV-Wallet MCP Integration Plan

## Overview

Add an MCP tools file to `%spv-wallet` following the same pattern as `%chorus`
(`~/gw/chorus/desk/fil/mcp-tools.hoon`). Each tool is a `thread-builder` that
runs as a Khan shed, sends HTTP requests to the `%spv-wallet` local web
interface via Urbit's Iris vane, and returns a JSON text response.

Because `%spv-wallet` already exposes all its actions as HTTP POST endpoints
(handled by `lib/rt/`), the threads drive it over localhost HTTP rather than
Gall pokes — no new poke mark or agent change is needed.

The file will live at `spv-wallet/fil/mcp-tools.hoon` and be registered with
the `%mcp` agent the same way chorus registers its tools.

---

## Makefile Change

`sur/mcp.hoon` is checked into the repo at `vendor/mcp/sur/mcp.hoon` — no
path assumption about the build machine needed. The developer should copy the
file from their local `%mcp` desk into the vendor tree and commit it:

```bash
mkdir -p vendor/mcp/sur
cp ~/gw/mcp/desk/sur/mcp.hoon vendor/mcp/sur/mcp.hoon
# then git add + commit as normal
```

After that the Makefile handles the rest.

Add to `../Makefile`:

```makefile
# Vendor files needed by spv-wallet desk (mcp)
VENDOR_MCP_SPV := \
    sur/mcp.hoon
```

In the `spv:` target, add after the btc-wall loop:

```makefile
    @for f in $(VENDOR_MCP_SPV); do \
        mkdir -p dist-spv/$$(dirname $$f); \
        cp vendor/mcp/$$f dist-spv/$$f; \
    done
```

The `mcp-tools.hoon` file itself is a `fil/` (file) entry, not vendored — it
lives directly in `spv-wallet/fil/mcp-tools.hoon` and is copied by the
`cp -r spv-wallet/* dist-spv/` step already in the Makefile.

---

## File Header (imports)

```hoon
/-  mcp, spider
/+  io=strandio
^-  (list tool:mcp)
```

No `/-  s=spv-wallet` needed — the threads don't inspect wallet state
directly; they just fire HTTP requests and parse the text responses.

---

## User-Facing Actions to Expose as MCP Tools

These are drawn from the `lib/rt/` action handlers. The list is ordered by
likely usefulness to an AI agent operating the wallet.

### Wallet Management (`lib/rt/wallet.hoon`)

| Tool name | Action | Key args | Notes |
|---|---|---|---|
| `spv__list-wallets` | — | — | Read-only scry; no action needed |
| `spv__generate-wallet` | `add-wallet-from-entropy` | `wallet-name` | Generates fresh BIP39 seed |
| `spv__import-wallet` | `add-wallet` | `wallet-name`, `seed-phrase`, `seed-format` | BIP39 or `@q` |
| `spv__remove-wallet` | `remove-wallet` | `pubkey` (hex) | Deletes wallet + boot state if boot wallet |
| `spv__add-watch-only` | `add-watch-only` | `account-name`, `xpub`, `script-type`, `network` | xpub/tpub only |
| `spv__add-signing` | `add-signing` | `account-name`, `xprv`, `script-type`, `network` | xprv/tprv only |
| `spv__delete-account` | `delete-watch-only` / `delete-signing` | `pubkey` (hex), `type` (`watch-only`\|`signing`) | Standalone accounts |

### Account Discovery (`lib/rt/wallet.hoon`)

| Tool name | Action | Key args | Notes |
|---|---|---|---|
| `spv__discover-accounts` | `discover-accounts` | `wallet-pubkey`, `purpose`, `coin-type` | BIP44 gap-limit scan |
| `spv__add-account` | `add-unlisted-account` | `wallet-pubkey`, `account-name`, `purpose`, `coin-type`, `account-number` | Manual derivation path |
| `spv__delete-wallet-account` | `delete-account` | `wallet-pubkey`, `account-path` | Removes from wallet's account map |

### Address Scanning (`lib/rt/account.hoon`)

| Tool name | Action | Key args | Notes |
|---|---|---|---|
| `spv__scan-account` | `full-scan` | `account-pubkey` | Runs BIP44 20-gap receiving+change scan |
| `spv__refresh-address` | `refresh-address` | `account-pubkey`, `chain`, `index` | Single address refresh |
| `spv__set-network` | `set-network` | `account-pubkey`, `network` | Switch active network |

### UTXO / Labels (`lib/rt/account.hoon`)

| Tool name | Action | Key args | Notes |
|---|---|---|---|
| `spv__label-utxo` | `set-output-labels` | `account-pubkey`, `utxo-txid`, `utxo-vout`, `labels` | BIP329 comma-separated labels |
| `spv__freeze-utxo` | `set-utxo-frozen` | `account-pubkey`, `utxo-txid`, `utxo-vout`, `frozen` | Prevents UTXO from auto-select |

### Transaction Building & Broadcasting (`lib/rt/send.hoon`)

| Tool name | Action | Key args | Notes |
|---|---|---|---|
| `spv__add-output` | `add-output` | `account-pubkey`, `address`, `amount` (sats) | Adds recipient to draft |
| `spv__remove-output` | `delete-output` | `account-pubkey`, `output-index` | Remove by index |
| `spv__add-input` | `add-input` | `account-pubkey`, `utxo-txid`, `utxo-vout`, `utxo-value` | Manual coin control |
| `spv__remove-input` | `remove-input` | `account-pubkey`, `utxo-txid`, `utxo-vout` | — |
| `spv__set-change` | `set-change-config` | `account-pubkey`, `fee-rate`, `change-address` | sat/vB fee rate |
| `spv__auto-select` | `run-auto-select` | `account-pubkey`, `mode` (`random`\|`largest-first`) | UTXO selection |
| `spv__clear-draft` | `clear-draft` | `account-pubkey` | Reset draft transaction |
| `spv__broadcast` | `build-transaction` | `account-pubkey` | Sign + broadcast; clears draft on success |

### SPV Header Sync (`lib/rt/spv.hoon`)

| Tool name | Action | Key args | Notes |
|---|---|---|---|
| `spv__set-checkpoint` | `set-checkpoint` | `network`, `height` | Reset SPV sync starting point |

### Comet Boot / Attestation (`lib/rt/boot.hoon`)

| Tool name | Action | Key args | Notes |
|---|---|---|---|
| `spv__start-boot` | `start` | `seed-phrase` (@q), `sponsor` (@p), optionally `boot-mode=fief`, `fief-ip`, `fief-port` | Runs full commit+reveal flow |
| `spv__cancel-boot` | `cancel` | — | Abort in-progress boot |
| `spv__retry-boot` | `retry` | — | Retry after error |

---

## Implementation Notes

### Thread structure

Each tool sends an HTTP POST to the `%spv-wallet` local server (Eyre on
localhost port 80, authenticated by the ship's session cookie) and reads back
the response body. Iris is accessed via `send-request:io` and
`take-client-response:io` from `strandio`.

The general shape of a mutating tool:

```hoon
^-  thread-builder:tool:mcp
|=  args=(map name:parameter:tool:mcp argument:tool:mcp)
^-  shed:khan
=/  m  (strand:spider ,vase)
^-  form:m
...arg extraction and validation...
=/  body=@t
  (crip "action=some-action&param1={(trip val1)}&param2={(trip val2)}")
=/  =request:http
  :*  %'POST'
      'http://localhost/spv-wallet/...'
      ~[['content-type' 'application/x-www-form-urlencoded']]
      `(as-octs:mimes:html body)
  ==
;<  ~                      bind:m  (send-request:io request)
;<  =client-response:iris  bind:m  take-client-response:io
%-  pure:m
!>  ^-  json
%-  pairs:enjs:format
:~  ['type' s+'text']
    ['text' s+(crip "Action completed.")]
==
```

The URL paths mirror the existing Eyre routes in `lib/rt/wallet.hoon` —
e.g. `POST /spv-wallet` for wallet actions, `POST /spv-wallet/wallet/<pubkey>`
for discovery actions, `POST /spv-wallet/account/<pubkey>` for account/scan
actions, `POST /spv-wallet/account/<pubkey>/send` for transaction actions,
`POST /spv-wallet/groundwire` for boot actions, and `POST /spv-wallet/spv` for
SPV actions.

Authentication: Eyre requires the `urbauth-~shipname` cookie. The thread
can obtain the session cookie via a scry or by reading ship state; alternatively
use the loopback interface which bypasses auth for same-ship requests (confirm
whether `%spv-wallet` opts into loopback in `app/spv-wallet.hoon`).

### `list-wallets` read tool

Send a `GET /spv-wallet` request and parse the response, or use `scry:io`
against `%gx/spv-wallet/wallets` if the agent exposes that path. The HTTP GET
approach is consistent with the Iris-only pattern and avoids depending on a
specific scry path.

### Sensitive data

`generate-wallet` returns a BIP39 seed phrase. The tool response must include
it so the user can back it up. Mark the description clearly that the seed is
only returned once. Do **not** expose the seed in list or read tools.

`start-boot` takes a `@q` seed — treat it as a secret; don't echo it back in
the response.

### `broadcast` tool

This is the highest-stakes action. Consider adding a `dry-run` boolean
parameter that returns the estimated fee and hex without broadcasting, so an
agent can confirm before sending.

---

## Files to Create / Edit

| Path | Change |
|---|---|
| `spv-wallet/fil/mcp-tools.hoon` | New — the tools list |
| `vendor/mcp/sur/mcp.hoon` | New — copy of `sur/mcp.hoon` from `%mcp` desk, to be added and committed by the developer |
| `../Makefile` | Add `VENDOR_MCP_SPV` variable and copy loop in `spv:` target |

No changes to `sur/spv-wallet.hoon` or `app/spv-wallet.hoon` — the existing
HTTP interface is sufficient.
