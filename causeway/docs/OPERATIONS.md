# Operations reference

Every Groundwire identity operation is an **sotx** (signed ord transaction) as
defined in `groundwire/groundwire/sur/urb.hoon`. Causeway builds each of these
by:

1. Constructing a `skim-sotx` value matching the op's field shape.
2. Encoding it bit-exactly using `protocol/encoder.ts` (an arm-by-arm port of
   `lib/urb-encoder.hoon`).
3. Wrapping the bytes in a `urb`-tagged Taproot leaf script.
4. Building **commit** and **reveal** PSBTs (BIP-174 + BIP-371) that move the
   ownership sat through the attestation.
5. Handing both to your hardware wallet for signing.

The user-facing operations:

| Op | What it does | Required fields |
| --- | --- | --- |
| **rekey** | Rotate the point's networking key. Optionally breach (discontinuity). | new pass (hex atom), breach flag |
| **escape** | Request a new sponsor. The new sponsor may sign off-chain first. | new sponsor @p, optional 512-bit sponsor sig |
| **cancel-escape** | Withdraw an in-flight escape request. | pending sponsor @p |
| **adopt** | Accept a child's escape to you. | child @p |
| **reject** | Refuse a child's escape request. | child @p |
| **detach** | Disown one of your current children. | child @p |
| **fief** | Pin a static networking endpoint (IPv4 or IPv6 + port) on-chain. | IP + port, or empty to clear |
| **set-mang** | Set a management proxy — a separate key or sponsor point that can sponsor others on your behalf. | pass hex (or empty to clear) |
| **spawn** | Bring a brand-new @p into existence. | precommit txid, vout, offset, optional prefix |

## Ordering & preconditions

- **Post-spawn ops** (`rekey`, `escape`, etc.) all spend the inscription UTXO
  as the commit input. urb-core's `is-sont-in-input` check rejects any tx
  where the point's current `sont.own` isn't an input to the reveal.
- **Commit must confirm before reveal.** Causeway broadcasts commit first and
  waits for mempool acceptance before broadcasting the reveal. If you run ops
  in quick succession, refresh the snapshot between them so Causeway picks up
  the new inscription UTXO.
- **Escape requires a sponsor signature** if you want the new sponsorship to
  take effect immediately. Request it out-of-band from the sponsor-signer
  service (`143.198.70.9:8081/apps/sponsor-signer/sign`) or coordinate with
  your intended sponsor directly.
- **Spawn requires a precommit satpoint.** In gw-onboard's flow this is the
  funding UTXO created by the faucet. In Causeway you supply the txid/vout/off
  manually; the UI could be extended to pick it from your wallet's UTXOs.

## Raw sotx shapes

From `sur/urb.hoon:33-45`:

```hoon
+$  single
  $%  [%spawn =pass fief=(unit fief) to=[spkh=@ux vout=(unit @) off tej]]
      [%keys =pass breach=?]
      [%escape parent=ship sig=(unit @)]
      [%cancel-escape parent=ship]
      [%adopt =ship]
      [%reject =ship]
      [%detach =ship]
      [%fief fief=(unit fief)]
      [%set-mang mang=(unit mang)]
  ==
```

Opcodes (from `lib/urb-encoder.hoon`):

| Op | Opcode |
| --- | --- |
| `%spawn` | 1 |
| `%keys` (rekey) | 2 |
| `%escape` | 3 |
| `%cancel-escape` | 4 |
| `%adopt` | 5 |
| `%reject` | 6 |
| `%detach` | 7 |
| `%set-mang` | 8 |
| `%fief` | 11 |
| `%batch` | 10 |

`%batch` wraps multiple singles; Causeway emits it only for the spawn+escape
combo (first sat's birth plus its initial sponsor assignment).
