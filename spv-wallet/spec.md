# SPV Wallet Spec Workshop

## Agenda
- Draft the spec
- Notes

## Purpose
Deliver a Bitcoin wallet on Urbit that does not require a full node or third party trust to view balance, verify, and execute transactions using the SPV model.

## Goals
- **Wallet support**
  - Xpub
  - Bring your own seed
  - Generate seed
  - Urbit HD Wallet
- **View balance**
- **Verify transactions**
- **Sign and submit transactions with coin control**
- **Watch only addresses** (can't sign)
- **Dust management**
- **No address reuse**
- **Multisig support**
- **View individual UTXOs**
- **Tag individual UTXOs**
- **Support standard UTXO tagging import/export format**
  - Should be able to do all UTXO tagging in urbit and export to sparrow and others
- **Farbling**
- **Full PSBT support**
- **QR support on frontend** (if easy)
- **Simple debug interface** for development purposes. More to come.

## Non-Goals
- Full node on Urbit

## Technical Implementation
All information for the application will come either from the user or from HTTP requests to Bitcoin full nodes. Because Bitcoin nodes do not natively expose HTTP interfaces, we will need to use third parties or a sidecar.

### Data Sources
- Mempool.space