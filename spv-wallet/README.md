# SPV Wallet for Urbit

A Bitcoin SPV (Simplified Payment Verification) wallet built entirely in Hoon, running natively on Urbit. Manage Bitcoin wallets with full cryptographic verification without running a full node.

## Features

### Currently Working

**Wallet Management**
- Generate BIP39 seed phrases (12/15/18/21/24 words)
- Import existing seed phrases with validation
- Multi-wallet support with labeled accounts
- BIP32 hierarchical deterministic key derivation
- Multiple accounts per wallet with custom labels

**Bitcoin Cryptography**
- Full BIP39/BIP32 implementation in pure Hoon
- Address generation (P2PKH, P2WPKH, P2SH-P2WPKH, P2TR)
- Transaction signing (P2WPKH, P2SH-P2WPKH, P2PKH)
- BIP-143 SegWit sighash with official test vector validation
- Comprehensive test suites with cross-implementation validation
- Merkle proof verification for SPV

**Web Interface**
- Clean, responsive Sailbox-based UI
- Wallet creation and management
- Account and address exploration
- Real-time updates via Server-Sent Events

**Development Tools**
- Native Hoon test suite
- Parsing tutorial with Bitcoin-specific examples
- Comprehensive test coverage

## Quick Start

### Installation

1. Clone this repository
2. Copy `config.example.json` to `config.json`
3. Set your ship's pier path in `config.json`
4. Mount the desk on your ship:
   ```
   |mount %spv-wallet
   ```

### Development Workflow

**Live Sync** (recommended):
```bash
./sync.sh
```
This watches for file changes and automatically syncs to your ship.

**Manual Sync**:
1. Copy files to your mounted desk
2. Commit changes:
   ```
   |commit %spv-wallet
   ```

### Access the Wallet

Navigate to `http://localhost/spv-wallet` (or your ship's URL) to access the web interface.

## Architecture

### Single Sailbox App

The wallet is built as a single Sailbox application (`/app/spv-wallet.hoon`) that handles:
- HTTP request routing and SSE streaming
- State management for wallets, accounts, and addresses
- UI rendering with server-side Sail
- Integration with Bitcoin blockchain APIs

### Organized Library Structure

**Core Bitcoin Libraries** (`/lib/`):
- `bip39.hoon` - Mnemonic seed phrase generation/validation
- `bip32.hoon` - HD wallet key derivation
- `bip329.hoon` - Wallet label import/export
- `bitcoin.hoon` - Bitcoin primitives and address generation
- `bitcoin-spv.hoon` - SPV verification (merkle proofs, headers)
- `seed-phrases.hoon` - Seed phrase management
- `transactions.hoon` - Transaction building and signing (P2WPKH, P2SH-P2WPKH, P2PKH)

**Request Handlers** (`/lib/rt/`):
- `wallet.hoon` - Wallet management endpoints
- `account.hoon` - Account operations
- `send.hoon` - Transaction sending
- `spv.hoon` - SPV verification endpoints

**UI Components** (`/lib/ui/`):
- `spv-wallet.hoon` - Main layout and pages
- `wallets.hoon` - Wallet list and details
- `accounts.hoon` - Account management
- `addresses.hoon` - Address display
- `primitives.hoon` - Reusable UI components

**Wallet Logic** (`/lib/wallet/`):
- `address.hoon` - Address derivation
- `discovery.hoon` - Gap limit scanning (in progress)
- `mempool-space.hoon` - Blockchain API integration

### Comprehensive Tests

All core functionality is tested with native Hoon tests in `/tests/lib/`:
- `seed-phrases.hoon` - BIP39 test vectors and edge cases
- `bitcoin-spv.hoon` - Merkle proof verification
- `bitcoin-spv-merkle.hoon` - Individual merkle operations
- `bip32.hoon` - HD wallet derivation
- `bip329.hoon` - Label format parsing
- `transactions.hoon` - Transaction signing with BIP-143 and cross-implementation validation
- `parsing-tutorial.hoon` - Parser examples

Run tests:
```
-test /=spv-wallet=/tests/lib/seed-phrases
-test /=spv-wallet=/tests/lib/bitcoin-spv
```

## Trust Model

**Blockchain Data**: Fetched from public APIs (mempool.space, blockstream.info)

**What We Trust**:
- APIs to provide correct blockchain data
- APIs for data availability

**What We Verify Locally**:
- Merkle proof correctness
- Block header proof-of-work
- Transaction inclusion in blocks
- Address derivation from seeds
- Seed phrase checksums

All cryptographic operations happen locally in Urbit. APIs cannot forge proofs or compromise wallet security.

## Documentation

- **`/docs/cheatsheets/`** - Bitcoin technical references (script opcodes, address types, crypto primitives)
- **`/docs/lib/bip32.md`** - BIP32 specification documentation
- **`/docs/prior-art.md`** - Index of existing Bitcoin implementations we reference
- **`spec.md`** - Project goals and specifications

## Development

### Project Structure
```
spv-wallet/
├── desk/
│   ├── app/spv-wallet.hoon          # Main Sailbox app
│   ├── lib/                          # Bitcoin & wallet libraries
│   │   ├── bip32.hoon, bip39.hoon   # BIP implementations
│   │   ├── bitcoin-spv.hoon         # SPV verification
│   │   ├── rt/                      # Request handlers
│   │   ├── ui/                      # UI components
│   │   └── wallet/                  # Wallet logic
│   ├── sur/                          # Type definitions
│   └── tests/lib/                    # Comprehensive tests
├── docs/                             # Documentation
├── spec.md                           # Project specification
└── sync.sh                           # Development sync script
```

### Adding Features

1. Implement pure functions in `/lib/`
2. Add comprehensive tests in `/tests/lib/`
3. Create request handlers in `/lib/rt/`
4. Build UI components in `/lib/ui/`
5. Wire up routing in `/app/spv-wallet.hoon`

### Testing

Test-driven development with official BIP test vectors:
```
# Test specific libraries
-test /=spv-wallet=/tests/lib/seed-phrases
-test /=spv-wallet=/tests/lib/bitcoin-spv
-test /=spv-wallet=/tests/lib/bip32
```

## Contributing

This is an active development project. The architecture is stabilizing around the Sailbox single-app pattern.

## License

[License TBD]
