# Prior Art

This document indexes existing Urbit-specific Bitcoin wallet implementations across three key repositories: bitcoin-wallet, groundwire/volt, and groundwire/urbit. 

Each section defines a concept or aspect of Bitcoin wallet systems, then provides a directory of where to find related code and functionality in our existing prior art. This helps identify what's already been built and what patterns we can reuse when building new wallet applications.

## Wallet Generation/Population

### bitcoin-wallet/
- `bitcoin/lib/bip39.hoon` - BIP39 mnemonic generation and validation
- `bitcoin/lib/bip32.hoon` - HD wallet derivation and extended keys  
- `bitcoin/app/btc-wallet.hoon` - Main wallet application
  - **XPUB wallet management**: `%add-wallet` command accepts XPUB directly
  - **Address generation**: `%gen-new-address` generates from current wallet
  - **Account scanning**: Automatic address gap-limit scanning for transaction history
  - **Wallet state**: Maps XPUB → wallet data (`walts=(map xpub:bc walt)`)
  - **Multi-wallet**: Support multiple wallets with `%set-current-wallet`
  - **Payment flows**: Both internal ship-to-ship and external address payments
  - **Transaction building**: Full UTXO selection and change address handling
  - **Address watching**: Track addresses for incoming transactions (`wach=(map address addi)`)
- `bitcoin/tests/lib/bip39.hoon` - BIP39 test vectors
- `bitcoin/tests/lib/bip32.hoon` - BIP32 test vectors

### groundwire/urbit/
- `tests/sys/zuse/crypto/secp256k1.hoon` - Elliptic curve cryptography
- `tests/sys/zuse/crypto/pbkdf.hoon` - Key derivation functions

### groundwire/volt/
- `btc-desk/lib/bip32.hoon` - BIP32 implementation
- `btc-desk/lib/bip39.hoon` - BIP39 implementation 
- `btc-desk/tests/lib/bip32.hoon` - BIP32 tests
- `btc-desk/tests/lib/bip39.hoon` - BIP39 tests

## External Resources

### SPV Wallet Reference Implementations
- **Electrum** - Popular Python-based SPV wallet with extensive HD wallet support (GPL, GitHub)
- **BDK (Bitcoin Dev Kit)** - Rust library for building SPV wallets with modern APIs (MIT/Apache, GitHub)
- **Sparrow Wallet** - Desktop SPV wallet with comprehensive PSBT and hardware wallet support (Apache 2.0, GitHub)
- **Brave Wallet** - Browser-native multi-chain wallet with SPV Bitcoin support (MPL 2.0, GitHub: brave-wallet-core)
- **BlueWallet** - Mobile SPV wallet with watch-only and multisig capabilities (MIT, GitHub - app only)
- **Samourai Wallet** - Android SPV wallet with advanced privacy features (formerly open source, development ceased 2024)

### Technical References
- **BIP32** - Hierarchical Deterministic Wallets specification
- **BIP39** - Mnemonic code for generating deterministic keys
- **BIP44/49/84** - Multi-account hierarchy standards for different address types
- **BIP174** - Partially Signed Bitcoin Transaction (PSBT) format