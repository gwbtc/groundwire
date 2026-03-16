# Bitcoin Cryptography Cheat Sheet

## Core Cryptographic Functions

### SHA-256
- **What**: Secure Hash Algorithm, 256-bit output
- **Bitcoin uses**: 
  - Block hashing (double SHA-256 for block IDs)
  - Transaction hashing (double SHA-256 for TXIDs)
  - Merkle tree construction
  - Mining proof-of-work
- **Example**: `SHA256("hello") → 0x2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824`

### RIPEMD-160
- **What**: RACE Integrity Primitives Evaluation Message Digest, 160-bit output
- **Bitcoin uses**: 
  - Address generation (part of HASH160)
  - Script hashing
- **Why**: Shorter output than SHA-256, different algorithm family for security diversity

### HASH160
- **What**: `RIPEMD160(SHA256(data))`
- **Bitcoin uses**:
  - Converting public keys to Bitcoin addresses
  - P2PKH and P2SH address generation
  - Parent fingerprints in BIP32
- **Example**: `pubkey → SHA256 → RIPEMD160 → address`

### HASH256 (Double SHA-256)
- **What**: `SHA256(SHA256(data))`
- **Bitcoin uses**:
  - Transaction IDs (note: displayed in reverse byte order)
  - Block hashes
  - Merkle tree nodes
- **Why double**: Protection against length extension attacks

### HMAC-SHA512
- **What**: Hash-based Message Authentication Code using SHA-512
- **Bitcoin uses**:
  - BIP32 HD wallet key derivation
  - BIP39 seed generation (via PBKDF2)
- **Format**: `HMAC-SHA512(key, message) → 64 bytes`
- **HD wallet**: Split output into private key (left 32 bytes) + chain code (right 32 bytes)

### PBKDF2
- **What**: Password-Based Key Derivation Function 2
- **Bitcoin uses**:
  - BIP39: Converting mnemonic words to 512-bit seed
- **Parameters**: 
  - Password: mnemonic phrase
  - Salt: "mnemonic" + optional passphrase
  - Iterations: 2048
  - Output: 512 bits

### ECDSA (Elliptic Curve Digital Signature Algorithm)
- **What**: Digital signature algorithm using elliptic curves
- **Bitcoin uses**:
  - Transaction signatures
  - Message signing
- **Curve**: secp256k1
- **Key sizes**: 
  - Private key: 256 bits (32 bytes)
  - Public key: 264 bits compressed (33 bytes) or 520 bits uncompressed (65 bytes)
  - Signature: ~72 bytes (DER encoded)

### Elliptic Curve Operations
- **Point multiplication**: `private_key * G = public_key`
  - G = generator point on secp256k1
  - One-way function (can't derive private from public)
- **Point addition**: Used in HD wallet unhardened derivation
  - `parent_public_key + (hash * G) = child_public_key`

## Common Encoding Functions

### Base58/Base58Check
- **What**: Bitcoin's human-readable encoding (no 0, O, I, l)
- **Uses**: 
  - Legacy Bitcoin addresses
  - WIF private keys
  - xpub/xpriv encoding
- **Base58Check**: Adds 4-byte checksum for error detection

### Bech32
- **What**: Modern address encoding (lowercase, error correction)
- **Uses**: 
  - Native SegWit addresses (bc1/tb1 prefix)
  - Lightning invoices
- **Benefits**: Better error detection, QR code efficiency

### DER Encoding
- **What**: Distinguished Encoding Rules for signatures
- **Uses**: Bitcoin transaction signatures
- **Format**: Standardized way to encode r,s signature values

## HD Wallet (BIP32) Specific

### Key Derivation Functions

#### Unhardened Child Derivation
```
Data = serP(parent_public_key) || ser32(index)
I = HMAC-SHA512(parent_chain_code, Data)
IL = I[0:32], IR = I[32:64]
child_private_key = (IL + parent_private_key) mod n
child_chain_code = IR
```
- Can derive child public keys from parent public key
- Vulnerable if child private key leaks

#### Hardened Child Derivation
```
Data = 0x00 || ser256(parent_private_key) || ser32(index)
I = HMAC-SHA512(parent_chain_code, Data)
IL = I[0:32], IR = I[32:64]
child_private_key = (IL + parent_private_key) mod n
child_chain_code = IR
```
- Requires parent private key for derivation
- No public key only derivation possible
- Stronger security isolation

### Serialization Functions
- **ser32(i)**: Serialize 32-bit integer (4 bytes, big-endian)
- **ser256(k)**: Serialize 256-bit integer (32 bytes, big-endian)
- **serP(P)**: Serialize elliptic curve point (33 bytes compressed)
- **parse256(b)**: Parse 32 bytes as 256-bit integer

## Bitcoin Script Operations

### OP_HASH160
- Performs HASH160 on stack top
- Used in P2PKH, P2SH scripts

### OP_CHECKSIG
- Verifies ECDSA signature
- Core of Bitcoin's spending authorization

### OP_CHECKMULTISIG
- Verifies m-of-n signatures
- Used in multisig wallets

## Transaction Building

### SIGHASH Types
- **SIGHASH_ALL**: Sign all inputs and outputs (default)
- **SIGHASH_NONE**: Sign all inputs, no outputs
- **SIGHASH_SINGLE**: Sign all inputs, one corresponding output
- **SIGHASH_ANYONECANPAY**: Can be combined with above, signs only one input

### Witness Transactions (SegWit)
- Signatures moved to witness data
- Different transaction ID calculation (doesn't include witness)
- Uses SHA256 for signature hashing (not double SHA256)

## Address Types & Their Crypto

### P2PKH (Pay to Public Key Hash)
- Address: `RIPEMD160(SHA256(public_key))`
- Script: `OP_DUP OP_HASH160 <pubkey_hash> OP_EQUALVERIFY OP_CHECKSIG`
- Starts with: 1 (mainnet), m/n (testnet)

### P2SH (Pay to Script Hash)  
- Address: `RIPEMD160(SHA256(script))`
- Script: `OP_HASH160 <script_hash> OP_EQUAL`
- Starts with: 3 (mainnet), 2 (testnet)

### P2WPKH (Pay to Witness Public Key Hash)
- Address: Bech32 encoded witness program
- Witness program: `[0x00, HASH160(public_key)]`
- Starts with: bc1 (mainnet), tb1 (testnet)

### P2WSH (Pay to Witness Script Hash)
- Address: Bech32 encoded witness program
- Witness program: `[0x00, SHA256(script)]`
- Starts with: bc1 (mainnet), tb1 (testnet)

## Security Considerations

### Why Double Hashing?
- **Length extension attacks**: Single SHA256 vulnerable in some contexts
- **Different contexts**: Block hashes vs merkle trees vs addresses

### Why Multiple Hash Functions?
- **Algorithm diversity**: SHA256 (NSA) + RIPEMD160 (European)
- **Defense in depth**: If one breaks, other provides backup
- **Size optimization**: RIPEMD160 for shorter addresses

### Key Security Properties
- **One-way**: Can't reverse hash to get input
- **Collision resistant**: Hard to find two inputs with same hash
- **Avalanche effect**: Small input change → completely different output
- **Deterministic**: Same input always produces same output