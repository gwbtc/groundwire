# BIP32 Library
`/lib/bip32.hoon`

HD wallet key derivation.

This core represents a single node in the HD wallet tree. Each node contains the 6 standard pieces of BIP32 data needed to derive children or extract keys:

1. **Private key** (`prv`) - 256-bit secret (0 if watch-only)
2. **Public key** (`pub`) - Elliptic curve point
3. **Chain code** (`cad`) - 256-bit entropy for derivation
4. **Depth** (`dep`) - How many levels from master (0 = master)
5. **Index** (`ind`) - Child number at this depth
6. **Parent fingerprint** (`pif`) - First 4 bytes of parent's key hash

**Key Concepts:**
- **Hardened derivation** (index ≥ 2^31): Requires private key, more secure isolation
- **Normal derivation** (index < 2^31): Can derive from public key only, enables watch-only wallets
- Most functions that return a core return a **new modified core** representing a different node

## Imports
```hoon
=,  hmac:crypto       :: HMAC-SHA512 for key derivation
=,  secp:crypto       :: secp256k1 curve operations  
=+  ecc=secp256k1     :: alias for the curve itself
```

- `hmac:crypto` - Exposes HMAC functions, particularly `hmac-sha512l` used in child key derivation
- `secp:crypto` - Exposes elliptic curve primitives (point arithmetic, compression)
- `ecc=secp256k1` - Binds the actual secp256k1 curve parameters (generator point, curve order)

## Types
- `+$  keyc` - `[key=@ cai=@]` - key + chain code pair (used by init functions)

## State
```hoon
|_  [prv=@ pub=point.ecc cad=@ dep=@ud ind=@ud pif=@]
```

## Init (Returns New Core)
These functions return a new core representing a node at a specific position:

- `++from-seed` - create master from BIP39 seed
  - 512-bit seed → **master node** (depth 0, index 0)
- `++from-private` - create from raw key material
  - private key + chain code → **node with unknown position** (depth/index/parent = 0)
- `++from-public` - create watch-only from compressed key
  - public key + chain code → **watch-only node with unknown position** (depth/index/parent = 0)
- `++from-public-point` - create watch-only from EC point
  - EC point + chain code → **watch-only node with unknown position** (depth/index/parent = 0)
- `++from-extended` - parse xpub/xprv string
  - xpub/xprv string → **node at encoded position** (includes all metadata)

## Derive (Returns New Core)
These functions return a new core representing a child node:

- `++derive-path` - derive using BIP32 path string
  - "m/84'/0'/0'/0/0" → **node at specified path**
  - Before: any depth/index → After: depth = path segments, index = last segment (e.g. depth 5, index 0)
- `++derive-sequence` - derive multiple levels sequentially (uses `++derive` for each)
  - list of indices → **final node only** (not intermediate nodes)
  - Before: depth N → After: depth N + (length of list), index = last in list
- `++derive` - auto-route: uses `++derive-private` if private key exists, else `++derive-public`
  - single index → **direct child node**
  - Before: depth N, any index → After: depth N+1, index = provided arg
- `++derive-private` - derive using private key (supports both normal and hardened)
  - index (any value) → **child node**
  - Before: depth N, any index → After: depth N+1, index = provided arg
- `++derive-public` - derive using public key only (cannot do hardened)
  - index (must be < 2^31) → **watch-only child node**
  - Before: depth N, any index → After: depth N+1, index = provided arg

## Extract
- `++private-key` - get private key atom
  - core → private key
- `++public-key` - get compressed public key
  - core → 33-byte public key
- `++chain-code` - get chain code
  - core → 256-bit chain code
- `++private-chain` - get key + chain code pair
  - core → [private-key, chain-code]
- `++public-chain` - get pubkey + chain code pair
  - core → [public-key, chain-code]
- `++identity` - compute key hash
  - core → hash160 of public key
- `++fingerprint` - compute 4-byte fingerprint
  - core → first 4 bytes of identity
- `++address` - generate legacy Bitcoin address
  - network → base58check address
- `++prv-extended` - encode as xprv string
  - network → xprv string
- `++pub-extended` - encode as xpub string
  - network → xpub string

## Helpers
- `++point` - convert private key to public point
  - private key → EC point
- `++ser-p` - compress EC point to 33 bytes
  - EC point → 33-byte compressed key
- `++n` - get curve order constant
  - → secp256k1 order
- `++set-metadata` - update depth/index/parent
  - [depth, index, parent] → **modified core with new metadata**
- `++derivation-path` - parse BIP32 path strings
  - "m/44'/0'/0'/0/0" → list of indices
- `++build-extended` - assemble extended key data
  - key → extended key bytes
- `++en-b58c-bip32` - encode extended key format
  - [version, key-data] → base58 string
- `++en-base58check` - add checksum and base58 encode
  - [version-bytes, data] → base58check string
- `++de-base58check` - decode and verify checksum
  - base58 string → raw bytes
- `++hash160` - compute SHA256 then RIPEMD160
  - data → 20-byte hash
- `++version-bytes` - get network-specific version bytes
  - [network, type, bip32?] → version bytes