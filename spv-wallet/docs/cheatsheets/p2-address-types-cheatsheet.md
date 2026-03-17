# Bitcoin P2 Address Types Cheat Sheet

## What P2 Means
**P2___ = "Pay to ___"** - These define HOW Bitcoin outputs can be spent.

Each type specifies:
1. **ScriptPubKey**: The "lock" on the coins (spending conditions)
2. **Unlocking method**: What's needed in ScriptSig/Witness to spend
3. **Address format**: How users see the receiving address

---

## The Essential Types (99% of Bitcoin)

### **P2PKH (Pay to Public Key Hash) - Legacy Standard**
- **Address**: Starts with `1` (mainnet), `m`/`n` (testnet)
- **Example**: `1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa`
- **ScriptPubKey**: `OP_DUP OP_HASH160 <20-byte-pubkey-hash> OP_EQUALVERIFY OP_CHECKSIG`
- **To spend**: `<signature> <public_key>` in ScriptSig
- **Use case**: Standard single-signature addresses (pre-SegWit)

### **P2WPKH (Pay to Witness Public Key Hash) - Native SegWit**
- **Address**: Starts with `bc1q` (mainnet), `tb1q` (testnet)  
- **Example**: `bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4`
- **ScriptPubKey**: `OP_0 <20-byte-pubkey-hash>`
- **To spend**: `<signature> <public_key>` in Witness
- **Benefits**: Lower fees, fixes malleability, smaller transactions

### **P2SH (Pay to Script Hash) - Wrapped Complex Scripts**
- **Address**: Starts with `3` (mainnet), `2` (testnet)
- **Example**: `3J98t1WpEZ73CNmQviecrnyiWrnqRhWNLy`
- **ScriptPubKey**: `OP_HASH160 <20-byte-script-hash> OP_EQUAL`
- **To spend**: `<data...> <actual_script>` in ScriptSig
- **Use cases**: Multisig, wrapped SegWit, complex conditions

---

## Complete P2 Type Reference

### **Original Bitcoin Types**

#### **P2PK (Pay to Public Key) - Obsolete**
- **ScriptPubKey**: `<33-byte-pubkey> OP_CHECKSIG`
- **To spend**: `<signature>`
- **Status**: Rarely used (wastes space, exposes pubkey)

#### **P2PKH (Pay to Public Key Hash)**
- **Era**: 2009-present (legacy standard)
- **Fee efficiency**: Medium
- **Privacy**: Good (pubkey hidden until spent)

#### **P2SH (Pay to Script Hash)**
- **Era**: 2012-present (BIP 16)
- **Innovation**: Moves complexity to spending time
- **Sender**: Doesn't need to know spending complexity

### **SegWit Types (2017+)**

#### **P2WPKH (Pay to Witness Public Key Hash)**
- **Era**: 2017-present (native SegWit v0)
- **vs P2PKH**: ~40% smaller transactions, lower fees
- **Malleability**: Fixed (TXID can't be changed)

#### **P2WSH (Pay to Witness Script Hash)**
- **Address**: Starts with `bc1q` (longer than P2WPKH)
- **Example**: `bc1qrp33g0q4c70dhzw0r4k2h5t8zllemhxvh6g7d2yge2h3wqr8qf5zxy3gn`
- **ScriptPubKey**: `OP_0 <32-byte-script-hash>`
- **To spend**: Script data in witness
- **Use cases**: SegWit multisig, complex SegWit scripts

#### **P2SH-P2WPKH (Wrapped SegWit)**
- **Address**: Starts with `3` (looks like regular P2SH)
- **Purpose**: SegWit benefits with legacy wallet compatibility
- **Method**: P2WPKH wrapped inside P2SH
- **Tradeoff**: Slightly larger than native SegWit

#### **P2SH-P2WSH (Wrapped SegWit Script)**
- **Address**: Starts with `3`
- **Purpose**: SegWit script benefits with legacy compatibility
- **Method**: P2WSH wrapped inside P2SH

### **Taproot Types (2021+)**

#### **P2TR (Pay to Taproot)**
- **Address**: Starts with `bc1p` (mainnet), `tb1p` (testnet)
- **Example**: `bc1p5cyxnuxmeuwuvkwfem96lqzszd02n6xdcjrs20cac6yqjjwudpxqkedrcr`
- **ScriptPubKey**: `OP_1 <32-byte-pubkey>`
- **Innovation**: Looks like single-sig, can hide complex scripts
- **Benefits**: Privacy, efficiency, Schnorr signatures

---

## Quick Reference by Address Prefix

| Prefix | Type | Network | Era | Example |
|--------|------|---------|-----|---------|
| `1`    | P2PKH | Mainnet | 2009+ | `1A1zP1eP...` |
| `3`    | P2SH | Mainnet | 2012+ | `3J98t1Wp...` |
| `bc1q` | P2WPKH/P2WSH | Mainnet | 2017+ | `bc1qw508d6...` |
| `bc1p` | P2TR | Mainnet | 2021+ | `bc1p5cyxn...` |
| `m/n`  | P2PKH | Testnet | 2009+ | `n4VQ5YdH...` |
| `2`    | P2SH | Testnet | 2012+ | `2MzQwSS...` |
| `tb1q` | P2WPKH/P2WSH | Testnet | 2017+ | `tb1qw508...` |
| `tb1p` | P2TR | Testnet | 2021+ | `tb1p5cyx...` |

---

## When to Use Each Type

### **For Receiving Bitcoin:**
- **Modern wallets**: Use P2WPKH (bc1q...) - lowest fees
- **Compatibility needed**: Use P2SH-P2WPKH (3...) - works with old wallets
- **Maximum privacy**: Use P2TR (bc1p...) - latest standard
- **Legacy only**: Use P2PKH (1...) - highest fees

### **For Complex Scripts:**
- **Multisig (modern)**: P2WSH or P2SH-P2WSH
- **Multisig (legacy)**: P2SH
- **Complex conditions**: P2SH or P2WSH
- **Privacy-focused complex scripts**: P2TR

### **For Development/Testing:**
- **Testnet**: Same patterns with testnet prefixes
- **Regtest**: Usually P2WPKH for efficiency

---

## Fee Comparison (Approximate)

| Type | Relative Size | Relative Fee |
|------|---------------|--------------|
| P2PK | 100% | Highest |
| P2PKH | 95% | High |
| P2SH-P2WPKH | 70% | Medium |
| P2WPKH | 60% | Low |
| P2WSH | Variable | Low (per byte) |
| P2TR | 55% | Lowest |

---

## Security & Privacy Notes

### **Public Key Exposure:**
- **P2PK**: Pubkey visible immediately (bad)
- **P2PKH/P2WPKH**: Pubkey hidden until spent (good)
- **P2TR**: Uses different pubkey math (best privacy)

### **Script Privacy:**
- **P2SH/P2WSH**: Script hash visible, actual script hidden until spent
- **P2TR**: Can hide complex scripts completely (best privacy)

### **Quantum Resistance:**
- **All current types**: Vulnerable once public key is exposed
- **P2TR**: Somewhat better due to different key construction
- **Future**: Will need new address types for quantum resistance

---

## Migration Timeline

**2009**: P2PK, P2PKH
**2012**: P2SH enables multisig
**2017**: SegWit (P2WPKH, P2WSH, wrapped variants)
**2021**: Taproot (P2TR)
**Future**: Quantum-resistant address types?

The evolution shows Bitcoin's ability to upgrade through soft forks while maintaining backward compatibility!