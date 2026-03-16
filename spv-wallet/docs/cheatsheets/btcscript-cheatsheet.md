# Bitcoin Script Cheat Sheet

## The Essential 20% (Learn These First)

These ~15 opcodes make up 80% of all Bitcoin transactions:

### The P2PKH Four (Most Important)
- **OP_DUP** - Duplicate top stack item
- **OP_HASH160** - Hash top item with SHA256+RIPEMD160
- **OP_EQUALVERIFY** - Check equality, fail if not equal
- **OP_CHECKSIG** - Verify signature against public key

**P2PKH Script Example:**
```
ScriptPubKey: OP_DUP OP_HASH160 <pubkey_hash> OP_EQUALVERIFY OP_CHECKSIG
ScriptSig: <signature> <public_key>
```

### Stack Manipulation
- **OP_DROP** - Remove top stack item
- **OP_SWAP** - Swap top two items

### Verification
- **OP_EQUAL** - Check if top two items are equal (leaves true/false)
- **OP_VERIFY** - Fail if top item is not true
- **OP_CHECKSIGVERIFY** - CHECKSIG + VERIFY combined

### Multisig
- **OP_CHECKMULTISIG** - Check m-of-n signatures
- **OP_2**, **OP_3** - Push numbers (for "2-of-3 multisig")

### Flow Control
- **OP_IF/OP_ELSE/OP_ENDIF** - Conditional execution
- **OP_RETURN** - Mark output as unspendable

### Time Locks
- **OP_CHECKLOCKTIMEVERIFY** - Require minimum block height/time
- **OP_CHECKSEQUENCEVERIFY** - Require relative time delay

---

## Complete Opcode Reference

### Constants (0x00-0x60)
```
0x00    OP_0, OP_FALSE         Push empty array
0x4c    OP_PUSHDATA1           Next byte contains data length
0x4d    OP_PUSHDATA2           Next 2 bytes contain data length  
0x4e    OP_PUSHDATA4           Next 4 bytes contain data length
0x4f    OP_1NEGATE             Push -1
0x50    OP_RESERVED            Invalid if executed
0x51    OP_1, OP_TRUE          Push 1
0x52-60 OP_2 through OP_16     Push 2-16
```

### Flow Control (0x61-0x6b)
```
0x61    OP_NOP                 Do nothing
0x63    OP_IF                  Execute if top stack true
0x64    OP_NOTIF               Execute if top stack false
0x65    OP_VERIF               DISABLED
0x66    OP_VERNOTIF            DISABLED
0x67    OP_ELSE                Else clause
0x68    OP_ENDIF               End if/else
0x69    OP_VERIFY              Fail if top stack false
0x6a    OP_RETURN              Mark as unspendable
```

### Stack Operations (0x6b-0x7e)
```
0x6b    OP_TOALTSTACK          Move to alt stack
0x6c    OP_FROMALTSTACK        Move from alt stack
0x6d    OP_2DROP               Remove top 2 items
0x6e    OP_2DUP                Duplicate top 2 items
0x6f    OP_3DUP                Duplicate top 3 items
0x70    OP_2OVER               Copy 3rd and 4th items to top
0x71    OP_2ROT                Move 5th and 6th items to top
0x72    OP_2SWAP               Swap top 2 pairs
0x73    OP_IFDUP               Duplicate if not zero
0x74    OP_DEPTH               Push stack size
0x75    OP_DROP                Remove top item
0x76    OP_DUP                 Duplicate top item
0x77    OP_NIP                 Remove second item
0x78    OP_OVER                Copy second item to top
0x79    OP_PICK                Copy nth item to top
0x7a    OP_ROLL                Move nth item to top
0x7b    OP_ROT                 Rotate top 3 items
0x7c    OP_SWAP                Swap top 2 items
0x7d    OP_TUCK                Copy top item before second
0x7e    OP_CAT                 DISABLED - Concatenate
0x7f    OP_SUBSTR              DISABLED - Substring
0x80    OP_LEFT                DISABLED - Left substring
0x81    OP_RIGHT               DISABLED - Right substring
0x82    OP_SIZE                Push length of top item
```

### Bitwise Logic (0x83-0x88)
```
0x83    OP_INVERT              DISABLED - Bitwise NOT
0x84    OP_AND                 DISABLED - Bitwise AND
0x85    OP_OR                  DISABLED - Bitwise OR
0x86    OP_XOR                 DISABLED - Bitwise XOR
0x87    OP_EQUAL               1 if equal, 0 otherwise
0x88    OP_EQUALVERIFY         OP_EQUAL + OP_VERIFY
0x89    OP_RESERVED1           Invalid if executed
0x8a    OP_RESERVED2           Invalid if executed
```

### Arithmetic (0x8b-0xa5)
```
0x8b    OP_1ADD                Add 1
0x8c    OP_1SUB                Subtract 1
0x8d    OP_2MUL                DISABLED - Multiply by 2
0x8e    OP_2DIV                DISABLED - Divide by 2
0x8f    OP_NEGATE              Negate
0x90    OP_ABS                 Absolute value
0x91    OP_NOT                 1 if 0, 0 otherwise
0x92    OP_0NOTEQUAL           1 if not 0, 0 otherwise
0x93    OP_ADD                 Add
0x94    OP_SUB                 Subtract
0x95    OP_MUL                 DISABLED - Multiply
0x96    OP_DIV                 DISABLED - Divide
0x97    OP_MOD                 DISABLED - Modulo
0x98    OP_LSHIFT              DISABLED - Left shift
0x99    OP_RSHIFT              DISABLED - Right shift
0x9a    OP_BOOLAND             1 if both not 0
0x9b    OP_BOOLOR              1 if either not 0
0x9c    OP_NUMEQUAL            1 if equal numbers
0x9d    OP_NUMEQUALVERIFY      OP_NUMEQUAL + OP_VERIFY
0x9e    OP_NUMNOTEQUAL         1 if not equal numbers
0x9f    OP_LESSTHAN            1 if less than
0xa0    OP_GREATERTHAN         1 if greater than
0xa1    OP_LESSTHANOREQUAL     1 if less than or equal
0xa2    OP_GREATERTHANOREQUAL  1 if greater than or equal
0xa3    OP_MIN                 Smaller of two
0xa4    OP_MAX                 Larger of two
0xa5    OP_WITHIN              1 if x within range [min,max)
```

### Crypto (0xa6-0xaf)
```
0xa6    OP_RIPEMD160           Hash with RIPEMD160
0xa7    OP_SHA1                Hash with SHA1
0xa8    OP_SHA256              Hash with SHA256
0xa9    OP_HASH160             SHA256 then RIPEMD160
0xaa    OP_HASH256             SHA256 twice
0xab    OP_CODESEPARATOR       Signature checking boundary
0xac    OP_CHECKSIG            Verify signature
0xad    OP_CHECKSIGVERIFY      OP_CHECKSIG + OP_VERIFY
0xae    OP_CHECKMULTISIG       Check multiple signatures
0xaf    OP_CHECKMULTISIGVERIFY OP_CHECKMULTISIG + OP_VERIFY
```

### Locktime (0xb0-0xb9)
```
0xb0    OP_NOP1                Reserved for soft forks
0xb1    OP_CHECKLOCKTIMEVERIFY Check if locktime passed (BIP65)
0xb2    OP_CHECKSEQUENCEVERIFY Check if relative time passed (BIP112)
0xb3-b9 OP_NOP4-10            Reserved for soft forks
```

### Invalid/Reserved (0xba-0xff)
```
0xba-0xff  Various            Unused, invalid if executed
```

## Key Patterns to Remember

### P2PKH (Pay to Public Key Hash) - Most Common
```
Lock:   OP_DUP OP_HASH160 <20-byte-hash> OP_EQUALVERIFY OP_CHECKSIG
Unlock: <signature> <public_key>
```

### P2SH (Pay to Script Hash)
```
Lock:   OP_HASH160 <20-byte-hash> OP_EQUAL
Unlock: <...data...> <serialized_script>
```

### 2-of-3 Multisig
```
Lock:   OP_2 <pubkey1> <pubkey2> <pubkey3> OP_3 OP_CHECKMULTISIG
Unlock: OP_0 <signature1> <signature2>  (OP_0 due to off-by-one bug)
```

### Provably Unspendable
```
OP_RETURN <data>  (Used for embedding data in blockchain)
```

## Script Execution Rules

1. **Combined Execution**: ScriptSig + ScriptPubKey are concatenated
2. **Stack-Based**: Operations manipulate a stack
3. **Success**: Script succeeds if stack has non-zero value after execution
4. **Failure**: Script fails if:
   - VERIFY operation fails
   - Script ends with empty stack or false
   - Disabled opcode executed
   - Stack size limits exceeded

## Size Limits
- Max script size: 10,000 bytes
- Max stack items: 1000
- Max item size: 520 bytes
- Max operations: 201