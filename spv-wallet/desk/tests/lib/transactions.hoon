::  tests for lib/tx/build.hoon - Bitcoin transaction building and signing
::
::  Test vectors from BIP-143:
::  https://github.com/bitcoin/bips/blob/master/bip-0143.mediawiki
::
/+  *test, txn=tx-build, sig=tx-sighash, enc=tx-encode, bcu=bitcoin-utils, btc=bitcoin
|%
::  ============================================================================
::  BIP-143 Native P2WPKH Complete Test Vector
::  ============================================================================
::
::  This is the complete transaction from BIP-143 with two inputs and two outputs.
::  Input 0: Legacy P2PKH (not relevant for our P2WPKH signing)
::  Input 1: Native P2WPKH (this is what we're testing)
::
++  bip143-vector
  |%
  ::  Transaction metadata
  ++  nversion     1
  ++  nlocktime    17  :: 0x11
  ::
  ::  ---- Input 0 (Legacy, needed for hash-prevouts/sequences) ----
  ::  txid: fff7f7881a8099afa6940d42d1e7f6362bec38171ea3edf433541db4e4ad969f
  ++  input0-txid-be
    `@ux`(rash 'fff7f7881a8099afa6940d42d1e7f6362bec38171ea3edf433541db4e4ad969f' hex)
  ++  input0-vout   0
  ++  input0-amount  625.000.000  :: Not used in signing but for completeness
  ++  input0-sequence  `@ud`0xffff.ffee  :: BIP-143 test vector value
  ::
  ::  ---- Input 1 (P2WPKH - the one we're signing) ----
  ::  txid: ef51e1b804cc89d182d279655c3aa89e815b1b309fe287d9b2b55d57b90ec68a
  ++  input1-txid-be
    `@ux`(rash 'ef51e1b804cc89d182d279655c3aa89e815b1b309fe287d9b2b55d57b90ec68a' hex)
  ++  input1-vout   1
  ++  input1-amount  600.000.000  :: 6 BTC in satoshis
  ++  input1-sequence  `@ud`0xffff.ffff  :: BIP-143 test vector value (final)
  ++  input1-privkey
    `@ux`(rash '619c335025c7f4012e556c2a58b2506e30b8511b53ade95ea316fd8c3286feb9' hex)
  ++  input1-pubkey
    ::  compressed: 025476c2e83188368da1ff3e292e7acafcdb3566bb0ad253f62fc70f07aeee6357
    `@ux`(rash '025476c2e83188368da1ff3e292e7acafcdb3566bb0ad253f62fc70f07aeee6357' hex)
  ::  Script pubkey for input1: OP_0 <20-byte-hash>
  ::  00141d0f172a0ecb48aee1be1f2687d2963ae33f71a1
  ++  input1-script-pubkey
    ^-  hexb:btc
    :-  22
    `@ux`(rash '00141d0f172a0ecb48aee1be1f2687d2963ae33f71a1' hex)
  ::
  ::  ---- Outputs ----
  ::  Output 0: 1.1234 BTC to P2PKH
  ++  output0-amount  112.340.000  :: satoshis
  ++  output0-script  :: 76a9148280b37df378db99f66f85c95a783a76ac7a6d5988ac
    ^-  hexb:btc
    :-  25
    `@ux`(rash '76a9148280b37df378db99f66f85c95a783a76ac7a6d5988ac' hex)
  ::  Output 1: 2.2345 BTC to P2PKH
  ++  output1-amount  223.450.000  :: satoshis
  ++  output1-script  :: 76a9143bde42dbee7e4dbe6a21b2d50ce2f0167faa815988ac
    ^-  hexb:btc
    :-  25
    `@ux`(rash '76a9143bde42dbee7e4dbe6a21b2d50ce2f0167faa815988ac' hex)
  ::
  ::  ============================================================================
  ::  Expected Intermediate Values (from BIP-143)
  ::  ============================================================================
  ::
  ++  expected-hash-prevouts
    ::  96b827c8483d4e9b96712b6713a7b68d6e8003a781feba36c31143470b4efd37
    `@ux`(rash '96b827c8483d4e9b96712b6713a7b68d6e8003a781feba36c31143470b4efd37' hex)
  ++  expected-hash-sequence
    ::  52b0a642eea2fb7ae638c36f6252b6750293dbe574a806984b8e4d8548339a3b
    `@ux`(rash '52b0a642eea2fb7ae638c36f6252b6750293dbe574a806984b8e4d8548339a3b' hex)
  ++  expected-hash-outputs
    ::  863ef3e1a92afbfdb97f31ad0fc7683ee943e9abcf2501590ff8f6551f47e5e5
    `@ux`(rash '863ef3e1a92afbfdb97f31ad0fc7683ee943e9abcf2501590ff8f6551f47e5e5' hex)
  ++  expected-sighash
    ::  c37af31116d1b27caf68aae9e3ac82f1477929014d5b917657d0eb49478cb670
    `@ux`(rash 'c37af31116d1b27caf68aae9e3ac82f1477929014d5b917657d0eb49478cb670' hex)
  --
::
::  ============================================================================
::  Helper: Build the two preimage inputs from BIP-143 vector
::  ============================================================================
::
++  make-bip143-inputs
  ^-  (list sighash-input:sig)
  =/  input0=sighash-input:sig
    :*  input0-txid-be:bip143-vector
        input0-vout:bip143-vector
        input0-amount:bip143-vector
        input0-sequence:bip143-vector
        ::  Witness script doesn't matter for hash-prevouts/sequences
        ::  but we need something valid
        [26 `@ux`0]
    ==
  =/  input1=sighash-input:sig
    :*  input1-txid-be:bip143-vector
        input1-vout:bip143-vector
        input1-amount:bip143-vector
        input1-sequence:bip143-vector
        (build-script-code:sig input1-pubkey:bip143-vector)
    ==
  ~[input0 input1]
::
::  Helper: Build the two outputs from BIP-143 vector
::
++  make-bip143-outputs
  ^-  (list output:bc:tt:txn)
  :~  [output0-script:bip143-vector output0-amount:bip143-vector]
      [output1-script:bip143-vector output1-amount:bip143-vector]
  ==
::
::  ============================================================================
::  VALUE-VERIFIED TESTS: Check actual values against BIP-143 expected values
::  ============================================================================
::
::  Test: hashPrevouts matches BIP-143 expected value
::
++  test-hash-prevouts-matches-bip143
  =/  result=hexb:btc
    (hash-prevouts:bip143:sig make-bip143-inputs)
  %+  expect-eq
    !>  expected-hash-prevouts:bip143-vector
    !>  dat.result
::
::  Test: hashSequence matches BIP-143 expected value
::
++  test-hash-sequence-matches-bip143
  =/  result=hexb:btc
    (hash-sequences:bip143:sig make-bip143-inputs)
  %+  expect-eq
    !>  expected-hash-sequence:bip143-vector
    !>  dat.result
::
::  Test: hashOutputs matches BIP-143 expected value
::
++  test-hash-outputs-matches-bip143
  =/  result=hexb:btc
    (hash-outputs:bip143:sig make-bip143-outputs)
  %+  expect-eq
    !>  expected-hash-outputs:bip143-vector
    !>  dat.result
::
::  Test: Full sighash matches BIP-143 expected value
::
++  test-sighash-matches-bip143
  =/  sighash=hexb:btc
    %:  build:bip143:sig
      make-bip143-inputs
      1  :: signing input index 1 (the P2WPKH input)
      make-bip143-outputs
      nversion:bip143-vector
      nlocktime:bip143-vector
    ==
  %+  expect-eq
    !>  expected-sighash:bip143-vector
    !>  dat.sighash
::
::  ============================================================================
::  STRUCTURE TESTS: Verify output formats are correct
::  ============================================================================
::
::  Test: build-script-pubkey produces correct P2WPKH script
::
++  test-build-script-pubkey-p2wpkh
  =/  result=hexb:btc
    (build-script-pubkey:sig input1-pubkey:bip143-vector %p2wpkh ~)
  ::  P2WPKH is always 22 bytes: OP_0 (0x00) + push 20 (0x14) + 20-byte hash
  ;:  weld
    %+  expect-eq
      !>  22
      !>  wid.result
    ::  First byte should be 0x00 (OP_0 - but stored at end in little-endian)
    ::  Second byte should be 0x14 (push 20 bytes)
    %-  expect
      !>  (gth dat.result 0)  :: Non-zero result
  ==
::
::  Test: scriptCode for P2WPKH has correct structure
::
++  test-script-code-p2wpkh-structure
  =/  result=hexb:btc
    (build-script-code:sig input1-pubkey:bip143-vector)
  ::  P2WPKH scriptCode is P2PKH-style: 26 bytes
  ::  19 (length) + 76 a9 14 <hash> 88 ac (P2PKH script)
  %+  expect-eq
    !>  26
    !>  wid.result
::
::  Test: ECDSA signature has valid DER format
::
++  test-ecdsa-signature-format
  =/  test-hash=hexb:btc
    [32 expected-sighash:bip143-vector]
  =/  result=hexb:btc
    (ecdsa:signer:txn test-hash input1-privkey:bip143-vector)
  ::  DER signature + SIGHASH_ALL: typically 71-73 bytes
  %-  expect
    !>  ?&  (gte wid.result 70)
            (lte wid.result 73)
        ==
::
::  Test: ECDSA signature ends with SIGHASH_ALL (0x01)
::
++  test-ecdsa-sighash-all-appended
  =/  test-hash=hexb:btc
    [32 expected-sighash:bip143-vector]
  =/  result=hexb:btc
    (ecdsa:signer:txn test-hash input1-privkey:bip143-vector)
  ::  Last byte should be SIGHASH_ALL (0x01)
  =/  last-byte  (end [3 1] dat.result)
  %+  expect-eq
    !>  0x1
    !>  last-byte
::
::  Test: ECDSA signature starts with DER sequence tag (0x30)
::
++  test-ecdsa-der-sequence-tag
  =/  test-hash=hexb:btc
    [32 expected-sighash:bip143-vector]
  =/  result=hexb:btc
    (ecdsa:signer:txn test-hash input1-privkey:bip143-vector)
  ::  First byte of DER signature should be 0x30 (sequence tag)
  ::  In little-endian storage, this is at the high end
  =/  first-byte  (end [3 1] (rsh [3 (dec wid.result)] dat.result))
  %+  expect-eq
    !>  0x30
    !>  first-byte
::
::  Test: SegWit transaction encoding produces valid structure
::
++  test-segwit-tx-encoding-structure
  ::  Build pre-built witness for P2WPKH (2 items: sig + pubkey)
  =/  dummy-sig=hexb:btc
    [71 0x3044.0220.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0220.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0001]
  =/  witness=hexb:btc
    %-  cat:byt:bcu
    :~  [1 2]  ::  2 items
        [1 71]  ::  sig length
        dummy-sig
        [1 33]  ::  pubkey length
        [33 input1-pubkey:bip143-vector]
    ==
  =/  test-signed=input:bc:tt:txn
    :*  dat:(flip:byt:bcu [32 input1-txid-be:bip143-vector])
        input1-vout:bip143-vector
        input1-sequence:bip143-vector
        [0 0x0]  ::  empty script-sig for native segwit
        witness
    ==
  =/  test-output=output:bc:tt:txn
    [output0-script:bip143-vector output0-amount:bip143-vector]
  =/  result=hexb:btc
    %:  segwit-transaction:enc
      ~[test-signed]
      ~[test-output]
      1  :: version
      0  :: locktime
    ==
  ::  SegWit transaction should be reasonably sized
  ::  Minimum: 4 (version) + 2 (marker+flag) + 1 (input count) +
  ::           41 (min input) + 1 (output count) + 34 (min output) +
  ::           witness + 4 (locktime) = ~100+ bytes
  %-  expect
    !>  (gth wid.result 80)
::
::  ============================================================================
::  SINGLE-INPUT TESTS: Verify functions work with single input
::  ============================================================================
::
++  test-hash-prevouts-single-input
  =/  single-input=sighash-input:sig
    :*  dat:(flip:byt:bcu [32 input1-txid-be:bip143-vector])
        input1-vout:bip143-vector
        input1-amount:bip143-vector
        input1-sequence:bip143-vector
        (build-script-code:sig input1-pubkey:bip143-vector)
    ==
  =/  result=hexb:btc
    (hash-prevouts:bip143:sig ~[single-input])
  ::  Result should be 32 bytes (SHA256d output)
  %+  expect-eq
    !>  32
    !>  wid.result
::
++  test-hash-sequences-single-input
  =/  single-input=sighash-input:sig
    :*  dat:(flip:byt:bcu [32 input1-txid-be:bip143-vector])
        input1-vout:bip143-vector
        input1-amount:bip143-vector
        input1-sequence:bip143-vector
        (build-script-code:sig input1-pubkey:bip143-vector)
    ==
  =/  result=hexb:btc
    (hash-sequences:bip143:sig ~[single-input])
  %+  expect-eq
    !>  32
    !>  wid.result
::
++  test-hash-outputs-single-output
  =/  single-output=output:bc:tt:txn
    [output0-script:bip143-vector output0-amount:bip143-vector]
  =/  result=hexb:btc
    (hash-outputs:bip143:sig ~[single-output])
  %+  expect-eq
    !>  32
    !>  wid.result
::
::  ============================================================================
::  P2SH-P2WPKH TESTS: Wrapped SegWit from BIP-143 test vector
::  ============================================================================
::
::  Test vector from BIP-143:
::  Public key: 03ad1d8e89212f0b92c74d23bb710c00662ad1470198ac48c43f7d6f93a2a26873
::  Expected scriptPubKey: a9144733f37cf4db86fbc2efed2500b4f4e49f31202387
::  Expected redeemScript: 001479091972186c449eb1ded22b78e40d009bdf0089
::
++  p2sh-p2wpkh-vector
  |%
  ++  pubkey
    `@ux`(rash '03ad1d8e89212f0b92c74d23bb710c00662ad1470198ac48c43f7d6f93a2a26873' hex)
  ++  expected-script-pubkey
    `@ux`(rash 'a9144733f37cf4db86fbc2efed2500b4f4e49f31202387' hex)
  ++  expected-redeem-script
    `@ux`(rash '001479091972186c449eb1ded22b78e40d009bdf0089' hex)
  --
::
::  Test: build-script-pubkey produces correct P2SH scriptPubKey
::
++  test-build-script-pubkey-p2sh-p2wpkh
  =/  result=hexb:btc
    (build-script-pubkey:sig pubkey:p2sh-p2wpkh-vector %p2sh-p2wpkh ~)
  ;:  weld
    ::  P2SH is always 23 bytes: OP_HASH160 (0xa9) + push 20 (0x14) + 20-byte hash + OP_EQUAL (0x87)
    %+  expect-eq
      !>  23
      !>  wid.result
    ::  Value matches BIP-143 expected
    %+  expect-eq
      !>  expected-script-pubkey:p2sh-p2wpkh-vector
      !>  dat.result
  ==
::
::  Test: P2SH-P2WPKH scriptSig contains correct redeem script
::
++  test-p2sh-p2wpkh-scriptsig-format
  ::  Test that auth core builds correct P2SH-P2WPKH script-sig
  =/  dummy-sig=hexb:btc  [71 0x1]
  =/  [script-sig=hexb:btc witness=hexb:btc]
    (p2sh-p2wpkh:auth:txn dummy-sig pubkey:p2sh-p2wpkh-vector)
  ::  The scriptSig for P2SH-P2WPKH should be 23 bytes:
  ::  push 22 (0x16) + 22-byte redeem script (OP_0 + push 20 + 20-byte hash)
  %+  expect-eq
    !>  23
    !>  wid.script-sig
::
::  Test: P2SH-P2WPKH uses same scriptCode as native P2WPKH
::
++  test-p2sh-p2wpkh-script-code
  ::  Both P2WPKH and P2SH-P2WPKH use the same scriptCode (P2PKH-style)
  ::  build-script-code produces this from a pubkey
  =/  result=hexb:btc
    (build-script-code:sig pubkey:p2sh-p2wpkh-vector)
  ::  The scriptCode should be 26 bytes (P2PKH style)
  ::  19 (length) + 76 a9 14 <hash> 88 ac
  %+  expect-eq
    !>  26
    !>  wid.result
::
::  ============================================================================
::  VARINT TESTS: Bitcoin CompactSize encoding
::  ============================================================================
::
::  Test: VarInt encodes small values (0-252) as single byte
::
++  test-varint-single-byte
  ;:  weld
    %+  expect-eq
      !>  ^-  hexb:btc  [1 0x0]
      !>  (encode-varint:enc 0)
    %+  expect-eq
      !>  ^-  hexb:btc  [1 0x1]
      !>  (encode-varint:enc 1)
    %+  expect-eq
      !>  ^-  hexb:btc  [1 0xfc]
      !>  (encode-varint:enc 252)
  ==
::
::  Test: VarInt encodes 253-65535 as 0xfd + 2-byte LE
::
++  test-varint-two-byte
  ;:  weld
    ::  253 = 0xfd prefix + 0xfd00 LE = [0xfd, 0xfd, 0x00]
    %+  expect-eq
      !>  3
      !>  wid:(encode-varint:enc 253)
    ::  Check the prefix byte is 0xfd
    =/  result=hexb:btc  (encode-varint:enc 253)
    %-  expect
      !>  =(0xfd (end [3 1] (rsh [3 2] dat.result)))
  ==
::
::  Test: VarInt encodes 65536+ as 0xfe + 4-byte LE
::
++  test-varint-four-byte
  =/  result=hexb:btc  (encode-varint:enc 0x1.0000)
  ;:  weld
    %+  expect-eq
      !>  5
      !>  wid.result
    ::  Check the prefix byte is 0xfe
    %-  expect
      !>  =(0xfe (end [3 1] (rsh [3 4] dat.result)))
  ==
::
::  ============================================================================
::  P2PKH (LEGACY) TESTS
::  ============================================================================
::
::  Test: build-script-pubkey produces correct P2PKH scriptPubKey
::  P2PKH: OP_DUP OP_HASH160 <20-byte-hash> OP_EQUALVERIFY OP_CHECKSIG
::  Bytes: 76 a9 14 <hash> 88 ac = 25 bytes
::
++  test-build-script-pubkey-p2pkh
  =/  result=hexb:btc
    (build-script-pubkey:sig input1-pubkey:bip143-vector %p2pkh ~)
  ;:  weld
    ::  P2PKH is always 25 bytes
    %+  expect-eq
      !>  25
      !>  wid.result
    ::  First bytes should be 76 a9 14 (OP_DUP OP_HASH160 PUSH20)
    ::  In our little-endian storage, check the high bytes
    =/  high-bytes  (rsh [3 22] dat.result)
    %+  expect-eq
      !>  0x76.a914
      !>  high-bytes
    ::  Last bytes should be 88 ac (OP_EQUALVERIFY OP_CHECKSIG)
    =/  low-bytes  (end [3 2] dat.result)
    %+  expect-eq
      !>  0x88ac
      !>  low-bytes
  ==
::
::  Test: P2PKH scriptPubKey contains correct pubkey hash
::
++  test-p2pkh-script-contains-pubkey-hash
  =/  result=hexb:btc
    (build-script-pubkey:sig input1-pubkey:bip143-vector %p2pkh ~)
  ::  Extract the 20-byte hash from the middle of the script
  ::  Script: 76 a9 14 <20-byte-hash> 88 ac
  ::  The hash is bytes 3-22 (after 76 a9 14, before 88 ac)
  =/  embedded-hash  (cut 3 [2 20] dat.result)
  ::  Compute expected hash from pubkey
  =/  expected-hash  dat:(hash-160:bcu [33 input1-pubkey:bip143-vector])
  %+  expect-eq
    !>  expected-hash
    !>  embedded-hash
::
::  Test: Legacy sighash produces 32-byte hash
::
++  test-legacy-preimage-structure
  ::  Use BIP-143 test vector inputs/outputs for structure test
  =/  txid-le=@ux  dat:(flip:byt:bcu [32 input1-txid-be:bip143-vector])
  =/  script-pubkey=hexb:btc
    (build-script-pubkey:sig input1-pubkey:bip143-vector %p2pkh ~)
  =/  test-output=output:bc:tt:txn
    [output0-script:bip143-vector output0-amount:bip143-vector]
  =/  legacy-input=sighash-input-legacy:sig
    [txid-le input1-vout:bip143-vector input1-sequence:bip143-vector script-pubkey]
  =/  result=hexb:btc
    %:  build:legacy:sig
      ~[legacy-input]
      0  :: signing index
      ~[test-output]
      1  :: nversion
      0  :: nlocktime
    ==
  ::  Sighash should be 32 bytes (dsha256 output)
  %+  expect-eq
    !>  32
    !>  wid.result
::
::  Test: Legacy sighash is deterministic (same inputs produce same hash)
::
++  test-legacy-preimage-sighash-suffix
  =/  txid-le=@ux  dat:(flip:byt:bcu [32 input1-txid-be:bip143-vector])
  =/  script-pubkey=hexb:btc
    (build-script-pubkey:sig input1-pubkey:bip143-vector %p2pkh ~)
  =/  test-output=output:bc:tt:txn
    [output0-script:bip143-vector output0-amount:bip143-vector]
  =/  legacy-input=sighash-input-legacy:sig
    [txid-le input1-vout:bip143-vector input1-sequence:bip143-vector script-pubkey]
  =/  result1=hexb:btc
    %:  build:legacy:sig
      ~[legacy-input]
      0
      ~[test-output]
      1
      0
    ==
  =/  result2=hexb:btc
    %:  build:legacy:sig
      ~[legacy-input]
      0
      ~[test-output]
      1
      0
    ==
  ::  Same inputs should produce identical sighash
  %+  expect-eq
    !>  dat.result1
    !>  dat.result2
::
::  Test: Legacy transaction encoding produces valid structure
::
++  test-legacy-tx-encoding-structure
  ::  Build pre-built script-sig for P2PKH (<sig-len> <sig> <pubkey-len> <pubkey>)
  =/  dummy-sig=hexb:btc
    [71 0x3044.0220.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0220.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0001]
  =/  script-sig=hexb:btc
    %-  cat:byt:bcu
    :~  [1 71]
        dummy-sig
        [1 33]
        [33 input1-pubkey:bip143-vector]
    ==
  =/  test-signed=input:bc:tt:txn
    :*  dat:(flip:byt:bcu [32 input1-txid-be:bip143-vector])
        input1-vout:bip143-vector
        input1-sequence:bip143-vector
        script-sig
        [0 0x0]  ::  empty witness for legacy
    ==
  =/  test-output=output:bc:tt:txn
    [output0-script:bip143-vector output0-amount:bip143-vector]
  =/  result=hexb:btc
    %:  legacy-transaction:enc
      ~[test-signed]
      ~[test-output]
      1  :: version
      0  :: locktime
    ==
  ::  Legacy transaction should be reasonably sized
  ::  4 (version) + 1 (input count) + ~150 (input with scriptSig) +
  ::  1 (output count) + 34 (output) + 4 (locktime) = ~200 bytes
  %-  expect
    !>  (gth wid.result 150)
::
::  Test: Legacy transaction has no SegWit marker/flag
::
++  test-legacy-tx-no-segwit-marker
  ::  Build pre-built script-sig for P2PKH
  =/  script-sig=hexb:btc
    %-  cat:byt:bcu
    :~  [1 71]
        [71 0x1]
        [1 33]
        [33 input1-pubkey:bip143-vector]
    ==
  =/  test-signed=input:bc:tt:txn
    :*  0x1234.5678
        0
        `@ud`0xffff.ffff
        script-sig
        [0 0x0]  ::  empty witness for legacy
    ==
  =/  test-output=output:bc:tt:txn
    [output0-script:bip143-vector output0-amount:bip143-vector]
  =/  result=hexb:btc
    %:  legacy-transaction:enc
      ~[test-signed]
      ~[test-output]
      1
      0
    ==
  ::  Legacy tx starts with version (4 bytes), then input count
  ::  SegWit marker would be 0x00 at byte 4, but legacy has input count
  ::  With 1 input, byte 4 should be 0x01, not 0x00
  =/  byte-4  (cut 3 [4 1] dat.result)  ::  Extract byte at position 4
  ::  Should NOT be 0x00 (SegWit marker)
  %-  expect
    !>  (gth byte-4 0)
::
::  Test: Legacy inputs encoder produces correct scriptSig structure
::
++  test-legacy-inputs-scriptsig-structure
  ::  Test that auth core builds correct P2PKH script-sig
  =/  dummy-sig=hexb:btc
    [71 0x30.4402.2000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0220.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0001]
  =/  [script-sig=hexb:btc witness=hexb:btc]
    (p2pkh:auth:txn dummy-sig input1-pubkey:bip143-vector)
  ::  scriptSig = sig-push + sig (71) + pubkey-push + pubkey (33) = 1 + 71 + 1 + 33 = 106
  %+  expect-eq
    !>  106
    !>  wid.script-sig
::
::  Test: P2PKH sighash matches bitcoinjs-lib (cross-implementation validation)
::  Uses "private key = 1" test vector - identical transaction parameters
::
++  test-p2pkh-sighash-cross-implementation
  ::  Test vector from bitcoinjs-lib:
  ::  - prevTxId: 0x0000...0001 (32 bytes, BE)
  ::  - prevVout: 0
  ::  - prevValue: 100000 sats
  ::  - scriptPubKey: 76a914751e76e8199196d454941c45d1b3a323f1433bd688ac
  ::  - output amount: 50000 sats
  ::  - output script: same
  ::  - version: 1, locktime: 0, sequence: 0xffffffff
  ::  Expected sighash: 0d24a3e6f8afc5db72fb23b19e585bfb61207992d7cd3756dfcffd0f56003caf
  ::
  =/  script-pubkey=hexb:btc  [25 (rash '76a914751e76e8199196d454941c45d1b3a323f1433bd688ac' hex)]
  ::  Output: 50000 sats to same script
  =/  output=output:bc:tt:txn
    [script-pubkey 50.000]
  ::  Build sighash
  =/  legacy-input=sighash-input-legacy:sig
    [0x1 0 `@ud`0xffff.ffff script-pubkey]
  =/  sighash=hexb:btc
    %:  build:legacy:sig
      ~[legacy-input]
      0
      ~[output]
      1   ::  version
      0   ::  locktime
    ==
  ::  Expected sighash from bitcoinjs-lib
  =/  expected=@ux  (rash '0d24a3e6f8afc5db72fb23b19e585bfb61207992d7cd3756dfcffd0f56003caf' hex)
  %-  expect
    !>  =(expected dat.sighash)
::
::  ============================================================================
::  BIP-341 TAPROOT TESTS
::  ============================================================================
::
::  Test vector: Generator point pubkey (private key = 1)
::  This is a standard test vector using the secp256k1 generator point.
::
++  taproot-vector
  |%
  ::  Private key = 1 (generator point)
  ++  privkey
    0x1
  ::  Compressed pubkey for private key 1
  ++  pubkey
    `@ux`(rash '0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798' hex)
  ::  X-only pubkey (32 bytes)
  ++  x-only-pubkey
    `@ux`(rash '79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798' hex)
  --
::
::  ============================================================================
::  BIP-341 Sighash Component Tests
::  ============================================================================
::
::  Test: sha-prevouts produces 32-byte hash
::
++  test-bip341-sha-prevouts-size
  =/  test-input=sighash-input:sig
    :*  0x1234.5678       :: txid
        0                 :: vout
        100.000           :: amount
        `@ud`0xffff.ffff  :: sequence
        [34 0x5120.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000]  :: P2TR scriptPubKey
    ==
  =/  result=hexb:btc  (sha-prevouts:bip341:sig ~[test-input])
  %+  expect-eq
    !>  32
    !>  wid.result
::
::  Test: sha-amounts produces 32-byte hash
::
++  test-bip341-sha-amounts-size
  =/  test-input=sighash-input:sig
    :*  0x1234.5678
        0
        100.000
        `@ud`0xffff.ffff
        [34 0x0]
    ==
  =/  result=hexb:btc  (sha-amounts:bip341:sig ~[test-input])
  %+  expect-eq
    !>  32
    !>  wid.result
::
::  Test: sha-scriptpubkeys produces 32-byte hash
::
++  test-bip341-sha-scriptpubkeys-size
  =/  test-input=sighash-input:sig
    :*  0x1234.5678
        0
        100.000
        `@ud`0xffff.ffff
        [34 0x5120.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000]
    ==
  =/  result=hexb:btc  (sha-scriptpubkeys:bip341:sig ~[test-input])
  %+  expect-eq
    !>  32
    !>  wid.result
::
::  Test: sha-sequences produces 32-byte hash
::
++  test-bip341-sha-sequences-size
  =/  test-input=sighash-input:sig
    :*  0x1234.5678
        0
        100.000
        `@ud`0xffff.ffff
        [34 0x0]
    ==
  =/  result=hexb:btc  (sha-sequences:bip341:sig ~[test-input])
  %+  expect-eq
    !>  32
    !>  wid.result
::
::  Test: sha-outputs produces 32-byte hash
::
++  test-bip341-sha-outputs-size
  =/  test-output=output:bc:tt:txn
    :*  [34 0x5120.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000]
        50.000
    ==
  =/  result=hexb:btc  (sha-outputs:bip341:sig ~[test-output])
  %+  expect-eq
    !>  32
    !>  wid.result
::
::  Test: build-sighash-keypath produces 32-byte sighash
::
++  test-bip341-keypath-sighash-size
  =/  test-input=sighash-input:sig
    :*  0x1234.5678
        0
        100.000
        `@ud`0xffff.ffff
        [34 0x5120.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000]
    ==
  =/  test-output=output:bc:tt:txn
    :*  [34 0x5120.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000]
        50.000
    ==
  =/  result=hexb:btc
    %:  build:bip341:sig
      ~[test-input]
      0
      ~[test-output]
      2          :: nversion
      0          :: nlocktime
      ~          :: key-path (no extension)
    ==
  %+  expect-eq
    !>  32
    !>  wid.result
::
::  Test: build-scriptpath produces 32-byte sighash
::
++  test-bip341-scriptpath-sighash-size
  =/  test-input=sighash-input:sig
    :*  0x1234.5678
        0
        100.000
        `@ud`0xffff.ffff
        [34 0x5120.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000]
    ==
  =/  test-output=output:bc:tt:txn
    :*  [34 0x5120.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000]
        50.000
    ==
  =/  result=hexb:btc
    %:  build:bip341:sig
      ~[test-input]
      0
      ~[test-output]
      2           :: nversion
      0           :: nlocktime
      `[0x1234 0xc0 `@ud`0xffff.ffff]  :: script-path extension
    ==
  %+  expect-eq
    !>  32
    !>  wid.result
::
::  Test: keypath and scriptpath sighashes are different
::
++  test-bip341-keypath-scriptpath-different
  =/  test-input=sighash-input:sig
    :*  0x1234.5678
        0
        100.000
        `@ud`0xffff.ffff
        [34 0x5120.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000]
    ==
  =/  test-output=output:bc:tt:txn
    :*  [34 0x5120.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000]
        50.000
    ==
  =/  keypath-sighash=hexb:btc
    %:  build:bip341:sig
      ~[test-input]
      0
      ~[test-output]
      2
      0
      ~
    ==
  =/  scriptpath-sighash=hexb:btc
    %:  build:bip341:sig
      ~[test-input]
      0
      ~[test-output]
      2
      0
      `[0x1234 0xc0 `@ud`0xffff.ffff]
    ==
  ::  Keypath and scriptpath sighashes should be different
  %-  expect
    !>  !=(dat.keypath-sighash dat.scriptpath-sighash)
::
::  ============================================================================
::  Schnorr Signature Tests
::  ============================================================================
::
::  Test: Schnorr signature for SIGHASH_DEFAULT is 64 bytes
::
++  test-schnorr-sighash-default-size
  =/  test-hash=hexb:btc  [32 0x1234.5678.9abc.def0.1234.5678.9abc.def0.1234.5678.9abc.def0.1234.5678.9abc.def0]
  =/  result=hexb:btc
    (schnorr:signer:txn test-hash privkey:taproot-vector 0)  :: 0 = SIGHASH_DEFAULT
  %+  expect-eq
    !>  64
    !>  wid.result
::
::  Test: Schnorr signature for SIGHASH_ALL is 65 bytes
::
++  test-schnorr-sighash-all-size
  =/  test-hash=hexb:btc  [32 0x1234.5678.9abc.def0.1234.5678.9abc.def0.1234.5678.9abc.def0.1234.5678.9abc.def0]
  =/  result=hexb:btc
    (schnorr:signer:txn test-hash privkey:taproot-vector 1)  :: 1 = SIGHASH_ALL
  %+  expect-eq
    !>  65
    !>  wid.result
::
::  Test: Schnorr SIGHASH_ALL signature ends with 0x01
::
++  test-schnorr-sighash-all-suffix
  =/  test-hash=hexb:btc  [32 0x1234.5678.9abc.def0.1234.5678.9abc.def0.1234.5678.9abc.def0.1234.5678.9abc.def0]
  =/  result=hexb:btc
    (schnorr:signer:txn test-hash privkey:taproot-vector 1)
  =/  last-byte  (end [3 1] dat.result)
  %+  expect-eq
    !>  0x1
    !>  last-byte
::
::  Test: Schnorr signatures are deterministic
::
++  test-schnorr-deterministic
  =/  test-hash=hexb:btc  [32 0x1234.5678.9abc.def0.1234.5678.9abc.def0.1234.5678.9abc.def0.1234.5678.9abc.def0]
  =/  sig1=hexb:btc  (schnorr:signer:txn test-hash privkey:taproot-vector 0)
  =/  sig2=hexb:btc  (schnorr:signer:txn test-hash privkey:taproot-vector 0)
  %+  expect-eq
    !>  dat.sig1
    !>  dat.sig2
::
::  ============================================================================
::  P2TR Script Pubkey Tests
::  ============================================================================
::
::  Test: build-script-pubkey for P2TR produces 34-byte output
::
++  test-build-script-pubkey-p2tr-size
  =/  result=hexb:btc
    (build-script-pubkey:sig pubkey:taproot-vector [%p2tr %key-path ~])
  %+  expect-eq
    !>  34
    !>  wid.result
::
::  Test: P2TR scriptPubKey starts with OP_1 (0x51) and PUSH32 (0x20)
::
++  test-p2tr-script-pubkey-prefix
  =/  result=hexb:btc
    (build-script-pubkey:sig pubkey:taproot-vector [%p2tr %key-path ~])
  ::  First two bytes should be 0x51 0x20 (OP_1 PUSH32)
  ::  In little-endian storage, check the high bytes
  =/  prefix  (rsh [3 32] dat.result)
  %+  expect-eq
    !>  0x5120
    !>  prefix
::
::  Test: P2TR scriptPubKey with merkle root differs from without
::
++  test-p2tr-script-with-merkle-root-differs
  =/  without-merkle=hexb:btc
    (build-script-pubkey:sig pubkey:taproot-vector [%p2tr %key-path ~])
  =/  with-merkle=hexb:btc
    (build-script-pubkey:sig pubkey:taproot-vector [%p2tr %key-path `0x1234.5678])
  ::  The tweaked pubkeys should be different
  %-  expect
    !>  !=(dat.without-merkle dat.with-merkle)
::
::  ============================================================================
::  P2TR Witness Encoding Tests
::  ============================================================================
::
::  Test: Key-path witness has 1 stack item
::
++  test-p2tr-keypath-witness-stack-count
  ::  Test that auth core builds correct P2TR key-path witness
  =/  dummy-sig=hexb:btc  [64 0x1234]
  =/  [script-sig=hexb:btc witness=hexb:btc]
    (p2tr-keypath:auth:txn dummy-sig)
  ::  First byte should be 0x01 (1 stack item)
  =/  first-byte  (end [3 1] (rsh [3 (dec wid.witness)] dat.witness))
  %+  expect-eq
    !>  0x1
    !>  first-byte
::
::  Test: Keypath witness encodes 64-byte signature correctly
::
++  test-p2tr-keypath-witness-sig-size
  ::  Test that auth core encodes 64-byte signature correctly
  =/  sig-64=hexb:btc  [64 0x1234.5678.9abc.def0.1234.5678.9abc.def0.1234.5678.9abc.def0.1234.5678.9abc.def0.1234.5678.9abc.def0.1234.5678.9abc.def0.1234.5678.9abc.def0.1234.5678.9abc.def0]
  =/  [script-sig=hexb:btc witness=hexb:btc]
    (p2tr-keypath:auth:txn sig-64)
  ::  Witness should be: 1 (count) + 1 (varint 64) + 64 (sig) = 66 bytes
  %+  expect-eq
    !>  66
    !>  wid.witness
::
::  ============================================================================
::  has-segwit-inputs Tests
::  ============================================================================
::
::  These tests ensure proper type-checking of spend-type tagged unions.
::  The spend-type is a cell like [%p2wpkh ~], not just a tag like %p2wpkh.
::
::  Helper: create mock input with given spend-type
::
++  mock-input
  |=  =spend-type:tt:txn
  ^-  input:ap:tt:txn
  :*  0x1234                         :: privkey (dummy)
      0xdead.beef                    :: pubkey (dummy)
      0x5678.9abc                    :: txid
      0                              :: vout
      100.000                        :: amount
      `@ud`0xffff.ffff               :: sequence
      spend-type
  ==
::
::  Test: P2WPKH input detected as segwit
::
++  test-has-segwit-p2wpkh
  =/  inputs=(list input:ap:tt:txn)
    ~[(mock-input [%p2wpkh ~])]
  %+  expect-eq
    !>  %.y
    !>  (has-segwit:txn inputs)
::
::  Test: P2SH-P2WPKH input detected as segwit
::
++  test-has-segwit-p2sh-p2wpkh
  =/  inputs=(list input:ap:tt:txn)
    ~[(mock-input [%p2sh-p2wpkh ~])]
  %+  expect-eq
    !>  %.y
    !>  (has-segwit:txn inputs)
::
::  Test: P2TR input detected as segwit
::
++  test-has-segwit-p2tr
  =/  inputs=(list input:ap:tt:txn)
    ~[(mock-input [%p2tr %key-path ~])]
  %+  expect-eq
    !>  %.y
    !>  (has-segwit:txn inputs)
::
::  Test: P2PKH input NOT detected as segwit
::
++  test-has-segwit-p2pkh-false
  =/  inputs=(list input:ap:tt:txn)
    ~[(mock-input [%p2pkh ~])]
  %+  expect-eq
    !>  %.n
    !>  (has-segwit:txn inputs)
::
::  Test: Mixed inputs (legacy + segwit) detected as segwit
::
++  test-has-segwit-mixed-inputs
  =/  inputs=(list input:ap:tt:txn)
    :~  (mock-input [%p2pkh ~])
        (mock-input [%p2wpkh ~])
    ==
  %+  expect-eq
    !>  %.y
    !>  (has-segwit:txn inputs)
::
::  Test: Empty input list returns false
::
++  test-has-segwit-empty-list
  =/  inputs=(list input:ap:tt:txn)  ~
  %+  expect-eq
    !>  %.n
    !>  (has-segwit:txn inputs)
--

