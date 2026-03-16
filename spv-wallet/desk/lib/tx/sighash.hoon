::  sighash.hoon - Transaction signing digests
::
::  Computes the hash that gets signed for different Bitcoin script types.
::  Each BIP requires different data in the preimage, so this library handles:
::
::    1. Script building - construct the script needed for each sighash type
::    2. Input conversion - transform app inputs into sighash-ready format
::    3. Preimage construction - serialize data per BIP specification
::    4. Final hash - apply the appropriate hash function
::
::  Supported algorithms:
::    - Legacy:  P2PKH - needs scriptPubKey in preimage
::    - BIP-143: P2WPKH, P2SH-P2WPKH - needs scriptCode (P2PKH-style)
::    - BIP-341: P2TR key/script-path - needs scriptPubKey, tagged hash
::
::  Entry points:
::    - to-*-inputs: convert (list input:ap:tt) for each algorithm
::    - build:legacy/bip143/bip341: compute final sighash
::
::  TODO: SIGHASH_NONE, SIGHASH_SINGLE, SIGHASH_ANYONECANPAY
::
/-  tt=transactions
/+  bcu=bitcoin-utils, btc=bitcoin, enc=tx-encode, taproot
|%
::  ============================================================================
::  Sighash Input Types
::  ============================================================================
::
::  Internal types with pre-built scripts. Callers use to-*-inputs converters.
::
+$  sighash-input                                 ::  BIP-143/341: includes amount
  $:  txid=@
      vout=@ud
      amount=@ud
      sequence=@ud
      script-pubkey=hexb:btc                      ::  scriptCode or scriptPubKey
  ==
::
+$  sighash-input-legacy                          ::  Legacy: no amount field
  $:  txid=@
      vout=@
      sequence=@
      script-pubkey=hexb:btc
  ==
::
::  ============================================================================
::  Script Building
::  ============================================================================
::
::  Build the scripts that go into sighash preimages. Different BIPs need
::  different scripts - this is input-side script building (we have the pubkey).
::
::  +build-script-pubkey: The actual locking script for an input
::
::    Used by Legacy and BIP-341 which commit to the real scriptPubKey.
::    Takes pubkey + spend-type because we're signing inputs we control.
::
++  build-script-pubkey
  |=  [pubkey=@ =spend-type:tt]
  ^-  hexb:btc
  ?-    -.spend-type
      %p2pkh
    ::  OP_DUP OP_HASH160 <20-byte-hash> OP_EQUALVERIFY OP_CHECKSIG
    =/  pubkey-hash=@ux  dat:(hash-160:bcu [33 pubkey])
    :-  25
    %+  con  (lsh [3 22] 0x76.a914)
    %+  con  (lsh [3 2] pubkey-hash)
    0x88ac
    ::
      %p2wpkh
    ::  OP_0 <20-byte-pubkey-hash>
    =/  pubkey-hash=@ux  dat:(hash-160:bcu [33 pubkey])
    [22 (con (lsh [3 20] 0x14) pubkey-hash)]
    ::
      %p2sh-p2wpkh
    ::  OP_HASH160 <20-byte-script-hash> OP_EQUAL
    =/  pubkey-hash=@ux  dat:(hash-160:bcu [33 pubkey])
    =/  witness-program=hexb:btc  [22 (con (lsh [3 20] 0x14) pubkey-hash)]
    =/  script-hash=@ux  dat:(hash-160:bcu witness-program)
    :-  23
    %+  con  (lsh [3 21] 0xa914)
    %+  con  (lsh [3 1] script-hash)
    0x87
    ::
      %p2tr
    ::  OP_1 <32-byte-x-only-tweaked-pubkey>
    =/  merkle-root=(unit @ux)
      ?-  -.+.spend-type
        %key-path     merkle-root.+.spend-type
        %script-path  `(merkle-root-from-proof:taproot tapleaf.+.spend-type proof.+.spend-type)
      ==
    =/  tweaked-x=@ux  (output-pubkey:taproot pubkey merkle-root)
    [34 (con (lsh [3 32] 0x5120) tweaked-x)]
  ==
::
::  +build-script-code: BIP-143 scriptCode (not a real script)
::
::    BIP-143 invented a special format for SegWit v0 sighash: a P2PKH-style
::    script with length prefix. This never appears on-chain - it's only for
::    the sighash preimage. Format: 19 76 a9 14 <20-byte-hash> 88 ac
::
++  build-script-code
  |=  pubkey=@
  ^-  hexb:btc
  =/  pubkey-hash=@ux  dat:(hash-160:bcu [33 pubkey])
  :-  26
  %+  con  (lsh [3 22] 0x1976.a914)
  %+  con  (lsh [3 2] pubkey-hash)
  0x88ac
::
::  ============================================================================
::  Input Conversion
::  ============================================================================
::
::  Transform app-level inputs into sighash-ready format. Each converter builds
::  the appropriate script for its algorithm - callers just pass input:ap:tt.
::
++  to-bip143-inputs                              ::  P2WPKH, P2SH-P2WPKH
  ::  Uses scriptCode (P2PKH-style with length prefix)
  |=  inputs=(list input:ap:tt)
  ^-  (list sighash-input)
  %+  turn  inputs
  |=  =input:ap:tt
  :*  txid.input
      vout.input
      amount.input
      sequence.input
      (build-script-code pubkey.input)
  ==
::
++  to-bip341-inputs                              ::  P2TR key-path, script-path
  ::  Uses actual scriptPubKey (with tweaked pubkey)
  |=  inputs=(list input:ap:tt)
  ^-  (list sighash-input)
  %+  turn  inputs
  |=  =input:ap:tt
  :*  txid.input
      vout.input
      amount.input
      sequence.input
      (build-script-pubkey pubkey.input spend-type.input)
  ==
::
++  to-legacy-inputs                              ::  P2PKH
  ::  Uses actual scriptPubKey (no amount field)
  |=  inputs=(list input:ap:tt)
  ^-  (list sighash-input-legacy)
  %+  turn  inputs
  |=  =input:ap:tt
  :*  txid.input
      vout.input
      sequence.input
      (build-script-pubkey pubkey.input spend-type.input)
  ==
::
::  ============================================================================
::  Shared Serializers
::  ============================================================================
::
::  Concatenate input/output fields for hashing. Used by both BIP-143 (dsha256)
::  and BIP-341 (sha256) - the hash function differs, but serialization is same.
::
++  cat-prevouts                               ::  txid || vout for each input
  |=  inputs=(list sighash-input)
  ^-  hexb:btc
  %-  cat:byt:bcu
  %+  turn  inputs
  |=  =sighash-input
  (cat:byt:bcu ~[[32 txid.sighash-input] (flip:byt:bcu [4 vout.sighash-input])])
::
++  cat-sequences                              ::  sequence for each input
  |=  inputs=(list sighash-input)
  ^-  hexb:btc
  %-  cat:byt:bcu
  (turn inputs |=(=sighash-input (flip:byt:bcu [4 sequence.sighash-input])))
::
++  cat-amounts                                ::  amount for each input (BIP-341 only)
  |=  inputs=(list sighash-input)
  ^-  hexb:btc
  %-  cat:byt:bcu
  (turn inputs |=(=sighash-input (flip:byt:bcu [8 amount.sighash-input])))
::
++  cat-scriptpubkeys                          ::  scriptPubKey for each input (BIP-341 only)
  |=  inputs=(list sighash-input)
  ^-  hexb:btc
  %-  cat:byt:bcu
  %+  turn  inputs
  |=  =sighash-input
  (cat:byt:bcu ~[(encode-varint:enc wid.script-pubkey.sighash-input) script-pubkey.sighash-input])
::
::  ============================================================================
::  BIP-143: SegWit v0 Sighash (P2WPKH, P2SH-P2WPKH)
::  ============================================================================
::
::  Commits to all input amounts (malleability fix). Uses dsha256.
::  Preimage: version || hashPrevouts || hashSequence || outpoint ||
::            scriptCode || amount || sequence || hashOutputs || locktime || sighash
::
++  bip143
  |%
  ++  hash-prevouts                            ::  dsha256(prevouts)
    |=  inputs=(list sighash-input)
    (dsha256:bcu (cat-prevouts inputs))
  ::
  ++  hash-sequences                           ::  dsha256(sequences)
    |=  inputs=(list sighash-input)
    (dsha256:bcu (cat-sequences inputs))
  ::
  ++  hash-outputs                             ::  dsha256(outputs)
    |=  outputs=(list output:bc:tt)
    (dsha256:bcu (serialize-outputs:enc outputs))
  ::  +signing-input: Serialize signing input fields for preimage
  ::
  ::  outpoint || scriptCode || amount || sequence
  ::
  ++  signing-input
    |=  inp=sighash-input
    ^-  hexb:btc
    %-  cat:byt:bcu
    :~  [32 txid.inp]
        (flip:byt:bcu [4 vout.inp])
        script-pubkey.inp
        (flip:byt:bcu [8 amount.inp])
        (flip:byt:bcu [4 sequence.inp])
    ==
  ::  +preimage: Build BIP-143 preimage
  ::
  ::  version || hashPrevouts || hashSequence || signing-input ||
  ::  hashOutputs || locktime || sighash_type
  ::
  ++  preimage
    |=  $:  inputs=(list sighash-input)
            signing-index=@ud
            outputs=(list output:bc:tt)
            nversion=@ud
            nlocktime=@ud
        ==
    ^-  hexb:btc
    %-  cat:byt:bcu
    :~  (flip:byt:bcu [4 nversion])
        (hash-prevouts inputs)
        (hash-sequences inputs)
        (signing-input (snag signing-index inputs))
        (hash-outputs outputs)
        (flip:byt:bcu [4 nlocktime])
        (flip:byt:bcu [4 1])                    ::  SIGHASH_ALL
    ==
  ::  +build: Build BIP-143 sighash (dsha256 of preimage)
  ::
  ++  build
    |=  $:  inputs=(list sighash-input)
            signing-index=@ud
            outputs=(list output:bc:tt)
            nversion=@ud
            nlocktime=@ud
        ==
    ^-  hexb:btc
    (dsha256:bcu (preimage inputs signing-index outputs nversion nlocktime))
  --
::
::  ============================================================================
::  BIP-341: Taproot Sighash (P2TR)
::  ============================================================================
::
::  Commits to all scriptPubKeys (cross-input signature aggregation safety).
::  Uses sha256 with tagged hash ("TapSighash"). Script-path adds leaf hash.
::  Message: epoch || sighash || version || locktime || sha256(prevouts) ||
::           sha256(amounts) || sha256(scriptpubkeys) || sha256(sequences) ||
::           sha256(outputs) || spend_type || input_index [|| ext]
::
++  bip341
  |%
  ++  sha-prevouts
    |=  inputs=(list sighash-input)
    (sha256:bcu (cat-prevouts inputs))
  ::
  ++  sha-amounts
    |=  inputs=(list sighash-input)
    (sha256:bcu (cat-amounts inputs))
  ::
  ++  sha-scriptpubkeys
    |=  inputs=(list sighash-input)
    (sha256:bcu (cat-scriptpubkeys inputs))
  ::
  ++  sha-sequences
    |=  inputs=(list sighash-input)
    (sha256:bcu (cat-sequences inputs))
  ::
  ++  sha-outputs
    |=  outputs=(list output:bc:tt)
    (sha256:bcu (serialize-outputs:enc outputs))
  ::  +sha-all: All 5 component hashes concatenated
  ::
  ++  sha-all
    |=  [inputs=(list sighash-input) outputs=(list output:bc:tt)]
    ^-  hexb:btc
    %-  cat:byt:bcu
    :~  (sha-prevouts inputs)
        (sha-amounts inputs)
        (sha-scriptpubkeys inputs)
        (sha-sequences inputs)
        (sha-outputs outputs)
    ==
  ::  Script-path extension for BIP-341
  ::
  +$  script-path-ext
    $:  leaf-hash=@ux
        key-version=@ux
        code-sep-pos=@ud
    ==
  ::  +ext-bytes: Serialize script-path extension
  ::
  ++  ext-bytes
    |=  ext=script-path-ext
    ^-  hexb:btc
    %-  cat:byt:bcu
    :~  [32 leaf-hash.ext]
        [1 key-version.ext]
        (flip:byt:bcu [4 code-sep-pos.ext])
    ==
  ::  +base-msg: Common message for key-path and script-path
  ::
  ++  base-msg
    |=  $:  inputs=(list sighash-input)
            signing-index=@ud
            outputs=(list output:bc:tt)
            nversion=@ud
            nlocktime=@ud
            is-script-path=?
        ==
    ^-  hexb:btc
    %-  cat:byt:bcu
    :~  [1 0x0]                                ::  epoch
        [1 0x0]                                ::  SIGHASH_DEFAULT
        (flip:byt:bcu [4 nversion])
        (flip:byt:bcu [4 nlocktime])
        (sha-all inputs outputs)
        ?:(is-script-path [1 0x2] [1 0x0])     ::  spend_type
        (flip:byt:bcu [4 signing-index])
    ==
  ::  +message: Build BIP-341 sighash message
  ::
  ++  message
    |=  $:  inputs=(list sighash-input)
            signing-index=@ud
            outputs=(list output:bc:tt)
            nversion=@ud
            nlocktime=@ud
            ext=(unit script-path-ext)
        ==
    ^-  hexb:btc
    =/  base=hexb:btc
      (base-msg inputs signing-index outputs nversion nlocktime ?=(^ ext))
    ?~  ext  base
    (cat:byt:bcu ~[base (ext-bytes u.ext)])
  ::  +build: Build BIP-341 sighash (tagged hash of message)
  ::
  ++  build
    |=  $:  inputs=(list sighash-input)
            signing-index=@ud
            outputs=(list output:bc:tt)
            nversion=@ud
            nlocktime=@ud
            ext=(unit script-path-ext)
        ==
    ^-  hexb:btc
    =/  msg=hexb:btc  (message inputs signing-index outputs nversion nlocktime ext)
    =/  sighash=@
      (tagged-hash:schnorr:secp256k1:secp:crypto 'TapSighash' [p=wid.msg q=dat.msg])
    [32 `@ux`sighash]
  --
::
::  ============================================================================
::  Legacy: P2PKH Sighash
::  ============================================================================
::
::  Original Bitcoin signing. Serializes entire transaction (no component hashes).
::  Signing input gets scriptPubKey as scriptSig, others get empty. Uses dsha256.
::  Preimage: version || inputs || outputs || locktime || sighash_type
::
++  legacy
  |%
  ++  serialize-input
    |=  [inp=sighash-input-legacy is-signing=?]
    ^-  hexb:btc
    =/  script-sig=hexb:btc
      ?:  is-signing  script-pubkey.inp
      [0 0x0]
    %-  cat:byt:bcu
    :~  [32 txid.inp]
        (flip:byt:bcu [4 vout.inp])
        (encode-varint:enc wid.script-sig)
        script-sig
        (flip:byt:bcu [4 sequence.inp])
    ==
  ::  +serialize-inputs: Serialize all inputs for sighash
  ::
  ++  serialize-inputs
    |=  [inputs=(list sighash-input-legacy) signing-index=@ud]
    ^-  hexb:btc
    =/  n=@  (lent inputs)
    ?:  =(0 n)  [0 0x0]
    %-  cat:byt:bcu
    %+  turn  (gulf 0 (dec n))
    |=  idx=@ud
    (serialize-input (snag idx inputs) =(idx signing-index))
  ::  +preimage: Build legacy preimage
  ::
  ::  version || inputs || outputs || locktime || sighash_type
  ::
  ++  preimage
    |=  $:  inputs=(list sighash-input-legacy)
            signing-index=@ud
            outputs=(list output:bc:tt)
            nversion=@ud
            nlocktime=@ud
        ==
    ^-  hexb:btc
    %-  cat:byt:bcu
    :~  (flip:byt:bcu [4 nversion])
        (encode-varint:enc (lent inputs))
        (serialize-inputs inputs signing-index)
        (encode-varint:enc (lent outputs))
        (serialize-outputs:enc outputs)
        (flip:byt:bcu [4 nlocktime])
        (flip:byt:bcu [4 1])                   ::  SIGHASH_ALL
    ==
  ::  +build: Build legacy sighash (dsha256 of preimage)
  ::
  ++  build
    |=  $:  inputs=(list sighash-input-legacy)
            signing-index=@ud
            outputs=(list output:bc:tt)
            nversion=@ud
            nlocktime=@ud
        ==
    ^-  hexb:btc
    (dsha256:bcu (preimage inputs signing-index outputs nversion nlocktime))
  --
--
