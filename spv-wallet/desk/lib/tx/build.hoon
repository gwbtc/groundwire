::  build.hoon - Bitcoin transaction building and signing
::
::  Pipeline: inputs:ap + outputs:ap → outputs:bc → inputs:bc → bytes → hex
::
::  Limitation: No taproot annex support
::
/-  tt=transactions
/+  bcu=bitcoin-utils, btc=bitcoin, taproot
/+  enc=tx-encode, sig=tx-sighash, auth=tx-auth, signer=tx-signer
|%
::  +decode-outputs: Convert app outputs to bitcoin outputs
::
::  Transforms human-readable addresses into scriptPubKeys.
::  Validates each address matches the network before encoding.
::
++  decode-outputs
  |=  [=network:tt outputs=(list output:ap:tt)]
  ^-  (list output:bc:tt)
  %+  turn  outputs
  |=  [address=@t amount=@ud]
  ::  Validate address matches network
  ::
  ?>  (validate-address address network)
  ::  Build scriptPubKey from address
  ::
  =/  script-pubkey=hexb:btc
    ?:  (is-taproot-address:taproot address)
      (to-script-pubkey:taproot address)
    =/  parsed=address:btc  (from-cord:adr:btc address)
    (to-script-pubkey:adr:btc parsed)
  [script-pubkey amount]
::  +validate-address: Check address prefix matches network
::
++  validate-address
  |=  [address=@t =network:tt]
  ^-  ?
  =/  addr=tape  (trip address)
  =/  c1=tape  (scag 1 addr)
  ?-  network
    %main  ?|  =("bc1" (scag 3 addr))
               =("1" c1)
               =("3" c1)
           ==
    %regtest  ?|  =("bcrt1" (scag 5 addr))
                  =("m" c1)  =("n" c1)  =("2" c1)
              ==
    ?(%testnet3 %testnet4 %signet)
      ?|  =("tb1" (scag 3 addr))
          =("bcrt1" (scag 5 addr))
          =("m" c1)  =("n" c1)  =("2" c1)
      ==
  ==
::  +sign-inputs: Sign all inputs in a transaction
::
++  sign-inputs
  |=  $:  inputs=(list input:ap:tt)
          outputs=(list output:bc:tt)
          nversion=@ud
          nlocktime=@ud
      ==
  ^-  (list input:bc:tt)
  =/  n=@ud  (lent inputs)
  =/  idx=@ud  0
  |-
  ?:  =(idx n)  ~
  :-  (sign-input (snag idx inputs) inputs idx outputs nversion nlocktime)
  $(idx +(idx))
::  +sign-input: Sign a single input
::
::  Dispatches to appropriate sighash algorithm based on spend-type.
::  Returns bitcoin-legible input with script-sig and witness.
::
++  sign-input
  |=  $:  input=input:ap:tt
          all-inputs=(list input:ap:tt)
          idx=@ud
          outputs=(list output:bc:tt)
          nversion=@ud
          nlocktime=@ud
      ==
  ^-  input:bc:tt
  =/  [script-sig=hexb:btc witness=hexb:btc]
    ?-    -.spend-type.input
    ::
    ::  Legacy P2PKH: ECDSA signature in scriptSig
    ::
        %p2pkh
      =/  sighash  (build:legacy:sig (to-legacy-inputs:sig all-inputs) idx outputs nversion nlocktime)
      =/  signature  (ecdsa:signer sighash privkey.input)
      (p2pkh:auth signature pubkey.input)
    ::
    ::  SegWit v0: ECDSA signature in witness (BIP-143 sighash)
    ::
        ?(%p2wpkh %p2sh-p2wpkh)
      =/  sighash  (build:bip143:sig (to-bip143-inputs:sig all-inputs) idx outputs nversion nlocktime)
      =/  signature  (ecdsa:signer sighash privkey.input)
      ?-  -.spend-type.input
        %p2wpkh       (p2wpkh:auth signature pubkey.input)
        %p2sh-p2wpkh  (p2sh-p2wpkh:auth signature pubkey.input)
      ==
    ::
    ::  Taproot: Schnorr signature in witness (BIP-341 sighash)
    ::
        %p2tr
      =/  bip341-inputs  (to-bip341-inputs:sig all-inputs)
      ?-    -.+.spend-type.input
        ::  Key-path: sign with tweaked private key
        ::
          %key-path
        =/  sighash  (build:bip341:sig bip341-inputs idx outputs nversion nlocktime ~)
        =/  merkle-root  merkle-root.+.spend-type.input
        =/  tweaked-privkey  (tweak-privkey:taproot privkey.input pubkey.input merkle-root)
        (p2tr-keypath:auth (schnorr:signer sighash tweaked-privkey 0))
        ::  Script-path: sign with internal key, include script + proof
        ::
          %script-path
        =/  leaf-hash  (leaf-hash:taproot tapleaf.+.spend-type.input)
        =/  ext  [leaf-hash 0xc0 `@ud`0xffff.ffff]
        =/  sighash  (build:bip341:sig bip341-inputs idx outputs nversion nlocktime `ext)
        =/  signature  (schnorr:signer sighash privkey.input 0)
        %:  p2tr-scriptpath:auth
          signature
          pubkey.input
          tapleaf.+.spend-type.input
          proof.+.spend-type.input
          witness.+.spend-type.input
        ==
      ==
    ==
  [txid.input vout.input sequence.input script-sig witness]
::  +encode-transaction: Serialize signed transaction to bytes
::
++  encode-transaction
  |=  $:  inputs=(list input:bc:tt)
          outputs=(list output:bc:tt)
          nversion=@ud
          nlocktime=@ud
          segwit=?
      ==
  ^-  hexb:btc
  ?:  segwit
    (segwit-transaction:enc inputs outputs nversion nlocktime)
  (legacy-transaction:enc inputs outputs nversion nlocktime)
::  +has-segwit: Check if any input requires witness data
::
++  has-segwit
  |=  inputs=(list input:ap:tt)
  ^-  ?
  %+  lien  inputs
  |=(=input:ap:tt ?=(?(%p2wpkh %p2sh-p2wpkh %p2tr) -.spend-type.input))
::  +render-hex: Convert bytes to hex string
::
++  render-hex
  |=  h=hexb:btc
  ^-  tape
  (trip (crip ((x-co:co (mul 2 wid.h)) dat.h)))
::  +build-transaction: Build and sign a complete transaction
::
::  Main entry point. Takes app-level inputs/outputs and returns
::  a broadcastable hex-encoded transaction.
::
++  build-transaction
  |=  $:  =network:tt
          nversion=@ud
          inputs=(list input:ap:tt)
          outputs=(list output:ap:tt)
          nlocktime=@ud
      ==
  ^-  tape
  ::  Decode outputs (addresses → scripts)
  ::
  =/  bc-outputs=(list output:bc:tt)
    (decode-outputs network outputs)
  ::  Sign inputs (app inputs → bitcoin inputs)
  ::
  =/  bc-inputs=(list input:bc:tt)
    (sign-inputs inputs bc-outputs nversion nlocktime)
  ::  Encode and render
  ::
  =/  tx-bytes=hexb:btc
    (encode-transaction bc-inputs bc-outputs nversion nlocktime (has-segwit inputs))
  (render-hex tx-bytes)
--
