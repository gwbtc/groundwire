::  encode.hoon - Bitcoin transaction serialization
::
::  Pure byte serialization for SegWit (BIP-141) and legacy formats.
::  Takes fully-built inputs (with script-sig and witness) and outputs.
::
/-  tt=transactions
/+  bcu=bitcoin-utils, btc=bitcoin
|%
::  +encode-varint: Encode number as Bitcoin VarInt (CompactSize)
::
++  encode-varint
  |=  n=@
  ^-  hexb:btc
  ::  0-252: single byte, value as-is
  ::
  ?:  (lth n 253)
    [1 n]
  ::  253-65535: marker 0xfd + 2 bytes little-endian
  ::
  ?:  (lth n 65.536)
    (cat:byt:bcu ~[[1 0xfd] (flip:byt:bcu [2 n])])
  ::  65536-4294967295: marker 0xfe + 4 bytes little-endian
  ::
  ?:  (lth n 4.294.967.296)
    (cat:byt:bcu ~[[1 0xfe] (flip:byt:bcu [4 n])])
  ::  larger: marker 0xff + 8 bytes little-endian
  ::
  (cat:byt:bcu ~[[1 0xff] (flip:byt:bcu [8 n])])
::  +serialize-outputs: Serialize outputs to wire format
::
::  Each output: [amount 8B LE] [script-len] [script-pubkey]
::
++  serialize-outputs
  |=  outputs=(list output:bc:tt)
  ^-  hexb:btc
  %-  cat:byt:bcu
  %+  turn  outputs
  |=  =output:bc:tt
  %-  cat:byt:bcu
  :~  (flip:byt:bcu [8 amount.output])      ::  amount: 8 bytes little-endian
      [1 wid.script-pubkey.output]          ::  script length
      script-pubkey.output                  ::  script-pubkey bytes
  ==
::  +serialize-inputs: Serialize inputs to wire format
::
::  Each input: [txid 32B] [vout 4B LE] [script-sig-len] [script-sig] [sequence 4B LE]
::
++  serialize-inputs
  |=  inputs=(list input:bc:tt)
  ^-  hexb:btc
  %-  cat:byt:bcu
  %+  turn  inputs
  |=  =input:bc:tt
  %-  cat:byt:bcu
  :~  [32 txid.input]                            ::  txid: 32 bytes
      (flip:byt:bcu [4 vout.input])              ::  vout: 4 bytes little-endian
      (encode-varint wid.script-sig.input)       ::  script-sig length
      script-sig.input                           ::  script-sig bytes
      (flip:byt:bcu [4 sequence.input])          ::  sequence: 4 bytes little-endian
  ==
::  +serialize-witnesses: Concatenate pre-built witness data
::
++  serialize-witnesses
  |=  inputs=(list input:bc:tt)
  ^-  hexb:btc
  %-  cat:byt:bcu
  (turn inputs |=(=input:bc:tt witness.input))
::  +segwit-transaction: Encode complete SegWit transaction (BIP-141)
::
::  Returns serialized transaction with marker/flag bytes and witness data.
::
++  segwit-transaction
  |=  $:  inputs=(list input:bc:tt)
          outputs=(list output:bc:tt)
          nversion=@ud
          nlocktime=@ud
      ==
  ^-  hexb:btc
  =/  input-count=@  (lent inputs)
  =/  output-count=@  (lent outputs)
  %-  cat:byt:bcu
  :~  (flip:byt:bcu [4 nversion])
      [1 0]                                    ::  marker (0x00)
      [1 1]                                    ::  flag (0x01)
      (encode-varint input-count)
      (serialize-inputs inputs)
      (encode-varint output-count)
      (serialize-outputs outputs)
      (serialize-witnesses inputs)
      (flip:byt:bcu [4 nlocktime])
  ==
::  +legacy-transaction: Encode complete legacy (non-SegWit) transaction
::
::  No marker/flag bytes, no witness data. Original Bitcoin format.
::
++  legacy-transaction
  |=  $:  inputs=(list input:bc:tt)
          outputs=(list output:bc:tt)
          nversion=@ud
          nlocktime=@ud
      ==
  ^-  hexb:btc
  =/  input-count=@  (lent inputs)
  =/  output-count=@  (lent outputs)
  %-  cat:byt:bcu
  :~  (flip:byt:bcu [4 nversion])
      (encode-varint input-count)
      (serialize-inputs inputs)
      (encode-varint output-count)
      (serialize-outputs outputs)
      (flip:byt:bcu [4 nlocktime])
  ==
--
