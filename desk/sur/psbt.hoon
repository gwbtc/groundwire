/-  bc=bitcoin
|%
++  global
  |%
  ++  unsigned-tx               0x0
  ++  xpub                      0x1
  ++  tx-version                0x2
  ++  fallback-locktime         0x3
  ++  input-count               0x4
  ++  output-count              0x5
  ++  tx-modifiable             0x6
  ++  version                   0xfb
  ++  proprietary               0xfc
  --
::
++  in
  |%
  ++  non-witness-utxo          0x0
  ++  witness-utxo              0x1
  ++  partial-sig               0x2
  ++  sighash                   0x3
  ++  redeemscript              0x4
  ++  witnessscript             0x5
  ++  bip32-derivation          0x6
  ++  scriptsig                 0x7
  ++  scriptwitness             0x8
  ++  previous-txid             0xe
  ++  output-index              0xf
  ++  sequence                  0x10
  ++  required-time-locktime    0x11
  ++  required-height-locktime  0x12
  ++  proprietary               0xfc
  --
::
++  out
  |%
  ++  redeemscript              0x0
  ++  witnessscript             0x1
  ++  bip32-derivation          0x2
  ++  amount                    0x3
  ++  script                    0x4
  ++  proprietary               0xfc
  --
::
++  sighash
  |%
  ++  all                       0x1
  ++  none                      0x2
  ++  single                    0x3
  ++  anyone-can-pay            0x80
  --
::
++  version    0
++  separator  0x0
++  magic      0x70.7362.74ff
::
+$  sats       @
:: +$  keyid      @
+$  key        hexb:bc
+$  value      hexb:bc
+$  pubkey     hexb:bc
+$  privkey    hexb:bc
+$  signature  hexb:bc
+$  witness    (list hexb:bc)
+$  keyinfo    [fprint=hexb:bc path=(list @u)]
+$  outpoint   [txid=@ux idx=@]
::
++  tx
  |%
  +$  in
    $+  psbt-tx-input
    $:  prevout=outpoint
        script-sig=(unit hexb:bc)
        nsequence=@
        script-witness=(unit witness)
    ==
  ::
  +$  out
    $+  psbt-tx-output
    $:  value=sats
        script-pubkey=hexb:bc
    ==
  ::
  +$  tx
    $+  psbt-tx
    $:  vin=(list in)
        vout=(list out)
        nversion=@
        nlocktime=@
    ==
  --
::
+$  script-type
  $+  psbt-script-type
  $?  %p2pk
      %p2pkh
      %p2sh
      %p2wpkh
      %p2wsh
      %unknown
  ==
::
+$  input
  $+  psbt-input
  $:  in:tx
      ::  TODO: distinguish between larval input and optional input - subgroup according to exclusivity and reason for unit
      ::  TODO: any mutual exclusives type as such
      non-witness-utxo=(unit tx:tx)
      witness-utxo=(unit out:tx)
      redeem-script=(unit hexb:bc)
      witness-script=(unit hexb:bc)
      final-script-sig=(unit hexb:bc)
      final-script-witness=(unit witness)
      hd-keypaths=(map pubkey keyinfo)
      partial-sigs=(map pubkey signature)
      unknown=(map key value)
      sighash=(unit @ux)
      ::
      =script-type
      num-sigs=@                ::  number required sigs for multisig
      ::  TODO: if ordering is irrelevant until finalization, change from list to set
      pubkeys=(list pubkey)     ::  pubkeys for multisig
      trusted-value=(unit sats)
  ==
::
+$  output
  $+  psbt-output
  $:  out:tx
      ::  see above mutual exclusivity?
      redeem-script=(unit hexb:bc)
      witness-script=(unit hexb:bc)
      hd-keypaths=(map pubkey keyinfo)
      unknown=(map key value)
  ==
::
+$  psbt
  $+  psbt
  $:  tx:tx
      inputs=(list input)
      outputs=(list output)
      unknown=(map key value)
  ==
--
