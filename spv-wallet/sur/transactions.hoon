::  transactions.hoon - Shared types for transaction building
::
::  Two namespaces:
::    ++ap  App-legible types (addresses, privkeys - what callers work with)
::    ++bc  Bitcoin-legible types (scripts, witnesses - wire format)
::
/+  btc=bitcoin, taproot
|%
::  Shared types used by both namespaces
::
+$  taproot-spend
  $%  [%key-path merkle-root=(unit @ux)]
      [%script-path =tapleaf:taproot proof=(list @ux) witness=(list hexb:btc)]
  ==
::
+$  spend-type
  $%  [%p2pkh ~]
      [%p2wpkh ~]
      [%p2sh-p2wpkh ~]
      [%p2tr taproot-spend]
  ==
::
+$  network  ?(%main %testnet3 %testnet4 %regtest %signet)
::
::  ============================================================================
::  App-legible types (what callers work with)
::  ============================================================================
::
++  ap
  |%
  ::  Input: a UTXO to spend, with the key to spend it
  ::
  +$  input
    $:  privkey=@ux
        pubkey=@ux
        txid=@ux                ::  little-endian wire format
        vout=@ud
        amount=@ud
        sequence=@ud            ::  0xffff.ffff for final
        =spend-type
    ==
  ::  Output: where to send coins
  ::
  +$  output  [address=@t amount=@ud]
  --
::
::  ============================================================================
::  Bitcoin-legible types (wire format for encoding)
::  ============================================================================
::
++  bc
  |%
  ::  Input: encoded for transaction serialization
  ::
  +$  input
    $:  txid=@
        vout=@
        sequence=@
        script-sig=hexb:btc     ::  fully built (empty for native segwit/taproot)
        witness=hexb:btc        ::  fully built (empty for legacy)
    ==
  ::  Output: encoded for transaction serialization
  ::
  +$  output
    $:  script-pubkey=hexb:btc
        amount=@
    ==
  --
--
