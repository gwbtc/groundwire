::  sur/ord.hoon
::
::  Type definitions for the ord bitcoin metaprotocol
::
|%
+$  address
  $+  ord-address
  $%  [%base58 @uc]
      [%bech32 @tas]
  ==
::
+$  sats  @ud
+$  txid  @ux
+$  vout  @ud   ::  index in tx output set
+$  off   @ud   ::  sat index within output
+$  sont  [=txid =vout =off]  :: satpoint
::
+$  insc  [=txid idx=@ud]
+$  ordi  [p=@ux q=@ud]
+$  urdi  [p=@ux q=@ud r=@ud]
::
+$  mail
  $:  mime=$@(~ [p=@ud (each @t @)])    :: tag 1 mimetype
      code=$@(~ [p=@ud (each @t @)])    :: tag 9 content encoding
      pntr=$@(~ [p=@ud (each @ud @)])   :: tag 2 pointer
      rent=$@(~ [p=@ud (each insc @)])  :: tag 3 parent
      gate=$@(~ [p=@ud (each insc @)])  :: tag 11 delegate
      meta=$@(~ octs)                   :: tag 5 metadata (multple pushes)
      prot=$@(~ octs)                   :: tag 7 meta protocol
      data=$@(~ octs)                   :: tag 0 content (all pushed data after push of 0 tag)
  ==
::
+$  draft
  (map @ud octs)
::
+$  sont-map  (map [txid vout] vout-map)
+$  vout-map  [value=@ud sats=(map off sont-val)]
+$  sont-val  [com=(unit @p) ins=(set insc)]
::
+$  insc-ids  (map insc [=sont =mail])
--
