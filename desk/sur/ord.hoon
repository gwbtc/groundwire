/-  bc=bitcoin
|%
++  script
  =<  script
  |%
  +$  script
    $+  ord-script
    (list op)
  ::
  +$  op
    $+  ord-script-op
    $@  $?  %op-nop
            %op-if
            %op-notif
            %op-else
            %op-endif
            %op-verify
            %op-return
        ::
            %op-toaltstack
            %op-fromaltstack
            %op-ifdup
            %op-depth
            %op-drop
            %op-dup
            %op-nip
            %op-over
            %op-pick
            %op-roll
            %op-rot
            %op-swap
            %op-tuck
            %op-2drop
            %op-2dup
            %op-3dup
            %op-2over
            %op-2rot
            %op-2swap
        ::
            %op-cat
            %op-substr
            %op-left
            %op-right
            %op-size
        ::
            %op-invert
            %op-and
            %op-or
            %op-xor
            %op-equal
            %op-equalverify
        ::
            %op-1add
            %op-1sub
            %op-2mul
            %op-2div
            %op-negate
            %op-abs
            %op-not
            %op-0notequal
            %op-add
            %op-sub
            %op-mul
            %op-div
            %op-mod
            %op-lshift
            %op-rshift
            %op-booland
            %op-boolor
            %op-numequal
            %op-numequalverify
            %op-numnotequal
            %op-lessthan
            %op-greaterthan
            %op-lessthanorequal
            %op-greaterthanorequal
            %op-min
            %op-max
            %op-within
        ::
            %op-ripemd160
            %op-sha1
            %op-sha256
            %op-hash160
            %op-hash256
            %op-codeseparator
            %op-checksig
            %op-checksigverify
            %op-checkmultisig
            %op-checkmultisigverify
        ::
            %op-checklocktimeverify
            %op-checksequenceverify
        ::
            %op-pubkeyhash
            %op-pubkey
            %op-invalidopcode
        ::
            %op-reserved
            %op-ver
            %op-verif
            %op-vernotif
            %op-reserved1
            %op-reserved2
        ::
            %op-nop1
            %op-nop4
            %op-nop5
            %op-nop6
            %op-nop7
            %op-nop8
            %op-nop9
            %op-nop10
        ==
     $:  %op-push
         $%  [p=%num octs=[p=%1 q=@]]
             [p=?(~ %1 %2 %4) =octs]
         ==
     ==
  ::
  --
::
+$  address
  $+  ord-address
  $%  [%base58 @uc]
      [%bech32 @tas]
  ==
::
+$  sats
  $+  ord-sats
  @ud
::
+$  txid
  $+  ord-txid
  @ux   ::  txid
::
+$  pos
  $+  ord-pos
  @ud   ::  index in tx output set
::
+$  off
  $+  ord-off
  @ud   ::  sat index in single output amount
::
+$  pntr
  $+  ord-pntr
  @ud   ::  sat index in total amount of tx outputs
::
+$  sont
  $+  ord-sont
  [=txid =pos =off]
::
::
+$  insc
  $+  ord-insc
  [=txid idx=@ud]
::
+$  ordi
  $+  ord-ordi
  [p=@ux q=@ud]
::
+$  urdi
  $+  ord-urdi
  [p=@ux q=@ud r=@ud]
::
+$  mail
  $+  ord-mail
  $:  mime=$@(~ [p=@ud (each @t @)])    :: tag 1 mimetype
      code=$@(~ [p=@ud (each @t @)])    :: tag 9 content encoding
      pntr=$@(~ [p=@ud (each @ud @)])   :: tag 2 pointer
      rent=$@(~ [p=@ud (each insc @)])  :: tag 3 parent
      gate=$@(~ [p=@ud (each insc @)])  :: tag 11 delegate
      meta=$@(~ octs)                   :: tag 5 metadata (multple pushes)
      prot=$@(~ octs)                   :: tag 7 meta protocol
      data=$@(~ octs)                   :: tag 0 content (all pushed data after push of 0 tag)
  ==
+$  draft
  $+  ord-draft
  (map @ud octs)
::
+$  raw-sotx
  $+  urb-raw-sotx
  [raw=octs sot=sotx]
::+$  raw-sotx     [sig=@ raw=octs =sotx]
+$  sotx
  $+  urb-sotx
  [[=ship sig=(unit @)] skim-sotx]
::
++  skim-sotx
  =<  many
  |%
  +$  many
    $+  urb-skim-many
    $%  single
        [%batch bat=(list single)]
    ==
  ::
  +$  single
    $+  urb-skim-single
    $%  $:  %spawn  =pass
            ::from=(unit [=pos =off])
            to=[spkh=@ux pos=(unit pos) =off tej=off]
        ==
        [%keys =pass breach=?]
        [%escape parent=ship]
        [%cancel-escape parent=ship]
        [%adopt =ship]
        [%reject =ship]
        [%detach =ship]
        [%fief fief=(unit fief)]
        [%set-mang mang=(unit mang)]
    ==
  ::
  --
::
+$  mang
  $+  urb-mang
  $%([%sont =sont] [%pass =pass])
::
+$  point
  $+  urb-point
  $:  ::  domain
      ::
      ::=dominion
      ::
      ::  ownership
      ::
      $=  own
      $:  =sont
          mang=(unit mang)
      ==
      ::
      ::  networking
      ::
      $=  net
      $:  rift=@ud
          =life
          =pass
          sponsor=[has=? who=@p]
          escape=(unit @p)
          fief=(unit fief)
      ==
  ==
::
+$  turf
  $+  urb-turf
  (list @t)                                     ::  domain, tld first
::
+$  fief
  $+  urb-fief
  $%  [%turf p=(list turf) q=@udE]
      [%if p=@ifF q=@udE]
      [%is p=@isH q=@udE]
  ==
::
+$  diff
  $+  urb-diff
  $%  [%dns domains=(list @t)]
      $:  %point  =ship
          $%  [%rift =rift]
              [%keys =life =pass]
              [%sponsor sponsor=(unit @p)]
              [%escape to=(unit @p)]
              [%owner =sont]
              ::[%spawn-proxy =sont]
              [%mang mang=(unit mang)]
              ::[%voting-proxy =sont]
              ::[%transfer-proxy =sont]
              ::[%dominion =dominion]
              [%fief fief=(unit fief)]
  ==  ==  ==
::
+$  sont-val
  $+  ord-sont-val
  [com=(unit @p) ins=(set insc)]
::
+$  vout-map
  $+  ord-vout-map
  [value=@ud sats=(map off sont-val)]
::
+$  sont-map
  $+  ord-sont-map
  (map [txid pos] vout-map)
::
+$  insc-ids
  $+  ord-insc-ids
  (map insc [=sont =mail])
::
+$  unv-ids
  $+  urb-unv-ids
  (map @p point)
::
+$  state
  $+  ord-state
  $:  block-id=id:block:bc
      =sont-map
      =insc-ids
      =unv-ids
  ==
::
+$  effect
  $+  ord-effect
  $%  diff
      [%xfer from=sont to=sont]
      [%insc =insc sont=$@(~ sont) =mail]
  ==
++  gw-tx
  =<  tx
  |%
  +$  tx
    $+  ord-gw-tx
    [id=txid data]
  ::
  +$  p-tx
    $+  ord-gw-tx-p-tx
    [id=txid data]
  ::
  +$  data
    $+  ord-gw-tx-data
    $:  is=(list input)
        os=(list output:tx:bc)
        locktime=@ud
        nversion=@ud
        segwit=(unit @ud)
    ==
  ::
  +$  input
    $+  ord-gw-tx-input
    [[sots=(list raw-sotx) value=@ud] inputw:tx:bc]
  --
::
++  gw-block
  =<  block
  |%
  +$  id
    $+  ord-gw-block-id
    [=hax =num]
  ::
  +$  hax
    $+  ord-gw-block-hax
    @ux
  ::
  +$  num
    $+  ord-gw-block-num
    @ud
  ::
  +$  block
    $+  ord-gw-block
    $:  =hax
        reward=@ud
        height=@ud
        txs=(list gw-tx)
    ==
  --
::
--
