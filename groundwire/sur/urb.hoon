::  sur/urb.hoon
::
::  Type definitions for the urb bitcoin metaprotocol,
::  based on the ord metaprotocol, along with state
::  for lib/urb and %gw-btc
::
/-  bitcoin, ord
|%
::
+$  unv-ids       (map @p point)
::  $jael-poke: %noun poke Jael sends the domain's verifier agent in
::  the confidential-comets protocol.  %jael-writ asks us to verify a
::  comet's self-attestation ($pass); %jael-anew asks us to re-encode
::  our own comet's pass with a fresh off-chain reveal log.  see
::  app/gw-btc.hoon and, in gwbtc/urbit, sys/vane/jael.hoon.
::
::    The companion poke on the OTHER side of %anew -- the one Causeway
::    sends %gw-btc to EXTEND that reveal log after an owner-driven
::    custody transaction -- is $ingest in sur/self-attestation.hoon
::    (`[%gw-custody-entry entry=custody-entry]`, also on the %noun
::    mark).  It cannot live here: sur/self-attestation already imports
::    this file for $point and $fief, so naming $custody-entry from here
::    would be a Clay import cycle.
::
+$  jael-poke
  $%  [%jael-writ dom=@tas who=ship =pass]
      [%jael-anew dom=@tas]
  ==
+$  state
  $:  block-id=id:block:bitcoin
      =sont-map:ord
      =insc-ids:ord
      =unv-ids
  ==
::
::  sotx = signed ord tx (?)
::  We often call a list of sotx, raw-sotx, 
::  or skim-sotx a "sots".
+$  sotx      [[=ship sig=(unit @)] skim-sotx]
+$  raw-sotx  [raw=octs sot=sotx]
++  skim-sotx
  =<  many
  |%
  +$  many
    $%  single
        [%batch bat=(list single)]
    ==
  ::
  +$  single
        ::  spkh may be redundant here; we could make vout not be a unit
    $%  $:  %spawn  =pass
            ::from=(unit [=vout =off])
            fief=(unit fief)
            to=[spkh=@ux vout=(unit vout:ord) =off:ord tej=off:ord]
        ==
        [%keys =pass breach=?]
        [%escape parent=ship sig=(unit @)]
        [%cancel-escape parent=ship]
        [%adopt =ship]
        [%reject =ship]
        [%detach =ship]
        [%fief fief=(unit fief)]
        [%set-mang mang=(unit mang)]
    ==
  --
::
+$  mang  $%([%sont =sont:ord] [%pass =pass])
::
::  Ownership and networking info for a @p
+$  point
  $:  $=  own
      $:  =sont:ord
          mang=(unit mang)
      ==
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
+$  turf  (list @t)  ::  domain, tld first
::
+$  fief
  $%  [%turf p=(list turf) q=@udE]
      [%if p=@ifF q=@udE]
      [%is p=@isH q=@udE]
  ==
::
::  effects are an intermediate type that gets
::  converted to jael udiffs
+$  effect
  $%  diff
      [%xfer from=sont:ord to=sont:ord]
      [%insc =insc:ord sont=$@(~ sont:ord) =mail:ord]
  ==
+$  diff
  $%  [%dns domains=(list @t)]
      $:  %point  =ship
          $%  [%rift =rift]
              [%keys =life =pass]
              [%sponsor sponsor=(unit @p)]
              [%escape to=(unit @p)]
              [%owner =sont:ord]
              ::  %public: this identity's state is now published on chain.
              ::  Emitted by +index-point:urb-core for every accepted
              ::  OP_RETURN publication, spawn or state update.  It carries
              ::  no jael udiff (+fx-to-udiffs drops it); its only consumer
              ::  is %gw-btc, which uses it to move a comet OUT of its
              ::  .confidential set -- the confidential -> public transition
              ::  of decisions addendum section 2.  Only the holder of the
              ::  tracked identity sat can produce a transaction that
              ::  reaches +index-point, so this effect IS the owner's
              ::  consent to declassify.
              ::
              [%public ~]
              ::[%spawn-proxy =sont:ord]
              [%mang mang=(unit mang)]
              ::[%voting-proxy =sont:ord]
              ::[%transfer-proxy =sont:ord]
              ::[%dominion =dominion]
              [%fief fief=(unit fief)]
  ==  ==  ==
::
++  urb-tx
  =<  tx
  |%
  +$  tx    [id=txid:ord data]
  +$  data
    $+  urb-tx-data
    $:  is=(list input)
        os=(list output:tx:bitcoin)
        locktime=@ud
        nversion=@ud
        segwit=(unit @ud)
    ==
  +$  input
    [[sots=(list raw-sotx) value=@ud] inputw:tx:bitcoin]
  --
::
++  urb-block
  =<  block
  |%
  +$  hax   @ux
  +$  num   @ud
  +$  id    [=hax =num]
  +$  block
    $:  =hax
        reward=@ud
        height=@ud
        txs=(list urb-tx)
    ==
  --
--
