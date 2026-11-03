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
::
::    .seen is PROVENANCE, and it is the only field here that is about US
::    rather than about the identity: the hash of the block we most
::    recently learned this point's state from.
::
::    It is REFRESHED ON EVERY OBSERVATION -- a verified attestation, an
::    on-chain publication, a custody move the block scanner walked --
::    and not fixed when the point was first indexed (team decision,
::    2026-08-10; ops/doc/opret-revision/04-decisions-addendum.md section
::    11b).  It therefore names the most recent evidence for this point,
::    which is the thing a chain reorganisation can take away.  Origin
::    would be the wrong quantity: a point spawned a year ago and moved
::    yesterday is invalidated by a reorg of yesterday's block.
::
::    ONE hash, so it names the LAST block the point was seen in, not
::    every block its custody log touches.  A reorg deep enough to orphan
::    an EARLIER hop is not caught by this field; it is caught by the
::    re-attestation, which cannot be fetched at a height that no longer
::    holds its txid (+fetch-tx-at:lc-attestation strand-fails).
::
::    ~ means NO PROVENANCE RECORDED: a point that predates this field
::    (+on-load's $gw-state-13 migration) and has not been observed
::    since.  It cannot be filtered against a list of orphaned blocks,
::    and a reorg therefore LEAVES IT ALONE -- ~ is "we cannot tell",
::    not "orphaned", and an unevaluable condition never produces a
::    negative outcome here.  See +orphaned-points in lib/urb-core.hoon.
::    The population of such points only ever shrinks: anything
::    re-observed acquires a hash.
::
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
  ::
      seen=(unit hax:block:bitcoin)
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
      ::  %claim: an OP_RETURN publication was found in this block.
      ::
      ::    .pass is the publication's own pass with the custody log
      ::    COMPLETED -- +process-publication:urb-core appended the entry
      ::    for the transaction that carried it, which is the one entry
      ::    the publisher could not write down.  So this is an ordinary
      ::    self-attestation that happens to have arrived by chain
      ::    instead of by ames, and it carries no judgement whatsoever:
      ::    the scanner cannot walk a custody log (the evidence is in
      ::    blocks it has already streamed past), so %gw-btc hands it to
      ::    the SAME +verify-lc a %jael-writ gets.
      ::
      ::    It produces no jael udiff (+fx-to-udiffs drops it) and it can
      ::    never produce a verdict: nobody asked us to judge this comet,
      ::    so a failed publication is a log line and nothing else.
      ::
      [%claim who=ship =pass]
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
