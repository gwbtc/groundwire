::  sur/urb.hoon
::
::  Type definitions for the urb bitcoin metaprotocol,
::  based on the ord metaprotocol, along with state
::  for lib/urb and %urb-watcher
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
+$  jael-poke
  $%  [%jael-writ dom=@tas who=ship =pass]
      [%jael-anew dom=@tas]
  ==
::  Confidential-comets custody log (spec section 8, variant B).  The
::  off-chain reveal of an on-chain, key-path-spend ownership history for
::  a single sat: one $xtr-entry per spend, spawn -> tip.  It rides in the
::  (untweaked) .xtr of a suite-%c pass and is verified by lib/gw-verify
::  against the agent's own bitcoin node.  The log is a LOCATOR, not a
::  proof: each entry names a txid + block-height the agent fetches and
::  re-checks; state commitments (spec section 2.1) mean only entries that
::  carry a $reveal re-attest networking state, and only the latest such
::  one is authoritative -- bare entries (reveal=~) just prove the sat
::  moved (an ordinary transfer from any wallet).
::
::  $reveal: the disclosed tapleaf and its internal key, enough to
::  recompute the committed output key Q = P + H_TapTweak(x(P)||leaf-hash)*G
::  (single-leaf sparse tree, so the merkle root IS the leaf hash) and to
::  re-parse the committed $sotx(es) from the leaf script.
::
+$  reveal
  $:  internal-key=@ux              ::  P, 33-byte COMPRESSED (0x02/0x03 ||
      ::                                x), as lib/taproot's decompress-point
      ::                                and Causeway's xtr encoder require
      leaf-version=@ux              ::  tapleaf version byte (0xc0)
      leaf-script=hexb:bitcoin      ::  the committed script (an "unv")
  ==
+$  xtr-entry  [txid=@ux block-height=@ud reveal=(unit reveal)]
+$  custody-log  (list xtr-entry)
::
::  $gw-state: the networking state a %spawn or %state commitment attests
::  (spec section 2.1: keys, life, sponsorship).  Distinct from $point:
::  this is the intermediate the custody walk folds toward the latest
::  commitment before projecting onto Jael's $point.
::
+$  gw-state
  $:  =life
      key=@                             ::  messaging key (cry.pub of the pass)
      sponsor=(unit @p)
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
        ::  %state (opcode 9): a full networking-STATE re-attestation for
        ::  the confidential-comets protocol (state-commitments model, spec
        ::  section 2.1).  Unlike the event singles above, %state is a
        ::  snapshot: the identity's current life, messaging key, and
        ::  sponsor.  Committed in an off-chain-revealed tapleaf; the
        ::  latest such commitment along a sat's custody chain is the
        ::  authoritative state (see lib/gw-verify).  A sponsor carries a
        ::  Schnorr consent signature proving it agreed to adopt the child.
        ::
        ::  .key is the raw messaging key (cry.pub of the suite-%c pass),
        ::  NOT a full $pass: the signing key that fixes the @p is immutable
        ::  (spec section 2.1), so only the encryption key rotates and needs
        ::  re-attesting.
        [%state =life key=@ sponsor=(unit consent)]
    ==
  --
::
::  $consent: a sponsor's assent to sponsor a confidential comet, as
::  committed in a %state leaf.  .who is the sponsor @p; .sig is its
::  BIP-340 Schnorr signature over the consent message (see lib/gw-verify).
+$  consent  [who=@p sig=@]
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
          ::  sponsor: ~ means no explicit sponsor (route to the @p-derived
          ::  default); `who means route to who (a ship may sponsor itself).
          ::  issue #117 -- was [has=? who=@p].
          sponsor=(unit @p)
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
