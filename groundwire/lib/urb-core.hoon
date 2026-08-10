::  %urb-core
::
::  This is where most of the heavy block processing in %gw-btc happens.
::  Before engaging with this codebase, make sure that you understand
::  Taproot script-path spends and ordinal inscriptions.
::  See sur/urb and lib/urb-encoder for more details on the types at play here.
::  The main ones to be aware of are:
::  - sont, a satpoint
::  - sotx, a comet attestation
::  - a list of sotx is often called a "sots." be warned that
::    this same name can appear on multiple parsing layers.
::
::  The state of urb-core is an index 
::  of urb-relevant transactions with their 
::  associated prevouts and inscriptions.
::
::  The logic flow here is:
::  1. %gw-btc receives a block from RPC and then
::     calls ++find-block-reveals, which filters it
::     down to txs containing urb reveals.
::  2. %gw-btc asynchronously fetches the prevout
::     values for each tx in the filtered block.
::  3. %gw-btc checks each tx for %spawn sotx.
::     If it finds one, it fetches a commit and
::     precommit transaction. See the %spawn case
::     down below for extensive detail on how and why
::     we do this.
::  4. %gw-btc calls ++apply-prevouts-and-urbify
::     on the block. This converts it to an urb-block.
::  5. %gw-btc calls ++handle-block on the
::     urb-block, which processes its txs for sotx and
::     returns an updated state and a list of fx.
::  6. %gw-btc turns these fx into udiffs and
::     gives them to Jael.
::
/-  bitcoin, ord, urb, sa=self-attestation
/+  cc=gw-btc-pass, ol=ord
|%
++  urb-core
  =|  state:urb
  =*  state  -
  |_  $+  urb-core-sample
      $:  ::
          :: cards=(list card:agent:gall)
          fx=(list [id:block:bitcoin effect:urb])
          cb-tx=[value=@ud urb-tx:urb]
          ::n-map=_n-map
      ==
  +*  cor  .
  ++  abed
    |=  =state:urb
    ^+  cor
    cor(state state)
  ::
  ++  emit
    |=  fc=effect:urb
    ^+  cor
    cor(fx :-([block-id fc] fx))
  ::
  ++  emil
    |=  fy=(list effect:urb)
    ^+  cor
    ?~  fy  cor
    =.  cor  (emit i.fy)
    $(fy t.fy)
  ::
  ++  abet
    ^+  [fx state]
    [(flop fx) state]
  ::
  ::  Given a block, return its "reveals" (aka a map
  ::  of spent utxo to raw-sotx) and the block filtered
  ::  down to urb-relevant txs. A tx is relevant either
  ::  if we had saved one of its inputs previously,
  ::  or if its witness contains an urb reveal.
  ++  find-block-reveals
    ::  A transaction is relevant if it publishes a %gw-btc OP_RETURN
    ::  output, or spends a satpoint we are tracking, or spends an
    ::  output of a tx already saved in this block.  For each saved tx
    ::  we record every input's prevout value from our sont index (0
    ::  when untracked) so ++handle-tx sees the whole input list.
    ::  Witnesses are no longer parsed: public identity is published in
    ::  outputs (OP_RETURN), and confidential identity never on-chain.
    ::
    :: XX the coinbase fee is understated for a spawn's untracked
    :: funding inputs (recorded as value 0); this only affects the
    :: land-in-fee edge of ++index-to-sont-with-coinbase.
    |=  =block:bitcoin
    =|  reveals=(map [txid:ord vout:ord] [sots=(list raw-sotx:urb) value=(unit @ud)])
    ^+  [reveals block]
    ?~  txs.block
      ~&  >>>  ["%urb-core: This block has no transactions:" block]  !!
    =/  cb-tx  i.txs.block
    =/  txs    t.txs.block
    =|  saved-txs=(list tx:bitcoin)
    |-
    ^+  [reveals block]
    ?~  txs
      :-  reveals
      block(txs [cb-tx (flop saved-txs)])
    =/  this  i.txs
    =/  spends-tracked
      %+  lien  is.this
      |=  inp=inputw:tx:bitcoin
      ?=(^ (get-vout:si:ol sont-map [txid pos]:inp))
    =/  spends-saved
      %+  lien  is.this
      |=  inp=inputw:tx:bitcoin
      (lien saved-txs |=(t=tx:bitcoin =(id.t txid.inp)))
    =/  has-pub
      %+  lien  os.this
      |=  o=output:tx:bitcoin
      ?=(^ (read-publication:cc script-pubkey.o))
    ?.  ?|(has-pub spends-tracked spends-saved)
      $(txs t.txs)
    =.  reveals
      =/  ins  is.this
      |-  ^+  reveals
      ?~  ins  reveals
      =/  vout  (get-vout:si:ol sont-map [txid pos]:i.ins)
      =/  v=@ud  ?~(vout 0 value.u.vout)
      $(ins t.ins, reveals (~(put by reveals) [txid pos]:i.ins [~ `v]))
    $(txs t.txs, saved-txs [this saved-txs])
  ::
  ::  Fill in a block's txs with prevouts
  ::  given in a reveals map and restructure it
  ::  to an urb-block. Unlike a block:bitcoin, an
  ::  urb-block tracks prevout values within inputs,
  ::  because we aren't indexing every previous block.
  ++  apply-prevouts-and-urbify
    |=  $:  block:bitcoin 
            reveals=(map [txid:ord vout:ord] [sots=(list raw-sotx:urb) value=(unit @ud)])
        ==
    ^-  urb-block:urb
    =*  block  +<-
    =>  ?>(?=(^ txs) [cb-tx=i.txs .(txs t.txs)])
    =-  %=  block
          txs  ^-  (list urb-tx:urb)
               %+  welp
                 ^-  (list urb-tx:urb)
                 :~  ^-  urb-tx:urb
                     %=  cb-tx
                       is  %+  turn
                             is.cb-tx
                           |=  inputw:tx:bitcoin
                           ^-  input:urb-tx:urb
                           [[~ 0] +<]
                     ==
                 ==
               ^-  (list urb-tx:urb)
               -
        ==
    |-  
    ^-  (list tx:urb-tx:urb)
    ?~  txs  ~
    =/  is  is.i.txs
    =-  [i.txs(is -) $(txs t.txs)]
    |-  
    ^-  (list input:urb-tx:urb)
    ?~  is
      ~
    =/  rev
      (~(get by reveals) [txid pos]:i.is)
    ?~  rev
      ~
    :-  [u.rev(value (need value.u.rev)) i.is]
    $(is t.is)
  ::
  ::  Given an urb-block, update state and emit fx.
  ++  handle-block
    |=  =urb-block:urb
    ^+  cor
    =.  num.block-id.state  +(num.block-id.state)
    ?~  txs.urb-block
      cor  ::  XX crash instead?
    =>  %=  .
          txs.urb-block  t.txs.urb-block
          cb-tx         [reward.urb-block i.txs.urb-block]
        ==
    |-
    ^+  cor
    :: XX handle coinbase tx
    ?~  txs.urb-block
      cor
    =.  cor  (handle-tx i.txs.urb-block)
    $(txs.urb-block t.txs.urb-block)
  ::
  ++  handle-tx
    =|  running-value=@ud
    |=  tx=urb-tx:urb
    ^+  cor
    =/  sum-out  (roll os.tx |=([[* a=@] b=@] (add a b)))
    =/  sum-in  (roll is.tx |=([a=input:urb-tx:urb b=@] (add value.a b)))
    =/  inputs  is.tx
    ?~  inputs  cor
    |^
    ^+  cor
    ::  Process the OP_RETURN publication (if any) once, against input 0.
    ::  running-value is 0 on the first iteration, so a spawn's or state
    ::  update's sat offset math is anchored at input 0 as the protocol
    ::  requires.  Then follow every tracked sat through this input.
    =?  cor  =(0 running-value)  process-publication
    =.  cor  update-sonts
    ::  Excess inputs get added to coinbase fee,
    ::  which we calculate iteratively because we need
    ::  its per-input value for math in ++update-sonts.
    =<  ?~(t.inputs cor $(inputs t.inputs))
    =/  new-val  (add running-value value.i.inputs)
    ?:  (lth new-val sum-out)
      .(running-value new-val)
    %=  .
      running-value  sum-out
      value.cb-tx    (add value.cb-tx (sub new-val sum-out))
    ==
    ::
    ::  XX For all failures in this arm figure out when
    ::  to loop and when to quit. Do we ever need to rewind?
    ::  Process this transaction's OP_RETURN publication, if any.
    ::
    ::    A publication is the comet's FULL ATTESTATION PACKET (see
    ::    $publication in sur/self-attestation): the pass it would hand a
    ::    peer over ames, custody log and all, plus the opening for the
    ::    hop this very transaction performs.  So there is nothing here
    ::    to judge and nothing to infer.  This arm reads the envelope,
    ::    completes the log with the two facts only a block reader has --
    ::    this transaction's txid and this block's height -- and emits a
    ::    %claim.  %gw-btc hands that to the SAME +verify-lc a %jael-writ
    ::    gets.
    ::
    ::    WHAT USED TO BE HERE, AND WHY IT IS GONE.  This arm had three
    ::    branches -- ++apply-state for a comet we already tracked,
    ::    ++apply-spawn for a stranger that opened its blind, and a
    ::    refusal for a stranger that did not -- and between them they
    ::    reimplemented, weakly, most of ++run-checks: the dat opening,
    ::    input-0 continuity, the state-key output commitment, the sat
    ::    landing, the life gate, sat occupancy.  ++apply-spawn also
    ::    demanded that input 0 BE the spawn satpoint, which is exactly
    ::    what rejected a late reveal: a comet publishing later in life
    ::    spends a satpoint further along its chain, and that constraint
    ::    said the publication was not a spawn -- while the other branch
    ::    said it was not a state update either, because we had never
    ::    tracked the comet.  With the custody log present there is
    ::    nothing left to infer, so the whole three-way distinction, and
    ::    the second grammar it was written in, dissolve into one emit.
    ::
    ::    All other transactions are pure custody moves handled by
    ::    ++update-sonts.  Confidential comets never publish, so they
    ::    never appear here.
    ::
    ++  process-publication
      ^+  cor
      ::  The `?~ pub` filter is the hot one and stays silent: it runs on
      ::  every transaction in every block, and all but a handful carry no
      ::  Groundwire OP_RETURN at all.  Everything BELOW it has already
      ::  matched the OP_RETURN "urb" envelope at this protocol kelvin, so
      ::  reaching one of these is an operator who published on chain and
      ::  will otherwise never learn why their comet did not appear.
      ::
      ?~  pub=(find-publication os.tx)  cor
      =*  pass  pass.u.pub
      =*  op    opening.u.pub
      ?~  meta=(parse-pass:cc pass)
        ~&  >>>  '%urb-core: publication carries a pass that does not parse'
        cor
      ?.  =(domain:cc dom.u.meta)
        ~&  >>>  ['%urb-core: publication pass names another PKI domain' dom.u.meta]
        cor
      ?.  =(kelvin:cc kel.u.meta)
        ~&  >>>  ['%urb-core: publication pass is at another protocol kelvin' kel.u.meta]
        cor
      =/  cac  (com:nu:cric:crypto pass)
      ?.  ?=(%c suite.+<.cac)
        ~&  >>>  '%urb-core: publication pass is not suite-C, so it is not a comet'
        cor
      =/  who  `@p`fig:ex:cac
      ::  Decode the carried log.  An EMPTY xtr is the spawn case and is
      ::  written as a bare 0 (see +with-xtr), never as (jam ~); anything
      ::  else must be canonical, exactly as +from-xtr demands of a
      ::  mailed pass, so a re-encoding cannot quietly launder a
      ::  non-canonical tail into a valid-looking claim.
      ::
      =/  base=(unit custody-log:sa)
        ?:  =(0 xtr.u.meta)  `~
        =/  dec  (mole |.(;;(custody-log:sa (cue xtr.u.meta))))
        ?~  dec  ~
        ?.(=(xtr.u.meta (jam u.dec)) ~ dec)
      ?~  base
        ~&  >>>  ['%urb-core: publication pass carries an unreadable custody log' who]
        cor
      ::  The entry this transaction IS.  Its txid is why the publisher
      ::  could not include it: the OP_RETURN is inside the transaction
      ::  the entry names.
      ::
      =/  full=custody-log:sa
        (snoc u.base [`txid:ord`id.tx num.block-id `op])
      ?~  done=(with-xtr:cc pass (jam full))
        ~&  >>>  ['%urb-core: publication pass could not be re-encoded' who]
        cor
      ~&  >  ["%gw-btc: on-chain self-attestation published by" who]
      (emit [%claim who u.done])
    ::
    ::  Find the first OP_RETURN "urb" publication among a tx's outputs.
    ++  find-publication
      |=  outs=(list output:tx:bitcoin)
      ^-  (unit publication:sa)
      ?~  outs  ~
      ?^  p=(read-publication:cc script-pubkey.i.outs)  p
      $(outs t.outs)
    ::
    ::  Given the transaction input that's currently in
    ::  ++handle-tx's context, get every sont we're tracking
    ::  in sont-map within that input (typically one per
    ::  input) and:
    ::  - Update sont-map with new landing sonts
    ::  - Update insc-ids with new owner sont (mostly vestigial)
    ::  - Update unv-ids with new owner sont
    ::  - Emit %xfer event signalling point transfer to new owner sont
    ++  update-sonts
      ^+  cor
      ?~  input=(~(get by sont-map) [txid pos]:i.inputs)
        cor
      =.  sont-map  (~(del by sont-map) [txid pos]:i.inputs)
      =/  input-sonts  ~(tap by sats.u.input)
      |-  
      ^+  cor
      ?~  input-sonts  cor
      =/  old-sont=sont:ord  [txid.i.inputs pos.i.inputs p.i.input-sonts] 
      =/  new-sunt  
        %-  index-to-sont-with-coinbase 
        (add running-value p.i.input-sonts)
      =/  new-sont=sont:ord  
        ?~  new-sunt 
          [0x0 0 0] 
        u.new-sunt
      =.  state  (update-ids state q.i.input-sonts new-sont)
      =.  cor  (emit [%xfer old-sont new-sont])
      %_  $
        input-sonts    t.input-sonts
        sont-map  ?~  new-sunt
                    sont-map
                  =/  out-value
                    ?:  =(txid.new-sont id.tx)
                      value:(snag vout.new-sont os.tx)
                    value:(snag vout.new-sont os.cb-tx)
                  %-  put-all:si:ol
                  :*  sont-map
                      txid.new-sont
                      vout.new-sont
                      off.new-sont
                      out-value  :: XX This is an LLM fix for the incorrect output value from the og codebase. Verify this.
                      q.i.input-sonts
                  ==
      ==
    ::
    ::  A wrapper around ++index-to-sont which has
    ::  access to context from ++handle-tx. We use this
    ::  context to handle the case where a sont lands
    ::  in the mining fee, in which case we transfer
    ::  ownership to the miner.
    ::  If this returns null, then something weird
    ::  happened.
    ++  index-to-sont-with-coinbase
      |=  index=@ud
      ^-  (unit sont:ord)
      ?:  (lth index sum-out)
        ?~  sont=(index-to-sont index os.tx)  ~
        `[id.tx vout.sont off.sont]
      =/  sont  
        %-  index-to-sont 
        :-  (add value.cb-tx (sub index sum-out)) 
        os.cb-tx
      ?:  ?|  =(~ sont) 
              (lte sum-in index)
          ==
        ~
      ?>  ?=(^ sont)
      `[id.cb-tx vout.sont off.sont]
    ::
    ::  Arms for updating insc-ids and unv-ids.
    ++  update-ids
      |=  [=state:urb old=sont-val:ord =sont:ord]
      =.  state  (update-inscriptions state ins.old sont)
      ?~  com.old  state
      (update-comet state u.com.old sont)
    ::
    ++  update-inscriptions
      |=  [=state:urb oids=(set insc:ord) =sont:ord]
      ?:  =(~ oids)  state
      %-  ~(rep in oids)
      |:  [*=insc:ord state]
      =/  dat  (~(got by insc-ids) insc)
      state(insc-ids (~(put by insc-ids) insc dat(sont sont)))
    ::
    ++  update-comet
      |=  [=state:urb com=@p =sont:ord]
      =/  point  (~(got by unv-ids:state) com)
      state(unv-ids (~(put by unv-ids:state) com point(sont.own sont)))
    ::
    ::
    --
  --
::
::  Take a list of outputs and a sat index across
::  those outputs, and return the output index
::  and relative sat offset.
::  Returns null if the sat index is greater than
::  the total number of output sats (probably meaning
::  to the caller that it landed in the miner fee).
::  [When lib/ord.hoon is a little cleaner and more
::  useful, this can probably move there.]
++  index-to-sont
  =|  vout=@ud
  |=  [index=@ud outs=(list output:tx:bitcoin)]
  ^-  $@(~ [vout=@ud off=@ud])
  ?~  outs  
    ~
  ?:  (lth index value.i.outs)  
    [vout index]
  %=  $
    vout   +(vout) 
    index  (sub index value.i.outs)
    outs   t.outs
  ==
--
