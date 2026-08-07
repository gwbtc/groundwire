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
    ::  Process this transaction's OP_RETURN publication, if any.  A
    ::  public %gw-btc comet reveals a $publication in a deliberate
    ::  OP_RETURN output: the pass binds the name (who = fig(pass)) and
    ::  the opening reveals the state committed in the sat-carrying
    ::  output.  A present blind-opening is a spawn; an absent one is a
    ::  state update (rekey/breach) of an already-tracked comet.  All
    ::  other transactions are pure custody moves handled by
    ::  ++update-sonts.  Confidential comets never publish, so they
    ::  never appear here.
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
      ::  A comet we ALREADY track publishes a STATE UPDATE, whatever the
      ::  shape of its opening.  We track its custody position, so input-0
      ::  continuity from that position is the ownership proof and a
      ::  blind-opening adds nothing -- but if one is present we check it
      ::  against dat anyway, because a mismatch means this publication is
      ::  not that comet's.
      ::
      ::  This is the confidential -> public transition (decisions addendum
      ::  section 2, "Self-rescue").  A comet we know only because its
      ::  self-attestation VERIFIED is in unv-ids exactly like a public one;
      ::  the difference lives in %gw-btc's .confidential set, not here.  So
      ::  the owner spending the sat we already follow, and revealing the
      ::  state it lands in, is a complete proof to us -- and only the
      ::  holder of that sat can build the transaction, which makes it the
      ::  owner's own consent to declassify.  ++index-point emits %public
      ::  so the agent can move the ship out of .confidential and start
      ::  handing jael its udiffs.
      ::
      ?^  (~(get by unv-ids) who)
        ?:  ?&  ?=(^ blind-opening.op)
                !=(d.u.meta (spawn-commit:cc [spawn blind]:u.blind-opening.op))
            ==
          ~&  >>>  ['%urb-core: publication blind-opening does not open dat' who]
          cor
        (apply-state who pass op)
      ?~  blind-opening.op
        ::  A state update for a comet NOBODY here has ever indexed.  We
        ::  cannot judge it and must not guess: the opening proves what
        ::  state this transaction commits, but nothing binds the name to
        ::  the sat the transaction spends.  That binding lives in the
        ::  pass's hiding dat commitment and is opened only by a
        ::  blind-opening -- and even then, a blind-opening names the SPAWN
        ::  satpoint, so a stranger still has to walk the custody chain
        ::  from there to here.  See the decisions addendum section 2b.
        ::
        ~&  >>>  ['%urb-core: state-update publication for a comet we do not track' who]
        cor
      (apply-spawn who pass d.u.meta op u.blind-opening.op)
    ::
    ::  Find the first OP_RETURN "urb" publication among a tx's outputs.
    ++  find-publication
      |=  outs=(list output:tx:bitcoin)
      ^-  (unit publication:sa)
      ?~  outs  ~
      ?^  p=(read-publication:cc script-pubkey.i.outs)  p
      $(outs t.outs)
    ::
    ::  The x-only key of a P2TR (OP_1 PUSH32) output script, else ~.
    ++  p2tr-xonly
      |=  spk=hexb:bitcoin
      ^-  (unit @ux)
      ?.  =(34 wid.spk)  ~
      ?.  =(0x5120 (rsh [3 32] dat.spk))  ~
      `(end [3 32] dat.spk)
    ::
    ::  A public spawn: one transaction whose input 0 spends the comet's
    ::  chosen (funding) satpoint and whose sat-carrying output commits
    ::  the initial snapshot.  Verify the name<->pass<->dat binding, the
    ::  funding spend, the on-chain state commitment, and that no other
    ::  comet already holds the landing sat; then index the point.
    ++  apply-spawn
      |=  [who=ship =pass d=@ux op=opening:sa bo=blind-opening:sa]
      ^+  cor
      ::  Defensive: ++process-publication routes an already-tracked comet
      ::  to ++apply-state before it gets here, so reaching this means the
      ::  two disagree.
      ::
      ?^  (~(get by unv-ids) who)
        ~&  >>>  ['%urb-core: spawn publication for a comet already indexed' who]
        cor
      ?.  =(d (spawn-commit:cc spawn.bo blind.bo))
        ~&  >>>  ['%urb-core: spawn blind-opening does not open dat' who]
        cor
      ?.  =([txid vout]:spawn.bo [txid pos]:i.inputs)
        ~&  >>>  ['%urb-core: spawn does not spend its funding satpoint' who]
        cor
      ?~  landed=(index-to-sont-with-coinbase off.spawn.bo)
        ~&  >>>  ['%urb-core: spawn sat did not land in an output' who]
        cor
      =/  sont  u.landed
      =/  out  (snag vout.sont os.tx)
      ?.  =(`(state-key:cc internal-key.op snapshot.op) (p2tr-xonly script-pubkey.out))
        ~&  >>>  "%urb-core: spawn state commitment mismatch"  cor
      ?.  (can-put-com:si:ol sont-map txid.sont vout.sont off.sont who)
        ~&  >>>  ['%urb-core: spawn sat already occupied' sont]  cor
      ~&  >  ["%gw-btc found public comet: " who]
      (index-point who pass snapshot.op sont value.out %.y)
    ::
    ::  A public state update: the comet spends its tracked sat through
    ::  input 0, committing a new snapshot in the sat-carrying output.
    ::  life must advance.  ++update-sonts relocates sont.own; here we
    ::  only refresh the networking fields.
    ++  apply-state
      |=  [who=ship =pass op=opening:sa]
      ^+  cor
      ?~  pt=(~(get by unv-ids) who)
        ~&  >>>  ['%urb-core: state-update publication for an unindexed comet' who]
        cor
      =/  cur  sont.own.u.pt
      ?.  =([txid vout]:cur [txid pos]:i.inputs)
        ~&  >>>  ['%urb-core: state update does not spend our tracked tip' who cur]
        cor
      ?~  landed=(index-to-sont-with-coinbase off.cur)
        ~&  >>>  ['%urb-core: state-update sat did not land in an output' who]
        cor
      =/  sont  u.landed
      =/  out  (snag vout.sont os.tx)
      ?.  =(`(state-key:cc internal-key.op snapshot.op) (p2tr-xonly script-pubkey.out))
        ~&  >>>  "%urb-core: state commitment mismatch"  cor
      ?.  (gth life.snapshot.op life.net.u.pt)
        ~&  >>>
        :*  '%urb-core: state update does not advance life'
            who  published=life.snapshot.op  held=life.net.u.pt
        ==
        cor
      (index-point who pass snapshot.op cur value.out %.n)
    ::
    ::  Write a point from a snapshot and emit the jael udiffs.  On a
    ::  spawn we seed sont-map at the landing and emit %owner; on a
    ::  state update we leave sont.own for ++update-sonts to relocate
    ::  and only change the net fields.
    ++  index-point
      |=  [who=ship =pass snap=snapshot:sa =sont:ord out-value=@ud spawn=?]
      ^+  cor
      =/  spo=[has=? who=@p]  ?~(sponsor.snap [| who] [& u.sponsor.snap])
      =/  old  (~(get by unv-ids) who)
      =/  =point:urb
        ?:  |(spawn ?=(~ old))
          [[sont ~] rift.snap life.snap pass spo ~ fief.snap]
        %=  u.old
          pass.net     pass
          life.net     life.snap
          rift.net     rift.snap
          sponsor.net  spo
          fief.net     fief.snap
        ==
      =?  sont-map  spawn
        (put-com:si:ol sont-map txid.sont vout.sont off.sont out-value who)
      =.  unv-ids  (~(put by unv-ids) who point)
      %-  emil
      %+  weld
        ^-  (list effect:urb)
        ?.  spawn  ~
        ~[[%point who %owner sont]]
      ^-  (list effect:urb)
      :~  [%point who %public ~]
          [%point who %sponsor ?~(sponsor.snap `who `u.sponsor.snap)]
          [%point who %keys life.snap pass]
          [%point who %rift rift.snap]
          [%point who %fief fief.snap]
      ==
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
