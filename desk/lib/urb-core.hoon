::  %urb-core
::
::  This is where most of the heavy block processing in %urb-watcher happens.
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
::  1. %urb-watcher receives a block from RPC and then
::     calls ++find-block-reveals, which filters it
::     down to txs containing urb reveals.
::  2. %urb-watcher asynchronously fetches the prevout 
::     values for each tx in the filtered block.
::  3. %urb-watcher checks each tx for %spawn sotx.
::     If it finds one, it fetches a commit and
::     precommit transaction. See the %spawn case
::     down below for extensive detail on how and why
::     we do this.
::  4. %urb-watcher calls ++apply-prevouts-and-urbify
::     on the block. This converts it to an urb-block.
::  5. %urb-watcher calls ++handle-block on the
::     urb-block, which processes its txs for sotx and
::     returns an updated state and a list of fx.
::  6. %urb-watcher turns these fx into udiffs and
::     gives them to Jael.
::
/-  bitcoin, ord, urb
/+  bscr=btc-script, ol=ord, urb-encoder
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
    ::
    :: XX in order to properly fulfill the coinbase transaction, we need to
    :: keep track of the fee change from skipped tx's
    |=  =block:bitcoin
    =|  reveals=(map [txid:ord vout:ord] [sots=(list raw-sotx:urb) value=(unit @ud)])
    ^+  [reveals block]
    :: XX num isn't in urb-block type yet, see other num.block comment
    :: ?.  =(num.block +(num.block-id.state))
    ::   %-  (slog leaf+"can't handle block {<num:block>}, expected block {<+(num.block-id.state)>}" ~)
    ::   [reveals block]  ::  XX crash instead?
    ::
    ::  Set aside this block's coinbase transaction.
    ?~  txs.block
      ~&  >>>  ["%urb-core: This block has no transactions:" block]  !!
    =/  cb-tx  i.txs.block
    =/  txs    t.txs.block
    ::
    ::  Loop through this block's transactions
    ::  and filter down to tx containing reveals.
    =|  saved-txs=(list tx:bitcoin)
    |-  
    ^+  [reveals block]
    ?~  txs
      :-  reveals
      block(txs [cb-tx (flop saved-txs)])
    ::
    ::  Loop through this transaction's inputs
    ::  and, if any contain sots, save the whole tx.
    =/  is  is.i.txs  :: list of inputs
    =|  need-tx=_|    :: whether we need to save this tx
    |^  
    ^+  ^$
    ?~  is
      %=  ^$
        txs        t.txs
        saved-txs  ?.  need-tx
                     saved-txs
                   [i.txs saved-txs]
      ==
    ::
    ::  If this input had been saved as an output in our state,
    ::  then that means it was relevant to urb, and
    ::  we for sure need to save this tx to track where
    ::  all the sats end up.
    =/  value=(unit @ud)
      =/  vout  (get-vout:si:ol sont-map [txid pos]:i.is)
      ?~(vout ~ `value.u.vout)
    =.  need-tx  |(need-tx ?=(^ value))
    ::
    ::  Similarly, if this input came from a transaction we've
    ::  already saved in this block, we save this tx too.
    =.  need-tx
      ?|  need-tx
          %+  lien
            saved-txs
          |=  =tx:bitcoin
          =(id.tx txid.i.is)
      ==
    ::
    ::  Parse the witness for sots. If no sots, just 
    ::  preserve our potential saved value and recurse.
    =/  raw-script=(unit octs)
      =/  rwit  (flop witness.i.is)
      ?.  ?=([* ^] rwit)  ~
      ?.  =+  i.rwit
          &(!=(0 wid) =(0x50 (cut 3 [(dec wid) 1] dat)))
        `i.t.rwit
      ?~  t.t.rwit  ~
      `i.t.t.rwit
    ?~  raw-script
      (add-to-reveals ~ value)
    =/  descr  (de:bscr u.raw-script)
    ?~  descr
      (add-to-reveals ~ value)
    ~|  [=+(u.raw-script [p `@ux`q]) =+((en:bscr u.descr) [p `@ux`q])]
    ?.  =(u.raw-script (en:bscr u.descr)) 
      ~&  >>>  "%urb-core: If you see this, there's a bug in the witness parsing logic."  !!
    =/  unvs=(unit (list @))  (some (unv:de:urb-encoder u.descr))
    ?~  unvs  (add-to-reveals ~ value)
    ::
    ::  If there is sots, get it, add it to reveals, 
    ::  flag this tx as needed, and recurse.
    ::  ~&  >>  unvs
    =/  sots=(list raw-sotx:urb)
      (zing (turn u.unvs parse-roll:urb-encoder))
    ::  ~&  >>  sots
    (add-to-reveals(need-tx &) sots value)
    ::
    ::  ^$ recurses to the inputs loop.
    ++  add-to-reveals
      |=  [sots=(list raw-sotx:urb) value=(unit @ud)]
      ^+  ^$
      ?>  ?=(^ is)
      ?:  ?&  =(~ sots)
              =(~ value)
          ==
        ^$(is t.is)
      %=  ^$
        is  t.is
        reveals  (~(put by reveals) [txid pos]:i.is sots value)
      ==
    --
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
    |=  $:  =urb-block:urb
            precommits=(map [txid:ord vout:ord] [commit=urb-tx:urb precommit=urb-tx:urb])
        ==
    ^+  cor
    :: XX num is actually not included in the urb-block type
    :: ?.  =(num.urb-block +(num.block-id.state))
    ::   %-  (slog leaf+"can't handle block {<num:block>}, expected block {<+(num.block-id.state)>}" ~)
    ::   cor  ::  XX crash instead?
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
    =.  cor  (handle-tx i.txs.urb-block precommits)
    $(txs.urb-block t.txs.urb-block)
  ::
  ++  handle-tx
    =|  running-value=@ud
    |=  $:  tx=urb-tx:urb
            precommits=(map [txid:ord vout:ord] [commit=urb-tx:urb precommit=urb-tx:urb])
        ==
    ^+  cor
    =/  sum-out  (roll os.tx |=([[* a=@] b=@] (add a b)))
    =/  sum-in  (roll is.tx |=([a=input:urb-tx:urb b=@] (add value.a b)))
    =/  inputs  is.tx
    ?~  inputs  cor
    |^  
    ^+  cor
    =.  cor  process-unv
    ::  =.  cor  check-for-insc
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
    ++  process-unv
      ^+  cor
      =/  sots  sots.i.inputs
      |-  
      ?~  sots
        cor
      ::  ~&  >>  "%urb-core: processing sots"
      =*  raw  raw.i.sots
      =*  who  ship.sot.i.sots
      ::  =*  sig   sig.sot.i.sots :: XX check networking key signature?
      =-  $.+(cor -, sots t.sots)
      =/  sots=(list single:skim-sotx:urb)
        ?:  ?=(%batch +<.sot.i.sots) 
          bat.sot.i.sots 
        ~[+.sot.i.sots]
      |^  
      ^+  cor
      =/  point  (~(get by unv-ids) who)
      =|  bat-cnt=@
      =.  bat-cnt  +(bat-cnt)
      ?~  sots  cor
      =*  sot  i.sots
      ::  XX more ordering constraints?
      ?:  ?=(%spawn -.sot)
        ::  ~&  >>  "%urb-core: processing %spawn"
        ?.  =(1 bat-cnt)  cor                   :: first sot in batch
        ?^  point  cor                          :: no data for @p yet
        ?:  (~(has by unv-ids) who)  cor        :: no data for @p yet
        =/  cac  (com:nu:cric:crypto pass.sot)
        ?.  ?=(%c suite.+<.cac)  cor            :: uses suite c encoded pass
        ?.  =(who fig:ex:cac)  cor              :: initial comet @p = hash of public key
        ::
        ::  A Groundwire user must choose the sat they want to own their comet
        ::  prior to boot-time and pass in its satpoint to Vere on first boot.
        ::  However, within their commit attestation, they must include their
        ::  ship's networking key, which is only knowable after boot.
        ::  Because of this, we have an additional "pre-commit" transaction
        ::  in addition to the typical ordinal protocol. A client will:
        ::
        ::  1. Pre-commit to a sat inside a precommit transaction.
        ::  2. Boot their comet using a satpoint within the precommit transaction.
        ::  3. Submit the commit transaction containing their precommit satpoint and ship networking key.
        ::  4. Submit the reveal transaction.
        ::
        ::  When processing a %spawn sotx then, rather than just checking
        ::  whether the sont is in this input, we need to check if it carried
        ::  through from *two* transactions back.
        ::
        ::  The %spawn sotx includes a supposed satpoint and spkh from 
        ::  the precommit transaction: [precommit-spkh vout off] 
        ::  (We know the txid by virtue of the %spawn being associated with this input.)
        ::  We call ++calc-precommit-sont to determine whether this
        ::  sotx-attested satpoint does indeed exist given the
        ::  precommit transaction's outputs. If so, we'll grab this sat,
        ::  and then check that it's also equal to the tweak of this
        ::  comet's networking key.
        ::
        =/  pcmtx  (~(get by precommits) [txid.i.inputs pos.i.inputs])
        ?~  pcmtx
          ~&  >>  "%urb-core: Couldn't find precommit tx."  cor
        =/  precommit-tx  precommit.u.pcmtx
        =/  commit-tx  commit.u.pcmtx
        ?~  precommit-sat=(calc-precommit-sont precommit-tx to.sot)  
          cor
        =/  tweak
          %+  rap 
            3 
          :~  %btc 
              %gw 
              txid=txid.u.precommit-sat 
              vout=vout.u.precommit-sat 
              off=off.u.precommit-sat
          ==
        ::  ~&  >>  ["%urb-core: based on the precommit sat we found, we're expecting this tweak data: " %btc %gw [txid vout off]:u.precommit-sat]
        ::  ~&  >>  ["%urb-core: this tweak data as an atom is: " tweak]
        ::
        ::  Check that the given comet networking key encodes the tweak 
        ::  that corresponds to the attestation.
        ?.  =(dat.tw.pub:+<:cac tweak)
          ~&  >>>  ["%urb-core: provided pass's networking key does not match tweak: " dat.tw.pub:+<:cac]
          cor
        ::  ~&  >>  "%urb-core: provided pass encodes the correct tweaked networking key!"
        ::
        ::  We now know that:
        ::  - the attested satpoint exists
        ::  - the attested satpoint is encoded in the attested comet's networking key
        ::  - the attested satpoint was in an input to the commit transaction
        ::    (because we found and verified the satpoint by fetching the commit
        ::    transaction's input), and therefore the precommit and commit transactions
        ::    share a controller
        ::
        ::  This proves ownership. All that's left is to find where the sat ultimately
        ::  ended up so we can track it appropriately. We thus transition 
        ::  the sat to [commit-txid vout off] to see where it landed afterwards.
        ::
        =/  commit-sat=(unit sont:ord)
          (apply-tx-to-sont commit-tx u.precommit-sat)
        ::  ~&  >>  commit-sat
        ?~  commit-sat
          cor
        ::
        ::  We check to make sure that the sat was indeed spent in the reveal transaction.
        ::  You could argue that this isn't strictly necessary here, since
        ::  we know that the creator of the commit tx controlled the sat at that time,
        ::  so this is a valid reveal regardless and ++update-sonts will still track it 
        ::  correctly, but in all other cases our security model is to check
        ::  (is-sont-in-input sont.own.u.point), so we enforce that here as well.
        ::
        ?.  (is-sont-in-input u.commit-sat)
          ~&  >>>  'The commit sat did not get spent in the reveal tx. Rejecting.' 
          cor
        ::
        ::  Now that we know where the sat ended up after the commit tx,
        ::  we can provisionally update sont-map and unv-ids
        ::  with everything we know so far and call ++update-sonts, which will
        ::  read the satpoint from sont-map, transition the satpoint to
        ::  [txid.reveal-tx vout off] automatically since we're currently
        ::  processing the reveal transaction and its commit input, and update
        ::  sont-map and unv-ids appropriately.
        ::
        ::  (When the ++update-sonts call happens in the outer loop
        ::  after ++process-unv finishes, it will simply do nothing
        ::  because the get:by check for this input will return null, as we've
        ::  already processed it and moved the comet correctly.)
        ::
        =.  sont-map  
          %:  put-com:si:ol 
              sont-map 
              txid.u.commit-sat 
              vout.u.commit-sat  
              off.u.commit-sat  
              value.i.inputs :: value of this input to the reveal tx, aka the commit utxo
              who
          ==
        =/  =point:urb
          :*  own=[u.commit-sat ~]
              rift=0
              life=1
              pass=pass.sot
              sponsor=[| who] :: no sponsor on spawn
              escape=~
              fief=fief.sot
          ==
        =.  unv-ids  (~(put by unv-ids) who point)
        ::  ~&  >>  "%urb-core: final step"
        =.  cor  update-sonts
        =/  reveal-sat  sont:own:(~(got by unv-ids) who)
        ~&  >  ["%urb-watcher found comet: " who]
        =.  cor
          %-  emil
          :~  [%point who %owner reveal-sat]
              [%point who %sponsor ~]
              [%point who %keys 1 pass.sot]
              [%point who %fief fief.sot]
          ==
        $(sots t.sots)
      ::
      ::  =^  point  cor  (spend-point point)
      ?~  point  cor
      ?.  (is-sont-in-input sont.own.u.point)
        ~&  >>>  'Input to this tx did not include the owner sont. Rejecting.' 
        cor
      ?-    -.sot
          %set-mang
        !!
        ::=.  cor  (emit [%point who %mang mang.sot])
        ::%_    $
        ::    sots     t.sots
        ::    unv-ids   (~(put by unv-ids) who u.point)
        ::==
      ::
          %fief
        =.  fief.net.u.point  fief.sot
        =.  cor  (emit [%point who %fief fief.sot])
        %_    $
            sots     t.sots
            unv-ids   (~(put by unv-ids) who u.point)
        ==
      ::
          %escape
        ::  sponsoring self, update now
        ?:  =(parent.sot who)
          =.  sponsor.net.u.point  &/who
          =.  escape.net.u.point   ~
          =.  cor  (emit [%point who %sponsor `who])
          %_    $
              sots     t.sots
              unv-ids   (~(put by unv-ids) who u.point)
          ==
        ::  sponsor already signed the request off-chain, update now
        ::  LLM: verify escape sig against sponsor's stored pass.
        ::  Message is (shaz (jam [sponsee height])), with a 10 block buffer
        ?^  sig.sot
          =/  sponsor  (~(get by unv-ids) parent.sot)
          ?~  sponsor  cor
          =/  cac  (com:nu:cric:crypto pass.net.u.sponsor)
          =/  lower-bound
            ?:  (lth num.block-id.state 10) 
              0
            (sub num.block-id.state 10)
          ?.  %+  lien
                (gulf lower-bound (add num.block-id.state 1))
              |=(h=@ud (veri-octs:ed:crypto u.sig.sot 512^(shaz (jam [who h])) sgn:ded:ex:cac))
            ~&  >>>  "%urb-core: sponsor's signature is bad"
            cor
          =.  sponsor.net.u.point  &/parent.sot
          =.  escape.net.u.point   ~
          =.  cor  (emit [%point who %sponsor `parent.sot])
          %_    $
              sots     t.sots
              unv-ids   (~(put by unv-ids) who u.point)
          ==
        ::  no signature, flag sponsorship as pending
        =.  escape.net.u.point  `parent.sot
        =.  cor  (emit [%point who %escape `parent.sot])
        %_    $
            sots     t.sots
            unv-ids   (~(put by unv-ids) who u.point)
        ==
      ::
          %cancel-escape
        ?.  =([~ parent.sot] escape.net.u.point)  cor ::$(sots t.sots)
        =.  escape.net.u.point  ~
        =.  cor  (emit [%point who %escape ~])
        %_    $
            sots     t.sots
            unv-ids   (~(put by unv-ids) who u.point)
        ==
      ::
          %detach
        ?~  child=(~(get by unv-ids) ship.sot)  cor ::$(sots t.sots)
        ?.  =([& who] sponsor.net.u.child)  cor ::$(sots t.sots)
        =.  sponsor.net.u.child  |/who
        =.  cor  (emit [%point ship.sot %sponsor ~])
        %_    $
            sots     t.sots
            unv-ids   (~(put by unv-ids) ship.sot u.child)
        ==
      ::
      ::  XX It would be nice to have an escapee's pending sponsor
      ::     sign their %escape transaction out-of-band with their networking key
      ::     so that they can auto-accept it when the %escape comes in.
          %adopt
        ?:  =(ship.sot who)
          =.  sponsor.net.u.point  &/who
          =.  escape.net.u.point   ~
          =.  cor  (emit [%point ship.sot %sponsor `who])
          %_    $
              sots     t.sots
              unv-ids   (~(put by unv-ids) who u.point)
          ==
        ?~  child=(~(get by unv-ids) ship.sot)  cor ::$(sots t.sots)
        ?.  =([~ who] escape.net.u.child)  cor ::$(sots t.sots)
        =.  escape.net.u.child  ~
        =.  sponsor.net.u.child  &/who
        =.  cor  (emit [%point ship.sot %sponsor `who])
        %_    $
            sots     t.sots
            unv-ids   (~(put by unv-ids) ship.sot u.child)
        ==
      ::
          %reject
        ?~  child=(~(get by unv-ids) ship.sot)  cor ::$(sots t.sots)
        ?.  =([~ who] escape.net.u.child)  cor ::$(sots t.sots)
        =.  escape.net.u.child  ~
        =.  cor  (emit [%point ship.sot %escape ~])
        %_    $
            sots     t.sots
            unv-ids   (~(put by unv-ids) ship.sot u.child)
        ==
      ::
          %keys
        ::=/  cac  (com:nu:cric:crypto pass.sot)
        ::?~  sig                  cor
        ::?.  ?=(%c suite.+<.cac)  cor
        ::?.  =(dat.tw.pub:+<:cac (rap 3 ~[+(life.net.u.point) %btc %ord %gw %test]))  cor
        ::?.  (veri-octs:ed:crypto u.sig 512^(shal raw.i.^sots) sgn:ded:ex:cac)
        ::  cor
        =.  net.u.point
          net.u.point(pass pass.sot, life +(life.net.u.point))
        =?  rift.net.u.point  breach.sot  +(rift.net.u.point)
        =.  cor  %-  emil
          :*  [%point who %keys life.net.u.point pass.sot]
              ?.  breach.sot  ~
              [%point who %rift rift.net.u.point]^~
          ==
        %_    $
            sots     t.sots
            unv-ids   (~(put by unv-ids) who u.point)
        ==
      ==
      ::
      ::  Is this sont in the input that's being processed? 
      ++  is-sont-in-input
        |=  sot=sont:ord
        ~|  [s=sot [txid pos value]:i.inputs]
        ?.  =([txid vout]:sot [txid pos]:i.inputs)  |
        ~|  %fatal-tracking-error
        ?>  (lth off.sot value.i.inputs)  &
      ::
      ::  Given a precommit tx and a [vout off tej],
      ::  check if the implied satpoint [txid vout off]
      ::  is a valid landing location within the tx outputs
      ::  and that off+tej doesn't exceed that output's value.
      ::  Additionally, check that the scriptPubkey hash of the
      ::  landing output matches the given spkh.
      ::  If both are true, return the implied satpoint,
      ::  otherwise return null.
      ++  calc-precommit-sont
        |=  $:  precommit=urb-tx:urb
                out=[spkh=@ux pos=(unit vout:ord) =off:ord tej=off:ord]
            ==
        ^-  (unit sont:ord)
        =|  out-pos=@ud
        =|  out-val=@ud
        =/  in-val
          (roll is.precommit |=([a=input:urb-tx:urb b=@] (add value.a b)))
        =/  outputs  os.precommit
        |-  
        ^-  (unit sont:ord)
        ?~  outputs  ~
        ::  Null pos.out is undefined behavior for now, fail
        ?~  pos.out
          ~
        ::  If we passed vout, fail
        ?:  (lth u.pos.out out-pos)  
          ~
        ::  If satpoint would exceed total available input value, fail
        ?:  (gth :(add out-val off.out tej.out) in-val)
          ~
        ::  If this isn't the correct output index, loop
        ?:  !=(out-pos u.pos.out)
          $(out-val (add out-val value.i.outputs), outputs t.outputs, out-pos +(out-pos))
        ::  Last check: this is the correct output index, but is it big enough?
        ?:  (gth (add off:out tej:out) value.i.outputs)
          ~
        ::  The satpoint is legit, we build it
        =/  sat=sont:ord  [id.precommit u.pos.out off.out]
        ::  Rebuild the hash of this output and check if it matches the given hash
        =/  en-out  (can 3 script-pubkey.i.outputs 8^value.i.outputs ~)
        =/  hax-out  (shay (add 8 wid.script-pubkey.i.outputs) en-out)
        ?:  =(hax-out spkh.out)  
          `sat
        ~
      ::
      ::  Experimental arm for allowing both the sat owner and the networking 
      ::  key controller to make attestations.
      ::  ++  spend-point
      ::    |=  point=(unit ^point)
      ::    ^+  [point cor]
      ::    ?~  point  [~ cor]
      ::    ?:  ?&  ?=(~ sig) 
      ::            (is-sont-in-input sont.own.u.point)
      ::        ==
      ::      [point cor]
      ::    ?~  sig  [~ cor]
      ::    XX rethink
      ::    ?.  ?=([~ %pass *] mang.own.u.point)  [~ cor]
      ::    ?:  =(txid (cut 8 [1 1] pass.u.mang.own.u.point))  [~ cor]
      ::    =/  pub  (end 8 pass.u.mang.own.u.point)
      ::    =/  tw  (scap:ed:crypto pub (shax:sha pass.u.mang.own.u.point))
      ::    ?.  (veri-octs:ed:crypto u.sig raw tw)  [~ cor]
      ::    =.  pass.u.mang.own.u.point  (can 8 [1 pub] [1 txid] ~)
      ::    [point cor(unv-ids (~(put by unv-ids) who u.point))]
      --
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
    ::  Given a transaction and a satpoint that refers to one of its inputs,
    ::  compute where that same sat ends up in this tx’s outputs.
    ::  XX  This arm is LLM-generated and needs to be vetted.
    ::      Also reason about how the caller should interpret a null returns.
    ++  apply-tx-to-sont
      |=  [tx=urb-tx:urb sot=sont:ord]
      ^-  (unit sont:ord)
      =/  inputs  is.tx
      =|  in-sum=@ud
      |-  
      ^-  (unit sont:ord)
      ?~  inputs
        ~
      =/  inp  i.inputs
      ::  Is this the input spending the sat’s prevout?
      ?:  =([txid vout]:sot [txid pos]:inp)
        ::  Off must be within that prevout’s value.
        ?.  (lth off.sot value.inp)
          ~
        =/  index=@ud
          (add in-sum off.sot)
        =/  out  (index-to-sont index os.tx)
        ?~  out
          ~
        ::  Landed in output vout.out at offset off.out (relative to that output).
        `[[id.tx vout.out off.out]]
      ::  Otherwise keep scanning; add this input’s value to the running sum.
      $(inputs t.inputs, in-sum (add in-sum value.inp))
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
