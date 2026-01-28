::  "unv" is short for "urbit envelope."
::  an unv is an atom that comes from the body
::  of an ordinal-style Taproot script tagged 
::  with "urb" instead of "ord":
::
::  OP_PUSH 1 0
::  OP_IF
::    "urb"
::    <len, dat>
::  OP_ENDIF
::
::  An unv is variable-sized and 
::  gets parsed into a list of "raw-sotx".
::  an ordinal script can also include multiple
::  unvs, giving us a (list (list raw-sotx))
::  that we flatten.
::
::  A raw-sotx is a [raw=octs sotx].
::  we keep the raw data around even after parsing
::  because several proof steps depend on it.
::
::  A sotx is similar to a jael udiff, and
::  indeed gets turned into a jael udiff,
::  which is what %ord-watcher ultimately
::  uses this library for.
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
::  3. %urb-watcher calls ++apply-prevouts-and-urbify
::     on the block. This converts it to an urb-block.
::  4. %urb-watcher calls ++handle-block on the
::     urb-block, which processes its txs for sotx and
::     returns an updated state and a list of fx.
::  5. %urb-watcher turns these fx into udiffs and
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
    =/  is  is.i.txs.block  :: list of inputs
    =|  need-tx=_|          :: whether we need to save this tx
    |^  
    ^+  ^$
    ?~  is
      %=  ^$
        txs        t.txs
        saved-txs  ?.  need-tx
                     saved-txs
                   [i.txs.block saved-txs]
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
    =/  sots=(list raw-sotx:urb)
      (zing (turn u.unvs parse-roll:urb-encoder))
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
    |=  =urb-block:urb
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
      =*  raw  raw.i.sots
      ~&  [%sot -.sot.i.sots]
      =*  who  ship.sot.i.sots
      ::  =*  sig   sig.sot.i.sots :: XX check networking key signature?
      =-  $.+(cor -, sots t.sots)
      =/  sots=(list single:skim-sotx:urb)
        ?:  ?=(%batch +<.sot.i.sots) 
          bat.sot.i.sots 
        ~[+.sot.i.sots]
      =/  point  (~(get by unv-ids) who)
      =|  bat-cnt=@
      |^  
      ^+  cor
      =.  bat-cnt  +(bat-cnt)
      ?~  sots  cor
      =*  sot  i.sots
      ::  XX more ordering constraints?
      ?:  ?=(%spawn -.sot)
        ?.  =(1 bat-cnt)  cor                   :: first sot in batch
        ?^  point  cor                          :: no data for @p yet
        ?:  (~(has by unv-ids) who)  cor        :: no data for @p yet
        =/  cac  (com:nu:cryc:crypto pass.sot)
        ?.  ?=(%c suite.+<.cac)  cor            :: uses suite c encoded pass
        ?.  =(who fig:ex:cac)  cor              :: initial comet @p = hash of public key
        ::  The %spawn sotx includes a supposed satpoint from 
        ::  the precommit transaction: [precommit-spkh vout off]
        ::  We call ++calc-precommit-sont to determine whether this
        ::  sotx-attested satpoint is indeed valid given the
        ::  precommit transaction's outputs. If so, we'll grab this sat,
        ::  and then check that it's also equal to the tweak of this
        ::  comet's networking key.
        ::=/  precommit-tx  (~(get by precommits) [txid pos].i.inputs)
        ::  XX Next step is to actually pass in precommits
        =/  precommit-tx  *urb-tx:urb
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
        ?.  =(dat.tw.pub:+<:cac tweak)
          cor
        ::
        ::  We then transition the sat to [commit-txid vout off].
        ?.  ?&  =(txid.u.precommit-sat txid.i.inputs)
                =(vout.u.precommit-sat pos.i.inputs)
            ==
          cor
        ::  We now know that the attestation sat to.sot was a valid spend of the
        ::  precommit transaction AND that the output of that transaction which
        ::  the sat landed in was indeed an input to the commit transaction, and
        ::  we know which input it was.
        =/  commit-sat=sont:ord
            ::  XX Assume for now that the commit tx only has one input and one output.
            ::  (Just to get the code compiling and testable.)
            ::  So we map from [txid.precommit-sat vout.precommit-sat off.precommit-sat]
            ::              to [txid.commit-tx 0 off.precommit-sat]
            ::   which would also be [txid.i.inputs pos.i.inputs off.precommit-sat].
            ::  We need to remove this assumption to allow for batching and to
            ::  ensure that a malicious spender doesn't duplicate the sat by
            ::  sending it to the second output when we assume it's in the first.
            ::  This will require a similar loop to ++update-sonts and will require
            ::  %urb-watcher to pass in the commit transaction itself in precommits
            ::  so we can access all of its inputs and outputs.
            ::  (See ++calc-precommit-sont actually. In fact I can maybe just reuse it.)
            ::  (I think I just need a ++sont-to-sont arm that takes a sont and a urb-tx. 
            ::  LLM can probably write that.)
            [txid.i.inputs pos.i.inputs off.u.precommit-sat]
        ::
        ::  Now that we've validated the commit tx's movement
        ::  of the sat, we can provisionally update sont-map and unv-ids
        ::  with everything we know so far and call ++update-sonts, which will
        ::  read the satpoint from sont-map, transition the satpoint to
        ::  [txid.reveal-tx vout off] automatically since we're currently
        ::  processing the reveal transaction and its commit input, and update
        ::  sont-map and unv-ids appropriately.
        ::  (When the ++update-sonts call happens in the outer loop
        ::  after ++process-unv finishes, it will simply do nothing
        ::  because the get:by check for this input will return null.)
        =.  sont-map  
          %:  put-com:si:ol 
              sont-map 
              txid.commit-sat 
              vout.commit-sat  
              off.commit-sat  
              value.i.inputs :: value of this input to reveal tx
              who
          ==
        =/  sponsor  `@p`(end 4 who)
        =/  =point:urb
          :*  own=[commit-sat ~]
              rift=0
              life=1
              pass=pass.sot
              sponsor=[& sponsor]
              escape=~
              fief=~
          ==
        =.  unv-ids  (~(put by unv-ids) who point)
        =.  cor  update-sonts
        =/  reveal-sat  sont:own:(~(got by unv-ids) who)
        =.  cor
          %-  emil
          :~  [%point who %owner reveal-sat]
              [%point who %sponsor `sponsor]
              [%point who %keys 1 pass.sot]
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
        ?:  =(parent.sot who)
          =.  sponsor.net.u.point  &/who
          =.  escape.net.u.point   ~
          =.  cor  (emit [%point who %sponsor `who])
          %_    $
              sots     t.sots
              unv-ids   (~(put by unv-ids) who u.point)
          ==
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
        ::=/  cac  (com:nu:cryc:crypto pass.sot)
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
                  %-  put-all:si:ol 
                  :*  sont-map 
                      txid.new-sont 
                      vout.new-sont 
                      off.new-sont 
                      value.i.inputs  :: XX this is wrong, it should be the output value
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
