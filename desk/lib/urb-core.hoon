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
::  XX Assume you submit a %spawn transaction and then spend the sont
::  in a second transaction within the same block, but without 
::  associating an attestation with it. Have we included that second
::  transaction in our ++handle-block loop? No, we filtered it out
::  on the basis that it didn't include an attestation or a known
::  utxo.
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
          cb-tx=[val=@ud urb-tx:urb]
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
  ::  of raw-sotx) and the block filtered down to
  ::  urb-relevant txs. A tx is relevant either if
  ::  we had saved one of its inputs previously,
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
    =>  ?>  ?=(^ txs.block)
        :-  cb-tx=i.txs.block
        ~&  [%coinbase-tx cb-tx]
        %=  .
          txs.block  t.txs.block
        ==
    ::
    ::  Filter through this block's transactions
    ::  for tx containing reveals.
    =|  tx-done=(list tx:bitcoin)
    |-  
    ^+  [reveals block]
    ?~  txs.block
      :-  reveals
      %=  block
        txs  [cb-tx (flop tx-done)]
      ==
    =/  is    is.i.txs.block  :: list of inputs
    =|  need-tx=_|            :: whether we need to save this tx
    ::
    ::  Loop through this transaction's inputs
    ::  and, if any contain sots, save the whole tx.
    |^  
    ^+  ^$
    ?~  is
      %=  ^$
        txs.block  t.txs.block
        tx-done    ?.  need-tx
                     tx-done
                   [i.txs.block tx-done]
      ==
    ::
    ::  If this input had been saved as an output in our state,
    ::  then that means it was relevant to urb, and
    ::  we for sure need to pay attention to it again.
    =/  value=(unit @ud)
      =/  vout  (get-vout:si:ol sont-map [txid pos]:i.is)
      ?~  vout
        ~
      `value.u.vout
    =.  need-tx  |(need-tx ?=(^ value))
    ::
    ::  Parse the witness for sots. If no sots, just 
    ::  preserve our potential saved value and recurse.
    =/  raw-script=(unit octs)
      =/  rwit
        (flop witness.i.is)
      ?.  ?=([* ^] rwit)
        ~
      ?.  =+  i.rwit
          ?&  !=(0 wid)
              =(0x50 (cut 3 [(dec wid) 1] dat))
          ==
        `i.t.rwit
      ?~  t.t.rwit
        ~
      `i.t.t.rwit
    ?~  raw-script
      (add-to-reveals ~ value)
    =/  descr
      (de:bscr u.raw-script)
    ?~  descr
      (add-to-reveals ~ value)
    ~|  [=+(u.raw-script [p `@ux`q]) =+((en:bscr u.descr) [p `@ux`q])]
    ?>  =(u.raw-script (en:bscr u.descr))
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
    =|  val=@ud
    =|  idx=@ud
    |=  tx=urb-tx:urb
    ~&  [%handling-trasaction tx]
    ^+  cor
    =/  sum-out  (roll os.tx |=([[* a=@] b=@] (add a b)))
    =/  sum-in  (roll is.tx |=([a=input:urb-tx:urb b=@] (add value.a b)))
    =/  is  is.tx
    ?~  is  cor
    |^  
    ^+  cor
    =.  cor  process-unv
    ::  =.  cor  check-for-insc
    =.  cor  calc-output-sont
    =<  ?~(t.is cor $(is t.is))
    ::  Excess inputs get added to coinbase fee,
    ::  which we calculate iteratively because we need
    ::  its per-input value for sont math.
    =/  new-val  (add val value.i.is)
    ?:  (lth new-val sum-out)
      .(val new-val)
    %=  .
      val        sum-out
      val.cb-tx  (add val.cb-tx (sub new-val sum-out))
    ==
    ::
    ::  XX For all failures in this arm figure out when 
    ::  to loop and when to quit. Do we ever need to rewind?
    ++  process-unv
      ^+  cor
      =/  sots  sots.i.is
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
        =/  cac  (com:nu:cric:crypto pass.sot)
        ?.  =(who fig:ex:cac)  cor              :: initial comet @p = hash of public key
        ?~  sat=(calc-spawn-sont +>.sot)  cor   :: valid spawn sont
        ?.  ?=(%c suite.+<.cac)  cor            :: uses suite c encoded pass
        =/  tweak  
          (rap 3 ~[%btc %gw txid=txid.u.sat vout=vout.u.sat off=off.u.sat])
        ?.  =(dat.tw.pub:+<:cac tweak)  cor     :: correct tweak, including spawn sat
        :: ?~  sig  cor                         :: verify networking key signature XX remove this?
        :: ?.  %:  veri-octs:ed:crypto 
        ::         u.sig 
        ::         512^(shal raw.i.^sots) 
        ::         sgn:ded:ex:cac
        ::     ==
        ::   cor
        =/  sponsor  `@p`(end 4 who)
        =/  =point:urb
          :*  own=[u.sat ~]
              rift=0
              life=1
              pass=pass.sot
              sponsor=[& sponsor]
              escape=~
              fief=~
          ==
        =.  cor
          %-  emil
            :~  [%point who %owner u.sat]
                [%point who %sponsor `sponsor]
                [%point who %keys 1 pass.sot]
            ==
        ~&  >  [%spawn-succeeded point]
        %_    $
            point    `point
            sots     t.sots
            sont-map  (put-com:si:ol sont-map txid.u.sat vout.u.sat off.u.sat value.i.is who)
            unv-ids   (~(put by unv-ids) who point)
        ==
      ::=^  point  cor  (spend-point point)
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
        ~|  [s=sot [txid pos value]:i.is]
        ?.  =([txid vout]:sot [txid pos]:i.is)  |
        ~|  %fatal-tracking-error
        ?>  (lth off.sot value.i.is)  &
      ::
      ::  Return a spawner's ordinal claim as a sont.
      ::  Return null if invalid.
      ++  calc-spawn-sont
        |=  $:  ::from=(unit [=vout:ord =off:ord])
                out=[spkh=@ux pos=(unit vout:ord) =off:ord tej=off:ord]
            ==
        ^-  (unit sont:ord)
        =|  out-pos=@ud
        =|  out-val=@ud
        =/  os  os.tx
        |-  
        ^-  (unit sont:ord)
        ?~  os  ~
        ?:  &(?=(^ pos.out) (lth u.pos.out out-pos))  ~
        ?:  |((lte (add out-val value.i.os) val) &(?=(^ pos.out) !=(out-pos u.pos.out)))
          $(out-val (add out-val value.i.os), os t.os, out-pos +(out-pos))
        ?:  (lte (add val value.i.is) :(add out-val off.out tej.out))  ~
        ?:  (lte value.i.os (add [off tej]:out))
          $(out-val (add out-val value.i.os), os t.os, out-pos +(out-pos))
        =/  sat=sont:ord  [txid.i.is pos.i.is (sub (add out-val off.out) val)]
        ::?.  |(?=(~ from) !=(u.from [pos off]:sat))  ~
        ?^  (get-com:si:ol sont-map sat)
          ?^(pos.out ~ $(out-val (add out-val value.i.os), os t.os, out-pos +(out-pos)))
        =/  en-out  (can 3 script-pubkey.i.os 8^value.i.os ~)
        =/  hax-out  (shay (add 8 wid.script-pubkey.i.os) en-out)
        ?:  =(hax-out spkh.out)  `sat
        ?.  =(~ pos.out)  ~
        $(out-val (add out-val value.i.os), os t.os, out-pos +(out-pos))
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
    ::  Calculate output sonts for all input sonts.
    ++  calc-output-sont
      :: XX: todo: optimize for updates per-input
      ^+  cor
      ?~  itxo=(~(get by sont-map) txid.i.is pos.i.is)  cor
      =.  sont-map  (~(del by sont-map) txid.i.is pos.i.is)
      =/  isonts  ~(tap by sats.u.itxo)
      |-  
      ^+  cor
      ?~  isonts  cor
      =/  osont=sont:ord  [txid.i.is pos.i.is p.i.isonts] 
      ?~  nsont=(off-to-sont p.i.isonts)
        =.  state  (update-ids state q.i.isonts [0x0 0 0])
        =.  cor  (emit [%xfer osont [0x0 0 0]])
        %_  $
          isonts  t.isonts
        ==
      =.  state  (update-ids state q.i.isonts nsont)
      =.  cor  (emit [%xfer osont nsont])
      %_  $
        isonts   t.isonts
        sont-map  (put-all:si:ol sont-map txid.nsont vout.nsont off.nsont value.i.is q.i.isonts)
      ==
    ::
    ++  pntr-to-sont
      |=  pntr=@ud
      ^-  $@(~ sont:ord)
      ?.  (lth pntr sum-out)
        =/  sont  (pointer-to-sont (add val.cb-tx (sub pntr sum-out)) os.cb-tx)
        ?:  |(=(~ sont) (lte sum-in pntr))  ~
        ?>  ?=(^ sont)
        [id.cb-tx vout.sont off.sont]
      ?~  sont=(pointer-to-sont pntr os.tx)  !!
      [id.tx vout.sont off.sont]
      ::=/  =txid:ord  txid.i.is
      ::  check for pointer validity here
      ::?.  &(?=([* %& *] pntr) (lth p.+.pntr sum-outs))
      ::  ?~  tracked=(off-to-sont idx)  ~
      ::  [txid tracked]
      ::?~  tagged=(pointer-to-sont p.+.pntr os.tx)  !!
      ::[txid tagged]
    ::
    ++  off-to-sont
      |=  off=@ud
      ^-  $@(~ sont:ord)
      ::  todo: double check this shorter code does what's intended
      (pntr-to-sont (add val off))
    ::  ?.  (lth (add val off) sum-out)
    ::    ?~  sont=(pointer-to-sont (add val.cb-tx (sub (add val off) sum-out)) os.cb-tx)
    ::      ~
    ::    [txid.cb-tx vout.sont off.sont]
    ::  ?~  sont=(pointer-to-sont (add val off) os.tx)  !!
    ::  [txid vout.sont off.sont]
    ::
    :: ++  inscription-to-sont
    ::   |=  mail
    ::   ^-  $@(~ sont)
    ::   =/  =txidash  txid.i.is
    ::   ::  check for pointer validity here
    ::   ?.  &(?=([* %& *] pntr) (lth p.+.pntr sum-outs))
    ::     ?~  tracked=(off-to-sont idx)  ~
    ::     [txidash tracked]
    ::   ?~  tagged=(pointer-to-sont p.+.pntr os.tx)  !!
    ::   [txidash tagged]
    :: 
    :: ++  check-for-insc
    ::   ^+  cor
    ::   =/  raw-script=(unit octs)
    ::     =/  rwit  (flop witness.i.is)
    ::     ?.  ?=([* ^] rwit)  ~
    ::     ?.  =+(,.-.rwit &(!=(0 wid) =(0x50 (rsh [3 (dec wid)] dat))))  `i.t.rwit
    ::     ?~(t.t.rwit ~ `i.t.rwit)
    ::   ?~  raw-script  cor
    ::   ::=/  scr  (mole |.((de:bscr u.raw-script)))
    ::   :: XX: make crash-proof
    ::   ::=/  scr  (de:bscr u.raw-script)
    ::   ?~  scr=(de:bscr u.raw-script)  cor
    ::   ?>  =(u.raw-script (en:bscr u.scr))
    ::   =/  mails=(list mail)  (mails:de:ol u.scr)
    ::   |-  ^+  cor
    ::   ?~  mails  cor
    ::   =/  pntr=@ud  ?:(?=([* %& *] pntr.i.mails) p.+.pntr.i.mails 0)
    ::   =/  =insc  id.tx^idx
    ::   =/  nsont  (pntr-to-sont pntr)
    ::   ?~  nsont
    ::     :: the ordinals docs suggests that if the pointer index is
    ::     :: invalid, then it is treated normally i.e. on 0 index
    ::     =.  cor  (emit [%insc insc ~ i.mails])
    ::     %_  $
    ::       idx        +(idx)
    ::       mails      t.mails
    ::       insc-ids   (~(put by insc-ids) insc [[id.tx 0 0] i.mails])
    ::     ==
    ::   =.  cor  (emit [%insc insc nsont i.mails])
    ::   %_  $
    ::     idx     +(idx)
    ::     mails   t.mails
    ::     sont-map  (put-ins:si:ol sont-map txid.nsont vout.nsont off.nsont insc^~^~)
    ::     insc-ids   (~(put by insc-ids) insc [nsont i.mails])
    ::    ==
    ::
    --
  --
::
++  pointer-to-sont
  =|  vout=@ud
  |=  [pntr=@ud outs=(list output:tx:bitcoin)]
  ^-  $@(~ [vout=@ud off=@ud])
  ?~  outs  ~
  ?:  (lth pntr value.i.outs)  [vout pntr]
  $(vout +(vout), pntr (sub pntr value.i.outs))
::
++  update-ins
  |=  [=state:urb oids=(set insc:ord) =sont:ord]
  ?:  =(~ oids)  state
  %-  ~(rep in oids)
  |:  [*=insc:ord state]
  =/  dat  (~(got by insc-ids) insc)
  state(insc-ids (~(put by insc-ids) insc dat(sont sont)))
::
++  update-com
  |=  [=state:urb com=@p =sont:ord]
  =/  point  (~(got by unv-ids:state) com)
  state(unv-ids (~(put by unv-ids:state) com point(sont.own sont)))
::
++  update-ids
  |=  [=state:urb old=sont-val:ord =sont:ord]
  =.  state  (update-ins state ins.old sont)
  ?~  com.old  state
  (update-com state u.com.old sont)
--
