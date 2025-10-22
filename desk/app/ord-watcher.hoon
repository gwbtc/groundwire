/-  bitcoin, spider, ord, urb
/+  bc=bitcoin, btcio, dbug, default-agent, ol=ord, ul=urb, strandio, verb
::
|%
+$  card  card:agent:gall
+$  versioned-state  $%(state-0)
+$  state-0
  $:  %0
      rpc=req-to:btcio
      ord-state=state:ord
  ==
--
::
%-  agent:dbug
^-  agent:gall
=|  state-0
=*  state  -
%+  verb  &
=<
|_  =bowl:gall
+*  this   .
    def    ~(. (default-agent this %|) bowl)
::
::  Tell Jael to subscribe to us for PKI updates,
::  initialize an ord-core, start a thread to
::  fetch and process the first batch of blocks,
::  and start a timer to fetch again.
++  on-init
  ^-  (quip card _this)
  =/  new-rpc  ['http://localhost:18443' [%basic 'bitcoinrpc:bitcoinrpc']]
  =/  new-ord-state
    :*  [start-hash:urb start-height:urb]
        *sont-map:ord
        *insc-ids:ord
        *unv-ids:ord
    ==
  :_  this(rpc new-rpc)
  :~  :*  %pass  /listen  %arvo  %j 
          %listen  *(set ship)  [%| dap.bowl]
      ==
      :*  %pass  /blocks  %arvo  %k
          %lard  q.byk.bowl
          (get-blocks new-rpc new-ord-state)
      ==  
  ==
::
++  on-save
  ^-  vase
  !>(state)
::
++  on-load
  |=  =vase
  ^-  (quip card _this)
  =/  old  !<(versioned-state vase)
  ?-    -.old
      %0
    `this(state old)
  ==
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?>  =(our src):bowl
  (on-poke:def mark vase)
::
++  on-peek
  |=  =(pole knot)
  ^-  (unit (unit cage))
  (on-peek:def pole)
::
++  on-watch
  |=  =(pole knot)
  ^-  (quip card _this)
  ?+    pole  (on-watch:def pole)
  ::
  ::  Jael subscribes to empty wire /, aka ~
      ~
    `this
  ==
::
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card _this)
  ?+    wire  (on-arvo:def wire sign-arvo)
  ::
  ::  Run +get-blocks at regular intervals.
      [%timer ~]
    :_  this
    :~  :*  %pass  /blocks  %arvo  %k
            %lard  q.byk.bowl
            (get-blocks [rpc ord-state]:state)
        ==  
    ==
  ::
  ::  Our +get-blocks thread returned. Update
  ::  ord-state, emit udiffs to Jael, and set a timer
  ::  to run the thread again.
      [%blocks ~]
    ?+    sign-arvo  (on-arvo:def wire sign-arvo)
        [%khan %arow *]
      ?.  -.p.sign-arvo
        ((slog leaf+<p.p.sign-arvo> ~) `this)
      ?>  ?=([%khan %arow %.y %noun *] sign-arvo)
      =/  [%khan %arow %.y %noun =vase]  sign-arvo
      =/  fx-and-state
        !<  
        [(list [id:block:bitcoin effect:ord]) state:ord]
        vase
      :_  this(ord-state.state +.fx-and-state)
      :-  [%pass /timer %arvo %b %wait (add ~s30 now.bowl)]
      (udiffs-to-jael-cards (fx-to-udiffs -.fx-and-state))
    ==
  ==
::
++  on-leave  on-leave:def
++  on-agent  on-agent:def
++  on-fail   on-fail:def
--
::
|%
::
::  Fetch blocks in range(last-processed + 1, latest - 6)
::  from the provided RPC endpoint, then use a stateful 
::  ord-core to process these blocks, returning
::  a new ord-state and a list of fx in +on-arvo.
++  get-blocks
  |=  [rpc=req-to:btcio ord-state=state:ord]
  ^-  shed:khan
  =/  i  (add 1 num.block-id.ord-state) :: last processed block height + 1
  =/  oc
    %-  abed:ord-core:ul
    ord-state
  ::  This barket lets us easily include oc in 
  ::  +convert-block's context.
  |^
  =/  m  (strand:strandio ,vase)
  ;<    latest-block=(unit @ud)
      bind:m
    (get-block-count:btcio rpc ~)
  ?~  latest-block  ~|  %couldnt-find-latest-block  !!
  ~&  >  "latest block is {<u.latest-block>}"
  =/  last-settled-block  (sub u.latest-block 6)
  |-  
  ?.  (lte i last-settled-block)
    (pure:m !>([fx state]:oc))
  ;<    bluck=(unit block:bitcoin)
      bind:m
    (get-block-by-number:btcio rpc ~ i)
  ?~  bluck  ~|  %cant-find-block-by-number  !!
  ;<    new=[num:id:block:bitcoin urb-block:urb]
      bind:m
    (convert-block i u.bluck)
  =.  oc  (handle-block:oc new)
  ~&  >  "processed block {<i>} of {<last-settled-block>}"
  $(i +(i))
  ::
  ::  Convert a block:bitcoin into a urb-block:urb.
  ::  This requires an async +get-raw-transaction call.
  ++  convert-block
    |=  [=num:id:block:bitcoin =block:bitcoin]
    =/  m  (strand:strandio ,[num=@ud urb-block:urb])
    =/  deps  (find-block-deps:oc num block)
    =/  txs  (tail txs.block)
    |-  
    ^-  form:m
    ?~  txs
      (pure:m (apply-block-deps:oc [num +.deps] -.deps))
    =/  is  is.i.txs
    |-  
    ^-  form:m
    :: XX refactor to use gettxout
    ?~  is  ^$(txs t.txs)
    =/  dep  (~(get by -.deps) [txid pos]:i.is)
    ?:  &(?=(^ dep) ?=(^ value.u.dep))  $(is t.is)
    ;<  utx=(unit tx:bc)  bind:m
      (get-raw-transaction:btcio rpc ~ txid.i.is)
    ?~  utx  !!
    =/  os  os.u.utx
    =|  pos=@ud
    |-  
    ^-  form:m
    ?~  os  ^$(is t.is)
    =/  dep  (~(get by -.deps) [id.u.utx pos])
    ?:  &(?=(^ dep) ?=(^ value.u.dep))  $(os t.os, pos +(pos))
    =/  sots=(list raw-sotx:urb)  ?~(dep ~ sots.u.dep)
    $(os t.os, pos +(pos), -.deps (~(put by -.deps) [id.u.utx pos] [sots `value.i.os]))
  --
::
::  Conversion arms. 
::  fx are ord-core's type for urb effects. 
::  udiffs are Jael's type for PKI updates. 
::  cards for Jael contain udiffs.
++  udiffs-to-jael-cards
  |=  =udiffs:point:jael
  ^-  (list card)
  %+  turn
    udiffs
  |=  [=ship =udiff:point:jael]
  ^-  card
  [%give %fact [/(scot %p ship)]~ %azimuth-udiffs !>([udiff]~)]
::
++  fx-to-udiffs
  |=  fx=(list [id:block:bitcoin effect:ord])
  ^-  udiffs:point:jael
  %+  murn
    fx
  |=  [=id:block:bitcoin eo=effect:ord]
  ^-  (unit (pair ship udiff:point:jael))
  ?.  ?=(%point -.eo) :: only if this effect is a %point diff:urb
    ~
  =/  pdiff  (tail (tail eo))
  ?+    -.pdiff   ~
      %rift
    `[ship.eo id %rift rift.pdiff %.n]
  ::
      %sponsor
    `[ship.eo id %spon sponsor.pdiff]
  ::
      %keys
    `[ship.eo id %keys [life.pdiff (sub (end 3 pass.pdiff) 'a') pass.pdiff] %.y]
  ::
      %fief
    `[ship.eo id %fief fief.pdiff]
  ==
::
++  state-to-udiffs
  |=  ord-state=state:ord
  ^-  udiffs:point:jael
  =/  points=(list [=ship point:ord])
    ~(tap by unv-ids.ord-state)
  =/  =id:block:jael
    block-id:ord-state
  =/  new-udiffs  *udiffs:point:jael
  |-  
  ^+  new-udiffs
  ?~  points
    new-udiffs
  %=  $
     points  t.points
  ::
     new-udiffs  
     %+  welp
      =,  i.points
      ^-  udiffs:point:jael
      :~  [ship id %keys [life.net (sub (end 3 pass.net) 'a') pass.net] %.y]
          [ship id %rift rift.net %.y]
          [ship id %spon ?:(has.sponsor.net `who.sponsor.net ~)]
          [ship id %fief fief.net]
      ==
    new-udiffs
  ==
--