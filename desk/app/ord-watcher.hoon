/-  bitcoin, spider, ord, urb, ow=ord-watcher
/+  bc=bitcoin, btcio, dbug, default-agent, ol=ord, ul=urb, strandio, verb
::
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  state-0:ow
  ==
--
::
%-  agent:dbug
^-  agent:gall
=|  state-0:ow
=*  state  -
%+  verb  &
=<
|_  =bowl:gall
+*  this   .
    def    ~(. (default-agent this %|) bowl)
    cor    +>
::
++  on-init
  ^-  (quip card _this)
  :-  :~  [%pass /lo %arvo %j [%listen *(set ship) [%| dap.bowl]]]
      ==
  =,  state
  %=  this
    start               start-height:urb
    block-id.ord-state  [start-hash:urb start-height:urb]
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
  ?+    mark  !!
      %debug
    =/  act  !<(debug:ow vase)
    ?-    -.act
    ::
    ::  Reset ord-core state.
        %clear-oc
      =/  new-oc           *state:ord
      =.  block-id.new-oc  [start-hash:urb start-height:urb]
      `this(ord-state.state new-oc)
    ==
  ::
      %action
    =/  act  !<(action:ow vase)
    ?-    -.act
    ::
    ::  Set the RPC endpoint.
        %config-rpc
      `this(req-to.state `req-to.act)
    ::
    ::  
        %start
      ?~  req-to.state
        %-  (slog leaf+"no bitcoin rpc node config, use %config-rpc poke" ~)
        `this
      :_  this
      ::  XX should interval be defined here?
      ::     in state? hardcoded?
      ::     why have an interval at all?
      :~  :*  %pass
             /got-blocks
             %arvo
             %k
             %lard
             q.byk.bowl
             %:  starting-blocks:cor
               u.req-to.state
               ord-state.state
               ::
               ::  XX should be a (unit num)
               ::
               ::  XX should we clear the ord-state if
               ::     the start number is specified?
               ::     /lib/ord arms expect incoming blocks
               ::     to be contiguous with the most recent
               ::     block in ord-core state
               ::
               num.act
               :: %+  max
               ::   start.state
               :: num.block-id.ord-state.state
             ==  
          ==  
      == 
    ==
  ==
::
++  on-peek
  |=  =(pole knot)
  ^-  (unit (unit cage))
  (on-peek:def pole)
::
++  on-watch
  |=  =(pole knot)
  ^-  (quip card _this)
  ?+    pole  !!
  ::
  ::  Jael subscribes to / aka ~
      ~
    `this
  ::
      [=ship ~]
    =/  nu-whos
      (~(put in whos.state) (slav %p ship.pole))
    :_  this(whos.state nu-whos)
    (udiffs-to-jael-cards (state-to-udiffs ord-state))
  ==
::
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card _this)
  ?+    wire  !!
      [%got-blocks ~]
    ?+    sign-arvo  !!
        [%khan %arow *]
      ?.  -.p.sign-arvo
        ((slog leaf+<p.p.sign-arvo> ~) `this)
      ?>  ?=([%khan %arow %.y %noun *] sign-arvo)
      =/  [%khan %arow %.y %noun =vase]  sign-arvo
      =/  val
        !<  
        (pair (list [id:block:bitcoin effect:ord]) state:ord)
        vase
      :_  this(ord-state.state +.val)
      (udiffs-to-jael-cards (fx-to-udiffs -.val))
    ==
  ==
::
++  on-leave
  ::  XX check subscriptions for a path
  on-leave:def
++  on-agent  on-agent:def
++  on-fail   on-fail:def
--
::
|%
++  udiffs-to-jael-cards
  |=  =udiffs:point:jael
  ^-  (list card)
  :-  [%give %fact [/]~ %azimuth-udiffs !>(udiffs)]
  %+  murn
    udiffs
  |=  [=ship =udiff:point:jael]
  ^-  (unit card)
  `[%give %fact [/(scot %p ship)]~ %azimuth-udiffs !>([udiff]~)]
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
  |-  ^+  new-udiffs
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
::
::  
++  starting-blocks
  |=  [=req-to:btcio =state:ord start=@ud]
  ^-  shed:khan
  ::  XX temp state mutation
  ::  XX better solution to index start issue than (dec start)?
  ::     this all assumes ord-core state is empty, which
  ::     won't be true in production
  ::  =/  oc  (abed:ord-core:ord state)
  =/  oc
    (abed:ord-core:ul state(num.block-id (dec start)))
  =/  m  (strand:strandio ,vase)
  |^  
  ^-  form:m
  ;<    latest-block=(unit @ud)
      bind:m
    (get-block-count:btcio req-to ~)
  ?~  latest-block
    ~|  %couldnt-find-latest-block
    !!
  ~&  >  "latest block is {<u.latest-block>}"
  ::  we wait 6 blocks for transactions to finalize
  =/  last-settled-block
    (sub u.latest-block 6)
  ::  start indexing from .start
  =/  num  start
  |-  
  ^-  form:m
  ?.  (lte num last-settled-block)
    (pure:m !>([fx state]:oc))
  ;<    bluck=(unit block:bitcoin)
      bind:m
    (get-block-by-number:btcio req-to ~ num)
  ?~  bluck
    ~|  %cant-find-block-by-number
    !!
  ;<    new-block=[num:id:block:bitcoin urb-block:urb]
      bind:m
    (convert-block num u.bluck)
  =.  oc  (handle-block:oc new-block)
  ~&  >  "processed block {<num>} of {<last-settled-block>}"
  $(num +(num))
  ::
  ::  Convert a block:bitcoin into a urb-block:urb.
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
    |-  ^-  form:m
    :: XX refactor to use gettxout
    ?~  is  ^$(txs t.txs)
    =/  dep  (~(get by -.deps) [txid pos]:i.is)
    ?:  &(?=(^ dep) ?=(^ value.u.dep))  $(is t.is)
    ;<  utx=(unit tx:bc)  bind:m
      (get-raw-transaction:btcio req-to ~ txid.i.is)
    ?~  utx  !!
    =/  os  os.u.utx
    =|  pos=@ud
    |-  ^-  form:m
    ?~  os  ^$(is t.is)
    =/  dep  (~(get by -.deps) [id.u.utx pos])
    ?:  &(?=(^ dep) ?=(^ value.u.dep))  $(os t.os, pos +(pos))
    =/  sots=(list raw-sotx:urb)  ?~(dep ~ sots.u.dep)
    $(os t.os, pos +(pos), -.deps (~(put by -.deps) [id.u.utx pos] [sots `value.i.os]))
  --
--