/-  bitcoin, spider, ord, urb
/+  bc=bitcoin, btcio, dbug, default-agent, ol=ord, strandio, verb
::
|%
+$  card  card:agent:gall
::
+$  state-versions
  $%  state-0
  ==
::
+$  state-0
  $+  state-0
  $:  %0
      ord-state=state:ord
      start=num:block:bc
      whos=(set ship)
      req-to=(unit req-to:btcio)
  ==
::
+$  ord-watcher-action
  $+  ord-watcher-action
  ::  XX %start num should be a unit
  ::     if null, %start poke will run as-is
  ::     if not, this number will override it
  $%  [%start =num:block:bc]
      [%config-rpc =req-to:btcio]
  ==
+$  ord-watcher-debug-action
  $+  ord-watcher-debug-action
  $%  [%clear-oc ~]
  ==
--
%-  agent:dbug
^-  agent:gall
=|  state-0
=*  state  -
%+  verb  &
=<
|_  =bowl:gall
+*  this   .
    def    ~(. (default-agent this %|) bowl)
    cor    ~(. +> bowl)
::
++  on-init
  :-  :~  [%pass /lo %arvo %j [%listen *(set ship) [%| dap.bowl]]]
      ==
  =,  state
  %=  this
    start               start-height:urb
    block-id.ord-state  [start-hash:urb start-height:urb]
  ==
::
++  on-save
  !>(state)
::
++  on-load
  |=  old=vase
  ^-  [(list card) _this]
  :-  ~
  %=  this
    state  !<(state-0 old)
  ==
::
++  on-poke
  |=  [=mark =vase]
  ^-  [(list card) _this]
  ?>  =(our src):bowl
  ?+  mark
    (on-poke:def mark vase)
  ::
      %action
    =/  act  !<(ord-watcher-action vase)
    ?-  -.act
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
              %:  get-blocks:cor
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
::                %+  max
::                  start.state
::                num.block-id.ord-state.state
      ==  ==  ==
    ::
        %config-rpc
      :-  ~
      %=  this
        req-to.state  `req-to.act
      ==
    ==
  ::
      %debug
    =/  act  !<(ord-watcher-debug-action vase)
    ?-    -.act
        %clear-oc
      =/  new-oc           *state:ord
      =.  block-id.new-oc  [start-hash:urb start-height:urb]
      :-  ~
      %=  this
        ord-state.state  new-oc
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
  ^-  [(list card) _this]
  ?+  pole
    (on-watch:def pole)
  ::
  ::  jael subscribes to /, which we receive here
      ~
    `this
  ::
      [=ship ~]
    =/  nu-whos
      (~(put in whos.state) (slav %p ship.pole))
    :-  %-  make-jael-update-from-udiffs
        %+  make-udiffs-from-state
          nu-whos
        ord-state.state
    %=  this
      whos.state  nu-whos
    ==
  ==
::
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card _this)
  ?+  wire
    (on-arvo:def wire sign-arvo)
  ::
      [%got-blocks ~]
    ?+  sign-arvo
      (on-arvo:def wire sign-arvo)
    ::
        [%khan %arow *]
      ?.  -.p.sign-arvo
        ((slog leaf+<p.p.sign-arvo> ~) `this)
      ?>  ?=([%khan %arow %.y %noun *] sign-arvo)
      =/  [%khan %arow %.y %noun =vase]  sign-arvo
      =/  val
        !<  (pair (list (pair id:block:bitcoin effect:ord)) state:ord)
        vase
      =/  jael-cards
        (make-jael-cards-from-fx whos.state -.val)
      :-  jael-cards
      %=  this
        ord-state.state  +.val
      ==
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
::  helper core
|_  =bowl:gall
++  make-jael-cards-from-fx
  ::  XX do we care about whos?
  |=  [whos=(set ship) fx=(list (pair id:block:bitcoin effect:ord))]
  ^-  (list card)
  (make-jael-update-from-udiffs (ord-core-fx-to-udiffs fx))
::
++  make-jael-update-from-udiffs
  |=  =udiffs:point:jael
  ^-  (list card)
  :-  [%give %fact [/]~ %azimuth-udiffs !>(udiffs)]
  %+  murn
    udiffs
  |=  [=ship =udiff:point:jael]
  ^-  (unit card)
  `[%give %fact [/(scot %p ship)]~ %azimuth-udiffs !>([udiff]~)]
::
++  make-udiffs-from-state
  |=  [whos=(set ship) ord-state=state:ord]
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
     points      t.points
     new-udiffs  %+  welp
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
++  ord-core-fx-to-udiffs
  |=  fx=(list [id:block:bitcoin effect:ord])
  ^-  udiffs:point:jael
  %+  murn
    fx
  |=  [=id:block:bitcoin eo=effect:ord]
  ^-  (unit (pair ship udiff:point:jael))
  ?.  ?=(%point -.eo)
    ~
  ::  XX rename
  =/  foo  (tail (tail eo))
  ?+  -.foo
    ~
      %rift
    `[ship.eo id %rift rift.foo %.n]
  ::
      %sponsor
    `[ship.eo id %spon sponsor.foo]
  ::
      %keys
    `[ship.eo id %keys [life.foo (sub (end 3 pass.foo) 'a') pass.foo] %.y]
  ::
      %fief
    `[ship.eo id %fief fief.foo]
  ==
::
++  get-blocks
  |=  [=req-to:btcio =state:ord start=@ud]
  ^-  shed:khan
  ::  XX temp state mutation
  ::  XX better solution to index start issue than (dec start)?
  ::     this all assumes ord-core state is empty, which
  ::     won't be true in production
  ::  =/  oc  (abed:ord-core:ord state)
  =/  oc
    (abed:ord-core:ol state(num.block-id (dec start)))
  =/  m  (strand:strandio ,vase)
  |^  ^-  form:m
      ::  XX why wait?
      =/  wait  ~s0
      ;<  *  bind:m  (sleep:strandio wait)
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
      |-  ^-  form:m
      ?.  (lte num last-settled-block)
        (pure:m !>([fx state]:oc))
      ;<    bok=(unit block:bitcoin)
          bind:m
        (get-block-by-number:btcio req-to ~ num)
      ?~  bok
        ~|  %cant-find-block-by-number
        !!
      ;<    nu-bok=(pair num:id:block:bitcoin urb-block:urb)
          bind:m
        (elab-block num u.bok)
      =.  oc  (handle-block:oc nu-bok)
      ~&  >  "processed block {<num>} of {<last-settled-block>}"
      $(num +(num))
    ::
  ::
  ::  XX rename
  ::     this arm takes a block:bitcoin and should turn
  ::     it into something called a block:urb or sth
  ++  elab-block
    |=  [=num:id:block:bitcoin =block:bitcoin]
    =/  m  (strand:strandio ,[num=@ud urb-block:urb])
    =/  deps  (find-block-deps:oc num block)
    =/  txs  (tail txs.block)
    |-  ^-  form:m
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
