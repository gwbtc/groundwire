::  btc-watcher: btc block source
::
/-  spider
/+  ord, bc=bitcoin, default-agent, verb, dbug, btcio
=,  jael
::
=>  |%
    +$  card  card:agent:gall
    +$  app-state
      $:  %0
          ord=state:ord
          start=(unit num:block:bc)
          whos=(set ship)
          ted=(unit [since=@da =tid:spider])
          req-to=(unit req-to:btcio)
      ==
    +$  blocks  (list [=num:block:bc block:bc])
    --
::
::  Helpers
::
=>  |%
    ++  wait
      |=  [=path now=@da time=@dr]
      ^-  card
      [%pass [%timer path] %arvo %b %wait (add now time)]
    ::
    ++  wait-shortcut
      |=  [=path now=@da]
      ^-  card
      [%pass [%timer path] %arvo %b %wait now]
    ::
    ++  poke-spider
      |=  [=path our=@p =cage]
      ^-  card
      [%pass [%running path] %agent [our %spider] %poke cage]
    ::
    ++  watch-spider
      |=  [=path our=@p =sub=path]
      ^-  card
      [%pass [%running path] %agent [our %spider] %watch sub-path]
    ::
    ++  leave-spider
      |=  [=path our=@p]
      ^-  card
      [%pass [%running path] %agent [our %spider] %leave ~]
    --
::
::  Main
::
%-  agent:dbug
^-  agent:gall
=|  state=app-state
%+  verb  |
=<
|_  bol=bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bol)
    cor   ~(. +> bol)
::
++  on-init
  ^-  (quip card _this)
  [~ this]
::
++  on-save   !>(state)
++  on-load
  |=  old=vase
  ^-  (quip card _this)
  =+  !<(old=app-state old)
  `this(state old)
::
++  on-poke
  |=  [=mark =vase]
  ?>  =(our src):bol
  ?:  ?=(%noun mark)
    ~&  state
    `this
  `this
::
::  +on-watch: subscribe & get initial subscription data
::
::    /logs/some-path:
::
++  on-watch
  |=  =path
  ^-  (quip card _this)
  =/  who=(unit ship)
    ?~  path  ~
    ?>  ?=([@ ~] path)
    `(slav %p i.path)
  =.  whos.state
    ?~  who
      ~
    (~(put in whos.state) u.who)
  ^-  (quip card _this)
  ::  Slow to recalculate all the diffs, but this is necessary to make
  ::  sure Jael gets the updates from the snapshot
  ::
  ::%-  %-  slog  :_  ~
  ::    :-  %leaf
  ::    "ship: processing azimuth snapshot ({<points>} points)"
  =/  cards  (jael-update:cor run-state:cor)
  [cards this]
::
++  on-leave  on-leave:def
::
::  +on-peek: get diagnostics data
::
::    /block/some-path: get next block number to check for /some-path
::
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  ~
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  ?.  ?=([%running *] wire)
    (on-agent:def wire sign)
  ?-    -.sign
      %poke-ack
    ?~  p.sign
      [~ this]
    %-  (slog leaf+"btc-watcher couldn't start thread" u.p.sign)
    =^  cards  state  (ted-start:cor ~s0)
    :_  this
    [(leave-spider t.wire our.bol) cards]

  ::
      %watch-ack
    ?~  p.sign
      [~ this]
    %-  (slog leaf+"btc-watcher couldn't start listening to thread" u.p.sign)
    ::  TODO: kill thread that may have started, although it may not
    ::  have started yet since we get this response before the
    ::  %start-spider poke is processed
    ::
    =^  cards  state  (ted-start:cor ~s0)
    [cards this]
  ::
      %kick
    =^  cards  state  (ted-start:cor ~s0)
    [cards this]
  ::
      %fact
    =*  path  t.wire
    ?+    p.cage.sign  (on-agent:def wire sign)
        %thread-fail
      =+  !<([=term =tang] q.cage.sign)
      %-  (slog leaf+"btc-watcher failed; will retry" leaf+<term> tang)
      =^  cards  state  (ted-start:cor ~s30)
      [cards this]
    ::
        %thread-done
      ::  if empty, that means we cancelled this thread
      ::
      ?:  =(*vase q.cage.sign)
        ::`this
        =^  cards  state  (ted-start:cor ~s0)
        [cards this]
      =+  !<(=blocks q.cage.sign)
      =^  cards-a  state  (ted-start:cor ~m3)
      =^  cards-b  state  (handle-blocks:cor blocks)
      [(weld cards-a cards-b) this]
    ==
  ==
::
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card _this)
  [~ this]
::
++  on-fail   on-fail:def
--
|_  bol=bowl:gall
+*  cor   .
    bec   byk.bol(r da+now.bol)
::
++  get-blocks
  |=  wait=@dr
  ^-  shed:khan
  =>  :*  at=block-id.ord.state  wait=wait
          req-to=(need req-to.state)  blocks=blocks
          sio=strandio:btcio  bio=btcio  bc=bc
          ..zuse
      ==
  =|  bocs=blocks
  =/  m  (strand:sio ,vase)
  |-  ^-  form:m
  ;<  *  bind:m  (sleep:sio wait)
  ;<  lat=(unit @ud)  bind:m  (get-block-count:bio req-to ~)
  ?~  lat  ~|  %ordw-get-block-count-failed  !!
  =/  to  (sub u.lat 6)
  |-  ^-  form:m
  ?.  (lth num.at to)  (pure:m !>((flop bocs)))
  ;<  boc=(unit block:bc)  bind:m
    (get-block-by-number:bio req-to ~ u.lat)
  ?~  boc  ~|  %ordw-get-block-by-number-failed  !!
  $(at [hax.u.boc u.lat], bocs [u.lat u.boc]^bocs)
::
++  ted-start
  |=  wait=@dr
  ^-  (quip card _state)
  =/  tid=@ta
    (cat 3 'ord-watcher--' (scot %uv eny.bol))
  :_  state(ted `[now.bol tid])
  =/  args  [~ `tid bec get-blocks]
  :~  (watch-spider /watcher-ted our.bol /thread-result/[tid])
      (poke-spider /watcher-ted our.bol %spider-inline !>(args))
  ==
::
++  handle-blocks
  |=  blocks=(list [=num:block:bc block:bc])
  =/  oc  (abed:ord-core:ord ord.state)
  |-  ^-  (quip card _state)
  ?~  blocks  =^(fx ord.state abet:oc (handle-fx fx))
  =.  oc  (handle-block:oc i.blocks)
  $(blocks t.blocks)
::
++  handle-fx
  |=  fx=(list [id:block:bc effect:ord])
  ^-  (quip card _state)
  :_  state
  (jael-update (fx-to-udiffs fx))
::
++  jael-update
  |=  =udiffs:point
  ^-  (list card)
  :-  [%give %fact ~[/] %groundwire-udiffs !>(udiffs)]
  |-  ^-  (list card)
  ?~  udiffs
    ~
  ?.  (~(has in whos.state) ship.i.udiffs)  ~
  =/  =path  /(scot %p ship.i.udiffs)
  ::  Should really give all diffs involving each ship at the same time
  ::
  :-  [%give %fact ~[path] %groundwire-udiffs !>(~[i.udiffs])]
  $(udiffs t.udiffs)
::
++  fx-to-udiffs
  |=  fx=(list [=id:block:bc effect:ord])
  ^-  udiffs:point
  ?~  fx  ~
  ?.  ?=(%point +<.i.fx)  $(fx t.fx)
  =,  i.fx
  ?+  +>+<.i.fx  $(fx t.fx)
    %rift     [ship id %rift rift |]^$(fx t.fx)
    %sponsor  [ship id %spon sponsor]^$(fx t.fx)
    %keys     [ship id %keys [life (sub (end 3 pass) 'a') pass] %.y]^$(fx t.fx)
    %fief     [ship id %fief fief]^$(fx t.fx)
  ==
::
++  run-state
  =/  points=(list [=ship point:ord])  ~(tap by unv-ids.ord.state)
  =/  =id:block:jael  block-id:ord.state
  |-  ^-  udiffs:point
  ^-  (list [@p udiff:point])
  ?~  points  ~
  =,  i.points
  :*  [ship id %keys [life.net (sub (end 3 pass.net) 'a') pass.net] %.y]
      [ship id %rift rift.net %.y]
      [ship id %spon ?:(has.sponsor.net `who.sponsor.net ~)]
      [ship id %fief fief.net]
      $(points t.points)
  ==
--
