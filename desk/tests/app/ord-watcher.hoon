/-  spider, bitcoin
/+  tag=test-agent, btclib=bitcoin, btcio, ord-lib=ord
/=  mock-agent  /app/ord-watcher
=>
::
::  def
|%
++  dap   %ord-watcher-test
++  doc   [~zod dap]
+$  card  card:agent:gall
::
::  XX $app-state-0 should be defined in a /sur/ord-watcher
::     so we don't have to define it here a second time
+$  app-state-0
  $:  %0
      ord=state:ord-lib
      start=(unit num:block:btclib)
      whos=(set ship)
      ted=(unit [since=@da =tid:spider])
      req-to=(unit req-to:btcio)
==
::
--
::
::  tests
|%
++  test-init
  %-  eval-mare:tag
  =/  m  (mare:tag ,~)
  ^-  form:m
  ;<  caz=(list card)  bind:m  (do-init:tag dap mock-agent)
  (pure:m ~)
::
++  test-poke-clear
  %-  eval-mare:tag
  =/  m  (mare:tag ,~)
  ^-  form:m
  ;<  *                bind:m  (do-init:tag dap mock-agent)
  ;<  ~                bind:m  (set-src:tag ~zod)
  ;<  caz=(list card)  bind:m  (do-poke:tag %noun !>(%clear))
  %+  ex-cards:tag
    caz
  :~  (ex-task:tag /running/watcher-ted [~zod %spider] %leave ~)
  ==
::
++  test-load
  %-  eval-mare:tag
  =/  m  (mare:tag ,~)
  ^-  form:m
  ;<  *      bind:m  (do-init:tag dap mock-agent)
  ;<  =vase  bind:m  get-save:tag
  ;<  *      bind:m  (do-load:tag mock-agent `vase)
  (pure:m ~)
::
++  test-watch-zod
  %-  eval-mare:tag
  =/  m  (mare:tag ,~)
  ^-  form:m
  ;<  *                bind:m  (do-init:tag dap mock-agent)
  ;<  caz=(list card)  bind:m  (do-watch:tag /~zod)
  ;<  =vase            bind:m  get-save:tag
  =/  =app-state-0
    !<(app-state-0 vase)
  ?.  (~(has in whos.app-state-0) ~zod)
    ~|  '~zod not added to whos.app-state-0'
    !!
  %+  ex-cards:tag
    caz
  :~  (ex-fact:tag ~[/] %groundwire-udiffs !>(*udiffs:point:jael))
  ==
::
++  test-agent-running-poke-ack
  %-  eval-mare:tag
  =/  m  (mare:tag ,~)
  ^-  form:m
  ;<  *  bind:m  (do-init:tag dap mock-agent)
  ;<  *  bind:m  (do-agent:tag /running/get-blocks doc %poke-ack ~)
  (pure:m ~)
::
::  ++  test-agent-running-poke-nack
::    %-  eval-mare:tag
::    =/  m  (mare:tag ,~)
::    ^-  form:m
::    ;<  *                bind:m  (do-init:tag dap mock-agent)
::    ;<  *                bind:m  (jab-bowl:tag |=(b=bowl:gall b(our ~zod)))
::    ;<  caz=(list card)  bind:m  (do-agent:tag /running/get-blocks doc %poke-ack (some ['test tank']~))
::    %+  ex-cards:tag
::      caz
::    :~  (ex-task:tag /running/get-blocks [~zod %spider] %leave ~)
::        (ex-task:tag /running/watcher-ted [~zod %spider] %watch /thread-result/ord-watcher--0v0)
::        (ex-poke:tag /running/watcher-ted [~zod %spider] %spider-inline !>([~ `~.ord-watcher--0v0 [%da ~2000.1.1] /btc/get-blocks]))
::    ==
::
::  XX test %watch-ack, %kick, and %fact code branches
::
--
