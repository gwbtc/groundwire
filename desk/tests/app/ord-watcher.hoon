/-  spider, bitcoin, urb, ord, ow=ord-watcher
/+  tag=test-agent, btcio, btcl=bitcoin, ol=ord
/=  mock-agent  /app/ord-watcher
=>
::
::  def
|%
++  dap   %mock-ord-watcher
++  dek   %groundwire
++  doc   [~zod dap]
::
++  mock-req-to
  ^-  req-to:btcio
  :-  'http://localhost:18443'
  [%basic 'bitcoinrpc:bitcoinrpc']
::
+$  card  card:agent:gall
--
::
::  tests
|%
++  test-init-cards
  %-  eval-mare:tag
  =/  m  (mare:tag ,~)
  ^-  form:m
::  ;<  *                bind:m  (jab-bowl:tag |=(b=bowl:gall b(our ~zod)))
::  ;<  =bowl:gall       bind:m  get-bowl:tag
  ;<  caz=(list card)  bind:m  (do-init:tag dap mock-agent)
  %+  ex-cards:tag
    caz
  :~  (ex-arvo:tag /lo %j %listen *(set ship) [%.n dap])
  ==
::
++  test-init-state
  %-  eval-mare:tag
  =/  m  (mare:tag ,~)
  ^-  form:m
  ;<  *  bind:m  (do-init:tag dap mock-agent)
  ;<  =vase  bind:m  get-save:tag
  =/  st=state-0:ow  !<(state-0:ow vase)
  ?.  =(start.st start-height:urb)
    ~|  'start.state not initializsed to constant'
    !!
  ?.  =(block-id.ord-state.st [start-hash:urb start-height:urb])
    ~|  'block-id.ord-state not initialized to constants'
    !!
  (pure:m ~)
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
::  XX can't test without calling +get-blocks
::     should probably be in /lib/ord's +ord-core
::
::++  test-poke-action-start
::  %-  eval-mare:tag
::  =/  m  (mare:tag ,~)
::  ^-  form:m
::  ;<  *                bind:m  (do-init:tag dap mock-agent)
::  ;<  caz=(list card)  bind:m  (do-poke:tag %action !>([%start start-height:urb]))
::  ;<  =vase  bind:m  get-save:tag
::  =/  st=state-0:ow  !<(state-0:ow vase)
::  %+  ex-cards:tag
::    caz
::  %-  ex-card:tag
::  :*  %pass
::      /got-blocks
::      %arvo
::      %k
::      %lard
::      dek
::      %:  get-blocks
::          req-to.st
::          ord-state.st
::          start-height:urb
::      ==
::  ==
::
++  test-poke-action-config-rpc
  %-  eval-mare:tag
  =/  m  (mare:tag ,~)
  ^-  form:m
  ;<  *      bind:m  (do-init:tag dap mock-agent)
  ;<  *      bind:m  (do-poke:tag %action !>([%config-rpc mock-req-to]))
  ;<  =vase  bind:m  get-save:tag
  =/  st=state-0:ow  !<(state-0:ow vase)
  ?>  =(req-to.st `mock-req-to)
  (pure:m ~)
::
++  test-poke-debug-clear-oc
  %-  eval-mare:tag
  =/  m  (mare:tag ,~)
  ^-  form:m
  ;<  *      bind:m  (do-init:tag dap mock-agent)
  ;<  *      bind:m  (do-poke:tag %debug !>([%clear-oc ~]))
  ;<  =vase  bind:m  get-save:tag
  =/  st=state-0:ow  !<(state-0:ow vase)
  =/  clear-ord  *state:ord
  =.  block-id.clear-ord  [start-hash:urb start-height:urb]
  ?>  =(ord-state.st clear-ord)
  (pure:m ~)
::
++  test-on-watch-ship
  %-  eval-mare:tag
  =/  m  (mare:tag ,~)
  ^-  form:m
  ;<  *      bind:m  (do-init:tag dap mock-agent)
  ::  XX could test caz too
  ::     but need to do a bunch of fake oc stuff
  ;<  *      bind:m  (do-watch:tag /(scot %p ~zod))
  ;<  =vase  bind:m  get-save:tag
  =/  st=state-0:ow  !<(state-0:ow vase)
  ?>  (~(has in whos.st) ~zod)
  (pure:m ~)
::
::  XX +test-on-arvo-got-blocks
::     would need to define:
::       mock blocks
::       run blocks through mock oc
::       run mock oc state and fx through +on-arvo
--
