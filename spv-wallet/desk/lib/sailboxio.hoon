/+  server, multipart, sailbox, html-utils
=|  hold=_| :: switch to interleave vs sequentialize processes
|%
++  fiber  fiber:fiber:sailbox
++  input  input:fiber:sailbox
::
++  send-raw-cards
  |=  cards=(list card:sailbox)
  =/  m  (fiber ,~)
  ^-  form:m
  |=  input
  [cards state %done ~]
::
++  send-raw-card
  |=  =card:sailbox
  =/  m  (fiber ,~)
  ^-  form:m
  (send-raw-cards card ~)
::
++  fiber-fail
  |=  err=tang
  |=  input
  [~ state %fail err]
::
++  get-state
  =/  m  (fiber ,vase)
  ^-  form:m
  |=  input
  [~ state %done state]
::
++  get-state-as
  |*  a=mold
  =/  m  (fiber ,a)
  ^-  form:m
  |=  input
  [~ state %done !<(a state)]
::
++  replace
  |=  new=vase
  =/  m  (fiber ,~)
  ^-  form:m
  |=  input
  ^-  output:m
  [~ new %done ~]
::
++  get-bowl
  =/  m  (fiber ,bowl:gall)
  ^-  form:m
  |=  input
  [~ state %done bowl]
::
++  get-beak
  =/  m  (fiber ,beak)
  ^-  form:m
  |=  input
  [~ state %done byk.bowl]
::
++  get-time
  =/  m  (fiber ,@da)
  ^-  form:m
  |=  input
  [~ state %done now.bowl]
::
++  get-our
  =/  m  (fiber ,ship)
  ^-  form:m
  |=  input
  [~ state %done our.bowl]
::
++  get-agent
  =/  m  (fiber ,dude:gall)
  ^-  form:m
  |=  input
  [~ state %done dap.bowl]
::
++  get-entropy
  =/  m  (fiber ,@uvJ)
  ^-  form:m
  |=  input
  [~ state %done eny.bowl]
::
++  get-guest
  =/  m  (fiber ,ship)
  ^-  form:m
  |=  input
  [~ state %done src.bowl]
::
++  get-provenance
  =/  m  (fiber ,path)
  ^-  form:m
  |=  input
  [~ state %done sap.bowl]
::
++  get-pid
  =/  m  (fiber ,@ta)
  ^-  form:m
  |=  input
  [~ state %done pid]
::
++  get-poke
  =/  m  (fiber ,cage)
  ^-  form:m
  |=  input
  [~ state %done cage.poke]
::
++  get-poke-guest
  =/  m  (fiber ,ship)
  ^-  form:m
  |=  input
  [~ state %done src.poke]
::
++  get-poke-provenance
  =/  m  (fiber ,path)
  ^-  form:m
  |=  input
  [~ state %done sap.poke]
::
++  soften
  |*  a=mold
  =/  m  (fiber ,a)
  =/  m-e  (fiber ,(each a tang))
  |=  =form:m
  ^-  form:m-e
  |=  tin=input
  =/  res  (form tin)
  %=    res
      next
    ?+  -.next.res  next.res
      %cont  [%cont ^$(form self.next.res)]
      %fail  [%done %| err.next.res]
      %done  [%done %& value.next.res]
    ==
  ==
::
++  unit-soften
  |*  a=mold
  =/  m  (fiber ,a)
  =/  m-u  (fiber ,(unit a))
  |=  =form:m
  ^-  form:m-u
  |=  tin=input
  =/  res  (form tin)
  %=    res
      next
    ?+  -.next.res  next.res
      %cont  [%cont ^$(form self.next.res)]
      %fail  [%done ~]
      %done  [%done ~ value.next.res]
    ==
  ==
::
++  take-bump
  =/  m  (fiber ,cage)
  ^-  form:m
  |=  input
  :+  ~  state
  ?+  in  [%skip hold]
     ~             [%wait hold]
    [~ %bump @ *]  [%done cage.u.in]
  ==
::
++  take-poke-ack
  |=  =wire
  =/  m  (fiber ,~)
  ^-  form:m
  |=  input
  :+  ~  state
  ?+  in  [%skip hold]
      ~  [%wait hold]
      [~ %agent * %poke-ack *]
    ?.  =(wire wire.u.in)
      [%skip hold]
    ?~  p.sign.u.in
      [%done ~]
    [%fail %poke-fail u.p.sign.u.in]
  ==
::
++  take-watch-ack
  |=  =wire
  =/  m  (fiber ,~)
  ^-  form:m
  |=  input
  :+  ~  state
  ?+  in  [%skip hold]
      ~  [%wait hold]
      [~ %agent * %watch-ack *]
    ?.  =(watch+wire wire.u.in)
      [%skip hold]
    ?~  p.sign.u.in
      [%done ~]
    [%fail %watch-ack-fail u.p.sign.u.in]
  ==
::  Wait for a subscription update on a wire
::
++  take-fact
  |=  =wire
  =/  m  (fiber ,cage)
  ^-  form:m
  |=  input
  :+  ~  state
  ?+  in  [%skip hold]
      ~  [%wait hold]
      [~ %agent * %fact *]
    ?.  =(watch+wire wire.u.in)
      [%skip hold]
    [%done cage.sign.u.in]
  ==
::  Wait for a subscription close
::
++  take-kick
  |=  =wire
  =/  m  (fiber ,~)
  ^-  form:m
  |=  input
  :+  ~  state
  ?+  in  [%skip hold]
      ~  [%wait hold]
      [~ %agent * %kick *]
    ?.  =(watch+wire wire.u.in)
      [%skip hold]
    [%done ~]
  ==
::
++  watch
  |=  [=wire =dock =path]
  =/  m  (fiber ,~)
  ^-  form:m
  =/  =card:agent:gall  [%pass watch+wire %agent dock %watch path]
  ;<  ~  bind:m  (send-raw-card card)
  (take-watch-ack wire)
::
++  watch-one
  |=  [=wire =dock =path]
  =/  m  (fiber ,cage)
  ^-  form:m
  ;<  ~  bind:m  (watch wire dock path)
  ;<  =cage  bind:m  (take-fact wire)
  ;<  ~  bind:m  (take-kick wire)
  (pure:m cage)
::
++  watch-our
  |=  [=wire =term =path]
  =/  m  (fiber ,~)
  ^-  form:m
  ;<  our=@p  bind:m  get-our
  (watch wire [our term] path)
::
++  scry
  |*  [=mold =path]
  =/  m  (fiber ,mold)
  ^-  form:m
  ?>  ?=(^ path)
  ?>  ?=(^ t.path)
  ;<  =bowl:gall  bind:m  get-bowl
  %-  pure:m
  .^(mold i.path (scot %p our.bowl) i.t.path (scot %da now.bowl) t.t.path)
::
++  get-all-bindings
  =/  m  (fiber ,(list [binding:eyre duct action:eyre]))
  ^-  form:m
  (scry ,(list [binding:eyre duct action:eyre]) %e /bindings)
::
++  get-our-bindings
  =/  m  (fiber ,(list binding:eyre))
  ^-  form:m
  ;<  bindings=(list [binding:eyre duct action:eyre])  bind:m  get-all-bindings
  ;<  =bowl:gall  bind:m  get-bowl
  %-  pure:m
  %+  murn  bindings
  |=  [=binding:eyre =duct =action:eyre]
  ^-  (unit binding:eyre)
  ?.  ?=(%app -.action)
    ~
  ?.  =(app.action dap.bowl)
    ~
  `binding
::
++  take-eyre-bound
  |=  =wire
  =/  m  (fiber ,~)
  ^-  form:m
  |=  input
  :+  ~  state
  ?+  in  [%skip hold]
      ~  [%wait hold]
      [~ %arvo * %eyre %bound *]
    ?.  =(wire wire.u.in)
      [%skip hold]
    ?:  accepted.sign.u.in
      [%done ~]
    [%fail leaf+"eyre bind failed on wire {(spud wire)} with binding {<binding.sign.u.in>}" ~]
  ==
::
++  eyre-connect
  |=  [=binding:eyre app=term]
  =/  m  (fiber ,~)
  ^-  form:m
  =/  =card:agent:gall
    [%pass /eyre/connect %arvo %e %connect binding app]
  ;<  ~  bind:m  (send-raw-card card)
  (take-eyre-bound /eyre/connect)
::
++  eyre-disconnect
  |=  =binding:eyre
  =/  m  (fiber ,~)
  ^-  form:m
  %-  send-raw-card
  :: NOTE: wire must be same as connecting for identical duct
  [%pass /eyre/connect %arvo %e %disconnect binding]
::
++  add-bindings
  |=  bindings=(list binding:eyre)
  =/  m  (fiber ,~)
  ^-  form:m
  ;<  =bowl:gall  bind:m  get-bowl
  |-  ^-  form:m
  ?~  bindings
    (pure:m ~)
  ;<  ~  bind:m  (eyre-connect i.bindings dap.bowl)
  $(bindings t.bindings)
::
++  set-bindings
  |=  desired=(list binding:eyre)
  =/  m  (fiber ,~)
  ^-  form:m
  ::  connect to all desired bindings (overwrites any from other ducts)
  ;<  ~  bind:m  (add-bindings desired)
  ;<  current=(list binding:eyre)  bind:m  get-our-bindings
  ::  disconnect from bindings not in desired list
  =/  to-disconnect=(list binding:eyre)
    %+  skip  current
    |=  =binding:eyre
    (~(has in (~(gas in *(set binding:eyre)) desired)) binding)
  ;<  ~  bind:m  (add-bindings to-disconnect)
  |-  ^-  form:m
  ?~  to-disconnect
    (pure:m ~)
  ;<  ~  bind:m  (eyre-disconnect i.to-disconnect)
  $(to-disconnect t.to-disconnect)
::
++  clear-bindings
  |=  bindings=(list binding:eyre)
  =/  m  (fiber ,~)
  ^-  form:m
  ;<  =bowl:gall  bind:m  get-bowl
  ::  connect first to overwrite any bindings from other ducts
  ;<  ~  bind:m  (add-bindings bindings)
  ::  now disconnect
  |-  ^-  form:m
  ?~  bindings
    (pure:m ~)
  ;<  ~  bind:m  (eyre-disconnect i.bindings)
  $(bindings t.bindings)
::
++  clear-all-bindings
  =/  m  (fiber ,~)
  ^-  form:m
  ;<  current=(list binding:eyre)  bind:m  get-our-bindings
  (clear-bindings current)
::
++  leave
  |=  [=wire =dock]
  =/  m  (fiber ,~)
  ^-  form:m
  =/  =card:agent:gall  [%pass watch+wire %agent dock %leave ~]
  (send-raw-card card)
::
++  leave-our
  |=  [=wire =term]
  =/  m  (fiber ,~)
  ^-  form:m
  ;<  our=@p  bind:m  get-our
  (leave wire [our term])
::
++  poke
  |=  [=dock =cage]
  =/  m  (fiber ,~)
  ^-  form:m
  =/  =card:agent:gall  [%pass /poke %agent dock %poke cage]
  ;<  ~  bind:m  (send-raw-card card)
  (take-poke-ack /poke)
::
++  fiber-bump
  |=  [pid=@ta bump=cage]
  =/  m  (fiber ,~)
  ^-  form:m
  ;<  =bowl:gall  bind:m  get-bowl
  (poke [our dap]:bowl fiber-bump+!>([pid bump]))
::
++  fiber-kill
  |=  pid=@ta
  =/  m  (fiber ,~)
  ^-  form:m
  ;<  =bowl:gall  bind:m  get-bowl
  (poke [our dap]:bowl fiber-kill+!>(pid))
::
++  fiber-poke
  |=  [sfx=@ta foke=cage]
  =/  m  (fiber ,~)
  ^-  form:m
  ;<  pid=@ta  bind:m  get-pid
  =/  nid=@ta  (rap 3 pid '_' sfx ~)
  ;<  =bowl:gall  bind:m  get-bowl
  =/  =dock  [our dap]:bowl
  ;<  ~  bind:m  (watch /fiber-result dock /fiber-result/[nid])
  ;<  ~  bind:m  (poke dock fiber-poke+!>([nid foke]))
  ;<  res=cage  bind:m  (take-fact /fiber-result)
  ;<  ~  bind:m  (take-kick /fiber-result)
  ?>  ?=(%fiber-ack p.res)
  =+  !<(err=(unit tang) q.res)
  ?~  err
    (pure:m ~)
  (fiber-fail u.err)
::
++  give-simple-payload
  |=  payload=simple-payload:http
  =/  m  (fiber ,~)
  ^-  form:m
  (send-raw-card %simple-payload payload)
::
++  send-sse-event
  |=  [site=(list @t) =sse-key:sailbox]
  =/  m  (fiber ,~)
  ^-  form:m
  (send-raw-card %sse site ~ sse-key)
::
++  send-sse-events
  |=  [site=(list @t) keys=(list sse-key:sailbox)]
  =/  m  (fiber ,~)
  ^-  form:m
  %-  send-raw-cards
  %+  turn  keys
  |=  =sse-key:sailbox
  [%sse site ~ sse-key]
::
++  send-wait
  |=  until=@da
  =/  m  (fiber ,~)
  ^-  form:m
  %-  send-raw-card
  [%pass /wait/(scot %da until) %arvo %b %wait until]
::
++  wait
  |=  until=@da
  =/  m  (fiber ,~)
  ^-  form:m
  ;<  ~  bind:m  (send-wait until)
  (take-wake `until)
::
++  take-wake
  |=  until=(unit @da)
  =/  m  (fiber ,~)
  ^-  form:m
  |=  input
  :+  ~  state
  ?+  in  [%skip hold]
    ~  [%wait hold]
      [~ %arvo [%wait @ ~] %behn %wake *]
    ?.  |(?=(~ until) =(`u.until (slaw %da i.t.wire.u.in)))
      [%skip hold]
    ?~  error.sign.u.in
      [%done ~]
    [%fail leaf+"timer-error" u.error.sign.u.in]
  ==
::
++  sleep
  |=  for=@dr
  =/  m  (fiber ,~)
  ^-  form:m
  ;<  now=@da  bind:m  get-time
  (wait (add now for))
::
++  backoff
  |=  [try=@ud limit=@dr]
  =/  m  (fiber ,~)
  ^-  form:m
  ;<  eny=@uvJ  bind:m  get-entropy
  %-  sleep
  %+  min  limit
  ?:  =(0 try)  ~s0
  %+  add
    (mul ~s1 (bex (dec try)))
  (mul ~s0..0001 (~(rad og eny) 1.000))
::
++  retry
  |*  result=mold
  |=  [crash-after=(unit @ud) computation=_*form:(fiber result)]
  =/  m  (fiber ,result)
  =|  try=@ud
  |-
  ^-  form:m
  ?:  =(crash-after `try)
    ;<  ~  bind:m  (flog-text "retry: giving up after {<try>} attempts")
    (fiber-fail %retry-too-many ~)
  ::  log retry attempts (skip first attempt, that's just normal)
  ::
  ~?  (gth try 0)  "retry: attempt {<try>}/{<crash-after>}"
  ;<  ~                  bind:m  (backoff try ~s10)
  ;<  res=(unit result)  bind:m  ((unit-soften ,result) computation)
  ?~  res
    $(try +(try))
  (pure:m u.res)
::
++  send-request
  |=  =request:http
  =/  m  (fiber ,~)
  ^-  form:m
  (send-raw-card %pass /request %arvo %i %request request *outbound-config:iris)
::
++  send-cancel-request
  =/  m  (fiber ,~)
  ^-  form:m
  (send-raw-card %pass /request %arvo %i %cancel-request ~)
::
++  take-client-response
  =/  m  (fiber ,client-response:iris)
  ^-  form:m
  |=  input
  :+  ~  state
  ?+  in  [%skip hold]
      ~  [%wait hold]
    ::
      [~ %arvo [%request ~] %iris %http-response %cancel *]
    ::NOTE  iris does not (yet?) retry after cancel, so it means failure
    :+  %fail
      leaf+"http-request-cancelled"
    ['http request was cancelled by the runtime']~
    ::
      [~ %arvo [%request ~] %iris %http-response %finished *]
    [%done client-response.sign.u.in]
  ==
::
++  extract-body
  |=  =client-response:iris
  =/  m  (fiber ,cord)
  ^-  form:m
  ?>  ?=(%finished -.client-response)
  %-  pure:m
  ?~  full-file.client-response  ''
  q.data.u.full-file.client-response
::
++  fetch-cord
  |=  url=tape
  =/  m  (fiber ,cord)
  ^-  form:m
  =/  =request:http  [%'GET' (crip url) ~ ~]
  ;<  ~                      bind:m  (send-request request)
  ;<  =client-response:iris  bind:m  take-client-response
  ?>  ?=(%finished -.client-response)
  =/  status  status-code.response-header.client-response
  ::  rate-limited: log and fail so +retry can backoff
  ::
  ?:  =(429 status)
    ;<  ~  bind:m  (flog-text "fetch: 429 rate limited on {url}")
    (fiber-fail leaf+"http-rate-limited" ~)
  ::  any non-2xx: fail with status code
  ::
  ?.  =(2 (div status 100))
    ;<  ~  bind:m  (flog-text "fetch: HTTP {<status>} on {url}")
    (fiber-fail leaf+"http-error-{<status>}" ~)
  (extract-body client-response)
::
++  fetch-json
  |=  url=tape
  =/  m  (fiber ,json)
  ^-  form:m
  ;<  =cord  bind:m  (fetch-cord url)
  =/  json=(unit json)  (de:json:html cord)
  ?~  json
    (fiber-fail leaf+"json-parse-error" ~)
  (pure:m u.json)
::    ----
::
::  Output
::
++  flog
  |=  =flog:dill
  =/  m  (fiber ,~)
  ^-  form:m
  (send-raw-card %pass / %arvo %d %flog flog)
::
++  flog-text
  |=  =tape
  =/  m  (fiber ,~)
  ^-  form:m
  (flog %text tape)
::
++  flog-tang
  |=  =tang
  =/  m  (fiber ,~)
  ^-  form:m
  =/  =wall
    (zing (turn (flop tang) (cury wash [0 80])))
  |-  ^-  form:m
  ?~  wall
    (pure:m ~)
  ;<  ~  bind:m  (flog-text i.wall)
  $(wall t.wall)
::
++  trace
  |=  =tang
  =/  m  (fiber ,~)
  ^-  form:m
  (pure:m ((slog tang) ~))
--
