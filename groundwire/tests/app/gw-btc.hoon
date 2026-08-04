/-  bitcoin, ord, urb, sa=self-attestation, lc=light-client
/+  tag=test-agent, btcio, bc=bitcoin, cc=gw-btc-pass,
    lca=lc-attestation, ol=ord, strandio
/=  mock-agent  /app/gw-btc
=>
::
::  Test-local copies of the app's unversioned state molds.  Keeping these
::  here makes every save/load assertion explicit without introducing a
::  production state migration surface solely for tests.
|%
+$  pending-writ
  $:  dom=@tas
      =pass
      sat=self-attestation:sa
  ==
+$  inflight-job
  $:  job=@uv
      token=@uv
      context=@ux
      epoch=@uv
  ==
+$  gw-state
  $:  rpc=req-to:btcio
      urb-state=state:urb
      ready=?
      best=(unit id:block:bc)
      pending=(map ship pending-writ)
      inflight=(map ship inflight-job)
      confidential=(set ship)
      attested=(map ship sont:ord)
      publicizing=(set ship)
      next-job=@uv
      chain-epoch=@uv
  ==
+$  card  card:agent:gall
--
::
::  Fixtures
|%
++  dap  %mock-gw-btc
++  mock-req-to
  ^-  req-to:btcio
  ['http://localhost:18443' [%basic 'bitcoinrpc:bitcoinrpc']]
::
++  fixture-spawn
  ^-  sont:ord
  [0xfeed.face 2 17]
::
++  custody-a
  ^-  custody-log:sa
  ~[[0xaaaa.bbbb 943.140 ~]]
::
++  custody-b
  ^-  custody-log:sa
  ~[[0xdead.beef 943.141 ~]]
::
++  make-pass-raw
  |=  [seed=@ xtr=@]
  ^-  pass
  =<  pub:ex
  %:  pit:nu:cric:crypto
      512
      (shaz seed)
      %c
      (make-dat:cc fixture-spawn)
      xtr
  ==
::
++  make-pass
  |=  [seed=@ chain=custody-log:sa]
  ^-  pass
  (make-pass-raw seed (jam chain))
::
++  pass-a  (make-pass 'gw-btc-test-key' custody-a)
++  pass-b  (make-pass 'gw-btc-test-key' custody-b)
++  public-pass  (make-pass-raw 'gw-btc-public-key' 0)
++  public-comet  (comet-for public-pass)
++  empty-chain-pass  (make-pass 'gw-btc-empty-chain' ~)
++  empty-chain-comet  (comet-for empty-chain-pass)
++  comet-for
  |=  =pass
  =/  cic  (com:nu:cric:crypto pass)
  `@p`fig:ex:cic
::
++  comet  (comet-for pass-a)
::
++  pass-at
  |=  i=@ud
  ^-  pass
  (make-pass (add 1.000 i) custody-a)
::
++  request-token
  |=  =pass
  ^-  @uv
  (sham [%gw-btc pass])
::
++  context-token
  |=  [st=state:urb ats=(map ship sont:ord)]
  ^-  @ux
  (shax (jam [block-id.st unv-ids.st ats]))
::
++  best-id
  ^-  id:block:bc
  [0xbeef 943.200]
::
++  verification-timeout  ~m2
::
++  blank-state
  ^-  gw-state
  :*  mock-req-to
      *state:urb
      |  ~  ~  ~  ~  ~  ~  `@uv`0  `@uv`0
  ==
::
++  public-ship  ~nec
::
++  fixture-point
  |=  [=pass sponsor=[has=? who=ship]]
  ^-  point:urb
  :*  [fixture-spawn ~]
      2
      3
      pass
      sponsor
      ~
      ~
  ==
::
++  point-at
  |=  [=pass sponsor=[has=? who=ship] tip=sont:ord]
  ^-  point:urb
  =/  point  (fixture-point pass sponsor)
  point(sont.own tip)
::
++  put-point
  |=  [st=state:urb who=ship point=point:urb value=@ud]
  ^-  state:urb
  =.  unv-ids.st  (~(put by unv-ids.st) who point)
  ?:  =([0x0 0 0] sont.own.point)
    st
  =.  sont-map.st
    %:  put-com:si:ol
        sont-map.st
        txid.sont.own.point
        vout.sont.own.point
        off.sont.own.point
        value
        who
    ==
  st
::
++  one-point-state
  |=  [block=id:block:bc who=ship =pass tip=sont:ord]
  ^-  state:urb
  =/  st  *state:urb
  =.  block-id.st  block
  (put-point st who (point-at pass [& public-ship] tip) 10.000)
::
++  block-sign
  |=  $:  base=state:urb
          fx=(list [id:block:bc effect:urb])
          result=state:urb
      ==
  ^-  sign-arvo
  [%khan %arow %.y %noun !>([base [fx result]])]
::
++  block-base-id  ^-  id:block:bc  [0x1001 943.210]
++  block-next-id  ^-  id:block:bc  [0x1002 943.211]
++  base-tip  ^-  sont:ord  [0xaaaa.0001 0 0]
++  live-tip  ^-  sont:ord  [0xbbbb.0002 0 0]
++  scan-tip  ^-  sont:ord  [0xcccc.0003 0 0]
++  public-tip  ^-  sont:ord  [0xdddd.0004 0 0]
++  verified-tip  ^-  sont:ord  [0xeeee.0005 1 7]
++  fixture-insc  ^-  insc:ord  [0x1111.2222 0]
++  fixture-mail  ^-  mail:ord  *mail:ord
::
++  inscription-urb-state
  ^-  state:urb
  =/  sat=sont:ord  fixture-spawn
  =/  point  (point-at pass-a [& public-ship] sat)
  =/  st  *state:urb
  =.  block-id.st  [0xcafe 943.199]
  =.  unv-ids.st  (~(put by unv-ids.st) comet point)
  =.  sont-map.st
    %:  put-all:si:ol
        sont-map.st
        txid.sat
        vout.sat
        off.sat
        12.345
        `comet
        (~(put in *(set insc:ord)) fixture-insc)
    ==
  =.  insc-ids.st
    (~(put by insc-ids.st) fixture-insc [sat fixture-mail])
  st
::
++  inscription-state
  ^-  gw-state
  =/  urb-state  inscription-urb-state
  =/  pending
    (~(put by *(map ship pending-writ)) comet [%gw-btc pass-a [comet fixture-spawn custody-a]])
  =/  attested  (~(put by *(map ship sont:ord)) comet fixture-spawn)
  =/  inflight
    (~(put by *(map ship inflight-job)) comet [`@uv`0 (request-token pass-a) (context-token urb-state attested) `@uv`0])
  :*  mock-req-to
      urb-state
      &
      `best-id
      pending
      inflight
      (~(put in *(set ship)) comet)
      attested
      ~
      `@uv`1
      `@uv`0
  ==
::
++  inscription-snapshot-state
  ^-  gw-state
  =/  st  fixture-state
  st(urb-state inscription-urb-state)
::
++  occupied-tip-state
  ^-  gw-state
  =/  urb-state  inscription-urb-state
  =.  urb-state
    (put-point urb-state public-ship (point-at pass-b [| public-ship] verified-tip) 98.765)
  =/  pending
    (~(put by *(map ship pending-writ)) comet [%gw-btc pass-a [comet fixture-spawn custody-a]])
  =/  attested  (~(put by *(map ship sont:ord)) comet fixture-spawn)
  =/  inflight
    (~(put by *(map ship inflight-job)) comet [`@uv`0 (request-token pass-a) (context-token urb-state attested) `@uv`0])
  :*  mock-req-to
      urb-state
      &
      `best-id
      pending
      inflight
      (~(put in *(set ship)) comet)
      attested
      ~
      `@uv`1
      `@uv`0
  ==
::
++  fixture-urb-state
  ^-  state:urb
  =/  ids  *unv-ids:urb
  =.  ids
    (~(put by ids) public-ship (fixture-point pass-a [| ~bud]))
  =.  ids
    (~(put by ids) comet (fixture-point pass-a [& public-ship]))
  :*  [0xcafe 943.199]
      *sont-map:ord
      *insc-ids:ord
      ids
  ==
::
++  fixture-state
  ^-  gw-state
  =/  confidential  (~(put in *(set ship)) comet)
  :*  mock-req-to
      fixture-urb-state
      &
      `best-id
      ~
      ~
      confidential
      ~
      ~
      `@uv`0
      `@uv`0
  ==
::
++  current-anew-state
  ^-  gw-state
  =/  st  fixture-state
  st(attested (~(put by attested.st) comet fixture-spawn))
::
++  stale-anew-state
  ^-  gw-state
  =/  st  fixture-state
  st(attested (~(put by attested.st) comet [0xfeed.face 2 18]))
::
::  Drive distinct writes through the public Gall poke interface.  Cards are
::  intentionally ignored here: final inflight membership proves which
::  requests launch, while the caller checks the first request over the cap.
++  poke-ready-prefix
  |=  [i=@ud end=@ud]
  =/  m  (mare:tag ,~)
  ^-  form:m
  ?:  =(i end)
    (pure:m ~)
  =/  pas  (pass-at i)
  =/  who  (comet-for pas)
  ;<  *  bind:m
    (do-poke:tag %noun !>(`jael-poke:urb`[%jael-writ %gw-btc who pas]))
  (poke-ready-prefix +(i) end)
::
++  filler-inflight
  ^-  (map ship inflight-job)
  =|  i=@ud
  =|  acc=(map ship inflight-job)
  |-
  ?:  =(15 i)
    acc
  %=  $
    i    +(i)
    acc  (~(put by acc) `@p`(add 100 i) [`@uv`i `@uv`i `@ux`0 `@uv`0])
  ==
::
++  dependent-pass  (pass-at 99)
++  dependent-comet  (comet-for dependent-pass)
++  dependent-sat
  ^-  self-attestation:sa
  [dependent-comet fixture-spawn custody-a]
::
++  dependent-verify-card
  |=  [byk=desk job=@uv sponsors=unv-ids:urb]
  ^-  card
  :*  %pass
      /verify/(scot %p dependent-comet)/(scot %uv job)
      %arvo
      %k
      %lard
      byk
      %+  (set-timeout:strandio ,vase)  verification-timeout
      (verify-lc:lca dependent-sat ~ sponsors)
  ==
::
++  result-order-state
  ^-  gw-state
  =/  pending  *(map ship pending-writ)
  =.  pending
    (~(put by pending) comet [%gw-btc pass-a [comet fixture-spawn custody-a]])
  =.  pending
    (~(put by pending) dependent-comet [%gw-btc dependent-pass dependent-sat])
  =/  inflight  filler-inflight
  =.  inflight
    (~(put by inflight) comet [`@uv`15 (request-token pass-a) (context-token *state:urb ~) `@uv`0])
  :*  mock-req-to
      *state:urb
      &
      `best-id
      pending
      inflight
      ~
      ~
      ~
      `@uv`16
      `@uv`0
  ==
::
::  A verifier card contains a khan shed, whose implementation details do not
::  belong in these agent tests.  Match the routing and desk and leave the
::  strand itself to tests/lib/self-attestation.hoon.
++  ex-verify-card
  |=  [expected-wire=wire expected-byk=*]
  |=  car=card
  ^-  tang
  ?.  ?=([%pass * %arvo %k %lard * *] car)
    ~[leaf+"expected a %gw-btc verification khan card"]
  =/  [%pass got-wire=wire %arvo %k %lard got-byk=* *]  car
  ?.  =(expected-wire got-wire)
    ~[leaf+"verification card used the wrong wire"]
  ?.  =(expected-byk got-byk)
    ~[leaf+"verification card used the wrong desk"]
  ~
::
++  ex-timeout-card
  |=  [expected-wire=wire expected-time=@da]
  |=  car=card
  ^-  tang
  ?.  ?=([%pass * %arvo %b %wait *] car)
    ~[leaf+"expected a verification timeout card"]
  =/  [%pass got-wire=wire %arvo %b %wait got-time=@da]  car
  ?.  =(expected-wire got-wire)
    ~[leaf+"verification timeout card used the wrong wire"]
  ?.  =(expected-time got-time)
    ~[leaf+"verification timeout card used the wrong deadline"]
  ~
::
++  ex-success-writ
  |=  expected=ship
  |=  car=card
  ^-  tang
  ?.  ?=([%give %fact * %writ-response *] car)
    ~[leaf+"expected a successful %writ-response fact"]
  =/  [%give %fact paths=(list path) %writ-response =vase]  car
  ?.  =(~[/writs] paths)
    ~[leaf+"%writ-response used the wrong path"]
  =/  res=writ-response:jael  !<(writ-response:jael vase)
  ?.  ?&  =(%gw-btc dom.res)
          =(expected ship.res)
          ?=(^ res.res)
      ==
    ~[leaf+"%writ-response did not carry the verified point"]
  ~
::
++  ex-failure-writ
  |=  expected=ship
  |=  car=card
  ^-  tang
  ?.  ?=([%give %fact * %writ-response *] car)
    ~[leaf+"expected a failed %writ-response fact"]
  =/  [%give %fact paths=(list path) %writ-response =vase]  car
  ?.  =(~[/writs] paths)
    ~[leaf+"%writ-response used the wrong path"]
  =/  out=writ-response:jael  !<(writ-response:jael vase)
  ?.  ?&  =(%gw-btc dom.out)
          =(expected ship.out)
          ?=(~ res.out)
      ==
    ~[leaf+"%writ-response did not carry a negative verdict"]
  ~
::
++  ex-anew-response
  |=  expected=pass
  |=  car=card
  ^-  tang
  ?.  ?=([%give %fact * %anew-response *] car)
    ~[leaf+"expected an %anew-response fact"]
  =/  [%give %fact paths=(list path) %anew-response =vase]  car
  ?.  =(~[/writs] paths)
    ~[leaf+"%anew-response used the wrong path"]
  =/  res=anew-response:jael  !<(anew-response:jael vase)
  ?.  ?&  =(%gw-btc dom.res)
          =(expected pass.res)
      ==
    ~[leaf+"%anew-response did not carry the %gw-btc pass"]
  ~
--
::
::  Tests
|%
++  test-init-cards
  %-  eval-mare:tag
  =/  m  (mare:tag ,~)
  ^-  form:m
  ;<  caz=(list card)  bind:m  (do-init:tag dap mock-agent)
  %+  ex-cards:tag  caz
  :~  (ex-arvo:tag /anex %j %anex /writs)
      (ex-task:tag /best-block [~zod %light-client] [%watch /best-block])
  ==
::
++  test-init-state
  %-  eval-mare:tag
  =/  m  (mare:tag ,~)
  ^-  form:m
  ;<  *      bind:m  (do-init:tag dap mock-agent)
  ;<  =vase  bind:m  get-save:tag
  =/  st=gw-state  !<(gw-state vase)
  ?>  =('http://localhost:8332' url.rpc.st)
  ?>  ?=(~ auth.rpc.st)
  ?>  =(*state:urb urb-state.st)
  ?>  =(ready.st |)
  ?>  ?=(~ best.st)
  ?>  =(~ pending.st)
  ?>  =(~ inflight.st)
  ?>  =(~ confidential.st)
  ?>  =(~ attested.st)
  ?>  =(~ publicizing.st)
  ?>  =(0 next-job.st)
  ?>  =(0 chain-epoch.st)
  (pure:m ~)
::
++  test-save-load-roundtrip
  %-  eval-mare:tag
  =/  m  (mare:tag ,~)
  ^-  form:m
  ;<  *       bind:m  (do-init:tag dap mock-agent)
  ;<  *       bind:m  (do-poke:tag %noun !>(`jael-poke:urb`[%jael-writ %gw-btc comet pass-a]))
  ;<  before=vase  bind:m  get-save:tag
  ;<  *       bind:m  (do-load:tag mock-agent `before)
  ;<  after=vase   bind:m  get-save:tag
  (ex-equal:tag after before)
::
++  test-local-writs-watch
  %-  eval-mare:tag
  =/  m  (mare:tag ,~)
  ^-  form:m
  ;<  *                bind:m  (do-init:tag dap mock-agent)
  ;<  caz=(list card)  bind:m  (do-watch:tag /writs)
  (ex-cards:tag caz ~)
::
++  test-anew-current-tip-emits
  %-  eval-mare:tag
  =/  m  (mare:tag ,~)
  ^-  form:m
  ;<  b=bowl:gall       bind:m  get-bowl:tag
  ;<  *                 bind:m  (set-bowl:tag b(our comet, src comet))
  ;<  *                 bind:m  (do-init:tag dap mock-agent)
  ;<  *                 bind:m  (do-load:tag mock-agent `!>(current-anew-state))
  ;<  caz=(list card)   bind:m
    (do-poke:tag %noun !>(`jael-poke:urb`[%jael-anew %gw-btc]))
  (ex-cards:tag caz ~[(ex-anew-response pass-a)])
::
++  test-anew-stale-tip-is-silent
  %-  eval-mare:tag
  =/  m  (mare:tag ,~)
  ^-  form:m
  ;<  b=bowl:gall       bind:m  get-bowl:tag
  ;<  *                 bind:m  (set-bowl:tag b(our comet, src comet))
  ;<  *                 bind:m  (do-init:tag dap mock-agent)
  ;<  *                 bind:m  (do-load:tag mock-agent `!>(stale-anew-state))
  ;<  caz=(list card)   bind:m
    (do-poke:tag %noun !>(`jael-poke:urb`[%jael-anew %gw-btc]))
  (ex-cards:tag caz ~)
::
++  test-anew-missing-tip-is-silent
  %-  eval-mare:tag
  =/  m  (mare:tag ,~)
  ^-  form:m
  ;<  b=bowl:gall       bind:m  get-bowl:tag
  ;<  *                 bind:m  (set-bowl:tag b(our comet, src comet))
  ;<  *                 bind:m  (do-init:tag dap mock-agent)
  ;<  *                 bind:m  (do-load:tag mock-agent `!>(fixture-state))
  ;<  caz=(list card)   bind:m
    (do-poke:tag %noun !>(`jael-poke:urb`[%jael-anew %gw-btc]))
  (ex-cards:tag caz ~)
::
++  test-writ-queues-before-readiness
  %-  eval-mare:tag
  =/  m  (mare:tag ,~)
  ^-  form:m
  ;<  *                bind:m  (do-init:tag dap mock-agent)
  ;<  caz=(list card)  bind:m
    (do-poke:tag %noun !>(`jael-poke:urb`[%jael-writ %gw-btc comet pass-a]))
  ;<  *                bind:m  (ex-cards:tag caz ~)
  ;<  =vase            bind:m  get-save:tag
  =/  st=gw-state  !<(gw-state vase)
  =/  req  (~(get by pending.st) comet)
  ?>  ?=(^ req)
  ?>  =(%gw-btc dom.u.req)
  ?>  =(pass-a pass.u.req)
  ?>  =(custody-a chain.sat.u.req)
  ?>  ?=(~ (~(get by inflight.st) comet))
  (pure:m ~)
::
++  test-readiness-launches-queued-writ
  %-  eval-mare:tag
  =/  m  (mare:tag ,~)
  ^-  form:m
  ;<  *                bind:m  (do-init:tag dap mock-agent)
  ;<  *                bind:m
    (do-poke:tag %noun !>(`jael-poke:urb`[%jael-writ %gw-btc comet pass-a]))
  ;<  =bowl:gall       bind:m  get-bowl:tag
  ;<  timer=(list card)  bind:m
    (do-poke:tag %urb-start-indexing !>(`(unit state:urb)`~))
  ;<  *                bind:m
    %+  ex-cards:tag  timer
    ~[(ex-arvo:tag /timer %b %wait now.bowl)]
  ;<  =bowl:gall       bind:m  get-bowl:tag
  =/  upd=best-block:update:lc  [%new num.best-id hax.best-id]
  ;<  launched=(list card)  bind:m
    (do-agent:tag /best-block [~zod %light-client] [%fact %best-block !>(upd)])
  ;<  *                bind:m
    %+  ex-cards:tag  launched
    :~  (ex-verify-card /verify/(scot %p comet)/(scot %uv 0) q.byk.bowl)
        %-  ex-timeout-card
        :_  (add now.bowl verification-timeout)
        /verify-timeout/(scot %p comet)/(scot %uv 0)
    ==
  ;<  =vase            bind:m  get-save:tag
  =/  st=gw-state  !<(gw-state vase)
  ?>  ready.st
  ?>  =(`best-id best.st)
  =/  active  (need (~(get by inflight.st) comet))
  ?>  =(0 job.active)
  ?>  =((request-token pass-a) token.active)
  ?>  =(1 next-job.st)
  ?>  (~(has by pending.st) comet)
  (pure:m ~)
::
++  test-latest-writ-wins-stale-result
  %-  eval-mare:tag
  =/  m  (mare:tag ,~)
  ^-  form:m
  =/  old-token  (request-token pass-a)
  =/  new-token  (request-token pass-b)
  ?>  !=(old-token new-token)
  ;<  *                bind:m  (do-init:tag dap mock-agent)
  ;<  *                bind:m
    (do-poke:tag %urb-start-indexing !>(`(unit state:urb)`~))
  =/  upd=best-block:update:lc  [%new num.best-id hax.best-id]
  ;<  *                bind:m
    (do-agent:tag /best-block [~zod %light-client] [%fact %best-block !>(upd)])
  ;<  =bowl:gall       bind:m  get-bowl:tag
  ;<  first=(list card)  bind:m
    (do-poke:tag %noun !>(`jael-poke:urb`[%jael-writ %gw-btc comet pass-a]))
  ;<  *                bind:m
    %+  ex-cards:tag  first
    :~  (ex-verify-card /verify/(scot %p comet)/(scot %uv 0) q.byk.bowl)
        %-  ex-timeout-card
        :_  (add now.bowl verification-timeout)
        /verify-timeout/(scot %p comet)/(scot %uv 0)
    ==
  ;<  second=(list card)  bind:m
    (do-poke:tag %noun !>(`jael-poke:urb`[%jael-writ %gw-btc comet pass-b]))
  ;<  *                bind:m  (ex-cards:tag second ~)
  ;<  relaunched=(list card)  bind:m
    (do-arvo:tag /verify/(scot %p comet)/(scot %uv 0) *sign-arvo)
  ;<  *                bind:m
    %+  ex-cards:tag  relaunched
    :~  (ex-verify-card /verify/(scot %p comet)/(scot %uv 1) q.byk.bowl)
        %-  ex-timeout-card
        :_  (add now.bowl verification-timeout)
        /verify-timeout/(scot %p comet)/(scot %uv 1)
    ==
  ;<  =vase            bind:m  get-save:tag
  =/  st=gw-state  !<(gw-state vase)
  =/  req  (~(get by pending.st) comet)
  ?>  ?=(^ req)
  ?>  =(pass-b pass.u.req)
  =/  active  (need (~(get by inflight.st) comet))
  ?>  =(1 job.active)
  ?>  =(new-token token.active)
  ?>  =(2 next-job.st)
  (pure:m ~)
::
++  test-ready-path-enforces-inflight-cap
  %-  eval-mare:tag
  =/  m  (mare:tag ,~)
  ^-  form:m
  ;<  *                bind:m  (do-init:tag dap mock-agent)
  ;<  *                bind:m
    (do-poke:tag %urb-start-indexing !>(`(unit state:urb)`~))
  =/  upd=best-block:update:lc  [%new num.best-id hax.best-id]
  ;<  *                bind:m
    (do-agent:tag /best-block [~zod %light-client] [%fact %best-block !>(upd)])
  ;<  *                bind:m  (poke-ready-prefix 0 16)
  =/  seventeenth-pass  (pass-at 16)
  =/  seventeenth       (comet-for seventeenth-pass)
  ;<  caz=(list card)  bind:m
    (do-poke:tag %noun !>(`jael-poke:urb`[%jael-writ %gw-btc seventeenth seventeenth-pass]))
  ;<  *                bind:m  (ex-cards:tag caz ~)
  ;<  =vase            bind:m  get-save:tag
  =/  st=gw-state  !<(gw-state vase)
  ?>  =(17 ~(wyt by pending.st))
  ?>  =(16 ~(wyt by inflight.st))
  ?>  (~(has by pending.st) seventeenth)
  ?>  !(~(has by inflight.st) seventeenth)
  (pure:m ~)
::
++  test-stale-timeout-relaunches-latest-writ
  %-  eval-mare:tag
  =/  m  (mare:tag ,~)
  ^-  form:m
  =/  old-token  (request-token pass-a)
  =/  new-token  (request-token pass-b)
  ?>  !=(old-token new-token)
  ;<  *                bind:m  (do-init:tag dap mock-agent)
  ;<  *                bind:m
    (do-poke:tag %urb-start-indexing !>(`(unit state:urb)`~))
  =/  upd=best-block:update:lc  [%new num.best-id hax.best-id]
  ;<  *                bind:m
    (do-agent:tag /best-block [~zod %light-client] [%fact %best-block !>(upd)])
  ;<  =bowl:gall       bind:m  get-bowl:tag
  ;<  first=(list card)  bind:m
    (do-poke:tag %noun !>(`jael-poke:urb`[%jael-writ %gw-btc comet pass-a]))
  ;<  *                bind:m
    %+  ex-cards:tag  first
    :~  (ex-verify-card /verify/(scot %p comet)/(scot %uv 0) q.byk.bowl)
        (ex-timeout-card /verify-timeout/(scot %p comet)/(scot %uv 0) (add now.bowl verification-timeout))
    ==
  ;<  second=(list card)  bind:m
    (do-poke:tag %noun !>(`jael-poke:urb`[%jael-writ %gw-btc comet pass-b]))
  ;<  *                bind:m  (ex-cards:tag second ~)
  =/  wake=sign-arvo  [%behn %wake ~]
  ;<  relaunched=(list card)  bind:m
    (do-arvo:tag /verify-timeout/(scot %p comet)/(scot %uv 0) wake)
  ;<  *                bind:m
    %+  ex-cards:tag  relaunched
    :~  (ex-verify-card /verify/(scot %p comet)/(scot %uv 1) q.byk.bowl)
        (ex-timeout-card /verify-timeout/(scot %p comet)/(scot %uv 1) (add now.bowl verification-timeout))
    ==
  ;<  =vase            bind:m  get-save:tag
  =/  st=gw-state  !<(gw-state vase)
  =/  req  (~(get by pending.st) comet)
  ?>  ?=(^ req)
  ?>  =(pass-b pass.u.req)
  =/  active  (need (~(get by inflight.st) comet))
  ?>  =(1 job.active)
  ?>  =(new-token token.active)
  ?>  =(2 next-job.st)
  (pure:m ~)
::
++  test-success-applies-before-queued-launch
  %-  eval-mare:tag
  =/  m  (mare:tag ,~)
  ^-  form:m
  ;<  *                bind:m  (do-init:tag dap mock-agent)
  ;<  *                bind:m  (do-load:tag mock-agent `!>(result-order-state))
  ;<  =bowl:gall       bind:m  get-bowl:tag
  =/  verified=point:urb  (fixture-point pass-a [& public-ship])
  =/  res=result:sa  [[comet & ~] `verified 100]
  =/  sign=sign-arvo
    [%khan %arow %.y %noun !>([res *hexb:bc])]
  ;<  caz=(list card)  bind:m
    (do-arvo:tag /verify/(scot %p comet)/(scot %uv 15) sign)
  =/  sponsors=unv-ids:urb
    (~(put by *unv-ids:urb) comet verified)
  =/  expected=card  (dependent-verify-card q.byk.bowl `@uv`16 sponsors)
  ;<  *                bind:m
    %+  ex-cards:tag  caz
    :~  (ex-success-writ comet)
        (ex-card:tag expected)
        %-  ex-timeout-card
        :_  (add now.bowl verification-timeout)
        /verify-timeout/(scot %p dependent-comet)/(scot %uv 16)
    ==
  ;<  =vase            bind:m  get-save:tag
  =/  st=gw-state  !<(gw-state vase)
  ?>  =(`verified (~(get by unv-ids.urb-state.st) comet))
  ?>  (~(has in confidential.st) comet)
  ?>  ?=(~ (~(get by pending.st) comet))
  ?>  (~(has by pending.st) dependent-comet)
  ?>  ?=(~ (~(get by inflight.st) comet))
  ?>  (~(has by inflight.st) dependent-comet)
  ?>  =(16 ~(wyt by inflight.st))
  (pure:m ~)
::
++  test-current-timeout-frees-slot-and-launches-next
  %-  eval-mare:tag
  =/  m  (mare:tag ,~)
  ^-  form:m
  ;<  *                bind:m  (do-init:tag dap mock-agent)
  ;<  *                bind:m  (do-load:tag mock-agent `!>(result-order-state))
  ;<  =bowl:gall       bind:m  get-bowl:tag
  =/  wake=sign-arvo  [%behn %wake ~]
  ;<  caz=(list card)  bind:m
    (do-arvo:tag /verify-timeout/(scot %p comet)/(scot %uv 15) wake)
  ;<  *                bind:m
    %+  ex-cards:tag  caz
    :~  (ex-card:tag (dependent-verify-card q.byk.bowl `@uv`16 *unv-ids:urb))
        (ex-timeout-card /verify-timeout/(scot %p dependent-comet)/(scot %uv 16) (add now.bowl verification-timeout))
    ==
  ;<  =vase            bind:m  get-save:tag
  =/  st=gw-state  !<(gw-state vase)
  ?>  ?=(~ (~(get by pending.st) comet))
  ?>  (~(has by pending.st) dependent-comet)
  ?>  ?=(~ (~(get by inflight.st) comet))
  =/  active  (need (~(get by inflight.st) dependent-comet))
  ?>  =(16 job.active)
  ?>  =((request-token dependent-pass) token.active)
  ?>  =(16 ~(wyt by inflight.st))
  ?>  ready.st
  ?>  =(`best-id best.st)
  (pure:m ~)
::
++  test-khan-crash-frees-slot-and-launches-next
  %-  eval-mare:tag
  =/  m  (mare:tag ,~)
  ^-  form:m
  ;<  *                bind:m  (do-init:tag dap mock-agent)
  ;<  *                bind:m  (do-load:tag mock-agent `!>(result-order-state))
  ;<  =bowl:gall       bind:m  get-bowl:tag
  =/  crash=sign-arvo
    [%khan %arow %.n [%test ~[leaf+"fixture crash"]]]
  ;<  caz=(list card)  bind:m
    (do-arvo:tag /verify/(scot %p comet)/(scot %uv 15) crash)
  ;<  *                bind:m
    %+  ex-cards:tag  caz
    :~  (ex-card:tag (dependent-verify-card q.byk.bowl `@uv`16 *unv-ids:urb))
        (ex-timeout-card /verify-timeout/(scot %p dependent-comet)/(scot %uv 16) (add now.bowl verification-timeout))
    ==
  ;<  =vase            bind:m  get-save:tag
  =/  st=gw-state  !<(gw-state vase)
  ?>  ?=(~ (~(get by pending.st) comet))
  ?>  (~(has by pending.st) dependent-comet)
  ?>  ?=(~ (~(get by inflight.st) comet))
  =/  active  (need (~(get by inflight.st) dependent-comet))
  ?>  =(16 job.active)
  ?>  =((request-token dependent-pass) token.active)
  ?>  =(16 ~(wyt by inflight.st))
  ?>  ready.st
  ?>  =(`best-id best.st)
  (pure:m ~)
::
++  test-confidential-points-filtered-from-scry
  %-  eval-mare:tag
  =/  m  (mare:tag ,~)
  ^-  form:m
  ;<  *       bind:m  (do-init:tag dap mock-agent)
  ;<  *       bind:m  (do-load:tag mock-agent `!>(fixture-state))
  ;<  res=(unit (unit cage))  bind:m  (get-peek:tag /x/points)
  =/  got-cage=cage  (need (need res))
  ?>  =(%urb-points p.got-cage)
  =/  ids=unv-ids:urb  !<(unv-ids:urb q.got-cage)
  ?>  (~(has by ids) public-ship)
  ?>  !(~(has by ids) comet)
  ;<  hidden=(unit (unit cage))  bind:m
    (get-peek:tag /x/point/(scot %p comet))
  ?>  ?=(^ hidden)
  ?>  ?=(~ u.hidden)
  (pure:m ~)
::
++  test-no-sponsor-falls-back-to-ship
  %-  eval-mare:tag
  =/  m  (mare:tag ,~)
  ^-  form:m
  ;<  *                bind:m  (do-init:tag dap mock-agent)
  ;<  *                bind:m  (do-load:tag mock-agent `!>(fixture-state))
  ;<  caz=(list card)  bind:m  (do-watch:tag /(scot %p public-ship))
  =.  caz
    %+  skip  caz
    |=  car=card
    ?=([%give %fact [[%verb ?(%events %events-plus) ~] ~] *] car)
  ?>  ?=([[%give %fact * %azimuth-udiffs *] ~] caz)
  =/  [%give %fact paths=(list path) %azimuth-udiffs =vase]  i.caz
  ?>  =(~ paths)
  =/  diffs=udiffs:point:jael  !<(udiffs:point:jael vase)
  ?>  =(4 (lent diffs))
  ?>  =([public-ship [0xcafe 943.199] %spon `public-ship] (snag 3 diffs))
  (pure:m ~)
::
++  test-confidential-ship-watch-is-silent
  %-  eval-mare:tag
  =/  m  (mare:tag ,~)
  ^-  form:m
  ;<  *                bind:m  (do-init:tag dap mock-agent)
  ;<  *                bind:m  (do-load:tag mock-agent `!>(fixture-state))
  ;<  caz=(list card)  bind:m  (do-watch:tag /(scot %p comet))
  (ex-cards:tag caz ~)
::
++  test-block-retries-verifier-only-insertion
  %-  eval-mare:tag
  =/  m  (mare:tag ,~)
  ^-  form:m
  =/  before=gw-state  fixture-state
  =/  base=state:urb  *state:urb
  =.  block-id.base  block-base-id
  =/  result=state:urb  base(block-id block-next-id)
  ;<  *                bind:m  (do-init:tag dap mock-agent)
  ;<  *                bind:m  (do-load:tag mock-agent `!>(before))
  ;<  =bowl:gall       bind:m  get-bowl:tag
  ;<  caz=(list card)  bind:m
    (do-arvo:tag /blocks (block-sign base ~ result))
  ;<  *                bind:m
    (ex-cards:tag caz ~[(ex-arvo:tag /timer %b %wait now.bowl)])
  ;<  =vase            bind:m  get-save:tag
  =/  after=gw-state  !<(gw-state vase)
  ?>  =(urb-state.before urb-state.after)
  ?>  =(confidential.before confidential.after)
  ?>  =(~ publicizing.after)
  ?>  (~(has by unv-ids.urb-state.after) comet)
  (pure:m ~)
::
++  test-block-retries-divergent-confidential-move
  %-  eval-mare:tag
  =/  m  (mare:tag ,~)
  ^-  form:m
  =/  who  dependent-comet
  =/  base  (one-point-state block-base-id who dependent-pass base-tip)
  =/  live-id=id:block:bc  [0x2001 943.210]
  =/  live  (one-point-state live-id who dependent-pass live-tip)
  =/  result  (one-point-state block-next-id who dependent-pass scan-tip)
  =/  before=gw-state
    :*  mock-req-to  live  &  `best-id  ~  ~
        (~(put in *(set ship)) who)  ~  ~  `@uv`0  `@uv`0
    ==
  ;<  *                bind:m  (do-init:tag dap mock-agent)
  ;<  *                bind:m  (do-load:tag mock-agent `!>(before))
  ;<  =bowl:gall       bind:m  get-bowl:tag
  ;<  caz=(list card)  bind:m
    (do-arvo:tag /blocks (block-sign base ~ result))
  ;<  *                bind:m
    (ex-cards:tag caz ~[(ex-arvo:tag /timer %b %wait now.bowl)])
  ;<  first=vase       bind:m  get-save:tag
  =/  after=gw-state  !<(gw-state first)
  ?>  =(live urb-state.after)
  ?>  =(live-tip sont.own:(need (~(get by unv-ids.urb-state.after) who)))
  ?>  =(~ publicizing.after)
  (pure:m ~)
::
++  test-block-conflict-rolls-back-then-replays-public-spawn
  %-  eval-mare:tag
  =/  m  (mare:tag ,~)
  ^-  form:m
  =/  pub=sont:ord  public-tip
  =/  conflict  dependent-comet
  =/  base
    (one-point-state block-base-id conflict dependent-pass base-tip)
  =/  live-id=id:block:bc  [0x3001 943.210]
  =/  live
    (one-point-state live-id conflict dependent-pass live-tip)
  =.  live
    (put-point live comet (point-at pass-a [& public-ship] public-tip) 8.000)
  =/  result
    (one-point-state block-next-id conflict dependent-pass scan-tip)
  =.  result
    (put-point result comet (point-at pass-a [& public-ship] public-tip) 8.000)
  =/  pending
    (~(put by *(map ship pending-writ)) comet [%gw-btc pass-a [comet fixture-spawn custody-a]])
  =/  attested  (~(put by *(map ship sont:ord)) comet public-tip)
  =/  inflight
    (~(put by *(map ship inflight-job)) comet [`@uv`7 (request-token pass-a) (context-token live attested) `@uv`0])
  =/  confidential
    (~(put in (~(put in *(set ship)) conflict)) comet)
  =/  before=gw-state
    :*  mock-req-to  live  &  `best-id  pending  inflight
        confidential  attested  ~  `@uv`8  `@uv`0
    ==
  =/  fx=(list [id:block:bc effect:urb])
    ~[[block-next-id [%point comet %owner public-tip]]]
  ;<  *                bind:m  (do-init:tag dap mock-agent)
  ;<  *                bind:m  (do-load:tag mock-agent `!>(before))
  ;<  =bowl:gall       bind:m  get-bowl:tag
  ;<  caz=(list card)  bind:m
    (do-arvo:tag /blocks (block-sign base fx result))
  ;<  *                bind:m
    (ex-cards:tag caz ~[(ex-arvo:tag /timer %b %wait now.bowl)])
  ;<  first=vase       bind:m  get-save:tag
  =/  after=gw-state  !<(gw-state first)
  ?>  =(live-id block-id.urb-state.after)
  ?>  !(~(has by unv-ids.urb-state.after) comet)
  ?>  (~(has by unv-ids.urb-state.after) conflict)
  ?>  (~(has in confidential.after) conflict)
  ?>  !(~(has in confidential.after) comet)
  ?>  (~(has in publicizing.after) comet)
  ?>  !(~(has by pending.after) comet)
  ?>  !(~(has by inflight.after) comet)
  ?>  !(~(has by attested.after) comet)
  ?>  ?=(~ (get:si:ol sont-map.urb-state.after txid.pub vout.pub off.pub))
  ::  Replay from the sanitized live base.  The public spawn is now applied
  ::  without the unrelated point's divergent branch, and the guard clears.
  =/  retry-base  urb-state.after
  =/  replay-result=state:urb  retry-base(block-id block-next-id)
  =.  replay-result
    (put-point replay-result comet (point-at pass-a [& public-ship] public-tip) 8.000)
  ;<  *  bind:m
    (do-arvo:tag /blocks (block-sign retry-base fx replay-result))
  ;<  last=vase  bind:m  get-save:tag
  =/  final=gw-state  !<(gw-state last)
  ?>  =(block-next-id block-id.urb-state.final)
  ?>  (~(has by unv-ids.urb-state.final) comet)
  ?>  !(~(has in confidential.final) comet)
  ?>  =(~ publicizing.final)
  =/  indexed
    (need (get:si:ol sont-map.urb-state.final txid.pub vout.pub off.pub))
  ?>  =(`comet com.indexed)
  (pure:m ~)
::
++  test-successful-block-wakes-queued-verification
  %-  eval-mare:tag
  =/  m  (mare:tag ,~)
  ^-  form:m
  =/  urb-state  *state:urb
  =.  block-id.urb-state  block-base-id
  =/  pending
    (~(put by *(map ship pending-writ)) comet [%gw-btc pass-a [comet fixture-spawn custody-a]])
  =/  before=gw-state
    :*  mock-req-to  urb-state  &  `best-id  pending  ~
        ~  ~  ~  `@uv`0  `@uv`0
    ==
  ;<  *                bind:m  (do-init:tag dap mock-agent)
  ;<  *                bind:m  (do-load:tag mock-agent `!>(before))
  ;<  =bowl:gall       bind:m  get-bowl:tag
  ;<  caz=(list card)  bind:m
    (do-arvo:tag /blocks (block-sign urb-state ~ urb-state))
  ;<  *                bind:m
    %+  ex-cards:tag  caz
    :~  (ex-fact:tag ~[/] %azimuth-udiffs !>(`udiffs:point:jael`~))
        (ex-arvo:tag /timer %b %wait (add ~s30 now.bowl))
        (ex-verify-card /verify/(scot %p comet)/(scot %uv 0) q.byk.bowl)
        (ex-timeout-card /verify-timeout/(scot %p comet)/(scot %uv 0) (add now.bowl verification-timeout))
    ==
  ;<  =vase            bind:m  get-save:tag
  =/  after=gw-state  !<(gw-state vase)
  =/  active  (need (~(get by inflight.after) comet))
  ?>  =(0 job.active)
  ?>  =((request-token pass-a) token.active)
  ?>  =(1 next-job.after)
  (pure:m ~)
::
++  test-verified-move-preserves-colocated-inscription
  %-  eval-mare:tag
  =/  m  (mare:tag ,~)
  ^-  form:m
  =/  tip=sont:ord  verified-tip
  =/  new-point  (point-at pass-a [& public-ship] tip)
  =/  res=result:sa  [[comet & ~] `new-point 54.321]
  =/  sign=sign-arvo
    [%khan %arow %.y %noun !>([res *hexb:bc])]
  ;<  *                bind:m  (do-init:tag dap mock-agent)
  ;<  *                bind:m  (do-load:tag mock-agent `!>(inscription-state))
  ;<  caz=(list card)  bind:m
    (do-arvo:tag /verify/(scot %p comet)/(scot %uv 0) sign)
  ;<  *                bind:m
    (ex-cards:tag caz ~[(ex-success-writ comet)])
  ;<  =vase            bind:m  get-save:tag
  =/  st=gw-state  !<(gw-state vase)
  =/  sat=sont:ord  fixture-spawn
  ?>  ?=(~ (get:si:ol sont-map.urb-state.st txid.sat vout.sat off.sat))
  =/  moved
    (need (get:si:ol sont-map.urb-state.st txid.tip vout.tip off.tip))
  ?>  =(`comet com.moved)
  ?>  (~(has in ins.moved) fixture-insc)
  =/  vm
    (need (get-vout:si:ol sont-map.urb-state.st txid.tip vout.tip))
  ?>  =(54.321 value.vm)
  =/  reverse  (need (~(get by insc-ids.urb-state.st) fixture-insc))
  ?>  =(tip sont.reverse)
  ?>  =(fixture-mail mail.reverse)
  ?>  =(tip (need (~(get by attested.st) comet)))
  (pure:m ~)
::
++  test-verified-tip-owned-by-other-comet-is-negative
  %-  eval-mare:tag
  =/  m  (mare:tag ,~)
  ^-  form:m
  =/  tip=sont:ord  verified-tip
  =/  before=gw-state  occupied-tip-state
  =/  verified  (point-at pass-a [& public-ship] tip)
  =/  res=result:sa  [[comet & ~] `verified 54.321]
  =/  sign=sign-arvo
    [%khan %arow %.y %noun !>([res *hexb:bc])]
  ;<  *                bind:m  (do-init:tag dap mock-agent)
  ;<  *                bind:m  (do-load:tag mock-agent `!>(before))
  ;<  caz=(list card)  bind:m
    (do-arvo:tag /verify/(scot %p comet)/(scot %uv 0) sign)
  ;<  *                bind:m
    (ex-cards:tag caz ~[(ex-failure-writ comet)])
  ;<  =vase            bind:m  get-save:tag
  =/  after=gw-state  !<(gw-state vase)
  ?>  =(unv-ids.urb-state.before unv-ids.urb-state.after)
  ?>  =(sont-map.urb-state.before sont-map.urb-state.after)
  ?>  =(insc-ids.urb-state.before insc-ids.urb-state.after)
  ?>  =(confidential.before confidential.after)
  ?>  =(attested.before attested.after)
  ?>  !(~(has by pending.after) comet)
  ?>  !(~(has by inflight.after) comet)
  =/  occupied
    (need (get:si:ol sont-map.urb-state.after txid.tip vout.tip off.tip))
  ?>  =(`public-ship com.occupied)
  (pure:m ~)
::
++  test-public-snapshot-preserves-colocated-inscription
  %-  eval-mare:tag
  =/  m  (mare:tag ,~)
  ^-  form:m
  ;<  *       bind:m  (do-init:tag dap mock-agent)
  ;<  *       bind:m
    (do-load:tag mock-agent `!>(inscription-snapshot-state))
  ;<  res=(unit (unit cage))  bind:m  (get-peek:tag /x/urb-state)
  =/  got-cage=cage  (need (need res))
  ?>  =(%noun p.got-cage)
  =/  public=state:urb  !<(state:urb q.got-cage)
  =/  sat=sont:ord  fixture-spawn
  ?>  !(~(has by unv-ids.public) comet)
  =/  kept
    (need (get:si:ol sont-map.public txid.sat vout.sat off.sat))
  ?>  ?=(~ com.kept)
  ?>  (~(has in ins.kept) fixture-insc)
  =/  vm  (need (get-vout:si:ol sont-map.public txid.sat vout.sat))
  ?>  =(12.345 value.vm)
  =/  reverse  (need (~(get by insc-ids.public) fixture-insc))
  ?>  =(sat sont.reverse)
  ?>  =(fixture-mail mail.reverse)
  (pure:m ~)
::
++  test-raw-zero-public-pass-is-silent
  %-  eval-mare:tag
  =/  m  (mare:tag ,~)
  ^-  form:m
  ;<  *                bind:m  (do-init:tag dap mock-agent)
  ;<  caz=(list card)  bind:m
    (do-poke:tag %noun !>(`jael-poke:urb`[%jael-writ %gw-btc public-comet public-pass]))
  ;<  *                bind:m  (ex-cards:tag caz ~)
  ;<  =vase            bind:m  get-save:tag
  =/  st=gw-state  !<(gw-state vase)
  ?>  !(~(has by pending.st) public-comet)
  ?>  !(~(has by inflight.st) public-comet)
  (pure:m ~)
::
++  test-canonical-empty-chain-is-negative
  %-  eval-mare:tag
  =/  m  (mare:tag ,~)
  ^-  form:m
  ;<  *                bind:m  (do-init:tag dap mock-agent)
  ;<  caz=(list card)  bind:m
    (do-poke:tag %noun !>(`jael-poke:urb`[%jael-writ %gw-btc empty-chain-comet empty-chain-pass]))
  ;<  *                bind:m
    (ex-cards:tag caz ~[(ex-failure-writ empty-chain-comet)])
  ;<  =vase            bind:m  get-save:tag
  =/  st=gw-state  !<(gw-state vase)
  ?>  !(~(has by pending.st) empty-chain-comet)
  (pure:m ~)
::
++  test-same-pass-stale-job-result-is-ignored
  %-  eval-mare:tag
  =/  m  (mare:tag ,~)
  ^-  form:m
  =/  urb-state  fixture-urb-state
  =/  pending
    (~(put by *(map ship pending-writ)) comet [%gw-btc pass-a [comet fixture-spawn custody-a]])
  =/  inflight
    (~(put by *(map ship inflight-job)) comet [`@uv`1 (request-token pass-a) (context-token urb-state ~) `@uv`0])
  =/  before=gw-state
    :*  mock-req-to  urb-state  &  `best-id  pending  inflight
        (~(put in *(set ship)) comet)  ~  ~  `@uv`2  `@uv`0
    ==
  =/  verified  (point-at pass-a [& public-ship] verified-tip)
  =/  res=result:sa  [[comet & ~] `verified 54.321]
  =/  sign=sign-arvo
    [%khan %arow %.y %noun !>([res *hexb:bc])]
  ;<  *                bind:m  (do-init:tag dap mock-agent)
  ;<  *                bind:m  (do-load:tag mock-agent `!>(before))
  ;<  caz=(list card)  bind:m
    (do-arvo:tag /verify/(scot %p comet)/(scot %uv 0) sign)
  ;<  *                bind:m  (ex-cards:tag caz ~)
  ;<  =vase            bind:m  get-save:tag
  (ex-equal:tag vase !>(before))
::
++  test-stale-context-relaunches-same-pass-with-new-job
  %-  eval-mare:tag
  =/  m  (mare:tag ,~)
  ^-  form:m
  =/  urb-state  fixture-urb-state
  =/  context  (context-token urb-state ~)
  =/  pending
    (~(put by *(map ship pending-writ)) comet [%gw-btc pass-a [comet fixture-spawn custody-a]])
  =/  inflight
    (~(put by *(map ship inflight-job)) comet [`@uv`0 (request-token pass-a) +(context) `@uv`0])
  =/  before=gw-state
    :*  mock-req-to  urb-state  &  `best-id  pending  inflight
        (~(put in *(set ship)) comet)  ~  ~  `@uv`1  `@uv`0
    ==
  ;<  *                bind:m  (do-init:tag dap mock-agent)
  ;<  *                bind:m  (do-load:tag mock-agent `!>(before))
  ;<  =bowl:gall       bind:m  get-bowl:tag
  ;<  relaunched=(list card)  bind:m
    (do-arvo:tag /verify/(scot %p comet)/(scot %uv 0) *sign-arvo)
  ;<  *                bind:m
    %+  ex-cards:tag  relaunched
    :~  (ex-verify-card /verify/(scot %p comet)/(scot %uv 1) q.byk.bowl)
        (ex-timeout-card /verify-timeout/(scot %p comet)/(scot %uv 1) (add now.bowl verification-timeout))
    ==
  ;<  stale=(list card)  bind:m
    (do-arvo:tag /verify/(scot %p comet)/(scot %uv 0) *sign-arvo)
  ;<  *                bind:m  (ex-cards:tag stale ~)
  ;<  =vase            bind:m  get-save:tag
  =/  st=gw-state  !<(gw-state vase)
  =/  active  (need (~(get by inflight.st) comet))
  ?>  =(1 job.active)
  ?>  =(context context.active)
  ?>  =(2 next-job.st)
  (pure:m ~)
::
++  test-start-indexing-is-local-only
  %-  eval-mare:tag
  =/  m  (mare:tag ,~)
  ^-  form:m
  ;<  *  bind:m  (do-init:tag dap mock-agent)
  ;<  *  bind:m
    (ex-fail:tag ((do-as:tag ~nec) (do-poke:tag %urb-start-indexing !>(`(unit state:urb)`~))))
  ;<  =vase  bind:m  get-save:tag
  =/  st=gw-state  !<(gw-state vase)
  ?>  =(| ready.st)
  ?>  =(*state:urb urb-state.st)
  (pure:m ~)
::
++  test-start-indexing-is-one-shot
  %-  eval-mare:tag
  =/  m  (mare:tag ,~)
  ^-  form:m
  ;<  *  bind:m  (do-init:tag dap mock-agent)
  ;<  *  bind:m
    (do-poke:tag %urb-start-indexing !>(`(unit state:urb)``fixture-urb-state))
  ;<  caz=(list card)  bind:m
    (do-poke:tag %urb-start-indexing !>(`(unit state:urb)``inscription-urb-state))
  ;<  *                bind:m  (ex-cards:tag caz ~)
  ;<  =vase            bind:m  get-save:tag
  =/  st=gw-state  !<(gw-state vase)
  ?>  ready.st
  ?>  =(fixture-urb-state urb-state.st)
  (pure:m ~)
--
