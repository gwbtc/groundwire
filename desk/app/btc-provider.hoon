::  btc-provider.hoon
::  Proxy that serves a BTC full node and ElectRS address indexer
::
::  Subscriptions: none
::  To Subscribers: /clients
::    current connection state
::    results/errors of RPC calls
::
::  Scrys
::  x/is-whitelisted/SHIP: bool, whether ship is whitelisted
::
/-  *bitcoin, json-rpc, *btc-provider
/+  dbug, default-agent, bl=btc, resource, verb
~%  %btc-provider-top  ..part  ~
|%
+$  card  card:agent:gall
+$  versioned-state
    $%  state-0
        state-1
        state-2
        state-3
    ==
::
+$  state-0  [%0 =host-info =whitelist]
+$  state-1  [%1 =host-info =whitelist timer=(unit @da)]
+$  state-2  [%2 =host-info =whitelist timer=(unit @da) interval=@dr]
+$  state-3  [%3 host-info=host-info-2 =whitelist timer=(unit @da) interval=@dr]
--
:: %+  verb  &
%-  agent:dbug
=|  state-3
=*  state  -
^-  agent:gall
=<
~%  %btc-provider-agent  ..send-status  ~
|_  =bowl:gall
+*  this      .
    def   ~(. (default-agent this %|) bowl)
    hc    ~(. +> bowl)
::
++  on-init
  ^-  (quip card _this)
  =|  wl=^whitelist
  :-  ~
  %_  this
    host-info  [~ ~ connected=%.n %main block=0 clients=*(set ship)]
    whitelist  wl(public %.n, kids %.n)
    timer      ~
    interval   ~m1
  ==
::
++  on-save
  ^-  vase
  !>(state)
::
++  on-load
  |=  old-state=vase
  ^-  (quip card _this)
  =/  old  !<(versioned-state old-state)
  ?-    -.old
      %3
    `this(state old)
      %2
    `this(state [%3 [~ ~ %.n %main 0 *(set ship)] whitelist.old ~ ~m1])
      %1
    `this(state [%3 [~ ~ %.n %main 0 *(set ship)] whitelist.old ~ ~m1])
      %0
    `this(state [%3 [~ ~ %.n %main 0 *(set ship)] whitelist.old ~ ~m1])
  ==
::
++  on-poke
  ~/  %on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  |^
  ?>  (team:title our.bowl src.bowl)
  =^  cards  state
    ?+  mark  (on-poke:def mark vase)
        %btc-provider-command
      ?>  (team:title our.bowl src.bowl)
      (handle-command !<(command vase))
    ::
        %btc-provider-action
      (handle-action !<(action vase))
    ::
    ::  unused?
        %noun
      ?.  =(q.vase %kick-timer)  `state
      :_  state(timer `now.bowl)
      :*  (start-ping-timer ~m1)
          ?~  timer  ~
          [[%pass /block-ping %arvo %b %rest u.timer] ~]
      ==
    ::
        %card
      :_  state
      [!<(card vase) ~]
    ==
  [cards this]
  ::
  ++  handle-command
    |=  comm=command
    ^-  (quip card _state)
    ?-  -.comm
        %set-credentials
      =/  =api-state  [url.comm port.comm local.comm]
      :_  %_  state
              host-info  [`api-state `our.bowl %.n network.comm 0 *(set ship)]
              timer      `now.bowl
          ==
      :*  (start-ping-timer:hc ~s10)
          :*  %give  %fact  ~[/rpc]  %btc-provider-status
              !>(`status`[%new-rpc url.comm port.comm network.comm])
          ==
          ?~  timer  ~
          [[%pass /block-ping %arvo %b %rest u.timer] ~]
      ==
    ::
        %set-external
      ?>  !=(our.bowl src.comm)
      ?:  =(src.host-info `src.comm)  `state
      :_  state(host-info [~ `src.comm %.n network.comm 0 *(set ship)])
      :-  [%pass /ext/[(scot %p src.comm)] %agent src.comm^%bitcoin-rpc %watch /rpc]  ~
      :: ?~  src.host-info  ~
      :: ?:  =(our.bowl u.src.host-info)  ~
      :: :-  :*  %pass   /ext/[(scot %p u.src.host-info)]
      ::         %agent  u.src.host-info^%btc-provider  %leave  ~
      ::     ==
      :: ~
    ::
        %add-whitelist
      :-  ~
      ?-  -.wt.comm
          %public
        state(public.whitelist %.y)
      ::
          %kids
        state(kids.whitelist %.y)
      ::
          %users
        state(users.whitelist (~(uni in users.whitelist) users.wt.comm))
      ::
          %groups
        state(groups.whitelist (~(uni in groups.whitelist) groups.wt.comm))
      ==
    ::
        %remove-whitelist
      =.  state
        ?-  -.wt.comm
            %public
          state(public.whitelist %.n)
        ::
            %kids
          state(kids.whitelist %.n)
        ::
            %users
          state(users.whitelist (~(dif in users.whitelist) users.wt.comm))
        ::
            %groups
          state(groups.whitelist (~(dif in groups.whitelist) groups.wt.comm))
        ==
      clean-client-list
    ::
        %set-interval
      `state(interval inte.comm)
    ==
  ::
  ::  +clean-client-list: remove clients who are no longer whitelisted
  ::   called after a whitelist change
  ::
  ++  clean-client-list
    ^-  (quip card _state)
    =/  to-kick=(set ship)
      %-  silt
      %+  murn  ~(tap in clients.host-info)
      |=  c=ship  ^-  (unit ship)
      ?:((is-whitelisted:hc c) ~ `c)
    :_  state(clients.host-info (~(dif in clients.host-info) to-kick))
    %+  turn  ~(tap in to-kick)
    |=(c=ship [%give %kick ~[/clients] `c])
  ::
  ::  if not connected, only %ping action is allowed
  ::
  ++  handle-action
    |=  act=action
    ^-  (quip card _state)
    :_  state
    ?.  ?|(connected.host-info ?=(%ping -.+.act))
      ~[(send-update-request:hc [%| id.act %not-connected 500])]
    :_  ~
    %+  req-card  act
    ^-  action:rpc-types
    ?-  -.+.act
      %address-info   [%get-address-info address.+.act]
      %tx-info        [%get-tx-vals txid.+.act]
      %raw-tx         [%get-raw-tx txid.+.act]
      %broadcast-tx   [%broadcast-tx rawtx.+.act]
      %ping           [%get-block-info ~]
      %block-info     [%get-block-info block.+.act]
      %histogram      [%get-histogram ~]
      %block-headers  [%get-block-headers start.+.act count.+.act cp.+.act]
      %tx-from-pos    [%get-tx-from-pos height.+.act pos.+.act merkle.+.act]
      %fee            [%get-fee block.+.act]
      %psbt           [%update-psbt psbt.+.act]
      %block-txs      [%get-block-txs blockhash.+.act]
      %mine-empty     [%mine-empty miner.+.act nblocks.+.act]
      %mine-trans     [%mine-trans miner.+.act txs.+.act]
    ==
  ::
  ++  req-card
    |=  [act=action ract=action:rpc-types]
    =/  req=request:http
      (gen-request:bl (need api.host-info) ract)
    [%pass (rpc-wire act) %arvo %i %request req *outbound-config:iris]
  ::
  ++  rpc-wire
    |=  act=action
    ^-  wire
    /[-.+.act]/(scot %uv id.act)/(scot %da now.bowl)
    :: (scot %ux (cut 3 [0 20] eny.bowl))
  --
::
++  on-watch
  ~/  %on-watch
  |=  pax=path
  ^-  (quip card _this)
  ::  checking provider permissions before trying to subscribe
  ::  terrible hack until we have cross-ship scries
  ::
  ?+    pax  (on-watch:def pax)
      [%permitted @ ~]
    :_  this
    =/  jon=json
      %+  frond:enjs:format
        %'providerStatus'
      %-  pairs:enjs:format
      :~  provider+s+(scot %p our.bowl)
          permitted+b+(is-whitelisted:hc src.bowl)
      ==
    [%give %fact ~ %json !>(jon)]~
  ::
      [%clients *]
    ~|  "btc-provider: blocked remote client app on {<src.bowl>}"
    ?>  =(src.bowl our.bowl)
    `this
  ::
      [%rpc ~]
    ?~  api.host-info  !!
    ?~  src.host-info  !!
    ?.  ?&  =(u.src.host-info our.bowl)
            (is-whitelisted:hc src.bowl)
        ==
      ~|("btc-provider: blocked RPC client request from {<src.bowl>}" !!)
    ~&  "btc-provider: accepted RPC client {<src.bowl>}"
    ~&  give=host-info
    :-  [%give %fact ~ %btc-provider-status !>(`status`[%new-rpc url.u.api.host-info port.u.api.host-info network.host-info])]~
    this(clients.host-info (~(put in clients.host-info) src.bowl))
  ==
::
++  on-arvo
  ~/  %on-arvo
  |=  [wir=wire =sign-arvo]
  |^
  ^-  (quip card _this)
  ::  check for connectivity every 30 seconds
  ::
  ?:  ?=([%ping-timer *] wir)
    `this
  ?:  ?=([%block-ping *] wir)
    :_  this(timer `(add now.bowl interval))
    :~  do-ping
        (start-ping-timer:hc interval)
        :: ?~  timer  ~
        :: [[%pass /block-ping %arvo %b %rest u.timer] ~]
    ==
  =^  cards  state
    ?+    +<.sign-arvo    (on-arvo:def wir sign-arvo)
        %http-response
      (handle-rpc-response wir client-response.sign-arvo)
    ==
  [cards this]
  ::
  ++  do-ping
    ^-  card
    =/  act=action  [`@uvH`eny.bowl %ping ~]
    :*  %pass  /ping/[(scot %uv eny.bowl)]  %agent
        [our.bowl %bitcoin-rpc]  %poke
        %btc-provider-action  !>(act)
    ==
  ::
  ::  Handles HTTP responses from RPC servers. Parses for errors,
  ::  then handles response. For actions that require collating multiple
  ::  RPC calls, uses req-card to call out to RPC again if more
  ::  information is required.
  ++  handle-rpc-response
    |=  [=wire response=client-response:iris]
    ^-  (quip card _state)
    ?.  ?=(%finished -.response)  `state
    =*  status  status-code.response-header.response
    ::  handle error types: connection errors, RPC errors (in order)
    ::
    =^  conn-err  state
      (connection-error status (slav %uv (snag 1 wire)))
    ?^  conn-err
      ~&  >  "connection error"
      :_  state(connected.host-info %.n)
      :-  (send-status:hc [%disconnected ~])
      ::  TODO: attempt to reestablish connection if %bad-request from client app?
      ?:  !=(%ping -.wire)
        ~[(send-update-request:hc [%| u.conn-err])]
      :*  (start-ping-timer:hc interval)
          ?~  timer  ~
          [[%pass /block-ping %arvo %b %rest u.timer] ~]
      ==
    ::
    %+  handle-rpc-result  wire
    %-  parse-result:rpc:bl
    ~!  response
    (get-rpc-response:bl response)
  ::
  ++  handle-rpc-result
    |=  [=wire r=result:rpc-types]
    ^-  (quip card _state)
    =/  req-id=@uvH
      (slav %uv (snag 1 wire))
    :: ~&  >  "btcp response received"
    ?+  -.wire  ~|("Unexpected HTTP response" !!)
        %address-info
      ?>  ?=([%get-address-info *] r)
      :_  state
      ~[(send-update-request:hc [%.y req-id %address-info +.r])]
      ::
        %tx-info
      ?>  ?=([%get-tx-vals *] r)
      :_  state
      ~[(send-update-request:hc [%.y req-id %tx-info +.r])]
      ::
        %raw-tx
      ?>  ?=([%get-raw-tx *] r)
      :_  state
      ~[(send-update-request:hc [%.y req-id %raw-tx +.r])]
      ::
        %broadcast-tx
      ?>  ?=([%broadcast-tx *] r)
      :_  state
      ~[(send-update-request:hc [%.y req-id %broadcast-tx +.r])]
      ::
        %ping
      ?>  ?=([%get-block-info *] r)
      :_  state(connected.host-info %.y, block.host-info block.r)
      :_  ~
      %-  send-status:hc
      ?:  =(block.host-info block.r)
        [%connected network.host-info block.r fee.r]
      [%new-block network.host-info block.r fee.r blockhash.r blockfilter.r]
      ::
        %block-info
      ?>  ?=([%get-block-info *] r)
      :_  state
      ~[(send-update-request:hc [%.y req-id %block-info network.host-info +.r])]
      :: 
        %histogram
      ?>  ?=([%get-histogram *] r)
      :_  state
      ~[(send-update-request:hc [%.y req-id %histogram +.r])]
      ::
        %block-headers
      ?>  ?=([%get-block-headers *] r)
      :_  state
      ~[(send-update-request:hc [%.y req-id %block-headers +.r])]
      ::
        %tx-from-pos
      ?>  ?=([%get-tx-from-pos *] r)
      :_  state
      ~[(send-update-request:hc [%.y req-id %tx-from-pos +.r])]
      ::
        %fee
      ?>  ?=([%get-fee *] r)
      :_  state
      ~[(send-update-request:hc [%.y req-id %fee +.r])]
      ::
        %psbt
      ?>  ?=([%update-psbt *] r)
      :_  state
      ~[(send-update-request:hc [%.y req-id %psbt +.r])]
      ::
        %block-txs
      ?>  ?=([%get-block-txs *] r)
      :_  state
      ~[(send-update-request:hc [%.y req-id %block-txs +.r])]
      ::
      ::  TODO: send-status error?
        %error
      ~&  >  "btcp rpc error received"
      ?>  ?=([%error *] r)
      :_  state
      ~[(send-update-request:hc [%.n req-id %rpc-error `+.r])]
    ==
  ::
  ++  connection-error
    |=  [status=@ud id=@uvH]
    ^-  [(unit error) _state]
    ?+  status  [`[id %rpc-error ~] state]
      %200  [~ state]
      %400  [`[id %bad-request status] state]
      %401  [`[id %no-auth status] state(connected.host-info %.n)]
      %502  [`[id %not-connected status] state(connected.host-info %.n)]
      %504  [`[id %not-connected status] state(connected.host-info %.n)]
    ==
  --
::
++  on-peek
  ~/  %on-peek
  |=  pax=path
  ^-  (unit (unit cage))
  ?+  pax  (on-peek:def pax)
      [%x %is-whitelisted @t ~]
    ``noun+!>((is-whitelisted:hc (ship (slav %p +>-.pax))))
    ::
      [%x %is-client @t ~]
    ``noun+!>((is-client:hc (ship (slav %p +>-.pax))))
==
::
++  on-leave  on-leave:def
++  on-agent
  |=  [wyr=(pole cord) =sign:agent:gall]
  ^-  (quip card _this)
  ?+    wyr  (on-agent:def wyr sign)
      [%ext ship=@ ~]
    ?~  src.host-info  `this
    ?>  ?&(=(src.bowl (slav %p ship.wyr)) =(u.src.host-info src.bowl))
    ?+    -.sign  (on-agent:def wyr sign)
        %watch-ack
      `this
    ::
        %fact
      ?+    p.cage.sign  `this
      ::     %atom
      ::   =/  [url=@t port=@t]  !<([@t @t] q.cage.sign)
      ::   ~&  on-agent=url
      ::   =.  host-info
      ::     :*  `[url port %.n]  `src.bowl  %.y
      ::         network.host-info  block.host-info
      ::         clients.host-info
      ::     ==
      ::   :_  this
      ::   [(start-ping-timer:hc ~s10)]~
      :: ::
          %btc-provider-status
        =/  =status  !<(status q.cage.sign)
        ?.  ?=(%new-rpc -.status)  `this
        =.  connected.host-info  %.n
        =.  api.host-info  `[url.status port.status %.n]
        :_  this
        :*  (send-status:hc [%disconnected ~])
            (start-ping-timer:hc ~s10)
            ?~  timer  ~
            [[%pass /rest %agent our.bowl^%bitcoin-rpc %poke %card !>(`card`[%pass /block-ping %arvo %b %rest u.timer])] ~]
        ==
      ==
    ==
  ==
::
++  on-fail   on-fail:def
--
::  helper core
~%  %btc-provider-helper  ..card  ~
|_  =bowl:gall
:: +*  grp  ~(. groupl bowl)
++  send-status
  |=  =status
  ^-  card
  %-  ?:  ?=(%new-block -.status)
        ~&(>> "%new-block: {<block.status>}" same)
      same
  [%give %fact ~[/clients] %btc-provider-status !>(status)]
::
:: ++  send-update-all
::   |=  =update
::   ^-  card
::   %-  ?:  ?=(%.y -.update)
::         same
::       ~&(>> "prov. err: {<p.update>}" same)
::   =-  [%give %fact ~[-] %btc-provider-update !>(update)]
::   ?~  target  /clients
::   /clients/(scot %p u.ship)
:: ::
:: ++  send-update-ship
::   |=  [=update =ship]
::   ^-  card
::   %-  ?:  ?=(%.y -.update)
::         same
::       ~&(>> "prov. err: {<p.update>}" same)
::   =-  [%give %fact ~[-] %btc-provider-update !>(update)]
::   /clients/(scot %p ship)
:: ::
++  send-update-request
  |=  =update
  ^-  card
  %-  ?:  ?=(%.y -.update)
        same
      ~&(>> "prov. err: {<p.update>}" same)
  =/  pax=path  /clients/(scot %uv -.+.update)
  [%give %fact ~[pax] %btc-provider-update !>(update)]
::  TODO
++  send-rpc-update  !!
++  is-whitelisted
  ~/  %is-whitelisted
  |=  user=ship
  ^-  ?
  |^
  ?|  public.whitelist
      =(our.bowl user)
      ?&(kids.whitelist is-kid)
      (~(has in users.whitelist) user)
      :: in-group
  ==
  ::
  ++  is-kid
    =(our.bowl (sein:title our.bowl now.bowl user))
  ::
  :: ++  in-group
  ::   =/  gs  ~(tap in groups.whitelist)
  ::   ?.  is-running:grp  %.n
  ::   |-
  ::   ?~  gs  %.n
  ::   ?:  (is-member:grp user i.gs)
  ::     %.y
  ::   $(gs t.gs)
  --
::
++  is-client
  |=  user=ship
  ^-  ?
  (~(has in clients.host-info) user)
::
++  start-ping-timer
  |=  interval=@dr
  ^-  card
  [%pass /block-ping %arvo %b %wait (add now.bowl interval)]
--
