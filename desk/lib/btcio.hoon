::  btcio: Asynchronous Bitcoin input/output functions.
::
/+  strandio, rpc=json-rpc, bc=bitcoin
|%
+$  auth
  $@  ~
  [%basic dat=@]
::
++  auth-to-base64
  |=  a=auth
  ^-  (unit @t)
  ::  ?~  a
  ?.  ?=([%basic *] a)  ~
  =-  `(cat 3 'Basic ' -)
  (en:base64:mimes:html (met 3 dat.a) dat.a)
::
+$  req-to  [url=@t =auth]
::  +request-rpc: send rpc request, with retry
::
++  request-rpc
  |=  [=req-to req=request:rpc]
  =/  m  (strand:strandio response:rpc)
  ^-  form:m
  ;<  res=(list response:rpc)  bind:m
    (request-batch-rpc-loose req-to req ~)
  ?:  ?=([* ~] res)
    (pure:m i.res)
  %+  strand-fail:strandio
    %unexpected-multiple-results
  [>(lent res)< ~]
::
::  +request-batch-rpc-loose: send rpc requests, with retry
::
::    sends a batch request. produces results for all requests in the batch,
::    including the ones that are unsuccessful.
::
++  request-batch-rpc-loose
  |=  [req-to reqs=(list request:rpc)]
  |^  %+  (retry:strandio results)
        `10
      attempt-request
  ::
  +$  result   response:rpc
  +$  results  (list response:rpc)
  ::
  ++  attempt-request
    =/  m  (strand:strandio ,(unit results))
    ^-  form:m
    =/  =request:http
      :*  method=%'POST'
          url=url
          ^=  header-list
          =+  'Content-Type'^'application/json'
          ?~  auth  [- ~]
          ['Authorization'^(need (auth-to-base64 auth)) - ~]
        ::
          ^=  body
          %-  some  %-  as-octs:mimes:html
          %-  en:json:html
          a+(turn reqs request-to-json:rpc)
      ==
    ;<  ~  bind:m
      (send-request:strandio request)
    ;<  rep=(unit client-response:iris)  bind:m
      take-maybe-response:strandio
    ?~  rep
      (pure:m ~)
    (parse-responses u.rep)
  ::
  ++  parse-responses
    |=  =client-response:iris
    =/  m  (strand:strandio ,(unit results))
    ^-  form:m
    ?>  ?=(%finished -.client-response)
    ?~  full-file.client-response
      (pure:m ~)
    =/  body=@t  q.data.u.full-file.client-response
    =/  jon=(unit json)  (de:json:html body)
    ?~  jon
      (pure:m ~)
    =/  array=(unit (list response:rpc))
      ((ar:dejs-soft:format parse-one-response) u.jon)
    ?~  array
      (strand-fail:strandio %rpc-result-incomplete-batch >u.jon< ~)
    (pure:m array)
  ::
  ++  parse-one-response
    |=  =json
    ^-  (unit response:rpc)
    ?.  &(?=([%o *] json) (~(has by p.json) 'error'))
      =/  res=(unit [@t ^json])
        %.  json
        =,  dejs-soft:format
        (ot id+so result+some ~)
      ?~  res  ~
      `[%result u.res]
    ~|  parse-one-response=json
    =/  error=(unit [id=@t ^json code=@ta mssg=@t])
      %.  json
      =,  dejs-soft:format
      ::  A 'result' member is present in the error
      ::  response when using ganache, even though
      ::  that goes against the JSON-RPC spec
      ::
      (ot id+so result+some error+(ot code+no message+so ~) ~)
    ?~  error  ~
    =*  err  u.error
    `[%error id.err code.err mssg.err]
  --
::
++  render-hex-bytes
  ::  atom to string of hex bytes without 0x prefix and dots.
  |=  a=octs
  ^-  @t
  (crip ((x-co:co (mul 2 p.a)) q.a))
::
++  get-raw-transaction
  |=  [=req-to id=(unit @t) txid=@ux]
  =/  m  (strand:strandio (unit tx:bc))
  ^-  form:m
  ;<  res=response:rpc  bind:m
    %+  request-rpc  req-to
    ^-  request:rpc
    :*  ?~(id 'get-raw-transaction' u.id)
        '2.0'
        'getrawtransaction'
        list+[s+(render-hex-bytes 32 txid) b+| ~]
    ==
  ?.  ?=([%result * [%s *]] res)  (pure:m ~)
  ?~  res=(de:base16:mimes:html p.res.res)  (pure:m ~)
  (pure:m `[txid (decodew:txu:bc u.res)])
::
++  get-block-count
  |=  [=req-to id=(unit @t)]
  =/  m  (strand:strandio (unit @ud))
  ^-  form:m
  ;<  res=response:rpc  bind:m
    %+  request-rpc  req-to
    ^-  request:rpc
    :*  ?~(id 'get-block-count' u.id)
        '2.0'
        'getblockcount'
        list+~
    ==
  ?.  ?=([%result *] res)  (pure:m ~)
  (pure:m `(ni:dejs:format res.res))
::
++  get-block-hash
  |=  [=req-to id=(unit @t) height=@ud]
  =/  m  (strand:strandio (unit @ux))
  ^-  form:m
  ;<  res=response:rpc  bind:m
    %+  request-rpc  req-to
    ^-  request:rpc
    :*  ?~(id 'get-block-hash' u.id)
        '2.0'
        'getblockhash'
        list+[(numb:enjs:format height) ~]
    ==
  ?.  ?=([%result * [%s *]] res)  (pure:m ~)
  ?~  res=(de:base16:mimes:html p.res.res)  (pure:m ~)
  (pure:m `q.u.res)
::
++  get-block
  |=  [=req-to id=(unit @t) bloq=$%([%num p=@ud] [%hax p=@ux])]
  =/  m  (strand:strandio (unit block:bc))
  ^-  form:m
  ?-  -.bloq
    %hax  (get-block-by-hash req-to id p.bloq)
    %num  (get-block-by-number req-to id p.bloq)
  ==
::
++  get-block-by-hash
  |=  [=req-to id=(unit @t) hax=@ux]
  =/  m  (strand:strandio (unit block:bc))
  ^-  form:m
  ;<  res=response:rpc  bind:m
    %+  request-rpc  req-to
    ^-  request:rpc
    :*  ?~(id 'get-block' u.id)
        '2.0'
        'getblock'
        list+[s+(render-hex-bytes 32 hax) (numb:enjs:format 2) ~]
    ==
  ?.  ?=([%result * [%o *]] res)  (pure:m ~)
  (pure:m `(parse-block res.res))
::
++  mine-blocks-to-address
  |=  [=req-to id=(unit @t) address=cord n=@]
  =/  m  (strand:strandio (unit (list @ux)))
  ^-  form:m
  ;<  res=response:rpc  bind:m
    %+  request-rpc  req-to
    ^-  request:rpc
    :*  ?~(id 'generate-to-address' u.id)
        '2.0'
        'generatetoaddress'
        list+[(numb:enjs:format n) s+address ~]
    ==
  ?.  ?=([%result *] res)  (pure:m ~)
  =,  dejs:format
  (pure:m `((ar (cu |=([* @] +<+) parse-hex)) res.res))
::
++  send-raw-transaction
  |=  [=req-to id=(unit @t) raw=octs]
  =/  m  (strand:strandio (unit @ux))
  ^-  form:m
  ;<  res=response:rpc  bind:m
    %+  request-rpc  req-to
    ^-  request:rpc
    :*  ?~(id 'send-raw-transaction' u.id)
        '2.0'
        'sendrawtransaction'
        list+[s+(render-hex-bytes raw) ~]
    ==
  ?.  ?=([%result * [%s *]] res)  (pure:m ~)
  ?~  res=(de:base16:mimes:html p.res.res)  (pure:m ~)
  (pure:m `q.u.res)
::
++  get-block-by-number
  |=  [=req-to id=(unit @t) height=@ud]
  =/  m  (strand:strandio (unit block:bc))
  ^-  form:m
  ;<  res=(unit @ux)  bind:m
    (get-block-hash req-to ?~(id ~ `(cat 3 'get-block-hash-' u.id)) height)
  ;<  *  bind:m  (sleep:strandio ~s2)
  ?~  res  (pure:m ~)
  (get-block-by-hash req-to id u.res)
::
++  parse-block
  |=  jon=json
  ^-  block:bc
  =-  [hax (reward-from-height height) height txs]
  ^-  [hax=@ux height=@ud txs=(list [txh=@ux tx=dataw:tx:bc])]
  %.  jon
  =,  dejs:format
  %-  ot
  :~  hash+(cu |=([* @] +<+) parse-hex)
      height+ni
      tx+(ar parse-tx)
  ==
::
++  parse-tx
  |=  jon=json
  ^-  [txid=@ux tx=dataw:tx:bc]
  %.  jon
  =,  dejs:format
  %-  ot
  :~  txid+(cu |=([* @] +<+) parse-hex)
      hex+(cu |=(a=octs (decodew:txu:bc a)) parse-hex)
  ==
::
++  parse-hex
  |=  jon=json
  ?>  ?=([%s *] jon)
  (hex-to-num p.jon)
::
++  reward-from-height
  |=  a=@ud
  ^-  @ud
  (div 5.000.000.000 (bex (div a 210.000)))
::
++  hex-to-num
  |=  a=@t
  %-  need
  (de:base16:mimes:html a)
::::  +read-contract: calls a read function on a contract, produces result hex
::::
::++  read-contract
::  |=  [=req-to req=proto-read-request:rpc:ethereum]
::  =/  m  (strand:strandio ,@t)
::  ;<  res=(list [id=@t res=@t])  bind:m
::    (batch-read-contract-strict req-to [req]~)
::  ?:  ?=([* ~] res)
::    (pure:m res.i.res)
::  %+  strand-fail:strandio
::    %unexpected-multiple-results
::  [>(lent res)< ~]
::::  +batch-read-contract-strict: calls read functions on contracts
::::
::::    sends a batch request. produces results for all requests in the batch,
::::    but only if all of them are successful.
::::
::++  batch-read-contract-strict
::  |=  [=req-to reqs=(list proto-read-request:rpc:ethereum)]
::  |^  =/  m  (strand:strandio ,results)
::      ^-  form:m
::      ;<  res=(list [id=@t =json])  bind:m
::        %+  request-batch-rpc-strict  req-to
::        (turn reqs proto-to-rpc)
::      =+  ^-  [=results =failures]
::        (roll res response-to-result)
::      ?~  failures  (pure:m results)
::      (strand-fail:strandio %batch-read-failed-for >failures< ~)
::  ::
::  +$  results   (list [id=@t res=@t])
::  +$  failures  (list [id=@t =json])
::  ::
::  ++  proto-to-rpc
::    |=  proto-read-request:rpc:ethereum
::    ^-  [(unit @t) request:rpc:ethereum]
::    :-  id
::    :+  %eth-call
::      ^-  call:rpc:ethereum
::      [~ to ~ ~ ~ `tape`(encode-call:rpc:ethereum function arguments)]
::    [%label %latest]
::  ::
::  ++  response-to-result
::    |=  [[id=@t =json] =results =failures]
::    ^+  [results failures]
::    ?:  ?=(%s -.json)
::      [[id^p.json results] failures]
::    [results [id^json failures]]
::  --
::::
::::
::++  get-latest-block
::  |=  =req-toa
::  =/  m  (strand:strandio ,block)
::  ^-  form:m
::  ;<  =json  bind:m
::    (request-rpc req-to `'block number' %eth-block-number ~)
::  (get-block-by-number req-to (parse-eth-block-number:rpc:ethereum json))
::::
::++  get-block-by-number
::  |=  [=req-toa =number:block]
::  =/  m  (strand:strandio ,block)
::  ^-  form:m
::  |^
::  %+  (retry:strandio ,block)  `10
::  =/  m  (strand:strandio ,(unit block))
::  ^-  form:m
::  ;<  =json  bind:m
::    %+  request-rpc  req-to
::    :-  `'block by number'
::    [%eth-get-block-by-number number |]
::  (pure:m (parse-block json))
::  ::
::  ++  parse-block
::    |=  =json
::    ^-  (unit block)
::    =<  ?~(. ~ `[[&1 &2] |2]:u)
::    ^-  (unit [@ @ @])
::    ~|  json
::    %.  json
::    =,  dejs-soft:format
::    %-  ot
::    :~  hash+parse-hex
::        number+parse-hex
::        'parentHash'^parse-hex
::    ==
::  ::
::  ++  parse-hex  |=(=json `(unit @)`(some (parse-hex-result:rpc:ethereum json)))
::  --
::::
::++  get-tx-by-hash
::  |=  [=req-toa tx-hash=@ux]
::  =/  m  (strand:strandio transaction-result:rpc:ethereum)
::  ^-  form:m
::  ;<  =json  bind:m
::    %+  request-rpc  req-to
::    :*  `'tx by hash'
::        %eth-get-transaction-by-hash
::        tx-hash
::    ==
::  %-  pure:m
::  (parse-transaction-result:rpc:ethereum json)
::::
::++  get-logs-by-hash
::  |=  [=req-toa =hash:block contracts=(list address) =topics]
::  =/  m  (strand:strandio (list event-log:rpc:ethereum))
::  ^-  form:m
::  ;<  =json  bind:m
::    %+  request-rpc  req-to
::    :*  `'logs by hash'
::        %eth-get-logs-by-hash
::        hash
::        contracts
::        topics
::    ==
::  %-  pure:m
::  (parse-event-logs:rpc:ethereum json)
::::
::++  get-logs-by-range
::  |=  $:  =req-toa
::          contracts=(list address)
::          =topics
::          =from=number:block
::          =to=number:block
::      ==
::  =/  m  (strand:strandio (list event-log:rpc:ethereum))
::  ^-  form:m
::  ;<  =json  bind:m
::    %+  request-rpc  req-to
::    :*  `'logs by range'
::        %eth-get-logs
::        `number+from-number
::        `number+to-number
::        contracts
::        topics
::    ==
::  %-  pure:m
::  (parse-event-logs:rpc:ethereum json)
::::
::++  get-next-nonce
::  |=  [=req-toa =address]
::  =/  m  (strand:strandio ,@ud)
::  ^-  form:m
::  ;<  =json  bind:m
::    %^  request-rpc  req-to  `'nonce'
::    [%eth-get-transaction-count address [%label %latest]]
::  %-  pure:m
::  (parse-eth-get-transaction-count:rpc:ethereum json)
::::
::++  get-balance
::  |=  [=req-toa =address]
::  =/  m  (strand:strandio ,@ud)
::  ^-  form:m
::  ;<  =json  bind:m
::    %^  request-rpc  req-to  `'balance'
::    [%eth-get-balance address [%label %latest]]
::  %-  pure:m
::  (parse-eth-get-balance:rpc:ethereum json)
--
