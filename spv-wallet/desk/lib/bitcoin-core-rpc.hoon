/-  *bitcoin-core-rpc
|%
::
++  make-rpc-http-request
  |=  [fig=node-config req=request]
  ^-  request:http
  =/  dat  [~ (as-octs:mimes:html (en-json-rpc req))]
  =/  aut  (en:base64:mimes:html (met 3 auth.fig) auth.fig)
  =;  hed
      [%'POST' url.fig hed dat]
  :~  ['Content-Type' 'application/json']
      ['Authorization' (cat 3 'Basic ' aut)]
  ==
::
++  handle-rpc-http-response
  |=  res=client-response:iris
  ^-  (unit response)
  ?.  ?=([%finished * ^] res)  ~
  :-  ~
  %-  de-json-rpc  q.data.u.full-file.res
::
++  en-json-rpc
  |=  req=request
  ^-  cord
  %-  en:json:html
  ^-  json
  ?-  -.req
    %get-block-count
      %:  make-request-object  -.req  ~  %getblockcount
          ~
      ==
    %get-block-hash
      %:  make-request-object  -.req  ~  %getblockhash
      :~  [%n (crip ((d-co:co 1) block-height.req))]
      ==  ==
    %get-block-header
      %:  make-request-object  -.req  ~  %getblockheader
      :~  [%s (print-hex-32 block-hash.req)]
      ==  ==
    %get-block-header-batch
      :-  %a
      %+  spun  batch.req
      |=  [=block-hash a=@ud]
      :_  +(a)
      %:  make-request-object  -.req  [~ a]  %getblockheader
      :~  [%s (print-hex-32 block-hash)]
      ==  ==
    %get-block
      %:  make-request-object  -.req  ~  %getblock
      :~  [%s (print-hex-32 block-hash.req)]
          [%n '2']  :: full verbosity
      ==  ==
    %get-mempool-entries
      %:  make-request-object  -.req  ~  %getrawmempool
      :~  [%b &]  :: full verbosity
      ==  ==
    %get-transaction
      %:  make-request-object  -.req  ~  %getrawtransaction
      :~  [%s (print-hex-32 txid.req)]
          [%b &]  :: full verbosity
          [%s (print-hex-32 block-hash.req)]
      ==  ==
    %get-mempool-transaction
      %:  make-request-object  -.req  ~  %getrawtransaction
      :~  [%s (print-hex-32 txid.req)]
          [%b &]  :: full verbosity
      ==  ==
    %get-mempool-transaction-batch
      :-  %a
      %+  spun  batch.req
      |=  [=txid a=@ud]
      :_  +(a)
      %:  make-request-object  -.req  [~ a]  %getrawtransaction
      :~  [%s (print-hex-32 txid)]
          [%b &]  :: full verbosity
      ==  ==
    %send-transaction
      %:  make-request-object  -.req  ~  %sendrawtransaction
      :~  [%s (print-hex hex.req)]
      ==  ==
  ==
::
++  de-json-rpc
  |=  cod=cord
  ^-  response
  =/  jon  (need (de:json:html cod))
  ?+  jon  !!
  ::
      [%o *]
    =/  dej  ~(. dj jon)
    =/  ver  t:(got:dej 'jsonrpc')
    =/  tag  t:(got:dej 'id')
    ?:  (has:dej 'error')
      =/  erj  (got:dej 'error')
      ~&  >>>  [%bitcoin-rpc-error tag]
      =/  cod  t:(got:erj 'code')
      =/  mes  t:(got:erj 'message')
      ~&  >>>  [%code cod]
      ~&  >>>  [%message mes]
      !!
    =/  res  (got:dej 'result')
    ?+  tag  !!
      %get-block-count          [tag ud:res]
      %get-block-hash           [tag ux:res]
      %get-block-header         [tag (json-to-block-header jo:res)]
      %get-block                [tag (json-to-partial-block jo:res)]
      %get-transaction          [tag (json-to-full-transaction jo:res)]
      %get-mempool-transaction  [tag (json-to-full-transaction jo:res)]
      %get-mempool-entries      [tag (json-to-mempool-entries jo:res)]
      %send-transaction         [tag ux:res]
    ==
  ::
      [%a *]
    |^  ^-  response
    =.  p.jon
      :: the json rpc specification states that batch responses
      :: may be returned in any order,
      :: so we sort the response objects by batch index encoded
      :: in the request id to ensure the response is ordered
      %+  sort  p.jon
      |=  [a=json b=json]
      %+  lth
          p:(parse-batch-id a)
          p:(parse-batch-id b)
    =/  tag
      ?>  ?=(^ p.jon)
      =<  q
      %-  parse-batch-id
          i.p.jon
    ?+  tag  !!
    ::
        %get-block-header-batch
      :-  tag
      %+  murn  p.jon
      |=  j=json
      ^-  (unit block-header)
      =/  dej  ~(. dj j)
      ?:  (has:dej 'error')  ~
      :-  ~
      %-  json-to-block-header
          jo:(got:dej 'result')
    ::
        %get-mempool-transaction-batch
      :-  tag
      %+  murn  p.jon
      |=  j=json
      ^-  (unit partial-transaction)
      =/  dej  ~(. dj j)
      ?:  (has:dej 'error')  ~
      :-  ~
      %-  json-to-partial-transaction
          jo:(got:dej 'result')
    ::
    ==
    ++  parse-batch-id
      |=  j=json
      ^-  (pair @ud @t)
      =/  rid  (trip t:(~(got dj j) 'id'))
      =/  sep  (need (find " " rid))
      :-  (slav %ud (crip (scag sep rid)))
          (crip (slag +(sep) rid))
    --
  ::
  ==
::
+$  method
  $?  %getblockcount
      %getblockhash
      %getblockheader
      %getblock
      %getrawmempool
      %getrawtransaction
      %sendrawtransaction
  ==
::
++  make-request-object
  |=  $:  req-tag=@tas
          batch-num=(unit @ud)
          =method
          params=(list json)
      ==
  ^-  json
  =/  req-id
    ^-  @t
    ?~  batch-num  req-tag
    %+  rap  3
    :~  (scot %ud u.batch-num)
        ' '
        req-tag
    ==
  :-  %o
  %-  malt
  ^-  (list [@t json])
  :~  ['jsonrpc' [%s '2.0']]
      ['id' [%s req-id]]
      ['method' [%s method]]
      ['params' [%a params]]
  ==
::
++  print-hex
  |=  hex=@ux
  ^-  @t
  %+  en:base16:mimes:html  (met 3 hex)  hex
::
++  print-hex-32
  |=  hex=@ux
  ^-  @t
  %+  en:base16:mimes:html  32  hex
::
++  json-to-mempool-entries
  |=  jon=json
  ^-  mempool-entries
  %+  turn  ~(tap by ~(ob dj jon))
  |=  [id=@t j=json]
  ^-  mempool-entry
  =/  dej  ~(. dj j)
  :*  ~(ux dj [%s id])
      da:(got:dej 'time')
      (silt (turn li:(got:dej 'depends') |=(i=json ~(ux dj i))))
      (silt (turn li:(got:dej 'spentby') |=(i=json ~(ux dj i))))
  ==
::
++  json-to-block-header
  |=  jon=json
  ^-  block-header
  =/  dej  ~(. dj jon)
  :*  ud:(got:dej 'version')
      da:(got:dej 'time')
      ux:(got:dej 'bits')
      ud:(got:dej 'nonce')
      ?.  (has:dej 'previousblockhash')  0x0
      ux:(got:dej 'previousblockhash')
      ux:(got:dej 'merkleroot')
  ==
::
++  json-to-partial-block
  |=  jon=json
  ^-  partial-block
  =/  dej  ~(. dj jon)
  :^  ud:(got:dej 'height')
      ux:(got:dej 'hash')
      ?.  (has:dej 'nextblockhash')  ~
      :_  ux:(got:dej 'nextblockhash')  ~
  %+  turn
      li:(got:dej 'tx')
      json-to-partial-transaction
::
++  json-to-partial-transaction
  |=  jon=json
  ^-  partial-transaction
  =/  dej  ~(. dj jon)
  =/  vin
    %+  murn  li:(got:dej 'vin')
    |=  jom=json
    ^-  (unit outpoint)
    =.  dej  ~(. dj jom)
    ?:  (has:dej 'coinbase')  ~
    :-  ~
    :*  ux:(got:dej 'txid')
        ud:(got:dej 'vout')
    ==
  =/  vout
    %+  turn  li:(got:dej 'vout')
    |=  jom=json
    ^-  [vout script-hash]
    =.  dej  ~(. dj jom)
    :-  ud:(got:dej 'n')
    %.  ux:(got:(got:dej 'scriptPubKey') 'hex')
        sha-256:sha
  :*  ux:(got:dej 'txid')
      vin
      vout
  ==
::
++  json-to-full-transaction
  |=  jon=json
  ^-  transaction
  =/  dej  ~(. dj jon)
  :*  ux:(got:dej 'txid')
      ux:(got:dej 'hash')
      ud:(got:dej 'size')
      ud:(got:dej 'vsize')
      ud:(got:dej 'weight')
      ud:(got:dej 'version')
      ud:(got:dej 'locktime')
      ux:(got:dej 'hex')
      %+  murn  li:(got:dej 'vin')
      |=  jom=json
      ^-  (unit transaction-vin)
      =.  dej  ~(. dj jom)
      ?:  (has:dej 'coinbase')  ~  :: TODO: coinbase input?
      :-  ~
      :*  ux:(got:dej 'txid')
          ud:(got:dej 'vout')
          ux:(got:(got:dej 'scriptSig') 'hex')
          ud:(got:dej 'sequence')
          ?.  (has:dej 'txinwitness')  ~
          %+  turn  li:(got:dej 'txinwitness')
          |=  j=json
          %~  ux  dj  j
      ==
      %+  turn  li:(got:dej 'vout')
      |=  jom=json
      ^-  transaction-vout
      =.  dej  ~(. dj jom)
      =/  sat  t:(got:dej 'value')
      =/  pub  (got:dej 'scriptPubKey')
      :*  (scan (skip (trip sat) |=(c=@t =('.' c))) dem)
          ux:(got:pub 'hex')
          ?:  (has:pub 'address')
            :-  t:(got:pub 'address')  ~
          ?.  (has:pub 'addresses')  ~
          %+  turn  li:(got:pub 'addresses')
          |=  j=json
          %~  t  dj  j
      ==
  ==
::
++  dj
  |_  jon=json
  ++  dj-cor  .
  ::
  ++  got
    |=  key=@t
    ?>  ?=([%o *] jon)
    %_  dj-cor
      jon  (~(got by p.jon) key)
    ==
  ::
  ++  has
    |=  key=@t
    ?>  ?=([%o *] jon)
    %-  ~(has by p.jon)  key
  ::
  ++  jo  jon
  ::
  ++  ob
    ^-  (map @t json)
    ?>  ?=([%o *] jon)  p.jon
  ::
  ++  li
    ^-  (list json)
    ?>  ?=([%a *] jon)  p.jon
  ::
  ++  t
    ^-  @t
    ?+  jon  !!
      [%s *]  p.jon
      [%n *]  p.jon
      [%b *]  (scot %f p.jon)
      ~  ''
    ==
  ::
  ++  ud
    ^-  @ud
    ?>  ?=([%n *] jon)
    %+  rash  p.jon  dem
  ::
  ++  ux
    ^-  @ux
    ?>  ?=([%s *] jon)
    =<  q
    %-  need
    %-  de:base16:mimes:html
        p.jon
  ::
  ++  da
    ^-  @da
    ?>  ?=([%n *] jon)
    %-  from-unix:chrono:userlib
    %+  rash  p.jon  dem
  ::
  --
::
--

