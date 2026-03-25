/-  *indexer
/+  btr=bitcoin-core-rpc, bip32=bip32-spv
|%
::
+$  timer
  $:  interval=@dr
      last=(unit @da)
  ==
+$  timers
  $:  polling=$~([~m1 ~] timer)
  ==
::
+$  state-0
  $:  block-count=block-height
      btc-node-config=$@(~ node-config)
      =timers
      =accounts
      =utxo-set
      =sh-index
      =sh-mempool
      mempool-outputs=utxo-set
      =mempool-txids
      =block-headers
  ==
+$  state-n
  $%  [%0 state-0]
  ==
+$  card  card:agent:gall
--
::
=|  $>(%0 state-n)
=*  state  -
=>
::
|_  [=bowl:gall cards=(list card)]
++  cor   .
++  abet  :-  (flop cards)  state
++  emit  |=  =card  cor(cards [card cards])
++  emil  |=  caz=(list card)  cor(cards (welp (flop caz) cards))
::
++  poke
  |=  [mak=mark vaz=vase]
  ^+  cor
  ?>  =(our.bowl src.bowl)
  ?+  mak  ~|(bad-poke/mak !!) 
  ::
      %log
    ~&  [%headers ~(wyt by block-headers)]
    ~&  >  [%accounts accounts]
    ~&  >>  [%utxo-set utxo-set]
    ~&  >>>  [%sh-index sh-index]
    ~&  >>  [%sh-mempool sh-mempool]
    ~&  [%mempool-txids (lent mempool-txids)]
    ~&  '--------------------------------------------------------'
    cor
  ::
      %configure-node
    ?>  =(src.bowl our.bowl)
    =.  btc-node-config  !<(node-config vaz)
    =^  caz  polling.timers  ~(reset ti /polling polling.timers)
    %-  emil  caz
  ::
      %add-account
    ?>  =(src.bowl our.bowl)
    =/  new  !<(new-account-args vaz)                :: TODO: add many at once
    ?:  (~(has by accounts) xpub.new)
      ~&  >>  'account already added'
      !!
    ?~  btc-node-config
      ~&  >>>  'node configuration missing'
      !!
    =/  aco  ac-abet:(ac-new:ac new)
    =.  accounts  (~(put by accounts) xpub.new aco)
    =^  caz  polling.timers  ~(reset ti /polling polling.timers)
    %-  emil  caz
  ::
      %del-account
    ?>  =(src.bowl our.bowl)
    =/  xub  !<(xpub vaz)
    =/  aco  (~(get by accounts) xub)
    ?~  aco
      ~&  >>>  'account not found'
      !!
    =.  cor  (del-account-from-index u.aco)
    %_  cor
      accounts  (~(del by accounts) xub)
    ==
  ::
      %broadcast-transaction
    ?>  ?=(^ btc-node-config)
    =/  dat  !<(@ux vaz)
    =/  act  [%send-transaction dat]
    %-  emit
    %+  make-request-card
        /broadcast-transaction/[now-t]
    %+  make-rpc-http-request:btr
        btc-node-config
        act
  ::
  ==
::
++  peek                 :: TODO: peek
  |=  poe=(pole @ta)
  ^-  (unit (unit cage))
  ~
::
++  watch
  |=  poe=(pole @ta)
  ^+  cor
  ?+  poe  !!
  ::
      [%block-headers ~]
    %-  emit
    :*  %give  %fact  ~
        %block-headers-update  !>(`block-headers-update`[%init block-headers])
    ==
  ::
      [%script-hash hash=@ta ~]
    =/  has  (slav %ux hash.poe)
    =/  his  (~(gut by sh-index) has *sh-tx-history)
    =/  mem  (~(gut by sh-mempool) has *sh-tx-mempool)
    %-  emit
    :*  %give  %fact  ~
        %script-hash-update  !>(`script-hash-update`[%init mem his])
    ==
  ::
      [%transaction %mempool txid=@ta ~]
    ?>  ?=(^ btc-node-config)
    =/  tix  (slav %ux txid.poe)
    =/  act  [%get-mempool-transaction tix]
    %-  emit
    %+  make-request-card
        /transaction/mempool/[txid.poe]/[now-t]
    %+  make-rpc-http-request:btr
        btc-node-config
        act
  ::
      [%transaction block-hash=@ta txid=@ta ~]
    ?>  ?=(^ btc-node-config)
    =/  has  (slav %ux block-hash.poe)
    =/  tix  (slav %ux txid.poe)
    =/  act  [%get-transaction has tix]
    %-  emit
    %+  make-request-card
        /transaction/[block-hash.poe]/[txid.poe]/[now-t]
    %+  make-rpc-http-request:btr
        btc-node-config
        act
  ::
      [%indexer-status ~]  :: TODO: indexer status
    cor
  ::
  ==
::
++  leave
  |=  poe=(pole @ta)
  ^+  cor
  cor
::
++  fail
  |=  [tem=term tan=tang]
  ^+  cor
  cor
::
++  agent
  |=  [wir=wire sin=sign:agent:gall]
  ^+  cor
  cor
::
++  arvo
  |=  [wir=(pole @ta) sin=sign-arvo]
  ^+  cor
  ?+  wir  cor
  ::
      [%retry %transaction mempool-or-block-hash=@ta txid=@ta *]
    ?>  ?=([%behn %wake *] sin)
    ?>  ?=(^ btc-node-config)
    =/  tix  (slav %ux txid.wir)
    =/  act
      ?+  mempool-or-block-hash.wir
                  [%get-transaction (slav %ux mempool-or-block-hash.wir) tix]
        %mempool  [%get-mempool-transaction tix]
      ==
    %-  emit
    %+  make-request-card
        /transaction/[mempool-or-block-hash.wir]/[txid.wir]/[now-t]
    %+  make-rpc-http-request:btr
        btc-node-config
        act
  ::
      [%transaction mempool-or-block-hash=@ta txid=@ta *]
    ?+  sin  cor
    ::
        [%iris %http-response %cancel *]
      ~&  >>>  'transaction request interrupted'
      ~&  >>>  'retrying...'
      %-  emil
      =>  ~(reset ti /retry/transaction/[mempool-or-block-hash.wir]/[txid.wir] [~s0 ~])
          -
    ::
        [%iris %http-response %finished *]
      =*  sus  status-code.response-header.client-response.sin
      ?:  (gte sus 400)
        ~&  >>>  (cat 3 'transaction request failed: ' (scot %ud sus))
        ~&  >>>  'retrying...'
        %-  emil
        =>  ~(set ti /retry/transaction/[mempool-or-block-hash.wir]/[txid.wir] [~m1 ~])
            -
      =/  res  (need (handle-rpc-http-response:btr client-response.sin))
      =/  dat
        ^-  transaction
        ?+  -.res  !!
          %get-transaction  transaction.res
          %get-mempool-transaction  transaction.res
        ==
      =/  for  /transaction/[mempool-or-block-hash.wir]/[txid.wir]
      %-  emil
      :~  [%give %fact [for ~] %transaction-update !>(dat)]
          [%give %kick [for ~] ~]
      ==
    ::
    ==
  ::
      [%polling *]
    ?+  sin  cor
    ::
        [%behn %wake *]
      ?:  =(~ last.polling.timers)  cor
      :: ~&  >  'polling...'
      ?.  .?(btc-node-config)
        ~&  >>>  'node configuration missing'
        ~&  >>>  'polling stopped'
        =.  last.polling.timers  ~
        cor
      =^  caz  polling.timers  ~(set ti /polling polling.timers)
      =.  cor  (emil caz)
      :: catch up any headers if behind
      =.  cor
        =/  hed  next-block-header-height
        ?~  hed  cor
        ?>  ?=(^ btc-node-config)
        =/  act  [%get-block-hash u.hed]
        %-  emit
        %+  make-request-card
            /block-headers/[(scot %ud u.hed)]/[now-t]
        %+  make-rpc-http-request:btr
            btc-node-config
            act
      :: catch up any accounts that are behind
      =.  cor
        =/  myn  (get-min-block-start accounts)
        ?~  myn  cor
        ?:  (gth u.myn block-count)  cor
        ?>  ?=(^ btc-node-config)
        =/  act  [%get-block-hash u.myn]
        %-  emit
        %+  make-request-card
            /index/[(scot %ud u.myn)]/start/[now-t]
        %+  make-rpc-http-request:btr
            btc-node-config
            act
      :: get the current mempool entries
      =.  cor
        ?>  ?=(^ btc-node-config)
        =/  act  [%get-mempool-entries ~]
        %-  emit
        %+  make-request-card
            /mempool/entries/[now-t]
        %+  make-rpc-http-request:btr
            btc-node-config
            act
      :: poll for the current block count
      ?>  ?=(^ btc-node-config)
      %-  emit
      %+  make-request-card
          /polling/[now-t]
      %+  make-rpc-http-request:btr
          btc-node-config
          [%get-block-count ~]
    ::
        [%iris %http-response %finished *]
      =*  sus  status-code.response-header.client-response.sin
      ?:  (gte sus 400)
        ~&  >>>  (cat 3 'polling request failed: ' (scot %ud sus))
        cor
      =/  res  (need (handle-rpc-http-response:btr client-response.sin))
      ?+  -.res  cor
      ::
          %get-block-count
        ?:  =(block-height.res block-count)  cor
        =.  block-count  block-height.res
        :: start indexing the new block if any accounts are up to date
        ?~  (get-accounts-for-block-height block-height.res)  cor
        ?>  ?=(^ btc-node-config)
        =/  act  [%get-block-hash block-height.res]
        %-  emit
        %+  make-request-card
            /index/[(scot %ud block-height.res)]/start/[now-t]
        %+  make-rpc-http-request:btr
            btc-node-config
            act
      ::
      ==
    ::
    ==
  ::
      [%mempool *]
    ?+  sin  cor
    ::
        [%iris %http-response %finished *]
      =*  sus  status-code.response-header.client-response.sin
      ?:  (gte sus 400)
        ~&  >>>  (cat 3 'mempool request failed: ' (scot %ud sus))
        cor
      =/  res  (need (handle-rpc-http-response:btr client-response.sin))
      ?+  -.res  !!
      ::
          %get-mempool-entries
        =/  txs  (mempool-entries-to-ordered-txid-list mempool-entries.res)
        :: get the set of txids from the cache no longer in the current
        :: mempool list, and handle removing them
        =/  del  (~(dif in mempool-txids) (silt txs))
        =^  caz  sh-mempool
          :: rebuild sh-mempool skipping any that now have no transactions
          :: and produce subscription updates for those deleted.
          :: note that the updates should be built in reverse order
          %-  ~(rep by sh-mempool)
          |=  $:  [key=script-hash val=sh-tx-mempool]
                  [caz=(list card) new=^sh-mempool]
              ==
          =^  c=(list card)  val
            %+  roll  val
            |=  [v=sh-tx c=(list card) n=sh-tx-mempool]
            ?.  (~(has in del) ?-(-.v %& txid.p.v, %| p.v))
              :-  c
              %+  snoc  n  v
            :_  n
            :_  c
            :*  %give  %fact  [/script-hash/[(scot %ux key)] ~]
                %script-hash-update  !>(`script-hash-update`[%mempool-del v])
            ==
          :-  (weld c caz)
          ?~  val  new
          %+  ~(put by new)
              key
              val
        =.  cor  (emil caz)
        =:  mempool-txids  (~(dif in mempool-txids) del)
            mempool-outputs
              %-  ~(rep by mempool-outputs)
              |=  [[k=outpoint v=script-hash] a=^utxo-set]
              ?:  (~(has in del) txid.k)  a
              %+  ~(put by a)  k  v
          ==
        =/  new
          %+  skip  txs
          |=  v=txid
          %-  ~(has in mempool-txids)
              v
        ?~  new  cor
        :: make a batch request for any new transactions in the mempool
        ?>  ?=(^ btc-node-config)
        =/  act  [%get-mempool-transaction-batch (scag 2.000 `(list txid)`new)]  :: TODO: make batch size limit configurable
        %-  emit
        %+  make-request-card
            /mempool/transactions/[now-t]
        %+  make-rpc-http-request:btr
            btc-node-config
            act
      ::
          %get-mempool-transaction-batch
        :: handle a batch request for transactions added to the mempool
        |-  ^+  cor
        ~+
        ?~  batch.res  cor
        :: do not process this transaction if redundant
        ?:  (~(has in mempool-txids) txid.i.batch.res)
          %=  $
            batch.res  t.batch.res
          ==
        :: handle mempool transaction inputs
        =.  cor
          =*  inp  inputs.i.batch.res
          |-  ^+  cor
          ?~  inp  cor
          =/  txo
            ^-  (unit script-hash)
            =/  utx  (~(get by utxo-set) i.inp)
            ?^  utx  utx
            =/  mem  (~(get by mempool-outputs) i.inp)
            ?^  mem  mem
            ~
          ?~  txo  $(inp t.inp)
          =/  new
            ^-  sh-tx
            :-  %|
                txid.i.batch.res
          =.  cor
            %-  emit
            :*  %give  %fact  [/script-hash/[(scot %ux u.txo)] ~]
                %script-hash-update  !>(`script-hash-update`[%mempool-add new])
            ==
          %=  $
            inp  t.inp
            sh-mempool
              %+  ~(jab by sh-mempool)  u.txo
              |=  his=sh-tx-mempool
              %+  snoc  his  new
          ==
        :: handle mempool transaction outputs
        =*  out  outputs.i.batch.res
        |-  ^+  cor
        ?~  out
          %=  ^$
            mempool-txids  (~(put in mempool-txids) txid.i.batch.res)
            batch.res  t.batch.res
          ==
        =/  found=?
          %-  ~(any by accounts)
          |=  val=account
          ?^  (find ~[script-hash.i.out] receive.account-cache.val)
              &
          ?=  ^
              (find ~[script-hash.i.out] change.account-cache.val)
        ?.  found  $(out t.out)
        =/  new
          ^-  sh-tx
          :+  %&
              txid.i.batch.res
              vout.i.out
        =.  cor
          %-  emit
          :*  %give  %fact  [/script-hash/[(scot %ux script-hash.i.out)] ~]
              %script-hash-update  !>(`script-hash-update`[%mempool-add new])
          ==
        %=  $
          out  t.out
          mempool-outputs
            %+  ~(put by mempool-outputs)
                [txid.i.batch.res vout.i.out]
                script-hash.i.out
          sh-mempool
            =/  tux  (~(gut by sh-mempool) script-hash.i.out *sh-tx-mempool)
            %+  ~(put by sh-mempool)
                script-hash.i.out
            %+  snoc  tux  new
        ==
      ::
      ==
    ::
    ==
  ::
      [%block-headers height=@ta *]
    =/  het  (slav %ud height.wir)
    =/  hed  next-block-header-height
    :: do not process if this was from a redundant request
    ?.  &(?=(^ hed) =(het u.hed))  cor
    ?+  sin  cor
    ::
        [%iris %http-response %cancel *]
      =^  caz  polling.timers  ~(reset ti /polling polling.timers)
      %-  emil  caz
    ::
        [%iris %http-response %finished *]
      =*  sus  status-code.response-header.client-response.sin
      ?:  (gte sus 400)
        ~&  >>>  (cat 3 'block header request failed: ' (scot %ud sus))
        cor
      =/  res  (need (handle-rpc-http-response:btr client-response.sin))
      ?+  -.res  !!
      ::
          %get-block-hash
        ?>  ?=(^ btc-node-config)
        =/  act  [%get-block-header block-hash.res]
        %-  emit
        %+  make-request-card
            /block-headers/[height.wir]/[now-t]
        %+  make-rpc-http-request:btr
            btc-node-config
            act
      ::
          %get-block-header
        =.  block-headers
          %:  put:on-block-headers
              block-headers
              het
              block-header.res
          ==
        =.  cor
          %-  emit
          :*  %give  %fact  [/block-headers ~]
              %block-headers-update
              !>(`block-headers-update`[%next het block-header.res])
          ==
        ?:  (gte het block-count)  cor
        ?>  ?=(^ btc-node-config)
        =/  nex  +(het)
        =/  act  [%get-block-hash nex]
        %-  emit
        %+  make-request-card
            /block-headers/[(scot %ud nex)]/[now-t]
        %+  make-rpc-http-request:btr
            btc-node-config
            act
      ::
      ==
    ::
    ==
  ::
      [%index height=@ta index-stage=*]
    =/  het  (slav %ud height.wir)
    :: do not process if this was from a redundant request
    ?.  %-  ~(any by accounts)
        |=  val=account
        ?-  last-indexed-block.val
          ^  =(het +(u.last-indexed-block.val))
          ~  =(het start-block.val)
        ==
      cor
    ?+  sin  cor
    ::
        [%iris %http-response %cancel *]
      =^  caz  polling.timers  ~(reset ti /polling polling.timers)
      %-  emil  caz
    ::
        [%iris %http-response %finished *]
      =*  sus  status-code.response-header.client-response.sin
      ?:  (gte sus 400)
        ~&  >>>  (cat 3 'index request failed: ' (scot %ud sus))
        cor
      =/  res  (need (handle-rpc-http-response:btr client-response.sin))
      ?+  -.res  !!
      ::
          %get-block-hash
        ?>  ?=(^ btc-node-config)
        =/  act  [%get-block block-hash.res]
        %-  emit
        %+  make-request-card
            /index/[height.wir]/start/[now-t]
        %+  make-rpc-http-request:btr
            btc-node-config
            act
      ::
          %get-block
        :: index a block
        =/  accounts-to-index  (get-accounts-for-block-height block-height.res)
        |-  ^+  cor
        ~+
        ?~  txs.res
          =.  accounts
            %-  ~(uni by accounts)
            %-  ~(urn by accounts-to-index)
            |=  [key=xpub val=account]
            %_  val
              last-indexed-block  [~ block-height.res]
            ==
          ?~  next-block-hash.res  cor
          :: move on to the next block
          ?>  ?=(^ btc-node-config)
          =/  act  [%get-block u.next-block-hash.res]
          %-  emit
          %+  make-request-card
              /index/[(scot %ud +(block-height.res))]/next/[now-t]
          %+  make-rpc-http-request:btr
              btc-node-config
              act
        :: handle transaction inputs
        =.  cor
          =*  inp  inputs.i.txs.res
          |-  ^+  cor
          ?~  inp  cor
          =/  txo  (~(get by utxo-set) i.inp)
          ?~  txo  $(inputs.i.txs.res t.inp)
          =/  new
            ^-  [block-hash sh-tx]
            :+  this-block-hash.res
                %|
                txid.i.txs.res
          =.  cor
            %-  emit
            :*  %give  %fact  [/script-hash/[(scot %ux u.txo)] ~]
                %script-hash-update  !>(`script-hash-update`[%confirmed new])
            ==
          %=  $
            inp  t.inp
            utxo-set
              %-  ~(del by utxo-set)  i.inp
            sh-index
              %+  ~(jab by sh-index)  u.txo
              |=  his=sh-tx-history
              %+  snoc  his  new
          ==
        :: handle transaction outputs
        =*  out  outputs.i.txs.res
        |-  ^+  cor
        ?~  out  ^$(txs.res t.txs.res)
        =^  found=?  accounts-to-index
          %+  ~(rib by accounts-to-index)  |
          |=  [[key=xpub val=account] found=?]
          ?:  found  [found key val]
          =^  is-receive=?  found
            ?^  (find ~[script-hash.i.out] receive.account-cache.val)
              [& &]
            :-  |
            ?=  ^
                (find ~[script-hash.i.out] change.account-cache.val)
          :-  found
          :-  key
          ?.  found  val
          =<  ac-abet
          %:  ac-handle-found-script-hash:(ac-use:ac key val)
              is-receive
              script-hash.i.out
          ==
        ?.  found  $(out t.out)
        =/  new
          ^-  [block-hash sh-tx]
          :^  this-block-hash.res
              %&
              txid.i.txs.res
              vout.i.out
        =.  cor
          %-  emit
          :*  %give  %fact  [/script-hash/[(scot %ux script-hash.i.out)] ~]
              %script-hash-update  !>(`script-hash-update`[%confirmed new])
          ==
        %=  $
          out  t.out
          utxo-set
            %+  ~(put by utxo-set)
                [txid.i.txs.res vout.i.out]
                script-hash.i.out
          sh-index
            =/  tux  (~(gut by sh-index) script-hash.i.out *sh-tx-history)
            %+  ~(put by sh-index)
                script-hash.i.out
            %+  snoc  tux  new
        ==
      ::
      ==
    ::
    ==
  ::
  ==
::
++  now-t  (scot %da now.bowl)
::
++  on-block-headers  ((on block-height block-header) lth)
::
++  make-request-card
  |=  [wir=wire req=request:http]
  ^-  card
  [%pass wir %arvo %i %request req *outbound-config:iris]
::
++  mempool-entries-to-ordered-txid-list
  |=  mez=mempool-entries
  ^-  (list txid)
  :: first sort txs from old to new
  =.  mez
    %+  sort  mez
    |=  [a=mempool-entry b=mempool-entry]
    %+  lth
        time.a
        time.b
  :: then order txs such that any tx with mempool ancestors
  :: always appears after all of its ancestors in the list
  =/  unc  *(set txid)
  =/  kid  *mempool-entries
  |-  ^-  (list txid)
  ~+
  ?~  mez  ~
  =/  mia
    ?.  .?(ancestors.i.mez)  ~
    %-  ~(dif in ancestors.i.mez)
        unc
  ?^  mia
    %=  $
      kid  (snoc kid i.mez)
      mez  t.mez
    ==
  ?~  descendants.i.mez
    :-  txid.i.mez
    %=  $
      mez  t.mez
    ==
  =/  new-kid  *mempool-entries
  =.  unc  (~(put in unc) txid.i.mez)
  :-  txid.i.mez
  |-  ^-  (list txid)
  ?~  kid
    %=  ^$
      kid  (flop new-kid)
      mez  t.mez
    ==
  ?^  (~(dif in ancestors.i.kid) unc)
    %=  $
      new-kid  [i.kid new-kid]
      kid  t.kid
    ==
  ?~  descendants.i.kid
    %=  $
      kid  t.kid
    ==
  =.  unc  (~(put in unc) txid.i.kid)
  :-  txid.i.kid
  %=  $
    kid  (weld (flop new-kid) t.kid)
  ==
::
++  next-block-header-height
  ^-  (unit block-height)
  =/  end  (ram:on-block-headers block-headers)
  ?~  end  [~ 0]
  ?:  =(key.u.end block-count)  ~
  :-  ~
      +(key.u.end)
::
++  get-min-block-start
  |=  acs=^accounts
  ^-  (unit block-height)
  %-  ~(rep by acs)
  |=  [[key=xpub val=account] acc=(unit block-height)]
  :-  ~
  =-  ?~  acc  -
      %+  min  u.acc  -
  ?^  last-indexed-block.val
      +(u.last-indexed-block.val)
      start-block.val
::
++  get-accounts-for-block-height
  |=  =block-height
  ^-  ^accounts
  %-  ~(rep by accounts)
  |=  [[key=xpub val=account] acc=^accounts]
  ?.  ?-  last-indexed-block.val
        ^  =(block-height +(u.last-indexed-block.val))
        ~  =(block-height start-block.val)
      ==
    acc
  %+  ~(put by acc)  key  val
::
++  del-account-from-index
  |=  aco=account
  ^+  cor
  =/  ads
    ^-  (list script-hash)
    %+  weld
        receive.account-cache.aco
        change.account-cache.aco
  |-  ^+  cor
  ?~  ads  cor
  =/  tux  (~(get by sh-index) i.ads)
  ?~  tux  $(ads t.ads)
  =/  out
    ^-  (list outpoint)
    %+  murn  u.tux
    |=  [hax=block-hash sax=sh-tx]
    ?-  -.sax
      %|  ~
      %&  [~ p.sax]
    ==
  =.  utxo-set
    |-  ^-  ^utxo-set
    ?~  out  utxo-set
    %=  $
      out  t.out
      utxo-set  (~(del by utxo-set) i.out)
    ==
  %=  $
    ads  t.ads
    sh-index  (~(del by sh-index) i.ads)
  ==
::
++  ac
  |_  [xub=xpub aco=account]
  ++  ac-core  .
  ++  ac-abet  aco
  ++  ac-use  |=([x=xpub a=account] ac-core(xub x, aco a))
  ++  ac-new
    |=  new=new-account-args
    =:  xub  xpub.new
        purpose.aco  purpose.new
        network.aco  network.new
        gap-limit.aco  gap-limit.new
        start-block.aco  (fall start-block.new 0)
      ==
    %_  ac-core
      account-cache.aco
        :: derive initial gap limit script-hashes
        :-  (~(many derive-script-hash %0^0) gap-limit.aco)  :: receive
            (~(many derive-script-hash %1^0) gap-limit.aco)  :: change
    ==
  ::
  ++  ac-handle-found-script-hash
    |=  [is-receive=? sax=script-hash]
    ^+  ac-core
    =/  cac
      ?-  is-receive
        %0  receive.account-cache.aco
        %1  change.account-cache.aco
      ==
    =.  cac
      =/  gap  0
      =/  got  |
      =/  tot  0
      |-  ^-  (list script-hash)
      ?~  cac
        ?.  got  ~
        %-  ~(many derive-script-hash [=>(is-receive ?-(. %1 ., %0 .)) tot])
        ?:  (gte gap gap-limit.aco)  0
        %+  sub
            gap-limit.aco
            gap
      =/  hit  =(i.cac sax)
      =/  had  (~(has by sh-index) i.cac)
      =:  gap  ?:(|(had hit) 0 +(gap))
          got  ?|(got hit)
          tot  +(tot)
        ==
      ?:  &(had got)  cac
      :-  i.cac
      %=  $
        cac  t.cac
      ==
    ?-  is-receive
      %0  ac-core(receive.account-cache.aco cac)
      %1  ac-core(change.account-cache.aco cac)
    ==
  ::
  ++  derive-script-hash
    =/  b32  (from-extended:bip32 (trip xub))
    |_  paf=(pair ?(%1 %0) @ud)
    ::
    ++  many
      |=  n=@ud
      ^-  (list script-hash)
      ?:  =(0 n)  ~
      :-  one
      %=  $
        n  (dec n)
        q.paf  +(q.paf)
      ==
    ::
    ++  one
      ^-  script-hash
      =.  b32
        %-  derive-path:b32
        ^-  tape
        ;:  weld  ((d-co:co 1) p.paf)
            "/"   ((d-co:co 1) q.paf)
        ==
      %-  sha-256:sha
      =<  dat
      %-  to-script-pubkey:adr:btc:b32
      ?-  purpose.aco
        %44  [%base58 (address:b32 network.aco)]
        %49  [%base58 (address-p2sh:b32 network.aco)]
        %84  [%bech32 (crip (address-p2wpkh:b32 network.aco))]
        %86  [%base58 (address:b32 network.aco)]               :: TODO: %86
      ==
    ::
    --
  ::
  --
::
++  ti
  |_  [wir=wire tim=timer]
  ++  set
    ^-  [(list card) timer]
    =/  wen  when
    =.  last.tim  [~ wen]
    :_  tim
    :~  ~(wait behn wen)
    ==
  ++  reset
    ^-  [(list card) timer]
    =/  wen  now.bowl
    =/  old  last.tim
    =.  last.tim  [~ wen]
    :_  tim
    ?~  old
      :~  ~(wait behn wen)
      ==
    :~  ~(rest behn u.old)
        ~(wait behn wen)
    ==
  ++  stop
    ^-  [(list card) timer]
    =/  old  last.tim
    ?~  old  [~ tim]
    =.  last.tim  ~
    :_  tim
    :~  ~(rest behn u.old)
    ==
  ++  when
    ^-  @da
    %+  add  interval.tim  now.bowl
  ++  behn
    |_  wen=time
    ++  rest  `card`[%pass wir %arvo %b %rest wen]
    ++  wait  `card`[%pass wir %arvo %b %wait wen]
    --
  --
::
  ::
::
++  init
  ^+  cor
  cor
::
++  save
  ^-  vase
  !>  state
::
++  load
  |=  vaz=vase
  ^+  cor
  =.  cor
    =/  old  (mole |.(!<(state-n vaz)))
    ?~  old
      ~&  >>>  %resetting-state
      cor
    ?-  -.u.old
      %0  cor(state u.old)
    ==
  =^  caz  polling.timers  ~(reset ti /polling polling.timers)
  %-  emil  caz
::
--
::
^-  agent:gall
|_  =bowl:gall
+*  this  .
    cor  ~(. +> [bowl ~])
::
++  on-init
  ^-  (quip card _this)
  =^  cards  state  abet:init:cor
  :-  cards  this
::
++  on-save
  ^-  vase
  =<  save  cor
::
++  on-load
  |=  =vase
  ^-  (quip card _this)
  =^  cards  state  abet:(load:cor vase)
  :-  cards  this
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  =^  cards  state  abet:(poke:cor mark vase)
  :-  cards  this
::
++  on-watch
  |=  =path
  ^-  (quip card _this)
  =^  cards  state  abet:(watch:cor path)
  :-  cards  this
::
++  on-leave
  |=  =path
  ^-  (quip card _this)
  =^  cards  state  abet:(leave:cor path)
  :-  cards  this
::
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  %-  peek:cor  path
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  =^  cards  state  abet:(agent:cor wire sign)
  :-  cards  this
::
++  on-arvo
  |=  [=wire sign=sign-arvo]
  ^-  (quip card _this)
  =^  cards  state  abet:(arvo:cor wire sign)
  :-  cards  this
::
++  on-fail
  |=  [=term =tang]
  ^-  (quip card _this)
  =^  cards  state  abet:(fail:cor term tang)
  :-  cards  this
--

