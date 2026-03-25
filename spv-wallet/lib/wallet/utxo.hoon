/-  *spv-wallet
/+  wallet-account
|%
::  Collect cached UTXOs from all addresses in an account
::  UTXOs are fetched per-address from mempool.space and stored in address-details
::
++  collect-cached-utxos
  |=  details=account-details
  ^-  (list [txid=@t vout=@ud value=@ud address=@t chain=@t index=@ud confirmations=(unit @ud)])
  =/  ac  ~(. ac:wallet-account [details active-network.details])
  =/  recv-list=(list [@ud hd-leaf])
    (tap:((on @ud hd-leaf) gth) receiving:ac)
  =/  change-list=(list [@ud hd-leaf])
    (tap:((on @ud hd-leaf) gth) change:ac)
  =/  utxo-type  *[txid=@t vout=@ud value=@ud address=@t chain=@t index=@ud confirmations=(unit @ud)]
  =|  result=(list _utxo-type)
  ::  Process receiving addresses (main + tapscript)
  =.  result
    |-
    ?~  recv-list  result
    =/  [idx=@ud leaf=hd-leaf]  i.recv-list
    =/  main-utxos=(list _utxo-type)
      %+  turn  utxos.main.leaf
      |=  [txid=@t vout=@ud value=@ud =tx-status]
      ^-  _utxo-type
      =/  confirmations=(unit @ud)
        ?-  -.tx-status
          %confirmed    `block-height.tx-status
          %unconfirmed  ~
        ==
      [txid vout value address.main.leaf 'receiving' idx confirmations]
    =/  ts-utxos=(list _utxo-type)
      %-  zing
      %+  turn  ~(val by script-trees.leaf)
      |=  td=tapscript-details
      ^-  (list _utxo-type)
      %+  turn  utxos.address-details.td
      |=  [txid=@t vout=@ud value=@ud =tx-status]
      ^-  _utxo-type
      =/  confirmations=(unit @ud)
        ?-  -.tx-status
          %confirmed    `block-height.tx-status
          %unconfirmed  ~
        ==
      [txid vout value address.address-details.td 'receiving' idx confirmations]
    $(recv-list t.recv-list, result (weld (weld main-utxos ts-utxos) result))
  ::  Process change addresses (main + tapscript)
  |-
  ?~  change-list  result
  =/  [idx=@ud leaf=hd-leaf]  i.change-list
  =/  main-utxos=(list _utxo-type)
    %+  turn  utxos.main.leaf
    |=  [txid=@t vout=@ud value=@ud =tx-status]
    ^-  _utxo-type
    =/  confirmations=(unit @ud)
      ?-  -.tx-status
        %confirmed    `block-height.tx-status
        %unconfirmed  ~
      ==
    [txid vout value address.main.leaf 'change' idx confirmations]
  =/  ts-utxos=(list _utxo-type)
    %-  zing
    %+  turn  ~(val by script-trees.leaf)
    |=  td=tapscript-details
    ^-  (list _utxo-type)
    %+  turn  utxos.address-details.td
    |=  [txid=@t vout=@ud value=@ud =tx-status]
    ^-  _utxo-type
    =/  confirmations=(unit @ud)
      ?-  -.tx-status
        %confirmed    `block-height.tx-status
        %unconfirmed  ~
      ==
    [txid vout value address.address-details.td 'change' idx confirmations]
  $(change-list t.change-list, result (weld (weld main-utxos ts-utxos) result))
::  Find the oldest confirmed UTXO (lowest block height)
::
++  oldest-utxo
  |=  utxos=(list [txid=@t vout=@ud value=@ud address=@t chain=@t index=@ud confirmations=(unit @ud)])
  ^-  (unit [txid=@t vout=@ud value=@ud address=@t chain=@t index=@ud confirmations=(unit @ud)])
  =+  utxo-type=*[txid=@t vout=@ud value=@ud address=@t chain=@t index=@ud confirmations=(unit @ud)]
  =/  confirmed=(list _utxo-type)
    %+  murn  utxos
    |=  u=_utxo-type
    ?~  confirmations.u  ~
    `u
  ?~  confirmed  ~
  =/  best=_utxo-type  i.confirmed
  =/  rest=(list _utxo-type)  t.confirmed
  |-
  ?~  rest  `best
  =/  cur=_utxo-type  i.rest
  ?:  (lth (need confirmations.cur) (need confirmations.best))
    $(best cur, rest t.rest)
  $(rest t.rest)
--
