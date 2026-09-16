/-  *bitcoin, resource
|%
+$  host-info
  $+  bp-host-info
  $:  api-url=@t
      connected=?
      =network
      block=@ud
      clients=(set ship)
  ==
+$  host-info-2
  $+  bp-host-info-2
  $:  api=(unit api-state)
      src=(unit ship)
      connected=?
      =network
      block=@ud
      clients=(set ship)
  ==
+$  api-state
  $+  bp-api-state
  [url=@t port=@t local=?]
::
:: +$  api-state
::   $%  [%unset ~]
::       [%setting-ext targ=ship]
::       [%set-loc =api-data]
::       [%set-ext src=ship =api-data]
::   ==
+$  whitelist
  $+  bp-whitelist
  $:  public=?
      kids=?
      users=(set ship)
      groups=(set resource:resource)
  ==
::
+$  whitelist-target
  $+  bp-whitelist-target
  $%  [%public ~]
      [%kids ~]
      [%users users=(set ship)]
      [%groups groups=(set resource:resource)]
  ==
+$  command
  $+  bp-command
  $%  [%set-credentials url=@t port=@t local=? =network]
      [%set-external src=@p =network]
      [%add-whitelist wt=whitelist-target]
      [%remove-whitelist wt=whitelist-target]
      [%set-interval inte=@dr]
  ==
+$  action
  $+  bp-action
  $:  id=@uvH
  $%  [%address-info =address]
      [%tx-info txid=@ux]
      [%raw-tx txid=@ux]
      [%broadcast-tx rawtx=hexb]
      [%ping ~]
      [%block-info block=(unit @ud)]
      [%histogram ~]
      [%block-headers start=@ud count=@ud cp=(unit @ud)]
      [%tx-from-pos height=@ud pos=@ud merkle=?]
      [%fee block=@ud]
      [%psbt psbt=@t]
      [%block-txs blockhash=hexb]
      [%mine-empty miner=address nblocks=@]
      [%mine-trans miner=address txs=(list hexb)]
  ==  ==
::
+$  result
  $+  bp-result
  $:  id=@uvH
  $%  [%address-info =address utxos=(set utxo) used=? block=@ud]
      [%tx-info =info:tx]
      [%raw-tx txid=@ux rawtx=hexb]
      [%broadcast-tx txid=@ux broadcast=? included=?]
      [%block-info =network block=@ud fee=(unit sats) blockhash=hexb blockfilter=hexb]
      [%histogram hist=(list (list @ud))]
      [%block-headers count=@ud hex=hexb max=@ud root=(unit hexb) branch=(list hexb)]
      [%tx-from-pos tx-hash=hexb merkle=(list hexb)]
      [%fee fee=@rd]
      [%psbt psbt=@t]
      [%block-txs blockhash=hexb txs=(list [txid=@ux rawtx=hexb])]
  ==  ==
++  error
  =<  error
  |%
  ::
  +$  error
    $+  bp-error
    $:  id=@uvH
    $%  [%not-connected status=@ud]
        [%bad-request status=@ud]
        [%no-auth status=@ud]
        [%rpc-error (unit rpc-error)]
    ==  ==
  ::
  +$  rpc-error
    $+  bp-rpc-error
    [id=@t code=@t message=@t]
  --
+$  update
  $+  bp-update
  (each result error)
::
+$  status
  $+  bp-status
  $%  [%connected =network block=@ud fee=(unit sats)]
      [%new-block =network block=@ud fee=(unit sats) blockhash=hexb blockfilter=hexb]
      [%new-rpc url=@t port=@t =network]
      [%disconnected ~]
  ==
::
++  rpc-types
  |%
  +$  action
    $+  bp-rpc-action
    $%  [%get-address-info =address]
        [%get-tx-vals txid=@ux]
        [%get-raw-tx txid=@ux]
        [%broadcast-tx rawtx=hexb]
        [%get-block-count ~]
        [%get-block-info block=(unit @ud)]
        [%get-histogram ~]
        [%get-block-headers start=@ud count=@ud cp=(unit @ud)]
        [%get-tx-from-pos height=@ud pos=@ud merkle=?]
        [%get-fee block=@ud]
        [%update-psbt psbt=@t]
        [%get-block-txs blockhash=hexb]
        [%mine-empty miner=address nblocks=@]
        [%mine-trans miner=address txs=(list hexb)]
    ==
  ::
  +$  result
    $+  bp-rpc-result
    $%  [%get-address-info =address utxos=(set utxo) used=? block=@ud]
        [%get-tx-vals =info:tx]
        [%get-raw-tx txid=@ux rawtx=hexb]
        [%create-raw-tx rawtx=hexb]
        [%broadcast-tx txid=@ux broadcast=? included=?]
        [%get-block-count block=@ud]
        [%get-block-info block=@ud fee=(unit sats) blockhash=hexb blockfilter=hexb]
        [%get-histogram hist=(list (list @ud))]
        [%get-block-headers count=@ud hex=hexb max=@ud root=(unit hexb) branch=(list hexb)]
        [%get-tx-from-pos tx-hash=hexb merkle=(list hexb)]
        [%get-fee fee=@rd]
        [%update-psbt psbt=@t]
        [%get-block-txs blockhash=hexb txs=(list [txid=@ux rawtx=hexb])]
        [%error id=@t code=@t message=@t]
    ==
  --
--
