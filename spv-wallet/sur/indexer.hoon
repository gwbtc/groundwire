|%
::
+$  block-headers-update
  $%  [%init =block-headers]
      [%next =block-height =block-header]
  ==
::
+$  script-hash-update
  $%  [%init mempool=sh-tx-mempool confirmed=sh-tx-history]
      [%confirmed =block-hash =sh-tx]
      [%mempool-add =sh-tx]
      [%mempool-del =sh-tx]
  ==
::
+$  node-config
  $:  url=@t
      auth=@t
  ==
::
+$  xpub          @t
+$  address       @t
+$  script-hash   @ux
+$  block-height  @ud
+$  block-hash    @ux
+$  txid          @ux
+$  vout          @ud
+$  outpoint      [=txid =vout]
::
+$  transaction
  $:  txid=@ux
      wtxid=@ux
      size=@ud
      vsize=@ud
      weight=@ud
      version=@ud
      lock-time=@ud
      raw=@ux
      vin=(list transaction-vin)
      vout=(list transaction-vout)
  ==
+$  transaction-vin
  $:  txid=@ux
      vout=@ud
      script-sig=@ux
      sequence=@ud
      witness=(list @ux)
  ==
+$  transaction-vout
  $:  value=@ud
      script-pub-key=@ux
      addresses=(list @t)
  ==
::
+$  block-headers  ((mop block-height block-header) lth)
+$  block-header
  $:  version=@ud
      time=@da
      bits=@ux
      nonce=@ud
      previous-block-hash=@ux
      merkle-root=@ux
  ==
::
+$  utxo-set       (map outpoint script-hash)
+$  sh-index       (map script-hash sh-tx-history)
+$  sh-mempool     (map script-hash sh-tx-mempool)
+$  sh-tx-history  (list [=block-hash =sh-tx])
+$  sh-tx-mempool  (list sh-tx)
+$  sh-tx          (each outpoint txid)
::                 [%& fund] [%| spend]
::
+$  mempool-txids  (set txid)
::
+$  mempool-entries  (list mempool-entry)
+$  mempool-entry
  $:  =txid
      time=@da
      ancestors=(set txid)
      descendants=(set txid)
  ==
::
+$  purpose  ?(%44 %49 %84 %86)
+$  network  ?(%main %regtest %testnet)
::
+$  gap-limit  $~(20 @ud)
+$  account-cache
  $:  receive=(list script-hash)
      change=(list script-hash)
  ==
+$  accounts  (map xpub account)
+$  account
  $:  =purpose
      =network
      =gap-limit
      start-block=block-height
      last-indexed-block=(unit block-height)
      =account-cache
  ==
+$  new-account-args
  $:  =xpub
      =purpose
      =network
      =gap-limit
      start-block=(unit block-height)
  ==
::
--

