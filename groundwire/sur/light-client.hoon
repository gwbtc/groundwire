::  sur/light-client.hoon
::
::  Request/response molds used by %gw-btc and %spv-wallet.
::
::    /transaction/<txid>
::    /transaction/<block-hash>/<txid>
::    /block-hash-by-height/<height>
::    /tx-out/<txid>/<vout>
::    /block/<block-hash>
::    /block-filter/<block-hash>
::    /best-block
::
/-  bc=bitcoin
|%
+$  transaction-response  tx:bc
::
::  The response repeats the canonical height so callers can reject a fact
::  delivered on the wrong request path.
+$  block-hash-response  id:block:bc
::
::  `%unknown` is distinct from `%spent`: callers must fail closed without
::  claiming that an unavailable answer is a confirmed spend.
+$  tx-out-response
  $%  [%unspent out=output:tx:bc]
      [%spent ~]
      [%unknown ~]
  ==
::
+$  block-response  block:bc
::
+$  block-filter-response
  $:  blockhash=@ux
      height=@ud
      filter=hexb:bc
  ==
::
+$  best-block-update
  $:  =id:block:bc
      reorg=(unit @ud)
  ==
--
