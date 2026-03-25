/-  *indexer
|%
::
+$  request
  $%  [%get-block-count ~]
      [%get-block-hash =block-height]
      [%get-block-header =block-hash]
      [%get-block-header-batch batch=(list block-hash)]
      [%get-block =block-hash]
      [%get-transaction =block-hash =txid]
      [%get-mempool-transaction =txid]
      [%get-mempool-transaction-batch batch=(list txid)]
      [%get-mempool-entries ~]
      [%send-transaction hex=@ux]
  ==
+$  response
  $%  [%get-block-count =block-height]
      [%get-block-hash =block-hash]
      [%get-block-header =block-header]
      [%get-block-header-batch batch=(list block-header)]
      [%get-block partial-block]
      [%get-transaction =transaction]
      [%get-mempool-transaction =transaction]
      [%get-mempool-transaction-batch batch=(list partial-transaction)]
      [%get-mempool-entries =mempool-entries]
      [%send-transaction hash=@ux]
  ==
::
+$  partial-block
  $:  =block-height
      this-block-hash=block-hash
      next-block-hash=(unit block-hash)
      txs=(list partial-transaction)
  ==
+$  partial-transaction
  $:  =txid
      inputs=(list outpoint)
      outputs=(list [=vout =script-hash])
  ==
::
--

