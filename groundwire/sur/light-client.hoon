::  sur/light-client.hoon
::
::  Interface molds for the (upcoming) %light-client agent's watch paths.
::  The light client fetches Bitcoin data via block filters and VERIFIES it
::  before serving, so consumers (e.g. %urb-watcher-2) use these directly with
::  no further trust step.
::
::  The watch paths:
::    /transaction/[block-hash]/[txid]  request-response -> a transaction
::    /block/[block-hash]               request-response -> a block (has height)
::    /block-filter/[block-hash]        request-response -> a BIP-158 filter
::    /best-block                       persistent sub   -> chain tip + reorgs
::
::  The first three are one-shot request-response (strandio +watch-one: watch ->
::  one fact -> kick). /best-block is a persistent subscription that emits a
::  fact on every new chain tip (and on reorgs).
::
/-  bc=bitcoin
|%
::  +transaction-response: a fully-decoded, already-verified transaction
::  (mirrors tx:bitcoin so verifiers consume it directly).
::
+$  transaction-response  tx:bc
::  +block-response: a block (header + txs); carries its height.
::
+$  block-response  block:bc
::  +block-filter-response: a BIP-158 basic filter for one block.
::
::    `blockhash` lets the consumer derive the GCS siphash key (to-key:b158);
::    `filter` is the full GCS bytes (incl. the leading element count), queried
::    against a set of scriptPubKeys with match:b158.
::
+$  block-filter-response
  $:  blockhash=@ux
      height=@ud
      filter=hexb:bc
  ==
::  +best-block-update: the new chain tip.
::
::    `id` is [block-hash height]. `reorg` is ~ on a plain extension, or
::    `fork-height on a reorg (the consumer rolls any filter scans back to that
::    height and re-checks the affected utxos).
::
+$  best-block-update
  $:  =id:block:bc
      reorg=(unit @ud)
  ==
--
