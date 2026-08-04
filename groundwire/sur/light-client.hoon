::  sur/light-client.hoon
::
::  Faithful SUBSET of the real node's sur/bitcoin/light-client.hoon
::  (gwbtc/node, branch develop).  These are the exact $update molds the
::  %bitcoin-client agent gives facts under, so that %gw-btc's
::  lib/lc-attestation can +!< a %light-client fact straight off the wire.
::
::  Each %bitcoin-client watch answers ONE fact and then kicks; the fact is
::  given under the agent's own fact mark, which is the update arm name:
::
::    watch path                      fact mark               vase mold
::    /best-block                     %best-block             best-block:update
::    /is-synced                      %is-synced              is-synced:update
::    /block-header/height/<height>   %block-header-by-height block-header-by-height:update
::    /block-header/hash/<hash>       %block-header-by-hash   block-header-by-hash:update
::    /block-filter/height/<height>   %block-filter-by-height block-filter-by-height:update
::    /block-filter/hash/<hash>       %block-filter-by-hash   block-filter-by-hash:update
::    /block/height/<height>          %block-by-height        block-by-height:update
::    /block/hash/<hash>              %block-by-hash          block-by-hash:update
::    /transaction/<block-hash>/<txid> %transaction           transaction:update
::
::  The height-form of a request always answers; the hash-form may answer ~
::  when the hash is unknown or not on the main chain.
::
/-  *bitcoin-common
|%
::
+$  confirmations  (unit @ud)
::
+$  next-block-hash  (unit block-hash)
::
+$  block-info
  $:  =block-hash
      =block-height
      =confirmations
      =next-block-hash
      =chainwork
  ==
::
++  update
  |%
  ::
  +$  is-synced  ?
  ::
  +$  best-block
    $%  [%new =block-height =block-hash]
        [%reorg-rollback =block-height =block-hash]
    ==
  ::
  +$  block-header-by-hash
    $@  ~
    $:  block-info
        =block-header
    ==
  +$  block-header-by-height
    $:  block-info
        =block-header
    ==
  ::
  +$  block-filter-by-hash
    $@  ~
    $:  block-info
        filter=hexb
    ==
  ::
  +$  block-filter-by-height
    $:  block-info
        filter=hexb
    ==
  ::
  +$  block-by-hash
    $@  ~
    $:  block-info
        =block
    ==
  ::
  +$  block-by-height
    $:  block-info
        =block
    ==
  ::
  +$  transaction
    $@  ~
    $:  block-info
        index=@ud
        =txid
        =wtxid
        =^transaction
    ==
  ::
  --
::
--
