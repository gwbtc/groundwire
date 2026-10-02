::  sur/light-client.hoon
::
::  Verbatim copy of gwbtc/node sur/bitcoin/client.hoon at develop 39648ac.
::  %bitcoin-client gives every fact under ONE mark, %bitcoin-client-update,
::  carrying a $bitcoin-client-update: a head tag naming the case and the
::  case's update arm as .p.  The whole union has to be here, not just the
::  cases %gw-btc reads: +!< checks that the incoming vase's type fits ours,
::  and the agent types every fact as the full union.
::
::    watch path                       case
::    /is-synced                       %is-synced              is-synced:update
::    /best-block                      %best-block             best-block:update
::    /block-header/height/<height>    %block-header-by-height
::    /block-filter/height/<height>    %block-filter-by-height
::    /block/height/<height>           %block-by-height
::    /transaction/<block-hash>/<txid> %transaction
::
::  Until node develop 29a84c8 each case had its own mark
::  (%bitcoin-client-is-synced, …); the old mar/bitcoin-client/ files stay in
::  this desk so a fact from an older %bitcoin-client is still buildable --
::  an unbuildable mark on a fact to a thread takes spider down -- and is
::  then skipped.
::
/-  *bitcoin-common,
    b-net=bitcoin-network
|%
::
+$  bitcoin-client-action
  $%  [%broadcast-transaction p=broadcast-transaction:action]
      [%connect-peer p=connect-peer:action]
      [%disconnect-peer p=disconnect-peer:action]
  ==
::
+$  bitcoin-client-update
  $%  [%is-synced p=is-synced:update]
      [%best-block p=best-block:update]
      [%block-header-by-hash p=block-header-by-hash:update]
      [%block-header-by-height p=block-header-by-height:update]
      [%block-filter-by-hash p=block-filter-by-hash:update]
      [%block-filter-by-height p=block-filter-by-height:update]
      [%block-by-hash p=block-by-hash:update]
      [%block-by-height p=block-by-height:update]
      [%transaction p=transaction:update]
      [%peers p=peers:update]
      [%addresses p=addresses:update]
      [%blacklist p=blacklist:update]
  ==
::
++  action
  |%
  ::
  +$  broadcast-transaction  transaction
  +$  connect-peer           earth-address
  +$  disconnect-peer        earth-address
  ::
  --
::
++  update
  |%
  ::
  +$  is-synced  ?
  ::
  +$  best-block
    $%  [%new =block-height =block-hash]
    $:  %reorg-rollback
        last-common=[=block-height =block-hash]
        stale-branch=(list [=block-height =block-hash])
    ==  ==
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
  +$  peers
    $%  [%all peers=(map earth-address earth-peer-info)]
        [%put address=earth-address info=earth-peer-info]
        [%del address=earth-address]
    ==
  ::
  +$  addresses
    $%  [%all addresses=earth-addresses]
        [%put address=earth-address info=earth-address-info]
        [%del address=earth-address]
    ==
  ::
  +$  blacklist
    $%  [%all blacklist=earth-blacklist]
        [%put address=earth-address info=earth-blacklist-info]
        [%del address=earth-address]
    ==
  ::
  --
::
+$  earth-addresses
  %+  map
      earth-address
      earth-address-info
+$  earth-address
  $:  net-id=network-address-id:b-net
      address=@ux
      port=@ud
  ==
+$  earth-address-info
  $:  =address-provenance
      =address-rank
      =last-heard
      =services:b-net
  ==
+$  earth-peer-info
  $:  handshake-done=_|
      wtxidrelay=_|
      =services:b-net
      connection-opened=time
      =last-heard
      =ping-average
  ==
+$  earth-blacklist
  %+  map
      earth-address
      earth-blacklist-info
+$  earth-blacklist-info
  $:  when=time
      reason=@t
      expiration=(unit @dr)
  ==
+$  address-rank
  $~  %unknown
  $?  %priority
      %known
      %unknown
  ==
+$  address-provenance
  $%  [%userspace ~]
      [%network who=earth-address]
  ==
+$  last-heard    (unit time)
+$  ping-average  (unit @dr)
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
--

