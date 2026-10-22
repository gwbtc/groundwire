::  sur/bitcoin-network.hoon
::
::  Verbatim copy of gwbtc/node sur/bitcoin/network.hoon at develop 39648ac,
::  needed by sur/light-client.hoon (its copy of sur/bitcoin/client.hoon).
::
/-  *bitcoin-common
|%
+$  protocol-version  _70.016
::
+$  network
  $?  %regtest
      %mainnet
      :: %testnet
      :: %testnet3
      :: %testnet4
      :: %signet
  ==
::
+$  message
  $%  [%version version-payload]
      [%verack ~]
      [%wtxidrelay ~]
      [%sendaddrv2 ~]
      [%sendheaders ~]
      [%sendtxrcncl version=@ud remote-salt=@ux]
      [%sendcmpct high-bandwidth-mode=? version=@ud]
      [%addr addresses=(list address-v1)]
      [%addrv2 addresses=(list address-v2)]
      [%ping nonce=@ux]
      [%pong nonce=@ux]
      [%inv =inventory]
      [%getdata =inventory]
      [%notfound =inventory]
      [%getblocks =block-locator hash-stop=(unit block-hash)]
      [%getheaders =block-locator hash-stop=(unit block-hash)]
      [%getblocktxn =block-hash differential-indexes=(list @ud)]
      [%getaddr ~]
      [%tx =transaction]
      [%block =block]
      [%headers headers=(list block-header)]
      [%feefilter amount=@ud]
      [%cmpctblock compact-block]
      [%blocktxn =block-hash transactions=(list transaction)]
      [%getcfilters filter-type=@ud start-height=block-height stop-hash=block-hash]
      [%cfilter filter-type=@ud =block-hash filter=hexb]
      [%getcfheaders filter-type=@ud start-height=block-height stop-hash=block-hash]
      [%cfheaders filter-type=@ud stop-hash=block-hash previous-filter-header=@ux filter-hashes=(list @ux)]
      [%getcfcheckpt filter-type=@ud stop-hash=block-hash]
      [%cfcheckpt filter-type=@ud stop-hash=block-hash filter-headers=(list @ux)]
      ::
      :: not implemented:
      ::
      :: [%mempool]
      :: [%merkleblock]
      :: [%filterload]
      :: [%filteradd]
      :: [%filterclear]
  ==
::
+$  prefilled-transaction
  $:  differential-index=@ud
      =transaction
  ==
::
+$  compact-block
  $:  =block-header
      nonce=@ud
      shortids=(list @ux)
      prefilledtxn=(list prefilled-transaction)
  ==
::
+$  block-locator
  $:  protocol-version=@ud
      locator=(list block-hash)
  ==
::
+$  inventory
  %-  list
  $:  type=inventory-type
      hash=@ux
  ==
::
+$  inventory-type
  $?  %msg-tx
      %msg-block
      %msg-wtx
      :: only in getdata
      %msg-filtered-block
      %msg-cmpct-block
      %msg-witness-block
      %msg-witness-tx
      %undefined
  ==
::
+$  network-address-id
  $?  %ipv4
      %ipv6
      %torv2
      %torv3
      %i2p
      %cjdns
      %yggdrasil
  ==
::
+$  address-v1-partial
  $:  =services
      ip=@is
      port=@ud
  ==
::
+$  address-v1
  $:  time=@ud
      address-v1-partial
  ==
::
+$  address-v2
  $:  time=@ud
      =services
      id=network-address-id
      address=@ux
      port=@ud
  ==
::
+$  version-payload
  $:  version=@ud
      =services
      time=@ud
      receiver=address-v1-partial
      sender=address-v1-partial
      nonce=@ud
      user-agent=@t
      starting-height=@ud
      relay=_|
  ==
::
+$  services
  $:  node-network=_|
      node-bloom=_|
      node-witness=_|
      node-compact-filters=_|
      node-network-limited=_|
      node-p2p-v2=_|
  ==
::
--

