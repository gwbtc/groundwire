/-  *spv-wallet
/+  sailbox, io=sailboxio, json-utils
|%
::  Fetch address data from mempool.space
::  Returns enriched JSON with transactions if tx_count > 0
::
++  fetch-address-data
  |=  [address=@t =network]
  =/  m  (fiber:io ,json)
  ^-  form:m
  =/  base=tape
    ?-  network
      %main      "https://mempool.space/api/address/"
      %testnet3  "https://mempool.space/testnet/api/address/"
      %testnet4  "https://mempool.space/testnet4/api/address/"
      %signet    "https://mempool.space/signet/api/address/"
      %regtest   "http://localhost:3000/address/"
    ==
  =/  mempool-url=tape  (weld base (trip address))
  ;<  mempool-data=json  bind:m
    ((retry:io json) `5 (fetch-json:io mempool-url))
  ::  Check if address has transactions
  =/  tx-count-check=(unit @ud)
    %-  mole
    |.
    (ni:dejs:format (~(got jo:json-utils mempool-data) /'chain_stats'/'tx_count'))
  =/  tx-count-val=@ud  (fall tx-count-check 0)
  ::  Fetch transactions if address has history
  ;<  enriched-data=json  bind:m
    =/  m  (fiber:io ,json)
    ?:  =(0 tx-count-val)
      (pure:m mempool-data)
    =/  txs-url=tape
      (weld (weld base (trip address)) "/txs")
    ;<  txs-json=json  bind:m
      ((retry:io json) `5 (fetch-json:io txs-url))
    (pure:m (~(put jo:json-utils mempool-data) /'transactions' txs-json))
  (pure:m enriched-data)
::  Fetch UTXOs for an address directly from mempool.space
::  Returns list of unspent outputs with txid, vout, value, status
::
++  fetch-utxos
  |=  [address=@t =network]
  =/  m  (fiber:io ,(list [txid=@t vout=@ud value=@ud =tx-status]))
  ^-  form:m
  =/  base=tape
    ?-  network
      %main      "https://mempool.space/api/address/"
      %testnet3  "https://mempool.space/testnet/api/address/"
      %testnet4  "https://mempool.space/testnet4/api/address/"
      %signet    "https://mempool.space/signet/api/address/"
      %regtest   "http://localhost:3000/address/"
    ==
  =/  utxo-url=tape
    (weld (weld base (trip address)) "/utxo")
  ;<  utxo-json=json  bind:m
    ((retry:io json) `5 (fetch-json:io utxo-url))
  =/  utxo-list=(list json)
    (fall (mole |.(((ar:dejs:format same) utxo-json))) ~)
  =/  parsed=(list [txid=@t vout=@ud value=@ud =tx-status])
    %+  murn  utxo-list
    |=  utxo=json
    ^-  (unit [txid=@t vout=@ud value=@ud =tx-status])
    =/  txid=(unit @t)
      (mole |.((so:dejs:format (~(got jo:json-utils utxo) /txid))))
    =/  vout=(unit @ud)
      (mole |.((ni:dejs:format (~(got jo:json-utils utxo) /vout))))
    =/  value=(unit @ud)
      (mole |.((ni:dejs:format (~(got jo:json-utils utxo) /value))))
    =/  status-json=(unit json)
      (mole |.((~(got jo:json-utils utxo) /status)))
    =/  status=tx-status
      ?~  status-json  [%unconfirmed ~]
      (fall (parse-tx-status u.status-json) [%unconfirmed ~])
    ?~  txid  ~
    ?~  vout  ~
    ?~  value  ~
    `[u.txid u.vout u.value status]
  (pure:m parsed)
::  Adapter functions: mempool.space JSON → canonical types
::
++  parse-address-info
  |=  [address=@t data=json]
  ^-  (unit address-info)
  ::  Extract chain_stats
  =/  tx-count=(unit @ud)
    (mole |.((ni:dejs:format (~(got jo:json-utils data) /'chain_stats'/'tx_count'))))
  =/  chain-funded=(unit @ud)
    (mole |.((ni:dejs:format (~(got jo:json-utils data) /'chain_stats'/'funded_txo_sum'))))
  =/  chain-spent=(unit @ud)
    (mole |.((ni:dejs:format (~(got jo:json-utils data) /'chain_stats'/'spent_txo_sum'))))
  ::  Extract mempool_stats
  =/  mempool-funded=(unit @ud)
    (mole |.((ni:dejs:format (~(got jo:json-utils data) /'mempool_stats'/'funded_txo_sum'))))
  =/  mempool-spent=(unit @ud)
    (mole |.((ni:dejs:format (~(got jo:json-utils data) /'mempool_stats'/'spent_txo_sum'))))
  ::  Return ~ if required fields are missing
  ?~  tx-count  ~
  ?~  chain-funded  ~
  ?~  chain-spent  ~
  ::  Mempool stats may not exist (default to 0)
  `[address u.tx-count u.chain-funded u.chain-spent (fall mempool-funded 0) (fall mempool-spent 0)]
::
++  parse-tx-status
  |=  status-json=json
  ^-  (unit tx-status)
  =/  confirmed=(unit ?)
    (mole |.((bo:dejs:format (~(got jo:json-utils status-json) /confirmed))))
  ?~  confirmed  `[%unconfirmed ~]
  ?.  u.confirmed  `[%unconfirmed ~]
  =/  block-hash=(unit @t)
    (mole |.((so:dejs:format (~(got jo:json-utils status-json) /'block_hash'))))
  =/  block-height=(unit @ud)
    (mole |.((ni:dejs:format (~(got jo:json-utils status-json) /'block_height'))))
  ?~  block-hash  ~
  ?~  block-height  ~
  `[%confirmed u.block-hash u.block-height]
::
++  parse-transaction
  |=  tx-json=json
  ^-  (unit transaction)
  ::  Parse txid
  =/  txid=(unit @t)
    (mole |.((so:dejs:format (~(got jo:json-utils tx-json) /txid))))
  ?~  txid  ~
  ::  Parse inputs
  =/  vin-json=(unit json)  (mole |.((~(got jo:json-utils tx-json) /vin)))
  =/  inputs=(list tx-input)
    ?~  vin-json  ~
    =/  vin-list=(list json)
      (fall (mole |.(((ar:dejs:format same) u.vin-json))) ~)
    %+  murn  vin-list
    |=  input=json
    ^-  (unit tx-input)
    =/  spent-txid=(unit @t)
      (mole |.((so:dejs:format (~(got jo:json-utils input) /txid))))
    =/  spent-vout=(unit @ud)
      (mole |.((ni:dejs:format (~(got jo:json-utils input) /vout))))
    ?~  spent-txid  ~
    ?~  spent-vout  ~
    ::  Parse prevout (if available)
    =/  prevout-json=(unit json)
      (mole |.((~(got jo:json-utils input) /prevout)))
    =/  prevout=(unit tx-output)
      ?~  prevout-json  ~
      =/  value=(unit @ud)
        (mole |.((ni:dejs:format (~(got jo:json-utils u.prevout-json) /value))))
      =/  address=(unit @t)
        (mole |.((so:dejs:format (~(got jo:json-utils u.prevout-json) /'scriptpubkey_address'))))
      ?~  value  ~
      ?~  address  ~
      `[u.value u.address]
    ::  Parse witness stack (list of hex strings)
    =/  witness=(list @t)
      =/  wit-json=(unit json)
        (mole |.((~(got jo:json-utils input) /witness)))
      ?~  wit-json  ~
      =/  wit-list=(list json)
        (fall (mole |.(((ar:dejs:format same) u.wit-json))) ~)
      %+  murn  wit-list
      |=(j=json (mole |.((so:dejs:format j))))
    `[u.spent-txid u.spent-vout prevout witness]
  ::  Parse outputs
  =/  vout-json=(unit json)  (mole |.((~(got jo:json-utils tx-json) /vout)))
  =/  outputs=(list tx-output)
    ?~  vout-json  ~
    =/  vout-list=(list json)
      (fall (mole |.(((ar:dejs:format same) u.vout-json))) ~)
    %+  murn  vout-list
    |=  output=json
    ^-  (unit tx-output)
    =/  value=(unit @ud)
      (mole |.((ni:dejs:format (~(got jo:json-utils output) /value))))
    =/  address=(unit @t)
      (mole |.((so:dejs:format (~(got jo:json-utils output) /'scriptpubkey_address'))))
    ?~  value  ~
    ?~  address  ~
    `[u.value u.address]
  ::  Parse status
  =/  status-json=(unit json)
    (mole |.((~(got jo:json-utils tx-json) /status)))
  =/  parsed-status=(unit tx-status)
    ?~  status-json  `[%unconfirmed ~]
    (parse-tx-status u.status-json)
  ?~  parsed-status  ~
  ::  Parse fee and size (optional mempool.space fields)
  =/  fee=(unit @ud)
    (mole |.((ni:dejs:format (~(got jo:json-utils tx-json) /fee))))
  =/  size=(unit @ud)
    (mole |.((ni:dejs:format (~(got jo:json-utils tx-json) /size))))
  `[u.txid inputs outputs u.parsed-status fee size]
::
::  Extract transactions and addresses from JSON array
::  Returns map of txid->json and set of all output addresses
::
++  extract-txs-from-json
  |=  txs-json=json
  ^-  [(map @t json) (set @t)]
  ?.  ?=(%a -.txs-json)  [~ ~]
  =/  tx-list=(list json)  p.txs-json
  |-  ^-  [(map @t json) (set @t)]
  ?~  tx-list  [~ ~]
  =/  tx=json  i.tx-list
  =/  txid=(unit @t)
    %-  mole  |.
    (so:dejs:format (~(got jo:json-utils tx) /txid))
  =/  vout=(unit json)  (~(get jo:json-utils tx) /vout)
  =/  addresses=(set @t)
    ?~  vout  ~
    ?.  ?=(%a -.u.vout)  ~
    =/  vout-list=(list json)  p.u.vout
    |-  ^-  (set @t)
    ?~  vout-list  ~
    =/  output=json  i.vout-list
    =/  addr=(unit @t)
      %-  mole  |.
      (so:dejs:format (~(got jo:json-utils output) /'scriptpubkey_address'))
    =/  rest-addresses=(set @t)  $(vout-list t.vout-list)
    ?~  addr  rest-addresses
    (~(put in rest-addresses) u.addr)
  =/  [rest-txs=(map @t json) rest-addrs=(set @t)]
    $(tx-list t.tx-list)
  :-  ?~(txid rest-txs (~(put by rest-txs) u.txid tx))
  (~(uni in addresses) rest-addrs)
::
::  Canonicalize raw transaction JSON map to transaction map
::
++  canonicalize-txs
  |=  raw-txs=(map @t json)
  ^-  (map @t transaction)
  %-  ~(gas by *(map @t transaction))
  %+  murn  ~(tap by raw-txs)
  |=  [txid=@t tx-json=json]
  ^-  (unit [txid=@t tx=transaction])
  =/  parsed=(unit transaction)
    (parse-transaction tx-json)
  ?~  parsed  ~
  `[txid u.parsed]
::
::  Parse merkle proof JSON from mempool.space
::  Returns merkle hashes and position
::
++  parse-merkle-proof
  |=  proof-json=json
  ^-  (unit [merkles=(list @t) pos=@ud])
  =/  merkles=(unit (list @t))
    %-  mole  |.
    %+  turn  ((ar:dejs:format same) (~(got jo:json-utils proof-json) /merkle))
    |=(j=json (so:dejs:format j))
  =/  pos=(unit @ud)
    (mole |.((ni:dejs:format (~(got jo:json-utils proof-json) /pos))))
  ?~  merkles  ~
  ?~  pos  ~
  `[u.merkles u.pos]
::
::  Build mempool.space merkle proof URL
::
++  tx-base-url
  |=  =network
  ^-  tape
  ?-  network
    %main      "https://mempool.space/api/tx"
    %testnet3  "https://mempool.space/testnet/api/tx"
    %testnet4  "https://mempool.space/testnet4/api/tx"
    %signet    "https://mempool.space/signet/api/tx"
    %regtest   "http://localhost:3000/tx"
  ==
::
++  merkle-proof-url
  |=  [txid=@t =network]
  ^-  tape
  =/  base-url=tape  (weld (tx-base-url network) "/")
  (weld (weld base-url (trip txid)) "/merkle-proof")
--
