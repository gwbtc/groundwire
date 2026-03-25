::  Core account operations for SPV wallet
::
/-  *spv-wallet, *bitcoin-spv
/+  io=sailboxio, sailbox, json-utils, *bitcoin-spv, *wallet-address
/+  *wallet-mempool-space, bech32=bip-b173, taproot, drft=tx-draft
|%
::
::  +ac: Pure function door for account-details manipulation
::
::  Usage: ~(arm ac account-details)
::  Similar to +by for maps, provides consistent interface for account ops.
::
++  ac
  |_  [act=account-details net=network]
  ::
  ::  ============================================
  ::  Network-Details Helper
  ::  ============================================
  ::
  ::  Get network-details for current network (or empty default)
  ::
  ++  get-net
    ^-  network-details
    (~(gut by networks.act) net *network-details)
  ::
  ::  Put updated network-details back into the map
  ::
  ++  put-net
    |=  nd=network-details
    ^-  account-details
    act(networks (~(put by networks.act) net nd))
  ::
  ::  ============================================
  ::  Chain Operations (receiving/change)
  ::  ============================================
  ::
  ::  Put an hd-leaf into the appropriate chain
  ::
  ++  put-leaf
    |=  [chain=@t idx=@ud =hd-leaf]
    ^-  account-details
    =/  nd  get-net
    ?:  =(chain 'receiving')
      (put-net nd(receiving (put:((on @ud ^hd-leaf) gth) receiving.nd idx hd-leaf)))
    (put-net nd(change (put:((on @ud ^hd-leaf) gth) change.nd idx hd-leaf)))
  ::
  ::  Put address-details as main address (creates hd-leaf with empty script-trees)
  ::
  ++  put-addr
    |=  [chain=@t idx=@ud addr=address-details]
    ^-  account-details
    =/  existing=(unit hd-leaf)  (get-leaf chain idx)
    =/  leaf=hd-leaf
      ?~  existing
        [addr ~]
      u.existing(main addr)
    (put-leaf chain idx leaf)
  ::
  ::  Get hd-leaf at index from chain
  ::
  ++  get-leaf
    |=  [chain=@t idx=@ud]
    ^-  (unit hd-leaf)
    =/  nd  get-net
    (get:((on @ud hd-leaf) gth) ?:(=(chain 'receiving') receiving.nd change.nd) idx)
  ::
  ::  Get main address-details at index from chain
  ::
  ++  get-addr
    |=  [chain=@t idx=@ud]
    ^-  (unit address-details)
    =/  leaf=(unit hd-leaf)  (get-leaf chain idx)
    ?~  leaf  ~
    `main.u.leaf
  ::
  ::  Add a tapscript address to an hd-leaf's script-trees
  ::
  ++  add-tapscript
    |=  [chain=@t idx=@ud addr=@t name=@t tree=ptst:taproot]
    ^-  account-details
    =/  existing=(unit hd-leaf)  (get-leaf chain idx)
    ?~  existing
      ~|  "cannot add tapscript to non-existent leaf"  !!
    =/  addr-details=address-details  [addr ~ ~ ~ ~]
    =/  updated-trees=(map @t tapscript-details)
      (~(put by script-trees.u.existing) addr [name tree addr-details])
    (put-leaf chain idx u.existing(script-trees updated-trees))
  ::
  ::  Get the highest index in a chain (head of mop)
  ::
  ++  top-idx
    |=  chain=@t
    ^-  (unit @ud)
    =/  nd  get-net
    =/  mop  ?:(=(chain 'receiving') receiving.nd change.nd)
    =/  head  (pry:((on @ud hd-leaf) gth) mop)
    ?~  head  ~
    `key.u.head
  ::
  ::  Check if adding at index would extend the chain
  ::
  ++  is-new-row
    |=  [chain=@t idx=@ud]
    ^-  ?
    =/  top  (top-idx chain)
    ?~  top  %.y
    (gth idx u.top)
  ::
  ::  Put address and update cache in one operation
  ::
  ++  put-addr-full
    |=  [chain=@t idx=@ud addr=address-details]
    ^-  account-details
    =/  chain-num=@ud  ?:(=(chain 'receiving') 0 1)
    =/  suffix=address-suffix:hd-path  [[%.n chain-num] [%.n idx]]
    =/  updated  (put-addr chain idx addr)
    ::  Get the updated network-details and add to address-cache
    =/  nd  (~(gut by networks.updated) net *network-details)
    updated(networks (~(put by networks.updated) net nd(address-cache (~(put by address-cache.nd) address.addr suffix))))
  ::
  ::  Merge multiple transactions
  ::
  ++  merge-txs
    |=  txs=(map @t transaction)
    ^-  account-details
    =/  nd  get-net
    (put-net nd(transactions (~(uni by transactions.nd) txs)))
  ::
  ::  Add addresses to tx-addresses set
  ::
  ++  add-tx-addrs
    |=  addrs=(set @t)
    ^-  account-details
    =/  nd  get-net
    (put-net nd(tx-addresses (~(uni in tx-addresses.nd) addrs)))
  ::
  ::  ============================================
  ::  Process State Operations
  ::  ============================================
  ::
  ::  Get proc map for chain
  ::
  ++  get-proc
    |=  chain=@t
    ^-  (map @ud [pid=@ta act=?])
    ?:(=(chain 'receiving') receiving.proc.act change.proc.act)
  ::
  ::  Set scan process
  ::
  ++  set-scan
    |=  [pid=@ta active=? scn=account-scan]
    ^-  account-details
    act(scan.proc `[pid active scn])
  ::
  ::  Set address refresh process
  ::
  ++  set-addr-proc
    |=  [chain=@t idx=@ud pid=@ta active=?]
    ^-  account-details
    ?:  =(chain 'receiving')
      act(receiving.proc (~(put by receiving.proc.act) idx [pid active]))
    act(change.proc (~(put by change.proc.act) idx [pid active]))
  ::
  ::  Clear address refresh process
  ::
  ++  clear-addr-proc
    |=  [chain=@t idx=@ud]
    ^-  account-details
    ?:  =(chain 'receiving')
      act(receiving.proc (~(del by receiving.proc.act) idx))
    act(change.proc (~(del by change.proc.act) idx))
  ::
  ::  Set tx verification process
  ::
  ++  set-tx-proc
    |=  [txid=@t pid=@ta active=?]
    ^-  account-details
    act(tx-verify.proc (~(put by tx-verify.proc.act) txid [pid active]))
  ::
  ::  Clear tx verification process
  ::
  ++  clear-tx-proc
    |=  txid=@t
    ^-  account-details
    act(tx-verify.proc (~(del by tx-verify.proc.act) txid))
  ::
  ::  ============================================
  ::  TX Verification Results
  ::  ============================================
  ::
  ::  Set tx verification result (pass `~ for success, ``tang for error)
  ::
  ++  set-tx-verify
    |=  [txid=@t result=(unit (unit tang))]
    ^-  account-details
    =/  nd  get-net
    (put-net nd(tx-verification (~(put by tx-verification.nd) txid result)))
  ::
  ::  Get tx_count for address at chain/index
  ::
  ++  get-addr-tx-count
    |=  [chain=@t idx=@ud]
    ^-  @ud
    =/  addr=(unit address-details)  (get-addr chain idx)
    ?~  addr  0
    ?~  info.u.addr  0
    tx-count.u.info.u.addr
  ::
  ::  ============================================
  ::  Network-Details Getters
  ::  ============================================
  ::
  ++  get-txs
    ^-  (map @t transaction)
    transactions:get-net
  ::
  ++  get-tx
    |=  txid=@t
    ^-  (unit transaction)
    (~(get by transactions:get-net) txid)
  ::
  ++  get-addr-cache
    ^-  (map @t address-suffix:hd-path)
    address-cache:get-net
  ::
  ++  get-draft
    ^-  (unit transaction:drft)
    draft:get-net
  ::
  ++  set-draft
    |=  d=transaction:drft
    ^-  account-details
    =/  nd  get-net
    (put-net nd(draft `d))
  ::
  ++  clear-draft
    ^-  account-details
    =/  nd  get-net
    (put-net nd(draft ~))
  ::
  ++  get-tx-verify
    |=  txid=@t
    ^-  (unit (unit (unit tang)))
    (~(get by tx-verification:get-net) txid)
  ::
  ++  get-all-tx-verify
    ^-  (map @t (unit (unit tang)))
    tx-verification:get-net
  ::
  ++  get-receiving-mop
    ^-  ((mop @ud hd-leaf) gth)
    receiving:get-net
  ::
  ++  get-change-mop
    ^-  ((mop @ud hd-leaf) gth)
    change:get-net
  ::
  ::  Simple getters for direct field access (backwards compatible)
  ::
  ++  receiving
    ^-  ((mop @ud hd-leaf) gth)
    receiving:get-net
  ::
  ++  change
    ^-  ((mop @ud hd-leaf) gth)
    change:get-net
  ::
  ++  transactions
    ^-  (map @t transaction)
    transactions:get-net
  ::
  ++  address-cache
    ^-  (map @t address-suffix:hd-path)
    address-cache:get-net
  ::
  ++  tx-verification
    ^-  (map @t (unit (unit tang)))
    tx-verification:get-net
  ::
  ++  draft
    ^-  (unit transaction:drft)
    draft:get-net
  ::
  ++  get-tx-addrs
    ^-  (set @t)
    tx-addresses:get-net
  --
::
::  ============================================
::  Pure Business Logic Helpers
::  ============================================
::
::  Derive address from extended key at chain/index
::
++  derive-address-from-xkey
  |=  [xkey=@t chain=@t idx=@ud =script-type =network]
  ^-  @t
  ::  Convert to bip32 network type
  =/  net=?(%main %testnet %regtest)
    ?+  network  %testnet
      %main     %main
      %regtest  %regtest
    ==
  =/  xkey-wallet  (from-extended:bip32 (trip xkey))
  =/  chain-num=@ud  ?:(=(chain 'receiving') 0 1)
  =/  path=tape
    "{(scow %ud chain-num)}/{(scow %ud idx)}"
  =/  derived  (derive-path:xkey-wallet path)
  %-  crip
  ?-  script-type
    %p2wpkh       (address-p2wpkh:derived net)
    %p2sh-p2wpkh  (slag 2 (scow %uc (address-p2sh:derived net)))
    %p2pkh        (slag 2 (scow %uc (address:derived net)))
    %p2tr
      ::  BIP-86: Use tweaked output pubkey with no script tree
      =/  pubkey=@ux  public-key:derived
      =/  tweaked-x=@ux  (output-pubkey:taproot pubkey ~)
      (trip (need (encode-taproot:bech32 net [32 tweaked-x])))
  ==
::
::  Helper: Convert account network to SPV network type
::  Maps old %testnet to %testnet3 (the legacy testnet)
::
++  to-network
  |=  net=?(%main %testnet)
  ^-  network
  ?-  net
    %main     %main
    %testnet  %testnet3
  ==
::
::  Find account by path string in wallet
::  Pure lookup - no IO
::
++  sse-list-event
  |=  chain=@t
  ^-  @t
  (crip "{(trip chain)}-list-update")
::
++  sse-row-event
  |=  chain=@t
  ^-  @t
  (crip "{(trip chain)}-row-update")
::
++  validate-tx-for-spv
  |=  [details=account-details txid=@t spv=(map network spv-chain)]
  ^-  (each [tx=transaction block-hash-hex=@uvI header=block-header:bitcoin-spv] tang)
  %-  mule  |.
  ::  Get transaction data
  =/  tx-data=(unit transaction)  (~(get-tx ac [details active-network.details]) txid)
  ?~  tx-data
    ~|  %no-transaction-data
    ~|  "Transaction not found - refresh account first"
    !!
  ::  Check if transaction is confirmed
  ?.  ?=(%confirmed -.tx-status.u.tx-data)
    ~|  %transaction-unconfirmed
    ~|  "Transaction not confirmed yet"
    !!
  ::  Get block hash and height
  =/  block-hash=@t  block-hash.tx-status.u.tx-data
  =/  block-height=@ud  block-height.tx-status.u.tx-data
  ::  Check if we have this block header
  =/  block-hash-hex=@uvI  (rash block-hash hex)
  =/  spv-net=network  active-network.details
  =/  chain=spv-chain  (get-spv-chain spv-net spv)
  =/  header=(unit block-header:bitcoin-spv)
    (~(get by headers.chain) block-hash-hex)
  ?~  header
    ~|  %header-not-in-chain
    ~|  "Block #{(scow %ud block-height)} ({(trip block-hash)}) not in SPV chain - sync headers first"
    !!
  [u.tx-data block-hash-hex u.header]
::
++  find-account-by-path
  |=  $:  =wallet
          global-accounts=(map @ux account-details)
          account-path=@t
      ==
  ^-  (unit [account:hd-path @ux account-details])
  %-  ~(rep by accounts.wallet)
  |=  [[acct=account:hd-path account-pubkey=@ux] result=(unit [account:hd-path @ux account-details])]
  ?^  result  result
  =/  path-str=tape  (format-account-path acct)
  ?.  =(path-str (trip account-path))  ~
  =/  details=(unit account-details)  (~(get by global-accounts) account-pubkey)
  ?~  details  ~
  `[acct account-pubkey u.details]
::  Helper: Reconstruct (map account:hd-path account-details) from normalized structure
::
++  wallet-accounts-with-details
  |=  [wallet-accounts=(map account:hd-path @ux) global-accounts=(map @ux account-details)]
  ^-  (map account:hd-path account-details)
  %-  ~(gas by *(map account:hd-path account-details))
  %+  murn  ~(tap by wallet-accounts)
  |=  [acct=account:hd-path account-pubkey=@ux]
  =/  details=(unit account-details)  (~(get by global-accounts) account-pubkey)
  ?~  details  ~
  `[acct u.details]
::
::  Unified refresh for any account type
::  Uses the extended-key stored in account-details to derive addresses
::
++  refresh-account-address
  |=  [pubkey=@ux chain=@t index=@ud]
  =/  m  (fiber:io ,~)
  ^-  form:m
  ::  Get state and account
  ;<  state=state-0  bind:m  (get-state-as:io state-0)
  ;<  now=@da  bind:m  get-time:io
  =/  details=(unit account-details)  (~(get by accounts.state) pubkey)
  ?~  details
    ~|  "account not found"  !!
  ::  Derive address (pure)
  =/  address-cord=@t
    %:  derive-address-from-xkey
      k.extended-key.u.details
      chain
      index
      script-type.u.details
      active-network.u.details
    ==
  =/  new-address-details=address-details
    [address-cord `now ~ ~ ~]
  ::  Check if this creates a new row
  =/  is-new-row=?  (~(is-new-row ac [u.details active-network.u.details]) chain index)
  ::  Update chain and cache
  =/  updated-details=account-details
    (~(put-addr-full ac [u.details active-network.u.details]) chain index new-address-details)
  ::  Save and send first SSE update
  =.  accounts.state  (~(put by accounts.state) pubkey updated-details)
  ;<  ~  bind:m  (replace:io !>(state))
  ;<  ~  bind:m
    ?:  is-new-row
      (send-sse-event:io (account-stream-path pubkey) ~ `(sse-list-event chain))
    (send-sse-event:io (account-stream-path pubkey) `(crip (scow %ud index)) `(sse-row-event chain))
  ::  Fetch address data from mempool.space
  ;<  enriched-data=json  bind:m
    (fetch-address-data address-cord active-network.u.details)
  ::  Extract transactions from JSON (pure)
  =/  txs-array=(unit json)
    (~(get jo:json-utils enriched-data) /'transactions')
  =/  [new-transactions=(map @t json) new-tx-addresses=(set @t)]
    ?~  txs-array  [~ ~]
    (extract-txs-from-json u.txs-array)
  ::  Parse address info (pure)
  =/  parsed-info=(unit address-info)
    (parse-address-info address-cord enriched-data)
  ::  Fetch UTXOs if address has transactions
  ;<  utxo-list=(list [txid=@t vout=@ud value=@ud =tx-status])  bind:m
    ?:  ?&  ?=(^ parsed-info)
            (gth tx-count.u.parsed-info 0)
        ==
      (fetch-utxos address-cord active-network.u.details)
    (pure:m ~)
  =/  updated-address-details=address-details
    [address-cord last-check.new-address-details parsed-info indexer-history.new-address-details utxo-list]
  ::  Get fresh state
  ;<  state=state-0  bind:m  (get-state-as:io state-0)
  =/  details=(unit account-details)  (~(get by accounts.state) pubkey)
  ?~  details
    ~|  "account not found after fetch"  !!
  ::  Canonicalize transactions (pure)
  =/  new-transactions-canonical=(map @t transaction)
    (canonicalize-txs new-transactions)
  ::  Update via +ac
  =/  spv-net  active-network.u.details
  =/  updated-details=account-details
    =/  d1  (~(put-addr ac [u.details spv-net]) chain index updated-address-details)
    =/  d2  (~(merge-txs ac [d1 spv-net]) new-transactions-canonical)
    (~(add-tx-addrs ac [d2 spv-net]) new-tx-addresses)
  ::  Save and send second SSE update
  =.  accounts.state  (~(put by accounts.state) pubkey updated-details)
  ;<  ~  bind:m  (replace:io !>(state))
  ;<  ~  bind:m
    ?:  is-new-row
      (send-sse-event:io (account-stream-path pubkey) ~ `(sse-list-event chain))
    (send-sse-event:io (account-stream-path pubkey) `(crip (scow %ud index)) `(sse-row-event chain))
  (pure:m ~)
::
::  Refresh a tapscript address (fetch tx data from indexer)
::
++  refresh-tapscript-address
  |=  [pubkey=@ux chain=@t index=@ud tapscript-addr=@t]
  =/  m  (fiber:io ,~)
  ^-  form:m
  ::  Get state and account
  ;<  state=state-0  bind:m  (get-state-as:io state-0)
  ;<  now=@da  bind:m  get-time:io
  =/  details=(unit account-details)  (~(get by accounts.state) pubkey)
  ?~  details
    ~|  "account not found"  !!
  =/  spv-net  active-network.u.details
  ::  Get the leaf
  =/  leaf-mop=((mop @ud hd-leaf) gth)
    ?:(=(chain 'receiving') ~(get-receiving-mop ac [u.details spv-net]) ~(get-change-mop ac [u.details spv-net]))
  =/  leaf=(unit hd-leaf)  (get:((on @ud hd-leaf) gth) leaf-mop index)
  ?~  leaf
    ~|  "leaf not found at index"  !!
  ::  Get the tapscript details
  =/  ts-details=(unit tapscript-details)  (~(get by script-trees.u.leaf) tapscript-addr)
  ?~  ts-details
    ~|  "tapscript not found"  !!
  ::  Fetch address data from mempool.space
  ;<  enriched-data=json  bind:m
    (fetch-address-data tapscript-addr spv-net)
  ::  Extract transactions from JSON
  =/  txs-array=(unit json)
    (~(get jo:json-utils enriched-data) /'transactions')
  =/  [new-transactions=(map @t json) new-tx-addresses=(set @t)]
    ?~  txs-array  [~ ~]
    (extract-txs-from-json u.txs-array)
  ::  Parse address info
  =/  parsed-info=(unit address-info)
    (parse-address-info tapscript-addr enriched-data)
  ~&  >>  "tapscript refresh: parsed-info={<parsed-info>}"
  ::  Fetch UTXOs for this tapscript address
  ;<  utxo-list=(list [txid=@t vout=@ud value=@ud =tx-status])  bind:m
    (fetch-utxos tapscript-addr spv-net)
  ~&  >>  "tapscript refresh: found {(scow %ud (lent utxo-list))} UTXOs"
  =/  updated-addr-details=address-details
    [tapscript-addr `now parsed-info ~ utxo-list]
  ~&  >>  "tapscript refresh: updated-addr-details info={<info.updated-addr-details>}"
  ::  Get fresh state
  ;<  state=state-0  bind:m  (get-state-as:io state-0)
  =/  fresh-details=(unit account-details)  (~(get by accounts.state) pubkey)
  ?~  fresh-details
    ~|  "account not found after fetch"  !!
  ::  Re-fetch the leaf from fresh state
  =/  fresh-spv-net  active-network.u.fresh-details
  =/  fresh-leaf-mop=((mop @ud hd-leaf) gth)
    ?:(=(chain 'receiving') ~(get-receiving-mop ac [u.fresh-details fresh-spv-net]) ~(get-change-mop ac [u.fresh-details fresh-spv-net]))
  =/  fresh-leaf=(unit hd-leaf)  (get:((on @ud hd-leaf) gth) fresh-leaf-mop index)
  ?~  fresh-leaf
    ~|  "leaf not found after fetch"  !!
  ::  Canonicalize transactions
  =/  new-transactions-canonical=(map @t transaction)
    (canonicalize-txs new-transactions)
  ::  Update the tapscript details in the leaf (use fresh leaf)
  =/  fresh-ts-details=(unit tapscript-details)  (~(get by script-trees.u.fresh-leaf) tapscript-addr)
  ?~  fresh-ts-details
    ~|  "tapscript not found after fetch"  !!
  =/  updated-ts-details=tapscript-details
    u.fresh-ts-details(address-details updated-addr-details)
  =/  updated-trees=(map @t tapscript-details)
    (~(put by script-trees.u.fresh-leaf) tapscript-addr updated-ts-details)
  =/  updated-leaf=hd-leaf  u.fresh-leaf(script-trees updated-trees)
  ::  Put the leaf back and merge transactions
  =/  nd=network-details  (~(gut by networks.u.fresh-details) fresh-spv-net *network-details)
  =/  updated-details=account-details
    ?:  =(chain 'receiving')
      =/  new-receiving=((mop @ud hd-leaf) gth)
        (put:((on @ud hd-leaf) gth) receiving.nd index updated-leaf)
      u.fresh-details(networks (~(put by networks.u.fresh-details) fresh-spv-net nd(receiving new-receiving)))
    =/  new-change=((mop @ud hd-leaf) gth)
      (put:((on @ud hd-leaf) gth) change.nd index updated-leaf)
    u.fresh-details(networks (~(put by networks.u.fresh-details) fresh-spv-net nd(change new-change)))
  ::  Merge transactions and tx-addresses
  =/  d2  (~(merge-txs ac [updated-details fresh-spv-net]) new-transactions-canonical)
  =/  d3  (~(add-tx-addrs ac [d2 fresh-spv-net]) new-tx-addresses)
  ::  Save and send SSE update
  =.  accounts.state  (~(put by accounts.state) pubkey d3)
  ;<  ~  bind:m  (replace:io !>(state))
  (send-sse-event:io (account-stream-path pubkey) `(crip (scow %ud index)) `(sse-row-event chain))
::
::  Unified set scan PID for any account type
::
++  set-account-scan-pid
  |=  [pubkey=@ux pid=@ta]
  =/  m  (fiber:io ,~)
  ^-  form:m
  ;<  state=state-0  bind:m  (get-state-as:io state-0)
  =/  details=(unit account-details)  (~(get by accounts.state) pubkey)
  ?~  details  ~|("account not found" !!)
  =/  existing-scan=(unit [pid=@ta act=? scn=account-scan])  scan.proc.u.details
  ?^  existing-scan
    ?:  =(pid pid.u.existing-scan)
      (pure:m ~)
    (fiber-fail:io leaf+"scan already in progress" ~)
  ::  Initialize with phase 1 (receiving) at idx=0, gap=0, active
  =/  updated-details=account-details
    (~(set-scan ac [u.details active-network.u.details]) pid %.y [%1 0 0])
  =.  accounts.state  (~(put by accounts.state) pubkey updated-details)
  ;<  ~  bind:m  (replace:io !>(state))
  (pure:m ~)
::
::  Unified set address PID for any account type
::
++  set-account-address-pid
  |=  [pubkey=@ux chain=@t index=@ud pid=@ta]
  =/  m  (fiber:io ,~)
  ^-  form:m
  ;<  state=state-0  bind:m  (get-state-as:io state-0)
  =/  details=(unit account-details)  (~(get by accounts.state) pubkey)
  ?~  details  ~|("account not found" !!)
  =/  existing-entry=(unit [pid=@ta act=?])  (~(get by (~(get-proc ac [u.details active-network.u.details]) chain)) index)
  ?^  existing-entry
    ?:  =(pid pid.u.existing-entry)
      (pure:m ~)
    (fiber-fail:io leaf+"address refresh already in progress" ~)
  =/  updated-details=account-details
    (~(set-addr-proc ac [u.details active-network.u.details]) chain index pid %.y)
  =.  accounts.state  (~(put by accounts.state) pubkey updated-details)
  ;<  ~  bind:m  (replace:io !>(state))
  (pure:m ~)
::
::  Unified clear address PID for any account type
::
++  clear-account-address-pid
  |=  [pubkey=@ux chain=@t index=@ud]
  =/  m  (fiber:io ,~)
  ^-  form:m
  ;<  state=state-0  bind:m  (get-state-as:io state-0)
  =/  details=(unit account-details)  (~(get by accounts.state) pubkey)
  ?~  details  (pure:m ~)
  =/  cleared-details=account-details
    (~(clear-addr-proc ac [u.details active-network.u.details]) chain index)
  =.  accounts.state  (~(put by accounts.state) pubkey cleared-details)
  ;<  ~  bind:m  (replace:io !>(state))
  (pure:m ~)
::
::  Refresh a single address: derive, fetch data, update state, send SSE events
::
++  refresh-address
  |=  [pubkey=@ux account-path=@t chain=@t index=@ud]
  =/  m  (fiber:io ,~)
  ^-  form:m
  ::  Get state and wallet
  ;<  state=state-0  bind:m  (get-state-as:io state-0)
  ;<  now=@da  bind:m  get-time:io
  =/  wallet=(unit wallet)  (~(get by wallets.state) pubkey)
  ?~  wallet
    ~|  "wallet not found"  !!
  ::  Find the account that matches the path (pure)
  =/  matching-account=(unit [account:hd-path @ux account-details])
    (find-account-by-path u.wallet accounts.state account-path)
  ?~  matching-account
    ~|  "account not found"  !!
  =/  [acct=account:hd-path account-pubkey=@ux details=account-details]  u.matching-account
  ::  Derive the address using library helper
  =/  chain-type=?(%receiving %change)
    ?:(=(chain 'receiving') %receiving %change)
  =/  address-cord=@t
    (derive-address-at-index seed.u.wallet acct chain-type index (en-crypto active-network.details))
  =/  new-address-details=address-details
    [address-cord `now ~ ~ ~]
  ::  Check if this creates a new row beyond current max
  =/  spv-net  active-network.details
  =/  is-new-row=?  (~(is-new-row ac [details spv-net]) chain index)
  ::  Update chain and cache in one operation
  =/  updated-details=account-details
    (~(put-addr-full ac [details spv-net]) chain index new-address-details)
  ::  Update account in wallet (account-pubkey from destructuring above)
  =.  accounts.state  (~(put by accounts.state) account-pubkey updated-details)
  =.  accounts.u.wallet  (~(put by accounts.u.wallet) acct account-pubkey)
  =.  wallets.state  (~(put by wallets.state) pubkey u.wallet)
  ;<  ~  bind:m  (replace:io !>(state))
  ::  Send first SSE update (address derived)
  ;<  ~  bind:m
    ?:  is-new-row
      (send-sse-event:io (account-stream-path account-pubkey) ~ `(sse-list-event chain))
    (send-sse-event:io (account-stream-path account-pubkey) `(crip (scow %ud index)) `(sse-row-event chain))
  ::  Fetch address data from mempool.space
  ;<  enriched-data=json  bind:m
    (fetch-address-data address-cord active-network.details)
  ::  Extract transactions from JSON (pure)
  =/  txs-array=(unit json)
    (~(get jo:json-utils enriched-data) /'transactions')
  =/  [new-transactions=(map @t json) new-tx-addresses=(set @t)]
    ?~  txs-array  [~ ~]
    (extract-txs-from-json u.txs-array)
  ::  Parse address info (pure)
  =/  parsed-info=(unit address-info)
    (parse-address-info address-cord enriched-data)
  ::  Fetch UTXOs if address has transactions
  ;<  utxo-list=(list [txid=@t vout=@ud value=@ud =tx-status])  bind:m
    ?:  ?&  ?=(^ parsed-info)
            (gth tx-count.u.parsed-info 0)
        ==
      (fetch-utxos address-cord spv-net)
    (pure:m ~)
  =/  updated-address-details=address-details
    [address-cord last-check.new-address-details parsed-info indexer-history.new-address-details utxo-list]
  ::  Get fresh state
  ;<  state=state-0  bind:m  (get-state-as:io state-0)
  =/  wallet=(unit ^wallet)  (~(get by wallets.state) pubkey)
  ?~  wallet
    ~|  "wallet not found after fetch"  !!
  ::  Find account again after async fetch (pure)
  =/  matching-account=(unit [account:hd-path @ux account-details])
    (find-account-by-path u.wallet accounts.state account-path)
  ?~  matching-account
    ~|  "account not found after fetch"  !!
  =/  [acct=account:hd-path account-pubkey=@ux details=account-details]  u.matching-account
  ::  Canonicalize transactions (pure)
  =/  new-transactions-canonical=(map @t transaction)
    (canonicalize-txs new-transactions)
  ::  Update via +ac
  =/  spv-net  active-network.details
  =/  updated-details=account-details
    =/  d1  (~(put-addr ac [details spv-net]) chain index updated-address-details)
    =/  d2  (~(merge-txs ac [d1 spv-net]) new-transactions-canonical)
    (~(add-tx-addrs ac [d2 spv-net]) new-tx-addresses)
  ::  Update account in wallet (account-pubkey from destructuring above)
  =.  accounts.state  (~(put by accounts.state) account-pubkey updated-details)
  =.  accounts.u.wallet  (~(put by accounts.u.wallet) acct account-pubkey)
  =.  wallets.state  (~(put by wallets.state) pubkey u.wallet)
  ;<  ~  bind:m  (replace:io !>(state))
  ::  Send second SSE update (with fetched data)
  ;<  ~  bind:m
    ?:  is-new-row
      (send-sse-event:io (account-stream-path account-pubkey) ~ `(sse-list-event chain))
    (send-sse-event:io (account-stream-path account-pubkey) `(crip (scow %ud index)) `(sse-row-event chain))
  ::  Send account summary update
  ;<  ~  bind:m
    (send-sse-event:io (account-stream-path account-pubkey) ~ `%account-summary-update)
  (pure:m ~)
::  Find matching account by path
::
++  find-account
  |=  [pubkey=@ux account-path=@t]
  =/  m  (fiber:io ,[account:hd-path @ux account-details])
  ^-  form:m
  ;<  state=state-0  bind:m  (get-state-as:io state-0)
  =/  wallet=(unit wallet)  (~(get by wallets.state) pubkey)
  ?~  wallet
    ~|  "wallet not found"  !!
  ::  Use pure helper for lookup
  =/  matching-account=(unit [account:hd-path @ux account-details])
    (find-account-by-path u.wallet accounts.state account-path)
  ?~  matching-account
    ~|  "account not found"  !!
  (pure:m u.matching-account)
::  Get proc map for chain (delegated to +ac)
::
++  get-proc-map
  |=  [details=account-details chain=@t]
  ^-  (map @ud [pid=@ta act=?])
  (~(get-proc ac [details active-network.details]) chain)
::  Construct SSE stream path for account using account pubkey
::
++  account-stream-path
  |=  account-pubkey=@ux
  ^-  path
  /spv-wallet/stream/account/(crip (hexn:sailbox account-pubkey))
::  Refresh address and check if it has transactions
::
++  refresh-and-check-usage
  |=  [pubkey=@ux account-path=@t chain=@t index=@ud]
  =/  m  (fiber:io ,?)
  ^-  form:m
  ;<  ~  bind:m  (refresh-address pubkey account-path chain index)
  ::  Get fresh state after refresh
  ;<  [acct=account:hd-path =account-pubkey=@ux details=account-details]  bind:m
    (find-account pubkey account-path)
  ::  Check tx_count from fresh data (pure)
  =/  tx-count=@ud  (~(get-addr-tx-count ac [details active-network.details]) chain index)
  (pure:m (gth tx-count 0))
::  Set transaction verification PID (account-pubkey version)
::
++  set-account-tx-verify-pid
  |=  [account-pubkey=@ux txid=@t pid=@ta]
  =/  m  (fiber:io ,~)
  ^-  form:m
  ;<  state=state-0  bind:m  (get-state-as:io state-0)
  =/  details=(unit account-details)  (~(get by accounts.state) account-pubkey)
  ?~  details  ~|("account not found" !!)
  =/  existing-entry=(unit [pid=@ta act=?])
    (~(get by tx-verify.proc.u.details) txid)
  ?^  existing-entry
    ?:  =(pid pid.u.existing-entry)
      (pure:m ~)
    (fiber-fail:io leaf+"verification already in progress" ~)
  =/  updated-details=account-details
    (~(set-tx-proc ac [u.details active-network.u.details]) txid pid %.y)
  =.  accounts.state  (~(put by accounts.state) account-pubkey updated-details)
  ;<  ~  bind:m  (replace:io !>(state))
  (pure:m ~)
::  Clear transaction verification PID (account-pubkey version)
::
++  clear-account-tx-verify-pid
  |=  [account-pubkey=@ux txid=@t]
  =/  m  (fiber:io ,~)
  ^-  form:m
  ;<  state=state-0  bind:m  (get-state-as:io state-0)
  =/  details=(unit account-details)  (~(get by accounts.state) account-pubkey)
  ?~  details  (pure:m ~)
  =/  cleared-details=account-details
    (~(clear-tx-proc ac [u.details active-network.u.details]) txid)
  =.  accounts.state  (~(put by accounts.state) account-pubkey cleared-details)
  ;<  ~  bind:m  (replace:io !>(state))
  (pure:m ~)
::  Verify a transaction using SPV (account-pubkey version)
::
++  verify-account-transaction
  |=  [account-pubkey=@ux txid=@t]
  =/  m  (fiber:io ,~)
  ^-  form:m
  ;<  state=state-0  bind:m  (get-state-as:io state-0)
  =/  details=(unit account-details)  (~(get by accounts.state) account-pubkey)
  ?~  details  ~|("account not found" !!)
  ::  Validate transaction can be SPV verified (pure)
  =/  validation-result=(each [tx=transaction block-hash-hex=@uvI header=block-header:bitcoin-spv] tang)
    (validate-tx-for-spv u.details txid spv.state)
  ::  Handle validation failure
  ?:  ?=(%| -.validation-result)
    =/  updated-details=account-details
      (~(set-tx-verify ac [u.details active-network.u.details]) txid ``p.validation-result)
    =.  accounts.state  (~(put by accounts.state) account-pubkey updated-details)
    ;<  ~  bind:m  (replace:io !>(state))
    (pure:m ~)
  ::  Validation passed - fetch merkle proof
  =/  [tx=transaction block-hash-hex=@uvI header=block-header:bitcoin-spv]
    p.validation-result
  =/  proof-url=tape  (merkle-proof-url txid active-network.u.details)
  ;<  proof-json=json  bind:m
    ((retry:io json) `100 (fetch-json:io proof-url))
  ::  Parse merkle proof (pure)
  =/  parsed=(unit [merkles=(list @t) pos=@ud])
    (parse-merkle-proof proof-json)
  ?~  parsed
    ::  Parse failed - store error
    =/  updated-details=account-details
      (~(set-tx-verify ac [u.details active-network.u.details]) txid ``~[leaf+"Could not parse merkle proof"])
    =.  accounts.state  (~(put by accounts.state) account-pubkey updated-details)
    ;<  ~  bind:m  (replace:io !>(state))
    (pure:m ~)
  ::  Verify merkle proof (pure)
  =/  proof-result=(each ~ tang)
    (verify-parsed-merkle-proof txid merkles.u.parsed pos.u.parsed header)
  ::  Store result
  =/  result=(unit (unit tang))
    ?-(-.proof-result %| ``p.proof-result, %& `~)
  =/  updated-details=account-details
    (~(set-tx-verify ac [u.details active-network.u.details]) txid result)
  =.  accounts.state  (~(put by accounts.state) account-pubkey updated-details)
  ;<  ~  bind:m  (replace:io !>(state))
  (pure:m ~)
--
