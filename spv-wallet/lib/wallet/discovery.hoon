::  Account discovery orchestration for SPV wallet
::
/-  s=spv-wallet
/+  io=sailboxio, sailbox, json-utils, *wallet-address, *wallet-account
|%
::  Helper: Convert BIP purpose to script type
++  purpose-to-script-type
  |=  purpose=@ud
  ^-  script-type
  ?+  purpose  %p2wpkh  :: default to native segwit
    %44  %p2pkh
    %49  %p2sh-p2wpkh
    %84  %p2wpkh
    %86  %p2tr
  ==
::
::  Helper: Convert coin type to network
++  coin-type-to-network
  |=  coin-type=@ud
  ^-  network
  ?:  =(0 coin-type)  %main
  %testnet3
::
::  Scan addresses for a specific account
::  Scans receiving and change addresses with gap limit of 20
::
++  scan-account-addresses
  |=  [pubkey=@ux account-path=@t scan-key=coin-type:hd-path scn=account-scan]
  =/  m  (fiber:io ,~)
  ^-  form:m
  ::  Check which phase we're in and resume from there
  ?-  -.scn
    %1  ::  Phase 1 - receiving address scan
      |-
      ::  Update progress in scan map
      ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
      =/  wallet=(unit wallet:s)  (~(get by wallets.state) pubkey)
      ?~  wallet
        ~|  "wallet not found during scan progress update"  !!
      =/  scan-entry=(unit [pid=@ta act=? idx=@ud scn=account-scan])
        (~(get by scan.u.wallet) scan-key)
      ?~  scan-entry
        ::  Scan was cancelled, exit
        (pure:m ~)
      =/  updated-wallet=wallet:s
        u.wallet(scan (~(put by scan.u.wallet) scan-key [pid.u.scan-entry act.u.scan-entry idx.u.scan-entry scn]))
      =.  wallets.state  (~(put by wallets.state) pubkey updated-wallet)
      ;<  ~  bind:m  (replace:io !>(state))
      ;<  ~  bind:m  (send-sse-event:io /spv-wallet/stream/wallet/(crip (hexn:sailbox pubkey)) ~ `'discovery-status-update')
      ?:  (gte gap.scn 20)
        ::  Receiving scan complete, transition to change phase
        ^$(scn [%2 idx=0 gap=0])
      ;<  ~  bind:m
        (refresh-address pubkey account-path 'receiving' idx.scn)
      ::  Get updated state to check tx_count
      ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
      =/  wallet=(unit wallet:s)  (~(get by wallets.state) pubkey)
      ?~  wallet
        ~|  "wallet not found during address scan"  !!
      =/  matching-account=(unit [account:hd-path account-details])
        %-  ~(rep by accounts.u.wallet)
        |=  [[acct=account:hd-path account-pubkey=@ux] result=(unit [account:hd-path account-details])]
        ?^  result  result
        =/  path-str=tape  (format-account-path acct)
        ?.  =(path-str (trip account-path))  ~
        ::  Look up account details from global accounts map
        =/  details=(unit account-details)  (~(get by accounts.state) account-pubkey)
        ?~  details  ~
        `[acct u.details]
      ?~  matching-account
        ~|  "account not found during address scan"  !!
      =/  [acct=account:hd-path details=account-details]  u.matching-account
      =/  address-data=(unit address-details)
        =/  leaf=(unit hd-leaf)  (get:((on @ud hd-leaf) gth) ~(get-receiving-mop ac [details active-network.details]) idx.scn)
        ?~(leaf ~ `main.u.leaf)
      =/  tx-count=@ud
        ?~  address-data  0
        ?~  info.u.address-data  0
        tx-count.u.info.u.address-data
      =/  new-gap=@ud
        ?:(=(0 tx-count) +(gap.scn) 0)
      $(scn [%1 idx=+(idx.scn) gap=new-gap])
    %2  ::  Phase 2 - change address scan
      |-
      ::  Update progress in scan map
      ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
      =/  wallet=(unit wallet:s)  (~(get by wallets.state) pubkey)
      ?~  wallet
        ~|  "wallet not found during scan progress update"  !!
      =/  scan-entry=(unit [pid=@ta act=? idx=@ud scn=account-scan])
        (~(get by scan.u.wallet) scan-key)
      ?~  scan-entry
        ::  Scan was cancelled, exit
        (pure:m ~)
      =/  updated-wallet=wallet:s
        u.wallet(scan (~(put by scan.u.wallet) scan-key [pid.u.scan-entry act.u.scan-entry idx.u.scan-entry scn]))
      =.  wallets.state  (~(put by wallets.state) pubkey updated-wallet)
      ;<  ~  bind:m  (replace:io !>(state))
      ;<  ~  bind:m  (send-sse-event:io /spv-wallet/stream/wallet/(crip (hexn:sailbox pubkey)) ~ `'discovery-status-update')
      ?:  (gte gap.scn 20)
        (pure:m ~)
      ;<  has-txs=?  bind:m
        (refresh-and-check-usage pubkey account-path 'change' idx.scn)
      =/  new-gap=@ud  ?:(has-txs 0 +(gap.scn))
      $(scn [%2 idx=+(idx.scn) gap=new-gap])
  ==
::  Run account discovery for a specific purpose/coin-type
::  Scans accounts sequentially until hitting one empty account (gap limit = 1)
::
++  run-account-discovery
  |=  [pubkey=@ux purpose=@ud coin-type=@ud]
  =/  m  (fiber:io ,~)
  ^-  form:m
  ::  Get PID and create scan key
  ;<  pid=@ta  bind:m  get-pid:io
  =/  scan-key=coin-type:hd-path  [[%.y purpose] [%.y coin-type]]
  ::  Initialize scan entry in wallet
  ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
  =/  wallet=(unit wallet:s)  (~(get by wallets.state) pubkey)
  ?~  wallet
    ~|  "wallet not found at discovery start"  !!
  =/  updated-wallet=wallet:s
    u.wallet(scan (~(put by scan.u.wallet) scan-key [pid %.y 0 [%1 idx=0 gap=0]]))
  =.  wallets.state  (~(put by wallets.state) pubkey updated-wallet)
  ;<  ~  bind:m  (replace:io !>(state))
  ::  Send SSE update to show discovery started
  ;<  ~  bind:m
    (send-sse-event:io /spv-wallet/stream/wallet/(crip (hexn:sailbox pubkey)) ~ `'discovery-status-update')
  ::  Start discovery loop
  =/  account-idx=@ud  0
  |-
  ::  Construct account path: m/purpose'/coin-type'/account-idx'
  =/  acct=account:hd-path  [[%.y purpose] [%.y coin-type] [%.y account-idx]]
  =/  account-path=@t  (crip (format-account-path acct))
  ::  Get current state and check if account exists
  ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
  =/  wallet=(unit wallet:s)  (~(get by wallets.state) pubkey)
  ?~  wallet
    ~|  "wallet not found during discovery"  !!
  =/  account-exists=?  (~(has by accounts.u.wallet) acct)
  ::  If account doesn't exist, add it with a temporary name
  ?.  account-exists
    =/  temp-name=@t  (crip "Account {(format-account-path acct)}")
    =/  script-type  (purpose-to-script-type q.purpose.acct)
    =/  network  (coin-type-to-network q.coin-type.acct)
    ::  Derive account pubkey and xpub for this account path
    =/  master-wallet  (from-seed:bip32 (seed-to-bytes seed.u.wallet))
    =/  path=tape
      %+  weld  "m/"
      %+  weld  (scow %ud q.purpose.acct)
      %+  weld  "'/"
      %+  weld  (scow %ud q.coin-type.acct)
      %+  weld  "'/"
      %+  weld  (scow %ud q.account.acct)
      "'"
    =/  derived  (derive-path:master-wallet path)
    =/  account-pubkey=@ux  public-key:derived
    ::  Convert to bip32 network for xprv encoding
    =/  bip32-net=?(%main %testnet %regtest)
      ?+  network  %testnet
        %main     %main
        %regtest  %regtest
      ==
    =/  xprv=@t  (crip (prv-extended:derived bip32-net))
    =/  new-details=account-details
      :*  temp-name              :: name
          `pubkey                :: wallet (reference to parent wallet)
          [%xprv xprv]           :: extended-key
          script-type            :: script-type
          network                :: network
          ~                      :: networks (empty map)
          [~ ~ ~ ~ ~]            :: proc
          %.n                    :: indexer-registered
      ==
    =.  accounts.state  (~(put by accounts.state) account-pubkey new-details)
    =/  updated-wallet=wallet:s
      u.wallet(accounts (~(put by accounts.u.wallet) acct account-pubkey))
    =.  wallets.state  (~(put by wallets.state) pubkey updated-wallet)
    ;<  ~  bind:m  (replace:io !>(state))
    ;<  ~  bind:m
      (send-sse-event:io /spv-wallet/stream/wallet/(crip (hexn:sailbox pubkey)) ~ `'account-list-update')
    $
  ::  Account exists, extract scan state and run full scan
  ::  Get scan state from discovery scan entry
  =/  scan-entry=(unit [pid=@ta act=? idx=@ud scn=account-scan])
    (~(get by scan.u.wallet) scan-key)
  ?~  scan-entry
    ~|  "scan entry not found during discovery"  !!
  ::  Run scan with current account-scan state
  ;<  ~  bind:m  (scan-account-addresses pubkey account-path scan-key scn.u.scan-entry)
  ::  After scan completes, update progress and check if account is empty
  ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
  =/  wallet=(unit wallet:s)  (~(get by wallets.state) pubkey)
  ?~  wallet
    ~|  "wallet not found after scan"  !!
  ::  Update scan entry with current account index
  =/  updated-wallet=wallet:s
    u.wallet(scan (~(put by scan.u.wallet) scan-key [pid %.y account-idx [%1 idx=0 gap=0]]))
  =.  wallets.state  (~(put by wallets.state) pubkey updated-wallet)
  ;<  ~  bind:m  (replace:io !>(state))
  ::  Send SSE update to show progress
  ;<  ~  bind:m
    (send-sse-event:io /spv-wallet/stream/wallet/(crip (hexn:sailbox pubkey)) ~ `'discovery-status-update')
  ::  Check if account is empty
  =/  account-pubkey=(unit @ux)
    (~(get by accounts.u.wallet) acct)
  ?~  account-pubkey
    ~|  "account pubkey not found after scan"  !!
  =/  account-data=(unit account-details)
    (~(get by accounts.state) u.account-pubkey)
  ?~  account-data
    ~|  "account not found after scan"  !!
  ::  Check if account has any transactions
  =/  has-transactions=?
    ?:  (gth ~(wyt by ~(get-txs ac [u.account-data active-network.u.account-data])) 0)  %.y
    %.n
  ::  If account is empty (no transactions), stop discovery
  ?.  has-transactions
    ::  Clear scan entry before stopping
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    =/  wallet=(unit wallet:s)  (~(get by wallets.state) pubkey)
    ?~  wallet
      ~|  "wallet not found at discovery end"  !!
    =/  updated-wallet=wallet:s
      u.wallet(scan (~(del by scan.u.wallet) scan-key))
    =.  wallets.state  (~(put by wallets.state) pubkey updated-wallet)
    ;<  ~  bind:m  (replace:io !>(state))
    ;<  ~  bind:m
      (send-sse-event:io /spv-wallet/stream/wallet/(crip (hexn:sailbox pubkey)) ~ `'discovery-status-update')
    (pure:m ~)
  ::  Account has transactions, continue to next account
  $(account-idx +(account-idx))
::  Resume account discovery from a saved state
::  Used when resuming a paused discovery
::
++  resume-account-discovery
  |=  [pubkey=@ux purpose=@ud coin-type=@ud start-idx=@ud]
  =/  m  (fiber:io ,~)
  ^-  form:m
  =/  scan-key=coin-type:hd-path  [[%.y purpose] [%.y coin-type]]
  ::  Start discovery loop from saved index
  =/  account-idx=@ud  start-idx
  |-
  ::  Construct account path: m/purpose'/coin-type'/account-idx'
  =/  acct=account:hd-path  [[%.y purpose] [%.y coin-type] [%.y account-idx]]
  =/  account-path=@t  (crip (format-account-path acct))
  ::  Get current state and check if account exists
  ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
  =/  wallet=(unit wallet:s)  (~(get by wallets.state) pubkey)
  ?~  wallet
    ~|  "wallet not found during discovery"  !!
  =/  account-exists=?  (~(has by accounts.u.wallet) acct)
  ::  If account doesn't exist, add it with a temporary name
  ?.  account-exists
    =/  temp-name=@t  (crip "Account {(format-account-path acct)}")
    =/  script-type  (purpose-to-script-type q.purpose.acct)
    =/  network  (coin-type-to-network q.coin-type.acct)
    ::  Derive account pubkey and xpub for this account path
    =/  master-wallet  (from-seed:bip32 (seed-to-bytes seed.u.wallet))
    =/  path=tape
      %+  weld  "m/"
      %+  weld  (scow %ud q.purpose.acct)
      %+  weld  "'/"
      %+  weld  (scow %ud q.coin-type.acct)
      %+  weld  "'/"
      %+  weld  (scow %ud q.account.acct)
      "'"
    =/  derived  (derive-path:master-wallet path)
    =/  account-pubkey=@ux  public-key:derived
    ::  Convert to bip32 network for xprv encoding
    =/  bip32-net=?(%main %testnet %regtest)
      ?+  network  %testnet
        %main     %main
        %regtest  %regtest
      ==
    =/  xprv=@t  (crip (prv-extended:derived bip32-net))
    =/  new-details=account-details
      :*  temp-name              :: name
          `pubkey                :: wallet (reference to parent wallet)
          [%xprv xprv]           :: extended-key
          script-type            :: script-type
          network                :: network
          ~                      :: networks (empty map)
          [~ ~ ~ ~ ~]            :: proc
          %.n                    :: indexer-registered
      ==
    =.  accounts.state  (~(put by accounts.state) account-pubkey new-details)
    =/  updated-wallet=wallet:s
      u.wallet(accounts (~(put by accounts.u.wallet) acct account-pubkey))
    =.  wallets.state  (~(put by wallets.state) pubkey updated-wallet)
    ;<  ~  bind:m  (replace:io !>(state))
    ;<  ~  bind:m
      (send-sse-event:io /spv-wallet/stream/wallet/(crip (hexn:sailbox pubkey)) ~ `'account-list-update')
    $
  ::  Account exists, extract scan state and run full scan
  ::  Get scan state from discovery scan entry
  =/  scan-entry=(unit [pid=@ta act=? idx=@ud scn=account-scan])
    (~(get by scan.u.wallet) scan-key)
  ?~  scan-entry
    ~|  "scan entry not found during discovery"  !!
  ;<  pid=@ta  bind:m  get-pid:io
  ::  Run scan with current account-scan state
  ;<  ~  bind:m  (scan-account-addresses pubkey account-path scan-key scn.u.scan-entry)
  ::  After scan completes, update progress and check if account is empty
  ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
  =/  wallet=(unit wallet:s)  (~(get by wallets.state) pubkey)
  ?~  wallet
    ~|  "wallet not found after scan"  !!
  ::  Update scan entry with current account index
  =/  updated-wallet
    u.wallet(scan (~(put by scan.u.wallet) scan-key [pid.u.scan-entry %.y account-idx [%1 idx=0 gap=0]]))
  =.  wallets.state  (~(put by wallets.state) pubkey updated-wallet)
  ;<  ~  bind:m  (replace:io !>(state))
  ::  Send SSE update to show progress
  ;<  ~  bind:m
    (send-sse-event:io /spv-wallet/stream/wallet/(crip (hexn:sailbox pubkey)) ~ `'discovery-status-update')
  ::  Check if account is empty
  =/  account-pubkey=(unit @ux)
    (~(get by accounts.u.wallet) acct)
  ?~  account-pubkey
    ~|  "account pubkey not found after scan"  !!
  =/  account-data=(unit account-details)
    (~(get by accounts.state) u.account-pubkey)
  ?~  account-data
    ~|  "account not found after scan"  !!
  ::  Check if account has any transactions
  =/  has-transactions=?
    ?:  (gth ~(wyt by ~(get-txs ac [u.account-data active-network.u.account-data])) 0)  %.y
    %.n
  ::  If account is empty (no transactions), stop discovery
  ?.  has-transactions
    ::  Clear scan entry before stopping
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    =/  wallet=(unit wallet:s)  (~(get by wallets.state) pubkey)
    ?~  wallet
      ~|  "wallet not found at discovery end"  !!
    =/  updated-wallet=wallet:s
      u.wallet(scan (~(del by scan.u.wallet) scan-key))
    =.  wallets.state  (~(put by wallets.state) pubkey updated-wallet)
    ;<  ~  bind:m  (replace:io !>(state))
    ;<  ~  bind:m
      (send-sse-event:io /spv-wallet/stream/wallet/(crip (hexn:sailbox pubkey)) ~ `'discovery-status-update')
    (pure:m ~)
  ::  Account has transactions, continue to next account
  $(account-idx +(account-idx))
--
