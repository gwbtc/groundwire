/-  s=spv-wallet, indexer
/+  io=sailboxio, html-utils, sailbox, json-utils,
    *wallet-address, *wallet-mempool-space, *wallet-account, bip329,
    rt-indexer=rt-indexer
|%
::  Unified full scan for any account type
::  Works with wallet accounts, signing accounts, and watch-only accounts
::
++  run-account-full-scan
  |=  pubkey=@ux
  =/  m  (fiber:io ,~)
  ^-  form:m
  ::  Start scanning loop
  |-
  ::  Get current state
  ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
  =/  details=(unit account-details)  (~(get by accounts.state) pubkey)
  ?~  details
    ~|  "account not found during scan"  !!
  ::  Extract current phase, index and gap from scan.proc
  ?~  scan.proc.u.details
    ~|  "scan.proc not initialized"  !!
  =/  [scan-pid=@ta scan-act=? scn=account-scan]  u.scan.proc.u.details
  ::  Branch on scan phase
  ?-    -.scn
      %1
    ::  Phase 1: Receiving address scan
    ?:  (gte gap.scn 20)
      ::  Transition to phase 2
      =/  updated-details=account-details
        u.details(proc proc.u.details(scan `[scan-pid scan-act [%2 0 0]]))
      =.  accounts.state  (~(put by accounts.state) pubkey updated-details)
      ;<  ~  bind:m  (replace:io !>(state))
      ;<  ~  bind:m
        (send-sse-event:io (account-stream-path pubkey) ~ `%scan-status-update)
      $
    ::  Continue receiving scan
    ;<  ~  bind:m  (refresh-account-address pubkey 'receiving' idx.scn)
    ::  Get updated state
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    =/  details=(unit account-details)  (~(get by accounts.state) pubkey)
    ?~  details
      ~|  "account not found after scan"  !!
    =/  address-data=(unit address-details)
      =/  leaf=(unit hd-leaf)  (get:((on @ud hd-leaf) gth) ~(get-receiving-mop ac [u.details active-network.u.details]) idx.scn)
      ?~(leaf ~ `main.u.leaf)
    =/  tx-count=@ud
      ?~  address-data  0
      ?~  info.u.address-data  0
      tx-count.u.info.u.address-data
    =/  new-gap=@ud  ?:(=(0 tx-count) +(gap.scn) 0)
    =/  updated-details=account-details
      u.details(proc proc.u.details(scan `[scan-pid scan-act [%1 +(idx.scn) new-gap]]))
    =.  accounts.state  (~(put by accounts.state) pubkey updated-details)
    ;<  ~  bind:m  (replace:io !>(state))
    ;<  ~  bind:m
      (send-sse-event:io (account-stream-path pubkey) ~ `%scan-status-update)
    $
  ::
      %2
    ::  Phase 2: Change address scan
    ?:  (gte gap.scn 20)
      ::  Clear scan.proc and finish
      =/  cleared-details=account-details
        u.details(proc proc.u.details(scan ~))
      =.  accounts.state  (~(put by accounts.state) pubkey cleared-details)
      ;<  ~  bind:m  (replace:io !>(state))
      ;<  ~  bind:m
        (send-sse-event:io (account-stream-path pubkey) ~ `%scan-status-update)
      (pure:m ~)
    ::  Continue change scan
    ;<  ~  bind:m  (refresh-account-address pubkey 'change' idx.scn)
    ::  Get updated state
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    =/  details=(unit account-details)  (~(get by accounts.state) pubkey)
    ?~  details
      ~|  "account not found after change scan"  !!
    =/  address-data=(unit address-details)
      =/  leaf=(unit hd-leaf)  (get:((on @ud hd-leaf) gth) ~(get-change-mop ac [u.details active-network.u.details]) idx.scn)
      ?~(leaf ~ `main.u.leaf)
    =/  tx-count=@ud
      ?~  address-data  0
      ?~  info.u.address-data  0
      tx-count.u.info.u.address-data
    =/  new-gap=@ud  ?:(=(0 tx-count) +(gap.scn) 0)
    =/  updated-details=account-details
      u.details(proc proc.u.details(scan `[scan-pid scan-act [%2 +(idx.scn) new-gap]]))
    =.  accounts.state  (~(put by accounts.state) pubkey updated-details)
    ;<  ~  bind:m  (replace:io !>(state))
    ;<  ~  bind:m
      (send-sse-event:io (account-stream-path pubkey) ~ `%scan-status-update)
    $
  ==
::
::  Unified handler for any account type (signing or watch-only)
::  Handles all actions: refresh, scan, pause, resume, cancel
::
++  handle-account-actions
  |=  [pubkey=@ux args=key-value-list:kv:html-utils]
  =/  m  (fiber:io ,~)
  ^-  form:m
  =/  action=@t  (need (get-key:kv:html-utils 'action' args))
  ?+  action  !!
      %refresh-address
    =/  chain=@t  (need (get-key:kv:html-utils 'chain' args))
    =/  index=@ud  (rash (need (get-key:kv:html-utils 'index' args)) dem)
    ;<  pid=@ta  bind:m  get-pid:io
    ;<  ~  bind:m  (set-account-address-pid pubkey chain index pid)
    ;<  ~  bind:m  (refresh-account-address pubkey chain index)
    (clear-account-address-pid pubkey chain index)
      %cancel-refresh
    =/  chain=@t  (need (get-key:kv:html-utils 'chain' args))
    =/  index=@ud  (rash (need (get-key:kv:html-utils 'index' args)) dem)
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    =/  details=account-details  (need (~(get by accounts.state) pubkey))
    =/  proc-map=(map @ud [pid=@ta act=?])
      (get-proc-map details chain)
    =/  maybe-entry=(unit [pid=@ta act=?])  (~(get by proc-map) index)
    ?~  maybe-entry  (pure:m ~)
    ;<  ~  bind:m  (fiber-kill:io pid.u.maybe-entry)
    ;<  ~  bind:m  (clear-account-address-pid pubkey chain index)
    =/  row-event=@t  (crip "{(trip chain)}-row-update")
    (send-sse-event:io (account-stream-path pubkey) `(crip (scow %ud index)) `row-event)
      %full-scan
    ;<  pid=@ta  bind:m  get-pid:io
    ;<  ~  bind:m  (set-account-scan-pid pubkey pid)
    ;<  ~  bind:m
      (send-sse-event:io (account-stream-path pubkey) ~ `%scan-status-update)
    (run-account-full-scan pubkey)
      %cancel-scan
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    =/  details=account-details  (need (~(get by accounts.state) pubkey))
    ?~  scan.proc.details
      (pure:m ~)
    =/  [scan-pid=@ta scan-act=? scn=account-scan]  u.scan.proc.details
    ;<  ~  bind:m  (fiber-kill:io scan-pid)
    =/  updated-details=account-details
      details(proc proc.details(scan ~))
    =.  accounts.state  (~(put by accounts.state) pubkey updated-details)
    ;<  ~  bind:m  (replace:io !>(state))
    (send-sse-event:io (account-stream-path pubkey) ~ `%scan-status-update)
      %pause-scan
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    =/  details=account-details  (need (~(get by accounts.state) pubkey))
    ?~  scan.proc.details
      (pure:m ~)
    =/  [scan-pid=@ta scan-act=? scn=account-scan]  u.scan.proc.details
    ?.  scan-act
      ~|  "scan already paused"  !!
    ;<  ~  bind:m  (fiber-kill:io scan-pid)
    =/  updated-details=account-details
      details(proc proc.details(scan `[scan-pid %.n scn]))
    =.  accounts.state  (~(put by accounts.state) pubkey updated-details)
    ;<  ~  bind:m  (replace:io !>(state))
    (send-sse-event:io (account-stream-path pubkey) ~ `%scan-status-update)
      %resume-scan
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    =/  details=account-details  (need (~(get by accounts.state) pubkey))
    ?~  scan.proc.details
      ~|  "no scan to resume"  !!
    =/  [scan-pid=@ta scan-act=? scn=account-scan]  u.scan.proc.details
    ?:  scan-act
      ~|  "scan already active"  !!
    ;<  new-pid=@ta  bind:m  get-pid:io
    =/  updated-details=account-details
      details(proc proc.details(scan `[new-pid %.y scn]))
    =.  accounts.state  (~(put by accounts.state) pubkey updated-details)
    ;<  ~  bind:m  (replace:io !>(state))
    ;<  ~  bind:m
      (send-sse-event:io (account-stream-path pubkey) ~ `%scan-status-update)
    (run-account-full-scan pubkey)
      %verify-transaction
    =/  txid=@t  (need (get-key:kv:html-utils 'txid' args))
    ::  Clear verification status first
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    =/  details=account-details  (need (~(get by accounts.state) pubkey))
    =/  cleared-details=account-details
      (~(set-tx-verify ac [details active-network.details]) txid ~)
    =.  accounts.state  (~(put by accounts.state) pubkey cleared-details)
    ;<  ~  bind:m  (replace:io !>(state))
    ;<  ~  bind:m  (send-sse-event:io (account-stream-path pubkey) `txid `'tx-verify-update')
    ::  Get PID and claim the slot
    ;<  pid=@ta  bind:m  get-pid:io
    ;<  ~  bind:m  (set-account-tx-verify-pid pubkey txid pid)
    ;<  ~  bind:m  (send-sse-event:io (account-stream-path pubkey) `txid `'tx-verify-update')
    ;<  ~  bind:m  (verify-account-transaction pubkey txid)
    ;<  ~  bind:m  (clear-account-tx-verify-pid pubkey txid)
    (send-sse-event:io (account-stream-path pubkey) `txid `'tx-verify-update')
      %cancel-verify
    =/  txid=@t  (need (get-key:kv:html-utils 'txid' args))
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    =/  details=account-details  (need (~(get by accounts.state) pubkey))
    =/  maybe-entry=(unit [pid=@ta act=?])
      (~(get by tx-verify.proc.details) txid)
    ?~  maybe-entry  (pure:m ~)
    ;<  ~  bind:m  (fiber-kill:io pid.u.maybe-entry)
    ;<  ~  bind:m  (clear-account-tx-verify-pid pubkey txid)
    (send-sse-event:io (account-stream-path pubkey) `txid `'tx-verify-update')
      %pause-refresh
    =/  chain=@t  (need (get-key:kv:html-utils 'chain' args))
    =/  index=@ud  (rash (need (get-key:kv:html-utils 'index' args)) dem)
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    =/  details=account-details  (need (~(get by accounts.state) pubkey))
    =/  proc-map=(map @ud [pid=@ta act=?])
      (get-proc-map details chain)
    =/  maybe-entry=(unit [pid=@ta act=?])
      (~(get by proc-map) index)
    ?~  maybe-entry  (pure:m ~)
    =/  [proc-pid=@ta proc-act=?]  u.maybe-entry
    ?.  proc-act
      ~|  "refresh already paused"  !!
    ;<  ~  bind:m  (fiber-kill:io proc-pid)
    =/  updated-proc-map=(map @ud [pid=@ta act=?])
      (~(put by proc-map) index [proc-pid %.n])
    =/  updated-details=account-details
      ?:  =(chain 'receiving')
        details(proc proc.details(receiving updated-proc-map))
      details(proc proc.details(change updated-proc-map))
    =.  accounts.state  (~(put by accounts.state) pubkey updated-details)
    ;<  ~  bind:m  (replace:io !>(state))
    =/  row-event=@t  (crip "{(trip chain)}-row-update")
    (send-sse-event:io (account-stream-path pubkey) `(crip (scow %ud index)) `row-event)
      %resume-refresh
    =/  chain=@t  (need (get-key:kv:html-utils 'chain' args))
    =/  index=@ud  (rash (need (get-key:kv:html-utils 'index' args)) dem)
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    =/  details=account-details  (need (~(get by accounts.state) pubkey))
    =/  proc-map=(map @ud [pid=@ta act=?])
      (get-proc-map details chain)
    =/  maybe-entry=(unit [pid=@ta act=?])
      (~(get by proc-map) index)
    ?~  maybe-entry
      ~|  "no refresh to resume"  !!
    =/  [proc-pid=@ta proc-act=?]  u.maybe-entry
    ?:  proc-act
      ~|  "refresh already active"  !!
    ;<  new-pid=@ta  bind:m  get-pid:io
    =/  updated-proc-map=(map @ud [pid=@ta act=?])
      (~(put by proc-map) index [new-pid %.y])
    =/  updated-details=account-details
      ?:  =(chain 'receiving')
        details(proc proc.details(receiving updated-proc-map))
      details(proc proc.details(change updated-proc-map))
    =.  accounts.state  (~(put by accounts.state) pubkey updated-details)
    ;<  ~  bind:m  (replace:io !>(state))
    =/  row-event=@t  (crip "{(trip chain)}-row-update")
    ;<  ~  bind:m
      (send-sse-event:io (account-stream-path pubkey) `(crip (scow %ud index)) `row-event)
    ;<  ~  bind:m  (refresh-account-address pubkey chain index)
    (clear-account-address-pid pubkey chain index)
      %pause-verify
    =/  txid=@t  (need (get-key:kv:html-utils 'txid' args))
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    =/  details=account-details  (need (~(get by accounts.state) pubkey))
    =/  maybe-entry=(unit [pid=@ta act=?])
      (~(get by tx-verify.proc.details) txid)
    ?~  maybe-entry
      ~|  "no verification to pause"  !!
    =/  [proc-pid=@ta proc-act=?]  u.maybe-entry
    ?.  proc-act
      ~|  "verification already paused"  !!
    ;<  ~  bind:m  (fiber-kill:io proc-pid)
    =/  updated-proc-map=(map @t [pid=@ta act=?])
      (~(put by tx-verify.proc.details) txid [proc-pid %.n])
    =/  updated-details=account-details
      details(proc proc.details(tx-verify updated-proc-map))
    =.  accounts.state  (~(put by accounts.state) pubkey updated-details)
    ;<  ~  bind:m  (replace:io !>(state))
    (send-sse-event:io (account-stream-path pubkey) `txid `'tx-verify-update')
      %resume-verify
    =/  txid=@t  (need (get-key:kv:html-utils 'txid' args))
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    =/  details=account-details  (need (~(get by accounts.state) pubkey))
    =/  maybe-entry=(unit [pid=@ta act=?])
      (~(get by tx-verify.proc.details) txid)
    ?~  maybe-entry
      ~|  "no verification to resume"  !!
    =/  [proc-pid=@ta proc-act=?]  u.maybe-entry
    ?:  proc-act
      ~|  "verification already active"  !!
    ;<  new-pid=@ta  bind:m  get-pid:io
    =/  updated-proc-map=(map @t [pid=@ta act=?])
      (~(put by tx-verify.proc.details) txid [new-pid %.y])
    =/  updated-details=account-details
      details(proc proc.details(tx-verify updated-proc-map))
    =.  accounts.state  (~(put by accounts.state) pubkey updated-details)
    ;<  ~  bind:m  (replace:io !>(state))
    ;<  ~  bind:m  (send-sse-event:io (account-stream-path pubkey) `txid `'tx-verify-update')
    ;<  ~  bind:m  (verify-account-transaction pubkey txid)
    ;<  ~  bind:m  (clear-account-tx-verify-pid pubkey txid)
    (send-sse-event:io (account-stream-path pubkey) `txid `'tx-verify-update')
      %delete-address
    =/  chain=@t  (need (get-key:kv:html-utils 'chain' args))
    =/  index=@ud  (rash (need (get-key:kv:html-utils 'index' args)) dem)
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    =/  details=account-details  (need (~(get by accounts.state) pubkey))
    ::  Check if this is the top address (head of mop with gth comparator)
    =/  spv-net  active-network.details
    =/  leaf-mop=((mop @ud hd-leaf) gth)
      ?:(=(chain 'receiving') ~(get-receiving-mop ac [details spv-net]) ~(get-change-mop ac [details spv-net]))
    =/  mop-head  (pry:((on @ud hd-leaf) gth) leaf-mop)
    =/  is-top-address=?
      ?~  mop-head
        %.n
      =(index key.u.mop-head)
    ::  Delete the address from the mop
    =/  nd  (~(gut by networks.details) spv-net *network-details)
    =/  updated-details=account-details
      ?:  =(chain 'receiving')
        =/  new-receiving=((mop @ud hd-leaf) gth)
          +:(del:((on @ud hd-leaf) gth) ~(get-receiving-mop ac [details spv-net]) index)
        details(networks (~(put by networks.details) spv-net nd(receiving new-receiving)))
      =/  new-change=((mop @ud hd-leaf) gth)
        +:(del:((on @ud hd-leaf) gth) ~(get-change-mop ac [details spv-net]) index)
      details(networks (~(put by networks.details) spv-net nd(change new-change)))
    =.  accounts.state  (~(put by accounts.state) pubkey updated-details)
    ;<  ~  bind:m  (replace:io !>(state))
    ::  Send appropriate SSE update
    =/  list-event=@t  (crip "{(trip chain)}-list-update")
    =/  row-event=@t  (crip "{(trip chain)}-row-update")
    ?:  is-top-address
      ::  Deleting top address - send full list update
      (send-sse-event:io (account-stream-path pubkey) ~ `list-event)
    ::  Not top address - send row update
    (send-sse-event:io (account-stream-path pubkey) `(crip (scow %ud index)) `row-event)
  ::
      %delete-tapscript
    =/  chain=@t  (need (get-key:kv:html-utils 'chain' args))
    =/  index=@ud  (rash (need (get-key:kv:html-utils 'index' args)) dem)
    =/  tapscript-addr=@t  (need (get-key:kv:html-utils 'tapscript-addr' args))
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    =/  details=account-details  (~(got by accounts.state) pubkey)
    =/  spv-net  active-network.details
    =/  nd  (~(got by networks.details) spv-net)
    ::  Get the mop first, then get the leaf
    =/  leaf-mop=((mop @ud hd-leaf) gth)
      ?:(=(chain 'receiving') ~(get-receiving-mop ac [details spv-net]) ~(get-change-mop ac [details spv-net]))
    =/  leaf=(unit hd-leaf)  (get:((on @ud hd-leaf) gth) leaf-mop index)
    ?~  leaf
      ~&  >>>  "delete-tapscript: leaf not found at index {<index>}"
      (pure:m ~)
    =/  updated-trees=(map @t tapscript-details)
      (~(del by script-trees.u.leaf) tapscript-addr)
    =/  updated-leaf=hd-leaf  u.leaf(script-trees updated-trees)
    ::  Put the updated leaf back
    =/  updated-details=account-details
      ?:  =(chain 'receiving')
        =/  new-receiving=((mop @ud hd-leaf) gth)
          (put:((on @ud hd-leaf) gth) leaf-mop index updated-leaf)
        details(networks (~(put by networks.details) spv-net nd(receiving new-receiving)))
      =/  new-change=((mop @ud hd-leaf) gth)
        (put:((on @ud hd-leaf) gth) leaf-mop index updated-leaf)
      details(networks (~(put by networks.details) spv-net nd(change new-change)))
    =.  accounts.state  (~(put by accounts.state) pubkey updated-details)
    ;<  ~  bind:m  (replace:io !>(state))
    ::  Send row update
    =/  row-event=@t  (crip "{(trip chain)}-row-update")
    (send-sse-event:io (account-stream-path pubkey) `(crip (scow %ud index)) `row-event)
  ::
      %rename-tapscript
    =/  chain=@t  (need (get-key:kv:html-utils 'chain' args))
    =/  index=@ud  (rash (need (get-key:kv:html-utils 'index' args)) dem)
    =/  tapscript-addr=@t  (need (get-key:kv:html-utils 'tapscript-addr' args))
    =/  new-name=@t  (need (get-key:kv:html-utils 'name' args))
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    =/  details=account-details  (~(got by accounts.state) pubkey)
    =/  spv-net  active-network.details
    =/  nd  (~(got by networks.details) spv-net)
    =/  leaf-mop=((mop @ud hd-leaf) gth)
      ?:(=(chain 'receiving') ~(get-receiving-mop ac [details spv-net]) ~(get-change-mop ac [details spv-net]))
    =/  leaf=(unit hd-leaf)  (get:((on @ud hd-leaf) gth) leaf-mop index)
    ?~  leaf
      ~&  >>>  "rename-tapscript: leaf not found at index {<index>}"
      (pure:m ~)
    =/  ts-details=(unit tapscript-details)  (~(get by script-trees.u.leaf) tapscript-addr)
    ?~  ts-details
      ~&  >>>  "rename-tapscript: tapscript not found at addr {(trip tapscript-addr)}"
      (pure:m ~)
    =/  updated-ts=tapscript-details  u.ts-details(name new-name)
    =/  updated-trees=(map @t tapscript-details)
      (~(put by script-trees.u.leaf) tapscript-addr updated-ts)
    =/  updated-leaf=hd-leaf  u.leaf(script-trees updated-trees)
    =/  updated-details=account-details
      ?:  =(chain 'receiving')
        =/  new-receiving=((mop @ud hd-leaf) gth)
          (put:((on @ud hd-leaf) gth) leaf-mop index updated-leaf)
        details(networks (~(put by networks.details) spv-net nd(receiving new-receiving)))
      =/  new-change=((mop @ud hd-leaf) gth)
        (put:((on @ud hd-leaf) gth) leaf-mop index updated-leaf)
      details(networks (~(put by networks.details) spv-net nd(change new-change)))
    =.  accounts.state  (~(put by accounts.state) pubkey updated-details)
    ;<  ~  bind:m  (replace:io !>(state))
    =/  row-event=@t  (crip "{(trip chain)}-row-update")
    (send-sse-event:io (account-stream-path pubkey) `(crip (scow %ud index)) `row-event)
  ::
      %refresh-tapscript
    =/  chain=@t  (need (get-key:kv:html-utils 'chain' args))
    =/  index=@ud  (rash (need (get-key:kv:html-utils 'index' args)) dem)
    =/  tapscript-addr=@t  (need (get-key:kv:html-utils 'tapscript-addr' args))
    ~&  >  "refresh-tapscript: {(trip chain)} index {<index>} addr {(trip tapscript-addr)}"
    ::  Set process tracking
    ;<  new-pid=@ta  bind:m  get-pid:io
    ~&  >>  "got new pid: {<new-pid>}"
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    =/  details=account-details  (~(got by accounts.state) pubkey)
    ~&  >>  "current tapscript proc: {<tapscript.proc.details>}"
    =/  updated-proc-map=(map @t [pid=@ta act=?])
      (~(put by tapscript.proc.details) tapscript-addr [new-pid %.y])
    ~&  >>  "updated tapscript proc: {<updated-proc-map>}"
    =/  updated-details=account-details
      details(proc proc.details(tapscript updated-proc-map))
    =.  accounts.state  (~(put by accounts.state) pubkey updated-details)
    ;<  ~  bind:m  (replace:io !>(state))
    ~&  >>  "saved state, sending SSE"
    =/  row-event=@t  (crip "{(trip chain)}-row-update")
    ;<  ~  bind:m  (send-sse-event:io (account-stream-path pubkey) `(crip (scow %ud index)) `row-event)
    ~&  >>  "SSE sent, now running refresh"
    ::  Run the refresh
    ;<  ~  bind:m  (refresh-tapscript-address pubkey chain index tapscript-addr)
    ::  Clear process tracking
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    =/  details=account-details  (~(got by accounts.state) pubkey)
    =/  cleared-proc-map=(map @t [pid=@ta act=?])
      (~(del by tapscript.proc.details) tapscript-addr)
    =/  cleared-details=account-details
      details(proc proc.details(tapscript cleared-proc-map))
    =.  accounts.state  (~(put by accounts.state) pubkey cleared-details)
    ;<  ~  bind:m  (replace:io !>(state))
    (send-sse-event:io (account-stream-path pubkey) `(crip (scow %ud index)) `row-event)
  ::
      %pause-tapscript-refresh
    =/  tapscript-addr=@t  (need (get-key:kv:html-utils 'tapscript-addr' args))
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    =/  details=account-details  (~(got by accounts.state) pubkey)
    =/  maybe-entry=(unit [pid=@ta act=?])  (~(get by tapscript.proc.details) tapscript-addr)
    ?~  maybe-entry  (pure:m ~)
    =/  [proc-pid=@ta proc-act=?]  u.maybe-entry
    ?.  proc-act  (pure:m ~)  ::  already paused
    ;<  ~  bind:m  (fiber-kill:io proc-pid)
    =/  updated-proc-map=(map @t [pid=@ta act=?])
      (~(put by tapscript.proc.details) tapscript-addr [proc-pid %.n])
    =/  updated-details=account-details
      details(proc proc.details(tapscript updated-proc-map))
    =.  accounts.state  (~(put by accounts.state) pubkey updated-details)
    ;<  ~  bind:m  (replace:io !>(state))
    ::  Send update - need to find chain and index for this tapscript
    (send-sse-event:io (account-stream-path pubkey) ~ `'receiving-list-update')
  ::
      %resume-tapscript-refresh
    =/  tapscript-addr=@t  (need (get-key:kv:html-utils 'tapscript-addr' args))
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    =/  details=account-details  (~(got by accounts.state) pubkey)
    =/  maybe-entry=(unit [pid=@ta act=?])  (~(get by tapscript.proc.details) tapscript-addr)
    ?~  maybe-entry  (pure:m ~)
    =/  [proc-pid=@ta proc-act=?]  u.maybe-entry
    ?:  proc-act  (pure:m ~)  ::  already active
    ::  Clear old entry and trigger new refresh
    =/  cleared-proc-map=(map @t [pid=@ta act=?])
      (~(del by tapscript.proc.details) tapscript-addr)
    =/  cleared-details=account-details
      details(proc proc.details(tapscript cleared-proc-map))
    =.  accounts.state  (~(put by accounts.state) pubkey cleared-details)
    ;<  ~  bind:m  (replace:io !>(state))
    ::  TODO: would need chain and index to restart - for now just clear
    (send-sse-event:io (account-stream-path pubkey) ~ `'receiving-list-update')
  ::
      %cancel-tapscript-refresh
    =/  tapscript-addr=@t  (need (get-key:kv:html-utils 'tapscript-addr' args))
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    =/  details=account-details  (~(got by accounts.state) pubkey)
    =/  maybe-entry=(unit [pid=@ta act=?])  (~(get by tapscript.proc.details) tapscript-addr)
    ?~  maybe-entry  (pure:m ~)
    ;<  ~  bind:m  (fiber-kill:io pid.u.maybe-entry)
    =/  cleared-proc-map=(map @t [pid=@ta act=?])
      (~(del by tapscript.proc.details) tapscript-addr)
    =/  cleared-details=account-details
      details(proc proc.details(tapscript cleared-proc-map))
    =.  accounts.state  (~(put by accounts.state) pubkey cleared-details)
    ;<  ~  bind:m  (replace:io !>(state))
    (send-sse-event:io (account-stream-path pubkey) ~ `'receiving-list-update')
  ::
      %set-output-labels
    ~&  >>  "=== SET OUTPUT LABELS HANDLER CALLED ==="
    ::  Parse form fields: txid, vout, labels (comma-separated)
    =/  utxo-txid=@t  (need (get-key:kv:html-utils 'utxo-txid' args))
    =/  utxo-vout=@ud  (need (rush (need (get-key:kv:html-utils 'utxo-vout' args)) dem))
    =/  labels-str=@t  (fall (get-key:kv:html-utils 'labels' args) '')
    ::  Build label key: "txid:vout"
    =/  label-key=@t  (crip "{(trip utxo-txid)}:{(scow %ud utxo-vout)}")
    ::  Parse comma-separated labels into list
    =/  label-list=(list @t)
      ?:  =('' labels-str)  ~
      ::  Split by comma
      =/  str=tape  (trip labels-str)
      =/  result=(list tape)  ~
      =/  current=tape  ~
      |-  ^-  (list @t)
      ?~  str
        ::  Done - add final segment if non-empty
        =/  final=(list tape)  ?~(current result [current result])
        (flop (turn final crip))
      ?:  =(i.str ',')
        ::  Found comma - add current to result and reset
        $(str t.str, current ~, result [current result])
      ::  Accumulate character
      $(str t.str, current (snoc current i.str))
    ~&  >>  "Setting labels for {<label-key>}: {<label-list>}"
    ::  Get current state and update global labels
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    =.  labels.state  (~(set-texts la:bip329 labels.state) %output label-key label-list)
    ;<  ~  bind:m  (replace:io !>(state))
    (pure:m ~)
  ::
      %set-utxo-frozen
    ~&  >>  "=== SET UTXO FROZEN HANDLER CALLED ==="
    =/  utxo-txid=@t  (need (get-key:kv:html-utils 'utxo-txid' args))
    =/  utxo-vout=@ud  (need (rush (need (get-key:kv:html-utils 'utxo-vout' args)) dem))
    =/  frozen=?  =('true' (need (get-key:kv:html-utils 'frozen' args)))
    ::  Build label key: "txid:vout"
    =/  label-key=@t  (crip "{(trip utxo-txid)}:{(scow %ud utxo-vout)}")
    ~&  >>  "Setting frozen={<frozen>} for {<label-key>}"
    ::  Get current state and update global labels
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    =.  labels.state
      ?:  frozen
        (~(freeze la:bip329 labels.state) label-key)
      (~(thaw la:bip329 labels.state) label-key)
    ;<  ~  bind:m  (replace:io !>(state))
    (pure:m ~)
  ::
      %set-network
    =/  network-str=@t  (need (get-key:kv:html-utils 'network' args))
    =/  new-network=network
      ?+  network-str  %testnet3
        %main      %main
        %testnet3  %testnet3
        %testnet4  %testnet4
        %signet    %signet
        %regtest   %regtest
      ==
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    =/  details=account-details  (need (~(get by accounts.state) pubkey))
    =/  updated-details=account-details
      details(active-network new-network)
    =/  new-state=state-0:s
      state(accounts (~(put by accounts.state) pubkey updated-details))
    ;<  ~  bind:m  (replace:io !>(new-state))
    (pure:m ~)
  ::
      %subscribe-indexer
    (handle-indexer-actions:rt-indexer pubkey args action)
  ::
      %unsubscribe-indexer
    (handle-indexer-actions:rt-indexer pubkey args action)
  ::
      %register-indexer
    ::  Register this account's xpub with the indexer agent
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    =/  details=account-details  (need (~(get by accounts.state) pubkey))
    =/  xub=@t
      ?-  -.extended-key.details
        %xpub  k.extended-key.details
        %xprv  k.extended-key.details  :: TODO: derive xpub from xprv
      ==
    ::  Convert script-type to indexer purpose
    =/  idx-purpose=purpose:indexer
      ?-  script-type.details
        %p2pkh        %44
        %p2sh-p2wpkh  %49
        %p2wpkh       %84
        %p2tr         %86
      ==
    ::  Convert network to indexer network (indexer only has %main, %testnet, %regtest)
    =/  idx-network=network:indexer
      ?-  active-network.details
        %main      %main
        %testnet4  %testnet
        %regtest   %regtest
        %testnet3  !!
        %signet    !!
      ==
    =/  new-acct=new-account-args:indexer
      :*  xpub=xub
          purpose=idx-purpose
          network=idx-network
          gap-limit=20
          start-block=[~ 26.640]
      ==
    ~&  >>  "Registering account with indexer: {<xub>}"
    ~&  >>  "Purpose: {<idx-purpose>}, Network: {<idx-network>}"
    ;<  =bowl:gall  bind:m  get-bowl:io
    ;<  ~  bind:m  (poke:io [our.bowl %indexer] add-account+!>(new-acct))
    ::  Update account to mark as registered
    =/  updated-details=account-details  details(indexer-registered %.y)
    =.  accounts.state  (~(put by accounts.state) pubkey updated-details)
    ;<  ~  bind:m  (replace:io !>(state))
    ;<  ~  bind:m  (send-sse-event:io (account-stream-path pubkey) ~ `%indexer-status-update)
    (pure:m ~)
  ::
      %deregister-indexer
    ::  Deregister this account's xpub from the indexer agent
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    =/  details=account-details  (need (~(get by accounts.state) pubkey))
    =/  xub=xpub:indexer
      ?-  -.extended-key.details
        %xpub  k.extended-key.details
        %xprv  k.extended-key.details
      ==
    ~&  >>  "Deregistering account from indexer: {<xub>}"
    ;<  =bowl:gall  bind:m  get-bowl:io
    ;<  ~  bind:m  (poke:io [our.bowl %indexer] del-account+!>(xub))
    ::  Update account to mark as not registered
    =/  updated-details=account-details  details(indexer-registered %.n)
    =.  accounts.state  (~(put by accounts.state) pubkey updated-details)
    ;<  ~  bind:m  (replace:io !>(state))
    ;<  ~  bind:m  (send-sse-event:io (account-stream-path pubkey) ~ `%indexer-status-update)
    (pure:m ~)
  ==
--
