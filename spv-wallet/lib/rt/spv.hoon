/-  *spv-wallet
/+  io=sailboxio, html-utils, bitcoin-spv, json-utils, spv=wallet-spv
=,  dejs:format
|%
::  Helper to get network from args, defaults to testnet3
::
++  get-network
  |=  args=key-value-list:kv:html-utils
  ^-  network
  =/  net=(unit @t)  (get-key:kv:html-utils 'network' args)
  ?~  net  %testnet3
  ?+  u.net  %testnet3
    %main      %main
    %testnet3  %testnet3
    %testnet4  %testnet4
    %signet    %signet
    %regtest   %regtest
  ==
::  Helper to get or create spv-chain for a network
::
++  get-chain
  |=  [net=network spv=(map network spv-chain)]
  ^-  spv-chain
  =/  chain=(unit spv-chain)  (~(get by spv) net)
  ?^  chain  u.chain
  ::  Return empty chain with network-specific checkpoint defaults
  =/  [height=@ud hash=@uvI]
    ?+  net  !!
      %main      [925.000 `@uvI`(rash '0000000000000000000067f9f40ca6960173ebee423f6130138762dfc40630bf' hex)]
      %testnet3  [2.500.000 `@uvI`(rash '000000000000004a6039a59a81ad9ca1f6b143ad7c487f2c9e1c1d8e96e1e5ba' hex)]
      %testnet4  [111.000 `@uvI`(rash '0000000007838a124187712a910ef0573cb76104c9a612d8dc4029b964a79007' hex)]
    ==
  :*  ~       :: headers
      ~       :: headers-by-height
      ~       :: headers-by-work
      height  :: checkpoint-height
      hash    :: checkpoint-hash
      ~       :: header-sync
      ~       :: sync-error
  ==
::  Helper to build SSE path for network
::
++  spv-sse-path
  |=  =network
  ^-  path
  /spv-wallet/stream/spv/[network]
::  Helper to get mempool.space API base URL for network
::
++  mempool-base
  |=  =network
  ^-  tape
  ?-  network
    %main      "https://mempool.space/api"
    %testnet3  "https://mempool.space/testnet/api"
    %testnet4  "https://mempool.space/testnet4/api"
    %signet    "https://mempool.space/signet/api"
    %regtest   "http://localhost:3000"
  ==
::  Handle SPV actions (block headers, verification)
::
++  handle-spv-actions
  |=  args=key-value-list:kv:html-utils
  =/  m  (fiber:io ,~)
  ^-  form:m
  =/  action=@t  (need (get-key:kv:html-utils 'action' args))
  =/  =network  (get-network args)
  ~&  [spv-action+action network+network]
  ?+  action  !!
      %set-checkpoint
    ::  Get height from form
    =/  height=@ud  (rash (need (get-key:kv:html-utils 'height' args)) dem)
    ::  Fetch block hash at this height from mempool.space
    =/  height-url=tape  "{(mempool-base network)}/block-height/{(a-co:co height)}"
    ;<  block-hash-text=cord  bind:m  (fetch-cord:io height-url)
    ::  Bitcoin block hashes from APIs are in little-endian format, same as our computed hashes
    =/  block-hash=@uvI  (rash block-hash-text hex)
    ::  Update state with new checkpoint
    ;<  state=state-0  bind:m  (get-state-as:io state-0)
    =/  chain=spv-chain  (get-chain network spv.state)
    =.  checkpoint-height.chain  height
    =.  checkpoint-hash.chain  block-hash
    =.  spv.state  (~(put by spv.state) network chain)
    ;<  ~  bind:m  (replace:io !>(state))
    ::  Send SSE update to refresh UI
    ;<  ~  bind:m  (send-sse-event:io (spv-sse-path network) ~ `'spv-update')
    (pure:m ~)
    ::
      %add-header
    ::  Get raw header hex and height from form
    =/  raw-hex-text=@t  (need (get-key:kv:html-utils 'raw-hex' args))
    =/  height=@ud  (rash (need (get-key:kv:html-utils 'height' args)) dem)
    ::  Parse header using library function
    =/  header=block-header:bitcoin-spv  (parse-raw-to-header:bitcoin-spv raw-hex-text height)
    ::  Validate PoW and calculate work
    =/  [validated-header=block-header:bitcoin-spv pow-valid=? work=@ud]
      (validate-header-with-hash:bitcoin-spv header `@ux`computed-hash.header)
    ::  Get state to validate chain
    ;<  state=state-0  bind:m  (get-state-as:io state-0)
    =/  chain=spv-chain  (get-chain network spv.state)
    ::  Validate chain connection and add header (pure computations that could fail)
    =/  chain-result=(each [spv-chain @uvI] tang)
      %-  mule  |.
      =/  [valid=? parent=(unit block-header:bitcoin-spv)]
        (validate-chain-connection:bitcoin-spv validated-header headers.chain checkpoint-hash.chain)
      ?.  valid
        !!
      ::  Calculate cumulative work and update header
      =/  cumulative-work=@ud
        (calculate-cumulative-work:bitcoin-spv parent work)
      =/  final-header=block-header:bitcoin-spv
        validated-header(cumulative-work cumulative-work)
      ::  Add header to chain (updates parent's children if needed)
      =/  updated-headers=(map @uvI block-header:bitcoin-spv)
        (add-header-to-chain:bitcoin-spv final-header parent headers.chain)
      [chain(headers updated-headers) computed-hash.final-header]
    ?-  -.chain-result
      %|
    ::  Chain validation failed - store error, clear sync, and exit
    =.  header-sync.chain  ~
    =.  sync-error.chain  `p.chain-result
    =.  spv.state  (~(put by spv.state) network chain)
    ;<  ~  bind:m  (replace:io !>(state))
    ;<  ~  bind:m  (send-sse-event:io (spv-sse-path network) ~ `'sync-status-update')
    (pure:m ~)
      %&
    =/  [updated-chain=spv-chain header-hash=@uvI]  p.chain-result
    =.  chain  updated-chain
    ::  Add to height index
    =/  existing-at-height=(unit (set @uvI))
      (get:((on @ud (set @uvI)) lth) headers-by-height.chain height)
    =/  new-set=(set @uvI)
      ?~  existing-at-height
        (~(put in *(set @uvI)) header-hash)
      (~(put in u.existing-at-height) header-hash)
    =.  headers-by-height.chain
      (put:((on @ud (set @uvI)) lth) headers-by-height.chain height new-set)
    =.  spv.state  (~(put by spv.state) network chain)
    ;<  ~  bind:m  (replace:io !>(state))
    (pure:m ~)
    ==
    ::
      %delete-header
    ::  Get hash from form
    =/  hash=@uvI  (rash (need (get-key:kv:html-utils 'hash' args)) hex)
    ::  Get state and header
    ;<  state=state-0  bind:m  (get-state-as:io state-0)
    =/  chain=spv-chain  (get-chain network spv.state)
    =/  header=(unit block-header:bitcoin-spv)  (~(get by headers.chain) hash)
    ?~  header
      (pure:m ~)
    ::  If I have children, delete first child recursively
    ?.  =(0 ~(wyt in children.u.header))
      =/  first-child=@uvI  (snag 0 ~(tap in children.u.header))
      ::  Delete child first by recursively calling handle-spv-actions
      =/  child-args=key-value-list:kv:html-utils
        :~  ['action' 'delete-header']
            ['hash' (scot %ux first-child)]
            ['network' (network-to-cord network)]
        ==
      ;<  ~  bind:m  (handle-spv-actions child-args)
      $
    ::  Remove from parent's children set
    =/  parent=(unit block-header:bitcoin-spv)  (~(get by headers.chain) prev-hash.u.header)
    =/  headers-after-parent-update
      ?~  parent
        headers.chain
      =/  updated-parent=block-header:bitcoin-spv  u.parent(children (~(del in children.u.parent) hash))
      (~(put by headers.chain) prev-hash.u.header updated-parent)
    ::  Remove from headers map
    =/  new-headers  (~(del by headers-after-parent-update) hash)
    ::  Remove from height map
    =/  height-set=(unit (set @uvI))  (get:((on @ud (set @uvI)) lth) headers-by-height.chain height.u.header)
    =/  new-by-height
      ?~  height-set  headers-by-height.chain
      =/  updated-set  (~(del in u.height-set) hash)
      ?:  =(0 ~(wyt in updated-set))
        +:(del:((on @ud (set @uvI)) lth) headers-by-height.chain height.u.header)
      (put:((on @ud (set @uvI)) lth) headers-by-height.chain height.u.header updated-set)
    ::  Update state
    =.  headers.chain  new-headers
    =.  headers-by-height.chain  new-by-height
    =.  spv.state  (~(put by spv.state) network chain)
    ;<  ~  bind:m  (replace:io !>(state))
    (pure:m ~)
    ::
      %fetch-add-header
    ::  Get height from form
    =/  height=@ud  (rash (need (get-key:kv:html-utils 'height' args)) dem)
    ::  Call business logic function
    ;<  ~  bind:m  (fetch-and-add-header:spv network height)
    ::  Send SSE event to refresh SPV page
    ;<  ~  bind:m  (send-sse-event:io (spv-sse-path network) ~ `'spv-update')
    (pure:m ~)
    ::
      %verify-transaction
    ::  Get txid from form
    =/  txid=@t  (need (get-key:kv:html-utils 'txid' args))
    ::  Fetch merkle proof from mempool.space
    =/  url=tape  "{(mempool-base network)}/tx/{(trip txid)}/merkle-proof"
    ;<  jon=json  bind:m  (fetch-json:io url)
    ::  Parse merkle proof data
    =/  jo-parser  ~(. jo:json-utils jon)
    =/  block-height=(unit @ud)
      (mole |.((ni (got:jo-parser /['block_height']))))
    ?~  block-height
      ~&  >>>  "Failed to get block_height from merkle proof response"
      ~&  >>>  "Transaction may not be confirmed yet"
      (pure:m ~)
    =/  merkle=(list @t)  ((ar so) (got:jo-parser /merkle))
    =/  pos=@ud  (ni (got:jo-parser /pos))
    ::  Fetch block hash from block height
    =/  hash-url=tape  "{(mempool-base network)}/block-height/{(a-co:co u.block-height)}"
    ;<  block-hash-hex=cord  bind:m  (fetch-cord:io hash-url)
    =/  block-hash=@uvI  (rash block-hash-hex hex)
    ::  Get state and check if we have this block header
    ;<  state=state-0  bind:m  (get-state-as:io state-0)
    =/  chain=spv-chain  (get-chain network spv.state)
    =/  header=(unit block-header:bitcoin-spv)  (~(get by headers.chain) block-hash)
    ?~  header
      ~&  >>>  "Block header not found locally: {<block-hash-hex>}"
      ~&  >>>  "You need to fetch block at height {<u.block-height>} first"
      (pure:m ~)
    ::  Verify the merkle proof against our local header
    =/  merkle-root-hex=@t  (crip ((x-co:co 64) merkle-root.u.header))
    =/  verification-result=?
      (verify-merkle-proof:bitcoin-spv txid merkle pos merkle-root-hex)
    ~&  >>>  "Transaction verification result: {<verification-result>}"
    ~&  >>>  "TXID: {<txid>}"
    ~&  >>>  "Block height: {<u.block-height>}"
    ~&  >>>  "Block hash: {<block-hash-hex>}"
    ?:  verification-result
      ~&  >>>  "✓ Transaction IS included in block {<u.block-height>}"
      (pure:m ~)
    ~&  >>>  "✗ Transaction NOT included in block (proof verification failed)"
    (pure:m ~)
    ::
      %start-header-sync
    ::  Get current tip height
    ;<  state=state-0  bind:m  (get-state-as:io state-0)
    =/  chain=spv-chain  (get-chain network spv.state)
    ::  Guard: don't start if already running
    ?^  header-sync.chain
      ~|  "header sync already running"  !!
    ::  Determine start height
    ::  If no headers, start at checkpoint (fetch it first as trusted base)
    ::  If headers exist, start at tip + 1 (continue from where we left off)
    =/  tip=(unit [key=@ud val=(set @uvI)])
      (ram:((on @ud (set @uvI)) lth) headers-by-height.chain)
    =/  start-height=@ud
      ?~  tip
        checkpoint-height.chain
      +(key.u.tip)
    ::  Get PID and initialize sync state, clear any previous errors
    ;<  pid=@ta  bind:m  get-pid:io
    =/  new-chain=spv-chain
      %=  chain
        header-sync  `[pid %.y start-height]
        sync-error   ~
      ==
    =.  spv.state  (~(put by spv.state) network new-chain)
    ;<  ~  bind:m  (replace:io !>(state))
    ;<  ~  bind:m  (send-sse-event:io (spv-sse-path network) ~ `'sync-status-update')
    ::  Start the sync fiber
    (run-header-sync network start-height)
    ::
      %pause-header-sync
    ;<  state=state-0  bind:m  (get-state-as:io state-0)
    =/  chain=spv-chain  (get-chain network spv.state)
    ?~  header-sync.chain
      ~|  "no header sync running"  !!
    =/  [sync-pid=@ta sync-act=? sync-height=@ud]  u.header-sync.chain
    ?.  sync-act
      ~|  "header sync already paused"  !!
    ::  Kill the fiber and mark as paused
    ;<  ~  bind:m  (fiber-kill:io sync-pid)
    =/  new-chain=spv-chain  chain(header-sync `[sync-pid %.n sync-height])
    =.  spv.state  (~(put by spv.state) network new-chain)
    ;<  ~  bind:m  (replace:io !>(state))
    ;<  ~  bind:m  (send-sse-event:io (spv-sse-path network) ~ `'sync-status-update')
    (pure:m ~)
    ::
      %resume-header-sync
    ;<  state=state-0  bind:m  (get-state-as:io state-0)
    =/  chain=spv-chain  (get-chain network spv.state)
    ?~  header-sync.chain
      ~|  "no header sync to resume"  !!
    =/  [sync-pid=@ta sync-act=? sync-height=@ud]  u.header-sync.chain
    ?:  sync-act
      ~|  "header sync already active"  !!
    ::  Get new PID and resume
    ;<  new-pid=@ta  bind:m  get-pid:io
    =/  new-chain=spv-chain  chain(header-sync `[new-pid %.y sync-height])
    =.  spv.state  (~(put by spv.state) network new-chain)
    ;<  ~  bind:m  (replace:io !>(state))
    ;<  ~  bind:m  (send-sse-event:io (spv-sse-path network) ~ `'sync-status-update')
    ::  Resume from saved height
    (run-header-sync network sync-height)
    ::
      %cancel-header-sync
    ;<  state=state-0  bind:m  (get-state-as:io state-0)
    =/  chain=spv-chain  (get-chain network spv.state)
    ?~  header-sync.chain
      (pure:m ~)
    =/  [sync-pid=@ta sync-act=? sync-height=@ud]  u.header-sync.chain
    ::  Kill the fiber (whether active or paused)
    ;<  ~  bind:m  (fiber-kill:io sync-pid)
    ::  Clear sync state
    =/  new-chain=spv-chain  chain(header-sync ~)
    =.  spv.state  (~(put by spv.state) network new-chain)
    ;<  ~  bind:m  (replace:io !>(state))
    ;<  ~  bind:m  (send-sse-event:io (spv-sse-path network) ~ `'sync-status-update')
    (pure:m ~)
    ::
      %clear-all-headers
    ::  Clear all headers from storage
    ;<  state=state-0  bind:m  (get-state-as:io state-0)
    =/  chain=spv-chain  (get-chain network spv.state)
    =.  headers.chain  ~
    =.  headers-by-height.chain  ~
    =.  headers-by-work.chain  ~
    =.  spv.state  (~(put by spv.state) network chain)
    ;<  ~  bind:m  (replace:io !>(state))
    ;<  ~  bind:m  (send-sse-event:io (spv-sse-path network) ~ `'spv-update')
    (pure:m ~)
    ::
      %clear-headers-after
    ::  Clear headers after specified height
    =/  height=@ud  (rash (need (get-key:kv:html-utils 'height' args)) dem)
    ;<  state=state-0  bind:m  (get-state-as:io state-0)
    =/  chain=spv-chain  (get-chain network spv.state)
    ::  Filter out headers with height > specified height
    =/  filtered-headers=(map @uvI block-header:bitcoin-spv)
      %-  ~(gas by *(map @uvI block-header:bitcoin-spv))
      %+  skim  ~(tap by headers.chain)
      |=  [hash=@uvI header=block-header:bitcoin-spv]
      (lte height.header height)
    =/  filtered-by-height=((mop @ud (set @uvI)) lth)
      %-  gas:((on @ud (set @uvI)) lth)
      :-  *((mop @ud (set @uvI)) lth)
      %+  skim  (tap:((on @ud (set @uvI)) lth) headers-by-height.chain)
      |=  [h=@ud hashes=(set @uvI)]
      (lte h height)
    =.  headers.chain  filtered-headers
    =.  headers-by-height.chain  filtered-by-height
    =.  headers-by-work.chain  ~  ::  TODO: recompute work index
    =.  spv.state  (~(put by spv.state) network chain)
    ;<  ~  bind:m  (replace:io !>(state))
    ;<  ~  bind:m  (send-sse-event:io (spv-sse-path network) ~ `'spv-update')
    (pure:m ~)
    ::
      %clear-headers-before
    ::  Clear headers before specified height
    =/  height=@ud  (rash (need (get-key:kv:html-utils 'height' args)) dem)
    ;<  state=state-0  bind:m  (get-state-as:io state-0)
    =/  chain=spv-chain  (get-chain network spv.state)
    ::  Filter out headers with height < specified height
    =/  filtered-headers=(map @uvI block-header:bitcoin-spv)
      %-  ~(gas by *(map @uvI block-header:bitcoin-spv))
      %+  skim  ~(tap by headers.chain)
      |=  [hash=@uvI header=block-header:bitcoin-spv]
      (gte height.header height)
    =/  filtered-by-height=((mop @ud (set @uvI)) lth)
      %-  gas:((on @ud (set @uvI)) lth)
      :-  *((mop @ud (set @uvI)) lth)
      %+  skim  (tap:((on @ud (set @uvI)) lth) headers-by-height.chain)
      |=  [h=@ud hashes=(set @uvI)]
      (gte h height)
    =.  headers.chain  filtered-headers
    =.  headers-by-height.chain  filtered-by-height
    =.  headers-by-work.chain  ~  ::  TODO: recompute work index
    =.  spv.state  (~(put by spv.state) network chain)
    ;<  ~  bind:m  (replace:io !>(state))
    ;<  ~  bind:m  (send-sse-event:io (spv-sse-path network) ~ `'spv-update')
    (pure:m ~)
  ==
::  Helper to convert network to cord
::
++  network-to-cord
  |=  =network
  ^-  @t
  ?+  network  !!
    %main      'main'
    %testnet3  'testnet3'
    %testnet4  'testnet4'
  ==
::  Scry helpers for querying block header data
::
++  get-header-count
  |=  =network
  =/  m  (fiber:io ,@ud)
  ^-  form:m
  ;<  state=state-0  bind:m  (get-state-as:io state-0)
  =/  chain=spv-chain  (get-chain network spv.state)
  (pure:m (lent ~(tap by headers.chain)))
::
++  get-header-by-hash
  |=  [=network hash=@uvI]
  =/  m  (fiber:io ,(unit block-header:bitcoin-spv))
  ^-  form:m
  ;<  state=state-0  bind:m  (get-state-as:io state-0)
  =/  chain=spv-chain  (get-chain network spv.state)
  (pure:m (~(get by headers.chain) hash))
::
++  get-highest-header
  |=  =network
  =/  m  (fiber:io ,(unit [key=@ud val=(set @uvI)]))
  ^-  form:m
  ;<  state=state-0  bind:m  (get-state-as:io state-0)
  =/  chain=spv-chain  (get-chain network spv.state)
  (pure:m (ram:((on @ud (set @uvI)) lth) headers-by-height.chain))
::
++  get-tip-height
  |=  =network
  =/  m  (fiber:io ,[height=(unit @ud) count=@ud])
  ^-  form:m
  ;<  state=state-0  bind:m  (get-state-as:io state-0)
  =/  chain=spv-chain  (get-chain network spv.state)
  =/  tip=(unit [key=@ud val=(set @uvI)])
    (ram:((on @ud (set @uvI)) lth) headers-by-height.chain)
  ?~  tip
    (pure:m [~ 0])
  (pure:m [`key.u.tip ~(wyt in val.u.tip)])
::
++  get-headers-at-height
  |=  [=network height=@ud]
  =/  m  (fiber:io ,(unit (set @uvI)))
  ^-  form:m
  ;<  state=state-0  bind:m  (get-state-as:io state-0)
  =/  chain=spv-chain  (get-chain network spv.state)
  (pure:m (get:((on @ud (set @uvI)) lth) headers-by-height.chain height))
::
++  get-checkpoint
  |=  =network
  =/  m  (fiber:io ,[@ud @uvI])
  ^-  form:m
  ;<  state=state-0  bind:m  (get-state-as:io state-0)
  =/  chain=spv-chain  (get-chain network spv.state)
  (pure:m [checkpoint-height.chain checkpoint-hash.chain])
::
++  get-longest-chain
  |=  =network
  =/  m  (fiber:io ,(unit [key=@ud val=(set @uvI)]))
  ^-  form:m
  ;<  state=state-0  bind:m  (get-state-as:io state-0)
  =/  chain=spv-chain  (get-chain network spv.state)
  (pure:m (pry:((on @ud (set @uvI)) lth) headers-by-work.chain))
::
::  Header sync process - continuously fetches headers from tip
::
++  run-header-sync
  |=  [=network start-height=@ud]
  =/  m  (fiber:io ,~)
  ^-  form:m
  =/  current-height=@ud  start-height
  |-
  ::  Check if sync was cancelled/paused
  ;<  state=state-0  bind:m  (get-state-as:io state-0)
  =/  chain=spv-chain  (get-chain network spv.state)
  ?~  header-sync.chain
    ::  Sync was cancelled, exit
    (pure:m ~)
  ::  Update current height in state
  =/  [sync-pid=@ta sync-act=? sync-height=@ud]  u.header-sync.chain
  =/  new-chain=spv-chain  chain(header-sync `[sync-pid sync-act current-height])
  =.  spv.state  (~(put by spv.state) network new-chain)
  ;<  ~  bind:m  (replace:io !>(state))
  ::  Call business logic function directly
  ;<  ~  bind:m  (fetch-and-add-header:spv network current-height)
  ::  Continue with next height
  $(current-height +(current-height))
--
