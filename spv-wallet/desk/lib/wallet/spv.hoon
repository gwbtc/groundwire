::  Core SPV operations for block header management
::
/-  *spv-wallet
/+  io=sailboxio, bitcoin-spv
|%
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
::  Fetch and add a block header at the given height
::  This is the core business logic that both HTTP handlers and auto-sync use
::
++  fetch-and-add-header
  |=  [=network height=@ud]
  =/  m  (fiber:io ,~)
  ^-  form:m
  ::  Step 1: Fetch block hash from height
  =/  height-url=tape  "{(mempool-base network)}/block-height/{(a-co:co height)}"
  ;<  block-hash=cord  bind:m  (fetch-cord:io height-url)
  ::  Step 2: Fetch raw header hex
  =/  header-url=tape  "{(mempool-base network)}/block/{(trip block-hash)}/header"
  ;<  raw-header-text=cord  bind:m  (fetch-cord:io header-url)
  ::  Validate length (should be 160 hex chars for 80 bytes)
  ?>  =(160 (lent (trip raw-header-text)))
  ::  Step 3: Convert hex text to @ux
  =/  raw-hex=@ux  (rash raw-header-text hex)
  ::  Step 4: Parse header using library function
  =/  header=block-header:bitcoin-spv  (parse-raw-to-header:bitcoin-spv raw-header-text height)
  ::  Step 5: Validate PoW and calculate work
  =/  [validated-header=block-header:bitcoin-spv pow-valid=? work=@ud]
    (validate-header-with-hash:bitcoin-spv header `@ux`computed-hash.header)
  ::  Step 6: Get state and chain to validate
  ;<  state=state-0  bind:m  (get-state-as:io state-0)
  =/  chain=spv-chain  (get-chain network spv.state)
  ::  Step 7: Validate hash match and chain connection (pure computations that could fail)
  =/  chain-result=(each [spv-chain @uvI] tang)
    %-  mule  |.
    ::  First verify our computed hash matches network's hash
    =/  network-hash=@uvI  (rash block-hash hex)
    ?.  =(network-hash computed-hash.validated-header)
      ~|  %hash-mismatch-network-vs-computed
      ~|  ['network-hash' network-hash]
      ~|  ['computed-hash' computed-hash.validated-header]
      ~|  ['height' height]
      !!
    ::  Now validate chain connection
    =/  [valid=? parent=(unit block-header:bitcoin-spv)]
      (validate-chain-connection:bitcoin-spv validated-header headers.chain checkpoint-hash.chain)
    ?.  valid
      ::  Add diagnostic context before crashing
      ~|  %chain-validation-failed
      ~|  ['Height' height.validated-header]
      ~|  ['Header-hash' computed-hash.validated-header]
      ~|  ['Previous-hash' prev-hash.validated-header]
      ~|  ['Parent-found' ?~(parent %no %yes)]
      ~|  ?~  parent
            %parent-not-in-chain
          ['Parent-hash' computed-hash.u.parent 'Parent-height' height.u.parent]
      ~|  ['Checkpoint-hash' checkpoint-hash.chain]
      !!
    ::  Calculate cumulative work and update header
    ~|  %failed-cumulative-work-calculation
    =/  cumulative-work=@ud
      (calculate-cumulative-work:bitcoin-spv parent work)
    =/  final-header=block-header:bitcoin-spv
      validated-header(cumulative-work cumulative-work)
    ::  Add header to chain (updates parent's children if needed)
    ~|  %failed-header-chain-insertion
    =/  updated-headers=(map @uvI block-header:bitcoin-spv)
      (add-header-to-chain:bitcoin-spv final-header parent headers.chain)
    [chain(headers updated-headers) computed-hash.final-header]
  ::  Step 8: Handle result
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
  (send-sse-event:io (spv-sse-path network) ~ `'spv-update')
  ==
--
