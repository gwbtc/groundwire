/-  s=spv-wallet, *indexer
/+  io=sailboxio, html-utils, sailbox, bitcoin, wallet-account
|%
::  Compute script-hash from address string
::  The indexer uses sha256(scriptPubKey) as the key
::
++  address-to-script-hash
  |=  address=@t
  ^-  @ux
  =/  script-pubkey=hexb:bitcoin  (to-script-pubkey:adr:bitcoin [%bech32 address])
  (sha-256:sha dat.script-pubkey)
::
::  Handle indexer subscription actions from address page
::
++  handle-indexer-actions
  |=  $:  pubkey=@ux
          args=key-value-list:kv:html-utils
          action=@t
      ==
  =/  m  (fiber:io ,~)
  ^-  form:m
  ?+  action  (pure:m ~)
    ::
      %subscribe-indexer
    =/  address=@t  (need (get-key:kv:html-utils 'address' args))
    =/  chain=@t  (fall (get-key:kv:html-utils 'chain' args) 'receiving')
    =/  index=@ud
      =/  idx-str  (get-key:kv:html-utils 'index' args)
      ?~  idx-str  0
      (rash u.idx-str dem)
    ::  Compute script-hash for this address
    =/  script-hash=@ux  (address-to-script-hash address)
    ~&  >>  "Subscribing to indexer for address {<address>}"
    ~&  >>  "Script-hash: {<script-hash>}"
    ::  Check if there's already a subscription - if so, kill old fiber first
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    =/  existing=(unit indexer-sub:s)  (~(get by indexer-subs.state) script-hash)
    ;<  ~  bind:m
      ?~  existing  (pure:m ~)
      ?:  =('' pid.u.existing)  (pure:m ~)
      ~&  >>  "Killing existing subscription fiber {<pid.u.existing>}"
      (fiber-kill:io pid.u.existing)
    ::  Get new pid for the fiber we're about to spawn
    ;<  new-pid=@ta  bind:m  get-pid:io
    ::  Store subscription with the pid
    =/  sub=indexer-sub:s
      :*  pubkey           :: account-pubkey
          chain            :: chain
          index            :: index
          address          :: address
          new-pid          :: pid
          %.y              :: act
          ~                :: last-update
      ==
    =.  indexer-subs.state  (~(put by indexer-subs.state) script-hash sub)
    ;<  ~  bind:m  (replace:io !>(state))
    ::  Run the subscription fiber (it will loop and handle updates)
    (indexer-subscription-fiber pubkey script-hash address chain index)
    ::
      %unsubscribe-indexer
    =/  address=@t  (need (get-key:kv:html-utils 'address' args))
    =/  script-hash=@ux  (address-to-script-hash address)
    ~&  >>  "Unsubscribing from indexer for address {<address>}"
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    =/  existing=(unit indexer-sub:s)  (~(get by indexer-subs.state) script-hash)
    ::  Kill the fiber if it exists
    ;<  ~  bind:m
      ?~  existing  (pure:m ~)
      ?:  =('' pid.u.existing)  (pure:m ~)
      ~&  >>  "Killing subscription fiber {<pid.u.existing>}"
      (fiber-kill:io pid.u.existing)
    ::  Get chain/index from existing sub before removing
    =/  sub-chain=@t  ?~(existing 'receiving' chain.u.existing)
    =/  sub-index=@ud  ?~(existing 0 index.u.existing)
    ::  Remove from state
    =.  indexer-subs.state  (~(del by indexer-subs.state) script-hash)
    ;<  ~  bind:m  (replace:io !>(state))
    ::  Send SSE to update UI
    =/  sse-path=path  /spv-wallet/stream/account/(crip (hexn:sailbox pubkey))
    =/  sse-data=@t  (crip "{(trip sub-chain)}/{(scow %ud sub-index)}")
    ;<  ~  bind:m  (send-sse-event:io sse-path `sse-data `'indexer-unsubscribed')
    (pure:m ~)
  ==
::
::  Long-running fiber that subscribes to indexer and forwards updates as SSE
::
++  indexer-subscription-fiber
  |=  [account-pubkey=@ux script-hash=@ux address=@t chain=@t index=@ud]
  =/  m  (fiber:io ,~)
  ^-  form:m
  ~&  >>  "Starting indexer subscription fiber for {<address>}"
  ::  Subscribe to the indexer agent
  =/  watch-path=path  /script-hash/(scot %ux script-hash)
  ;<  ~  bind:m  (watch-our:io /indexer/sub/(scot %ux script-hash) %indexer watch-path)
  ~&  >>  "Subscribed to indexer at {<watch-path>}"
  ::  Send initial SSE to confirm subscription
  =/  sse-path=path  /spv-wallet/stream/account/(crip (hexn:sailbox account-pubkey))
  =/  sse-data=@t  (crip "{(trip chain)}/{(scow %ud index)}")
  ;<  ~  bind:m  (send-sse-event:io sse-path `sse-data `'indexer-subscribed')
  ::  Loop waiting for facts
  |-
  ;<  =cage  bind:m  (take-fact:io /indexer/sub/(scot %ux script-hash))
  ~&  >>  "Received indexer update: {<p.cage>}"
  ::  Check if we're still subscribed (haven't been removed from state)
  ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
  ?.  (~(has by indexer-subs.state) script-hash)
    ::  We've been unsubscribed, leave and stop
    ~&  >>  "Subscription removed, leaving indexer"
    ;<  ~  bind:m  (leave-our:io /indexer/sub/(scot %ux script-hash) %indexer)
    (pure:m ~)
  ::  Update last-update time in state
  =/  sub=indexer-sub:s  (~(got by indexer-subs.state) script-hash)
  ;<  now=@da  bind:m  get-time:io
  =.  last-update.sub  `now
  =.  indexer-subs.state  (~(put by indexer-subs.state) script-hash sub)
  ;<  ~  bind:m  (replace:io !>(state))
  ::  Parse and forward the update as SSE
  ?>  ?=(%script-hash-update p.cage)
  =/  update=script-hash-update  !<(script-hash-update q.cage)
  ~&  >>  "Indexer update type: {<-.update>}"
  ::  Store history in address-details
  =/  acct=account-details:s  (~(got by accounts.state) account-pubkey)
  =/  network=network:s  active-network.acct
  =/  addr-unit=(unit address-details:s)  (~(get-addr ac:wallet-account [acct network]) chain index)
  ?~  addr-unit
    ~&  >>>  "No address found at {<chain>}/{<index>}, skipping history update"
    $
  =/  addr=address-details:s  u.addr-unit
  ::  Update history based on update type
  =.  indexer-history.addr
    ?+  -.update   indexer-history.addr
      %init        confirmed.update
      %confirmed   (snoc indexer-history.addr +.update)
    ==
  ::  Store updated address back in state
  =/  updated-acct=account-details:s  (~(put-addr ac:wallet-account [acct network]) chain index addr)
  =.  accounts.state  (~(put by accounts.state) account-pubkey updated-acct)
  ;<  ~  bind:m  (replace:io !>(state))
  ~&  >>  "Stored indexer history for {<chain>}/{<index>}"
  ::  Send SSE event with chain/index so UI can refresh the right section
  =/  sse-data=@t  (crip "{(trip chain)}/{(scow %ud index)}")
  ;<  ~  bind:m  (send-sse-event:io sse-path `sse-data `'indexer-update')
  ::  Continue looping
  $
--
