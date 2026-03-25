/-  s=spv-wallet, tt=transactions
/+  io=sailboxio, html-utils, sailbox, json-utils, txns=tx-build, *wallet-address,
    bip32=bip32-spv, txns=tx-build, bip329, wallet-account, bcu=bitcoin-utils,
    fees=tx-fees, sel=tx-select, drft=tx-draft, wutxo=wallet-utxo, taproot
|%
::  Safe list indexing (returns unit instead of crashing)
::
++  snag-safe
  |*  [index=@ud list=(list)]
  ^-  (unit _?>(?=(^ list) i.list))
  ?~  list  ~
  ?:  =(index 0)  `i.list
  $(index (dec index), list t.list)
::  Handle send transaction actions
::  Uses account-pubkey directly - much simpler than old wallet+path approach
::
++  handle-send-actions
  |=  [account-pubkey=@ux args=key-value-list:kv:html-utils]
  =/  m  (fiber:io ,~)
  ^-  form:m
  =/  action=@t  (need (get-key:kv:html-utils 'action' args))
  ~&  >>  "handle-send-actions called with action: {<action>}"
  ::  Get state and account details directly
  ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
  =/  details=(unit account-details:s)  (~(get by accounts.state) account-pubkey)
  ?~  details
    ~&  >>>  "account not found for pubkey {<account-pubkey>}"
    (pure:m ~)
  =/  ac  ~(. ac:wallet-account [u.details active-network.u.details])
  ?+    action  !!
      %add-output
    ~&  >>  "=== ADD OUTPUT HANDLER CALLED ==="
    ::  Parse form fields
    =/  output-address=@t  (need (get-key:kv:html-utils 'output-address' args))
    =/  output-amount=@ud  (need (rush (need (get-key:kv:html-utils 'output-amount' args)) dem))
    ::  Get or create draft
    ;<  now=@da  bind:m  get-time:io
    =/  existing-draft  draft:ac
    =/  draft=transaction:drft
      ?~  existing-draft
        [~ ~ ~ `%random now now]
      u.existing-draft(modified now)
    ::  Add new output
    =/  new-output=output:drft  [output-address output-amount]
    =.  outputs.draft  (snoc outputs.draft new-output)
    ::  Update account details
    =/  updated=account-details:s  (set-draft:ac draft)
    =.  accounts.state  (~(put by accounts.state) account-pubkey updated)
    ;<  ~  bind:m  (replace:io !>(state))
    ::  Send SSE update
    (send-sse-event:io /spv-wallet/stream/account/(crip (hexn:sailbox account-pubkey))/send ~ `%draft-outputs-update)
    ::
      %clear-draft
    ~&  >>  "=== CLEAR DRAFT HANDLER CALLED ==="
    =/  updated=account-details:s  clear-draft:ac
    =.  accounts.state  (~(put by accounts.state) account-pubkey updated)
    ;<  ~  bind:m  (replace:io !>(state))
    (send-sse-event:io /spv-wallet/stream/account/(crip (hexn:sailbox account-pubkey))/send ~ `%draft-outputs-update)
    ::
      %delete-output
    ~&  >>  "=== DELETE OUTPUT HANDLER CALLED ==="
    =/  output-index=@ud  (need (rush (need (get-key:kv:html-utils 'output-index' args)) dem))
    =/  existing-draft  draft:ac
    ?~  existing-draft
      ~&  >>>  "no draft to delete from"
      (pure:m ~)
    ;<  now=@da  bind:m  get-time:io
    =/  draft=transaction:drft  u.existing-draft(modified now)
    ::  Delete output at index
    =/  outputs-before=(list output:drft)  (scag output-index outputs.draft)
    =/  outputs-after=(list output:drft)  (slag +(output-index) outputs.draft)
    =.  outputs.draft  (weld outputs-before outputs-after)
    =/  updated=account-details:s  (set-draft:ac draft)
    =.  accounts.state  (~(put by accounts.state) account-pubkey updated)
    ;<  ~  bind:m  (replace:io !>(state))
    (send-sse-event:io /spv-wallet/stream/account/(crip (hexn:sailbox account-pubkey))/send ~ `%draft-outputs-update)
    ::
      %add-input
    ~&  >>  "=== ADD INPUT HANDLER CALLED ==="
    =/  utxo-txid=@t  (need (get-key:kv:html-utils 'utxo-txid' args))
    =/  utxo-vout=@ud  (need (rush (need (get-key:kv:html-utils 'utxo-vout' args)) dem))
    =/  utxo-value=@ud  (need (rush (need (get-key:kv:html-utils 'utxo-value' args)) dem))
    ::  Check if UTXO is frozen (spendable=%.n) using global labels
    =/  label-key=@t  (crip "{(trip utxo-txid)}:{(scow %ud utxo-vout)}")
    ?:  (~(frozen la:bip329 labels.state) label-key)
      ~&  >>>  "UTXO is frozen, cannot add to inputs"
      (pure:m ~)
    ;<  now=@da  bind:m  get-time:io
    =/  existing-draft  draft:ac
    =/  draft=transaction:drft
      ?~  existing-draft
        [~ ~ ~ `%random now now]
      u.existing-draft(modified now)
    ::  Add input with account's spend type
    =/  spend=spend:fees  script-type.u.details
    =/  new-input=utxo-input:drft  [utxo-txid utxo-vout utxo-value spend]
    =.  inputs.draft  (weld inputs.draft [new-input ~])
    =/  updated=account-details:s  (set-draft:ac draft)
    =.  accounts.state  (~(put by accounts.state) account-pubkey updated)
    ;<  ~  bind:m  (replace:io !>(state))
    (send-sse-event:io /spv-wallet/stream/account/(crip (hexn:sailbox account-pubkey))/send ~ `%draft-outputs-update)
    ::
      %remove-input
    ~&  >>  "=== REMOVE INPUT HANDLER CALLED ==="
    =/  utxo-txid=@t  (need (get-key:kv:html-utils 'utxo-txid' args))
    =/  utxo-vout=@ud  (need (rush (need (get-key:kv:html-utils 'utxo-vout' args)) dem))
    =/  existing-draft  draft:ac
    ?~  existing-draft
      ~&  >>>  "no draft to remove from"
      (pure:m ~)
    ;<  now=@da  bind:m  get-time:io
    =/  draft=transaction:drft  u.existing-draft(modified now)
    =.  inputs.draft
      %+  skip  inputs.draft
      |=  input=utxo-input:drft
      &(=(txid.input utxo-txid) =(vout.input utxo-vout))
    =/  updated=account-details:s  (set-draft:ac draft)
    =.  accounts.state  (~(put by accounts.state) account-pubkey updated)
    ;<  ~  bind:m  (replace:io !>(state))
    (send-sse-event:io /spv-wallet/stream/account/(crip (hexn:sailbox account-pubkey))/send ~ `%draft-outputs-update)
    ::
      %set-change-config
    ~&  >>  "=== SET CHANGE CONFIG HANDLER CALLED ==="
    =/  fee-rate=@ud  (need (rush (need (get-key:kv:html-utils 'fee-rate' args)) dem))
    =/  change-address=@t  (need (get-key:kv:html-utils 'change-address' args))
    ;<  now=@da  bind:m  get-time:io
    =/  existing-draft  draft:ac
    =/  draft=transaction:drft
      ?~  existing-draft
        [~ ~ ~ `%random now now]
      u.existing-draft(modified now)
    ::  Set change config
    =/  new-change=change-config:drft  [fee-rate change-address]
    =.  change.draft  `new-change
    =/  updated=account-details:s  (set-draft:ac draft)
    =.  accounts.state  (~(put by accounts.state) account-pubkey updated)
    ;<  ~  bind:m  (replace:io !>(state))
    (send-sse-event:io /spv-wallet/stream/account/(crip (hexn:sailbox account-pubkey))/send ~ `%draft-outputs-update)
    ::
      %clear-change-config
    ~&  >>  "=== CLEAR CHANGE CONFIG HANDLER CALLED ==="
    =/  existing-draft  draft:ac
    ?~  existing-draft
      ~&  >>>  "no draft to clear change from"
      (pure:m ~)
    ;<  now=@da  bind:m  get-time:io
    =/  draft=transaction:drft  u.existing-draft(modified now)
    =.  change.draft  ~
    =/  updated=account-details:s  (set-draft:ac draft)
    =.  accounts.state  (~(put by accounts.state) account-pubkey updated)
    ;<  ~  bind:m  (replace:io !>(state))
    (send-sse-event:io /spv-wallet/stream/account/(crip (hexn:sailbox account-pubkey))/send ~ `%draft-outputs-update)
    ::
      %enable-auto-select
    =/  existing-draft  draft:ac
    ?~  existing-draft  (pure:m ~)
    =/  mode-text=@t  (fall (get-key:kv:html-utils 'mode' args) 'random')
    =/  mode=select-mode:drft
      ?:  =('largest-first' mode-text)  %largest-first
      %random
    ;<  now=@da  bind:m  get-time:io
    =/  draft=transaction:drft  u.existing-draft(auto-select `mode, modified now)
    =/  updated=account-details:s  (set-draft:ac draft)
    =.  accounts.state  (~(put by accounts.state) account-pubkey updated)
    ;<  ~  bind:m  (replace:io !>(state))
    (send-sse-event:io /spv-wallet/stream/account/(crip (hexn:sailbox account-pubkey))/send ~ `%draft-outputs-update)
    ::
      %disable-auto-select
    =/  existing-draft  draft:ac
    ?~  existing-draft  (pure:m ~)
    ;<  now=@da  bind:m  get-time:io
    =/  draft=transaction:drft  u.existing-draft(auto-select ~, modified now)
    =/  updated=account-details:s  (set-draft:ac draft)
    =.  accounts.state  (~(put by accounts.state) account-pubkey updated)
    ;<  ~  bind:m  (replace:io !>(state))
    (send-sse-event:io /spv-wallet/stream/account/(crip (hexn:sailbox account-pubkey))/send ~ `%draft-outputs-update)
    ::
    ::  Refresh auto-select (re-run selection with current mode)
    ::
      %refresh-auto-select
    ~&  >>  "=== REFRESH AUTO SELECT ==="
    =/  existing-draft  draft:ac
    ?~  existing-draft  (pure:m ~)
    ?~  auto-select.u.existing-draft  (pure:m ~)
    =/  mode=select-mode:drft  u.auto-select.u.existing-draft
    ~&  >>  "using mode: {<mode>}"
    =/  fee-rate=@ud
      ?~  change.u.existing-draft  1
      fee-rate.u.change.u.existing-draft
    ::  Build list of available UTXOs from cached per-address data
    =/  spend=spend:fees  script-type.u.details
    =/  cached  (collect-cached-utxos:wutxo u.details)
    ~&  >>  "cached utxos from collect-cached-utxos: {<(lent cached)>}"
    =/  utxos=(list utxo-input:drft)
      %+  murn  cached
      |=  [txid=@t vout=@ud value=@ud address=@t chain=@t index=@ud confirmations=(unit @ud)]
      ^-  (unit utxo-input:drft)
      ::  Check if frozen
      =/  label-key=@t  (crip "{(trip txid)}:{(scow %ud vout)}")
      ?:  (~(frozen la:bip329 labels.state) label-key)
        ~
      `[txid vout value spend]
    ~&  >>  "available utxos after frozen filter: {<(lent utxos)>}"
    ::  Calculate target (outputs + fee)
    =/  total-outputs=@ud  (sum-outputs:drft outputs.u.existing-draft)
    =/  vbytes=@ud  (calculate-vbytes:drft u.existing-draft)
    =/  fee=@ud  (calculate-fee:fees vbytes fee-rate)
    =/  target=@ud  (add total-outputs fee)
    ~&  >>  "target: {<target>} (outputs: {<total-outputs>} + fee: {<fee>})"
    ::  If target is 0, clear inputs
    ?:  =(0 target)
      ~&  >>  "refresh-auto-select: target is 0, clearing inputs"
      ;<  now=@da  bind:m  get-time:io
      =/  draft=transaction:drft  u.existing-draft(inputs ~, modified now)
      =/  updated=account-details:s  (set-draft:ac draft)
      =.  accounts.state  (~(put by accounts.state) account-pubkey updated)
      ;<  ~  bind:m  (replace:io !>(state))
      (send-sse-event:io /spv-wallet/stream/account/(crip (hexn:sailbox account-pubkey))/send ~ `%draft-outputs-update)
    ::  Select inputs based on mode
    ;<  eny=@uvJ  bind:m  get-entropy:io
    ::  Calculate output vbytes for selection
    =/  output-vbytes=@ud
      %+  add
        ::  Sum output vbytes
        %+  roll  outputs.u.existing-draft
        |=  [out=output:drft sum=@ud]
        (add sum (output-vbytes:fees (address-to-spend:drft address.out)))
      ::  Add change output vbytes if configured
      ?~  change.u.existing-draft  0
      (output-vbytes:fees (address-to-spend:drft address.u.change.u.existing-draft))
    ::  Convert to selectables, run selection, convert back
    =/  selectables=(list selectable:sel)
      (turn utxos |=(u=utxo-input:drft [txid.u vout.u amount.u spend.u]))
    =/  sel-result=(unit (list selectable:sel))
      ?-  mode
        %largest-first  (largest-first:sel selectables target output-vbytes fee-rate)
        %random         (random:sel selectables target output-vbytes fee-rate eny)
      ==
    =/  selected=(unit (list utxo-input:drft))
      ?~  sel-result  ~
      :-  ~
      %+  turn  u.sel-result
      |=  s=selectable:sel
      =/  match  (skim utxos |=(u=utxo-input:drft &(=(txid.u txid.s) =(vout.u vout.s))))
      ?>(?=(^ match) i.match)
    ?~  selected
      ~&  >>>  "refresh-auto-select failed: insufficient funds"
      (pure:m ~)
    ~&  >>  "refresh-auto-selected {<(lent u.selected)>} inputs"
    ;<  now=@da  bind:m  get-time:io
    =/  draft=transaction:drft  u.existing-draft(inputs u.selected, modified now)
    =/  updated=account-details:s  (set-draft:ac draft)
    =.  accounts.state  (~(put by accounts.state) account-pubkey updated)
    ;<  ~  bind:m  (replace:io !>(state))
    (send-sse-event:io /spv-wallet/stream/account/(crip (hexn:sailbox account-pubkey))/send ~ `%draft-outputs-update)
    ::
    ::  Set auto-select mode (just updates preference, doesn't run selection)
    ::  mode: 'disabled' | 'random' | 'largest-first'
    ::
      %set-auto-select-mode
    ~&  >>  "=== SET AUTO SELECT MODE ==="
    =/  mode-text=@t  (fall (get-key:kv:html-utils 'mode' args) 'random')
    ~&  >>  "mode: {<mode-text>}"
    =/  new-auto-select=(unit select-mode:drft)
      ?:  =('disabled' mode-text)  ~
      ?:  =('largest-first' mode-text)  `%largest-first
      `%random
    ;<  now=@da  bind:m  get-time:io
    =/  existing-draft  draft:ac
    =/  draft=transaction:drft
      ?~  existing-draft
        [~ ~ ~ new-auto-select now now]
      u.existing-draft(auto-select new-auto-select, modified now)
    =/  updated=account-details:s  (set-draft:ac draft)
    =.  accounts.state  (~(put by accounts.state) account-pubkey updated)
    ;<  ~  bind:m  (replace:io !>(state))
    (send-sse-event:io /spv-wallet/stream/account/(crip (hexn:sailbox account-pubkey))/send ~ `%draft-outputs-update)
    ::
    ::  Run auto-select using stored mode (or provided override)
    ::
      %run-auto-select
    ~&  >>  "=== RUN AUTO SELECT ==="
    ::  Get mode from draft (default random if not set)
    =/  existing-draft  draft:ac
    =/  mode=select-mode:drft
      ?~  existing-draft  %random
      (fall auto-select.u.existing-draft %random)
    ~&  >>  "using mode: {<mode>}"
    =/  fee-rate=@ud
      ?~  existing-draft  1
      ?~  change.u.existing-draft  1
      fee-rate.u.change.u.existing-draft
    ::  Build list of available UTXOs from cached per-address data
    =/  spend=spend:fees  script-type.u.details
    =/  cached  (collect-cached-utxos:wutxo u.details)
    ~&  >>  "cached utxos from collect-cached-utxos: {<(lent cached)>}"
    =/  utxos=(list utxo-input:drft)
      %+  murn  cached
      |=  [txid=@t vout=@ud value=@ud address=@t chain=@t index=@ud confirmations=(unit @ud)]
      ^-  (unit utxo-input:drft)
      ::  Check if frozen
      =/  label-key=@t  (crip "{(trip txid)}:{(scow %ud vout)}")
      ?:  (~(frozen la:bip329 labels.state) label-key)
        ~
      `[txid vout value spend]
    ~&  >>  "available utxos after frozen filter: {<(lent utxos)>}"
    ::  Get target amount (sum of outputs)
    =/  target=@ud
      ?~  existing-draft  0
      (sum-outputs:drft outputs.u.existing-draft)
    ::  If target is 0, clear inputs (nothing to fund)
    ?:  =(0 target)
      ~&  >>  "auto-select: target is 0, clearing inputs"
      ;<  now=@da  bind:m  get-time:io
      =/  draft=transaction:drft
        ?~  existing-draft
          [~ ~ ~ `mode now now]
        u.existing-draft(inputs ~, modified now)
      =/  updated=account-details:s  (set-draft:ac draft)
      =.  accounts.state  (~(put by accounts.state) account-pubkey updated)
      ;<  ~  bind:m  (replace:io !>(state))
      (send-sse-event:io /spv-wallet/stream/account/(crip (hexn:sailbox account-pubkey))/send ~ `%draft-outputs-update)
    ::  Select inputs based on mode
    ;<  eny=@uvJ  bind:m  get-entropy:io
    ::  Calculate output vbytes for selection
    =/  output-vbytes=@ud
      ?~  existing-draft  0
      %+  add
        ::  Sum output vbytes
        %+  roll  outputs.u.existing-draft
        |=  [out=output:drft sum=@ud]
        (add sum (output-vbytes:fees (address-to-spend:drft address.out)))
      ::  Add change output vbytes if configured
      ?~  change.u.existing-draft  0
      (output-vbytes:fees (address-to-spend:drft address.u.change.u.existing-draft))
    ::  Convert to selectables, run selection, convert back
    =/  selectables=(list selectable:sel)
      (turn utxos |=(u=utxo-input:drft [txid.u vout.u amount.u spend.u]))
    =/  sel-result=(unit (list selectable:sel))
      ?-  mode
        %largest-first  (largest-first:sel selectables target output-vbytes fee-rate)
        %random         (random:sel selectables target output-vbytes fee-rate eny)
      ==
    =/  selected=(unit (list utxo-input:drft))
      ?~  sel-result  ~
      :-  ~
      %+  turn  u.sel-result
      |=  s=selectable:sel
      =/  match  (skim utxos |=(u=utxo-input:drft &(=(txid.u txid.s) =(vout.u vout.s))))
      ?>(?=(^ match) i.match)
    ::  Handle selection result
    ?~  selected
      ~&  >>>  "auto-select failed: insufficient funds"
      (pure:m ~)
    ~&  >>  "auto-selected {<(lent u.selected)>} inputs"
    ::  Update draft with selected inputs
    ;<  now=@da  bind:m  get-time:io
    =/  draft=transaction:drft
      ?~  existing-draft
        [u.selected ~ ~ `mode now now]
      u.existing-draft(inputs u.selected, modified now)
    =/  updated=account-details:s  (set-draft:ac draft)
    =.  accounts.state  (~(put by accounts.state) account-pubkey updated)
    ;<  ~  bind:m  (replace:io !>(state))
    (send-sse-event:io /spv-wallet/stream/account/(crip (hexn:sailbox account-pubkey))/send ~ `%draft-outputs-update)
    ::
      %build-transaction
    ~&  >>  "=== BUILD AND BROADCAST TRANSACTION ==="
    ~&  >>  "extended-key type: {<-.extended-key.u.details>}"
    ::  Check if this account has an xprv (needed for signing)
    ?.  ?=([%xprv @] extended-key.u.details)
      ~&  >>>  "account has no private key - cannot sign"
      (pure:m ~)
    =/  xprv=@  +.extended-key.u.details
    ::  Check if draft exists
    =/  existing-draft  draft:ac
    ?~  existing-draft
      ~&  >>>  "no draft transaction"
      (pure:m ~)
    =/  draft-inputs=(list utxo-input:drft)  inputs.u.existing-draft
    =/  draft-outputs=(list output:drft)  outputs.u.existing-draft
    ?:  =(~ draft-inputs)
      ~&  >>>  "no inputs in draft"
      (pure:m ~)
    ?:  =(~ draft-outputs)
      ~&  >>>  "no outputs in draft"
      (pure:m ~)
    ::  Create account-level bip32 wallet from xprv (stored as base58 cord)
    =/  account-wallet  (from-extended:bip32 (trip xprv))
    ::  Build inputs for lib/tx/build - derive keys from xprv
    =/  tx-inputs=(list input:ap:tt)
      %+  turn  draft-inputs
      |=  in=utxo-input:drft
      =/  tx=(unit transaction:s)  (~(get by transactions:ac) txid.in)
      ?~  tx  ~|("tx not found: {<txid.in>}" !!)
      =/  output=(unit tx-output:s)  (snag-safe vout.in outputs.u.tx)
      ?~  output  ~|("output index {<vout.in>} not found" !!)
      =/  addr=@t  address.u.output
      ::  Look up address: first try address cache, then tapscript trees
      =/  addr-path=(unit address-suffix:hd-path)
        (~(get by address-cache:ac) addr)
      =/  [path=address-suffix:hd-path ts-merkle=(unit @ux)]
        ?^  addr-path  [u.addr-path ~]
        ::  Not in cache - search tapscript addresses in all leaves
        =/  recv-list=(list [@ud hd-leaf:s])
          (tap:((on @ud hd-leaf:s) gth) receiving:ac)
        =/  change-list=(list [@ud hd-leaf:s])
          (tap:((on @ud hd-leaf:s) gth) change:ac)
        =/  found=(unit [address-suffix:hd-path @ux])
          =|  result=(unit [address-suffix:hd-path @ux])
          =.  result
            |-
            ?~  recv-list  result
            ?^  result  result
            =/  [idx=@ud leaf=hd-leaf:s]  i.recv-list
            =/  td=(unit tapscript-details:s)  (~(get by script-trees.leaf) addr)
            ?.  ?=(^ td)
              $(recv-list t.recv-list)
            $(recv-list t.recv-list, result `[[[%.n 0] [%.n idx]] (hash:taproot ptst.u.td)])
          |-
          ?~  change-list  result
          ?^  result  result
          =/  [idx=@ud leaf=hd-leaf:s]  i.change-list
          =/  td=(unit tapscript-details:s)  (~(get by script-trees.leaf) addr)
          ?.  ?=(^ td)
            $(change-list t.change-list)
          $(change-list t.change-list, result `[[[%.n 1] [%.n idx]] (hash:taproot ptst.u.td)])
        ?~  found
          ~|("address not found in cache or tapscripts: {<addr>}" !!)
        [-.u.found `+.u.found]
      ::  Derive from account xprv using just change/index
      =/  [change=seg:hd-path index=seg:hd-path]  path
      =/  path-str=tape
        "m/{(format-seg change)}/{(format-seg index)}"
      =/  derived  (derive-path:account-wallet path-str)
      =/  privkey=@ux  prv.derived
      =/  pubkey=@ux  (ser-p:derived pub.derived)
      =/  txid-display=@ux  (rash txid.in hex)
      =/  txid=@ux  dat:(flip:byt:bcu [32 txid-display])
      ::  Convert simple spend to full spend-type for transaction builder
      ::  For tapscript addresses, include merkle root for key-path tweak
      =/  spend=spend-type:tt
        ?-  spend.in
          %p2pkh        [%p2pkh ~]
          %p2sh-p2wpkh  [%p2sh-p2wpkh ~]
          %p2wpkh       [%p2wpkh ~]
          %p2tr         [%p2tr %key-path ts-merkle]
        ==
      [privkey pubkey txid vout.in amount.in `@ud`0xffff.ffff spend]
    ::  Build outputs (including change if configured)
    =/  tx-outputs=(list output:ap:tt)
      (incorporate-change:drft u.existing-draft)
    ::  Build and sign transaction
    ~&  >>  "Building transaction with {<(lent tx-inputs)>} inputs and {<(lent tx-outputs)>} outputs"
    =/  tx-hex=tape  (build-transaction:txns active-network.u.details 2 tx-inputs tx-outputs 0)
    =/  tx-hex-cord=@t  (crip tx-hex)
    ~&  >>  "Transaction hex: {<tx-hex-cord>}"
    ::  Broadcast transaction - use correct network
    =/  broadcast-url=@t
      ?-  active-network.u.details
        %main      'https://mempool.space/api/tx'
        %testnet3  'https://mempool.space/testnet/api/tx'
        %testnet4  'https://mempool.space/testnet4/api/tx'
        %signet    'https://mempool.space/signet/api/tx'
        %regtest   'http://localhost:3000/tx'
      ==
    =/  =request:http
      :*  %'POST'
          broadcast-url
          ~[['content-type' 'text/plain']]
          `(as-octs:mimes:html tx-hex-cord)
      ==
    ;<  ~                      bind:m  (send-request:io request)
    ;<  =client-response:iris  bind:m  take-client-response:io
    =/  broadcast-result=cord
      ?+  client-response  'broadcast-failed'
        [%finished * [~ [* [p=@ q=@]]]]
      q.data.u.full-file.client-response
      ==
    ~&  >>  "Broadcast result: {<broadcast-result>}"
    ::  Clear draft on success
    =/  updated=account-details:s  clear-draft:ac
    =.  accounts.state  (~(put by accounts.state) account-pubkey updated)
    ;<  ~  bind:m  (replace:io !>(state))
    (pure:m ~)
  ==
--
