/-  s=spv-wallet, urb
/+  io=sailboxio, html-utils, seed-phrases, sailbox, json-utils, server,
    *wallet-address, *wallet-account, *wallet-discovery,
    ui=ui-spv-wallet, gw=ui-groundwire
|%
::  Helper: Infer script-type from extended key prefix
++  prefix-to-script-type
  |=  prefix=tape
  ^-  script-type
  ?:  ?|  =("zpub" prefix)
          =("vpub" prefix)
          =("zprv" prefix)
          =("vprv" prefix)
      ==
    %p2wpkh
  ?:  ?|  =("ypub" prefix)
          =("upub" prefix)
          =("yprv" prefix)
          =("uprv" prefix)
      ==
    %p2sh-p2wpkh
  %p2wpkh  :: default to native segwit for generic xpub/tpub/xprv/tprv
::
::  Handle wallet management actions (add, remove, generate)
::
++  handle-wallet-actions
  |=  args=key-value-list:kv:html-utils
  =/  m  (fiber:io ,~)
  ^-  form:m
  =/  action=@t  (need (get-key:kv:html-utils 'action' args))
  ?+  action  !!
      %add-wallet
    =/  wallet-name=@t  (need (get-key:kv:html-utils 'wallet-name' args))
    =/  seed-phrase=@t  (need (get-key:kv:html-utils 'seed-phrase' args))
    =/  seed-format=@t  (fall (get-key:kv:html-utils 'seed-format' args) 'bip39')
    =/  =seed:s
      ?:  =(seed-format 'q')
        ::  Parse @q format
        =/  parsed=(unit @q)  (slaw %q seed-phrase)
        ?~  parsed
          ~|(%invalid-q-format !!)
        [%q u.parsed]
      ::  BIP39 format
      =/  val=(unit ?)
        (mole |.((validate-seed-phrase:seed-phrases seed-phrase)))
      ?.  ?=([~ %&] val)
        ~|(%invalid-bip39-format !!)
      [%t seed-phrase]
    =/  pubkey=@ux  (seed-to-pubkey seed)
    =/  new-wallet=wallet:s  [wallet-name seed pubkey ~ ~]
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    =.  wallets.state  (~(put by wallets.state) pubkey new-wallet)
    ;<  ~  bind:m  (replace:io !>(state))
    (send-sse-event:io /spv-wallet/stream ~ `'wallet-list-update')
      %add-wallet-from-entropy
    =/  wallet-name=@t  (need (get-key:kv:html-utils 'wallet-name' args))
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    ;<  eny=@uvJ  bind:m  get-entropy:io
    =/  existing-seeds=(set cord)
      %-  ~(gas in *(set cord))
      %+  murn  ~(tap by wallets.state)
      |=  [k=@ux v=wallet:s]
      ?.  ?=(%t -.seed.v)  ~
      `t.seed.v
    =/  generated-seed=cord  (gen-unique:seed-phrases eny %256 existing-seeds)
    =/  pubkey=@ux  (seed-to-pubkey [%t generated-seed])
    =/  new-wallet=wallet:s  [wallet-name [%t generated-seed] pubkey ~ ~]
    =.  wallets.state  (~(put by wallets.state) pubkey new-wallet)
    ;<  ~  bind:m  (replace:io !>(state))
    %-  send-raw-cards:io
    :~  [%sse /spv-wallet/stream ~ ~ `'generate-clear']
        [%sse /spv-wallet/stream ~ ~ `'wallet-list-update']
    ==
      %remove-wallet
    =/  pubkey-to-remove=@ux  (rash (need (get-key:kv:html-utils 'pubkey' args)) hex)
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    ::  If this is the boot wallet, clear boot state
    =/  removed=(unit wallet:s)  (~(get by wallets.state) pubkey-to-remove)
    =?  boot.state  ?&  ?=(^ removed)
                        ?=(%q -.seed.u.removed)
                        ?=(^ boot.state)
                        =(q.seed.u.removed boot-secret.data.u.boot.state)
                    ==
      ~
    =.  wallets.state  (~(del by wallets.state) pubkey-to-remove)
    ;<  ~  bind:m  (replace:io !>(state))
    (send-sse-event:io /spv-wallet/stream ~ `'wallet-list-update')
    ::
      %toggle-empty-addresses
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    =.  hide-empty-addresses.state  !hide-empty-addresses.state
    (replace:io !>(state))
    ::
      %add-watch-only
    =/  account-name=@t  (need (get-key:kv:html-utils 'account-name' args))
    =/  xpub=@t  (need (get-key:kv:html-utils 'xpub' args))
    =/  xpub-tape=tape  (trip xpub)
    ::  Check prefix is xpub/tpub
    ?.  ?|  =((scag 4 xpub-tape) "xpub")
            =((scag 4 xpub-tape) "tpub")
        ==
      ~|  "Must be an extended public key (xpub/tpub)"  !!
    ::  Validate xpub and extract pubkey
    =/  account  (from-extended:bip32 xpub-tape)
    =/  pubkey=@ux  public-key:account
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    ::  Get script-type and network from form
    =/  script-type-str=@t  (need (get-key:kv:html-utils 'script-type' args))
    =/  network-str=@t  (need (get-key:kv:html-utils 'network' args))
    ::  Parse script-type
    =/  =script-type
      ?+  script-type-str  %p2wpkh
        %p2pkh         %p2pkh
        %p2sh-p2wpkh   %p2sh-p2wpkh
        %p2wpkh        %p2wpkh
        %p2tr          %p2tr
      ==
    ::  Parse network
    =/  network=network
      ?+  network-str  %testnet3
        %main      %main
        %testnet3  %testnet3
        %testnet4  %testnet4
        %signet    %signet
        %regtest   %regtest
      ==
    ::  Create new account-details with the given name and xpub
    =/  new-account=account-details
      :*  account-name           :: name
          ~                      :: wallet (standalone, no parent wallet)
          [%xpub xpub]           :: extended-key
          script-type            :: script-type
          network                :: network
          ~                      :: networks (empty map)
          [~ ~ ~ ~ ~]            :: proc
          %.n                    :: indexer-registered
      ==
    ::  Add to global accounts map and watch-only set
    =.  accounts.state  (~(put by accounts.state) pubkey new-account)
    =.  watch-only.state  (~(put in watch-only.state) pubkey)
    ;<  ~  bind:m  (replace:io !>(state))
    (send-sse-event:io /spv-wallet/stream ~ `'wallet-list-update')
    ::
      %add-signing
    =/  account-name=@t  (need (get-key:kv:html-utils 'account-name' args))
    =/  xprv=@t  (need (get-key:kv:html-utils 'xprv' args))
    =/  xprv-tape=tape  (trip xprv)
    ::  Check prefix is xprv/tprv
    ?.  ?|  =((scag 4 xprv-tape) "xprv")
            =((scag 4 xprv-tape) "tprv")
        ==
      ~|  "Must be an extended private key (xprv/tprv)"  !!
    ::  Validate xprv and extract pubkey
    =/  account  (from-extended:bip32 xprv-tape)
    =/  pubkey=@ux  public-key:account
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    ::  Get script-type and network from form
    =/  script-type-str=@t  (need (get-key:kv:html-utils 'script-type' args))
    =/  network-str=@t  (need (get-key:kv:html-utils 'network' args))
    ::  Parse script-type
    =/  =script-type
      ?+  script-type-str  %p2wpkh
        %p2pkh         %p2pkh
        %p2sh-p2wpkh   %p2sh-p2wpkh
        %p2wpkh        %p2wpkh
        %p2tr          %p2tr
      ==
    ::  Parse network
    =/  network=network
      ?+  network-str  %testnet3
        %main      %main
        %testnet3  %testnet3
        %testnet4  %testnet4
        %signet    %signet
        %regtest   %regtest
      ==
    ::  Create new account-details with the given name and xprv
    =/  new-account=account-details
      :*  account-name           :: name
          ~                      :: wallet (standalone, no parent wallet)
          [%xprv xprv]           :: extended-key
          script-type            :: script-type
          network                :: network
          ~                      :: networks (empty map)
          [~ ~ ~ ~ ~]            :: proc
          %.n                    :: indexer-registered
      ==
    ::  Add to global accounts map and signing set
    =.  accounts.state  (~(put by accounts.state) pubkey new-account)
    =.  signing.state  (~(put in signing.state) pubkey)
    ;<  ~  bind:m  (replace:io !>(state))
    (send-sse-event:io /spv-wallet/stream ~ `'wallet-list-update')
    ::
      %delete-watch-only
    =/  pubkey=@ux  (rash (need (get-key:kv:html-utils 'pubkey' args)) hex)
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    ::  Remove from watch-only set and global accounts map
    =.  watch-only.state  (~(del in watch-only.state) pubkey)
    =.  accounts.state  (~(del by accounts.state) pubkey)
    ;<  ~  bind:m  (replace:io !>(state))
    (send-sse-event:io /spv-wallet/stream ~ `'wallet-list-update')
    ::
      %delete-signing
    =/  pubkey=@ux  (rash (need (get-key:kv:html-utils 'pubkey' args)) hex)
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    ::  Remove from signing set and global accounts map
    =.  signing.state  (~(del in signing.state) pubkey)
    =.  accounts.state  (~(del by accounts.state) pubkey)
    ;<  ~  bind:m  (replace:io !>(state))
    (send-sse-event:io /spv-wallet/stream ~ `'wallet-list-update')
  ==
::  Handle wallet discovery and account management actions
::
++  handle-wallet-discovery-actions
  |=  [pubkey=@ux args=key-value-list:kv:html-utils]
  =/  m  (fiber:io ,~)
  ^-  form:m
  =/  action=@t  (need (get-key:kv:html-utils 'action' args))
  ?+  action  !!
      %delete-account
    ::  Get account path from form
    =/  path-str=@t  (need (get-key:kv:html-utils 'account-path' args))
    ::  Get state and wallet
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    =/  wallet=(unit wallet:s)  (~(get by wallets.state) pubkey)
    ?~  wallet
      ~|  "wallet not found"  !!
    ::  Filter out the account with matching path
    =/  new-accounts=(map account:hd-path @ux)
      %-  ~(gas by *(map account:hd-path @ux))
      %+  skip  ~(tap by accounts.u.wallet)
      |=  [acct=account:hd-path account-pubkey=@ux]
      =(path-str (crip (format-account-path acct)))
    ::  Update wallet and state
    =/  updated-wallet=wallet:s
      u.wallet(accounts new-accounts)
    =.  wallets.state  (~(put by wallets.state) pubkey updated-wallet)
    ;<  ~  bind:m  (replace:io !>(state))
    ;<  ~  bind:m  (send-sse-event:io /spv-wallet/stream/wallet/(crip (hexn:sailbox pubkey)) ~ `'account-list-update')
    (pure:m ~)
    ::
      %add-unlisted-account
    ::  Parse form inputs
    =/  account-name=@t  (need (get-key:kv:html-utils 'account-name' args))
    =/  purpose-select=@t  (need (get-key:kv:html-utils 'purpose-select' args))
    =/  coin-type-select=@t  (need (get-key:kv:html-utils 'coin-type-select' args))
    =/  account-number=@t  (need (get-key:kv:html-utils 'account-number' args))
    ::  Determine purpose value (handle custom)
    =/  purpose-val=@ud
      ?:  =(purpose-select 'custom')
        (rash (need (get-key:kv:html-utils 'purpose-custom' args)) dem)
      (rash purpose-select dem)
    ::  Determine coin-type value (handle custom)
    =/  coin-type-val=@ud
      ?:  =(coin-type-select 'custom')
        (rash (need (get-key:kv:html-utils 'coin-type-custom' args)) dem)
      (rash coin-type-select dem)
    ::  Parse account number
    =/  account-val=@ud  (rash account-number dem)
    ::  Validate BIP32 constraints (0 to 2^31-1)
    ?.  (lte purpose-val 2.147.483.647)
      ~|  "purpose must be 0-2147483647"  !!
    ?.  (lte coin-type-val 2.147.483.647)
      ~|  "coin-type must be 0-2147483647"  !!
    ?.  (lte account-val 2.147.483.647)
      ~|  "account must be 0-2147483647"  !!
    ::  Construct account path (all hardened)
    =/  new-account=account:hd-path
      [[%.y purpose-val] [%.y coin-type-val] [%.y account-val]]
    ::  Get state and add account
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    =/  wallet=(unit wallet:s)  (~(get by wallets.state) pubkey)
    ?~  wallet
      ~|  "wallet not found"  !!
    ::  Add account to wallet
    =/  script-type  (purpose-to-script-type purpose-val)
    =/  network  (coin-type-to-network coin-type-val)
    ::  Derive account pubkey and xpub from wallet seed
    =/  master-wallet  (from-seed:bip32 (seed-to-bytes seed.u.wallet))
    =/  path=tape
      %+  weld  "m/"
      %+  weld  (scow %ud q.purpose.new-account)
      %+  weld  "'/"
      %+  weld  (scow %ud q.coin-type.new-account)
      %+  weld  "'/"
      %+  weld  (scow %ud q.account.new-account)
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
    =/  new-account-details=account-details
      :*  account-name           :: name
          `pubkey                :: wallet (reference to parent wallet)
          [%xprv xprv]           :: extended-key
          script-type            :: script-type
          network                :: network
          ~                      :: networks (empty map)
          [~ ~ ~ ~ ~]            :: proc
          %.n                    :: indexer-registered
      ==
    ::  Add to global accounts map and wallet
    =.  accounts.state  (~(put by accounts.state) account-pubkey new-account-details)
    =/  updated-wallet=wallet:s
      u.wallet(accounts (~(put by accounts.u.wallet) new-account account-pubkey))
    =.  wallets.state  (~(put by wallets.state) pubkey updated-wallet)
    ;<  ~  bind:m  (replace:io !>(state))
    ;<  ~  bind:m  (send-sse-event:io /spv-wallet/stream/wallet/(crip (hexn:sailbox pubkey)) ~ `'account-list-update')
    ;<  ~  bind:m  (sleep:io ~s1)
    (pure:m ~)
    ::
      %discover-accounts
    ::  Parse form inputs
    =/  purpose-select=@t  (need (get-key:kv:html-utils 'purpose' args))
    =/  coin-type-select=@t  (need (get-key:kv:html-utils 'coin-type' args))
    ::  Determine purpose value (handle custom)
    =/  purpose-val=@ud
      ?:  =(purpose-select 'custom')
        (rash (need (get-key:kv:html-utils 'purpose-custom' args)) dem)
      (rash purpose-select dem)
    ::  Determine coin-type value (handle custom)
    =/  coin-type-val=@ud
      ?:  =(coin-type-select 'custom')
        (rash (need (get-key:kv:html-utils 'coin-type-custom' args)) dem)
      (rash coin-type-select dem)
    ::  Validate BIP32 constraints (0 to 2^31-1)
    ?.  (lte purpose-val 2.147.483.647)
      ~|  "purpose must be 0-2147483647"  !!
    ?.  (lte coin-type-val 2.147.483.647)
      ~|  "coin-type must be 0-2147483647"  !!
    ::  Run account discovery
    (run-account-discovery pubkey purpose-val coin-type-val)
    ::
      %pause-discovery
    ::  Parse scan key (format: "purpose-cointype")
    =/  scan-key-str=@t  (need (get-key:kv:html-utils 'scan-key' args))
    =/  parts=(list @ud)  (scan (trip scan-key-str) (star ;~(pfix (punt hep) dem)))
    ?>  =((lent parts) 2)
    =/  purpose-val=@ud  (snag 0 parts)
    =/  coin-type-val=@ud  (snag 1 parts)
    =/  scan-key=coin-type:hd-path  [[%.y purpose-val] [%.y coin-type-val]]
    ::  Get state and wallet
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    =/  wallet=(unit wallet:s)  (~(get by wallets.state) pubkey)
    ?~  wallet
      ~|  "wallet not found"  !!
    ::  Get scan entry
    =/  scan-entry=(unit [pid=@ta act=? idx=@ud scn=account-scan])
      (~(get by scan.u.wallet) scan-key)
    ?~  scan-entry
      ~|  "scan not found"  !!
    =/  [scan-pid=@ta scan-act=? scan-idx=@ud scan-scn=account-scan]  u.scan-entry
    ::  Guard: only pause if active
    ?.  scan-act
      ~|  "discovery already paused"  !!
    ::  Kill the fiber
    ;<  ~  bind:m  (fiber-kill:io scan-pid)
    ::  Update scan entry with act=%.n, keep same PID and state
    =/  updated-wallet=wallet:s
      u.wallet(scan (~(put by scan.u.wallet) scan-key [scan-pid %.n scan-idx scan-scn]))
    =.  wallets.state  (~(put by wallets.state) pubkey updated-wallet)
    ;<  ~  bind:m  (replace:io !>(state))
    ;<  ~  bind:m  (send-sse-event:io /spv-wallet/stream/wallet/(crip (hexn:sailbox pubkey)) ~ `'discovery-status-update')
    (pure:m ~)
    ::
      %resume-discovery
    ::  Parse scan key (format: "purpose-cointype")
    =/  scan-key-str=@t  (need (get-key:kv:html-utils 'scan-key' args))
    =/  parts=(list @ud)  (scan (trip scan-key-str) (star ;~(pfix (punt hep) dem)))
    ?>  =((lent parts) 2)
    =/  purpose-val=@ud  (snag 0 parts)
    =/  coin-type-val=@ud  (snag 1 parts)
    =/  scan-key=coin-type:hd-path  [[%.y purpose-val] [%.y coin-type-val]]
    ::  Get state and wallet
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    =/  wallet=(unit wallet:s)  (~(get by wallets.state) pubkey)
    ?~  wallet
      ~|  "wallet not found"  !!
    ::  Get scan entry
    =/  scan-entry=(unit [pid=@ta act=? idx=@ud scn=account-scan])
      (~(get by scan.u.wallet) scan-key)
    ?~  scan-entry
      ~|  "no discovery to resume"  !!
    =/  [scan-pid=@ta scan-act=? scan-idx=@ud scan-scn=account-scan]  u.scan-entry
    ::  Guard: only resume if paused
    ?:  scan-act
      ~|  "discovery already active"  !!
    ::  Get new PID
    ;<  new-pid=@ta  bind:m  get-pid:io
    ::  Update scan entry with new PID and act=%.y
    =/  updated-wallet=wallet:s
      u.wallet(scan (~(put by scan.u.wallet) scan-key [new-pid %.y scan-idx scan-scn]))
    =.  wallets.state  (~(put by wallets.state) pubkey updated-wallet)
    ;<  ~  bind:m  (replace:io !>(state))
    ;<  ~  bind:m  (send-sse-event:io /spv-wallet/stream/wallet/(crip (hexn:sailbox pubkey)) ~ `'discovery-status-update')
    ::  Resume discovery from saved state
    (resume-account-discovery pubkey purpose-val coin-type-val scan-idx)
    ::
      %cancel-discovery
    ::  Parse scan key (format: "purpose-cointype")
    =/  scan-key-str=@t  (need (get-key:kv:html-utils 'scan-key' args))
    =/  parts=(list @ud)  (scan (trip scan-key-str) (star ;~(pfix (punt hep) dem)))
    ?>  =((lent parts) 2)
    =/  purpose-val=@ud  (snag 0 parts)
    =/  coin-type-val=@ud  (snag 1 parts)
    =/  scan-key=coin-type:hd-path  [[%.y purpose-val] [%.y coin-type-val]]
    ::  Get state and wallet
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    =/  wallet=(unit wallet:s)  (~(get by wallets.state) pubkey)
    ?~  wallet
      ~|  "wallet not found"  !!
    ::  Get scan entry to find PID
    =/  scan-entry=(unit [pid=@ta act=? idx=@ud scn=account-scan])
      (~(get by scan.u.wallet) scan-key)
    ?~  scan-entry
      ~|  "scan not found"  !!
    ::  Kill the process
    ;<  ~  bind:m  (fiber-kill:io pid.u.scan-entry)
    ::  Remove scan entry from wallet
    =/  updated-wallet=wallet:s
      u.wallet(scan (~(del by scan.u.wallet) scan-key))
    =.  wallets.state  (~(put by wallets.state) pubkey updated-wallet)
    ;<  ~  bind:m  (replace:io !>(state))
    ;<  ~  bind:m  (send-sse-event:io /spv-wallet/stream/wallet/(crip (hexn:sailbox pubkey)) ~ `'discovery-status-update')
    (pure:m ~)
  ==
::
++  do-get
  |=  $:  =bowl:gall
          state=vase
          =header-list:http
          [ext=(unit @ta) site=(list @t)]
          args=(list [key=@t value=@t])
      ==
  ^-  simple-payload:http
  ?.  =(our src):bowl
    (login-redirect:sailbox [ext site] args)
  =+  !<(state-0:s state)
  ::  Check urb-watcher for our point to determine spawn status
  ::  %spawned: confirmed on chain (in urb-watcher)
  ::  %pending: boot done but not yet confirmed (in mempool)
  ::  %unspawned: not spawned yet
  =/  spawn-status=?(%spawned %pending %unspawned)
    =/  in-watcher=?
      ?.  .^(? %gu /(scot %p our.bowl)/urb-watcher/(scot %da now.bowl)/$)
        %.n
      =/  points=(map @p point:urb)
        .^((map @p point:urb) %gx /(scot %p our.bowl)/urb-watcher/(scot %da now.bowl)/points/urb-points)
      (~(has by points) our.bowl)
    ?:  in-watcher  %spawned
    ?:  ?&(?=(^ boot) =(%done step.u.boot))  %pending
    %unspawned
  ::  Handle address data endpoint (returns simple-payload:http)
  ?:  ?=([%spv-wallet %wallet @ %account @ %address @ %data ~] site)
    =/  pubkey=@ux  (rash i.t.site hex)
    =/  account-path=@t  i.t.t.t.t.site
    =/  index=@ud  (rash i.t.t.t.t.t.t.site dem)
    =/  chain=@t  (need (get-key:kv:html-utils 'chain' args))
    (handle-address-data-get:ui pubkey account-path index chain wallets accounts)
  ::  All other routes use standard mime-response
  %-  mime-response:sailbox
  ?+    site  ~|(%unrecognized-get !!)
      [%spv-wallet ~]
    [/text/html (manx-to-octs:server (wallet-page:ui wallets watch-only signing accounts spawn-status boot args))]
      [%spv-wallet %spv ~]
    ::  Network selection list page
    =/  spv-state=state-0:s  !<(state-0:s state)
    [/text/html (manx-to-octs:server (spv-list-page:ui spv-state))]
      [%spv-wallet %groundwire ~]
    ::  Groundwire management page - ordinal comet identities
    =/  gw-state=state-0:s  !<(state-0:s state)
    [/text/html (manx-to-octs:server (groundwire-page:gw gw-state))]
      [%spv-wallet %spv @ ~]
    =/  spv-state=state-0:s  !<(state-0:s state)
    ::  Parse network from path
    =/  net-str=@t  i.t.t.site
    =/  net=network
      ?+  net-str  %testnet3
        %main      %main
        %testnet3  %testnet3
        %testnet4  %testnet4
        %signet    %signet
        %regtest   %regtest
      ==
    ::  Parse page parameter (default to 0 if not provided)
    =/  page=@ud
      =/  page-arg=(unit @t)  (get-key:kv:html-utils 'page' args)
      ?~  page-arg  0
      (rash u.page-arg dem)
    [/text/html (manx-to-octs:server (spv-page:ui spv-state page net))]
      [%spv-wallet %spv %header @ ~]
    =/  hash=@uvI  (rash i.t.t.t.site hex)
    =/  spv-state=state-0:s  !<(state-0:s state)
    [/text/html (manx-to-octs:server (block-header-detail-page:ui hash spv.spv-state))]
      [%spv-wallet %timer ~]
    [/text/html (manx-to-octs:server timer-page:ui)]
      [%spv-wallet %wallet @ ~]
    [/text/html (manx-to-octs:server (wallet-detail-page:ui (rash i.t.t.site hex) wallets accounts))]
      [%spv-wallet %account @ ~]
    ::  Unified account detail page - works for wallet accounts, watch-only, and signing
    =/  pubkey=@ux  (rash i.t.t.site hex)
    =/  account=(unit account-details)  (~(get by accounts) pubkey)
    ?~  account  ~|(%account-not-found !!)
    =/  details=account-details  u.account
    =/  pubkey-hex=@t  (crip (hexn:sailbox pubkey))
    ::  Check if this account belongs to a wallet
    =/  wallet-info=(unit [wallet-pubkey=@ux wallet-name=@t acct=account:hd-path])
      %-  ~(rep by wallets)
      |=  [[wallet-pubkey=@ux wal=wallet] result=(unit [wallet-pubkey=@ux wallet-name=@t acct=account:hd-path])]
      ?^  result  result
      ::  Check if this pubkey is in the wallet's accounts
      =/  found-account=(unit account:hd-path)
        %-  ~(rep by accounts.wal)
        |=  [[acct=account:hd-path account-pubkey=@ux] res=(unit account:hd-path)]
        ?^  res  res
        ?:  =(account-pubkey pubkey)  `acct
        ~
      ?~  found-account  ~
      `[wallet-pubkey name.wal u.found-account]
    ::  Render page with wallet info if found, otherwise standalone account
    ?^  wallet-info
      [/text/html (manx-to-octs:server (account-detail-page:ui pubkey pubkey-hex `wallet-name.u.wallet-info `acct.u.wallet-info details now.bowl hide-empty-addresses))]
    [/text/html (manx-to-octs:server (account-detail-page:ui pubkey pubkey-hex ~ ~ details now.bowl hide-empty-addresses))]
      [%spv-wallet %account @ %address @ @ ~]
    =/  pubkey=@ux  (rash i.t.t.site hex)
    =/  chain=@t  i.t.t.t.t.site
    =/  index=@ud  (rash i.t.t.t.t.t.site dem)
    [/text/html (manx-to-octs:server (address-detail-page:ui pubkey (crip (hexn:sailbox pubkey)) index chain wallets accounts indexer-subs now.bowl))]
      [%spv-wallet %account @ %tapscript @ @ @ ~]
    =/  pubkey=@ux  (rash i.t.t.site hex)
    =/  chain=@t  i.t.t.t.t.site
    =/  index=@ud  (rash i.t.t.t.t.t.site dem)
    =/  tapscript-addr=@t  i.t.t.t.t.t.t.site
    [/text/html (manx-to-octs:server (tapscript-detail-page:ui pubkey (crip (hexn:sailbox pubkey)) chain index tapscript-addr wallets accounts now.bowl))]
      [%spv-wallet %account @ %tx @ ~]
    =/  pubkey=@ux  (rash i.t.t.site hex)
    =/  txid=@t  i.t.t.t.t.site
    [/text/html (manx-to-octs:server (transaction-detail-page:ui pubkey (crip (hexn:sailbox pubkey)) txid wallets accounts))]
      [%spv-wallet %account @ %send ~]
    =/  pubkey=@ux  (rash i.t.t.site hex)
    [/text/html (manx-to-octs:server (send-page:ui pubkey wallets accounts labels))]
  ==
--
