/-  *spv-wallet
/+  dbug, sailbox, io=sailboxio, server, multipart,
    ui=ui-spv-wallet, html-utils, json-utils,
    bip39, bip32, btc=bitcoin, bip329,
    wallet-address, wallet-account, *wallet-mempool-space,
    rt-wallet, rt-account, rt-send, rt-spv, rt-boot, taproot
/=  t-  /tests/lib/bitcoin-spv
/=  t-  /tests/lib/seed-phrases
/=  t-  /tests/lib/transactions
/=  t-  /tests/lib/taproot
/=  m-  /mar/sponsorship-request
/=  m-  /mar/sponsorship-response
=>
  |%
  ++  kv    kv:html-utils
  +$  card  card:sailbox
  --
^-  agent:gall
%-  agent:dbug
%-  agent:sailbox
^-  sailbox:sailbox
|%
++  initial
  ^-  vase
  =|  state=state-0
  =.  binding.state  [~ /spv-wallet]
  !>(state)
  ::  ::
  ::  ::  Seeds for full wallets
  ::  ::
  ::  =/  main-seed=@t
  ::    'abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about'
  ::  =/  cold-seed=@t
  ::    'zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo wrong'
  ::  =/  fauceted-seed=@t
  ::    'injury idea term fox crop movie type critic hello inquiry lottery agree'
  ::  ::
  ::  ::  Seeds for standalone accounts (not stored, just for derivation)
  ::  ::
  ::  =/  watch-seed=@t
  ::    'legal winner thank year wave sausage worth useful legal winner thank yellow'
  ::  =/  sign-seed=@t
  ::    'letter advice cage absurd amount doctor acoustic avoid letter advice cage above'
  ::  ::
  ::  ::  Derive master keys from seeds
  ::  ::
  ::  =/  main-master  (from-seed:bip32 64^(to-seed:bip39 (trip main-seed) ""))
  ::  =/  cold-master  (from-seed:bip32 64^(to-seed:bip39 (trip cold-seed) ""))
  ::  =/  fauceted-master  (from-seed:bip32 64^(to-seed:bip39 (trip fauceted-seed) ""))
  ::  =/  watch-master  (from-seed:bip32 64^(to-seed:bip39 (trip watch-seed) ""))
  ::  =/  sign-master  (from-seed:bip32 64^(to-seed:bip39 (trip sign-seed) ""))
  ::  ::
  ::  ::  Wallet pubkeys (fingerprints)
  ::  ::
  ::  =/  main-wallet-pubkey=@ux  (seed-to-pubkey:wallet-address [%t main-seed])
  ::  =/  cold-wallet-pubkey=@ux  (seed-to-pubkey:wallet-address [%t cold-seed])
  ::  =/  fauceted-wallet-pubkey=@ux  (seed-to-pubkey:wallet-address [%t fauceted-seed])
  ::  ::
  ::  ::  Derive account keys - Main Wallet accounts
  ::  ::
  ::  =/  main-acct-0  (derive-path:main-master "m/84'/0'/0'")
  ::  =/  main-acct-0-pubkey=@ux  public-key:main-acct-0
  ::  =/  main-acct-0-xprv=@t  (crip (prv-extended:main-acct-0 %main))
  ::  ::
  ::  =/  main-acct-1  (derive-path:main-master "m/84'/0'/1'")
  ::  =/  main-acct-1-pubkey=@ux  public-key:main-acct-1
  ::  =/  main-acct-1-xprv=@t  (crip (prv-extended:main-acct-1 %main))
  ::  ::
  ::  =/  main-acct-testnet  (derive-path:main-master "m/84'/1'/0'")
  ::  =/  main-acct-testnet-pubkey=@ux  public-key:main-acct-testnet
  ::  =/  main-acct-testnet-xprv=@t  (crip (prv-extended:main-acct-testnet %testnet))
  ::  ::
  ::  ::  Derive account keys - Taproot Test account (m/86'/1'/0')
  ::  ::
  ::  =/  main-acct-taproot  (derive-path:main-master "m/86'/1'/0'")
  ::  =/  main-acct-taproot-pubkey=@ux  public-key:main-acct-taproot
  ::  =/  main-acct-taproot-xprv=@t  (crip (prv-extended:main-acct-taproot %testnet))
  ::  ::  Derive receive addresses 0 and 1 for taproot account
  ::  =/  taproot-recv-0  (derive-path:main-acct-taproot "m/0/0")
  ::  =/  taproot-recv-0-pubkey=@ux  public-key:taproot-recv-0
  ::  =/  taproot-recv-1  (derive-path:main-acct-taproot "m/0/1")
  ::  =/  taproot-recv-1-pubkey=@ux  public-key:taproot-recv-1
  ::  ::  Generate main taproot addresses (key-path spend only)
  ::  =/  taproot-addr-0=@t  (need (encode-taproot:bech32:btc %testnet [32 (x-only:taproot taproot-recv-0-pubkey)]))
  ::  =/  taproot-addr-1=@t  (need (encode-taproot:bech32:btc %testnet [32 (x-only:taproot taproot-recv-1-pubkey)]))
  ::  ::  Create checksig script trees using the same pubkey
  ::  =/  script-0-a=hexb:btc  (checksig-script:taproot taproot-recv-0-pubkey)
  ::  =/  script-0-b=hexb:btc  (csv-checksig-script:taproot 144 taproot-recv-0-pubkey)
  ::  =/  script-1-a=hexb:btc  (checksig-script:taproot taproot-recv-1-pubkey)
  ::  =/  script-1-b=hexb:btc  (csv-checksig-script:taproot 144 taproot-recv-1-pubkey)
  ::  ::  Build script trees (single leaf each for simplicity)
  ::  =/  tree-0-a=ptst:taproot  [%leaf 0xc0 script-0-a]
  ::  =/  tree-0-b=ptst:taproot  [%leaf 0xc0 script-0-b]
  ::  =/  tree-1-a=ptst:taproot  [%leaf 0xc0 script-1-a]
  ::  =/  tree-1-b=ptst:taproot  [%leaf 0xc0 script-1-b]
  ::  ::  Generate tapscript addresses
  ::  =/  tapscript-addr-0-a=@t  (tapscript-address:taproot taproot-recv-0-pubkey tree-0-a %testnet)
  ::  =/  tapscript-addr-0-b=@t  (tapscript-address:taproot taproot-recv-0-pubkey tree-0-b %testnet)
  ::  =/  tapscript-addr-1-a=@t  (tapscript-address:taproot taproot-recv-1-pubkey tree-1-a %testnet)
  ::  =/  tapscript-addr-1-b=@t  (tapscript-address:taproot taproot-recv-1-pubkey tree-1-b %testnet)
  ::  ::  Build hd-leaf structures for addresses 0 and 1
  ::  =/  taproot-leaf-0=hd-leaf
  ::    :*  [taproot-addr-0 ~ ~ ~ ~]  ::  main address-details
  ::        ^-  (map @t tapscript-details)
  ::        %-  ~(gas by *(map @t tapscript-details))
  ::        :~  [tapscript-addr-0-a 'checksig' tree-0-a [tapscript-addr-0-a ~ ~ ~ ~]]
  ::            [tapscript-addr-0-b 'csv-144' tree-0-b [tapscript-addr-0-b ~ ~ ~ ~]]
  ::        ==
  ::    ==
  ::  =/  taproot-leaf-1=hd-leaf
  ::    :*  [taproot-addr-1 ~ ~ ~ ~]  ::  main address-details
  ::        ^-  (map @t tapscript-details)
  ::        %-  ~(gas by *(map @t tapscript-details))
  ::        :~  [tapscript-addr-1-a 'checksig' tree-1-a [tapscript-addr-1-a ~ ~ ~ ~]]
  ::            [tapscript-addr-1-b 'csv-144' tree-1-b [tapscript-addr-1-b ~ ~ ~ ~]]
  ::        ==
  ::    ==
  ::  ::  Build network-details with pre-populated addresses
  ::  =/  taproot-recv-mop=((mop @ud hd-leaf) gth)
  ::    %+  put:((on @ud hd-leaf) gth)
  ::      (put:((on @ud hd-leaf) gth) *((mop @ud hd-leaf) gth) [0 taproot-leaf-0])
  ::    [1 taproot-leaf-1]
  ::  =/  taproot-network-details=network-details
  ::    :*  *((mop @ud hd-leaf) gth)   ::  change addresses (empty)
  ::        taproot-recv-mop           ::  receiving addresses
  ::        *(map @t address-suffix:hd-path)  ::  address-cache (empty)
  ::        *(map @t transaction)      ::  transactions (empty)
  ::        *(set @t)                  ::  tx-addresses (empty)
  ::        *(map @t (unit (unit tang)))  ::  tx-verification (empty)
  ::        ~                          ::  draft (none)
  ::    ==
  ::  ::  Build taproot account networks map
  ::  =/  taproot-networks=(map network network-details)
  ::    (~(put by *(map network network-details)) %testnet3 taproot-network-details)
  ::  ::
  ::  ::  Derive account keys - Cold Storage accounts
  ::  ::
  ::  =/  cold-acct-0  (derive-path:cold-master "m/84'/0'/0'")
  ::  =/  cold-acct-0-pubkey=@ux  public-key:cold-acct-0
  ::  =/  cold-acct-0-xprv=@t  (crip (prv-extended:cold-acct-0 %main))
  ::  ::
  ::  =/  cold-acct-legacy  (derive-path:cold-master "m/49'/0'/0'")
  ::  =/  cold-acct-legacy-pubkey=@ux  public-key:cold-acct-legacy
  ::  =/  cold-acct-legacy-xprv=@t  (crip (prv-extended:cold-acct-legacy %main))
  ::  ::
  ::  ::  Derive account keys - Fauceted Wallet accounts
  ::  ::
  ::  =/  fauceted-acct-0  (derive-path:fauceted-master "m/84'/1'/0'")
  ::  =/  fauceted-acct-0-pubkey=@ux  public-key:fauceted-acct-0
  ::  =/  fauceted-acct-0-xprv=@t  (crip (prv-extended:fauceted-acct-0 %testnet))
  ::  ::
  ::  ::  Derive account keys - Watch-only standalone (xpub only, no wallet)
  ::  ::
  ::  =/  watch-acct  (derive-path:watch-master "m/84'/0'/0'")
  ::  =/  watch-acct-pubkey=@ux  public-key:watch-acct
  ::  =/  watch-acct-xpub=@t  (crip (pub-extended:watch-acct %main))
  ::  ::
  ::  ::  Derive account keys - Signing standalone (xprv, no wallet)
  ::  ::
  ::  =/  sign-acct  (derive-path:sign-master "m/84'/1'/0'")
  ::  =/  sign-acct-pubkey=@ux  public-key:sign-acct
  ::  =/  sign-acct-xprv=@t  (crip (prv-extended:sign-acct %testnet))
  ::  ::
  ::  ::  Build accounts map
  ::  ::
  ::  =/  empty-networks=(map network network-details)  ~
  ::  =/  accounts=(map @ux account-details)
  ::    %-  ~(gas by *(map @ux account-details))
  ::    :~  ::  Main Wallet accounts (wallet reference set)
  ::        :-  main-acct-0-pubkey
  ::        ['Main Account' `main-wallet-pubkey [%xprv main-acct-0-xprv] %p2wpkh %main empty-networks [~ ~ ~ ~ ~] %.n]
  ::        ::
  ::        :-  main-acct-1-pubkey
  ::        ['Savings' `main-wallet-pubkey [%xprv main-acct-1-xprv] %p2wpkh %main empty-networks [~ ~ ~ ~ ~] %.n]
  ::        ::
  ::        :-  main-acct-testnet-pubkey
  ::        ['Testnet Account' `main-wallet-pubkey [%xprv main-acct-testnet-xprv] %p2wpkh %testnet3 empty-networks [~ ~ ~ ~ ~] %.n]
  ::        ::
  ::        :-  main-acct-taproot-pubkey
  ::        ['Taproot Test' `main-wallet-pubkey [%xprv main-acct-taproot-xprv] %p2tr %testnet3 taproot-networks [~ ~ ~ ~ ~] %.n]
  ::        ::
  ::        ::  Cold Storage accounts (wallet reference set)
  ::        :-  cold-acct-0-pubkey
  ::        ['Primary' `cold-wallet-pubkey [%xprv cold-acct-0-xprv] %p2wpkh %main empty-networks [~ ~ ~ ~ ~] %.n]
  ::        ::
  ::        :-  cold-acct-legacy-pubkey
  ::        ['Legacy Account' `cold-wallet-pubkey [%xprv cold-acct-legacy-xprv] %p2sh-p2wpkh %main empty-networks [~ ~ ~ ~ ~] %.n]
  ::        ::
  ::        ::  Fauceted Wallet accounts (wallet reference set)
  ::        :-  fauceted-acct-0-pubkey
  ::        ['Testnet Funded' `fauceted-wallet-pubkey [%xprv fauceted-acct-0-xprv] %p2wpkh %testnet3 empty-networks [~ ~ ~ ~ ~] %.n]
  ::        ::
  ::        ::  Watch-only standalone (no wallet reference)
  ::        :-  watch-acct-pubkey
  ::        ['Spending' ~ [%xpub watch-acct-xpub] %p2wpkh %main empty-networks [~ ~ ~ ~ ~] %.n]
  ::        ::
  ::        ::  Signing standalone (no wallet reference, has xprv)
  ::        :-  sign-acct-pubkey
  ::        ['Testnet Signer' ~ [%xprv sign-acct-xprv] %p2wpkh %testnet3 empty-networks [~ ~ ~ ~ ~] %.n]
  ::    ==
  ::  ::
  ::  ::  Build wallets map
  ::  ::
  ::  =/  wallets=(map @ux wallet)
  ::    %-  ~(gas by *(map @ux wallet))
  ::    :~  :-  main-wallet-pubkey
  ::        :*  'Main Wallet'
  ::            [%t main-seed]
  ::            main-wallet-pubkey
  ::            ^-  (map account:hd-path @ux)
  ::            %-  ~(gas by *(map account:hd-path @ux))
  ::            :~  [[[%.y 84] [%.y 0] [%.y 0]] main-acct-0-pubkey]
  ::                [[[%.y 84] [%.y 0] [%.y 1]] main-acct-1-pubkey]
  ::                [[[%.y 84] [%.y 1] [%.y 0]] main-acct-testnet-pubkey]
  ::                [[[%.y 86] [%.y 1] [%.y 0]] main-acct-taproot-pubkey]
  ::            ==
  ::            ~
  ::        ==
  ::        :-  cold-wallet-pubkey
  ::        :*  'Secondary Wallet'
  ::            [%t cold-seed]
  ::            cold-wallet-pubkey
  ::            ^-  (map account:hd-path @ux)
  ::            %-  ~(gas by *(map account:hd-path @ux))
  ::            :~  [[[%.y 84] [%.y 0] [%.y 0]] cold-acct-0-pubkey]
  ::                [[[%.y 49] [%.y 0] [%.y 0]] cold-acct-legacy-pubkey]
  ::            ==
  ::            ~
  ::        ==
  ::        :-  fauceted-wallet-pubkey
  ::        :*  'Fauceted Wallet'
  ::            [%t fauceted-seed]
  ::            fauceted-wallet-pubkey
  ::            ^-  (map account:hd-path @ux)
  ::            %-  ~(gas by *(map account:hd-path @ux))
  ::            :~  [[[%.y 84] [%.y 1] [%.y 0]] fauceted-acct-0-pubkey]
  ::            ==
  ::            ~
  ::        ==
  ::    ==
  ::  ::
  ::  ::  Build watch-only and signing sets
  ::  ::
  ::  =/  watch-only=(set @ux)  (silt ~[watch-acct-pubkey])
  ::  =/  signing=(set @ux)  (silt ~[sign-acct-pubkey])
  ::  ::
  ::  ::  Build SPV chain data per network
  ::  ::
  ::  =/  testnet3-spv=spv-chain
  ::    :*  ~                     :: headers
  ::        ~                     :: headers-by-height
  ::        ~                     :: headers-by-work
  ::        2.500.000             :: checkpoint-height
  ::        `@uvI`(rash '000000000000004a6039a59a81ad9ca1f6b143ad7c487f2c9e1c1d8e96e1e5ba' hex)  :: checkpoint-hash
  ::        ~                     :: header-sync
  ::        ~                     :: sync-error
  ::    ==
  ::  =/  spv-map=(map network spv-chain)
  ::    (my ~[[%testnet3 testnet3-spv]])
  ::  ::
  ::  ::  Build final state
  ::  ::
  ::  =/  final-state=state-0
  ::    %=  state
  ::      accounts              accounts
  ::      wallets               wallets
  ::      watch-only            watch-only
  ::      signing               signing
  ::      spv                   spv-map
  ::    ==
  ::  !>(final-state)
++  migrate
  |=  old=vase
  ^-  vase
  =/  try-new=(unit state-0)  ((soft state-0) q.old)
  ?^  try-new
    !>(u.try-new)
  ~|  "state migration required - please nuke and restart"
  !!
::
++  on-peek
  |=  [=bowl:gall state=vase =path]
  ~|  "unexpected scry into {<dap.bowl>} on path {<path>}"
  ?+  path  [~ ~]
    [%x %dbug %state ~]  ``noun+state
  ==
::
++  process
  =/  m  (fiber:io ,~)
  ;<  our=@p  bind:m  get-our:io
  ;<  src=@p  bind:m  get-poke-guest:io
  ;<  [=mark =vase]  bind:m  get-poke:io
  ::  Handle sponsorship requests from foreign ships
  ::
  ?:  ?=(%sponsorship-request mark)
    ~&  "sponsorship request from {<src>}"
    ;<  state=state-0  bind:m  (get-state-as:io state-0)
    ?.  auto-sponsor.state
      ~&  "sponsorship: auto-sponsor disabled, rejecting {<src>}"
      (pure:m ~)
    ::  Scry jael for our deed (for pass) and ring (for signing)
    ;<  deed=[=life =pass sec=(unit @)]  bind:m
      (scry:io ,[life pass (unit @)] %j /deed/(scot %p our)/1)
    ;<  =ring  bind:m
      (scry:io ,ring %j /vein/(scot %ud life.deed))
    =/  cac  (nol:nu:cric:crypto ring)
    ?>  &(?=(%c suite.+<.cac) ?=(^ sek.+<.cac))
    ::  Get current block height from urb-watcher
    ;<  =bowl:gall  bind:m  get-bowl:io
    ?.  .^(? %gu /(scot %p our.bowl)/urb-watcher/(scot %da now.bowl)/$)
      ~&  "sponsorship: urb-watcher not running, cannot sign"
      (pure:m ~)
    =/  [* height=@ud]
      .^([@ @ud] %gx /(scot %p our.bowl)/urb-watcher/(scot %da now.bowl)/block-id/block-id)
    ~&  "sponsorship: signing at height {<height>}"
    ::  Sign: (shaz (jam [sponsee height])) using raw ed25519
    ::  Must match urb-core verification: veri-octs:ed:crypto
    =/  msg=octs  512^(shaz (jam [src height]))
    =/  sig=@  (sign-octs-raw:ed:crypto msg [sgn.pub sgn.sek]:+<:cac)
    ::  Self-check: verify our own signature before sending
    =/  pub-cac  (com:nu:cric:crypto pass.deed)
    =/  self-check=?  (veri-octs:ed:crypto sig msg sgn:ded:ex:pub-cac)
    ~&  "sponsorship: self-check={<self-check>}"
    ?>  self-check
    ~&  "sponsorship: signed for {<src>} at height {<height>}"
    ~&  "sponsorship: sig={<sig>}"
    ::  Poke back the requester with the response
    ;<  ~  bind:m  (poke:io [src %spv-wallet] %fiber-poke !>(['sponsor-res' %sponsorship-response [sig height]]))
    (pure:m ~)
  ::  Handle sponsorship responses from sponsor
  ::
  ?:  ?=(%sponsorship-response mark)
    =/  [sig=@ height=@ud]  !<([@ @ud] vase)
    ~&  "sponsorship response from {<src>}: sig={<sig>} height={<height>}"
    ;<  state=state-0  bind:m  (get-state-as:io state-0)
    =.  sponsor-response.state  `[sig height]
    ;<  ~  bind:m  (replace:io !>(state))
    (pure:m ~)
  ::  HTTP requests may be unauthenticated (eyre sets src to guest)
  ::  Let the GET handler redirect to login if needed
  ::
  ?:  ?=(%handle-http-request mark)
    =+  !<(req=inbound-request:eyre vase)
    =/  lin=request-line:server  (parse-request-line:server url.request.req)
    =/  site=(list @t)  site.lin
    ::  Authenticated as someone else: reject
    ?:  &(authenticated.req !=(our src))  !!
    ::  Handle GET requests (unauthenticated redirects to login)
    ?:  ?=(%'GET' method.request.req)
      ?:  !=(our src)
        (give-simple-payload:io (login-redirect:sailbox lin))
      ;<  state=^vase  bind:m  get-state:io
      ;<  =bowl:gall  bind:m  get-bowl:io
      =/  =simple-payload:http
        (do-get:rt-wallet bowl state header-list.request.req [ext site]:lin args.lin)
      (give-simple-payload:io simple-payload)
    ::  All POST requests require authentication
    ?>  =(our src)
    ::  Handle POST requests - check for multipart vs form data
    =/  parts=(unit (list [@t part:multipart]))
      (de-request:multipart [header-list body]:request.req)
    ?^  parts
      ::  Multipart POST - Uploads, etc.
      !!
    ::  Form-encoded POST
    =/  args=key-value-list:kv  (parse-body:kv body.request.req)
    ?:  ?=([%spv-wallet %timer ~] site)
      =/  action=@t  (need (get-key:kv 'action' args))
      ?+  action  !!
          %start
        =.  io  io(hold &) :: claim the mutex
        ;<  state=state-0  bind:m  (get-state-as:io state-0)
        =.  state  state(counter 0)
        ;<  ~  bind:m  (replace:io !>(state))
        ;<  ~  bind:m  (send-sse-event:io /spv-wallet/timer ~ `'/timer/counter-update')
        |-
        ;<  state=state-0  bind:m  (get-state-as:io state-0)
        ?:  (gte counter.state 5)
          (pure:m ~)
        ;<  ~  bind:m  (replace:io !>(state(counter +(counter.state))))
        ;<  ~  bind:m  (send-sse-event:io /spv-wallet/timer ~ `'/timer/counter-update')
        ;<  ~  bind:m  (sleep:io ~s1)
        $
      ==
    ?:  ?=([%spv-wallet %progress ~] site)
      (handle-boot-actions:rt-boot args)
    ?+    site  !!
        [%spv-wallet ~]
      (handle-wallet-actions:rt-wallet args)
      ::
        [%spv-wallet %wallet @ ~]
      (handle-wallet-discovery-actions:rt-wallet (rash i.t.t.site hex) args)
      ::
        [%spv-wallet %account @ %send ~]
      ::  Send page uses account-pubkey directly
      (handle-send-actions:rt-send (rash i.t.t.site hex) args)
      ::
        [%spv-wallet %account @ ~]
      ::  Universal account route - works for ALL account types
      (handle-account-actions:rt-account (rash i.t.t.site hex) args)
      ::
        [%spv-wallet %spv ~]
      (handle-spv-actions:rt-spv args)
    ==
  ::  All other marks require self-poke
  ?>  =(our src)
  ?+    mark  !!
      %on-init :: sent by sailbox
    ;<  state=state-0  bind:m  (get-state-as:io state-0)
    (set-bindings:io ~[binding.state])
    ::
      %on-load :: sent by sailbox
    ;<  state=state-0  bind:m  (get-state-as:io state-0)
    (set-bindings:io ~[binding.state])
    ::
      %set-binding
    =+  !<(new-binding=binding:eyre vase)
    ;<  state=state-0  bind:m  (get-state-as:io state-0)
    =.  binding.state  new-binding
    ;<  ~  bind:m  (replace:io !>(state))
    (set-bindings:io ~[new-binding])
    ::
      %on-fail :: sent by sailbox
    =+  !<([=term =tang] vase)
    (pure:m ~)
  ==
::
++  first-sse-event
  |=  $:  site=(list @t)
          args=(list [key=@t value=@t])
          last-event-id=(unit @t)
      ==
  ^-  (unit sse-key:sailbox)
  ?+    site  ~
      [%spv-wallet %timer ~]
    `[~ `'/timer/counter-update']
    ::
      [%spv-wallet %progress ~]
    `[~ `'progress-update']
    ::
      [%spv-wallet %stream ~]
    `[~ ~]
    ::
      [%spv-wallet %stream %spv @ ~]
    ::  SPV page SSE - no initial event, just register subscription
    `[~ ~]
    ::
      [%spv-wallet %account @ %send ~]
    `[~ `'draft-outputs-update']
  ==
::
++  make-sse-event
  |=  $:  =bowl:gall
          state=vase
          site=(list @t)
          args=(list [key=@t value=@t])
          id=(unit @t)
          event=(unit @t)
      ==
  ^-  wain
  =+  !<(state-0 state)
  ?:  ?=([%spv-wallet %timer ~] site)
    (handle-test-pages-sse:ui bowl state site args id event)
  ?:  ?=([%spv-wallet %progress ~] site)
    =/  prog=(unit progress-info)
      ?~(boot ~ `(boot-progress u.boot))
    =/  err=(unit [term tang])
      ?~(boot ~ error.u.boot)
    (handle-progress-sse:ui prog err event)
  ?:  ?=([%spv-wallet %stream ~] site)
    (handle-spv-sse:ui bowl state %main args id event)
  ?+    site  !!
      [%spv-wallet %stream %spv @ ~]
    =/  net-str=@t  i.t.t.t.site
    =/  net=network
      ?:  =('main' net-str)  %main
      ?:  =('testnet3' net-str)  %testnet3
      ?:  =('testnet4' net-str)  %testnet4
      %testnet3
    (handle-spv-sse:ui bowl state net args id event)
    ::
      [%spv-wallet %stream %wallet @ ~]
    =/  pubkey=@ux  (rash i.t.t.t.site hex)
    (handle-discovery-sse:ui bowl state pubkey args id event)
    ::
      [%spv-wallet %stream %wallet @ %account @ ~]
    ::  Legacy path - look up account-pubkey from wallet + path
    =/  wallet-pubkey=@ux  (rash i.t.t.t.site hex)
    =/  account-path-str=@t  i.t.t.t.t.t.site
    =/  wallet=(unit wallet)  (~(get by wallets) wallet-pubkey)
    ?~  wallet  ~
    =/  account-pubkey=(unit @ux)
      %-  ~(rep by accounts.u.wallet)
      |=  [[acct=account:hd-path pk=@ux] result=(unit @ux)]
      ?^  result  result
      =/  path-str=tape  (format-account-path:wallet-address acct)
      ?.  =(path-str (trip account-path-str))  ~
      `pk
    ?~  account-pubkey  ~
    (handle-account-sse:ui bowl state u.account-pubkey args id event)
    ::
      [%spv-wallet %stream %account @ %send ~]
    =/  account-pubkey=@ux  (rash i.t.t.t.site hex)
    (handle-send-sse:ui bowl state account-pubkey args id event)
    ::
      [%spv-wallet %stream %watch-only @ ~]
    =/  account-pubkey=@ux  (rash i.t.t.t.site hex)
    (handle-account-sse:ui bowl state account-pubkey args id event)
    ::
      [%spv-wallet %stream %signing @ ~]
    =/  account-pubkey=@ux  (rash i.t.t.t.site hex)
    (handle-account-sse:ui bowl state account-pubkey args id event)
    ::
      [%spv-wallet %stream %account @ ~]
    ::  Universal account SSE route - all accounts use account-pubkey
    =/  account-pubkey=@ux  (rash i.t.t.t.site hex)
    (handle-account-sse:ui bowl state account-pubkey args id event)
  ==
--
