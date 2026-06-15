/-  *spv-wallet, mcp, s0=spv-wallet-0
/+  dbug, sailbox, io=sailboxio, server, multipart,
    ui=ui-spv-wallet, html-utils, json-utils,
    bip39, bip32=bip32-spv, btc=bitcoin, bip329,
    wallet-address, wallet-account, seed-phrases, *wallet-mempool-space,
    rt-wallet, rt-account, rt-send, rt-spv, rt-boot, taproot,
    draft=ui-draft
/=  t-  /tests/lib/bitcoin-spv
/=  t-  /tests/lib/seed-phrases
/=  t-  /tests/lib/transactions
/=  t-  /tests/lib/taproot
/=  m-  /mar/sponsorship-request
/=  m-  /mar/sponsorship-response
/=  m-  /mar/address-request
/=  m-  /mar/address-offer
/=  f-  /fil/mcp/tools/generate-wallet-2
=>
  |%
  ++  kv    kv:html-utils
  +$  card  card:sailbox
  ::  Auto-generate a <simple> wallet on first boot.
  ::  Creates a BIP84 native segwit wallet from entropy so the
  ::  simple UI (/apps/wallet) has something to show immediately.
  ::  Skips if a <simple> wallet already exists.
  ::  Resolve the simple wallet's account pubkey for a given network
  ::  Falls back to first account if no network match
  ::
  ++  resolve-simple-pubkey
    |=  [state=state-1 req-net=@t]
    ^-  (unit @ux)
    =/  wal-list=(list [k=@ux v=wallet])  ~(tap by wallets.state)
    ?~  wal-list  ~
    =/  simple=(unit [k=@ux v=wallet])
      =/  rem=(list [k=@ux v=wallet])  wal-list
      |-
      ?~  rem  ~
      =/  mx=(unit manx)  (de-xml:html name.v.i.rem)
      ?:  ?&(?=(^ mx) =(%simple n.g.u.mx))
        `i.rem
      $(rem t.rem)
    =/  wal=wallet  ?~(simple v.i.wal-list v.u.simple)
    =/  acct-pairs=(list [account:hd-path @ux])  ~(tap by accounts.wal)
    ?~  acct-pairs  ~
    =/  matched=(unit @ux)
      =/  rem=(list [account:hd-path @ux])  acct-pairs
      |-
      ?~  rem  ~
      =/  d=(unit account-details)  (~(get by accounts.state) +.i.rem)
      ?~  d  $(rem t.rem)
      ?:  =(req-net (crip (trip active-network.u.d)))  `+.i.rem
      $(rem t.rem)
    `?~(matched +.i.acct-pairs u.matched)
  ::
  ++  ensure-default-wallet
    |=  state=state-1
    =/  m  (fiber:io ,~)
    ^-  form:m
    ::  Check if a <simple> wallet already exists
    =/  has-simple=_|
      %-  ~(rep by wallets.state)
      |=  [[k=@ux v=wallet] found=_|]
      ?:  found  %.y
      =/  mx=(unit manx)  (de-xml:html name.v)
      ?~  mx  %.n
      =(%simple n.g.u.mx)
    ~&  "ensure-default-wallet: has-simple={<has-simple>} wallets={<~(wyt by wallets.state)>}"
    ?:  has-simple
      ::  Ensure the simple wallet has both mainnet and testnet3 accounts
      =/  simple=(unit [k=@ux v=wallet])
        =/  rem=(list [k=@ux v=wallet])  ~(tap by wallets.state)
        |-
        ?~  rem  ~
        =/  mx=(unit manx)  (de-xml:html name.v.i.rem)
        ?:  ?&(?=(^ mx) =(%simple n.g.u.mx))
          `i.rem
        $(rem t.rem)
      ?~  simple  (pure:m ~)
      =/  acct-pairs=(list [account:hd-path @ux])  ~(tap by accounts.v.u.simple)
      =/  has-main=_|
        =/  rem  acct-pairs
        |-
        ?~  rem  %.n
        =/  det=(unit account-details)  (~(get by accounts.state) +.i.rem)
        ?~  det  $(rem t.rem)
        ?:  =(%main active-network.u.det)  %.y
        $(rem t.rem)
      =/  has-testnet=_|
        =/  rem  acct-pairs
        |-
        ?~  rem  %.n
        =/  det=(unit account-details)  (~(get by accounts.state) +.i.rem)
        ?~  det  $(rem t.rem)
        ?:  =(%testnet3 active-network.u.det)  %.y
        $(rem t.rem)
      =/  wal=wallet  v.u.simple
      =/  master  (from-seed:bip32 (seed-to-bytes:wallet-address seed.wal))
      ;<  state=state-1  bind:m  (get-state-as:io state-1)
      ?.  has-main
        =/  derived  (derive-path:master "m/84'/0'/0'")
        =/  account-pubkey=@ux  public-key:derived
        =/  xprv=@t  (crip (prv-extended:derived %main))
        =/  m-acct=account:hd-path  [[%.y 84] [%.y 0] [%.y 0]]
        =/  new-account=account-details
          :*  'Bitcoin'
              `k.u.simple
              [%xprv xprv]
              %p2wpkh
              %main
              ~
              [~ ~ ~ ~ ~]
              %.n
          ==
        =.  accounts.wal  (~(put by accounts.wal) m-acct account-pubkey)
        =.  wallets.state  (~(put by wallets.state) k.u.simple wal)
        =.  accounts.state  (~(put by accounts.state) account-pubkey new-account)
        ;<  ~  bind:m  (replace:io !>(state))
        (pure:m ~)
      ?.  has-testnet
        =/  derived  (derive-path:master "m/84'/1'/0'")
        =/  account-pubkey=@ux  public-key:derived
        =/  xprv=@t  (crip (prv-extended:derived %testnet))
        =/  t-acct=account:hd-path  [[%.y 84] [%.y 1] [%.y 0]]
        =/  new-account=account-details
          :*  'Bitcoin Testnet'
              `k.u.simple
              [%xprv xprv]
              %p2wpkh
              %testnet3
              ~
              [~ ~ ~ ~ ~]
              %.n
          ==
        =.  accounts.wal  (~(put by accounts.wal) t-acct account-pubkey)
        =.  wallets.state  (~(put by wallets.state) k.u.simple wal)
        =.  accounts.state  (~(put by accounts.state) account-pubkey new-account)
        ;<  ~  bind:m  (replace:io !>(state))
        (pure:m ~)
      (pure:m ~)
    ::  No simple wallet — generate from entropy
    ~&  "ensure-default-wallet: generating new simple wallet"
    ;<  eny=@uvJ  bind:m  get-entropy:io
    =/  generated-seed=cord  (gen-seed:seed-phrases eny %256)
    =/  =seed  [%t generated-seed]
    =/  pubkey=@ux  (seed-to-pubkey:wallet-address seed)
    =/  master  (from-seed:bip32 (seed-to-bytes:wallet-address seed))
    =/  master-xpub=@t  (crip (pub-extended:master %main))
    ::  Create wallet
    =/  new-wallet=wallet
      ['<simple>My Wallet</simple>' seed pubkey master-xpub ~ ~]
    ::  Derive BIP84 mainnet account: m/84'/0'/0'
    =/  main-acct=account:hd-path  [[%.y 84] [%.y 0] [%.y 0]]
    =/  main-derived  (derive-path:master "m/84'/0'/0'")
    =/  main-account-pubkey=@ux  public-key:main-derived
    =/  main-xprv=@t  (crip (prv-extended:main-derived %main))
    =/  main-account=account-details
      :*  'Bitcoin'
          `pubkey
          [%xprv main-xprv]
          %p2wpkh
          %main
          ~
          [~ ~ ~ ~ ~]
          %.n
      ==
    =.  accounts.new-wallet  (~(put by accounts.new-wallet) main-acct main-account-pubkey)
    ::  Derive BIP84 testnet account: m/84'/1'/0'
    =/  test-acct=account:hd-path  [[%.y 84] [%.y 1] [%.y 0]]
    =/  test-derived  (derive-path:master "m/84'/1'/0'")
    =/  test-account-pubkey=@ux  public-key:test-derived
    =/  test-xprv=@t  (crip (prv-extended:test-derived %testnet))
    =/  test-account=account-details
      :*  'Bitcoin Testnet'
          `pubkey
          [%xprv test-xprv]
          %p2wpkh
          %testnet3
          ~
          [~ ~ ~ ~ ~]
          %.n
      ==
    =.  accounts.new-wallet  (~(put by accounts.new-wallet) test-acct test-account-pubkey)
    ::  Save state
    ;<  state=state-1  bind:m  (get-state-as:io state-1)
    =.  wallets.state  (~(put by wallets.state) pubkey new-wallet)
    =.  accounts.state
      %-  ~(gas by accounts.state)
      ~[[main-account-pubkey main-account] [test-account-pubkey test-account]]
    ;<  ~  bind:m  (replace:io !>(state))
    (pure:m ~)
  --
^-  agent:gall
%-  agent:dbug
%-  agent:sailbox
^-  sailbox:sailbox
|%
++  initial
  ^-  vase
  =|  state=state-1
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
  ::  =/  final-state=state-1
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
  =/  ver  -.q.old
  ?+  ver  old
    %1  !>(;;(state-1 q.old))
    %0
      =/  os=state-0:s0  ;;(state-0:s0 q.old)
      =/  migrate-status=$-(tx-status:s0 tx-status)
        |=  old=tx-status:s0
        ?:  ?=([%unconfirmed ~] old)  old
        [%confirmed block-hash.old block-height.old 0]
      =/  migrate-addr=$-(address-details:s0 address-details)
        |=  ad=address-details:s0
        %=  ad
          utxos  %+  turn  utxos.ad
                 |=  u=[txid=@t vout=@ud value=@ud tx-status=tx-status:s0]
                 u(tx-status (migrate-status tx-status.u))
        ==
      =/  migrate-leaf=$-(hd-leaf:s0 hd-leaf)
        |=  lf=hd-leaf:s0
        :-  (migrate-addr main.lf)
        %-  ~(run by script-trees.lf)
        |=  td=tapscript-details:s0
        td(address-details (migrate-addr address-details.td))
      =/  migrate-tx=$-(transaction:s0 transaction)
        |=  tx=transaction:s0
        tx(tx-status (migrate-status tx-status.tx))
      =/  migrate-net=$-(network-details:s0 network-details)
        |=  nd=network-details:s0
        %=  nd
          transactions  (~(run by transactions.nd) migrate-tx)
          change        (run:((on @ud hd-leaf:s0) gth) change.nd migrate-leaf)
          receiving     (run:((on @ud hd-leaf:s0) gth) receiving.nd migrate-leaf)
        ==
      =/  new-accounts=(map @ux account-details)
        %-  ~(run by accounts.os)
        |=  ad=account-details:s0
        ^-  account-details
        ad(networks (~(run by networks.ad) migrate-net))
      =/  new-wallets=(map @ux wallet)
        %-  ~(run by wallets.os)
        |=  w=wallet:s0
        ^-  wallet
        =/  master  (from-seed:bip32 (seed-to-bytes:wallet-address seed.w))
        =/  master-xpub=@t  (crip (pub-extended:master %main))
        [name.w seed.w fingerprint.w master-xpub accounts.w scan.w]
      !>  ^-  state-1
      :*  %1
          auto-sponsor.os
          boot.os
          sponsor-response.os
          new-accounts
          watch-only.os
          signing.os
          new-wallets
          labels.os
          spv.os
          indexer-subs.os
          hide-empty-addresses.os
          binding.os
          counter.os
          *(map @t broadcast)
      ==
  ==
::
++  on-peek
  |=  [=bowl:gall state=vase =path]
  ^-  (unit (unit cage))
  ~|  "unexpected scry into {<dap.bowl>} on path {<path>}"
  ?+  path  [~ ~]
    [%x %dbug %state ~]  ``noun+state
  ::
      [%x %mcp %tools ~]
    %-  some
    %-  some
    :-  %mcp-tools
    !>  ^-  (list tool:mcp)
    %+  turn
      .^  (list ^path)
          %ct
          /(scot %p our.bowl)/[q.byk.bowl]/(scot %da now.bowl)/fil/mcp/tools
      ==
    |=  =^path
    ^-  tool:mcp
    !<(tool:mcp .^(vase %ca (welp /(scot %p our.bowl)/[q.byk.bowl]/(scot %da now.bowl) path)))
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
    ;<  state=state-1  bind:m  (get-state-as:io state-1)
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
    ;<  state=state-1  bind:m  (get-state-as:io state-1)
    =.  sponsor-response.state  `[sig height]
    ;<  ~  bind:m  (replace:io !>(state))
    (pure:m ~)
  ::  Handle address requests from foreign ships
  ::  Someone is asking us for a receive address
  ::
  ?:  ?=(%address-request mark)
    =/  req-net=network  !<(network vase)
    ~&  "address-request from {<src>} for {<req-net>}"
    ;<  state=state-1  bind:m  (get-state-as:io state-1)
    =/  pk-unit=(unit @ux)  (resolve-simple-pubkey state req-net)
    ?~  pk-unit
      ~&  >>>  "address-request: no account for {<req-net>}"
      (pure:m ~)
    =/  pk=@ux  u.pk-unit
    =/  det=(unit account-details)  (~(get by accounts.state) pk)
    ?~  det
      ~&  >>>  "address-request: account details not found"
      (pure:m ~)
    =/  ac  ~(. ac:wallet-account [u.det active-network.u.det])
    =/  xpub=@t  k.extended-key.u.det
    =/  offer-idx=@ud
      (get-next-offer-index:draft receiving:ac labels.state xpub)
    ::  Derive address at the offer index
    =/  addr=@t
      %:  derive-address-from-xkey:wallet-account
        xpub  'receiving'  offer-idx
        script-type.u.det  active-network.u.det
      ==
    ~&  "address-request: offering index {<offer-idx>} addr {<addr>} to {<src>}"
    ::  Label it simple:offered:to:~requester
    =/  lbl=@t  (crip "simple:offered:to:{(scow %p src)}")
    =.  labels.state  (~(put la:bip329 labels.state) [%addr addr lbl ~ ~])
    ::  Update last-offered
    =.  labels.state  (set-last-offered:draft labels.state xpub offer-idx)
    ;<  ~  bind:m  (replace:io !>(state))
    ::  Ensure address is derived in our local state
    ;<  ~  bind:m  (refresh-account-address:wallet-account pk 'receiving' offer-idx)
    ::  Send address back to requester
    ;<  ~  bind:m  (poke:io [src %spv-wallet] %address-offer !>([addr req-net]))
    (pure:m ~)
  ::  Handle address offers from foreign ships
  ::  Someone is giving us an address we requested
  ::
  ?:  ?=(%address-offer mark)
    =/  [addr=@t req-net=network]  !<([@t network] vase)
    ~&  "address-offer from {<src>}: {<addr>} on {<req-net>}"
    ;<  state=state-1  bind:m  (get-state-as:io state-1)
    ::  Clear old simple:send:active from all addresses
    =/  addr-labels=(list [@t (set label-entry:bip329)])
      ~(tap by addr.labels.state)
    =.  labels.state
      |-
      ?~  addr-labels  labels.state
      =/  [ref=@t entries=(set label-entry:bip329)]  i.addr-labels
      =/  has-active=?
        %+  lien  ~(tap in entries)
        |=(e=label-entry:bip329 =('simple:send:active' label.e))
      ?.  has-active  $(addr-labels t.addr-labels)
      $(addr-labels t.addr-labels, labels.state (~(del la:bip329 labels.state) %addr ref 'simple:send:active'))
    ::  Label it simple:offered:from:~sender and simple:send:active
    =/  lbl=@t  (crip "simple:offered:from:{(scow %p src)}")
    =.  labels.state  (~(put la:bip329 labels.state) [%addr addr lbl ~ ~])
    =.  labels.state  (~(put la:bip329 labels.state) [%addr addr 'simple:send:active' ~ ~])
    ;<  ~  bind:m  (replace:io !>(state))
    ::  Notify UI via SSE
    ;<  ~  bind:m  (send-sse-event:io /spv-wallet/stream ~ `'address-offer-received')
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
      ::  Ensure at least one receiving address exists for draft pages
      ?:  ?&  ?|  ?=([%apps %wallet ~] site.lin)
                  ?=([%apps %wallet-1 ~] site.lin)
                  ?=([%apps %wallet-2 ~] site.lin)
              ==
          ==
        =/  st=state-1  !<(state-1 state)
        =/  req-net=@t
          (fall (get-key:kv:html-utils 'net' args.lin) 'main')
        =/  pk-unit=(unit @ux)  (resolve-simple-pubkey st req-net)
        ?~  pk-unit
          ;<  =bowl:gall  bind:m  get-bowl:io
          =/  =simple-payload:http
            (do-get:rt-wallet bowl state header-list.request.req [ext site]:lin args.lin)
          (give-simple-payload:io simple-payload)
        =/  det=(unit account-details)  (~(get by accounts.st) u.pk-unit)
        ?~  det
          ;<  =bowl:gall  bind:m  get-bowl:io
          =/  =simple-payload:http
            (do-get:rt-wallet bowl state header-list.request.req [ext site]:lin args.lin)
          (give-simple-payload:io simple-payload)
        =/  ac  ~(. ac:wallet-account [u.det active-network.u.det])
        =/  recv-leaves=(list [@ud hd-leaf])
          (tap:((on @ud hd-leaf) gth) receiving:ac)
        ?^  recv-leaves
          ::  Has receiving addresses, render normally
          ;<  =bowl:gall  bind:m  get-bowl:io
          =/  =simple-payload:http
            (do-get:rt-wallet bowl state header-list.request.req [ext site]:lin args.lin)
          (give-simple-payload:io simple-payload)
        ::  No receiving addresses — derive index 0 before rendering
        ;<  ~  bind:m
          (refresh-account-address:wallet-account u.pk-unit 'receiving' 0)
        ;<  state=^vase  bind:m  get-state:io
        ;<  =bowl:gall  bind:m  get-bowl:io
        =/  =simple-payload:http
          (do-get:rt-wallet bowl state header-list.request.req [ext site]:lin args.lin)
        (give-simple-payload:io simple-payload)
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
        ;<  state=state-1  bind:m  (get-state-as:io state-1)
        =.  state  state(counter 0)
        ;<  ~  bind:m  (replace:io !>(state))
        ;<  ~  bind:m  (send-sse-event:io /spv-wallet/timer ~ `'/timer/counter-update')
        |-
        ;<  state=state-1  bind:m  (get-state-as:io state-1)
        ?:  (gte counter.state 5)
          (pure:m ~)
        ;<  ~  bind:m  (replace:io !>(state(counter +(counter.state))))
        ;<  ~  bind:m  (send-sse-event:io /spv-wallet/timer ~ `'/timer/counter-update')
        ;<  ~  bind:m  (sleep:io ~s1)
        $
      ==
    ::  spawn disabled
    ?:  ?=([%spv-wallet %progress ~] site)
      ::  (handle-boot-actions:rt-boot args)
      (pure:m ~)
    ?+    site  !!
        [%apps %wallet ~]
      ::  Simple wallet actions
      =/  action=@t  (need (get-key:kv 'action' args))
      ?+  action  !!
          %rename-wallet
        =/  new-name=@t  (need (get-key:kv 'name' args))
        ;<  state=state-1  bind:m  (get-state-as:io state-1)
        =/  simple=(unit [key=@ux val=wallet])
          (find-simple-wallet:draft wallets.state)
        ?~  simple  (pure:m ~)
        =/  tagged-name=@t
          (crip "<simple>{(trip new-name)}</simple>")
        =.  wallets.state
          (~(put by wallets.state) key.u.simple val.u.simple(name tagged-name))
        ;<  ~  bind:m  (replace:io !>(state))
        (pure:m ~)
        ::
          %toggle-saved
        ;<  state=state-1  bind:m  (get-state-as:io state-1)
        =/  simple=(unit [key=@ux val=wallet])
          (find-simple-wallet:draft wallets.state)
        ?~  simple  (pure:m ~)
        =/  master-xpub=@t  xpub.val.u.simple
        =/  saved=?  (get-simple-saved:draft labels.state master-xpub)
        =.  labels.state  (set-simple-saved:draft labels.state master-xpub !saved)
        ;<  ~  bind:m  (replace:io !>(state))
        (pure:m ~)
        ::
          %set-fee-rate
        =/  fee=@t  (fall (get-key:kv 'fee-rate' args) '2')
        =/  req-net=@t  (fall (get-key:kv 'net' args) 'main')
        ;<  state=state-1  bind:m  (get-state-as:io state-1)
        =/  pk-unit=(unit @ux)  (resolve-simple-pubkey state req-net)
        ?~  pk-unit  (pure:m ~)
        =/  det=(unit account-details)  (~(get by accounts.state) u.pk-unit)
        ?~  det  (pure:m ~)
        =/  acct-xpub=@t  k.extended-key.u.det
        =/  fee-val=@ud  (fall (rush fee dem) 2)
        =.  labels.state  (set-simple-fee:draft labels.state acct-xpub fee-val)
        ;<  ~  bind:m  (replace:io !>(state))
        (pure:m ~)
        ::
          %request-address
        ::  Request a receive address from another ship
        =/  ship-name=@t  (need (get-key:kv 'ship' args))
        =/  req-net=@t  (fall (get-key:kv 'net' args) 'main')
        =/  target=@p  (rash ship-name ;~(pfix sig fed:ag))
        ~&  "request-address: asking {<target>} for {<req-net>} address"
        ;<  state=state-1  bind:m  (get-state-as:io state-1)
        ::  Clear old simple:send:active from all addresses
        =/  addr-labels=(list [@t (set label-entry:bip329)])
          ~(tap by addr.labels.state)
        =.  labels.state
          |-
          ?~  addr-labels  labels.state
          =/  [ref=@t entries=(set label-entry:bip329)]  i.addr-labels
          =/  has-active=?
            %+  lien  ~(tap in entries)
            |=(e=label-entry:bip329 =('simple:send:active' label.e))
          ?.  has-active  $(addr-labels t.addr-labels)
          $(addr-labels t.addr-labels, labels.state (~(del la:bip329 labels.state) %addr ref 'simple:send:active'))
        ;<  ~  bind:m  (replace:io !>(state))
        =/  net=network  ;;(network (crip (trip req-net)))
        ;<  ~  bind:m  (poke:io [target %spv-wallet] %address-request !>(net))
        (pure:m ~)
        ::
          %refresh-wallet
        ::  Refresh: pending addresses + next unused receiving/change
        =/  req-net=@t  (fall (get-key:kv 'net' args) 'main')
        ;<  state=state-1  bind:m  (get-state-as:io state-1)
        =/  pk-unit=(unit @ux)  (resolve-simple-pubkey state req-net)
        ?~  pk-unit  (pure:m ~)
        =/  pk=@ux  u.pk-unit
        =/  det=(unit account-details)  (~(get by accounts.state) pk)
        ?~  det
          ;<  ~  bind:m  (refresh-account-address:wallet-account pk 'receiving' 0)
          (pure:m ~)
        =/  ac  ~(. ac:wallet-account [u.det active-network.u.det])
        ::  Collect addresses with pending mempool activity
        =/  pending-addrs=(list [chain=@t idx=@ud])
          =/  recv=(list [@ud hd-leaf])
            (tap:((on @ud hd-leaf) gth) receiving:ac)
          =/  chng=(list [@ud hd-leaf])
            (tap:((on @ud hd-leaf) gth) change:ac)
          ;:  weld
            %+  murn  recv
            |=  [idx=@ud =hd-leaf]
            =/  info-unit  info.main.hd-leaf
            ?~  info-unit  ~
            ?.  ?|  (gth mempool-funded.u.info-unit 0)
                    (gth mempool-spent.u.info-unit 0)
                ==
              ~
            `['receiving' idx]
          ::
            %+  murn  chng
            |=  [idx=@ud =hd-leaf]
            =/  info-unit  info.main.hd-leaf
            ?~  info-unit  ~
            ?.  ?|  (gth mempool-funded.u.info-unit 0)
                    (gth mempool-spent.u.info-unit 0)
                ==
              ~
            `['change' idx]
          ==
        ::  Collect addresses from unconfirmed transactions
        =/  unconf-addrs=(list [chain=@t idx=@ud])
          =/  txns=(list [txid=@t tx=transaction])  ~(tap by transactions:ac)
          =/  addrs=(set @t)  ~
          =.  addrs
            |-
            ?~  txns  addrs
            =/  tx=transaction  tx.i.txns
            ?.  ?=([%unconfirmed *] tx-status.tx)  $(txns t.txns)
            =/  out-addrs=(list @t)
              (turn outputs.tx |=(o=tx-output address.o))
            =/  in-addrs=(list @t)
              (murn inputs.tx |=(i=tx-input ?~(prevout.i ~ `address.u.prevout.i)))
            $(txns t.txns, addrs (~(gas in addrs) (weld out-addrs in-addrs)))
          ::  Resolve to chain/index via address-cache
          %+  murn  ~(tap in addrs)
          |=  addr=@t
          =/  suffix=(unit address-suffix:hd-path)
            (~(get by address-cache:ac) addr)
          ?~  suffix  ~
          =/  [chain-num=@ud addr-idx=@ud]
            [q.change.u.suffix q.index.u.suffix]
          `[?:(=(0 chain-num) 'receiving' 'change') addr-idx]
        ::  Collect addresses from broadcasts not yet in transaction map
        =/  broadcast-addrs=(list [chain=@t idx=@ud])
          =/  bcs=(list [txid=@t bc=broadcast])  ~(tap by broadcasts.state)
          =/  addrs=(set @t)  ~
          =.  addrs
            |-
            ?~  bcs  addrs
            =/  [txid=@t bc=broadcast]  i.bcs
            ?.  =(network.bc active-network.u.det)  $(bcs t.bcs)
            ?:  (~(has by transactions:ac) txid)  $(bcs t.bcs)
            =/  out-addrs=(list @t)
              (turn outputs.bc |=([addr=@t *] addr))
            =/  in-addrs=(list @t)
              (turn inputs.bc |=([addr=@t *] addr))
            $(bcs t.bcs, addrs (~(gas in addrs) (weld out-addrs in-addrs)))
          %+  murn  ~(tap in addrs)
          |=  addr=@t
          =/  suffix=(unit address-suffix:hd-path)
            (~(get by address-cache:ac) addr)
          ?~  suffix  ~
          =/  [chain-num=@ud addr-idx=@ud]
            [q.change.u.suffix q.index.u.suffix]
          `[?:(=(0 chain-num) 'receiving' 'change') addr-idx]
        ::  Find next unused receiving index
        =/  next-recv-idx=@ud
          =/  next=(unit @t)
            (get-next-unused-address:wallet-address receiving:ac)
          ?~  next
            (lent (tap:((on @ud hd-leaf) gth) receiving:ac))
          =/  leaves=(list [@ud hd-leaf])
            (tap:((on @ud hd-leaf) gth) receiving:ac)
          |-
          ?~  leaves
            (lent (tap:((on @ud hd-leaf) gth) receiving:ac))
          =/  [lidx=@ud =hd-leaf]  i.leaves
          ?:  =(address.main.hd-leaf u.next)  lidx
          $(leaves t.leaves)
        ::  Find next unused change index
        =/  next-chng-idx=@ud
          =/  next=(unit @t)
            (get-next-unused-address:wallet-address change:ac)
          ?~  next
            (lent (tap:((on @ud hd-leaf) gth) change:ac))
          =/  leaves=(list [@ud hd-leaf])
            (tap:((on @ud hd-leaf) gth) change:ac)
          |-
          ?~  leaves
            (lent (tap:((on @ud hd-leaf) gth) change:ac))
          =/  [lidx=@ud =hd-leaf]  i.leaves
          ?:  =(address.main.hd-leaf u.next)  lidx
          $(leaves t.leaves)
        ::  Build refresh list: pending + unconfirmed + broadcast + next unused
        =/  refresh-list=(list [chain=@t idx=@ud])
          =/  all=(list [chain=@t idx=@ud])
            ;:  weld
              pending-addrs
              unconf-addrs
              broadcast-addrs
              ~[['receiving' next-recv-idx]]
              ~[['change' next-chng-idx]]
            ==
          ::  Deduplicate
          =/  seen=(set [chain=@t idx=@ud])  ~
          =/  out=(list [chain=@t idx=@ud])  ~
          |-
          ?~  all  (flop out)
          ?:  (~(has in seen) i.all)  $(all t.all)
          $(all t.all, seen (~(put in seen) i.all), out [i.all out])
        ::  Refresh all sequentially
        |-
        ?~  refresh-list  (pure:m ~)
        ;<  ~  bind:m
          (refresh-account-address:wallet-account pk chain.i.refresh-list idx.i.refresh-list)
        $(refresh-list t.refresh-list)
        ::
          %refresh-address
        ::  Refresh a single address by chain + index
        =/  chain=@t   (need (get-key:kv 'chain' args))
        =/  index=@ud  (rash (need (get-key:kv 'index' args)) dem)
        =/  req-net=@t  (fall (get-key:kv 'net' args) 'main')
        ;<  state=state-1  bind:m  (get-state-as:io state-1)
        =/  pk-unit=(unit @ux)  (resolve-simple-pubkey state req-net)
        ?~  pk-unit  (pure:m ~)
        ;<  ~  bind:m  (refresh-account-address:wallet-account u.pk-unit chain index)
        (pure:m ~)
        ::
          %send-bitcoin
        ::  Build, sign, and broadcast a transaction
        =/  address=@t   (need (get-key:kv 'address' args))
        =/  amount=@t    (need (get-key:kv 'amount' args))
        =/  fee-rate=@t  (fall (get-key:kv 'fee-rate' args) '2')
        =/  req-net=@t   (fall (get-key:kv 'net' args) 'main')
        ;<  state=state-1  bind:m  (get-state-as:io state-1)
        =/  pk-unit=(unit @ux)  (resolve-simple-pubkey state req-net)
        ?~  pk-unit  (pure:m ~)
        =/  pk=@ux  u.pk-unit
        ::  Walk change addresses from 0, refresh each, find first unused
        =/  det=(unit account-details)  (~(get by accounts.state) pk)
        ?~  det  (pure:m ~)
        =/  change-idx=@ud  0
        =/  net=network  active-network.u.det
        |-
        =/  change-addr=@t
          %:  derive-address-from-xkey:wallet-account
            k.extended-key.u.det  'change'  change-idx
            script-type.u.det  net
          ==
        ;<  data=json  bind:m  (fetch-address-data change-addr net)
        =/  chain-tc=(unit @ud)
          %-  mole  |.
          (ni:dejs:format (~(got jo:json-utils data) /'chain_stats'/'tx_count'))
        =/  mem-tc=(unit @ud)
          %-  mole  |.
          (ni:dejs:format (~(got jo:json-utils data) /'mempool_stats'/'tx_count'))
        ;<  ~  bind:m  (refresh-account-address:wallet-account pk 'change' change-idx)
        ?:  (gth (add (fall chain-tc 0) (fall mem-tc 0)) 0)
          $(change-idx +(change-idx))
        ::  Chain through existing send handlers:
        ::  clear-draft → add-output → set-change → auto-select → build
        ;<  ~  bind:m
          (handle-send-actions:rt-send pk ~[['action' 'clear-draft']])
        ;<  ~  bind:m
          %:  handle-send-actions:rt-send  pk
            :~  ['action' 'add-output']
                ['output-address' address]
                ['output-amount' amount]
            ==
          ==
        ;<  ~  bind:m
          %:  handle-send-actions:rt-send  pk
            :~  ['action' 'set-change-config']
                ['fee-rate' fee-rate]
                ['change-address' change-addr]
            ==
          ==
        ;<  ~  bind:m
          (handle-send-actions:rt-send pk ~[['action' 'run-auto-select']])
        ::  Snapshot input addresses before build clears the draft
        ;<  state=state-1  bind:m  (get-state-as:io state-1)
        =/  pre-det=(unit account-details)  (~(get by accounts.state) pk)
        =/  refresh-list=(list [chain=@t idx=@ud])
          ?~  pre-det  ~
          =/  ac  ~(. ac:wallet-account [u.pre-det active-network.u.pre-det])
          =/  drft  draft:ac
          ?~  drft
            ~&  >>>  "send-bitcoin: no draft found, empty refresh-list"
            ~
          ~&  >>  "send-bitcoin: draft has {<(lent inputs.u.drft)>} inputs"
          ~&  >>  "send-bitcoin: transactions map has {<~(wyt by transactions:ac)>} entries"
          ~&  >>  "send-bitcoin: address-cache has {<~(wyt by address-cache:ac)>} entries"
          ::  Resolve each input's address via tx data + address-cache
          =/  result=(list [chain=@t idx=@ud])
            %+  murn  inputs.u.drft
            |=  [txid=@t vout=@ud *]
            ^-  (unit [chain=@t idx=@ud])
            =/  tx=(unit transaction)  (~(get by transactions:ac) txid)
            ?~  tx
              ~&  >>>  "send-bitcoin: tx {<txid>} NOT in transactions map"
              ~
            ?.  (lth vout (lent outputs.u.tx))
              ~&  >>>  "send-bitcoin: vout {<vout>} out of range for tx {<txid>} (has {<(lent outputs.u.tx)>} outputs)"
              ~
            =/  addr=@t  address:(snag vout outputs.u.tx)
            ~&  >>  "send-bitcoin: input {<txid>}:{<vout>} -> addr {<addr>}"
            =/  suffix=(unit address-suffix:hd-path)
              (~(get by address-cache:ac) addr)
            ?~  suffix
              ~&  >>>  "send-bitcoin: addr {<addr>} NOT in address-cache"
              ~
            =/  [chain-num=@ud addr-idx=@ud]
              [q.change.u.suffix q.index.u.suffix]
            ~&  >  "send-bitcoin: resolved input -> {?:(=(0 chain-num) "receiving" "change")}/{<addr-idx>}"
            `[?:(=(0 chain-num) 'receiving' 'change') addr-idx]
          ~&  >>  "send-bitcoin: input refresh-list: {<result>}"
          result
        ::  Add the change address we selected
        ~&  >>  "send-bitcoin: adding change address at index {<change-idx>}"
        =/  refresh-list  (snoc refresh-list ['change' change-idx])
        ::  Add destination if it's ours
        =/  dest-suffix=(unit address-suffix:hd-path)
          ?~  pre-det  ~
          =/  ac  ~(. ac:wallet-account [u.pre-det active-network.u.pre-det])
          (~(get by address-cache:ac) address)
        =/  refresh-list
          ?~  dest-suffix  refresh-list
          =/  [chain-num=@ud addr-idx=@ud]
            [q.change.u.dest-suffix q.index.u.dest-suffix]
          ~&  >>  "send-bitcoin: destination is ours -> {?:(=(0 chain-num) "receiving" "change")}/{<addr-idx>}"
          (snoc refresh-list [?:(=(0 chain-num) 'receiving' 'change') addr-idx])
        ~&  >  "send-bitcoin: FINAL refresh-list: {<refresh-list>}"
        ::  Build, sign, broadcast
        ;<  ~  bind:m
          (handle-send-actions:rt-send pk ~[['action' 'build-transaction']])
        ::  Get the txid we just broadcast from broadcasts
        ;<  state=state-1  bind:m  (get-state-as:io state-1)
        =/  broadcast-txid=@t
          =/  pairs=(list [@t broadcast])  ~(tap by broadcasts.state)
          =/  best=[txid=@t sent=@da]  ['' *@da]
          |-
          ?~  pairs  txid.best
          =/  [txid=@t bc=broadcast]  i.pairs
          ?:  (gth sent.bc sent.best)
            $(pairs t.pairs, best [txid sent.bc])
          $(pairs t.pairs)
        ~&  >  "send-bitcoin: polling for txid {<broadcast-txid>} in {<(lent refresh-list)>} addresses"
        ::  Poll each address until the txid shows up in its transactions
        =/  max-polls=@ud  10
        =/  polls=@ud  0
        |-
        ?:  (gte polls max-polls)
          ~&  >>>  "send-bitcoin: gave up polling after {<max-polls>} attempts"
          (pure:m ~)
        ;<  ~  bind:m  (sleep:io ~s3)
        ::  Refresh all touched addresses
        =/  rem  refresh-list
        |-
        ?~  rem
          ::  Check if txid is now visible in any address's transactions
          ;<  state=state-1  bind:m  (get-state-as:io state-1)
          =/  det=(unit account-details)  (~(get by accounts.state) pk)
          ?~  det
            ~&  >>>  "send-bitcoin: account gone during poll"
            (pure:m ~)
          =/  ac  ~(. ac:wallet-account [u.det active-network.u.det])
          ?:  (~(has by transactions:ac) broadcast-txid)
            ~&  >  "send-bitcoin: txid confirmed in indexer after {<+(polls)>} polls"
            (pure:m ~)
          ~&  >>  "send-bitcoin: poll {<+(polls)>}/{<max-polls>}: txid not yet visible"
          ^$(polls +(polls))
        =/  [chain=@t idx=@ud]  i.rem
        ~&  >  "send-bitcoin: refreshing {<chain>}/{<idx>}"
        ;<  ~  bind:m  (refresh-account-address:wallet-account pk chain idx)
        $(rem t.rem)
        ::
          %get-receive-address
        ::  Find next unused address by checking mempool.space
        =/  req-net=@t  (fall (get-key:kv 'net' args) 'main')
        ;<  state=state-1  bind:m  (get-state-as:io state-1)
        =/  pk-unit=(unit @ux)  (resolve-simple-pubkey state req-net)
        ?~  pk-unit
          %-  give-simple-payload:io
          [[200 ~[['content-type' 'text/plain']]] `(as-octs:mimes:html '')]
        =/  det=(unit account-details)  (~(get by accounts.state) u.pk-unit)
        ?~  det
          %-  give-simple-payload:io
          [[200 ~[['content-type' 'text/plain']]] `(as-octs:mimes:html '')]
        ::  Check local state first to find candidate unused address
        ::  Only refresh from there forward (skip already-known used addresses)
        =/  pk=@ux  u.pk-unit
        =/  net=network  active-network.u.det
        =/  ac  ~(. ac:wallet-account [u.det active-network.u.det])
        =/  local-next=(unit @t)  (get-next-unused-address:wallet-address receiving:ac)
        ::  Find the index of the local candidate (or start at mop size)
        =/  idx=@ud
          ?~  local-next
            ::  No unused in local state — start at end of mop
            (lent (tap:((on @ud hd-leaf) gth) receiving:ac))
          ::  Find the index of the candidate address
          =/  leaves=(list [@ud hd-leaf])
            (tap:((on @ud hd-leaf) gth) receiving:ac)
          |-
          ?~  leaves  (lent (tap:((on @ud hd-leaf) gth) receiving:ac))
          =/  [lidx=@ud =hd-leaf]  i.leaves
          ?:  =(address.main.hd-leaf u.local-next)
            lidx
          $(leaves t.leaves)
        ::  Refresh from candidate index forward until confirmed unused
        |-
        ;<  ~  bind:m  (refresh-account-address:wallet-account pk 'receiving' idx)
        ;<  state=state-1  bind:m  (get-state-as:io state-1)
        =/  det=(unit account-details)  (~(get by accounts.state) pk)
        ?~  det
          %-  give-simple-payload:io
          [[200 ~[['content-type' 'text/plain']]] `(as-octs:mimes:html '')]
        =/  ac  ~(. ac:wallet-account [u.det active-network.u.det])
        =/  leaf=(unit hd-leaf)  (get:((on @ud hd-leaf) gth) receiving:ac idx)
        =/  has-txs=?
          ?~  leaf  %.n
          =/  info-unit  info.main.u.leaf
          ?~  info-unit  %.n
          ?|  (gth tx-count.u.info-unit 0)
              (gth mempool-funded.u.info-unit 0)
              (gth mempool-spent.u.info-unit 0)
          ==
        ?:  has-txs
          ::  Address got used since last check, try next
          $(idx +(idx))
        ::  Found unused address — get it from refreshed state
        =/  addr=@t
          ?~  leaf  ''
          address.main.u.leaf
        %-  give-simple-payload:io
        [[200 ~[['content-type' 'text/plain']]] `(as-octs:mimes:html addr)]
      ==
      ::
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
    ;<  state=state-1  bind:m  (get-state-as:io state-1)
    ;<  ~  bind:m  (ensure-default-wallet state)
    (set-bindings:io ~[binding.state [~ /apps/wallet] [~ /apps/wallet-1] [~ /apps/wallet-2]])
    ::
      %on-load :: sent by sailbox
    ;<  state=state-1  bind:m  (get-state-as:io state-1)
    ;<  ~  bind:m  (ensure-default-wallet state)
    (set-bindings:io ~[binding.state [~ /apps/wallet] [~ /apps/wallet-1] [~ /apps/wallet-2]])
    ::
      %set-binding
    =+  !<(new-binding=binding:eyre vase)
    ;<  state=state-1  bind:m  (get-state-as:io state-1)
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
  =+  !<(state-1 state)
  ?:  ?=([%spv-wallet %timer ~] site)
    (handle-test-pages-sse:ui bowl state site args id event)
  ?:  ?=([%spv-wallet %progress ~] site)
    =/  prog=(unit progress-info)
      ?~(boot ~ `(boot-progress u.boot))
    =/  err=(unit [term tang])
      ?~(boot ~ error.u.boot)
    (handle-progress-sse:ui prog err event)
  ?:  ?=([%spv-wallet %stream ~] site)
    ?:  =(`'address-offer-received' event)
      ::  Return the active send address from labels
      =/  st=state-1  !<(state-1 state)
      =/  active-addr=@t
        =/  addr-list=(list [@t (set label-entry:bip329)])
          ~(tap by addr.labels.st)
        |-
        ?~  addr-list  ''
        =/  [ref=@t entries=(set label-entry:bip329)]  i.addr-list
        =/  has-active=?
          %+  lien  ~(tap in entries)
          |=(e=label-entry:bip329 =('simple:send:active' label.e))
        ?:  has-active  ref
        $(addr-list t.addr-list)
      ~[active-addr]
    (handle-spv-sse:ui bowl state %main args id event)
  ?+    site  !!
      [%spv-wallet %stream %spv @ ~]
    =/  net-str=@t  i.t.t.t.site
    =/  net=network
      ?:  =('main' net-str)  %main
      ?:  =('testnet3' net-str)  %testnet3
      ?:  =('testnet4' net-str)  %testnet4
      %main
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
