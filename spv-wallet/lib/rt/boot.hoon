/-  s=spv-wallet, tx=transactions, urb
/+  io=sailboxio, html-utils, bip32=bip32-spv, btc=bitcoin,
    wallet-address, wallet-account, *wallet-mempool-space,
    taproot, txns=tx-build,
    bcu=bitcoin-utils, fees=tx-fees, sig=tx-sighash, urb-encoder,
    bscr=btc-script
|%
::  Boot network configuration
::  Change these to retarget the boot sequence.
::
++  boot-network    `network:tx`%signet
::  Pure helpers
::
++  sots-to-ptst
  |=  sots=(list sotx:urb)
  ^-  ptst:taproot
  =/  encoded=@  (encode:urb-encoder sots)
  =/  scr=hexb:btc  (en:bscr (unv-to-script:en:urb-encoder encoded))
  [%leaf [0xc0 scr]]
::
++  extract-spawn-fields
  |=  [=pass =input:ap:tx]
  =/  cic  (com:nu:cric:crypto pass)
  ?>  =(%c suite.+<.cic)
  =/  comet=@p  `@p`fig:ex:cic
  =/  script-pubkey=hexb:btc
    (build-script-pubkey:sig pubkey.input spend-type.input)
  =/  en-out  (can 3 script-pubkey 8^amount.input ~)
  =/  spkh  (shay (add 8 wid.script-pubkey) en-out)
  =/  sam  +<.cic
  ?>  ?=([%c [@ @ [@ @ @]] *] sam)
  =/  dat  dat.tw.pub.sam
  =/  offset  ;:(add 5 (met 3 txid.input) (met 3 vout.input))
  =/  off  (cut 3 [offset (sub (met 3 dat) offset)] dat)
  [comet spkh off]
::
++  build-spawn-ptst
  |=  [=pass fief=(unit fief:urb) =input:ap:tx]
  ^-  ptst:taproot
  =/  [comet=@p spkh=@ux off=@]  (extract-spawn-fields pass input)
  =/  sot=sotx:urb  [[comet ~] [%spawn `@`pass fief [spkh `vout.input off 0]]]
  (sots-to-ptst ~[sot])
::
++  build-batch-ptst
  |=  [=pass fief=(unit fief:urb) sponsor=ship sponsor-sig=@ =input:ap:tx]
  ^-  ptst:taproot
  =/  [comet=@p spkh=@ux off=@]  (extract-spawn-fields pass input)
  =/  spawn  [%spawn `@`pass fief [spkh `vout.input off 0]]
  =/  escape  [%escape sponsor `sponsor-sig]
  =/  sot=sotx:urb  [[comet ~] [%batch ~[spawn escape]]]
  (sots-to-ptst ~[sot])
::
::  State machine helpers
::
++  save-step
  |=  [next=@tas bd=boot-data:s]
  =/  m  (fiber:io ,~)
  ^-  form:m
  ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
  =.  boot.state  `[next bd ~]
  ;<  ~  bind:m  (replace:io !>(state))
  (send-sse-event:io /spv-wallet/progress ~ `'progress-update')
::
++  store-boot-error
  |=  [label=term trace=tang]
  =/  m  (fiber:io ,~)
  ^-  form:m
  ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
  ?~  boot.state
    (pure:m ~)
  =.  error.u.boot.state  `[label trace]
  ;<  ~  bind:m  (replace:io !>(state))
  (send-sse-event:io /spv-wallet/progress ~ `'progress-update')
::
::  Entry point: POST handler
::
++  handle-boot-actions
  |=  args=key-value-list:kv:html-utils
  =/  m  (fiber:io ,~)
  ^-  form:m
  =/  action=@t  (need (get-key:kv:html-utils 'action' args))
    ?+    action  !!
        %start
      ::  Refuse to start a new boot if the comet is already confirmed
      ::  on-chain or if the previous boot already finished and is just
      ::  waiting for urb-watcher to catch up.
      ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
      ;<  our=@p  bind:m  get-our:io
      ;<  =bowl:gall  bind:m  get-bowl:io
      =/  in-watcher=?
        ?.  .^(? %gu /(scot %p our)/urb-watcher/(scot %da now.bowl)/$)
          %.n
        =/  points=(map @p point:urb)
          .^((map @p point:urb) %gx /(scot %p our)/urb-watcher/(scot %da now.bowl)/points/urb-points)
        (~(has by points) our)
      ?:  in-watcher
        ~&  "boot[start]: refusing to start because comet is already present in urb-watcher"
        (pure:m ~)
      ?:  ?&(?=(^ boot.state) =(%done step.u.boot.state))
        ~&  "boot[start]: refusing to start because previous boot is pending confirmation"
        =.  error.u.boot.state
          `[ %spawn-pending
             ~['spawn transaction already broadcast; waiting for block confirmation']
           ]
        ;<  ~  bind:m  (replace:io !>(state))
        ;<  ~  bind:m  (send-sse-event:io /spv-wallet/progress ~ `'progress-update')
        (pure:m ~)
      ::  Parse seed phrase as @q
      =/  seed-text=@t  (need (get-key:kv:html-utils 'seed-phrase' args))
      =/  txt=tape  (trip seed-text)
    =/  with-sig=tape
      ?:(?=(~ txt) txt ?:(=('~' i.txt) (weld "." txt) (weld ".~" txt)))
    =/  parsed=(each @q tang)
      (mule |.(`@q`(slav %q (crip with-sig))))
    ?:  ?=(%| -.parsed)
      (store-boot-error %bad-seed p.parsed)
    =/  boot-secret=@q  p.parsed
    ::  Parse boot mode
    =/  mode-text=@t  (fall (get-key:kv:html-utils 'boot-mode' args) '')
    =/  fief=(unit fief:urb)
      ?.  =('fief' mode-text)  ~
      =/  ip-text=@t  (fall (get-key:kv:html-utils 'fief-ip' args) '')
      =/  port-text=@t  (fall (get-key:kv:html-utils 'fief-port' args) '')
      ?:  |(?=(~ (trip ip-text)) ?=(~ (trip port-text)))
        ~|(%empty-fief-fields !!)
      =/  ip-parsed=(each @if tang)  (mule |.((scan (trip ip-text) lip:ag)))
      ?:  ?=(%| -.ip-parsed)  ~|(%bad-fief-ip !!)
      =/  port-parsed=(each @ud tang)  (mule |.((scan (trip port-text) dum:ag)))
      ?:  ?=(%| -.port-parsed)  ~|(%bad-fief-port !!)
      `[%if p.ip-parsed p.port-parsed]
    ::  Build initial boot-data
    ?^  fief
      ::  Sponsor mode: set auto-sponsor flag
      ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
      =.  auto-sponsor.state  %.y
      ;<  ~  bind:m  (replace:io !>(state))
      =|  bd=boot-data:s
      =.  boot-mode.bd  %sponsor
      =.  boot-secret.bd  boot-secret
      =.  fief.bd  fief
      ;<  ~  bind:m  (save-step %start bd)
      run-boot
    ::  Normal mode: parse sponsor @p from form
    =/  sponsor-text=@t  (fall (get-key:kv:html-utils 'sponsor' args) '')
    ~&  "boot: sponsor field raw text: '{(trip sponsor-text)}'"
    =/  sponsor-parsed=(unit @p)  (slaw %p sponsor-text)
    ~&  "boot: sponsor parsed: {<sponsor-parsed>}"
    ?~  sponsor-parsed
      (store-boot-error %bad-sponsor ~['invalid sponsor @p' (crip "raw input: '{(trip sponsor-text)}'")])
    ~&  "boot: normal mode, sponsor={<u.sponsor-parsed>}"
    =|  bd=boot-data:s
    =.  boot-mode.bd  %normal
    =.  boot-secret.bd  boot-secret
    =.  sponsor.bd  `u.sponsor-parsed
    ;<  ~  bind:m  (save-step %start bd)
    run-boot
  ::
      %retry
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    ?~  boot.state
      (pure:m ~)
    =.  boot.state  `u.boot.state(error ~)
    ;<  ~  bind:m  (replace:io !>(state))
    ;<  ~  bind:m  (send-sse-event:io /spv-wallet/progress ~ `'progress-update')
    run-boot
  ::
      %cancel
    ~&  "boot[cancel]: canceling boot process"
    ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
    =.  boot.state  ~
    ;<  ~  bind:m  (replace:io !>(state))
    ;<  ~  bind:m  (send-sse-event:io /spv-wallet/progress ~ `'progress-update')
    (pure:m ~)
  ==
::
::  Dispatcher
::
++  run-boot
  =/  m  (fiber:io ,~)
  ^-  form:m
  ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
  ?~  boot.state
    (pure:m ~)
  =/  bd=boot-data:s  data.u.boot.state
  ~&  "boot[dispatch]: step={<step.u.boot.state>} mode={<boot-mode.bd>} sponsor={<sponsor.bd>}"
  ?+  step.u.boot.state  (pure:m ~)
    %start              (do-start bd)
    %derive             (do-derive bd)
    %address            (do-address bd)
    %fetch-utxos        (do-fetch-utxos bd)
    %discover-sponsor   (do-discover-sponsor bd)
    %sponsor            (do-sponsor bd)
    %poll-sponsor       (do-poll-sponsor bd)
    %attest             (do-attest bd)
    %commit             (do-commit bd)
    %confirm-commit     (do-confirm-commit bd)
    %reveal             (do-reveal bd)
    %confirm-reveal     (do-confirm-reveal bd)
    %refresh            (do-refresh bd)
  ==
::
::  Step handlers
::
++  do-start
  |=  bd=boot-data:s
  =/  m  (fiber:io ,~)
  ^-  form:m
  =/  result=(each [@ux wallet:s] tang)
    %-  mule  |.
    =/  pubkey=@ux  (seed-to-pubkey:wallet-address [%q boot-secret.bd])
    =/  new-wallet=wallet:s  ['Boot Wallet' [%q boot-secret.bd] pubkey ~ ~]
    [pubkey new-wallet]
  ?:  ?=(%| -.result)
    (store-boot-error %wallet-derivation p.result)
  =/  [pubkey=@ux new-wallet=wallet:s]  p.result
  ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
  =.  wallets.state  (~(put by wallets.state) pubkey new-wallet)
  ;<  ~  bind:m  (replace:io !>(state))
  ;<  ~  bind:m  (send-sse-event:io /spv-wallet/stream ~ `'wallet-list-update')
  ;<  ~  bind:m  (sleep:io ~s1)
  =/  bd  bd(wallet-pubkey `pubkey)
  ;<  ~  bind:m  (save-step %derive bd)
  (do-derive bd)
::
++  do-derive
  |=  bd=boot-data:s
  =/  m  (fiber:io ,~)
  ^-  form:m
  =/  pubkey=@ux  (need wallet-pubkey.bd)
  =/  result=(each [account-pubkey=@ux xprv=@t] tang)
    %-  mule  |.
    =/  master-wallet  (from-seed:bip32 (seed-to-bytes:wallet-address [%q boot-secret.bd]))
    =/  derived  (derive-path:master-wallet "m/86'/1'/0'")
    [public-key:derived (crip (prv-extended:derived (en-crypto:wallet-address boot-network)))]
  ?:  ?=(%| -.result)
    (store-boot-error %account-derivation p.result)
  =/  account-pubkey=@ux  account-pubkey.p.result
  =/  xprv=@t  xprv.p.result
  ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
  =/  wallet=(unit wallet:s)  (~(get by wallets.state) pubkey)
  ?~  wallet
    (store-boot-error %boot-wallet-missing ~['boot wallet not found in state'])
  =/  new-account=account:hd-path  [[%.y 86] [%.y 1] [%.y 0]]
  =/  new-account-details=account-details:s
    :*  'Boot Account'
        `pubkey
        [%xprv xprv]
        %p2tr
        boot-network
        ~
        [~ ~ ~ ~ ~]
        %.n
    ==
  =.  accounts.state  (~(put by accounts.state) account-pubkey new-account-details)
  =/  updated-wallet
    u.wallet(accounts (~(put by accounts.u.wallet) new-account account-pubkey))
  =.  wallets.state  (~(put by wallets.state) pubkey updated-wallet)
  ;<  ~  bind:m  (replace:io !>(state))
  ;<  ~  bind:m  (sleep:io ~s1)
  =/  bd  bd(account-pubkey `account-pubkey)
  ;<  ~  bind:m  (save-step %address bd)
  (do-address bd)
::
++  do-address
  |=  bd=boot-data:s
  =/  m  (fiber:io ,~)
  ^-  form:m
  =/  account-pubkey=@ux  (need account-pubkey.bd)
  =/  acct=account:hd-path  [[%.y 86] [%.y 1] [%.y 0]]
  =/  result=(each @t tang)
    %-  mule  |.
    (derive-address-at-index:wallet-address [%q boot-secret.bd] acct %receiving 0 (en-crypto:wallet-address boot-network))
  ?:  ?=(%| -.result)
    (store-boot-error %address-derivation p.result)
  =/  address=@t  p.result
  ~&  "derived boot address: {(trip address)}"
  ;<  addr-json=json  bind:m  (fetch-address-data address boot-network)
  =/  parsed-info=(unit address-info:s)  (parse-address-info address addr-json)
  ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
  =/  acct-details=account-details:s  (~(got by accounts.state) account-pubkey)
  ;<  now=@da  bind:m  get-time:io
  =/  new-addr=address-details:s  [address `now parsed-info ~ ~]
  =/  updated-acct=account-details:s
    (~(put-addr-full ac:wallet-account [acct-details boot-network]) 'receiving' 0 new-addr)
  =.  accounts.state  (~(put by accounts.state) account-pubkey updated-acct)
  ;<  ~  bind:m  (replace:io !>(state))
  ;<  ~  bind:m  (sleep:io ~s1)
  =/  bd  bd(address `address)
  ;<  ~  bind:m  (save-step %fetch-utxos bd)
  (do-fetch-utxos bd)
::
++  do-fetch-utxos
  |=  bd=boot-data:s
  =/  m  (fiber:io ,~)
  ^-  form:m
  =/  address=@t  (need address.bd)
  =/  account-pubkey=@ux  (need account-pubkey.bd)
  ;<  utxo-list=(list [txid=@t vout=@ud value=@ud tx-status=tx-status:s])  bind:m
    (fetch-utxos address boot-network)
  ~&  "found {(scow %ud (lent utxo-list))} UTXOs"
  ?~  utxo-list
    (store-boot-error %no-utxos ~['no UTXOs found at boot address' (crip "address: {(trip address)}")])
  ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
  =/  acct-details=account-details:s  (~(got by accounts.state) account-pubkey)
  =/  addr-unit=(unit address-details:s)
    (~(get-addr ac:wallet-account [acct-details boot-network]) 'receiving' 0)
  ?~  addr-unit
    (store-boot-error %address-missing ~['address not found in account after storing it'])
  =/  updated-addr=address-details:s  u.addr-unit(utxos utxo-list)
  =/  updated-acct=account-details:s
    (~(put-addr ac:wallet-account [acct-details boot-network]) 'receiving' 0 updated-addr)
  =.  accounts.state  (~(put by accounts.state) account-pubkey updated-acct)
  ;<  ~  bind:m  (replace:io !>(state))
  ;<  ~  bind:m  (sleep:io ~s1)
  ::  Select first UTXO (drop tx-status for boot-data)
  =/  bd  bd(selected-utxo `[txid vout value]:i.utxo-list)
  ::  Branch: sponsor mode skips to attest, normal mode requests sponsorship
  ~&  "boot[fetch-utxos]: branching, mode={<boot-mode.bd>}"
  ?:  ?=(%sponsor boot-mode.bd)
    ~&  "boot[fetch-utxos]: sponsor mode -> skipping to %attest"
    ;<  ~  bind:m  (save-step %attest bd)
    (do-attest bd)
  ~&  "boot[fetch-utxos]: normal mode -> going to %discover-sponsor, sponsor={<sponsor.bd>}"
  ;<  ~  bind:m  (save-step %discover-sponsor bd)
  (do-discover-sponsor bd)
::
++  do-discover-sponsor
  |=  bd=boot-data:s
  =/  m  (fiber:io ,~)
  ^-  form:m
  =/  sponsor=@p  (need sponsor.bd)
  ~&  "boot[discover]: waiting for sponsor {<sponsor>} fief in urb-watcher..."
  ;<  our=@p  bind:m  get-our:io
  |-
  ;<  =bowl:gall  bind:m  get-bowl:io
  =/  scry-base=tape
    "/{(scow %p our.bowl)}/urb-watcher/{(scow %da now.bowl)}"
  =/  watcher-running=?
    .^(? %gu /(scot %p our.bowl)/urb-watcher/(scot %da now.bowl)/$)
  ~&  "boot[discover]: scry %gu {scry-base}/$ -> {<watcher-running>}"
  ?.  watcher-running
    ~&  "boot[discover]: urb-watcher not running yet, retrying in 5s..."
    ;<  ~  bind:m  (send-sse-event:io /spv-wallet/progress ~ `'progress-update')
    ;<  ~  bind:m  (sleep:io ~s5)
    $
  =/  points=(map @p point:urb)
    .^((map @p point:urb) %gx /(scot %p our.bowl)/urb-watcher/(scot %da now.bowl)/points/urb-points)
  ~&  "boot[discover]: scry %gx {scry-base}/points -> {<~(wyt by points)>} points in map"
  =/  known-ships=(list @p)  ~(tap in ~(key by points))
  ~&  "boot[discover]: known ships: {<known-ships>}"
  =/  point=(unit point:urb)  (~(get by points) sponsor)
  ?~  point
    ~&  "boot[discover]: sponsor {<sponsor>} not in points map, retrying in 5s..."
    ;<  ~  bind:m  (send-sse-event:io /spv-wallet/progress ~ `'progress-update')
    ;<  ~  bind:m  (sleep:io ~s5)
    $
  ~&  "boot[discover]: sponsor {<sponsor>} found. life={<life.net.u.point>} pass={<pass.net.u.point>} sponsor={<sponsor.net.u.point>} fief={<fief.net.u.point>}"
  ?~  fief.net.u.point
    ~&  "boot[discover]: sponsor {<sponsor>} has no fief yet, retrying in 5s..."
    ;<  ~  bind:m  (send-sse-event:io /spv-wallet/progress ~ `'progress-update')
    ;<  ~  bind:m  (sleep:io ~s5)
    $
  ~&  "boot[discover]: sponsor {<sponsor>} fief confirmed: {<u.fief.net.u.point>}"
  ;<  ~  bind:m  (sleep:io ~s1)
  ;<  ~  bind:m  (save-step %sponsor bd)
  (do-sponsor bd)
::
++  do-sponsor
  |=  bd=boot-data:s
  =/  m  (fiber:io ,~)
  ^-  form:m
  =/  sponsor=@p  (need sponsor.bd)
  ~&  "boot[sponsor]: sponsor @p from boot-data: {<sponsor>}"
  ;<  our=@p  bind:m  get-our:io
  ~&  "boot[sponsor]: our={<our>}, sending request to [{<sponsor>} %spv-wallet]"
  ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
  =.  sponsor-response.state  ~
  ;<  ~  bind:m  (replace:io !>(state))
  ~&  "boot[sponsor]: firing poke to {<sponsor>} %spv-wallet %fiber-poke %sponsorship-request"
  ;<  ~  bind:m
    (poke:io [sponsor %spv-wallet] %fiber-poke !>(['sponsor-req' %sponsorship-request our]))
  ~&  "boot[sponsor]: poke sent, polling for response..."
  ;<  ~  bind:m  (save-step %poll-sponsor bd)
  (do-poll-sponsor bd)
::
++  do-poll-sponsor
  |=  bd=boot-data:s
  =/  m  (fiber:io ,~)
  ^-  form:m
  =/  polls=@ud  0
  |-
  ?:  (gte polls 30)
    (store-boot-error %sponsor-timeout ~['sponsor did not respond within 60s'])
  ;<  ~  bind:m  (sleep:io ~s2)
  ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
  ?~  sponsor-response.state
    ~&  "sponsor poll {(scow %ud polls)}: waiting..."
    $(polls +(polls))
  ~&  "boot[poll-sponsor]: response received! sig={<sig.u.sponsor-response.state>} height={<height.u.sponsor-response.state>}"
  =/  bd  bd(sponsor-sig `sig.u.sponsor-response.state)
  ;<  ~  bind:m  (save-step %attest bd)
  (do-attest bd)
::
++  do-attest
  |=  bd=boot-data:s
  =/  m  (fiber:io ,~)
  ^-  form:m
  =/  sel=[txid=@t vout=@ud value=@ud]  (need selected-utxo.bd)
  ;<  our=@p  bind:m  get-our:io
  ;<  deed=[=life =pass sec=(unit @ux)]  bind:m
    (scry:io ,[life pass (unit @ux)] %j /deed/(scot %p our)/1)
  ::  Build ptst: spawn-only for sponsor mode, batch spawn+escape for normal
  ~&  "boot[attest]: mode={<boot-mode.bd>} sponsor={<sponsor.bd>} sig={<sponsor-sig.bd>}"
  =/  result=(each ptst:taproot tang)
    %-  mule  |.
    =/  master-wallet  (from-seed:bip32 (seed-to-bytes:wallet-address [%q boot-secret.bd]))
    =/  derived  (derive-path:master-wallet "m/86'/1'/0'/0/0")
    =/  boot-pubkey=@ux  (ser-p:derived pub.derived)
    =/  txid-display=@ux  (rash txid.sel hex)
    =/  txid-le=@ux  dat:(flip:byt:bcu [32 txid-display])
    =/  inp=input:ap:tx
      :*  0x0  boot-pubkey  txid-le
          vout.sel  value.sel
          `@ud`0xffff.ffff  [%p2tr %key-path ~]
      ==
    ?:  ?=(%sponsor boot-mode.bd)
      (build-spawn-ptst pass.deed fief.bd inp)
    (build-batch-ptst pass.deed ~ (need sponsor.bd) (need sponsor-sig.bd) inp)
  ?:  ?=(%| -.result)
    (store-boot-error %attestation-build p.result)
  =/  spawn-ptst=ptst:taproot  p.result
  ?>  ?=(%leaf -.spawn-ptst)
  =/  spawn-script=hexb:btc  script.tapleaf.spawn-ptst
  ~&  "script built: {<wid.spawn-script>} bytes"
  ::  Derive commit address internal key at m/86'/1'/0'/0/1
  =/  commit-key-result=(each @ux tang)
    %-  mule  |.
    =/  master-wallet  (from-seed:bip32 (seed-to-bytes:wallet-address [%q boot-secret.bd]))
    =/  derived  (derive-path:master-wallet "m/86'/1'/0'/0/1")
    public-key:derived
  ?:  ?=(%| -.commit-key-result)
    (store-boot-error %commit-key-derivation p.commit-key-result)
  =/  commit-pubkey=@ux  p.commit-key-result
  =/  commit-address=@t  (tapscript-address:taproot commit-pubkey spawn-ptst (en-crypto:wallet-address boot-network))
  ~&  "commit address: {(trip commit-address)}"
  ::  Store index 1: main address + spawn tapscript
  =/  account-pubkey=@ux  (need account-pubkey.bd)
  =/  acct=account:hd-path  [[%.y 86] [%.y 1] [%.y 0]]
  ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
  =/  acct-details=account-details:s  (~(got by accounts.state) account-pubkey)
  =/  main-addr-1=(each @t tang)
    %-  mule  |.
    (derive-address-at-index:wallet-address [%q boot-secret.bd] acct %receiving 1 (en-crypto:wallet-address boot-network))
  ?:  ?=(%| -.main-addr-1)
    (store-boot-error %addr-1-derivation p.main-addr-1)
  ;<  now=@da  bind:m  get-time:io
  =/  addr-1-details=address-details:s  [p.main-addr-1 `now ~ ~ ~]
  =/  acct-with-1=account-details:s
    (~(put-addr-full ac:wallet-account [acct-details boot-network]) 'receiving' 1 addr-1-details)
  =/  acct-with-tapscript=account-details:s
    %:  ~(add-tapscript ac:wallet-account [acct-with-1 boot-network])
      'receiving'  1  commit-address  'spawn'  spawn-ptst
    ==
  =.  accounts.state  (~(put by accounts.state) account-pubkey acct-with-tapscript)
  ;<  ~  bind:m  (replace:io !>(state))
  ;<  ~  bind:m  (sleep:io ~s1)
  =/  bd
    %=  bd
      spawn-script      `dat.spawn-script
      spawn-script-wid  `wid.spawn-script
      commit-address    `commit-address
    ==
  ;<  ~  bind:m  (save-step %commit bd)
  (do-commit bd)
::
++  do-commit
  |=  bd=boot-data:s
  =/  m  (fiber:io ,~)
  ^-  form:m
  =/  sel=[txid=@t vout=@ud value=@ud]  (need selected-utxo.bd)
  =/  commit-address=@t  (need commit-address.bd)
  ::  Derive boot address private key at m/86'/1'/0'/0/0
  =/  result=(each [privkey=@ux boot-pubkey=@ux] tang)
    %-  mule  |.
    =/  master-wallet  (from-seed:bip32 (seed-to-bytes:wallet-address [%q boot-secret.bd]))
    =/  derived  (derive-path:master-wallet "m/86'/1'/0'/0/0")
    [prv.derived (ser-p:derived pub.derived)]
  ?:  ?=(%| -.result)
    (store-boot-error %boot-key-derivation p.result)
  =/  boot-privkey=@ux  privkey.p.result
  =/  boot-pubkey=@ux  boot-pubkey.p.result
  ::  Calculate fee: 1 P2TR input + 1 P2TR output + overhead
  =/  fee-rate=@ud  2
  =/  vbytes=@ud
    %+  add  overhead-vbytes:fees
    (add (input-vbytes:fees %p2tr) (output-vbytes:fees %p2tr))
  =/  fee=@ud  (calculate-fee:fees vbytes fee-rate)
  ?:  (lte value.sel fee)
    (store-boot-error %insufficient-funds ~['boot UTXO value too low for commit tx fee'])
  =/  commit-value=@ud  (sub value.sel fee)
  ~&  "commit tx: {<value.sel>} sats -> {<commit-value>} sats + {<fee>} fee"
  =/  txid-display=@ux  (rash txid.sel hex)
  =/  txid-le=@ux  dat:(flip:byt:bcu [32 txid-display])
  =/  tx-input=input:ap:tx
    :*  boot-privkey
        boot-pubkey
        txid-le
        vout.sel
        value.sel
        `@ud`0xffff.ffff
        [%p2tr %key-path ~]
    ==
  =/  build-result=(each tape tang)
    %-  mule  |.
    (build-transaction:txns boot-network 2 ~[tx-input] ~[[commit-address commit-value]] 0)
  ?:  ?=(%| -.build-result)
    (store-boot-error %commit-tx-build p.build-result)
  =/  tx-hex-cord=@t  (crip p.build-result)
  ~&  "commit tx built: {(scow %ud (lent p.build-result))} hex chars"
  =/  =request:http
    :*  %'POST'
        (crip (tx-base-url boot-network))
        ~[['content-type' 'text/plain']]
        `(as-octs:mimes:html tx-hex-cord)
    ==
  ;<  ~                      bind:m  (send-request:io request)
  ;<  =client-response:iris  bind:m  take-client-response:io
  =/  broadcast-result=@t
    ?+  client-response  'broadcast-failed'
      [%finished * [~ [* [p=@ q=@]]]]
    q.data.u.full-file.client-response
    ==
  ~&  "commit broadcast result: {(trip broadcast-result)}"
  ?:  !=(64 (met 3 broadcast-result))
    (store-boot-error %broadcast-failed ~['broadcast did not return a valid txid' broadcast-result])
  =/  bd  bd(commit-txid `broadcast-result)
  ;<  ~  bind:m  (save-step %confirm-commit bd)
  (do-confirm-commit bd)
::
++  do-confirm-commit
  |=  bd=boot-data:s
  =/  m  (fiber:io ,~)
  ^-  form:m
  =/  commit-txid=@t  (need commit-txid.bd)
  ~&  "polling for commit txid: {(trip commit-txid)}"
  =/  poll-url=@t  (crip :(weld (tx-base-url boot-network) "/" (trip commit-txid)))
  =/  polls=@ud  0
  |-
  ?:  (gte polls 30)
    (store-boot-error %commit-timeout ~['commit tx not found in mempool after 60s'])
  ;<  ~  bind:m  (sleep:io ~s2)
  =/  poll-req=request:http  [%'GET' poll-url ~ ~]
  ;<  ~                      bind:m  (send-request:io poll-req)
  ;<  =client-response:iris  bind:m  take-client-response:io
  =/  found=?
    ?&  ?=([%finished *] client-response)
        =(200 status-code.response-header.client-response)
    ==
  ?.  found
    ~&  "poll {(scow %ud polls)}: not yet in mempool, retrying..."
    $(polls +(polls))
  ~&  "commit tx confirmed in mempool after {(scow %ud polls)} polls"
  ;<  ~  bind:m  (save-step %reveal bd)
  (do-reveal bd)
::
++  do-reveal
  |=  bd=boot-data:s
  =/  m  (fiber:io ,~)
  ^-  form:m
  =/  sel=[txid=@t vout=@ud value=@ud]  (need selected-utxo.bd)
  =/  commit-address=@t  (need commit-address.bd)
  =/  commit-txid=@t  (need commit-txid.bd)
  =/  spawn-script=hexb:btc  [(need spawn-script-wid.bd) (need spawn-script.bd)]
  ::  Recompute commit-value (same fee formula as do-commit)
  =/  fee-rate=@ud  2
  =/  commit-fee=@ud
    %-  calculate-fee:fees
    :_  fee-rate
    %+  add  overhead-vbytes:fees
    (add (input-vbytes:fees %p2tr) (output-vbytes:fees %p2tr))
  =/  commit-value=@ud  (sub value.sel commit-fee)
  ::  Derive commit address private key at m/86'/1'/0'/0/1
  =/  result=(each [privkey=@ux commit-pub=@ux] tang)
    %-  mule  |.
    =/  master-wallet  (from-seed:bip32 (seed-to-bytes:wallet-address [%q boot-secret.bd]))
    =/  derived  (derive-path:master-wallet "m/86'/1'/0'/0/1")
    [prv.derived (ser-p:derived pub.derived)]
  ?:  ?=(%| -.result)
    (store-boot-error %reveal-key-derivation p.result)
  =/  commit-privkey=@ux  privkey.p.result
  =/  commit-pub=@ux  commit-pub.p.result
  ::  Verify reconstructed address matches
  =/  verify-ptst=ptst:taproot  [%leaf 0xc0 spawn-script]
  =/  verify-address=@t  (tapscript-address:taproot commit-pub verify-ptst (en-crypto:wallet-address boot-network))
  ?:  !=(verify-address commit-address)
    (store-boot-error %address-mismatch ~['reconstructed address does not match' verify-address commit-address])
  ::  Calculate reveal fee (script-path spend is heavier)
  =/  input-weight=@ud  (add 265 wid.spawn-script)
  =/  input-vb=@ud  (div (add input-weight 3) 4)
  =/  reveal-vbytes=@ud
    ;:(add overhead-vbytes:fees input-vb (output-vbytes:fees %p2tr))
  =/  reveal-fee=@ud  (calculate-fee:fees reveal-vbytes fee-rate)
  ?:  (lte commit-value reveal-fee)
    (store-boot-error %reveal-insufficient ~['commit output too small for reveal fee'])
  =/  reveal-value=@ud  (sub commit-value reveal-fee)
  ?:  (lth reveal-value 330)
    (store-boot-error %reveal-dust ~['reveal output would be dust (<330 sats); need a larger boot UTXO'])
  ~&  "reveal tx: {<commit-value>} sats -> {<reveal-value>} sats + {<reveal-fee>} fee ({<reveal-vbytes>} vB)"
  =/  commit-txid-display=@ux  (rash commit-txid hex)
  =/  commit-txid-le=@ux  dat:(flip:byt:bcu [32 commit-txid-display])
  =/  reveal-input=input:ap:tx
    :*  commit-privkey
        commit-pub
        commit-txid-le
        0
        commit-value
        `@ud`0xffff.ffff
        [%p2tr %script-path [0xc0 spawn-script] ~ ~]
    ==
  ::  Derive reveal destination address at index 2
  =/  acct=account:hd-path  [[%.y 86] [%.y 1] [%.y 0]]
  =/  addr-result=(each @t tang)
    %-  mule  |.
    (derive-address-at-index:wallet-address [%q boot-secret.bd] acct %receiving 2 (en-crypto:wallet-address boot-network))
  ?:  ?=(%| -.addr-result)
    (store-boot-error %reveal-addr-derivation p.addr-result)
  =/  reveal-address=@t  p.addr-result
  ~&  "reveal destination address (idx 2): {(trip reveal-address)}"
  ::  Store index 2 in account
  =/  account-pubkey=@ux  (need account-pubkey.bd)
  ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
  =/  acct-details=account-details:s  (~(got by accounts.state) account-pubkey)
  ;<  now=@da  bind:m  get-time:io
  =/  addr-2-details=address-details:s  [reveal-address `now ~ *sh-tx-history:indexer:s ~]
  =/  acct-with-2=account-details:s
    (~(put-addr-full ac:wallet-account [acct-details boot-network]) 'receiving' 2 addr-2-details)
  =.  accounts.state  (~(put by accounts.state) account-pubkey acct-with-2)
  ;<  ~  bind:m  (replace:io !>(state))
  ::  Build + broadcast reveal tx
  =/  build-result=(each tape tang)
    %-  mule  |.
    (build-transaction:txns boot-network 2 ~[reveal-input] ~[[reveal-address reveal-value]] 0)
  ?:  ?=(%| -.build-result)
    (store-boot-error %reveal-tx-build p.build-result)
  =/  reveal-hex-cord=@t  (crip p.build-result)
  ~&  "reveal tx built: {(scow %ud (lent p.build-result))} hex chars"
  =/  reveal-req=request:http
    :*  %'POST'
        (crip (tx-base-url boot-network))
        ~[['content-type' 'text/plain']]
        `(as-octs:mimes:html reveal-hex-cord)
    ==
  ;<  ~                      bind:m  (send-request:io reveal-req)
  ;<  =client-response:iris  bind:m  take-client-response:io
  =/  reveal-result=@t
    ?+  client-response  'reveal-broadcast-failed'
      [%finished * [~ [* [p=@ q=@]]]]
    q.data.u.full-file.client-response
    ==
  ~&  "reveal broadcast result: {(trip reveal-result)}"
  ?:  !=(64 (met 3 reveal-result))
    (store-boot-error %reveal-broadcast-failed ~['reveal broadcast did not return a valid txid' reveal-result])
  =/  bd  bd(reveal-address `reveal-address, reveal-txid `reveal-result)
  ;<  ~  bind:m  (save-step %confirm-reveal bd)
  (do-confirm-reveal bd)
::
++  do-confirm-reveal
  |=  bd=boot-data:s
  =/  m  (fiber:io ,~)
  ^-  form:m
  =/  reveal-txid=@t  (need reveal-txid.bd)
  ~&  "polling for reveal txid: {(trip reveal-txid)}"
  =/  poll-url=@t  (crip :(weld (tx-base-url boot-network) "/" (trip reveal-txid)))
  =/  polls=@ud  0
  |-
  ?:  (gte polls 30)
    (store-boot-error %reveal-timeout ~['reveal tx not found in mempool after 60s'])
  ;<  ~  bind:m  (sleep:io ~s2)
  =/  poll-req=request:http  [%'GET' poll-url ~ ~]
  ;<  ~                      bind:m  (send-request:io poll-req)
  ;<  =client-response:iris  bind:m  take-client-response:io
  =/  found=?
    ?&  ?=([%finished *] client-response)
        =(200 status-code.response-header.client-response)
    ==
  ?.  found
    ~&  "boot[confirm-reveal]: poll {(scow %ud polls)}: not yet in mempool, retrying..."
    $(polls +(polls))
  ~&  "boot[confirm-reveal]: tx confirmed in mempool after {(scow %ud polls)} polls"
  ;<  ~  bind:m  (save-step %refresh bd)
  (do-refresh bd)
::
++  do-refresh
  |=  bd=boot-data:s
  =/  m  (fiber:io ,~)
  ^-  form:m
  =/  address=@t  (need address.bd)
  =/  commit-address=@t  (need commit-address.bd)
  =/  reveal-address=@t  (need reveal-address.bd)
  =/  account-pubkey=@ux  (need account-pubkey.bd)
  ::  Refresh boot address (index 0)
  ;<  addr0-json=json  bind:m  (fetch-address-data address boot-network)
  =/  addr0-info=(unit address-info:s)  (parse-address-info address addr0-json)
  ;<  addr0-utxos=(list [txid=@t vout=@ud value=@ud tx-status=tx-status:s])  bind:m
    (fetch-utxos address boot-network)
  ::  Refresh commit tapscript address (index 1)
  ;<  commit-json=json  bind:m  (fetch-address-data commit-address boot-network)
  =/  commit-info=(unit address-info:s)  (parse-address-info commit-address commit-json)
  ;<  commit-utxos=(list [txid=@t vout=@ud value=@ud tx-status=tx-status:s])  bind:m
    (fetch-utxos commit-address boot-network)
  ::  Refresh reveal address (index 2)
  ;<  addr2-json=json  bind:m  (fetch-address-data reveal-address boot-network)
  =/  addr2-info=(unit address-info:s)  (parse-address-info reveal-address addr2-json)
  ;<  addr2-utxos=(list [txid=@t vout=@ud value=@ud tx-status=tx-status:s])  bind:m
    (fetch-utxos reveal-address boot-network)
  ::  Update all addresses in state
  ;<  state=state-0:s  bind:m  (get-state-as:io state-0:s)
  =/  acct-details=account-details:s  (~(got by accounts.state) account-pubkey)
  ::  Update index 0 (boot address)
  =/  a0=(unit address-details:s)
    (~(get-addr ac:wallet-account [acct-details boot-network]) 'receiving' 0)
  =/  acct-1=account-details:s
    ?~  a0  acct-details
    (~(put-addr ac:wallet-account [acct-details boot-network]) 'receiving' 0 u.a0(info addr0-info, utxos addr0-utxos))
  ::  Update index 1 commit tapscript
  =/  leaf-1=(unit hd-leaf:s)
    (~(get-leaf ac:wallet-account [acct-1 boot-network]) 'receiving' 1)
  =/  acct-2=account-details:s
    ?~  leaf-1  acct-1
    =/  ts-unit=(unit tapscript-details:s)
      (~(get by script-trees.u.leaf-1) commit-address)
    ?~  ts-unit  acct-1
    =/  updated-ts=tapscript-details:s
      u.ts-unit(address-details address-details.u.ts-unit(info commit-info, utxos commit-utxos))
    =/  updated-leaf=hd-leaf:s
      u.leaf-1(script-trees (~(put by script-trees.u.leaf-1) commit-address updated-ts))
    (~(put-leaf ac:wallet-account [acct-1 boot-network]) 'receiving' 1 updated-leaf)
  ::  Update index 2 (reveal address)
  =/  a2=(unit address-details:s)
    (~(get-addr ac:wallet-account [acct-2 boot-network]) 'receiving' 2)
  =/  acct-3=account-details:s
    ?~  a2  acct-2
    (~(put-addr ac:wallet-account [acct-2 boot-network]) 'receiving' 2 u.a2(info addr2-info, utxos addr2-utxos))
  =.  accounts.state  (~(put by accounts.state) account-pubkey acct-3)
  ;<  ~  bind:m  (replace:io !>(state))
  ;<  ~  bind:m  (send-sse-event:io /spv-wallet/stream ~ `'wallet-list-update')
  (save-step %done bd)
--
