/-  mcp, spider, s=spv-wallet
/+  io=strandio
=,  strand-fail=strand-fail:strand:spider
|%
++  respond-text
  |=  msg=@t
  %-  pairs:enjs:format
  :~  ['type' s+'text']
      ['text' s+msg]
  ==
++  expect-string
  |=  [args=(map name:parameter:tool:mcp argument:tool:mcp) key=@tas]
  ^-  @t
  =/  arg  (~(get by args) key)
  ?~  arg  (strand-fail key ~)
  ?>  ?=([%string @t] u.arg)
  p.u.arg
++  optional-string
  |=  [args=(map name:parameter:tool:mcp argument:tool:mcp) key=@tas]
  ^-  (unit @t)
  =/  arg  (~(get by args) key)
  ?~  arg  ~
  ?>  ?=([%string @t] u.arg)
  `p.u.arg
++  expect-number
  |=  [args=(map name:parameter:tool:mcp argument:tool:mcp) key=@tas]
  ^-  @ud
  =/  arg  (~(get by args) key)
  ?~  arg  (strand-fail key ~)
  ?>  ?=([%number @ud] u.arg)
  p.u.arg
++  optional-number
  |=  [args=(map name:parameter:tool:mcp argument:tool:mcp) key=@tas]
  ^-  (unit @ud)
  =/  arg  (~(get by args) key)
  ?~  arg  ~
  ?>  ?=([%number @ud] u.arg)
  `p.u.arg
++  expect-boolean
  |=  [args=(map name:parameter:tool:mcp argument:tool:mcp) key=@tas]
  ^-  ?
  =/  arg  (~(get by args) key)
  ?~  arg  (strand-fail key ~)
  ?>  ?=([%boolean ?] u.arg)
  p.u.arg
++  optional-boolean
  |=  [args=(map name:parameter:tool:mcp argument:tool:mcp) key=@tas default=?]
  ^-  ?
  =/  arg  (~(get by args) key)
  ?~  arg  default
  ?>  ?=([%boolean ?] u.arg)
  p.u.arg
++  ud-text
  |=  val=@ud
  ^-  @t
  (crip (scow %ud val))
++  hex-text
  |=  val=@ux
  ^-  @t
  (crip (scow %ux val))
++  bool-word
  |=  flag=?
  ^-  @t
  ?:  flag  'yes'  'no'
++  bool-text
  |=  flag=?
  ^-  @t
  ?:  flag  'true'  'false'
++  sanitize-pubkey
  |=  raw=@t
  ^-  @t
  =/  tape=tape  (cass (trip raw))
  =/  trimmed=tape
    ?:  &((gte (lent tape) 2) =((scag 2 tape) "0x"))
      (slag 2 tape)
    tape
  (crip trimmed)
++  lower-text
  |=  txt=@t
  ^-  @t
  (crip (cass (trip txt)))
++  url-encode
  |=  txt=@t
  ^-  @t
  =/  digits=tape  "0123456789ABCDEF"
  =/  go
    |=  [chars=tape acc=tape]
    ^-  tape
    ?~  chars  acc
    =/  ch=@t  i.chars
    =/  code=@ud  ch
    =/  allowed=?
      |(  &((gte code 'a') (lte code 'z'))
          &((gte code 'A') (lte code 'Z'))
          &((gte code '0') (lte code '9'))
          =(code '-')
          =(code '_')
          =(code '.')
          =(code '~')
       )
    =/  next=tape
      ?:  allowed
        (snoc acc ch)
      =/  hi=@ud  (div code 16)
      =/  lo=@ud  (mod code 16)
      (weld acc :~ '%' (snag hi digits) (snag lo digits) ==)
    $(chars t.chars, acc next)
  (crip (go (trip txt) ~))
++  join-with-newlines
  |=  lines=(list @t)
  ^-  @t
  =/  go
    |=  [lst=(list @t) acc=tape]
    ^-  tape
    ?~  lst  acc
    =/  line=tape  (trip i.lst)
    =/  next-acc=tape
      ?:  =(acc ~)  line
      (weld acc (weld "\n" line))
    $(lst t.lst, acc next-acc)
  (crip (go lines ~))
++  form-body
  |=  fields=(list [@t @t])
  ^-  @t
  =/  pairs=(list tape)
    %+  turn  fields
    |=  [name=@t value=@t]
    =/  enc-name=tape  (trip (url-encode name))
    =/  enc-value=tape  (trip (url-encode value))
    (weld enc-name (weld "=" enc-value))
  =/  go
    |=  [lst=(list tape) acc=tape]
    ^-  tape
    ?~  lst  acc
    =/  part=tape  i.lst
    =/  next=tape
      ?:  =(acc ~)  part
      (weld acc (weld "&" part))
    $(lst t.lst, acc next)
  (crip (go pairs ~))
++  absolute-url
  |=  path=@t
  ^-  @t
  (crip (weld "http://localhost" (trip path)))
++  wallet-path
  |=  pubkey=@t
  ^-  @t
  =/  hex=@t  (sanitize-pubkey pubkey)
  (crip (weld "/spv-wallet/wallet/" (trip hex)))
++  account-path
  |=  pubkey=@t
  ^-  @t
  =/  hex=@t  (sanitize-pubkey pubkey)
  (crip (weld "/spv-wallet/account/" (trip hex)))
++  send-path
  |=  pubkey=@t
  ^-  @t
  (crip (weld (trip (account-path pubkey)) "/send"))
++  http-post
  |=  [path=@t fields=(list [@t @t])]
  =/  m  (strand:spider ,[status=@ud body=@t])
  ^-  form:m
  =/  url=@t  (absolute-url path)
  =/  body-text=@t  (form-body fields)
  =/  =request:http
    :*  %'POST'
        url
        ~[['content-type' 'application/x-www-form-urlencoded']]
        `(as-octs:mimes:html body-text)
  ==
  ;<  ~                      bind:m  (send-request:io request)
  ;<  resp=client-response:iris  bind:m  take-client-response:io
  =/  status=@ud
    ?:  ?=([%finished *] resp)
      status-code.response-header.resp
    0
  ;<  cord=cord  bind:m  (extract-body:io resp)
  (pure:m [status cord])
++  http-get
  |=  path=@t
  =/  m  (strand:spider ,[status=@ud body=@t])
  ^-  form:m
  =/  url=@t  (absolute-url path)
  =/  request=http  [%'GET' url ~ ~]
  ;<  ~                      bind:m  (send-request:io request)
  ;<  resp=client-response:iris  bind:m  take-client-response:io
  =/  status=@ud
    ?:  ?=([%finished *] resp)
      status-code.response-header.resp
    0
  ;<  cord=cord  bind:m  (extract-body:io resp)
  (pure:m [status cord])
++  format-http-response
  |=  [status=@ud body=@t label=@t]
  ^-  json
  =/  stat=@t
    ?:  =(status 0)
      'unknown'
    (ud-text status)
  =/  text=@t
    (crip (rap 3 label " (status " stat ")\n\n" body ~))
  (respond-text text)
++  get-state
  =/  m  (strand:spider ,state-0:s)
  ^-  form:m
  ;<  st=state-0:s  bind:m  (scry:io state-0:s [%x %dbug %state ~])
  (pure:m st)
++  script-type-text
  |=  st=script-type:s
  ^-  @t
  ?-    st
      %p2pkh        'p2pkh'
      %p2sh-p2wpkh  'p2sh-p2wpkh'
      %p2wpkh       'p2wpkh'
      %p2tr         'p2tr'
  ==
++  network-text
  |=  net=network:s
  ^-  @t
  ?-    net
      %main      'main'
      %testnet3  'testnet3'
      %testnet4  'testnet4'
      %signet    'signet'
      %regtest   'regtest'
  ==
++  wallet-lines
  |=  wallets=(map @ux wallet:s)
  ^-  (list @t)
  %+  turn  ~(tap by wallets)
  |=  [pubkey=@ux wal=wallet:s]
  ^-  @t
  =/  acct-count=@ud  (lent ~(tap by accounts.wal))
  (crip (rap 3 (trip name.wal) ' — pubkey ' (hex-text pubkey) ', accounts ' (ud-text acct-count) ~))
++  account-lines
  |=  [keys=(set @ux) accounts=(map @ux account-details:s)]
  ^-  (list @t)
  %+  murn  ~(tap in keys)
  |=  pubkey=@ux
  ^-  (unit @t)
  =/  details=(unit account-details:s)  (~(get by accounts) pubkey)
  ?~  details  ~
  ` (crip (rap 3 (trip name.u.details) ' — pubkey ' (hex-text pubkey)
        ' — ' (script-type-text script-type.u.details)
        ' / ' (network-text active-network.u.details)
        ' — indexer ' (bool-word indexer-registered.u.details)
        ~))
++  list-wallets-summary
  |=  st=state-0:s
  ^-  @t
  =/  full  (wallet-lines wallets.st)
  =/  watch  (account-lines [watch-only.st accounts.st])
  =/  signing  (account-lines [signing.st accounts.st])
  =/  parts=(list @t)
    :~  (crip (rap 3 'Full wallets (' (ud-text (lent full)) '):' ~))
        (join-with-newlines full)
        (crip (rap 3 '\n\nWatch-only accounts (' (ud-text (lent watch)) '):' ~))
        (join-with-newlines watch)
        (crip (rap 3 '\n\nSigning accounts (' (ud-text (lent signing)) '):' ~))
        (join-with-newlines signing)
    ==
  (join-with-newlines parts)
++  find-wallet-by-name
  |=  [st=state-0:s target=@t]
  ^-  (unit [pubkey=@ux wallet:wallet:s])
  =/  entries=(list [@ux wallet:s])  ~(tap by wallets.st)
  |-  ^-  (unit [@ux wallet:wallet:s])
  ?~  entries  ~
  =/  entry=[@ux wallet:s]  i.entries
  ?:  =(target name.entry)
    `[entry]
  $(entries t.entries)
++  find-account
  |=  [st=state-0:s pubkey=@t]
  ^-  (unit account-details:s)
  =/  key=(unit @ux)  (slaw %ux (crip (weld "0x" (trip (sanitize-pubkey pubkey)))))
  ?~  key  ~
  (~(get by accounts.st) u.key)
++  format-seed
  |=  =seed:s
  ^-  @t
  ?-    seed
      [%q q=@q]  (scot %q q)
      [%t t=@t]  t
  ==
--
^-  (list tool:mcp)
:~
  :*  'spv__list-wallets'
      'List full wallets plus watch-only and signing accounts (seeds are never exposed).'
      %-  my  ~
      ~
      ^-  thread-builder:tool:mcp
      |=  args=(map name:parameter:tool:mcp argument:tool:mcp)
      ^-  shed:khan
      =/  m  (strand:spider ,vase)
      ^-  form:m
      ;<  st=state-0:s  bind:m  get-state
      %-  pure:m
      !>  (respond-text (list-wallets-summary st))
  ==
  ::
  :*  'spv__generate-wallet'
      'Generate a new wallet from entropy and return the seed phrase exactly once (store it safely).'
      %-  my
      :~  ['wallet-name' [%string 'Label for the new wallet.']]
      ==
      ~['wallet-name']
      ^-  thread-builder:tool:mcp
      |=  args=(map name:parameter:tool:mcp argument:tool:mcp)
      ^-  shed:khan
      =/  m  (strand:spider ,vase)
      ^-  form:m
      =/  wallet-name=@t  (expect-string args 'wallet-name')
      ;<  [status=@ud body=@t]  bind:m
        (http-post "/spv-wallet"
          :~  ['action' 'add-wallet-from-entropy']
              ['wallet-name' wallet-name]
          ==)
      ;<  st=state-0:s  bind:m  get-state
      =/  nw=(unit [pubkey=@ux wallet:wallet:s])  (find-wallet-by-name st wallet-name)
      =/  msg=@t
        ?~  nw
          (crip (rap 3 'Requested wallet generation for "' (trip wallet-name) '". HTTP ' (ud-text status) '\n\n' body ~))
        =/  pub=@ux        p.u.nw
        =/  wal=wallet:s   q.u.nw
        (crip (rap 3 'Generated wallet "' (trip wallet-name) '" (pubkey '
             (hex-text pub)
             ') — seed phrase: '
             (format-seed seed.wal)
             '\n(Status ' (ud-text status) ')
'
             body ~))
      %-  pure:m
      !>  (respond-text msg)
  ==
  ::
  :*  'spv__import-wallet'
      'Import a wallet from an existing seed phrase (BIP39 mnemonic or Urbit @q).'
      %-  my
      :~  ['wallet-name' [%string 'Label to show in the UI.']]
          ['seed-phrase' [%string 'Existing seed (BIP39 words or @q).']]
          ['seed-format' [%string "Use 'bip39' (default) or 'q'."]]
      ==
      ~['wallet-name' 'seed-phrase']
      ^-  thread-builder:tool:mcp
      |=  args=(map name:parameter:tool:mcp argument:tool:mcp)
      ^-  shed:khan
      =/  m  (strand:spider ,vase)
      ^-  form:m
      =/  wallet-name=@t  (expect-string args 'wallet-name')
      =/  seed=@t         (expect-string args 'seed-phrase')
      =/  fmt=@t
        (fall (optional-string args 'seed-format') 'bip39')
      ;<  [status=@ud body=@t]  bind:m
        (http-post "/spv-wallet"
          :~  ['action' 'add-wallet']
              ['wallet-name' wallet-name]
              ['seed-phrase' seed]
              ['seed-format' fmt]
          ==)
      %-  pure:m
      !>  (format-http-response status body (crip (rap 3 'Imported wallet "' (trip wallet-name) '"' ~)))
  ==
  ::
  :*  'spv__remove-wallet'
      'Remove a full wallet by its master pubkey (also clears boot data if it was the boot wallet).'
      %-  my
      :~  ['pubkey' [%string 'Wallet pubkey in hex (as shown in the UI).']]
      ==
      ~['pubkey']
      ^-  thread-builder:tool:mcp
      |=  args=(map name:parameter:tool:mcp argument:tool:mcp)
      ^-  shed:khan
      =/  m  (strand:spider ,vase)
      ^-  form:m
      =/  pub=@t  (expect-string args 'pubkey')
      =/  clean=@t  (sanitize-pubkey pub)
      ;<  [status=@ud body=@t]  bind:m
        (http-post "/spv-wallet"
          :~  ['action' 'remove-wallet']
              ['pubkey' clean]
          ==)
      %-  pure:m
      !>  (format-http-response status body (crip (rap 3 'Removed wallet ' clean ~)))
  ==
  ::
  :*  'spv__add-watch-only'
      'Register a watch-only account from an xpub/tpub.'
      %-  my
      :~  ['account-name' [%string 'Friendly name for the account.']]
          ['xpub' [%string 'Extended public key (xpub/tpub/zpub/etc.).']]
          ['script-type' [%string 'p2pkh, p2sh-p2wpkh, p2wpkh, or p2tr.']]
          ['network' [%string 'main, testnet3, testnet4, signet, or regtest.']]
      ==
      ~['account-name' 'xpub' 'script-type' 'network']
      ^-  thread-builder:tool:mcp
      |=  args=(map name:parameter:tool:mcp argument:tool:mcp)
      ^-  shed:khan
      =/  m  (strand:spider ,vase)
      ^-  form:m
      =/  name=@t   (expect-string args 'account-name')
      =/  xpub=@t   (expect-string args 'xpub')
      =/  script=@t (expect-string args 'script-type')
      =/  net=@t    (expect-string args 'network')
      ;<  [status=@ud body=@t]  bind:m
        (http-post "/spv-wallet"
          :~  ['action' 'add-watch-only']
              ['account-name' name]
              ['xpub' xpub]
              ['script-type' script]
              ['network' net]
          ==)
      %-  pure:m
      !>  (format-http-response status body (crip (rap 3 'Added watch-only account "' (trip name) '"' ~)))
  ==
  ::
  :*  'spv__add-signing'
      'Import a signing (xprv/tprv) account that can create and broadcast transactions.'
      %-  my
      :~  ['account-name' [%string 'Friendly name for the account.']]
          ['xprv' [%string 'Extended private key (xprv/tprv/zprv/etc.).']]
          ['script-type' [%string 'p2pkh, p2sh-p2wpkh, p2wpkh, or p2tr.']]
          ['network' [%string 'main, testnet3, testnet4, signet, or regtest.']]
      ==
      ~['account-name' 'xprv' 'script-type' 'network']
      ^-  thread-builder:tool:mcp
      |=  args=(map name:parameter:tool:mcp argument:tool:mcp)
      ^-  shed:khan
      =/  m  (strand:spider ,vase)
      ^-  form:m
      =/  name=@t   (expect-string args 'account-name')
      =/  xprv=@t   (expect-string args 'xprv')
      =/  script=@t (expect-string args 'script-type')
      =/  net=@t    (expect-string args 'network')
      ;<  [status=@ud body=@t]  bind:m
        (http-post "/spv-wallet"
          :~  ['action' 'add-signing']
              ['account-name' name]
              ['xprv' xprv]
              ['script-type' script]
              ['network' net]
          ==)
      %-  pure:m
      !>  (format-http-response status body (crip (rap 3 'Added signing account "' (trip name) '"' ~)))
  ==
  ::
  :*  'spv__delete-account'
      'Delete a standalone watch-only or signing account by pubkey.'
      %-  my
      :~  ['pubkey' [%string 'Account pubkey in hex.']]
          ['type' [%string "Use 'watch-only' or 'signing'."]]
      ==
      ~['pubkey' 'type']
      ^-  thread-builder:tool:mcp
      |=  args=(map name:parameter:tool:mcp argument:tool:mcp)
      ^-  shed:khan
      =/  m  (strand:spider ,vase)
      ^-  form:m
      =/  pub=@t    (expect-string args 'pubkey')
      =/  clean=@t  (sanitize-pubkey pub)
      =/  typ=@t    (lower-text (expect-string args 'type'))
      =/  action=@t
        ?:  =('watch-only' typ)  'delete-watch-only'
        ?:  =('signing' typ)     'delete-signing'
        (strand-fail %invalid-account-type ~)
      ;<  [status=@ud body=@t]  bind:m
        (http-post "/spv-wallet"
          :~  ['action' action]
              ['pubkey' clean]
          ==)
      %-  pure:m
      !>  (format-http-response status body (crip (rap 3 'Deleted ' typ ' account ' clean ~)))
  ==
  ::
  :*  'spv__discover-accounts'
      'Run BIP44 gap-limit discovery for a wallet pubkey (purpose/coin-type pair).'
      %-  my
      :~  ['wallet-pubkey' [%string 'Wallet pubkey in hex.']]
          ['purpose' [%number 'BIP purpose (e.g. 84).']]
          ['coin-type' [%number 'BIP coin type (0 mainnet, 1 testnet, 60 eth, etc.).']]
      ==
      ~['wallet-pubkey' 'purpose' 'coin-type']
      ^-  thread-builder:tool:mcp
      |=  args=(map name:parameter:tool:mcp argument:tool:mcp)
      ^-  shed:khan
      =/  m  (strand:spider ,vase)
      ^-  form:m
      =/  pub=@t      (expect-string args 'wallet-pubkey')
      =/  purpose=@ud (expect-number args 'purpose')
      =/  coin=@ud    (expect-number args 'coin-type')
      ;<  [status=@ud body=@t]  bind:m
        (http-post (wallet-path pub)
          :~  ['action' 'discover-accounts']
              ['purpose' (ud-text purpose)]
              ['coin-type' (ud-text coin)]
          ==)
      %-  pure:m
      !>  (format-http-response status body 'Triggered account discovery')
  ==
  ::
  :*  'spv__add-account'
      'Manually add an account path (purpose/coin-type/account-number) to a wallet.'
      %-  my
      :~  ['wallet-pubkey' [%string 'Wallet pubkey in hex.']]
          ['account-name' [%string 'Name for the derived account.']]
          ['purpose' [%number 'BIP purpose (e.g. 84).']]
          ['coin-type' [%number 'BIP coin type.']]
          ['account-number' [%number 'Account number (usually 0).']]
      ==
      ~['wallet-pubkey' 'account-name' 'purpose' 'coin-type' 'account-number']
      ^-  thread-builder:tool:mcp
      |=  args=(map name:parameter:tool:mcp argument:tool:mcp)
      ^-  shed:khan
      =/  m  (strand:spider ,vase)
      ^-  form:m
      =/  pub=@t      (expect-string args 'wallet-pubkey')
      =/  name=@t     (expect-string args 'account-name')
      =/  purpose=@ud (expect-number args 'purpose')
      =/  coin=@ud    (expect-number args 'coin-type')
      =/  acct=@ud    (expect-number args 'account-number')
      ;<  [status=@ud body=@t]  bind:m
        (http-post (wallet-path pub)
          :~  ['action' 'add-unlisted-account']
              ['account-name' name]
              ['purpose' (ud-text purpose)]
              ['coin-type' (ud-text coin)]
              ['account-number' (ud-text acct)]
          ==)
      %-  pure:m
      !>  (format-http-response status body (crip (rap 3 'Added manual account "' (trip name) '"' ~)))
  ==
  ::
  :*  'spv__delete-wallet-account'
      'Remove a discovered account from a wallet by its derivation path.'
      %-  my
      :~  ['wallet-pubkey' [%string 'Wallet pubkey in hex.']]
          ['account-path' [%string "Account path such as m/84'/0'/0'."]]
      ==
      ~['wallet-pubkey' 'account-path']
      ^-  thread-builder:tool:mcp
      |=  args=(map name:parameter:tool:mcp argument:tool:mcp)
      ^-  shed:khan
      =/  m  (strand:spider ,vase)
      ^-  form:m
      =/  pub=@t   (expect-string args 'wallet-pubkey')
      =/  path=@t  (expect-string args 'account-path')
      ;<  [status=@ud body=@t]  bind:m
        (http-post (wallet-path pub)
          :~  ['action' 'delete-account']
              ['account-path' path]
          ==)
      %-  pure:m
      !>  (format-http-response status body (crip (rap 3 'Deleted account ' path ' from wallet ' (sanitize-pubkey pub) ~)))
  ==
  ::
  :*  'spv__scan-account'
      'Kick off a full gap-limit scan for all receiving/change addresses on an account.'
      %-  my
      :~  ['account-pubkey' [%string 'Account pubkey in hex.']]
      ==
      ~['account-pubkey']
      ^-  thread-builder:tool:mcp
      |=  args=(map name:parameter:tool:mcp argument:tool:mcp)
      ^-  shed:khan
      =/  m  (strand:spider ,vase)
      ^-  form:m
      =/  pub=@t  (expect-string args 'account-pubkey')
      ;<  [status=@ud body=@t]  bind:m
        (http-post (account-path pub)
          :~  ['action' 'full-scan']
          ==)
      %-  pure:m
      !>  (format-http-response status body (crip (rap 3 'Scanning account ' (sanitize-pubkey pub) ~)))
  ==
  ::
  :*  'spv__refresh-address'
      'Refresh a single address on an account (receiving or change) by index.'
      %-  my
      :~  ['account-pubkey' [%string 'Account pubkey in hex.']]
          ['chain' [%string "receiving or change."]]
          ['index' [%number 'Address index (0-based).']]
      ==
      ~['account-pubkey' 'chain' 'index']
      ^-  thread-builder:tool:mcp
      |=  args=(map name:parameter:tool:mcp argument:tool:mcp)
      ^-  shed:khan
      =/  m  (strand:spider ,vase)
      ^-  form:m
      =/  pub=@t    (expect-string args 'account-pubkey')
      =/  chain=@t  (lower-text (expect-string args 'chain'))
      =/  idx=@ud   (expect-number args 'index')
      ?.  |(=(chain 'receiving') =(chain 'change'))
        (strand-fail %invalid-chain ~)
      ;<  [status=@ud body=@t]  bind:m
        (http-post (account-path pub)
          :~  ['action' 'refresh-address']
              ['chain' chain]
              ['index' (ud-text idx)]
          ==)
      %-  pure:m
      !>  (format-http-response status body (crip (rap 3 'Refreshing ' chain ' index ' (ud-text idx) ' for ' (sanitize-pubkey pub) ~)))
  ==
  ::
  :*  'spv__set-network'
      'Switch the active Bitcoin network for an account (main/testnet/etc.).'
      %-  my
      :~  ['account-pubkey' [%string 'Account pubkey in hex.']]
          ['network' [%string 'main, testnet3, testnet4, signet, or regtest.']]
      ==
      ~['account-pubkey' 'network']
      ^-  thread-builder:tool:mcp
      |=  args=(map name:parameter:tool:mcp argument:tool:mcp)
      ^-  shed:khan
      =/  m  (strand:spider ,vase)
      ^-  form:m
      =/  pub=@t  (expect-string args 'account-pubkey')
      =/  net=@t  (expect-string args 'network')
      ;<  [status=@ud body=@t]  bind:m
        (http-post (account-path pub)
          :~  ['action' 'set-network']
              ['network' net]
          ==)
      %-  pure:m
      !>  (format-http-response status body (crip (rap 3 'Set network to ' net ' for ' (sanitize-pubkey pub) ~)))
  ==
  ::
  :*  'spv__label-utxo'
      'Apply BIP329 labels to a specific UTXO (txid:vout) for an account.'
      %-  my
      :~  ['account-pubkey' [%string 'Account pubkey in hex.']]
          ['utxo-txid' [%string 'Transaction id hex.']]
          ['utxo-vout' [%number 'Output index.']]
          ['labels' [%string 'Comma-separated labels (BIP329).']]
      ==
      ~['account-pubkey' 'utxo-txid' 'utxo-vout']
      ^-  thread-builder:tool:mcp
      |=  args=(map name:parameter:tool:mcp argument:tool:mcp)
      ^-  shed:khan
      =/  m  (strand:spider ,vase)
      ^-  form:m
      =/  pub=@t    (expect-string args 'account-pubkey')
      =/  txid=@t   (expect-string args 'utxo-txid')
      =/  vout=@ud  (expect-number args 'utxo-vout')
      =/  labels=@t (fall (optional-string args 'labels') '')
      ;<  [status=@ud body=@t]  bind:m
        (http-post (account-path pub)
          :~  ['action' 'set-output-labels']
              ['utxo-txid' txid]
              ['utxo-vout' (ud-text vout)]
              ['labels' labels]
          ==)
      %-  pure:m
      !>  (format-http-response status body (crip (rap 3 'Updated labels for ' txid ':' (ud-text vout) ~)))
  ==
  ::
  :*  'spv__freeze-utxo'
      'Freeze or unfreeze a specific UTXO so auto-selection can (or cannot) spend it.'
      %-  my
      :~  ['account-pubkey' [%string 'Account pubkey in hex.']]
          ['utxo-txid' [%string 'Transaction id hex.']]
          ['utxo-vout' [%number 'Output index.']]
          ['frozen' [%boolean 'true to freeze, false to thaw.']]
      ==
      ~['account-pubkey' 'utxo-txid' 'utxo-vout' 'frozen']
      ^-  thread-builder:tool:mcp
      |=  args=(map name:parameter:tool:mcp argument:tool:mcp)
      ^-  shed:khan
      =/  m  (strand:spider ,vase)
      ^-  form:m
      =/  pub=@t    (expect-string args 'account-pubkey')
      =/  txid=@t   (expect-string args 'utxo-txid')
      =/  vout=@ud  (expect-number args 'utxo-vout')
      =/  froz=?    (expect-boolean args 'frozen')
      ;<  [status=@ud body=@t]  bind:m
        (http-post (account-path pub)
          :~  ['action' 'set-utxo-frozen']
              ['utxo-txid' txid]
              ['utxo-vout' (ud-text vout)]
              ['frozen' (bool-text froz)]
          ==)
      %-  pure:m
      !>  (format-http-response status body (crip (rap 3 (?: froz 'Froze ' 'Thawed ') txid ':' (ud-text vout) ~)))
  ==
  ::
  :*  'spv__add-output'
      'Add a recipient output to the current draft transaction.'
      %-  my
      :~  ['account-pubkey' [%string 'Account pubkey in hex.']]
          ['address' [%string 'Destination Bitcoin address.']]
          ['amount' [%number 'Amount in satoshis.']]
      ==
      ~['account-pubkey' 'address' 'amount']
      ^-  thread-builder:tool:mcp
      |=  args=(map name:parameter:tool:mcp argument:tool:mcp)
      ^-  shed:khan
      =/  m  (strand:spider ,vase)
      ^-  form:m
      =/  pub=@t     (expect-string args 'account-pubkey')
      =/  addr=@t    (expect-string args 'address')
      =/  amount=@ud (expect-number args 'amount')
      ;<  [status=@ud body=@t]  bind:m
        (http-post (send-path pub)
          :~  ['action' 'add-output']
              ['output-address' addr]
              ['output-amount' (ud-text amount)]
          ==)
      %-  pure:m
      !>  (format-http-response status body (crip (rap 3 'Added output ' (ud-text amount) ' sats to ' addr ~)))
  ==
  ::
  :*  'spv__remove-output'
      'Remove an output from the draft transaction by its index (0-based).'
      %-  my
      :~  ['account-pubkey' [%string 'Account pubkey in hex.']]
          ['output-index' [%number 'Index in the current draft output list.']]
      ==
      ~['account-pubkey' 'output-index']
      ^-  thread-builder:tool:mcp
      |=  args=(map name:parameter:tool:mcp argument:tool:mcp)
      ^-  shed:khan
      =/  m  (strand:spider ,vase)
      ^-  form:m
      =/  pub=@t   (expect-string args 'account-pubkey')
      =/  idx=@ud  (expect-number args 'output-index')
      ;<  [status=@ud body=@t]  bind:m
        (http-post (send-path pub)
          :~  ['action' 'delete-output']
              ['output-index' (ud-text idx)]
          ==)
      %-  pure:m
      !>  (format-http-response status body (crip (rap 3 'Removed output #' (ud-text idx) ~)))
  ==
  ::
  :*  'spv__add-input'
      'Force-add a specific UTXO input (txid:vout:value) to the draft transaction.'
      %-  my
      :~  ['account-pubkey' [%string 'Account pubkey in hex.']]
          ['utxo-txid' [%string 'Transaction id hex.']]
          ['utxo-vout' [%number 'Output index.']]
          ['utxo-value' [%number 'Value in satoshis.']]
      ==
      ~['account-pubkey' 'utxo-txid' 'utxo-vout' 'utxo-value']
      ^-  thread-builder:tool:mcp
      |=  args=(map name:parameter:tool:mcp argument:tool:mcp)
      ^-  shed:khan
      =/  m  (strand:spider ,vase)
      ^-  form:m
      =/  pub=@t     (expect-string args 'account-pubkey')
      =/  txid=@t    (expect-string args 'utxo-txid')
      =/  vout=@ud   (expect-number args 'utxo-vout')
      =/  value=@ud  (expect-number args 'utxo-value')
      ;<  [status=@ud body=@t]  bind:m
        (http-post (send-path pub)
          :~  ['action' 'add-input']
              ['utxo-txid' txid]
              ['utxo-vout' (ud-text vout)]
              ['utxo-value' (ud-text value)]
          ==)
      %-  pure:m
      !>  (format-http-response status body (crip (rap 3 'Added input ' txid ':' (ud-text vout) ~)))
  ==
  ::
  :*  'spv__remove-input'
      'Remove a specific UTXO input from the draft transaction.'
      %-  my
      :~  ['account-pubkey' [%string 'Account pubkey in hex.']]
          ['utxo-txid' [%string 'Transaction id hex.']]
          ['utxo-vout' [%number 'Output index.']]
      ==
      ~['account-pubkey' 'utxo-txid' 'utxo-vout']
      ^-  thread-builder:tool:mcp
      |=  args=(map name:parameter:tool:mcp argument:tool:mcp)
      ^-  shed:khan
      =/  m  (strand:spider ,vase)
      ^-  form:m
      =/  pub=@t   (expect-string args 'account-pubkey')
      =/  txid=@t  (expect-string args 'utxo-txid')
      =/  vout=@ud (expect-number args 'utxo-vout')
      ;<  [status=@ud body=@t]  bind:m
        (http-post (send-path pub)
          :~  ['action' 'remove-input']
              ['utxo-txid' txid]
              ['utxo-vout' (ud-text vout)]
          ==)
      %-  pure:m
      !>  (format-http-response status body (crip (rap 3 'Removed input ' txid ':' (ud-text vout) ~)))
  ==
  ::
  :*  'spv__set-change'
      'Configure change address and fee rate for the current draft transaction.'
      %-  my
      :~  ['account-pubkey' [%string 'Account pubkey in hex.']]
          ['fee-rate' [%number 'Fee rate in sat/vB.']]
          ['change-address' [%string 'Optional override change address.']]
      ==
      ~['account-pubkey' 'fee-rate' 'change-address']
      ^-  thread-builder:tool:mcp
      |=  args=(map name:parameter:tool:mcp argument:tool:mcp)
      ^-  shed:khan
      =/  m  (strand:spider ,vase)
      ^-  form:m
      =/  pub=@t    (expect-string args 'account-pubkey')
      =/  fee=@ud   (expect-number args 'fee-rate')
      =/  addr=@t   (expect-string args 'change-address')
      ;<  [status=@ud body=@t]  bind:m
        (http-post (send-path pub)
          :~  ['action' 'set-change-config']
              ['fee-rate' (ud-text fee)]
              ['change-address' addr]
          ==)
      %-  pure:m
      !>  (format-http-response status body (crip (rap 3 'Set change address ' addr ' @ ' (ud-text fee) ' sat/vB' ~)))
  ==
  ::
  :*  'spv__auto-select'
      'Run the auto-selector (optionally updating the selection mode first).'
      %-  my
      :~  ['account-pubkey' [%string 'Account pubkey in hex.']]
          ['mode' [%string "Optional: random, largest-first, or disabled."]]
      ==
      ~['account-pubkey']
      ^-  thread-builder:tool:mcp
      |=  args=(map name:parameter:tool:mcp argument:tool:mcp)
      ^-  shed:khan
      =/  m  (strand:spider ,vase)
      ^-  form:m
      =/  pub=@t   (expect-string args 'account-pubkey')
      =/  mode=(unit @t)  (optional-string args 'mode')
      =/  norm-mode=(unit @t)
        ?~  mode  ~
        ` (lower-text u.mode)
      ?:  ?~ norm-mode
        ~
        =/  mode-text=@t  u.norm-mode
        ?.  |(=(mode-text 'random') =(mode-text 'largest-first') =(mode-text 'disabled'))
          (strand-fail %invalid-auto-select-mode ~)
        ;<  _  bind:m
          (http-post (send-path pub)
            :~  ['action' 'set-auto-select-mode']
                ['mode' mode-text]
            ==)
        ~
      ;<  [status=@ud body=@t]  bind:m
        (http-post (send-path pub)
          :~  ['action' 'run-auto-select']
          ==)
      %-  pure:m
      !>  (format-http-response status body 'Auto-select complete')
  ==
  ::
  :*  'spv__clear-draft'
      'Clear the current draft transaction (inputs, outputs, change).'
      %-  my
      :~  ['account-pubkey' [%string 'Account pubkey in hex.']]
      ==
      ~['account-pubkey']
      ^-  thread-builder:tool:mcp
      |=  args=(map name:parameter:tool:mcp argument:tool:mcp)
      ^-  shed:khan
      =/  m  (strand:spider ,vase)
      ^-  form:m
      =/  pub=@t  (expect-string args 'account-pubkey')
      ;<  [status=@ud body=@t]  bind:m
        (http-post (send-path pub)
          :~  ['action' 'clear-draft']
          ==)
      %-  pure:m
      !>  (format-http-response status body 'Cleared draft transaction')
  ==
  ::
  :*  'spv__broadcast'
      'Build, sign, and broadcast the draft transaction (no dry-run support yet).'
      %-  my
      :~  ['account-pubkey' [%string 'Account pubkey in hex.']]
          ['dry-run' [%boolean 'If true, only report that dry-run is not available.']]
      ==
      ~['account-pubkey']
      ^-  thread-builder:tool:mcp
      |=  args=(map name:parameter:tool:mcp argument:tool:mcp)
      ^-  shed:khan
      =/  m  (strand:spider ,vase)
      ^-  form:m
      =/  pub=@t   (expect-string args 'account-pubkey')
      =/  dry=?    (optional-boolean args 'dry-run' %.n)
      ?:  dry
        %-  pure:m
        !>  (respond-text 'Dry-run is not available yet; please call without dry-run to broadcast.')
      ;<  [status=@ud body=@t]  bind:m
        (http-post (send-path pub)
          :~  ['action' 'build-transaction']
          ==)
      %-  pure:m
      !>  (format-http-response status body 'Broadcast sent')
  ==
  ::
  :*  'spv__set-checkpoint'
      'Reset the SPV header sync checkpoint to a specific height on a network.'
      %-  my
      :~  ['network' [%string 'main, testnet3, testnet4, signet, or regtest.']]
          ['height' [%number 'Block height to anchor at.']]
      ==
      ~['network' 'height']
      ^-  thread-builder:tool:mcp
      |=  args=(map name:parameter:tool:mcp argument:tool:mcp)
      ^-  shed:khan
      =/  m  (strand:spider ,vase)
      ^-  form:m
      =/  net=@t    (expect-string args 'network')
      =/  height=@ud (expect-number args 'height')
      ;<  [status=@ud body=@t]  bind:m
        (http-post "/spv-wallet/spv"
          :~  ['action' 'set-checkpoint']
              ['network' net]
              ['height' (ud-text height)]
          ==)
      %-  pure:m
      !>  (format-http-response status body (crip (rap 3 'Set checkpoint for ' net ' @ height ' (ud-text height) ~)))
  ==
  ::
  :*  'spv__start-boot'
      'Start the comet boot/attestation flow using a boot secret (normal or fief mode).'
      %-  my
      :~  ['seed-phrase' [%string 'Urbit @q boot secret.']]
          ['sponsor' [%string 'Sponsor @p (required for normal mode).']]
          ['boot-mode' [%string "Optional: 'fief' for direct mode."]]
          ['fief-ip' [%string 'Required when boot-mode=fief.']]
          ['fief-port' [%string 'Required when boot-mode=fief.']]
      ==
      ~['seed-phrase']
      ^-  thread-builder:tool:mcp
      |=  args=(map name:parameter:tool:mcp argument:tool:mcp)
      ^-  shed:khan
      =/  m  (strand:spider ,vase)
      ^-  form:m
      =/  seed=@t      (expect-string args 'seed-phrase')
      =/  sponsor=(unit @t)  (optional-string args 'sponsor')
      =/  mode=(unit @t)     (optional-string args 'boot-mode')
      =/  lower-mode=(unit @t)
        ?~  mode  ~
        ` (lower-text u.mode)
      =/  fields=(list [@t @t])
        :~  ['action' 'start']
            ['seed-phrase' seed]
        ==
      =/  fields
        ?~  lower-mode  fields
        (snoc fields ['boot-mode' u.lower-mode])
      =/  fields
        ?~  lower-mode
          ?:  ?~ sponsor
              (strand-fail %missing-sponsor ~)
            (snoc fields ['sponsor' u.sponsor])
        ?:  =('fief' u.lower-mode)
            =/  ip=@t   (expect-string args 'fief-ip')
            =/  port=@t (expect-string args 'fief-port')
            (snoc (snoc fields ['fief-ip' ip]) ['fief-port' port])
        ?:  ?~ sponsor
            (strand-fail %missing-sponsor ~)
            (snoc fields ['sponsor' u.sponsor])
      ;<  [status=@ud body=@t]  bind:m
        (http-post "/spv-wallet/progress" fields)
      %-  pure:m
      !>  (format-http-response status body 'Boot sequence started (monitor /spv-wallet/progress)')
  ==
  ::
  :*  'spv__cancel-boot'
      'Cancel any in-flight comet boot process.'
      %-  my  ~
      ~
      ^-  thread-builder:tool:mcp
      |=  args=(map name:parameter:tool:mcp argument:tool:mcp)
      ^-  shed:khan
      =/  m  (strand:spider ,vase)
      ^-  form:m
      ;<  [status=@ud body=@t]  bind:m
        (http-post "/spv-wallet/progress"
          :~  ['action' 'cancel']
          ==)
      %-  pure:m
      !>  (format-http-response status body 'Boot cancelled')
  ==
  ::
  :*  'spv__retry-boot'
      'Retry the boot flow after clearing the previous error state.'
      %-  my  ~
      ~
      ^-  thread-builder:tool:mcp
      |=  args=(map name:parameter:tool:mcp argument:tool:mcp)
      ^-  shed:khan
      =/  m  (strand:spider ,vase)
      ^-  form:m
      ;<  [status=@ud body=@t]  bind:m
        (http-post "/spv-wallet/progress"
          :~  ['action' 'retry']
          ==)
      %-  pure:m
      !>  (format-http-response status body 'Boot retry triggered')
  ==
==
