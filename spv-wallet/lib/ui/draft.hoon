/-  *spv-wallet
/+  feather, *ui-utils, wallet-account, *wallet-address, sailbox
|%
::  Simple bitcoin wallet UI — /apps/wallet
::
::  Dead simple vanilla bitcoin wallet. No coin control, no UTXO
::  management, no derivation paths. Just a balance, a way to
::  receive bitcoin (show address + QR), and a way to send it.
::
::  Looks for a wallet tagged with <simple>Title</simple> in
::  its name field. The app auto-generates one on first boot.
::  Everything else (accounts, keys, script types) is hidden
::  from the user.
::
++  draft-page
  |=  $:  num=@ud
          wallets=(map @ux wallet)
          accounts=(map @ux account-details)
          args=(list [key=@t value=@t])
          broadcasts=(map @t broadcast)
      ==
  ^-  manx
  ?+  num  (draft-0 wallets accounts args broadcasts)
    %0  (draft-0 wallets accounts args broadcasts)
  ==
::
++  format-btc
  |=  sats=@ud
  ^-  tape
  =/  whole=@ud  (div sats 100.000.000)
  =/  frac=@ud  (mod sats 100.000.000)
  =/  frac-tape=tape  (a-co:co frac)
  =/  pad=@ud  (sub 8 (min 8 (lent frac-tape)))
  =/  padded=tape  (weld (reap pad '0') frac-tape)
  (weld (a-co:co whole) (weld "." padded))
::
++  total-balance
  |=  accounts=(map @ux account-details)
  ^-  @ud
  %-  ~(rep by accounts)
  |=  [[k=@ux v=account-details] acc=@ud]
  (add acc (compute-account-balance v))
::
::
::
::  <simple> tag convention: wallets with names like
::  "<simple>My Wallet</simple>" are owned by the simple UI.
::  This lets the simple wallet coexist with user-created
::  wallets in the same agent without collision.
::
::  Tag formats:
::    <simple>Name</simple>         — not backed up
::    <simple saved>Name</simple>   — backed up
::
::  Parse a <simple> tag into its components using de-xml:html.
::  Returns ~ if not a valid <simple> tag.
::
++  parse-simple-tag
  |=  name=@t
  ^-  (unit [saved=? fee=@ud title=tape])
  =/  mx=(unit manx)  (de-xml:html name)
  ?~  mx  ~
  ?.  =(%simple n.g.u.mx)  ~
  =/  saved=?
    %+  lien  a.g.u.mx
    |=  [n=mane v=tape]
    =(%saved n)
  =/  fee=@ud
    =/  fee-attr=(unit tape)
      |-
      ?~  a.g.u.mx  ~
      ?:  =(%fee n.i.a.g.u.mx)  `v.i.a.g.u.mx
      $(a.g.u.mx t.a.g.u.mx)
    ?~  fee-attr  2
    (fall (rush (crip u.fee-attr) dem) 2)
  =/  title=tape
    ?~  c.u.mx  ""
    =/  node=manx  i.c.u.mx
    ?.  =(%$ n.g.node)  ""
    ?~  a.g.node  ""
    v.i.a.g.node
  `[saved fee title]
::
++  build-simple-tag
  |=  [saved=? fee=@ud title=tape]
  ^-  @t
  =/  attrs=tape
    ;:  weld
      ?:(saved " saved" "")
      ?:(=(fee 2) "" " fee=\"{(a-co:co fee)}\"")
    ==
  (crip "<simple{attrs}>{title}</simple>")
::
++  is-simple
  |=  name=@t
  !=(~ (parse-simple-tag name))
::
++  is-saved
  |=  name=@t
  =/  parsed  (parse-simple-tag name)
  ?~  parsed  %.n
  saved.u.parsed
::
++  parse-simple-title
  |=  name=@t
  ^-  tape
  =/  parsed  (parse-simple-tag name)
  ?~  parsed  (trip name)
  title.u.parsed
::
++  find-simple-wallet
  |=  wallets=(map @ux wallet)
  ^-  (unit [key=@ux val=wallet])
  =/  wal-list=(list [key=@ux val=wallet])  ~(tap by wallets)
  |-
  ?~  wal-list  ~
  ?:  (is-simple name.val.i.wal-list)
    `i.wal-list
  $(wal-list t.wal-list)
::
++  draft-0
  |=  $:  all-wallets=(map @ux wallet)
          all-accounts=(map @ux account-details)
          args=(list [key=@t value=@t])
          broadcasts=(map @t broadcast)
      ==
  ^-  manx
  ::  Find the <simple> wallet, fall back to first wallet
  =/  simple=(unit [key=@ux val=wallet])
    =/  tagged  (find-simple-wallet all-wallets)
    ?^  tagged  tagged
    =/  wal-list=(list [key=@ux val=wallet])  ~(tap by all-wallets)
    ?~  wal-list  ~
    `i.wal-list
  =/  wal-name=tape
    ?~  simple  "Wallet"
    ?:  (is-simple name.val.u.simple)
      (parse-simple-title name.val.u.simple)
    (trip name.val.u.simple)
  =/  wal-seed=tape
    ?~  simple  ""
    (trip (seed-to-cord seed.val.u.simple))
  =/  wal-seed-masked=tape
    ?~  simple  ""
    (trip (mask-seed-phrase seed.val.u.simple))
  =/  backed-up=?
    ?~  simple  %.n
    (is-saved name.val.u.simple)
  =/  fee-rate=@ud
    ?~  simple  2
    =/  parsed  (parse-simple-tag name.val.u.simple)
    ?~  parsed  2
    fee.u.parsed
  ::  Collect all accounts from the simple wallet with their networks
  ::  Mainnet accounts first
  =/  all-acct-details=(list account-details)
    ?~  simple  ~
    =/  acct-pairs=(list [account:hd-path @ux])  ~(tap by accounts.val.u.simple)
    =/  unsorted=(list account-details)
      %+  murn  acct-pairs
      |=  [* pubkey=@ux]
      (~(get by all-accounts) pubkey)
    %+  weld
      (skim unsorted |=(d=account-details =(active-network.d %main)))
    (skip unsorted |=(d=account-details =(active-network.d %main)))
  ::  Build list of available networks (same order)
  =/  available-nets=(list network)
    (turn all-acct-details |=(d=account-details active-network.d))
  ::  Pick active network from ?net= query param, or first available
  =/  requested-net=(unit @t)
    (get-header:http 'net' args)
  =/  acct=(unit account-details)
    ?~  all-acct-details  ~
    ?~  requested-net
      `i.all-acct-details
    =/  match=(unit account-details)
      %-  ~(rep in (sy all-acct-details))
      |=  [d=account-details acc=(unit account-details)]
      ?^  acc  acc
      ?:  =((crip (trip active-network.d)) u.requested-net)  `d
      ~
    ?~  match  `i.all-acct-details
    match
  =/  net-label=tape
    ?~  acct  "unknown"
    (trip active-network.u.acct)
  ::  Compute balance and pending for the selected account only
  =/  bal=@ud
    ?~  acct  0
    (compute-account-balance u.acct)
  =/  bal-tape=tape  (format-btc bal)
  =/  bal-sats=tape  (a-co:co bal)
  ::  pending-in/out computed after tx-list is built (below)
  =/  is-mainnet=?
    ?~  acct  %.n
    =(%main active-network.u.acct)
  =/  accent=tape  ?:(is-mainnet "#f7931a" "#6496ff")
  =/  accent-hover=tape  ?:(is-mainnet "#e8850f" "#5080e0")
  =/  accent-bg=tape  ?:(is-mainnet "rgba(247, 147, 26, 0.1)" "rgba(100, 150, 255, 0.1)")
  =/  accent-border=tape  ?:(is-mainnet "rgba(247, 147, 26, 0.25)" "rgba(100, 150, 255, 0.25)")
  ::  Address is now fetched live via POST when Receive is clicked
  ::  Build transaction list for activity popup
  ::  Each tx gets a direction (sent/received) and net amount
  =/  our-addrs=(set @t)
    ?~  acct  ~
    =/  ac  ~(. ac:wallet-account [u.acct active-network.u.acct])
    =/  addrs=(set @t)  ~
    =/  recv=(list [@ud hd-leaf])  (tap:((on @ud hd-leaf) gth) receiving:ac)
    |-
    ?~  recv
      =/  chng=(list [@ud hd-leaf])  (tap:((on @ud hd-leaf) gth) change:ac)
      |-
      ?~  chng  addrs
      $(chng t.chng, addrs (~(put in addrs) address.main.+.i.chng))
    $(recv t.recv, addrs (~(put in addrs) address.main.+.i.recv))
  =/  tx-list=(list [dir=?(%sent %received) amt=@ud conf=? =transaction])
    ?~  acct  ~
    =/  net-det=network-details
      =/  nd  (~(get by networks.u.acct) active-network.u.acct)
      ?~  nd  *network-details
      u.nd
    =/  txns=(list [txid=@t tx=transaction])  ~(tap by transactions.net-det)
    =/  acc=(list [dir=?(%sent %received) amt=@ud conf=? =transaction])  ~
    |-
    ?~  txns  acc
    =/  tx=transaction  tx.i.txns
    ::  Sum outputs going to our addresses
    =/  in-val=@ud
      =/  outs=(list tx-output)  outputs.tx
      =/  s=@ud  0
      |-
      ?~  outs  s
      ?:  (~(has in our-addrs) address.i.outs)
        $(outs t.outs, s (add s value.i.outs))
      $(outs t.outs)
    ::  Sum inputs spending from our addresses
    =/  out-val=@ud
      =/  ins=(list tx-input)  inputs.tx
      =/  s=@ud  0
      |-
      ?~  ins  s
      ?~  prevout.i.ins  $(ins t.ins)
      ?:  (~(has in our-addrs) address.u.prevout.i.ins)
        $(ins t.ins, s (add s value.u.prevout.i.ins))
      $(ins t.ins)
    =/  conf=?  ?=([%confirmed *] tx-status.tx)
    =/  entries=(list [dir=?(%sent %received) amt=@ud conf=? =transaction])
      ?:  ?&((gth in-val 0) (gth out-val 0))
        ::  Self-send: show both legs
        :~  [%received in-val conf tx]
            [%sent out-val conf tx]
        ==
      ?:  (gth out-val in-val)
        ~[[%sent (sub out-val in-val) conf tx]]
      ~[[%received (sub in-val out-val) conf tx]]
    $(txns t.txns, acc (weld entries acc))
  ::  Compute pending from unconfirmed transactions in tx-list
  =/  [pending-in=@ud pending-out=@ud]
    =/  rem=(list [dir=?(%sent %received) amt=@ud conf=? =transaction])  tx-list
    =/  pin=@ud  0
    =/  pout=@ud  0
    |-
    ?~  rem  [pin pout]
    ?:  conf.i.rem  $(rem t.rem)
    ?:  ?=(%received dir.i.rem)
      $(rem t.rem, pin (add pin amt.i.rem))
    $(rem t.rem, pout (add pout amt.i.rem))
  =/  pending-in-tape=tape  (format-btc pending-in)
  =/  pending-out-tape=tape  (format-btc pending-out)
  ::  Inject pending broadcasts not yet in transaction map
  =/  pending-items=(list manx)
    ?~  acct  ~
    =/  net-det=network-details
      =/  nd  (~(get by networks.u.acct) active-network.u.acct)
      ?~  nd  *network-details
      u.nd
    =/  rem=(list [txid=@t bc=broadcast])  ~(tap by broadcasts)
    =/  items=(list manx)  ~
    |-
    ?~  rem  (flop items)
    =/  [txid=@t bc=broadcast]  i.rem
    ?.  =(network.bc active-network.u.acct)
      $(rem t.rem)
    ?:  (~(has by transactions.net-det) txid)
      $(rem t.rem)
    ::  Compute send amount: sum non-change outputs
    =/  send-amt=@ud
      =/  outs=(list [address=@t amount=@ud])  outputs.bc
      =/  s=@ud  0
      |-
      ?~  outs  s
      ?:  ?&(?=(^ change-address.bc) =(address.i.outs u.change-address.bc))
        $(outs t.outs)
      $(outs t.outs, s (add s amount.i.outs))
    =/  amt-tape=tape  (format-btc send-amt)
    ::  Destination: first non-change output
    =/  dest=tape
      =/  outs=(list [address=@t amount=@ud])  outputs.bc
      |-
      ?~  outs  ""
      ?:  ?&(?=(^ change-address.bc) =(address.i.outs u.change-address.bc))
        $(outs t.outs)
      (trip address.i.outs)
    =/  sent-da=@da  sent.bc
    =/  =date  (yore sent-da)
    =/  [dy=@ud hr=@ud mn=@ud *]  t.date
    =/  sent-tape=tape
      "{(a-co:co m.date)}/{(a-co:co dy)} {(a-co:co hr)}:{?:((lth mn 10) "0" "")}{(a-co:co mn)}"
    =/  txid-full=tape  (trip txid)
    =/  item=manx
      ;div.activity-tx(onclick "showTxDetail(this)", data-txid txid-full, data-addr dest, data-addr-label "To", data-status "Broadcast", data-time sent-tape)
        ;div.activity-tx-icon.tx-sent
          ;svg(xmlns "http://www.w3.org/2000/svg", viewBox "0 0 24 24", width "16", height "16", fill "none", stroke "currentColor", stroke-width "2.5", stroke-linecap "round", stroke-linejoin "round")
            ;line(x1 "12", y1 "19", x2 "12", y2 "5");
            ;polyline(points "5 12 12 5 19 12");
          ==
        ==
        ;div.activity-tx-body
          ;+  ?.  (gth (lent dest) 0)  ;span;
              ;div.activity-tx-addr: {dest}
          ;div.activity-tx-meta
            ;span.activity-tx-status.pending: Broadcast
            ;span.activity-tx-time: {sent-tape}
          ==
        ==
        ;div.activity-tx-value
          ;div(class "activity-tx-amt tx-sent")
            ; -{amt-tape} BTC
          ==
          ;div.activity-tx-fiat(data-sats "{(a-co:co send-amt)}"): ;
        ==
      ==
    $(rem t.rem, items [item items])
  ::  Pre-render activity items outside sail to avoid fuse-loop
  ::  Sort: pending (unconfirmed) first, then confirmed
  =/  tx-list-sorted=(list [dir=?(%sent %received) amt=@ud conf=? =transaction])
    %+  weld
      (skim tx-list |=(e=[* * conf=? *] !conf.e))
    (skim tx-list |=(e=[* * conf=? *] conf.e))
  =/  tx-items=(list manx)
    =/  rem=(list [dir=?(%sent %received) amt=@ud conf=? =transaction])  tx-list-sorted
    =/  items=(list manx)  ~
    |-
    ?~  rem  (flop items)
    =/  e  i.rem
    =/  amt-tape=tape  (format-btc amt.e)
    =/  dir-class=tape  ?:(?=(%sent dir.e) "tx-sent" "tx-received")
    =/  tx-time=tape
      ?.  ?=([%confirmed *] tx-status.transaction.e)  ""
      =/  bt=@ud  block-time.tx-status.transaction.e
      ?:  =(0 bt)  ""
      =/  =date  (yore (add ~1970.1.1 (mul bt ~s1)))
      =/  [dy=@ud hr=@ud mn=@ud *]  t.date
      "{(a-co:co m.date)}/{(a-co:co dy)} {(a-co:co hr)}:{?:((lth mn 10) "0" "")}{(a-co:co mn)}"
    =/  status=tape  ?:(conf.e "Confirmed" "Pending")
    =/  counterparty=tape
      ?:  ?=(%sent dir.e)
        ::  Sent: first output address
        =/  outs=(list tx-output)  outputs.transaction.e
        ?~(outs "" (trip address.i.outs))
      ::  Received: first input prevout address
      =/  ins=(list tx-input)  inputs.transaction.e
      |-
      ?~  ins  ""
      ?~  prevout.i.ins  $(ins t.ins)
      (trip address.u.prevout.i.ins)
    =/  addr-label=tape  ?:(?=(%sent dir.e) "To" "From")
    =/  txid-full=tape  (trip txid.transaction.e)
    =/  item=manx
      ;div.activity-tx(onclick "showTxDetail(this)", data-txid txid-full, data-addr counterparty, data-addr-label addr-label, data-status status, data-time tx-time)
        ;div(class "activity-tx-icon {dir-class}")
          ;svg(xmlns "http://www.w3.org/2000/svg", viewBox "0 0 24 24", width "16", height "16", fill "none", stroke "currentColor", stroke-width "2.5", stroke-linecap "round", stroke-linejoin "round")
            ;+  ?:  ?=(%sent dir.e)
                ;polyline(points "5 12 12 5 19 12");
            ;polyline(points "19 12 12 19 5 12");
            ;line(x1 "12", y1 "19", x2 "12", y2 "5");
          ==
        ==
        ;div.activity-tx-body
          ;+  ?.  (gth (lent counterparty) 0)  ;span;
              ;div.activity-tx-addr: {counterparty}
          ;div.activity-tx-meta
            ;span.activity-tx-status(class ?:(conf.e "confirmed" "pending")): {status}
            ;+  ?.  (gth (lent tx-time) 0)  ;span;
                ;span.activity-tx-time: {tx-time}
          ==
        ==
        ;div.activity-tx-value
          ;div(class "activity-tx-amt {dir-class}")
            ; {?:(?=(%sent dir.e) "-" "+")}{amt-tape} BTC
          ==
          ;div.activity-tx-fiat(data-sats "{(a-co:co amt.e)}"): ;
        ==
      ==
    $(rem t.rem, items [item items])
  ::  Pre-render address items for addresses popup
  =/  addr-items=[recv=(list manx) chng=(list manx)]
    ?~  acct  [~ ~]
    =/  ac  ~(. ac:wallet-account [u.acct active-network.u.acct])
    =/  next-recv=(unit @t)  (get-next-unused-address receiving:ac)
    =/  next-chng=(unit @t)  (get-next-unused-address change:ac)
    :-  (render-addr-list (tap:((on @ud hd-leaf) gth) receiving:ac) 'receiving' next-recv)
    (render-addr-list (tap:((on @ud hd-leaf) gth) change:ac) 'change' next-chng)
  =/  all-tx-items=(list manx)  (weld pending-items tx-items)
  =/  body-content=manx  (render-body bal bal-tape bal-sats pending-in pending-out pending-in-tape pending-out-tape wal-name net-label accent accent-hover accent-bg accent-border backed-up wal-seed wal-seed-masked fee-rate all-tx-items addr-items available-nets)
  body-content
::
++  text
  |=  t=tape
  ^-  manx
  [[%$ [%$ t] ~] ~]
::
++  render-addr-list
  |=  [leaves=(list [@ud hd-leaf]) chain=@t next-unused=(unit @t)]
  ^-  (list manx)
  =/  chain-tape=tape  (trip chain)
  =/  items=(list manx)  ~
  |-
  ?~  leaves  (flop items)
  =/  [idx=@ud =hd-leaf]  i.leaves
  =/  det=address-details  main.hd-leaf
  =/  addr=tape  (trip address.det)
  =/  is-next=?
    ?~  next-unused  %.n
    =(address.det u.next-unused)
  =/  addr-short=tape
    ?:  (lth (lent addr) 20)  addr
    (weld (scag 10 addr) (weld "..." (slag (sub (lent addr) 6) addr)))
  =/  bal=@ud
    =/  rem=(list [txid=@t vout=@ud value=@ud =tx-status])  utxos.det
    =/  s=@ud  0
    |-
    ?~  rem  s
    $(rem t.rem, s (add s value.i.rem))
  =/  bal-tape=tape  (format-btc bal)
  =/  idx-tape=tape  (a-co:co idx)
  =/  last-title=tape
    ?~  last-check.det  "Last refreshed: never"
    =/  d=date  (yore u.last-check.det)
    "Last refreshed: {(a-co:co m.d)}/{(a-co:co d.t.d)} {(a-co:co h.t.d)}:{?:((lth m.t.d 10) "0" "")}{(a-co:co m.t.d)}"
  =/  has-pending=?
    =/  rem=(list [txid=@t vout=@ud value=@ud =tx-status])  utxos.det
    |-
    ?~  rem  %.n
    ?:  ?=([%unconfirmed ~] tx-status.i.rem)  %.y
    $(rem t.rem)
  =/  item=manx
    ;div.addr-item
      ;div.addr-item-left
        ;+  ?.  is-next
              ;span.addr-next-spacer;
            =/  tip=tape  ?:(=(chain 'receiving') "Next receive address" "Next change address")
            ;span.addr-next(title tip);
        ;span.addr-idx: #{idx-tape}
        ;span.addr-short: {addr-short}
        ;button.tx-copy-btn(onclick "copyTxid(this)", data-txid addr)
          ;svg(xmlns "http://www.w3.org/2000/svg", viewBox "0 0 24 24", width "11", height "11", fill "none", stroke "currentColor", stroke-width "2", stroke-linecap "round", stroke-linejoin "round")
            ;rect(x "9", y "9", width "13", height "13", rx "2", ry "2");
            ;path(d "M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1");
          ==
        ==
        ;+  ?.  has-pending  ;span;
            ;span.addr-pending: pending
      ==
      ;div.addr-item-right
        ;span.addr-bal: {bal-tape}
        ;span.addr-clock(title last-title)
          ;svg(xmlns "http://www.w3.org/2000/svg", viewBox "0 0 24 24", width "12", height "12", fill "none", stroke "currentColor", stroke-width "2", stroke-linecap "round", stroke-linejoin "round")
            ;circle(cx "12", cy "12", r "10");
            ;polyline(points "12 6 12 12 16 14");
          ==
        ==
        ;button.addr-refresh(onclick "refreshAddr('{chain-tape}', {idx-tape})", title "Refresh address")
          ;svg(xmlns "http://www.w3.org/2000/svg", viewBox "0 0 24 24", width "12", height "12", fill "none", stroke "currentColor", stroke-width "2", stroke-linecap "round", stroke-linejoin "round")
            ;polyline(points "23 4 23 10 17 10");
            ;polyline(points "1 20 1 14 7 14");
            ;path(d "M3.51 9a9 9 0 0 1 14.85-3.36L23 10M1 14l4.64 4.36A9 9 0 0 0 20.49 15");
          ==
        ==
      ==
    ==
  $(leaves t.leaves, items [item items])
::
++  render-activity-list
  |=  items=(list manx)
  ^-  manx
  ;div.activity-list
    ;+  ?.  =(~ items)  ;span;
        ;div.activity-empty: No transactions yet
    ;*  items
  ==
::
++  render-header
  |=  [wal-name=tape net-label=tape available-nets=(list network)]
  ^-  manx
  ;div
    ;div.wallet-header
      ;div.wallet-header-left
        ;div.wallet-logo
          ;svg(xmlns "http://www.w3.org/2000/svg", viewBox "0 0 24 24", width "16", height "16", fill "white")
            ;path(d "M23.638,14.904c-1.602,6.43-8.113,10.34-14.542,8.736C2.67,22.05-1.244,15.525.362,9.105,1.962,2.67,8.475-1.243,14.9.358c6.43,1.605,10.342,8.115,8.738,14.546z");
            ;path(d "M17.204,10.296c0.239-1.596-0.977-2.453-2.64-3.025l0.54-2.163-1.317-0.328-0.525,2.107c-0.346-0.086-0.702-0.168-1.055-0.248l0.529-2.12-1.317-0.328-0.54,2.162c-0.286-0.065-0.567-0.13-0.84-0.198l0.001-0.007-1.816-0.453-0.35,1.407s0.977,0.224,0.957,0.238c0.533,0.133,0.63,0.486,0.614,0.766l-0.615,2.464c0.037,0.009,0.084,0.023,0.137,0.044l-0.139-0.035-0.862,3.453c-0.065,0.162-0.231,0.405-0.604,0.313,0.013,0.019-0.957-0.239-0.957-0.239l-0.654,1.508,1.714,0.427c0.319,0.08,0.631,0.164,0.939,0.243l-0.546,2.189,1.316,0.328,0.54-2.164c0.359,0.098,0.708,0.188,1.05,0.273l-0.538,2.155,1.317,0.328,0.546-2.186c2.245,0.425,3.933,0.253,4.643-1.778,0.572-1.635-0.028-2.578-1.21-3.194,0.861-0.199,1.509-0.764,1.681-1.933zm-3.009,4.22c-0.407,1.636-3.162,0.751-4.055,0.529l0.723-2.899c0.893,0.223,3.757,0.664,3.332,2.37zm0.407-4.243c-0.371,1.489-2.664,0.732-3.407,0.547l0.656-2.63c0.743,0.186,3.138,0.532,2.751,2.083z", fill "white");
          ==
        ==
        ;span.wallet-title(onclick "editWalletName(this)", data-name wal-name)
          ;+  (text wal-name)
        ==
        ;+  ?:  (lth (lent available-nets) 2)
              ;span.net-badge
                ;+  (text net-label)
              ==
            ;div.net-dropdown
              ;button.net-badge.net-badge-toggle(onclick "toggleNetDropdown(event)")
                ;+  (text net-label)
                ;span.net-arrow: ▾
              ==
              ;div#net-menu.net-menu
                ;*  %+  turn  available-nets
                    |=  =network
                    =/  net-tape=tape  (trip network)
                    ;button.net-menu-item(onclick "switchNet('{net-tape}')"): {net-tape}
              ==
            ==
      ==
      ;button.info-btn(onclick "toggleInfo()")
        ;svg(xmlns "http://www.w3.org/2000/svg", viewBox "0 0 24 24", width "18", height "18", fill "none", stroke "currentColor", stroke-width "2", stroke-linecap "round", stroke-linejoin "round")
          ;circle(cx "12", cy "12", r "10");
          ;line(x1 "12", y1 "16", x2 "12", y2 "12");
          ;line(x1 "12", y1 "8", x2 "12.01", y2 "8");
        ==
      ==
    ==
  ==
::
++  render-banner
  |=  backed-up=?
  ^-  manx
  ;div(class "backup-banner {?:(backed-up "hidden" "")}", onclick "toggleInfo()")
    ;span.backup-banner-text: Back up your recovery phrase!
  ==
::
++  render-balance
  |=  $:  bal-tape=tape
          bal-sats=tape
          pending-in=@ud
          pending-out=@ud
          pending-in-tape=tape
          pending-out-tape=tape
      ==
  ^-  manx
  =/  has-pending=?  |(!=(0 pending-in) !=(0 pending-out))
  =/  in-sats=tape  ?:(=(0 pending-in) "0" (a-co:co pending-in))
  =/  out-sats=tape  ?:(=(0 pending-out) "0" (a-co:co pending-out))
  =/  uc=tape  ?:(has-pending "unconf-balance" "unconf-balance hidden")
  =/  in-text=tape  (weld "+" pending-in-tape)
  =/  out-text=tape  (weld "-" pending-out-tape)
  ;div
    ;div.balance-section
      ;div.balance-label-row
        ;span.balance-label: Total Balance
        ;button#sync-btn.sync-btn(onclick "refreshWallet()")
          ;svg(xmlns "http://www.w3.org/2000/svg", viewBox "0 0 24 24", width "14", height "14", fill "none", stroke "currentColor", stroke-width "2", stroke-linecap "round", stroke-linejoin "round")
            ;polyline(points "23 4 23 10 17 10");
            ;polyline(points "1 20 1 14 7 14");
            ;path(d "M3.51 9a9 9 0 0 1 14.85-3.36L23 10M1 14l4.64 4.36A9 9 0 0 0 20.49 15");
          ==
        ==
      ==
      ;div.balance-amount
        ;+  (text bal-tape)
      ==
      ;div.balance-unit: BTC
      ;div.balance-fiat
        ;span#fiat-value(data-sats bal-sats): —
      ==
      ;div.balance-rate
        ;span#btc-rate: —
      ==
    ==
    ;div(class uc)
      ;div.unconf-label: Unconfirmed
      ;div.unconf-row
        ;+  ?:  =(0 pending-in)  ;span;
            ;div.unconf-in
              ;span.unconf-amount-in
                ;+  (text in-text)
              ==
              ;span.unconf-unit: BTC
            ==
        ;+  ?:  =(0 pending-out)  ;span;
            ;div.unconf-out
              ;span.unconf-amount-out
                ;+  (text out-text)
              ==
              ;span.unconf-unit: BTC
            ==
      ==
    ==
  ==
::
++  render-actions
  |=  ~
  ^-  manx
  ;div.action-buttons
    ;button.action-btn.action-btn-secondary(onclick "toggleReceive()"): Receive
    ;button.action-btn.action-btn-primary(onclick "toggleSend()"): Send
  ==
::
++  render-tab-panel
  |=  [tx-items=(list manx) addr-items=[recv=(list manx) chng=(list manx)]]
  ^-  manx
  ;div.tab-panel
    ;div.tab-bar
      ;button.tab-btn.active(onclick "switchTab('activity', this)"): Activity
      ;button.tab-btn(onclick "switchTab('addresses', this)"): Addresses
    ==
    ;div#tab-activity.tab-content
      ;+  (render-activity-list tx-items)
    ==
    ;div#tab-addresses.tab-content.hidden
      ;div.addr-tabs
        ;button.addr-tab.active(onclick "switchAddrTab('recv', this)"): Receiving
        ;button.addr-tab(onclick "switchAddrTab('chng', this)"): Change
      ==
      ;div#addr-recv.addr-list
        ;+  ?.  =(~ recv.addr-items)  ;span;
            ;div.addr-empty: No receiving addresses derived
        ;*  recv.addr-items
      ==
      ;div#addr-chng.addr-list.hidden
        ;+  ?.  =(~ chng.addr-items)  ;span;
            ;div.addr-empty: No change addresses derived
        ;*  chng.addr-items
      ==
    ==
  ==
::
++  render-tx-detail-popup
  |=  ~
  ^-  manx
  ;div.tx-detail-overlay.hidden(id "tx-detail-overlay", onclick "closeTxDetail(event)")
    ;div.tx-detail-modal
      ;button.receive-close(onclick "closeTxDetail()"): ×
      ;div.tx-detail-row.hidden(id "tx-detail-addr")
        ;span.tx-detail-label(id "tx-detail-addr-label"): From
        ;div.tx-detail-value-row
          ;span.tx-detail-value(id "tx-detail-addr-value");
          ;button.tx-copy-btn(onclick "copyTxid(this)", data-txid "")
            ;svg(xmlns "http://www.w3.org/2000/svg", viewBox "0 0 24 24", width "14", height "14", fill "none", stroke "currentColor", stroke-width "2", stroke-linecap "round", stroke-linejoin "round")
              ;rect(x "9", y "9", width "13", height "13", rx "2", ry "2");
              ;path(d "M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1");
            ==
          ==
        ==
      ==
      ;div.tx-detail-row
        ;span.tx-detail-label: Txid
        ;div.tx-detail-value-row
          ;span.tx-detail-value(id "tx-detail-txid");
          ;button.tx-copy-btn(onclick "copyTxid(this)", data-txid "")
            ;svg(xmlns "http://www.w3.org/2000/svg", viewBox "0 0 24 24", width "14", height "14", fill "none", stroke "currentColor", stroke-width "2", stroke-linecap "round", stroke-linejoin "round")
              ;rect(x "9", y "9", width "13", height "13", rx "2", ry "2");
              ;path(d "M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1");
            ==
          ==
        ==
      ==
      ;div.tx-detail-row
        ;span.tx-detail-label: Status
        ;span.tx-detail-value(id "tx-detail-status");
      ==
      ;div.tx-detail-row.hidden(id "tx-detail-time-row")
        ;span.tx-detail-label: Time
        ;span.tx-detail-value(id "tx-detail-time");
      ==
    ==
  ==
::
++  render-receive-popup
  |=  ~
  ^-  manx
  ;div#receive-overlay.receive-overlay(onclick "closeReceive(event)")
    ;div.receive-modal
      ;button.receive-close(onclick "toggleReceive()"): ×
      ;div.receive-title: Receive Bitcoin
      ;div#receive-spinner.receive-spinner
        ;div.spinner;
        ;div.spinner-text: Finding unused address...
      ==
      ;div#receive-content.receive-content
        ;div#receive-qr.receive-qr;
        ;div.receive-addr-row
          ;span#receive-addr.receive-addr;
          ;button#receive-copy-btn.receive-copy(onclick "copyAddr(this)")
            ;svg(xmlns "http://www.w3.org/2000/svg", viewBox "0 0 24 24", width "16", height "16", fill "none", stroke "currentColor", stroke-width "2", stroke-linecap "round", stroke-linejoin "round")
              ;rect(x "9", y "9", width "13", height "13", rx "2", ry "2");
              ;path(d "M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1");
            ==
          ==
        ==
      ==
      ;div#receive-error.receive-empty;
    ==
  ==
::
++  render-send-popup
  |=  [bal=@ud bal-tape=tape]
  ^-  manx
  ::  Estimate max: balance minus worst-case fee
  ::  1 P2WPKH input (68 vB) + 1 output (31 vB) + overhead (11 vB) = 110 vB × 2 sat/vB
  =/  est-fee=@ud  220
  =/  est-max=@ud  ?:((lth bal est-fee) 0 (sub bal est-fee))
  =/  est-max-tape=tape  (format-btc est-max)
  ;div#send-overlay.send-overlay(onclick "closeSend(event)")
    ;div.send-modal
      ;button.send-close(onclick "toggleSend()"): ×
      ;div.send-title: Send Bitcoin
      ;div.send-field
        ;label.send-label: To
        ;input#send-to.send-input(type "text", placeholder "bc1q...", autocomplete "off");
      ==
      ;div.send-field
        ;label.send-label: Amount (BTC)
        ;input#send-amount.send-input(type "text", placeholder "0.00000000", autocomplete "off");
      ==
      ;div.send-balance: Est. max: {est-max-tape} BTC
      ;div#send-status.send-status;
      ;button#send-btn.send-btn(onclick "sendBitcoin()"): Send
    ==
  ==
::
++  render-info-popup
  |=  [wal-seed=tape wal-seed-masked=tape backed-up=? fee-rate=@ud]
  ^-  manx
  =/  fee-tape=tape  (a-co:co fee-rate)
  ;div#info-overlay.info-overlay(onclick "closeInfo(event)")
    ;div.info-modal
      ;button.info-close(onclick "toggleInfo()"): ×
      ;div.info-title: Wallet Info
      ;div.info-section
        ;div.info-label: Recovery Phrase
        ;div.info-seed-row
          ;span.info-seed: {wal-seed-masked}
          ;button.info-copy(onclick "copySeed(this)", data-seed "{wal-seed}")
            ;svg(xmlns "http://www.w3.org/2000/svg", viewBox "0 0 24 24", width "16", height "16", fill "none", stroke "currentColor", stroke-width "2", stroke-linecap "round", stroke-linejoin "round")
              ;rect(x "9", y "9", width "13", height "13", rx "2", ry "2");
              ;path(d "M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1");
            ==
          ==
        ==
        ;div.info-warning: Anyone with this phrase can access your funds. Store it somewhere safe.
      ==
      ;div.info-section
        ;label.info-saved-row
          ;input#info-saved.info-checkbox(type "checkbox", onchange "toggleSaved(this)", data-checked "{?:(backed-up "true" "false")}");
          ;span.info-saved-text: I've saved my recovery phrase
        ==
      ==
      ;div.info-section
        ;div.info-label: Fee Rate (sat/vB)
        ;div.info-fee-row
          ;input#info-fee.info-fee-input(type "number", min "1", value fee-tape);
          ;button.info-fee-save(onclick "saveFeeRate()"): Save
        ==
      ==
    ==
  ==
::
++  render-body
  |=  $:  bal=@ud
          bal-tape=tape
          bal-sats=tape
          pending-in=@ud
          pending-out=@ud
          pending-in-tape=tape
          pending-out-tape=tape
          wal-name=tape
          net-label=tape
          accent=tape
          accent-hover=tape
          accent-bg=tape
          accent-border=tape
          backed-up=?
          wal-seed=tape
          wal-seed-masked=tape
          fee-rate=@ud
          tx-items=(list manx)
          addr-items=[recv=(list manx) chng=(list manx)]
          available-nets=(list network)
      ==
  ^-  manx
  ;html
    ;head
      ;title: Groundwire Wallet
      ;meta(charset "utf-8");
      ;meta(name "viewport", content "width=device-width, initial-scale=1");
      ;+  feather:feather
      ;style
        ; @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap');
        ; body {
        ;   font-family: 'Inter', -apple-system, sans-serif;
        ;   background: var(--b0);
        ;   color: var(--f0);
        ;   margin: 0;
        ;   min-height: 100vh;
        ; }
        ; *, *:focus, *:active, *:focus-visible {
        ;   outline: none !important;
        ;   -webkit-tap-highlight-color: transparent;
        ;   box-shadow: none !important;
        ; }
        ;
        ; /* --- Shell & layout --- */
        ; .wallet-shell {
        ;   max-width: 480px;
        ;   margin: 0 auto;
        ;   min-height: 100vh;
        ;   display: flex;
        ;   flex-direction: column;
        ; }
        ;
        ; /* --- Header --- */
        ; .wallet-header {
        ;   display: flex;
        ;   align-items: center;
        ;   justify-content: space-between;
        ;   padding: 20px 24px 12px;
        ; }
        ; .wallet-header-left {
        ;   display: flex;
        ;   align-items: center;
        ;   gap: 10px;
        ; }
        ; .wallet-logo {
        ;   width: 28px;
        ;   height: 28px;
        ;   border-radius: 6px;
        ;   background: var(--accent);
        ;   display: flex;
        ;   align-items: center;
        ;   justify-content: center;
        ; }
        ; .wallet-title {
        ;   font-size: 16px;
        ;   font-weight: 600;
        ;   letter-spacing: -0.01em;
        ;   cursor: pointer;
        ; }
        ; .wallet-title-input {
        ;   font-size: 16px;
        ;   font-weight: 600;
        ;   letter-spacing: -0.01em;
        ;   background: var(--b1);
        ;   border: 1px solid var(--b3);
        ;   border-radius: 6px;
        ;   color: var(--f0);
        ;   padding: 2px 8px;
        ;   font-family: inherit;
        ; }
        ;
        ; /* --- Balance --- */
        ; .balance-section {
        ;   text-align: center;
        ;   padding: 32px 24px 28px;
        ; }
        ; .balance-label-row {
        ;   display: flex;
        ;   align-items: center;
        ;   justify-content: center;
        ;   gap: 6px;
        ;   margin-bottom: 8px;
        ; }
        ; .balance-label {
        ;   font-size: 13px;
        ;   color: var(--f3);
        ;   text-transform: uppercase;
        ;   letter-spacing: 0.08em;
        ; }
        ; .balance-amount {
        ;   font-size: 40px;
        ;   font-weight: 700;
        ;   letter-spacing: -0.02em;
        ;   line-height: 1;
        ;   margin-bottom: 6px;
        ; }
        ; .balance-unit {
        ;   font-size: 15px;
        ;   color: var(--f4);
        ; }
        ; .balance-fiat {
        ;   margin-top: 8px;
        ;   font-size: 14px;
        ;   color: var(--f3);
        ;   display: flex;
        ;   align-items: center;
        ;   justify-content: center;
        ;   gap: 6px;
        ; }
        ; .balance-rate {
        ;   margin-top: 4px;
        ;   font-size: 11px;
        ;   color: var(--f4);
        ;   text-align: center;
        ; }
        ; .sync-btn {
        ;   background: none;
        ;   border: none;
        ;   color: var(--f4);
        ;   cursor: pointer;
        ;   padding: 2px;
        ;   display: flex;
        ;   align-items: center;
        ;   justify-content: center;
        ;   border-radius: 50%;
        ;   width: 22px;
        ;   height: 22px;
        ;   transition: background 0.15s, color 0.15s;
        ; }
        ; .sync-btn:hover {
        ;   background: var(--b3);
        ;   color: var(--f2);
        ; }
        ; .sync-btn.spinning svg {
        ;   animation: spin 0.6s linear infinite;
        ; }
        ; @keyframes spin {
        ;   from {
        ;     transform: rotate(0deg);
        ;   }
        ;   to {
        ;     transform: rotate(360deg);
        ;   }
        ; }
        ;
        ; /* --- Action buttons --- */
        ; .action-buttons {
        ;   display: flex;
        ;   gap: 12px;
        ;   padding: 0 24px 28px;
        ;   justify-content: center;
        ; }
        ; .action-btn {
        ;   flex: 1;
        ;   max-width: 140px;
        ;   padding: 14px 0;
        ;   border-radius: 14px;
        ;   border: none;
        ;   font-size: 15px;
        ;   font-weight: 600;
        ;   cursor: pointer;
        ;   display: flex;
        ;   align-items: center;
        ;   justify-content: center;
        ;   transition: all 0.15s;
        ;   font-family: inherit;
        ; }
        ; .action-btn-primary {
        ;   background: var(--accent);
        ;   color: #fff;
        ; }
        ; .action-btn-primary:hover {
        ;   background: var(--accent-hover);
        ; }
        ; .action-btn-secondary {
        ;   background: var(--b2);
        ;   color: var(--f0);
        ; }
        ; .action-btn-secondary:hover {
        ;   background: var(--b3);
        ; }
        ;
        ; /* --- Unconfirmed balance --- */
        ; .hidden { display: none; }
        ; .unconf-balance {
        ;   text-align: center;
        ;   padding: 0 24px 24px;
        ;   opacity: 0.5;
        ; }
        ; .unconf-label {
        ;   font-size: 11px;
        ;   color: var(--f4);
        ;   text-transform: uppercase;
        ;   letter-spacing: 0.05em;
        ;   margin-bottom: 6px;
        ; }
        ; .unconf-row {
        ;   display: flex;
        ;   justify-content: center;
        ;   gap: 16px;
        ; }
        ; .unconf-in, .unconf-out {
        ;   display: flex;
        ;   align-items: baseline;
        ;   gap: 4px;
        ; }
        ; .unconf-amount-in {
        ;   font-size: 20px;
        ;   font-weight: 600;
        ;   color: #22c55e;
        ;   line-height: 1;
        ; }
        ; .unconf-amount-out {
        ;   font-size: 20px;
        ;   font-weight: 600;
        ;   color: #ef4444;
        ;   line-height: 1;
        ; }
        ; .unconf-unit {
        ;   font-size: 12px;
        ;   color: var(--f4);
        ; }
        ;
        ; /* --- Misc --- */
        ; .net-badge {
        ;   font-size: 11px;
        ;   padding: 3px 8px;
        ;   border-radius: 6px;
        ;   background: var(--b2);
        ;   color: var(--f3);
        ;   font-weight: 500;
        ;   border: none;
        ;   font-family: inherit;
        ; }
        ; .net-dropdown {
        ;   position: relative;
        ;   display: inline-block;
        ; }
        ; .net-badge-toggle {
        ;   cursor: pointer;
        ;   display: inline-flex;
        ;   align-items: center;
        ;   gap: 4px;
        ; }
        ; .net-badge-toggle:hover {
        ;   background: var(--b3);
        ; }
        ; .net-arrow {
        ;   font-size: 9px;
        ;   opacity: 0.6;
        ; }
        ; .net-menu {
        ;   display: none;
        ;   position: absolute;
        ;   top: calc(100% + 4px);
        ;   left: 0;
        ;   background: var(--b2);
        ;   border: 1px solid var(--b3);
        ;   border-radius: 8px;
        ;   padding: 4px;
        ;   z-index: 100;
        ;   min-width: 100px;
        ; }
        ; .net-menu.open {
        ;   display: block;
        ; }
        ; .net-menu-item {
        ;   display: block;
        ;   width: 100%;
        ;   padding: 6px 10px;
        ;   border: none;
        ;   background: none;
        ;   color: var(--f2);
        ;   font-size: 12px;
        ;   font-family: inherit;
        ;   text-align: left;
        ;   border-radius: 4px;
        ;   cursor: pointer;
        ; }
        ; .net-menu-item:hover {
        ;   background: var(--b3);
        ; }
        ; .backup-banner {
        ;   margin: 0 24px 16px;
        ;   padding: 14px 18px;
        ;   background: var(--accent-bg);
        ;   border: 1px solid var(--accent-border);
        ;   border-radius: 12px;
        ;   text-align: center;
        ;   cursor: pointer;
        ; }
        ; .backup-banner:hover {
        ;   background: var(--accent-bg);
        ; }
        ; .backup-banner-text {
        ;   font-size: 13px;
        ;   font-weight: 600;
        ;   color: var(--accent);
        ; }
        ;
        ; /* --- Tx detail overlay --- */
        ; .tx-detail-overlay {
        ;   position: fixed;
        ;   top: 0; left: 0; right: 0; bottom: 0;
        ;   background: rgba(0,0,0,0.6);
        ;   z-index: 100;
        ;   display: flex;
        ;   align-items: center;
        ;   justify-content: center;
        ; }
        ; .tx-detail-overlay.hidden {
        ;   display: none;
        ; }
        ; .tx-detail-modal {
        ;   background: var(--b0);
        ;   border-radius: 20px;
        ;   padding: 24px;
        ;   max-width: 400px;
        ;   width: 90%;
        ;   position: relative;
        ; }
        ; .tx-detail-row {
        ;   margin-bottom: 16px;
        ; }
        ; .tx-detail-row:last-child {
        ;   margin-bottom: 0;
        ; }
        ; .tx-detail-label {
        ;   font-size: 11px;
        ;   color: var(--f4);
        ;   display: block;
        ;   margin-bottom: 4px;
        ; }
        ; .tx-detail-value {
        ;   font-size: 13px;
        ;   font-family: monospace;
        ;   color: var(--f1);
        ;   white-space: nowrap;
        ;   overflow: hidden;
        ;   text-overflow: ellipsis;
        ;   min-width: 0;
        ; }
        ; .tx-detail-value-row {
        ;   display: flex;
        ;   align-items: center;
        ;   gap: 8px;
        ;   min-width: 0;
        ; }
        ; .tx-detail-value-row .tx-copy-btn {
        ;   flex-shrink: 0;
        ;   margin-top: 2px;
        ; }
        ;
        ; /* --- Receive overlay --- */
        ; .receive-overlay {
        ;   display: none;
        ;   position: fixed;
        ;   top: 0; left: 0; right: 0; bottom: 0;
        ;   background: rgba(0,0,0,0.6);
        ;   z-index: 100;
        ;   align-items: center;
        ;   justify-content: center;
        ; }
        ; .receive-overlay.open {
        ;   display: flex;
        ; }
        ; .receive-modal {
        ;   background: var(--b0);
        ;   border-radius: 20px;
        ;   padding: 28px 24px;
        ;   max-width: 360px;
        ;   width: 90%;
        ;   text-align: center;
        ;   position: relative;
        ; }
        ; .receive-close {
        ;   position: absolute;
        ;   top: 16px;
        ;   right: 16px;
        ;   background: none;
        ;   border: none;
        ;   color: var(--f4);
        ;   font-size: 20px;
        ;   cursor: pointer;
        ;   line-height: 1;
        ; }
        ; .receive-title {
        ;   font-size: 16px;
        ;   font-weight: 600;
        ;   margin-bottom: 20px;
        ; }
        ; .receive-qr {
        ;   display: inline-block;
        ;   padding: 16px;
        ;   background: white;
        ;   border-radius: 12px;
        ;   margin-bottom: 16px;
        ; }
        ; .receive-addr-row {
        ;   display: flex;
        ;   align-items: center;
        ;   gap: 8px;
        ;   padding: 12px 14px;
        ;   background: var(--b1);
        ;   border-radius: 10px;
        ; }
        ; .receive-addr {
        ;   flex: 1;
        ;   font-family: monospace;
        ;   font-size: 13px;
        ;   word-break: break-all;
        ;   text-align: left;
        ; }
        ; .receive-copy {
        ;   color: var(--f3);
        ;   cursor: pointer;
        ;   background: none;
        ;   border: none;
        ;   padding: 4px;
        ;   display: flex;
        ;   align-items: center;
        ;   flex-shrink: 0;
        ;   transition: color 0.15s;
        ; }
        ; .receive-copy:hover {
        ;   color: var(--accent);
        ; }
        ; .receive-content {
        ;   display: none;
        ; }
        ; .receive-content.show {
        ;   display: block;
        ; }
        ; .receive-spinner {
        ;   text-align: center;
        ;   padding: 32px 0;
        ; }
        ; .receive-spinner.hide {
        ;   display: none;
        ; }
        ; .spinner {
        ;   width: 28px;
        ;   height: 28px;
        ;   border: 3px solid var(--b3);
        ;   border-top-color: var(--accent);
        ;   border-radius: 50%;
        ;   animation: spin 0.7s linear infinite;
        ;   margin: 0 auto 12px;
        ; }
        ; .spinner-text {
        ;   font-size: 13px;
        ;   color: var(--f4);
        ; }
        ; .receive-empty {
        ;   display: none;
        ;   text-align: center;
        ;   padding: 24px;
        ;   color: var(--f4);
        ;   font-size: 14px;
        ; }
        ; .receive-empty.show {
        ;   display: block;
        ; }
        ;
        ; /* --- Send overlay --- */
        ; .send-overlay {
        ;   display: none;
        ;   position: fixed;
        ;   top: 0; left: 0; right: 0; bottom: 0;
        ;   background: rgba(0,0,0,0.6);
        ;   z-index: 100;
        ;   align-items: center;
        ;   justify-content: center;
        ; }
        ; .send-overlay.open {
        ;   display: flex;
        ; }
        ; .send-modal {
        ;   background: var(--b0);
        ;   border-radius: 20px;
        ;   padding: 28px 24px;
        ;   max-width: 360px;
        ;   width: 90%;
        ;   position: relative;
        ; }
        ; .send-close {
        ;   position: absolute;
        ;   top: 16px;
        ;   right: 16px;
        ;   background: none;
        ;   border: none;
        ;   color: var(--f4);
        ;   font-size: 20px;
        ;   cursor: pointer;
        ;   line-height: 1;
        ; }
        ; .send-title {
        ;   font-size: 16px;
        ;   font-weight: 600;
        ;   margin-bottom: 20px;
        ; }
        ; .send-field {
        ;   margin-bottom: 16px;
        ; }
        ; .send-label {
        ;   display: block;
        ;   font-size: 12px;
        ;   font-weight: 500;
        ;   color: var(--f3);
        ;   margin-bottom: 6px;
        ; }
        ; .send-input {
        ;   width: 100%;
        ;   padding: 12px 14px;
        ;   background: var(--b1);
        ;   border: 1px solid var(--b3);
        ;   border-radius: 10px;
        ;   color: var(--f0);
        ;   font-size: 15px;
        ;   font-family: monospace;
        ;   box-sizing: border-box;
        ; }
        ; .send-input::placeholder {
        ;   color: var(--f4);
        ; }
        ; .send-balance {
        ;   font-size: 12px;
        ;   color: var(--f4);
        ;   margin-bottom: 20px;
        ; }
        ; .send-btn {
        ;   width: 100%;
        ;   padding: 14px;
        ;   border-radius: 14px;
        ;   border: none;
        ;   background: var(--accent);
        ;   color: #fff;
        ;   font-size: 15px;
        ;   font-weight: 600;
        ;   cursor: pointer;
        ;   font-family: inherit;
        ;   transition: background 0.15s;
        ; }
        ; .send-btn:hover {
        ;   background: var(--accent-hover);
        ; }
        ; .send-btn:disabled {
        ;   opacity: 0.5;
        ;   cursor: not-allowed;
        ; }
        ; .send-status {
        ;   font-size: 12px;
        ;   text-align: center;
        ;   margin-bottom: 8px;
        ;   min-height: 16px;
        ; }
        ; .send-status.error {
        ;   color: #ff3b30;
        ; }
        ; .send-status.success {
        ;   color: #34c759;
        ; }
        ; .send-status.pending {
        ;   color: var(--f4);
        ; }
        ;
        ; /* --- Info overlay --- */
        ; .info-btn {
        ;   background: none;
        ;   border: none;
        ;   color: var(--f4);
        ;   cursor: pointer;
        ;   padding: 4px;
        ;   display: flex;
        ;   align-items: center;
        ; }
        ; .info-btn:hover {
        ;   color: var(--f2);
        ; }
        ; .info-overlay {
        ;   display: none;
        ;   position: fixed;
        ;   top: 0; left: 0; right: 0; bottom: 0;
        ;   background: rgba(0,0,0,0.6);
        ;   z-index: 100;
        ;   align-items: center;
        ;   justify-content: center;
        ; }
        ; .info-overlay.open {
        ;   display: flex;
        ; }
        ; .info-modal {
        ;   background: var(--b0);
        ;   border-radius: 20px;
        ;   padding: 28px 24px;
        ;   max-width: 360px;
        ;   width: 90%;
        ;   position: relative;
        ; }
        ; .info-close {
        ;   position: absolute;
        ;   top: 16px;
        ;   right: 16px;
        ;   background: none;
        ;   border: none;
        ;   color: var(--f4);
        ;   font-size: 20px;
        ;   cursor: pointer;
        ;   line-height: 1;
        ; }
        ; .info-title {
        ;   font-size: 16px;
        ;   font-weight: 600;
        ;   margin-bottom: 20px;
        ; }
        ; .info-section {
        ;   margin-bottom: 20px;
        ; }
        ; .info-section:last-child {
        ;   margin-bottom: 0;
        ; }
        ; .info-label {
        ;   font-size: 12px;
        ;   font-weight: 500;
        ;   color: var(--f3);
        ;   margin-bottom: 8px;
        ; }
        ; .info-seed-row {
        ;   display: flex;
        ;   align-items: flex-start;
        ;   gap: 8px;
        ;   padding: 12px 14px;
        ;   background: var(--b1);
        ;   border-radius: 10px;
        ;   margin-bottom: 10px;
        ; }
        ; .info-seed {
        ;   flex: 1;
        ;   font-family: monospace;
        ;   font-size: 13px;
        ;   word-break: break-all;
        ;   line-height: 1.5;
        ; }
        ; .info-copy {
        ;   color: var(--f3);
        ;   cursor: pointer;
        ;   background: none;
        ;   border: none;
        ;   padding: 4px;
        ;   display: flex;
        ;   align-items: center;
        ;   flex-shrink: 0;
        ;   transition: color 0.15s;
        ; }
        ; .info-copy:hover {
        ;   color: var(--accent);
        ; }
        ; .info-warning {
        ;   font-size: 12px;
        ;   color: var(--f4);
        ;   line-height: 1.4;
        ; }
        ; .info-fee-row {
        ;   display: flex;
        ;   align-items: center;
        ;   gap: 10px;
        ;   padding: 4px 0;
        ; }
        ; .info-fee-input {
        ;   width: 70px;
        ;   padding: 6px 10px;
        ;   border: 1px solid var(--b3);
        ;   border-radius: 8px;
        ;   background: var(--b1);
        ;   color: var(--f1);
        ;   font-size: 14px;
        ;   font-family: inherit;
        ; }
        ; .info-fee-input:focus {
        ;   outline: none;
        ;   border-color: var(--accent);
        ; }
        ; .info-fee-save {
        ;   padding: 6px 14px;
        ;   border: none;
        ;   border-radius: 8px;
        ;   background: var(--accent);
        ;   color: white;
        ;   font-size: 13px;
        ;   cursor: pointer;
        ;   font-family: inherit;
        ; }
        ; .info-fee-save:hover {
        ;   background: var(--accent-hover);
        ; }
        ; .info-saved-row {
        ;   display: flex;
        ;   align-items: center;
        ;   gap: 10px;
        ;   cursor: pointer;
        ;   padding: 10px 0;
        ; }
        ; .info-checkbox {
        ;   width: 18px;
        ;   height: 18px;
        ;   accent-color: var(--accent);
        ;   cursor: pointer;
        ;   flex-shrink: 0;
        ; }
        ; .info-saved-text {
        ;   font-size: 14px;
        ;   color: var(--f1);
        ;   cursor: pointer;
        ; }
        ;
        ; /* --- Tab panel --- */
        ; .tab-panel {
        ;   flex: 1;
        ;   display: flex;
        ;   flex-direction: column;
        ;   min-height: 0;
        ;   margin: 0 24px 24px;
        ;   background: var(--b1);
        ;   border: 1px solid var(--b3);
        ;   border-radius: 16px;
        ;   overflow: hidden;
        ; }
        ; .tab-bar {
        ;   display: flex;
        ;   background: var(--b2);
        ;   padding: 4px;
        ;   gap: 0;
        ; }
        ; .tab-btn {
        ;   flex: 1;
        ;   background: transparent;
        ;   border: none;
        ;   border-radius: 12px;
        ;   color: var(--f4);
        ;   font-size: 13px;
        ;   font-weight: 500;
        ;   padding: 8px 0;
        ;   cursor: pointer;
        ;   font-family: inherit;
        ;   transition: all 0.15s;
        ; }
        ; .tab-btn:hover {
        ;   color: var(--f2);
        ; }
        ; .tab-btn.active {
        ;   background: var(--accent-bg);
        ;   color: var(--accent);
        ;   font-weight: 600;
        ; }
        ; .tab-content {
        ;   flex: 1;
        ;   display: flex;
        ;   flex-direction: column;
        ;   min-height: 0;
        ;   overflow-y: auto;
        ;   padding: 12px 16px;
        ; }
        ; .tab-content.hidden {
        ;   display: none;
        ; }
        ; .activity-empty {
        ;   text-align: center;
        ;   padding: 32px 0;
        ;   color: var(--f4);
        ;   font-size: 14px;
        ; }
        ; .activity-list {
        ;   overflow-y: auto;
        ;   flex: 1;
        ; }
        ; .activity-tx {
        ;   display: flex;
        ;   flex-direction: row;
        ;   gap: 12px;
        ;   padding: 12px 0;
        ;   border-bottom: 1px solid var(--b2);
        ;   align-items: center;
        ;   cursor: pointer;
        ; }
        ; .activity-tx:hover {
        ;   opacity: 0.8;
        ; }
        ; .activity-tx:last-child {
        ;   border-bottom: none;
        ; }
        ; .activity-tx-icon {
        ;   width: 36px;
        ;   height: 36px;
        ;   border-radius: 8px;
        ;   display: flex;
        ;   align-items: center;
        ;   justify-content: center;
        ;   flex-shrink: 0;
        ; }
        ; .activity-tx-icon.tx-received {
        ;   background: #10b981;
        ;   color: #fff;
        ; }
        ; .activity-tx-icon.tx-sent {
        ;   background: var(--b3);
        ;   color: var(--f2);
        ; }
        ; .activity-tx-body {
        ;   display: flex;
        ;   flex-direction: column;
        ;   gap: 2px;
        ;   min-width: 0;
        ;   flex: 1;
        ; }
        ; .activity-tx-addr {
        ;   font-size: 13px;
        ;   font-family: monospace;
        ;   color: var(--f1);
        ;   white-space: nowrap;
        ;   overflow: hidden;
        ;   text-overflow: ellipsis;
        ; }
        ; .activity-tx-meta {
        ;   display: flex;
        ;   gap: 8px;
        ;   align-items: center;
        ; }
        ; .activity-tx-top {
        ;   display: flex;
        ;   justify-content: space-between;
        ;   align-items: center;
        ; }
        ; .activity-tx-value {
        ;   display: flex;
        ;   flex-direction: column;
        ;   align-items: flex-end;
        ;   flex-shrink: 0;
        ;   margin-left: auto;
        ; }
        ; .activity-tx-amt {
        ;   font-size: 14px;
        ;   font-weight: 600;
        ;   white-space: nowrap;
        ; }
        ; .activity-tx-fiat {
        ;   font-size: 11px;
        ;   color: var(--f4);
        ;   white-space: nowrap;
        ; }
        ; .activity-tx-amt.tx-sent {
        ;   color: var(--f1);
        ; }
        ; .activity-tx-amt.tx-received {
        ;   color: #10b981;
        ; }
        ; .activity-tx-detail {
        ;   display: flex;
        ;   align-items: center;
        ;   gap: 6px;
        ;   min-width: 0;
        ; }
        ; .activity-tx-label {
        ;   font-size: 11px;
        ;   color: var(--f4);
        ;   flex-shrink: 0;
        ; }
        ; .activity-tx-mono {
        ;   font-size: 11px;
        ;   font-family: monospace;
        ;   color: var(--f3);
        ;   white-space: nowrap;
        ;   overflow: hidden;
        ;   text-overflow: ellipsis;
        ;   min-width: 0;
        ; }
        ; .tx-copy-btn {
        ;   background: none;
        ;   border: none;
        ;   color: var(--f4);
        ;   cursor: pointer;
        ;   padding: 1px;
        ;   display: flex;
        ;   align-items: center;
        ;   flex-shrink: 0;
        ;   opacity: 0.5;
        ;   transition: opacity 0.15s;
        ; }
        ; .tx-copy-btn:hover {
        ;   opacity: 1;
        ; }
        ; .activity-tx-bottom {
        ;   display: flex;
        ;   justify-content: space-between;
        ;   align-items: center;
        ; }
        ; .activity-tx-status {
        ;   font-size: 11px;
        ;   color: var(--f4);
        ; }
        ; .activity-tx-status.pending {
        ;   color: var(--accent);
        ; }
        ; .activity-tx-time {
        ;   font-size: 11px;
        ;   color: var(--f4);
        ; }
        ;
        ; /* --- Addresses popup --- */
        ; .addr-tabs {
        ;   display: flex;
        ;   gap: 4px;
        ;   margin-bottom: 12px;
        ; }
        ; .addr-tab {
        ;   flex: 1;
        ;   background: var(--b2);
        ;   border: 1px solid var(--b3);
        ;   border-radius: 6px;
        ;   color: var(--f4);
        ;   font-size: 12px;
        ;   font-weight: 500;
        ;   padding: 6px 0;
        ;   cursor: pointer;
        ;   font-family: inherit;
        ;   transition: all 0.15s;
        ; }
        ; .addr-tab:hover {
        ;   color: var(--f2);
        ; }
        ; .addr-tab.active {
        ;   background: var(--accent-bg);
        ;   border-color: var(--accent-border);
        ;   color: var(--accent);
        ; }
        ; .addr-list {
        ;   display: flex;
        ;   flex-direction: column;
        ;   gap: 4px;
        ;   max-height: 400px;
        ;   overflow-y: auto;
        ; }
        ; .addr-item {
        ;   display: flex;
        ;   justify-content: space-between;
        ;   align-items: center;
        ;   padding: 6px 8px;
        ;   background: var(--b1);
        ;   border: 1px solid var(--b3);
        ;   border-radius: 6px;
        ;   gap: 8px;
        ; }
        ; .addr-item-left {
        ;   display: flex;
        ;   align-items: center;
        ;   gap: 6px;
        ;   min-width: 0;
        ; }
        ; .addr-next, .addr-next-spacer {
        ;   width: 7px;
        ;   height: 7px;
        ;   flex-shrink: 0;
        ; }
        ; .addr-next {
        ;   border-radius: 50%;
        ;   background: #34c759;
        ;   cursor: default;
        ; }
        ; .addr-idx {
        ;   font-size: 11px;
        ;   font-family: monospace;
        ;   color: var(--f4);
        ;   flex-shrink: 0;
        ; }
        ; .addr-short {
        ;   font-size: 12px;
        ;   font-family: monospace;
        ;   color: var(--f2);
        ;   white-space: nowrap;
        ;   overflow: hidden;
        ;   text-overflow: ellipsis;
        ; }
        ; .addr-pending {
        ;   font-size: 9px;
        ;   color: var(--accent);
        ;   font-weight: 600;
        ;   text-transform: uppercase;
        ;   flex-shrink: 0;
        ; }
        ; .addr-item-right {
        ;   display: flex;
        ;   align-items: center;
        ;   flex-shrink: 0;
        ;   gap: 6px;
        ; }
        ; .addr-bal {
        ;   font-size: 11px;
        ;   font-weight: 500;
        ;   font-family: monospace;
        ;   color: var(--f3);
        ; }
        ; .addr-clock {
        ;   display: flex;
        ;   align-items: center;
        ;   color: var(--f4);
        ;   opacity: 0.3;
        ;   cursor: default;
        ;   transition: opacity 0.15s;
        ; }
        ; .addr-clock:hover {
        ;   opacity: 0.8;
        ; }
        ; .addr-refresh {
        ;   background: none;
        ;   border: none;
        ;   color: var(--f4);
        ;   cursor: pointer;
        ;   padding: 2px;
        ;   display: flex;
        ;   align-items: center;
        ;   opacity: 0.5;
        ;   transition: opacity 0.15s;
        ; }
        ; .addr-refresh:hover {
        ;   opacity: 1;
        ; }
        ; .addr-refresh.spinning svg {
        ;   animation: spin 0.6s linear infinite;
        ; }
        ; .addr-empty {
        ;   text-align: center;
        ;   padding: 24px 0;
        ;   font-size: 13px;
        ;   color: var(--f4);
        ; }
        ;
        ; /* --- Desktop: 768px+ --- */
        ; @media (min-width: 768px) {
        ;   .wallet-shell {
        ;     max-width: 900px;
        ;     min-height: 100vh;
        ;   }
        ;   .wallet-header {
        ;     padding: 24px 32px 16px;
        ;   }
        ;   .balance-section {
        ;     padding: 40px 32px 36px;
        ;   }
        ;   .balance-amount {
        ;     font-size: 52px;
        ;   }
        ;   .action-buttons {
        ;     padding: 0 32px 36px;
        ;     gap: 16px;
        ;   }
        ;   .action-btn {
        ;     max-width: 160px;
        ;     padding: 16px 0;
        ;     border-radius: 16px;
        ;     font-size: 16px;
        ;   }
        ; }
        ;
        ; /* --- Wide desktop: 1100px+ --- */
        ; @media (min-width: 1100px) {
        ;   .wallet-shell {
        ;     max-width: 1060px;
        ;   }
        ;   .balance-amount {
        ;     font-size: 58px;
        ;   }
        ; }
      ==
      ;script(src "https://cdnjs.cloudflare.com/ajax/libs/qrcodejs/1.0.0/qrcode.min.js");
      ;script
        ; function fetchPrice() {
        ;   var el = document.getElementById('fiat-value');
        ;   if (!el) return;
        ;   var sats = parseInt(el.dataset.sats, 10);
        ;   var url = 'https://api.coingecko.com/api/v3/simple/price';
        ;   url += '?ids=bitcoin';
        ;   url += '&vs_currencies=usd';
        ;   fetch(url)
        ;     .then(function(r) {
        ;       return r.json();
        ;     }
        ;     )
        ;     .then(function(d) {
        ;       var price = d.bitcoin.usd;
        ;       var usd = (sats / 100000000) * price;
        ;       var fmtOpts = new Object();
        ;       fmtOpts.style = 'currency';
        ;       fmtOpts.currency = 'USD';
        ;       el.textContent = usd.toLocaleString('en-US', fmtOpts);
        ;       var rateEl = document.getElementById('btc-rate');
        ;       if (rateEl) {
        ;         var rateFmt = new Object();
        ;         rateFmt.style = 'currency';
        ;         rateFmt.currency = 'USD';
        ;         rateFmt.maximumFractionDigits = 0;
        ;         rateEl.textContent = '1 BTC = ' + price.toLocaleString('en-US', rateFmt);
        ;       }
        ;       var txFiats = document.querySelectorAll('.activity-tx-fiat');
        ;       for (var i = 0; i < txFiats.length; i++) {
        ;         var s = parseInt(txFiats[i].dataset.sats, 10);
        ;         if (isNaN(s)) continue;
        ;         var val = (s / 100000000) * price;
        ;         txFiats[i].textContent = val.toLocaleString('en-US', fmtOpts);
        ;       }
        ;     }
        ;     )
        ;     .catch(function(err) {
        ;       console.error('fetchPrice error', err);
        ;     }
        ;     );
        ; }
        ; function refreshAddr(chain, index) {
        ;   var btn = event.currentTarget;
        ;   btn.classList.add('spinning');
        ;   walletPost('action=refresh-address&chain=' + chain + '&index=' + index)
        ;   .then(function() {
        ;     location.reload();
        ;   }).catch(function(err) {
        ;     console.error('refreshAddr error', err);
        ;     btn.classList.remove('spinning');
        ;   });
        ; }
        ; function refreshWallet() {
        ;   var btn = document.getElementById('sync-btn');
        ;   if (btn) btn.classList.add('spinning');
        ;   walletPost('action=refresh-wallet').then(function() {
        ;     location.reload();
        ;   }).catch(function(err) {
        ;     console.error('refreshWallet error', err);
        ;     if (btn) btn.classList.remove('spinning');
        ;   });
        ; }
        ; document.addEventListener('DOMContentLoaded', function() {
        ;   fetchPrice();
        ;   var cb = document.getElementById('info-saved');
        ;   if (cb) cb.checked = cb.dataset.checked === 'true';
        ; });
        ; var copyIcon = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" width="16" height="16" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><rect x="9" y="9" width="13" height="13" rx="2" ry="2"></rect><path d="M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1"></path></svg>';
        ; var checkIcon = '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" width="16" height="16" fill="none" stroke="#10b981" stroke-width="2.5" stroke-linecap="round" stroke-linejoin="round"><polyline points="20 6 9 17 4 12"></polyline></svg>';
        ; function flashCheck(btn, size) {
        ;   var orig = btn.innerHTML;
        ;   btn.innerHTML = checkIcon.replace(/16/g, size || '16');
        ;   setTimeout(function() { btn.innerHTML = orig; }, 1500);
        ; }
        ; function copySeed(btn) {
        ;   navigator.clipboard.writeText(btn.dataset.seed).then(function() {
        ;     flashCheck(btn, '14');
        ;   });
        ; }
        ; function copyAddr(btn) {
        ;   navigator.clipboard.writeText(btn.dataset.addr).then(function() {
        ;     flashCheck(btn);
        ;   });
        ; }
        ; function showTxDetail(row) {
        ;   var overlay = document.getElementById('tx-detail-overlay');
        ;   var txid = row.dataset.txid;
        ;   var addr = row.dataset.addr;
        ;   var addrLabel = row.dataset.addrLabel;
        ;   var status = row.dataset.status;
        ;   var time = row.dataset.time;
        ;   document.getElementById('tx-detail-txid').textContent = txid;
        ;   var txidBtn = document.getElementById('tx-detail-txid').parentElement.querySelector('.tx-copy-btn');
        ;   if (txidBtn) txidBtn.dataset.txid = txid;
        ;   var addrRow = document.getElementById('tx-detail-addr');
        ;   if (addr) {
        ;     addrRow.classList.remove('hidden');
        ;     document.getElementById('tx-detail-addr-label').textContent = addrLabel;
        ;     document.getElementById('tx-detail-addr-value').textContent = addr;
        ;     var addrBtn = addrRow.querySelector('.tx-copy-btn');
        ;     if (addrBtn) addrBtn.dataset.txid = addr;
        ;   } else {
        ;     addrRow.classList.add('hidden');
        ;   }
        ;   document.getElementById('tx-detail-status').textContent = status;
        ;   var timeRow = document.getElementById('tx-detail-time-row');
        ;   if (time) {
        ;     timeRow.classList.remove('hidden');
        ;     document.getElementById('tx-detail-time').textContent = time;
        ;   } else {
        ;     timeRow.classList.add('hidden');
        ;   }
        ;   overlay.classList.remove('hidden');
        ; }
        ; function closeTxDetail(e) {
        ;   var overlay = document.getElementById('tx-detail-overlay');
        ;   if (!e || e.target === overlay) overlay.classList.add('hidden');
        ; }
        ; function copyTxid(btn) {
        ;   if (event) event.stopPropagation();
        ;   navigator.clipboard.writeText(btn.dataset.txid).then(function() {
        ;     flashCheck(btn, '10');
        ;   });
        ; }
        ; function toggleReceive() {
        ;   var overlay = document.getElementById('receive-overlay');
        ;   var isOpen = overlay.classList.contains('open');
        ;   if (isOpen) {
        ;     overlay.classList.remove('open');
        ;     return;
        ;   }
        ;   overlay.classList.add('open');
        ;   document.getElementById('receive-spinner').classList.remove('hide');
        ;   document.getElementById('receive-content').classList.remove('show');
        ;   document.getElementById('receive-error').classList.remove('show');
        ;   var qr = document.getElementById('receive-qr');
        ;   qr.innerHTML = '';
        ;   walletPost('action=get-receive-address')
        ;   .then(function(r) {
        ;     return r.text();
        ;   }
        ;   )
        ;   .then(function(addr) {
        ;     addr = addr.trim();
        ;     if (!addr) {
        ;       document.getElementById('receive-spinner').classList.add('hide');
        ;       document.getElementById('receive-error').textContent = 'No address available';
        ;       document.getElementById('receive-error').classList.add('show');
        ;       return;
        ;     }
        ;     document.getElementById('receive-addr').textContent = addr;
        ;     document.getElementById('receive-copy-btn').dataset.addr = addr;
        ;     new QRCode(qr, {
        ;       text: 'bitcoin:' + addr, width: 200, height: 200
        ;     }
        ;     );
        ;     document.getElementById('receive-spinner').classList.add('hide');
        ;     document.getElementById('receive-content').classList.add('show');
        ;   }
        ;   )
        ;   .catch(function() {
        ;     document.getElementById('receive-spinner').classList.add('hide');
        ;     document.getElementById('receive-error').textContent = 'Failed to fetch address';
        ;     document.getElementById('receive-error').classList.add('show');
        ;   }
        ;   );
        ; }
        ; function closeReceive(e) {
        ;   if (e.target === document.getElementById('receive-overlay')) {
        ;     toggleReceive();
        ;   }
        ; }
        ; function walletPost(params) {
        ;   params += '&net=' + document.querySelector('.wallet-shell').dataset.net;
        ;   return fetch('/apps/wallet', {
        ;     method: 'POST',
        ;     headers: {'Content-Type': 'application/x-www-form-urlencoded'},
        ;     body: params
        ;   });
        ; }
        ; function toggleNetDropdown(e) {
        ;   e.stopPropagation();
        ;   document.getElementById('net-menu').classList.toggle('open');
        ; }
        ; function switchNet(net) {
        ;   window.location.search = '?net=' + net;
        ; }
        ; document.addEventListener('click', function() {
        ;   var m = document.getElementById('net-menu');
        ;   if (m) m.classList.remove('open');
        ; });
        ; function toggleSend() {
        ;   document.getElementById('send-overlay').classList.toggle('open');
        ; }
        ; function closeSend(e) {
        ;   if (e.target === document.getElementById('send-overlay')) {
        ;     toggleSend();
        ;   }
        ; }
        ; function sendBitcoin() {
        ;   var addr = document.getElementById('send-to').value.trim();
        ;   var amtStr = document.getElementById('send-amount').value.trim();
        ;   var feeRate = document.getElementById('info-fee').value || '2';
        ;   var status = document.getElementById('send-status');
        ;   var btn = document.getElementById('send-btn');
        ;   status.className = 'send-status';
        ;   status.textContent = '';
        ;   if (!addr) {
        ;     status.className = 'send-status error';
        ;     status.textContent = 'Enter a destination address';
        ;     return;
        ;   }
        ;   var amt = parseFloat(amtStr);
        ;   if (isNaN(amt) || amt <= 0) {
        ;     status.className = 'send-status error';
        ;     status.textContent = 'Enter a valid amount';
        ;     return;
        ;   }
        ;   var sats = Math.round(amt * 100000000);
        ;   if (sats < 546) {
        ;     status.className = 'send-status error';
        ;     status.textContent = 'Amount below dust limit (546 sats)';
        ;     return;
        ;   }
        ;   if (!confirm('Send ' + amtStr + ' BTC to ' + addr + '?')) return;
        ;   btn.disabled = true;
        ;   status.className = 'send-status pending';
        ;   status.textContent = 'Building & broadcasting...';
        ;   var body = 'action=send-bitcoin&address=' + encodeURIComponent(addr)
        ;     + '&amount=' + sats + '&fee-rate=' + (feeRate || '2');
        ;   walletPost(body).then(function(res) {
        ;     if (!res.ok) throw new Error('HTTP ' + res.status);
        ;     status.className = 'send-status success';
        ;     status.textContent = 'Transaction broadcast!';
        ;     setTimeout(function() { location.reload(); }, 1500);
        ;   }).catch(function(err) {
        ;     status.className = 'send-status error';
        ;     status.textContent = 'Send failed: ' + err.message;
        ;     btn.disabled = false;
        ;   });
        ; }
        ; function editWalletName(span) {
        ;   var name = span.dataset.name;
        ;   var input = document.createElement('input');
        ;   input.type = 'text';
        ;   input.value = name;
        ;   input.className = 'wallet-title-input';
        ;   input.size = Math.max(name.length, 8);
        ;   span.replaceWith(input);
        ;   input.focus();
        ;   input.select();
        ;   function save() {
        ;     var val = input.value.trim();
        ;     if (!val || val === name) {
        ;       var s = document.createElement('span');
        ;       s.className = 'wallet-title';
        ;       s.textContent = name;
        ;       s.dataset.name = name;
        ;       s.onclick = function() {
        ;         editWalletName(s);
        ;       };
        ;       input.replaceWith(s);
        ;       return;
        ;     }
        ;     walletPost('action=rename-wallet&name=' + encodeURIComponent(val))
        ;     .then(function() {
        ;       location.reload();
        ;     });
        ;   }
        ;   input.addEventListener('keydown', function(e) {
        ;     if (e.key === 'Enter') {
        ;       e.preventDefault(); save();
        ;     }
        ;     if (e.key === 'Escape') {
        ;       var s = document.createElement('span');
        ;       s.className = 'wallet-title';
        ;       s.textContent = name;
        ;       s.dataset.name = name;
        ;       s.onclick = function() {
        ;         editWalletName(s);
        ;       };
        ;       input.replaceWith(s);
        ;     }
        ;   });
        ;   input.addEventListener('blur', save);
        ; }
        ; function switchTab(tab, btn) {
        ;   document.getElementById('tab-activity').classList.toggle('hidden', tab !== 'activity');
        ;   document.getElementById('tab-addresses').classList.toggle('hidden', tab !== 'addresses');
        ;   var tabs = btn.parentElement.querySelectorAll('.tab-btn');
        ;   for (var i = 0; i < tabs.length; i++) tabs[i].classList.remove('active');
        ;   btn.classList.add('active');
        ; }
        ; function switchAddrTab(tab, btn) {
        ;   document.getElementById('addr-recv').classList.toggle('hidden', tab !== 'recv');
        ;   document.getElementById('addr-chng').classList.toggle('hidden', tab !== 'chng');
        ;   var tabs = btn.parentElement.querySelectorAll('.addr-tab');
        ;   for (var i = 0; i < tabs.length; i++) tabs[i].classList.remove('active');
        ;   btn.classList.add('active');
        ; }
        ; function toggleInfo() {
        ;   document.getElementById('info-overlay').classList.toggle('open');
        ; }
        ; function closeInfo(e) {
        ;   if (e.target === document.getElementById('info-overlay')) {
        ;     toggleInfo();
        ;   }
        ; }
        ; function toggleSaved(cb) {
        ;   walletPost('action=toggle-saved').then(function() {
        ;     location.reload();
        ;   });
        ; }
        ; function saveFeeRate() {
        ;   var fee = document.getElementById('info-fee').value || '2';
        ;   walletPost('action=set-fee-rate&fee-rate=' + fee).then(function() {
        ;     location.reload();
        ;   });
        ; }
      ==
    ==
    ;body
      ;div.wallet-shell(style "--accent: {accent}; --accent-hover: {accent-hover}; --accent-bg: {accent-bg}; --accent-border: {accent-border};", data-net net-label)
        ;+  (render-header wal-name net-label available-nets)
        ;+  (render-banner backed-up)
        ;+  (render-balance bal-tape bal-sats pending-in pending-out pending-in-tape pending-out-tape)
        ;+  (render-actions)
        ;+  (render-tab-panel tx-items addr-items)
        ;+  (render-tx-detail-popup)
        ;+  (render-receive-popup)
        ;+  (render-send-popup bal bal-tape)
        ;+  (render-info-popup wal-seed wal-seed-masked backed-up fee-rate)
      ==
    ==
  ==
--
