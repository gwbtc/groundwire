/-  ord, urb, bitcoin
/+  *test, ul=urb-core, urb-encoder, scr=btc-script, cc=gw-btc-pass
=>
|%
++  start-hash
  0x1.62b3.04e4.d48c.3a53.d80a.96de.0210.d325.c0a9.a464.8b3c
::
++  start-height  943.140
::
++  bunt-id
  ^-  id:block:bitcoin
  [start-hash start-height]
::
++  mock-tx
  ^-  tx:bitcoin
  :*  id=0xabc1.2345.6789.def0.1234.5678.9abc.def0.1234.5678.9abc.def0.1234.5678
      ^=  is
      ^-  (list inputw:tx:bitcoin)
      :~  :*  witness=~
              id=0x1234.5678.9abc.def0.1234.5678.9abc.def0.1234.5678.9abc.def0.1234.5678
              pos=0
              sequence=[wid=4 dat=0xffff.ffff]
              script-sig=`[wid=70 dat=0x3045.0221.00ab.cdef]
              pubkey=`[wid=33 dat=0x279.be66.7ef9.dcbb.ac55]
          ==
      ==
      ^=  os
      ^-  (list output:tx:bitcoin)
      :~  :*  script-pubkey=[wid=25 dat=0x76.a914.88ac]
              value=50.000.000
          ==
          :*  script-pubkey=[wid=25 dat=0x76.a914.99ac]
              value=50.000.000
          ==
      ==
      locktime=0
      nversion=1
      segwit=~
  ==
::
++  mock-block
  ^-  block:bitcoin
  :*  start-hash        ::  hash
      50.000.000        ::  reward
      start-height      ::  height
      txs=[mock-tx]~    ::  transactions
  ==
::
::  generate mock keypair
++  cut
  |=  sed=pass
  =/  lyf  1  ::  $life
  =/  xtr  0  ::  extra data
  =<  ?>(&(?=(%c suite.+<) ?=(^ sek.+<)) .)
  %:  pit:nu:cric:crypto
      512  (shaz (jam sed lyf))
      %c   (make-dat:cc [(shax sed) 0 0])
      xtr
  ==
::
++  mock-output
  |=  sed=pass
  ^-  output:tx:bitcoin
  ::  use seed for script pubkey
  :*  script-pubkey=[wid=34 dat=(end 5 sed)]
      value=50.000.000
  ==
::
++  mock-output-hash
  |=  sed=pass
  ^-  @ux
  =/  out  (mock-output sed)
  =/  en-out  (can 3 script-pubkey.out [8 value.out] ~)
  (shay (add 8 wid.script-pubkey.out) en-out)
::
++  mk-sot
  |=  [sot=skim-sotx:urb sed=pass]
  ^-  sotx:urb
  =/  ent  (skim:encode:urb-encoder sot)
  =/  sig  (sign-octs-raw:ed:crypto [512 (shaz ent)] [sgn.pub sgn.sek]:+<:(cut sed))
  [[`@p`fig:ex:(cut sed) [~ sig]] sot]
::
++  mk-raw-sot
  |=  [sot=skim-sotx:urb sed=pass]
  ^-  octs
  =/  ent  (skim:encode:urb-encoder sot)
  [(met 3 ent) ent]
::
++  mk-skim-spawn
  |=  sed=pass
  ^-  skim-sotx:urb
  [%spawn pub:ex:(cut sed) ~ [spkh=(mock-output-hash sed) vout=`0 off=0 tej=0]]
::
++  mk-skim-adopt
  |=  sed=pass
  ^-  skim-sotx:urb
  [%adopt `@p`fig:ex:(cut sed)]
::
++  mock-skim-batch
  ^-  skim-sotx:urb
  :-  %batch
  :~  [%fief `[%if p=.127.0.0.1 q=8.080]]  ::  set IPv4 fief
      [%fief `[%if p=.192.168.1.1 q=80]]   ::  update IPv4 fief
      [%fief ~]                            ::  clear fief
  ==
::
++  mock-skim-fief
  ^-  skim-sotx:urb
  [%fief `[%if p=.127.0.0.1 q=8.080]]
::
++  mk-skim-escape
  |=  sed=pass
  ^-  skim-sotx:urb
  [%escape `@p`fig:ex:(cut sed) ~]
::
++  mk-skim-cancel-escape
  |=  sed=pass
  ^-  skim-sotx:urb
  [%cancel-escape parent=`@p`fig:ex:(cut sed)]
::
++  mk-skim-detach
  |=  sed=pass
  ^-  skim-sotx:urb
  [%detach `@p`fig:ex:(cut sed)]
::
++  mk-skim-reject
  |=  sed=pass
  ^-  skim-sotx:urb
  [%reject `@p`fig:ex:(cut sed)]
::
++  mk-tx-with-urb-witness
  |=  =skim-sotx:urb
  ^-  tx:bitcoin
  =/  =urb-tx:urb       (mk-urb-tx skim-sotx 0xdead.beef)
  =/  =data:urb-tx:urb  +.urb-tx
  :*  id.urb-tx
      ^-  dataw:tx:bitcoin
      :*  ::  Convert urb inputs to bitcoin inputs
          ^-  (list inputw:tx:bitcoin)
          %+  turn
            is.data
          |=  inp=input:urb-tx:urb
          +.inp
          os.data
          locktime.data
          nversion.data
          segwit.data
      ==
  ==
::
++  mock-coinbase-tx
  ^-  tx:bitcoin
  :*  id=0x1111.2222.3333.4444.5555.6666.7777.8888.9999.aaaa.bbbb.cccc.dddd.eeee.ffff
      ^-  dataw:tx:bitcoin
      :*  ^=  is
          ^-  (list inputw:tx:bitcoin)
          :~  :-  ~       ::  coinbase usually has no witness
              :*  id=0x0  ::  coinbase input references null hash
                  pos=4.294.967.295
                  sequence=[wid=4 dat=0xffff.ffff]
                  script-sig=~
                  pubkey=~
              ==
          ==
          ^=  os
          ^-  (list output:tx:bitcoin)
          :~  :*  script-pubkey=[wid=25 dat=0x76.a914.88ac]  ::  output
                  value=50.000.000                           ::  reward
              ==
          ==
          locktime=0
          nversion=1
          segwit=~
      ==
  ==
::
++  mk-block-with-urb-deps-output
  |=  =skim-sotx:urb
  ^-  block:bitcoin
  :*  hax=0x0
      reward=0
      height=start-height
      =/  tx
        (mk-tx-with-urb-witness skim-sotx)
      ^=  txs
      ^-  (list tx:bitcoin)
      :~  mock-coinbase-tx  ::  coinbase tx comes first
          ::  XX duplicated because ned=&
          ::       what is ned?
          tx
          tx
      ==
  ==
::
++  bunt-deps
  *(map [txid:ord vout:ord] [sots=(list raw-sotx:urb) value=(unit @ud)])
::
++  mock-deps
  |=  sed=pass
  ^+  bunt-deps
  %-  my
  :~  :-  ^-  [txid:ord vout:ord]
          [(shax sed) 0]
      :-  ^=  sots
          ^-  (list raw-sotx:urb)
          :~  :-  raw=(mk-raw-sot (mk-skim-spawn sed) sed)
              sot=(mk-sot (mk-skim-spawn sed) sed)
          ==
      value=(some 50.000.000)
  ==
::
++  mock-deps-no-value
  |=  sed=pass
  ^+  bunt-deps
  %-  my
  :~  :-  ^-  [txid:ord vout:ord]
          [(shax sed) 0]
      :-  ^=  sots
          ^-  (list raw-sotx:urb)
          :~  :-  raw=(mk-raw-sot (mk-skim-spawn sed) sed)
              sot=(mk-sot (mk-skim-spawn sed) sed)
          ==
      value=~
  ==
::
++  bunt-effect
  *effect:urb
::
++  init-state
  ^-  state:urb
  :*  bunt-id        ::  last indexed block
      *sont-map:ord  ::  known satpoints
      *insc-ids:ord  ::  transactions with inscriptions
      *unv-ids:urb   ::  transactions with unvelopes
  ==
::
++  bunt-fx
  ^-  (list [id:block:bitcoin effect:urb])
  [[bunt-id bunt-effect]]~
::
++  mock-urb-coinbase-tx
  ^-  tx:urb-tx:urb
  :-  id=0x1111.2222.3333.4444.5555.6666.7777.8888.9999.aaaa.bbbb.cccc.dddd.eeee.ffff
  :*  ^=  is
      ^-  (list input:urb-tx:urb)
      :~  :-  :-  ~  ::  empty sots list - coinbase has no urb data
              0      ::  value 0 - coinbase has no input value
          ^-  inputw:tx:bitcoin
          :-  ~
          :*  id=0x0
              pos=4.294.967.295
              sequence=[wid=4 dat=0xffff.ffff]
              script-sig=~
              pubkey=~
          ==
      ==
      ^=  os
      ^-  (list output:tx:bitcoin)
      :~  :*  script-pubkey=[wid=25 dat=0x76.a914.88ac]  ::  output
              value=50.000.000                           ::  reward
          ==
      ==
      locktime=0
      nversion=1
      segwit=~
  ==
::
++  mk-urb-tx
  |=  [=skim-sotx:urb sed=pass]
  ^-  tx:urb-tx:urb
  =/  sot  (mk-sot skim-sotx sed)      ::  signed sotx
  =/  raw  (mk-raw-sot skim-sotx sed)  ::  encoded unvelope
  :*  id=(shax sed)
      ^-  data:urb-tx:urb
      :*  ^=  is
          ^-  (list input:urb-tx:urb)
          :~  :-  :-  :~  :-  raw
                          sot
                      ==
                  50.000.000
              :-  ^=  witness
                      :~  raw
                      [wid=0 dat=0x0]  :: OP_0 (for P2TR structure)
                  ==
              :*  id=(shax sed)
                  pos=0
                  sequence=[wid=4 dat=0xffff.ffff]
                  script-sig=~  ::  empty for P2TR
                  pubkey=~      ::  empty for P2TR
              ==
          ==
          ^=  os
          ^-  (list output:tx:bitcoin)
          :~  (mock-output sed)
              (mock-output (add 1 sed))
          ==
          locktime=0
          nversion=2  ::  version 2 for taproot
          segwit=`1   ::  segwit version 1 for taproot
      ==
  ==
::
++  mk-urb-block
  |=  =skim-sotx:urb
  ^-  urb-block:urb
  =/  urb-tx
    (mk-urb-tx skim-sotx 0xdead.beef)
  :*  hax=start-hash
      reward=0
      height=start-height
      ^=  txs
      ^-  (list urb-tx:urb)
      :~  mock-urb-coinbase-tx
          ::  XX duplicated for .ned in +urb-core
          urb-tx
          urb-tx
      ==
  ==
::
++  mock-precommits
  |=  sed=pass
  ^-  (map [txid:ord vout:ord] [commit=urb-tx:urb precommit=urb-tx:urb])
  =/  tx  (mk-urb-tx mock-skim-fief sed)
  %-  my
  ~[[[(shax sed) 0] [tx tx]]]
::
++  comet-for
  |=  sed=pass
  ^-  ship
  `@p`fig:ex:(cut sed)
::
++  dead-comet  (comet-for 0xdead.beef)
++  cafe-comet  (comet-for 0xcafe.babe)
++  feed-comet  (comet-for 0xfeed.face)
--
::
|%
++  test-abed
  =/  oc  urb-core:ul
  =.  oc  oc(state init-state)
  %+  expect-eq
    !>  oc
    !>  (abed:oc init-state)
::
++  test-emit
  =/  oc  urb-core:ul
  =.  oc  oc(block-id.state bunt-id)
  %+  expect-eq
    !>  oc(fx :-([bunt-id bunt-effect] ~))
    !>  (emit:oc bunt-effect)
::
++  test-emil
  =/  oc  urb-core:ul
  %+  expect-eq
    !>  (emit:oc bunt-effect)
    !>  (emil:oc [bunt-effect]~)
::
++  test-abet
  =/  oc  urb-core:ul
  =.  oc  (abed:oc init-state)
  =.  oc  (emit:oc bunt-effect)
  %+  expect-eq
    !>  [(flop fx:oc) state:oc]
    !>  abet:oc
::
++  test-handle-block-state
  =/  oc     urb-core:ul
  =.  oc     (abed:oc init-state)
  =.  oc
    %+  handle-block:oc
      (mk-urb-block (mk-skim-spawn 0xdead.beef))
    (mock-precommits 0xdead.beef)
  =/  ex-oc  urb-core:ul
  =.  ex-oc  (abed:ex-oc init-state)
  =.  ex-oc  ex-oc(num.block-id.state +(start-height))
  =.  ex-oc
    %+  handle-tx:ex-oc
      (mk-urb-tx (mk-skim-spawn 0xdead.beef) 0xdead.beef)
    (mock-precommits 0xdead.beef)
  %+  expect-eq
    !>  state.ex-oc
    !>  state.oc
::
++  test-handle-tx-spawn
  =/  oc  urb-core:ul
  =.  oc  (abed:oc init-state)
  =.  oc
    %+  handle-tx:oc
      (mk-urb-tx (mk-skim-spawn 0xdead.beef) 0xdead.beef)
    (mock-precommits 0xdead.beef)
  =/  ex-spawn-fx
    ^-  (list [id:block:bitcoin effect:urb])
    :~  :-  bunt-id
        [%xfer [(shax 0xdead.beef) 0 0] [(shax 0xdead.beef) 0 0]]
        :-  bunt-id
        [%point dead-comet %fief ~]
        :-  bunt-id
        [%point dead-comet %keys 1 pub:ex:(cut 0xdead.beef)]
        :-  bunt-id
        [%point dead-comet %sponsor `dead-comet]
        :-  bunt-id
        [%point dead-comet %owner [(shax 0xdead.beef) 0 0]]
    ==
  %+  expect-eq
    !>  ex-spawn-fx
    !>  fx.oc
::
++  test-handle-tx-adopt
  =/  oc  urb-core:ul
  =.  oc  (abed:oc init-state)
  =.  oc
    %+  handle-tx:oc
      (mk-urb-tx (mk-skim-spawn 0xdead.beef) 0xdead.beef)
    (mock-precommits 0xdead.beef)
  =.  oc
    %+  handle-tx:oc
      (mk-urb-tx (mk-skim-spawn 0xcafe.babe) 0xcafe.babe)
    (mock-precommits 0xcafe.babe)
  =.  oc
    (handle-tx:oc (mk-urb-tx (mk-skim-escape 0xcafe.babe) 0xdead.beef) ~)
  =.  fx.oc  ~
  =.  oc
    (handle-tx:oc (mk-urb-tx (mk-skim-adopt 0xdead.beef) 0xcafe.babe) ~)
  =/  ex-adopt-fx
    ^-  (list [id:block:bitcoin effect:urb])
    :~  :-  bunt-id
        [%xfer [(shax 0xcafe.babe) 0 0] [(shax 0xcafe.babe) 0 0]]
        :-  bunt-id
        [%point dead-comet %sponsor [~ cafe-comet]]
    ==
  %+  expect-eq
    !>  ex-adopt-fx
    !>  fx.oc
::
++  test-handle-tx-escape
  =/  oc  urb-core:ul
  =.  oc  (abed:oc init-state)
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-spawn 0xdead.beef) 0xdead.beef) (mock-precommits 0xdead.beef))
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-spawn 0xcafe.babe) 0xcafe.babe) (mock-precommits 0xcafe.babe))
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-spawn 0xfeed.face) 0xfeed.face) (mock-precommits 0xfeed.face))
  =.  fx.oc  ~
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-escape 0xfeed.face) 0xcafe.babe) ~)
  =/  ex-escape-fx
    ^-  (list [id:block:bitcoin effect:urb])
    :~  :-  bunt-id
        [%xfer [(shax 0xcafe.babe) 0 0] [(shax 0xcafe.babe) 0 0]]
        :-  bunt-id
        [%point cafe-comet %escape [~ feed-comet]]
    ==
  %+  expect-eq
    !>  ex-escape-fx
    !>  fx.oc
::
++  test-handle-tx-cancel-escape
  =/  oc  urb-core:ul
  =.  oc  (abed:oc init-state)
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-spawn 0xdead.beef) 0xdead.beef) (mock-precommits 0xdead.beef))
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-spawn 0xcafe.babe) 0xcafe.babe) (mock-precommits 0xcafe.babe))
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-spawn 0xfeed.face) 0xfeed.face) (mock-precommits 0xfeed.face))
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-escape 0xfeed.face) 0xcafe.babe) ~)
  =.  fx.oc  ~
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-cancel-escape 0xfeed.face) 0xcafe.babe) ~)
  =/  ex-cancel-escape-fx
    ^-  (list [id:block:bitcoin effect:urb])
    :~  :-  bunt-id
        [%xfer [(shax 0xcafe.babe) 0 0] [(shax 0xcafe.babe) 0 0]]
        :-  bunt-id
        [%point cafe-comet %escape ~]
    ==
  %+  expect-eq
    !>  ex-cancel-escape-fx
    !>  fx.oc
::
++  test-handle-tx-reject
  =/  oc  urb-core:ul
  =.  oc  (abed:oc init-state)
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-spawn 0xdead.beef) 0xdead.beef) (mock-precommits 0xdead.beef))
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-spawn 0xcafe.babe) 0xcafe.babe) (mock-precommits 0xcafe.babe))
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-spawn 0xfeed.face) 0xfeed.face) (mock-precommits 0xfeed.face))
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-escape 0xfeed.face) 0xcafe.babe) ~)
  =.  fx.oc  ~
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-reject 0xcafe.babe) 0xfeed.face) ~)
  =/  ex-reject-fx
    ^-  (list [id:block:bitcoin effect:urb])
    :~  :-  bunt-id
        [%xfer [(shax 0xfeed.face) 0 0] [(shax 0xfeed.face) 0 0]]
        :-  bunt-id
        [%point cafe-comet %escape ~]
    ==
  %+  expect-eq
    !>  ex-reject-fx
    !>  fx.oc
::
++  test-handle-tx-detach
  =/  oc  urb-core:ul
  =.  oc  (abed:oc init-state)
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-spawn 0xdead.beef) 0xdead.beef) (mock-precommits 0xdead.beef))
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-spawn 0xcafe.babe) 0xcafe.babe) (mock-precommits 0xcafe.babe))
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-escape 0xdead.beef) 0xcafe.babe) ~)
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-adopt 0xcafe.babe) 0xdead.beef) ~)
  =.  fx.oc  ~
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-detach 0xcafe.babe) 0xdead.beef) ~)
  =/  ex-detach-fx
    ^-  (list [id:block:bitcoin effect:urb])
    :~  :-  bunt-id
        [%xfer [(shax 0xdead.beef) 0 0] [(shax 0xdead.beef) 0 0]]
        :-  bunt-id
        [%point cafe-comet %sponsor `cafe-comet]
    ==
  %+  expect-eq
    !>  ex-detach-fx
    !>  fx.oc
::
++  test-handle-tx-fief
  =/  oc  urb-core:ul
  =.  oc  (abed:oc init-state)
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-spawn 0xdead.beef) 0xdead.beef) (mock-precommits 0xdead.beef))
  =.  fx.oc  ~
  =.  oc  (handle-tx:oc (mk-urb-tx mock-skim-fief 0xdead.beef) ~)
  =/  ex-fief-fx
    ^-  (list [id:block:bitcoin effect:urb])
    :~  :-  bunt-id
        [%xfer [(shax 0xdead.beef) 0 0] [(shax 0xdead.beef) 0 0]]
        :-  bunt-id
        [%point dead-comet %fief `[%if p=.127.0.0.1 q=8.080]]
    ==
  %+  expect-eq
    !>  ex-fief-fx
    !>  fx.oc
::
++  test-handle-tx-batch
  =/  oc  urb-core:ul
  =.  oc  (abed:oc init-state)
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-spawn 0xdead.beef) 0xdead.beef) (mock-precommits 0xdead.beef))
  =.  fx.oc  ~
  =.  oc  (handle-tx:oc (mk-urb-tx mock-skim-batch 0xdead.beef) ~)
  =/  ex-batch-fx
    ^-  (list [id:block:bitcoin effect:urb])
    :~  :-  bunt-id
        [%xfer [(shax 0xdead.beef) 0 0] [(shax 0xdead.beef) 0 0]]
        :-  bunt-id
        [%point dead-comet %fief ~]
        :-  bunt-id
        [%point dead-comet %fief `[%if p=.192.168.1.1 q=80]]
        :-  bunt-id
        [%point dead-comet %fief `[%if p=.127.0.0.1 q=8.080]]
    ==
  %+  expect-eq
    !>  ex-batch-fx
    !>  fx.oc
--
