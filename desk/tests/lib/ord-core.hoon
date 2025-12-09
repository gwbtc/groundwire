/-  ord, urb, bitcoin
/+  *test, ul=urb, lais, scr=btc-script
=>
|%
++  bunt-id
  ^-  id:block:bitcoin
  [start-hash:urb start-height:urb]
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
  :*  start-hash:urb    ::  hash
      50.000.000        ::  reward
      start-height:urb  ::  height
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
      %c   (rap 3 ~[lyf %btc %ord %gw %test])
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
  =/  ent  (skim:encode:lais sot)
  =/  sig  (sign-octs-raw:ed:crypto [512 (shaz ent)] [sgn.pub sgn.sek]:+<:(cut sed))
  [[`@p`fig:ex:(cut sed) [~ sig]] sot]
::
++  mk-raw-sot
  |=  [sot=skim-sotx:urb sed=pass]
  ^-  octs
  =/  ent  (skim:encode:lais sot)
  [(met 3 ent) ent]
::
++  mk-skim-spawn
  |=  sed=pass
  ^-  skim-sotx:urb
  [%spawn pub:ex:(cut sed) [spkh=(mock-output-hash sed) pos=~ off=0 tej=0]]
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
  [%escape `@p`fig:ex:(cut sed)]
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
      height=start-height:urb
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
  *(map [txid:ord pos:urb] [sots=(list raw-sotx:urb) value=(unit @ud)])
::
++  mock-deps
  |=  sed=pass
  ^+  bunt-deps
  %-  my
  :~  :-  ^-  [txid:ord pos:urb]
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
  :~  :-  ^-  [txid:ord pos:urb]
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
  *effect:ord
::
++  init-state
  ^-  state:ord
  :*  bunt-id        ::  last indexed block
      *sont-map:ord  ::  known satpoints
      *insc-ids:ord  ::  transactions with inscriptions
      *unv-ids:ord   ::  transactions with unvelopes
  ==
::
++  bunt-fx
  ^-  (list [id:block:bitcoin effect:ord])
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
  :*  hax=start-hash:urb
      reward=0
      height=start-height:urb
      ^=  txs
      ^-  (list urb-tx:urb)
      :~  mock-urb-coinbase-tx
          ::  XX duplicated for .ned in +ord-core
          urb-tx
          urb-tx
      ==
  ==
::
++  dead-comet
  ~radlyx-lomsev-sorseb-batnyr--nommes-bolseg-hacbyl-todhet
::
++  cafe-comet
  ~sabduc-lonful-mapfyl-bansyt--hinsec-hacnev-racful-fopbyl
::
++  feed-comet
  ~batmeb-hocmus-racper-sicwyx--tilnus-hoblup-nimmep-paster
--
::
|%
++  test-abed
  =/  oc  ord-core:ul
  =.  oc  oc(state init-state)
  %+  expect-eq
    !>  oc
    !>  (abed:oc init-state)
::
++  test-emit
  =/  oc  ord-core:ul
  =.  oc  oc(block-id.state bunt-id)
  %+  expect-eq
    !>  oc(fx :-([bunt-id bunt-effect] ~))
    !>  (emit:oc bunt-effect)
::
++  test-emil
  =/  oc  ord-core:ul
  %+  expect-eq
    !>  (emit:oc bunt-effect)
    !>  (emil:oc [bunt-effect]~)
::
++  test-abet
  =/  oc  ord-core:ul
  =.  oc  (abed:oc init-state)
  =.  oc  (emit:oc bunt-effect)
  %+  expect-eq
    !>  [(flop fx:oc) state:oc]
    !>  abet:oc
::
++  test-find-block-deps-no-deps
  =/  oc  ord-core:ul
  %+  expect-eq
    !>  [bunt-deps mock-block]
    !>  (find-block-deps:oc [start-height:urb mock-block])
::
++  test-find-block-deps-with-deps
  =/  oc  ord-core:ul
  =/  input-block
    :*  hax=0x0
        reward=0
        height=start-height:urb
        ^=  txs
        :~  ::  first tx must be coinbase
            mock-coinbase-tx
            ::  second tx with unvelope
            (mk-tx-with-urb-witness (mk-skim-spawn 0xdead.beef))
        ==
    ==
  =/  ex-block
    :*  hax=0x0
        reward=0
        height=start-height:urb
        ::  only coinbase remains after urb tx extraction
        txs=[mock-coinbase-tx]~
    ==
  %+  expect-eq
    !>  [bunt-deps ex-block]
    !>  (find-block-deps:oc [start-height:urb input-block])
::
++  test-find-block-deps-with-deps-no-value
  =/  oc  ord-core:ul
  =/  input-block
    :*  hax=0x0
        reward=0
        height=start-height:urb
        ^=  txs
        :~  mock-coinbase-tx
            (mk-tx-with-urb-witness (mk-skim-spawn 0xdead.beef))
        ==
    ==
  =/  ex-block
    :*  hax=0x0
        reward=0
        height=start-height:urb
        txs=[mock-coinbase-tx]~
    ==
  %+  expect-eq
    !>  [bunt-deps ex-block]
    !>  (find-block-deps:oc [start-height:urb input-block])
::
++  test-apply-block-deps-with-value
  =/  oc  ord-core:ul
  %+  expect-eq
    !>  [num=start-height:urb (mk-urb-block (mk-skim-spawn 0xdead.beef))]
    !>
    %+  apply-block-deps:oc
      :-  start-height:urb
      (mk-block-with-urb-deps-output (mk-skim-spawn 0xdead.beef))
    (mock-deps 0xdead.beef)
::
++  test-apply-block-deps-no-value-fails
  ::
  ::  +apply-block-deps fails with ~ rather than [~ value]
  =/  oc  ord-core:ul
  %-  expect-fail
    |.
    %+  apply-block-deps:oc
      :-  start-height:urb
      (mk-block-with-urb-deps-output (mk-skim-spawn 0xdead.beef))
    (mock-deps-no-value 0xdead.beef)
::
++  test-handle-block-state
  =/  oc     ord-core:ul
  =.  oc     (abed:oc init-state)
  =.  oc     %+  handle-block:oc
               start-height:urb
             (mk-urb-block (mk-skim-spawn 0xdead.beef))
  =/  ex-oc  ord-core:ul
  =.  ex-oc  (abed:ex-oc init-state)
  =.  ex-oc  ex-oc(num.block-id.state +(start-height:urb))
  =.  ex-oc  %-  handle-tx:ex-oc
             (mk-urb-tx (mk-skim-spawn 0xdead.beef) 0xdead.beef)
  %+  expect-eq
    !>  state.ex-oc
    !>  state.oc
::
++  test-handle-tx-spawn
  =/  oc  ord-core:ul
  =.  oc  (abed:oc init-state)
  =.  oc  %-  handle-tx:oc
          (mk-urb-tx (mk-skim-spawn 0xdead.beef) 0xdead.beef)
  =/  ex-spawn-fx
    ^-  (list [id:block:bitcoin effect:ord])
    :~  :-  bunt-id
        [%xfer [(shax 0xdead.beef) 0 0] [(shax 0xdead.beef) 0 0]]
        :-  bunt-id
        [%point dead-comet %keys 1 518.847.371.617.947.361.520.838.967.418.184.237.645.890.125.796.308.409.702.941.729.937.039.949.187.475.686.862.653.933.909.072.579.455.217.271.195.095.941.577.817.884.171.538.188.583.408.722.515.276.586.322.052.198.175.980.680.778.630.683.042.191.715.939]
        :-  bunt-id
        [%point dead-comet %sponsor [~ ~todhet]]
        :-  bunt-id
        [%point dead-comet %owner [(shax 0xdead.beef) 0 0]]
    ==
  %+  expect-eq
    !>  ex-spawn-fx
    !>  fx.oc
::
++  test-handle-tx-adopt
  =/  oc  ord-core:ul
  =.  oc  (abed:oc init-state)
  =.  oc  %-  handle-tx:oc
          (mk-urb-tx (mk-skim-spawn 0xdead.beef) 0xdead.beef)
  =.  oc  %-  handle-tx:oc
          (mk-urb-tx (mk-skim-spawn 0xcafe.babe) 0xcafe.babe)
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-escape 0xcafe.babe) 0xdead.beef))
  =.  fx.oc  ~
  =.  oc  %-  handle-tx:oc
          (mk-urb-tx (mk-skim-adopt 0xdead.beef) 0xcafe.babe)
  =/  ex-adopt-fx
    ^-  (list [id:block:bitcoin effect:ord])
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
  =/  oc  ord-core:ul
  =.  oc  (abed:oc init-state)
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-spawn 0xdead.beef) 0xdead.beef))
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-spawn 0xcafe.babe) 0xcafe.babe))
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-spawn 0xfeed.face) 0xfeed.face))
  =.  fx.oc  ~
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-escape 0xfeed.face) 0xcafe.babe))
  =/  ex-escape-fx
    ^-  (list [id:block:bitcoin effect:ord])
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
  =/  oc  ord-core:ul
  =.  oc  (abed:oc init-state)
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-spawn 0xdead.beef) 0xdead.beef))
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-spawn 0xcafe.babe) 0xcafe.babe))
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-spawn 0xfeed.face) 0xfeed.face))
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-escape 0xfeed.face) 0xcafe.babe))
  =.  fx.oc  ~
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-cancel-escape 0xfeed.face) 0xcafe.babe))
  =/  ex-cancel-escape-fx
    ^-  (list [id:block:bitcoin effect:ord])
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
  =/  oc  ord-core:ul
  =.  oc  (abed:oc init-state)
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-spawn 0xdead.beef) 0xdead.beef))
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-spawn 0xcafe.babe) 0xcafe.babe))
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-spawn 0xfeed.face) 0xfeed.face))
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-escape 0xfeed.face) 0xcafe.babe))
  =.  fx.oc  ~
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-reject 0xcafe.babe) 0xfeed.face))
  =/  ex-reject-fx
    ^-  (list [id:block:bitcoin effect:ord])
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
  =/  oc  ord-core:ul
  =.  oc  (abed:oc init-state)
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-spawn 0xdead.beef) 0xdead.beef))
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-spawn 0xcafe.babe) 0xcafe.babe))
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-escape 0xdead.beef) 0xcafe.babe))
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-adopt 0xcafe.babe) 0xdead.beef))
  =.  fx.oc  ~
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-detach 0xcafe.babe) 0xdead.beef))
  =/  ex-detach-fx
    ^-  (list [id:block:bitcoin effect:ord])
    :~  :-  bunt-id
        [%xfer [(shax 0xdead.beef) 0 0] [(shax 0xdead.beef) 0 0]]
        :-  bunt-id
        [%point cafe-comet %sponsor ~]
    ==
  %+  expect-eq
    !>  ex-detach-fx
    !>  fx.oc
::
++  test-handle-tx-fief
  =/  oc  ord-core:ul
  =.  oc  (abed:oc init-state)
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-spawn 0xdead.beef) 0xdead.beef))
  =.  fx.oc  ~
  =.  oc  (handle-tx:oc (mk-urb-tx mock-skim-fief 0xdead.beef))
  =/  ex-fief-fx
    ^-  (list [id:block:bitcoin effect:ord])
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
  =/  oc  ord-core:ul
  =.  oc  (abed:oc init-state)
  =.  oc  (handle-tx:oc (mk-urb-tx (mk-skim-spawn 0xdead.beef) 0xdead.beef))
  =.  fx.oc  ~
  =.  oc  (handle-tx:oc (mk-urb-tx mock-skim-batch 0xdead.beef))
  =/  ex-batch-fx
    ^-  (list [id:block:bitcoin effect:ord])
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
