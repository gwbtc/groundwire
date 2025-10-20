::
::++  handle-block
::++  handle-tx
::  ++  process-unv
::    ++  spend-point
::    ++  spending-sont
::    ++  get-spawn-sont
::  ++  check-for-insc
::  ++  pntr-to-sont
::  ++  inscription-to-sont
::  ++  off-to-sont
::  ++  sont-track-input
::  ++  next-input
::
/-  ord, urb, bitcoin
/+  *test, ul=urb, lais, scr=btc-script
=>
|%
++  mock-id
  ^-  id:block:bitcoin
  [start-hash:urb start-height:urb]
::
++  mock-tx
  ^-  tx:bitcoin
  :*  id=0xabc1.2345.6789.def0.1234.5678.9abc.def0.1234.5678.9abc.def0.1234.5678
      ::  ^-  dataw:tx:bitcoin
      ^=  is
      ::  ^-  (list inputw:tx:bitcoin)
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
              value=5.000
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
      *@ud              ::  reward
      start-height:urb  ::  height
      txs=[mock-tx]~    ::  transactions
  ==
::
++  mock-sotx-spawn
  ^-  sotx:urb
  ::  XX no signaure; we don't check
  :-  [~sampel-palnet ~]
  ^-  skim-sotx:urb
  [%spawn 0xdead.beef [0xbeef ~ 0 0]]
::
++  mock-urb-en-script
  ^-  octs
  =/  sots=(list sotx:urb)  [mock-sotx-spawn]~
  =/  en-sots=@  (encode:lais sots)
  =/  script=script:ord  (unv-to-script:en:ul en-sots)
  (en:scr script)
::
++  mock-tx-with-urb-witness
  ^-  tx:bitcoin
  :*  id=0xdef0.1234.5678.9abc.def0.1234.5678.9abc.def0.1234.5678.9abc.def0.1234.5678
      ^-  dataw:tx:bitcoin
      :*  ^=  is
          ^-  (list inputw:tx:bitcoin)
          :~  :*  ^=  witness
                  :~  mock-urb-en-script  :: encoded unvelope
                      [wid=0 dat=0x0]     :: OP_0 (for P2TR structure)
                  ==
                  id=0x2345.6789.abcd.ef01.2345.6789.abcd.ef01.2345.6789.abcd.ef01.2345.6789
                  pos=0
                  sequence=[wid=4 dat=0xffff.ffff]
                  script-sig=~  ::  empty for P2TR
                  pubkey=~      ::  empty for P2TR
              ==
          ==
          ^=  os
          ^-  (list output:tx:bitcoin)
          :~  :*  script-pubkey=[wid=34 dat=0x5120.1234.5678.9abc.def0]  :: P2TR output
                  value=5.000
              ==
              :*  script-pubkey=[wid=34 dat=0x5120.9876.5432.1fed.cba0]  :: P2TR output
                  value=50.000.000
              ==
          ==
          locktime=0
          nversion=2  ::  version 2 for taproot
          segwit=`1   ::  segwit version 1 for taproot
      ==
  ==
::
++  mock-coinbase-tx
  ^-  tx:bitcoin
  :*  id=0x1111.2222.3333.4444.5555.6666.7777.8888.9999.aaaa.bbbb.cccc.dddd.eeee.ffff
      ^-  dataw:tx:bitcoin
      :*  ^=  is
          ^-  (list inputw:tx:bitcoin)
          :~  :*  witness=~  ::  coinbase usually has empty witness
                  id=0x0     ::  coinbase input references null hash
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
++  mock-block-with-urb-deps-output
  ^-  block:bitcoin
  :*  hax=0x0
      reward=0
      height=start-height:urb
      ^=  txs
      ^-  (list tx:bitcoin)
      :~  mock-coinbase-tx              ::  coinbase tx comes first
          mock-tx-with-urb-witness      ::  transaction with urbit data
          ::  XX duplicated because ned=&
          ::       what is ned?
          mock-tx-with-urb-witness
      ==
  ==
::
++  bunt-deps
  *(map [txid:ord pos:urb] [sots=(list raw-sotx:urb) value=(unit @ud)])
::
++  mock-deps-2
  ^+  bunt-deps
  %-  my
  :~  :-  ^-  [txid:ord pos:urb]
          [0x2345.6789.abcd.ef01.2345.6789.abcd.ef01.2345.6789.abcd.ef01.2345.6789 0]
      :-  ^=  sots
          ^-  (list raw-sotx:urb)
          :~  :-  raw=[p=39 q=1.564.443.629.824.885.314.254.166.288.698.402.427.687.527.726.335.439.040.488.555.865.104.164.838.438.377.953.940.116.357.121]
              sot=mock-sotx-spawn
          ==
      value=~
  ==
::
++  mock-effect
  *effect:ord
::
++  mock-state
  ^-  state:ord
  :*  mock-id        ::  last indexed block
      *sont-map:ord  ::  known satpoints
      *insc-ids:ord  ::  transactions with inscriptions
      *unv-ids:ord   ::  transactions with unvelopes
  ==
::
++  mock-fx
  ^-  (list [id:block:bitcoin effect:ord])
  [[mock-id mock-effect]]~
::
++  mock-urb-coinbase-tx
  ::  ^-  tx:urb-tx:urb
  :*  id=0x1111.2222.3333.4444.5555.6666.7777.8888.9999.aaaa.bbbb.cccc.dddd.eeee.ffff
      ::  ^-  data:urb-tx:urb
      :*  ^=  is
          ::  ^-  input:urb-tx:urb
          :-  :_  50.000.000
              ::  XX fill in
              *(list raw-sotx:urb)
          ^-  inputw:tx:bitcoin
          :-  *witness:tx:bitcoin
          *input:tx:bitcoin
::          :~  :*  witness=~  ::  coinbase usually has empty witness
::                  id=0x0     ::  coinbase input references null hash
::                  pos=4.294.967.295
::                  sequence=[wid=4 dat=0xffff.ffff]
::                  script-sig=~
::                  pubkey=~
::              ==
::          ==
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
::++  mock-urb-tx-with-urb-witness
::  !!
::::
::++  mock-urb-block
::  :*  hax=start-hash:urb
::      reward=0
::      height=start-height:urb
::      ^=  txs
::      :~  mock-coinbase-urb-tx
::          mock-urb-tx-with-urb-witness
::          mock-urb-tx-with-urb-witness
::      ==
::  ==
--
::
|%
++  test-abed
  =/  oc  ord-core:ul
  =.  oc  oc(state mock-state)
  %+  expect-eq
    !>  oc
    !>  (abed:oc mock-state)
::
++  test-emit
  =/  oc  ord-core:ul
  =.  oc  oc(block-id.state mock-id)
  %+  expect-eq
    !>  oc(fx :-([mock-id mock-effect] ~))
    !>  (emit:oc mock-effect)
::
++  test-emil
  =/  oc  ord-core:ul
  %+  expect-eq
    !>  (emit:oc mock-effect)
    !>  (emil:oc [mock-effect]~)
::
++  test-abet
  =/  oc  ord-core:ul
  =.  oc  (abed:oc mock-state)
  =.  oc  (emit:oc mock-effect)
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
        :~  mock-coinbase-tx          ::  first tx must be coinbase
            mock-tx-with-urb-witness  ::  second tx with unvelope
        ==
    ==
  %+  expect-eq
    !>  [mock-deps-2 mock-block-with-urb-deps-output]
    !>  (find-block-deps:oc [start-height:urb input-block])
::
::++  test-apply-block-deps
::  =/  oc  ord-core:ul
::  %+  expect-eq
::    !>  [start-height:urb mock-urb-block]
::    !>  (apply-block-deps:oc mock-block mock-deps-2)
--
