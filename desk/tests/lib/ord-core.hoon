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
/+  *test, ul=urb, lais, scr=btc-script, crac
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
      *@ud              ::  reward
      start-height:urb  ::  height
      txs=[mock-tx]~    ::  transactions
  ==
::
::  generate mock keypair
++  cut
  =/  sed  0xdead.beef  ::  test seed
  =/  lyf  1            ::  $life
  =/  xtr  0            ::  extra data
  =<  ?>(&(?=(%c suite.+<) ?=(^ sek.+<)) .)
  %:  pit:nu:crac
      512  (shaz (jam sed lyf))
      %c   (rap 3 ~[lyf %btc %ord %gw %test])
      xtr
  ==
::
++  mock-output
  ^-  output:tx:bitcoin
  :*  script-pubkey=[wid=34 dat=0x5120.1234.5678.9abc.def0]  :: P2TR output
      value=50.000.000
  ==
::
++  mock-output-hash
  ^-  @ux
  =/  out  mock-output
  =/  en-out  (can 3 script-pubkey.out 8^value.out ~)
  (shay (add 8 wid.script-pubkey.out) en-out)
::
++  mock-sotx-spawn
  ^-  sotx:urb
  =/  sot  ^-  skim-sotx:urb
    [%spawn pub:ex:cut [spkh=mock-output-hash pos=~ off=0 tej=0]]
  =/  ent  (skim:encode:lais sot)
  =/  sig  (sign-octs-raw:ed:crypto 512^(shaz ent) [sgn.pub sgn.sek]:+<:cut)
  [[`@p`fig:ex:cut [~ sig]] sot]
::
++  mock-raw-sotx-spawn
  ^-  octs
  =/  sot  ^-  skim-sotx:urb
    [%spawn pub:ex:cut [spkh=mock-output-hash pos=~ off=0 tej=0]]
  =/  ent  (skim:encode:lais sot)
  [(met 3 ent) ent]
::
++  mock-tx-with-urb-witness
  ^-  tx:bitcoin
  :*  id=0xdef0.1234.5678.9abc.def0.1234.5678.9abc.def0.1234.5678.9abc.def0.1234.5678
      ^-  dataw:tx:bitcoin
      :*  ^=  is
          ^-  (list inputw:tx:bitcoin)
          :~  :*  ^=  witness
                  :~  mock-raw-sotx-spawn  :: encoded unvelope
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
                  value=50.000.000
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
++  mock-deps
  ^+  bunt-deps
  %-  my
  :~  :-  ^-  [txid:ord pos:urb]
          [0x2345.6789.abcd.ef01.2345.6789.abcd.ef01.2345.6789.abcd.ef01.2345.6789 0]
      :-  ^=  sots
          ^-  (list raw-sotx:urb)
          :~  :-  raw=mock-raw-sotx-spawn
              sot=mock-sotx-spawn
          ==
      value=(some 50.000.000)
  ==
::
++  mock-deps-no-value
  ^+  bunt-deps
  %-  my
  :~  :-  ^-  [txid:ord pos:urb]
          [0x2345.6789.abcd.ef01.2345.6789.abcd.ef01.2345.6789.abcd.ef01.2345.6789 0]
      :-  ^=  sots
          ^-  (list raw-sotx:urb)
          :~  :-  raw=mock-raw-sotx-spawn
              sot=mock-sotx-spawn
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
              0   ::  value 0 - coinbase has no input value
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
++  mock-urb-tx-with-urb-witness
  ^-  tx:urb-tx:urb
  :*  id=0xdef0.1234.5678.9abc.def0.1234.5678.9abc.def0.1234.5678.9abc.def0.1234.5678
      ^-  data:urb-tx:urb
      :*  ^=  is
          ^-  (list input:urb-tx:urb)
          :~  :-  :-  :~  :-  raw=mock-raw-sotx-spawn
                          sot=mock-sotx-spawn
                      ==
                  50.000.000
              :-  ^=  witness
                      :~  mock-raw-sotx-spawn  :: encoded unvelope
                      [wid=0 dat=0x0]     :: OP_0 (for P2TR structure)
                  ==
              :*  id=0x2345.6789.abcd.ef01.2345.6789.abcd.ef01.2345.6789.abcd.ef01.2345.6789
                  pos=0
                  sequence=[wid=4 dat=0xffff.ffff]
                  script-sig=~  ::  empty for P2TR
                  pubkey=~      ::  empty for P2TR
              ==
          ==
          ^=  os
          ^-  (list output:tx:bitcoin)
          :~  mock-output  :: First output matches the spawn spkh
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
++  mock-urb-block
  ^-  urb-block:urb
  :*  hax=start-hash:urb
      reward=0
      height=start-height:urb
      ^=  txs
      ^-  (list urb-tx:urb)
      :~  mock-urb-coinbase-tx
          mock-urb-tx-with-urb-witness
          mock-urb-tx-with-urb-witness
      ==
  ==
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
::
::++  test-find-block-deps-with-deps
::  =/  oc  ord-core:ul
::  =/  input-block
::    :*  hax=0x0
::        reward=0
::        height=start-height:urb
::        ^=  txs
::        :~  mock-coinbase-tx          ::  first tx must be coinbase
::            mock-tx-with-urb-witness  ::  second tx with unvelope
::        ==
::    ==
::  %+  expect-eq
::    !>  [mock-deps mock-block-with-urb-deps-output]
::    !>  (find-block-deps:oc [start-height:urb input-block])
::
::++  test-find-block-deps-with-deps-no-value
::  =/  oc  ord-core:ul
::  =/  input-block
::    :*  hax=0x0
::        reward=0
::        height=start-height:urb
::        ^=  txs
::        :~  mock-coinbase-tx          ::  first tx must be coinbase
::            mock-tx-with-urb-witness  ::  second tx with unvelope
::        ==
::    ==
::  %+  expect-eq
::    !>  [mock-deps-no-value mock-block-with-urb-deps-output]
::    !>  (find-block-deps:oc [start-height:urb input-block])
::
++  test-apply-block-deps
  =/  oc  ord-core:ul
  %+  expect-eq
    !>  [num=start-height:urb mock-urb-block]
    !>
    %+  apply-block-deps:oc
      :-  start-height:urb
      mock-block-with-urb-deps-output
    mock-deps
::
++  test-apply-block-deps-no-value-fails
  =/  oc  ord-core:ul
  %-  expect-fail
    |.
    %+  apply-block-deps:oc
      :-  start-height:urb
      mock-block-with-urb-deps-output
    mock-deps-no-value
::
++  test-handle-block-state
  =/  oc  ord-core:ul
  =.  oc  (abed:oc init-state)
  =.  oc  (handle-block:oc start-height:urb mock-urb-block)
  ::  Create expected state by running the same operations but starting from expected initial state
  =/  expected-oc  ord-core:ul
  =.  expected-oc  (abed:expected-oc init-state)
  =.  expected-oc  expected-oc(num.block-id.state +(start-height:urb))
  ::  Process the transactions to build expected state
  =.  expected-oc  (handle-tx:expected-oc mock-urb-tx-with-urb-witness)
  =.  expected-oc  (handle-tx:expected-oc mock-urb-tx-with-urb-witness)
  %+  expect-eq
    !>  state.expected-oc
    !>  state.oc
::
++  test-handle-tx
  =/  oc  ord-core:ul
  =.  oc  (abed:oc init-state)
  =.  oc  (handle-tx:oc mock-urb-tx-with-urb-witness)
  =/  expected-effects
    ^-  (list [id:block:bitcoin effect:ord])
    :~  :-  bunt-id
        [%xfer [0x2345.6789.abcd.ef01.2345.6789.abcd.ef01.2345.6789.abcd.ef01.2345.6789 0 0] [0xdef0.1234.5678.9abc.def0.1234.5678.9abc.def0.1234.5678.9abc.def0.1234.5678 0 0]]
        :-  bunt-id
        [%point ~radlyx-lomsev-sorseb-batnyr--nommes-bolseg-hacbyl-todhet %keys 1 518.847.371.617.947.361.520.838.967.418.184.237.645.890.125.796.308.409.702.941.729.937.039.949.187.475.686.862.653.933.909.072.579.455.217.271.195.095.941.577.817.884.171.538.188.583.408.722.515.276.586.322.052.198.175.980.680.778.630.683.042.191.715.939]
        :-  bunt-id
        [%point ~radlyx-lomsev-sorseb-batnyr--nommes-bolseg-hacbyl-todhet %sponsor [~ ~todhet]]
        :-  bunt-id
        [%point ~radlyx-lomsev-sorseb-batnyr--nommes-bolseg-hacbyl-todhet %owner [0x2345.6789.abcd.ef01.2345.6789.abcd.ef01.2345.6789.abcd.ef01.2345.6789 0 0]]
    ==
  %+  expect-eq
    !>  expected-effects
    !>  fx.oc
--
