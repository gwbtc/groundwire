::
::  ++  find-block-deps
::    ++  add-to-deps
::  ++  apply-block-deps
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
/+  *test, ul=urb
=>
|%
++  mock-id
  [start-hash:urb start-height:urb]
::
++  mock-block
  :*  start-hash:urb          ::  hash
      *@ud                    ::  reward
      start-height:urb        ::  height
      ::  XX needs to be non-empty
      txs=*(list tx:bitcoin)  ::  transactions
      ::  what is a tx:bitcoin?
      ::  [id=txid dataw]
      ::  txid:bitcoin = @ux
      ::  dataw:tx:bitcoin
      ::    is=(list inputw)
      ::      =txid
      ::      pos=@ud
      ::      sequence=hexb
      ::        [wid=@ dat=@ub]
      ::      script-sig=(unit hexb)
      ::      pubkey=(unit hexb)
      ::    os=(list output)
      ::      script-pubkey=hexb
      ::      value=sats
      ::        @ud
      ::    locktime=@ud
      ::    nversion=@ud
      ::    segwit=(unit @ud)
  ==
::
++  mock-deps
  *(map [txid:ord pos:urb] [sots=(list raw-sotx:urb) value=(unit @ud)])
::
++  mock-effect
  *effect:ord
::
++  mock-state
  :*  mock-id        ::  last indexed block
      *sont-map:ord  ::  satpoints
      *insc-ids:ord  ::  transactions with inscriptions
      *unv-ids:ord   ::  transactions with unvelopes
  ==
::
++  mock-fx
  [[mock-id mock-effect]]~
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
::++  test-find-block-deps
::  =/  oc  ord-core:ul
::  %+  expect-eq
::    !>  [mock-deps mock-block]
::    !>  (find-block-deps:oc [start-height:urb mock-block])
--
