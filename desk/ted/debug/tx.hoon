/-  spider, urb, bitcoin
/+  *ord, ul=urb, strandio, btcio, bc=bitcoin
/=  test-ord-core  /tests/lib/ord-core
^-  thread:spider
|=  args=vase
=/  m  (strand:strandio ,vase)
^-  form:m
=/  [=req-to:btcio =txid:ord]
  (need !<((unit [req-to:btcio @ux]) args))
=/  oc  ord-core:ul
|^
;<  raw-tx-result=(unit tx:bc)  bind:m
  (get-raw-transaction:btcio req-to ~ txid)
?~  raw-tx-result
  ~|  %failed-to-get-raw-tx
  !!
=/  =block:bitcoin
  :*  hax=start-hash:urb
      reward=50.000.000
      height=start-height:urb
      ^=  txs
      :~  mock-coinbase-tx:test-ord-core
          ::  u.raw-tx-result
          u.raw-tx-result
      ==
  ==
=+  ^=  [deps block-processed]
    (find-block-deps:oc start-height:urb block)
?~  deps
  ~|  %no-deps
  !!
~&  >>  deps
::=/  urb-block-result
::  (apply-block-deps:oc [start-height:urb block-processed] deps)
::=.  oc  (handle-block:oc urb-block-result)
::=+  ^=  [effects state-out]  abet:oc
(pure:m !>(~))
--
