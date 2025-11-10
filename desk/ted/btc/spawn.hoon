/-  spider, urb
/+  *ord, ul=urb, *test, *mip, lais, gw=groundwire, bip32, b173=bip-b173,
    rpc=json-rpc, scr=btc-script, strandio, btcio, bc=bitcoin
/=  unv-tests  /tests/unv
^-  thread:spider
|=  args=vase
=/  m  (strand:strandio ,vase)
^-  form:m
=/  [=req-to:btcio sed=@uw]  (need !<((unit [req-to:btcio @uw]) args))
|^
:: Create a single wallet like test-0 does
=+  [kp i]=%*(derive wallet:unv-tests sed sed)
=/  tw=keypair:gw  ~(tweak-keypair p2tr:gw `x.pub.kp ~ `priv.kp)
=/  address=cord  (need (encode-taproot:b173 %regtest 32^x.pub.tw))
;<  mined=(unit (list @ux))  bind:m
  (mine-blocks-to-address:btcio req-to ~ address 1)
?>  ?=([~ * ~] mined)
;<  *  bind:m
  (mine-blocks-to-address:btcio req-to ~ dumb-wal 100)
;<  boc=(unit block:bc)  bind:m  (get-block-by-hash:btcio req-to ~ i.u.mined)
?~  boc  !!
=/  tx  (head txs.u.boc)
=/  out  (head os.tx)
=/  output  (make-output:unv-tests kp `value.out ~)
=/  utxo  [[id.tx 0] output]
=/  wal  (nu:wallet:unv-tests sed i utxo)
:: Run mini-test-0 - simplified version that just creates a spawn
=/  txs  (mini-test-0 wal)
;<  *  bind:m
  (mine-blocks-to-address:btcio req-to ~ dumb-wal 1)
=|  ret=(list @ux)
|-  ^-  form:m
?~  txs  
  ;<  *  bind:m
    (mine-blocks-to-address:btcio req-to ~ dumb-wal 8)
  (pure:m !>(ret))
;<  *  bind:m
  (mine-blocks-to-address:btcio req-to ~ dumb-wal 1)
;<  res=(unit @ux)  bind:m  (send-raw-transaction:btcio req-to ~ i.txs)
?~  res  ~|('tx failed' !!)
$(txs t.txs, ret u.res^ret)
::
++  mini-test-0
  |=  wal=_wallet:unv-tests
  ^-  (list byts)
  =+  walt=(nu:walt:unv-tests 0 wal)
  :: Copy test-0 lines 348-351 exactly - escape+spawn pattern
  =^  escape-commit-out  walt  (escape:btc:walt fig:walt)
  =^  spawn-commit-out   walt  (spawn:btc:walt [spk=escape-commit-out pos=`0 off=0 tej=0])
  =^  spawn-commit-tx    walt  (spend:btc:walt spawn-commit-out)
  =^  escape-commit-tx   walt  (spend:btc:walt escape-commit-out)
  :~  spawn-commit-tx
      escape-commit-tx
  ==
::
++  dumb-wal
  ^-  @t
  =+  [kp i]=%*(derive wallet:unv-tests sed (shax 'dumb-wall'))
  (need (encode-taproot:b173 %regtest 32^x.pub.kp))
--