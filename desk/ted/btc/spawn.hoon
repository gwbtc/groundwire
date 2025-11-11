/-  spider
/+  gw=groundwire, b173=bip-b173,
    scr=btc-script, strandio, btcio, bl=bitcoin
/=  unv-tests  /tests/unv
^-  thread:spider
|=  args=vase
=/  m  (strand:strandio ,vase)
^-  form:m
=/  [=req-to:btcio sed=@uw]  (need !<((unit [req-to:btcio @uw]) args))
::  derive the wallet from the sed
=+  [kp i]=%*(derive wallet:unv-tests sed sed)
=/  tw=keypair:gw  ~(tweak-keypair p2tr:gw `x.pub.kp ~ `priv.kp)
=/  address=@t  (need (encode-taproot:b173 %regtest 32^x.pub.tw))
::  mine some Bitcoin to this address
;<    mined=(unit (list @ux))
    bind:m
  (mine-blocks-to-address:btcio req-to ~ address 1)
?>  ?=([~ * ~] mined)
::  mature the coinbase tx
;<    *
    bind:m
  (mine-blocks-to-address:btcio req-to ~ address 100)
;<    boc=(unit block:bl)
    bind:m
  (get-block-by-hash:btcio req-to ~ i.u.mined)
?~  boc
  !!
=/  tx      (head txs.u.boc)
=/  out     (head os.tx)
=/  output  (make-output:unv-tests kp `value.out ~)
=/  utxo    [[id.tx 0] output]
=/  wal     (nu:wallet:unv-tests sed i utxo)
=/  txs
  ^-  (list byts)
  =+  walt=(nu:walt:unv-tests 0 wal)
  =^  escape-commit-out  walt  (escape:btc:walt fig:walt)
  =^  spawn-commit-out   walt  (spawn:btc:walt [spk=escape-commit-out pos=`0 off=0 tej=0])
  =^  spawn-commit-tx    walt  (spend:btc:walt spawn-commit-out)
  =^  escape-commit-tx   walt  (spend:btc:walt escape-commit-out)
  ::
  :~  spawn-commit-tx
      escape-commit-tx
  ==
;<    *
    bind:m
  (mine-blocks-to-address:btcio req-to ~ address 1)
=|  ret=(list @ux)
|-  ^-  form:m
?~  txs
  ::  finish by mining 8 blocks to finalize the spawn tx
  ;<  *  bind:m
    (mine-blocks-to-address:btcio req-to ~ address 8)
  ::  return list of sent txs
  (pure:m !>(ret))
::  send txs to testnet
;<    *
    bind:m
  (mine-blocks-to-address:btcio req-to ~ address 1)
;<    res=(unit @ux)
    bind:m
  (send-raw-transaction:btcio req-to ~ i.txs)
?~  res
  ~|  'tx failed'
  !!
$(txs t.txs, ret u.res^ret)
