/-  spider, bitcoin
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
=/  tx=tx:bitcoin          (head txs.u.boc)
=/  out=output:tx:bitcoin  (head os.tx)
=/  output=output:gw       (make-output:unv-tests kp `value.out ~)
=/  utxo=utxo:unv-tests    [[id.tx 0] output]
=/  wal                    (nu:wallet:unv-tests sed i utxo)
=+  walt=(nu:walt:unv-tests 0 wal)
=^  escape-commit-out  walt  (escape:btc:walt fig:walt)
=^  spawn-commit-out   walt  (spawn:btc:walt [spk=escape-commit-out pos=`0 off=0 tej=0])
=^  spawn-commit-tx    walt  (spend:btc:walt spawn-commit-out)
=^  escape-commit-tx   walt  (spend:btc:walt escape-commit-out)
=/  txs  :~  spawn-commit-tx
             escape-commit-tx
         ==
=/  final-utxo  utxo:wal:walt
::  send spawn-commit-tx
;<    spawn-res=(unit @ux)
    bind:m
  (send-raw-transaction:btcio req-to ~ spawn-commit-tx)
?~  spawn-res
  ~|  'spawn tx failed'
  !!
::  send escape-commit-tx  
;<    escape-res=(unit @ux)
    bind:m
  (send-raw-transaction:btcio req-to ~ escape-commit-tx)
?~  escape-res
  ~|  'escape tx failed'
  !!
::  finish by mining 8 blocks to finalize the transactions
;<  *  bind:m
  (mine-blocks-to-address:btcio req-to ~ address 8)
(pure:m !>(final-utxo))
