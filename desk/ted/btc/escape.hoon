/-  bitcoin, spider
/+  btcio, bl=bitcoin, b173=bip-b173,
    gw=groundwire, scr=btc-script, strandio
/=  unv-tests  /tests/unv
^-  thread:spider
|=  args=vase
=/  m  (strand:strandio ,vase)
^-  form:m
=/  [=req-to:btcio her=ship sed=@uw =utxo:unv-tests]
  (need !<((unit [req-to:btcio @p @uw utxo:unv-tests]) args))
::  derive wallet from seed to match the UTXO
=+  [kp i]=%*(derive wallet:unv-tests sed sed)  :: Get the keypair info
=/  tw=keypair:gw  ~(tweak-keypair p2tr:gw `x.pub.kp ~ `priv.kp)
=/  address=@t  (need (encode-taproot:b173 %regtest 32^x.pub.tw))
=/  wal  (nu:wallet:unv-tests sed i utxo)
=+  walt=(nu:walt:unv-tests 0 wal)
=^  escape-commit-out  walt  (escape:btc:walt her)
=^  escape-commit-tx   walt  (spend:btc:walt escape-commit-out)
=/  final-utxo
  utxo:wal:walt
;<    esc-res=(unit @ux)
    bind:m
  (send-raw-transaction:btcio req-to ~ escape-commit-tx)
?~  esc-res
  ~|  'escape tx failed'
  !!
::  finish by mining 8 blocks to finalize the transactions
;<    *
    bind:m
  (mine-blocks-to-address:btcio req-to ~ address 8)
::  return our ID and outpoint
=/  outpoint  -.final-utxo
(pure:m !>([fig:walt outpoint]))
