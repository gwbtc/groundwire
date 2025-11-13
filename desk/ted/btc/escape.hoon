/-  bitcoin, spider
/+  btcio, bl=bitcoin, b173=bip-b173,
    gw=groundwire, scr=btc-script, strandio
/=  unv-tests  /tests/unv
^-  thread:spider
|=  args=vase
=/  m  (strand:strandio ,vase)
^-  form:m
=/  [=req-to:btcio =ship sed=@uw txid=@ux pos=@ud]
  (need !<((unit [req-to:btcio @p @uw @ux @ud]) args))
::  lookup UTXO by outpoint
;<    utxo-data=(unit json)
    bind:m
  (get-tx-out:btcio req-to ~ txid pos %.y)
?~  utxo-data
  ~|  'UTXO not found or already spent'
  !!
::  reconstruct UTXO from outpoint and seed
=+  [kp i]=%*(derive wallet:unv-tests sed sed)  :: Get the keypair info
=/  tw=keypair:gw  ~(tweak-keypair p2tr:gw `x.pub.kp ~ `priv.kp)
=/  address=@t  (need (encode-taproot:b173 %regtest 32^x.pub.tw))
=/  output=output:gw  (make-output:unv-tests kp ~ ~)  :: Reconstruct output
=/  current-utxo=utxo:unv-tests  [[txid pos] output]
=/  wal  (nu:wallet:unv-tests sed i current-utxo)
=+  walt=(nu:walt:unv-tests 0 wal)
=^  escape-commit-out  walt  (escape:btc:walt ship)
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
::  return ship ID and outpoint
=/  outpoint  -.final-utxo
(pure:m !>([fig:walt output]))
