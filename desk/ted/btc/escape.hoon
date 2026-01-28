/-  spider, bitcoin, urb
/+  gw=groundwire, b173=bip-b173,
    scr=btc-script, strandio, btcio, bl=bitcoin
/=  unv-tests  /tests/unv
^-  thread:spider
|=  args=vase
=/  m  (strand:strandio ,vase)
^-  form:m
=/  [=req-to:btcio sed=@uw =utxo:unv-tests =single:skim-sotx:urb]
  (need !<((unit [req-to:btcio @uw utxo:unv-tests single:skim-sotx:urb]) args))
?.  =(%escape -.single)
  ~|  %wrong-sotx
  !!
?>  ?=([%escape *] single)
=+  [kp i]=%*(derive wallet:unv-tests sed sed)
=/  tw=keypair:gw  ~(tweak-keypair p2tr:gw `x.pub.kp ~ `priv.kp)
=/  address=@t  (need (encode-taproot:b173 %regtest 32^x.pub.tw))
=/  wal
  (nu:walt:unv-tests 0 (nu:wallet:unv-tests sed i utxo))
=^  escape-commit-out    wal  (escape:btc:wal parent.single)
=^  escape-reveal-tx     wal  (spend:btc:wal escape-commit-out)
=/  final-utxo  utxo:wal:wal
;<    tx-res=(unit @ux)
    bind:m
  (send-raw-transaction:btcio req-to ~ escape-reveal-tx)
?~  tx-res
  ~|  %send-tx-failed
  !!
;<    *
    bind:m
  (mine-blocks-to-address:btcio req-to ~ address 8)
(pure:m !>(final-utxo))
