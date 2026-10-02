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
?.  =(%keys -.single)
  ~|  %wrong-sotx
  !!
?>  ?=([%keys *] single)
=+  [kp i]=%*(derive wallet:unv-tests sed sed)
=/  tw=keypair:gw  ~(tweak-keypair p2tr:gw `x.pub.kp ~ `priv.kp)
=/  address=@t  (need (encode-taproot:b173 %regtest 32^x.pub.tw))
=/  wal
  (nu:walt:unv-tests 0 (nu:wallet:unv-tests sed i utxo))
=^  keys-commit-out      wal  (keys:btc:wal breach.single)
=^  keys-reveal-tx       wal  (spend:btc:wal keys-commit-out)
=^  keyspend-commit-out  wal  make-key-out:btc:wal
=^  keyspend-reveal-tx   wal  (spend:btc:wal keyspend-commit-out)
=/  final-utxo  utxo:wal:wal
;<    tx-res=(unit @ux)
    bind:m
  (send-raw-transaction:btcio req-to ~ keys-reveal-tx)
?~  tx-res
  ~|  %send-tx-failed
  !!
;<    keyspend-res=(unit @ux)
    bind:m
  (send-raw-transaction:btcio req-to ~ keyspend-reveal-tx)
?~  keyspend-res
  ~|  %failed-to-send-keyspend-tx
  !!
;<    *
    bind:m
  (mine-blocks-to-address:btcio req-to ~ address 8)
(pure:m !>(final-utxo))
