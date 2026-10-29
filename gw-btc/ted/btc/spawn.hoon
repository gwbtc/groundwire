/-  spider, bitcoin, urb
/+  gw=groundwire, b173=bip-b173,
    scr=btc-script, strandio, btcio, bl=bitcoin
/=  unv-tests  /tests/unv
^-  thread:spider
|=  args=vase
=/  m  (strand:strandio ,vase)
^-  form:m
=/  [=req-to:btcio sed=@uw =single:skim-sotx:urb]
  (need !<((unit [req-to:btcio @uw single:skim-sotx:urb]) args))
?.  =(%spawn -.single)
  ~|  %wrong-sotx
  !!
::  derive the wallet from the sed
=+  ^=  [kp i]
    %*(derive wallet:unv-tests sed sed)
=/  tw=keypair:gw
  ~(tweak-keypair p2tr:gw `x.pub.kp ~ `priv.kp)
=/  address=@t
  (need (encode-taproot:b173 %regtest 32^x.pub.tw))
::  mine some Bitcoin to this address
;<    mined=(unit (list @ux))
    bind:m
  (mine-blocks-to-address:btcio req-to ~ address 1)
?>  ?=([~ * ~] mined)
::  mature the coinbase tx from the .mined block
;<    *
    bind:m
  (mine-blocks-to-address:btcio req-to ~ address 100)
;<    boc=(unit block:bl)
    bind:m
  (get-block-by-hash:btcio req-to ~ i.u.mined)
?~  boc
  ~|  %failed-to-get-mined-block
  !!
=/  tx=tx:bitcoin          (head txs.u.boc)
=/  out=output:tx:bitcoin  (head os.tx)
::  XX does output need to be defined in /lib/groundwire?
::       if we need some extra metadata beyond $output:tx:bitcoin
::       then we should have an $output:groundwire defined
::       in a new file /sur/groundwire
=/  output=output:gw       (make-output:unv-tests kp `value.out ~)
::  XX $utxo should not be defined in /tests/unv
=/  utxo=utxo:unv-tests    [[id.tx 0] output]
=/  wal
  (nu:walt:unv-tests 0 (nu:wallet:unv-tests sed i utxo))
::  XX should use inputs given in the args to spawn
=^  adopt-commit-out     wal  (adopt:btc:wal fig:wal)
=^  spawn-commit-out     wal  (spawn:btc:wal adopt-commit-out `0 0 0)
=^  spawn-reveal-tx      wal  (spend:btc:wal spawn-commit-out)
=^  adopt-reveal-tx      wal  (spend:btc:wal adopt-commit-out)
=^  keyspend-commit-out  wal  make-key-out:btc:wal
=^  keyspend-reveal-tx   wal  (spend:btc:wal keyspend-commit-out)
=/  final-utxo  utxo:wal:wal
;<    spawn-res=(unit @ux)
    bind:m
  (send-raw-transaction:btcio req-to ~ spawn-reveal-tx)
?~  spawn-res
  ~|  %failed-to-send-spawn-tx
  !!
;<    adopt-res=(unit @ux)
    bind:m
  (send-raw-transaction:btcio req-to ~ adopt-reveal-tx)
?~  adopt-res
  ~|  %failed-to-send-adopt-tx
  !!
;<    keyspend-res=(unit @ux)
    bind:m
  (send-raw-transaction:btcio req-to ~ keyspend-reveal-tx)
?~  keyspend-res
  ~|  %failed-to-send-keyspend-tx
  !!
::  mine buffer to finalize tx
;<    *
    bind:m
  (mine-blocks-to-address:btcio req-to ~ address 8)
(pure:m !>(final-utxo))
