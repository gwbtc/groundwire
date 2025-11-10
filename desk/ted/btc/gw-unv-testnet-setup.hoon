/-  spider, urb
/+  *ord, ul=urb, *test, gw=groundwire, bip32, b173=bip-b173,
    rpc=json-rpc, scr=btc-script, strandio, btcio, bc=bitcoin
/=  unv-tests  /tests/unv
^-  thread:spider
|=  args=vase
=/  m  (strand:strandio ,vase)
^-  form:m
=/  [=req-to:btcio seds=(list @uw)]  (need !<((unit [req-to:btcio (list @uw)]) args))
=/  oc  ord-core:ul
|^
?~  seds
  ~|  %no-seeds
  !!
?.  =(2 (lent seds))
  ~|  %need-exactly-two-seeds
  !!
;<  malz=(list mal)  bind:m  (min-malz seds)
?~  malz
  ~|  %no-malz
  !!
=/  walz=(list _wallet:unv-tests)
  %+  turn  malz
  |=  mal=_+<:nu:wallet:unv-tests
  ^+  wallet:unv-tests
  =/  wal  (nu:wallet:unv-tests mal)
  wal
?~  walz
  ~|  %no-walz
  !!
=+  [ali=i.walz car=(head (tail walz))]
=/  txs  (test-0:unv-tests [ali car])
;<  *  bind:m
  (mine-blocks-to-address:btcio req-to ~ dumb-wal 1)
=|  ret=(list @ux)
;<  lat=(unit @ud)  bind:m  (get-block-count:btcio req-to ~)
=/  from  (need lat)
|-  ^-  form:m
?~  txs  (process-blocks from)
;<  *  bind:m
  (mine-blocks-to-address:btcio req-to ~ dumb-wal 1)
;<  res=(unit @ux)  bind:m  (send-raw-transaction:btcio req-to ~ i.txs)
?~  res  ~|('commit tx failed' !!)
$(txs t.txs, ret u.res^ret)
::
++  process-blocks
  |=  from=@ud
  =*  id  +<
  =/  m  (strand:strandio ,vase)
  ^-  form:m
  ;<  *  bind:m
    (mine-blocks-to-address:btcio req-to ~ dumb-wal 1)
  ;<  res=(unit @ux)  bind:m
    (get-block-hash:btcio req-to ~ (dec from))
  =/  hax  (need res)
  =.  block-id.oc  [hax (dec from)]
  ;<  lat=(unit @ud)  bind:m
    (get-block-count:btcio req-to ~)
  ?~  lat  !!
  =/  to  u.lat
  =/  num  from
  |-  ^-  form:m
  ?.  (lte num to)  (pure:m !>([fx state]:oc))
  ;<  boc=(unit block:bc)  bind:m
    (get-block-by-number:btcio req-to ~ num)
  ?~  boc  !!
  ;<  boc=[num=@ud urb-block:urb]  bind:m  (elab-block num u.boc)
  =.  oc  (handle-block:oc boc)
  $(num +(num))
::
  ++  elab-block
    |=  [=num:id:block:bc =block:bc]
    =/  m  (strand:strandio ,[num=@ud urb-block:urb])
    =/  deps  (find-block-deps:oc num block)
    =/  txs  (tail txs.block)
    |-  ^-  form:m
    ?~  txs
      (pure:m (apply-block-deps:oc [num +.deps] -.deps))
    =/  is  is.i.txs
    |-  ^-  form:m
    :: refactor to use gettxout
    ?~  is  ^$(txs t.txs)
    =/  dep  (~(get by -.deps) [txid pos]:i.is)
    ?:  &(?=(^ dep) ?=(^ value.u.dep))  $(is t.is)
    ;<  utx=(unit tx:bc)  bind:m
      (get-raw-transaction:btcio req-to ~ txid.i.is)
    ?~  utx  !!
    =/  os  os.u.utx
    =|  pos=@ud
    |-  ^-  form:m
    ?~  os  ^$(is t.is)
    =/  dep  (~(get by -.deps) [id.u.utx pos])
    ?:  &(?=(^ dep) ?=(^ value.u.dep))  $(os t.os, pos +(pos))
    =/  sots=(list raw-sotx:urb)  ?~(dep ~ sots.u.dep)
    $(os t.os, pos +(pos), -.deps (~(put by -.deps) [id.u.utx pos] [sots `value.i.os]))
::=/  =wallet  (make-wallet bowl)
::;<  mined=(unit (list octs))  bind:m  (mine-blocks-to-address:btcio req-to ~ address.ext.wallet 104)
::?~  mined  ~|(%mine-block-fail !!)
::;<  block=(unit block:bc)  bind:m  (get-block:btcio req-to ~ [%hax q:(head u.mined)])
::?~  block  ~|(%wtf !!)
::?~  txs.u.block  ~|(%wtf !!)
::=/  txid=@ux  txid:(head txs.u.block)
::;<  fresh1=bowl:spider  bind:m  get-bowl:strandio
::=+  val=value:(head os.tx:(head txs.u.block))
::=/  commit=transaction:gw
::  %:  build-commit-tx
::    [txid 0]
::    ext.wallet
::    val
::    own.wallet
::    fresh1
::    ~
::  ==
::=/  commit-hex=octs  (txn:encode:gw commit)
::=/  commit-txid=@ux  (make-txid commit)
::;<  comres=(unit @ux)  bind:m  (send-raw-transaction:btcio req-to ~ commit-hex)
::?~  comres  ~|('commit tx failed' !!)
::;<  fresh2=bowl:spider  bind:m  get-bowl:strandio
::=/  reveal=transaction:gw
::  %:  build-reveal-tx
::    [u.comres 0]
::    (snag 0 outputs.commit)
::    internal.own.wallet
::    fresh2
::  ==
::=/  reveal-hex  (txn:encode:gw reveal)
::=/  reveal-txid=@ux  (make-txid reveal)
::;<  revres=(unit @ux)  bind:m  (send-raw-transaction:btcio req-to ~ reveal-hex)
::?~  revres  ~|('reveal tx failed' !!)
::(pure:m !>([comres revres]))
::::
+$  mal  _+<:nu:wallet:unv-tests
++  dumb-wal
  ^-  @t
  =+  [kp i]=%*(derive wallet:unv-tests sed (shax 'dumb-wall'))
  (need (encode-taproot:b173 %regtest 32^x.pub.kp))
::
++  min-malz
  |=  seds=(list @)
  =/  m  (strand:strandio (list mal))
  =|  malz=(list mal)
  |-  ^-  form:m
  ?~  seds
    ;<  *  bind:m
      (mine-blocks-to-address:btcio req-to ~ dumb-wal 100)
    (pure:m (flop malz))
  =+  [kp i]=%*(derive wallet:unv-tests sed i.seds)
  =/  tw=keypair:gw  ~(tweak-keypair p2tr:gw `x.pub.kp ~ `priv.kp)
  =/  address=cord  (need (encode-taproot:b173 %regtest 32^x.pub.tw))
  ;<  mined=(unit (list @ux))  bind:m
    (mine-blocks-to-address:btcio req-to ~ address 1)
  ?>  ?=([~ * ~] mined)
  ;<  boc=(unit block:bc)  bind:m  (get-block-by-hash:btcio req-to ~ i.u.mined)
  ?~  boc  !!
  =/  tx  (head txs.u.boc)
  =/  out  (head os.tx)
  =/  output  (make-output:unv-tests kp `value.out ~)
  =/  =mal  [i.seds i [[id.tx 0] output]]
  $(seds t.seds, malz mal^malz)
--
