/-  spider
/+  *ord, *test, gw=groundwire, bip32, b173=bip-b173, rpc=json-rpc, scr=btc-script, strandio, btcio, psbt, bc=bitcoin
=<
^-  thread:spider
|=  args=vase
=/  m  (strand:strandio ,vase)
^-  form:m
=/  =req-to:btcio  (need !<((unit req-to:btcio) args))
;<  =bowl:spider  bind:m  get-bowl:strandio
=/  =wallet  (make-wallet bowl)
~&  `@ux`x.pub.internal.own.wallet
;<  mined=(unit (list @ux))  bind:m  (mine-blocks-to-address:btcio req-to ~ address.ext.wallet 101)
?~  mined  ~|(%mine-block-fail !!)
;<  block=(unit block:bc)  bind:m  (get-block:btcio req-to ~ [%hax (head u.mined)])
?~  block  ~|(%wtf !!)
?~  txs.u.block  ~|(%wtf !!)
=/  txid=@ux  id:(head txs.u.block)
;<  fresh1=bowl:spider  bind:m  get-bowl:strandio
=+  val=value:(head os:(head txs.u.block))
=/  commit=tx:gw
  %:  build-commit-tx
    [txid 0]
    ext.wallet
    val
    own.wallet
    fresh1
    ~
  ==
=/  commit-hex=octs  (txn:encode:gw commit)
=/  commit-txid=@ux  (make-txid commit)
;<  comres=(unit @ux)  bind:m  (send-raw-transaction:btcio req-to ~ commit-hex)
?~  comres  ~|('commit tx failed' !!)
;<  fresh2=bowl:spider  bind:m  get-bowl:strandio
=/  reveal=tx:gw
  %:  build-reveal-tx
    [u.comres 0]
    (snag 0 outputs.commit)
    internal.own.wallet
    fresh2
  ==
=/  reveal-hex  (txn:encode:gw reveal)
=/  reveal-txid=@ux  (make-txid reveal)
;<  revres=(unit @ux)  bind:m  (send-raw-transaction:btcio req-to ~ reveal-hex)
?~  revres  ~|('reveal tx failed' !!)
~&  comres=[res=u.comres txid=commit-txid =(u.comres commit-txid)]
~&  revres=[res=u.revres txid=reveal-txid =(u.revres reveal-txid)]
(pure:m !>([comres revres]))
::
|%
+$  spender
  $:  internal=keypair:gw
      tweaked=keypair:gw
      address=cord
  ==
+$  wallet  
  $:  seed=@ux
      ext=spender
      own=spender
  ==
++  compress-point  compress-point:secp256k1:secp:crypto
::
++  make-wallet
  |=  =bowl:spider
  ^-  wallet
  =+  seed=(~(raw og eny.bowl) 256)
  =+  init=(derive-sequence:(from-seed:bip32 32^seed) ~[1.337 0 0])
  =/  =keypair:gw  [pub=pub.init priv=prv.init]
  =/  tweaked=keypair:gw  ~(tweak-keypair p2tr:gw `x.pub.keypair ~ `priv.keypair)
  =/  addr=cord  (need (encode-taproot:b173 %regtest 32^x.pub.tweaked))
  =/  ext=spender  [keypair tweaked addr]
  =+  owner=(derive-sequence:(from-seed:bip32 32^seed) ~[1.338 0 0])
  =/  k=keypair:gw  [pub=pub.owner priv=prv.owner]
  =/  tweak=keypair:gw  ~(tweak-keypair p2tr:gw `x.pub.k ~ `priv.k)
  =/  address=cord  (need (encode-taproot:b173 %regtest 32^x.pub.tweak))
  [seed ext [k tweak address]]
::
++  make-spend-script
  |=  [int-key=@ lopes=script:scr]
  ^-  script:scr:gw
  ~&  make-script+`@ux`int-key
  [[%op-push ~ (flipb:gw 32^int-key)] %op-checksig lopes]
  :: todo test 0x55 ord corner case
::
++  build-commit-tx
  |=  $:  =outpoint:gw
          from=spender
          val=@ud
          owner=spender
          =bowl:spider
          lopes=script:scr
      ==
  ^-  tx:gw
  =|  =tx:gw
  =|  =output:gw
  =.  output
    %_  output
      value  val
      internal-keys  internal.from
      script-pubkey  ~(scriptpubkey p2tr:gw `x.pub.internal.from ~ ~)
    ==
  =/  spend-script=script:scr:gw  (make-spend-script x.pub.internal.owner lopes)
  :: by passing ~ we default to SIGHASH_DEFAULT, equivalent to SIGHASH_ALL, so when we sign this input later we'll commit to all and only
  :: the inputs and outputs we've added to the transaction up to that point
  =.  tx  (~(add-input build:gw tx) outpoint output ~ ~)
  =.  tx
    %^  ~(add-output build:gw tx)
        (sub val 150)  :: a tx with 1 keypath-spend input and 1 P2TR output should weigh approximately 103vB
      internal.owner
    `spend-script
  -:(~(finalize build:gw tx) eny.bowl)
::
++  build-reveal-tx
  |=  $:  =outpoint:gw
          =output:gw
          =keypair:gw
          =bowl:spider
      ==
  ^-  tx:gw
  ?~  spend-script.output  !!
  =|  reveal=tx:gw
  =.  reveal
    %:  ~(add-input build:gw reveal)
      outpoint
      output
      ~  ~
    ==
  =/  less-fees=@
    (sub value.output (add (lent spend-script.output) 400))
  =.  reveal
    %^  ~(add-output build:gw reveal)
        less-fees
      keypair
    ~
  -:(~(finalize build:gw reveal) eny.bowl)
::
++  make-txid
  |=  t=tx:gw
  ^-  @ux
  (txid:encode:gw t)
--
