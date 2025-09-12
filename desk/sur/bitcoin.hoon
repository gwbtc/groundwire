::  sur/btc.hoon
::  Utilities for working with BTC data types and transactions
::
::  chyg: whether account is (non-)change. 0 or 1
::  bytc: "btc-byts" with dat cast to @ux
|%
+$  network
  ?(%main %testnet %regtest)
::
+$  hexb
  $+  btc-hexb
  [wid=@ dat=@ux]                :: hex byts
::
+$  bits
  $+  btc-bits
  [wid=@ dat=@ub]
::
+$  xpub  @ta
::
+$  address
  $+  btc-address
  $%  [%base58 @uc]
      [%bech32 cord]
  ==
::
+$  fprint  hexb
+$  bipt  $?(%44 %49 %84)
+$  chyg  $?(%0 %1)
+$  idx   @ud
::
+$  hdkey
  $+  btc-hdkey
  [=fprint pubkey=hexb =network =bipt =chyg =idx]
::
+$  sats  @ud
+$  vbytes  @ud
+$  txid  @ux
+$  utxo
  $+  btc-utxo
  [pos=@ =txid height=@ value=sats recvd=(unit @da)]
::
++  address-info
  $:  =address
      confirmed-value=sats
      unconfirmed-value=sats
      utxos=(set utxo)
  ==
++  tx
  =<  tx
  |%
  +$  tx
    $+  btc-tx
    [id=txid dataw]
  ::
  +$  dataw
    $+  btc-tx-data-witness
    $:  is=(list inputw)
        os=(list output)
        locktime=@ud
        nversion=@ud
        segwit=(unit @ud)
    ==
  ::
  +$  data
    $+  btc-tx-data
    $:  is=(list input)
        os=(list output)
        locktime=@ud
        nversion=@ud
        segwit=(unit @ud)
    ==
  ::
  +$  in-val
    $+  btc-input-value
    $:  =txid
        pos=@ud
        =address
    ==
  ::
  +$  out-val
    $+  btc-output-value
    $:  =txid
        pos=@ud
        =address
        value=sats
    ==
  ::  included: whether tx is in the mempool or blockchain
  ::
  +$  info
    $+  btc-tx-info
    $:  included=?
        =txid
        confs=@ud
        recvd=(unit @da)
        inputs=(list in-val)
        outputs=(list out-val)
    ==
  ::
  +$  input
    $+  btc-tx-input
    $:  =txid
        pos=@ud
        sequence=hexb
        script-sig=(unit hexb)
        pubkey=(unit hexb)
    ==
  ::
  +$  inputw
    $+  btc-tx-input-witness
    [=witness input]
  ::
  +$  output
    $+  btc-tx-output
    $:  script-pubkey=hexb
        value=sats
    ==
  ::
  +$  witness
    $+  btc-tx-witness
    (list hexb)
  --
++  block
  =<  block
  |%
  +$  id
    $+  btc-block-id
    [=hax =num]
  +$  hax  @ux
  +$  num  @ud
  +$  block
    $+  btc-block
    $:  =hax
        reward=@ud
        height=@ud
        txs=(list tx)
    ==
  --
++  psbt
  |%
  +$  base64  cord
  +$  in
    $+  btc-psbt-input
    [=utxo rawtx=hexb =hdkey]
  ::
  +$  out
    $+  btc-psbt-output
    [=address hk=(unit hdkey)]
  ::
  +$  target  $?(%input %output)
  +$  keyval
    $+  btc-psbt-keyval
    [key=hexb val=hexb]
  ::
  +$  map  (list keyval)
  --
++  ops
  |%
  ++  op-dup  118
  ++  op-equalverify  136
  ++  op-hash160      169
  ++  op-checksig     172
  --
--
