::  test vectors from https://github.com/lightningnetwork/lightning-rfc/blob/master/03-transactions.md#appendix-b-funding-transaction-test-vectors
::
/-  *bolt
/+  *test, script, psbt
/+  bc=bitcoin, tx=transactions
|%
++  test-obscure-commitment-number
  =,  secp256k1:secp:crypto
  =/  oc=point
    %-  decompress-point
    0x3.4f35.5bdc.b7cc.0af7.28ef.3cce.b961.5d90.684b.b5b2.ca5f.859a.b0f0.b704.0758.71aa
  =/  ac=point
    %-  decompress-point
    0x3.2c0b.7cf9.5324.a07d.0539.8b24.0174.dc0c.2be4.44d9.6b15.9aa6.c7f7.b1e6.6868.0991
  =/  cn=commitment-number  42
  %+  expect-eq
    !>(`@ux`(mix 0x2bb0.3852.1914 cn))
    !>(`@ux`(obscure-commitment-number:tx cn oc ac))
::
++  test-funding-tx
  =,  secp256k1:secp:crypto
  =,  tx
  |^
  ;:  weld
    check-witness-script
    check-funding-tx
  ==
  ::
  ++  check-witness-script
    %+  expect-eq
      !>  funding-witness-script
    !>  %-  en:btc-script
        %+  funding-output:script
          local-funding-pubkey
        remote-funding-pubkey
  ::
  ++  check-funding-tx
    %+  expect-eq
      !>  funding-tx
      !>  %-  encode-tx:psbt
          :*  vin=funding-inputs
              vout=funding-outputs
              nversion=2
              nlocktime=0
          ==
  ::
  ++  funding-outputs
    :~  %~  to-output  txout:psbt
        %:  funding-output:tx
          local-funding-pubkey
          remote-funding-pubkey
          10.000.000
        ==
        funding-change
    ==
  ::
  ++  privkey
    [33 0x6b.d078.650f.cee8.444e.4e09.8252.27b8.01a1.ca92.8deb.b750.eb36.e6d5.6124.bb20.e801]
  ::
  ++  local-funding-pubkey
    ^-  pubkey
    %-  decompress-point:secp256k1:secp:crypto
    0x2.3da0.92f6.980e.58d2.c037.1731.80e9.a465.4760.26ee.50f9.6695.963e.8efe.436f.54eb
  ::
  ++  remote-funding-pubkey
    ^-  pubkey
    %-  decompress-point:secp256k1:secp:crypto
    0x3.0e9f.7b62.3d2c.cc7c.9bd4.4d66.d5ce.21ce.504c.0acf.6385.a132.cec6.d3c3.9fa7.11c1
  ::
  ++  funding-tx
    ^-  hexb:bc
    :-  232
        0x200.0000.01ad.bb20.ea41.a842.3ea9.37e7.
    6e81.5163.6bf6.093b.70ea.ff94.2930.d205.7660.
    0521.fd00.0000.006b.4830.4502.2100.9058.7b62.
    01e1.66ad.6af0.227d.3036.a945.4223.d49a.1f11.
    839c.1a36.2184.340e.f024.0220.577f.7cd5.cca7.
    8719.405c.bf1d.e741.4ac0.27f0.239e.f6e2.14c9.
    0fca.ab04.54d8.4b3b.0121.0353.5b32.d5eb.0a6e.
    d098.2a04.79bb.adc9.868d.9836.f6ba.94dd.5a63.
    be16.d875.0691.84ff.ffff.ff02.8096.9800.0000.
    0000.2200.20c0.15c4.a6be.010e.2165.7068.fc2e.
    6a9d.02b2.7ebe.4d49.0a25.846f.7237.f104.d1a3.
    cd20.256d.2901.0000.0016.0014.3ca3.3c2e.4446.
    f4a3.05f2.3c80.df8a.d1af.dcf6.52f9.0000.0000
  ::
  ++  funding-witness-script
    ^-  hexb:bc
    :-  71
    0x52.2102.3da0.92f6.980e.58d2.c037.1731.80e9.
    a465.4760.26ee.50f9.6695.963e.8efe.436f.54eb.
    2103.0e9f.7b62.3d2c.cc7c.9bd4.4d66.d5ce.21ce.
    504c.0acf.6385.a132.cec6.d3c3.9fa7.11c1.52ae
  ::
  ++  input-txid
    ^-  hexb:bc
    :*  wid=32
        dat=0xfd21.0560.7605.d230.2994.ffea.703b.09f6.6b63.5181.6ee7.37a9.3e42.a841.ea20.bbad
    ==
  ::
  ++  funding-script-sig
    :*  wid=107
        dat=0x48.3045.0221.0090.587b.6201.e166.ad6a.f022.
            7d30.36a9.4542.23d4.9a1f.1183.9c1a.3621.8434.
            0ef0.2402.2057.7f7c.d5cc.a787.1940.5cbf.1de7.
            414a.c027.f023.9ef6.e214.c90f.caab.0454.d84b.
            3b01.2103.535b.32d5.eb0a.6ed0.982a.0479.bbad.
            c986.8d98.36f6.ba94.dd5a.63be.16d8.7506.9184
    ==
  ::
  ++  funding-inputs
    :~  =|  =in:tx:psbt
        %=  in
          prevout        [txid=input-txid idx=0]
          nsequence      0xffff.ffff
          script-sig     `funding-script-sig
         ==
    ==
  ::
  ++  funding-change
    ^-  out:tx:psbt
    =|  =out:tx:psbt
    %=  out
      script-pubkey  22^0x14.3ca3.3c2e.4446.f4a3.05f2.3c80.df8a.d1af.dcf6.52f9
      value          4.989.986.080
    ==
  --
::  +tx-test: commitment transaction constants
::
++  tx-test
  |%
  ++  funding-outpoint
    |^  ^-  outpoint
    :*  txid=funding-txid
        pos=funding-output-index
        sats=funding-amount
    ==
    ++  funding-amount  10.000.000
    ++  funding-output-index  0
    ++  funding-txid
      ^-  hexb:bc
      :-  32
      0x8984.484a.580b.825b.9972.d7ad.b150.50b3.ab62.4ccd.7319.46b3.eedd.b92f.4e7e.f6be
    --
  ::
  ++  commitment-number
    ^-  ^commitment-number
    42
  ::
  ++  local-delay
    ^-  blocks
    144
  ::
  ++  dust-limit
    ^-  sats:bc
    546
  ::
  ++  local-payment-basepoint
    =,  secp256k1:secp:crypto
    ^-  point
    %-  decompress-point
    0x3.4f35.5bdc.b7cc.0af7.28ef.3cce.b961.5d90.684b.b5b2.ca5f.859a.b0f0.b704.0758.71aa
  ::
  ++  remote-payment-basepoint
    =,  secp256k1:secp:crypto
    ^-  point
    %-  decompress-point
    0x3.2c0b.7cf9.5324.a07d.0539.8b24.0174.dc0c.2be4.44d9.6b15.9aa6.c7f7.b1e6.6868.0991
  ::
  ++  local-funding-privkey
    ^-  hexb:bc
    :-  32
    0x30ff.4956.bbdd.3222.d44c.c5e8.a126.1dab.1e07.957b.dac5.ae88.fe32.61ef.321f.3749
  ::
  ++  local-funding-pubkey
    ^-  pubkey
    %-  decompress-point:secp256k1:secp:crypto
    0x2.3da0.92f6.980e.58d2.c037.1731.80e9.a465.4760.26ee.50f9.6695.963e.8efe.436f.54eb
  ::
  ++  remote-funding-privkey
    ^-  hexb:bc
    :-  32
    0x1552.dfba.4f6c.f29a.62a0.af13.c8d6.981d.36d0.ef8d.61ba.10fb.0fe9.0da7.634d.7e13
  ::
  ++  remote-funding-pubkey
    ^-  pubkey
    %-  decompress-point:secp256k1:secp:crypto
    0x3.0e9f.7b62.3d2c.cc7c.9bd4.4d66.d5ce.21ce.504c.0acf.6385.a132.cec6.d3c3.9fa7.11c1
  ::
  ++  local-privkey
    ^-  hexb:bc
    :-  32
    0xbb13.b121.cdc3.57cd.2e60.8b0a.ea29.4afc.a36e.2b34.cf95.8e2e.6451.a2f2.7469.4491
  ::
  ++  local-pubkey
    ^-  pubkey
    %-  decompress-point:secp256k1:secp:crypto
    0x3.0d41.7a46.9463.84f8.8d5f.3337.267c.5e57.9765.875d.c4da.ca81.3e21.734b.1406.39e7
  ::
  ++  remote-pubkey
    ^-  pubkey
    %-  decompress-point:secp256k1:secp:crypto
    0x3.9485.4aa6.eab5.b2a8.122c.c726.e9dd.ed05.3a21.84d8.8256.8168.26d6.231c.068d.4a5b
  ::
  ++  local-delayed-pubkey
    ^-  pubkey
    %-  decompress-point:secp256k1:secp:crypto
    0x3.fd59.6052.8dc1.5201.4952.efdb.702a.88f7.1e3c.1653.b231.4431.701e.c77e.57fd.e83c
  ::
  ++  local-revocation-pubkey
    ^-  pubkey
    %-  decompress-point:secp256k1:secp:crypto
    0x2.12a1.40cd.0c65.39d0.7cd0.8dfe.0998.4dec.3251.ea80.8b89.2efe.ac3e.de94.02bf.2b19
  ::
  ++  remote-funding-sig
    ^-  hexb:bc
    :-  72
    0x3045.0221.00f5.1d2e.566a.70ba.740f.c5d8.
      c0f0.7b9b.93d2.ed74.1c3c.0860.c613.173d.
      e7d3.9e79.6802.2041.376d.520e.9c0e.1ad5.
      2248.ddf4.b22e.12be.8763.007d.f977.253e.
      f45a.4ca3.bdb7.c001
  ::
  ++  local-funding-sig
    ^-  hexb:bc
    :-  71
    0x30.4402.2051.b75c.7319.8c6d.eee1.a875.
    871c.3961.8329.09ac.d297.c6b9.08d5.9e33.
    19e5.185a.4602.2055.c419.379c.5051.a78d.
    00db.bce1.1b5b.664a.0c22.815f.bcc6.fcef.
    6b19.37c3.8369.3901
  ::
  +$  htlc-data
    $:  received=?
        id=@ud
        amount=msats
        preimage=hexb:bc
        expiry=@ud
    ==
  ::
  +$  test-vector
    $:  to-local-msat=msats
        to-remote-msat=msats
        htlcs=(list htlc-data)
        local-feerate-per-kw=@ud
        our-funding-signature=(unit signature)
        her-funding-signature=(unit signature)
        output-commit-tx=hexb:bc
    ==
  --
::
++  test-commitment-tx
  ^-  tang
  |^
  ;:  weld
    %+  category  "without anchors"
      %-  zing
      %+  turn  tx-test-vectors
      (check-tx %.n)
    %+  category  "with anchors"
      %-  zing
      %+  turn  anchor-tx-test-vectors
      (check-tx %.y)
  ==
  ::
  ++  make-add-update
    |=  h=update-add-htlc:msg
    ^-  add-htlc-update
    =|  update=add-htlc-update
    %=  update
      htlc-id       htlc-id.h
      timeout       cltv-expiry.h
      amount-msats  amount-msats.h
      payment-hash  payment-hash.h
    ==
  ::
  ++  check-tx
    |=  anchor=?
    |=  =test-vector:tx-test
    =/  [received=(list update-add-htlc:msg) offered=(list update-add-htlc:msg)]
      (partition-htlcs htlcs.test-vector)
    ::
    =/  received-htlcs=(list htlc)
      %+  turn  received
      |=  h=update-add-htlc:msg
      :-  (make-add-update h)
      %-  p2wsh:script
      %:  htlc-received:script
        local-htlc-pubkey=local-pubkey:tx-test
        remote-htlc-pubkey=remote-pubkey:tx-test
        revocation-pubkey=local-revocation-pubkey:tx-test
        payment-hash=payment-hash.h
        cltv-expiry=cltv-expiry.h
        confirmed-spend=anchor
      ==
    ::
    =.  received-htlcs
      %+  skip  received-htlcs
      |=  h=htlc
      %:  is-trimmed:tx
        %received
        amount-msats.h
        local-feerate-per-kw.test-vector
        dust-limit:tx-test
        anchor
      ==
    ::
    =/  offered-htlcs=(list htlc)
      %+  turn  offered
      |=  h=update-add-htlc:msg
      :-  (make-add-update h)
      %-  p2wsh:script
      %:  htlc-offered:script
        local-htlc-pubkey=local-pubkey:tx-test
        remote-htlc-pubkey=remote-pubkey:tx-test
        revocation-pubkey=local-revocation-pubkey:tx-test
        payment-hash=payment-hash.h
        confirmed-spend=anchor
      ==
    ::
    =.  offered-htlcs
      %+  skip  offered-htlcs
      |=  h=htlc
      %:  is-trimmed:tx
        %sent
        amount-msats.h
        local-feerate-per-kw.test-vector
        dust-limit:tx-test
        anchor
      ==
    ::
    =/  fees=(map owner msats)
      %:  commitment-fee:tx
        num-htlcs=(lent (welp offered-htlcs received-htlcs))
        feerate=local-feerate-per-kw.test-vector
        is-local-initiator=%.y
        anchors=anchor
        round=%.n
      ==
    ::
    =+  remote-pubkey=remote-pubkey:tx-test
    =?  remote-pubkey  anchor
      remote-payment-basepoint:tx-test
    ::
    =+  ^=  commitment-tx
      %:  commitment:tx
        commitment-number=commitment-number:tx-test
        local-funding-pubkey=local-funding-pubkey:tx-test
        remote-funding-pubkey=remote-funding-pubkey:tx-test
        remote-payment-pubkey=remote-pubkey
        funder-payment-basepoint=local-payment-basepoint:tx-test
        fundee-payment-basepoint=remote-payment-basepoint:tx-test
        revocation-pubkey=local-revocation-pubkey:tx-test
        delayed-pubkey=local-delayed-pubkey:tx-test
        to-self-delay=local-delay:tx-test
        funding-outpoint=funding-outpoint:tx-test
        local-amount=to-local-msat.test-vector
        remote-amount=to-remote-msat.test-vector
        dust-limit-sats=dust-limit:tx-test
        anchor-outputs=anchor
        htlcs=(welp received-htlcs offered-htlcs)
        fees-per-participant=fees
      ==
    ::
    %+  expect-eq
      !>  output-commit-tx.test-vector
      !>  %-  extract:psbt
          %:  sign-and-add-remote-sig
            commitment-tx
            remote-funding-pubkey:tx-test
            (fall her-funding-signature.test-vector remote-funding-sig:tx-test)
            local-funding-pubkey:tx-test
            local-funding-privkey:tx-test
          ==
  ::
  ++  partition-htlcs
    |=  htlcs=(list htlc-data:tx-test)
    =/  [received=(list htlc-data:tx-test) offered=(list htlc-data:tx-test)]
      %+  skid  htlcs
      |=  h=htlc-data:tx-test
      received.h
    :-  (turn received create-htlc)
        (turn offered create-htlc)
  ::
  ++  create-htlc
    |=  h=htlc-data:tx-test
    ^-  update-add-htlc:msg
    =|  htlc=update-add-htlc:msg
    %=  htlc
      channel-id    0
      htlc-id       id.h
      amount-msats  amount.h
      payment-hash  (sha256:bcu:bc preimage.h)
      cltv-expiry   expiry.h
    ==
  ::
  ++  sign-and-add-remote-sig
    |=  [tx=psbt:psbt =remote=pubkey =remote=signature =pubkey privkey=hexb:bc]
    ^-  psbt:psbt
    =+  keys=(malt ~[[pubkey privkey]])
    =.  tx  (~(all sign:psbt tx) keys)
    =+  txin=(snag 0 inputs.tx)
    =.  partial-sigs.txin
      %+  ~(put by partial-sigs.txin)
        33^(compress-point:secp256k1:secp:crypto remote-pubkey)
      remote-signature
    tx(inputs (snap inputs.tx 0 txin))
  ::
  ++  htlc-data
    ^-  (list htlc-data:tx-test)
    :~
      :*  received=%.y
          id=0
          amount=1.000.000
          preimage=[wid=32 dat=0x0]
          expiry=500
      ==
      ::
      :*  received=%.y
          id=1
          amount=2.000.000
          preimage=[wid=32 dat=0x101.0101.0101.0101.0101.0101.0101.0101.0101.0101.0101.0101.0101.0101.0101.0101]
          expiry=501
      ==
      ::
      :*  received=%.n
          id=2
          amount=2.000.000
          preimage=[wid=32 dat=0x202.0202.0202.0202.0202.0202.0202.0202.0202.0202.0202.0202.0202.0202.0202.0202]
          expiry=502
      ==
      ::
      :*  received=%.n
          id=3
          amount=3.000.000
          preimage=[wid=32 dat=0x303.0303.0303.0303.0303.0303.0303.0303.0303.0303.0303.0303.0303.0303.0303.0303]
          expiry=503
      ==
      ::
      :*  received=%.y
          id=4
          amount=4.000.000
          preimage=[wid=32 dat=0x404.0404.0404.0404.0404.0404.0404.0404.0404.0404.0404.0404.0404.0404.0404.0404]
          expiry=504
      ==
    ==
  ::
  ++  htlc-data-2
    ^-  (list htlc-data:tx-test)
    :~
      :*  received=%.y
          id=1
          amount=2.000.000
          preimage=[wid=32 dat=0x101.0101.0101.0101.0101.0101.0101.0101.0101.0101.0101.0101.0101.0101.0101.0101]
          expiry=501
      ==
      ::
      :*  received=%.n
          id=5
          amount=5.000.000
          preimage=[wid=32 dat=0x505.0505.0505.0505.0505.0505.0505.0505.0505.0505.0505.0505.0505.0505.0505.0505]
          expiry=506
      ==
      ::
      :*  received=%.n
          id=6
          amount=5.000.001
          preimage=[wid=32 dat=0x505.0505.0505.0505.0505.0505.0505.0505.0505.0505.0505.0505.0505.0505.0505.0505]
          expiry=505
      ==
    ==
  ::
  ++  tx-test-vectors
    ^-  (list test-vector:tx-test)
    :~
      :*  to-local-msat=7.000.000.000
          to-remote-msat=3.000.000.000
          htlcs=~
          local-feerate-per-kw=15.000
          our-funding-signature=~
          her-funding-signature=~
          output-commit-tx=[346 0x200.0000.0001.01be.f67e.4e2f.b9dd.eeb3.4619.73cd.4c62.abb3.5050.b1ad.d772.995b.820b.584a.4884.8900.0000.0000.38b0.2b80.02c0.c62d.0000.0000.0016.0014.ccf1.af2f.2aab.ee14.bb40.fa38.51ab.2301.de84.3110.54a5.6a00.0000.0000.2200.204a.db4e.2f00.643d.b396.dd12.0d4e.7dc1.7625.f5f2.c11a.40d8.57ac.cc86.2d6b.7dd8.0e04.0047.3044.0220.51b7.5c73.198c.6dee.e1a8.7587.1c39.6183.2909.acd2.97c6.b908.d59e.3319.e518.5a46.0220.55c4.1937.9c50.51a7.8d00.dbbc.e11b.5b66.4a0c.2281.5fbc.c6fc.ef6b.1937.c383.6939.0148.3045.0221.00f5.1d2e.566a.70ba.740f.c5d8.c0f0.7b9b.93d2.ed74.1c3c.0860.c613.173d.e7d3.9e79.6802.2041.376d.520e.9c0e.1ad5.2248.ddf4.b22e.12be.8763.007d.f977.253e.f45a.4ca3.bdb7.c001.4752.2102.3da0.92f6.980e.58d2.c037.1731.80e9.a465.4760.26ee.50f9.6695.963e.8efe.436f.54eb.2103.0e9f.7b62.3d2c.cc7c.9bd4.4d66.d5ce.21ce.504c.0acf.6385.a132.cec6.d3c3.9fa7.11c1.52ae.3e19.5220]
      ==
      ::
      :*  to-local-msat=6.988.000.000
          to-remote-msat=3.000.000.000
          htlcs=htlc-data
          local-feerate-per-kw=0
          our-funding-signature=(some [71 0x30.4402.2027.5b0c.325a.5e93.5565.0dc3.0c0e.ccfb.c7ef.b239.87c2.4b55.6b9d.fdd4.0eff.ca18.d202.206c.aceb.2c06.7836.c51f.2967.40c7.ae80.7ffc.bfbf.1dd3.a0d5.6b6d.e9a5.b247.985f.0601])
          her-funding-signature=(some [71 0x30.4402.204f.d492.8835.db1c.cdfc.40f5.c78c.e9bd.6524.9b16.348d.f81f.0c44.328d.cdef.c97d.6302.2019.4d38.69c3.8bc7.32dd.87d1.3d29.5801.5e2f.c168.29e7.4cd4.377f.84d2.15c0.b706.0601])
          output-commit-tx=[560 0x200.0000.0001.01be.f67e.4e2f.b9dd.eeb3.4619.73cd.4c62.abb3.5050.b1ad.d772.995b.820b.584a.4884.8900.0000.0000.38b0.2b80.07e8.0300.0000.0000.0022.0020.52bf.ef04.79d7.b293.c27e.0f1e.b294.bea1.54c6.3a32.94ef.092c.19af.5140.9bce.0e2a.d007.0000.0000.0000.2200.2040.3d39.4747.cae4.2e98.ff01.734a.d5c0.8f82.ba12.3d3d.9a62.0abd.a889.8965.1e2a.b5d0.0700.0000.0000.0022.0020.748e.ba94.4fed.c882.7f6b.06bc.4467.8f93.c0f9.e607.8b35.c633.1ed3.1e75.f8ce.0c2d.b80b.0000.0000.0000.2200.20c2.0b5d.1f85.84fd.9044.3e7b.7b72.0136.174f.a4b9.333c.261d.04db.bd01.2635.c0f4.19a0.0f00.0000.0000.0022.0020.8c48.d151.6039.7c97.31df.9bc3.b236.656e.fb66.65fb.fe92.b4a6.878e.88a4.99f7.41c4.c0c6.2d00.0000.0000.1600.14cc.f1af.2f2a.abee.14bb.40fa.3851.ab23.01de.8431.10e0.a06a.0000.0000.0022.0020.4adb.4e2f.0064.3db3.96dd.120d.4e7d.c176.25f5.f2c1.1a40.d857.accc.862d.6b7d.d80e.0400.4730.4402.2027.5b0c.325a.5e93.5565.0dc3.0c0e.ccfb.c7ef.b239.87c2.4b55.6b9d.fdd4.0eff.ca18.d202.206c.aceb.2c06.7836.c51f.2967.40c7.ae80.7ffc.bfbf.1dd3.a0d5.6b6d.e9a5.b247.985f.0601.4730.4402.204f.d492.8835.db1c.cdfc.40f5.c78c.e9bd.6524.9b16.348d.f81f.0c44.328d.cdef.c97d.6302.2019.4d38.69c3.8bc7.32dd.87d1.3d29.5801.5e2f.c168.29e7.4cd4.377f.84d2.15c0.b706.0601.4752.2102.3da0.92f6.980e.58d2.c037.1731.80e9.a465.4760.26ee.50f9.6695.963e.8efe.436f.54eb.2103.0e9f.7b62.3d2c.cc7c.9bd4.4d66.d5ce.21ce.504c.0acf.6385.a132.cec6.d3c3.9fa7.11c1.52ae.3e19.5220]
      ==
      ::
      :*  to-local-msat=6.988.000.000
          to-remote-msat=3.000.000.000
          htlcs=htlc-data
          local-feerate-per-kw=647
          our-funding-signature=(some [72 0x3045.0221.0094.bfd8.f557.2ac0.157e.c76a.9551.b6c5.216a.4538.c07c.d13a.51af.4a54.cb26.fa14.3202.2076.8efc.e8ce.6f4a.5efa.c875.142f.f192.37c0.1134.3670.adf9.c7ac.6970.4a12.0d11.6301])
          her-funding-signature=(some [72 0x3045.0221.00a5.c013.83d3.ec64.6d97.e40f.4431.8d49.def8.17fc.d61a.0ef1.8008.a665.b3e1.5178.5502.203e.648e.fddd.5838.981e.f55e.c954.be69.c4a6.52d0.21e6.081a.100d.034d.e366.815e.9b01])
          output-commit-tx=[562 0x200.0000.0001.01be.f67e.4e2f.b9dd.eeb3.4619.73cd.4c62.abb3.5050.b1ad.d772.995b.820b.584a.4884.8900.0000.0000.38b0.2b80.07e8.0300.0000.0000.0022.0020.52bf.ef04.79d7.b293.c27e.0f1e.b294.bea1.54c6.3a32.94ef.092c.19af.5140.9bce.0e2a.d007.0000.0000.0000.2200.2040.3d39.4747.cae4.2e98.ff01.734a.d5c0.8f82.ba12.3d3d.9a62.0abd.a889.8965.1e2a.b5d0.0700.0000.0000.0022.0020.748e.ba94.4fed.c882.7f6b.06bc.4467.8f93.c0f9.e607.8b35.c633.1ed3.1e75.f8ce.0c2d.b80b.0000.0000.0000.2200.20c2.0b5d.1f85.84fd.9044.3e7b.7b72.0136.174f.a4b9.333c.261d.04db.bd01.2635.c0f4.19a0.0f00.0000.0000.0022.0020.8c48.d151.6039.7c97.31df.9bc3.b236.656e.fb66.65fb.fe92.b4a6.878e.88a4.99f7.41c4.c0c6.2d00.0000.0000.1600.14cc.f1af.2f2a.abee.14bb.40fa.3851.ab23.01de.8431.10e0.9c6a.0000.0000.0022.0020.4adb.4e2f.0064.3db3.96dd.120d.4e7d.c176.25f5.f2c1.1a40.d857.accc.862d.6b7d.d80e.0400.4830.4502.2100.94bf.d8f5.572a.c015.7ec7.6a95.51b6.c521.6a45.38c0.7cd1.3a51.af4a.54cb.26fa.1432.0220.768e.fce8.ce6f.4a5e.fac8.7514.2ff1.9237.c011.3436.70ad.f9c7.ac69.704a.120d.1163.0148.3045.0221.00a5.c013.83d3.ec64.6d97.e40f.4431.8d49.def8.17fc.d61a.0ef1.8008.a665.b3e1.5178.5502.203e.648e.fddd.5838.981e.f55e.c954.be69.c4a6.52d0.21e6.081a.100d.034d.e366.815e.9b01.4752.2102.3da0.92f6.980e.58d2.c037.1731.80e9.a465.4760.26ee.50f9.6695.963e.8efe.436f.54eb.2103.0e9f.7b62.3d2c.cc7c.9bd4.4d66.d5ce.21ce.504c.0acf.6385.a132.cec6.d3c3.9fa7.11c1.52ae.3e19.5220]
      ==
      ::
      :*  to-local-msat=6.988.000.000
          to-remote-msat=3.000.000.000
          htlcs=htlc-data
          local-feerate-per-kw=648
          our-funding-signature=(some [72 0x3045.0221.00a2.270d.5950.c89a.e084.1233.f6ef.ea9c.9518.98b3.01b2.e89e.0adb.d2c6.87b9.f32e.fa02.2079.43d9.0f95.b961.0458.e7c6.5a57.6e14.9750.ff3a.ccaa.cad0.04cd.85e7.0b23.5e27.de01])
          her-funding-signature=(some [71 0x30.4402.2072.714e.2fbb.93cd.d1c4.2eb0.828b.4f2e.ff14.3f71.7d8f.26e7.9d6a.da4f.0dcb.681b.be02.2009.11be.4e51.61dd.6ebe.59ff.1c58.e199.7c4a.ea80.4f81.db6b.6988.21db.6093.d7b0.5701])
          output-commit-tx=[518 0x200.0000.0001.01be.f67e.4e2f.b9dd.eeb3.4619.73cd.4c62.abb3.5050.b1ad.d772.995b.820b.584a.4884.8900.0000.0000.38b0.2b80.06d0.0700.0000.0000.0022.0020.403d.3947.47ca.e42e.98ff.0173.4ad5.c08f.82ba.123d.3d9a.620a.bda8.8989.651e.2ab5.d007.0000.0000.0000.2200.2074.8eba.944f.edc8.827f.6b06.bc44.678f.93c0.f9e6.078b.35c6.331e.d31e.75f8.ce0c.2db8.0b00.0000.0000.0022.0020.c20b.5d1f.8584.fd90.443e.7b7b.7201.3617.4fa4.b933.3c26.1d04.dbbd.0126.35c0.f419.a00f.0000.0000.0000.2200.208c.48d1.5160.397c.9731.df9b.c3b2.3665.6efb.6665.fbfe.92b4.a687.8e88.a499.f741.c4c0.c62d.0000.0000.0016.0014.ccf1.af2f.2aab.ee14.bb40.fa38.51ab.2301.de84.3110.4e9d.6a00.0000.0000.2200.204a.db4e.2f00.643d.b396.dd12.0d4e.7dc1.7625.f5f2.c11a.40d8.57ac.cc86.2d6b.7dd8.0e04.0048.3045.0221.00a2.270d.5950.c89a.e084.1233.f6ef.ea9c.9518.98b3.01b2.e89e.0adb.d2c6.87b9.f32e.fa02.2079.43d9.0f95.b961.0458.e7c6.5a57.6e14.9750.ff3a.ccaa.cad0.04cd.85e7.0b23.5e27.de01.4730.4402.2072.714e.2fbb.93cd.d1c4.2eb0.828b.4f2e.ff14.3f71.7d8f.26e7.9d6a.da4f.0dcb.681b.be02.2009.11be.4e51.61dd.6ebe.59ff.1c58.e199.7c4a.ea80.4f81.db6b.6988.21db.6093.d7b0.5701.4752.2102.3da0.92f6.980e.58d2.c037.1731.80e9.a465.4760.26ee.50f9.6695.963e.8efe.436f.54eb.2103.0e9f.7b62.3d2c.cc7c.9bd4.4d66.d5ce.21ce.504c.0acf.6385.a132.cec6.d3c3.9fa7.11c1.52ae.3e19.5220]
      ==
      ::
      :*  to-local-msat=6.988.000.000
          to-remote-msat=3.000.000.000
          htlcs=htlc-data
          local-feerate-per-kw=2.069
          our-funding-signature=(some [71 0x30.4402.203c.a8f3.1c6a.4751.9f83.255d.c69f.1894.d9a6.d747.6a19.f498.d31e.af0c.d3a8.5eeb.6302.2026.fd92.dc75.2b33.905c.4c83.8c52.8b69.2a8a.d4ce.d959.990b.5d5e.e2ff.940f.a90e.ea01])
          her-funding-signature=(some [71 0x30.4402.2001.d55e.488b.8b03.5b2d.d29d.50b6.5b53.0923.a416.d47f.3772.8414.5bc8.767b.1b6a.7502.2019.bb53.ddfe.1cef.af15.6f92.4777.eaaf.8fdc.a181.0695.a7d0.a247.ad2a.fba8.232e.b401])
          output-commit-tx=[517 0x2.0000.0000.0101.bef6.7e4e.2fb9.ddee.b346.1973.cd4c.62ab.b350.50b1.add7.7299.5b82.0b58.4a48.8489.0000.0000.0038.b02b.8006.d007.0000.0000.0000.2200.2040.3d39.4747.cae4.2e98.ff01.734a.d5c0.8f82.ba12.3d3d.9a62.0abd.a889.8965.1e2a.b5d0.0700.0000.0000.0022.0020.748e.ba94.4fed.c882.7f6b.06bc.4467.8f93.c0f9.e607.8b35.c633.1ed3.1e75.f8ce.0c2d.b80b.0000.0000.0000.2200.20c2.0b5d.1f85.84fd.9044.3e7b.7b72.0136.174f.a4b9.333c.261d.04db.bd01.2635.c0f4.19a0.0f00.0000.0000.0022.0020.8c48.d151.6039.7c97.31df.9bc3.b236.656e.fb66.65fb.fe92.b4a6.878e.88a4.99f7.41c4.c0c6.2d00.0000.0000.1600.14cc.f1af.2f2a.abee.14bb.40fa.3851.ab23.01de.8431.1077.956a.0000.0000.0022.0020.4adb.4e2f.0064.3db3.96dd.120d.4e7d.c176.25f5.f2c1.1a40.d857.accc.862d.6b7d.d80e.0400.4730.4402.203c.a8f3.1c6a.4751.9f83.255d.c69f.1894.d9a6.d747.6a19.f498.d31e.af0c.d3a8.5eeb.6302.2026.fd92.dc75.2b33.905c.4c83.8c52.8b69.2a8a.d4ce.d959.990b.5d5e.e2ff.940f.a90e.ea01.4730.4402.2001.d55e.488b.8b03.5b2d.d29d.50b6.5b53.0923.a416.d47f.3772.8414.5bc8.767b.1b6a.7502.2019.bb53.ddfe.1cef.af15.6f92.4777.eaaf.8fdc.a181.0695.a7d0.a247.ad2a.fba8.232e.b401.4752.2102.3da0.92f6.980e.58d2.c037.1731.80e9.a465.4760.26ee.50f9.6695.963e.8efe.436f.54eb.2103.0e9f.7b62.3d2c.cc7c.9bd4.4d66.d5ce.21ce.504c.0acf.6385.a132.cec6.d3c3.9fa7.11c1.52ae.3e19.5220]
      ==
      ::
      :*  to-local-msat=6.988.000.000
          to-remote-msat=3.000.000.000
          htlcs=htlc-data
          local-feerate-per-kw=2.070
          our-funding-signature=(some [71 0x30.4402.2044.3cb0.7f65.0aeb.bba1.4b8b.c8d8.1e09.6712.590f.524c.5991.ac0e.d3bb.c8fd.3bd0.c702.2028.a635.f548.e3ca.64b1.9b69.b1ea.00f0.5b22.752f.91da.f0b6.dab7.8e62.ba52.eb7f.d001])
          her-funding-signature=(some [72 0x3045.0221.00f2.377f.7a67.b7fc.7f4e.2c0c.9e3a.7de9.35c3.2417.f566.8eda.31ea.1db4.01b7.dc53.0302.2041.5fdb.c8e9.1d0f.735e.70c2.1952.3427.42e2.5249.b0d0.62d4.3efb.fc56.4499.f375.2601])
          output-commit-tx=[475 0x2.0000.0000.0101.bef6.7e4e.2fb9.ddee.b346.1973.cd4c.62ab.b350.50b1.add7.7299.5b82.0b58.4a48.8489.0000.0000.0038.b02b.8005.d007.0000.0000.0000.2200.2040.3d39.4747.cae4.2e98.ff01.734a.d5c0.8f82.ba12.3d3d.9a62.0abd.a889.8965.1e2a.b5b8.0b00.0000.0000.0022.0020.c20b.5d1f.8584.fd90.443e.7b7b.7201.3617.4fa4.b933.3c26.1d04.dbbd.0126.35c0.f419.a00f.0000.0000.0000.2200.208c.48d1.5160.397c.9731.df9b.c3b2.3665.6efb.6665.fbfe.92b4.a687.8e88.a499.f741.c4c0.c62d.0000.0000.0016.0014.ccf1.af2f.2aab.ee14.bb40.fa38.51ab.2301.de84.3110.da96.6a00.0000.0000.2200.204a.db4e.2f00.643d.b396.dd12.0d4e.7dc1.7625.f5f2.c11a.40d8.57ac.cc86.2d6b.7dd8.0e04.0047.3044.0220.443c.b07f.650a.ebbb.a14b.8bc8.d81e.0967.1259.0f52.4c59.91ac.0ed3.bbc8.fd3b.d0c7.0220.28a6.35f5.48e3.ca64.b19b.69b1.ea00.f05b.2275.2f91.daf0.b6da.b78e.62ba.52eb.7fd0.0148.3045.0221.00f2.377f.7a67.b7fc.7f4e.2c0c.9e3a.7de9.35c3.2417.f566.8eda.31ea.1db4.01b7.dc53.0302.2041.5fdb.c8e9.1d0f.735e.70c2.1952.3427.42e2.5249.b0d0.62d4.3efb.fc56.4499.f375.2601.4752.2102.3da0.92f6.980e.58d2.c037.1731.80e9.a465.4760.26ee.50f9.6695.963e.8efe.436f.54eb.2103.0e9f.7b62.3d2c.cc7c.9bd4.4d66.d5ce.21ce.504c.0acf.6385.a132.cec6.d3c3.9fa7.11c1.52ae.3e19.5220]
      ==
      ::
      :*  to-local-msat=6.988.000.000
          to-remote-msat=3.000.000.000
          htlcs=htlc-data
          local-feerate-per-kw=2.194
          our-funding-signature=(some [71 0x30.4402.203b.1b01.0c10.9c2e.cbe7.feb2.d259.b9c4.126b.d5dc.99ee.693c.422e.c0a5.781f.e161.ba02.2057.1fe4.e2c6.49de.a9c7.aaf7.e49b.3829.62f6.a349.4963.c97d.80fe.f9a4.30ca.3f70.6101])
          her-funding-signature=(some [72 0x3045.0221.00d3.3c4e.541a.a1d2.55d4.1ea9.a3b4.43b3.b822.ad8f.7f86.8626.38aa.c1f6.9f8f.7605.7702.2007.e2a1.8e69.31ce.3d3a.804b.1c78.eda1.de17.dbe1.fb7a.9548.8c9a.4ec8.6203.9533.4801])
          output-commit-tx=[475 0x2.0000.0000.0101.bef6.7e4e.2fb9.ddee.b346.1973.cd4c.62ab.b350.50b1.add7.7299.5b82.0b58.4a48.8489.0000.0000.0038.b02b.8005.d007.0000.0000.0000.2200.2040.3d39.4747.cae4.2e98.ff01.734a.d5c0.8f82.ba12.3d3d.9a62.0abd.a889.8965.1e2a.b5b8.0b00.0000.0000.0022.0020.c20b.5d1f.8584.fd90.443e.7b7b.7201.3617.4fa4.b933.3c26.1d04.dbbd.0126.35c0.f419.a00f.0000.0000.0000.2200.208c.48d1.5160.397c.9731.df9b.c3b2.3665.6efb.6665.fbfe.92b4.a687.8e88.a499.f741.c4c0.c62d.0000.0000.0016.0014.ccf1.af2f.2aab.ee14.bb40.fa38.51ab.2301.de84.3110.4096.6a00.0000.0000.2200.204a.db4e.2f00.643d.b396.dd12.0d4e.7dc1.7625.f5f2.c11a.40d8.57ac.cc86.2d6b.7dd8.0e04.0047.3044.0220.3b1b.010c.109c.2ecb.e7fe.b2d2.59b9.c412.6bd5.dc99.ee69.3c42.2ec0.a578.1fe1.61ba.0220.571f.e4e2.c649.dea9.c7aa.f7e4.9b38.2962.f6a3.4949.63c9.7d80.fef9.a430.ca3f.7061.0148.3045.0221.00d3.3c4e.541a.a1d2.55d4.1ea9.a3b4.43b3.b822.ad8f.7f86.8626.38aa.c1f6.9f8f.7605.7702.2007.e2a1.8e69.31ce.3d3a.804b.1c78.eda1.de17.dbe1.fb7a.9548.8c9a.4ec8.6203.9533.4801.4752.2102.3da0.92f6.980e.58d2.c037.1731.80e9.a465.4760.26ee.50f9.6695.963e.8efe.436f.54eb.2103.0e9f.7b62.3d2c.cc7c.9bd4.4d66.d5ce.21ce.504c.0acf.6385.a132.cec6.d3c3.9fa7.11c1.52ae.3e19.5220]
      ==
      ::
      :*  to-local-msat=6.988.000.000
          to-remote-msat=3.000.000.000
          htlcs=htlc-data
          local-feerate-per-kw=2.195
          our-funding-signature=(some [71 0x30.4402.203b.12d4.4254.244b.8ff3.bb41.29b0.920f.d451.20ab.42f5.53d9.9763.94b0.99d5.00c9.9e02.205e.95bb.7a31.6485.2ef0.c48f.9e0e.af14.5218.f8e2.c412.51b2.31f0.3cbd.c4f2.9a54.2901])
          her-funding-signature=(some [71 0x30.4402.205e.2f76.d465.7fb7.32c0.dfc8.20a1.8a73.01e3.68f5.799e.06b7.8280.0763.3741.bda6.df02.2045.8009.ae59.d0c6.2460.65c4.1935.9e05.eb2a.4b4e.f4a1.b310.cc91.2db4.4eb7.9242.9801])
          output-commit-tx=[431 0x2.0000.0000.0101.bef6.7e4e.2fb9.ddee.b346.1973.cd4c.62ab.b350.50b1.add7.7299.5b82.0b58.4a48.8489.0000.0000.0038.b02b.8004.b80b.0000.0000.0000.2200.20c2.0b5d.1f85.84fd.9044.3e7b.7b72.0136.174f.a4b9.333c.261d.04db.bd01.2635.c0f4.19a0.0f00.0000.0000.0022.0020.8c48.d151.6039.7c97.31df.9bc3.b236.656e.fb66.65fb.fe92.b4a6.878e.88a4.99f7.41c4.c0c6.2d00.0000.0000.1600.14cc.f1af.2f2a.abee.14bb.40fa.3851.ab23.01de.8431.10b8.976a.0000.0000.0022.0020.4adb.4e2f.0064.3db3.96dd.120d.4e7d.c176.25f5.f2c1.1a40.d857.accc.862d.6b7d.d80e.0400.4730.4402.203b.12d4.4254.244b.8ff3.bb41.29b0.920f.d451.20ab.42f5.53d9.9763.94b0.99d5.00c9.9e02.205e.95bb.7a31.6485.2ef0.c48f.9e0e.af14.5218.f8e2.c412.51b2.31f0.3cbd.c4f2.9a54.2901.4730.4402.205e.2f76.d465.7fb7.32c0.dfc8.20a1.8a73.01e3.68f5.799e.06b7.8280.0763.3741.bda6.df02.2045.8009.ae59.d0c6.2460.65c4.1935.9e05.eb2a.4b4e.f4a1.b310.cc91.2db4.4eb7.9242.9801.4752.2102.3da0.92f6.980e.58d2.c037.1731.80e9.a465.4760.26ee.50f9.6695.963e.8efe.436f.54eb.2103.0e9f.7b62.3d2c.cc7c.9bd4.4d66.d5ce.21ce.504c.0acf.6385.a132.cec6.d3c3.9fa7.11c1.52ae.3e19.5220]
      ==
      ::
      :*  to-local-msat=6.988.000.000
          to-remote-msat=3.000.000.000
          htlcs=htlc-data
          local-feerate-per-kw=3.702
          our-funding-signature=(some [71 0x30.4402.200e.930a.43c7.9511.62dc.15a2.b734.4f48.091c.74c7.0f70.24e7.116e.900d.8bcf.ba86.1c02.2066.fa6c.bda3.929e.21da.a2e7.e16a.4b94.8db7.e891.9ef9.7840.2360.d109.5ffd.aff7.b001])
          her-funding-signature=(some [72 0x3045.0221.00c1.a3b0.b60c.a092.ed50.8012.1f26.a74a.20ce.c6bd.ee3f.8e47.bae9.73fc.dceb.3eda.5502.207d.467a.9873.c939.bf3a.a758.014a.e672.95fe.dbca.5241.2633.f7e5.b267.0fc7.c381.c101])
          output-commit-tx=[432 0x200.0000.0001.01be.f67e.4e2f.b9dd.eeb3.4619.73cd.4c62.abb3.5050.b1ad.d772.995b.820b.584a.4884.8900.0000.0000.38b0.2b80.04b8.0b00.0000.0000.0022.0020.c20b.5d1f.8584.fd90.443e.7b7b.7201.3617.4fa4.b933.3c26.1d04.dbbd.0126.35c0.f419.a00f.0000.0000.0000.2200.208c.48d1.5160.397c.9731.df9b.c3b2.3665.6efb.6665.fbfe.92b4.a687.8e88.a499.f741.c4c0.c62d.0000.0000.0016.0014.ccf1.af2f.2aab.ee14.bb40.fa38.51ab.2301.de84.3110.6f91.6a00.0000.0000.2200.204a.db4e.2f00.643d.b396.dd12.0d4e.7dc1.7625.f5f2.c11a.40d8.57ac.cc86.2d6b.7dd8.0e04.0047.3044.0220.0e93.0a43.c795.1162.dc15.a2b7.344f.4809.1c74.c70f.7024.e711.6e90.0d8b.cfba.861c.0220.66fa.6cbd.a392.9e21.daa2.e7e1.6a4b.948d.b7e8.919e.f978.4023.60d1.095f.fdaf.f7b0.0148.3045.0221.00c1.a3b0.b60c.a092.ed50.8012.1f26.a74a.20ce.c6bd.ee3f.8e47.bae9.73fc.dceb.3eda.5502.207d.467a.9873.c939.bf3a.a758.014a.e672.95fe.dbca.5241.2633.f7e5.b267.0fc7.c381.c101.4752.2102.3da0.92f6.980e.58d2.c037.1731.80e9.a465.4760.26ee.50f9.6695.963e.8efe.436f.54eb.2103.0e9f.7b62.3d2c.cc7c.9bd4.4d66.d5ce.21ce.504c.0acf.6385.a132.cec6.d3c3.9fa7.11c1.52ae.3e19.5220]
      ==
      ::
      :*  to-local-msat=6.988.000.000
          to-remote-msat=3.000.000.000
          htlcs=htlc-data
          local-feerate-per-kw=3.703
          our-funding-signature=(some [71 0x30.4402.2047.3055.31dd.4439.1dce.03ae.20f8.7350.05c6.15eb.077a.974e.db00.59ea.1a31.1857.d602.202e.0ed6.972f.bdd1.e8cb.542b.06e0.929b.c41b.2ddf.236e.04cb.75ed.d561.51f4.1975.0601])
          her-funding-signature=(some [72 0x3045.0221.008b.7c19.1dd4.6893.b67b.628e.618d.2dc8.e811.69d3.8bad.e310.181a.b77d.7c94.c667.5e02.203b.4dd1.31fd.7c9d.eb29.9560.983d.cdc4.8554.5c98.f989.f7ae.8180.c282.89f9.e6bd.b001])
          output-commit-tx=[389 0x2.0000.0000.0101.bef6.7e4e.2fb9.ddee.b346.1973.cd4c.62ab.b350.50b1.add7.7299.5b82.0b58.4a48.8489.0000.0000.0038.b02b.8003.a00f.0000.0000.0000.2200.208c.48d1.5160.397c.9731.df9b.c3b2.3665.6efb.6665.fbfe.92b4.a687.8e88.a499.f741.c4c0.c62d.0000.0000.0016.0014.ccf1.af2f.2aab.ee14.bb40.fa38.51ab.2301.de84.3110.eb93.6a00.0000.0000.2200.204a.db4e.2f00.643d.b396.dd12.0d4e.7dc1.7625.f5f2.c11a.40d8.57ac.cc86.2d6b.7dd8.0e04.0047.3044.0220.4730.5531.dd44.391d.ce03.ae20.f873.5005.c615.eb07.7a97.4edb.0059.ea1a.3118.57d6.0220.2e0e.d697.2fbd.d1e8.cb54.2b06.e092.9bc4.1b2d.df23.6e04.cb75.edd5.6151.f419.7506.0148.3045.0221.008b.7c19.1dd4.6893.b67b.628e.618d.2dc8.e811.69d3.8bad.e310.181a.b77d.7c94.c667.5e02.203b.4dd1.31fd.7c9d.eb29.9560.983d.cdc4.8554.5c98.f989.f7ae.8180.c282.89f9.e6bd.b001.4752.2102.3da0.92f6.980e.58d2.c037.1731.80e9.a465.4760.26ee.50f9.6695.963e.8efe.436f.54eb.2103.0e9f.7b62.3d2c.cc7c.9bd4.4d66.d5ce.21ce.504c.0acf.6385.a132.cec6.d3c3.9fa7.11c1.52ae.3e19.5220]
      ==
      ::
      :*  to-local-msat=6.988.000.000
          to-remote-msat=3.000.000.000
          htlcs=htlc-data
          local-feerate-per-kw=4.914
          our-funding-signature=(some [71 0x30.4402.206a.2679.efa3.c7aa.ffd2.a447.fd0d.f7ab.a879.2858.b589.750f.6a12.03f9.2591.7319.8a02.2008.d52a.0e77.a99a.b533.c362.06cb.15ad.7aeb.2aa7.2b93.d4b5.71e7.28cb.5ec2.f6fe.2601])
          her-funding-signature=(some [71 0x30.4402.206d.6cb9.3969.d391.77a0.9d5d.45b5.83f3.4966.195b.77c7.e585.cf47.ac5c.ce0c.90ce.fb02.2031.d71a.e4e3.3a4e.80df.7f98.1d69.6fbd.ee51.7337.806a.3c71.38b7.491e.2cbb.077a.0e01])
          output-commit-tx=[388 0x200.0000.0001.01be.f67e.4e2f.b9dd.eeb3.4619.73cd.4c62.abb3.5050.b1ad.d772.995b.820b.584a.4884.8900.0000.0000.38b0.2b80.03a0.0f00.0000.0000.0022.0020.8c48.d151.6039.7c97.31df.9bc3.b236.656e.fb66.65fb.fe92.b4a6.878e.88a4.99f7.41c4.c0c6.2d00.0000.0000.1600.14cc.f1af.2f2a.abee.14bb.40fa.3851.ab23.01de.8431.10ae.8f6a.0000.0000.0022.0020.4adb.4e2f.0064.3db3.96dd.120d.4e7d.c176.25f5.f2c1.1a40.d857.accc.862d.6b7d.d80e.0400.4730.4402.206a.2679.efa3.c7aa.ffd2.a447.fd0d.f7ab.a879.2858.b589.750f.6a12.03f9.2591.7319.8a02.2008.d52a.0e77.a99a.b533.c362.06cb.15ad.7aeb.2aa7.2b93.d4b5.71e7.28cb.5ec2.f6fe.2601.4730.4402.206d.6cb9.3969.d391.77a0.9d5d.45b5.83f3.4966.195b.77c7.e585.cf47.ac5c.ce0c.90ce.fb02.2031.d71a.e4e3.3a4e.80df.7f98.1d69.6fbd.ee51.7337.806a.3c71.38b7.491e.2cbb.077a.0e01.4752.2102.3da0.92f6.980e.58d2.c037.1731.80e9.a465.4760.26ee.50f9.6695.963e.8efe.436f.54eb.2103.0e9f.7b62.3d2c.cc7c.9bd4.4d66.d5ce.21ce.504c.0acf.6385.a132.cec6.d3c3.9fa7.11c1.52ae.3e19.5220]
      ==
      ::
      :*  to-local-msat=6.988.000.000
          to-remote-msat=3.000.000.000
          htlcs=htlc-data
          local-feerate-per-kw=4.915
          our-funding-signature=(some [72 0x3045.0221.00a0.1269.1ba6.cea2.f73f.a8ba.c377.5047.7e66.363c.6d28.813b.0bb6.da77.c8eb.3fb0.2702.2036.5e99.c513.04b0.b1a6.ab9e.a1c8.500d.b186.693e.39ec.1ad5.743e.e231.b013.8384.b901])
          her-funding-signature=(some [71 0x30.4402.2007.69ba.89c7.330d.fa4f.eba4.47b6.e322.305f.12ac.7dac.70ec.6ba9.97ed.7c1b.598d.0802.204f.e8d3.37e7.fee7.81f9.b7b1.a06e.580b.22f4.f79d.7400.5956.0191.d7db.53f8.7655.5201])
          output-commit-tx=[346 0x200.0000.0001.01be.f67e.4e2f.b9dd.eeb3.4619.73cd.4c62.abb3.5050.b1ad.d772.995b.820b.584a.4884.8900.0000.0000.38b0.2b80.02c0.c62d.0000.0000.0016.0014.ccf1.af2f.2aab.ee14.bb40.fa38.51ab.2301.de84.3110.fa92.6a00.0000.0000.2200.204a.db4e.2f00.643d.b396.dd12.0d4e.7dc1.7625.f5f2.c11a.40d8.57ac.cc86.2d6b.7dd8.0e04.0048.3045.0221.00a0.1269.1ba6.cea2.f73f.a8ba.c377.5047.7e66.363c.6d28.813b.0bb6.da77.c8eb.3fb0.2702.2036.5e99.c513.04b0.b1a6.ab9e.a1c8.500d.b186.693e.39ec.1ad5.743e.e231.b013.8384.b901.4730.4402.2007.69ba.89c7.330d.fa4f.eba4.47b6.e322.305f.12ac.7dac.70ec.6ba9.97ed.7c1b.598d.0802.204f.e8d3.37e7.fee7.81f9.b7b1.a06e.580b.22f4.f79d.7400.5956.0191.d7db.53f8.7655.5201.4752.2102.3da0.92f6.980e.58d2.c037.1731.80e9.a465.4760.26ee.50f9.6695.963e.8efe.436f.54eb.2103.0e9f.7b62.3d2c.cc7c.9bd4.4d66.d5ce.21ce.504c.0acf.6385.a132.cec6.d3c3.9fa7.11c1.52ae.3e19.5220]
      ==
      ::
      :*  to-local-msat=6.988.000.000
          to-remote-msat=3.000.000.000
          htlcs=htlc-data
          local-feerate-per-kw=9.651.180
          our-funding-signature=(some [71 0x30.4402.2051.4f97.7bf7.edc4.42de.8ce4.3ace.9686.e5eb.dc0f.8930.33f1.3e40.fb46.c8b8.c6e1.f902.2018.8006.227d.175f.5c35.da0b.092c.57be.a825.37ae.d89f.7778.204d.c5ba.cf4f.29f2.b901])
          her-funding-signature=(some [71 0x30.4402.2037.f83f.f00c.8e5f.b18a.e1f9.18ff.c24e.5458.1775.a20f.f1ae.7192.97ef.066c.71ca.a902.2039.c529.cccd.89ff.6c5e.d1db.7996.1453.3844.bd6d.101d.a503.761c.45c7.1399.6e3b.bd01])
          output-commit-tx=[345 0x2.0000.0000.0101.bef6.7e4e.2fb9.ddee.b346.1973.cd4c.62ab.b350.50b1.add7.7299.5b82.0b58.4a48.8489.0000.0000.0038.b02b.8002.2202.0000.0000.0000.2200.204a.db4e.2f00.643d.b396.dd12.0d4e.7dc1.7625.f5f2.c11a.40d8.57ac.cc86.2d6b.7dd8.0ec0.c62d.0000.0000.0016.0014.ccf1.af2f.2aab.ee14.bb40.fa38.51ab.2301.de84.3110.0400.4730.4402.2051.4f97.7bf7.edc4.42de.8ce4.3ace.9686.e5eb.dc0f.8930.33f1.3e40.fb46.c8b8.c6e1.f902.2018.8006.227d.175f.5c35.da0b.092c.57be.a825.37ae.d89f.7778.204d.c5ba.cf4f.29f2.b901.4730.4402.2037.f83f.f00c.8e5f.b18a.e1f9.18ff.c24e.5458.1775.a20f.f1ae.7192.97ef.066c.71ca.a902.2039.c529.cccd.89ff.6c5e.d1db.7996.1453.3844.bd6d.101d.a503.761c.45c7.1399.6e3b.bd01.4752.2102.3da0.92f6.980e.58d2.c037.1731.80e9.a465.4760.26ee.50f9.6695.963e.8efe.436f.54eb.2103.0e9f.7b62.3d2c.cc7c.9bd4.4d66.d5ce.21ce.504c.0acf.6385.a132.cec6.d3c3.9fa7.11c1.52ae.3e19.5220]
      ==
      ::
      :*  to-local-msat=6.988.000.000
          to-remote-msat=3.000.000.000
          htlcs=htlc-data
          local-feerate-per-kw=9.651.181
          our-funding-signature=(some [71 0x30.4402.2031.a82b.51bd.0149.15fe.6892.8d1a.bf4b.9885.353f.b896.cac1.0c3f.dd88.d7f9.c7f2.e002.2071.6bda.8196.41d2.c63e.65d3.549b.6120.112e.1aea.f174.2eed.94a4.7148.8e79.e206.b101])
          her-funding-signature=(some [71 0x30.4402.2064.9019.50be.922e.62cb.e3f2.ab93.de2b.99f3.7cff.9fc4.73e7.3e39.4b27.f88e.f073.1d02.206d.1dfa.2275.27b4.df44.a075.9928.9e20.7d6f.d9cc.a60c.0365.682d.cd3d.eaf7.3956.7e01])
          output-commit-tx=[302 0x200.0000.0001.01be.f67e.4e2f.b9dd.eeb3.4619.73cd.4c62.abb3.5050.b1ad.d772.995b.820b.584a.4884.8900.0000.0000.38b0.2b80.01c0.c62d.0000.0000.0016.0014.ccf1.af2f.2aab.ee14.bb40.fa38.51ab.2301.de84.3110.0400.4730.4402.2031.a82b.51bd.0149.15fe.6892.8d1a.bf4b.9885.353f.b896.cac1.0c3f.dd88.d7f9.c7f2.e002.2071.6bda.8196.41d2.c63e.65d3.549b.6120.112e.1aea.f174.2eed.94a4.7148.8e79.e206.b101.4730.4402.2064.9019.50be.922e.62cb.e3f2.ab93.de2b.99f3.7cff.9fc4.73e7.3e39.4b27.f88e.f073.1d02.206d.1dfa.2275.27b4.df44.a075.9928.9e20.7d6f.d9cc.a60c.0365.682d.cd3d.eaf7.3956.7e01.4752.2102.3da0.92f6.980e.58d2.c037.1731.80e9.a465.4760.26ee.50f9.6695.963e.8efe.436f.54eb.2103.0e9f.7b62.3d2c.cc7c.9bd4.4d66.d5ce.21ce.504c.0acf.6385.a132.cec6.d3c3.9fa7.11c1.52ae.3e19.5220]
      ==
      ::
      :*  to-local-msat=6.988.000.000
          to-remote-msat=3.000.000.000
          htlcs=htlc-data
          local-feerate-per-kw=9.651.936
          our-funding-signature=(some [71 0x30.4402.2031.a82b.51bd.0149.15fe.6892.8d1a.bf4b.9885.353f.b896.cac1.0c3f.dd88.d7f9.c7f2.e002.2071.6bda.8196.41d2.c63e.65d3.549b.6120.112e.1aea.f174.2eed.94a4.7148.8e79.e206.b101])
          her-funding-signature=(some [71 0x30.4402.2064.9019.50be.922e.62cb.e3f2.ab93.de2b.99f3.7cff.9fc4.73e7.3e39.4b27.f88e.f073.1d02.206d.1dfa.2275.27b4.df44.a075.9928.9e20.7d6f.d9cc.a60c.0365.682d.cd3d.eaf7.3956.7e01])
          output-commit-tx=[302 0x200.0000.0001.01be.f67e.4e2f.b9dd.eeb3.4619.73cd.4c62.abb3.5050.b1ad.d772.995b.820b.584a.4884.8900.0000.0000.38b0.2b80.01c0.c62d.0000.0000.0016.0014.ccf1.af2f.2aab.ee14.bb40.fa38.51ab.2301.de84.3110.0400.4730.4402.2031.a82b.51bd.0149.15fe.6892.8d1a.bf4b.9885.353f.b896.cac1.0c3f.dd88.d7f9.c7f2.e002.2071.6bda.8196.41d2.c63e.65d3.549b.6120.112e.1aea.f174.2eed.94a4.7148.8e79.e206.b101.4730.4402.2064.9019.50be.922e.62cb.e3f2.ab93.de2b.99f3.7cff.9fc4.73e7.3e39.4b27.f88e.f073.1d02.206d.1dfa.2275.27b4.df44.a075.9928.9e20.7d6f.d9cc.a60c.0365.682d.cd3d.eaf7.3956.7e01.4752.2102.3da0.92f6.980e.58d2.c037.1731.80e9.a465.4760.26ee.50f9.6695.963e.8efe.436f.54eb.2103.0e9f.7b62.3d2c.cc7c.9bd4.4d66.d5ce.21ce.504c.0acf.6385.a132.cec6.d3c3.9fa7.11c1.52ae.3e19.5220]
      ==
      ::
      :*  to-local-msat=6.988.000.000
          to-remote-msat=3.000.000.000
          htlcs=htlc-data-2
          local-feerate-per-kw=253
          our-funding-signature=(some [72 0x3045.0221.0098.6746.86a1.3c7d.a2d9.5abe.a08d.27e9.3245.7315.6d79.e5eb.08cd.96d1.e33b.b004.5002.2063.9121.6f4f.d5fb.7b0f.e8c4.3074.fd19.f485.dd47.d7b0.7f73.25c5.a612.1f5b.0a59.1b01])
          her-funding-signature=(some [71 0x30.4402.2044.f807.aefa.4148.0a5d.1df2.fc31.2c48.6900.617f.d244.93cf.4197.6428.cb24.9ec2.c202.2007.fd12.29cf.b57d.638b.9c13.7b03.bc79.17d0.e418.b63e.62a3.642f.1c13.354c.c71d.f801])
          output-commit-tx=[475 0x2.0000.0000.0101.bef6.7e4e.2fb9.ddee.b346.1973.cd4c.62ab.b350.50b1.add7.7299.5b82.0b58.4a48.8489.0000.0000.0038.b02b.8005.d007.0000.0000.0000.2200.2074.8eba.944f.edc8.827f.6b06.bc44.678f.93c0.f9e6.078b.35c6.331e.d31e.75f8.ce0c.2d88.1300.0000.0000.0022.0020.305c.12e1.a0bc.21e2.83c1.31ce.a1c6.6d68.857d.28b7.b2fc.e0a6.fbc4.0c16.4852.121b.8813.0000.0000.0000.2200.2030.5c12.e1a0.bc21.e283.c131.cea1.c66d.6885.7d28.b7b2.fce0.a6fb.c40c.1648.5212.1bc0.c62d.0000.0000.0016.0014.ccf1.af2f.2aab.ee14.bb40.fa38.51ab.2301.de84.3110.a79f.6a00.0000.0000.2200.204a.db4e.2f00.643d.b396.dd12.0d4e.7dc1.7625.f5f2.c11a.40d8.57ac.cc86.2d6b.7dd8.0e04.0048.3045.0221.0098.6746.86a1.3c7d.a2d9.5abe.a08d.27e9.3245.7315.6d79.e5eb.08cd.96d1.e33b.b004.5002.2063.9121.6f4f.d5fb.7b0f.e8c4.3074.fd19.f485.dd47.d7b0.7f73.25c5.a612.1f5b.0a59.1b01.4730.4402.2044.f807.aefa.4148.0a5d.1df2.fc31.2c48.6900.617f.d244.93cf.4197.6428.cb24.9ec2.c202.2007.fd12.29cf.b57d.638b.9c13.7b03.bc79.17d0.e418.b63e.62a3.642f.1c13.354c.c71d.f801.4752.2102.3da0.92f6.980e.58d2.c037.1731.80e9.a465.4760.26ee.50f9.6695.963e.8efe.436f.54eb.2103.0e9f.7b62.3d2c.cc7c.9bd4.4d66.d5ce.21ce.504c.0acf.6385.a132.cec6.d3c3.9fa7.11c1.52ae.3e19.5220]
      ==
    ==
  ::
  ++  anchor-tx-test-vectors
    ^-  (list test-vector:tx-test)
    :~
      :*  to-local-msat=7.000.000.000
          to-remote-msat=3.000.000.000
          htlcs=~
          local-feerate-per-kw=15.000
          our-funding-signature=(some [72 0x3045.0221.0082.66ac.6db5.ea71.aac3.c95d.97b0.e172.ff59.6844.851a.3216.eb88.382a.8ddd.fd33.d202.2050.e240.974c.fd5d.7087.08b4.3655.7451.7c18.e7ae.535e.f732.a348.4d43.d0d8.2be9.f701])
          her-funding-signature=(some [72 0x3045.0221.00f8.9034.eba1.6b2b.e0e5.581f.750a.0a63.0919.2b75.cce0.f202.f0ee.2b4e.c0cc.3948.5002.2076.c65d.c507.fe42.2761.52b7.a3d9.0e96.1e67.8adb.e966.e916.ecfe.85e6.4d43.0e75.f301])
          output-commit-tx=[445 0x2.0000.0000.0101.bef6.7e4e.2fb9.ddee.b346.1973.cd4c.62ab.b350.50b1.add7.7299.5b82.0b58.4a48.8489.0000.0000.0038.b02b.8004.4a01.0000.0000.0000.2200.202b.1b58.5418.3c12.d331.6565.972c.4668.929d.314d.81c5.dcdb.b21c.b45f.e8a9.a811.4f4a.0100.0000.0000.0022.0020.e9e8.6e48.23fa.a62e.222e.bc85.8a22.6636.8561.58f0.7e69.898d.a3b0.d1af.0ddb.3994.c0c6.2d00.0000.0000.2200.20f3.394e.1e61.9b0e.ca1f.91be.2fb5.ab4d.fc59.ba5b.84eb.e014.ad1d.43a5.64d0.1299.4a50.8b6a.0000.0000.0022.0020.4adb.4e2f.0064.3db3.96dd.120d.4e7d.c176.25f5.f2c1.1a40.d857.accc.862d.6b7d.d80e.0400.4830.4502.2100.8266.ac6d.b5ea.71aa.c3c9.5d97.b0e1.72ff.5968.4485.1a32.16eb.8838.2a8d.ddfd.33d2.0220.50e2.4097.4cfd.5d70.8708.b436.5574.517c.18e7.ae53.5ef7.32a3.484d.43d0.d82b.e9f7.0148.3045.0221.00f8.9034.eba1.6b2b.e0e5.581f.750a.0a63.0919.2b75.cce0.f202.f0ee.2b4e.c0cc.3948.5002.2076.c65d.c507.fe42.2761.52b7.a3d9.0e96.1e67.8adb.e966.e916.ecfe.85e6.4d43.0e75.f301.4752.2102.3da0.92f6.980e.58d2.c037.1731.80e9.a465.4760.26ee.50f9.6695.963e.8efe.436f.54eb.2103.0e9f.7b62.3d2c.cc7c.9bd4.4d66.d5ce.21ce.504c.0acf.6385.a132.cec6.d3c3.9fa7.11c1.52ae.3e19.5220]
      ==
      ::
      :*  to-local-msat=7.000.000.000
          to-remote-msat=0
          htlcs=~
          local-feerate-per-kw=15.000
          our-funding-signature=(some [72 0x3045.0221.00da.5310.620e.72bc.23dc.57af.25d1.8102.cc75.479a.ea02.58ab.89fe.1a66.ca17.6033.ec02.2033.9efb.450c.1287.2e13.4c8b.da98.6bb9.2f3e.4eeb.caa2.d0fe.e5d9.a2b1.257d.12f1.2a01])
          her-funding-signature=(some [71 0x30.4402.200d.c305.42c9.b8b2.ff4b.8d98.f467.98b3.218a.088a.07e9.7b9e.7861.7728.7dc6.a534.7b02.203d.23b1.c2bf.1726.2362.fdb4.cdcc.36db.b449.a9ef.cdb1.0051.ad52.cfa0.9fc7.6842.b001])
          output-commit-tx=[358 0x200.0000.0001.01be.f67e.4e2f.b9dd.eeb3.4619.73cd.4c62.abb3.5050.b1ad.d772.995b.820b.584a.4884.8900.0000.0000.38b0.2b80.024a.0100.0000.0000.0022.0020.2b1b.5854.183c.12d3.3165.6597.2c46.6892.9d31.4d81.c5dc.dbb2.1cb4.5fe8.a9a8.114f.508b.6a00.0000.0000.2200.204a.db4e.2f00.643d.b396.dd12.0d4e.7dc1.7625.f5f2.c11a.40d8.57ac.cc86.2d6b.7dd8.0e04.0048.3045.0221.00da.5310.620e.72bc.23dc.57af.25d1.8102.cc75.479a.ea02.58ab.89fe.1a66.ca17.6033.ec02.2033.9efb.450c.1287.2e13.4c8b.da98.6bb9.2f3e.4eeb.caa2.d0fe.e5d9.a2b1.257d.12f1.2a01.4730.4402.200d.c305.42c9.b8b2.ff4b.8d98.f467.98b3.218a.088a.07e9.7b9e.7861.7728.7dc6.a534.7b02.203d.23b1.c2bf.1726.2362.fdb4.cdcc.36db.b449.a9ef.cdb1.0051.ad52.cfa0.9fc7.6842.b001.4752.2102.3da0.92f6.980e.58d2.c037.1731.80e9.a465.4760.26ee.50f9.6695.963e.8efe.436f.54eb.2103.0e9f.7b62.3d2c.cc7c.9bd4.4d66.d5ce.21ce.504c.0acf.6385.a132.cec6.d3c3.9fa7.11c1.52ae.3e19.5220]
      ==
      ::
      :*  to-local-msat=6.988.000.000
          to-remote-msat=3.000.000.000
          htlcs=htlc-data
          local-feerate-per-kw=644
          our-funding-signature=(some [72 0x3045.0221.00ef.82a4.0536.4bfc.4007.e63a.7cc8.2925.a513.d790.65bd.bc21.6d60.b6a4.223a.323f.8a02.2007.1673.0b85.61f3.c6d3.62ea.f47f.202e.99fb.30d0.557b.61b9.2b5f.9134.f8e2.de36.8101])
          her-funding-signature=(some [72 0x3045.0221.00e0.1068.3046.7a55.8c07.544a.3de7.7156.10c1.1470.62e7.d091.deee.be8b.5c66.1cda.9402.202a.d049.c1a6.d048.3431.7a78.483f.723c.205c.9f63.8d17.222a.afc6.2080.0cc1.b6ae.3501])
          output-commit-tx=[660 0x200.0000.0001.01be.f67e.4e2f.b9dd.eeb3.4619.73cd.4c62.abb3.5050.b1ad.d772.995b.820b.584a.4884.8900.0000.0000.38b0.2b80.094a.0100.0000.0000.0022.0020.2b1b.5854.183c.12d3.3165.6597.2c46.6892.9d31.4d81.c5dc.dbb2.1cb4.5fe8.a9a8.114f.4a01.0000.0000.0000.2200.20e9.e86e.4823.faa6.2e22.2ebc.858a.2266.3685.6158.f07e.6989.8da3.b0d1.af0d.db39.94e8.0300.0000.0000.0022.0020.10f8.8bf0.9e56.f14f.b454.3fd2.6e47.b0db.50ea.5de9.cf3f.c464.3479.2471.0826.21ae.d007.0000.0000.0000.2200.203e.6811.5ae0.b15b.8de7.5b6c.6bc9.af5a.c9f0.1391.544e.0870.dae4.43a1.e8fe.7837.ead0.0700.0000.0000.0022.0020.fe05.98d7.4fee.2205.cc36.72e6.e664.7706.b4f3.0997.13b4.661b.6248.2c3a.ddd0.4a5e.b80b.0000.0000.0000.2200.20f9.6d03.34fe.b64a.4f40.eb27.2031.d07a.fcb0.38db.56aa.5744.6d60.308c.9f8c.cade.f9a0.0f00.0000.0000.0022.0020.ce6e.7512.7483.6ff5.9622.a0d1.e07f.8831.d80b.d673.0bd4.8581.398b.fadd.2bb8.da9a.c0c6.2d00.0000.0000.2200.20f3.394e.1e61.9b0e.ca1f.91be.2fb5.ab4d.fc59.ba5b.84eb.e014.ad1d.43a5.64d0.1299.4a4f.996a.0000.0000.0022.0020.4adb.4e2f.0064.3db3.96dd.120d.4e7d.c176.25f5.f2c1.1a40.d857.accc.862d.6b7d.d80e.0400.4830.4502.2100.ef82.a405.364b.fc40.07e6.3a7c.c829.25a5.13d7.9065.bdbc.216d.60b6.a422.3a32.3f8a.0220.0716.730b.8561.f3c6.d362.eaf4.7f20.2e99.fb30.d055.7b61.b92b.5f91.34f8.e2de.3681.0148.3045.0221.00e0.1068.3046.7a55.8c07.544a.3de7.7156.10c1.1470.62e7.d091.deee.be8b.5c66.1cda.9402.202a.d049.c1a6.d048.3431.7a78.483f.723c.205c.9f63.8d17.222a.afc6.2080.0cc1.b6ae.3501.4752.2102.3da0.92f6.980e.58d2.c037.1731.80e9.a465.4760.26ee.50f9.6695.963e.8efe.436f.54eb.2103.0e9f.7b62.3d2c.cc7c.9bd4.4d66.d5ce.21ce.504c.0acf.6385.a132.cec6.d3c3.9fa7.11c1.52ae.3e19.5220]
      ==
      ::
      :*  to-local-msat=6.988.000.000
          to-remote-msat=3.000.000.000
          htlcs=htlc-data
          local-feerate-per-kw=645
          our-funding-signature=(some [72 0x3045.0221.00d5.7697.c707.b6f6.d053.febf.24b9.8e89.89f1.86ee.a42e.37e9.e916.63ec.2c70.bb8f.7002.2079.b071.5a47.2118.f262.f430.16a6.74f5.9c01.5d9c.afcc.ec88.5968.e76d.9d9c.5d00.5101])
          her-funding-signature=(some [71 0x30.4402.2025.d974.66c8.049e.955a.5afc.e28e.322f.4b34.d256.1118.e523.32fb.400f.9b90.8cc0.a402.205d.c6fb.a3a0.d67e.e142.c428.c535.580c.d1f2.ff42.e2f8.9b47.e0c8.a018.47ca.ffc3.1201])
          output-commit-tx=[616 0x200.0000.0001.01be.f67e.4e2f.b9dd.eeb3.4619.73cd.4c62.abb3.5050.b1ad.d772.995b.820b.584a.4884.8900.0000.0000.38b0.2b80.084a.0100.0000.0000.0022.0020.2b1b.5854.183c.12d3.3165.6597.2c46.6892.9d31.4d81.c5dc.dbb2.1cb4.5fe8.a9a8.114f.4a01.0000.0000.0000.2200.20e9.e86e.4823.faa6.2e22.2ebc.858a.2266.3685.6158.f07e.6989.8da3.b0d1.af0d.db39.94d0.0700.0000.0000.0022.0020.3e68.115a.e0b1.5b8d.e75b.6c6b.c9af.5ac9.f013.9154.4e08.70da.e443.a1e8.fe78.37ea.d007.0000.0000.0000.2200.20fe.0598.d74f.ee22.05cc.3672.e6e6.6477.06b4.f309.9713.b466.1b62.482c.3add.d04a.5eb8.0b00.0000.0000.0022.0020.f96d.0334.feb6.4a4f.40eb.2720.31d0.7afc.b038.db56.aa57.446d.6030.8c9f.8cca.def9.a00f.0000.0000.0000.2200.20ce.6e75.1274.836f.f596.22a0.d1e0.7f88.31d8.0bd6.730b.d485.8139.8bfa.dd2b.b8da.9ac0.c62d.0000.0000.0022.0020.f339.4e1e.619b.0eca.1f91.be2f.b5ab.4dfc.59ba.5b84.ebe0.14ad.1d43.a564.d012.994a.bc99.6a00.0000.0000.2200.204a.db4e.2f00.643d.b396.dd12.0d4e.7dc1.7625.f5f2.c11a.40d8.57ac.cc86.2d6b.7dd8.0e04.0048.3045.0221.00d5.7697.c707.b6f6.d053.febf.24b9.8e89.89f1.86ee.a42e.37e9.e916.63ec.2c70.bb8f.7002.2079.b071.5a47.2118.f262.f430.16a6.74f5.9c01.5d9c.afcc.ec88.5968.e76d.9d9c.5d00.5101.4730.4402.2025.d974.66c8.049e.955a.5afc.e28e.322f.4b34.d256.1118.e523.32fb.400f.9b90.8cc0.a402.205d.c6fb.a3a0.d67e.e142.c428.c535.580c.d1f2.ff42.e2f8.9b47.e0c8.a018.47ca.ffc3.1201.4752.2102.3da0.92f6.980e.58d2.c037.1731.80e9.a465.4760.26ee.50f9.6695.963e.8efe.436f.54eb.2103.0e9f.7b62.3d2c.cc7c.9bd4.4d66.d5ce.21ce.504c.0acf.6385.a132.cec6.d3c3.9fa7.11c1.52ae.3e19.5220]
      ==
      ::
      :*  to-local-msat=6.988.000.000
          to-remote-msat=3.000.000.000
          htlcs=htlc-data
          local-feerate-per-kw=2.060
          our-funding-signature=(some [71 0x30.4402.201c.e37a.44b9.5213.358c.20f4.4404.d6db.7a60.83be.a6f5.8de6.c465.47ae.41a4.7c9f.8202.206d.b1d4.5be4.1373.e92f.90d3.4638.1feb.bea8.c786.71b2.8c15.3e30.ad1d.b344.1a94.9701])
          her-funding-signature=(some [71 0x30.4402.2062.08ae.b34e.404b.d052.ce3f.298d.fa83.2891.c9d4.2cae.c99f.e2a0.d283.2e96.90b9.4302.201b.034b.fcc6.fa9f.aec6.67a9.b7cb.fe0b.8d85.e954.aa23.9b66.2778.87b5.088a.ff08.c301])
          output-commit-tx=[615 0x2.0000.0000.0101.bef6.7e4e.2fb9.ddee.b346.1973.cd4c.62ab.b350.50b1.add7.7299.5b82.0b58.4a48.8489.0000.0000.0038.b02b.8008.4a01.0000.0000.0000.2200.202b.1b58.5418.3c12.d331.6565.972c.4668.929d.314d.81c5.dcdb.b21c.b45f.e8a9.a811.4f4a.0100.0000.0000.0022.0020.e9e8.6e48.23fa.a62e.222e.bc85.8a22.6636.8561.58f0.7e69.898d.a3b0.d1af.0ddb.3994.d007.0000.0000.0000.2200.203e.6811.5ae0.b15b.8de7.5b6c.6bc9.af5a.c9f0.1391.544e.0870.dae4.43a1.e8fe.7837.ead0.0700.0000.0000.0022.0020.fe05.98d7.4fee.2205.cc36.72e6.e664.7706.b4f3.0997.13b4.661b.6248.2c3a.ddd0.4a5e.b80b.0000.0000.0000.2200.20f9.6d03.34fe.b64a.4f40.eb27.2031.d07a.fcb0.38db.56aa.5744.6d60.308c.9f8c.cade.f9a0.0f00.0000.0000.0022.0020.ce6e.7512.7483.6ff5.9622.a0d1.e07f.8831.d80b.d673.0bd4.8581.398b.fadd.2bb8.da9a.c0c6.2d00.0000.0000.2200.20f3.394e.1e61.9b0e.ca1f.91be.2fb5.ab4d.fc59.ba5b.84eb.e014.ad1d.43a5.64d0.1299.4ab8.8f6a.0000.0000.0022.0020.4adb.4e2f.0064.3db3.96dd.120d.4e7d.c176.25f5.f2c1.1a40.d857.accc.862d.6b7d.d80e.0400.4730.4402.201c.e37a.44b9.5213.358c.20f4.4404.d6db.7a60.83be.a6f5.8de6.c465.47ae.41a4.7c9f.8202.206d.b1d4.5be4.1373.e92f.90d3.4638.1feb.bea8.c786.71b2.8c15.3e30.ad1d.b344.1a94.9701.4730.4402.2062.08ae.b34e.404b.d052.ce3f.298d.fa83.2891.c9d4.2cae.c99f.e2a0.d283.2e96.90b9.4302.201b.034b.fcc6.fa9f.aec6.67a9.b7cb.fe0b.8d85.e954.aa23.9b66.2778.87b5.088a.ff08.c301.4752.2102.3da0.92f6.980e.58d2.c037.1731.80e9.a465.4760.26ee.50f9.6695.963e.8efe.436f.54eb.2103.0e9f.7b62.3d2c.cc7c.9bd4.4d66.d5ce.21ce.504c.0acf.6385.a132.cec6.d3c3.9fa7.11c1.52ae.3e19.5220]
      ==
      ::
      :*  to-local-msat=6.988.000.000
          to-remote-msat=3.000.000.000
          htlcs=htlc-data
          local-feerate-per-kw=2.061
          our-funding-signature=(some [71 0x30.4402.204a.b07c.6594.12dd.2cd6.043b.1ad8.11ab.215e.901b.6b56.53e0.8cb3.d2fe.63d3.e3dc.5702.2031.c7b3.d130.f938.0ef0.9581.f4f5.a15c.b6f3.59a2.e0a5.9714.6b96.c353.3a26.d6f4.cd01])
          her-funding-signature=(some [72 0x3045.0221.00a2.faf2.ad7e.323b.2a82.e07d.c40b.6847.207c.a6ad.7b08.9f2c.21de.a9a4.d37e.52d5.9d02.204c.9480.ce03.58eb.51d9.2a43.4235.5a97.e272.e3cc.45f8.6c61.2a76.a3fe.32fc.3c4c.b401])
          output-commit-tx=[573 0x2.0000.0000.0101.bef6.7e4e.2fb9.ddee.b346.1973.cd4c.62ab.b350.50b1.add7.7299.5b82.0b58.4a48.8489.0000.0000.0038.b02b.8007.4a01.0000.0000.0000.2200.202b.1b58.5418.3c12.d331.6565.972c.4668.929d.314d.81c5.dcdb.b21c.b45f.e8a9.a811.4f4a.0100.0000.0000.0022.0020.e9e8.6e48.23fa.a62e.222e.bc85.8a22.6636.8561.58f0.7e69.898d.a3b0.d1af.0ddb.3994.d007.0000.0000.0000.2200.203e.6811.5ae0.b15b.8de7.5b6c.6bc9.af5a.c9f0.1391.544e.0870.dae4.43a1.e8fe.7837.eab8.0b00.0000.0000.0022.0020.f96d.0334.feb6.4a4f.40eb.2720.31d0.7afc.b038.db56.aa57.446d.6030.8c9f.8cca.def9.a00f.0000.0000.0000.2200.20ce.6e75.1274.836f.f596.22a0.d1e0.7f88.31d8.0bd6.730b.d485.8139.8bfa.dd2b.b8da.9ac0.c62d.0000.0000.0022.0020.f339.4e1e.619b.0eca.1f91.be2f.b5ab.4dfc.59ba.5b84.ebe0.14ad.1d43.a564.d012.994a.1891.6a00.0000.0000.2200.204a.db4e.2f00.643d.b396.dd12.0d4e.7dc1.7625.f5f2.c11a.40d8.57ac.cc86.2d6b.7dd8.0e04.0047.3044.0220.4ab0.7c65.9412.dd2c.d604.3b1a.d811.ab21.5e90.1b6b.5653.e08c.b3d2.fe63.d3e3.dc57.0220.31c7.b3d1.30f9.380e.f095.81f4.f5a1.5cb6.f359.a2e0.a597.146b.96c3.533a.26d6.f4cd.0148.3045.0221.00a2.faf2.ad7e.323b.2a82.e07d.c40b.6847.207c.a6ad.7b08.9f2c.21de.a9a4.d37e.52d5.9d02.204c.9480.ce03.58eb.51d9.2a43.4235.5a97.e272.e3cc.45f8.6c61.2a76.a3fe.32fc.3c4c.b401.4752.2102.3da0.92f6.980e.58d2.c037.1731.80e9.a465.4760.26ee.50f9.6695.963e.8efe.436f.54eb.2103.0e9f.7b62.3d2c.cc7c.9bd4.4d66.d5ce.21ce.504c.0acf.6385.a132.cec6.d3c3.9fa7.11c1.52ae.3e19.5220]
      ==
      ::
      :*  to-local-msat=6.988.000.000
          to-remote-msat=3.000.000.000
          htlcs=htlc-data
          local-feerate-per-kw=2.185
          our-funding-signature=(some [72 0x3045.0221.00cd.8479.cfe1.edb1.e5a1.d487.391e.0451.a469.c717.1e51.e680.183f.19eb.4321.f20e.9b02.204e.ab7d.5a63.84b1.b08e.03ba.a6e4.d974.8dfd.2b5a.b2ba.e7e3.9604.a0d0.055b.bffd.d501])
          her-funding-signature=(some [71 0x30.4402.2040.f63a.1614.8cf3.5c8d.3d41.827f.5ae7.f7c3.7468.85bb.64d4.d1b8.9589.2a83.812b.3e02.202f.cf95.c2bf.02c4.6616.3b3f.a3ce.d6a2.4926.fbb4.0350.95a9.6842.ef51.6e86.ba54.c001])
          output-commit-tx=[530 0x200.0000.0001.01be.f67e.4e2f.b9dd.eeb3.4619.73cd.4c62.abb3.5050.b1ad.d772.995b.820b.584a.4884.8900.0000.0000.38b0.2b80.064a.0100.0000.0000.0022.0020.2b1b.5854.183c.12d3.3165.6597.2c46.6892.9d31.4d81.c5dc.dbb2.1cb4.5fe8.a9a8.114f.4a01.0000.0000.0000.2200.20e9.e86e.4823.faa6.2e22.2ebc.858a.2266.3685.6158.f07e.6989.8da3.b0d1.af0d.db39.94b8.0b00.0000.0000.0022.0020.f96d.0334.feb6.4a4f.40eb.2720.31d0.7afc.b038.db56.aa57.446d.6030.8c9f.8cca.def9.a00f.0000.0000.0000.2200.20ce.6e75.1274.836f.f596.22a0.d1e0.7f88.31d8.0bd6.730b.d485.8139.8bfa.dd2b.b8da.9ac0.c62d.0000.0000.0022.0020.f339.4e1e.619b.0eca.1f91.be2f.b5ab.4dfc.59ba.5b84.ebe0.14ad.1d43.a564.d012.994a.c591.6a00.0000.0000.2200.204a.db4e.2f00.643d.b396.dd12.0d4e.7dc1.7625.f5f2.c11a.40d8.57ac.cc86.2d6b.7dd8.0e04.0048.3045.0221.00cd.8479.cfe1.edb1.e5a1.d487.391e.0451.a469.c717.1e51.e680.183f.19eb.4321.f20e.9b02.204e.ab7d.5a63.84b1.b08e.03ba.a6e4.d974.8dfd.2b5a.b2ba.e7e3.9604.a0d0.055b.bffd.d501.4730.4402.2040.f63a.1614.8cf3.5c8d.3d41.827f.5ae7.f7c3.7468.85bb.64d4.d1b8.9589.2a83.812b.3e02.202f.cf95.c2bf.02c4.6616.3b3f.a3ce.d6a2.4926.fbb4.0350.95a9.6842.ef51.6e86.ba54.c001.4752.2102.3da0.92f6.980e.58d2.c037.1731.80e9.a465.4760.26ee.50f9.6695.963e.8efe.436f.54eb.2103.0e9f.7b62.3d2c.cc7c.9bd4.4d66.d5ce.21ce.504c.0acf.6385.a132.cec6.d3c3.9fa7.11c1.52ae.3e19.5220]
      ==
      ::
      :*  to-local-msat=6.988.000.000
          to-remote-msat=3.000.000.000
          htlcs=htlc-data
          local-feerate-per-kw=3.686
          our-funding-signature=(some [72 0x3045.0221.00c2.6849.6aad.5c3f.97f2.5cf4.1c1b.a548.3a12.982d.e29b.2220.51b6.de3d.aa22.2941.3b02.207f.3c82.d77a.2c14.f009.6ed9.bb4c.3464.9483.bb20.fa71.f819.f71a.f44d.e659.3e8b.b201])
          her-funding-signature=(some [71 0x30.4402.2078.4485.cf7a.0ad7.979d.af2c.858f.fdaf.5298.d002.0cea.7aea.4668.43e7.9482.23bd.9902.2060.31b8.1d25.e02a.178c.64e6.2f84.3577.fdcd.fc7a.1dec.bbfb.54cd.895d.e692.df85.ca01])
          output-commit-tx=[530 0x200.0000.0001.01be.f67e.4e2f.b9dd.eeb3.4619.73cd.4c62.abb3.5050.b1ad.d772.995b.820b.584a.4884.8900.0000.0000.38b0.2b80.064a.0100.0000.0000.0022.0020.2b1b.5854.183c.12d3.3165.6597.2c46.6892.9d31.4d81.c5dc.dbb2.1cb4.5fe8.a9a8.114f.4a01.0000.0000.0000.2200.20e9.e86e.4823.faa6.2e22.2ebc.858a.2266.3685.6158.f07e.6989.8da3.b0d1.af0d.db39.94b8.0b00.0000.0000.0022.0020.f96d.0334.feb6.4a4f.40eb.2720.31d0.7afc.b038.db56.aa57.446d.6030.8c9f.8cca.def9.a00f.0000.0000.0000.2200.20ce.6e75.1274.836f.f596.22a0.d1e0.7f88.31d8.0bd6.730b.d485.8139.8bfa.dd2b.b8da.9ac0.c62d.0000.0000.0022.0020.f339.4e1e.619b.0eca.1f91.be2f.b5ab.4dfc.59ba.5b84.ebe0.14ad.1d43.a564.d012.994a.2989.6a00.0000.0000.2200.204a.db4e.2f00.643d.b396.dd12.0d4e.7dc1.7625.f5f2.c11a.40d8.57ac.cc86.2d6b.7dd8.0e04.0048.3045.0221.00c2.6849.6aad.5c3f.97f2.5cf4.1c1b.a548.3a12.982d.e29b.2220.51b6.de3d.aa22.2941.3b02.207f.3c82.d77a.2c14.f009.6ed9.bb4c.3464.9483.bb20.fa71.f819.f71a.f44d.e659.3e8b.b201.4730.4402.2078.4485.cf7a.0ad7.979d.af2c.858f.fdaf.5298.d002.0cea.7aea.4668.43e7.9482.23bd.9902.2060.31b8.1d25.e02a.178c.64e6.2f84.3577.fdcd.fc7a.1dec.bbfb.54cd.895d.e692.df85.ca01.4752.2102.3da0.92f6.980e.58d2.c037.1731.80e9.a465.4760.26ee.50f9.6695.963e.8efe.436f.54eb.2103.0e9f.7b62.3d2c.cc7c.9bd4.4d66.d5ce.21ce.504c.0acf.6385.a132.cec6.d3c3.9fa7.11c1.52ae.3e19.5220]
      ==
      ::
      :*  to-local-msat=6.988.000.000
          to-remote-msat=3.000.000.000
          htlcs=htlc-data
          local-feerate-per-kw=3.687
          our-funding-signature=(some [72 0x3045.0221.00c9.7079.9bcb.33f4.3179.eb43.b337.8a0a.6199.1cf2.923f.69b3.6ef1.2548.c3df.0e6d.5002.2041.3dc2.7d2e.39ee.5830.93ad.fcb7.799b.e680.1417.38ba.bb31.cc7b.0669.a777.a31f.5d01])
          her-funding-signature=(some [72 0x3045.0221.00ad.6c71.5698.56b2.d7ff.42e8.38b4.abe7.4a71.3426.b37f.22fa.667a.195a.4c88.908c.6902.202b.3727.2b02.a42d.c6d9.f4f8.2cab.3eaf.84ac.882d.9ed7.6285.9e1e.7545.5c2c.2283.7701])
          output-commit-tx=[488 0x200.0000.0001.01be.f67e.4e2f.b9dd.eeb3.4619.73cd.4c62.abb3.5050.b1ad.d772.995b.820b.584a.4884.8900.0000.0000.38b0.2b80.054a.0100.0000.0000.0022.0020.2b1b.5854.183c.12d3.3165.6597.2c46.6892.9d31.4d81.c5dc.dbb2.1cb4.5fe8.a9a8.114f.4a01.0000.0000.0000.2200.20e9.e86e.4823.faa6.2e22.2ebc.858a.2266.3685.6158.f07e.6989.8da3.b0d1.af0d.db39.94a0.0f00.0000.0000.0022.0020.ce6e.7512.7483.6ff5.9622.a0d1.e07f.8831.d80b.d673.0bd4.8581.398b.fadd.2bb8.da9a.c0c6.2d00.0000.0000.2200.20f3.394e.1e61.9b0e.ca1f.91be.2fb5.ab4d.fc59.ba5b.84eb.e014.ad1d.43a5.64d0.1299.4aa2.8b6a.0000.0000.0022.0020.4adb.4e2f.0064.3db3.96dd.120d.4e7d.c176.25f5.f2c1.1a40.d857.accc.862d.6b7d.d80e.0400.4830.4502.2100.c970.799b.cb33.f431.79eb.43b3.378a.0a61.991c.f292.3f69.b36e.f125.48c3.df0e.6d50.0220.413d.c27d.2e39.ee58.3093.adfc.b779.9be6.8014.1738.babb.31cc.7b06.69a7.77a3.1f5d.0148.3045.0221.00ad.6c71.5698.56b2.d7ff.42e8.38b4.abe7.4a71.3426.b37f.22fa.667a.195a.4c88.908c.6902.202b.3727.2b02.a42d.c6d9.f4f8.2cab.3eaf.84ac.882d.9ed7.6285.9e1e.7545.5c2c.2283.7701.4752.2102.3da0.92f6.980e.58d2.c037.1731.80e9.a465.4760.26ee.50f9.6695.963e.8efe.436f.54eb.2103.0e9f.7b62.3d2c.cc7c.9bd4.4d66.d5ce.21ce.504c.0acf.6385.a132.cec6.d3c3.9fa7.11c1.52ae.3e19.5220]
      ==
      ::
      :*  to-local-msat=6.988.000.000
          to-remote-msat=3.000.000.000
          htlcs=htlc-data
          local-feerate-per-kw=4.893
          our-funding-signature=(some [71 0x30.4402.2008.6288.face.ab47.461e.b2d8.08e9.e9b0.cb3f.fc24.a03c.2f18.db71.9824.7d38.f10e.5802.2031.d1c2.782a.58c8.c6ce.187d.0019.eb47.a83b.abdf.3040.e2ca.ff29.9ab4.8f7e.12b1.fa01])
          her-funding-signature=(some [72 0x3045.0221.00a8.7711.4710.9e4d.3f44.a597.6c3c.3de9.8732.bbb7.7308.d214.44db.e0d7.6faf.0648.0e02.200b.4e91.6e85.0c3d.1f91.8de8.7bbb.bb07.843f.fea1.d465.8dfe.060b.6f9c.cd96.d34b.e801])
          output-commit-tx=[487 0x2.0000.0000.0101.bef6.7e4e.2fb9.ddee.b346.1973.cd4c.62ab.b350.50b1.add7.7299.5b82.0b58.4a48.8489.0000.0000.0038.b02b.8005.4a01.0000.0000.0000.2200.202b.1b58.5418.3c12.d331.6565.972c.4668.929d.314d.81c5.dcdb.b21c.b45f.e8a9.a811.4f4a.0100.0000.0000.0022.0020.e9e8.6e48.23fa.a62e.222e.bc85.8a22.6636.8561.58f0.7e69.898d.a3b0.d1af.0ddb.3994.a00f.0000.0000.0000.2200.20ce.6e75.1274.836f.f596.22a0.d1e0.7f88.31d8.0bd6.730b.d485.8139.8bfa.dd2b.b8da.9ac0.c62d.0000.0000.0022.0020.f339.4e1e.619b.0eca.1f91.be2f.b5ab.4dfc.59ba.5b84.ebe0.14ad.1d43.a564.d012.994a.8785.6a00.0000.0000.2200.204a.db4e.2f00.643d.b396.dd12.0d4e.7dc1.7625.f5f2.c11a.40d8.57ac.cc86.2d6b.7dd8.0e04.0047.3044.0220.0862.88fa.ceab.4746.1eb2.d808.e9e9.b0cb.3ffc.24a0.3c2f.18db.7198.247d.38f1.0e58.0220.31d1.c278.2a58.c8c6.ce18.7d00.19eb.47a8.3bab.df30.40e2.caff.299a.b48f.7e12.b1fa.0148.3045.0221.00a8.7711.4710.9e4d.3f44.a597.6c3c.3de9.8732.bbb7.7308.d214.44db.e0d7.6faf.0648.0e02.200b.4e91.6e85.0c3d.1f91.8de8.7bbb.bb07.843f.fea1.d465.8dfe.060b.6f9c.cd96.d34b.e801.4752.2102.3da0.92f6.980e.58d2.c037.1731.80e9.a465.4760.26ee.50f9.6695.963e.8efe.436f.54eb.2103.0e9f.7b62.3d2c.cc7c.9bd4.4d66.d5ce.21ce.504c.0acf.6385.a132.cec6.d3c3.9fa7.11c1.52ae.3e19.5220]
      ==
      ::
      :*  to-local-msat=6.988.000.000
          to-remote-msat=3.000.000.000
          htlcs=htlc-data
          local-feerate-per-kw=4.894
          our-funding-signature=(some [72 0x3045.0221.009f.16ac.85d2.32e4.eddb.3fcd.750a.68eb.f0b5.8e33.56ea.ada4.5d35.13ed.e7e8.17bf.4c02.207c.2b04.3b4e.5f97.1261.9754.06cb.9552.19fa.56bf.fe5d.834a.8336.94b5.abc1.ce4c.fd01])
          her-funding-signature=(some [72 0x3045.0221.00e7.84a6.6b15.8857.5801.e237.d35e.510f.d92a.81ae.3a4a.2a1b.90c0.31ad.803d.07b3.f302.2021.bc5f.1650.1f16.7607.d63b.6814.42da.193e.b0a7.6b4b.7fd2.5c2e.d4f8.b28f.d35b.9501])
          output-commit-tx=[445 0x2.0000.0000.0101.bef6.7e4e.2fb9.ddee.b346.1973.cd4c.62ab.b350.50b1.add7.7299.5b82.0b58.4a48.8489.0000.0000.0038.b02b.8004.4a01.0000.0000.0000.2200.202b.1b58.5418.3c12.d331.6565.972c.4668.929d.314d.81c5.dcdb.b21c.b45f.e8a9.a811.4f4a.0100.0000.0000.0022.0020.e9e8.6e48.23fa.a62e.222e.bc85.8a22.6636.8561.58f0.7e69.898d.a3b0.d1af.0ddb.3994.c0c6.2d00.0000.0000.2200.20f3.394e.1e61.9b0e.ca1f.91be.2fb5.ab4d.fc59.ba5b.84eb.e014.ad1d.43a5.64d0.1299.4ad0.886a.0000.0000.0022.0020.4adb.4e2f.0064.3db3.96dd.120d.4e7d.c176.25f5.f2c1.1a40.d857.accc.862d.6b7d.d80e.0400.4830.4502.2100.9f16.ac85.d232.e4ed.db3f.cd75.0a68.ebf0.b58e.3356.eaad.a45d.3513.ede7.e817.bf4c.0220.7c2b.043b.4e5f.9712.6197.5406.cb95.5219.fa56.bffe.5d83.4a83.3694.b5ab.c1ce.4cfd.0148.3045.0221.00e7.84a6.6b15.8857.5801.e237.d35e.510f.d92a.81ae.3a4a.2a1b.90c0.31ad.803d.07b3.f302.2021.bc5f.1650.1f16.7607.d63b.6814.42da.193e.b0a7.6b4b.7fd2.5c2e.d4f8.b28f.d35b.9501.4752.2102.3da0.92f6.980e.58d2.c037.1731.80e9.a465.4760.26ee.50f9.6695.963e.8efe.436f.54eb.2103.0e9f.7b62.3d2c.cc7c.9bd4.4d66.d5ce.21ce.504c.0acf.6385.a132.cec6.d3c3.9fa7.11c1.52ae.3e19.5220]
      ==
      ::
      :*  to-local-msat=6.988.000.000
          to-remote-msat=3.000.000.000
          htlcs=htlc-data
          local-feerate-per-kw=6.216.010
          our-funding-signature=(some [72 0x3045.0221.009a.d807.92e3.038f.e696.8d12.ff23.e688.8a56.5c3d.dd06.5037.f357.445f.0167.5d63.f302.2018.3849.15e5.f1f4.ae15.7e15.debf.4f49.b61c.8d9d.2b07.3c7d.6f97.c4a6.8caa.3ed4.c101])
          her-funding-signature=(some [72 0x3045.0221.008f.d5db.ff02.e4b5.9020.d4cd.23a3.c30d.3e28.7065.fda7.5a0a.09b4.0298.0adf.68cc.da02.2001.e0b8.b620.cd91.5ddf.f11f.1de3.2add.f23d.81d5.1b90.e684.1b2c.b8dc.af3f.aa5e.cf01])
          output-commit-tx=[359 0x2.0000.0000.0101.bef6.7e4e.2fb9.ddee.b346.1973.cd4c.62ab.b350.50b1.add7.7299.5b82.0b58.4a48.8489.0000.0000.0038.b02b.8002.4a01.0000.0000.0000.2200.20e9.e86e.4823.faa6.2e22.2ebc.858a.2266.3685.6158.f07e.6989.8da3.b0d1.af0d.db39.94c0.c62d.0000.0000.0022.0020.f339.4e1e.619b.0eca.1f91.be2f.b5ab.4dfc.59ba.5b84.ebe0.14ad.1d43.a564.d012.994a.0400.4830.4502.2100.9ad8.0792.e303.8fe6.968d.12ff.23e6.888a.565c.3ddd.0650.37f3.5744.5f01.675d.63f3.0220.1838.4915.e5f1.f4ae.157e.15de.bf4f.49b6.1c8d.9d2b.073c.7d6f.97c4.a68c.aa3e.d4c1.0148.3045.0221.008f.d5db.ff02.e4b5.9020.d4cd.23a3.c30d.3e28.7065.fda7.5a0a.09b4.0298.0adf.68cc.da02.2001.e0b8.b620.cd91.5ddf.f11f.1de3.2add.f23d.81d5.1b90.e684.1b2c.b8dc.af3f.aa5e.cf01.4752.2102.3da0.92f6.980e.58d2.c037.1731.80e9.a465.4760.26ee.50f9.6695.963e.8efe.436f.54eb.2103.0e9f.7b62.3d2c.cc7c.9bd4.4d66.d5ce.21ce.504c.0acf.6385.a132.cec6.d3c3.9fa7.11c1.52ae.3e19.5220]
      ==
      ::
      :*  to-local-msat=6.988.000.000
          to-remote-msat=3.000.000.000
          htlcs=htlc-data-2
          local-feerate-per-kw=253
          our-funding-signature=(some [72 0x3045.0221.00c3.7ac4.fc85.3867.7631.230c.4b28.6f36.b6f5.4c51.fb4b.34ef.0bd0.ba21.9ba4.7452.6302.2027.8e09.a745.454e.a380.f369.4392.ed11.3762.c68d.d209.b483.60f5.4754.1088.be9e.4501])
          her-funding-signature=(some [72 0x3045.0221.00c5.92f6.b80d.35b4.f5d1.e3bc.9788.f511.41a0.065b.e601.3bad.53a1.977f.7c44.4651.6602.2027.8ac0.6ead.9016.bfb8.dc47.6f18.6eab.ace2.b027.93b2.f308.442f.5b0d.5f24.a689.4801])
          output-commit-tx=[574 0x200.0000.0001.01be.f67e.4e2f.b9dd.eeb3.4619.73cd.4c62.abb3.5050.b1ad.d772.995b.820b.584a.4884.8900.0000.0000.38b0.2b80.074a.0100.0000.0000.0022.0020.2b1b.5854.183c.12d3.3165.6597.2c46.6892.9d31.4d81.c5dc.dbb2.1cb4.5fe8.a9a8.114f.4a01.0000.0000.0000.2200.20e9.e86e.4823.faa6.2e22.2ebc.858a.2266.3685.6158.f07e.6989.8da3.b0d1.af0d.db39.94d0.0700.0000.0000.0022.0020.fe05.98d7.4fee.2205.cc36.72e6.e664.7706.b4f3.0997.13b4.661b.6248.2c3a.ddd0.4a5e.8813.0000.0000.0000.2200.2018.e40f.9072.c443.50f1.34bd.c887.bab4.d9bd.fc8a.a468.a256.16c8.0e21.757b.a5da.c788.1300.0000.0000.0022.0020.18e4.0f90.72c4.4350.f134.bdc8.87ba.b4d9.bdfc.8aa4.68a2.5616.c80e.2175.7ba5.dac7.c0c6.2d00.0000.0000.2200.20f3.394e.1e61.9b0e.ca1f.91be.2fb5.ab4d.fc59.ba5b.84eb.e014.ad1d.43a5.64d0.1299.4aae.9c6a.0000.0000.0022.0020.4adb.4e2f.0064.3db3.96dd.120d.4e7d.c176.25f5.f2c1.1a40.d857.accc.862d.6b7d.d80e.0400.4830.4502.2100.c37a.c4fc.8538.6776.3123.0c4b.286f.36b6.f54c.51fb.4b34.ef0b.d0ba.219b.a474.5263.0220.278e.09a7.4545.4ea3.80f3.6943.92ed.1137.62c6.8dd2.09b4.8360.f547.5410.88be.9e45.0148.3045.0221.00c5.92f6.b80d.35b4.f5d1.e3bc.9788.f511.41a0.065b.e601.3bad.53a1.977f.7c44.4651.6602.2027.8ac0.6ead.9016.bfb8.dc47.6f18.6eab.ace2.b027.93b2.f308.442f.5b0d.5f24.a689.4801.4752.2102.3da0.92f6.980e.58d2.c037.1731.80e9.a465.4760.26ee.50f9.6695.963e.8efe.436f.54eb.2103.0e9f.7b62.3d2c.cc7c.9bd4.4d66.d5ce.21ce.504c.0acf.6385.a132.cec6.d3c3.9fa7.11c1.52ae.3e19.5220]
      ==
    ==
  --
--
