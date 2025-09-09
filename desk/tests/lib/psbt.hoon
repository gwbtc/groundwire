/+  *test, *psbt, bc=bitcoin
|%
++  test-valid-psbt-001
  |^
  ;:  welp
    %+  category  "tx1"
    (check-tx tx1)
  ::
    %+  category  "tx2"
    (check-tx tx2)
  ==
  ++  check-tx
    |=  tx=psbt
    ;:  welp
      %+  category  "input-length"
      %+  expect-eq
        !>  1
        !>  (lent inputs.tx)
      ::
      %+  category  "input-0"
      %+  expect-eq
        !>  %.n
        !>  ~(is-complete txin (snag 0 inputs.tx))
    ==
  ::
  ++  tx1
    ^-  psbt
    %-  from-byts:create
    =+
    0x70.7362.74ff.0100.7502.0000.0001.2681.7137.1edf.f285.e937.adee.a4b3.7b78.000c.0566.cbb3.ad64.6417.13ca.4217.1bf6.0000.0000.00fe.ffff.ff02.d3df.f505.0000.0000.1976.a914.d0c5.9903.c5ba.c286.8760.e90f.d521.a466.5aa7.6520.88ac.00e1.f505.0000.0000.17a9.1435.45e6.e33b.832c.4705.0f24.d3ee.b93c.9c03.948b.c787.b32e.1300.0001.00fd.a501.0100.0000.0001.0289.a3c7.1eab.4d20.e037.1bbb.a4cc.698f.a295.c946.3afa.2e39.7f85.33cc.b62f.9567.e501.0000.0017.1600.14be.18d1.52a9.b012.039d.af3d.a7de.4f53.349e.ecb9.85ff.ffff.ff86.f8aa.43a7.1dff.1448.893a.530a.7237.ef6b.4608.bbb2.dd2d.0171.e63a.ec6a.4890.b401.0000.0017.1600.14fe.3e9e.f1a7.45e9.74d9.02c4.3559.43ab.cb34.bd53.53ff.ffff.ff02.00c2.eb0b.0000.0000.1976.a914.85cf.f109.7fd9.e008.bb34.af70.9c62.197b.3897.8a48.88ac.72fe.f84e.2c00.0000.17a9.1433.9725.ba21.efd6.2ac7.53a9.bcd0.67d6.c7a6.a39d.0587.0247.3044.0220.2712.be22.e027.0f39.4f56.8311.dc7c.a9a6.8970.b802.5fdd.3b24.0229.f07f.8a5f.3a24.0220.018b.38d7.dcd3.14e7.34c9.276b.d6fb.40f6.7332.5bc4.baa1.44c8.00d2.f2f0.2db2.765c.0121.03d2.e156.7494.1bad.4a99.6372.cb87.e185.6d36.5260.6d98.562f.e39c.5e9e.7e41.3f21.0502.4830.4502.2100.d12b.852d.85dc.d961.d2f5.f4ab.6606.54df.6eed.cc79.4c0c.33ce.5cc3.09ff.b5fc.e58d.0220.6733.8a8e.0e17.25c1.97fb.1a88.af59.f51e.44e4.255b.2016.7c86.8403.1c05.d1f2.592a.0121.0223.b72b.eef0.965d.10be.0778.efec.d61f.cac6.f79a.4ea1.6939.3380.7344.64f8.4f2a.b300.0000.0000.0000
  [(met 3 -) -]
  ::
  ++  tx2
    ^-  psbt
    %-  need
    %-  from-base64:create
    'cHNidP8BAHUCAAAAASaBcTce3/KF6Tet7qSze3gADAVmy7OtZGQXE8pCFxv2AAAAAAD+////AtPf9QUAAAAAGXapFNDFmQPFusKGh2DpD9UhpGZap2UgiKwA4fUFAAAAABepFDVF5uM7gyxHBQ8k0+65PJwDlIvHh7MuEwAAAQD9pQEBAAAAAAECiaPHHqtNIOA3G7ukzGmPopXJRjr6Ljl/hTPMti+VZ+UBAAAAFxYAFL4Y0VKpsBIDna89p95PUzSe7LmF/////4b4qkOnHf8USIk6UwpyN+9rRgi7st0tAXHmOuxqSJC0AQAAABcWABT+Pp7xp0XpdNkCxDVZQ6vLNL1TU/////8CAMLrCwAAAAAZdqkUhc/xCX/Z4Ai7NK9wnGIZeziXikiIrHL++E4sAAAAF6kUM5cluiHv1irHU6m80GfWx6ajnQWHAkcwRAIgJxK+IuAnDzlPVoMR3HyppolwuAJf3TskAinwf4pfOiQCIAGLONfc0xTnNMkna9b7QPZzMlvEuqFEyADS8vAtsnZcASED0uFWdJQbrUqZY3LLh+GFbTZSYG2YVi/jnF6efkE/IQUCSDBFAiEA0SuFLYXc2WHS9fSrZgZU327tzHlMDDPOXMMJ/7X85Y0CIGczio4OFyXBl/saiK9Z9R5E5CVbIBZ8hoQDHAXR8lkqASECI7cr7vCWXRC+B3jv7NYfysb3mk6haTkzgHNEZPhPKrMAAAAAAAAA'
  --
::
++  test-valid-psbt-002
  |^
  ;:  welp
    %+  category  "tx1"
    (check-tx tx1)
  ::
    %+  category  "tx2"
    (check-tx tx2)
  ==
  ++  check-tx
    |=  tx=psbt
    ;:  welp
      %+  category  "input-length"
      %+  expect-eq
        !>  2
        !>  (lent inputs.tx)
    ::
      %+  category  "input-0"
      %+  expect-eq
        !>  %.y
        !>  ~(is-complete txin (snag 0 inputs.tx))
    ::
      %+  category  "input-1"
      %+  expect-eq
        !>  %.n
        !>  ~(is-complete txin (snag 1 inputs.tx))
    ==
  ++  tx1
    ^-  psbt
    %-  need
    %-  from-base16:create
     '70736274ff0100a00200000002ab0949a08c5af7c49b8212f417e2f15ab3f5c33dcf153821a8139f877a5b7be40000000000feffffffab0949a08c5af7c49b8212f417e2f15ab3f5c33dcf153821a8139f877a5b7be40100000000feffffff02603bea0b000000001976a914768a40bbd740cbe81d988e71de2a4d5c71396b1d88ac8e240000000000001976a9146f4620b553fa095e721b9ee0efe9fa039cca459788ac000000000001076a47304402204759661797c01b036b25928948686218347d89864b719e1f7fcf57d1e511658702205309eabf56aa4d8891ffd111fdf1336f3a29da866d7f8486d75546ceedaf93190121035cdc61fc7ba971c0b501a646a2a83b102cb43881217ca682dc86e2d73fa882920001012000e1f5050000000017a9143545e6e33b832c47050f24d3eeb93c9c03948bc787010416001485d13537f2e265405a34dbafa9e3dda01fb82308000000'
  ::
  ++  tx2
    ^-  psbt
    %-  need
    %-  from-base64:create
    'cHNidP8BAKACAAAAAqsJSaCMWvfEm4IS9Bfi8Vqz9cM9zxU4IagTn4d6W3vkAAAAAAD+////qwlJoIxa98SbghL0F+LxWrP1wz3PFTghqBOfh3pbe+QBAAAAAP7///8CYDvqCwAAAAAZdqkUdopAu9dAy+gdmI5x3ipNXHE5ax2IrI4kAAAAAAAAGXapFG9GILVT+glechue4O/p+gOcykWXiKwAAAAAAAEHakcwRAIgR1lmF5fAGwNrJZKJSGhiGDR9iYZLcZ4ff89X0eURZYcCIFMJ6r9Wqk2Ikf/REf3xM286KdqGbX+EhtdVRs7tr5MZASEDXNxh/HupccC1AaZGoqg7ECy0OIEhfKaC3Ibi1z+ogpIAAQEgAOH1BQAAAAAXqRQ1RebjO4MsRwUPJNPuuTycA5SLx4cBBBYAFIXRNTfy4mVAWjTbr6nj3aAfuCMIAAAA'
  --
::
++  test-valid-psbt-003
  |^
  ;:  weld
    %+  category  "tx1"
    %-  check-tx  tx1
  ::
    %+  category  "tx2"
    %-  check-tx  tx2
  ==
  ++  check-tx
    |=  tx=psbt
    ;:  weld
      %+  category  "input-length"
      %+  expect-eq
        !>  1
        !>  (lent inputs.tx)
      ::
      %+  category  "input-sighash"
      %+  expect-eq
        !>  `all:sighash
        !>  sighash:(snag 0 inputs.tx)
      ::
      %+  category  "input-complete"
      %+  expect-eq
        !>  %.n
        !>  ~(is-complete txin (snag 0 inputs.tx))
    ==
  ::
  ++  tx1
    ^-  psbt
    %-  need
    %-  from-base16:create
    '70736274ff0100750200000001268171371edff285e937adeea4b37b78000c0566cbb3ad64641713ca42171bf60000000000feffffff02d3dff505000000001976a914d0c59903c5bac2868760e90fd521a4665aa7652088ac00e1f5050000000017a9143545e6e33b832c47050f24d3eeb93c9c03948bc787b32e1300000100fda5010100000000010289a3c71eab4d20e0371bbba4cc698fa295c9463afa2e397f8533ccb62f9567e50100000017160014be18d152a9b012039daf3da7de4f53349eecb985ffffffff86f8aa43a71dff1448893a530a7237ef6b4608bbb2dd2d0171e63aec6a4890b40100000017160014fe3e9ef1a745e974d902c4355943abcb34bd5353ffffffff0200c2eb0b000000001976a91485cff1097fd9e008bb34af709c62197b38978a4888ac72fef84e2c00000017a914339725ba21efd62ac753a9bcd067d6c7a6a39d05870247304402202712be22e0270f394f568311dc7ca9a68970b8025fdd3b240229f07f8a5f3a240220018b38d7dcd314e734c9276bd6fb40f673325bc4baa144c800d2f2f02db2765c012103d2e15674941bad4a996372cb87e1856d3652606d98562fe39c5e9e7e413f210502483045022100d12b852d85dcd961d2f5f4ab660654df6eedcc794c0c33ce5cc309ffb5fce58d022067338a8e0e1725c197fb1a88af59f51e44e4255b20167c8684031c05d1f2592a01210223b72beef0965d10be0778efecd61fcac6f79a4ea169393380734464f84f2ab30000000001030401000000000000'
  ::
  ++  tx2
    ^-  psbt
    %-  need
    %-  from-base64:create
    'cHNidP8BAHUCAAAAASaBcTce3/KF6Tet7qSze3gADAVmy7OtZGQXE8pCFxv2AAAAAAD+////AtPf9QUAAAAAGXapFNDFmQPFusKGh2DpD9UhpGZap2UgiKwA4fUFAAAAABepFDVF5uM7gyxHBQ8k0+65PJwDlIvHh7MuEwAAAQD9pQEBAAAAAAECiaPHHqtNIOA3G7ukzGmPopXJRjr6Ljl/hTPMti+VZ+UBAAAAFxYAFL4Y0VKpsBIDna89p95PUzSe7LmF/////4b4qkOnHf8USIk6UwpyN+9rRgi7st0tAXHmOuxqSJC0AQAAABcWABT+Pp7xp0XpdNkCxDVZQ6vLNL1TU/////8CAMLrCwAAAAAZdqkUhc/xCX/Z4Ai7NK9wnGIZeziXikiIrHL++E4sAAAAF6kUM5cluiHv1irHU6m80GfWx6ajnQWHAkcwRAIgJxK+IuAnDzlPVoMR3HyppolwuAJf3TskAinwf4pfOiQCIAGLONfc0xTnNMkna9b7QPZzMlvEuqFEyADS8vAtsnZcASED0uFWdJQbrUqZY3LLh+GFbTZSYG2YVi/jnF6efkE/IQUCSDBFAiEA0SuFLYXc2WHS9fSrZgZU327tzHlMDDPOXMMJ/7X85Y0CIGczio4OFyXBl/saiK9Z9R5E5CVbIBZ8hoQDHAXR8lkqASECI7cr7vCWXRC+B3jv7NYfysb3mk6haTkzgHNEZPhPKrMAAAAAAQMEAQAAAAAAAA=='
  --
::
++  test-sighash-types
  |^
  check-sighash-all
  ::
  ++  locktime  0
  ::
  ++  prevout
    :*  ^=  txid
        ^-  hexb:bc
        :-  32
        0x6eb9.8797.a21c.6c10.aa74.edf2.9d61.8be1.09f4.8a8e.94c6.94f3.701e.08ca.6918.6436
        pos=1
    ==
  ::
  ++  txin
    ^-  input
    =|  i=input
    %_  i
      prevout         prevout
      nsequence       0xffff.ffff
      trusted-value   `987.654.321
      redeem-script
        %-  some
        :-  34
        0x20.a16b.5755.f7f6.f96d.bd65.f5f0.d6ab.9418.b89a.f4b1.f14a.1bb8.a090.62c3.5f0d.cb54
      witness-script
        %-  some
        =-  [wid=(met 3 -) dat=-]
        0x56.2103.07b8.ae49.ac90.a048.e9b5.3357.a235.4b33.34e9.c8be.e813.ecb9.8e99.a7e0.7e8c.3ba3.2103.b28f.0c28.bfab.5455.4ae8.c658.ac5c.3e0c.e6e7.9ad3.3633.1f78.c428.dd43.eea8.449b.2103.4b81.13d7.0341.3d57.761b.8b97.8195.7b8c.0ac1.dfe6.9f49.2580.ca41.95f5.0376.ba4a.2103.3400.f6af.ecb8.3309.2a9a.21cf.df1e.d137.6e58.c5d1.f47d.e746.8312.3987.e967.a8f4.2103.a6d4.8b11.31e9.4ba0.4d97.37d6.1acd.aa13.2200.8af9.602b.3b14.862c.07a1.789a.ac16.2102.d8b6.61b0.b330.2ee2.f162.b09e.07a5.5ad5.dfbe.673a.9f01.d9f0.c196.1768.1024.306b.56ae
    ==
  ::
  ++  txout1
    ^-  output
    =|  o=output
    %_  o
      script-pubkey  [wid=25 dat=0x76.a914.389f.fce9.cd9a.e88d.cc06.31e8.8a82.1ffd.be9b.fe26.88ac]
      value          900.000.000
    ==
  ::
  ++  txout2
    ^-  output
    =|  o=output
    %_  o
      script-pubkey  [wid=25 dat=0x76.a914.7480.a33f.9506.89af.511e.6e84.c138.dbbd.3c3e.e415.88ac]
      value          87.000.000
    ==
  ::
  ++  check-sighash-all
    =+  ^=  privkey
        :*  wid=32
            dat=0x730f.ff80.e141.3068.a05b.57d6.a582.61f0.7551.1633.6978.7f34.9438.ea38.ca80.fac6
        ==
    =+  ^=  tx
        =|  =psbt
        %_  psbt
          inputs     ~[txin]
          outputs    ~[txout2 txout1]
          nlocktime  locktime
          nversion   1
        ==
    %+  expect-eq
      !>  =-  [wid=(met 3 -) dat=-]
          0x30.4402.2065.8040.3bc8.efc8.d1b3.9b3a.ffee.7691.395b.d68e.5507.a833.efc0.46dd.6850.b647.9f02.207b.d251.9a5e.fd49.2647.437c.7c3a.7fb9.381c.d37d.5c42.e340.fd77.6b8c.ecfb.1547.d401
      ::
      !>  (~(one sign tx) 0 privkey ~)
  --
::
++  test-segwit
  |^
  ;:  weld
    check-decode
    check-encode
  ==
  ::
  ++  check-decode
    %+  expect-eq
      !>  decoded-tx
      !>  (decode-tx raw-tx)
  ::
  ++  check-encode
    %+  expect-eq
      !>  raw-tx
      !>  (encode-tx decoded-tx)
  ::
  ++  decoded-inputs
    ^-  (list in:tx)
    :~  :*  prevout=[txid=input-txid idx=0]
            script-sig=~
            nsequence=0
            script-witness=`~[witness-part-1 witness-part-2]
        ==
    ==
  ::
  ++  decoded-outputs
    ^-  (list out:tx)
    :~  :*  value=25.000
            script-pubkey=output1-script-pubkey
        ==
        :*  value=9.999.967.363
            script-pubkey=output2-script-pubkey
        ==
    ==
  ::
  ++  decoded-tx
    ^-  tx:tx
    :*  vin=decoded-inputs
        vout=decoded-outputs
        nversion=2
        nlocktime=0
    ==
  ::
  ++  input-txid
    ^-  hexb:bc
    :-  32
    0x6371.37c2.8d8f.a677.e75a.851f.a93a.7323.57f4.32ea.e311.6075.9eb2.774a.45d6.e8ca
  ::
  ++  output1-script-pubkey
    ^-  hexb:bc
    :-  34
    0x20.e7b0.b352.33c8.214a.a0e9.885f.9392.e5f1.e967.3a3b.9160.5a1c.1b29.5b45.4430.cc83
  ::
  ++  output2-script-pubkey
    ^-  hexb:bc
    :-  22
    0x14.503c.901b.fb25.268d.74a0.b4a4.0387.8215.4b8f.ad6a
  ::
  ++  witness-part-1
    ^-  hexb:bc
    :-  72
    0x3045.0221.0081.82e7.a153.8235.4a50.3913.85f2.
    9214.9f02.0e3f.b422.0f45.0e13.8d9b.32cf.999f.
    2002.203f.6135.1531.335b.3965.a166.d2b3.d47b.
    a909.ae1b.2c0c.5821.f293.40eb.31b4.6c06.2301
  ::
  ++  witness-part-2
    ^-  hexb:bc
    :-  33
    0x2.7dc3.13f1.aaa7.723f.2cab.b5fc.9a85.3d60.8b2e.ce4b.3eb3.ba1a.cb23.a742.9c15.fcc8
  ::
  ++  raw-tx
    ^-  hexb:bc
    :-  235
    0x2.0000.0000.0101.cae8.d645.4a77.b29e.7560.11e3.
    ea32.f457.2373.3aa9.1f85.5ae7.77a6.8f8d.c237.7163.
    0000.0000.0000.0000.0002.a861.0000.0000.0000.2200.
    20e7.b0b3.5233.c821.4aa0.e988.5f93.92e5.f1e9.673a.
    3b91.605a.1c1b.295b.4544.30cc.8383.640b.5402.0000.
    0016.0014.503c.901b.fb25.268d.74a0.b4a4.0387.8215.
    4b8f.ad6a.0248.3045.0221.0081.82e7.a153.8235.4a50.
    3913.85f2.9214.9f02.0e3f.b422.0f45.0e13.8d9b.32cf.
    999f.2002.203f.6135.1531.335b.3965.a166.d2b3.d47b.
    a909.ae1b.2c0c.5821.f293.40eb.31b4.6c06.2301.2102.
    7dc3.13f1.aaa7.723f.2cab.b5fc.9a85.3d60.8b2e.ce4b.
    3eb3.ba1a.cb23.a742.9c15.fcc8.0000.0000
  --
--
