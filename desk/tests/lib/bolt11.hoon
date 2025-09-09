/-  btc=bitcoin
/+  *test, bolt11
=,  btc
|%
::
++  rhash
  :*  wid=32
      dat=0x1.0203.0405.0607.0809.0001.0203.0405.0607.0809.0001.0203.0405.0607.0809.0102
  ==
++  privkey
  :*  wid=32
      dat=0xe126.f68f.7eaf.cc8b.74f5.4d26.9fe2.06be.7150.00f9.4dac.067d.1c04.a8ca.3b2d.b734
  ==
++  pubkey
  :*  wid=33
      dat=0x3.e715.6ae3.3b0a.208d.0744.1991.6317.7e90.9e80.176e.55d9.7a2f.221e.de0f.934d.d9ad
  ==
++  time  ~2017.6.1..10.57.38
::
+$  bolt11-test-vector  [encoded=@t decoded=invoice:bolt11]
::
++  valid-test-vectors
  :~
    :-  'lnbc1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdpl2pkx2ctnv5sxxmmwwd5kgetjypeh2ursdae8g6twvus8g6rfwvs8qun0dfjkxaq8rkx3yf5tcsyz3d73gafnh3cax9rn449d9p5uxz9ezhhypd0elx87sjle52x86fux2ypatgddc6k63n7erqz25le42c4u4ecky03ylcqca784w'
    %-  invoice:bolt11
    :*  network=%main
        timestamp=time
        payment-hash=rhash
        payment-secret=~
        signature=[v=0 r=0x38ec.6891.345e.2041.45be.8a3a.99de.38e9.8a39.d6a5.6943.4e18.45c8.af72.05af.cfcc s=0x7f42.5fcd.1463.e93c.3288.1ead.0d6e.356d.467e.c8c0.2553.f9aa.b15e.5738.b11f.127f]
        pubkey=pubkey
        expiry=~s3600
        min-final-cltv-expiry=18
        amount=~
        description=(some 'Please consider supporting this project')
        description-hash=~
        unknown-tags=*(map @tD hexb)
        fallback-address=~
        route=~
        feature-bits=0^0b0
    ==
    ::
    ::  please send $3 for a cup of coffee to the same peer, within one minute
    ::
    :-  'lnbc2500u1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdq5xysxxatsyp3k7enxv4jsxqzpuaztrnwngzn3kdzw5hydlzf03qdgm2hdq27cqv3agm2awhz5se903vruatfhq77w3ls4evs3ch9zw97j25emudupq63nyw24cg27h2rspfj9srp'
    %-  invoice:bolt11
    :*  network=%main
        timestamp=time
        payment-hash=rhash
        payment-secret=~
        signature=[v=1 r=0xe896.39ba.6814.e366.89d4.b91b.f125.f103.51b5.5da0.57b0.0647.a8da.baeb.8a90.c95f s=0x160f.9d5a.6e0f.79d1.fc2b.9642.38b9.44e2.fa4a.a677.c6f0.20d4.6647.2ab8.42bd.750e]
        pubkey=pubkey
        expiry=~s60
        min-final-cltv-expiry=18
        amount=(some [2.500 (some %u)])
        description=(some '1 cup coffee')
        description-hash=~
        unknown-tags=*(map @tD hexb)
        fallback-address=~
        route=~
        feature-bits=0^0b0
    ==
    ::
    ::  please send 0.0025 BTC for a cup of nonsense to the same peer, within one minute
    ::
    :-  'lnbc2500u1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdpquwpc4curk03c9wlrswe78q4eyqc7d8d0xqzpuyk0sg5g70me25alkluzd2x62aysf2pyy8edtjeevuv4p2d5p76r4zkmneet7uvyakky2zr4cusd45tftc9c5fh0nnqpnl2jfll544esqchsrny'
    %-  invoice:bolt11
    :*  network=%main
        timestamp=time
        payment-hash=rhash
        payment-secret=~
        signature=[v=0 r=0x259f.0451.1e7e.f2aa.77f6.ff04.d51b.4ae9.2095.0484.3e5a.b967.2ce3.2a15.3681.f687 s=0x515b.73ce.57ee.309d.b588.a10e.b8e4.1b5a.2d2b.c171.44dd.f398.033f.aa49.ffe9.5ae6]
        pubkey=pubkey
        expiry=~s60
        min-final-cltv-expiry=18
        amount=(some [2.500 (some %u)])
        description=(some 'ナンセンス 1杯')
        description-hash=~
        unknown-tags=*(map @tD hexb)
        fallback-address=~
        route=~
        feature-bits=0^0b0
    ==
    ::
    ::  now send $24 for an entire list of things (hashed)
    ::
    :-  'lnbc20m1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqhp58yjmdan79s6qqdhdzgynm4zwqd5d7xmw5fk98klysy043l2ahrqscc6gd6ql3jrc5yzme8v4ntcewwz5cnw92tz0pc8qcuufvq7khhr8wpald05e92xw006sq94mg8v2ndf4sefvf9sygkshp5zfem29trqq2yxxz7'
    %-  invoice:bolt11
    :*  network=%main
        timestamp=time
        payment-hash=rhash
        payment-secret=~
        signature=[v=0 r=0xc634.86e8.1f8c.878a.105b.c9d9.59af.1973.854c.4dc5.52c4.f0e0.e0c7.3896.03d6.bdc6 s=0x7707.bf6b.e992.a8ce.7bf5.0016.bb41.d8a9.b535.8652.c496.0445.a170.d049.ced4.558c]
        pubkey=pubkey
        expiry=~m60
        min-final-cltv-expiry=18
        amount=(some [20 (some %m)])
        description=~
        description-hash=(some [wid=32 dat=0x3925.b6f6.7e2c.3400.36ed.1209.3dd4.4e03.68df.1b6e.a26c.53db.e481.1f58.fd5d.b8c1])
        unknown-tags=*(map @tD hexb)
        fallback-address=~
        route=~
        feature-bits=0^0b0
    ==
    ::
    ::  the same, on testnet, with fallback address mk2QpYatsKicvFVuTAQLBryyccRXMUaGHP
    ::
    :-  'lntb20m1pvjluezhp58yjmdan79s6qqdhdzgynm4zwqd5d7xmw5fk98klysy043l2ahrqspp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqfpp3x9et2e20v6pu37c5d9vax37wxq72un98kmzzhznpurw9sgl2v0nklu2g4d0keph5t7tj9tcqd8rexnd07ux4uv2cjvcqwaxgj7v4uwn5wmypjd5n69z2xm3xgksg28nwht7f6zspwp3f9t'
    %-  invoice:bolt11
    :*  network=%testnet
        timestamp=time
        payment-hash=rhash
        payment-secret=~
        signature=[v=1 r=0xb6c4.2b8a.61e0.dc58.23ea.63e7.6ff1.48ab.5f6c.86f4.5f97.22af.0069.c793.4daf.f70d s=0x5e31.5893.3007.74c8.9799.5e3a.7476.c819.3693.d144.a36e.2645.a085.1e6e.bafc.9d0a]
        pubkey=pubkey
        expiry=~m60
        min-final-cltv-expiry=18
        amount=(some [20 (some %m)])
        description=~
        description-hash=(some [wid=32 dat=0x3925.b6f6.7e2c.3400.36ed.1209.3dd4.4e03.68df.1b6e.a26c.53db.e481.1f58.fd5d.b8c1])
        unknown-tags=*(map @tD hexb)
        fallback-address=(some [%base58 0cmk2QpYatsKicvFVuTAQLBryyccRXMUaGHP])
        route=~
        feature-bits=0^0b0
    ==
    ::
    ::  on mainnet, with fallback address 1RustyRX2oai4EYYDpQGWvEL62BBGqN9T with extra routing info
    ::
    :-  'lnbc20m1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqhp58yjmdan79s6qqdhdzgynm4zwqd5d7xmw5fk98klysy043l2ahrqsfpp3qjmp7lwpagxun9pygexvgpjdc4jdj85fr9yq20q82gphp2nflc7jtzrcazrra7wwgzxqc8u7754cdlpfrmccae92qgzqvzq2ps8pqqqqqqpqqqqq9qqqvpeuqafqxu92d8lr6fvg0r5gv0heeeqgcrqlnm6jhphu9y00rrhy4grqszsvpcgpy9qqqqqqgqqqqq7qqzqj9n4evl6mr5aj9f58zp6fyjzup6ywn3x6sk8akg5v4tgn2q8g4fhx05wf6juaxu9760yp46454gpg5mtzgerlzezqcqvjnhjh8z3g2qqdhhwkj'
    =/  r=(list route:bolt11)
    :~  :*  pubkey=[wid=33 dat=0x2.9e03.a901.b855.34ff.1e92.c43c.7443.1f7c.e720.4606.0fcf.7a95.c37e.148f.78c7.7255]
            short-channel-id=`@ud`0x102.0304.0506.0708
            feebase=1
            feerate=20
            cltv-expiry-delta=3
        ==
        ::
        :*  pubkey=[wid=33 dat=0x3.9e03.a901.b855.34ff.1e92.c43c.7443.1f7c.e720.4606.0fcf.7a95.c37e.148f.78c7.7255]
            short-channel-id=`@ud`0x304.0506.0708.090a
            feebase=2
            feerate=30
            cltv-expiry-delta=4
        ==
    ==
    %-  invoice:bolt11
    :*  network=%main
        timestamp=time
        payment-hash=rhash
        payment-secret=~
        signature=[v=0 r=0x9167.5cb3.fad8.e9d9.1534.3883.a492.42e0.7447.4e26.d42c.7ed9.1465.5689.a807.4553 s=0x733e.8e4e.a5ce.9b85.f69e.40d7.55a5.5014.536b.1232.3f8b.2206.00c9.4ef2.b9c5.1428]
        pubkey=pubkey
        expiry=~m60
        min-final-cltv-expiry=18
        amount=(some [20 (some %m)])
        description=~
        description-hash=(some [wid=32 dat=0x3925.b6f6.7e2c.3400.36ed.1209.3dd4.4e03.68df.1b6e.a26c.53db.e481.1f58.fd5d.b8c1])
        unknown-tags=*(map @tD hexb)
        fallback-address=(some [%base58 0c1RustyRX2oai4EYYDpQGWvEL62BBGqN9T])
        route=r
        feature-bits=0^0b0
    ==
    ::
    ::  on mainnet, with fallback (P2SH) address 3EktnHQD7RiAE6uzMj2ZifT9YgRrkSgzQX
    ::
    :-  'lnbc20m1pvjluezhp58yjmdan79s6qqdhdzgynm4zwqd5d7xmw5fk98klysy043l2ahrqspp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqfppj3a24vwu6r8ejrss3axul8rxldph2q7z9kmrgvr7xlaqm47apw3d48zm203kzcq357a4ls9al2ea73r8jcceyjtya6fu5wzzpe50zrge6ulk4nvjcpxlekvmxl6qcs9j3tz0469gq5g658y'
    %-  invoice:bolt11
    :*  network=%main
        timestamp=time
        payment-hash=rhash
        payment-secret=~
        signature=[v=0 r=0xb6c6.860f.c6ff.41ba.fba1.745b.538b.6a7c.6c2c.0234.f76b.f817.bf56.7be8.8cf2.c632 s=0x492c.9dd2.7947.0841.cd1e.21a3.3ae7.ed59.b258.09bf.9b33.66fe.8188.1651.589f.5d15]
        pubkey=pubkey
        expiry=~m60
        min-final-cltv-expiry=18
        amount=(some [20 (some %m)])
        description=~
        description-hash=(some [wid=32 dat=0x3925.b6f6.7e2c.3400.36ed.1209.3dd4.4e03.68df.1b6e.a26c.53db.e481.1f58.fd5d.b8c1])
        unknown-tags=*(map @tD hexb)
        fallback-address=(some [%base58 0c3EktnHQD7RiAE6uzMj2ZifT9YgRrkSgzQX])
        route=~
        feature-bits=0^0b0
    ==
    ::
    ::  on mainnet, with fallback (P2WPKH) address bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4
    ::
    :-  'lnbc20m1pvjluezhp58yjmdan79s6qqdhdzgynm4zwqd5d7xmw5fk98klysy043l2ahrqspp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqfppqw508d6qejxtdg4y5r3zarvary0c5xw7kepvrhrm9s57hejg0p662ur5j5cr03890fa7k2pypgttmh4897d3raaq85a293e9jpuqwl0rnfuwzam7yr8e690nd2ypcq9hlkdwdvycqa0qza8'
    %-  invoice:bolt11
    :*  network=%main
        timestamp=time
        payment-hash=rhash
        payment-secret=~
        signature=[v=0 r=0xc858.3b8f.6585.3d7c.c90f.0eb4.ae0e.92a6.06f8.9caf.4f7d.6504.8142.d7bb.d4e5.f362 s=0x3ef4.07a7.5458.e4b2.0f00.efbc.734f.1c2e.efc4.19f3.a2be.6d51.0380.16ff.b35c.d613]
        pubkey=pubkey
        expiry=~m60
        min-final-cltv-expiry=18
        amount=(some [20 (some %m)])
        description=~
        description-hash=(some [wid=32 dat=0x3925.b6f6.7e2c.3400.36ed.1209.3dd4.4e03.68df.1b6e.a26c.53db.e481.1f58.fd5d.b8c1])
        unknown-tags=*(map @tD hexb)
        fallback-address=(some [%bech32 'bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4'])
        route=~
        feature-bits=0^0b0
    ==
    ::
    ::  on mainnet, with fallback (P2WSH) address
    ::
    :-  'lnbc20m1pvjluezhp58yjmdan79s6qqdhdzgynm4zwqd5d7xmw5fk98klysy043l2ahrqspp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqfp4qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3q28j0v3rwgy9pvjnd48ee2pl8xrpxysd5g44td63g6xcjcu003j3qe8878hluqlvl3km8rm92f5stamd3jw763n3hck0ct7p8wwj463cql26ava'
    %-  invoice:bolt11
    :*  network=%main
        timestamp=time
        payment-hash=rhash
        payment-secret=~
        signature=[v=0 r=0x51e4.f644.6e41.0a16.4a6d.a9f3.9507.e730.c262.41b4.456a.b6ea.28d1.b12c.71ef.8ca2 s=0xc9c.fe3d.ffc0.7d9f.8db6.71ec.aa4d.20be.edb1.93bd.a8ce.37c5.9f85.f827.73a5.5d47]
        pubkey=pubkey
        expiry=~m60
        min-final-cltv-expiry=18
        amount=(some [20 (some %m)])
        description=~
        description-hash=(some [wid=32 dat=0x3925.b6f6.7e2c.3400.36ed.1209.3dd4.4e03.68df.1b6e.a26c.53db.e481.1f58.fd5d.b8c1])
        unknown-tags=*(map @tD hexb)
        fallback-address=(some [%bech32 'bc1qrp33g0q5c5txsp9arysrx4k6zdkfs4nce4xj0gdcccefvpysxf3qccfmv3'])
        route=~
        feature-bits=0^0b0
    ==
    ::
    ::  please send 0.00967878534 BTC for a list of items within one week, amount in pico-BTC
    ::
    :-  'lnbc9678785340p1pwmna7lpp5gc3xfm08u9qy06djf8dfflhugl6p7lgza6dsjxq454gxhj9t7a0sd8dgfkx7cmtwd68yetpd5s9xar0wfjn5gpc8qhrsdfq24f5ggrxdaezqsnvda3kkum5wfjkzmfqf3jkgem9wgsyuctwdus9xgrcyqcjcgpzgfskx6eqf9hzqnteypzxz7fzypfhg6trddjhygrcyqezcgpzfysywmm5ypxxjemgw3hxjmn8yptk7untd9hxwg3q2d6xjcmtv4ezq7pqxgsxzmnyyqcjqmt0wfjjq6t5v4khxxqyjw5qcqp2rzjq0gxwkzc8w6323m55m4jyxcjwmy7stt9hwkwe2qxmy8zpsgg7jcuwz87fcqqeuqqqyqqqqlgqqqqn3qq9qn07ytgrxxzad9hc4xt3mawjjt8znfv8xzscs7007v9gh9j569lencxa8xeujzkxs0uamak9aln6ez02uunw6rd2ht2sqe4hz8thcdagpleym0j'
    =/  r=(list route:bolt11)
    :~  :*  pubkey=[wid=33 dat=0x3.d067.5858.3bb5.1547.74a6.eb22.1b12.76c9.e82d.65bb.acec.a806.d90e.20c1.08f4.b1c7]
            short-channel-id=648.041.158.511.951.873
            feebase=1.000
            feerate=2.500
            cltv-expiry-delta=40
        ==
    ==
    %-  invoice:bolt11
    :*  network=%main
        timestamp=(from-unix:chrono:userlib 1.572.468.703)
        payment-hash=[wid=32 dat=0x4622.64ed.e7e1.4047.e9b2.49da.94fe.fc47.f41f.7d02.ee9b.0918.15a5.506b.c8ab.f75f]
        payment-secret=~
        signature=[v=1 r=70.554.217.535.581.738.360.942.194.593.908.413.670.549.898.242.039.726.534.639.659.782.031.894.261.747 s=27.187.629.845.978.539.153.383.377.872.603.871.580.687.283.370.554.544.765.072.830.699.443.864.962.805]
        pubkey=pubkey
        expiry=~s604800
        min-final-cltv-expiry=10
        amount=(some [9.678.785.340 (some %p)])
        description=(some 'Blockstream Store: 88.85 USD for Blockstream Ledger Nano S x 1, "Back In My Day" Sticker x 2, "I Got Lightning Working" Sticker x 2 and 1 more items')
        description-hash=~
        unknown-tags=*(map @tD hexb)
        fallback-address=~
        route=r
        feature-bits=0^0b0
    ==
    ::
    ::  please send $30 for coffee beans to the same peer, which supports features 9, 15 and 99, using secret 0x1111111111111111111111111111111111111111111111111111111111111111
    ::
    :-  'lnbc25m1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdq5vdhkven9v5sxyetpdeessp5zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zygs9q5sqqqqqqqqqqqqqqqpqsq67gye39hfg3zd8rgc80k32tvy9xk2xunwm5lzexnvpx6fd77en8qaq424dxgt56cag2dpt359k3ssyhetktkpqh24jqnjyw6uqd08sgptq44qu'
    %-  invoice:bolt11
    :*  network=%main
        timestamp=(from-unix:chrono:userlib 1.496.314.658)
        payment-hash=rhash
        payment-secret=(some [wid=32 dat=0x1111.1111.1111.1111.1111.1111.1111.1111.1111.1111.1111.1111.1111.1111.1111.1111])
        signature=[v=1 r=0xd790.4cc4.b74a.2226.9c68.c1df.68a9.6c21.4d65.1b93.76e9.f164.d360.4da4.b7de.ccce s=0xe82.aaab.4c85.d358.ea14.d0ae.342d.a308.12f9.5d97.6082.eaac.8139.11da.e01a.f3c1]
        pubkey=pubkey
        expiry=~h1
        min-final-cltv-expiry=18
        amount=(some [25 (some %m)])
        description=(some 'coffee beans')
        description-hash=~
        unknown-tags=*(map @tD hexb)
        fallback-address=~
        route=~
        feature-bits=[wid=100 dat=0b1000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.1000.0010.0000.0000]
    ==
    ::
    ::  same but all uppercase
    ::
    :-  'LNBC25M1PVJLUEZPP5QQQSYQCYQ5RQWZQFQQQSYQCYQ5RQWZQFQQQSYQCYQ5RQWZQFQYPQDQ5VDHKVEN9V5SXYETPDEESSP5ZYG3ZYG3ZYG3ZYG3ZYG3ZYG3ZYG3ZYG3ZYG3ZYG3ZYG3ZYG3ZYGS9Q5SQQQQQQQQQQQQQQQPQSQ67GYE39HFG3ZD8RGC80K32TVY9XK2XUNWM5LZEXNVPX6FD77EN8QAQ424DXGT56CAG2DPT359K3SSYHETKTKPQH24JQNJYW6UQD08SGPTQ44QU'
    %-  invoice:bolt11
    :*  network=%main
        timestamp=(from-unix:chrono:userlib 1.496.314.658)
        payment-hash=rhash
        payment-secret=(some [wid=32 dat=0x1111.1111.1111.1111.1111.1111.1111.1111.1111.1111.1111.1111.1111.1111.1111.1111])
        signature=[v=1 r=0xd790.4cc4.b74a.2226.9c68.c1df.68a9.6c21.4d65.1b93.76e9.f164.d360.4da4.b7de.ccce s=0xe82.aaab.4c85.d358.ea14.d0ae.342d.a308.12f9.5d97.6082.eaac.8139.11da.e01a.f3c1]
        pubkey=pubkey
        expiry=~h1
        min-final-cltv-expiry=18
        amount=(some [25 (some %m)])
        description=(some 'coffee beans')
        description-hash=~
        unknown-tags=*(map @tD hexb)
        fallback-address=~
        route=~
        feature-bits=[wid=100 dat=0b1000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.1000.0010.0000.0000]
    ==
    ::
    ::  same, but including fields which must be ignored
    ::
    :-  'lnbc25m1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdq5vdhkven9v5sxyetpdeessp5zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zygs9q5sqqqqqqqqqqqqqqqpqsq2qrqqqfppnqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqppnqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqpp4qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqhpnqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqhp4qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqspnqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqsp4qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqnp5qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqnpkqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq2jxxfsnucm4jf4zwtznpaxphce606fvhvje5x7d4gw7n73994hgs7nteqvenq8a4ml8aqtchv5d9pf7l558889hp4yyrqv6a7zpq9fgpskqhza'
    =/  tags=(map @tD hexb)
      %-  my
      :~  ['n' [wid=33 dat=0x0]]
          ['h' [wid=33 dat=0x0]]
          ['p' [wid=33 dat=0x0]]
          ['s' [wid=33 dat=0x0]]
          ['2' [wid=1 dat=0x0]]
      ==
    %-  invoice:bolt11
    :*  network=%main
        timestamp=(from-unix:chrono:userlib 1.496.314.658)
        payment-hash=rhash
        payment-secret=(some [wid=32 dat=0x1111.1111.1111.1111.1111.1111.1111.1111.1111.1111.1111.1111.1111.1111.1111.1111])
        signature=[v=1 r=0x548c.64c2.7cc6.eb24.d44e.58a6.1e98.37c6.74fd.2597.64b3.4379.b543.bd3f.44a5.add1 s=0xf4d.7903.3330.1fb5.dfcf.d02f.1765.1a50.a7df.a50e.7396.e1a9.0830.335d.f082.02a5]
        pubkey=pubkey
        expiry=~h1
        min-final-cltv-expiry=18
        amount=(some [25 (some %m)])
        description=(some 'coffee beans')
        description-hash=~
        unknown-tags=tags
        fallback-address=~
        route=~
        feature-bits=[wid=100 dat=0b1000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.0000.1000.0010.0000.0000]
    ==
  ==
::
++  invalid-test-vectors
  :~  'lnbc25m1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdq5vdhkven9v5sxyetpdeessp5zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zyg3zygs9q4psqqqqqqqqqqqqqqqpqsqq40wa3khl49yue3zsgm26jrepqr2eghqlx86rttutve3ugd05em86nsefzh4pfurpd9ek9w2vp95zxqnfe2u7ckudyahsa52q66tgzcp6t2dyk'
      'lnbc2500u1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdpquwpc4curk03c9wlrswe78q4eyqc7d8d0xqzpuyk0sg5g70me25alkluzd2x62aysf2pyy8edtjeevuv4p2d5p76r4zkmneet7uvyakky2zr4cusd45tftc9c5fh0nnqpnl2jfll544esqchsrnt'
      'pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdpquwpc4curk03c9wlrswe78q4eyqc7d8d0xqzpuyk0sg5g70me25alkluzd2x62aysf2pyy8edtjeevuv4p2d5p76r4zkmneet7uvyakky2zr4cusd45tftc9c5fh0nnqpnl2jfll544esqchsrny'
      'LNBC2500u1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdpquwpc4curk03c9wlrswe78q4eyqc7d8d0xqzpuyk0sg5g70me25alkluzd2x62aysf2pyy8edtjeevuv4p2d5p76r4zkmneet7uvyakky2zr4cusd45tftc9c5fh0nnqpnl2jfll544esqchsrny'
      'lnbc2500u1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdq5xysxxatsyp3k7enxv4jsxqzpuaxtrnwngzn3kdzw5hydlzf03qdgm2hdq27cqv3agm2awhz5se903vruatfhq77w3ls4evs3ch9zw97j25emudupq63nyw24cg27h2rspk28uwq'
      'lnbc1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdpl2pkx2ctnv5sxxmmwwd5kgetjypeh2ursdae8g6na6hlh'
      'lnbc2500x1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdq5xysxxatsyp3k7enxv4jsxqzpujr6jxr9gq9pv6g46y7d20jfkegkg4gljz2ea2a3m9lmvvr95tq2s0kvu70u3axgelz3kyvtp2ywwt0y8hkx2869zq5dll9nelr83zzqqpgl2zg'
      'lnbc2500000001p1pvjluezpp5qqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqqqsyqcyq5rqwzqfqypqdq5xysxxatsyp3k7enxv4jsxqzpu7hqtk93pkf7sw55rdv4k9z2vj050rxdr6za9ekfs3nlt5lr89jqpdmxsmlj9urqumg0h9wzpqecw7th56tdms40p2ny9q4ddvjsedzcplva53s'
  ==
::
++  test-all-vectors
  ^-  tang
  |^
  ;:  weld
    %+  category  "invoice decoding"
    (zing (turn valid-test-vectors check-decode))
    ::
    %+  category  "invoice encoding"
    (zing (turn valid-test-vectors check-encode))
    ::
    ::  %+  category  "invoice decoding (invalid)"
    ::  (zing (turn invalid-test-vectors check-invalid))
  ==
  ::
  ++  check-decode
    |=  v=bolt11-test-vector
    =/  out=invoice:bolt11  (need (de:bolt11 -.v))
    %+  expect-eq  !>(+.v)  !>(out)
  ::
  ++  check-invalid
    |=  v=@t
    %+  expect-eq  !>(~)  !>((de:bolt11 v))
  ::
  ::  test that we can round-trip, with pubkey recovery.
  ::  it should be the identity, but we have to ignore ecdsa randomness
  ::
  ++  check-encode
    |=  v=bolt11-test-vector
    =/  in=invoice:bolt11  +.v
    =/  chk=invoice:bolt11  +.v
    =.  pubkey.in  0^0x0                   ::  force pubkey to be recovered
    =.  unknown-tags.chk  *(map @tD hexb)  ::  unknown tags not serialized
    =/  en=cord  (en:bolt11 in privkey)
    =/  de=invoice:bolt11  (need (de:bolt11 en))
    =.  signature.chk  signature.de        ::  ignore ECDSA k
    %+  expect-eq  !>(chk)  !>(de)
  --
::
--
