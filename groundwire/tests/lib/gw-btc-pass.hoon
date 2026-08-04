::  tests/lib/gw-btc-pass.hoon
::
::  Golden values are pinned by /vectors/gw-kelvin-9.json and were
::  cross-checked against an independent python implementation of the
::  BIP-340 tagged hashes.  Any change here is a protocol change.
::
/-  ord, sa=self-attestation
/+  *test, cc=gw-btc-pass, taproot, btc=bitcoin
|%
++  spawn  ^-  sont:ord  [txid=0x1234.5678.9abc.def0 vout=1 off=0]
++  seed   0xdead.beef
++  blind  (make-blind:cc seed)
++  dat    (make-dat:cc spawn blind)
++  snap   ^-  snapshot:sa  [life=1 rift=0 key=0xabcd sponsor=~ fief=~]
++  key
  |=  [sed=@ xtr=@]
  =<  pub:ex
  %:  pit:nu:cric:crypto
      512  (shaz sed)
      %c   dat
      xtr
  ==
::
++  test-golden-blind
  %+  expect-eq
    !>  0xf0de.dc6a.72ec.b8c1.6b5d.af25.2b8d.b53b.2510.4f32.c2d9.e9bc.3459.db71.6535.ef2e
    !>  blind
::
++  test-golden-spawn-commit
  %+  expect-eq
    !>  0x134a.ab80.f09d.97d8.0efc.2573.570e.2767.279a.ad7b.43ea.2740.18a7.570d.26fc.57e0
    !>  (spawn-commit:cc spawn blind)
::
++  test-golden-dat
  %+  expect-eq
    !>  0x2.6955.701e.13b2.fb01.df84.ae6a.e1c4.ece4.f355.af68.7d44.e803.14ea.e1a4.df8a.fc12.4637.4622.d776.77c0
    !>  `@ux`dat
::
++  test-golden-state-commit
  %+  expect-eq
    !>  0xc31d.0c5c.e2b3.5a94.79ce.af7b.284a.fac8.e955.324c.0b40.59f6.bdc8.b182.22aa.ba75
    !>  (state-commit:cc snap)
::
++  test-golden-state-leaf
  %+  expect-eq
    !>  ^-  tapleaf:taproot
        :+  0xc0  37
        0x6a.0267.7720.c31d.0c5c.e2b3.5a94.79ce.af7b.284a.fac8.e955.324c.0b40.59f6.bdc8.b182.22aa.ba75
    !>  (state-leaf:cc (state-commit:cc snap))
::
++  test-golden-state-key
  ::  P = 02 || secp generator x; Q = +state-key over the basic snapshot
  =/  p  0x2.79be.667e.f9dc.bbac.55a0.6295.ce87.0b07.029b.fcdb.2dce.28d9.59f2.815b.16f8.1798
  %+  expect-eq
    !>  0xc136.d880.9683.7f8d.167e.6141.555a.6e1d.0068.e24b.7df6.a973.7cea.44c8.29ab.1e7b
    !>  (state-key:cc p snap)
::
++  test-dat-roundtrip
  %+  expect-eq
    !>(`(unit [dom=@tas kel=@ud d=@ux])``[%gw-btc 9 (spawn-commit:cc spawn blind)])
    !>((parse-dat:cc dat))
::
++  test-dat-rejects-trailing
  %+  expect-eq
    !>(*(unit [dom=@tas kel=@ud d=@ux]))
    !>((parse-dat:cc (can 0 ~[[(met 0 dat) dat] [8 0xab]])))
::
++  test-verify-dat
  ;:  weld
    (expect-eq !>(%.y) !>((verify-dat:cc dat [spawn 0 blind])))
    (expect-eq !>(%.n) !>((verify-dat:cc dat [spawn 0 +(blind)])))
    (expect-eq !>(%.n) !>((verify-dat:cc dat [[0x1234.5678.9abc.def0 2 0] 0 blind])))
  ==
::
++  test-pass-roundtrip
  =/  pas  (key 'gw-btc-pass' 0x1234)
  %+  expect-eq
    !>  ^-  (unit [dom=@tas kel=@ud d=@ux xtr=@])
        `[%gw-btc 9 (spawn-commit:cc spawn blind) 0x1234]
    !>((parse-pass:cc pas))
::
++  test-same-key-ignores-xtr
  %+  expect-eq
    !>(%.y)
    !>((same-key:cc (key 'same' 0x1) (key 'same' 0x2)))
::
++  test-same-key-rejects-other-key
  %+  expect-eq
    !>(%.n)
    !>((same-key:cc (key 'one' 0x1) (key 'two' 0x1)))
::
++  test-same-key-rejects-malformed-pass
  ;:  weld
    (expect-eq !>(%.n) !>((same-key:cc 42 (key 'valid' 0x1))))
    (expect-eq !>(%.n) !>((same-key:cc (key 'valid' 0x1) 42)))
  ==
::
++  test-publication-roundtrip-small
  =/  payload=hexb:btc  [4 0xcafe.babe]
  %+  expect-eq
    !>(`(unit [kel=@ud payload=hexb:btc])``[9 payload])
    !>((parse-publication:cc (publication-script:cc payload)))
::
++  test-publication-roundtrip-pushdata
  =/  payload=hexb:btc  [100 (fil 3 100 0xab)]
  %+  expect-eq
    !>(`(unit [kel=@ud payload=hexb:btc])``[9 payload])
    !>((parse-publication:cc (publication-script:cc payload)))
::
++  test-publication-rejects-garbage
  %+  expect-eq
    !>(*(unit [kel=@ud payload=hexb:btc]))
    !>((parse-publication:cc [5 0x6a.6a6a.6a6a]))
::  ------------------------------------------------------------------
::  Publication payload BYTE ORDER.
::
::  The payload is the jam's ordinary LITTLE-endian byte dump (+jam-octs),
::  the same convention as every hash preimage in this lib and as
::  Causeway's jam_bytes.  The shipped encoder/decoder instead used the raw
::  jam atom as a big-endian byte string; that pair was self-consistent and
::  inconsistent with everything else, so the desk could neither write a
::  publication Causeway could read nor read a real one off the chain.
::
::  +test-publication-real-onchain is the one that matters: it decodes the
::  ACTUAL mainnet OP_RETURN of the public comet
::  ~ligdes-risbur-folmus-mattyp--firpec-lispec-noddyl-daplyd (spawn
::  ec5c1fbe... in block 961.059) and re-derives the name, the spawn
::  satpoint and the sat output's P2TR key from it alone.
::  ------------------------------------------------------------------
++  test-publication-payload-is-jam-octs
  =/  pub  ^-(publication:sa [(key 'pubkey' 0) [0x2 snap ~]])
  =/  script  (make-publication:cc pub)
  =/  env  (parse-publication:cc script)
  ;:  weld
    (expect !>(?=(^ env)))
    ::  the payload IS +jam-octs, not the raw jam atom read big-endian
    (expect-eq !>((jam-octs:cc pub)) !>(payload:(need env)))
    ::  ... and reading it back reproduces the publication exactly
    (expect-eq !>(`(unit publication:sa)``pub) !>((read-publication:cc script)))
  ==
::
++  c3-onchain-opret
  ^-  hexb:btc
  :-  263
  0x6a.0375.7262.0109.4cfe.01e0.d7b1.e431.8330.b6a4.cbf6.1189.b748.431a.
    2f0d.b8d5.2c9d.5a33.a360.b913.daec.f231.3fac.2b1c.523e.5eb8.af73.6889.
    af70.cab7.8248.c063.3675.d362.06af.effe.b27a.194b.986f.008b.80ef.ecae.
    458c.6e8c.2405.5937.5259.bd9d.df4e.c3e0.104e.c2b4.931a.7bcc.f74d.2dea.
    253c.ed83.3454.eaa3.6900.1418.b843.cfe7.6c4d.bfa4.06af.b441.8126.649f.
    5346.aba7.318c.f0e1.1467.2d5d.7f4f.09b7.3803.20c0.150e.291f.2fdc.d739.
    b4c4.5738.e55b.4124.e031.9bba.6931.83d7.777f.59bd.8c25.cc77.9a05.e09f.
    8a37.559b.eece.5d7e.3f66.0661.aad9.6486.3c4f.6444.b4e1.3f7c.de42.6863.
    5981.461e.9b00.0448.208c.b04b.69dd.4170.9a02.b357.5877.1ba7.b840.3e86.
    f2d1.1450.f26b.623b.bd79.5d07
::
++  test-publication-real-onchain
  =/  pub  (read-publication:cc c3-onchain-opret)
  ?~  pub
    (expect-eq !>('decodes') !>('SHIPPED DECODER CANNOT READ A REAL PUBLICATION'))
  =*  o    opening.u.pub
  =/  cic  (com:nu:cric:crypto pass.u.pub)
  ?.  ?=(%c suite.+<.cic)
    (expect-eq !>('suite-c') !>('published pass is not suite-C'))
  ;:  weld
    ::  the published pass fingerprints to the comet's real @p
    %+  expect-eq
      !>  `@p`~ligdes-risbur-folmus-mattyp--firpec-lispec-noddyl-daplyd
      !>  `@p`fig:ex:cic
    ::  the blind-opening opens that pass's own hiding dat commitment
    (expect !>(?=(^ blind-opening.o)))
    (expect !>((verify-dat:cc dat.tw.pub.+<.cic (need blind-opening.o))))
    ::  ... to the real spawn satpoint 72340acb...:1
    %+  expect-eq
      !>  ^-  sont:ord
          :+  0x7234.0acb.1b42.16f3.e1ff.0da2.2322.79e4.3326.cd53.0833.31fb.f2ee.7774.daa9.bc54
            1
          0
      !>  spawn:(need blind-opening.o)
    ::  the published snapshot is life 1 / rift 0 with the messaging key
    %+  expect-eq
      !>  ^-  snapshot:sa
          :*  life=1
              rift=0
              key=0xdf30.9632.f565.fddf.5e0c.c5a6.ea6c.c780.9105.6f94.e15f.12d0.e75f.70bc.7ca4.3857
              sponsor=~
              fief=~
          ==
      !>  snapshot.o
    ::  and it recomputes the sat output's on-chain P2TR key exactly
    %+  expect-eq
      !>  0xca2.828c.764a.0f3e.76e3.3df0.7be9.8703.06ec.5650.af8e.cae8.8f6f.0c64.2ec8.0bee
      !>  (state-key:cc internal-key.o snapshot.o)
  ==
--
