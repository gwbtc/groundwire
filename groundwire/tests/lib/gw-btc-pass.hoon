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
    (expect-eq !>(%.y) !>((verify-dat:cc dat [spawn blind])))
    (expect-eq !>(%.n) !>((verify-dat:cc dat [spawn +(blind)])))
    (expect-eq !>(%.n) !>((verify-dat:cc dat [[txid.spawn 2 off.spawn] blind])))
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
    !>((parse-publication:cc [5 0x6a6a.6a6a.6a]))
--
