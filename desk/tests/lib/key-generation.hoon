/-  *bolt
/+  *test
/+  bc=bitcoin, keys=key-generation
=,  secp256k1:secp:crypto
|%
++  public-key
  ^-  pubkey
  %-  decompress-point
  0x2.35f2.dbfa.a89b.57ec.7b05.5afe.2984.9ef7.ddfe.b1ce.fdb9.ebdc.43f5.4949.84db.29e5
::
++  private-key
  ^-  privkey
  0xcbce.d912.d3b2.1bf1.96a7.6665.1e43.6aff.1923.6262.1ce3.1770.4ea2.f75d.87e7.be0f
::
++  revocation-pubkey
  ^-  pubkey
  %-  decompress-point
  0x2.916e.3266.36d1.9c33.f13e.8c0c.3a03.dd15.7f33.2f3e.99c3.17c1.41dd.865e.b01f.8ff0
::
++  revocation-privkey
  ^-  privkey
  0xd09f.fff6.2ddb.2297.ab00.0cc8.5bcb.4283.fdeb.6aa0.52af.fbc9.dddc.f33b.6107.8110
::
++  basepoint-secret
  ^-  privkey
  0x1.0203.0405.0607.0809.0a0b.0c0d.0e0f.1011.1213.1415.1617.1819.1a1b.1c1d.1e1f
::
++  per-commitment-secret
  ^-  privkey
  0x1f1e.1d1c.1b1a.1918.1716.1514.1312.1110.0f0e.0d0c.0b0a.0908.0706.0504.0302.0100
::
++  basepoint
  ^-  point
  %-  decompress-point
  0x3.6d6c.aac2.48af.96f6.afa7.f904.f550.253a.0f3e.f3f5.aa2f.e683.8a95.b216.6914.68e2
::
++  per-commitment-point
  ^-  point
  %-  decompress-point
  0x2.5f71.17a7.8150.fe2e.f97d.b7cf.c83b.d57b.2e2c.0d0d.d25e.af46.7a4a.1c2a.45ce.1486
::
++  check-public-key
  %+  expect-eq
    !>  public-key
    !>  %+  derive-pubkey:keys
          basepoint
        per-commitment-point
::
++  check-private-key
  %+  expect-eq
    !>  private-key
    !>  %^    derive-privkey:keys
            basepoint
          per-commitment-point
        basepoint-secret
::
++  check-revocation-pubkey
  %+  expect-eq
    !>  revocation-pubkey
    !>  %+  derive-revocation-pubkey:keys
          basepoint
        per-commitment-point
::
++  check-revocation-secret
  %+  expect-eq
    !>  revocation-privkey
    !>  %:  derive-revocation-privkey:keys
          revocation-basepoint=basepoint
          revocation-basepoint-secret=basepoint-secret
          per-commitment-point=per-commitment-point
          per-commitment-secret=per-commitment-secret
        ==
--
