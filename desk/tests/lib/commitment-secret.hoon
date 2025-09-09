/-  *bolt
/+  *test, secret=commitment-secret
|%
++  check-zero-final-node
  %+  expect-eq
    !>  [32 0x2a4.0c85.b6f2.8da0.8dfd.be09.26c5.3fab.2de6.d28c.1030.1f8f.7c40.73d5.e42e.3148]
    !>  %^    generate-from-seed:secret
            [32 0x0]
          281.474.976.710.655
        ~
::
++  check-fs-final-node
  %+  expect-eq
    !>  [32 0x7cc8.54b5.4e3e.0dcd.b010.d7a3.fee4.64a9.687b.e6e8.db3b.e685.4c47.5621.e007.a5dc]
    !>  %^    generate-from-seed:secret
            :-  32
            0xffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff
          281.474.976.710.655
        ~
::
++  check-fs-alternate-bits-1
  %+  expect-eq
    !>  [32 0x56f4.008f.b007.ca9a.cf0e.15b0.54d5.c9fd.12ee.06ce.a347.914d.dbae.d70d.1c13.a528]
    !>  %^    generate-from-seed:secret
            :-  32
            0xffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff
          0xaaa.aaaa.aaaa
        ~
::
++  check-fs-alternate-bits-2
  %+  expect-eq
    !>  [32 0x9015.daae.b06d.ba4c.cc05.b91b.2f73.bd54.405f.2be9.f217.fbac.d3c5.ac2e.6232.7d31]
    !>  %^    generate-from-seed:secret
            :-  32
            0xffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff
          0x5555.5555.5555
        ~
::
++  check-last-nontrivial
  %+  expect-eq
    !>  [32 0x915c.7594.2a26.bb3a.433a.8ce2.cb04.27c2.9ec6.c177.5cfc.7832.8b57.f6ba.7bfe.aa9c]
    !>  %^    generate-from-seed:secret
            :-  32
            0x101.0101.0101.0101.0101.0101.0101.0101.0101.0101.0101.0101.0101.0101.0101.0101
          1
        ~
--
