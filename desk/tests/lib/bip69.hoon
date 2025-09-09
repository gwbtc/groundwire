/+  *test, bip69, psbt
|%
++  test-sort-inputs
  =|  a=in:tx:psbt
  =|  b=in:tx:psbt
  =|  c=in:tx:psbt
  =.  txid.prevout.a
    :-  32
    0xe53.ec5d.fb2c.b8a7.1fec.32dc.9a63.4a35.b7e2.4799.295d.dd52.7821.7822.e0b3.1f57
  =.  idx.prevout.a  0
  =.  txid.prevout.b
    :-  32
    0x54ff.ff18.2965.ed09.57db.a123.9c27.164a.ce5a.73c9.b62a.660c.74b7.b7f1.5ff6.1e7a
  =.  idx.prevout.b  0
  =.  txid.prevout.c  txid.prevout.b
  =.  idx.prevout.c  1
  %+  expect-eq
    !>(~[a b c])
    !>((sort-inputs:bip69 ~[b a c]))
--
