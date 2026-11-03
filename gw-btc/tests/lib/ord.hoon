/-  ord
/+  *test, ol=ord
|%
++  test-del-last-sat-removes-correct-vout
  =/  txid=@ux  0xaaaa.bbbb
  =/  keep-vout=@ud  0
  =/  del-vout=@ud   2
  =/  off=@ud        0
  =/  before=sont-map:ord
    =/  m  (put-com:si:ol *sont-map:ord txid keep-vout off 1.000 ~zod)
    (put-com:si:ol m txid del-vout off 1.000 ~nec)
  =/  after=sont-map:ord
    (del:si:ol before txid del-vout off)
  ;:  weld
    (expect !>(?=(^ (get-vout:si:ol after txid keep-vout))))
    (expect !>(?=(~ (get-vout:si:ol after txid del-vout))))
    (expect !>(=(1 ~(wyt by after))))
  ==
::
++  test-can-put-com-rejects-different-occupant
  =/  txid=@ux  0xaaaa.bbbb
  =/  vout=@ud  2
  =/  off=@ud  17
  =/  state=sont-map:ord
    (put-com:si:ol *sont-map:ord txid vout off 1.000 ~zod)
  ;:  weld
    (expect !>((can-put-com:si:ol state txid vout off ~zod)))
    (expect !>(=(| (can-put-com:si:ol state txid vout off ~nec))))
    (expect !>((can-put-com:si:ol state txid vout +(off) ~nec)))
  ==
--
