/-  ord
/+  *test, cc=gw-btc-pass
|%
++  spawn  ^-  sont:ord  [0xfeed.face 2 17]
++  dat    (make-dat:cc spawn)
++  key
  |=  [seed=@ xtr=@]
  =<  pub:ex
  %:  pit:nu:cric:crypto
      512  (shaz seed)
      %c   dat
      xtr
  ==
::
++  test-dat-roundtrip
  %+  expect-eq
    !>(`(unit [dom=@tas spawn=sont:ord])``[%gw-btc spawn])
    !>((parse-dat:cc dat))
::
++  test-pass-roundtrip
  =/  pas  (key 'gw-btc-pass' 0x1234)
  %+  expect-eq
    !>(`(unit [dom=@tas spawn=sont:ord xtr=@])``[%gw-btc spawn 0x1234])
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
--
