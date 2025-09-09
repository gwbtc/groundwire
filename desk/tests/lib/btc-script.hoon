/+  *test, script=bolt-script
::
|%
::
++  p2pkh
  :~  %op-dup
      %op-hash160
      [%op-pushdata wid=`@ud`0x14 dat=0x89ab.cdef.abba.abba.abba.abba.abba.abba.abba.abba]
      %op-equalverify
      %op-checksig
  ==
::
++  lock-funds
  :~  [%op-pushdata wid=2 dat=0x4242]
      %op-checklocktimeverify
      %op-drop
      %op-dup
      %op-hash160
      [%op-pushdata wid=4 dat=0x4141.4141]
      %op-equalverify
      %op-checksig
   ==
::
++  puzzle
  :~  %op-hash256
      [%op-pushdata wid=32 dat=0x6fe2.8c0a.b6f1.b372.c1a6.a246.ae63.f74f.931e.8365.e15a.089c.68d6.1900.0000.0000]
      %op-equal
  ==
::
++  test-p2pkh
  %+  expect-eq
    !>(0x76.a914.89ab.cdef.abba.abba.abba.abba.abba.abba.abba.abba.88ac)
    !>(dat:(en:script p2pkh))
::
++  test-lock-funds
  %+  expect-eq
    !>(0x242.42b1.7576.a904.4141.4141.88ac)
    !>(dat:(en:script lock-funds))
::
++  test-puzzle
  %+  expect-eq
    !>(0xaa.206f.e28c.0ab6.f1b3.72c1.a6a2.46ae.63f7.4f93.1e83.65e1.5a08.9c68.d619.0000.0000.0087)
    !>(dat:(en:script puzzle))
::
--
