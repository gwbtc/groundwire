/+  *test, seed=seed-phrases
|%
::  Test that generated seeds can be validated
++  test-gen-and-validate
  ;:  weld
  ::  Test 12-word seeds (128 bits) - 3 different entropy values
  %+  expect-eq
    !>  %.y
    !>  =/  test-entropy=@  0xabcd.1234.5678.90ab.cdef.1234.5678.90ab
        =/  generated=cord  (gen-seed:seed test-entropy %128)
        (validate-seed-phrase:seed generated)
  ::
  %+  expect-eq
    !>  %.y
    !>  =/  test-entropy=@  0x1
        =/  generated=cord  (gen-seed:seed test-entropy %128)
        (validate-seed-phrase:seed generated)
  ::
  %+  expect-eq
    !>  %.y
    !>  =/  test-entropy=@  0xffff.ffff.ffff.ffff.ffff.ffff.ffff.ffff
        =/  generated=cord  (gen-seed:seed test-entropy %128)
        (validate-seed-phrase:seed generated)
  ::
  ::  Test 15-word seeds (160 bits) - 3 different entropy values
  %+  expect-eq
    !>  %.y
    !>  =/  test-entropy=@  0x1234.5678.90ab.cdef.1234.5678.90ab.cdef.1234.5678
        =/  generated=cord  (gen-seed:seed test-entropy %160)
        (validate-seed-phrase:seed generated)
  ::
  %+  expect-eq
    !>  %.y
    !>  =/  test-entropy=@  0xdead.beef.cafe.babe.1234.5678.90ab.cdef.fedc.ba98
        =/  generated=cord  (gen-seed:seed test-entropy %160)
        (validate-seed-phrase:seed generated)
  ::
  %+  expect-eq
    !>  %.y
    !>  =/  test-entropy=@  0x5555.5555.5555.5555.5555.5555.5555.5555.5555.5555
        =/  generated=cord  (gen-seed:seed test-entropy %160)
        (validate-seed-phrase:seed generated)
  ::
  ::  Test 18-word seeds (192 bits) - 3 different entropy values
  %+  expect-eq
    !>  %.y
    !>  =/  test-entropy=@  0xabcd.1234.5678.90ab.cdef.1234.5678.90ab.cdef.1234.5678.90ab
        =/  generated=cord  (gen-seed:seed test-entropy %192)
        (validate-seed-phrase:seed generated)
  ::
  %+  expect-eq
    !>  %.y
    !>  =/  test-entropy=@  0x1111.2222.3333.4444.5555.6666.7777.8888.9999.aaaa.bbbb.cccc
        =/  generated=cord  (gen-seed:seed test-entropy %192)
        (validate-seed-phrase:seed generated)
  ::
  %+  expect-eq
    !>  %.y
    !>  =/  test-entropy=@  0xaaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa.aaaa
        =/  generated=cord  (gen-seed:seed test-entropy %192)
        (validate-seed-phrase:seed generated)
  ::
  ::  Test 21-word seeds (224 bits) - 3 different entropy values
  %+  expect-eq
    !>  %.y
    !>  =/  test-entropy=@  0x1234.5678.90ab.cdef.1234.5678.90ab.cdef.1234.5678.90ab.cdef.1234
        =/  generated=cord  (gen-seed:seed test-entropy %224)
        (validate-seed-phrase:seed generated)
  ::
  %+  expect-eq
    !>  %.y
    !>  =/  test-entropy=@  0xfeed.face.dead.beef.cafe.babe.c0de.d00d.1337.beef.dead.c0de.fade
        =/  generated=cord  (gen-seed:seed test-entropy %224)
        (validate-seed-phrase:seed generated)
  ::
  %+  expect-eq
    !>  %.y
    !>  =/  test-entropy=@  0x3333.3333.3333.3333.3333.3333.3333.3333.3333.3333.3333.3333.3333
        =/  generated=cord  (gen-seed:seed test-entropy %224)
        (validate-seed-phrase:seed generated)
  ::
  ::  Test 24-word seeds (256 bits) - 3 different entropy values
  %+  expect-eq
    !>  %.y
    !>  =/  test-entropy=@  0xabcd.1234.5678.90ab.cdef.1234.5678.90ab.1234.5678.90ab.cdef.abcd.1234.5678.90ab
        =/  generated=cord  (gen-seed:seed test-entropy %256)
        (validate-seed-phrase:seed generated)
  ::
  %+  expect-eq
    !>  %.y
    !>  =/  test-entropy=@  0x123.4567.89ab.cdef.0123.4567.89ab.cdef.0123.4567.89ab.cdef.0123.4567.89ab.cdef
        =/  generated=cord  (gen-seed:seed test-entropy %256)
        (validate-seed-phrase:seed generated)
  ::
  %+  expect-eq
    !>  %.y
    !>  =/  test-entropy=@  0x8888.8888.8888.8888.8888.8888.8888.8888.8888.8888.8888.8888.8888.8888.8888.8888
        =/  generated=cord  (gen-seed:seed test-entropy %256)
        (validate-seed-phrase:seed generated)
  ==
::
::  Test with known BIP39 test vector
::  Entropy: 0x00000000000000000000000000000000
::  Expected mnemonic: "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about"
++  test-known-vector
  %+  expect-eq
    !>  %.y
    !>  =/  known-seed=cord  'abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about'
        (validate-seed-phrase:seed known-seed)
::
::  Test that validation correctly rejects invalid seeds
++  test-invalid-seed
  %+  expect-eq
    !>  %.n
    !>  =/  invalid-seed=cord  'abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon'
        (validate-seed-phrase:seed invalid-seed)
::
::  BIP39 test vector: all 1s entropy (128 bits)
++  test-known-vector-all-ones-128
  %+  expect-eq
    !>  %.y
    !>  (validate-seed-phrase:seed 'zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo wrong')
::
::  BIP39 test vector: all 1s entropy (256 bits)
++  test-known-vector-all-ones-256
  %+  expect-eq
    !>  %.y
    !>  (validate-seed-phrase:seed 'zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo when')
::
::  BIP39 test vector: 0x7f7f... entropy
++  test-known-vector-7f
  %+  expect-eq
    !>  %.y
    !>  (validate-seed-phrase:seed 'legal winner thank year wave sausage worth useful legal winner thank yellow')
::
::  Invalid word not in BIP39 wordlist
++  test-invalid-word
  %+  expect-eq
    !>  %.n
    !>  (validate-seed-phrase:seed 'abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon notaword')
::
::  Wrong word count: too few
++  test-wrong-word-count-few
  %+  expect-eq
    !>  %.n
    !>  (validate-seed-phrase:seed 'abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon')
::
::  Wrong word count: too many
++  test-wrong-word-count-many
  %+  expect-eq
    !>  %.n
    !>  (validate-seed-phrase:seed 'abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about abandon')
::
::
::  Test gen-seed is deterministic
++  test-gen-seed-deterministic
  %+  expect-eq
    !>  (gen-seed:seed 0xabcd.1234.abcd.1234.abcd.1234.abcd.1234 %128)
    !>  (gen-seed:seed 0xabcd.1234.abcd.1234.abcd.1234.abcd.1234 %128)
::
::  Test word-count function
++  test-word-count
  ;:  weld
  %+  expect-eq
    !>  12
    !>  (word-count:seed 'abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about')
  %+  expect-eq
    !>  18
    !>  (word-count:seed 'zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo zoo when')
  ==
::
::  Test checksum failure with wrong middle word
++  test-checksum-fail-middle
  %+  expect-eq
    !>  %.n
    !>  (validate-seed-phrase:seed 'abandon abandon zoo abandon abandon abandon abandon abandon abandon abandon abandon about')
--
