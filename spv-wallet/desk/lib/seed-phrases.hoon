::  Seed Phrases Library
::  Core functionality for BIP39 seed phrase management and validation
::
/+  dict=bip39-english, bip39
|%
++  c
  |%
  +$  eny  ?(%128 %160 %192 %224 %256)
  +$  wor  ?(%12 %15 %18 %21 %24)
  --
::
++  gen-seed
  |=  [ent=@ bits=eny:c]
  ^-  cord
  =/  entropy=@  (end [0 bits] ent)
  =/  byte-count=@ud  (div bits 8)
  =/  entropy-bytes=byts  [byte-count entropy]
  (crip (from-entropy:bip39 entropy-bytes))
::
++  gen-unique
  |=  [ent=@ bits=eny:c existing=(set cord)]
  ^-  cord
  =/  seed=cord  (gen-seed ent bits)
  ?.  (~(has in existing) seed)
    seed
  $(ent (shax ent))
::  BIP39 Validation Functions
::
++  word-count
  |=  seed=cord
  ^-  @ud
  (lent (split-words (trip seed)))
::
++  word-count-to-checksum-bits
  |=  wc=wor:c
  ^-  @ud
  (div wc 3)
::
++  word-count-to-entropy-bytes
  |=  wc=wor:c
  ^-  @ud
  ::  BIP39: entropy bytes = (word_count * 4) / 3
  ::  12 words -> 16 bytes, 15 -> 20, 18 -> 24, 21 -> 28, 24 -> 32
  (div (mul wc 4) 3)
::
++  split-words
  |=  input=tape
  ^-  wall
  (scan input (more ace (star (shim 'a' 'z'))))
::
++  find-word-index
  =|  index=@ud
  |=  word=tape
  ^-  (unit @ud)
  ?~  dict  ~
  ?:  =(word i.dict)
    `index
  $(dict t.dict, index +(index))
::
++  all-words-to-indices
  |=  words=(list tape)
  ^-  (list (unit @ud))
  %+  turn  words
  |=  word=tape
  (find-word-index word)
::
++  indices-to-full-entropy
  |=  indices=(list (unit @ud))
  ^-  (unit @)
  =|  result=@
  |-
  ?~  indices  `result
  ?~  i.indices  ~
  =/  index-11-bits=@  (mod u.i.indices 2.048)
  =/  new-result=@  (add (lsh [0 11] result) index-11-bits)
  $(indices t.indices, result new-result)
::
++  verify-checksum
  |=  [entropy-bits=@ checksum-bits=@ wc=wor:c]
  ^-  ?
  =/  entropy-bytes=@ud  (word-count-to-entropy-bytes wc)
  =/  checksum-size=@ud  (word-count-to-checksum-bits wc)
  =/  hash=@  (sha-256l:sha entropy-bytes entropy-bits)
  =/  expected-checksum=@  (rsh [0 (sub 256 checksum-size)] hash)
  =(checksum-bits expected-checksum)
::
++  words-to-entropy-pair
  |=  words=(list tape)
  ^-  (unit [entropy=@ checksum=@])
  =/  all-indices=(list (unit @ud))  (all-words-to-indices words)
  =/  full-bits=(unit @)  (indices-to-full-entropy all-indices)
  ?~  full-bits  ~
  =/  wc=(unit wor:c)  (mole |.(;;(wor:c (lent words))))
  ?~  wc  ~
  =/  checksum-bits=@ud  (word-count-to-checksum-bits u.wc)
  =/  extracted-entropy=@  (rsh [0 checksum-bits] u.full-bits)
  =/  extracted-checksum=@  (end [0 checksum-bits] u.full-bits)
  ?.  (verify-checksum extracted-entropy extracted-checksum u.wc)
    ~
  `[extracted-entropy extracted-checksum]
::
++  validate-mnemonic
  |=  [words=(list tape) original-entropy=@]
  ^-  (unit [entropy-match=? checksum-valid=? entropy=@ checksum=@])
  =/  extracted=(unit [entropy=@ checksum=@])  (words-to-entropy-pair words)
  ?~  extracted  ~
  =/  wc=(unit wor:c)  (mole |.(;;(wor:c (lent words))))
  ?~  wc  ~
  =/  entropy-match=?  =(entropy.u.extracted original-entropy)
  =/  checksum-valid=?
    (verify-checksum entropy.u.extracted checksum.u.extracted u.wc)
  `[entropy-match checksum-valid u.extracted]
::
++  validate-seed-phrase
  |=  seed=cord
  ^-  ?
  =/  words=(list tape)  (split-words (trip seed))
  =/  wc=(unit wor:c)  (mole |.(;;(wor:c (lent words))))
  ?~  wc  %.n
  =/  entropy-pair=(unit [entropy=@ checksum=@])
    (words-to-entropy-pair words)
  ?=(^ entropy-pair)
--