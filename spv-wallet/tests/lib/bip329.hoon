/+  *test, bip329
|%
::  Test atomic origin parsing components
::
++  test-parse-script-type
  ;:  weld
    %+  expect-eq
      !>  'wpkh'
      !>  (crip (scan "wpkh" (star (shim 'a' 'z'))))
    ::
    %+  expect-eq
      !>  'wsh'
      !>  (crip (scan "wsh" (star (shim 'a' 'z'))))
    ::
    %+  expect-eq
      !>  'tr'
      !>  (crip (scan "tr" (star (shim 'a' 'z'))))
  ==
::
::  Test origin-rule comprehensive
::
++  test-parse-origin-wpkh
  =/  origin=@t  'wpkh([d34db33f/84\'/0\'/0\'])'
  =/  result=(unit parsed-origin:bip329)  (rust (trip origin) origin-rule:bip329)
  =/  expected=parsed-origin:bip329
    :*  %wpkh
        0xd34d.b33f
        ~[[%.y 84] [%.y 0] [%.y 0]]
    ==
  %+  expect-eq
    !>  `expected
    !>  result
::
++  test-parse-origin-wsh
  =/  origin=@t  'wsh([a1b2c3d4/49\'/0\'/0\'])'
  =/  result=(unit parsed-origin:bip329)  (rust (trip origin) origin-rule:bip329)
  =/  expected=parsed-origin:bip329
    :*  %wsh
        0xa1b2.c3d4
        ~[[%.y 49] [%.y 0] [%.y 0]]
    ==
  %+  expect-eq
    !>  `expected
    !>  result
::
++  test-parse-origin-tr
  =/  origin=@t  'tr([00112233/86\'/0\'/0\'])'
  =/  result=(unit parsed-origin:bip329)  (rust (trip origin) origin-rule:bip329)
  =/  expected=parsed-origin:bip329
    :*  %tr
        0x11.2233
        ~[[%.y 86] [%.y 0] [%.y 0]]
    ==
  %+  expect-eq
    !>  `expected
    !>  result
::
++  test-parse-origin-sh
  =/  origin=@t  'sh([deadbeef/49\'/0\'/0\'])'
  =/  result=(unit parsed-origin:bip329)  (rust (trip origin) origin-rule:bip329)
  =/  expected=parsed-origin:bip329
    :*  %sh
        0xdead.beef
        ~[[%.y 49] [%.y 0] [%.y 0]]
    ==
  %+  expect-eq
    !>  `expected
    !>  result
::
++  test-parse-origin-pkh
  =/  origin=@t  'pkh([ffffffff/44\'/0\'/0\'])'
  =/  result=(unit parsed-origin:bip329)  (rust (trip origin) origin-rule:bip329)
  =/  expected=parsed-origin:bip329
    :*  %pkh
        0xffff.ffff
        ~[[%.y 44] [%.y 0] [%.y 0]]
    ==
  %+  expect-eq
    !>  `expected
    !>  result
::
++  test-parse-origin-single-segment
  =/  origin=@t  'wpkh([d34db33f/0\'])'
  =/  result=(unit parsed-origin:bip329)  (rust (trip origin) origin-rule:bip329)
  =/  expected=parsed-origin:bip329
    :*  %wpkh
        0xd34d.b33f
        ~[[%.y 0]]
    ==
  %+  expect-eq
    !>  `expected
    !>  result
::
++  test-parse-origin-many-segments
  =/  origin=@t  'wpkh([d34db33f/84\'/0\'/0\'/1\'/2\'/3\'])'
  =/  result=(unit parsed-origin:bip329)  (rust (trip origin) origin-rule:bip329)
  =/  expected=parsed-origin:bip329
    :*  %wpkh
        0xd34d.b33f
        ~[[%.y 84] [%.y 0] [%.y 0] [%.y 1] [%.y 2] [%.y 3]]
    ==
  %+  expect-eq
    !>  `expected
    !>  result
::
++  test-parse-origin-large-indices
  =/  origin=@t  'wpkh([d34db33f/2147483647\'/1000000\'/999999\'])'
  =/  result=(unit parsed-origin:bip329)  (rust (trip origin) origin-rule:bip329)
  =/  expected=parsed-origin:bip329
    :*  %wpkh
        0xd34d.b33f
        ~[[%.y 2.147.483.647] [%.y 1.000.000] [%.y 999.999]]
    ==
  %+  expect-eq
    !>  `expected
    !>  result
::
++  test-parse-origin-hex-case-insensitive
  ;:  weld
    ::  lowercase hex
    %+  expect-eq
      !>  `[%wpkh 0xabcd.ef12 ~[[%.y 84] [%.y 0] [%.y 0]]]
      !>  (rust (trip 'wpkh([abcdef12/84\'/0\'/0\'])') origin-rule:bip329)
    ::  uppercase hex
    %+  expect-eq
      !>  `[%wpkh 0xabcd.ef12 ~[[%.y 84] [%.y 0] [%.y 0]]]
      !>  (rust (trip 'wpkh([ABCDEF12/84\'/0\'/0\'])') origin-rule:bip329)
    ::  mixed case hex
    %+  expect-eq
      !>  `[%wpkh 0xabcd.ef12 ~[[%.y 84] [%.y 0] [%.y 0]]]
      !>  (rust (trip 'wpkh([AbCdEf12/84\'/0\'/0\'])') origin-rule:bip329)
  ==
::
++  test-parse-origin-all-zeros
  =/  origin=@t  'wpkh([00000000/0\'/0\'/0\'])'
  =/  result=(unit parsed-origin:bip329)  (rust (trip origin) origin-rule:bip329)
  =/  expected=parsed-origin:bip329
    :*  %wpkh
        0x0
        ~[[%.y 0] [%.y 0] [%.y 0]]
    ==
  %+  expect-eq
    !>  `expected
    !>  result
::
++  test-parse-origin-all-fs
  =/  origin=@t  'wpkh([ffffffff/4294967295\'/4294967295\'/4294967295\'])'
  =/  result=(unit parsed-origin:bip329)  (rust (trip origin) origin-rule:bip329)
  =/  expected=parsed-origin:bip329
    :*  %wpkh
        0xffff.ffff
        ~[[%.y 4.294.967.295] [%.y 4.294.967.295] [%.y 4.294.967.295]]
    ==
  %+  expect-eq
    !>  `expected
    !>  result
::
::  Invalid origin tests - should all return ~
::
++  test-parse-origin-invalid
  ;:  weld
    ::  completely invalid
    %+  expect-eq
      !>  ~
      !>  (rust (trip 'invalid') origin-rule:bip329)
    ::  missing brackets
    %+  expect-eq
      !>  ~
      !>  (rust (trip 'wpkh[d34db33f/84\'/0\'/0\']') origin-rule:bip329)
    ::  missing parentheses
    %+  expect-eq
      !>  ~
      !>  (rust (trip 'wpkh([d34db33f/84\'/0\'/0\')') origin-rule:bip329)
    ::  fingerprint too short
    %+  expect-eq
      !>  ~
      !>  (rust (trip 'wpkh([d34db3/84\'/0\'/0\']') origin-rule:bip329)
    ::  fingerprint too long
    %+  expect-eq
      !>  ~
      !>  (rust (trip 'wpkh([d34db33f00/84\'/0\'/0\']') origin-rule:bip329)
    ::  missing hardened marker
    %+  expect-eq
      !>  ~
      !>  (rust (trip 'wpkh([d34db33f/84/0/0])') origin-rule:bip329)
    ::  no path segments
    %+  expect-eq
      !>  ~
      !>  (rust (trip 'wpkh([d34db33f])') origin-rule:bip329)
    ::  missing slash before segments
    %+  expect-eq
      !>  ~
      !>  (rust (trip 'wpkh([d34db33f84\'/0\'/0\']') origin-rule:bip329)
    ::  invalid hex character
    %+  expect-eq
      !>  ~
      !>  (rust (trip 'wpkh([g34db33f/84\'/0\'/0\']') origin-rule:bip329)
    ::  empty string
    %+  expect-eq
      !>  ~
      !>  (rust (trip '') origin-rule:bip329)
    ::  script type with uppercase
    %+  expect-eq
      !>  ~
      !>  (rust (trip 'WPKH([d34db33f/84\'/0\'/0\']') origin-rule:bip329)
    ::  trailing slash
    %+  expect-eq
      !>  ~
      !>  (rust (trip 'wpkh([d34db33f/84\'/0\'/0\'/])') origin-rule:bip329)
    ::  double slash
    %+  expect-eq
      !>  ~
      !>  (rust (trip 'wpkh([d34db33f//84\'/0\'/0\']') origin-rule:bip329)
  ==
::
::  Test JSON parsing
::
++  test-parse-entry-tx
  =/  jon=json
    %-  need
    %-  de:json:html
    '''
    {"type":"tx","ref":"f91d0a8a78462bc59398f2c5d7a84fcff491c26ba54c4833478b202796c8aafd","label":"Transaction"}
    '''
  =/  result=(unit label-entry:bip329)  (parse-entry:bip329 jon)
  =/  expected=label-entry:bip329
    :*  %tx
        'f91d0a8a78462bc59398f2c5d7a84fcff491c26ba54c4833478b202796c8aafd'
        'Transaction'
        ~
        ~
    ==
  %+  expect-eq
    !>  `expected
    !>  result
::
++  test-parse-entry-addr
  =/  jon=json
    %-  need
    %-  de:json:html
    '''
    {"type":"addr","ref":"bc1q34aq5drpuwy3wgl9lhup9892qp6svr8ldzyy7c","label":"Address"}
    '''
  =/  result=(unit label-entry:bip329)  (parse-entry:bip329 jon)
  =/  expected=label-entry:bip329
    :*  %addr
        'bc1q34aq5drpuwy3wgl9lhup9892qp6svr8ldzyy7c'
        'Address'
        ~
        ~
    ==
  %+  expect-eq
    !>  `expected
    !>  result
::
++  test-parse-entry-pubkey
  =/  jon=json
    %-  need
    %-  de:json:html
    '''
    {"type":"pubkey","ref":"0283409659355b6d1cc3c32decd5d561abaac86c37a353b52895a5e6c196d6f448","label":"Public Key"}
    '''
  =/  result=(unit label-entry:bip329)  (parse-entry:bip329 jon)
  =/  expected=label-entry:bip329
    :*  %pubkey
        '0283409659355b6d1cc3c32decd5d561abaac86c37a353b52895a5e6c196d6f448'
        'Public Key'
        ~
        ~
    ==
  %+  expect-eq
    !>  `expected
    !>  result
::
++  test-parse-entry-with-origin
  =/  jon=json
    %-  need
    %-  de:json:html
    '''
    {"type":"addr","ref":"bc1q34aq5drpuwy3wgl9lhup9892qp6svr8ldzyy7c","label":"Address","origin":"wpkh([d34db33f/84'/0'/0'])"}
    '''
  =/  result=(unit label-entry:bip329)  (parse-entry:bip329 jon)
  =/  expected=label-entry:bip329
    :*  %addr
        'bc1q34aq5drpuwy3wgl9lhup9892qp6svr8ldzyy7c'
        'Address'
        `[%wpkh 0xd34d.b33f ~[[%.y 84] [%.y 0] [%.y 0]]]
        ~
    ==
  %+  expect-eq
    !>  `expected
    !>  result
::
++  test-parse-entry-with-spendable
  =/  jon=json
    %-  need
    %-  de:json:html
    '''
    {"type":"addr","ref":"bc1q34aq5drpuwy3wgl9lhup9892qp6svr8ldzyy7c","label":"Address","spendable":true}
    '''
  =/  result=(unit label-entry:bip329)  (parse-entry:bip329 jon)
  =/  expected=label-entry:bip329
    :*  %addr
        'bc1q34aq5drpuwy3wgl9lhup9892qp6svr8ldzyy7c'
        'Address'
        ~
        `%.y
    ==
  %+  expect-eq
    !>  `expected
    !>  result
::
::  Test origin-to-cord serialization
::
++  test-origin-to-cord-wpkh
  =/  origin=parsed-origin:bip329
    [%wpkh 0xd34d.b33f ~[[%.y 84] [%.y 0] [%.y 0]]]
  =/  result=@t  (origin-to-cord:bip329 origin)
  %+  expect-eq
    !>  'wpkh([d34db33f/84\'/0\'/0\'])'
    !>  result
::
++  test-origin-to-cord-leading-zeros
  =/  origin=parsed-origin:bip329
    [%tr 0x11.2233 ~[[%.y 86] [%.y 0] [%.y 0]]]
  =/  result=@t  (origin-to-cord:bip329 origin)
  %+  expect-eq
    !>  'tr([00112233/86\'/0\'/0\'])'
    !>  result
::
++  test-origin-to-cord-all-zeros
  =/  origin=parsed-origin:bip329
    [%wpkh 0x0 ~[[%.y 0] [%.y 0] [%.y 0]]]
  =/  result=@t  (origin-to-cord:bip329 origin)
  %+  expect-eq
    !>  'wpkh([00000000/0\'/0\'/0\'])'
    !>  result
::
++  test-origin-to-cord-single-segment
  =/  origin=parsed-origin:bip329
    [%wsh 0xa1b2.c3d4 ~[[%.y 49]]]
  =/  result=@t  (origin-to-cord:bip329 origin)
  %+  expect-eq
    !>  'wsh([a1b2c3d4/49\'])'
    !>  result
::
++  test-origin-to-cord-many-segments
  =/  origin=parsed-origin:bip329
    [%pkh 0xdead.beef ~[[%.y 44] [%.y 1] [%.y 2] [%.y 3] [%.y 4]]]
  =/  result=@t  (origin-to-cord:bip329 origin)
  %+  expect-eq
    !>  'pkh([deadbeef/44\'/1\'/2\'/3\'/4\'])'
    !>  result
::
++  test-origin-roundtrip-wpkh
  =/  original=@t  'wpkh([d34db33f/84\'/0\'/0\'])'
  =/  parsed=(unit parsed-origin:bip329)  (rust (trip original) origin-rule:bip329)
  =/  serialized=@t  (origin-to-cord:bip329 (need parsed))
  %+  expect-eq
    !>  original
    !>  serialized
::
++  test-origin-roundtrip-tr
  =/  original=@t  'tr([00112233/86\'/0\'/0\'])'
  =/  parsed=(unit parsed-origin:bip329)  (rust (trip original) origin-rule:bip329)
  =/  serialized=@t  (origin-to-cord:bip329 (need parsed))
  %+  expect-eq
    !>  original
    !>  serialized
::
++  test-origin-roundtrip-all-zeros
  =/  original=@t  'wpkh([00000000/0\'/0\'/0\'])'
  =/  parsed=(unit parsed-origin:bip329)  (rust (trip original) origin-rule:bip329)
  =/  serialized=@t  (origin-to-cord:bip329 (need parsed))
  %+  expect-eq
    !>  original
    !>  serialized
::
++  test-origin-roundtrip-all-fs
  =/  original=@t  'sh([ffffffff/4294967295\'/4294967295\'/4294967295\'])'
  =/  parsed=(unit parsed-origin:bip329)  (rust (trip original) origin-rule:bip329)
  =/  serialized=@t  (origin-to-cord:bip329 (need parsed))
  %+  expect-eq
    !>  original
    !>  serialized
::
::  Test JSON serialization
::
++  test-entry-to-json-basic
  =/  entry=label-entry:bip329
    :*  %tx
        'f91d0a8a78462bc59398f2c5d7a84fcff491c26ba54c4833478b202796c8aafd'
        'Transaction'
        ~
        ~
    ==
  =/  result=json  (entry-to-json:bip329 entry)
  =/  expected=json
    %-  pairs:enjs:format
    :~  ['type' s+'tx']
        ['ref' s+'f91d0a8a78462bc59398f2c5d7a84fcff491c26ba54c4833478b202796c8aafd']
        ['label' s+'Transaction']
    ==
  %+  expect-eq
    !>  expected
    !>  result
--
