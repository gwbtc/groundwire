::  Helpers for the suite-%c pass format owned by the %gw-btc PKI domain.
::
::  The immutable tweak data is:
::
::      (cat 0 (mat %gw-btc) (jam spawn-sont))
::
::  Ames reads only the leading mat to route the pass to %gw-btc.  This
::  library additionally decodes the committed spawn satpoint.  The pass's
::  xtr tail is deliberately excluded from the key tweak and may therefore
::  grow without changing the comet's name.
::
/-  ord
|%
++  domain  %gw-btc
::
++  make-dat
  |=  spawn=sont:ord
  ^-  @
  =/  head  (mat domain)
  (cat 0 q.head (jam spawn))
::
++  parse-dat
  |=  dat=@
  ^-  (unit [dom=@tas spawn=sont:ord])
  %-  mole
  |.
  =/  head  (rub 0 dat)
  =/  dom=@tas  `@tas`q.head
  =/  spawn=sont:ord  ;;(sont:ord (cue (rsh [0 p.head] dat)))
  [dom spawn]
::
++  parse-pass
  |=  =pass
  ^-  (unit [dom=@tas spawn=sont:ord xtr=@])
  %-  mole
  |.
  =/  cic  (com:nu:cric:crypto pass)
  ?>  ?=(%c suite.+<.cic)
  =/  meta  (need (parse-dat dat.tw.pub.+<.cic))
  [dom.meta spawn.meta xtr.tw.pub.+<.cic]
::
::  Equality of the cryptographic key material while deliberately ignoring
::  xtr.  This is the comparison needed between an on-chain state pass and a
::  freshly-carried self-attestation pass.
++  same-key
  |=  [a=pass b=pass]
  ^-  ?
  =/  same=(unit ?)
    %-  mole
    |.
    =/  ca  (com:nu:cric:crypto a)
    =/  cb  (com:nu:cric:crypto b)
    ?.  &(?=(%c suite.+<.ca) ?=(%c suite.+<.cb))  |
    ?&  =(ugn.tw.pub.+<.ca ugn.tw.pub.+<.cb)
        =(cry.pub.+<.ca cry.pub.+<.cb)
        =(dat.tw.pub.+<.ca dat.tw.pub.+<.cb)
    ==
  ?~(same | u.same)
--
