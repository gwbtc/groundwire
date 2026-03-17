:: adapted from ~lonhep-tamfeb (Yun)'s
::
/-  tx=transactions, urb
/+  urb-encoder, gw=groundwire, bscr=btc-script
::
++  build-spawn-output
|=  [=input:tx comet=@p int-pubkey=@ux]  ::  int-pubkey=internal pubkey
::  scry my own keys, this will only work if the comet is us
=/  [=life pubkey=pass sign=(unit @uv)]  
  .^([life pass (unit @ux)] %j /(scot %p comet)/deed/(scot %da now.bowl)/(scot %p comet)/1)
=/  cic  (com:nu:cric:crypto pubkey)  ::  cric-core
?>  =(%c suite:+<:cic)
=/  dat  dat:tw:pub:+<:cic  ::  tweak data
=/  offset  ;:(add 5 (met 3 txid.input) (met 3 vout.input))
=/  off  (cut 3 [offset (sub (met 3 dat) offset)] dat)
|^
::  logic from urb-core `+calc-spawn-sotx`
=/  value=@ud  1                                         ::  value set to 1 sat
=/  en-out  (can 3 script-pubkey.input 8^value ~)        ::  satoshi amount of the output encoded as 8 bytes
=/  spkh  (shay (add 8 wid.script-pubkey.input) en-out)  ::  SHA-256 encoded hash
::  where do we get tej offset ?
::
=/  sot=skim-sotx:urb  [%spawn pass [spkh `vout.input off tej=0]]
                                                            ::  spkh - hash commitment to the output, hash of scriptPubKey of the pre-commit transaction
=/  spawn=sotx:urb  (sign-skim sot)
::  encode skim-sotx and pass it to form urb-tagget taproot script
=/  =script:bscr
  %-  unv-to-script:en:urb-encoder
  %-  encode:urb-encoder  ~[spawn]
::  +p2tr currently handles only single tapleaf
::  creates leaf-hash
::  tweaks internal key with merkle-root(leaf-hash) 'TapTweak'
::  creates tweak-point, tweaked pubkey
::  placing tweaked key into a locking script 'OP_1 OP_PUSHBITES_32'
::
=/  taproot=octs  ~(scriptpubkey p2tr:gw `int-pubkey `script ~)
!!
::
  ++  sign-skim  ::  signing sotx
  |=  sot=skim-sotx:urb
  ^-  sotx:urb
  =/  ent  (skim:encode:urb-encoder sot)  ::  encode skim
  =/  fig  `@p`fig:ex:cic                 ::  pubkey fingerprint
  =/  sig  (sign-octs-raw:ed:crypto 512^(shaz ent) [sgn.pub sgn.sek]:+<:cic)  ::  certify octs
  [fig^[~ sig] sot]
--
