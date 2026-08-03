::  Codec for the suite-%c pass format owned by the %gw-btc PKI domain,
::  protocol kelvin 9 (doc/opret-revision/01-spec-revision.md as amended
::  by 04-decisions-addendum.md).
::
::  The immutable tweak data is:
::
::      dat   = (can 0 (mat %gw-btc) (mat 9) [256 d] ~)
::      d     = H_tag("gw/spawn-commit", (jam spawn-sont) || blind)
::      blind = H_tag("gw/spawn-blind", seed)
::
::  Ames reads only the leading mat to route the pass to %gw-btc.  The
::  kelvin is plaintext, so any holder of a pass can read a comet's mint
::  version without an opening.  d is a hiding commitment: the spawn
::  satpoint is learned only from an explicit $blind-opening (in the
::  xtr, or in a public OP_RETURN publication), never parsed out of
::  dat.  Trailing data after d is rejected.  The pass's xtr tail is
::  excluded from the key tweak and may grow without changing the
::  comet's name.
::
::  Byte conventions, pinned by the shared golden vectors: H_tag is the
::  BIP-340 tagged hash (+tagged-hash:taproot) over big-endian byte
::  strings; a jammed noun enters a hash message as its minimal
::  little-endian byte dump (the ordinary serialization of a jam);
::  blind, d, and all commitment hashes are exactly 32 bytes.
::
/-  ord, sa=self-attestation
/+  taproot, btc=bitcoin, bcu=bitcoin-utils
|%
++  domain  %gw-btc
++  kelvin  9
::  +jam-octs: a jammed noun as a byte string (minimal LE byte dump)
::
++  jam-octs
  |=  n=*
  ^-  hexb:btc
  =/  jm  (jam n)
  =/  wid  (met 3 jm)
  [wid (rev 3 wid jm)]
::  +make-blind: the recommended seed-derived blind
::
::    deterministic from the master seed alone, so the opening is
::    recoverable without extra stored state.  the seed enters as its
::    minimal byte dump.
::
++  make-blind
  |=  seed=@
  ^-  @ux
  =/  wid  (met 3 seed)
  (tagged-hash:taproot 'gw/spawn-blind' [wid (rev 3 wid seed)])
::  +spawn-commit: d, the hiding commitment to the spawn satpoint
::
++  spawn-commit
  |=  [spawn=sont:ord blind=@ux]
  ^-  @ux
  %+  tagged-hash:taproot  'gw/spawn-commit'
  (cat:byt:bcu ~[(jam-octs spawn) [32 blind]])
::  +make-dat: the full immutable tweak data
::
++  make-dat
  |=  [spawn=sont:ord blind=@ux]
  ^-  @
  (can 0 ~[(mat domain) (mat kelvin) [256 (spawn-commit spawn blind)]])
::  +parse-dat: domain tag, kelvin, and commitment -- nothing more
::
::    rejects trailing data: a dat must be exactly the two mat items
::    followed by 256 bits of commitment.
::
++  parse-dat
  |=  dat=@
  ^-  (unit [dom=@tas kel=@ud d=@ux])
  %-  mole
  |.
  =/  hed  (rub 0 dat)
  =/  kel  (rub p.hed dat)
  =/  pos  (add p.hed p.kel)
  =/  d  `@ux`(cut 0 [pos 256] dat)
  ?>  (lte (met 0 dat) (add pos 256))
  [`@tas`q.hed `@ud`q.kel d]
::  +verify-dat: check a blind-opening against a dat
::
++  verify-dat
  |=  [dat=@ open=blind-opening:sa]
  ^-  ?
  =/  psd  (parse-dat dat)
  ?~  psd  |
  ?&  =(domain dom.u.psd)
      =(kelvin kel.u.psd)
      =(d.u.psd (spawn-commit spawn.open blind.open))
  ==
::
++  parse-pass
  |=  =pass
  ^-  (unit [dom=@tas kel=@ud d=@ux xtr=@])
  %-  mole
  |.
  =/  cic  (com:nu:cric:crypto pass)
  ?>  ?=(%c suite.+<.cic)
  =/  meta  (need (parse-dat dat.tw.pub.+<.cic))
  [dom.meta kel.meta d.meta xtr.tw.pub.+<.cic]
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
::  +state-commit: c, the commitment to a complete state snapshot
::
++  state-commit
  |=  snap=snapshot:sa
  ^-  @ux
  (tagged-hash:taproot 'gw/state-commit' (jam-octs snap))
::  +state-leaf: the canonical unspendable commitment tapleaf
::
::    OP_RETURN PUSH2 'gw' PUSH32 <c>.  execution fails at OP_RETURN,
::    so the script path is provably unspendable and every custody
::    spend is key-path by construction; a BIP-371 signer shown this
::    one-leaf tree can verify no alternative spend path exists.
::
++  state-leaf
  |=  c=@ux
  ^-  tapleaf:taproot
  :-  0xc0
  :-  37
  %+  add  (lsh [3 33] 0x6a02.6777)
  (add (lsh [3 32] 0x20) `@`c)
::  +state-key: the x-only P2TR output key committing .snap under .p
::
::    .p is the 33-byte compressed internal key.  single-leaf tree:
::    root = leaf hash.
::
++  state-key
  |=  [p=@ux snap=snapshot:sa]
  ^-  @ux
  %+  output-pubkey:taproot  p
  `(leaf-hash:taproot (state-leaf (state-commit snap)))
::
::  Publication: the optional OP_RETURN transaction output for
::  deliberate on-chain revelation (default off; confidential custody
::  transactions carry no publication output at all).
::
::      scriptPubKey = OP_RETURN PUSH3 'urb' PUSH1 <kelvin> <payload>
::
::  For a public spawn the payload is (jam [pass spawn-sont blind]) as
::  a byte string, opening the dat commitment so the name is publicly
::  verifiable and indexable with no packet exchange.
::
++  max-publication  512
::
++  publication-script
  |=  payload=hexb:btc
  ^-  hexb:btc
  ?>  (lte wid.payload max-publication)
  =/  psh=hexb:btc
    ?:  (lte wid.payload 75)  [1 wid.payload]
    (cat:byt:bcu ~[[1 0x4c] [1 wid.payload]])
  (cat:byt:bcu ~[[5 0x6a03.7572.62] [1 0x1] [1 kelvin] psh payload])
::  +parse-publication: (unit [kelvin payload]) from an output script
::
++  parse-publication
  |=  script=hexb:btc
  ^-  (unit [kel=@ud payload=hexb:btc])
  %-  mole
  |.
  ?>  (gte wid.script 7)
  ?>  =(0x6a03.7572.62 dat:(take:byt:bcu 5 script))
  =/  rst  (drop:byt:bcu 5 script)
  ?>  =(0x1 dat:(take:byt:bcu 1 rst))
  =/  kel  dat:(take:byt:bcu 1 (drop:byt:bcu 1 rst))
  =.  rst  (drop:byt:bcu 2 rst)
  =?  rst  =(0x4c dat:(take:byt:bcu 1 rst))
    (drop:byt:bcu 1 rst)
  =/  len=@ud  dat:(take:byt:bcu 1 rst)
  =.  rst  (drop:byt:bcu 1 rst)
  ?>  =(len wid.rst)
  ?>  (lte len max-publication)
  [`@ud`kel rst]
--
