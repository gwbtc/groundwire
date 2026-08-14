::  Codec for the suite-%c pass format owned by the %gw-btc PKI domain,
::  protocol kelvin 9 (ops/doc/opret-revision/01-spec-revision.md as amended
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
::  +with-xtr: the same suite-%c pass carrying a different xtr tail
::
::    This is what a %anew refresh emits: the custody log grows, the NAME
::    does not.  xtr is excluded from the key tweak (see +make-dat), so
::    only `ugn`, `cry` and `dat` fix the @p -- all three are copied
::    verbatim here and `fig` is therefore unchanged by construction.
::
::    The layout is exactly what +pub:ex:cric writes and +nol/+com read
::    (sys/zuse.hoon):
::
::        'c' | 32^ugn | 32^cry | (mat dat) | xtr
::
::    with `xtr` occupying every remaining bit and OMITTED entirely when
::    it is 0.  Pinned by tests/lib/gw-btc-pass: re-encoding a pass with
::    its own xtr must reproduce the pass byte-for-byte, which is the
::    check that this stays in step with the kernel's encoder.
::
::    ~ if .pass is not a well-formed suite-%c pass.
::
++  with-xtr
  |=  [=pass xtr=@]
  ^-  (unit ^pass)
  %-  mole
  |.
  ?>  =('c' (end 3 pass))
  ::  reject anything +com would refuse, so we never mint a pass that
  ::  cannot be read back
  ::
  =/  cic  (com:nu:cric:crypto pass)
  ?>  ?=(%c suite.+<.cic)
  =/  bod  (rsh 3 pass)
  =/  hed  (rub 512 bod)
  ^-  ^pass
  %+  can  0
  :~  [8 'c']
      [256 (end 8 bod)]           ::  ugn -- the untweaked signing key
      [256 (cut 8 [1 1] bod)]     ::  cry -- the messaging key
      (mat q.hed)                 ::  dat -- the immutable tweak data
      [(met 0 xtr) xtr]
  ==
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
::      payload      = (jam [pass opening])
::
::  The payload is the FULL ATTESTATION PACKET: .pass is byte-for-byte
::  the pass a comet hands a peer over ames, custody log and all, and
::  .opening is the one hop the packet cannot contain -- the one this
::  very transaction performs, whose txid does not exist until the
::  transaction is signed.  A watcher completes the log with
::  [txid height opening] from the block it is reading and hands the
::  result to the same +verify-lc a mailed attestation goes through.
::
::  +max-publication: the byte cap on that payload
::
::    1.024 bytes, and the number is not arbitrary: it is THE PACKET
::    BOUND.  Decisions addendum section 6 fixes a complete jammed
::    first-contact attestation at one Mesa fragment (~1 KiB), and a
::    publication carries that same packet.  A payload this codec would
::    accept but ames could never carry would describe an identity that
::    can be published and then never attest -- so the two bounds are
::    one bound, stated once.
::
::    What it buys and costs, since an OP_RETURN is all non-witness data
::    and payload bytes convert ~1:1 into vbytes:
::
::      - a pass core is ~108 B, entry 0's opening ~120 B and the
::        terminal opening ~100 B, so the floor is ~330 B and each
::        further custody hop adds ~40 B.  1.024 B is therefore ~17
::        hops, against the four that 512 allowed.
::      - ~1.024 payload bytes is a ~1.160 vB transaction: ~2.320 sats
::        at 2 sat/vB.  Payable only because a state update may take a
::        funding input (c534cba); before that the fee came out of the
::        identity sat and a comet could be priced out of its own name.
::      - it bounds what a stranger can make every watcher on the
::        network do for one transaction fee: ~17 light-client fetches
::        plus a one-block filter scan, single-flighted per ship.
::
::    The hard ceiling is MAX_SCRIPT_SIZE (10.000); OP_PUSHDATA2 has
::    reached it since 194e56d, so raising this later is a constant,
::    not a format change.  THE SAME NUMBER LIVES IN THREE PLACES --
::    here, causeway/desktop/causeway.py and
::    causeway/src/spawn/publication.ts -- and they must agree byte for
::    byte or an encoder writes a script the others refuse.
::
++  max-publication  1.024
::  +push-data: the minimal Bitcoin push opcode(s) for a byte string
::
::    A direct push (opcode = length) reaches 75.  OP_PUSHDATA1 (0x4c)
::    carries ONE length byte and therefore stops at 255 -- which is
::    far below this codec's own 1.024-byte cap, so a fief-carrying
::    publication (265-269 bytes in practice) already needs
::    OP_PUSHDATA2 (0x4d) and its TWO-byte LITTLE-ENDIAN length, and a
::    full-packet one is never anything else.
::
::    Every arm here is total or crashes: a width this encoding cannot
::    express, or a `dat` too wide for its declared `wid`, is an %exit
::    with a named reason.  It must never be a silently truncated
::    length field -- `[1 wid]` with wid > 255 packs to wid mod 256 and
::    emits a script that no parser can read back and no operator can
::    see is wrong.
::
++  push-data
  |=  b=hexb:btc
  ^-  hexb:btc
  ~|  [%push-data-width-exceeds-declared-wid (met 3 dat.b) wid.b]
  ?>  (lte (met 3 dat.b) wid.b)
  ?:  (lte wid.b 75)  [1 wid.b]
  ?:  (lte wid.b 0xff)
    (cat:byt:bcu ~[[1 0x4c] [1 wid.b]])
  ?:  (lte wid.b 0xffff)
    (cat:byt:bcu ~[[1 0x4d] (flip:byt:bcu [2 wid.b])])
  ~|([%push-data-too-big-for-pushdata2 wid.b] !!)
::
++  publication-script
  |=  payload=hexb:btc
  ^-  hexb:btc
  ~|  [%publication-payload-over-cap wid.payload max-publication]
  ?>  (lte wid.payload max-publication)
  =/  psh=hexb:btc  (push-data payload)
  (cat:byt:bcu ~[[5 0x6a.0375.7262] [1 0x1] [1 kelvin] psh payload])
::  +make-publication: full OP_RETURN scriptPubKey for a $publication
::
::    The payload is the jam's ORDINARY LITTLE-ENDIAN byte dump, i.e.
::    exactly +jam-octs -- the same convention used by every hash preimage
::    in this file and by Causeway's jam_bytes.  Do not "simplify" this to
::    [(met 3 jm) jm]: that treats the jam atom as a big-endian byte string
::    and produces a payload no other implementation can read (and cannot
::    read a real one back).
::
++  make-publication
  |=  pub=publication:sa
  ^-  hexb:btc
  (publication-script (jam-octs pub))
::  +read-publication: decode a publication from an output script.
::  Rejects a wrong kelvin so a future protocol version's publications
::  are ignored, not mis-parsed.
::
++  read-publication
  |=  script=hexb:btc
  ^-  (unit publication:sa)
  ::  THE HOT FILTER, and the only silent exit in this arm: it runs on
  ::  every output of every transaction in every block, and virtually
  ::  none of them is an OP_RETURN "urb" envelope.  Announcing it would
  ::  drown the log in millions of lines a day.
  ::
  ::  It is asked SEPARATELY from parsing, which it was not.  The whole
  ::  of +parse-publication sits inside a +mole -- including the envelope
  ::  match -- so a script that IS ours but whose push header or length
  ::  is wrong came back ~ and took this same silent exit, indistinguish-
  ::  able from the millions of scripts that are simply not ours.  The
  ::  comment below claimed there were two refusals past this point and
  ::  there were three, the third silent.  Concretely: a comet past the
  ::  ~17-hop ceiling that publishes anyway produced ZERO log lines on
  ::  every watcher on the network, and the transaction was not even
  ::  retained.  The operator paid the fee and got no signal from anyone.
  ::
  ?.  (publication-envelope script)  ~
  =/  env  (parse-publication script)
  ?~  env
    ~&  >>>  [%gw-btc-publication-unreadable-push bytes=wid.script]
    ~
  ::  Past it, the script IS a Groundwire publication envelope, and every
  ::  refusal here is us declining to read something an operator paid
  ::  miner fees to put on chain.  They are rare by construction -- and
  ::  they were invisible, which is the whole of test 6.7's complaint.
  ::
  ?.  =(kelvin kel.u.env)
    ~&  >>>  [%gw-btc-publication-foreign-kelvin found=kel.u.env ours=kelvin]
    ~
  =/  dec
    %-  mole
    |.
    ::  undo +jam-octs: little-endian byte dump back into the jam atom
    ;;(publication:sa (cue (rev 3 wid.payload.u.env dat.payload.u.env)))
  ?^  dec  dec
  ~&  >>>  [%gw-btc-publication-undecodable payload-bytes=wid.payload.u.env]
  ~
::  +parse-publication: (unit [kelvin payload]) from an output script
::
::    Reads the three push forms +push-data can emit -- a direct push
::    (1-75), OP_PUSHDATA1 (0x4c, one length byte) and OP_PUSHDATA2
::    (0x4d, two LITTLE-ENDIAN length bytes).  Any other leading opcode
::    is not a push this codec produced, so the script is refused
::    rather than read as a length.
::
::  +publication-envelope: is this script ours AT ALL?
::
::    Split out of +parse-publication so that "not a Groundwire output"
::    and "a Groundwire output we cannot read" are different answers.
::    Inside the +mole they were the same one, and the second is the case
::    somebody paid to put on chain.
::
::    Cheap and total: every output of every transaction in every block
::    reaches this.  The width test guards the takes below it, and ?&
::    short-circuits, so a 3-byte script is refused rather than read.
::
++  publication-envelope
  |=  script=hexb:btc
  ^-  ?
  ?&  (gte wid.script 7)
      =(0x6a.0375.7262 dat:(take:byt:bcu 5 script))
      =(0x1 dat:(take:byt:bcu 1 (drop:byt:bcu 5 script)))
  ==
::
++  parse-publication
  |=  script=hexb:btc
  ^-  (unit [kel=@ud payload=hexb:btc])
  %-  mole
  |.
  ?>  (gte wid.script 7)
  ?>  =(0x6a.0375.7262 dat:(take:byt:bcu 5 script))
  =/  rst  (drop:byt:bcu 5 script)
  ?>  =(0x1 dat:(take:byt:bcu 1 rst))
  =/  kel  dat:(take:byt:bcu 1 (drop:byt:bcu 1 rst))
  =.  rst  (drop:byt:bcu 2 rst)
  =/  opc=@ux  dat:(take:byt:bcu 1 rst)
  =/  [len=@ud hed=@ud]
    ?:  =(0x4c opc)  [dat:(take:byt:bcu 1 (drop:byt:bcu 1 rst)) 2]
    ?:  =(0x4d opc)  [dat:(flip:byt:bcu (take:byt:bcu 2 (drop:byt:bcu 1 rst))) 3]
    ::  a direct push: the opcode IS the length, and only 0-75 is one
    ?>  (lte opc 75)
    [`@ud`opc 1]
  =.  rst  (drop:byt:bcu hed rst)
  ?>  =(len wid.rst)
  ?>  (lte len max-publication)
  [`@ud`kel rst]
--
