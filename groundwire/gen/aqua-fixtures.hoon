::  +aqua-fixtures: regenerate the confidential-comet fixtures in base arvo.
::
::    Base arvo implements GENERIC pluggable comet PKI and must not carry
::    a %gw-btc codec: there is exactly one authoritative implementation
::    of the format that fixes a confidential comet's name, and it is
::    lib/gw-btc-pass.hoon in this desk (pinned in turn by the shared
::    golden vectors in vectors/gw-kelvin-9.json, which Causeway's
::    TypeScript and Python implementations also match).
::
::    The Aqua simulation in gwbtc/urbit still needs two confidential
::    comets to route packets for, so it carries this generator's OUTPUT
::    as checked-in data: the dat TAIL and the xtr are opaque atoms to
::    arvo, which hands them to +cc-crub and never looks inside.
::    Everything that defines the fixtures -- seeds, spawn satpoints,
::    start height, internal key, comet indices -- is written down HERE
::    and nowhere else.
::
::    Base arvo does not name a vendor, so the SIMULATION does not run
::    the %gw-btc domain: +aqua-domain below is the term the fixtures
::    commit to, and it must equal +cc-domain:aqua-azimuth there.  A dat
::    is a +mat domain tag followed by that domain's own data; arvo
::    spells out the tag (all it reads) and holds the rest as one opaque
::    literal, so this generator prints that tail rather than a whole
::    dat.  Change the domain and every fixture comet is renamed, which
::    is the point: the tag is inside the key tweak.
::
::    Usage:
::
::        +groundwire!aqua-fixtures
::
::    Paste the printed literals into +cc-dat-tail and +cc-xtr in
::    gwbtc/urbit/pkg/arvo/lib/aqua-azimuth.hoon.  The comet names it
::    prints must equal +cc-comet-ok and +cc-comet-fail there, which
::    arvo derives from the pasted data with its own generic cric; if
::    they disagree, the paste is wrong, the domain moved, or the
::    kernel's suite-%c encoding moved.
::
::    The %fail fixture is broken deliberately and minimally: its
::    blind-opening names a satpoint its dat does NOT commit to, so a
::    real verifier's commitment check -- and only that check -- fails.
::    Give it (spawn %fail) below instead and it verifies like %ok.
::
/-  sa=self-attestation, ord
/+  gwp=gw-btc-pass
:-  %say
|=  [[now=@da eny=@uvJ bec=beak] ~ ~]
:-  %noun
=<  fixtures
|%
::  +seed: a fixture comet's master seed (its entry in +comets)
::
++  seed
  |=  which=?(%ok %fail)
  ^-  @
  ?:(?=(%ok which) 1 2)
::  +spawn: the spawn satpoint a fixture comet's dat commits to
::
++  spawn
  |=  which=?(%ok %fail)
  ^-  sont:ord
  ?:(?=(%ok which) [0x1111 0 0] [0x2222 1 0])
::  +start-height: block height of the spawn transaction
::
++  start-height  778.000
::  +internal-key: 33-byte compressed P2TR internal key (secp G)
::
::    The Aqua fixtures have no chain to check taproot output keys
::    against, so this is decorative; it matches the golden vectors.
::
++  internal-key
  ^-  @ux
  0x2.79be.667e.f9dc.bbac.55a0.6295.ce87.0b07.
  029b.fcdb.2dce.28d9.59f2.815b.16f8.1798
::  +fief-index: the comet's slot in +comets:ted/aqua/ames, which the
::  simulation decodes as a fake %if lane
::
++  fief-index
  |=  which=?(%ok %fail)
  ^-  @ud
  ?:(?=(%ok which) 12 13)
::  +aqua-domain: the pki domain the Aqua fixtures commit to
::
::    NOT %gw-btc.  Base arvo must not name a vendor anywhere, including
::    in the name of the Gall agent its scenarios install, and jael
::    routes a %writ to the agent named by the pass's leading +mat.  So
::    the simulation runs a domain of its own; must equal
::    +cc-domain:aqua-azimuth in gwbtc/urbit.
::
++  aqua-domain  %test-pki
::  +dat: the immutable tweak data, under +aqua-domain
::
::    The real codec's output with its domain tag swapped.  Everything
::    after the tag -- kelvin and hiding commitment -- is byte-identical
::    to what +make-dat:gwp produces, which is what makes these fixtures
::    derived data rather than an invention.
::
++  dat
  |=  which=?(%ok %fail)
  ^-  @
  (can 0 ~[(mat aqua-domain) [(met 0 (dat-tail which)) (dat-tail which)]])
::  +dat-tail: the domain's own data, as base arvo holds it
::
++  dat-tail
  |=  which=?(%ok %fail)
  ^-  @
  =/  real=@  (make-dat:gwp (spawn which) (make-blind:gwp (seed which)))
  (rsh [0 p:(mat domain:gwp)] real)
::  +sed: the 64-byte cric seed of a fixture comet at .lyfe
::
::    Suite C splits the seed into a signing half (bytes 0-31, which
::    fixes the @p through the tweak) and a messaging half (bytes
::    32-63).  Rekeying a confidential comet rotates ONLY the messaging
::    half: life rides in the on-chain snapshot, never in the seed.
::
++  sed
  |=  [which=?(%ok %fail) lyfe=@ud]
  ^-  @
  =/  base  (shal 64 (seed which))
  =/  sgn   (end 8 base)
  =/  cry   (shax (can 3 ~[[32 (cut 8 [1 1] base)] [8 lyfe]]))
  (can 3 ~[[32 sgn] [32 cry]])
::  +crub: activate a suite-%c core from an explicit 64-byte seed
::
::    +pit:nu:cric derives the whole seed by hashing one number, which
::    cannot express "same signing key, new messaging key".  This builds
::    the same $ring +pit would, with the seed supplied outright.  It is
::    a verbatim copy of +gw-crub:aqua-azimuth, so that what this
::    generator prints is what arvo will build.
::
++  crub
  |=  [sed=@ dat=@ xtr=@]
  %-  nol:nu:cric:crypto
  ^-  ring
  =<  p
  %-  fax:plot
  :-  0
  :*  [s+~ 3 [1 'C'] ~]
      [s+~ 3 [64 sed] ~]
      (mat dat)
      ?:  =(0 xtr)  ~
      [(met 0 xtr)^xtr ~]
  ==
::  +cry: a fixture comet's messaging public key at .lyfe
::
::    Independent of dat and xtr, so it can be computed before the
::    custody log that commits to it.
::
++  cry
  |=  [which=?(%ok %fail) lyfe=@ud]
  ^-  @
  cry:ded:ex:(crub (sed which lyfe) 0 0)
::  +log: a fixture comet's custody log, oldest entry first
::
::    One entry per life.  Entry 0 is the spawn: it alone carries the
::    $blind-opening that opens the pass's hiding dat commitment.  Later
::    entries are rekeys, each opening the snapshot committed at that
::    custody hop; the newest snapshot is the comet's current state.
::
++  log
  |=  [which=?(%ok %fail) lyfe=@ud]
  ^-  custody-log:sa
  =/  open=blind-opening:sa
    :+  ?:(?=(%ok which) (spawn which) [0x3333 1 0])
      start-height
    (make-blind:gwp (seed which))
  =/  idx  (fief-index which)
  %+  turn  (gulf 1 lyfe)
  |=  l=@ud
  ^-  custody-entry:sa
  :+  `@ux`(add 0x1111.0000 l)
    (add start-height (dec l))
  :-  ~
  :+  internal-key
    :*  life=l
        rift=0
        key=(cry which l)
        sponsor=~
        fief=`[%if `@`0xdead.beef `@`idx]
    ==
  ?:(=(1 l) `open ~)
::  +xtr: the pass's mutable tail -- the jammed custody log
::
++  xtr
  |=  [which=?(%ok %fail) lyfe=@ud]
  ^-  @
  (jam (log which lyfe))
::  +keys: a fixture comet's full suite-%c core at .lyfe
::
++  keys
  |=  [which=?(%ok %fail) lyfe=@ud]
  (crub (sed which lyfe) (dat which) (xtr which lyfe))
::  +fixtures: everything base arvo needs, and the names to check
::
++  fixtures
  :*  comet-ok=`@p`fig:ex:(keys %ok 1)
      comet-fail=`@p`fig:ex:(keys %fail 1)
      domain=aqua-domain
      dat-tail-ok=`@ux`(dat-tail %ok)
      dat-tail-fail=`@ux`(dat-tail %fail)
      xtr-ok-life-1=`@ux`(xtr %ok 1)
      xtr-ok-life-2=`@ux`(xtr %ok 2)
      xtr-fail-life-1=`@ux`(xtr %fail 1)
      xtr-fail-life-2=`@ux`(xtr %fail 2)
  ==
--
