::  tests/app/gw-btc.hoon
::
::  Agent-level tests for the %gw-btc verifier's v9 sponsorship surface,
::  driven through the formal gall interface (on-poke / on-peek).  The
::  security-critical invariant: DECLINING sponsorship is silence, never
::  a negative Jael verdict -- a valid ship whose sponsorship we refuse
::  must not be snubbed.
::
/-  urb, sa=self-attestation, ord, bitcoin
/+  *test, cc=gw-btc-pass
/=  gw-btc  /app/gw-btc
=>
|%
++  bowl0
  ^-  bowl:agent:gall
  :*  [our=~zod src=~zod dap=%gw-btc sap=/]
      [wex=~ sup=~ sky=~]
      [act=0 eny=`@uvJ`0xbeef now=~2000.1.1 byk=[~zod %base da+~2000.1.1]]
  ==
::  a pass that is neither a valid confidential attestation nor the
::  public onboarding shape: it must draw a %fail (negative) verdict for
::  a NON-declined ship, and be short-circuited into silence for a
::  declined one.
::
++  bad-pass  ^-(pass 0x0)
::  a well-formed suite-C %gw-btc pass at protocol kelvin .kel, carrying a
::  canonically-empty custody log.  Only the kelvin varies, so the two
::  passes below differ in exactly the property under test.
::
++  kelvin-dat
  |=  kel=@ud
  ^-  @
  %+  can  0
  :~  (mat domain:cc)
      (mat kel)
      [256 0xd0.d0d0.d0d0.d0d0.d0d0.d0d0.d0d0.d0d0.d0d0.d0d0.d0d0.d0d0.d0d0.d0d0]
  ==
::
++  kelvin-pass
  |=  kel=@ud
  ^-  pass
  =/  seed  (shaz 'gw-btc-kelvin-probe')
  pub:ex:(pit:nu:cric:crypto 512 seed %c (kelvin-dat kel) (jam ~))
::
++  fig-of
  |=  =pass
  ^-  ship
  `@p`fig:ex:(com:nu:cric:crypto pass)
::  the mark+vase for a %jael-writ poke naming .who and carrying .pass
::
++  writ-vase-pass
  |=  [who=ship =pass]
  ^-  vase
  !>(`jael-poke:urb`[%jael-writ %gw-btc who pass])
::
++  writ-vase
  |=  who=ship
  ^-  vase
  (writ-vase-pass who bad-pass)
::  the writ poke EXACTLY as Jael builds it: +poke-watch does `!>(pok)`
::  with `pok` typed `*`, so the vase's type is `*` and carries no
::  structure at all.  A `*`-typed vase does not nest under a head-tagged
::  union, so an agent that unpacks with +!< bails on every writ that
::  really came from Jael -- while every dojo-driven test passes, because
::  `&noun [%jael-writ ...]` builds a fully typed vase.
::
++  writ-vase-untyped
  |=  [who=ship =pass]
  ^-  vase
  !>(`*`[%jael-writ %gw-btc who pass])
::  extract the noun a %x scry returned
::
++  peek-noun
  |=  res=(unit (unit cage))
  ^-  *
  ?~  res  !!
  ?~  u.res  !!
  q.q.u.u.res
::  a %give %fact card's mark, or ~ if it is not one
::
++  fact-mark
  |=  =card:agent:gall
  ^-  (unit @tas)
  ?.  ?=([%give %fact *] card)  ~
  `p.cage.p.card
::  a %give %fact card's payload noun, or ~ if it is not one
::
++  fact-payload
  |=  =card:agent:gall
  ^-  (unit *)
  ?.  ?=([%give %fact *] card)  ~
  `q.q.cage.p.card
::  drop the %verb-event/%verb-event-plus tracking cards the agent:dbug
::  wrapper emits on every poke, leaving only the agent's own output.
::
++  app-cards
  |=  cards=(list card:agent:gall)
  ^-  (list card:agent:gall)
  %+  skip  cards
  |=  c=card:agent:gall
  ?~  m=(fact-mark c)  %.n
  ?=(?(%verb-event %verb-event-plus) u.m)
--
|%
::  Operator declines sponsorship: the ship is recorded in `declined`,
::  removed from `sponsees`, and NO cards are emitted (never a snub).
::
++  test-sponsor-decline-records-and-is-silent
  =/  agent  gw-btc
  =^  cards  agent  (~(on-poke agent bowl0) %gw-sponsor-decline !>(`ship`~wes))
  =/  declined  (peek-noun (~(on-peek agent bowl0) /x/declined))
  ;:  weld
    ::  the decline poke itself emits nothing
    ::
    (expect-eq !>(~) !>((app-cards cards)))
    ::  /x/declined now names ~wes
    ::
    (expect-eq !>((silt ~[~wes])) !>(;;((set ship) declined)))
  ==
::
++  test-sponsor-clear-undoes-decline
  =/  agent  gw-btc
  =^  *  agent  (~(on-poke agent bowl0) %gw-sponsor-decline !>(`ship`~wes))
  =^  cards  agent  (~(on-poke agent bowl0) %gw-sponsor-clear !>(`ship`~wes))
  =/  declined  (peek-noun (~(on-peek agent bowl0) /x/declined))
  ;:  weld
    (expect-eq !>(~) !>((app-cards cards)))
    (expect-eq !>(*(set ship)) !>(;;((set ship) declined)))
  ==
::  THE security invariant: a %jael-writ for a ship we have declined is
::  answered with SILENCE (no cards) -- while the identical bad pass for
::  a ship we have NOT declined draws a negative %writ-response.  So
::  declining never turns into the negative verdict that would snub a
::  ship whose attestation is perfectly valid.
::
++  test-declined-writ-is-silent-contrast
  =/  agent  gw-btc
  =^  *  agent  (~(on-poke agent bowl0) %gw-sponsor-decline !>(`ship`~wes))
  ::  declined ship -> silence
  ::
  =^  c-dec  agent  (~(on-poke agent bowl0) %noun (writ-vase ~wes))
  ::  non-declined ship, same bad pass -> a negative writ-response
  ::
  =^  c-und  agent  (~(on-poke agent bowl0) %noun (writ-vase ~dev))
  =/  dec  (app-cards c-dec)
  =/  und  (app-cards c-und)
  ;:  weld
    ::  declined: NO cards at all
    ::
    (expect-eq !>(~) !>(dec))
    ::  non-declined: exactly one %writ-response fact
    ::
    (expect-eq !>(1) !>((lent und)))
    (expect-eq !>(`%writ-response) !>((fact-mark (snag 0 und))))
    ::  and that writ-response is a NEGATIVE verdict (res=~): a %fail,
    ::  the very verdict the declined path must never produce.
    ::
    %+  expect-eq
      !>  `(unit *)`~
      !>  ^-  (unit *)
          ?~  pay=(fact-payload (snag 0 und))  `**
          res:;;([dom=@tas =ship res=(unit *)] u.pay)
  ==
::  A pass minted under a FOREIGN protocol kelvin gets SILENCE, not a
::  negative verdict.  A negative verdict becomes a Jael %fail and an Ames
::  snub, so condemning foreign kelvins would make every old ship and
::  every new ship blacklist each other across a kelvin bump -- a network
::  partition on upgrade.  We cannot verify such a pass; we also have no
::  business declaring it bad.
::
::  The control is the SAME pass at our own kelvin, which is well-formed
::  but carries a canonically-empty custody log: that one does draw the
::  negative verdict.  So the silence below is caused by the kelvin and
::  nothing else.
::
++  test-foreign-kelvin-writ-is-silent
  =/  ours    (kelvin-pass kelvin:cc)
  =/  theirs  (kelvin-pass +(kelvin:cc))
  =/  agent  gw-btc
  =^  c-for  agent
    (~(on-poke agent bowl0) %noun (writ-vase-pass (fig-of theirs) theirs))
  =^  c-own  agent
    (~(on-poke agent bowl0) %noun (writ-vase-pass (fig-of ours) ours))
  =/  for  (app-cards c-for)
  =/  own  (app-cards c-own)
  ;:  weld
    ::  foreign kelvin: NO cards at all -- no verdict, no snub
    ::
    (expect-eq !>(~) !>(for))
    ::  our kelvin, same construction: exactly one NEGATIVE writ-response
    ::
    (expect-eq !>(1) !>((lent own)))
    (expect-eq !>(`%writ-response) !>((fact-mark (snag 0 own))))
    %+  expect-eq
      !>  `(unit *)`~
      !>  ^-  (unit *)
          ?~  pay=(fact-payload (snag 0 own))  `**
          res:;;([dom=@tas =ship res=(unit *)] u.pay)
    ::  and the two really are distinct ships (the kelvin is in the tweak)
    ::
    (expect !>(!=((fig-of ours) (fig-of theirs))))
  ==
::  A sponsorship request the operator has NOT declined is not short-
::  circuited: /x/declined and /x/sponsees start empty.
::
++  test-sponsees-and-declined-start-empty
  =/  agent  gw-btc
  ;:  weld
    (expect-eq !>(*(set ship)) !>(;;((set ship) (peek-noun (~(on-peek agent bowl0) /x/declined)))))
    (expect-eq !>(*(map ship *)) !>(;;((map ship *) (peek-noun (~(on-peek agent bowl0) /x/sponsees)))))
  ==
::  A writ poke whose vase is `*`-typed -- the ONLY shape Jael ever sends
::  -- must be handled identically to a typed one.  Unpacking with +!<
::  instead of a mold-cast makes every real, network-originated
::  attestation crash the agent (`nest-fail`, `-have.*`), which no
::  dojo-driven test can see because `&noun [%jael-writ ...]` builds a
::  fully typed vase.  Confidential comets were 100% non-functional over
::  real networking for exactly this reason.
::
++  test-untyped-writ-poke-is-handled
  =/  agent  gw-btc
  ::  the SAME bad pass, once through a typed vase and once through the
  ::  `*`-typed vase Jael actually produces: both must reach the verdict
  ::  path, neither may bail.
  ::
  =^  c-typed    agent  (~(on-poke agent bowl0) %noun (writ-vase ~dev))
  =^  c-untyped  agent
    (~(on-poke agent bowl0) %noun (writ-vase-untyped ~rut bad-pass))
  =/  typed    (app-cards c-typed)
  =/  untyped  (app-cards c-untyped)
  ;:  weld
    ::  the untyped poke produced a verdict rather than crashing
    (expect-eq !>(1) !>((lent untyped)))
    (expect-eq !>(`%writ-response) !>((fact-mark (snag 0 untyped))))
    ::  and it is the same verdict the typed poke drew
    (expect-eq !>((lent typed)) !>((lent untyped)))
    %+  expect-eq
      !>  `(unit *)`~
      !>  ^-  (unit *)
          ?~  pay=(fact-payload (snag 0 untyped))  `**
          res:;;([dom=@tas =ship res=(unit *)] u.pay)
    ::  ... naming the ship the untyped poke carried
    %+  expect-eq
      !>  `ship`~rut
      !>  ^-  ship
          ?~  pay=(fact-payload (snag 0 untyped))  ~zod
          ship:;;([dom=@tas =ship res=(unit *)] u.pay)
  ==
::  The public block scanner is bootstrapped by HEIGHT.  Under the light
::  client a block is addressed by height alone, so choosing a start point
::  no longer means hand-building a $state:urb around a block hash.
::  .start is the FIRST block to scan, so the cursor lands one below it.
::
++  test-index-from-height-sets-cursor
  =/  agent  gw-btc
  =^  cards  agent  (~(on-poke agent bowl0) %gw-index-from !>(`@ud`961.055))
  =/  bid  (peek-noun (~(on-peek agent bowl0) /x/block-id))
  ;:  weld
    (expect-eq !>(`id:block:bitcoin`[0x0 961.054]) !>(;;(id:block:bitcoin bid)))
    ::  and the block timer is armed immediately
    (expect-eq !>(1) !>((lent (app-cards cards))))
  ==
::  Bootstrap is one-shot: a second poke must not silently rewind the
::  cursor (it would mix two index epochs).
::
++  test-index-from-height-is-one-shot
  =/  agent  gw-btc
  =^  *      agent  (~(on-poke agent bowl0) %gw-index-from !>(`@ud`961.055))
  =^  cards  agent  (~(on-poke agent bowl0) %gw-index-from !>(`@ud`700.000))
  =/  bid  (peek-noun (~(on-peek agent bowl0) /x/block-id))
  ;:  weld
    (expect-eq !>(`id:block:bitcoin`[0x0 961.054]) !>(;;(id:block:bitcoin bid)))
    (expect-eq !>(~) !>((app-cards cards)))
  ==
::  UPGRADE.  Revisions before the light-client port carried a leading
::  `rpc=req-to:btcio` ([url=@t auth]) ahead of urb-state.  +on-load must
::  drop it rather than crash -- an unmigrated +on-load bricks every
::  existing pier on upgrade, and a %gw-btc that will not load is a ship
::  that answers no attestation at all.
::
++  test-on-load-drops-legacy-rpc-field
  =/  legacy=*
    :*  ['http://localhost:8332' ~]                  :: rpc  (dropped)
        `state:urb`[[0xdead.beef 943.140] ~ ~ ~]     :: urb-state
        %.y                                          :: ready -> indexing
        ~                                            :: best
        ~  ~  ~  ~  0  ~  ~
    ==
  =/  agent  gw-btc
  =^  cards  agent  (~(on-load agent bowl0) !>(legacy))
  ;:  weld
    ::  the index survived the migration intact
    %+  expect-eq
      !>  `id:block:bitcoin`[0xdead.beef 943.140]
      !>  ;;(id:block:bitcoin (peek-noun (~(on-peek agent bowl0) /x/block-id)))
    (expect-eq !>(~) !>((app-cards cards)))
  ==
::  ... and the current shape still loads, unchanged.
::
++  test-on-load-round-trips-current-state
  =/  agent  gw-btc
  =^  *      agent  (~(on-poke agent bowl0) %gw-index-from !>(`@ud`961.055))
  =^  *      agent  (~(on-poke agent bowl0) %gw-sponsor-decline !>(`ship`~wes))
  =^  *      agent  (~(on-load agent bowl0) ~(on-save agent bowl0))
  ;:  weld
    %+  expect-eq
      !>  `id:block:bitcoin`[0x0 961.054]
      !>  ;;(id:block:bitcoin (peek-noun (~(on-peek agent bowl0) /x/block-id)))
    (expect-eq !>((silt ~[~wes])) !>(;;((set ship) (peek-noun (~(on-peek agent bowl0) /x/declined)))))
  ==
--
