::  tests/app/gw-btc.hoon
::
::  Agent-level tests for the %gw-btc verifier's v9 sponsorship surface,
::  driven through the formal gall interface (on-poke / on-peek).  The
::  security-critical invariant: DECLINING sponsorship is silence, never
::  a negative Jael verdict -- a valid ship whose sponsorship we refuse
::  must not be snubbed.
::
/-  urb, sa=self-attestation
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
--
