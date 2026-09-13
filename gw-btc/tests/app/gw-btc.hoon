::  tests/app/gw-btc.hoon
::
::  Agent-level tests for the %gw-btc verifier's v9 sponsorship surface,
::  driven through the formal gall interface (on-poke / on-peek).  The
::  security-critical invariant: DECLINING sponsorship is silence, never
::  a negative Jael verdict -- a valid ship whose sponsorship we refuse
::  must not be snubbed.
::
/-  urb, sa=self-attestation, ord, bitcoin, lc=light-client
/+  *test, cc=gw-btc-pass, lsa=self-attestation, ol=ord, uc=urb-core
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
      (mat (jam `sont:ord`[txid=0xd0d0.d0d0.d0d0.d0d0 vout=0 off=0]))
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
::  the /is-synced subscription card +on-init and every +on-load migration
::  branch must emit (see $gw-state's .synced)
::
++  synced-watch
  ^-  card:agent:gall
  [%pass /is-synced %agent [~zod %bitcoin-client] %watch /is-synced]
::
++  app-cards
  |=  cards=(list card:agent:gall)
  ^-  (list card:agent:gall)
  %+  skip  cards
  |=  c=card:agent:gall
  ?~  m=(fact-mark c)  %.n
  ?=(?(%verb-event %verb-event-plus) u.m)
::  ---------------------------------------------------------------------
::  %anew fixtures
::  ---------------------------------------------------------------------
::
::  one custody entry, and the pass a refresh would publish for it
::
++  entry0  ^-  custody-entry:sa  [txid=0xdead.beef height=900.001 opening=~]
++  entry1  ^-  custody-entry:sa  [txid=0xf00d.cafe height=900.050 opening=~]
++  cand    ^-  custody-log:sa    ~[entry0 entry1]
++  anew-pass  (kelvin-pass kelvin:cc)
::  the Causeway -> %gw-btc ingestion poke, on the %noun mark
::
++  ingest-vase
  |=  =custody-entry:sa
  ^-  vase
  !>(`ingest:sa`[%gw-custody-entry custody-entry])
::  ... and the `*`-typed shape a khan thread / lens poke really produces
::
++  ingest-vase-untyped
  |=  =custody-entry:sa
  ^-  vase
  !>(`*`[%gw-custody-entry custody-entry])
::  the anew poke jael sends
::
++  anew-vase  ^-(vase !>(`jael-poke:urb`[%jael-anew %gw-btc]))
::  A state noun with a %anew validation IN FLIGHT (job 0, candidate
::  .cand, pass .anew-pass) and a chain tip known.  Fed through +on-load
::  because that is the only way a test can put the agent in the middle
::  of an asynchronous job.
::
++  pending-state
  |=  [stored=custody-log:sa]
  ^-  *
  :*  `state:urb`[[0xdead.beef 900.100] ~ ~ ~]   :: urb-state
      %.y                                         :: indexing
      `[0xdead.beef 900.100]                      :: best
      ~                                           :: inflight
      ~                                           :: confidential
      ~                                           :: attested
      ~                                           :: publicizing
      1                                           :: next-job
      ~                                           :: sponsees
      ~                                           :: declined
      [stored `[0 cand anew-pass]]                :: own
  ==
::  the state shape BEFORE .publicizing was dropped (13 fields), for the
::  migration.  Deliberately the AMBIGUOUS case: .publicizing, .next-job
::  and .sponsees are all ~/0, so if the tail did not discriminate the
::  whole tuple would shift by one and still look plausible.  .declined
::  is non-empty precisely so a shift would be visible.
::
++  legacy-12-state
  ^-  *
  :*  `state:urb`[[0xdead.beef 961.100] ~ ~ ~]  :: urb-state
      %.y                                        :: indexing
      `[0xdead.beef 961.100]                     :: best
      ~                                          :: inflight
      ~                                          :: confidential
      ~                                          :: attested
      ~                                          :: publicizing (dropped)
      0                                          :: next-job
      ~                                          :: sponsees
      (silt ~[~wes])                             :: declined
      [`custody-log:sa`~ ~]                      :: own
      %.y                                        :: synced
      ~                                          :: reorg-halt
  ==
::  the state shape BEFORE .own existed (10 fields), for the migration
::
++  legacy-10-state
  ^-  *
  :*  `state:urb`[[0xdead.beef 943.140] ~ ~ ~]
      %.y  ~  ~  ~  ~  ~  0  ~  ~
  ==
::  ---------------------------------------------------------------------
::  the pre-PROVENANCE state (13), and the point inside it
::  ---------------------------------------------------------------------
::
::  A $point gained .seen -- the block it was most recently observed in --
::  and every point written before that has no such field.  This is the
::  state shape immediately before it: the CURRENT twelve fields, with a
::  two-field point.
::
::  The unv-ids entry is what makes this case real rather than notional.
::  A -13 state with an EMPTY index is genuinely indistinguishable from a
::  current one, and does not need to be distinguished: there is nothing
::  to convert, and the current mold takes it (which +legacy-12-state
::  already pins for the ambiguous all-empty tail).  With a point in it,
::  .seen is a TAIL field and discriminates by itself.
::
++  legacy-13-point
  ^-  *
  :*  [[0xf00d.cafe 0 0] ~]                        :: own
      0                                            :: rift
      1                                            :: life
      anew-pass                                    :: pass
      [%.y ~marzod]                                :: sponsor
      ~                                            :: escape
      ~                                            :: fief
  ==
::
++  legacy-13-state
  ^-  *
  :*  :*  `id:block:bitcoin`[0xdead.beef 961.100]  :: block-id
          *sont-map:ord                            :: sont-map
          *insc-ids:ord                            :: insc-ids
          (malt ~[[`ship`~wes legacy-13-point]])   :: unv-ids
      ==
      %.y                                          :: indexing
      `[0xdead.beef 961.100]                       :: best
      ~                                            :: inflight
      ~                                            :: confidential
      ~                                            :: attested
      0                                            :: next-job
      ~                                            :: sponsees
      (silt ~[~wes])                               :: declined
      [`custody-log:sa`~ ~]                        :: own
      %.y                                          :: synced
      ~                                            :: reorg-halt
  ==
::  a verifier result: .ok, naming .who, carrying a routable point
::
++  anew-result
  |=  [ok=? who=ship]
  ^-  [result:sa hexb:bitcoin]
  :_  *hexb:bitcoin
  :*  [who ok ~]
      ?.  ok  ~
      :-  ~
      ^-  point:urb
      :+  [[0xf00d.cafe 0 0] ~]
        [rift=0 life=1 anew-pass [%.y ~marzod] ~ ~]
      seen=`0xb10c
      0
  ==
::  the khan sign a finished verification thread delivers
::
++  anew-sign
  |=  [ok=? who=ship]
  ^-  sign-arvo
  [%khan %arow %.y %noun !>((anew-result ok who))]
::  ---------------------------------------------------------------------
::  peer verification fixtures (the %jael-writ side)
::  ---------------------------------------------------------------------
::
++  peer  ~wes
::  A state with ONE verification in flight for .peer as job 0, so an
::  /verify/~wes/0 sign lands on a live slot.  Built as a raw noun and
::  fed through +on-load, exactly like +pending-state: an inflight job is
::  otherwise unreachable from a test.
::
++  verify-state
  |=  [conf=(set ship) ats=(map ship sont:ord) ids=unv-ids:urb]
  ^-  *
  (verify-state-sm conf ats ids *sont-map:ord)
::  ... the same, with the sat index populated.  A verified tip that our
::  own index attributes to ANOTHER comet is the one local refusal that is
::  reachable in production (+local-refusal, %tip-owned).
::
++  verify-state-sm
  |=  $:  conf=(set ship)
          ats=(map ship sont:ord)
          ids=unv-ids:urb
          sm=sont-map:ord
      ==
  ^-  *
  :*  `state:urb`[[0xdead.beef 900.100] sm ~ ids]  :: urb-state
      %.y                                          :: indexing
      `[0xdead.beef 900.100]                       :: best
      ::  inflight: one $inflight-writ [dom pass sat job]
      ::
      (malt ~[[peer [%gw-btc anew-pass [peer anew-pass ~[entry0]] 0]]])
      conf                                         :: confidential
      ats                                          :: attested
      ~                                            :: publicizing
      1                                            :: next-job
      ~                                            :: sponsees
      ~                                            :: declined
      [`custody-log:sa`~ ~]                        :: own
  ==
::  a $result whose verdict FAILED on exactly the named checks
::
::  ---------------------------------------------------------------------
::  readiness fixtures (test 6.7 / Phase 6.1)
::  ---------------------------------------------------------------------
::
::  a well-formed suite-C %gw-btc pass at OUR kelvin whose xtr carries a
::  NON-EMPTY custody log.  Every gate before the readiness ones keys on
::  the pass, so a log with something in it is what it takes to reach
::  them at all -- +kelvin-pass carries `(jam ~)` and stops at %empty-log.
::
++  logged-pass
  |=  chain=custody-log:sa
  ^-  pass
  =/  seed  (shaz 'gw-btc-readiness-probe')
  pub:ex:(pit:nu:cric:crypto 512 seed %c (kelvin-dat kelvin:cc) (jam chain))
::
::  a state with a chain tip above the log's evidence and NOTHING else
::  going on, so the only thing left to decide a writ is .synced.
::
++  tip-state
  ^-  *
  :*  `state:urb`[[0xdead.beef 900.100] ~ ~ ~]   :: urb-state
      %.y                                         :: indexing
      `[0xdead.beef 900.100]                      :: best
      ~                                           :: inflight
      ~                                           :: confidential
      ~                                           :: attested
      ~                                           :: publicizing
      1                                           :: next-job
      ~                                           :: sponsees
      ~                                           :: declined
      [`custody-log:sa`~ ~]                       :: own
  ==
++  failed-sign
  |=  [who=ship bad=(list cord)]
  ^-  sign-arvo
  :^  %khan  %arow  %.y
  :-  %noun
  !>  ^-  [result:sa hexb:bitcoin]
  :_  *hexb:bitcoin
  [[who %.n (turn bad |=(c=cord `check:sa`[c %.n]))] ~ 0]
::  ---------------------------------------------------------------------
::  fixtures for a verdict that PASSED and is refused LOCALLY
::  ---------------------------------------------------------------------
::
::  the satpoint every fixture point below lands on
::
++  tip-sont  ^-(sont:ord [0xf00d.cafe 0 0])
::  a point that the agent will accept: its .pass.net is the very pass the
::  writ carried, so +attested-point-ok holds.
::
++  ok-point
  |=  =pass
  ^-  point:urb
  [[tip-sont ~] [rift=0 life=1 pass [%.y ~marzod] ~ ~] seen=`0xb10c]
::  a $result that PASSED every check, naming .who and carrying .pt
::
++  ok-sign
  |=  [who=ship pt=(unit point:urb)]
  ^-  sign-arvo
  :^  %khan  %arow  %.y
  :-  %noun
  !>  ^-  [result:sa hexb:bitcoin]
  :_  *hexb:bitcoin
  [[who %.y ~] pt 0]
::  a sat index in which .tip-sont already belongs to a DIFFERENT comet
::
++  taken-sont-map
  ^-  sont-map:ord
  %-  malt
  :~  :-  [`@ux`0xf00d.cafe `@ud`0]
      [value=9.000 sats=(malt ~[[`@ud`0 `sont-val:ord`[`~marzod ~]]])]
  ==
::  ---------------------------------------------------------------------
::  PROVENANCE fixtures: a point, the block it was last observed in, and
::  the state a ship written by THIS revision holds
::  ---------------------------------------------------------------------
::
::  +ok-point with the provenance spelled out.  .seen is what a
::  reorg filters against (+orphaned-points:uc) and what every
::  observation refreshes, so it is the one field these tests vary.
::
++  ok-point-at
  |=  [=pass seen=(unit @ux)]
  ^-  point:urb
  [[tip-sont ~] [rift=0 life=1 pass [%.y ~marzod] ~ ~] seen]
::
++  seen-of
  |=  pt=point:urb
  ^-  (unit @ux)
  seen.pt
::  the satpoint the scanner watches the identity move TO
::
++  moved-sont  ^-(sont:ord [0xfeed.face 0 0])
::
::  A CURRENT-shape state with one verification in flight for .peer, plus
::  whatever index/registry the test needs.  +verify-state builds the -12
::  shape, which predates .seen and therefore cannot carry a modern point;
::  this is what a ship running this revision actually saves.
::
++  live-state
  |=  $:  ids=unv-ids:urb
          sm=sont-map:ord
          conf=(set ship)
          ats=(map ship sont:ord)
      ==
  ^-  *
  :*  `state:urb`[[0xdead.beef 961.100] sm ~ ids]  :: urb-state
      %.y                                          :: indexing
      `[0xdead.beef 961.100]                       :: best
      (malt ~[[peer [%gw-btc anew-pass [peer anew-pass ~[entry0]] 0]]])
      conf                                         :: confidential
      ats                                          :: attested
      1                                            :: next-job
      ~                                            :: sponsees
      ~                                            :: declined
      [`custody-log:sa`~ ~]                        :: own
      %.y                                          :: synced
  ==
::  ... and the same state one revision back, when it still carried
::  .reorg-halt.  $gw-state-14 is the shape every ship that ran the
::  halting scanner saved; the field is DROPPED on load, not converted.
::
++  halted-14-state
  |=  [ids=unv-ids:urb halt=*]
  ^-  *
  :*  `state:urb`[[0xdead.beef 961.100] *sont-map:ord ~ ids]
      %.y                                          :: indexing
      `[0xdead.beef 961.100]                       :: best
      ~                                            :: inflight
      ~                                            :: confidential
      ~                                            :: attested
      1                                            :: next-job
      ~                                            :: sponsees
      (silt ~[~wes])                               :: declined
      [`custody-log:sa`~ ~]                        :: own
      %.y                                          :: synced
      halt                                         :: reorg-halt
  ==
::  the sat index that names .peer as the owner of .tip-sont
::
++  peer-sont-map
  ^-  sont-map:ord
  (put-com:si:ol *sont-map:ord 0xf00d.cafe 0 0 9.000 peer)
::  the block batch in which the scanner watches .peer's identity sat
::  MOVE -- the physical fact that makes a verified attestation stale.
::
::    base and live agree (the point sits at .tip-sont); the scanned
::    result has it at .moved-sont, in a later block.  +reconcile-block
::    takes the scanner's tip, and +detect-stale then sees it disagree
::    with the tip .peer attested to.
::
++  moved-blocks-sign
  ^-  sign-arvo
  :^  %khan  %arow  %.y
  :-  %noun
  !>  ^-  [state:urb [(list [id:block:bitcoin effect:urb]) state:urb]]
  =/  base=state:urb
    :*  [0xdead.beef 961.100]
        peer-sont-map
        *insc-ids:ord
        (malt ~[[peer (ok-point-at anew-pass `0xb.10c1)]])
    ==
  =/  result=state:urb
    :*  [0xb.10c2 961.101]
        (put-com:si:ol *sont-map:ord 0xfeed.face 0 0 9.000 peer)
        *insc-ids:ord
        %-  malt
        :~  :-  peer
            `point:urb`[[moved-sont ~] [0 1 anew-pass [%.y ~marzod] ~ ~] `0xb.10c2]
        ==
    ==
  [base [~ result]]
::  the wire a finished verification for .peer (job 0) arrives on
::
++  verify-wire  /verify/(scot %p peer)/0
::  the wire an ON-CHAIN publication's verification (job 0) arrives on.
::  Same job, same slot, same +verify-lc -- a different wire is the ONLY
::  thing that distinguishes it, and it is what decides whether an answer
::  is worth a verdict or only a log line.
::
++  claim-wire  /claim/(scot %p peer)/0
::  ---------------------------------------------------------------------
::  block-scanner fixtures: a batch that found an OP_RETURN publication
::  ---------------------------------------------------------------------
::
::  a pass whose custody log has one entry, and the ship it names
::
++  claim-pass  (logged-pass ~[entry0])
++  claimer     (fig-of claim-pass)
::  a synced state at the tip, with nothing else going on
::
++  synced-state
  ^-  *
  :*  `state:urb`[[0xdead.beef 961.100] ~ ~ ~]  :: urb-state
      %.y                                        :: indexing
      `[0xdead.beef 961.100]                     :: best
      ~                                          :: inflight
      ~                                          :: confidential
      ~                                          :: attested
      0                                          :: next-job
      ~                                          :: sponsees
      ~                                          :: declined
      [`custody-log:sa`~ ~]                      :: own
      %.y                                        :: synced
  ==
::  the khan sign +get-blocks delivers: [base [fx result]]
::
++  blocks-sign
  |=  fx=(list [id:block:bitcoin effect:urb])
  ^-  sign-arvo
  :^  %khan  %arow  %.y
  :-  %noun
  !>  ^-  [state:urb [(list [id:block:bitcoin effect:urb]) state:urb]]
  =/  st  `state:urb`[[0xdead.beef 961.100] ~ ~ ~]
  [st [fx st]]
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
::  a ship we have NOT declined draws a negative %verdict.  So
::  declining never turns into the negative verdict that would snub a
::  ship whose attestation is perfectly valid.
::
++  test-declined-writ-is-silent-contrast
  =/  agent  gw-btc
  =^  *  agent  (~(on-poke agent bowl0) %gw-sponsor-decline !>(`ship`~wes))
  ::  declined ship -> silence
  ::
  =^  c-dec  agent  (~(on-poke agent bowl0) %noun (writ-vase ~wes))
  ::  non-declined ship, same bad pass -> a negative verdict
  ::
  =^  c-und  agent  (~(on-poke agent bowl0) %noun (writ-vase ~dev))
  =/  dec  (app-cards c-dec)
  =/  und  (app-cards c-und)
  ;:  weld
    ::  declined: NO cards at all
    ::
    (expect-eq !>(~) !>(dec))
    ::  non-declined: exactly one %verdict fact
    ::
    (expect-eq !>(1) !>((lent und)))
    (expect-eq !>(`%verdict) !>((fact-mark (snag 0 und))))
    ::  and that verdict is NEGATIVE (res=~): a %fail, the very
    ::  outcome the declined path must never produce.
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
    ::  our kelvin, same construction: exactly one NEGATIVE verdict
    ::
    (expect-eq !>(1) !>((lent own)))
    (expect-eq !>(`%verdict) !>((fact-mark (snag 0 own))))
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
    (expect-eq !>(`%verdict) !>((fact-mark (snag 0 untyped))))
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
::  THE INDEX STARTS ONLY WHERE IT WAS TOLD TO.  A virgin index used to
::  bootstrap itself from +gw-epoch the first time the light client
::  reported synced -- a day of pinned CPU on every fresh comet, chosen
::  by nobody, and on the first real mints it fired because Causeway had
::  failed to hand the comet its sponsor.  Now it parks in %pending and
::  says so; an %index-origin poke (boot.sh after installing the sponsor,
::  or the user in the Gevulot pane) is what starts it.
::  ops/doc/gevulot-state-model.md section 2.
::
++  scan-origin-mode
  |=  scn=*
  ^-  @tas
  =/  sc
    ;;  $:  cursor=@ud  epoch=@ud  tip=(unit @ud)  confirmations=@ud
            batch=@ud  indexing=?  points=@ud
            origin=[mode=@tas start=@ud by=@tas at=@da]
        ==
    scn
  mode.origin.sc
::
++  test-index-parks-pending-without-an-origin
  =/  agent  gw-btc
  =^  *  agent  ~(on-init agent bowl0)
  =^  *  agent  (~(on-agent agent bowl0) /best-block (new-block-sign 963.150))
  =^  cards  agent
    (~(on-agent agent bowl0) /is-synced [%fact %bitcoin-client-is-synced !>(&)])
  =/  bid  (peek-noun (~(on-peek agent bowl0) /x/block-id))
  =/  rdy  (peek-noun (~(on-peek agent bowl0) /x/ready))
  =/  scn  (peek-noun (~(on-peek agent bowl0) /x/scan))
  ;:  weld
    ::  nothing moved ...
    (expect-eq !>(`id:block:bitcoin`[0x0 0]) !>(;;(id:block:bitcoin bid)))
    ::  ... synced, NOT indexing ...
    (expect-eq !>(`*`[%.y [~ 963.150] %.n]) !>(`*`rdy))
    ::  ... no timer, and the pane can see why
    (expect-eq !>(0) !>((lent (app-cards cards))))
    (expect-eq !>(%pending) !>((scan-origin-mode scn)))
  ==
::  An %index-origin poke on a synced ship starts the scanner where it
::  says, at once: the cursor lands one below the start, the timer is
::  armed, and the provenance is recorded.
::
++  test-index-origin-poke-starts-from-the-epoch
  =/  agent  gw-btc
  =^  *  agent  ~(on-init agent bowl0)
  =^  *  agent  (~(on-agent agent bowl0) /best-block (new-block-sign 963.150))
  =^  *  agent
    (~(on-agent agent bowl0) /is-synced [%fact %bitcoin-client-is-synced !>(&)])
  =^  cards  agent
    (~(on-poke agent bowl0) %noun !>([%index-origin %from-epoch 0 %user]))
  =/  bid  (peek-noun (~(on-peek agent bowl0) /x/block-id))
  =/  rdy  (peek-noun (~(on-peek agent bowl0) /x/ready))
  =/  scn  (peek-noun (~(on-peek agent bowl0) /x/scan))
  ;:  weld
    (expect-eq !>(`id:block:bitcoin`[0x0 963.103]) !>(;;(id:block:bitcoin bid)))
    (expect-eq !>(`*`[%.y [~ 963.150] %.y]) !>(`*`rdy))
    (expect-eq !>(1) !>((lent (app-cards cards))))
    (expect-eq !>(%from-epoch) !>((scan-origin-mode scn)))
  ==
::  ... but never on a chain that has not reached the start: regtest and
::  the harness keep waiting; the decision is kept and applied once the
::  chain gets there.
::
++  test-index-origin-waits-for-the-chain
  =/  agent  gw-btc
  =^  *  agent  ~(on-init agent bowl0)
  =^  *  agent  (~(on-agent agent bowl0) /best-block (new-block-sign 500))
  =^  *  agent
    (~(on-poke agent bowl0) %noun !>([%index-origin %from-epoch 0 %user]))
  =^  cards  agent
    (~(on-agent agent bowl0) /is-synced [%fact %bitcoin-client-is-synced !>(&)])
  =/  bid  (peek-noun (~(on-peek agent bowl0) /x/block-id))
  =/  scn  (peek-noun (~(on-peek agent bowl0) /x/scan))
  ;:  weld
    (expect-eq !>(`id:block:bitcoin`[0x0 0]) !>(;;(id:block:bitcoin bid)))
    (expect-eq !>(0) !>((lent (app-cards cards))))
    (expect-eq !>(%from-epoch) !>((scan-origin-mode scn)))
  ==
::  ... and never over an index that exists: an operator who chose a
::  start point keeps it, and a second sync transition cannot rewind a
::  scanner that has already moved.
::
++  test-epoch-auto-bootstrap-defers-to-an-existing-index
  =/  agent  gw-btc
  =^  *  agent  ~(on-init agent bowl0)
  =^  *  agent  (~(on-poke agent bowl0) %gw-index-from !>(`@ud`961.055))
  =^  *  agent  (~(on-agent agent bowl0) /best-block (new-block-sign 963.150))
  =^  cards  agent
    (~(on-agent agent bowl0) /is-synced [%fact %bitcoin-client-is-synced !>(&)])
  =/  bid  (peek-noun (~(on-peek agent bowl0) /x/block-id))
  ;:  weld
    (expect-eq !>(`id:block:bitcoin`[0x0 961.054]) !>(;;(id:block:bitcoin bid)))
    (expect-eq !>(0) !>((lent (app-cards cards))))
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
    ::  ... and the ONLY card is the /is-synced subscription the readiness
    ::  gate needs.  +on-init issues it for a fresh install; an upgraded
    ::  ship would otherwise never have one, .synced would stay %.n, and
    ::  the ship would hold every attestation forever -- safe, but dead.
    (expect-eq !>(~[synced-watch]) !>((app-cards cards)))
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
::
::  ---------------------------------------------------------------------
::  %anew -- extending our own custody log in band
::
::  THE safety property: %gw-btc never answers %anew with a pass it has
::  not just proved against the chain.  Every negative path below is
::  SILENCE, not a wrong answer.
::  ---------------------------------------------------------------------
::
::  A bare %jael-anew on a fresh agent emits nothing.  ~zod is not a
::  %pawn, so there is no confidential identity to refresh -- and the old
::  implementation's failure mode (silence) is still the right ANSWER
::  here; what changed is that a real comet now gets a verdict-backed
::  pass instead.
::
++  test-anew-with-no-custody-log-is-silent
  =/  agent  gw-btc
  =^  cards  agent  (~(on-poke agent bowl0) %noun anew-vase)
  (expect-eq !>(~) !>((app-cards cards)))
::
::  A Causeway ingestion poke is accepted as a POKE (it must not crash
::  the agent, which is what the old code did -- ;;(jael-poke ...) bails
::  on an unknown head tag), but emits nothing until it has been proved.
::  Typed and `*`-typed vases must behave identically, exactly as for
::  %jael-writ.
::
++  test-custody-entry-poke-does-not-crash-and-is-silent
  =/  agent  gw-btc
  =^  c-typed    agent  (~(on-poke agent bowl0) %noun (ingest-vase entry0))
  =^  c-untyped  agent  (~(on-poke agent bowl0) %noun (ingest-vase-untyped entry0))
  ;:  weld
    (expect-eq !>(~) !>((app-cards c-typed)))
    (expect-eq !>(~) !>((app-cards c-untyped)))
    ::  and nothing was stored on the strength of the poke alone
    (expect-eq !>(`custody-log:sa`~) !>(;;(custody-log:sa (peek-noun (~(on-peek agent bowl0) /x/custody)))))
  ==
::
::  A POSITIVE verdict for OUR ship publishes exactly the pass that was
::  verified, on /writs, and stores the candidate log.
::
++  test-anew-positive-verdict-publishes-the-verified-pass
  =/  agent  gw-btc
  =^  *      agent  (~(on-load agent bowl0) !>((pending-state ~)))
  =^  cards  agent  (~(on-arvo agent bowl0) /anew/0 (anew-sign %.y ~zod))
  =/  out  (app-cards cards)
  ;:  weld
    (expect-eq !>(1) !>((lent out)))
    (expect-eq !>(`%anew-response) !>((fact-mark (snag 0 out))))
    ::  the fact carries [dom pass], and the pass is byte-identical to
    ::  the one the light client just walked
    %+  expect-eq
      !>  `[@tas pass]`[%gw-btc anew-pass]
      !>  ^-  [@tas pass]
          ?~  pay=(fact-payload (snag 0 out))  [%$ 0x0]
          ;;([dom=@tas =pass] u.pay)
    ::  ... and the validated log is now ours
    (expect-eq !>(cand) !>(;;(custody-log:sa (peek-noun (~(on-peek agent bowl0) /x/custody)))))
  ==
::
::  A NEGATIVE verdict answers with silence and leaves the previous log
::  alone.  Emitting the candidate here is the one thing that must never
::  happen: ames would install a pass no peer will accept.
::
++  test-anew-negative-verdict-is-silent-and-keeps-the-old-log
  =/  agent  gw-btc
  =^  *      agent  (~(on-load agent bowl0) !>((pending-state ~[entry0])))
  =^  cards  agent  (~(on-arvo agent bowl0) /anew/0 (anew-sign %.n ~zod))
  ;:  weld
    (expect-eq !>(~) !>((app-cards cards)))
    (expect-eq !>(`custody-log:sa`~[entry0]) !>(;;(custody-log:sa (peek-noun (~(on-peek agent bowl0) /x/custody)))))
  ==
::
::  A verdict that is positive but names a DIFFERENT ship is not our
::  refresh.  Publishing it would hand ames a pass for someone else.
::
++  test-anew-verdict-for-another-ship-is-silent
  =/  agent  gw-btc
  =^  *      agent  (~(on-load agent bowl0) !>((pending-state ~)))
  =^  cards  agent  (~(on-arvo agent bowl0) /anew/0 (anew-sign %.y ~marzod))
  ;:  weld
    (expect-eq !>(~) !>((app-cards cards)))
    (expect-eq !>(`custody-log:sa`~) !>(;;(custody-log:sa (peek-noun (~(on-peek agent bowl0) /x/custody)))))
  ==
::
::  A verdict for a job that is not the one in flight is ignored: a
::  stale thread must not be able to install a log.
::
++  test-anew-stale-job-verdict-is-ignored
  =/  agent  gw-btc
  =^  *      agent  (~(on-load agent bowl0) !>((pending-state ~)))
  =^  cards  agent  (~(on-arvo agent bowl0) /anew/7 (anew-sign %.y ~zod))
  ;:  weld
    (expect-eq !>(~) !>((app-cards cards)))
    (expect-eq !>(`custody-log:sa`~) !>(;;(custody-log:sa (peek-noun (~(on-peek agent bowl0) /x/custody)))))
  ==
::
::  The leak backstop releases the slot and emits NOTHING -- a job that
::  died silently is infrastructure failure, never evidence.
::
++  test-anew-guard-releases-the-slot-without-a-verdict
  =/  agent  gw-btc
  =^  *      agent  (~(on-load agent bowl0) !>((pending-state ~)))
  =^  cards  agent
    (~(on-arvo agent bowl0) /anew-guard/0 `sign-arvo`[%behn %wake ~])
  ;:  weld
    (expect-eq !>(~) !>((app-cards cards)))
    ::  slot released: a verdict for job 0 no longer applies
    =^  late  agent  (~(on-arvo agent bowl0) /anew/0 (anew-sign %.y ~zod))
    (expect-eq !>(~) !>((app-cards late)))
  ==
::
::  ---------------------------------------------------------------------
::  Staleness is not fraud, on the PACKET path
::
::  Decisions addendum section 3.  A peer's attestation whose tip our own
::  filter scan finds SPENT is out of date, not forged.  Answering it with
::  res=~ makes jael %fail and ames SNUB -- and the snub then blocks the
::  refreshed attestation that would fix it, which on mainnet left a comet
::  permanently unreachable from its own sponsor.  It must take the same
::  %stale route the scanner path takes.
::  ---------------------------------------------------------------------
::
++  test-stale-tip-verdict-emits-stale-not-fail
  =/  agent  gw-btc
  =^  *      agent  (~(on-load agent bowl0) !>((verify-state ~ ~ ~)))
  =^  cards  agent
    (~(on-arvo agent bowl0) /verify/(scot %p peer)/0 (failed-sign peer ~['tip-unspent']))
  =/  out  (app-cards cards)
  ;:  weld
    (expect-eq !>(1) !>((lent out)))
    ::  a %stale-notice, NOT a %verdict: no %fail, so no snub
    ::
    (expect-eq !>(`%stale-notice) !>((fact-mark (snag 0 out))))
    %+  expect-eq
      !>  `[@tas ship]`[%gw-btc peer]
      !>  ^-  [@tas ship]
          ?~  pay=(fact-payload (snag 0 out))  [%$ ~zod]
          ;;([dom=@tas =ship] u.pay)
    ::  and the slot is released either way, so the replacement
    ::  attestation can re-enter
    ::
    (expect-eq !>(*(set ship)) !>(;;((set ship) (peek-noun (~(on-peek agent bowl0) /x/inflight)))))
  ==
::
::  ---------------------------------------------------------------------
::  A DEEP LAG IS STALE AND EMITS NO SNUB
::  ---------------------------------------------------------------------
::
::  `%behind =(1 by.rel)' in +anchor-ok made a log more than one custody
::  entry behind FRAUD -- a permanent ames snub.  A lag is exactly what a
::  REPLAYED packet looks like (ames has already proved the pass hashes to
::  the claimed @p, so nobody can forge one, but anyone who saw one can
::  re-send it, and a replay is always a prefix and never a fork), so that
::  handed any observer a way to sever two honest ships for good.
::
::  The check list below is DERIVED FROM THE RULE rather than written
::  down: `tracked-prefix' carries +anchor-ok's verdict on the relation,
::  so a rule that condemned a five-deep lag would put that name in the
::  failing list and this test would see a %verdict card instead of a
::  %stale-notice.  That is what makes it a mutation test and not a
::  restatement of the classifier.
::
++  test-deep-lag-is-stale-and-never-snubs
  =/  agent  gw-btc
  =^  *  agent  (~(on-load agent bowl0) !>((verify-state ~ ~ ~)))
  ::  what a log behind by FIVE, from a tracked peer, really fails on:
  ::  its tip is not the one we tracked, and the lag is reported.
  ::
  =/  bad=(list cord)
    %+  weld  ~['tracked-tip' 'tracked-lag']
    ?:  (anchor-ok:lsa [%behind 5] ~ tip-sont)  ~
    ~['tracked-prefix']
  =^  cards  agent  (~(on-arvo agent bowl0) verify-wire (failed-sign peer bad))
  =/  out  (app-cards cards)
  ;:  weld
    ::  the rule itself: any lag reconciles, at any depth
    (expect !>((anchor-ok:lsa [%behind 5] ~ tip-sont)))
    ::  ... so the verdict is stale-class ...
    %+  expect-eq
      !>  %stale
      !>  (classify:lsa `verdict:sa`[peer %.n (turn bad |=(c=cord [c %.n]))])
    ::  ... and the peer is DEMOTED, not condemned: one card, a
    ::  %stale-notice naming it, and no %verdict anywhere.
    (expect-eq !>(1) !>((lent out)))
    (expect-eq !>(`%stale-notice) !>((fact-mark (snag 0 out))))
    (expect !>(!(lien out |=(c=card:agent:gall =(`%verdict (fact-mark c))))))
    %+  expect-eq
      !>  `[@tas ship]`[%gw-btc peer]
      !>  ^-  [@tas ship]
          ?~  pay=(fact-payload (snag 0 out))  [%$ ~zod]
          ;;([dom=@tas =ship] u.pay)
    ::  and the single-flight slot is released, so the replacement
    ::  attestation can re-enter
    (expect-eq !>(*(set ship)) !>(;;((set ship) (peek-noun (~(on-peek agent bowl0) /x/inflight)))))
  ==
::
++  test-fraud-verdict-still-fails
  ::  the control: the SAME machinery, one genuinely-invalid check, still
  ::  produces the negative verdict that snubs.
  ::
  =/  agent  gw-btc
  =^  *      agent  (~(on-load agent bowl0) !>((verify-state ~ ~ ~)))
  =^  cards  agent
    (~(on-arvo agent bowl0) /verify/(scot %p peer)/0 (failed-sign peer ~['spawn-matches']))
  =/  out  (app-cards cards)
  ;:  weld
    (expect-eq !>(1) !>((lent out)))
    (expect-eq !>(`%verdict) !>((fact-mark (snag 0 out))))
    ::  res=~ is the %fail
    ::
    %+  expect-eq
      !>  `(unit *)`~
      !>  ^-  (unit *)
          ?~  pay=(fact-payload (snag 0 out))  `**
          res:;;([dom=@tas =ship res=(unit *)] u.pay)
  ==
::
++  test-fraud-alongside-staleness-still-fails
  ::  a spent tip does not launder a forged commitment
  ::
  =/  agent  gw-btc
  =^  *      agent  (~(on-load agent bowl0) !>((verify-state ~ ~ ~)))
  =/  sign  (failed-sign peer ~['tip-unspent' 'entry-0-commitment'])
  =^  cards  agent  (~(on-arvo agent bowl0) /verify/(scot %p peer)/0 sign)
  =/  out  (app-cards cards)
  ;:  weld
    (expect-eq !>(1) !>((lent out)))
    (expect-eq !>(`%verdict) !>((fact-mark (snag 0 out))))
  ==
::
::  UPGRADE.  A pre-%anew state (10 fields, no .own) must load with an
::  empty custody log rather than crashing -- an agent that will not load
::  answers no attestation at all.
::
::  +on-load drops .publicizing, which was a MIDDLE field.  Nothing
::  reads it any more: a publication is no longer indexed by the block
::  scanner, it is handed to the same verifier a packet goes to, so
::  there is no public-spawn replay to guard and .inflight is the only
::  single-flight left.
::
::  The migration is only safe because the TAIL discriminates: a -12
::  noun offered to the -13 mold puts [synced reorg-halt] where the halt
::  unit is expected, and neither shape of that pair can read as one.
::  Prove it on a state where every field the shift would land on is ~/0
::  -- i.e. the case where arity alone would NOT have told them apart --
::  and check that a value AFTER the removed field still arrives where it
::  belongs.
::
++  test-on-load-drops-the-publicizing-field
  =/  agent  gw-btc
  =^  cards  agent  (~(on-load agent bowl0) !>(legacy-12-state))
  ;:  weld
    ::  urb-state came through
    ::
    %+  expect-eq
      !>  `id:block:bitcoin`[0xdead.beef 961.100]
      !>  ;;(id:block:bitcoin (peek-noun (~(on-peek agent bowl0) /x/block-id)))
    ::  .declined sits three fields PAST the one we removed; if the tail
    ::  had shifted it would be empty (or the cast would have taken the
    ::  wrong branch entirely)
    ::
    (expect-eq !>((silt ~[~wes])) !>(;;((set ship) (peek-noun (~(on-peek agent bowl0) /x/declined)))))
    ::  .synced and .reorg-halt are the tail that did the discriminating;
    ::  .reorg-halt is then DROPPED, so /x/ready is three fields
    ::
    %+  expect-eq
      !>  [synced=%.y tip=`961.100 indexing=%.y]
      !>  ;;([? (unit @ud) ?] (peek-noun (~(on-peek agent bowl0) /x/ready)))
    ::  a -12 state already had /is-synced; do not re-subscribe
    ::
    (expect-eq !>(~) !>((app-cards cards)))
  ==
::
++  test-on-load-migrates-the-pre-anew-state
  =/  agent  gw-btc
  =^  cards  agent  (~(on-load agent bowl0) !>(legacy-10-state))
  ;:  weld
    %+  expect-eq
      !>  `id:block:bitcoin`[0xdead.beef 943.140]
      !>  ;;(id:block:bitcoin (peek-noun (~(on-peek agent bowl0) /x/block-id)))
    (expect-eq !>(`custody-log:sa`~) !>(;;(custody-log:sa (peek-noun (~(on-peek agent bowl0) /x/custody)))))
    (expect-eq !>(~[synced-watch]) !>((app-cards cards)))
  ==
::  ---------------------------------------------------------------------
::  READINESS AND THE THIRD VERDICT CLASS  (Phase 6.1, live mainnet)
::  ---------------------------------------------------------------------
::
::  C3 judged C1's attestation with its light client at GENESIS and its
::  block scanner 20 blocks below C3's own publication.  43 of 44 checks
::  passed; `sponsor-known` failed because C3 did not yet know that C3
::  existed.  The verdict became a jael %fail and ames snubbed -- a comet
::  snubbed by its own sponsor, for naming that sponsor.  Two hours later
::  the identical attestation from the identical peer verified VALID with
::  nothing changed but the scan position.
::
::  So: a verdict whose failures are all UNEVALUABLE must produce no
::  cards at all.  Not %verdict (a snub), not %stale-notice (a
::  demotion on a finding we did not make).  Nothing.
::
::  +on-load must PRESERVE the bootstrap flag.  It did not: .indexing was
::  declared `_|` to force its bunt, and `_` is $_ -- the mold that IGNORES
::  its input and returns the pinned value -- so `;;(gw-state ...)` rewrote
::  it to %.n on EVERY upgrade.  That reopened the one-shot guard below,
::  after which a %gw-index-from would silently replace urb-state wholesale,
::  including .unv-ids, which is the set `sponsor-known` reads.  Observed on
::  a live mainnet ship 2026-08-06: %.y before a desk redeploy, %.n after,
::  nothing else changed.
::
++  test-on-load-preserves-the-bootstrap-guard
  =/  agent  gw-btc
  =^  *  agent  (~(on-poke agent bowl0) %gw-index-from !>(`@ud`961.055))
  =^  *  agent  (~(on-load agent bowl0) ~(on-save agent bowl0))
  ::  a second bootstrap, at a DIFFERENT height, must still be refused
  =^  *  agent  (~(on-poke agent bowl0) %gw-index-from !>(`@ud`700.000))
  %+  expect-eq
    !>  `id:block:bitcoin`[0x0 961.054]
    !>  ;;(id:block:bitcoin (peek-noun (~(on-peek agent bowl0) /x/block-id)))
::
::  ... and the guard holds even for an agent whose flag says otherwise:
::  a populated index is refused on its own evidence.
::
++  test-bootstrap-refused-when-an-index-exists
  =/  agent  gw-btc
  ::  a state that HAS an index but whose .indexing flag is %.n -- exactly
  ::  what every pre-fix upgrade produced
  =^  *  agent  (~(on-load agent bowl0) !>(legacy-10-state))
  =^  *  agent  (~(on-poke agent bowl0) %gw-index-from !>(`@ud`700.000))
  %+  expect-eq
    !>  `id:block:bitcoin`[0xdead.beef 943.140]
    !>  ;;(id:block:bitcoin (peek-noun (~(on-peek agent bowl0) /x/block-id)))
::
::  ---------------------------------------------------------------------
::  CHAIN REORGS  (Phase 6.5; repaired rather than halted since
::  gwbtc/node@063720b9)
::  ---------------------------------------------------------------------
::
::  %bitcoin-client reports a reorg as %reorg-rollback on /best-block, and
::  this agent once handled it byte-identically to %new: only .best moved.
::  The scan cursor never rewound, so the orphaned range was never
::  rescanned; facts indexed out of orphaned blocks stayed in .unv-ids
::  forever; and facts unique to the winning chain fell in the skipped
::  range.  All silently.  With block-confirmations = 1 a single-block
::  reorg reaches it, and those happen several times a month on mainnet.
::
::  From 2026-08-06 the scanner HALTED instead, because a $point recorded
::  no provenance and %reorg-rollback named no losers: there was nothing
::  to filter and no way to tell a good fact from a bad one.  Both halves
::  exist now -- .seen on a $point, .stale-branch on the fact -- so the
::  index is REPAIRED: select the points the orphaned blocks were the
::  evidence for, forget exactly those, rewind the cursor to the fork
::  point.  The halt, its poke, its timer and its /x/ready field are gone.
::
::  .last-common is the FORK POINT, not the new tip; .stale-branch is the
::  losing chain above it, latest first.
::
::  Both signs are written STRUCTURALLY rather than by naming
::  best-block:update:lc.  What arrives is the node desk's own type, and
::  all this agent may rely on is that it NESTS -- building the fixture
::  from our own mold would prove only that our mold equals itself.
::
++  rollback-sign
  |=  [fork=@ud stale=(list [@ud @ux])]
  ^-  sign:agent:gall
  ::  the mark %bitcoin-client uses, named for its path in node's desk.
  ::  A test that synthesizes the OLD name does not fail loudly -- it
  ::  falls through to on-agent:def and CRASHES, which reads as a broken
  ::  test rather than as the agent correctly refusing an unknown mark.
  :+  %fact  %bitcoin-client-best-block
  !>  ^-  $:  %reorg-rollback
              last-common=[block-height=@ud block-hash=@ux]
              stale-branch=(list [block-height=@ud block-hash=@ux])
          ==
  [%reorg-rollback [fork 0xf0.0000] stale]
::
++  new-block-sign
  |=  height=@ud
  ^-  sign:agent:gall
  :+  %fact  %bitcoin-client-best-block
  !>(`[%new block-height=@ud block-hash=@ux]`[%new height 0xbeef])
::  four points, one per case the selector has to tell apart.  .peer is
::  CONFIDENTIAL (so it also carries an attested tip and an in-flight
::  job); the rest are public, which is what lets /x/urb-state witness
::  them.
::
::    ~wes  0xb.10c1  in the losing branch  -> forgotten
::    ~des  0xb.10c1  in the losing branch  -> forgotten
::    ~nec  0xb.10c9  on the winning chain  -> kept
::    ~lex  ~         NO PROVENANCE         -> kept, always
::
++  reorg-ids
  ^-  unv-ids:urb
  %-  malt
  :~  [peer (ok-point-at anew-pass `0xb.10c1)]
      [~des (ok-point-at anew-pass `0xb.10c1)]
      [~nec (ok-point-at anew-pass `0xb.10c9)]
      [~lex (ok-point-at anew-pass ~)]
  ==
::  +live-state's cursor is 961.100, so a fork at 961.098 is BELOW it and
::  the two blocks above the fork are ones we had already scanned.
::
++  reorg-agent
  ^-  _gw-btc
  =/  agent  gw-btc
  =^  *  agent
    %-  ~(on-load agent bowl0)
    !>  %^  live-state  reorg-ids  *sont-map:ord
        [(silt ~[peer]) (malt ~[[peer tip-sont]])]
  agent
::
++  orphaned-branch  ^-((list [@ud @ux]) ~[[961.100 0xb.10c2] [961.099 0xb.10c1]])
++  untouched-branch  ^-((list [@ud @ux]) ~[[961.100 0xbad.0002] [961.099 0xbad.0001]])
::  the sign a block thread delivers when it was ALREADY RUNNING when a
::  %reorg-rollback landed.  Its base is the index as it stood BEFORE the
::  repair -- every point still in it, cursor at 961.100 -- and its result
::  carries that same index forward to 961.102, on the branch that turned
::  out to lose.
::
++  stale-blocks-sign
  ^-  sign-arvo
  :^  %khan  %arow  %.y
  :-  %noun
  !>  ^-  [state:urb [(list [id:block:bitcoin effect:urb]) state:urb]]
  =/  base=state:urb    [[0xdead.beef 961.100] ~ ~ reorg-ids]
  =/  result=state:urb  [[0xb.10c4 961.102] ~ ~ reorg-ids]
  [base [~ result]]
::
++  indexed-ships
  |=  agent=_gw-btc
  ^-  (set ship)
  =/  st  ;;(state:urb (peek-noun (~(on-peek agent bowl0) /x/urb-state)))
  ~(key by unv-ids.st)
::
::  A reorg whose losing branch holds a point's .seen forgets EXACTLY that
::  point, by the same road a moved identity sat takes.
::
++  test-reorg-forgets-the-points-the-orphaned-blocks-proved
  =/  agent  reorg-agent
  =^  cards  agent
    (~(on-agent agent bowl0) /best-block (rollback-sign 961.098 orphaned-branch))
  =/  out    (app-cards cards)
  =/  notes  (skim out |=(c=card:agent:gall =(`%stale-notice (fact-mark c))))
  ;:  weld
    ::  ~des was in the losing branch and is gone; ~nec was not and is
    ::  still here; ~lex has NO provenance and is therefore left alone --
    ::  ~ means "we cannot tell whether this was orphaned", which is not
    ::  the claim that it was, and an unevaluable condition never produces
    ::  a negative outcome.  (~wes is confidential, so /x/urb-state hides
    ::  it either way; the stores below are what witness it.)
    ::
    (expect-eq !>((silt ~[`ship`~nec ~lex])) !>((indexed-ships agent)))
    ::  the confidential stores that describe ~wes are all cleared
    (expect-eq !>(*(map ship sont:ord)) !>(;;((map ship sont:ord) (peek-noun (~(on-peek agent bowl0) /x/attested)))))
    (expect-eq !>(*(set ship)) !>(;;((set ship) (peek-noun (~(on-peek agent bowl0) /x/inflight)))))
    ::  ONE %stale-notice per forgotten point, and nothing else at all
    (expect-eq !>(2) !>((lent notes)))
    (expect-eq !>(2) !>((lent out)))
    ::  ... and NEVER a %verdict.  A snub is permanent on every transport,
    ::  so spending one on a chain event would need an operator to undo it
    ::  -- and it would block the re-attestation that corrects us.
    (expect !>(!(lien out |=(c=card:agent:gall =(`%verdict (fact-mark c))))))
    ::  the cursor rewound to the FORK POINT, so the scanner walks the
    ::  winning branch from 961.099 on the next tick
    %+  expect-eq
      !>  `id:block:bitcoin`[0xf0.0000 961.098]
      !>  ;;(id:block:bitcoin (peek-noun (~(on-peek agent bowl0) /x/block-id)))
  ==
::
::  A reorg that orphaned blocks none of our points came from forgets
::  NOTHING -- including the hashless point, which the old rule would have
::  taken on any reorg at all.  The cursor still rewinds: those blocks did
::  not happen, whatever they did or did not contain.
::
++  test-reorg-that-misses-our-points-forgets-nothing
  =/  agent  reorg-agent
  =^  cards  agent
    (~(on-agent agent bowl0) /best-block (rollback-sign 961.098 untouched-branch))
  ;:  weld
    (expect-eq !>((silt ~[`ship`~des ~nec ~lex])) !>((indexed-ships agent)))
    ::  ~wes keeps its attested tip and its in-flight job
    %+  expect-eq
      !>  (malt ~[[`ship`peer tip-sont]])
      !>  ;;((map ship sont:ord) (peek-noun (~(on-peek agent bowl0) /x/attested)))
    (expect-eq !>((silt ~[peer])) !>(;;((set ship) (peek-noun (~(on-peek agent bowl0) /x/inflight)))))
    ::  no cards: nothing was un-known, so nobody is told anything
    (expect-eq !>(~) !>((app-cards cards)))
    %+  expect-eq
      !>  `id:block:bitcoin`[0xf0.0000 961.098]
      !>  ;;(id:block:bitcoin (peek-noun (~(on-peek agent bowl0) /x/block-id)))
  ==
::
::  A fork point ABOVE the cursor is harmless: every orphaned block is one
::  the scanner had not reached.  Note it, move .best, touch nothing --
::  and in particular do NOT drag the cursor forward.
::
++  test-reorg-above-the-cursor-changes-nothing
  =/  agent  reorg-agent
  =^  cards  agent
    %-  ~(on-agent agent bowl0)
    [/best-block (rollback-sign 961.200 ~[[961.202 0xb.10c1]])]
  =/  ready  (peek-noun (~(on-peek agent bowl0) /x/ready))
  ;:  weld
    ::  .best moved to the fork point ...
    (expect-eq !>(`*`[~ 961.200]) !>(`*`-:+:ready))
    ::  ... and the cursor did not move, even though 0xb.10c1 is named in
    ::  the branch: those blocks are above us, so nothing we hold is from
    ::  them, and a point is not forgotten on a coincidence of hashes we
    ::  never scanned.
    %+  expect-eq
      !>  `id:block:bitcoin`[0xdead.beef 961.100]
      !>  ;;(id:block:bitcoin (peek-noun (~(on-peek agent bowl0) /x/block-id)))
    (expect-eq !>((silt ~[`ship`~des ~nec ~lex])) !>((indexed-ships agent)))
    (expect-eq !>(~) !>((app-cards cards)))
  ==
::
::  ... and the scanner is NOT stopped by any of it.  This is the whole
::  point of removing the halt: the very next tick dispatches a block
::  thread, from the rewound cursor.
::
++  test-scanner-advances-after-a-reorg
  =/  agent  reorg-agent
  =^  *      agent
    (~(on-agent agent bowl0) /best-block (rollback-sign 961.098 orphaned-branch))
  =^  cards  agent  (~(on-arvo agent bowl0) /timer [%behn %wake ~])
  =/  cs  (app-cards cards)
  ;:  weld
    (expect-eq !>(1) !>((lent cs)))
    ::  a %lard block thread, NOT a bare timer re-arm
    (expect !>(?=([[%pass [%blocks ~] %arvo %k %lard *] ~] cs)))
  ==
::
::  THE RACE.  +get-blocks awaits each block from the light client, so a
::  thread genuinely spans events and a %reorg-rollback lands in the
::  middle of one.  The rollback forgets the orphaned points and rewinds
::  the cursor; the thread then returns from a base that predates both,
::  and merging it undoes the entire repair.
::
::  Losing the rewind is the mild half -- the winning branch's blocks are
::  never scanned and a %urb-state fact goes out for an index spliced
::  from two chains.  The severe half is the points: a forgotten
::  CONFIDENTIAL comet comes back into .unv-ids ALONE, because
::  .confidential and .attested are separate legs the merge never
::  restores.  +known-public is then true for a comet that never
::  published, and its next %jael-writ is dropped %already-public --
::  silently declassified, and unverifiable from then on.
::
::  +reconcile-block cannot catch this by iterating .conf, which is what
::  it does for the verifier race: +forget-points drops a ship from the
::  index and from .conf in LOCKSTEP, so an orphaned ship is in neither
::  and the loop never examines it.  Comparing the two indexes directly
::  is the only test that sees them.
::
++  test-a-stale-batch-cannot-undo-a-reorg
  =/  agent  reorg-agent
  =^  *  agent
    (~(on-agent agent bowl0) /best-block (rollback-sign 961.098 orphaned-branch))
  =/  repaired  (indexed-ships agent)
  ::  ... and only NOW does the in-flight thread return.
  =^  cards  agent  (~(on-arvo agent bowl0) /blocks stale-blocks-sign)
  =/  st  ;;(state:urb (peek-noun (~(on-peek agent bowl0) /x/urb-state)))
  =/  cs  (app-cards cards)
  ;:  weld
    ::  ~des was proved by an orphaned block and forgotten; the stale
    ::  batch, whose base still holds it, does not bring it back.
    ::
    (expect !>(!(~(has in (indexed-ships agent)) ~des)))
    (expect-eq !>(repaired) !>((indexed-ships agent)))
    ::  and the cursor is not dragged forward onto the losing branch
    ::
    (expect-eq !>(961.098) !>(num.block-id.st))
    ::  the batch is discarded and retried from the unchanged cursor,
    ::  with no %urb-state fact published for it
    ::
    (expect-eq !>(1) !>((lent cs)))
    (expect !>(?=([[%pass [%timer ~] %arvo %b %wait *] ~] cs)))
  ==
::
::  and a plain %new still just moves the tip.
::
++  test-new-block-moves-the-tip
  =/  agent  gw-btc
  =^  *  agent  (~(on-load agent bowl0) !>((verify-state ~ ~ ~)))
  =^  *  agent  (~(on-agent agent bowl0) /best-block (new-block-sign 900.101))
  =/  ready  (peek-noun (~(on-peek agent bowl0) /x/ready))
  (expect-eq !>(`*`[~ 900.101]) !>(`*`-:+:ready))
::
::  UPGRADE FROM THE HALT.  A -14 state carries .reorg-halt; the field is
::  dropped, not converted, and a ship that was halted comes back
::  SCANNING.  It cannot repair the index for the reorg that stopped it --
::  that orphan list is long gone -- so it does exactly what
::  %gw-reorg-resume ~ did, and the next reorg is repaired properly.
::
++  test-on-load-drops-a-reorg-halt
  =/  agent  gw-btc
  =^  cards  agent
    %-  ~(on-load agent bowl0)
    !>((halted-14-state reorg-ids [~ 961.098 961.100 `@da`~2000.1.1]))
  =^  ticks  agent  (~(on-arvo agent bowl0) /timer [%behn %wake ~])
  ;:  weld
    ::  the index survived in full: a halt is not a reason to forget
    (expect-eq !>((silt ~[`ship`peer ~des ~nec ~lex])) !>((indexed-ships agent)))
    ::  the cursor is where the halt left it
    %+  expect-eq
      !>  `id:block:bitcoin`[0xdead.beef 961.100]
      !>  ;;(id:block:bitcoin (peek-noun (~(on-peek agent bowl0) /x/block-id)))
    ::  /x/ready is three fields now; there is no halt to report
    %+  expect-eq
      !>  [synced=%.y tip=`961.100 indexing=%.y]
      !>  ;;([? (unit @ud) ?] (peek-noun (~(on-peek agent bowl0) /x/ready)))
    ::  a -14 state already had /is-synced; do not re-subscribe
    (expect-eq !>(~) !>((app-cards cards)))
    ::  and the scanner runs again
    (expect !>(?=([[%pass [%blocks ~] %arvo %k %lard *] ~] (app-cards ticks))))
  ==
::
::  A -14 state with NO halt in force takes the same branch and is
::  likewise indistinguishable afterwards -- the ~ case is the one every
::  healthy ship actually upgrades from.
::
++  test-on-load-takes-an-unhalted-14-state
  =/  agent  gw-btc
  =^  cards  agent  (~(on-load agent bowl0) !>((halted-14-state ~ ~)))
  ;:  weld
    (expect-eq !>((silt ~[~wes])) !>(;;((set ship) (peek-noun (~(on-peek agent bowl0) /x/declined)))))
    %+  expect-eq
      !>  [synced=%.y tip=`961.100 indexing=%.y]
      !>  ;;([? (unit @ud) ?] (peek-noun (~(on-peek agent bowl0) /x/ready)))
    (expect-eq !>(~) !>((app-cards cards)))
  ==
::
++  test-unknown-verdict-emits-nothing
  =/  agent  gw-btc
  =^  *      agent  (~(on-load agent bowl0) !>((verify-state ~ ~ ~)))
  =^  cards  agent
    (~(on-arvo agent bowl0) /verify/(scot %p peer)/0 (failed-sign peer ~['sponsor-known']))
  (expect-eq !>(~) !>((app-cards cards)))
::
::  An undeterminable liveness scan is the same class: `tip-scanned`, not
::  `tip-unspent`.  Before this split, "we could not look" was reported as
::  "we looked and the sat is gone" and demoted the peer.
::
++  test-unscannable-tip-emits-nothing
  =/  agent  gw-btc
  =^  *      agent  (~(on-load agent bowl0) !>((verify-state ~ ~ ~)))
  =^  cards  agent
    (~(on-arvo agent bowl0) /verify/(scot %p peer)/0 (failed-sign peer ~['tip-scanned']))
  (expect-eq !>(~) !>((app-cards cards)))
::
::  ... but ignorance never launders fraud: one fraud-class check failing
::  alongside an unevaluable one is still a negative verdict.
::
++  test-fraud-alongside-ignorance-still-fails
  =/  agent  gw-btc
  =^  *      agent  (~(on-load agent bowl0) !>((verify-state ~ ~ ~)))
  =/  sign  (failed-sign peer ~['sponsor-known' 'entry-0-commitment'])
  =^  cards  agent  (~(on-arvo agent bowl0) /verify/(scot %p peer)/0 sign)
  =/  cs  (app-cards cards)
  ;:  weld
    (expect-eq !>(1) !>((lent cs)))
    (expect-eq !>(`%verdict) !>((fact-mark (snag 0 cs))))
  ==
::
::  A ship that has never heard from its light client answers NOTHING --
::  not even the negative verdict a structurally broken pass would
::  otherwise draw.  The old gate was `?~ best`, which %bitcoin-client
::  satisfies with the GENESIS block within milliseconds of boot.
::
::  The control is the same poke on a fresh agent, which does emit that
::  negative verdict: so the silence below is caused by readiness alone.
::  (Both passes are undecodable, so this pins the ORDER of the two
::  branches -- structural rejection must not outrank readiness once a
::  pass does decode, and the readiness branch is reached by everything
::  that decodes.)
::
++  test-fresh-agent-has-no-readiness
  =/  agent  gw-btc
  =^  *  agent  ~(on-init agent bowl0)
  ::  [synced tip indexing] -- not synced, no tip, not indexing.  A ship
  ::  in this state judges nothing.
  ::
  ::  Both booleans are PINNED by +on-init: the bunt of ? is %.y, so a
  ::  mold-level default would have a fresh agent believe its index was
  ::  bootstrapped and its light client caught up.
  ::
  %+  expect-eq
    !>  `*`[%.n ~ %.n]
    !>  `*`(peek-noun (~(on-peek agent bowl0) /x/ready))
::
::  ... and .synced must SURVIVE an upgrade.  /is-synced answers the
::  initial watch and then only on TRANSITIONS, so a value reset by
::  +on-load is not repaired by the next fact -- it is repaired when the
::  light client next changes its mind.  Resetting it would hold every
::  attestation until then: fail-closed, but dead.
::
++  test-on-load-preserves-syncedness
  =/  agent  gw-btc
  =^  *  agent  ~(on-init agent bowl0)
  =^  *  agent
    (~(on-agent agent bowl0) /is-synced [%fact %bitcoin-client-is-synced !>(&)])
  =/  before  (peek-noun (~(on-peek agent bowl0) /x/ready))
  =^  *  agent  (~(on-load agent bowl0) ~(on-save agent bowl0))
  =/  after   (peek-noun (~(on-peek agent bowl0) /x/ready))
  ;:  weld
    (expect-eq !>(`*`[%.y ~ %.n]) !>(`*`before))
    (expect-eq !>(`*`before) !>(`*`after))
  ==
::
::  ---------------------------------------------------------------------
::  DOORWAY ONE: +verify-lc's early aborts must route by their class
::  ---------------------------------------------------------------------
::
::  The library decides what each abort MEANS (+abort-class); this pins
::  what the agent DOES about it, which is the only thing a peer ever
::  experiences.  Before the fix all four names were unclassified and the
::  classifier's fall-through snubbed on every one of them.
::
++  test-abort-names-route-by-class
  =/  agent  gw-btc
  =^  *  agent  (~(on-load agent bowl0) !>((verify-state ~ ~ ~)))
  ::  one card, and it is the negative %verdict that becomes a jael
  ::  %fail and an ames snub
  ::
  =/  snubs
    |=  [ag=_agent name=cord]
    ^-  ?
    =^  cards  ag  (~(on-arvo ag bowl0) verify-wire (failed-sign peer ~[name]))
    =/  cs  (app-cards cards)
    ?&  =(1 (lent cs))
        =(`%verdict (fact-mark (snag 0 cs)))
    ==
  ;:  weld
    ::  fraud-class: the peer's own xtr contradicts the identity it claims
    (expect !>((snubs agent 'empty-chain')))
    (expect !>((snubs agent 'spawn-opening')))
    ::  fraud-class: the custody hop the log describes did not happen, on
    ::  transactions we confirmed on the main chain ourselves
    (expect !>((snubs agent 'derive-tip')))
  ==
::
::  ... and the one abort that is NOT the peer's fault emits nothing.
::  %tip-vout-range can only fire when +derive-tip's own output index
::  disagrees with the output list it came from -- a bug in this desk.
::  Snubbing a peer over that is exactly the failure this whole
::  classification exists to prevent, and it is what used to happen.
::
++  test-abort-names-never-snub-when-unknown
  =/  agent  gw-btc
  =^  *      agent  (~(on-load agent bowl0) !>((verify-state ~ ~ ~)))
  =^  cards  agent
    (~(on-arvo agent bowl0) verify-wire (failed-sign peer ~['tip-vout-range']))
  ::  NO cards at all: not a %verdict (a snub), not even a
  ::  %stale-notice (a demotion)
  (expect-eq !>(~) !>((app-cards cards)))
::
::  ---------------------------------------------------------------------
::  DOORWAY TWO: a verdict that PASSED and is refused LOCALLY
::  ---------------------------------------------------------------------
::
::  ++run-checks says ok=%.y and the agent still declines the point.  The
::  four reasons are all about OUR state, so none of them may reach the
::  snub branch -- and all four used to, because `verified=~' erased which
::  one had fired and dropped straight through to the negative outcome.
::
::  THE CONTROL FIRST: the identical sign with nothing objecting really
::  does install the point and emit a POSITIVE %verdict.  Without
::  it, four tests asserting silence prove only that the fixture is inert.
::
::  Each refusal test below is THIS test with exactly one ingredient
::  swapped -- the ship in the verdict, the point, the point's pass, or
::  the sat index -- so the silence it asserts is caused by that
::  ingredient and nothing else.
::
::  A %claim effect out of the block scanner launches a verification, on
::  the /claim wire, in the SAME single-flight slot a %jael-writ takes.
::  Nothing is indexed by the scan itself: the point appears only when the
::  light client has walked the log.
::
++  test-claim-effect-launches-a-verification
  =/  agent  gw-btc
  =^  *      agent  (~(on-load agent bowl0) !>(synced-state))
  =^  cards  agent
    %-  ~(on-arvo agent bowl0)
    :-  /blocks
    (blocks-sign ~[[[0xb.10c1 961.100] [%claim claimer claim-pass]]])
  =/  cs  (app-cards cards)
  =/  wires
    %+  murn  cs
    |=(c=card:agent:gall ?.(?=([%pass *] c) ~ `p.c))
  ;:  weld
    ::  the job, and its leak guard, both on claim wires naming the ship
    ::
    (expect !>((lien wires |=(=path =(path /claim/(scot %p claimer)/0)))))
    (expect !>((lien wires |=(=path =(path /claim-stuck/(scot %p claimer)/0)))))
    ::  it holds the single-flight slot
    ::
    %+  expect-eq
      !>((silt ~[claimer]))
      !>(;;((set ship) (peek-noun (~(on-peek agent bowl0) /x/inflight))))
    ::  and NOTHING was indexed: the scanner asserts nothing about a
    ::  publication, it only forwards it
    ::
    ::  /x/point/<ship> answers [~ ~] for a ship it holds no point for
    ::
    (expect !>(?=([~ ~] (~(on-peek agent bowl0) /x/point/(scot %p claimer)))))
  ==
::
::  ... and a second publication for the same ship while that job runs is
::  dropped, out loud.  The on-chain road has no separate latch: .inflight
::  is the only single-flight there is.
::
++  test-second-claim-while-one-is-in-flight-is-dropped
  =/  agent  gw-btc
  =^  *      agent  (~(on-load agent bowl0) !>(synced-state))
  =^  *      agent
    %-  ~(on-arvo agent bowl0)
    :-  /blocks
    (blocks-sign ~[[[0xb.10c1 961.100] [%claim claimer claim-pass]]])
  =^  cards  agent
    %-  ~(on-arvo agent bowl0)
    :-  /blocks
    (blocks-sign ~[[[0xb.10c2 961.101] [%claim claimer claim-pass]]])
  =/  wires
    %+  murn  (app-cards cards)
    |=(c=card:agent:gall ?.(?=([%pass *] c) ~ `p.c))
  ::  no job 1 was launched
  (expect !>(!(lien wires |=(=path =(path /claim/(scot %p claimer)/1)))))
::
::  ---------------------------------------------------------------------
::  THE ON-CHAIN ROAD: a publication verified through the same +verify-lc
::
::  A publication is the comet's whole attestation packet, so it takes the
::  same job in the same single-flight slot as a %jael-writ.  Three things
::  must differ, and all three are about the fact that NOBODY ASKED US:
::  no verdict ever, no sponsorship decision, and the ship comes out
::  PUBLIC rather than confidential.
::  ---------------------------------------------------------------------
::
::  A publication that verifies installs the point and declassifies -- and
::  emits NO %verdict, because there is no writ to answer.
::
++  test-claim-that-verifies-installs-a-public-point
  =/  agent  gw-btc
  ::  the ship starts CONFIDENTIAL, which is the interesting case: the
  ::  publication is the owner's own consent to declassify
  ::
  =^  *      agent
    (~(on-load agent bowl0) !>((verify-state (silt ~[peer]) ~ ~)))
  =^  cards  agent
    (~(on-arvo agent bowl0) claim-wire (ok-sign peer `(ok-point anew-pass)))
  =/  cs    (app-cards cards)
  =/  conf  ;;((set ship) (peek-noun (~(on-peek agent bowl0) /x/confidential)))
  =/  ats   ;;((map ship sont:ord) (peek-noun (~(on-peek agent bowl0) /x/attested)))
  ;:  weld
    ::  NOT confidential any more -- and that does not undo
    ::
    (expect-eq !>(*(set ship)) !>(conf))
    (expect-eq !>(`tip-sont) !>((~(get by ats) peer)))
    ::  the point is in the PUBLIC snapshot now
    ::
    %+  expect-eq
      !>(`@ud`1)
      !>  =/  pt  ;;(point:urb (peek-noun (~(on-peek agent bowl0) /x/point/(scot %p peer))))
          life.net.pt
    ::  and not one card is a %verdict: a broadcast is not a writ
    ::
    %+  expect-eq
      !>(~)
      !>  %+  skim  cs
          |=(c=card:agent:gall ?=(^ (fact-mark c)))
    ::  the slot is released
    ::
    (expect-eq !>(*(set ship)) !>(;;((set ship) (peek-noun (~(on-peek agent bowl0) /x/inflight)))))
  ==
::
::  A publication that does NOT verify is a log line and nothing else.
::  On the writ road the same failing checks are fraud-class and snub;
::  here they cannot, because we were never asked.
::
++  test-claim-that-fails-emits-no-verdict-and-never-snubs
  =/  agent  gw-btc
  =^  *      agent  (~(on-load agent bowl0) !>((verify-state ~ ~ ~)))
  =^  cards  agent
    (~(on-arvo agent bowl0) claim-wire (failed-sign peer ~['entry-0-commitment']))
  ;:  weld
    ::  'entry-0-commitment' is fraud-class: on /verify this is a SNUB
    ::
    (expect-eq !>(%fraud) !>((classify:lsa [peer %.n ~[['entry-0-commitment' %.n]]])))
    ::  ... and on /claim it is nothing at all
    ::
    (expect-eq !>(~) !>((app-cards cards)))
    ::  the slot is still released, so a later publication can retry
    ::
    (expect-eq !>(*(set ship)) !>(;;((set ship) (peek-noun (~(on-peek agent bowl0) /x/inflight)))))
  ==
::
::  A stale-class failure is the same: no %stale-notice either.  Nobody
::  told us this comet was talking to us, so we have nothing to demote.
::
++  test-claim-that-is-stale-emits-nothing
  =/  agent  gw-btc
  =^  *      agent  (~(on-load agent bowl0) !>((verify-state ~ ~ ~)))
  =^  cards  agent
    (~(on-arvo agent bowl0) claim-wire (failed-sign peer ~['tip-unspent']))
  (expect-eq !>(~) !>((app-cards cards)))
::
++  test-passing-verdict-is-installed-control
  =/  agent  gw-btc
  =^  *      agent  (~(on-load agent bowl0) !>((verify-state ~ ~ ~)))
  =^  cards  agent
    (~(on-arvo agent bowl0) verify-wire (ok-sign peer `(ok-point anew-pass)))
  =/  cs   (app-cards cards)
  ::  NB: /x/points is the PUBLIC snapshot, which filters confidential
  ::  comets out by design.  /x/attested is where a verified confidential
  ::  identity's tip is recorded.
  ::
  =/  ats  (peek-noun (~(on-peek agent bowl0) /x/attested))
  ;:  weld
    (expect-eq !>(1) !>((lent cs)))
    (expect-eq !>(`%verdict) !>((fact-mark (snag 0 cs))))
    ::  ... and it is a POSITIVE one: [dom who `point], not [dom who ~]
    (expect !>(?=([@ @ ^] (need (fact-payload (snag 0 cs))))))
    ::  ... and the peer really was installed, at the tip it proved
    %+  expect-eq  !>(`(unit sont:ord)`[~ tip-sont])
    !>((~(get by ;;((map ship sont:ord) ats)) peer))
  ==
::
::  %who-mismatch: the verdict names a ship the writ did not.  The peer
::  never supplies who.verdict -- +pass-attestation set it from the writ
::  after checking the pass fingerprints to it -- so a mismatch is our own
::  bookkeeping and says nothing about the peer.
::
++  test-refusal-who-mismatch-is-silent
  =/  agent  gw-btc
  =^  *      agent  (~(on-load agent bowl0) !>((verify-state ~ ~ ~)))
  =^  cards  agent
    (~(on-arvo agent bowl0) verify-wire (ok-sign ~dev `(ok-point anew-pass)))
  (expect-eq !>(~) !>((app-cards cards)))
::
::  %no-point: ok=%.y with no point at all.  ++run-checks builds the point
::  whenever ok holds, so this is a contradiction inside the verifier --
::  again nothing the peer did.
::
++  test-refusal-no-point-is-silent
  =/  agent  gw-btc
  =^  *      agent  (~(on-load agent bowl0) !>((verify-state ~ ~ ~)))
  =^  cards  agent  (~(on-arvo agent bowl0) verify-wire (ok-sign peer ~))
  (expect-eq !>(~) !>((app-cards cards)))
::
::  %pass-mismatch: the rebuilt pass's key disagrees with the pass jael
::  forwarded.  They are the same pass in every real run, so reaching this
::  means the desk contradicted itself; the peer is not the one who is
::  wrong, and must not be snubbed for it.
::
++  test-refusal-pass-mismatch-is-silent
  =/  agent  gw-btc
  =^  *      agent  (~(on-load agent bowl0) !>((verify-state ~ ~ ~)))
  ::  a well-formed suite-C pass that is NOT the one the writ carried
  =/  other  (kelvin-pass 7)
  =^  cards  agent
    (~(on-arvo agent bowl0) verify-wire (ok-sign peer `(ok-point other)))
  (expect-eq !>(~) !>((app-cards cards)))
::
::  %tip-owned: THE reachable one.  Our own sat index already attributes
::  the proven tip to another comet.  Two chain-valid logs cannot both end
::  at one satpoint, so a conflict means one of the two views is out of
::  date -- and ours is a forward-only scanner over an operator-chosen
::  window, while the peer's is a proof against the chain.  We cannot tell
::  which, so we do neither thing: the point is refused (the other comet's
::  sat is not overwritten) AND no verdict is emitted.
::
++  test-refusal-tip-owned-by-another-comet-is-silent
  =/  agent  gw-btc
  =^  *  agent
    (~(on-load agent bowl0) !>((verify-state-sm ~ ~ ~ taken-sont-map)))
  =^  cards  agent
    (~(on-arvo agent bowl0) verify-wire (ok-sign peer `(ok-point anew-pass)))
  =/  ats  (peek-noun (~(on-peek agent bowl0) /x/attested))
  ;:  weld
    ::  no snub, no demotion -- nothing at all
    (expect-eq !>(~) !>((app-cards cards)))
    ::  ... and we did not install the peer over the other comet's sat
    ::  (the control above records exactly this tip for exactly this peer)
    (expect-eq !>(*(map ship sont:ord)) !>(;;((map ship sont:ord) ats)))
  ==
::
::  The two doorways meet: a FRAUD verdict is still a snub.  Closing the
::  fail-open must not have closed the fail-shut, or forged logs become
::  unpunishable -- which would be exactly as wrong as snubbing on our own
::  ignorance.
::
++  test-fraud-still-snubs-after-the-fix
  =/  agent  gw-btc
  =^  *      agent  (~(on-load agent bowl0) !>((verify-state ~ ~ ~)))
  =^  cards  agent
    (~(on-arvo agent bowl0) verify-wire (failed-sign peer ~['entry-0-commitment']))
  =/  cs  (app-cards cards)
  ;:  weld
    (expect-eq !>(1) !>((lent cs)))
    (expect-eq !>(`%verdict) !>((fact-mark (snag 0 cs))))
    ::  a NEGATIVE verdict: [dom who ~]
    (expect !>(?=([@ @ ~] (need (fact-payload (snag 0 cs))))))
  ==
::  ---------------------------------------------------------------------
::  READINESS NEVER CONDEMNS (test 6.7 / Phase 6.1), at the agent
::  ---------------------------------------------------------------------
::
::  The two gates a live mainnet ship actually sits behind, driven through
::  +on-poke with a pass whose custody log is non-empty -- which is what
::  it takes to get past the structural checks and reach them at all.
::
::  Both used to be `?~ best` / `?. synced` returning silently, and Phase
::  6.1 is what happens when the first of them is wrong: a ship 961,000
::  blocks behind judged a real attestation and snubbed the honest comet
::  it sponsors.  What must hold now is that neither gate can EVER emit a
::  verdict, whatever else it does.
::
++  test-writ-with-no-chain-tip-emits-no-verdict
  =/  pas    (logged-pass ~[entry0])
  =/  agent  gw-btc
  =^  cards  agent
    (~(on-poke agent bowl0) %noun (writ-vase-pass (fig-of pas) pas))
  ::  a fresh agent has best=~, so this is the %no-tip hold
  ::
  (expect-eq !>(~) !>((app-cards cards)))
::
++  test-unsynced-writ-holds-and-never-condemns
  =/  pas    (logged-pass ~[entry0])
  =/  agent  gw-btc
  =^  *  agent  (~(on-load agent bowl0) !>(tip-state))
  =^  cards  agent
    (~(on-poke agent bowl0) %noun (writ-vase-pass (fig-of pas) pas))
  =/  out  (app-cards cards)
  ;:  weld
    ::  the tip is above the log's evidence, so the ONLY thing left is
    ::  .synced -- which +on-load's migration correctly leaves %.n.
    ::  It re-reads /is-synced (a %leave and a %watch on one wire) ...
    ::
    (expect-eq !>(2) !>((lent out)))
    ::  ... and emits NO fact of any kind.  Not a verdict, not a
    ::  %stale-notice: a ship that cannot tell whether it is synced has
    ::  no business condemning anyone.
    ::
    %-  expect  !>
    %+  levy  out
    |=(c=card:agent:gall ?=(~ (fact-mark c)))
  ==
::
::  ---------------------------------------------------------------------
::  PROVENANCE: a point records the block it was last observed in
::  ---------------------------------------------------------------------
::
::  The public index had NO provenance at all, which is why a reorg could
::  only be answered with a halt: nothing recorded which block a fact came
::  out of, so nothing could be filtered.  A $point now carries .seen.
::
::  It is REFRESHED ON EVERY OBSERVATION rather than fixed when the point
::  is first indexed -- the team's refinement on the original proposal --
::  because what a reorg invalidates is the most recent evidence, not the
::  origin.  These two tests are the write and the refresh, driven through
::  the PUBLICATION road because that is the one that leaves a point the
::  scries will show (a confidential point is filtered out of all of them,
::  deliberately).
::
++  test-verified-point-records-its-block
  =/  agent  gw-btc
  =^  *  agent  (~(on-load agent bowl0) !>((live-state ~ ~ ~ ~)))
  =^  *  agent
    %-  ~(on-arvo agent bowl0)
    [claim-wire (ok-sign peer `(ok-point-at anew-pass `0xb.10c1))]
  =/  got  ;;(point:urb (peek-noun (~(on-peek agent bowl0) /x/point/(scot %p peer))))
  ;:  weld
    (expect-eq !>(`(unit @ux)``0xb.10c1) !>((seen-of got)))
    ::  the rest of the point is untouched by the new field
    (expect-eq !>(tip-sont) !>(sont.own.got))
    (expect-eq !>(`life`1) !>(life.net.got))
  ==
::
++  test-a-later-observation-refreshes-the-block
  ::  the point is already indexed, out of block 0xb.10c1.  A fresh
  ::  observation of the SAME state -- same satpoint, same life -- in a
  ::  LATER block must move .seen forward.  If provenance were fixed at
  ::  index time this would still read 0xb.10c1, and a reorg of the block
  ::  that actually carries our evidence would not touch the point.
  ::
  =/  agent  gw-btc
  =^  *  agent
    %-  ~(on-load agent bowl0)
    !>  %^  live-state  (malt ~[[peer (ok-point-at anew-pass `0xb.10c1)]])
          peer-sont-map
        [~ ~]
  =/  before  ;;(point:urb (peek-noun (~(on-peek agent bowl0) /x/point/(scot %p peer))))
  =^  *  agent
    %-  ~(on-arvo agent bowl0)
    [claim-wire (ok-sign peer `(ok-point-at anew-pass `0xb.10c2))]
  =/  after  ;;(point:urb (peek-noun (~(on-peek agent bowl0) /x/point/(scot %p peer))))
  ;:  weld
    (expect-eq !>(`(unit @ux)``0xb.10c1) !>((seen-of before)))
    (expect-eq !>(`(unit @ux)``0xb.10c2) !>((seen-of after)))
  ==
::
::  ---------------------------------------------------------------------
::  FORGET AND RE-ALIEN: the one way this agent un-knows an identity
::  ---------------------------------------------------------------------
::
::  A reorg that orphans the block a point was last seen in will take this
::  route (+orphaned-points:uc -> +forget-points -> +forget-cards), and it
::  is the route a moved identity sat already takes.  So drive the moved
::  sat, and pin what it emits and what it clears -- because that is
::  exactly what the reorg path will emit and clear when the light client
::  reports which blocks were orphaned.
::
++  test-a-moved-sat-forgets-the-point-and-never-snubs
  =/  agent  gw-btc
  =^  *  agent
    %-  ~(on-load agent bowl0)
    !>  %^  live-state  (malt ~[[peer (ok-point-at anew-pass `0xb.10c1)]])
          peer-sont-map
        [(silt ~[peer]) (malt ~[[peer tip-sont]])]
  =^  cards  agent  (~(on-arvo agent bowl0) /blocks moved-blocks-sign)
  =/  out    (app-cards cards)
  =/  notes  (skim out |=(c=card:agent:gall =(`%stale-notice (fact-mark c))))
  =/  index  ;;(state:urb (peek-noun (~(on-peek agent bowl0) /x/urb-state)))
  ;:  weld
    ::  ONE %stale-notice, naming the peer.  Jael drops its point and ames
    ::  demotes the peer to a fresh alien; it keeps our lane and
    ::  re-attests of its own accord.
    (expect-eq !>(1) !>((lent notes)))
    %+  expect-eq
      !>  `[@tas ship]`[%gw-btc peer]
      !>  ^-  [@tas ship]
          ?~  pay=(fact-payload (snag 0 notes))  [%$ ~zod]
          ;;([dom=@tas =ship] u.pay)
    ::  and NEVER a %verdict.  A snub is permanent on every transport, so
    ::  spending one on a chain event would need an operator to undo.
    (expect !>(!(lien out |=(c=card:agent:gall =(`%verdict (fact-mark c))))))
    ::  all four stores are cleared: the index, the confidential
    ::  registry, the attested tip, and the verification slot -- which
    ::  would otherwise land on a point that no longer exists.
    ::
    ::  (/x/urb-state hides CONFIDENTIAL points, so a peer visible in it
    ::  is a peer that is no longer in the registry; absent from it means
    ::  absent from .unv-ids too.)
    (expect !>(!(~(has by unv-ids.index) peer)))
    (expect-eq !>(*(map ship sont:ord)) !>(;;((map ship sont:ord) (peek-noun (~(on-peek agent bowl0) /x/attested)))))
    (expect-eq !>(*(set ship)) !>(;;((set ship) (peek-noun (~(on-peek agent bowl0) /x/inflight)))))
  ==
::
::  +orphaned-points is the SELECTOR the reorg path is built on: given the
::  orphaned hashes it names exactly the points to hand +forget-points,
::  and nothing else.
::
++  test-orphaned-points-selects-by-block
  =/  fresh   (ok-point-at anew-pass `0xb.10c2)
  =/  stale   (ok-point-at anew-pass `0xb.10c1)
  =/  legacy  (ok-point-at anew-pass ~)
  =/  st=state:urb
    :*  [0xdead.beef 961.100]  *sont-map:ord  *insc-ids:ord
        (malt ~[[~wes fresh] [~des stale] [~lex legacy]])
    ==
  ;:  weld
    ::  the point whose evidence is in an orphaned block is taken and the
    ::  one in a surviving block is not.  The HASHLESS one is NEVER taken:
    ::  .seen=~ says "we cannot determine whether this was orphaned",
    ::  which is not the claim that it was, and an unevaluable condition
    ::  must not produce a negative outcome -- forgetting costs the peer
    ::  its point, and for a PUBLIC point it is a silent, permanent index
    ::  loss (nobody re-attests a public point; it comes back only by
    ::  rescanning a range the rewind need not cover).  Keeping one is at
    ::  worst the status quo the old halt already left in place.
    %+  expect-eq
      !>  (silt ~[`ship`~des])
      !>  (orphaned-points:uc st (silt ~[`@ux`0xb.10c1]))
    ::  a reorg that orphans a block none of our points came from takes
    ::  NOTHING -- the case the old rule got wrong, and the common one
    %+  expect-eq
      !>  *(set ship)
      !>  (orphaned-points:uc st (silt ~[`@ux`0xdead.dead]))
    ::  ... and an empty orphan set is likewise empty
    %+  expect-eq
      !>  *(set ship)
      !>  (orphaned-points:uc st *(set @ux))
    ::  orphaning every block we have provenance for takes every point we
    ::  have provenance for, and still not the hashless one
    %+  expect-eq
      !>  (silt ~[`ship`~wes ~des])
      !>  (orphaned-points:uc st (silt ~[`@ux`0xb.10c1 0xb.10c2]))
  ==
::
::  ---------------------------------------------------------------------
::  MIGRATION: a point written before provenance existed
::  ---------------------------------------------------------------------
::
::  $gw-state-13 is the current twelve fields around a TWO-field point.
::  .seen is a tail field, so it discriminates on its own: a -13 point
::  offered to the current mold puts [rift life pass sponsor escape fief]
::  where [net seen] is expected, and the six-tuple $net mold then has to
::  read the bare @ud .rift, which bails.
::
::  The lift must be honest rather than convenient: .seen becomes ~, which
::  says "we have no provenance for this point", and NOT some stand-in
::  hash that a reorg would then filter against and get wrong.
::
++  test-on-load-lifts-a-pre-provenance-point
  =/  agent  gw-btc
  =^  cards  agent  (~(on-load agent bowl0) !>(legacy-13-state))
  =/  got  ;;(point:urb (peek-noun (~(on-peek agent bowl0) /x/point/~wes)))
  ;:  weld
    ::  the point survived, in full ...
    (expect-eq !>(`sont:ord`[0xf00d.cafe 0 0]) !>(sont.own.got))
    (expect-eq !>(`life`1) !>(life.net.got))
    (expect-eq !>(`[has=? who=@p]`[%.y ~marzod]) !>(sponsor.net.got))
    ::  ... with no provenance, which is the truth about it
    (expect-eq !>(`(unit @ux)`~) !>((seen-of got)))
    ::  the fields around it did not shift
    %+  expect-eq
      !>  `id:block:bitcoin`[0xdead.beef 961.100]
      !>  ;;(id:block:bitcoin (peek-noun (~(on-peek agent bowl0) /x/block-id)))
    (expect-eq !>((silt ~[~wes])) !>(;;((set ship) (peek-noun (~(on-peek agent bowl0) /x/declined)))))
    %+  expect-eq
      !>  [synced=%.y tip=`961.100 indexing=%.y]
      !>  ;;([? (unit @ud) ?] (peek-noun (~(on-peek agent bowl0) /x/ready)))
    ::  a -13 state already had /is-synced; do not re-subscribe
    (expect !>(!(lien (app-cards cards) |=(c=card:agent:gall =(c synced-watch)))))
  ==
::
::  ... and a hashless point is EXACTLY what the reorg selector treats as
::  unfilterable, which the selector answers by leaving it alone.  This is
::  the migration question answered end to end: an upgraded ship's old
::  points survive every reorg until they are re-observed, at which point
::  they acquire a hash and become filterable like everything else.  The
::  worst case is that one of them outlives the block it came from -- the
::  same exposure the halt left, and strictly better than losing a public
::  point nobody can re-attest.
::
++  test-a-lifted-point-is-unfilterable
  =/  agent  gw-btc
  =^  *  agent  (~(on-load agent bowl0) !>(legacy-13-state))
  =/  index  ;;(state:urb (peek-noun (~(on-peek agent bowl0) /x/urb-state)))
  ;:  weld
    ::  it is in the index ...
    (expect-eq !>((silt ~[`ship`~wes])) !>(~(key by unv-ids.index)))
    ::  ... and no orphan set reaches it, including one naming every hash
    (expect-eq !>(*(set ship)) !>((orphaned-points:uc index *(set @ux))))
    %+  expect-eq
      !>  *(set ship)
      !>  (orphaned-points:uc index (silt ~[`@ux`0xb.10c1 0xb.10c2 0xdead.beef]))
  ==
--
