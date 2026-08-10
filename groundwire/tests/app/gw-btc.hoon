::  tests/app/gw-btc.hoon
::
::  Agent-level tests for the %gw-btc verifier's v9 sponsorship surface,
::  driven through the formal gall interface (on-poke / on-peek).  The
::  security-critical invariant: DECLINING sponsorship is silence, never
::  a negative Jael verdict -- a valid ship whose sponsorship we refuse
::  must not be snubbed.
::
/-  urb, sa=self-attestation, ord, bitcoin, lc=light-client
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
::  the state shape BEFORE .own existed (10 fields), for the migration
::
++  legacy-10-state
  ^-  *
  :*  `state:urb`[[0xdead.beef 943.140] ~ ~ ~]
      %.y  ~  ~  ~  ~  ~  0  ~  ~
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
      :-  [[0xf00d.cafe 0 0] ~]
      [rift=0 life=1 anew-pass [%.y ~marzod] ~ ~]
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
  [[tip-sont ~] rift=0 life=1 pass [%.y ~marzod] ~ ~]
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
::  the wire a finished verification for .peer (job 0) arrives on
::
++  verify-wire  /verify/(scot %p peer)/0
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
++  test-fraud-verdict-still-fails
  ::  the control: the SAME machinery, one genuinely-invalid check, still
  ::  produces the negative verdict that snubs.
  ::
  =/  agent  gw-btc
  =^  *      agent  (~(on-load agent bowl0) !>((verify-state ~ ~ ~)))
  =^  cards  agent
    (~(on-arvo agent bowl0) /verify/(scot %p peer)/0 (failed-sign peer ~['spawn-commit']))
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
::  CHAIN REORGS  (Phase 6.5)
::  ---------------------------------------------------------------------
::
::  %bitcoin-client reports a reorg as %reorg-rollback on /best-block, and
::  this agent handled it byte-identically to %new: only .best moved.  The
::  scan cursor never rewound, so the orphaned range was never rescanned;
::  facts indexed out of orphaned blocks stayed in .unv-ids forever; and
::  facts unique to the winning chain fell in the skipped range.  All
::  silently.  With block-confirmations = 1 a single-block reorg reaches
::  it, and those happen several times a month on mainnet.
::
::  Undoing the orphaned facts is not possible with what is stored (a
::  $point does not record the height it was indexed at), so the scanner
::  STOPS and says so.  A halted index announces itself; a silently forked
::  one does not.
::
++  best-block-sign
  |=  [rollback=? height=@ud]
  ^-  sign:agent:gall
  :+  %fact  %best-block
  ?:  rollback
    !>(`[%reorg-rollback block-height=@ud block-hash=@ux]`[%reorg-rollback height 0xbeef])
  !>(`[%new block-height=@ud block-hash=@ux]`[%new height 0xbeef])
::
++  test-reorg-below-the-cursor-halts-the-scanner
  =/  agent  gw-btc
  ::  +verify-state's cursor is 900.100
  =^  *  agent  (~(on-load agent bowl0) !>((verify-state ~ ~ ~)))
  =^  cards  agent
    (~(on-agent agent bowl0) /best-block (best-block-sign & 900.050))
  =/  ready  (peek-noun (~(on-peek agent bowl0) /x/ready))
  ::  [synced tip indexing reorg-halt]; the halt records [at cursor since]
  =/  halt  +:+:+:ready
  ;:  weld
    ::  the rollback is recorded, with the height and the cursor it caught
    (expect-eq !>(`*`[~ 900.050 900.100 `@da`~2000.1.1]) !>(`*`halt))
    ::  and nothing was emitted -- a reorg is not a verdict about anyone
    (expect-eq !>(~) !>((app-cards cards)))
  ==
::
::  ... and while halted the block timer refuses to dispatch a scan.  It
::  keeps re-arming and keeps complaining, because a stopped scanner that
::  stops mentioning it is indistinguishable from a working one.
::
++  test-halted-scanner-does-not-advance
  =/  agent  gw-btc
  =^  *  agent  (~(on-load agent bowl0) !>((verify-state ~ ~ ~)))
  =^  *  agent
    (~(on-agent agent bowl0) /best-block (best-block-sign & 900.050))
  =^  cards  agent  (~(on-arvo agent bowl0) /timer [%behn %wake ~])
  =/  cs  (app-cards cards)
  ;:  weld
    ::  exactly one card, and it is the timer re-arming -- NOT a %lard
    ::  block thread
    (expect-eq !>(1) !>((lent cs)))
    (expect !>(?=([[%pass [%timer ~] %arvo %b %wait *] ~] cs)))
  ==
::
::  A rollback ABOVE the cursor is harmless: nothing we hold came out of
::  the orphaned blocks.  Note it, move .best, carry on.
::
++  test-reorg-above-the-cursor-does-not-halt
  =/  agent  gw-btc
  =^  *  agent  (~(on-load agent bowl0) !>((verify-state ~ ~ ~)))
  =^  *  agent
    (~(on-agent agent bowl0) /best-block (best-block-sign & 900.200))
  =/  ready  (peek-noun (~(on-peek agent bowl0) /x/ready))
  ;:  weld
    ::  not halted ...
    (expect-eq !>(`*`~) !>(`*`+:+:+:ready))
    ::  ... and .best did move to the rollback height
    (expect-eq !>(`*`[~ 900.200]) !>(`*`-:+:ready))
  ==
::
::  and a plain %new still just moves the tip.
::
++  test-new-block-moves-the-tip
  =/  agent  gw-btc
  =^  *  agent  (~(on-load agent bowl0) !>((verify-state ~ ~ ~)))
  =^  *  agent
    (~(on-agent agent bowl0) /best-block (best-block-sign | 900.101))
  =/  ready  (peek-noun (~(on-peek agent bowl0) /x/ready))
  ;:  weld
    (expect-eq !>(`*`[~ 900.101]) !>(`*`-:+:ready))
    (expect-eq !>(`*`~) !>(`*`+:+:+:ready))
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
  ::  [synced tip indexing reorg-halt] -- not synced, no tip, not
  ::  indexing, not halted.  A ship in this state judges nothing.
  ::
  ::  Both booleans are PINNED by +on-init: the bunt of ? is %.y, so a
  ::  mold-level default would have a fresh agent believe its index was
  ::  bootstrapped and its light client caught up.
  ::
  %+  expect-eq
    !>  `*`[%.n ~ %.n ~]
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
    (~(on-agent agent bowl0) /is-synced [%fact %is-synced !>(&)])
  =/  before  (peek-noun (~(on-peek agent bowl0) /x/ready))
  =^  *  agent  (~(on-load agent bowl0) ~(on-save agent bowl0))
  =/  after   (peek-noun (~(on-peek agent bowl0) /x/ready))
  ;:  weld
    (expect-eq !>(`*`[%.y ~ %.n ~]) !>(`*`before))
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
--
