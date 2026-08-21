::  %gw-btc
::
::  This agent is the Groundwire equivalent of %azimuth and %eth-watcher.
::  It fetches Bitcoin blocks on a timer and parses them for Jael events.
::  Its helper core at the bottom works in conjunction with lib/urb-core.
::
::  Every byte it reads off the chain comes from the local
::  %bitcoin-client light client (lib/lc-attestation), for BOTH the
::  public block scanner and the confidential verifier.  There is no
::  Bitcoin Core RPC anywhere in this agent and no -txindex requirement.
::
::  It is also the verifier agent for the %gw-btc PKI domain in the
::  confidential-comets kernel protocol (see the companion spec
::  ops/doc/confidential-comets-agent.md and, in gwbtc/urbit,
::  pkg/arvo/doc/spec/pluggable-comet-pki.md).  On boot it registers
::  itself with Jael via a %anex task, so that when a confidential
::  (suite-%c) comet self-attests to us, Ames routes the attestation
::  here as a %jael-writ poke.  We decode the custody log carried in the
::  pass, fetch its transactions asynchronously through %light-client,
::  and answer with a %verdict fact; on success
::  Jael stores the point and promotes the comet.
::
::  The other half of that protocol is our OWN identity.  After the owner
::  performs a custody transaction through Causeway (a rekey, or a plain
::  sat move), Causeway pokes us the new xtr entry and its opening
::  ([%gw-custody-entry ...], $ingest in sur/self-attestation).  We run
::  the EXTENDED log through the same light-client verification a peer's
::  attestation gets, and only on a positive verdict do we store it,
::  re-encode our pass around it, and hand jael an %anew-response.  A
::  %jael-anew poke re-validates the stored log and answers the same way.
::  Either path answers with SILENCE if validation fails: a stale or
::  unproven pass is worse than none.  (Known accepted gap: the refreshed
::  pass is not written back to the boot keyfile, so a reboot re-derives
::  from the feed and needs another %anew round-trip.)
::
::  The domain name is this agent's name (1:1 by construction): a comet
::  commits the tag %gw-btc in its key tweak, and Jael derives the
::  same tag from the gall duct our %anex arrives on.
::
::  You may want to change block-confirmations and scan-batch as well.
::
/-  bitcoin, spider, ord, urb, sa=self-attestation, lc=light-client
/+  bc=bitcoin, dbug, default-agent, uc=urb-core, strandio, verb,
    ol=ord, cc=gw-btc-pass, lsa=self-attestation, lca=lc-attestation
::
|%
+$  card  card:agent:gall
::  $inflight-writ: the single verification job a ship may have
::
::    single-flight per ship: while one exists, every further
::    %jael-writ for that ship is dropped silently.  job disambiguates
::    the thread/timeout wires of successive jobs for the same ship.
::
+$  inflight-writ
  $:  dom=@tas
      =pass
      sat=self-attestation:sa
      job=@ud
  ==
::  $sponsee: a comet whose verified snapshot names us as its sponsor
::  and whose sponsorship this ship has accepted.  life is the life we
::  accepted at; the policy re-runs on every re-attestation.
::
+$  sponsee  [=life since=@da]
::  $anew-job: the single in-flight validation of OUR OWN custody log
::
::    .pass is the EXACT pass being validated -- built once, at launch,
::    from jael's current-life ring plus (jam .chain).  The %anew answer
::    republishes that byte-identical pass, so what ames installs is
::    precisely what the light client verified.  Rebuilding it at answer
::    time instead would open a window where the two differ.
::
+$  anew-job
  $:  job=@ud
      chain=custody-log:sa
      =pass
  ==
::  $own-custody: our OWN comet's custody log, and its validation slot
::
::    .chain is the log this ship will serve in its self-attestation --
::    every entry of it validated against the chain by the same
::    +verify-lc a peer's attestation goes through.  It starts empty and
::    is seeded from our own pass's xtr (whatever Causeway baked into the
::    boot feed) the first time it is needed.
::
::    .pending is single-flight: at most one self-validation at a time.
::
+$  own-custody
  $:  chain=custody-log:sa
      pending=(unit anew-job)
  ==
::  $gw-state
::
::    .indexing, .best and .synced are the INDEPENDENT readiness signals,
::    and keeping them apart is load-bearing:
::
::      .indexing -- the PUBLIC index has been bootstrapped (an operator
::      chose a start height with %urb-start-indexing / %gw-index-from).
::      It gates only the block scanner's timer.
::
::      .best -- the chain tip our persistent /best-block subscription last
::      reported.  It is what the verifier scans UP TO, and what the scanner
::      measures its own cursor against.
::
::      .synced -- the light client says it is CAUGHT UP: block headers and
::      filter headers both at the tip, with live peers.  This is the real
::      readiness signal for the confidential verifier, and it is not
::      derivable from .best: %bitcoin-client answers a fresh /best-block
::      subscription with the GENESIS block, so `?~ best` opened the gate on
::      a ship 961,000 blocks behind and it judged an attestation against a
::      chain it had never seen (live mainnet, 2026-08-06 -- a false INVALID
::      and a snub of an honest peer by its own sponsor).  A ~-check is not
::      a sync check.
::
::    A light-client-only deployment that never bootstraps a public index
::    still verifies confidential comets, and a node with no confidential
::    traffic still indexes public ones.  Conflating the two (as an
::    earlier revision did) forced operators to poke the public-indexer
::    bootstrap just to turn confidential verification on.
::
::    .indexing WAS declared `_|`, to force its bunt to %.n (the bunt of ?
::    is %.y, and an agent that came up believing its index was already
::    bootstrapped would never scan a block).  That was a silent
::    state-corrupter: `_` is $_, the mold that IGNORES its input and
::    always returns the pinned value, so `;;(gw-state ...)` in +on-load
::    rewrote .indexing to %.n on EVERY upgrade -- verified on a live
::    mainnet ship, 2026-08-06: C3 held %.y before a desk redeploy and
::    %.n after, with nothing else changed.  It reopened the one-shot
::    bootstrap guard on every upgrade, so a later %gw-index-from would
::    silently wipe .unv-ids -- and .unv-ids is what `sponsor-known`
::    reads.  So: an honest ? here, +on-init pinning %.n explicitly, and
::    a bootstrap guard that looks at the INDEX rather than at a flag.
::
+$  gw-state
  $:  urb-state=state:urb
      indexing=?
      best=(unit id:block:bc)
      inflight=(map ship inflight-writ)
      confidential=(set ship)
      attested=(map ship sont:ord)
      next-job=@ud
      sponsees=(map ship sponsee)
      declined=(set ship)
      own=own-custody
      ::  ? and NOT `_|`, for the reason above and with a sharper
      ::  consequence: /is-synced gives a fact on the initial watch and
      ::  then only on TRANSITIONS, so a value silently reset by +on-load
      ::  is not repaired by the next fact -- it is repaired by the next
      ::  time the light client changes its mind, which on a healthy node
      ::  is "when a block arrives".  An upgrade would therefore hold
      ::  every attestation until then.  Fail-closed, but dead.
      ::
      synced=?
  ==
::  $gw-state-14 / -13 / -12 / -11 / -10: the five earlier shapes
::
::    -14 is the state that carried .reorg-halt, the block scanner's
::    reorg STOP flag; -13 is -14 before a $point carried provenance; -12
::    is -13 with .publicizing still present; -11 is -12 without .synced
::    and .reorg-halt; -10 is -11 without .own.  +on-load discriminates
::    on them; ;; is strict about arity (it bails on both a missing and
::    an extra tail), so trying the current mold first and falling back
::    is exact, not a guess.
::
::    THE LAST FOUR CARRY $point-13, not the current $point.  They
::    describe nouns written by revisions that predate .seen, so naming
::    the current $point in them would demand a field those nouns cannot
::    have -- and the -12/-11/-10 branches would then never match
::    anything with a point in it.  -14 is the exception: it is the
::    current index shape and differs from $gw-state only in the tail.
::
::    .reorg-halt held a $reorg-stop, a type this agent no longer has:
::    the scanner does not halt on a reorg any more, it repairs the index
::    (see the %reorg-rollback branch of +on-agent).  The mold below
::    therefore spells the dead field out inline rather than naming a
::    type, because it is a fossil in a migration and not something
::    anything may construct.  It discriminates by itself: a -14 noun
::    offered to $gw-state puts [synced reorg-halt] where a bare ? is
::    expected, and a cell does not nest under ?(%.y %.n).
::
::    .seen is a TAIL field on $point, so it discriminates by itself: a
::    -13 point offered to the current mold puts [rift life pass sponsor
::    escape fief] where [net seen] is expected, so the 6-tuple $net mold
::    has to read the bare @ud .rift, which bails.  A state whose .unv-ids
::    is EMPTY is genuinely ambiguous -- and does not care, because there
::    is nothing to convert; the current mold takes it and the conversion
::    would have been the identity.
::
::    .publicizing was a MIDDLE field, so dropping it shifts everything
::    after it and arity alone would not discriminate -- except that the
::    TAIL still does, unconditionally.  A -12 noun offered to the -13
::    mold puts [synced reorg-halt] where the halt unit is expected: the
::    pair is a cell, so `synced` has to read as the unit's ~ head (only
::    true when synced=%.y), and then `reorg-halt` -- either ~ or [~ [at
::    cursor since]] -- has to read as a bare [at cursor since], which is
::    a three-atom cell.  Neither shape can, so the cast always fails and
::    the -12 branch is always the one that takes it.
::
::    Nothing else inside these molds changed, which is why they can keep
::    naming the current $inflight-writ.
::
::  the dead .reorg-halt field, spelled out for the three migration molds
::  that have to describe a noun containing it.  It held a $reorg-stop:
::  [at=@ud cursor=@ud since=@da], the height rolled back to, our scan
::  cursor at the time, and when.  Nothing constructs one any more.
::
+$  dead-halt  (unit [at=@ud cursor=@ud since=@da])
::
+$  point-13
  $:  $=  own
      $:  =sont:ord
          mang=(unit mang:urb)
      ==
  ::
      $=  net
      $:  rift=@ud
          =life
          =pass
          sponsor=[has=? who=@p]
          escape=(unit @p)
          fief=(unit fief:urb)
      ==
  ==
::
+$  urb-state-13
  $:  block-id=id:block:bc
      sont-map=sont-map:ord
      insc-ids=insc-ids:ord
      unv-ids=(map @p point-13)
  ==
::
+$  gw-state-14
  $:  urb-state=state:urb
      indexing=?
      best=(unit id:block:bc)
      inflight=(map ship inflight-writ)
      confidential=(set ship)
      attested=(map ship sont:ord)
      next-job=@ud
      sponsees=(map ship sponsee)
      declined=(set ship)
      own=own-custody
      synced=?
      reorg-halt=dead-halt
  ==
::
+$  gw-state-13
  $:  urb-state=urb-state-13
      indexing=?
      best=(unit id:block:bc)
      inflight=(map ship inflight-writ)
      confidential=(set ship)
      attested=(map ship sont:ord)
      next-job=@ud
      sponsees=(map ship sponsee)
      declined=(set ship)
      own=own-custody
      synced=?
      reorg-halt=dead-halt
  ==
::
+$  gw-state-12
  $:  urb-state=urb-state-13
      indexing=?
      best=(unit id:block:bc)
      inflight=(map ship inflight-writ)
      confidential=(set ship)
      attested=(map ship sont:ord)
      publicizing=(set ship)
      next-job=@ud
      sponsees=(map ship sponsee)
      declined=(set ship)
      own=own-custody
      synced=?
      reorg-halt=dead-halt
  ==
::
+$  gw-state-11
  $:  urb-state=urb-state-13
      indexing=?
      best=(unit id:block:bc)
      inflight=(map ship inflight-writ)
      confidential=(set ship)
      attested=(map ship sont:ord)
      publicizing=(set ship)
      next-job=@ud
      sponsees=(map ship sponsee)
      declined=(set ship)
      own=own-custody
  ==
::
+$  gw-state-10
  $:  urb-state=urb-state-13
      indexing=?
      best=(unit id:block:bc)
      inflight=(map ship inflight-writ)
      confidential=(set ship)
      attested=(map ship sont:ord)
      publicizing=(set ship)
      next-job=@ud
      sponsees=(map ship sponsee)
      declined=(set ship)
  ==
--
::
%-  agent:dbug
^-  agent:gall
=|  gw-state
=*  state  -
%+  verb  |
=<
|_  =bowl:gall
+*  this   .
    def    ~(. (default-agent this %|) bowl)
::
++  on-init
  ^-  (quip card _this)
  ::  register as the verifier agent for our own PKI domain (= our
  ::  agent name).  Jael watches /writs for %verdict /
  ::  %anew-response / %azimuth-udiffs facts.
  ::
  ::  .indexing and .synced are pinned here rather than left to the mold's
  ::  bunt (which is %.y for ?): a fresh agent has NOT bootstrapped its
  ::  public index and has NOT heard from its light client.  Both must
  ::  start false or the agent comes up believing things it has no
  ::  evidence for.  See $gw-state for why neither is declared `_|`.
  ::
  :_  this(indexing %.n, synced %.n)
  :~  [%pass /anex %arvo %j %anex /writs]
      [%pass /best-block %agent [our.bowl light-client-agent:lca] %watch /best-block]
      (watch-synced our.bowl)
  ==
::
++  on-save
  ^-  vase
  !>(state)
::
++  on-load
  |=  =vase
  ^-  (quip card _this)
  ::  Revisions before the light-client port carried a leading
  ::  `rpc=req-to:btcio` ([url=@t auth]) ahead of urb-state.  Nothing
  ::  reads a Bitcoin Core RPC any more, so drop it.  Discriminate on the
  ::  head of the head: legacy is the url ATOM, current is urb-state's
  ::  block-id CELL.
  ::
  =/  nou=*  q.vase
  =.  nou  ?:(?=([[@ *] *] nou) +.nou nou)
  ::  .own (our own custody log), then .synced and .reorg-halt, were
  ::  appended later, and .reorg-halt has since been removed again.  ;; is
  ::  strict about arity, so the current mold succeeding IS the
  ::  discriminator; a pre-.own state loads with an empty log and re-seeds
  ::  itself from our pass's xtr on the next %anew.
  ::
  ::  An upgrade from either older shape must also SUBSCRIBE to /is-synced.
  ::  Gall keeps outgoing subscriptions across +on-load, so the existing
  ::  /best-block one survives and must not be re-issued -- but nothing has
  ::  ever asked for /is-synced, and +on-init (which would) does not run on
  ::  an upgrade.  Emitting it exactly on the migration branches is what
  ::  makes the new readiness gate work on a ship that is upgraded in place
  ::  rather than reinstalled.
  ::
  =/  cur  (mole |.(;;(gw-state nou)))
  ?^  cur  `this(state u.cur)
  ::  A -14 state carries .reorg-halt, the scanner's reorg STOP flag.  It
  ::  is dropped, not converted: the scanner no longer halts on a reorg,
  ::  it forgets the points the orphaned blocks were evidence for and
  ::  rewinds (see the %reorg-rollback branch of +on-agent).
  ::
  ::  A ship upgrading WHILE HALTED therefore comes back scanning, from
  ::  wherever its cursor was left.  That is the right outcome and it is
  ::  the same one %gw-reorg-resume gave an operator who poked `~`:
  ::  resume from the cursor.  The reorg that caused the halt is over and
  ::  its orphan list is not recoverable now, so this branch cannot
  ::  repair the index -- the NEXT reorg is repaired properly, and this
  ::  one is left exactly where the halt already left it.
  ::
  =/  o14  (mole |.(;;(gw-state-14 nou)))
  ?^  o14
    =/  ext=gw-state
      :*  urb-state.u.o14
          indexing.u.o14  best.u.o14  inflight.u.o14
          confidential.u.o14  attested.u.o14
          next-job.u.o14  sponsees.u.o14  declined.u.o14  own.u.o14
          synced.u.o14
      ==
    ?~  reorg-halt.u.o14  `this(state ext)
    %-  %-  slog
        :~  leaf+"%gw-btc: clearing a reorg halt from {<since.u.reorg-halt.u.o14>}; the scanner resumes from cursor {<num.block-id.urb-state.u.o14>}"
            leaf+"  (the halt is gone: a reorg now forgets the points the orphaned blocks proved, and rewinds)"
        ==
    `this(state ext)
  ::  A -13 state's points carry no .seen (sur/urb): they were indexed
  ::  before a $point recorded which block it was last observed in.  They
  ::  are lifted with .seen=~, which is not a placeholder -- it is the
  ::  honest statement that we have no provenance for them and cannot
  ::  filter them against a reorg's orphan list.  See +orphaned-points.
  ::
  =/  o13  (mole |.(;;(gw-state-13 nou)))
  ?^  o13
    =/  ext=gw-state
      :*  (lift-urb-state urb-state.u.o13)
          indexing.u.o13  best.u.o13  inflight.u.o13
          confidential.u.o13  attested.u.o13
          next-job.u.o13  sponsees.u.o13  declined.u.o13  own.u.o13
          synced.u.o13
      ==
    `this(state ext)
  ::  A -12 state carries .publicizing, which no longer exists: the block
  ::  scanner does not index a publication any more, it hands it to the
  ::  same verifier a packet goes to, so there is no public-spawn replay
  ::  to guard against and .inflight is the only single-flight left.
  ::  Dropping it needs no other work -- its readers are gone.
  ::
  =/  o12  (mole |.(;;(gw-state-12 nou)))
  ?^  o12
    =/  ext=gw-state
      :*  (lift-urb-state urb-state.u.o12)
          indexing.u.o12  best.u.o12  inflight.u.o12
          confidential.u.o12  attested.u.o12
          next-job.u.o12  sponsees.u.o12  declined.u.o12  own.u.o12
          synced.u.o12
      ==
    `this(state ext)
  =/  o11  (mole |.(;;(gw-state-11 nou)))
  ?^  o11
    =/  ext=gw-state
      :*  (lift-urb-state urb-state.u.o11)
          indexing.u.o11  best.u.o11  inflight.u.o11
          confidential.u.o11  attested.u.o11
          next-job.u.o11  sponsees.u.o11  declined.u.o11  own.u.o11
          %.n
      ==
    :_  this(state ext)
    ~[(watch-synced our.bowl)]
  =/  o  ;;(gw-state-10 nou)
  =/  ext=gw-state
    :*  (lift-urb-state urb-state.o)
        indexing.o  best.o  inflight.o  confidential.o
        attested.o  next-job.o  sponsees.o  declined.o
        *own-custody  %.n
    ==
  :_  this(state ext)
  ~[(watch-synced our.bowl)]
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?+    mark  !!
      ::  Jael forwards a comet self-attestation for on-chain
      ::  verification.  The pass's xtr carries a jammed Groundwire
      ::  custody locator log; the verification strand fetches each named
      ::  transaction through %light-client before running the pure checks.
      ::
      %noun
    ?>  =(our src):bowl
    ::  MOLD-CAST, never +!<.  Jael builds this poke as `!>(pok)` with
    ::  `pok` typed `*` (sys/vane/jael.hoon, +poke-watch), and a `*`-typed
    ::  vase does not NEST under a head-tagged union -- so +!< bails on
    ::  every writ that actually came from Jael, i.e. on every real
    ::  attestation over the network.  Poking `&noun [%jael-writ ...]`
    ::  from the dojo builds a fully typed vase and hides this completely.
    ::  Causeway's custody-log ingestion ($ingest, sur/self-attestation)
    ::  rides the %noun mark too, so neither side needs a mark file.
    ::  Dispatch on the head tag BEFORE the $jael-poke mold-cast, which
    ::  would bail on a tag outside its union.
    ::
    ?:  ?=([%gw-custody-entry *] q.vase)
      =/  pok  ;;(ingest:sa q.vase)
      =/  res
        %:  begin-anew
            q.byk.bowl
            our.bowl
            now.bowl
          (extend-log:lsa (base-chain our.bowl now.bowl) entry.pok)
        ==
      [-.res this(state +.res)]
    ::  %gevulot hands us a peer attestation to install WITHOUT verifying
    ::  it on chain -- our sponsor already did, and %gevulot (local) has
    ::  checked it came from our sponsor with our opt-in on.  We still do
    ::  the OFFLINE self-check (+pass-attestation: @p == fingerprint, and
    ::  the custody log decodes) and build the jael point from the latest
    ::  opening's snapshot; we fetch nothing.  It is idempotent and cannot
    ::  emit a NEGATIVE verdict (a bad pass yields ~ and no card), so a
    ::  duplicate -- the same peer heard twice, or heard then later seen on
    ::  chain -- never snubs.  This is the sync-wait-free discovery path.
    ::
    ?:  ?=([%gw-trusted-peer *] q.vase)
      =/  tp  ;;([%gw-trusted-peer who=ship =pass] q.vase)
      :_  this
      (install-trusted-peer who.tp pass.tp)
    =/  poke  ;;(jael-poke:urb q.vase)
    ?-    -.poke
        %jael-writ
      ::  EVERY drop below is announced, and now that is TRUE.  Phase 5b's
      ::  finding 9 was that %jael-writ has eleven distinct pre-verification
      ::  exits which from outside look identical to "nobody is talking to
      ::  this ship".  Phase 6.2 then paid for it for real: a runtime fault
      ::  stranded the single-flight slot and the two retries that would
      ::  have diagnosed it were swallowed without a word.  bf90840
      ::  announced nine of them and this comment claimed all of
      ::  them; the clean-room run (6.7) found the other three still mute
      ::  -- and all three of THOSE emit a sticky snub.
      ::
      ::  So every exit goes through +drop-writ, which takes a $writ-drop
      ::  and derives BOTH the log line (+writ-drop-report:lsa) and the
      ::  cards (+writ-drop-fate:lsa) from it.  Neither can be omitted and
      ::  the two cannot disagree; a twelfth disposition does not
      ::  compile until it has a line and a fate.
      ::
      ::  These lines are one-per-dropped-writ, so they are also the rate
      ::  at which a peer is retrying, which is itself the thing you want
      ::  to know.
      ::
      ::  Single-flight per ship: at most one verification job, and that
      ::  is now the ONLY single-flight there is -- an on-chain
      ::  publication takes the same slot, because it is the same
      ::  self-attestation arriving by a different road.  A duplicate or
      ::  replacement writ while one is in flight is dropped -- the
      ::  peer's retries re-enter after the verdict, and the on-chain
      ::  cost of minting states is the rate limit.  No queue, no slot
      ::  economy.
      ?:  (~(has by inflight) who.poke)
        :_  this
        (drop-writ our.bowl dom.poke who.poke %in-flight ~)
      ::  A ship whose sponsorship we declined re-attests on every
      ::  retry; short-circuit before spending a full verification on
      ::  it.  Clearing the entry (%gw-sponsor-clear) restores normal
      ::  handling immediately -- the refusal is never sticky in the
      ::  kernel, only here.
      ?:  (~(has in declined) who.poke)
        :_  this
        (drop-writ our.bowl dom.poke who.poke %declined ~)
      ?~  sat=(pass-attestation [dom who pass]:poke)
        ::  Two shapes we decline to JUDGE rather than judge negatively,
        ::  because a negative verdict becomes a Jael %fail and an Ames
        ::  snub:
        ::
        ::    - the public onboarding packet: a valid suite-C %gw-btc pass
        ::      with no xtr tail at all, resolved by the block scanner.
        ::      Its temporary absence from the index is not evidence.
        ::
        ::    - a pass minted under a FOREIGN protocol kelvin: we cannot
        ::      verify it and must not blacklist it.  Snubbing here would
        ::      make old and new ships mutually snub each other across a
        ::      kelvin bump, i.e. partition the network on upgrade.
        ::
        ::  Everything else that fails to decode really is malformed.
        ::  All three were silent, and the third SNUBS -- an operator
        ::  watching a peer get blacklisted saw nothing whatsoever, on
        ::  the one path where the two harmless cases and the destructive
        ::  one are told apart by a predicate they cannot see.
        ::
        ?:  (public-pass [dom who pass]:poke)
          :_  this
          (drop-writ our.bowl dom.poke who.poke %onboarding ~)
        ?:  (foreign-kelvin [dom who pass]:poke)
          :_  this
          (drop-writ our.bowl dom.poke who.poke %foreign-kelvin ~)
        :_  this
        (drop-writ our.bowl dom.poke who.poke %undecodable ~)
      ::  A present, canonically decoded xtr must contain the spawn opening.
      ::  The raw-0 public shape was classified above before +from-xtr.
      ?~  chain.u.sat
        :_  this
        (drop-writ our.bowl dom.poke who.poke %empty-log ~)
      ?:  (gth (lent chain.u.sat) max-custody-log)
        :_  this
        %-  drop-writ
        :*  our.bowl  dom.poke  who.poke
            %log-too-long  (lent chain.u.sat)  max-custody-log
        ==
      ?:  (known-public who.poke)
        :_  this
        (drop-writ our.bowl dom.poke who.poke %already-public ~)
      ::  ------------------------------------------------------------
      ::  READINESS.  Drop until the LIGHT CLIENT can actually answer;
      ::  readiness is infrastructure, never evidence.
      ::  ------------------------------------------------------------
      ::
      ::  This gate was `?~ best` and that was NOT a readiness check.
      ::  %bitcoin-client answers a fresh /best-block subscription with the
      ::  GENESIS block, so `best` is set within milliseconds of boot and a
      ::  ship 961,000 blocks behind sailed straight through.  It then
      ::  judged a real attestation against a chain it had never seen and
      ::  snubbed the honest comet it sponsors (live mainnet 2026-08-06,
      ::  Phase 6.1).  Three things must hold now:
      ::
      ::    1. `best` exists -- it is the tip the strand scans up to, and
      ::       the strand must NOT re-read /best-block itself (persistent
      ::       subscription, never kicks).
      ::    2. the light client says it is SYNCED: block headers AND filter
      ::       headers at the tip, with live peers.  The BIP-158 liveness
      ::       scan is a filter-header consumer, so nothing less will do.
      ::    3. the tip we believe in actually COVERS this log's evidence.
      ::       Belt and braces for (2): it is the property the verification
      ::       mathematically needs, it is checkable from the writ itself,
      ::       and it is what stops +scan-liveness being handed a range
      ::       that runs backwards.
      ::
      ::  A writ that fails any of these gets NO verdict.  Dropping is
      ::  already safe -- the peer retransmits, and the log line above says
      ::  how often -- whereas judging is not.
      ::
      ::  Deliberately NOT gated on .indexing.  Confidential verification
      ::  needs nothing from the public index except the sponsor-existence
      ::  set, which is now unevaluable-when-empty rather than false, so a
      ::  light-client-only deployment still verifies unsponsored comets.
      ::
      ?~  best
        :_  this
        (drop-writ our.bowl dom.poke who.poke %no-tip ~)
      ::  ... and the %unsynced fate re-reads the light client's answer
      ::  while we are at it, because it may have caught up without
      ::  telling us.  See +refresh-synced and +writ-drop-fate:lsa.
      ?.  synced
        :_  this
        (drop-writ our.bowl dom.poke who.poke %unsynced num.u.best)
      =/  need=@ud  (log-top-height u.sat)
      ?.  (gte num.u.best need)
        :_  this
        (drop-writ our.bowl dom.poke who.poke %tip-below-log num.u.best need)
      =/  job  next-job
      =.  next-job  +(next-job)
      =/  req=inflight-writ  [dom.poke pass.poke u.sat job]
      =.  inflight  (~(put by inflight) who.poke req)
      :_  this
      (verify-cards %verify q.byk.bowl now.bowl who.poke req num.u.best)
    ::
        %jael-anew
      ::  Our own comet asking for a fresh self-attestation.
      ::
      ::  We answer by RE-VALIDATING our current custody log against the
      ::  chain and emitting the pass only if it still holds -- never
      ::  from memory.  The log may have gone stale since we last checked
      ::  (the identity sat can be spent by anyone holding the key), and
      ::  a stale pass is strictly worse than silence: it would be
      ::  installed in ames and then rejected by every peer.
      ::
      ::  The answer therefore arrives asynchronously, on the same
      ::  /writs path, once the light client has walked the log.  Jael
      ::  relays any %anew-response fact to its %sybl subscribers, so
      ::  there is no request/response pairing to keep and no deadline to
      ::  miss (ames simply installs the pass when it lands).
      ::
      ::  Jael addressed a %anew to a PKI domain that is not ours.  It
      ::  cannot happen through the %anex registration (the domain is this
      ::  agent's own name, 1:1 by construction), so a hand-poked or
      ::  cross-domain %jael-anew is the only way here -- and it used to
      ::  vanish, which reads exactly like a %anew that was accepted and
      ::  then produced nothing.
      ::
      ?.  =(dom.poke domain:cc)
        %-  %-  slog
            :~  leaf+"%gw-btc: %jael-anew for domain {<dom.poke>} ignored; this agent serves {<domain:cc>}"
                leaf+"  (no pass will be refreshed -- poke %jael-anew with our own domain)"
            ==
        `this
      =/  res
        %:  begin-anew
            q.byk.bowl
            our.bowl
            now.bowl
          (base-chain our.bowl now.bowl)
        ==
      [-.res this(state +.res)]
    ==
  ::
      ::  Operator control over sponsorship.  %gw-sponsor-decline
      ::  refuses (and drops) a sponsee; %gw-sponsor-clear undoes that
      ::  so the ship's next attestation is handled normally again.
      ::  Neither touches the kernel: a declined ship is never snubbed.
      ::
      %gw-sponsor-decline
    ?>  =(our src):bowl
    =/  who  !<(ship vase)
    %-  (slog leaf+"%gw-btc: sponsorship of {<who>} declined by operator" ~)
    `this(declined (~(put in declined) who), sponsees (~(del by sponsees) who))
  ::
      %gw-sponsor-clear
    ?>  =(our src):bowl
    =/  who  !<(ship vase)
    `this(declined (~(del in declined) who))
  ::
      %urb-start-indexing
    ?>  =(our src):bowl
    ::  Bootstrap is one-shot. Replacing the public snapshot while private
    ::  points or verifier jobs exist would mix incompatible index epochs;
    ::  an operator who deliberately needs to rebootstrap must nuke first.
    ::  Guarded on the INDEX, not just the flag -- see %gw-index-from.
    ::  NB: `!=(~ ...)` rather than `?=(^ ...)`.  A ?= on a state leg
    ::  narrows that leg's TYPE in the false branch, and the branches
    ::  below assign a whole fresh $state:urb into it -- which then
    ::  nest-fails against the narrowed `unv-ids=~`.
    ::
    ::  Deliberately NOT `?| indexing ...`.  The bunt of ? is %.y, so a
    ::  flag-based guard refuses the FIRST bootstrap on a fresh agent
    ::  unless +on-init has run -- and it is exactly the kind of thing
    ::  that is true in production and false under test.  The index
    ::  itself is the evidence: a cursor that has moved, or a single
    ::  indexed point, means there is something here to lose.
    ::
    =/  have-index=?
      ?|  !=(0 num.block-id.urb-state)
          !=(~ unv-ids.urb-state)
      ==
    ?:  have-index
      %-  (slog leaf+"%gw-btc: refusing %urb-start-indexing: an index already exists" ~)
      `this
    =/  start-urb  ;;((unit state:urb) !<((unit noun) vase))
    ?~  start-urb
      %-  (slog :_(~ [%leaf "%gw-btc: indexing from block {<num.block-id:(state:urb default-urb-state)>}"]))
      =.  urb-state  default-urb-state
      =.  indexing  &
      :_  this
      ~[[%pass /timer %arvo %b %wait now.bowl]]
    %-  (slog :_(~ [%leaf "%gw-btc: processing Groundwire snapshot ({<~(wyt by unv-ids.u.start-urb)>} points)"]))
    =.  urb-state  u.start-urb
    =.  indexing  &
    :_  this
    :~  (listen-to-urb ~(key by unv-ids.u.start-urb) [%| dap.bowl])
        [%pass /timer %arvo %b %wait (add ~s30 now.bowl)]
    ==
  ::
      ::  Bootstrap the public index at a bare HEIGHT: the first block to
      ::  scan.  Under the light client a block is addressed by height
      ::  alone, so an operator no longer has to supply (or fake) a block
      ::  hash to choose a start point -- the whole reason
      ::  %urb-start-indexing needed a hand-built $state:urb.
      ::
      %gw-index-from
    ?>  =(our src):bowl
    ::  Bootstrap is one-shot, and the guard must not rest on a FLAG.
    ::  This poke replaces urb-state wholesale -- cursor, sat index and
    ::  .unv-ids -- so getting the guard wrong destroys the public index
    ::  that `sponsor-known` reads.  .indexing was silently reset to %.n
    ::  by every +on-load for as long as it was declared `_|` (see
    ::  $gw-state), which left this open on every upgrade.  Refuse
    ::  whenever there is an index to lose, whatever the flag says.
    ::
    ::  NB: `!=(~ ...)` rather than `?=(^ ...)`.  A ?= on a state leg
    ::  narrows that leg's TYPE in the false branch, and the branches
    ::  below assign a whole fresh $state:urb into it -- which then
    ::  nest-fails against the narrowed `unv-ids=~`.
    ::
    ::  Deliberately NOT `?| indexing ...`.  The bunt of ? is %.y, so a
    ::  flag-based guard refuses the FIRST bootstrap on a fresh agent
    ::  unless +on-init has run -- and it is exactly the kind of thing
    ::  that is true in production and false under test.  The index
    ::  itself is the evidence: a cursor that has moved, or a single
    ::  indexed point, means there is something here to lose.
    ::
    =/  have-index=?
      ?|  !=(0 num.block-id.urb-state)
          !=(~ unv-ids.urb-state)
      ==
    ?:  have-index
      %-  %-  slog
          :~  leaf+"%gw-btc: refusing %gw-index-from: an index already exists"
              leaf+"  (cursor {<num.block-id.urb-state>}, {<~(wyt by unv-ids.urb-state)>} points;"
              leaf+"   rebootstrapping means nuking the agent, deliberately)"
          ==
      `this
    =/  start=@ud  !<(@ud vase)
    ::  Height 0 is the "did you mean to?" guard -- block 0 is the genesis
    ::  block and nobody bootstraps from it -- and it used to swallow the
    ::  poke without a word, so an operator who fat-fingered the argument
    ::  saw an agent that accepted the command and then never indexed.
    ::
    ?:  =(0 start)
      %-  (slog leaf+"%gw-btc: refusing %gw-index-from 0: name the first block to scan" ~)
      `this
    %-  (slog leaf+"%gw-btc: indexing from block {<start>}" ~)
    =.  urb-state  [[0x0 (dec start)] *sont-map:ord *insc-ids:ord *unv-ids:urb]
    =.  indexing   &
    :_  this
    ~[[%pass /timer %arvo %b %wait now.bowl]]
  ==
::
++  on-peek
  |=  =(pole knot)
  ^-  (unit (unit cage))
  ?+    pole  (on-peek:def pole)
    ::  /x/block-id — current synced block hash + height
    ::
      [%x %block-id ~]
    ``block-id+!>(block-id.urb-state)
    ::  /x/point/<ship> — look up a ship in unv-ids
    ::
    ::  /x/sponsees — comets we are sponsoring (and thus relaying for)
    ::
      [%x %sponsees ~]
    ``noun+!>(sponsees)
    ::  /x/declined — comets whose sponsorship we have refused
    ::
      [%x %declined ~]
    ``noun+!>(declined)
    ::  /x/custody — OUR OWN validated custody log (the xtr we serve)
    ::
    ::  Only ever holds a log that passed a full light-client walk; an
    ::  ingestion that failed leaves the previous one in place.
    ::
      [%x %custody ~]
    ``noun+!>(chain.own)
    ::  The single-flight bookkeeping, exposed because it is otherwise
    ::  invisible and a stuck slot silences a ship forever.  %jael-writ
    ::  has eight distinct silent-drop returns and .inflight is one of
    ::  them, indistinguishable from outside (Phase 5b, finding 9).  It
    ::  now also holds on-chain publication jobs, which take the same slot.
    ::
      [%x %inflight ~]
    ``noun+!>(~(key by inflight))
    ::  The %anew single-flight slot.  Same argument as /x/inflight: a
    ::  stranded one silences our OWN pass refresh forever and had no
    ::  witness at all (Phase 7.2 spent an hour eliminating the other six
    ::  causes before reaching it).
    ::
      [%x %pending-own ~]
    ``noun+!>(?~(pending.own ~ `job.u.pending.own))
    ::  READINESS, exposed.  `?~ best` used to drop writs with no log line
    ::  and nothing exposed `best`, so a ship quietly refusing every
    ::  attestation looked exactly like a ship nobody was talking to
    ::  (Phase 6.7).  [synced best-height indexing].
    ::
    ::  A fourth field, .reorg-halt, used to ride here: the scanner could
    ::  be stopped indefinitely by a reorg and an operator needed to see
    ::  that without reading logs.  It cannot be stopped that way any
    ::  more, so there is nothing to expose.
    ::
      [%x %ready ~]
    :^  ~  ~  %noun
    !>  :*  synced=synced
            tip=?~(best ~ `num.u.best)
            indexing=indexing
        ==
    ::  Which identities this ship holds CONFIDENTIALLY (so their points
    ::  are withheld from /x/points and from jael's udiffs), and the tip
    ::  each one last attested to.
    ::
      [%x %confidential ~]
    ``noun+!>(confidential)
    ::
      [%x %attested ~]
    ``noun+!>(attested)
    ::
      [%x %point ship=@ ~]
    ?~  who=(slaw %p ship.pole)  ~
    ?:  (~(has in confidential) u.who)
      [~ ~]
    ?~  point=(~(get by unv-ids.urb-state) u.who)
      [~ ~]
    ``urb-point+!>(u.point)
    ::  /x/points — all spawned identities
    ::
      [%x %points ~]
    =/  public  (filter-snapshot urb-state confidential)
    ``urb-points+!>(unv-ids.public)
    ::  /x/urb-state — entire urb-state for snapshots
    ::
      [%x %urb-state ~]
    ``noun+!>((filter-snapshot urb-state confidential))
  ==
::
++  on-watch
  |=  =(pole knot)
  ^-  (quip card _this)
  ?+    pole  (on-watch:def pole)
  ::
  ::  Jael's %anex registration watches this path for asynchronous
  ::  %verdict and %anew-response facts.
      [%writs ~]
    ?>  =(our src):bowl
    `this
  ::
  ::  %urb-snapshot listens for new urb-states
      [%urb-state ~]
    ?>  =(our src):bowl
    =/  public  (filter-snapshot urb-state confidential)
    :_  this
    :~  :*  %give  %fact  ~
            %urb-state  !>(public)
        ==
    ==
  ::
  ::  Jael subscribes to / (aka ~) if it hears
  ::  that this agent is the default PKI source
      ~
    ?>  =(our src):bowl
    `this
  ::
  ::  Jael subcribes to /ship when it hears about a new ship
      [=ship ~]
    ?>  =(our src):bowl
    =/  who  (slav %p ship.pole)
    ::  Withholding a confidential point is the DESIGN, not a failure, and
    ::  it is silent on purpose: jael and %urb-snapshot both read this
    ::  surface, so a line here would fire on ordinary traffic rather than
    ::  on anything going wrong.  /x/confidential is the witness -- it
    ::  lists exactly the identities whose points are withheld.
    ::
    ?:  (~(has in confidential) who)
      `this
    :_  this
    :~  :*  %give
            %fact
            ~
            %azimuth-udiffs
            !>  ^-  udiffs:point:jael
            %+  murn
              (state-to-udiffs urb-state.state)
            |=  [=ship =udiff:point:jael]
            ^-  (unit [^ship udiff:point:jael])
            ::  ignore all ships but /ship
            ?.  =(ship who)
              ~
            `[ship udiff]
        ==
    ==
  ==
::
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card _this)
  ?+    wire  (on-arvo:def wire sign-arvo)
  ::
  ::  Run +get-blocks at regular intervals.  The chain tip comes from our
  ::  own persistent /best-block subscription -- the scanner never asks the
  ::  node for a tip, and with no tip yet there is simply nothing to do.
      [%timer ~]
    ?~  best
      :_  this
      ~[[%pass /timer %arvo %b %wait (add ~s30 now.bowl)]]
    :_  this
    :~  :*  %pass  /blocks  %arvo  %k
            %lard  q.byk.bowl
            (get-blocks urb-state num.u.best)
        ==
    ==
  ::
  ::  A %light-client-backed attestation verification returned.
      [%lc-retry ~]
    :_  this
    :~  [%pass /best-block %agent [our.bowl light-client-agent:lca] %watch /best-block]
    ==
  ::
  ::  Re-arm the /is-synced subscription after a rejected watch.  While it
  ::  is down .synced stays %.n, i.e. we hold every writ rather than judge
  ::  it -- the fail-closed direction.
      [%synced-retry ~]
    :_  this
    ~[(watch-synced our.bowl)]
  ::
      ::  (%verify-timeout is the pre-rename wire; accepted so a timer set
      ::  by an older revision cannot crash the agent on upgrade.
      ::  %claim-stuck is the same guard for an on-chain publication: same
      ::  slot, same release, so the same arm serves both.)
      [?(%stuck-job %verify-timeout %claim-stuck) ship=@ job=@ ~]
    ?.  ?=([%behn %wake *] sign-arvo)
      (on-arvo:def wire sign-arvo)
    =/  who  (slav %p i.t.wire)
    =/  job  (slav %ud i.t.t.wire)
    =/  active  (~(get by inflight) who)
    ::  DELIBERATELY SILENT, both of them, and this is the one place in
    ::  this agent where silence is right.  +verify-cards sets a ~h2 behn
    ::  timer per job and never rests it, so EVERY completed verification
    ::  wakes here two hours later with nothing to release.  A line would
    ::  be one per verification, two hours after the fact, saying nothing
    ::  happened -- which is worse than nothing: it looks like a fault.
    ::
    ?~  active
      `this
    ?.  =(job job.u.active)
      `this
    ::  The leak guard fired: this job made no progress for +stuck-job-guard
    ::  and is presumed dead.  That is an infrastructure failure, not
    ::  evidence that the attestation is invalid.  Release the ship's slot
    ::  silently so a later packet can retry; emitting res=~ here would
    ::  cause Ames to snub a valid peer.
    %-  (slog leaf+"%gw-btc: releasing stuck verification slot for {(scow %p who)}" ~)
    `this(inflight (~(del by inflight) who))
  ::
      ::  Our OWN custody log finished validating.  A positive verdict
      ::  is the ONLY thing that may store the log or publish the pass;
      ::  anything else (crash, timeout, negative verdict) leaves the
      ::  previous log alone and answers %anew with silence.
      ::
      [%anew job=@ ~]
    =/  job  (slav %ud i.t.wire)
    ::  NB: narrow a COPY.  ?~ on `pending.own` would retype the state
    ::  leg itself, and the `=.` below could then no longer put ~ back.
    =/  pend  pending.own
    ::  An answer for a slot that is no longer ours.  Not routine: the
    ::  only way here is that the ~h2 leak guard already released the
    ::  slot (or a newer job took it) and the thread then answered
    ::  anyway, so the work is real and is being thrown away.  Silence
    ::  made that indistinguishable from an %anew that never ran.
    ::
    ?~  pend
      %-  %-  slog
          :~  leaf+"%gw-btc: discarding a %anew result for job {<job>}: no self-validation is pending"
              leaf+"  (the ~h2 leak guard released the slot before the thread answered;"
              leaf+"   our pass is NOT refreshed -- re-poke %jael-anew)"
          ==
      `this
    ?.  =(job job.u.pend)
      %-  %-  slog
          :~  leaf+"%gw-btc: discarding a %anew result for job {<job>}: job {<job.u.pend>} holds the slot"
              leaf+"  (a stale thread answered after a newer self-validation started)"
          ==
      `this
    =/  req=anew-job  u.pend
    =.  pending.own  ~
    ?+    sign-arvo  (on-arvo:def wire sign-arvo)
        [%khan %arow *]
      ?.  -.p.sign-arvo
        ?>  ?=([%khan %arow %.n *] sign-arvo)
        ::  THE line test 6.7 caught.  It read `%anew self-validation
        ::  ended without a verdict' and nothing else -- twice in the
        ::  clean-room run, from two unrelated causes, indistinguishable.
        ::  The mote was thrown away here and the tang is EMPTY for the
        ::  commonest death of all (+set-timeout:strandio fails with
        ::  `[%timeout ~]'), so the line carried no information at all.
        ::  +strand-death-report:lsa names the job, what it was doing, the
        ::  mote, and what an empty tang means.
        ::
        %-  %-  slog
            %:  strand-death-report:lsa
                [%own job (lent chain.req)]
                -.p.p.sign-arvo
              +.p.p.sign-arvo
            ==
        `this
      ?>  ?=([%khan %arow %.y %noun *] sign-arvo)
      =/  [%khan %arow %.y %noun =vase]  sign-arvo
      =/  parsed  !<([result:sa hexb:bc] vase)
      =/  res=result:sa  -.parsed
      %-  (slog (report:lsa verdict.res))
      ?.  &(ok.verdict.res =(our.bowl who.verdict.res))
        %-  %-  slog
            :~  leaf+"%gw-btc: our own custody log did not verify; %anew stays silent"
                leaf+"  (the stored log is unchanged; a stale pass is worse than none)"
            ==
        `this
      ::  Warn the OPERATOR (never the network) if the state we just
      ::  proved leaves us unreachable.
      %-  (slog (unroutable-point our.bowl point.res))
      %-  (slog leaf+"%gw-btc: custody log verified ({<(lent chain.req)>} entries); refreshing our pass" ~)
      :_  this(chain.own chain.req)
      ~[(anew-card domain:cc pass.req)]
    ==
  ::
      ::  Leak backstop for the %anew job, mirroring +stuck-job-guard for
      ::  peer verification: release the slot, emit nothing.
      ::
      [%anew-guard job=@ ~]
    ?.  ?=([%behn %wake *] sign-arvo)
      (on-arvo:def wire sign-arvo)
    =/  job  (slav %ud i.t.wire)
    =/  pend  pending.own
    ::  Silent for the same reason as +stuck-job-guard above: the guard
    ::  timer is never rested, so every successful %anew wakes here two
    ::  hours later with an empty slot and nothing to say.
    ::
    ?~  pend  `this
    ?.  =(job job.u.pend)  `this
    %-  (slog leaf+"%gw-btc: releasing stuck %anew self-validation slot" ~)
    `this(pending.own ~)
  ::
      [%verify ship=@ job=@ ~]
    =/  who  (slav %p i.t.wire)
    =/  job  (slav %ud i.t.t.wire)
    =/  active  (~(get by inflight) who)
    ::  An answer for a slot this ship no longer holds.  Three ways in,
    ::  all of them things that already went wrong: the ~h2 leak guard
    ::  released it, the block scanner dropped it (+drop-inflight, on a
    ::  public spawn or a stale tip), or a newer job replaced it.  The
    ::  verification really ran and its verdict is being discarded, which
    ::  is worth exactly one line -- and used to be worth none.
    ::
    ?~  active
      %-  %-  slog
          :~  leaf+"%gw-btc: discarding a verdict for {(scow %p who)} (job {<job>}): it holds no verification slot"
              leaf+"  (released by the ~h2 leak guard, or dropped by the block scanner;"
              leaf+"   no verdict is emitted, and the peer's next packet re-runs it)"
          ==
      `this
    ?.  =(job job.u.active)
      %-  %-  slog
          :~  leaf+"%gw-btc: discarding a verdict for {(scow %p who)} (job {<job>}): job {<job.u.active>} holds the slot"
              leaf+"  (a stale thread answered after a newer verification started)"
          ==
      `this
    =/  req=inflight-writ  u.active
    =.  inflight  (~(del by inflight) who)
    ?+    sign-arvo  (on-arvo:def wire sign-arvo)
        [%khan %arow *]
      ?.  -.p.sign-arvo
        ?>  ?=([%khan %arow %.n *] sign-arvo)
        ::  A generic strand crash is retryable/indeterminate.  Only a
        ::  mold-valid verifier result may emit a Jael verdict.  Same
        ::  reasonless-line problem as the %anew path above: the mote was
        ::  dropped and a +set-timeout death carries an empty tang, so
        ::  the operator was told a thread ended and nothing else.
        ::
        %-  %-  slog
            %:  strand-death-report:lsa
                [%peer who job]
                -.p.p.sign-arvo
              +.p.p.sign-arvo
            ==
        `this
      ?>  ?=([%khan %arow %.y %noun *] sign-arvo)
      =/  [%khan %arow %.y %noun =vase]  sign-arvo
      =/  [res=result:sa tip-spk=hexb:bc]
        !<([result:sa hexb:bc] vase)
      ::  A concurrently indexed public spawn wins this race.  It no longer
      ::  belongs to the confidential verifier, but that is not evidence the
      ::  peer supplied a bad attestation, so do not emit a sticky failure.
      ?:  (known-public who)
        %-  %-  slog
            :~  leaf+"%gw-btc: verdict for {(scow %p who)} discarded: the block scanner indexed it as PUBLIC first"
                leaf+"  (it is no longer the confidential verifier's to judge; not evidence"
                leaf+"   against the peer, so no verdict is emitted and nothing is snubbed)"
            ==
        `this
      %-  (slog (report:lsa verdict.res))
      ::  THE SECOND DOORWAY ONTO THE SNUB PATH.  ++run-checks can say ok
      ::  and this agent can still decline, for reasons that are about OUR
      ::  state rather than the peer's evidence.  Those refusals used to
      ::  fall straight through to the negative branch below, so a peer
      ::  with a cryptographically perfect attestation could be snubbed --
      ::  stickily -- for something it neither sent nor could observe.
      ::  +local-refusal names them; +refusal-class:lsa classes them.
      ::
      =/  refused=(unit refusal:sa)  (local-refusal who req res)
      =/  verified=(unit point:urb)
        ?.  &(ok.verdict.res ?=(~ refused))  ~
        ?~  point.res  ~
        `u.point.res(pass.net pass.req)
      ?~  verified
        ::  Decisions addendum section 3, on the PACKET path.  A verdict
        ::  that failed ONLY because the evidence is out of date is not
        ::  fraud, and emitting res=~ here would make jael %fail and ames
        ::  SNUB -- which then blocks the very packet that would fix it.
        ::  Observed on mainnet: a comet performed an honest, correctly
        ::  formed state update; its ship kept sending the attestation it
        ::  booted with; its own SPONSOR verified that packet, found the
        ::  tip spent, and snubbed it forever.
        ::
        ::  So route staleness to the same %stale outcome the scanner
        ::  path already produces (+detect-stale -> +stale-card): jael
        ::  drops the point if it holds one and ames demotes the peer to
        ::  a fresh alien, never a snub, so the refreshed attestation can
        ::  arrive.  The same physical fact must take the same path
        ::  whether the scanner or the inbox saw it first.
        ::
        ::  Emitting %stale for a ship jael never verified under this
        ::  domain is a no-op there (it checks .hep first), so this is
        ::  also the "stay silent" case, without a second code path.
        ::
        ::  We deliberately do NOT drop our own indexes here.  The peer
        ::  is the only thing that can fix this, by re-attesting, and
        ::  keeping .attested/.unv-ids means +tracked-anchor still
        ::  supplies the anchor that proves the REPLACEMENT log extends
        ::  the one we already verified.  Our scanner drops them itself
        ::  when it reaches the block that spent the sat.
        ::
        ::  The THIRD outcome, and the one Phase 6.1 proved was missing.
        ::  A verdict whose only failures are checks we could not EVALUATE
        ::  says nothing about the peer, so we say nothing about the peer.
        ::  Not %fail (a snub), not even %stale (a demotion): silence, and
        ::  the peer's next retransmission gets a fresh look once whatever
        ::  we were missing has arrived.  See +unknown-checks in
        ::  lib/self-attestation for what qualifies and why.
        ::
        ::  ALL THREE OUTCOMES, IN ONE SWITCH OVER A CLOSED UNION.  This
        ::  used to be a chain of ?: whose final else-branch was the snub,
        ::  so anything the two predicates did not recognise -- a
        ::  +verify-lc early abort, a local refusal -- arrived at the
        ::  destructive outcome by DEFAULT.  A ?- over $verdict-class has
        ::  no default: a fourth outcome stops the compiler right here.
        ::
        ::  The class comes from the refusal when there was one (the
        ::  verdict itself passed every check, so it has nothing to say
        ::  about the peer), and otherwise from the failing checks.
        ::
        =/  class=verdict-class:sa
          ?^  refused  (refusal-class:lsa u.refused)
          (classify:lsa verdict.res)
        ?-    class
            %unknown
          %-  %-  slog
              ?^  refused
                :~  leaf+"%gw-btc: attestation for {(scow %p who)} passed, but we refused it locally ({<u.refused>}); emitting no verdict"
                    leaf+"  (that refusal is about OUR state, not the peer's evidence, so it"
                    leaf+"   is not something the peer can be blamed -- or snubbed -- for)"
                ==
              :~  leaf+"%gw-btc: attestation for {(scow %p who)} is UNDETERMINED; emitting no verdict"
                  leaf+"  (the [??] checks above could not be evaluated from what we can see;"
                  leaf+"   that is our ignorance, not the peer's fraud -- it will be retried)"
              ==
          `this
        ::
            %stale
          %-  %-  slog
              :~  leaf+"%gw-btc: attestation for {(scow %p who)} is STALE, not invalid"
                  leaf+"  (its identity sat has moved; demoting to alien, never snubbing)"
              ==
          :_  this
          ~[(stale-card dom.req who)]
        ::
            %fraud
          ::  A negative verdict is destructive: jael %fails and ames snubs,
          ::  stickily, which then blocks the packet that could correct it.
          ::  It has never announced itself -- Phase 6.7 found the snub
          ::  itself to be invisible in the logs, discoverable only through
          ::  .^(/snubbed) -- so say it here, at the one place that causes
          ::  it.
          ::
          =/  why=tape
            ?^  refused
              "its checks passed but the result was refused locally ({<u.refused>}), and that refusal is classed as fraud"
            "the [XX] checks above are fraud-class: evidence that was never true, not evidence that expired"
          %-  %-  slog
              :~  leaf+"%gw-btc: SNUBBING {(scow %p who)} on a negative %gw-btc verdict"
                  leaf+"  {why}"
                  leaf+"  (a snub is sticky and blocks the packet that would correct it;"
                  leaf+"   inspect with .^(/snubbed) and undo with %snub %deny %del)"
              ==
          :_  this
          ~[(verdict-card dom.req who ~)]
        ==
      ::  Sponsorship decision point.  A valid attestation whose snapshot
      ::  names US as sponsor IS the sponsorship request -- there is no
      ::  separate handshake and no consent signature.  Declining is
      ::  NOT a %fail: the attestation is valid, only the sponsorship
      ::  role is refused, so we stay silent (no verdict) rather than
      ::  emitting a negative one that would snub a legitimate ship.
      ::
      ::  Silence withholds the point, so the peer never becomes %known
      ::  here and we never acquire a lane for it -- which is exactly
      ::  what stops us relaying (+on-hear-forward forwards for any
      ::  ship we hold a lane to).  It also declines direct contact;
      ::  splitting those two would need a kernel no-relay gate, which
      ::  is deferred until a policy actually wants it.
      ::
      =/  claims-us=?
        ?&  has.sponsor.net.u.verified
            =(our.bowl who.sponsor.net.u.verified)
        ==
      ?:  ?&  claims-us
              ?=(%decline (sponsor-policy who life.net.u.verified))
          ==
        %-  (slog leaf+"%gw-btc: declining sponsorship of {<who>}" ~)
        `this(declined (~(put in declined) who))
      =/  applied  (apply-verified who u.verified tip-value.res)
      =.  urb-state  -.applied
      =.  confidential  +.applied
      =.  attested  (~(put by attested) who sont.own.u.verified)
      =?  sponsees  claims-us
        (~(put by sponsees) who [life.net.u.verified now.bowl])
      =?  declined  claims-us  (~(del in declined) who)
      ::  Operator visibility only -- an unroutable comet is legal and
      ::  its verdict is unaffected.
      %-  (slog (unroutable-point who verified))
      :_  this
      ~[(verdict-card dom.req who `(urb-point-to-jael u.verified who))]
    ==
  ::
      ::  An ON-CHAIN self-attestation finished verifying.
      ::
      ::    Same evidence, same +verify-lc, same single-flight slot as a
      ::    %jael-writ -- and three deliberate differences, all of which
      ::    come from the fact that NOBODY ASKED US:
      ::
      ::      - no verdict, ever.  A publication that does not verify is a
      ::        log line.  Jael has no writ outstanding for this ship, and
      ::        a %fail here would snub a peer on the strength of a
      ::        transaction anyone can pay to put in a block.
      ::      - no sponsorship decision.  Sponsorship is requested by
      ::        attesting TO the sponsor; a broadcast is not addressed to
      ::        anyone, so it cannot request anything.
      ::      - the ship comes out PUBLIC.  Building this transaction
      ::        needed the identity sat, so the publication IS the owner's
      ::        consent to declassify -- and it is irreversible.
      ::
      [%claim ship=@ job=@ ~]
    =/  who  (slav %p i.t.wire)
    =/  job  (slav %ud i.t.t.wire)
    =/  active  (~(get by inflight) who)
    ?~  active
      %-  %-  slog
          :~  leaf+"%gw-btc: discarding a publication result for {(scow %p who)} (job {<job>}): it holds no slot"
              leaf+"  (released by the ~h2 leak guard, or dropped by the block scanner;"
              leaf+"   nothing is installed, and a later publication re-runs it)"
          ==
      `this
    ?.  =(job job.u.active)
      %-  %-  slog
          :~  leaf+"%gw-btc: discarding a publication result for {(scow %p who)} (job {<job>}): job {<job.u.active>} holds the slot"
          ==
      `this
    =/  req=inflight-writ  u.active
    =.  inflight  (~(del by inflight) who)
    ?+    sign-arvo  (on-arvo:def wire sign-arvo)
        [%khan %arow *]
      ?.  -.p.sign-arvo
        ?>  ?=([%khan %arow %.n *] sign-arvo)
        %-  %-  slog
            %:  strand-death-report:lsa
                [%peer who job]
                -.p.p.sign-arvo
              +.p.p.sign-arvo
            ==
        `this
      ?>  ?=([%khan %arow %.y %noun *] sign-arvo)
      =/  [%khan %arow %.y %noun =vase]  sign-arvo
      =/  [res=result:sa tip-spk=hexb:bc]
        !<([result:sa hexb:bc] vase)
      %-  (slog (report:lsa verdict.res))
      =/  refused=(unit refusal:sa)  (local-refusal who req res)
      =/  verified=(unit point:urb)
        ?.  &(ok.verdict.res ?=(~ refused))  ~
        ?~  point.res  ~
        `u.point.res(pass.net pass.req)
      ?~  verified
        %-  %-  slog
            :~  leaf+"%gw-btc: the on-chain publication by {(scow %p who)} did not verify"
                ?^  refused
                  leaf+"  (it passed, and we refused it locally: {<u.refused>})"
                leaf+"  (see the checks above; no verdict is emitted and nobody is snubbed)"
            ==
        `this
      =/  applied  (apply-verified who u.verified tip-value.res)
      =.  urb-state  -.applied
      ::  +apply-verified files every verified identity as confidential,
      ::  which is right for a packet and exactly wrong for a broadcast.
      ::
      =.  confidential  (~(del in +.applied) who)
      =.  attested  (~(put by attested) who sont.own.u.verified)
      %-  %-  slog
          :~  leaf+"%gw-btc: {(scow %p who)} published a verified self-attestation on chain"
              leaf+"  (it is PUBLIC from here on -- that is what publishing means, and it does not undo)"
          ==
      %-  (slog (unroutable-point who verified))
      :_  this
      ::  Jael only receives udiffs for ships it has subscribed to; for
      ::  the rest, ask it to subscribe and answer the resulting /ship
      ::  watch from the index we just wrote (+on-watch).
      ::
      ?.  (~(has in (subs-to-ships sup.bowl)) who)
        ~[(listen-to-urb (silt ~[who]) [%| dap.bowl])]
      %-  jael-update
      %+  murn  (state-to-udiffs urb-state)
      |=  [=ship =udiff:point:jael]
      ^-  (unit [^ship udiff:point:jael])
      ?.(=(ship who) ~ `[ship udiff])
    ==
  ::
  ::  Our +get-blocks thread returned. Update
  ::  urb-state, emit udiffs to Jael and full
  ::  urb-state snapshots to /urb-state watchers,
  ::  and set a timer to run the thread again.
      [%blocks ~]
    ?+    sign-arvo  (on-arvo:def wire sign-arvo)
        [%khan %arow *]
      ?.  -.p.sign-arvo
        ?>  ?=([%khan %arow %.n *] sign-arvo)
        %-  (slog leaf+"%gw-btc: block thread failed, retrying" +.p.p.sign-arvo)
        :_  this
        :~  [%pass /timer %arvo %b %wait (add ~s30 now.bowl)]
        ==
      ?>  ?=([%khan %arow %.y %noun *] sign-arvo)
      =/  [%khan %arow %.y %noun =vase]  sign-arvo
      =/  block-result
        !<  
        [state:urb [(list [id:block:bitcoin effect:urb]) state:urb]]
        vase
      =/  base-state  -.block-result
      =/  fx-and-state  +.block-result
      ::  Jael is subscribed to %gw-btc to receive udiffs for some ships,
      ::  and it isn't subscribed yet for others. For the ones in fx it is, we 
      ::  send udiffs. For the ones it isn't subscribed to yet, we tell it to,
      ::  and it will hit ++on-agent to get the udiff afterwards.
      =/  fx-ships=(set ship)
        %.  -.fx-and-state
        |=  fx=(list [id:block:bitcoin effect:urb])
        ^-  (set ship)
        %-  silt
        %+  murn
          fx
        |=  [id:block:bitcoin eu=effect:urb]
        ^-  (unit ship)
        ::  ignore %dns, %insc, %xfer effects
        ?.  ?=(%point -.eu)
          ~
        `ship.eu
      ::
      =/  tracked-ships=(set ship)
        (subs-to-ships sup.bowl)
      ::
      ::  The publications this batch found.  The scanner does not judge
      ::  them and cannot: a custody log names transactions in blocks it
      ::  has already streamed past.  Each one is a complete
      ::  self-attestation (+process-publication:urb-core completed the
      ::  log with the carrying transaction), so it takes the SAME
      ::  +verify-lc job a %jael-writ takes, in the same single-flight
      ::  slot.  Nothing is indexed here and no ship declassifies here;
      ::  that happens when the verification answers, on /claim.
      ::
      =/  cj  (start-claims q.byk.bowl now.bowl -.fx-and-state)
      =.  inflight  jobs.cj
      =.  next-job  nxt.cj
      ::
      ::  Three-way merge block-derived custody with any verifier result that
      ::  landed since `base-state`.  A divergent double move is ambiguous;
      ::  discard the whole batch and rerun from the unchanged cursor rather
      ::  than mixing two incompatible histories.
      =/  merged
        (reconcile-block base-state urb-state.state +.fx-and-state confidential)
      ?~  merged
        ::  NB: the tang MUST be a list.  This read `(slog leaf+"...")` --
        ::  a bare tank -- which +slog walks as if it were the list,
        ::  printing nothing at all.  The branch below retries the batch
        ::  from an UNCHANGED cursor, so a bug that lands here loops
        ::  forever and, with the message swallowed, does it in total
        ::  silence: the agent looks alive and simply never indexes again.
        ::  Cause-neutral, because there are now several.  This line used
        ::  to assert "concurrent custody conflict", which is only one of
        ::  them -- a reorg-discarded batch printed its own accurate
        ::  reason and was then contradicted by this one, two lines later.
        ::  The reorg guards in +reconcile-block slog before returning ~;
        ::  the custody cases do not, so silence above means custody.
        %-  %-  slog
            :~  leaf+"%gw-btc: block batch discarded; retrying from the unchanged cursor"
                leaf+"  cursor={<num.block-id.urb-state>}"
                leaf+"  confidential={<confidential>}"
                leaf+"  (no reason logged above = a verifier result moved a tracked sat mid-batch)"
            ==
        ::  Schedule the batch for immediate retry from an UNCHANGED
        ::  cursor.  The claim jobs launched above are not retried with
        ::  it: they are self-attestations, judged against the chain by
        ::  the light client, and owe nothing to this index.
        ::
        :_  this
        %+  weld  cards.cj
        ^-  (list card)
        :~  [%pass /timer %arvo %b %wait now.bowl]
        ==
      =/  filtered-udiffs=udiffs:point:jael
        %+  murn
          (fx-to-udiffs -.fx-and-state)
        |=  [=ship =udiff:point:jael]
        ^-  (unit [^ship udiff:point:jael])
        ::  Ignore ships Jael has not subscribed to and all confidential
        ::  points. Declassification is deliberately not inferred per block.
        ?.  ?&  (~(has in tracked-ships) ship)
                !(~(has in confidential) ship)
            ==
          ~
        `[ship udiff]
      =/  new-urb-state  u.merged
      =/  old-block-id  block-id.urb-state.state
      ::  A confidential comet whose attested tip the scanner just saw
      ::  spent has gone stale: its committed state may have changed and
      ::  is unknown until it re-attests.  Drop our trust and tell Jael
      ::  (which demotes the peer to an alien, never a snub) so the
      ::  owner's next attestation re-verifies from scratch.
      =/  gone-stale=(set ship)  (detect-stale new-urb-state confidential attested)
      ::  the ONE way this agent un-knows an identity (+forget-points).
      ::  A reorg that orphans a point's evidence takes this same route,
      ::  from the %reorg-rollback branch of +on-agent.
      ::
      =/  forgot
        %:  forget-points
            new-urb-state  confidential  attested  inflight  gone-stale
        ==
      =.  urb-state     index.forgot
      =.  confidential  confidential.forgot
      =.  attested      attested.forgot
      =.  inflight      inflight.forgot
      =/  stale-cards=(list card)  (forget-cards dap.bowl gone-stale)
      :_  this
      %+  welp  cards.cj
      %+  welp  stale-cards
      %+  welp
        ?.  =(~ fx-ships)
          ::  don't send a %listen task for ships
          ::  that Jael is already subscribed to
          :~  %+  listen-to-urb
                (~(dif in fx-ships) tracked-ships)
              [%| dap.bowl]
          ==
        ~
      %+  welp
        (jael-update filtered-udiffs)
      %+  welp
        :~  (scan-again now.bowl urb-state best)
        ==
      ?:  =(num.block-id.urb-state num.old-block-id)
        ~
      :~  :*  %give  %fact  ~[/urb-state]  %urb-state
              !>((filter-snapshot urb-state confidential))
          ==
      ==
    ==
  ==
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  ?+    wire  (on-agent:def wire sign)
  ::  THE readiness signal for the confidential verifier: %bitcoin-client's
  ::  own answer to "am I caught up".  /is-synced is a PERSISTENT
  ::  subscription like /best-block -- it answers the watch immediately with
  ::  the current value and then gives a fact on every transition, and it
  ::  never kicks.  It must therefore never be read with +watch-one.
  ::
  ::  Any failure of this subscription leaves .synced %.n, which holds writs
  ::  instead of judging them.  That is the safe direction: a ship that
  ::  cannot tell whether it is synced has no business condemning anyone.
  ::
      [%is-synced ~]
    ?+    -.sign  (on-agent:def wire sign)
        %watch-ack
      ?~  p.sign  `this
      %-  (slog leaf+"%gw-btc: {<light-client-agent:lca>} /is-synced watch rejected" u.p.sign)
      :_  this(synced %.n)
      ~[[%pass /synced-retry %arvo %b %wait (add ~s30 now.bowl)]]
    ::
        %kick
      :_  this(synced %.n)
      ~[(watch-synced our.bowl)]
    ::
        %fact
      ::  %bitcoin-client names its fact marks after their path in its own
      ::  desk, mar/bitcoin-client/is-synced.hoon -- so the mark is
      ::  %bitcoin-client-is-synced, NOT %is-synced.  A mismatch here is
      ::  not a quiet no-op: the fact falls through to on-agent:def, which
      ::  crashes on an unexpected update, gall closes the subscription,
      ::  the %kick arm above re-watches, and the ship spins.  Measured at
      ::  136.539 iterations and 191MB of transcript in ~15 minutes on an
      ::  IDLE ship, against node@063720b9.
      ?.  ?=(%bitcoin-client-is-synced p.cage.sign)
        (on-agent:def wire sign)
      =/  syn  !<(is-synced:update:lc q.cage.sign)
      ?:  =(syn synced)  `this
      %-  %-  slog
          :_  ~
          :-  %leaf
          ?:  syn
            "%gw-btc: light client is SYNCED; confidential verification enabled"
          "%gw-btc: light client is NOT synced; holding all attestations (no verdicts)"
      =/  new  this(synced syn)
      ::  EPOCH AUTO-BOOTSTRAP.  The first time the client reports
      ::  synced on a chain that has reached the kelvin-9 epoch, a
      ::  VIRGIN index (cursor never moved, nothing indexed -- the same
      ::  evidence the %gw-index-from guard reads, so a bootstrap that
      ::  already happened, by poke or by this arm, refuses here too)
      ::  starts itself from +gw-epoch.  Without this every fresh ship
      ::  held an empty public index until an operator poked it, and
      ::  could not resolve even the sponsor its own snapshot names.
      ::
      =/  virgin=?
        ?&  syn
            !indexing
            =(0 num.block-id.urb-state)
            =(~ unv-ids.urb-state)
        ==
      ?.  ?&(virgin ?=(^ best) (gte num.u.best gw-epoch))
        `new
      %-  %-  slog  :_  ~
          leaf+"%gw-btc: public index: auto-bootstrap from the kelvin-9 epoch, block {<gw-epoch>}"
      :_  %=  new
            urb-state  [[0x0 (dec gw-epoch)] *sont-map:ord *insc-ids:ord *unv-ids:urb]
            indexing   %.y
          ==
      ~[[%pass /timer %arvo %b %wait now.bowl]]
    ==
  ::
      [%best-block ~]
    ?+    -.sign  (on-agent:def wire sign)
        %watch-ack
      ?~  p.sign  `this
      %-  (slog leaf+"%gw-btc: {<light-client-agent:lca>} /best-block watch rejected" u.p.sign)
      :_  this(best ~)
      :~  [%pass /lc-retry %arvo %b %wait (add ~s30 now.bowl)]
      ==
    ::
        %kick
      :_  this(best ~)
      :~  [%pass /best-block %agent [our.bowl light-client-agent:lca] %watch /best-block]
      ==
    ::
        %fact
      ::  The node's %bitcoin-client gives /best-block facts under its own
      ::  fact mark, carrying best-block:update (a %new, or a
      ::  %reorg-rollback naming the fork point and the losing branch).
      ::
      ::  That mark is %bitcoin-client-best-block, named for its path in
      ::  node's desk (mar/bitcoin-client/best-block.hoon).  It read
      ::  %best-block until node@063720b9 -- the very ref we pin for
      ::  .stale-branch -- moved every mark under mar/bitcoin-client/ and
      ::  renamed all of them with it.  See the +is-synced arm above for
      ::  what a mismatch costs; the two desks still COMPILE independently,
      ::  which is why CI cannot see this and a live ship must.
      ?.  ?=(%bitcoin-client-best-block p.cage.sign)
        (on-agent:def wire sign)
      =/  upd  !<(best-block:update:lc q.cage.sign)
      ?:  ?=(%new -.upd)
        `this(best ``id:block:bc`[block-hash.upd block-height.upd])
      ?>  ?=(%reorg-rollback -.upd)
      ::  A REORG.  Until 2026-08-06 this arm was byte-identical to %new:
      ::  only .best moved, the scan cursor never rewound, and every fact
      ::  indexed out of an orphaned block stayed in .unv-ids forever while
      ::  the winning chain's replacements were skipped -- all silently.
      ::  With block-confirmations = 1 a single-block reorg, several a month
      ::  on mainnet, is enough to reach it.  From then until now the
      ::  scanner HALTED here instead, because a $point recorded no
      ::  provenance and %reorg-rollback named no losers, so there was
      ::  nothing to filter and no way to tell a good fact from a bad one.
      ::
      ::  Both halves are here now.  A $point carries .seen, the block it
      ::  was most recently observed in (sur/urb), and gwbtc/node@063720b9
      ::  made %reorg-rollback carry .stale-branch, the orphaned blocks.
      ::  So the index is REPAIRED rather than frozen, and the halt is
      ::  gone -- it existed only for want of this list.
      ::
      ::  .last-common is the FORK POINT, not the new tip: the light
      ::  client grafts the winning branch on and then gives an ordinary
      ::  %new for each of its blocks, which arrive immediately after this
      ::  and carry .best up.  So .best goes to the fork point here, which
      ::  is the highest block we know is on the main chain right now.
      ::
      =/  fork=id:block:bc
        [block-hash.last-common.upd block-height.last-common.upd]
      ::  Rollback ABOVE our cursor: every orphaned block is one the
      ::  scanner had not reached, so nothing we hold came from them and
      ::  there is nothing to forget or rewind.
      ::
      ?:  (gth block-height.last-common.upd num.block-id.urb-state)
        %-  %-  slog
            :~  leaf+"%gw-btc: chain reorg to {<block-height.last-common.upd>}, above our cursor {<num.block-id.urb-state>}"
                leaf+"  ({<(lent stale-branch.upd)>} blocks orphaned; nothing we have indexed came from them, continuing)"
            ==
        `this(best `fork)
      ::  Rollback AT OR BELOW our cursor.  Three things, in order:
      ::
      ::    SELECT.  +orphaned-points names the points whose most recent
      ::    evidence was in a block that did not happen.  A point with no
      ::    provenance (.seen=~) is NOT among them: ~ means "we cannot
      ::    tell", which is not "orphaned", and an unevaluable condition
      ::    never produces a negative outcome here.  See +orphaned-points.
      ::
      ::    FORGET.  The same +forget-points / +forget-cards the %stale
      ::    path uses, so the two roads out of "we no longer know this
      ::    identity" are one road: the peer drops to a fresh %alien and
      ::    is asked to re-attest over %sybl, keeping our lane.  NEVER a
      ::    snub -- a reorg is not fraud, and a snub is permanent on every
      ::    transport (b0a8e962ff), so it would block the very packet that
      ::    would correct us.
      ::
      ::    REWIND.  .block-id is the LAST block scanned, so setting it to
      ::    .last-common makes the scanner resume at the first block of
      ::    the winning branch.  The range is then walked normally, on the
      ::    timer chain that is already running -- no card is emitted to
      ::    hurry it.  A /timer re-arms itself exactly once per tick
      ::    (+scan-again), so injecting a second %wait here would fork the
      ::    chain in two and leave the scanner running at double rate
      ::    forever.  The rewind lands on the next tick, within ~s30.
      ::
      =/  orphans=(set hax:block:bc)
        %-  silt
        %+  turn  stale-branch.upd
        |=([* haz=@ux] `hax:block:bc`haz)
      =/  gone=(set ship)  (orphaned-points:uc urb-state orphans)
      =/  forgot
        (forget-points urb-state confidential attested inflight gone)
      =.  urb-state     index.forgot
      =.  confidential  confidential.forgot
      =.  attested      attested.forgot
      =.  inflight      inflight.forgot
      =.  block-id.urb-state  fork
      %-  %-  slog
          :~  leaf+"%gw-btc: chain reorg to {<block-height.last-common.upd>}, at or below our cursor"
              leaf+"  ({<(lent stale-branch.upd)>} blocks orphaned; forgot {<~(wyt in gone)>} points, rescanning from {<+(block-height.last-common.upd)>})"
          ==
      :_  this(best `fork)
      (forget-cards dap.bowl gone)
    ==
  ==
++  on-leave  on-leave:def
++  on-fail   on-fail:def
--
::
|%
::  +stuck-job-guard: RESOURCE-LEAK BACKSTOP -- NOT a verification deadline
::
::    Verification has no wall-clock policy deadline, deliberately.  It is
::    fully asynchronous (the peer sits as an %alien until a verdict
::    arrives; nothing in the kernel blocks on it), and its cost is
::    dominated by the BIP-158 filter scan, which is O(blocks since the
::    comet last moved its sat).  A fixed deadline would therefore make
::    long-dormant comets arbitrarily unverifiable as a function of a magic
::    number, silently converting "slow" into "no verdict".  A
::    slow-but-progressing verification is allowed to take as long as it
::    takes.
::
::    This constant exists only so a job that dies SILENTLY -- a strand
::    wedged on a subscription that never answers, say -- cannot hold a
::    ship's single-flight `inflight` slot forever and make that ship
::    permanently unverifiable.  When it fires it releases the slot and
::    lets Khan tear the thread (and its light-client watches) down.  It
::    emits NO verdict, ever: a stuck job is infrastructure failure, never
::    evidence about the peer.  Hence: absurdly generous.
::
++  stuck-job-guard  ~h2
::
::  +watch-synced: subscribe to the light client's own readiness signal
::
::    Issued by +on-init and by the two migration branches of +on-load.  It
::    must be exactly one card in exactly those places: gall keeps outgoing
::    subscriptions across an upgrade, so re-issuing it on every load would
::    stack duplicates, and never issuing it on an upgrade would leave
::    .synced permanently %.n on a ship that was reinstalled rather than
::    rebooted (which holds every attestation forever -- safe, but dead).
::
++  watch-synced
  |=  our=@p
  ^-  card
  [%pass /is-synced %agent [our light-client-agent:lca] %watch /is-synced]
::
::  +refresh-synced: re-read the light client's sync state, on demand
::
::    /is-synced answers the WATCH immediately with the current value and
::    then gives a fact only on the node's own transitions -- and that emit
::    coverage is INCOMPLETE.  %bitcoin-client announces losing its last
::    peer (+have-live-peers goes false, so +is-fully-synced does too), but
::    nothing announces the recovery: a peer reconnecting does not emit,
::    and the headers/cfheaders handlers only emit when they process a
::    batch.  A node that was already at the tip therefore never says so
::    again.
::
::    Observed live on mainnet, 2026-08-06, minutes after the gate landed:
::    %bitcoin-client reported `is-synced %.y` with 72 live peers and
::    filter headers at 961.280 while %gw-btc still held .synced %.n --
::    and would have gone on holding every attestation indefinitely.  That
::    is fail-closed, and it is also dead, and trading a false INVALID for
::    a permanent silence is not a fix.
::
::    A fresh subscription's initial fact is the reliable read, so drop and
::    re-establish it.  Gall processes a %leave and a %watch on the same
::    wire in order within one event (+ap-move's move loop deletes from
::    .boat before the %watch checks it), so this is one card pair and no
::    window.
::
::    Driven by DEMAND rather than by a timer: it runs exactly when a writ
::    or a %anew was held for lack of readiness, i.e. exactly when the
::    answer matters, and a retransmitting peer is the poll.  A quiet ship
::    never asks, and does not need to.
::
++  refresh-synced
  |=  our=@p
  ^-  (list card)
  :~  [%pass /is-synced %agent [our light-client-agent:lca] %leave ~]
      (watch-synced our)
  ==
::
::  +max-custody-log: the longest custody log this desk will walk
::
::    A denial-of-service bound, not a protocol limit on identity age: a
::    log is O(n) light-client fetches and n is chosen by whoever built
::    the pass.  Shared by the %jael-writ gate and +begin-anew so a log we
::    would refuse from a peer is also one we refuse to publish ourselves.
::
++  max-custody-log  1.024
::
::  +drop-writ: dispose of a %jael-writ, out loud
::
::    THE fix for test 6.7 on the peer path.  Twelve dispositions reach
::    this agent before a verification is ever launched; nine announced
::    themselves and three did not, and all three of the silent ones
::    emitted a NEGATIVE verdict -- a sticky ames snub -- with no line in
::    the log at all.  From outside, a ship snubbing every peer looked
::    exactly like a ship nobody was talking to.
::
::    So there is now one exit, and it derives the log line and the cards
::    from the SAME $writ-drop: +writ-drop-report:lsa says what happened
::    and +writ-drop-fate:lsa says what it costs.  Neither can be skipped
::    and the two cannot contradict each other, and both are ?- over a
::    closed union, so a twelfth disposition is a compile error rather
::    than a silent drop.  (Same shape as +report / +check-class for the
::    post-verification outcomes.)
::
++  drop-writ
  |=  [our=@p dom=@tas who=ship drop=writ-drop:sa]
  ^-  (list card)
  %-  (slog (writ-drop-report:lsa who drop))
  ?-  (writ-drop-fate:lsa drop)
    %drop     ~
    %hold     ~
    %refresh  (refresh-synced our)
    %fail     ~[(verdict-card dom who ~)]
  ==
::
::  +refuse-anew: decline to refresh OUR OWN pass, out loud
::
::    The %anew counterpart of +drop-writ, and the same discipline: the
::    reason is a $anew-refusal, the line comes from
::    +anew-refusal-report:lsa and the cards from +anew-refusal-fate:lsa,
::    so a tenth refusal cannot be added without a line.  Nothing here is
::    a finding about anybody, so the only card any of them can emit is
::    the light-client re-read.
::
++  refuse-anew
  |=  [our=@p ref=anew-refusal:sa]
  ^-  (list card)
  %-  (slog (anew-refusal-report:lsa our ref))
  ?-  (anew-refusal-fate:lsa ref)
    %drop     ~
    %refresh  (refresh-synced our)
  ==
::
::  +log-top-height: the highest chain height a custody log's evidence needs
::
::    The spawn transaction's block, plus every entry's block.  Below this
::    our view of the chain simply does not reach the evidence, so no check
::    over it can mean anything -- +fetch-tx-at would be asking the node for
::    a block it does not have, and +scan-liveness would be handed a range
::    that runs backwards and (before this commit) called it clean.
::
::    Cheap, and derived from the writ itself rather than from a global
::    notion of readiness, which is why it is worth having ALONGSIDE
::    .synced rather than instead of it.
::
++  log-top-height
  |=  sat=self-attestation:sa
  ^-  @ud
  =/  entries  chain.sat
  =/  top=@ud
    ?~  open=(spawn-of:lsa chain.sat)  0
    start-height.u.open
  |-  ^-  @ud
  ?~  entries  top
  $(entries t.entries, top (max top height.i.entries))
::
::
::  Hard-coded initial sync state used if
::  %urb-start-indexing receives a null snapshot
++  default-urb-state
  ^-  state:urb
  =/  start-height  943.140
  =/  start-hash    0x1.62b3.04e4.d48c.3a53.d80a.96de.0210.d325.c0a9.a464.8b3c
  :*  [start-hash start-height]
      *sont-map:ord
      *insc-ids:ord
      *unv-ids:urb
  ==
::
::  +block-confirmations: blocks behind the tip the scanner stays
++  block-confirmations  1  :: 1 for alpha
::
::  +gw-epoch: the first mainnet block that can contain a kelvin-9
::  %gw-btc publication -- the block of the first one ever minted (the
::  ~barmul sponsor spawn, tx 48c2ea65..., block 963.104).  A virgin
::  index that starts here provably misses nothing, so the agent
::  bootstraps itself from it the first time the light client reports
::  synced (see the /is-synced %fact arm) and no operator poke is needed.
::  Found live: a fresh comet could not resolve its own SPONSOR -- and a
::  sponsor could not resolve ITSELF ([??] sponsor-known, UNDETERMINED
::  forever) -- because nothing ever started the scanner.
::
::  On a chain that has not reached the epoch (regtest, the test
::  harness) the auto-bootstrap simply never fires and %gw-index-from
::  remains the way to choose a start, exactly as before.
::
++  gw-epoch  963.104
::
::  +scan-batch: most blocks one run of the block thread will process
::
::    A batch is all-or-nothing: the cursor advances only when the thread
::    returns, so a failed fetch or a rejected +reconcile-block discards
::    every block in it.  A full mainnet block costs tens of seconds to
::    fetch on a small node, which makes this a bound on MINUTES of lost
::    work, not just on event size.  When a run stops short of the tip,
::    +scan-again re-arms immediately rather than idling for the poll
::    interval, so a small batch costs nothing in throughput.
::
++  scan-batch  25
::
::  +block-fetch-timeout: how long one /block/height/<h> fetch may take
::
::    A light-client watch has no timeout of its own: if the node never
::    answers (no peer will serve the block, the sidecar died, ...) the
::    strand waits forever, and because the block timer is only re-armed
::    when the thread RETURNS, the public scanner would stop permanently
::    with no error and no retry.  The confidential verifier is covered
::    against the same hazard by +stuck-job-guard; this is the scanner's
::    equivalent.  Bounding the individual fetch rather than the batch
::    keeps a legitimately slow +scan-batch run from being killed.
::
::    On expiry the strand fails, +on-arvo's %blocks failure branch logs
::    and re-arms in 30s, and the batch is simply rescanned -- the cursor
::    only advances on a thread that returned.
::
++  block-fetch-timeout  ~m5
::
::  +scan-again: re-arm the block timer
::
::    Immediately if the scanner is still behind the settled tip (a batch
::    ended on the +scan-batch bound), otherwise at the normal poll
::    interval.
::
++  scan-again
  |=  [now=@da st=state:urb tip=(unit id:block:bc)]
  ^-  card
  =/  soon=?
    ?~  tip  |
    ?:  (lth num.u.tip block-confirmations)  |
    (lth num.block-id.st (sub num.u.tip block-confirmations))
  [%pass /timer %arvo %b %wait ?:(soon now (add ~s30 now))]
::
::  +get-blocks: index range(last-processed + 1, tip - block-confirmations)
::
::    Every block comes whole from the local light client's
::    /block/height/<h> endpoint -- no Bitcoin Core, no getblockhash, no
::    getrawtransaction, and therefore no -txindex.  .best-height is the
::    chain tip OUR OWN persistent /best-block subscription reported; the
::    scanner must not read that endpoint itself (it never kicks, so a
::    +watch-one on it hangs forever).
::
::    Under the OP_RETURN revision no per-transaction fetch is needed at
::    all: a public identity is spawned and updated in single transactions
::    whose sat-carrying output commits the state and whose OP_RETURN
::    output opens it, and +find-block-reveals takes every input value it
::    needs from the sont index it already holds.  (The old RPC path still
::    carried a prevout-fetch loop for this; it had been dead code since
::    the OP_RETURN revision, because +find-block-reveals records a value
::    for every input of every saved tx.)
::
++  get-blocks
  |=  [urb-state=state:urb best-height=@ud]
  ^-  shed:khan
  =/  uc
    %-  abed:urb-core:uc
    urb-state
  =/  m  (strand:strandio ,vase)
  ^-  form:m
  ;<  our=@p  bind:m  get-our:strandio
  ?:  (lth best-height block-confirmations)
    (pure:m !>([urb-state [fx state]:uc]))
  =/  last-settled-block  (sub best-height block-confirmations)
  =/  stop
    (min last-settled-block (add num.block-id.urb-state scan-batch))
  =/  from  +(num.block-id.urb-state)
  =/  i  from
  |-
  ^-  form:m
  ?.  (lte i stop)
    ?:  =(i from)  (pure:m !>([urb-state [fx state]:uc]))
    ~&  >  [%gw-btc-scanned from=from to=(dec i) settled-tip=last-settled-block]
    (pure:m !>([urb-state [fx state]:uc]))
  ;<  =block:bitcoin  bind:m
    %+  (set-timeout:strandio ,block:bitcoin)  block-fetch-timeout
    (fetch-block-at:lca our i)
  ::  Filter the block to urb-relevant txs, fill in the input values we
  ::  already track, and run the OP_RETURN scanner over the result.
  =/  revs-and-block  (find-block-reveals:uc block)
  =.  uc  uc(hax.block-id.state hax.block)
  =.  uc
    %-  handle-block:uc
    (apply-prevouts-and-urbify:uc +.revs-and-block -.revs-and-block)
  $(i +(i))
::
::  Confidential-comet verification helpers.
::
::  Decode the immutable %gw-btc domain from dat and the mutable custody
::  log from xtr.  Malformed or non-canonical xtr is rejected by
::  +from-xtr before any asynchronous fetches are launched.
++  pass-attestation
  |=  [dom=@tas who=ship =pass]
  ^-  (unit self-attestation:sa)
  ?.  =(dom domain:cc)  ~
  =/  meta  (parse-pass:cc pass)
  ?~  meta  ~
  ?.  =(dom dom.u.meta)  ~
  ?.  =(kelvin:cc kel.u.meta)  ~
  ?.  =(who fig:ex:(com:nu:cric:crypto pass))  ~
  (from-xtr:lsa who pass)
::
::  Recognize the public onboarder's intentionally absent xtr tail.  This is
::  narrower than an empty decoded custody log: `(jam ~)` is a present but
::  invalid confidential attestation and must receive a negative verdict.
::  The spawn satpoint in dat is not checked against the chain here, so
::  this only confirms a well-formed suite-C %gw-btc pass with no xtr; the
::  block scanner resolves the actual on-chain spawn.
++  public-pass
  |=  [dom=@tas who=ship =pass]
  ^-  ?
  ?.  =(dom domain:cc)  %.n
  =/  meta  (parse-pass:cc pass)
  ?~  meta  %.n
  =/  cic  (com:nu:cric:crypto pass)
  ?&  =(dom dom.u.meta)
      =(kelvin:cc kel.u.meta)
      =(0 xtr.u.meta)
      ?=(%c suite.+<.cic)
      =(who fig:ex:cic)
  ==
::
::  Recognize a well-formed %gw-btc pass minted under a protocol kelvin
::  that is not ours.  We have no way to check such a pass and no business
::  condemning it: a negative verdict here becomes a Jael %fail and an Ames
::  snub, so across a kelvin bump every old ship would blacklist every new
::  one and vice versa.  Treat it exactly like the public-onboarding packet
::  -- silence, no verdict.  The comet's OWN kelvin is plaintext in dat by
::  design precisely so this is decidable without an opening.
++  foreign-kelvin
  |=  [dom=@tas who=ship =pass]
  ^-  ?
  ?.  =(dom domain:cc)  %.n
  =/  meta  (parse-pass:cc pass)
  ?~  meta  %.n
  ?&  =(dom dom.u.meta)
      !=(kelvin:cc kel.u.meta)
  ==
::
::  Launch the single light-client-backed verification thread for a ship,
::  with a resource-leak backstop (see +stuck-job-guard).  The block watcher
::  state supplies the previously tracked tip and the set of known-public
::  ships used for the sponsor-existence check; .best-height is the chain
::  tip our own /best-block subscription last reported.
::    .kin says which road the attestation arrived by, and is the ONLY
::    difference between the two: %verify for a %jael-writ, %claim for an
::    OP_RETURN publication.  It picks the answer wire, and the answer
::    wire picks the arm in +on-arvo that decides what an answer is worth
::    -- a verdict, or a log line.  The work itself is identical.
::
++  verify-cards
  |=  $:  kin=?(%verify %claim)
          byk=desk
          now=@da
          who=ship
          req=inflight-writ
          best-height=@ud
      ==
  ^-  (list card)
  =/  wir  /(scot %p who)/(scot %ud job.req)
  =/  gud=@tas  ?:(?=(%verify kin) %stuck-job %claim-stuck)
  :~  :*  %pass  [kin wir]  %arvo  %k
          %lard  byk
          %+  (set-timeout:strandio ,vase)  stuck-job-guard
          %:  verify-lc:lca
              sat.req
              (tracked-anchor urb-state who)
              ~(key by unv-ids.urb-state)
              best-height
          ==
      ==
      :*  %pass  [gud wir]
          %arvo  %b  %wait  (add now stuck-job-guard)
      ==
  ==
::  +start-claims: launch a verification for each publication in a batch
::
::    The %claim effects a block batch produced, turned into jobs.  Every
::    refusal here is announced for the same reason every %jael-writ
::    refusal is: an operator paid a miner to put this on chain and will
::    otherwise never learn why their comet did not appear.
::
::    .best-height is the height of the block the publication was FOUND
::    in, not the chain tip.  That makes +scan-liveness examine exactly
::    one block -- the publication's own -- which is the honest question
::    to ask here: everything above that block is the scanner's own job,
::    and it will see the sat move itself.  Handing it the real tip would
::    make every publication found during a historical backfill trigger a
::    filter scan across every block since.
::
++  start-claims
  |=  [byk=desk now=@da fx=(list [id:block:bc effect:urb])]
  ^-  [cards=(list card) jobs=(map ship inflight-writ) nxt=@ud]
  =/  jobs  inflight
  =/  nxt   next-job
  =|  cards=(list card)
  |-
  ^-  [(list card) (map ship inflight-writ) @ud]
  ?~  fx  [cards jobs nxt]
  =*  eu  +.i.fx
  ?.  ?=([%claim *] eu)
    $(fx t.fx)
  =/  who=ship  who.eu
  =/  height=@ud  num.-.i.fx
  =*  skip  $(fx t.fx)
  ?:  (~(has by jobs) who)
    %-  %-  slog
        :~  leaf+"%gw-btc: publication by {(scow %p who)} dropped: a verification already holds its slot"
        ==
    skip
  ?~  sat=(pass-attestation domain:cc who pass.eu)
    %-  %-  slog
        :~  leaf+"%gw-btc: publication by {(scow %p who)} dropped: its completed pass does not decode"
        ==
    skip
  ?~  chain.u.sat
    %-  (slog leaf+"%gw-btc: publication by {(scow %p who)} dropped: empty custody log" ~)
    skip
  ?:  (gth (lent chain.u.sat) max-custody-log)
    %-  %-  slog
        :~  leaf+"%gw-btc: publication by {(scow %p who)} dropped: custody log of {<(lent chain.u.sat)>} exceeds {<max-custody-log>}"
        ==
    skip
  ?.  synced
    %-  %-  slog
        :~  leaf+"%gw-btc: publication by {(scow %p who)} held: the light client is not synced"
            leaf+"  (nothing is judged from a chain we have not seen; rescan to retry)"
        ==
    skip
  =/  req=inflight-writ  [domain:cc pass.eu u.sat nxt]
  %=  $
    fx     t.fx
    jobs   (~(put by jobs) who req)
    nxt    +(nxt)
    cards  (weld cards (verify-cards %claim byk now who req height))
  ==
::
::  A verifier result may carry a pass reconstructed from the latest
::  committed Groundwire state.  Its cryptographic key must equal the pass
::  Jael forwarded, but xtr is intentionally allowed to differ.
++  attested-point-ok
  |=  [submitted=pass verified=point:urb]
  ^-  ?
  (same-key:cc submitted pass.net.verified)
::
::  +local-refusal: why we decline a result that PASSED every check
::
::    ~ means nothing local objects.  Otherwise the named $refusal, whose
::    class +refusal-class:lsa decides -- and today decides is %unknown for
::    every one of them, because none is a finding about the peer.  These
::    conditions used to be inlined as `?. ... ~` in the +verified
::    computation, which erased WHICH one fired and dropped the result into
::    the same branch as fraud.  Naming them is what makes the outcome
::    classifiable, and what puts the reason in the operator's log.
::
::    A verdict that did NOT pass is not a local refusal: it is classified
::    from its own failing checks, so this answers ~ for it.
::
++  local-refusal
  |=  [who=ship req=inflight-writ res=result:sa]
  ^-  (unit refusal:sa)
  ?.  ok.verdict.res  ~
  ::  the three below are internal-consistency guards: each is unreachable
  ::  unless this desk contradicts itself (see +refusal-class:lsa), so if
  ::  one ever fires it is a bug report, not evidence about the peer.
  ::
  ?.  =(who who.verdict.res)                             `%who-mismatch
  ?~  point.res                                          `%no-point
  ?.  (attested-point-ok pass.req u.point.res)           `%pass-mismatch
  ::  ... and this one is reachable: our own sat index already attributes
  ::  the proven tip to another comet.  We refuse the point, and we refuse
  ::  to snub, because our index is a lagging window on the chain.
  ::
  ?.  (tip-owner-ok urb-state who sont.own.u.point.res)   `%tip-owned
  ~
::
::  Confidential comets whose attested tip the scanner just saw move.  A
::  moved identity sat means the committed state may have changed and is
::  unknown until the owner re-attests, so the attestation is stale.
++  detect-stale
  |=  [st=state:urb conf=(set ship) ats=(map ship sont:ord)]
  ^-  (set ship)
  %-  silt
  %+  murn  ~(tap by ats)
  |=  [who=ship tip=sont:ord]
  ^-  (unit ship)
  ?.  (~(has in conf) who)  ~
  ?~  pt=(~(get by unv-ids.st) who)  ~
  ?:  =(tip sont.own.u.pt)  ~
  `who
::
::  Tell Jael a confidential comet's verified attestation went stale.  Jael
::  drops the point and Ames demotes the peer to an alien (never a snub).
++  stale-card
  |=  [dom=@tas who=ship]
  ^-  card
  :*  %give  %fact  ~[/writs]
      %stale-notice  !>(`stale-notice:jael`[dom who])
  ==
::
::  ----------------------------------------------------------------
::  FORGETTING A POINT: the one way this agent un-knows an identity
::  ----------------------------------------------------------------
::
::  Two things reach it.  A moved identity sat (+detect-stale, above) is
::  one; a chain REORGANISATION that orphans the block a point was last
::  observed in is the other (+orphaned-points:uc, in lib/urb-core).
::  Neither is fraud, so both take the same route and neither may ever
::  emit a %verdict:
::
::    - our own indexes lose the point (+forget-points), so nothing we
::      hold still claims to know this identity's state;
::    - jael is told (+forget-cards -> +stale-card), which drops ITS point
::      and demotes the peer to a fresh %alien.  The peer keeps our lane,
::      re-attests of its own accord through the ordinary %sybl/%writ
::      path, and a positive verdict promotes it again.
::
::  A snub would do the opposite of all of that: it is permanent on every
::  transport (b0a8e962ff) and it blocks the very packet that would
::  correct us.  Spending one on a chain event would need an operator to
::  undo.  A reorg is not fraud.
::
++  forget-cards
  |=  [dom=@tas ships=(set ship)]
  ^-  (list card)
  %+  turn  ~(tap in ships)
  |=(=ship (stale-card dom ship))
::
::  +forget-points: drop every index entry these ships own
::
::    The four stores that together constitute "we know this identity":
::    the public/private point index and its sat index (.urb-state), the
::    confidential registry, the tip each peer last attested to, and any
::    verification job in flight for it (which would otherwise land on a
::    slot describing a point that no longer exists).
::
++  forget-points
  |=  $:  st=state:urb
          conf=(set ship)
          ats=(map ship sont:ord)
          jobs=(map ship inflight-writ)
          ships=(set ship)
      ==
  ^-  $:  index=state:urb
          confidential=(set ship)
          attested=(map ship sont:ord)
          inflight=(map ship inflight-writ)
      ==
  :^    (drop-private-insertions st ships)
      (~(dif in conf) ships)
    (drop-attested ats ships)
  (drop-inflight jobs ships)
::
::  Which points a reorg took the evidence for is +orphaned-points:uc, in
::  lib/urb-core: it is a pure query over the index and belongs beside the
::  rest of the index arithmetic.  The %reorg-rollback branch of +on-agent
::  is its one caller, and it hands the result straight to the two arms
::  above -- which is the whole of the reorg repair.
::
::  ----------------------------------------------------------------
::  %anew: extending OUR OWN custody log, in band
::  ----------------------------------------------------------------
::
::  +own-pass: our own current pass, straight from jael
::
::    NOT from the poke.  The poke supplies only chain evidence; the key
::    material comes from the ring jael holds for our CURRENT life, so a
::    kernel %rekey (which installs a new ring at a new life) is picked
::    up automatically and the refreshed pass carries the rotated
::    messaging key as well as the extended log.
::
::    Scries cannot be run inside +mole (mule's scry gate blocks
::    everything), so every branch here has to be one jael always
::    answers:
::
::      - the %pawn guard keeps us off ships with no confidential
::        identity at all, including the fake galaxies the test harness
::        runs on (where the door would otherwise scry a comet's deed).
::      - %life for our own ship always answers.
::      - %deed for our own %pawn at life 1 always answers -- and on a
::        fake ship answers with a suite-%b pass, which +with-xtr then
::        rejects.  Silence, not a crash.
::
++  own-pass
  |=  [our=@p now=@da]
  ^-  (unit pass)
  ?.  ?=(%pawn (clan:title our))  ~
  =/  lyf=life
    .^(life %j /(scot %p our)/life/(scot %da now)/(scot %p our))
  ?:  =(1 lyf)
    =/  ded
      .^  [=life =pass ded=(unit @ux)]  %j
          /(scot %p our)/deed/(scot %da now)/(scot %p our)/(scot %ud lyf)
      ==
    `pass.ded
  ::  after a kernel %rekey our life advances and the deed endpoint stops
  ::  answering for a pawn; the vault ring is then the only source.
  =/  rig=ring
    .^(ring %j /(scot %p our)/vein/(scot %da now)/(scot %ud lyf))
  `pub:ex:(nol:nu:cric:crypto rig)
::
::  +base-chain: the custody log a new entry extends
::
::    Our stored (validated) log if we have one, else the log Causeway
::    baked into the boot feed's xtr.  ~ for a comet that booted with a
::    plain feed -- in which case an ingested SPAWN entry starts the log
::    from nothing, which is exactly the "finalize after boot" path.
::
++  base-chain
  |=  [our=@p now=@da]
  ^-  custody-log:sa
  ?^  chain.own  chain.own
  =/  base  (own-pass our now)
  ?~  base  ~
  =/  sat  (from-xtr:lsa our u.base)
  ?~  sat  ~
  chain.u.sat
::
::  +begin-anew: validate a candidate custody log for OUR OWN comet
::
::    Everything the poke claims is re-derived from the chain by the SAME
::    +verify-lc a peer's attestation goes through -- see +anew-cards
::    below, +verify-lc in lib/lc-attestation, and ++run-checks in
::    lib/self-attestation for the full list.  Fetches are addressed by
::    [height txid] through %light-client, so an entry naming a
::    transaction that is not in the block it claims cannot even be
::    fetched, let alone accepted.
::
::    Nothing is stored and nothing is emitted until the verdict lands
::    and is positive: a failed validation leaves the previous log in
::    place and answers with silence.
::
++  begin-anew
  |=  [byk=desk our=@p now=@da cand=custody-log:sa]
  ^-  (quip card gw-state)
  ::  SEVEN silent refusals used to live here, and Phase 7.2 had to
  ::  eliminate six of them from outside, by re-deriving each precondition
  ::  against the ship's own libraries, before it could conclude the
  ::  seventh (a stranded .pending slot) was the real one.  Every one of
  ::  them now says which it was, through +refuse-anew: the reason is a
  ::  $anew-refusal, and its line and its cards come from one place, so a
  ::  tenth refusal does not compile until it has both.  See also
  ::  /x/pending-own.
  ::
  ::  single-flight, exactly as for peer verification
  =/  pend  pending.own
  ?^  pend
    [(refuse-anew our %in-flight job.u.pend) state]
  ?~  cand
    [(refuse-anew our %no-log ~) state]
  ?:  (gth (lent cand) max-custody-log)
    [(refuse-anew our %log-too-long (lent cand) max-custody-log) state]
  ::  readiness is infrastructure, never evidence -- and the same
  ::  correction as the %jael-writ gate applies here: `?~ best` passes on
  ::  the genesis block %bitcoin-client answers a fresh subscription with.
  ::  Publishing a pass validated against a chain we have not seen would
  ::  install it in ames for every peer to reject.
  =/  tip  best
  ?~  tip
    [(refuse-anew our %no-tip ~) state]
  ?.  synced
    [(refuse-anew our %unsynced num.u.tip) state]
  =/  need=@ud  (log-top-height [our *pass cand])
  ?.  (gte num.u.tip need)
    [(refuse-anew our %tip-below-log num.u.tip need) state]
  =/  base  (own-pass our now)
  ?~  base
    [(refuse-anew our %no-pass ~) state]
  =/  pas  (with-xtr:cc u.base (jam cand))
  ?~  pas
    [(refuse-anew our %encode-failed ~) state]
  ::  the pass we are about to publish must still hash to our name.  the
  ::  tweak is immutable, so this can only fail if +with-xtr or the ring
  ::  ever drifts from the kernel's encoder -- but publishing a pass that
  ::  is not ours would be catastrophic, so check it anyway.
  ?.  =(our `@p`fig:ex:(com:nu:cric:crypto u.pas))
    [(refuse-anew our %name-mismatch ~) state]
  =/  job  next-job
  =/  sat=self-attestation:sa  [our u.pas cand]
  :-  (anew-cards byk now job sat (self-known-public cand) num.u.tip)
  %=  state
    next-job     +(next-job)
    pending.own  `[job cand u.pas]
  ==
::
::  +self-known-public: the sponsor-existence set for OUR OWN log
::
::    ++run-checks fails a snapshot naming a sponsor the verifier cannot
::    see as a public point.  That check belongs to the PEER: it is how a
::    stranger refuses to believe in a sponsor that does not exist.
::    Applied to our own log it is only a liveness hazard -- a comet
::    whose node does not index the public chain could never refresh its
::    own pass and would be stuck on a stale log forever.  So our own
::    owner-chosen sponsors are admitted here; peers still check them.
::
++  self-known-public
  |=  chain=custody-log:sa
  ^-  (set ship)
  =/  opens  (openings-of:lsa chain)
  =/  out    ~(key by unv-ids.urb-state)
  |-  ^-  (set ship)
  ?~  opens  out
  =*  snap  snapshot.opening.i.opens
  ?~  sponsor.snap
    $(opens t.opens)
  $(opens t.opens, out (~(put in out) u.sponsor.snap))
::
::  Launch the light-client validation of OUR OWN candidate custody log.
::
::    Deliberately the SAME thread a peer's attestation runs through, with
::    tracked=~ (there is no prior anchor for ourselves; monotonicity is
::    guaranteed structurally instead, because a candidate can only be
::    built by appending to the stored log).  Reusing +verify-lc is the
::    point: it means we can never publish a pass that we would ourselves
::    reject if a peer sent it.
::
++  anew-cards
  |=  $:  byk=desk
          now=@da
          job=@ud
          sat=self-attestation:sa
          known-public=(set ship)
          best-height=@ud
      ==
  ^-  (list card)
  =/  wir  /(scot %ud job)
  :~  :*  %pass  [%anew wir]  %arvo  %k
          %lard  byk
          %+  (set-timeout:strandio ,vase)  stuck-job-guard
          (verify-lc:lca sat ~ known-public best-height)
      ==
      :*  %pass  [%anew-guard wir]
          %arvo  %b  %wait  (add now stuck-job-guard)
      ==
  ==
::
::  Hand jael our freshly re-encoded pass.  Jael relays it to its %sybl
::  subscribers and ames installs it (+sy-sybl %anew), which re-checks
::  that it still hashes to our name.
++  anew-card
  |=  [dom=@tas =pass]
  ^-  card
  :*  %give  %fact  ~[/writs]
      %anew-response  !>(`anew-response:jael`[dom pass])
  ==
::
::  +unroutable-point: operator warning for a comet nothing can reach
::
::    Neither a sponsor nor a fief means no cold contact: +urb-point-to-jael
::    projects an absent sponsor to SELF, so a peer that has forgotten this
::    identity can never find it again.  This is LEGAL (decisions addendum
::    section 2) and must never affect a verdict -- Causeway refuses to mint
::    one, and this is how a hand-rolled transaction that never touched
::    Causeway still becomes visible to an operator.
::
++  unroutable-point
  |=  [who=ship pt=(unit point:urb)]
  ^-  tang
  ?~  pt  ~
  ?:  |(has.sponsor.net.u.pt ?=(^ fief.net.u.pt))  ~
  :~  leaf+"%gw-btc: WARNING {(scow %p who)} commits neither a sponsor nor a fief"
      leaf+"  nothing can cold-contact it (an absent sponsor projects to self);"
      leaf+"  it can only ever speak first, on lanes a peer already holds."
  ==
::
::  The same warning for freshly indexed PUBLIC identities.
++  unroutable-points
  |=  [st=state:urb ships=(set ship)]
  ^-  tang
  %-  zing
  %+  turn  ~(tap in ships)
  |=  who=ship
  ^-  tang
  (unroutable-point who (~(get by unv-ids.st) who))
::
::  Emit an asynchronous Jael verdict on the path registered by %anex.
++  verdict-card
  |=  [dom=@tas who=ship res=(unit point:jael)]
  ^-  card
  :*  %give  %fact  ~[/writs]
      %verdict  !>(`verdict:jael`[dom who res])
  ==
::
::  A ship already indexed publicly may not transition into the confidential
::  registry.  Re-check this after async verification to close the race with
::  the block thread.
++  known-public
  |=  who=ship
  ^-  ?
  ?&  (~(has by unv-ids.urb-state) who)
      !(~(has in confidential) who)
  ==
::
::  Anchor replay at the last accepted attestation while retaining any newer
::  scanner-derived custody position in the private point.
++  tracked-anchor
  |=  [st=state:urb who=ship]
  ^-  (unit anchor:sa)
  ?~  point=(~(get by unv-ids.st) who)  ~
  ?~  tip=(~(get by attested) who)  ~
  `[u.point u.tip]
::
::  +sponsor-policy: accept or decline sponsoring .who at .life
::
::    The single place sponsorship policy lives.  v9 accepts every
::    request: a comet that commits sponsor=us and proves its identity
::    gets carried.  Replace this arm (capacity limits, an allowlist,
::    payment, whatever) without touching the surrounding flow.  It is
::    consulted on EVERY re-attestation, so a sponsor can drop a
::    sponsee at its next state change with no extra machinery.
::
++  sponsor-policy
  |=  [who=ship =life]
  ^-  ?(%accept %decline)
  ?:  (~(has in declined) who)  %decline
  %accept
::
::  A verified confidential identity may claim an empty or inscription-only
::  sat, or its own existing sat, but never overwrite a different comet.
++  tip-owner-ok
  |=  [st=state:urb who=ship tip=sont:ord]
  ^-  ?
  ?~  val=(get:si:ol sont-map.st [txid vout off]:tip)
    &
  ?~  com.u.val
    &
  =(who u.com.u.val)
::
::  Apply a successful verification to the block watcher's indexes.  The real
::  tip output value is required because urb-core later uses it for sat-offset
::  arithmetic when that output is spent.
++  apply-verified
  |=  [who=ship new=point:urb tip-value=@ud]
  ^-  [state:urb (set ship)]
  =/  st  urb-state
  =/  old  (~(get by unv-ids.st) who)
  =/  old-sont=(unit sont:ord)
    ?~(old ~ `sont.own.u.old)
  =/  old-val=(unit sont-val:ord)
    ?~  old-sont  ~
    ?:  =([0x0 0 0] u.old-sont)  ~
    (get:si:ol sont-map.st [txid vout off]:u.old-sont)
  =/  moved-ins=(set insc:ord)
    ?~(old-val ~ ins.u.old-val)
  ::  A comet and every inscription on the same ordinal sat move together.
  ::  Deleting the old sont-val without carrying `ins` forward corrupts both
  ::  the sat index and insc-ids on the next spend.
  =?  sont-map.st  ?&  ?=(^ old-sont)
                           !=(u.old-sont sont.own.new)
                           !=([0x0 0 0] u.old-sont)
                       ==
    (del:si:ol sont-map.st [txid vout off]:u.old-sont)
  =.  unv-ids.st  (~(put by unv-ids.st) who new)
  =.  sont-map.st
    %:  put-all:si:ol
        sont-map.st
        txid.sont.own.new
        vout.sont.own.new
        off.sont.own.new
        tip-value
        `who
        moved-ins
    ==
  =.  insc-ids.st  (move-insc-ids insc-ids.st moved-ins sont.own.new)
  [st (~(put in confidential) who)]
::
::  Move the reverse inscription index alongside a sat.
++  move-insc-ids
  |=  [ids=insc-ids:ord moving=(set insc:ord) to=sont:ord]
  ^-  insc-ids:ord
  =/  entries  ~(tap in moving)
  |-
  ?~  entries  ids
  =/  dat  (~(get by ids) i.entries)
  =?  ids  ?=(^ dat)
    (~(put by ids) i.entries u.dat(sont to))
  $(entries t.entries)
::
::  Remove this private identity and the inscriptions moving with it from one
::  sat in a merge target, preserving any unrelated co-located occupants.
++  strip-private-sat
  |=  $:  map=sont-map:ord
          at=sont:ord
          who=ship
          moving=(set insc:ord)
      ==
  ^-  sont-map:ord
  ?:  =([0x0 0 0] at)  map
  =/  vm  (get-vout:si:ol map [txid vout]:at)
  ?~  vm  map
  =/  sv  (~(get by sats.u.vm) off.at)
  ?~  sv  map
  =/  keep-com=(unit @p)
    ?:  =(`who com.u.sv)  ~
    com.u.sv
  =/  keep-ins  (~(dif in ins.u.sv) moving)
  =/  out  (del:si:ol map [txid vout off]:at)
  ?:  &(?=(~ keep-com) =(~ keep-ins))  out
  (put-all:si:ol out txid.at vout.at off.at value.u.vm keep-com keep-ins)
::
::  Overlay a verifier-authoritative private point and its exact sat index
::  onto a block result.  This is used when the verifier inserted or advanced
::  the ship after the block thread took its base snapshot.
++  graft-private
  |=  [result=state:urb live=state:urb who=ship point=point:urb]
  ^-  (unit state:urb)
  =/  to=sont:ord  sont.own.point
  ?:  =([0x0 0 0] to)
    `result(unv-ids (~(put by unv-ids.result) who point))
  =/  vm  (get-vout:si:ol sont-map.live [txid vout]:to)
  ?~  vm  ~
  =/  sv  (~(get by sats.u.vm) off.to)
  ?~  sv  ~
  ?.  =(`who com.u.sv)  ~
  =/  scanned  (~(get by unv-ids.result) who)
  =?  sont-map.result  ?&  ?=(^ scanned)
                             !=(sont.own.u.scanned to)
                         ==
    (strip-private-sat sont-map.result sont.own.u.scanned who ins.u.sv)
  =.  sont-map.result
    (put-all:si:ol sont-map.result txid.to vout.to off.to value.u.vm `who ins.u.sv)
  =.  insc-ids.result  (move-insc-ids insc-ids.result ins.u.sv to)
  =.  unv-ids.result  (~(put by unv-ids.result) who point)
  `result
::
::  Use the block result's custody position while retaining the live private
::  networking fields.  The result must contain the exact comet index it
::  claims; otherwise retry the batch instead of committing inconsistency.
++  use-scanned-private
  |=  [result=state:urb who=ship point=point:urb]
  ^-  (unit state:urb)
  =/  at=sont:ord  sont.own.point
  ?:  =([0x0 0 0] at)
    `result(unv-ids (~(put by unv-ids.result) who point))
  =/  sv  (get:si:ol sont-map.result [txid vout off]:at)
  ?~  sv  ~
  ?.  =(`who com.u.sv)  ~
  `result(unv-ids (~(put by unv-ids.result) who point))
::
::  +any-forgotten: did a point leave the index while a batch was running?
::
::    +forget-points is the ONE way this agent un-knows an identity, and
::    it drops the ship from the index and from .confidential in
::    LOCKSTEP.  That lockstep is why +reconcile-block cannot find these
::    ships by iterating .conf: an orphaned ship is in neither set, so
::    the loop never examines it and its `?~ current ~` bail never fires.
::    Comparing the two indexes directly is the only test that sees them.
::
++  any-forgotten
  |=  [base=state:urb live=state:urb]
  ^-  ?
  =/  who=(list @p)  ~(tap in ~(key by unv-ids.base))
  |-  ^-  ?
  ?~  who  %.n
  ?.  (~(has by unv-ids.live) i.who)  %.y
  $(who t.who)
::
::  A block thread computes from `base`, while attestation results can update
::  `live` concurrently.  Block-derived `sont.own` is accepted, but all other
::  fields of a confidential point remain live/private.  If both branches
::  moved the sat away from base to different tips, return ~ so the caller can
::  discard the entire block batch and retry without advancing its cursor.
++  reconcile-block
  |=  [base=state:urb live=state:urb result=state:urb conf=(set ship)]
  ^-  (unit state:urb)
  ::  BASE FRESHNESS.  A +get-blocks thread genuinely spans events -- it
  ::  awaits each block from the light client -- so a %reorg-rollback can
  ::  and does land while one is in flight.  That arm REPAIRS the index:
  ::  it forgets the points whose most recent evidence was orphaned and
  ::  it rewinds the cursor.  This batch was computed from a base that
  ::  predates the repair, so merging it undoes the repair.  Nothing else
  ::  in the merge below can see that, because it reasons about tips
  ::  rather than about the index having been rebuilt underneath it.
  ::
  ::  Two things move, so both are checked.  Either one discards the
  ::  batch through the caller's existing `?~ merged` path, which retries
  ::  from an UNCHANGED cursor -- and cannot livelock, because the retry's
  ::  base is the live state, so the next attempt passes both tests.
  ::
  ::    THE CURSOR.  A rewind moves .block-id backwards.  Merging puts it
  ::    back where the losing branch left it, so the winning branch's
  ::    blocks between the fork point and this batch's base are NEVER
  ::    scanned -- and a %urb-state fact is broadcast for an index
  ::    spliced from two chains.  This half needs no confidential comet
  ::    and no forgotten point: any at-or-below-cursor reorg reaches it.
  ::
  ?.  =(block-id.base block-id.live)
    %-  %-  slog
        :~  leaf+"%gw-btc: block batch discarded: the cursor moved while it ran"
            leaf+"  batch base={<num.block-id.base>}, live={<num.block-id.live>}"
            leaf+"  (a reorg repair landed mid-batch; rescanning from the live cursor)"
        ==
    ~
  ::    THE POINTS.  A reorg whose fork point is AT our cursor height
  ::    rewinds nothing -- the fork point IS the last block we scanned --
  ::    and still forgets, because a $point's .seen is the block of its
  ::    last custody entry, which +verify-lc fetched and which routinely
  ::    sits ABOVE the scanner.  So the cursor test alone is not enough.
  ::
  ::    Resurrecting such a point is worse than losing a block: it comes
  ::    back into .unv-ids ALONE, since .confidential and .attested are
  ::    separate legs of the agent's state that this arm never restores.
  ::    +known-public is then true for a comet that never published --
  ::    a silent declassification, leaking through /x/point, /x/points,
  ::    the %urb-state fact and the udiffs to jael -- and the comet is
  ::    unverifiable forever after, its next %jael-writ dropped
  ::    %already-public with no verdict and no line an operator reads as
  ::    a fault.
  ::
  ?:  (any-forgotten base live)
    %-  %-  slog
        :~  leaf+"%gw-btc: block batch discarded: a point was forgotten while it ran"
            leaf+"  cursor={<num.block-id.live>}"
            leaf+"  (a reorg orphaned evidence at or above the cursor; rescanning)"
        ==
    ~
  =/  ships  ~(tap in conf)
  =/  out  result
  |-
  ?~  ships  `out
  =/  who=ship  i.ships
  =/  before  (~(get by unv-ids.base) who)
  =/  current  (~(get by unv-ids.live) who)
  =/  scanned  (~(get by unv-ids.result) who)
  ::  A verifier-only insertion is absent from the block base.  If this batch
  ::  advances the cursor, the scanner may have crossed a spend of its tip
  ::  without knowing the sat was tracked. Retry from live so the insertion is
  ::  in the next base and every transaction is replayed against it.
  ?~  before
    ::  Retry even for a no-op cursor: one deterministic replay is cheaper and
    ::  safer than relying on a block thread that did not know this sat.
    ~
  ?~  current
    ~
  ?~  scanned
    ~
  =/  base-tip=sont:ord  sont.own.u.before
  =/  live-tip=sont:ord  sont.own.u.current
  =/  scan-tip=sont:ord  sont.own.u.scanned
  =/  live-moved=?  !=(base-tip live-tip)
  =/  scan-moved=?  !=(base-tip scan-tip)
  ?:  ?&  live-moved
           scan-moved
           !=(live-tip scan-tip)
       ==
    ~
  ::  If the block branch moved (including the same move seen by both), its
  ::  indexes are authoritative. Otherwise a verifier-only move is grafted.
  ?:  |(scan-moved !live-moved)
    ::  PROVENANCE FOLLOWS THE OBSERVATION THAT SUPPLIED THE SATPOINT.
    ::  When the scanner moved the sat, this block is the most recent
    ::  evidence and .seen comes with the tip.  When NOTHING moved, the
    ::  scanner observed nothing about this comet in this batch, and its
    ::  copy of the point is the one it started from -- taking .seen from
    ::  there would REGRESS the provenance past an attestation that landed
    ::  while the batch was running.
    ::
    =/  point=point:urb
      ?.  scan-moved  u.current
      u.current(sont.own scan-tip, seen seen.u.scanned)
    =/  used  (use-scanned-private out who point)
    ?~  used  ~
    $(ships t.ships, out u.used)
  =/  graft  (graft-private out live who u.current)
  ?~  graft  ~
  $(ships t.ships, out u.graft)
::
++  drop-attested
  |=  [ats=(map ship sont:ord) ships=(set ship)]
  ^-  (map ship sont:ord)
  =/  entries  ~(tap in ships)
  |-
  ?~  entries  ats
  $(entries t.entries, ats (~(del by ats) i.entries))
::
++  drop-inflight
  |=  [jobs=(map ship inflight-writ) ships=(set ship)]
  ^-  (map ship inflight-writ)
  =/  entries  ~(tap in ships)
  |-
  ?~  entries  jobs
  $(entries t.entries, jobs (~(del by jobs) i.entries))
::
::  Roll back only verifier insertions that a rejected batch independently
::  proved were public spawns.  Clear their private comet association while
::  preserving any unrelated inscriptions co-located on the sat.
++  drop-private-insertions
  |=  [st=state:urb ships=(set ship)]
  ^-  state:urb
  =/  entries  ~(tap in ships)
  |-
  ?~  entries  st
  =/  who=ship  i.entries
  =/  point  (~(get by unv-ids.st) who)
  ?~  point
    $(entries t.entries)
  =/  at=sont:ord  sont.own.u.point
  =?  sont-map.st  !=([0x0 0 0] at)
    (del-com:si:ol sont-map.st [txid vout off]:at)
  =.  unv-ids.st  (~(del by unv-ids.st) who)
  $(entries t.entries)
::
::  +lift-urb-state: a pre-provenance index, lifted into the current mold
::
::    Every point gains .seen=~.  That is not a placeholder for a hash we
::    could have worked out: nothing in the old state records which block
::    a point came from, which is exactly why this field was added.  ~ is
::    the truthful value, and it is what makes a lifted point unfilterable:
::    a reorg LEAVES IT ALONE, because "we cannot tell" is not evidence of
::    anything (+orphaned-points).
::
++  lift-urb-state
  |=  old=urb-state-13
  ^-  state:urb
  :^    block-id.old
      sont-map.old
    insc-ids.old
  %-  ~(run by unv-ids.old)
  |=(pt=point-13 `point:urb`[own.pt net.pt ~])
::
::  Strip confidential identities from snapshots and public scries.  Their
::  points propagate only through self-attestation, never through the classic
::  Groundwire snapshot path.
++  filter-snapshot
  |=  [st=state:urb conf-ships=(set ship)]
  ^-  state:urb
  ?:  =(~ conf-ships)  st
  =.  unv-ids.st
    =/  ships  ~(tap in conf-ships)
    |-
    ^+  unv-ids.st
    ?~  ships  unv-ids.st
    $(ships t.ships, unv-ids.st (~(del by unv-ids.st) i.ships))
  =.  sont-map.st
    =/  entries  ~(tap by sont-map.st)
    =|  acc=sont-map:ord
    |-
    ^-  sont-map:ord
    ?~  entries  acc
    =/  [key=[=txid:ord =vout:ord] vm=vout-map:ord]  i.entries
    =/  sats
      %-  ~(gas by *(map off:ord sont-val:ord))
      %+  murn  ~(tap by sats.vm)
      |=  [o=off:ord sv=sont-val:ord]
      ^-  (unit [off:ord sont-val:ord])
      ?.  &(?=(^ com.sv) (~(has in conf-ships) u.com.sv))
        `[o sv]
      ::  Keep public inscriptions on a confidential comet's sat, but clear
      ::  the comet association.  With no inscriptions, omit the exact
      ::  private satpoint altogether.
      ?:  =(~ ins.sv)  ~
      `[o sv(com ~)]
    ?:  =(~ sats)
      $(entries t.entries)
    $(entries t.entries, acc (~(put by acc) key vm(sats sats)))
  st
::
::  The ships Jael currently watches through /<ship> paths.
++  subs-to-ships
  |=  sup=bitt:gall
  ^-  (set ship)
  %-  silt
  %+  murn  ~(val by sup)
  |=  [who=ship =path]
  ^-  (unit ship)
  ?.  ?=([@ ~] path)  ~
  (slaw %p i.path)
::
::  +urb-point-to-jael: project a urb $point onto Jael's $point
::
++  urb-point-to-jael
  |=  [pt=point:urb who=ship]
  ^-  point:jael
  :*  rift.net.pt
      life.net.pt
      (my [life.net.pt (sub (end 3 pass.net.pt) 'a') pass.net.pt] ~)
      ?:  has.sponsor.net.pt  `who.sponsor.net.pt
      `who
      fief.net.pt
  ==
::
::  +install-trusted-peer: install a peer's point into jael on TRUST,
::  offline, for %gevulot's sponsor-mediated discovery.  Returns the cards
::  (a single positive %verdict) or ~ if the pass does not survive the
::  offline self-check.  It NEVER returns a negative verdict, so it cannot
::  snub: a malformed or duplicate packet is simply a no-op here (jael
::  installing keys it already holds is idempotent).  No chain fetch, so
::  this works before the light client has synced -- the whole point.
::
++  install-trusted-peer
  |=  [who=ship =pass]
  ^-  (list card)
  ?~  sat=(pass-attestation domain:cc who pass)  ~
  ?~  pt=(offline-point u.sat who)  ~
  ~[(verdict-card domain:cc who `(urb-point-to-jael u.pt who))]
::
::  +offline-point: build a $point:urb from a decoded self-attestation
::  WITHOUT walking the chain.  The current on-chain state is the snapshot
::  in the NEWEST custody-log opening; +urb-point-to-jael reads only .net
::  (rift, life, pass, sponsor, fief), so a bunt sat and ~ provenance are
::  fine -- we never store this point in our own index, only hand it to
::  jael.  An absent sponsor projects to the peer itself, as everywhere.
::
++  offline-point
  |=  [sat=self-attestation:sa who=ship]
  ^-  (unit point:urb)
  =/  snap=(unit snapshot:sa)
    =/  entries=(list custody-entry:sa)  (flop chain.sat)
    |-  ^-  (unit snapshot:sa)
    ?~  entries  ~
    ?~  opening.i.entries  $(entries t.entries)
    `snapshot.u.opening.i.entries
  ?~  snap  ~
  :-  ~
  ^-  point:urb
  :+  [*sont:ord ~]
    :*  rift.u.snap
        life.u.snap
        pass.sat
        ?~  sponsor.u.snap  [%.n who]
        [%.y u.sponsor.u.snap]
        ~
        fief.u.snap
    ==
  ~
::
++  listen-to-urb
  |=  [ships=(set ship) =source:point:jael]
  ^-  card
  [%pass /lo %arvo %j %listen ships source]
::
++  jael-update
  |=  =udiffs:point:jael
  ^-  (list card)
  :-  [%give %fact ~[/] %azimuth-udiffs !>(udiffs)]
  ?~  udiffs
    ~
  ::
  ::  XX comment from %azimuth:
  ::     "Should really give all diffs involving each ship at the same time"
  %+  turn
    udiffs
  |=  [=ship =udiff:point:jael]
  ^-  card
  ::  The per-ship fact must carry the SAME $udiffs shape as the broadcast
  ::  on ~[/]: jael mold-casts every %azimuth-udiffs fact with
  ::  ;;(=udiffs:point ...) (sys/vane/jael.hoon +take, ~985), and
  ::  $udiffs is (list [=ship =udiff]) -- not (list udiff).  Dropping the
  ::  .ship head put the udiff's $id (a cell) where jael wanted an @p, so
  ::  the clam bailed %exit and took the whole event down with it.  Jael
  ::  only subscribes to /<ship> once it already tracks that ship, which
  ::  is why a SPAWN indexed fine and the first STATE UPDATE afterwards
  ::  crashed: the scanner's batch was rolled back forever and no peer
  ::  could ever learn a public comet's new fief/sponsor/life/key.
  =/  =udiffs:point:jael  [ship udiff]~
  [%give %fact [/(scot %p ship)]~ %azimuth-udiffs !>(udiffs)]
::
++  fx-to-udiffs
  |=  fx=(list [id:block:bitcoin effect:urb])
  ^-  udiffs:point:jael
  %+  murn
    fx
  |=  [=id:block:bitcoin eu=effect:urb]
  ^-  (unit (pair ship udiff:point:jael))
  ?.  ?=(%point -.eu) :: only if this effect is a %point diff:urb
    ~
  =/  pdiff  (tail (tail eu))
  ?+    -.pdiff   ~
      %rift
    `[ship.eu id %rift rift.pdiff %.n]
  ::
      %keys
    `[ship.eu id %keys [life.pdiff (sub (end 3 pass.pdiff) 'a') pass.pdiff] %.y]
  ::
      %fief
    `[ship.eu id %fief fief.pdiff]
  ::
      %sponsor
    ::
    ::  defensively guarantee that a ship with no
    ::  onchain sponsor is sponsoring itself in jael
    ?~  sponsor.pdiff
      `[ship.eu id %spon `ship.eu]
    `[ship.eu id %spon sponsor.pdiff]
  ==
::
++  state-to-udiffs
  |=  urb-state=state:urb
  ^-  udiffs:point:jael
  =/  points=(list [=ship point:urb])
    ~(tap by unv-ids.urb-state)
  =/  =id:block:jael
    block-id:urb-state
  =/  new-udiffs  *udiffs:point:jael
  |-  
  ^+  new-udiffs
  ?~  points
    new-udiffs
  %=  $
     points  t.points
  ::
     new-udiffs  
     %+  welp
      =,  i.points
      ^-  udiffs:point:jael
      :~  [ship id %keys [life.net (sub (end 3 pass.net) 'a') pass.net] %.y]
          [ship id %rift rift.net %.y]
          [ship id %fief fief.net]
          :*  ship
              id
              %spon
              ?.  has.sponsor.net
                `ship
              `who.sponsor.net
          ==
      ==
    new-udiffs
  ==
--
