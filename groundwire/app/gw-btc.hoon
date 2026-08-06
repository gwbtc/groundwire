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
::  doc/confidential-comets-agent.md and, in gwbtc/urbit,
::  pkg/arvo/doc/spec/confidential-comets.md).  On boot it registers
::  itself with Jael via a %anex task, so that when a confidential
::  (suite-%c) comet self-attests to us, Ames routes the attestation
::  here as a %jael-writ poke.  We decode the custody log carried in the
::  pass, fetch its transactions asynchronously through %light-client,
::  and answer with a %writ-response fact; on success
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
::  $reorg-stop: the block scanner, halted by a chain reorganisation
::
::    %bitcoin-client reports a reorg as a %reorg-rollback on /best-block.
::    Until 2026-08-06 this agent handled it IDENTICALLY to %new: only .best
::    moved, while .block-id.urb-state (the scan cursor) and .unv-ids (every
::    fact derived from the orphaned blocks) were untouched.  Three
::    consequences, all silent: the cursor sat ABOVE the new tip so nothing
::    was rescanned, facts indexed out of orphaned blocks stayed forever,
::    and facts unique to the winning chain were never seen.
::
::    Undoing the orphaned facts is not possible with what we store: a point
::    in .unv-ids does not record the height it was indexed at, so there is
::    no way to tell which entries came from the losing chain.  Rewinding
::    the cursor and rescanning would therefore replay the winning chain ON
::    TOP of a corrupted index, not instead of it.
::
::    So the scanner STOPS, loudly, and waits for an operator.  A halted
::    index is a liveness failure that announces itself; a silently forked
::    one is a correctness failure that does not.  %gw-reorg-resume decides
::    what to do about it (see +on-poke).
::
::    Confidential verification deliberately keeps running: it reads the
::    chain through the light client, which does its own reorg handling, and
::    its answers do not come from this index.
::
+$  reorg-stop
  $:  at=@ud        ::  height the chain rolled back to
      cursor=@ud    ::  our scan cursor when that happened
      since=@da
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
      publicizing=(set ship)
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
      reorg-halt=(unit reorg-stop)
  ==
::  $gw-state-11 / $gw-state-10: the two earlier shapes
::
::    -11 is the state before .synced and .reorg-halt; -10 is the state
::    before .own as well.  +on-load discriminates on them; ;; is strict
::    about arity (it bails on both a missing and an extra tail), so trying
::    the current mold first and falling back is exact, not a guess.
::
::    Nothing inside these molds changed, only the tail they are missing,
::    which is why they can keep naming the current $inflight-writ.
::
+$  gw-state-11
  $:  urb-state=state:urb
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
  $:  urb-state=state:urb
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
  ::  agent name).  Jael watches /writs for %writ-response /
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
  ::  appended later.  ;; is strict about arity, so the current mold
  ::  succeeding IS the discriminator; a pre-.own state loads with an empty
  ::  log and re-seeds itself from our pass's xtr on the next %anew.
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
  =/  o11  (mole |.(;;(gw-state-11 nou)))
  ?^  o11
    =/  ext=gw-state
      :*  urb-state.u.o11  indexing.u.o11  best.u.o11  inflight.u.o11
          confidential.u.o11  attested.u.o11  publicizing.u.o11
          next-job.u.o11  sponsees.u.o11  declined.u.o11  own.u.o11
          %.n  ~
      ==
    :_  this(state ext)
    ~[(watch-synced our.bowl)]
  =/  o  ;;(gw-state-10 nou)
  =/  ext=gw-state
    :*  urb-state.o  indexing.o  best.o  inflight.o  confidential.o
        attested.o  publicizing.o  next-job.o  sponsees.o  declined.o
        *own-custody  %.n  ~
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
    =/  poke  ;;(jael-poke:urb q.vase)
    ?-    -.poke
        %jael-writ
      ::  A prior block result has already proved this is a public spawn and
      ::  an exact sanitized replay is in progress.  Ignore reinsertion until
      ::  that block job resolves.
      ::  EVERY drop below is announced.  They used to be silent, and
      ::  Phase 5b's finding 9 was that %jael-writ has nine distinct
      ::  silent-drop returns which from outside look identical to "nobody
      ::  is talking to this ship".  Phase 6.2 then paid for it for real: a
      ::  runtime fault stranded the single-flight slot and the two retries
      ::  that would have diagnosed it were swallowed without a word.
      ::  These lines are one-per-dropped-writ, so they are also the rate
      ::  at which a peer is retrying, which is itself the thing you want
      ::  to know.
      ::
      ?:  (~(has in publicizing) who.poke)
        %-  (slog leaf+"%gw-btc: writ from {(scow %p who.poke)} dropped: public-spawn replay in progress" ~)
        `this
      ::  Single-flight per ship: at most one verification job.  A
      ::  duplicate or replacement writ while one is in flight is
      ::  dropped -- the peer's retries re-enter after the verdict, and
      ::  the on-chain cost of minting states is the rate limit.  No
      ::  queue, no slot economy.
      ?:  (~(has by inflight) who.poke)
        %-  (slog leaf+"%gw-btc: writ from {(scow %p who.poke)} dropped: a verification is already in flight" ~)
        `this
      ::  A ship whose sponsorship we declined re-attests on every
      ::  retry; short-circuit before spending a full verification on
      ::  it.  Clearing the entry (%gw-sponsor-clear) restores normal
      ::  handling immediately -- the refusal is never sticky in the
      ::  kernel, only here.
      ?:  (~(has in declined) who.poke)
        %-  (slog leaf+"%gw-btc: writ from {(scow %p who.poke)} dropped: sponsorship declined by operator" ~)
        `this
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
        ?:  ?|  (public-pass [dom who pass]:poke)
                (foreign-kelvin [dom who pass]:poke)
            ==
          `this
        :_  this
        ~[(writ-card dom.poke who.poke ~)]
      ::  A present, canonically decoded xtr must contain the spawn opening.
      ::  The raw-0 public shape was classified above before +from-xtr.
      ?~  chain.u.sat
        :_  this
        ~[(writ-card dom.poke who.poke ~)]
      ?:  (gth (lent chain.u.sat) 1.024)
        :_  this
        ~[(writ-card dom.poke who.poke ~)]
      ?:  (known-public who.poke)
        %-  (slog leaf+"%gw-btc: writ from {(scow %p who.poke)} dropped: already a public point" ~)
        `this
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
        %-  (slog leaf+"%gw-btc: writ from {(scow %p who.poke)} held: no chain tip yet" ~)
        `this
      ?.  synced
        %-  %-  slog
            :~  leaf+"%gw-btc: writ from {(scow %p who.poke)} held: light client NOT synced"
                leaf+"  (tip {<num.u.best>}; no verdict will be emitted until it catches up)"
            ==
        ::  ... and re-read the light client's answer while we are at it,
        ::  because it may have caught up without telling us.  See
        ::  +refresh-synced.
        :_  this
        (refresh-synced our.bowl)
      =/  need=@ud  (log-top-height u.sat)
      ?.  (gte num.u.best need)
        %-  %-  slog
            :~  leaf+"%gw-btc: writ from {(scow %p who.poke)} held: tip {<num.u.best>} below evidence height {<need>}"
                leaf+"  (our chain view does not reach this log; that is ignorance, not fraud)"
            ==
        `this
      =/  job  next-job
      =.  next-job  +(next-job)
      =/  req=inflight-writ  [dom.poke pass.poke u.sat job]
      =.  inflight  (~(put by inflight) who.poke req)
      :_  this
      (verify-cards q.byk.bowl now.bowl who.poke req num.u.best)
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
      ?.  =(dom.poke domain:cc)
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
      ::  Restart a block scanner halted by a reorg (see $reorg-stop).
      ::
      ::    ~          resume from the current cursor.  The operator is
      ::               accepting that facts indexed out of orphaned blocks
      ::               may still be in .unv-ids and that the winning
      ::               chain's replacements in the skipped range were
      ::               never seen.
      ::    [~ height] rewind the cursor to `height` first, so the winning
      ::               chain from there is scanned.  This ADDS the correct
      ::               facts; it cannot remove wrong ones.
      ::
      ::  Neither is a repair.  The repair is to rebootstrap the public
      ::  index, which needs a nuke (see %urb-start-indexing).  This poke
      ::  exists so an operator can choose, deliberately and on the record,
      ::  rather than have the agent guess for them.
      ::
      %gw-reorg-resume
    ?>  =(our src):bowl
    ?~  reorg-halt
      %-  (slog leaf+"%gw-btc: no reorg halt in force" ~)
      `this
    ::  MOLD-CAST the raw noun, never +!<: an operator poking `&noun ~`
    ::  builds a vase whose type is bare %~, which does not nest under a
    ::  head-tagged union, and +!< would bail on exactly the invocation the
    ::  docs above tell them to use.
    ::
    =/  to=(unit @ud)  ;;((unit @ud) q.vase)
    =/  msg=tape
      ?~  to  "resuming from cursor {<num.block-id.urb-state>}"
      "rewinding cursor to {<u.to>} and resuming"
    %-  %-  slog
        :~  leaf+"%gw-btc: reorg halt cleared by operator; {msg}"
            leaf+"  (halted at {<since.u.reorg-halt>} by a rollback to {<at.u.reorg-halt>})"
        ==
    ::  .block-id is the LAST block scanned, so rewinding to first-scan
    ::  height h means storing h-1 (the %gw-index-from convention).
    =?  urb-state  ?=(^ to)
      urb-state(block-id [0x0 (dec (max 1 u.to))])
    :_  this(reorg-halt ~)
    ~[[%pass /timer %arvo %b %wait now.bowl]]
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
    ?:  =(0 start)  `this
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
    ::  has nine distinct silent-drop returns; .inflight and .publicizing
    ::  are two of them and could not be told apart from outside at all
    ::  (Phase 5b, finding 9).
    ::
      [%x %inflight ~]
    ``noun+!>(~(key by inflight))
    ::
      [%x %publicizing ~]
    ``noun+!>(publicizing)
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
    ::  (Phase 6.7).  [synced best-height indexing halted].
    ::
      [%x %ready ~]
    :^  ~  ~  %noun
    !>  :*  synced=synced
            tip=?~(best ~ `num.u.best)
            indexing=indexing
            reorg-halt=reorg-halt
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
  ::  %writ-response and %anew-response facts.
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
    ::  Halted by a reorg (see the /best-block %reorg-rollback arm).  Keep
    ::  the timer alive and keep saying so -- a stopped scanner that stops
    ::  mentioning it is indistinguishable from a working one.
    ?^  reorg-halt
      %-  %-  slog
          :~  leaf+"%gw-btc: block scanner HALTED since {<since.u.reorg-halt>} by a reorg to {<at.u.reorg-halt>}"
              leaf+"  (cursor {<cursor.u.reorg-halt>}; clear with %gw-reorg-resume)"
          ==
      :_  this
      ~[[%pass /timer %arvo %b %wait (add ~m5 now.bowl)]]
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
      ::  by an older revision cannot crash the agent on upgrade.)
      [?(%stuck-job %verify-timeout) ship=@ job=@ ~]
    ?.  ?=([%behn %wake *] sign-arvo)
      (on-arvo:def wire sign-arvo)
    =/  who  (slav %p i.t.wire)
    =/  job  (slav %ud i.t.t.wire)
    =/  active  (~(get by inflight) who)
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
    ?~  pend  `this
    ?.  =(job job.u.pend)  `this
    =/  req=anew-job  u.pend
    =.  pending.own  ~
    ?+    sign-arvo  (on-arvo:def wire sign-arvo)
        [%khan %arow *]
      ?.  -.p.sign-arvo
        ?>  ?=([%khan %arow %.n *] sign-arvo)
        %-  (slog leaf+"%gw-btc: %anew self-validation ended without a verdict" +.p.p.sign-arvo)
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
    ?~  pend  `this
    ?.  =(job job.u.pend)  `this
    %-  (slog leaf+"%gw-btc: releasing stuck %anew self-validation slot" ~)
    `this(pending.own ~)
  ::
      [%verify ship=@ job=@ ~]
    =/  who  (slav %p i.t.wire)
    =/  job  (slav %ud i.t.t.wire)
    =/  active  (~(get by inflight) who)
    ?~  active
      `this
    ?.  =(job job.u.active)
      `this
    =/  req=inflight-writ  u.active
    =.  inflight  (~(del by inflight) who)
    ?+    sign-arvo  (on-arvo:def wire sign-arvo)
        [%khan %arow *]
      ?.  -.p.sign-arvo
        ?>  ?=([%khan %arow %.n *] sign-arvo)
        %-  (slog leaf+"%gw-btc: verification thread for {(scow %p who)} ended without a verdict" +.p.p.sign-arvo)
        ::  A generic strand crash is retryable/indeterminate.  Only a
        ::  mold-valid verifier result may emit a Jael verdict.
        `this
      ?>  ?=([%khan %arow %.y %noun *] sign-arvo)
      =/  [%khan %arow %.y %noun =vase]  sign-arvo
      =/  [res=result:sa tip-spk=hexb:bc]
        !<([result:sa hexb:bc] vase)
      ::  A concurrently indexed public spawn wins this race.  It no longer
      ::  belongs to the confidential verifier, but that is not evidence the
      ::  peer supplied a bad attestation, so do not emit a sticky failure.
      ?:  (known-public who)
        `this
      %-  (slog (report:lsa verdict.res))
      =/  verified=(unit point:urb)
        ?.  &(ok.verdict.res =(who who.verdict.res))  ~
        ?~  point.res  ~
        ?.  (attested-point-ok pass.req u.point.res)  ~
        ?.  (tip-owner-ok urb-state who sont.own.u.point.res)  ~
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
        ?:  (unknown-verdict:lsa verdict.res)
          %-  %-  slog
              :~  leaf+"%gw-btc: attestation for {(scow %p who)} is UNDETERMINED; emitting no verdict"
                  leaf+"  (the [??] checks above could not be evaluated from what we can see;"
                  leaf+"   that is our ignorance, not the peer's fraud -- it will be retried)"
              ==
          `this
        ?:  (stale-verdict:lsa verdict.res)
          %-  %-  slog
              :~  leaf+"%gw-btc: attestation for {(scow %p who)} is STALE, not invalid"
                  leaf+"  (its identity sat has moved; demoting to alien, never snubbing)"
              ==
          :_  this
          ~[(stale-card dom.req who)]
        ::  Everything else IS a negative verdict, and a negative verdict is
        ::  destructive: jael %fails and ames snubs, stickily, which then
        ::  blocks the packet that could correct it.  It has never announced
        ::  itself -- Phase 6.7 found the snub itself to be invisible in the
        ::  logs, discoverable only through .^(/snubbed) -- so say it here,
        ::  at the one place that causes it.
        ::
        =/  why=tape
          ?:  ok.verdict.res
            "its checks passed but the result was refused locally (the rebuilt pass's key disagrees with the forwarded pass, or its tip sat belongs to another comet)"
          "the [XX] checks above are fraud-class: evidence that was never true, not evidence that expired"
        %-  %-  slog
            :~  leaf+"%gw-btc: SNUBBING {(scow %p who)} on a negative %gw-btc verdict"
                leaf+"  {why}"
                leaf+"  (a snub is sticky and blocks the packet that would correct it;"
                leaf+"   inspect with .^(/snubbed) and undo with %snub %deny %del)"
            ==
        :_  this
        ~[(writ-card dom.req who ~)]
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
      ~[(writ-card dom.req who `(urb-point-to-jael u.verified who))]
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
      ::  A point absent from the block thread's base but introduced by an
      ::  on-chain %owner effect is a genuine public spawn.  This narrowly
      ::  resolves the race where asynchronous verification inserted the same
      ::  ship while the block thread was running.  Existing confidential
      ::  points never declassify merely because a later block names them:
      ::  effects lack the input provenance required to make that inference.
      =/  public-new
        (public-spawns base-state +.fx-and-state -.fx-and-state)
      ::  A confidential comet that PUBLISHED itself in this batch has
      ::  declassified, on purpose and irreversibly: +index-point:urb-core
      ::  emits %public only for an OP_RETURN publication that spent the
      ::  identity sat we already track, which nobody but the owner can
      ::  build.  This is the second, deliberate way out of .confidential
      ::  (the first, +public-spawns, is a race resolution) and it is what
      ::  makes decisions-addendum section 2's "publication" self-rescue
      ::  actually happen: while the ship stays confidential its udiffs are
      ::  suppressed by +filtered-udiffs and +detect-stale deletes its point
      ::  the instant the publication's own sat move is seen, so the rescue
      ::  leaves it strictly worse off than before.
      ::
      =/  declassified  (published-comets confidential -.fx-and-state)
      =/  gone-public  (~(uni in public-new) declassified)
      =/  new-confidential  (~(dif in confidential) gone-public)
      =/  new-attested  (drop-attested attested gone-public)
      %-  (slog (declassify-report declassified))
      ::
      ::  Three-way merge block-derived custody with any verifier result that
      ::  landed since `base-state`.  A divergent double move is ambiguous;
      ::  discard the whole batch and rerun from the unchanged cursor rather
      ::  than mixing two incompatible histories.
      =/  merged
        (reconcile-block base-state urb-state.state +.fx-and-state new-confidential)
      ?~  merged
        ::  NB: the tang MUST be a list.  This read `(slog leaf+"...")` --
        ::  a bare tank -- which +slog walks as if it were the list,
        ::  printing nothing at all.  The branch below retries the batch
        ::  from an UNCHANGED cursor, so a bug that lands here loops
        ::  forever and, with the message swallowed, does it in total
        ::  silence: the agent looks alive and simply never indexes again.
        %-  %-  slog
            :~  leaf+"%gw-btc: concurrent custody conflict; retrying batch"
                leaf+"  cursor={<num.block-id.urb-state>} public-new={<public-new>}"
                leaf+"  confidential={<new-confidential>}"
            ==
        ::  If this rejected batch also proved a public spawn that raced a
        ::  verifier-only insertion, remove just that private insertion before
        ::  retrying.  Otherwise the duplicate spawn would be suppressed on
        ::  replay and could never produce another %owner effect.
        =/  retry-state  (drop-private-insertions urb-state public-new)
        :_  %=  this
              urb-state     retry-state
              confidential  new-confidential
              attested     new-attested
              publicizing  (~(uni in publicizing) public-new)
              inflight     (drop-inflight inflight public-new)
            ==
        ::  Schedule the sanitized snapshot for immediate retry. The publicizing
        ::  guard keeps this exact base stable while Behn schedules the retry,
        ::  and also across any transient block-thread failure.
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
                !(~(has in new-confidential) ship)
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
      =/  gone-stale=(set ship)  (detect-stale new-urb-state new-confidential attested)
      =.  confidential  (~(dif in new-confidential) gone-stale)
      =.  attested      (drop-attested new-attested gone-stale)
      =.  inflight      (drop-inflight inflight gone-stale)
      =.  urb-state     (drop-private-insertions new-urb-state gone-stale)
      =.  publicizing   ~
      =/  stale-cards=(list card)
        %+  turn  ~(tap in gone-stale)
        |=(=ship (stale-card dap.bowl ship))
      ::  A PUBLIC comet just indexed with neither a sponsor nor a fief
      ::  is unreachable in exactly the same way; hand-rolled spawns that
      ::  never touched Causeway show up here.
      %-  (slog (unroutable-points urb-state gone-public))
      :_  this
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
      ?.  ?=(%is-synced p.cage.sign)
        (on-agent:def wire sign)
      =/  syn  !<(is-synced:update:lc q.cage.sign)
      ?:  =(syn synced)  `this
      %-  %-  slog
          :_  ~
          :-  %leaf
          ?:  syn
            "%gw-btc: light client is SYNCED; confidential verification enabled"
          "%gw-btc: light client is NOT synced; holding all attestations (no verdicts)"
      `this(synced syn)
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
      ::  fact mark %best-block, carrying best-block:update (a %new / a
      ::  %reorg-rollback with block-height + block-hash).
      ?.  ?=(%best-block p.cage.sign)
        (on-agent:def wire sign)
      =/  upd  !<(best-block:update:lc q.cage.sign)
      ?:  ?=(%new -.upd)
        `this(best ``id:block:bc`[block-hash.upd block-height.upd])
      ?>  ?=(%reorg-rollback -.upd)
      =/  new-best=id:block:bc  [block-hash.upd block-height.upd]
      ::  A REORG.  Until 2026-08-06 this arm was byte-identical to %new:
      ::  only .best moved, the scan cursor never rewound, and every fact
      ::  indexed out of an orphaned block stayed in .unv-ids forever while
      ::  the winning chain's replacements were skipped -- all silently.
      ::  With block-confirmations = 1 a single-block reorg, several a month
      ::  on mainnet, is enough to reach it.
      ::
      ::  Two cases, and only one of them is a problem:
      ::
      ::    rollback ABOVE our cursor -- the orphaned blocks are ones we had
      ::    not scanned.  Nothing we hold came from them.  Note it and carry
      ::    on; the scanner will walk the winning chain normally.
      ::
      ::    rollback AT OR BELOW our cursor -- our index contains facts
      ::    derived from blocks that no longer exist, and we cannot tell
      ::    WHICH: a $point does not record the height it was indexed at.
      ::    Rewinding and rescanning would replay the winning chain on top
      ::    of the corrupted index rather than instead of it, so it is not a
      ::    fix, it is a second bug.  Stop the scanner and say so.  See
      ::    $reorg-stop and %gw-reorg-resume.
      ::
      ?:  (gth block-height.upd num.block-id.urb-state)
        %-  %-  slog
            :~  leaf+"%gw-btc: chain reorg to {<block-height.upd>}, above our cursor {<num.block-id.urb-state>}"
                leaf+"  (nothing we have indexed came from the orphaned blocks; continuing)"
            ==
        `this(best `new-best)
      ?^  reorg-halt
        `this(best `new-best)
      %-  %-  slog
          :~  leaf+"%gw-btc: CHAIN REORG TO {<block-height.upd>} -- BLOCK SCANNER HALTED"
              leaf+"  our scan cursor was {<num.block-id.urb-state>}, so this index may contain facts"
              leaf+"  derived from orphaned blocks, and we cannot tell which: a $point does not"
              leaf+"  record the height it was indexed at.  The scanner will not advance."
              leaf+"  Confidential verification is UNAFFECTED (it reads the light client directly)."
              leaf+"  Operator: `:gw-btc &gw-reorg-resume ~` resumes from the current cursor and"
              leaf+"  accepts the risk; `:gw-btc &gw-reorg-resume [~ height]` rewinds the cursor"
              leaf+"  first.  A correct repair is to rebootstrap the public index."
          ==
      :-  ~
      %=  this
        best        `new-best
        reorg-halt  `[block-height.upd num.block-id.urb-state now.bowl]
      ==
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
::  The hiding dat commitment is not opened here (no blind is carried), so
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
++  verify-cards
  |=  [byk=desk now=@da who=ship req=inflight-writ best-height=@ud]
  ^-  (list card)
  =/  wir  /(scot %p who)/(scot %ud job.req)
  :~  :*  %pass  [%verify wir]  %arvo  %k
          %lard  byk
          %+  (set-timeout:strandio ,vase)  stuck-job-guard
          %:  verify-lc:lca
              sat.req
              (tracked-anchor urb-state who)
              ~(key by unv-ids.urb-state)
              best-height
          ==
      ==
      :*  %pass  [%stuck-job wir]
          %arvo  %b  %wait  (add now stuck-job-guard)
      ==
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
  ::  them now says which it was.  See also /x/pending-own.
  ::
  ::  single-flight, exactly as for peer verification
  =/  pend  pending.own
  ?^  pend
    %-  (slog leaf+"%gw-btc: %anew refused: a self-validation is already in flight (job {<job.u.pend>})" ~)
    `state
  ?~  cand
    %-  (slog leaf+"%gw-btc: %anew refused: no custody log to validate" ~)
    `state
  ?:  (gth (lent cand) 1.024)
    %-  (slog leaf+"%gw-btc: %anew refused: custody log too long ({<(lent cand)>} entries)" ~)
    `state
  ::  readiness is infrastructure, never evidence -- and the same
  ::  correction as the %jael-writ gate applies here: `?~ best` passes on
  ::  the genesis block %bitcoin-client answers a fresh subscription with.
  ::  Publishing a pass validated against a chain we have not seen would
  ::  install it in ames for every peer to reject.
  =/  tip  best
  ?~  tip
    %-  (slog leaf+"%gw-btc: %anew refused: no chain tip yet" ~)
    `state
  ?.  synced
    %-  (slog leaf+"%gw-btc: %anew refused: light client not synced (tip {<num.u.tip>})" ~)
    [(refresh-synced our) state]
  =/  need=@ud  (log-top-height [our *pass cand])
  ?.  (gte num.u.tip need)
    %-  (slog leaf+"%gw-btc: %anew refused: tip {<num.u.tip>} below evidence height {<need>}" ~)
    `state
  =/  base  (own-pass our now)
  ?~  base
    %-  (slog leaf+"%gw-btc: %anew refused: jael has no suite-C pass for us" ~)
    `state
  =/  pas  (with-xtr:cc u.base (jam cand))
  ?~  pas
    %-  (slog leaf+"%gw-btc: %anew refused: +with-xtr could not re-encode our pass" ~)
    `state
  ::  the pass we are about to publish must still hash to our name.  the
  ::  tweak is immutable, so this can only fail if +with-xtr or the ring
  ::  ever drifts from the kernel's encoder -- but publishing a pass that
  ::  is not ours would be catastrophic, so check it anyway.
  ?.  =(our `@p`fig:ex:(com:nu:cric:crypto u.pas))
    %-  (slog leaf+"%gw-btc: %anew refused: re-encoded pass does not hash to our own name" ~)
    `state
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
++  writ-card
  |=  [dom=@tas who=ship res=(unit point:jael)]
  ^-  card
  :*  %give  %fact  ~[/writs]
      %writ-response  !>(`writ-response:jael`[dom who res])
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
::  A block thread computes from `base`, while attestation results can update
::  `live` concurrently.  Block-derived `sont.own` is accepted, but all other
::  fields of a confidential point remain live/private.  If both branches
::  moved the sat away from base to different tips, return ~ so the caller can
::  discard the entire block batch and retry without advancing its cursor.
++  reconcile-block
  |=  [base=state:urb live=state:urb result=state:urb conf=(set ship)]
  ^-  (unit state:urb)
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
    =/  point=point:urb  u.current(sont.own scan-tip)
    =/  used  (use-scanned-private out who point)
    ?~  used  ~
    $(ships t.ships, out u.used)
  =/  graft  (graft-private out live who u.current)
  ?~  graft  ~
  $(ships t.ships, out u.graft)
::
::  Find public spawns introduced by this block batch.  %owner is emitted only
::  by a successful on-chain spawn; later point effects are not sufficient to
::  declassify an existing confidential identity.
++  public-spawns
  |=  $:  base=state:urb
          result=state:urb
          fx=(list [id:block:bitcoin effect:urb])
      ==
  ^-  (set ship)
  =|  out=(set ship)
  |-
  ?~  fx  out
  =/  eu=effect:urb  +.i.fx
  ?.  ?=([%point * %owner *] eu)
    $(fx t.fx)
  =/  [%point who=ship %owner *]  eu
  ?.  ?&  !(~(has by unv-ids.base) who)
          (~(has by unv-ids.result) who)
      ==
    $(fx t.fx)
  $(fx t.fx, out (~(put in out) who))
::
::  +published-comets: confidential comets that declassified in this batch
::
::    %public is emitted by +index-point:urb-core for every accepted
::    OP_RETURN publication.  Reaching +index-point at all requires either
::    spending the comet's funding satpoint (a spawn) or spending the
::    identity sat we are already tracking (a state update) -- neither of
::    which anyone but the owner can do.  So a %public effect naming a ship
::    we hold as CONFIDENTIAL is that owner's own, on-chain, permanent
::    decision to become public.
::
::    We only report ships that were confidential: %public for a comet that
::    was already public is a no-op, and %gw-btc must never infer
::    declassification from anything weaker than this.
::
++  published-comets
  |=  [conf=(set ship) fx=(list [id:block:bitcoin effect:urb])]
  ^-  (set ship)
  =|  out=(set ship)
  |-
  ?~  fx  out
  =/  eu=effect:urb  +.i.fx
  ?.  ?=([%point * %public ~] eu)
    $(fx t.fx)
  =/  [%point who=ship %public ~]  eu
  ?.  (~(has in conf) who)
    $(fx t.fx)
  $(fx t.fx, out (~(put in out) who))
::
::  Operator record of an irreversible privacy change.
++  declassify-report
  |=  ships=(set ship)
  ^-  tang
  %+  turn  ~(tap in ships)
  |=  who=ship
  ^-  tank
  :-  %leaf
  "%gw-btc: {(scow %p who)} published itself on chain; now PUBLIC, permanently"
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
