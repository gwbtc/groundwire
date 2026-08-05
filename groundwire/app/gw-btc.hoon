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
::  Jael stores the point and promotes the comet.  A %jael-anew poke
::  (our own comet asking for a fresh attestation) is answered only when
::  our indexed confidential tip still matches the last verified pass;
::  extending that pass with new custody entries is not implemented yet.
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
::  $gw-state
::
::    .indexing and .best are the two INDEPENDENT readiness signals, and
::    keeping them apart is load-bearing:
::
::      .indexing -- the PUBLIC index has been bootstrapped (an operator
::      chose a start height with %urb-start-indexing / %gw-index-from).
::      It gates only the block scanner's timer.
::
::      .best -- the light client is usable: our persistent /best-block
::      subscription has reported a chain tip.  It gates only the
::      confidential verifier, which needs a tip to scan up to.
::
::    A light-client-only deployment that never bootstraps a public index
::    still verifies confidential comets, and a node with no confidential
::    traffic still indexes public ones.  Conflating the two (as an
::    earlier revision did) forced operators to poke the public-indexer
::    bootstrap just to turn confidential verification on.
::
::    (.indexing bunts to %.n on purpose -- the bunt of ? is %.y, and an
::    agent that came up believing its index was already bootstrapped
::    would never scan a block.)
::
+$  gw-state
  $:  urb-state=state:urb
      indexing=_|
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
  :_  this
  :~  [%pass /anex %arvo %j %anex /writs]
      [%pass /best-block %agent [our.bowl light-client-agent:lca] %watch /best-block]
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
  `this(state ;;(gw-state nou))
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
    =/  poke  ;;(jael-poke:urb q.vase)
    ?-    -.poke
        %jael-writ
      ::  A prior block result has already proved this is a public spawn and
      ::  an exact sanitized replay is in progress.  Ignore reinsertion until
      ::  that block job resolves.
      ?:  (~(has in publicizing) who.poke)
        `this
      ::  Single-flight per ship: at most one verification job.  A
      ::  duplicate or replacement writ while one is in flight is
      ::  dropped silently -- the peer's retries re-enter after the
      ::  verdict, and the on-chain cost of minting states is the rate
      ::  limit.  No queue, no slot economy.
      ?:  (~(has by inflight) who.poke)
        `this
      ::  A ship whose sponsorship we declined re-attests on every
      ::  retry; short-circuit before spending a full verification on
      ::  it.  Clearing the entry (%gw-sponsor-clear) restores normal
      ::  handling immediately -- the refusal is never sticky in the
      ::  kernel, only here.
      ?:  (~(has in declined) who.poke)
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
        `this
      ::  Drop silently until the LIGHT CLIENT is usable; readiness is
      ::  infrastructure, never evidence.  `best` doubles as the chain tip
      ::  the verification strand scans up to: we hold a live /best-block
      ::  subscription, and the strand must NOT re-read that persistent
      ::  endpoint itself (it never kicks).
      ::
      ::  Deliberately NOT gated on .indexing.  Confidential verification
      ::  needs nothing from the public index except the sponsor-existence
      ::  set, which is allowed to be empty; requiring a public-indexer
      ::  bootstrap here would make a light-client-only deployment unable
      ::  to verify anything at all.
      ?~  best
        `this
      =/  job  next-job
      =.  next-job  +(next-job)
      =/  req=inflight-writ  [dom.poke pass.poke u.sat job]
      =.  inflight  (~(put by inflight) who.poke req)
      :_  this
      (verify-cards q.byk.bowl now.bowl who.poke req num.u.best)
    ::
        %jael-anew
      ::  Our own comet asking for a fresh self-attestation.  Return the last
      ::  verified pass only while its recorded tip is still current; custody
      ::  discovery/xtr extension is not implemented yet.  If we do not index
      ::  ourselves, stay silent.
      ::
      ?.  =(dom.poke domain:cc)
        `this
      ?~  pas=(fresh-pass our.bowl)
        `this
      :_  this
      :~  :*  %give  %fact  ~[/writs]
              %anew-response  !>(`anew-response:jael`[dom.poke u.pas])
          ==
      ==
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
    ?:  indexing
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
    ?:  indexing
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
      =/  new-confidential  (~(dif in confidential) public-new)
      =/  new-attested  (drop-attested attested public-new)
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
      =/  new-best=id:block:bc
        ?-  -.upd
          %new             [block-hash.upd block-height.upd]
          %reorg-rollback  [block-hash.upd block-height.upd]
        ==
      `this(best `new-best)
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
::  +fresh-pass: our own current pass, for a %jael-anew refresh
::
++  fresh-pass
  |=  who=ship
  ^-  (unit pass)
  ?.  (~(has in confidential) who)  ~
  ?~  pt=(~(get by unv-ids.urb-state) who)  ~
  ?~  tip=(~(get by attested) who)  ~
  ?.  =(u.tip sont.own.u.pt)  ~
  `pass.net.u.pt
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
