::  app/gevulot.hoon — %gevulot, the identity control pane
::
::    A Landscape-navigable control pane (server-rendered Sail, no glob)
::    for off-chain identity operations on top of the %gw-btc verifier.
::    First feature: SPONSOR-MEDIATED PEER DISCOVERY.  See sur/gevulot.hoon.
::
::    Roles are not exclusive: a ship with a sponsor is a SPONSEE, a ship
::    with sponsees is a SPONSOR, and a comet under a comet is both.
::
::    Sponsee side:
::      - a receive toggle (default OFF); when on, attestations pushed by
::        OUR sponsor are installed into jael on TRUST, and stay on trust
::        until the user asks for the on-chain verification from the pane
::        (or, with the auto-verify toggle on, until our light client is
::        synced).
::      - "distribute my attestation": ask our sponsor to broadcast us.
::      - paste a peer's pass to install it by hand (on trust as well; a
::        paste carries no sponsor's word, so verify it soon).
::      - a live view of every installed peer and whether it is trusted,
::        re-checking, or confirmed on chain, plus the light client's own
::        sync status and a debug dump of %gw-btc's state.
::
::    Sponsor side:
::      - a serving toggle (default ON); when on, a sponsee's %announce is
::        added to the roster and pushed to every current sponsee, and the
::        announcer is sent the whole roster (so a sponsee that announces
::        on boot both joins the broadcast and learns everyone already in).
::      - the sponsor reads each sponsee's pass from its OWN jael (it
::        verified them), so a sponsee can only ask to be broadcast, not
::        dictate what is broadcast.
::
::    PROVISIONAL TRUST.  A trusted install (+poke-gw-install -> %gw-btc's
::    offline %gw-trusted-peer) is a head start, not a verdict.  A recheck
::    sweep (+recheck-sweep, on a behn timer) re-submits every unconfirmed
::    peer through %gw-btc's on-chain %jael-writ once synced.  That path
::    already confirms a valid peer idempotently, demotes a stale one
::    WITHOUT a snub, and catches a forgery -- so re-verification adds the
::    check without changing %gw-btc's verdict path.  .tries bounds the
::    retry so a permanently unconfirmable entry stops re-poking.
::
::    THE SWEEP IS STARTED BY THE USER, not by an install.  Verifying the
::    peers on trust is one liveness walk over every block's compact
::    filter from their oldest custody entry to the tip -- 10,490 blocks
::    and 1 h 52 m with one core pinned on a real comet (2026-10-28, 19
::    peers) -- and nothing waits on it: the peers are already in Jael.
::    So +install-peer arms no timer; the pane explains the walk and its
::    length and offers "Verify on chain now" (%recheck), which runs the
::    sweep until every peer is confirmed or out of tries.  .auto-verify
::    restores the old arm-on-install behaviour for ships nobody watches.
::
/-  gev=gevulot, urb, ord, sa=self-attestation
/+  default-agent, dbug, server, schooner, cc=gw-btc-pass, lsa=self-attestation
/+  mne=mnemonyms
::  gwbtc/mnemonyms@4cbb198 (lib/mnemonyms.hoon, wordlists/english.txt),
::  the same pin as the %dns desk; identical to Causeway's vendored list
/*  english  %txt  /fil/wordlists/english/txt
::
|%
+$  versioned-state  $%(state-0 state-1 state-2 state-3)
+$  state-0
  $:  %0
      receive=?
      serving=?
      roster=(set @p)
      installed=(map @p peer:gev)
  ==
::  state-1: the roster keeps each announced sponsee's PASS.  A sponsor
::  cannot read a sponsee's pass back from jael (%deed is own-only for
::  comets), so the pass a sponsee announces with is what gets broadcast.
::
+$  state-1
  $:  %1
      receive=?
      serving=?
      roster=(map @p pass)
      installed=(map @p peer:gev)
  ==
::  state-2: .share-index.  A sponsor that pushes only its roster gives a
::  sponsee a social index; pushing every PUBLIC identity it has indexed
::  gives it a complete one, which is what lets a sponsee start its own
::  scanner at its spawn block instead of the epoch.
::
::  .installed is provenance and retry bookkeeping ONLY.  The list of
::  peers the pane shows is derived from %gw-btc (its trusted mirror plus
::  its verified index) on every render; this map annotates that list
::  and is never the list itself.  ops/doc/gevulot-state-model.md section 9.
::
+$  state-2
  $:  %2
      receive=?               :: sponsee: accept pushes from our sponsor?  (default no)
      serving=?               :: sponsor: run the distribution service?    (default yes)
      share-index=?           :: sponsor: push my whole public index to announcers? (default yes)
      roster=(map @p pass)    :: sponsor: announced sponsees, each with the pass to push
      installed=(map @p peer:gev) :: sponsee: how each peer was learned, and retry counts
  ==
::  state-3: .auto-verify and .asked.  The on-chain verification of the
::  peers on trust no longer starts when a peer is installed: the user
::  starts it from the pane, which explains what it proves and how long
::  the walk is (see the header).  .auto-verify (default off) restores
::  the arm-on-install behaviour; .asked is when the button was last
::  pressed, for the pane's "asked at".
::
+$  state-3
  $:  %3
      receive=?               :: sponsee: accept pushes from our sponsor?  (default no)
      serving=?               :: sponsor: run the distribution service?    (default yes)
      share-index=?           :: sponsor: push my whole public index to announcers? (default yes)
      auto-verify=?           :: sponsee: start verifying trusted installs by itself once synced? (default no)
      asked=(unit @da)        :: sponsee: when "verify on chain now" was last pressed
      roster=(map @p pass)    :: sponsor: announced sponsees, each with the pass to push
      installed=(map @p peer:gev) :: sponsee: how each peer was learned, and retry counts
  ==
+$  card  card:agent:gall
+$  ready  [synced=? tip=(unit @) indexing=?]  :: shape of %gw-btc /x/ready
+$  scan                                       :: shape of %gw-btc /x/scan
  $:  cursor=@ud
      epoch=@ud
      tip=(unit @ud)
      confirmations=@ud
      batch=@ud
      indexing=?
      points=@ud
      origin=index-origin
  ==
+$  index-origin                               :: %gw-btc's $index-origin
  $:  mode=?(%unstarted %pending %from-spawn %from-height %from-epoch)
      start=@ud
      decided-by=?(%causeway %user %default)
      at=@da
  ==
+$  walks  (map @p [job=@ud top=@ud])          :: shape of %gw-btc /x/inflight-detail
--
::
=|  state-3
=*  state  -
%-  agent:dbug
^-  agent:gall
=<
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
    hc    ~(. +> bowl)
::
++  on-init
  ^-  (quip card _this)
  ::  a loobean bunts to %.y, so every "default no" flag is set here
  :_  this(serving %.y, auto-verify %.n)
  :~  :*  %pass  /eyre/connect  %arvo  %e
          %connect  `/apps/gevulot  dap.bowl
      ==
  ==
::
++  on-save  !>(state)
++  on-load
  |=  old=vase
  ^-  (quip card _this)
  =/  ver  !<(versioned-state old)
  ?-  -.ver
    %3  `this(state ver)
    ::  2 -> 3: verification is started from the pane from now on; a
    ::  sweep timer already armed by the old code still fires and runs
    ::  (the sweep is the same code the button runs), so a ship upgraded
    ::  mid-sweep finishes it.
    %2  `this(state [%3 receive.ver serving.ver share-index.ver %.n ~ roster.ver installed.ver])
    ::  1 -> 3: share the index by default.
    %1  `this(state [%3 receive.ver serving.ver %.y %.n ~ roster.ver installed.ver])
    ::  0 -> 3: the old roster held ships without passes, and there is no
    ::  way to recover them (see state-1).  Start it empty; every sponsee
    ::  re-announces on boot (boot.sh's %distribute) and can from the pane.
    %0  `this(state [%3 receive.ver serving.ver %.y %.n ~ ~ installed.ver])
  ==
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?+    mark  (on-poke:def mark vase)
      ::  the eyre control-pane request
      %handle-http-request
    =^  cards  state
      (handle-http:hc !<([@ta =inbound-request:eyre] vase))
    [cards this]
      ::  everything else rides %noun: a LOCAL $action (from the UI, the
      ::  dojo, or Causeway's boot handoff) when src is us, a remote $wire
      ::  (from a sponsor or sponsee) otherwise.  The two tag-unions do not
      ::  overlap, and the src split is the authorization.
      %noun
    ?:  =(our.bowl src.bowl)
      =^  cards  state  (handle-action:hc ;;(action:gev q.vase))
      [cards this]
    =^  cards  state  (handle-wire:hc src.bowl ;;(wire:gev q.vase))
    [cards this]
  ==
::
++  on-watch
  |=  =path
  ^-  (quip card _this)
  ?+    path  (on-watch:def path)
    [%http-response *]  `this
  ==
::
++  on-agent  on-agent:def
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card _this)
  ?+    sign-arvo  (on-arvo:def wire sign-arvo)
      [%eyre %bound *]
    ?:  accepted.sign-arvo  `this
    %-  (slog leaf+"%gevulot: eyre refused to bind /apps/gevulot" ~)
    `this
  ::
      ::  the recheck sweep timer: re-verify trusted installs on chain.
      [%behn %wake *]
    ?.  ?=([%recheck ~] wire)  (on-arvo:def wire sign-arvo)
    =^  cards  state  recheck-sweep:hc
    [cards this]
  ==
::
++  on-peek   on-peek:def
++  on-leave  on-leave:def
++  on-fail   on-fail:def
--
::
::  ---------------------------------------------------------------------
::  helper core
::  ---------------------------------------------------------------------
::
|_  =bowl:gall
::
++  max-tries  3
::
::  How this pane names a ship.  A comet is shown as its mnemonym
::  foreshortened to four words (`.foo.bar..baz.bat`), anything else as
::  its @p.  +name-span carries the full nym in data-n and the @p in
::  data-p and the hover text: a click copies the full nym, and the
::  pane's "Show @p" toggle swaps the @p in to be read and copied.
::
++  me  ~(. me:mne [.y 128 english])
::
++  name-span
  |=  who=@p
  ^-  manx
  =/  p=tape  (scow %p who)
  ?.  =(%pawn (clan:title who))
    ;span.nm(title p, data-n p, data-p p): {p}
  =/  n=@t  (de:ship:me who)
  ;span.nm(title p, data-n (trip n), data-p p)
    ;span.n: {(trip (fore:me n))}
    ;span.p: {p}
  ==
::
::  +comet-cite: a peer's short @p (`~sampel_dozzod`).  For a comet it is
::  read back out of the nym with +en:ship, which also checks the nym's
::  checksum, so the two names in a list row are shown to agree.
::
++  comet-cite
  |=  who=@p
  ^-  tape
  ?.  =(%pawn (clan:title who))  (cite:title who)
  (cite:title (en:ship:me (de:ship:me who)))
::
::  +name-key: the text a ship sorts by -- a comet's full mnemonym,
::  anything else its @p.  A nym opens with `.` and a @p with `~`, so
::  comets come first.
::
++  name-key
  |=  who=@p
  ^-  @t
  ?.  =(%pawn (clan:title who))  (scot %p who)
  (de:ship:me who)
::
::  +sort-ships: ships in alphabetical order of +name-key.  Each key is
::  computed once, not once per comparison.
::
++  sort-ships
  |=  ships=(list @p)
  ^-  (list @p)
  %+  turn
    %+  sort  (turn ships |=(who=@p [(name-key who) who]))
    |=([a=[key=@t @p] b=[key=@t @p]] (aor key.a key.b))
  |=([@t who=@p] who)
::
::  +our-sponsor: the sponsor we COMMITTED, if any.
::
::    Two sources, in order.  jael's own point, when it names one (a comet
::    with no sponsor projects to itself, which we treat as "none").  But
::    jael does not yet record a Groundwire comet's committed sponsor:
::    dawn writes the classic parent star, so a freshly booted comet's
::    `sein` is itself.  The sponsor it actually committed is in the
::    snapshot of the custody log its own baked pass carries (the xtr tail
::    finalize wrote into the boot feed) -- so fall back to reading that,
::    from the pass jael holds for us at our current life.  Without this a
::    fresh comet showed "SPONSOR none" and %distribute was a silent no-op,
::    so its sponsor never learned to push peers to it (first real mint,
::    2026-09-10).
::
++  our-sponsor
  ^-  (unit @p)
  =/  s=@p
    .^(@p %j /(scot %p our.bowl)/sein/(scot %da now.bowl)/(scot %p our.bowl))
  ?.  =(our.bowl s)  `s
  =/  pas=(unit pass)  our-pass
  ?~  pas  ~
  =/  res
    %-  mule  |.
    =/  sat  (from-xtr:lsa our.bowl u.pas)
    ?~  sat  ~
    ?~  chain.u.sat  ~
    =/  last=custody-entry:sa  (rear chain.u.sat)
    ?~  opening.last  ~
    sponsor.snapshot.u.opening.last
  ?:  ?=(%| -.res)  ~
  p.res
::
::  +our-pass: OUR attestation pass at our current life -- the pass with
::  the xtr custody log that finalize baked into the boot feed, which is
::  what ames serves and what we announce to our sponsor.  jael's %deed
::  serves this for `our` at any life it holds.
::
::  SCRY DISCIPLINE, learned the hard way: on this kernel a scry that
::  answers [~ ~] or blocks does NOT come back through +mule as %| -- it
::  blocks the whole event (an eyre request 500s with the scry path; a
::  remote poke nacks).  So every scry here must be one the vane always
::  answers for the path we ask: jael's %life and %deed for our own ship,
::  and %gw-btc's whole-map paths (/sponsees, /points, ...).  Never scry
::  another comet's %deed (own-only) or /point/<ship> (withheld when the
::  ship is confidential or unknown).
::
++  our-pass
  ^-  (unit pass)
  =/  res
    %-  mule  |.
    =/  lyf=@ud
      .^(@ud %j /(scot %p our.bowl)/life/(scot %da now.bowl)/(scot %p our.bowl))
    ;;  [lyf=@ud =pass rest=*]
    .^(* %j /(scot %p our.bowl)/deed/(scot %da now.bowl)/(scot %p our.bowl)/(scot %ud lyf))
  ?:  ?=(%| -.res)  ~
  `pass.p.res
::
::  +fingerprint: the @p a pass is the attestation OF.  A pass binds its
::  own identity (the comet's @p is derived from it), so this is how a
::  hand-pasted pass names its ship and how a sponsor checks that an
::  announcing sponsee sent its own pass and not someone else's.
::
++  fingerprint
  |=  =pass
  ^-  @p
  `@p`fig:ex:(com:nu:cric:crypto pass)
::
::  +our-sponsees: the comets %gw-btc says we sponsor, with each life
::  (so we can read its pass from jael's %deed, which is keyed by life).
::
++  our-sponsees
  ^-  (map @p @ud)
  =/  res
    %-  mule  |.
    ;;  (map @p [life=@ud since=@da])
    .^(* %gx /(scot %p our.bowl)/gw-btc/(scot %da now.bowl)/sponsees/noun)
  ?:  ?=(%| -.res)  ~
  (~(run by p.res) |=([l=@ud *] l))
::
::  +gw-ready: %gw-btc's readiness -- [synced tip indexing].  A blocked or
::  absent scry reads as "not synced", the fail-closed direction.
::
++  gw-ready
  ^-  ready
  =/  res
    %-  mule  |.
    ;;  ready
    .^(* %gx /(scot %p our.bowl)/gw-btc/(scot %da now.bowl)/ready/noun)
  ?:  ?=(%| -.res)  [%.n ~ %.n]
  p.res
::
::  +gw-confidential: the identities %gw-btc has VERIFIED on chain as
::  confidential comets.  Presence here is a positive on-chain verdict.
::
++  gw-confidential
  ^-  (set @p)
  =/  res
    %-  mule  |.
    ;;  (set @p)
    .^(* %gx /(scot %p our.bowl)/gw-btc/(scot %da now.bowl)/confidential/noun)
  ?:  ?=(%| -.res)  ~
  p.res
::
::  +gw-attested: map of verified confidential ship -> the satpoint it last
::  attested to.  Debug surface only.
::
++  gw-attested
  ^-  (map @p sont:ord)
  =/  res
    %-  mule  |.
    ;;  (map @p sont:ord)
    .^(* %gx /(scot %p our.bowl)/gw-btc/(scot %da now.bowl)/attested/noun)
  ?:  ?=(%| -.res)  ~
  p.res
::
::  +gw-inflight: ships with a verification in flight in %gw-btc.  Its
::  verifier is single-flight per ship, so the sweep skips these.
::
++  gw-inflight
  ^-  (set @p)
  =/  res
    %-  mule  |.
    ;;  (set @p)
    .^(* %gx /(scot %p our.bowl)/gw-btc/(scot %da now.bowl)/inflight/noun)
  ?:  ?=(%| -.res)  ~
  p.res
::
::  +gw-scan: the public-index scanner's progress.  A %gw-btc without the
::  path (older desk) reads as "nothing scanned", which the pane shows as
::  "no data" rather than a wrong number.
::
++  gw-scan
  ^-  (unit scan)
  =/  res
    %-  mule  |.
    ;;  scan
    .^(* %gx /(scot %p our.bowl)/gw-btc/(scot %da now.bowl)/scan/noun)
  ?:  ?=(%| -.res)  ~
  `p.res
::
::  +gw-walks: for each verification in flight, the height its liveness
::  walk started at -- what "re-checking" is actually waiting on.
::
++  gw-walks
  ^-  walks
  =/  res
    %-  mule  |.
    ;;  walks
    .^(* %gx /(scot %p our.bowl)/gw-btc/(scot %da now.bowl)/inflight-detail/noun)
  ?:  ?=(%| -.res)  ~
  p.res
::
::  +gw-trusted: peers %gw-btc put into Jael on trust and has not verified
::  yet (ship -> when).  With +gw-confidential and +gw-public this is the
::  whole set of identities this ship holds through %gw-btc -- the list
::  the pane derives from.  Whole-map path; always answers.
::
++  gw-trusted
  ^-  (map @p @da)
  =/  res
    %-  mule  |.
    ;;  (map @p @da)
    .^(* %gx /(scot %p our.bowl)/gw-btc/(scot %da now.bowl)/trusted/noun)
  ?:  ?=(%| -.res)  ~
  p.res
::
::  +gw-public: every PUBLIC identity in %gw-btc's index, with the pass
::  its point carries -- what a sponsor pushes to an announcing sponsee
::  when .share-index is on.
::
++  gw-public
  ^-  (map @p pass)
  =/  res
    %-  mule  |.
    %-  ~(run by ;;((map @p point:urb) .^(* %gx /(scot %p our.bowl)/gw-btc/(scot %da now.bowl)/points/noun)))
    |=(p=point:urb pass.net.p)
  ?:  ?=(%| -.res)  ~
  p.res
::
::  +gw-point-public: does %gw-btc hold a PUBLIC on-chain point for .who?
::  (Confidential peers are withheld from /points and show up in
::  +gw-confidential instead.)  Read as membership in the whole /points
::  map, which always answers; /point/<ship> is [~ ~] for a confidential
::  or unknown ship and that blocks the event (see +our-pass) -- it took
::  the pane down the moment a fresh comet learned its sponsor.
::
++  gw-point-public
  |=  who=@p
  ^-  ?
  =/  res
    %-  mule  |.
    ;;  (map @p *)
    .^(* %gx /(scot %p our.bowl)/gw-btc/(scot %da now.bowl)/points/noun)
  ?:  ?=(%| -.res)  %.n
  (~(has by p.res) who)
::
::  +peer-confirmed: has %gw-btc verified .who on chain, publicly or
::  confidentially?
::
++  peer-confirmed
  |=  [who=@p conf=(set @p)]
  ^-  ?
  ?|  (~(has in conf) who)
      (gw-point-public who)
  ==
::
::  +peer-status: the UI/driver state of an installed peer.
::    %confirmed -- %gw-btc has verified it on chain.
::    %syncing   -- our light client is not synced; we trust it for now.
::    %checking  -- synced; ITS verification is running right now.
::    %queued    -- synced; waiting for the next recheck sweep.  Used to
::                  be shown as "re-checking" too, which read as if this
::                  peer's check were the slow thing.
::    %failed    -- synced and exhausted; never confirmed (stale or forged).
::
++  peer-status
  |=  [who=@p conf=(set @p) synced=? infl=(set @p) p=peer:gev]
  ^-  ?(%confirmed %syncing %checking %queued %failed)
  ?:  (peer-confirmed who conf)  %confirmed
  ?.  synced  %syncing
  ?:  (~(has in infl) who)  %checking
  ?:  (gte tries.p max-tries)  %failed
  %queued
::
::  +poke-gw-install: hand a trusted peer pass to %gw-btc for an
::  offline (no-fetch, never-snub) install into jael.
::
++  poke-gw-install
  |=  [who=@p =pass]
  ^-  card
  [%pass /gw-install %agent [our.bowl %gw-btc] %poke %noun !>([%gw-trusted-peer who pass])]
::
::  +poke-gw-verify: hand a peer pass to %gw-btc's ON-CHAIN verifier, the
::  same %jael-writ path a peer's own packet takes.  This is the double
::  check of a trusted install: valid -> confirmed, stale -> demoted (no
::  snub), forged -> snubbed.  Only meaningful once %gw-btc is synced.
::  Writs that arrive together are verified together: %gw-btc walks the
::  chain ONCE for every writ it holds when its launch timer fires (see
::  .batch in app/gw-btc), so a sweep sends all of its peers at once.
::
++  poke-gw-verify
  |=  [who=@p =pass]
  ^-  card
  =/  pk=jael-poke:urb  [%jael-writ domain:cc who pass]
  [%pass /gw-verify %agent [our.bowl %gw-btc] %poke %noun !>(pk)]
::
::  +recheck-timer: schedule the next recheck sweep.
::
++  recheck-timer
  |=  dly=@dr
  ^-  card
  [%pass /recheck %arvo %b %wait (add dly now.bowl)]
::
::  +wire-to: send a $wire to another ship's %gevulot.
::
++  wire-to
  |=  [who=@p =wire:gev]
  ^-  card
  [%pass /wire %agent [who %gevulot] %poke %noun !>(wire)]
::
::  +install-peer: record a peer and get it into jael ON TRUST, always --
::  the peer is reachable at once, whatever the light client is doing.
::  The on-chain verification is the user's to start (the pane's "Verify
::  on chain now", %recheck), so this arms nothing.  With .auto-verify on
::  it arms the recheck sweep instead, which verifies this peer and every
::  other unconfirmed one once the light client is synced, re-arming
::  until all are confirmed or out of tries.  (It used to send the
::  verify writ directly when already synced; the sweep does that within
::  two minutes now, and the verifier being single-flight per ship often
::  dropped the direct writ anyway.)  No scry here: nothing about the
::  install depends on the sync state.
::
++  install-peer
  |=  [who=@p =pass via=provenance:gev]
  ^-  (quip card _state)
  =/  entry=peer:gev  [pass via now.bowl 0]
  :_  state(installed (~(put by installed) who entry))
  :-  (poke-gw-install who pass)
  ?.  auto-verify  ~
  ~[(recheck-timer ~m2)]
::
::  =====================================================================
::  local actions (UI / dojo / Causeway)
::  =====================================================================
::
++  handle-action
  |=  act=action:gev
  ^-  (quip card _state)
  ?-    -.act
      %set-receive      `state(receive on.act)
      %set-serving      `state(serving on.act)
      %set-share-index  `state(share-index on.act)
      ::  Turning auto-verify ON does not start anything by itself: the
      ::  next install arms the sweep, or the user presses the button.
      %set-auto-verify  `state(auto-verify on.act)
  ::
      %distribute
    ::  ask our sponsor to broadcast us, sending the attestation we want
    ::  broadcast.  Harmless if we have no sponsor (or, absurdly, no pass).
    =/  spo=(unit @p)  our-sponsor
    ?~  spo  `state
    =/  pas=(unit pass)  our-pass
    ?~  pas  `state
    :_  state
    ~[(wire-to u.spo %announce u.pas)]
  ::
      %withdraw
    =/  spo=(unit @p)  our-sponsor
    ?~  spo  `state
    :_  state
    ~[(wire-to u.spo %withdraw ~)]
  ::
      %ingest-peer
    ::  install a hand-pasted attestation.  The peer @p is the pass's own
    ::  fingerprint; a wrong paste is a silent no-op in %gw-btc.  A paste
    ::  carries no sponsor's word, so it is verified on chain as soon as we
    ::  can rather than merely trusted.
    (install-peer (fingerprint pass.act) pass.act %paste)
  ::
      %forget-peer
    ::  One operation, every store: %gw-btc drops its trusted mirror and
    ::  its index entry and tells Jael; we drop our annotation.  Forgetting
    ::  used to be ours alone ("we do not un-tell jael") -- a peer list
    ::  that disagreed with Jael by design.
    :_  state(installed (~(del by installed) who.act))
    :~  :*  %pass  /gw-forget  %agent  [our.bowl %gw-btc]  %poke
            %noun  !>([%gw-forget-peer who.act])
        ==
    ==
  ::
      %recheck
    ::  THE button: reset the attempt counters and verify everything on
    ::  chain now.  The sweep re-arms itself until every peer is confirmed
    ::  or out of tries (and waits for the light client if it is not
    ::  synced yet), so one press is one complete verification.
    =.  state
      %=  state
        asked      `now.bowl
        installed  (~(run by installed) |=(p=peer:gev p(tries 0)))
      ==
    recheck-sweep
  ::
      ::  The user decides where the public index starts.  %gw-btc resolves
      ::  the height (own spawn block, or the epoch) and ignores it once an
      ::  index exists, so a repeated click cannot restart anything.
      %index-start
    :_  state
    :~  :*  %pass  /gw-index  %agent  [our.bowl %gw-btc]  %poke
            %noun  !>([%index-origin mode.act 0 %user])
        ==
    ==
  ::
      ::  Re-read every block from the epoch.  Keeps every verified
      ::  identity, every trusted install and everything in Jael: the
      ::  scanner's cursor is the only thing that moves.  The epoch is
      ::  read from %gw-btc so the two can never disagree.
      %index-rewind
    =/  sc  gw-scan
    ?~  sc  `state
    :_  state
    :~  :*  %pass  /gw-index  %agent  [our.bowl %gw-btc]  %poke
            %noun  !>([%gw-index-rewind epoch.u.sc])
        ==
    ==
  ==
::
::  +recheck-sweep: re-verify unconfirmed installs on chain, all at once
::
::    One %jael-writ per unconfirmed peer not already in flight, all in
::    one event, so %gw-btc verifies them in one batch (one chain walk).
::    The verifier is single-flight per SHIP, so a peer still in flight
::    is left for the next sweep.  Re-arms until every peer is confirmed
::    or has exhausted its tries.
::
++  recheck-sweep
  ^-  (quip card _state)
  ::  nothing installed, nothing to ask %gw-btc about (and no scry: a
  ::  %gx scry needs the live case, which a test bowl does not have)
  ?:  =(~ installed)  `state
  =/  rdy   gw-ready
  =/  conf  gw-confidential
  =/  todo=(list [who=@p p=peer:gev])
    %+  skim  ~(tap by installed)
    |=  [who=@p p=peer:gev]
    ?&  !(peer-confirmed who conf)
        (lth tries.p max-tries)
    ==
  ?~  todo  `state                       :: all confirmed or given up; stop
  ?.  synced.rdy                         :: cannot verify yet; wait
    :_  state
    ~[(recheck-timer ~m5)]
  =/  infl  gw-inflight
  =/  go=(list [who=@p p=peer:gev])
    %+  skip  `(list [who=@p p=peer:gev])`todo
    |=([who=@p *] (~(has in infl) who))
  ?~  go                                 :: all of them running; wait
    :_  state
    ~[(recheck-timer ~m1)]
  =.  installed
    %-  ~(gas by installed)
    (turn go |=([who=@p p=peer:gev] [who p(tries +(tries.p))]))
  :_  state
  %+  snoc
    (turn go |=([who=@p p=peer:gev] (poke-gw-verify who pass.p)))
  (recheck-timer ~m2)
::
::  =====================================================================
::  inter-ship wire (sponsor <-> sponsee)
::  =====================================================================
::
++  handle-wire
  |=  [src=@p =wire:gev]
  ^-  (quip card _state)
  ?-    -.wire
      ::  A SPONSEE asks us (its sponsor) to broadcast it, with the pass to
      ::  broadcast.  Ignore unless we are serving, the pass is src's OWN
      ::  (its fingerprint is src -- ames authenticated src, so a sponsee
      ::  can only ever announce itself), AND src really is a current
      ::  sponsee of ours per %gw-btc's on-chain view -- the sponsor is the
      ::  source of truth, a stranger cannot inject itself.  Then add it to
      ::  the roster, push it to every current sponsee, and send src the
      ::  whole roster so a just-booted sponsee learns everyone already in.
      %announce
    ?.  serving  `state
    ?.  =(src (fingerprint pass.wire))  `state
    ?.  (~(has by our-sponsees) src)  `state
    =.  roster  (~(put by roster) src pass.wire)
    :_  state
    %+  weld  (broadcast-announce src pass.wire)
    ::  With .share-index, the newcomer also gets every PUBLIC identity
    ::  we have indexed, each as its own attestation to verify -- so its
    ::  index can start at its own spawn block and still be complete.
    ?.  share-index  ~
    %+  murn  ~(tap by gw-public)
    |=  [who=@p =pass]
    ?:  |(=(who src) =(who our.bowl))  ~
    `(wire-to src %peer who pass)
  ::
      %withdraw
    `state(roster (~(del by roster) src))
  ::
      ::  OUR sponsor pushes us a peer to install.  Accept iff we opted in
      ::  AND src is actually our sponsor.  +install-peer verifies it on
      ::  chain now if we can, or trusts it and rechecks once synced.
      ::  Idempotent; a peer we already hold is just refreshed.
      %peer
    ?.  receive  `state
    =/  spo=(unit @p)  our-sponsor
    ?~  spo  `state
    ?.  =(src u.spo)  `state
    (install-peer who.wire pass.wire %sponsor)
  ==
::
::  +broadcast-announce: cards to send when .newcomer announces with .pas:
::    - push .newcomer's pass to every OTHER current sponsee;
::    - push every other roster member's pass to .newcomer.
::  Only current sponsees get pushed; a roster member that is no longer
::  one (rekeyed away, breached) is skipped, not forgotten.
::
++  broadcast-announce
  |=  [newcomer=@p pas=pass]
  ^-  (list card)
  =/  spees  our-sponsees                :: (map @p @ud) — @p -> life
  ;:  weld
    ::  newcomer -> every other current sponsee
    %+  murn  ~(tap in ~(key by spees))
    |=  s=@p
    ?:  =(s newcomer)  ~
    `(wire-to s %peer newcomer pas)
  ::
    ::  every other roster member -> newcomer
    %+  murn  ~(tap by roster)
    |=  [m=@p mp=pass]
    ?:  =(m newcomer)  ~
    ?.  (~(has by spees) m)  ~
    `(wire-to newcomer %peer m mp)
  ==
::
::  =====================================================================
::  the control pane (server-rendered Sail, no JS glob)
::  =====================================================================
::
++  handle-http
  |=  [eyre-id=@ta =inbound-request:eyre]
  ^-  (quip card _state)
  =/  req  request.inbound-request
  =/  ,request-line:server  (parse-request-line:server url.req)
  =+  send=(cury response:schooner eyre-id)
  ?.  authenticated.inbound-request
    :_  state
    (send [403 ~ [%plain "gevulot: log in with +code first"]])
  ?+    method.req
    :_  state
    (send [405 ~ [%plain "gevulot: method not allowed"]])
  ::
      %'GET'
    :_  state
    (send [200 ~ [%manx render-page]])
  ::
      %'POST'
    ::  a plain <form> POST: application/x-www-form-urlencoded body.  Parse
    ::  it as a query string, run the action, then 303 back to GET so a
    ::  refresh does not resubmit.
    =/  quay=(list [key=@t value=@t])
      ?~  body.req  ~
      (form-quay q.u.body.req)
    =^  cards  state  (handle-action (action-from-quay quay))
    =.  cards  (weld cards (send [303 ['location' '/apps/gevulot']~ [%plain ""]]))
    [cards state]
  ==
::
::  +form-quay: parse a urlencoded form body into a quay.
::
++  form-quay
  |=  body=@t
  ^-  (list [@t @t])
  (fall (rush body yquy:de-purl:html) ~)
::
::  +action-from-quay: map a decoded form into an $action.  Unknown / empty
::  forms become a harmless no-op (%set-serving keeping the current value).
::
++  action-from-quay
  |=  q=(list [key=@t value=@t])
  ^-  action:gev
  =/  act  (~(gut by (malt q)) 'act' '')
  ?:  =(act 'set-receive')   [%set-receive =('true' (~(gut by (malt q)) 'on' ''))]
  ?:  =(act 'set-serving')   [%set-serving =('true' (~(gut by (malt q)) 'on' ''))]
  ?:  =(act 'set-share-index')  [%set-share-index =('true' (~(gut by (malt q)) 'on' ''))]
  ?:  =(act 'set-auto-verify')  [%set-auto-verify =('true' (~(gut by (malt q)) 'on' ''))]
  ?:  =(act 'distribute')    [%distribute ~]
  ?:  =(act 'withdraw')      [%withdraw ~]
  ?:  =(act 'recheck')       [%recheck ~]
  ?:  =(act 'forget')        [%forget-peer (slav %p (~(gut by (malt q)) 'who' '~zod'))]
  ?:  =(act 'ingest')
    =/  raw  (~(gut by (malt q)) 'pass' '')
    [%ingest-peer (slav %ux raw)]
  ?:  =(act 'index-spawn')   [%index-start %from-spawn]
  ?:  =(act 'index-epoch')   [%index-start %from-epoch]
  ::  The scary one.  A plain click is not enough: the form makes the
  ::  user type the epoch height, and anything else is a no-op.
  ?:  =(act 'index-rewind')
    ?.  =('963104' (~(gut by (malt q)) 'confirm' ''))  [%set-serving serving]
    [%index-rewind ~]
  [%set-serving serving]
::
::  +render-page: the whole control pane as a $manx.
::
++  render-page
  ^-  manx
  =/  spo    our-sponsor
  =/  spees  our-sponsees
  =/  rdy    gw-ready
  =/  conf   gw-confidential
  =/  am-sponsor  ?=(^ ~(tap by spees))
  ;html
    ;head
      ;meta(charset "utf-8");
      ;title: Gevulot
      ;style: {style}
    ==
    ;body
      ;div.wrap
        ;div.top
          ;h1: Gevulot
          ;button#nmt.ghost.small(type "button", title "Swap every name on this page between its mnemonym and its @p"): Show @p
        ==
        ;p.sub: identity control pane
        ;+  (status-card rdy spo gw-scan gw-walks)
        ;+  (index-card gw-scan)
        ;*  ?~  spo  ~
            :~  (sponsee-section u.spo conf rdy gw-inflight)
            ==
        ;*  ?.  am-sponsor  ~
            :~  (sponsor-section spees)
            ==
        ;+  (debug-card rdy conf gw-inflight gw-attested spees)
      ==
      ;script: {name-toggle}
    ==
  ==
::
++  status-card
  |=  [rdy=ready spo=(unit @p) sc=(unit scan) wk=walks]
  ^-  manx
  ;div.card.status
    ;div.statrow
      ;span.k: you
      ;span.v
        ;+  (name-span our.bowl)
      ==
    ==
    ;div.statrow
      ;span.k: your sponsor
      ;+  ?~  spo  ;span.v: none
          ;span.v
            ;+  (name-span u.spo)
          ==
    ==
    ;div.statrow
      ;span.k: light client
      ;+  (sync-span rdy)
    ==
    ;div.statrow
      ;span.k: public index
      ;+  (index-span sc)
    ==
    ;div.statrow
      ;span.k: verifying
      ;+  (walks-span rdy wk)
    ==
  ==
::
::  +index-span: where %gw-btc's public-index scanner is.  Two different
::  things run after "synced": this scan (every full block from the epoch,
::  needed only to DISCOVER public names) and per-peer verification.  The
::  scan is the one that pins the CPU for hours on a fresh comet, and it
::  used to be invisible behind the word "indexing".
::
::  +when-txt: a @da as "2026-09-11 03:06" -- the pane's dates are for a
::  human glancing at provenance, not for parsing.
::
++  when-txt
  |=  da=@da
  ^-  tape
  =/  d  (yore da)
  =/  pad  |=(n=@ud ?:((lth n 10) "0{(scow %ud n)}" (scow %ud n)))
  "{(a-co:co y.d)}-{(pad m.d)}-{(pad d.t.d)} {(pad h.t.d)}:{(pad m.t.d)}"
::
::  +index-span: the public index's row.  A short status pill, then the
::  provenance as a quiet second line, then (while catching up) a bar and
::  the numbers -- each its own line, so nothing has to fit in one.
::
++  index-span
  |=  sc=(unit scan)
  ^-  manx
  ?~  sc
    ;div.v.idx
      ;span.st.st-wait: no data (older %gw-btc)
    ==
  =/  org  origin.u.sc
  =/  who=tape
    ?-  decided-by.org
      %causeway  "set by Causeway at boot"
      %user      "chosen by you"
      %default   "recorded at upgrade"
    ==
  =/  when=tape  (when-txt at.org)
  ?:  ?=(%unstarted mode.org)
    ;div.v.idx
      ;span.st.st-wait: not started
      ;span.idx-meta: waiting for the light client
    ==
  ?:  ?=(%pending mode.org)
    ;div.v.idx
      ;span.st.st-bad: pending your decision
      ;span.idx-meta: since {when} -- choose in the Public index card below
    ==
  =/  origin-txt=tape
    ?-  mode.org
      %from-spawn   "from my spawn block {(scow %ud start.org)}"
      %from-height  "from block {(scow %ud start.org)}"
      %from-epoch   "from the epoch, block {(scow %ud start.org)}"
    ==
  =/  meta=tape  "{origin-txt} · {who}, {when}"
  ?~  tip.u.sc
    ;div.v.idx
      ;span.st.st-wait: waiting for a chain tip
      ;span.idx-meta: {meta}
    ==
  =/  settled=@ud  (sub u.tip.u.sc (min u.tip.u.sc confirmations.u.sc))
  ?.  indexing.u.sc
    ;div.v.idx
      ;span.st.st-wait: starts when the light client is synced
      ;span.idx-meta: {meta}
    ==
  ?:  (gte cursor.u.sc settled)
    ;div.v.idx
      ;span.st.st-ok: complete at block {(scow %ud cursor.u.sc)} · {(scow %ud points.u.sc)} identit(ies) indexed
      ;span.idx-meta: {meta}
    ==
  =/  left=@ud  (sub settled cursor.u.sc)
  =/  done=@ud  (sub cursor.u.sc (min cursor.u.sc start.org))
  =/  total=@ud  (add done left)
  =/  pct=@ud  ?:(=(0 total) 0 (div (mul 100 done) total))
  ;div.v.idx
    ;span.st.st-wait: scanning · block {(scow %ud cursor.u.sc)} of {(scow %ud settled)}
    ;div.bar
      ;div.bar-fill(style "width:{(scow %ud pct)}%");
    ==
    ;span.idx-meta
      ; {(scow %ud left)} blocks to go · {(scow %ud done)} done ({(scow %ud pct)}%) ·
      ; {(scow %ud batch.u.sc)} per batch · the ship is slow until this ends
    ==
    ;span.idx-meta: {meta}
  ==
::
::  +epoch-blocks / +epoch-hours: what a scan from the epoch costs today,
::  read from the scan peek rather than hardcoded.  The rate is the one
::  measured on ~ponnyd (a 2-vCPU droplet, serf pinned) on 2026-09-16
::  with the concurrent batch fetch: 656 blocks an hour.  The tip moves
::  about 144 blocks a day, so the figure grows a quarter hour a day --
::  which is why it is computed, not written down.
::
++  epoch-rate  650
++  epoch-blocks
  |=  sc=scan
  ^-  (unit @ud)
  ::  no tip, or a tip the light client has not carried past the epoch
  ::  yet (a fresh ship reports 0): the count is unknown, not zero.
  ?~  tip.sc  ~
  ?:  (lte u.tip.sc epoch.sc)  ~
  `(sub u.tip.sc epoch.sc)
::
++  epoch-hours
  |=  sc=scan
  ^-  tape
  =/  blocks  (epoch-blocks sc)
  ?~  blocks  "several hours"
  =/  hours  (div (add u.blocks (dec epoch-rate)) epoch-rate)
  ?:  (lth hours 2)  "about an hour"
  "about {(scow %ud hours)} hours"
::
::  The rewind form's whole cost phrase.
++  epoch-cost
  |=  sc=scan
  ^-  tape
  =/  blocks  (epoch-blocks sc)
  =/  what
    ?~  blocks  "every block since the epoch"
    "{(scow %ud u.blocks)} blocks to re-read"
  :(weld what ", " (epoch-hours sc) " at full CPU on a 2-vCPU droplet, and a quarter hour more each day")
::
::  +index-card: the decisions about the public index that only a human
::  may take.  Shown whenever the index is pending, plus the rewind form
::  whenever an index exists.  Nothing here happens on its own.
::
++  index-card
  |=  sc=(unit scan)
  ^-  manx
  ?~  sc
    ;div;
  =/  org  origin.u.sc
  ;div.card
    ;h2: Public index
    ;p.hint
      ; Two different syncs run on this ship.  The LIGHT CLIENT downloads
      ; block headers and compact filters (minutes); it is what the
      ; on-chain verification of your peers reads, and that verification
      ; is a walk of its own that you start from the "As a sponsee" card.
      ; The PUBLIC INDEX reads every full block from its start height to
      ; discover public names nobody told you about and to follow known
      ; names' moves; it is slow (a few seconds per block on a 2-vCPU
      ; droplet) and pins the ship while it catches up.
    ==
    ;*  ?.  ?=(%pending mode.org)  ~
        :_  ~
        ;div.pending
          ;p.warn
            ; No start was chosen for the public index.  Nothing is being
            ; indexed until you choose.  From your spawn block is the
            ; normal choice: your sponsor pushes you everything older.
          ==
          ;div.row
            ;form(method "post", action "/apps/gevulot")
              ;input(type "hidden", name "act", value "index-spawn");
              ;button(type "submit"): Index from my spawn block
            ==
            ;form(method "post", action "/apps/gevulot")
              ;input(type "hidden", name "act", value "index-epoch");
              ;button.ghost(type "submit"): Index from the epoch ({(epoch-hours u.sc)} at full CPU)
            ==
          ==
        ==
    ;*  ?.  ?=(?(%from-spawn %from-height %from-epoch) mode.org)  ~
        :_  ~
        ;form.paste(method "post", action "/apps/gevulot")
          ;input(type "hidden", name "act", value "index-rewind");
          ;label: Re-index from the epoch (block {(scow %ud epoch.u.sc)}).  Slow: {(epoch-cost u.sc)}.  Keeps every verified identity, every trusted install, and everything in Jael; only the scanner's cursor moves.  Type the epoch height to confirm.
          ;input(type "text", name "confirm", placeholder (scow %ud epoch.u.sc), spellcheck "false");
          ;button.ghost(type "submit"): Re-index from the epoch
        ==
  ==
::
::  +walks-span: what each in-flight verification is doing.  A re-check
::  walks every block filter from the peer's last custody entry to the
::  tip to prove its sat has not moved, so a peer that spawned long ago
::  costs thousands of fetches -- which is what "re-checking" waits on.
::
++  walks-span
  |=  [rdy=ready wk=walks]
  ^-  manx
  =/  jobs  ~(tap by wk)
  ?~  jobs
    ;span.v: idle
  ;span.v.st.st-wait
    ;*  %+  turn  jobs
        |=  [who=@p job=@ud top=@ud]
        =/  txt=tape
          ?~  tip.rdy
            ": liveness walk from block {(scow %ud top)}"
          =/  n=@ud  (sub u.tip.rdy (min u.tip.rdy top))
          ": liveness walk from block {(scow %ud top)} to {(scow %ud u.tip.rdy)} ({(scow %ud n)} filters)"
        ;div
          ;+  (name-span who)
          ;span: {txt}
        ==
  ==
::
++  sync-span
  |=  rdy=ready
  ^-  manx
  =/  txt=tape
    ?:  synced.rdy  "synced"
    ?:  indexing.rdy  "indexing…"
    "not synced"
  =/  tiptxt=tape  ?~(tip.rdy "" (weld " — block " (scow %ud u.tip.rdy)))
  ;span(class ?:(synced.rdy "v st st-ok" "v st st-wait")): {(weld txt tiptxt)}
::
::  +walk-rate: blocks a minute the liveness walk manages on a 2-vCPU
::  droplet with one core pinned, measured 2026-10-28 on a real comet
::  walking 19 peers in one batch: 10,490 blocks in 1 h 52 m.  The rate
::  does not depend on how many peers share the walk.
::
++  walk-rate  100
::
::  +gw-clean: per ship, the custody entry and the height an earlier walk
::  already found clean.  %gw-btc resumes a walk for the SAME entry from
::  there (+resume-from in app/gw-btc), so the pane's estimate must too:
::  re-verifying a peer costs blocks-since-last-check, not the whole log.
::
++  gw-clean
  ^-  (map @p [last=[height=@ud =txid:ord] to=@ud])
  =/  res
    %-  mule  |.
    ;;  (map @p [last=[height=@ud =txid:ord] to=@ud])
    .^(* %gx /(scot %p our.bowl)/gw-btc/(scot %da now.bowl)/clean/noun)
  ?:  ?=(%| -.res)  ~
  p.res
::
::  +walk-start: the height the verification of the peers on trust would
::  walk from -- the LOWEST start among them, where a peer's start is its
::  last custody entry (decoded from the pass we hold) or, if an earlier
::  walk of that same entry was clean through some height, the block
::  after it.  ~ when nothing is on trust or no pass we hold decodes.
::
++  walk-start
  |=  todo=(list @p)
  ^-  (unit @ud)
  =/  clean  gw-clean
  %+  roll  todo
  |=  [who=@p lo=(unit @ud)]
  ^-  (unit @ud)
  ?~  p=(~(get by installed) who)  lo
  =/  sat  (from-xtr:lsa who pass.u.p)
  ?~  sat  lo
  ?~  chain.u.sat  lo
  =/  last  (rear chain.u.sat)
  =/  h=@ud
    ?~  cur=(~(get by clean) who)  height.last
    ?.  =(last.u.cur [height.last txid.last])  height.last
    +(to.u.cur)
  ?~  lo  `h
  `(min u.lo h)
::
::  +walk-eta: the walk's length and wall time at +walk-rate.
::
++  walk-eta
  |=  [from=@ud tip=@ud]
  ^-  tape
  ?:  (gth from tip)
    "already clean through block {(scow %ud tip)}, so only the blocks mined since: seconds"
  =/  blocks=@ud  (sub tip (min tip from))
  =/  mins=@ud  (div (add blocks (dec walk-rate)) walk-rate)
  =/  when=tape
    ?:  (lth mins 2)  "a minute or two"
    ?:  (lth mins 90)  "about {(scow %ud mins)} minutes"
    =/  hrs=@ud  (div mins 60)
    =/  rem=@ud  (mod mins 60)
    ?:  (lth rem 5)  "about {(scow %ud hrs)} hours"
    "about {(scow %ud hrs)} h {(scow %ud rem)} min"
  :(weld (scow %ud blocks) " blocks, from block " (scow %ud from) " to " (scow %ud tip) ": " when)
::
::  +verify-block: the on-chain verification of the peers on trust --
::  what it proves, what it costs, and the button that starts it.  The
::  numbers come from the passes we hold and the light client's tip;
::  the one constant is +walk-rate.
::
++  verify-block
  |=  [conf=(set @p) rdy=ready infl=(set @p)]
  ^-  manx
  =/  todo=(list @p)
    %+  skim  ~(tap in ~(key by gw-trusted))
    |=(who=@p !(peer-confirmed who conf))
  =/  n=@ud  (lent todo)
  =/  running=@ud  ~(wyt in infl)
  =/  asked-txt=tape  ?~(asked "" " (asked {(when-txt u.asked)})")
  =/  status=manx
    ?:  (gth running 0)
      ;p.warn: Verifying {(scow %ud running)} peer(s) on chain now{asked-txt}.  The walk's range is in the "verifying" row above.
    ?:  =(0 n)
      ;p.empty: Nothing to verify: every peer this ship holds is verified on chain.
    ?.  synced.rdy
      ;p.warn: {(scow %ud n)} peer(s) on trust.  The light client is not synced yet; a verification you start now begins as soon as it is.
    =/  cost=tape
      =/  start  (walk-start todo)
      ?~  start  "every block from their oldest custody entry to the tip"
      ?~  tip.rdy  "every block from block {(scow %ud u.start)} to the tip"
      (walk-eta u.start u.tip.rdy)
    ;p.warn: {(scow %ud n)} peer(s) on trust.  One walk covers all of them: {cost}, with one core busy the whole time.
  =/  button=manx
    ?:  |(=(0 n) (gth running 0))
      ;button.ghost(type "submit", disabled "disabled"): Verify on chain now
    ;button(type "submit"): Verify {(scow %ud n)} peer(s) on chain now
  ;div
    ;h3: On-chain verification
    ;p.hint
      ; A peer installed on trust is in Jael on your sponsor's word: you
      ; can reach it, but nothing on this ship has checked it.  Verifying
      ; it means your own light client proves the identity against the
      ; chain: it fetches the custody transactions, walks every block's
      ; compact filter from the last custody entry to the tip (resuming
      ; where an earlier walk of that entry left off) to confirm the sat
      ; was never spent, and checks the sponsor relation.  A
      ; verified peer rests on the chain instead of on a sponsor, and only
      ; verified peers count when this ship judges others.  Nothing else
      ; waits on it, which is why it does not start by itself.
    ==
    ;p.hint
      ; The walk runs at about {(scow %ud walk-rate)} blocks a minute on a
      ; 2-vCPU droplet, however many peers share it.  The ship stays
      ; usable meanwhile but answers slowly in bursts.
    ==
    ;+  status
    ;div.row
      ;form(method "post", action "/apps/gevulot")
        ;input(type "hidden", name "act", value "recheck");
        ;+  button
      ==
      ;form(method "post", action "/apps/gevulot")
        ;input(type "hidden", name "act", value "set-auto-verify");
        ;input(type "hidden", name "on", value ?:(auto-verify "false" "true"));
        ;button.ghost.small(type "submit")
          ; Start it by itself after sync: {?:(auto-verify "ON — turn off" "OFF — turn on")}
        ==
      ==
    ==
  ==
::
++  sponsee-section
  |=  [spo=@p conf=(set @p) rdy=ready infl=(set @p)]
  ^-  manx
  ;div.card
    ;h2: As a sponsee
    ;p
      ;span: Your sponsor is {" "}
      ;span: {?.(=(%pawn (clan:title spo)) (scow %p spo) (trip (fore:me (de:ship:me spo))))}
    ==
    ;div.row
      ;form(method "post", action "/apps/gevulot")
        ;input(type "hidden", name "act", value "set-receive");
        ;input(type "hidden", name "on", value ?:(receive "false" "true"));
        ;button.toggle(type "submit")
          ; Peer discovery: {?:(receive "ON — turn off" "OFF — turn on")}
        ==
      ==
    ==
    ;p.hint
      ; When ON, attestations your sponsor pushes are installed on trust,
      ; so you can reach peers without waiting for your own light client
      ; to sync.  They stay on your sponsor's word until you verify them
      ; on chain below.
    ==
    ;div.row
      ;form(method "post", action "/apps/gevulot")
        ;input(type "hidden", name "act", value "distribute");
        ;button(type "submit"): Ask my sponsor to broadcast me
      ==
      ;form(method "post", action "/apps/gevulot")
        ;input(type "hidden", name "act", value "withdraw");
        ;button.ghost(type "submit"): Stop broadcasting me
      ==
    ==
    ;form.paste(method "post", action "/apps/gevulot")
      ;input(type "hidden", name "act", value "ingest");
      ;label: Install a peer by hand (paste a 0x… pass)
      ;input(type "text", name "pass", placeholder "0x…", spellcheck "false");
      ;button(type "submit"): Install
    ==
    ;+  (verify-block conf rdy infl)
    ;+  (installed-list conf synced.rdy infl)
  ==
::
++  installed-list
  |=  [conf=(set @p) synced=? infl=(set @p)]
  ^-  manx
  ::  The list is DERIVED: every identity %gw-btc holds (trusted mirror,
  ::  verified confidential set, public index), annotated by .installed
  ::  where we have a provenance.  Nothing is listed from .installed
  ::  alone: a ship only we remember and %gw-btc does not is exactly the
  ::  drift the design forbids, and it would show here as absent.
  =/  trusted  gw-trusted
  =/  public   gw-public
  =/  ships=(set @p)
    %-  ~(uni in (~(uni in ~(key by trusted)) conf))
    ~(key by public)
  =/  peers=(list [who=@p p=peer:gev])
    %+  turn  (sort-ships ~(tap in ships))
    |=  who=@p
    ^-  [@p peer:gev]
    ?^  a=(~(get by installed) who)  [who u.a]
    :-  who
    :*  pass=?^(b=(~(get by public) who) u.b 0)
        via=?:((~(has in conf) who) %packet %index)
        since=?^(t=(~(get by trusted) who) u.t *@da)
        tries=0
    ==
  ?~  peers
    ;p.empty: No peers yet.
  ;div.peers
    ;h3: Peers ({(scow %ud (lent peers))})
    ;*  %+  turn  peers
        |=  [who=@p p=peer:gev]
        =/  st  (peer-status who conf synced infl p)
        ;div.peer
          ;div.peer-id
            ;span.patp
              ;+  (name-span who)
            ==
            ;span.comet: {(comet-cite who)}
          ==
          ;div.peer-meta
            ;div.peer-top
              ;span(class (status-class st)): {(status-label st)}
              ;form(method "post", action "/apps/gevulot")
                ;input(type "hidden", name "act", value "forget");
                ;input(type "hidden", name "who", value (scow %p who));
                ;button.ghost.small(type "submit"): forget
              ==
            ==
            ;span.via: via {(trip via.p)}
          ==
        ==
  ==
::
++  status-label
  |=  st=?(%confirmed %syncing %checking %queued %failed)
  ^-  tape
  ?-  st
    %confirmed  "verified on chain"
    %syncing    "trusted — awaiting sync"
    %checking   "trusted — verifying on chain now"
    %queued     ?:(auto-verify "trusted — queued for verification" "trusted — not verified yet")
    %failed     "unconfirmed"
  ==
::
++  status-class
  |=  st=?(%confirmed %syncing %checking %queued %failed)
  ^-  tape
  ?-  st
    %confirmed  "st st-ok"
    %syncing    "st st-wait"
    %checking   "st st-wait"
    %queued     "st st-wait"
    %failed     "st st-bad"
  ==
::
++  sponsor-section
  |=  spees=(map @p @ud)
  ^-  manx
  ;div.card
    ;h2: As a sponsor
    ;p: You sponsor {(scow %ud ~(wyt by spees))} comet(s); {(scow %ud ~(wyt by roster))} in the broadcast roster.
    ;div.row
      ;form(method "post", action "/apps/gevulot")
        ;input(type "hidden", name "act", value "set-serving");
        ;input(type "hidden", name "on", value ?:(serving "false" "true"));
        ;button.toggle(type "submit")
          ; Distribution service: {?:(serving "ON — turn off" "OFF — turn on")}
        ==
      ==
    ==
    ;p.hint
      ; When ON, a sponsee that asks to be broadcast is added to the
      ; roster and pushed to every one of your sponsees.
    ==
    ;div.row
      ;form(method "post", action "/apps/gevulot")
        ;input(type "hidden", name "act", value "set-share-index");
        ;input(type "hidden", name "on", value ?:(share-index "false" "true"));
        ;button.toggle(type "submit")
          ; Share my public index: {?:(share-index "ON — turn off" "OFF — turn on")}
        ==
      ==
    ==
    ;p.hint
      ; When ON, a sponsee that announces also receives every public
      ; identity you have indexed, so it can index from its own spawn
      ; block instead of the epoch and still know every public name.
    ==
    ;+  (roster-list spees)
  ==
::
++  roster-list
  |=  spees=(map @p @ud)
  ^-  manx
  =/  rs  (sort-ships ~(tap in ~(key by roster)))
  ?~  rs
    ;p.empty: No sponsees have asked to be broadcast yet.
  ;div.peers
    ;h3: Broadcast roster
    ;*  %+  turn  rs
        |=  p=@p
        ;div.peer
          ;span.patp
            ;+  (name-span p)
          ==
          ;span.tag: {?:((~(has by spees) p) "sponsee" "stale")}
        ==
  ==
::
++  debug-card
  |=  $:  rdy=ready
          conf=(set @p)
          infl=(set @p)
          att=(map @p sont:ord)
          spees=(map @p @ud)
      ==
  ^-  manx
  ;details.debug
    ;summary: Debug — raw %gw-btc state
    ;div.dbg
      ;div.statrow
        ;span.k: synced / tip / indexing
        ;span.v: {<rdy>}
      ==
      ;div.statrow
        ;span.k: verified (confidential)
        ;span.v: {<~(tap in conf)>}
      ==
      ;div.statrow
        ;span.k: attested satpoints
        ;span.v: {<~(key by att)>}
      ==
      ;div.statrow
        ;span.k: verifications in flight
        ;span.v: {<~(tap in infl)>}
      ==
      ;div.statrow
        ;span.k: our sponsees (ship→life)
        ;span.v: {<~(tap by spees)>}
      ==
      ;div.statrow
        ;span.k: broadcast roster
        ;span.v: {<~(tap in ~(key by roster))>}
      ==
      ;div.statrow
        ;span.k: receive / serving / auto-verify
        ;span.v: {<[receive serving auto-verify]>}
      ==
      ;div.statrow
        ;span.k: verification last asked
        ;span.v: {?~(asked "never" (when-txt u.asked))}
      ==
    ==
  ==
::
::  +name-toggle: the "Show @p" button and click-to-copy.  The toggle is
::  per page and per browser only (localStorage), never a ship setting;
::  the hover text always has the @p.  A click on a name copies what the
::  toggle shows: the full nym, or the @p, and a "copied" tooltip fades
::  in above the cursor, holds a second, and fades out.
::  navigator.clipboard exists only on https and localhost, so a ship
::  served over plain http falls back to a hidden textarea.
::
++  name-toggle
  ^-  tape
  %-  trip
  '''
  (function(){var k='gevulot-show-patp',on=false,r=document.documentElement;
  function show(){r.classList.toggle('show-p',on);
  var b=document.getElementById('nmt');if(b)b.textContent=on?'Show names':'Show @p';}
  try{on=localStorage.getItem(k)==='1'}catch(e){}
  var b=document.getElementById('nmt');if(b)b.onclick=function(){on=!on;try{localStorage.setItem(k,on?'1':'0')}catch(e){}show()};
  function old(t){var a=document.createElement('textarea');a.value=t;a.style.position='fixed';a.style.opacity='0';
  document.body.appendChild(a);a.select();try{document.execCommand('copy')}catch(e){}document.body.removeChild(a);}
  function copy(t){try{navigator.clipboard.writeText(t).catch(function(){old(t)})}catch(e){old(t)}}
  function tip(x,y){var d=document.createElement('div');d.className='tip';d.textContent='copied';
  d.style.left=x+'px';d.style.top=y+'px';document.body.appendChild(d);
  d.addEventListener('animationend',function(){d.remove()});}
  document.querySelectorAll('.nm').forEach(function(e){e.onclick=function(v){copy(on?e.dataset.p:e.dataset.n);
  tip(v.clientX,v.clientY)}});
  show();})();
  '''
::
++  style
  ^-  tape
  %-  trip
  '''
  :root{--bg:#0c0d10;--fg:#e8e8ea;--mut:#8a8c93;--acc:#ff6a00;--card:#16171c;--ok:#3ecf6a;--wait:#e0a83e;--bad:#e05a5a}
  *{box-sizing:border-box}
  body{margin:0;background:var(--bg);color:var(--fg);font:15px/1.5 -apple-system,system-ui,sans-serif}
  .wrap{max-width:660px;margin:0 auto;padding:32px 20px}
  h1{color:var(--acc);margin:0 0 2px;font-size:26px}
  .sub{color:var(--mut);margin:0 0 24px;font-size:13px}
  .card{background:var(--card);border:1px solid #24252c;border-radius:12px;padding:20px 22px;margin:0 0 18px}
  .status{display:flex;flex-direction:column;gap:8px}
  h2{margin:0 0 8px;font-size:18px}
  h3{margin:16px 0 8px;font-size:14px;color:var(--mut);text-transform:uppercase;letter-spacing:.04em}
  .row{display:flex;gap:10px;flex-wrap:wrap;margin:12px 0}
  .hint,.note{color:var(--mut);font-size:13px;margin:8px 0 0}
  button{background:var(--acc);color:#111;border:0;border-radius:8px;padding:9px 14px;font-size:14px;font-weight:600;cursor:pointer}
  button.ghost{background:transparent;color:var(--mut);border:1px solid #2c2d35}
  button.small{padding:4px 9px;font-size:12px}
  .toggle{min-width:230px}
  .paste{margin-top:16px;display:flex;flex-direction:column;gap:6px}
  .paste label{color:var(--mut);font-size:13px}
  .paste input{background:#0c0d10;border:1px solid #2c2d35;border-radius:8px;color:var(--fg);padding:9px;font:13px monospace}
  .statrow{display:flex;align-items:baseline;gap:12px}
  .statrow .k{color:var(--mut);font-size:12px;text-transform:uppercase;letter-spacing:.04em;min-width:170px;flex:none}
  .statrow .v{font:12px monospace;color:var(--fg);word-break:break-all;min-width:0;flex:1}
  .statrow .st{white-space:normal;word-break:break-word;display:inline-block;line-height:1.5}
  .idx{display:flex;flex-direction:column;gap:6px;min-width:0;word-break:normal}
  .idx .st{align-self:flex-start}
  .idx-meta{font:12px monospace;color:var(--mut);white-space:normal;word-break:break-word}
  .bar{height:6px;background:#24252c;border-radius:3px;overflow:hidden;max-width:420px}
  .bar-fill{height:100%;background:var(--wait);border-radius:3px}
  .peers{margin-top:12px}
  .peer{display:flex;align-items:flex-start;justify-content:space-between;gap:14px;padding:8px 0;border-top:1px solid #1f2027}
  .peer-id{display:flex;flex-direction:column;min-width:0}
  .patp{font:12px monospace;color:var(--fg);min-width:0}
  .peer-id>*,.peer-meta>*{line-height:24px}
  .peer-meta{display:flex;flex-direction:column;flex:none;width:270px}
  .peer-top{display:flex;align-items:center;gap:8px;height:24px}
  .peer-top .st{line-height:1.5}
  .peer-top form{margin:0 0 0 auto}
  .comet{font:12px monospace;color:var(--mut)}
  .nm{cursor:pointer;transition:color .15s}
  .nm:hover{color:var(--acc)}
  .tip{position:fixed;z-index:9;transform:translate(-50%,-100%);margin-top:-10px;pointer-events:none;white-space:nowrap;background:var(--fg);color:#111;font:600 11px/1 -apple-system,system-ui,sans-serif;padding:5px 8px;border-radius:6px;opacity:0;animation:tip 1.3s ease forwards}
  @keyframes tip{0%{opacity:0}12%{opacity:1}88%{opacity:1}100%{opacity:0}}
  .nm .p{display:none;word-break:break-all}
  .show-p .nm .n{display:none}
  .show-p .nm .p{display:inline}
  @media(max-width:560px){.peer{flex-direction:column;gap:2px}.peer-meta{width:auto;align-self:stretch}}
  .top{display:flex;align-items:center;justify-content:space-between;gap:1em}
  .via{font-size:10px;color:var(--mut);text-transform:uppercase;letter-spacing:.04em}
  .tag{font-size:11px;color:var(--mut)}
  .st{font-size:11px;font-weight:600;padding:2px 8px;border-radius:99px;white-space:nowrap;flex:none}
  .st-ok{color:var(--ok);background:rgba(62,207,106,.12)}
  .st-wait{color:var(--wait);background:rgba(224,168,62,.12)}
  .st-bad{color:var(--bad);background:rgba(224,90,90,.12)}
  .empty{color:var(--mut);font-size:13px;font-style:italic}
  .debug{background:var(--card);border:1px solid #24252c;border-radius:12px;padding:14px 18px}
  .debug summary{color:var(--mut);font-size:13px;cursor:pointer;user-select:none}
  .dbg{display:flex;flex-direction:column;gap:8px;margin-top:14px}
  '''
--
