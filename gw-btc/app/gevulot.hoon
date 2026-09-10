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
::        OUR sponsor are installed into jael -- verified on chain if our
::        light client is already synced, otherwise installed on TRUST and
::        re-verified on chain the moment it catches up.
::      - "distribute my attestation": ask our sponsor to broadcast us.
::      - paste a peer's pass to install it by hand (verified on chain
::        whenever we can; a paste carries no sponsor's word).
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
/-  gev=gevulot, urb, ord, sa=self-attestation
/+  default-agent, dbug, server, schooner, cc=gw-btc-pass, lsa=self-attestation
::
|%
+$  versioned-state  $%(state-0 state-1)
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
      receive=?               :: sponsee: accept pushes from our sponsor?  (default no)
      serving=?               :: sponsor: run the distribution service?    (default yes)
      roster=(map @p pass)    :: sponsor: announced sponsees, each with the pass to push
      installed=(map @p peer:gev) :: sponsee: peers we installed, with provenance
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
  ==
+$  walks  (map @p [job=@ud top=@ud])          :: shape of %gw-btc /x/inflight-detail
--
::
=|  state-1
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
  :_  this(serving %.y)
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
    %1  `this(state ver)
    ::  0 -> 1: the old roster held ships without passes, and there is no
    ::  way to recover them (see state-1).  Start it empty; every sponsee
    ::  re-announces on boot (boot.sh's %distribute) and can from the pane.
    %0  `this(state [%1 receive.ver serving.ver ~ installed.ver])
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
::  verifier is single-flight, so we recheck one peer at a time and wait
::  on this before starting another.
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
::    %queued    -- synced; waiting for the verifier's single slot (some
::                  other ship's verification is running).  Used to be
::                  shown as "re-checking" too, which read as if this
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
::  +install-peer: record a peer and get it into jael.  Verify on chain
::  right now if we can; otherwise install on trust and arm the sweep that
::  will verify it once the light client catches up.
::
++  install-peer
  |=  [who=@p =pass via=provenance:gev]
  ^-  (quip card _state)
  =/  rdy  gw-ready
  =/  entry=peer:gev  [pass via now.bowl ?:(synced.rdy 1 0)]
  :_  state(installed (~(put by installed) who entry))
  ?:  synced.rdy
    ~[(poke-gw-verify who pass)]
  ~[(poke-gw-install who pass) (recheck-timer ~m2)]
::
::  =====================================================================
::  local actions (UI / dojo / Causeway)
::  =====================================================================
::
++  handle-action
  |=  act=action:gev
  ^-  (quip card _state)
  ?-    -.act
      %set-receive   `state(receive on.act)
      %set-serving   `state(serving on.act)
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
    ::  drop it from OUR list only; we do not un-tell jael (harmless to keep).
    `state(installed (~(del by installed) who.act))
  ::
      %recheck
    ::  reset the attempt counters and re-verify everything on chain now.
    =.  state
      state(installed (~(run by installed) |=(p=peer:gev p(tries 0))))
    recheck-sweep
  ==
::
::  +recheck-sweep: re-verify unconfirmed installs on chain, one at a time
::  (the verifier is single-flight), re-arming until every peer is
::  confirmed or has exhausted its tries.
::
++  recheck-sweep
  ^-  (quip card _state)
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
  ?^  infl                               :: a verification is running; wait
    :_  state
    ~[(recheck-timer ~m1)]
  =/  who  who.i.todo
  =/  p    p.i.todo
  :_  state(installed (~(put by installed) who p(tries +(tries.p))))
  :~  (poke-gw-verify who pass.p)
      (recheck-timer ~m2)
  ==
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
    (broadcast-announce src pass.wire)
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
  ?:  =(act 'distribute')    [%distribute ~]
  ?:  =(act 'withdraw')      [%withdraw ~]
  ?:  =(act 'recheck')       [%recheck ~]
  ?:  =(act 'forget')        [%forget-peer (slav %p (~(gut by (malt q)) 'who' '~zod'))]
  ?:  =(act 'ingest')
    =/  raw  (~(gut by (malt q)) 'pass' '')
    [%ingest-peer (slav %ux raw)]
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
        ;h1: Gevulot
        ;p.sub: identity control pane
        ;+  (status-card rdy spo gw-scan gw-walks)
        ;*  ?~  spo  ~
            :~  (sponsee-section u.spo conf synced.rdy gw-inflight)
            ==
        ;*  ?.  am-sponsor  ~
            :~  (sponsor-section spees)
            ==
        ;+  (debug-card rdy conf gw-inflight gw-attested spees)
      ==
    ==
  ==
::
++  status-card
  |=  [rdy=ready spo=(unit @p) sc=(unit scan) wk=walks]
  ^-  manx
  ;div.card.status
    ;div.statrow
      ;span.k: ship
      ;span.v: {(scow %p our.bowl)}
    ==
    ;div.statrow
      ;span.k: sponsor
      ;span.v: {?~(spo "none" (scow %p u.spo))}
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
++  index-span
  |=  sc=(unit scan)
  ^-  manx
  ?~  sc
    ;span.v.st.st-wait: no data (older %gw-btc)
  ?~  tip.u.sc
    ;span.v.st.st-wait: waiting for a chain tip
  =/  settled=@ud  (sub u.tip.u.sc (min u.tip.u.sc confirmations.u.sc))
  ?.  indexing.u.sc
    ;span.v.st.st-wait: not started
  ?:  (gte cursor.u.sc settled)
    ;span.v.st.st-ok: complete at block {(scow %ud cursor.u.sc)}; {(scow %ud points.u.sc)} identit(ies) indexed
  =/  left=@ud  (sub settled cursor.u.sc)
  =/  done=@ud  (sub cursor.u.sc (min cursor.u.sc epoch.u.sc))
  ;span.v.st.st-wait
    ; scanning block {(scow %ud cursor.u.sc)} of {(scow %ud settled)}:
    ; {(scow %ud left)} to go, {(scow %ud done)} done since the epoch
    ; ({(scow %ud batch.u.sc)} per batch; the ship is slow until this ends)
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
            "{(scow %p who)}: liveness walk from block {(scow %ud top)}"
          =/  n=@ud  (sub u.tip.rdy (min u.tip.rdy top))
          "{(scow %p who)}: liveness walk from block {(scow %ud top)} to {(scow %ud u.tip.rdy)} ({(scow %ud n)} filters)"
        ;div: {txt}
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
++  sponsee-section
  |=  [spo=@p conf=(set @p) synced=? infl=(set @p)]
  ^-  manx
  ;div.card
    ;h2: As a sponsee
    ;p: Your sponsor: {(scow %p spo)}
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
      ; When ON, attestations your sponsor pushes are installed so you can
      ; reach peers without waiting for your own light client to sync. Each
      ; is re-verified on chain automatically once it does.
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
    ;div.row
      ;form(method "post", action "/apps/gevulot")
        ;input(type "hidden", name "act", value "recheck");
        ;button.ghost.small(type "submit"): Re-check all on chain now
      ==
    ==
    ;+  (installed-list conf synced infl)
  ==
::
++  installed-list
  |=  [conf=(set @p) synced=? infl=(set @p)]
  ^-  manx
  =/  peers  ~(tap by installed)
  ?~  peers
    ;p.empty: No peers installed yet.
  ;div.peers
    ;h3: Installed peers ({(scow %ud (lent peers))})
    ;*  %+  turn  peers
        |=  [who=@p p=peer:gev]
        =/  st  (peer-status who conf synced infl p)
        ;div.peer
          ;div.peer-id
            ;span.patp: {(scow %p who)}
            ;span.via: via {(trip via.p)}
          ==
          ;span(class (status-class st)): {(status-label st)}
          ;form(method "post", action "/apps/gevulot")
            ;input(type "hidden", name "act", value "forget");
            ;input(type "hidden", name "who", value (scow %p who));
            ;button.ghost.small(type "submit"): forget
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
    %queued     "trusted — queued for verification"
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
    ;+  (roster-list spees)
  ==
::
++  roster-list
  |=  spees=(map @p @ud)
  ^-  manx
  =/  rs  ~(tap in ~(key by roster))
  ?~  rs
    ;p.empty: No sponsees have asked to be broadcast yet.
  ;div.peers
    ;h3: Broadcast roster
    ;*  %+  turn  rs
        |=  p=@p
        ;div.peer
          ;span.patp: {(scow %p p)}
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
        ;span.k: receive / serving
        ;span.v: {<[receive serving]>}
      ==
    ==
  ==
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
  .statrow .v{font:12px monospace;color:var(--fg);word-break:break-all}
  .peers{margin-top:12px}
  .peer{display:flex;align-items:center;justify-content:space-between;gap:10px;padding:8px 0;border-top:1px solid #1f2027}
  .peer-id{display:flex;flex-direction:column;gap:1px;min-width:0}
  .patp{font:12px monospace;color:var(--fg);word-break:break-all}
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
