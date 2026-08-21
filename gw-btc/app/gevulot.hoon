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
/-  gev=gevulot, urb, ord
/+  default-agent, dbug, server, schooner, cc=gw-btc-pass
::
|%
+$  versioned-state  $%(state-0)
+$  state-0
  $:  %0
      receive=?               :: sponsee: accept pushes from our sponsor?  (default no)
      serving=?               :: sponsor: run the distribution service?    (default yes)
      roster=(set @p)         :: sponsor: sponsees who asked to be broadcast
      installed=(map @p peer:gev) :: sponsee: peers we installed, with provenance
  ==
+$  card  card:agent:gall
+$  ready  [synced=? tip=(unit @) indexing=?]  :: shape of %gw-btc /x/ready
--
::
=|  state-0
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
  `this(state !<(versioned-state old))
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
::  +our-sponsor: the ship OUR own jael point names as our sponsor, if any.
::  A comet with no sponsor projects to itself, which we treat as "none".
::
++  our-sponsor
  ^-  (unit @p)
  =/  s=@p
    .^(@p %j /(scot %p our.bowl)/sein/(scot %da now.bowl)/(scot %p our.bowl))
  ?:  =(our.bowl s)  ~
  `s
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
::  +gw-point-public: does %gw-btc hold a PUBLIC on-chain point for .who?
::  (Confidential peers answer ~ here and show up in +gw-confidential
::  instead.)  A blocked scry -- confidential or unknown -- reads as no.
::
++  gw-point-public
  |=  who=@p
  ^-  ?
  =/  res
    %-  mule  |.
    .^(point:urb %gx /(scot %p our.bowl)/gw-btc/(scot %da now.bowl)/point/(scot %p who)/urb-point)
  ?=(%& -.res)
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
::    %pending   -- synced; re-verification is under way (tries left).
::    %failed    -- synced and exhausted; never confirmed (stale or forged).
::
++  peer-status
  |=  [who=@p conf=(set @p) synced=? p=peer:gev]
  ^-  ?(%confirmed %syncing %pending %failed)
  ?:  (peer-confirmed who conf)  %confirmed
  ?.  synced  %syncing
  ?:  (gte tries.p max-tries)  %failed
  %pending
::
::  +pass-of: the attestation pass jael holds for .who at .life.  This is
::  the pass %gw-btc installed when it verified .who, so it carries the
::  xtr custody log a trusted install needs.  jael's %deed answers
::  [life pass (unit @)].
::
++  pass-of
  |=  [who=@p life=@ud]
  ^-  (unit pass)
  =/  res
    %-  mule  |.
    ;;  [lyf=@ud =pass rest=*]
    .^(* %j /(scot %p our.bowl)/deed/(scot %da now.bowl)/(scot %p who)/(scot %ud life))
  ?:  ?=(%| -.res)  ~
  `pass.p.res
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
    ::  ask our sponsor to broadcast us.  Harmless if we have no sponsor.
    =/  spo=(unit @p)  our-sponsor
    ?~  spo  `state
    :_  state
    ~[(wire-to u.spo %announce ~)]
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
    =/  who=@p  `@p`fig:ex:(com:nu:cric:crypto pass.act)
    (install-peer who pass.act %paste)
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
      ::  A SPONSEE asks us (its sponsor) to broadcast it.  Ignore unless
      ::  we are serving AND src really is a current sponsee of ours -- the
      ::  sponsor is the source of truth, a stranger cannot inject itself.
      ::  We read src's pass from OUR jael (we verified it), add it to the
      ::  roster, push it to every current sponsee, and send src the whole
      ::  roster so a just-booted sponsee learns everyone already in.
      %announce
    ?.  serving  `state
    ?.  (~(has by our-sponsees) src)  `state
    =.  roster  (~(put in roster) src)
    :_  state
    (broadcast-announce src)
  ::
      %withdraw
    `state(roster (~(del in roster) src))
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
::  +broadcast-announce: cards to send when .newcomer announces:
::    - push .newcomer's pass to every OTHER current sponsee;
::    - push every roster member's pass (incl. newcomer) to .newcomer.
::  A missing pass (jael does not hold it) drops that one peer silently.
::
++  broadcast-announce
  |=  newcomer=@p
  ^-  (list card)
  =/  spees  our-sponsees                :: (map @p @ud) — @p -> life
  =/  members=(list @p)  ~(tap in roster)
  ;:  weld
    ::  newcomer -> every other current sponsee
    =/  nl  (~(get by spees) newcomer)
    ?~  nl  ~
    ?~  np=(pass-of newcomer u.nl)  ~
    %+  murn  ~(tap in ~(key by spees))
    |=  s=@p
    ?:  =(s newcomer)  ~
    `(wire-to s %peer newcomer u.np)
  ::
    ::  every roster member -> newcomer
    %+  murn  members
    |=  m=@p
    ?:  =(m newcomer)  ~
    =/  ml  (~(get by spees) m)
    ?~  ml  ~
    ?~  mp=(pass-of m u.ml)  ~
    `(wire-to newcomer %peer m u.mp)
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
        ;+  (status-card rdy spo)
        ;*  ?~  spo  ~
            :~  (sponsee-section u.spo conf synced.rdy)
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
  |=  [rdy=ready spo=(unit @p)]
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
  |=  [spo=@p conf=(set @p) synced=?]
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
    ;+  (installed-list conf synced)
  ==
::
++  installed-list
  |=  [conf=(set @p) synced=?]
  ^-  manx
  =/  peers  ~(tap by installed)
  ?~  peers
    ;p.empty: No peers installed yet.
  ;div.peers
    ;h3: Installed peers ({(scow %ud (lent peers))})
    ;*  %+  turn  peers
        |=  [who=@p p=peer:gev]
        =/  st  (peer-status who conf synced p)
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
  |=  st=?(%confirmed %syncing %pending %failed)
  ^-  tape
  ?-  st
    %confirmed  "verified on chain"
    %syncing    "trusted — awaiting sync"
    %pending    "trusted — re-checking"
    %failed     "unconfirmed"
  ==
::
++  status-class
  |=  st=?(%confirmed %syncing %pending %failed)
  ^-  tape
  ?-  st
    %confirmed  "st st-ok"
    %syncing    "st st-wait"
    %pending    "st st-wait"
    %failed     "st st-bad"
  ==
::
++  sponsor-section
  |=  spees=(map @p @ud)
  ^-  manx
  ;div.card
    ;h2: As a sponsor
    ;p: You sponsor {(scow %ud ~(wyt by spees))} comet(s); {(scow %ud ~(wyt in roster))} in the broadcast roster.
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
  =/  rs  ~(tap in roster)
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
        ;span.v: {<~(tap in roster)>}
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
