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
::        OUR sponsor are installed into jael on trust (no chain fetch).
::      - "distribute my attestation": ask our sponsor to broadcast us.
::      - paste a peer's pass to install it by hand.
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
/-  gev=gevulot, sa=self-attestation
/+  default-agent, dbug, server, schooner
::
|%
+$  versioned-state  $%(state-0)
+$  state-0
  $:  %0
      receive=?          :: sponsee: accept pushes from our sponsor?  (default no)
      serving=?          :: sponsor: run the distribution service?    (default yes)
      roster=(set @p)    :: sponsor: sponsees who asked to be broadcast
      installed=(set @p) :: sponsee: peers we installed (sponsor push or paste)
  ==
+$  card  card:agent:gall
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
::  +wire-to: send a $wire to another ship's %gevulot.
::
++  wire-to
  |=  [who=@p =wire:gev]
  ^-  card
  [%pass /wire %agent [who %gevulot] %poke %noun !>(wire)]
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
    ::  fingerprint; %gw-btc re-derives and re-checks it, so a wrong paste
    ::  is a silent no-op there.  We record it for the UI.
    =/  who=@p  `@p`fig:ex:(com:nu:cric:crypto pass.act)
    :_  state(installed (~(put in installed) who))
    ~[(poke-gw-install who pass.act)]
  ::
      %forget-peer
    ::  drop it from OUR list only; we do not un-tell jael (harmless to keep).
    `state(installed (~(del in installed) who.act))
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
      ::  AND src is actually our sponsor.  Then hand it to %gw-btc for the
      ::  offline install.  Idempotent; a peer we already hold is a no-op.
      %peer
    ?.  receive  `state
    =/  spo=(unit @p)  our-sponsor
    ?~  spo  `state
    ?.  =(src u.spo)  `state
    :_  state(installed (~(put in installed) who.wire))
    ~[(poke-gw-install who.wire pass.wire)]
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
        ;p.sub: identity control pane — {(scow %p our.bowl)}
        ;*  ?~  spo  ~
            :~  (sponsee-section u.spo)
            ==
        ;*  ?.  am-sponsor  ~
            :~  (sponsor-section spees)
            ==
        ;*  ?:  |(?=(^ spo) am-sponsor)  ~
            :~  ;p.note
                  ; This ship has neither a sponsor nor sponsees yet, so
                  ; there is nothing to discover.
                ==
            ==
      ==
    ==
  ==
::
++  sponsee-section
  |=  spo=@p
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
      ; When ON, attestations your sponsor pushes are installed on trust,
      ; so you can reach peers without waiting for your own light client
      ; to sync.
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
    ;+  installed-list
  ==
::
++  installed-list
  ^-  manx
  =/  peers  ~(tap in installed)
  ?~  peers
    ;p.empty: No peers installed yet.
  ;div.peers
    ;h3: Installed peers ({(scow %ud (lent peers))})
    ;*  %+  turn  peers
        |=  p=@p
        ;div.peer
          ;span.patp: {(scow %p p)}
          ;form(method "post", action "/apps/gevulot")
            ;input(type "hidden", name "act", value "forget");
            ;input(type "hidden", name "who", value (scow %p p));
            ;button.ghost.small(type "submit"): forget
          ==
        ==
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
++  style
  ^-  tape
  %-  trip
  '''
  :root{--bg:#0c0d10;--fg:#e8e8ea;--mut:#8a8c93;--acc:#ff6a00;--card:#16171c}
  *{box-sizing:border-box}
  body{margin:0;background:var(--bg);color:var(--fg);font:15px/1.5 -apple-system,system-ui,sans-serif}
  .wrap{max-width:640px;margin:0 auto;padding:32px 20px}
  h1{color:var(--acc);margin:0 0 2px;font-size:26px}
  .sub{color:var(--mut);margin:0 0 24px;font-size:13px}
  .card{background:var(--card);border:1px solid #24252c;border-radius:12px;padding:20px 22px;margin:0 0 18px}
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
  .peers{margin-top:12px}
  .peer{display:flex;align-items:center;justify-content:space-between;gap:10px;padding:7px 0;border-top:1px solid #1f2027}
  .patp{font:12px monospace;color:var(--fg);word-break:break-all}
  .tag{font-size:11px;color:var(--mut)}
  .empty{color:var(--mut);font-size:13px;font-style:italic}
  '''
--
