::  %urb-watcher-2
::
::  A substantially simpler Groundwire attestation handler. It relies on the
::  (upcoming) %light-client agent for all Bitcoin data, so it carries NO block
::  loop, NO Bitcoin RPC, NO snapshotting, and NO sont/insc indexes.
::
::  It handles two pokes:
::
::    %add-keyfile           -- a keyfile (attestation chain = self-attestation),
::      from Causeway / Ames / any source. A khan strand (verify-lc:lca) fetches
::      the chain's txs through %light-client and runs the full verifier; on a
::      VALID result the ship's point is pushed to Jael and its tip sat is
::      tracked. On INVALID it is skipped + slogged.
::
::    %subscribe-for-keyfiles -- [ship agent path]: subscribe to an indexer that
::      forwards public attestations. Each keyfile received on the %fact is
::      processed EXACTLY like %add-keyfile (we always re-verify in full; an
::      indexer's tl;dr is never trusted). Invalid handling is identical to
::      %add-keyfile (skip + slog) -- no verdict report-back, no indexer distrust.
::
::  It also watches %light-client's /best-block. On each new block it queries
::  /block-filter for that block and checks the tracked tips' scriptPubKeys: a
::  hit means a tracked sat moved, so it pokes Ames (%request-keyfile) to ask
::  the ship for a fresh keyfile (Ames later replies with an %add-keyfile poke).
::
/-  urb, ord, sa=self-attestation, lc=light-client, bc=bitcoin
/+  dbug, default-agent, verb, lsa=self-attestation, lca=lc-attestation
|%
+$  card  card:agent:gall
+$  versioned-state  $%(state-0)
+$  state-0
  $:  %0
      ::  watched attestation tips: ship -> [tip sont, tip output scriptPubKey]
      tracked=(map @p [tip=sont:ord spk=hexb:bc])
      ::  verified points, kept only to answer the on-watch /ship Jael replay
      points=(map @p point:urb)
      ::  indexer subscriptions we initiated (re-/watch on %kick), by wire serial
      indexers=(map @ta [=ship agent=term =path])
      ::  the current chain tip (from /best-block); the block-id for udiffs
      best=id:block:bc
      ::  recent height -> block-hash, accumulated from /best-block, for the
      ::  tip-unspent filter scan in verify-lc
      recent=(map @ud @ux)
  ==
--
%-  agent:dbug
^-  agent:gall
=|  state-0
=*  state  -
%+  verb  &
=<
|_  =bowl:gall
+*  this   .
    def    ~(. (default-agent this %|) bowl)
::
++  on-init
  ^-  (quip card _this)
  :_  this
  :~  [%pass /best-block %agent [our.bowl %light-client] %watch /best-block]
      (listen-to-urb ~ [%| dap.bowl])
  ==
::
++  on-save  !>(state)
++  on-load
  |=  =vase
  ^-  (quip card _this)
  `this(state !<(versioned-state vase))
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?+    mark  (on-poke:def mark vase)
  ::  a keyfile to verify (from Causeway / Ames / anywhere)
      %add-keyfile
    =/  sat  !<(self-attestation:sa vase)
    ?:  (gth (lent chain.sat) 1.024)
      %-  (slog leaf+"%urb-watcher-2: chain for {<who.sat>} too long; skipping" ~)
      `this
    :_  this
    ~[(validate-card byk.bowl eny.bowl sat tracked best recent points)]
  ::  subscribe to an indexer that forwards keyfiles
      %subscribe-for-keyfiles
    =+  !<([who=ship agent=term =path] vase)
    =/  ser=@ta  (scot %uv (sham [who agent path eny.bowl]))
    :_  this(indexers (~(put by indexers) ser [who agent path]))
    :~  [%pass /indexer/[ser] %agent [who agent] %watch path]
    ==  
  ==
::
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card _this)
  ?+    wire  (on-arvo:def wire sign-arvo)
  ::  a keyfile-validation strand returned
      [%validate @ ~]
    ?+    sign-arvo  (on-arvo:def wire sign-arvo)
        [%khan %arow *]
      ?.  -.p.sign-arvo
        ?>  ?=([%khan %arow %.n *] sign-arvo)
        %-  (slog leaf+"%urb-watcher-2: validation thread crashed" +.p.p.sign-arvo)
        `this
      ?>  ?=([%khan %arow %.y %noun *] sign-arvo)
      =/  [%khan %arow %.y %noun =vase]  sign-arvo
      =+  !<([res=result:sa tip-spk=hexb:bc] vase)
      %-  (slog (report:lsa verdict.res))
      ::  uniform invalid handling: skip + slog, no report-back
      ?.  ok.verdict.res  `this
      ?~  point.res
        %-  (slog leaf+"%urb-watcher-2: valid but no point; skipping" ~)
        `this
      =*  who  who.verdict.res
      ::  store the point (for the Jael replay), track the tip + its spk, and
      ::  push to Jael (directly if subscribed, else via %listen + on-watch).
      =.  points   (~(put by points) who u.point.res)
      =.  tracked  (~(put by tracked) who [sont.own.u.point.res tip-spk])
      :_  this
      ?:  (~(has in (subs-to-ships sup.bowl)) who)
        (jael-update (point-to-udiffs who u.point.res best))
      ~[(listen-to-urb (silt ~[who]) [%| dap.bowl])]
    ==
  ::  a block-filter strand returned: the set of ships whose tracked tip moved
      [%filter @ ~]
    ?+    sign-arvo  (on-arvo:def wire sign-arvo)
        [%khan %arow *]
      ?.  -.p.sign-arvo
        ?>  ?=([%khan %arow %.n *] sign-arvo)
        %-  (slog leaf+"%urb-watcher-2: block-filter thread crashed" +.p.p.sign-arvo)
        `this
      ?>  ?=([%khan %arow %.y %noun *] sign-arvo)
      =/  [%khan %arow %.y %noun =vase]  sign-arvo
      =/  hits=(set @p)  !<((set @p) vase)
      :_  this
      %+  turn  ~(tap in hits)
      |=(who=@p (request-keyfile who))
    ==
  ==
::
++  on-watch
  |=  =(pole knot)
  ^-  (quip card _this)
  ?+    pole  (on-watch:def pole)
  ::  Jael subscribes to / when it learns we are a PKI source (local-only).
      ~
    ?>  =(our src):bowl
    `this
  ::  Jael subscribes to /ship when it hears about a new ship.
      [=ship ~]
    ?>  =(our src):bowl
    :_  this
    :~  :*  %give  %fact  ~  %azimuth-udiffs
            !>  ^-  udiffs:point:jael
            %+  murn  (state-to-udiffs points best)
            |=  [=ship =udiff:point:jael]
            ^-  (unit [^ship udiff:point:jael])
            ?.  =(ship (slav %p ship.pole))  ~
            `[ship udiff]
    ==  ==
  ==
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  ?+    wire  (on-agent:def wire sign)
  ::  /best-block from %light-client
      [%best-block ~]
    ?+    -.sign  (on-agent:def wire sign)
        %watch-ack
      ?~  p.sign  `this
      %-  (slog leaf+"%urb-watcher-2: /best-block nacked" u.p.sign)
      `this
    ::
        %kick
      :_  this
      :~  [%pass /best-block %agent [our.bowl %light-client] %watch /best-block]
      ==
    ::
        %fact
      ?.  ?=(%light-client-best-block p.cage.sign)  (on-agent:def wire sign)
      =+  !<(upd=best-block-update:lc q.cage.sign)
      ::  XX KNOWN GAP (audit P2, reorg-ignored): upd carries a `reorg` field
      ::  (sur/light-client:44) that we do NOT act on. On a reorg we should
      ::  prune `recent` above the fork height and re-request keyfiles for
      ::  tracked ships whose tip/landing height is above it (a previously-valid
      ::  tip may have vanished). urb-core is likewise forward-only, so this is
      ::  a shared hardening gap, not a regression. Not attacker-triggerable.
      =.  best    id.upd
      =.  recent  (~(put by recent) num.id.upd hax.id.upd)
      ?:  =(~ tracked)  `this
      =/  ser=@ta  (scot %uv (sham [hax.id.upd eny.bowl]))
      :_  this
      :~  :*  %pass  /filter/[ser]  %arvo  %k  %lard
              q.byk.bowl  (check-filter:lca hax.id.upd tracked)
      ==  ==
    ==
  ::  /indexer/[ser]: a forwarded keyfile from an indexer
      [%indexer @ ~]
    =/  ser=@ta  i.t.wire
    ?+    -.sign  (on-agent:def wire sign)
        %watch-ack
      ?~  p.sign  `this
      %-  (slog leaf+"%urb-watcher-2: indexer subscription nacked" u.p.sign)
      `this(indexers (~(del by indexers) ser))
    ::
        %kick
      ?~  ix=(~(get by indexers) ser)  `this
      :_  this
      :~  [%pass wire %agent [ship.u.ix agent.u.ix] %watch path.u.ix]
      ==
    ::
        %fact
      ?.  ?=(?(%self-attestation %add-keyfile) p.cage.sign)
        (on-agent:def wire sign)
      =/  sat  !<(self-attestation:sa q.cage.sign)
      ?:  (gth (lent chain.sat) 1.024)
        %-  (slog leaf+"%urb-watcher-2: chain for {<who.sat>} too long; skipping" ~)
        `this
      :_  this
      ~[(validate-card byk.bowl eny.bowl sat tracked best recent points)]
    ==
  ==
::
++  on-leave  on-leave:def
++  on-fail   on-fail:def
++  on-peek   on-peek:def
--
::
|%
::
::  +validate-card: the card to launch verify-lc for a keyfile (shared by
::  %add-keyfile and the indexer %fact). Callers gate on chain length first.
::  Uniform invalid handling is decided in on-arvo.
++  validate-card
  |=  $:  byk=beak  eny=@uvJ  sat=self-attestation:sa
          tracked=(map @p [tip=sont:ord spk=hexb:bc])
          best=id:block:bc  recent=(map @ud @ux)
          points=(map @p point:urb)
      ==
  ^-  card
  =/  tip=(unit sont:ord)  ?~(e=(~(get by tracked) who.sat) ~ `tip.u.e)
  =/  ser=@ta  (scot %uv (sham [who.sat eny]))
  [%pass /validate/[ser] %arvo %k %lard q.byk (verify-lc:lca sat tip best recent points)]
::
::  +listen-to-urb: tell Jael to subscribe to us as a PKI source.
++  listen-to-urb
  |=  [ships=(set ship) =source:point:jael]
  ^-  card
  [%pass /lo %arvo %j %listen ships source]
::
::  +request-keyfile: a tracked sat moved on-chain -- ask the Ames vane to fetch a
::  fresh keyfile for that ship, via the %attest-request task. (The vane CASE is
::  defined in this kernel's lull; its HANDLER is the to-be-implemented Ames side.
::  Ames will later reply with an %add-keyfile poke, closing the loop.)
++  request-keyfile
  |=  who=@p
  ^-  card
  [%pass /attest/request/(scot %p who) %arvo %a [%attest-request who]]
::
::  +subs-to-ships: the set of ships Jael is subscribed to (via /ship paths).
++  subs-to-ships
  |=  sup=bitt:gall
  ^-  (set ship)
  %-  silt
  %+  murn  ~(val by sup)
  |=  [ship =path]
  ^-  (unit ship)
  ?.  ?=([@ ~] path)  ~
  (slaw %p i.path)
::
::  +jael-update: emit %azimuth-udiffs facts (on / and per-/ship) for Jael.
++  jael-update
  |=  =udiffs:point:jael
  ^-  (list card)
  :-  [%give %fact ~[/] %azimuth-udiffs !>(udiffs)]
  ?~  udiffs  ~
  %+  turn  udiffs
  |=  [=ship =udiff:point:jael]
  ^-  card
  [%give %fact [/(scot %p ship)]~ %azimuth-udiffs !>([udiff]~)]
::
::  +point-to-udiffs: the four udiffs that describe one point to Jael.
++  point-to-udiffs
  |=  [=ship =point:urb =id:block:jael]
  ^-  udiffs:point:jael
  =,  point
  :~  [ship id %keys [life.net (sub (end 3 pass.net) 'a') pass.net] %.y]
      [ship id %rift rift.net %.y]
      [ship id %spon ?:(has.sponsor.net `who.sponsor.net ~)]
      [ship id %fief fief.net]
  ==
::
::  +state-to-udiffs: all stored points' udiffs, for the on-watch /ship replay.
++  state-to-udiffs
  |=  [points=(map @p point:urb) =id:block:jael]
  ^-  udiffs:point:jael
  =/  ps=(list [=ship point:urb])  ~(tap by points)
  =|  out=udiffs:point:jael
  |-  ^+  out
  ?~  ps  out
  $(ps t.ps, out (welp (point-to-udiffs ship.i.ps +.i.ps id) out))
--
