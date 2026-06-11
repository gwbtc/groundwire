::  %urb-watcher
::
::  This agent is the Groundwire equivalent of %azimuth and %eth-watcher.
::  It fetches Bitcoin blocks on a timer and parses them for Jael events.
::  Its helper core at the bottom works in conjunction with lib/urb-core.
::
::  It is also the registered handler agent for CONFIDENTIAL COMETS
::  (protocol 2.0; see sur/self-attestation and lib/self-attestation):
::
::    - A peer's self-attestation packet arrives as a %self-attestation poke
::      (from Ames, whose side of this does not exist yet -- our pokes to it
::      use placeholder marks against a nonexistent %ames agent). A khan
::      thread verifies the packet against the Bitcoin node; on success the
::      ship's point is stored in urb-state (and thus served to Jael) and its
::      ownership sat is tracked by the normal block machinery.
::    - When a tracked confidential sat MOVES with no on-chain sotx, we poke
::      Ames to request a fresh packet from that ship (%attestation-request);
::      every remote verification outcome is reported to Ames either way
::      (%attestation-verdict). If a confidential ship instead continues its
::      chain with a PUBLIC on-chain reveal, it permanently leaves the
::      confidential registry and is handled by classic chain-watching.
::    - The user pokes their OWN chain in as %attestation-keyfile. It is
::      verified eagerly: a bad keyfile only reports (no state change); a
::      good one is stored, its sat watched, and re-verified when that sat
::      is seen moving (failure there is unspecified behavior at launch).
::    - The snapshot endpoint serves urb-state FILTERED of confidential
::      ships, so confidential identities propagate only via packets.
::
::  Change new-rpc and start-height in ++init to change the network.
::  If you're using this in conjunction with the SPV wallet, that
::  will need to be pointed to the same Bitcoin network as this.
::  The RPC node must have -txindex enabled for ++get-raw-transaction to succeed,
::  which means it can't be pruned.
::
::  You may want to change block-confirmations as well.
::
/-  bitcoin, spider, ord, urb, sa=self-attestation
/+  bc=bitcoin, btcio, dbug, default-agent, uc=urb-core, strandio, verb, server,
    ol=ord, lsa=self-attestation
::
|%
+$  card  card:agent:gall
::  Confidential-comet registry entry: the tip sont of the last VERIFIED
::  self-attestation, and the landing sont we last asked Ames about (so a
::  move triggers exactly one request, but a second move re-requests).
+$  conf-meta
  $:  attested=sont:ord
      requested=(unit sont:ord)
  ==
+$  versioned-state  $%(state-0 state-1)
+$  state-0
  $:  %0
      rpc=req-to:btcio
      urb-state=state:urb
  ==
+$  state-1
  $:  %1
      rpc=req-to:btcio
      urb-state=state:urb
      conf=(map @p conf-meta)                     ::  confidential ships (incl. our own)
      keyfile=(unit self-attestation:sa)          ::  our own verified chain
      pending-keyfile=(unit self-attestation:sa)  ::  keyfile poked, verify in flight
      pending=(set @p)                            ::  in-flight remote verifications
  ==
--
::
%-  agent:dbug
^-  agent:gall
=|  state-1
=*  state  -
%+  verb  &
=<
|_  =bowl:gall
+*  this   .
    def    ~(. (default-agent this %|) bowl)
::
::  Tell Jael to subscribe to us for PKI updates,
::  initialize an urb-core, start a thread to
::  fetch and process the first batch of blocks,
::  and start a timer to fetch again.
++  on-init
  ^-  (quip card _this)
  =/  new-rpc  ['https://alpha.groundwire.dev/rpc' [%basic 'mainnetrpcuser:fc3d36ce83e15484e75a658b2a9a8a90a66f4cb017ace74c8631fe082b93adbf']]
  =/  start-height  943.140
  =/  start-hash    0x1.62b3.04e4.d48c.3a53.d80a.96de.0210.d325.c0a9.a464.8b3c
  =/  new-urb-state
    :*  [start-hash start-height]
        *sont-map:ord
        *insc-ids:ord
        *unv-ids:urb
    ==
  ::  Short-term performance hack: request a snapshot from the default sponsor.
  :_  this(rpc new-rpc, urb-state new-urb-state)
  ?:  =(our.bowl ~linluc-palnus-barpub-dalweg--miptyp-molfer-pitren-daplyd)
    :~  :*  %pass  /blocks  %arvo  %k
            %lard  q.byk.bowl
            (get-blocks new-rpc new-urb-state)
        ==
        :*  %pass  /eyre/connect  %arvo  %e
            %connect  `/apps/urb-watcher  dap.bowl
        ==
    ==
  ::  XX harness branch: ordinary comets stay INERT until a %watcher-config
  ::  poke points them at a node and starts the block loop. (The mainnet
  ::  snapshot bootstrap auto-fell into a runaway mainnet self-watch that
  ::  saturated regtest comets; production snapshot bootstrap is TODO here.)
  :~  :*  %pass  /eyre/connect  %arvo  %e
          %connect  `/apps/urb-watcher  dap.bowl
      ==
  ==
::
++  on-save
  ^-  vase
  !>(state)
::
++  on-load
  |=  =vase
  ^-  (quip card _this)
  =/  old  !<(versioned-state vase)
  ?-    -.old
      %1
    `this(state old)
  ::
      %0
    :_  this(state [%1 rpc.old urb-state.old ~ ~ ~ ~])
    ::  This wasn't bound in our first deployment:
    :~  :*  %pass  /eyre/connect  %arvo  %e
            %connect  `/apps/urb-watcher  dap.bowl
        ==
    ==
  ==
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?+    mark  !!
      %handle-http-request
    =+  !<([eyre-id=@ta =inbound-request:eyre] vase)
    =/  ,request-line:server
      (parse-request-line:server url.request.inbound-request)
    ?+    method.request.inbound-request  !!
        %'GET'
      ?+    site  !!
          [%apps %urb-watcher %snapshot ~]
        ::  XX should sign the snapshot with +sign:as:cic
        ::     if this is more than a short-term hack
        ::  Confidential ships are stripped: their identities propagate
        ::  only via self-attestation packets, never via snapshots.
        :_  this
        %+  give-simple-payload:app:server
          eyre-id
        ^-  simple-payload:http
        :_  `(as-octs:mimes:html (jam (filter-snapshot urb-state ~(key by conf))))
        [200 ~[['content-type' 'application/x-urb-jam']]]
      ==
    ::
    ::  The harness POSTs a jammed self-attestation SKELETON here (conn %fyrd
    ::  can't run non-base-desk threads). We cue it, re-derive the sots from
    ::  each leaf, and self-poke the resulting packet: /keyfile -> our own
    ::  chain (%attestation-keyfile); /peer -> a peer's packet
    ::  (%self-attestation).
        %'POST'
      ?.  ?|  =([%apps %urb-watcher %keyfile ~] site)
              =([%apps %urb-watcher %peer ~] site)
          ==
        !!
      =/  is-keyfile  =([%apps %urb-watcher %keyfile ~] site)
      =/  body  body.request.inbound-request
      =/  sat-u=(unit self-attestation:sa)
        ?~  body  ~
        =/  parsed  (mule |.(;;(skeleton:sa (cue q.u.body))))
        ?:  ?=(%| -.parsed)  ~
        (from-skeleton:lsa p.parsed)
      ?~  sat-u
        %-  (slog leaf+"%urb-watcher: bad skeleton POST" ~)
        :_  this
        (give-simple-payload:app:server eyre-id [[400 ~] ~])
      =/  poke-mark=@tas  ?:(is-keyfile %attestation-keyfile %self-attestation)
      :_  this
      %+  weld
        (give-simple-payload:app:server eyre-id [[200 ~] ~])
      ^-  (list card)
      :~  :*  %pass  /self/attest  %agent  [our.bowl %urb-watcher]
              %poke  poke-mark  !>(u.sat-u)
          ==
      ==
    ==
  ::
  ::  Repoint the watcher at a different Bitcoin node / start block and restart
  ::  the block loop. Used by the local regtest harness to override the
  ::  hardcoded mainnet defaults; self-poke only.
      %noun
    ?>  =(our.bowl src.bowl)
    =+  !<([%watcher-config url=@t auth=@t hash=@ux height=@ud] vase)
    =/  new-rpc=req-to:btcio  [url [%basic auth]]
    =/  new-urb=state:urb
      [[hash height] *sont-map:ord *insc-ids:ord *unv-ids:urb]
    ~&  >  "%urb-watcher: reconfigured to {(trip url)} from block {<height>}"
    :_  this(rpc new-rpc, urb-state new-urb, conf ~, keyfile ~, pending-keyfile ~, pending ~)
    :~  :*  %pass  /blocks  %arvo  %k  %lard  q.byk.bowl
            (get-blocks new-rpc new-urb)
        ==
    ==
  ::
  ::  A remote ship's self-attestation packet, relayed by Ames (placeholder:
  ::  that kernel functionality does not exist yet). Spawn a verification
  ::  thread; every outcome is reported to Ames via %attestation-verdict.
      %self-attestation
    =/  sat  !<(self-attestation:sa vase)
    =*  who  who.sat
    ?:  =(who our.bowl)
      %-  (slog leaf+"%urb-watcher: own ship must use %attestation-keyfile" ~)
      [~[(verdict-poke our.bowl who %.n)] this]
    ?:  (known-public who)
      ::  Permanently public per protocol: a ship that has revealed on-chain
      ::  cannot return to confidential status.
      %-  (slog leaf+"%urb-watcher: {<who>} is public on-chain; packet refused" ~)
      [~[(verdict-poke our.bowl who %.n)] this]
    ?:  (~(has in pending) who)
      %-  (slog leaf+"%urb-watcher: verification for {<who>} already in flight" ~)
      `this
    ?:  (gth (lent chain.sat) 1.024)
      %-  (slog leaf+"%urb-watcher: chain for {<who>} too long; refused" ~)
      [~[(verdict-poke our.bowl who %.n)] this]
    ~&  >  "%urb-watcher: verifying self-attestation for {<who>} ({<(lent chain.sat)>} links)..."
    :_  this(pending (~(put in pending) who))
    :~  :*  %pass  /verify/remote/(scot %p who)  %arvo  %k
            %lard  q.byk.bowl
            (verify:lsa rpc.state sat (tracked-sont urb-state who))
        ==
    ==
  ::
  ::  Our own updated attestation keyfile (from the user, e.g. via Causeway).
  ::  Verified eagerly: a bad keyfile must give feedback WITHOUT changing
  ::  state; a good one is stored and its sat watched.
      %attestation-keyfile
    =/  sat  !<(self-attestation:sa vase)
    ?.  =(who.sat our.bowl)
      %-  (slog leaf+"%urb-watcher: keyfile is for {<who.sat>}, not us; refused" ~)
      `this
    ?:  (known-public our.bowl)
      %-  (slog leaf+"%urb-watcher: we are public on-chain; keyfile refused" ~)
      `this
    ?^  pending-keyfile
      ::  One in-flight keyfile verification at a time: the thread result
      ::  doesn't echo the packet, so a second poke would make the ok-branch
      ::  store a packet the thread never verified.
      %-  (slog leaf+"%urb-watcher: a keyfile verification is already in flight; refused" ~)
      `this
    ?:  (gth (lent chain.sat) 1.024)
      %-  (slog leaf+"%urb-watcher: keyfile chain too long; refused" ~)
      `this
    ~&  >  "%urb-watcher: verifying attestation keyfile ({<(lent chain.sat)>} links)..."
    :_  this(pending-keyfile `sat)
    :~  :*  %pass  /verify/keyfile/new  %arvo  %k
            %lard  q.byk.bowl
            (verify:lsa rpc.state sat (tracked-sont urb-state our.bowl))
        ==
    ==
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
      [%x %point ship=@ ~]
    ?~  who=(slaw %p ship.pole)  ~
    ?~  point=(~(get by unv-ids.urb-state) u.who)
      [~ ~]
    ``urb-point+!>(u.point)
    ::  /x/points — all spawned identities
    ::
      [%x %points ~]
    ``urb-points+!>(unv-ids.urb-state)
    ::  /x/keyfile — our own attestation keyfile, if any
    ::  (Ames reads this to serve self-attestation packets to peers.)
    ::
      [%x %keyfile ~]
    ``noun+!>(keyfile)
    ::  /x/conf — the confidential-comet registry
    ::
      [%x %conf ~]
    ``noun+!>(conf)
  ==
::
++  on-watch
  |=  =(pole knot)
  ^-  (quip card _this)
  ?+    pole  (on-watch:def pole)
      [%http-response *]  `this
  ::
  ::  Jael subscribes to / (aka ~) if it hears
  ::  that this agent is the default PKI source.
  ::  Local-only: udiffs include confidential ships, whose identities must
  ::  propagate only via self-attestation packets.
      ~
    ?>  =(our src):bowl
    `this
  ::
  ::  Jael subcribes to /ship when it hears about a new ship
      [=ship ~]
    ?>  =(our src):bowl
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
            ?.  =(ship (slav %p ship.pole))
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
  ::  Send the iris snapshot request now that
  ::  the agent is fully initialized.
      [%init %snapshot ~]
    ?.  ?=([%behn %wake *] sign-arvo)  (on-arvo:def wire sign-arvo)
    ?^  error.sign-arvo
      %-  (slog leaf+"%urb-watcher: /init/snapshot timer error" ~)
      `this
    ~&  "%urb-watcher: requesting a snapshot from the default sponsor."
    :_  this
    :~  :*  %pass  /snapshot
            %arvo  %i
            %request
            ^-  request:http
            :*  %'GET'
                'http://143.198.70.9:8081/apps/urb-watcher/snapshot'
                :~  ['accept' 'application/x-urb-jam']
                ==
                ~
            ==
            *outbound-config:iris
        ==
    ==
  ::
  ::  Run +get-blocks at regular intervals.
      [%timer ~]
    :_  this
    :~  :*  %pass  /blocks  %arvo  %k
            %lard  q.byk.bowl
            (get-blocks [rpc urb-state]:state)
        ==
    ==
  ::
  ::  A remote self-attestation verification thread returned.
      [%verify %remote @ ~]
    =/  who  (slav %p i.t.t.wire)
    =.  pending  (~(del in pending) who)
    ?+    sign-arvo  (on-arvo:def wire sign-arvo)
        [%khan %arow *]
      ?.  -.p.sign-arvo
        ?>  ?=([%khan %arow %.n *] sign-arvo)
        %-  (slog leaf+"%urb-watcher: verify thread for {<who>} crashed" +.p.p.sign-arvo)
        [~[(verdict-poke our.bowl who %.n)] this]
      ?>  ?=([%khan %arow %.y %noun *] sign-arvo)
      =/  [%khan %arow %.y %noun =vase]  sign-arvo
      =/  res  !<(result:sa vase)
      %-  (slog (report:lsa verdict.res))
      ?.  ok.verdict.res
        [~[(verdict-poke our.bowl who %.n)] this]
      ::  TOCTOU: the ship may have continued publicly while we verified.
      ::  The packet itself was valid (tell Ames so), but apply nothing.
      ?:  (known-public who)
        %-  (slog leaf+"%urb-watcher: {<who>} went public mid-verification; not applying" ~)
        [~[(verdict-poke our.bowl who %.y)] this]
      ?~  point.res
        %-  (slog leaf+"%urb-watcher: {<who>} verified but produced no point; dropping" ~)
        [~[(verdict-poke our.bowl who %.n)] this]
      ::  Apply: store the point, track the tip sat, register as confidential,
      ::  and store the data in Jael (directly if it is already subscribed
      ::  to this ship, else via %listen and ++on-watch).
      =/  applied  (apply-verified who u.point.res tip-value.res)
      =/  jael-cards=(list card)
        ?:  (~(has in (subs-to-ships sup.bowl)) who)
          (jael-update (point-to-udiffs who u.point.res block-id.urb-state))
        ~[(listen-to-urb (silt ~[who]) [%| dap.bowl])]
      :_  this(urb-state.state -.applied, conf +.applied)
      [(verdict-poke our.bowl who %.y) jael-cards]
    ==
  ::
  ::  Our own keyfile verification returned (%new = fresh poke,
  ::  %recheck = re-verification after our sat was seen moving).
      [%verify %keyfile @ ~]
    =/  kind  i.t.t.wire
    ?+    sign-arvo  (on-arvo:def wire sign-arvo)
        [%khan %arow *]
      ?.  -.p.sign-arvo
        ?>  ?=([%khan %arow %.n *] sign-arvo)
        =/  msg=tank
          ?:  =(%new kind)
            leaf+"%urb-watcher: keyfile verification thread crashed"
          leaf+"%urb-watcher: keyfile re-verification crashed: unspecified behavior"
        %-  (slog msg +.p.p.sign-arvo)
        ::  Only a %new outcome owns the pending stash; a returning %recheck
        ::  must not discard a concurrently-poked fresh keyfile.
        =?  pending-keyfile  =(%new kind)  ~
        `this
      ?>  ?=([%khan %arow %.y %noun *] sign-arvo)
      =/  [%khan %arow %.y %noun =vase]  sign-arvo
      =/  res  !<(result:sa vase)
      %-  (slog (report:lsa verdict.res))
      ?.  ok.verdict.res
        ?:  =(%new kind)
          ::  Bad keyfile: feedback only, NO state change.
          `this(pending-keyfile ~)
        ::  Per the protocol, this outcome is unspecified at launch.
        %-  (slog leaf+"%urb-watcher: own keyfile re-verification failed: unspecified behavior" ~)
        `this
      ::  TOCTOU: we may have continued publicly while the thread ran;
      ::  a public ship cannot return to confidential status.
      ?:  (known-public our.bowl)
        %-  (slog leaf+"%urb-watcher: we went public mid-verification; keyfile not applied" ~)
        =?  pending-keyfile  =(%new kind)  ~
        `this
      ?~  point.res
        =?  pending-keyfile  =(%new kind)  ~
        `this
      ::  Good keyfile: store it, track our sat, register as confidential.
      ::  XX no self-udiffs to Jael: Jael is the SOURCE of our own keys,
      ::     not a consumer of them; feeding our own point back through
      ::     %azimuth-udiffs risks fighting its own-ship handling.
      =?  keyfile  =(%new kind)  pending-keyfile
      =?  pending-keyfile  =(%new kind)  ~
      =/  applied  (apply-verified our.bowl u.point.res tip-value.res)
      `this(urb-state.state -.applied, conf +.applied)
    ==
  ::
  ::  Receive a snapshot from the default sponsor
  ::  containing an urb-state and tell Jael to subscribe
  ::  to %urb-watcher for udiffs for each ship in that
  ::  urb-state, which we'll fulfill immediately in ++on-agent.
  ::  Now start indexing ourselves.
      [%snapshot ~]
    ?.  ?=([%iris %http-response *] sign-arvo)  [(snapshot-fail bowl) this]
    =/  response  client-response.sign-arvo
    ?+    -.response  [(snapshot-fail bowl) this]
        %finished
      ?~  full-file.response  [(snapshot-fail bowl) this]
      =/  =mime-data:iris  u.full-file.response
      ?+    type.mime-data  [(snapshot-fail bowl) this]
          %'application/x-urb-jam'
        ::  XX if we implement signed snapshots,
        ::     verify here with +sure:as:cic
        =/  new-urb=state:urb  ;;(state:urb (cue q.data.mime-data))
        ~&  >  '%urb-watcher received a snapshot! Now beginning indexing from its latest block.'
        ::  Re-apply any confidential points verified while the snapshot
        ::  request was in flight (snapshots are served conf-filtered, so
        ::  a wholesale assignment would drop them unrecoverably).
        :_  this(urb-state (reapply-conf-points urb-state new-urb))
        :~  [%pass /timer %arvo %b %wait (add ~s30 now.bowl)]
            (listen-to-urb ~ [%| dap.bowl])
        ==
      ==
    ==
  ::
  ::  Our +get-blocks thread returned. Update
  ::  urb-state, emit udiffs to Jael, and set a timer
  ::  to run the thread again.
      [%blocks ~]
    ?+    sign-arvo  (on-arvo:def wire sign-arvo)
        [%khan %arow *]
      ?.  -.p.sign-arvo
        ?>  ?=([%khan %arow %.n *] sign-arvo)
        %-  (slog leaf+"%urb-watcher: thread failed, retrying" +.p.p.sign-arvo)
        :_  this
        :~  [%pass /timer %arvo %b %wait (add ~s30 now.bowl)]
        ==
      ?>  ?=([%khan %arow %.y %noun *] sign-arvo)
      =/  [%khan %arow %.y %noun =vase]  sign-arvo
      =/  fx-and-state
        !<
        [(list [id:block:bitcoin effect:urb]) state:urb]
        vase
      ::  The thread computed from a snapshot of urb-state taken when it was
      ::  spawned; confidential points verified and applied while it was in
      ::  flight would be silently dropped by wholesale assignment. Re-apply
      ::  them onto the incoming state before anything else.
      =/  merged=state:urb
        (reapply-conf-points urb-state.state +.fx-and-state)
      ::  Jael is subscribed to %urb-watcher to receive udiffs for some ships,
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
      =/  tracked-ships=(set ship)  (subs-to-ships sup.bowl)
      ::
      =/  filtered-udiffs=udiffs:point:jael
        %+  murn
          (fx-to-udiffs -.fx-and-state)
        |=  [=ship =udiff:point:jael]
        ^-  (unit [^ship udiff:point:jael])
        ::  ignore ships Jael hasn't subscribed to yet
        ?.  (~(has in tracked-ships) ship)
          ~
        `[ship udiff]
      ::
      ::  Scan the confidential registry against the merged state: detect
      ::  public continuations, confidential sat moves (-> ask Ames for a
      ::  fresh packet), and our own sat moving (-> re-verify the keyfile).
      =/  scan  (scan-conf fx-ships merged bowl)
      =/  recheck-cards=(list card)
        ?~  keyfile  ~
        ?.  &(own-moved.scan !own-public.scan)  ~
        :_  ~
        :*  %pass  /verify/keyfile/recheck  %arvo  %k
            %lard  q.byk.bowl
            (verify:lsa rpc.state u.keyfile (tracked-sont merged our.bowl))
        ==
      ::
      :_  %_  this
            urb-state.state  merged
            conf             conf.scan
            keyfile          ?:(own-public.scan ~ keyfile)
          ==
      ;:  welp
        ^-  (list card)
        ?:  =(~ fx-ships)  ~
        ::  don't send a %listen task for ships
        ::  that Jael is already subscribed to
        ~[(listen-to-urb (~(dif in fx-ships) tracked-ships) [%| dap.bowl])]
      ::
        ~[[%pass /timer %arvo %b %wait (add ~s30 now.bowl)]]
        (jael-update filtered-udiffs)
        cards.scan
        recheck-cards
      ==
    ==
  ==
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  ?+    wire  (on-agent:def wire sign)
    ::  Placeholder Ames pokes inevitably nack (the %ames agent doesn't
    ::  exist yet); swallow them quietly.
      [%ames *]
    ?.  ?=(%poke-ack -.sign)  (on-agent:def wire sign)
    `this
  ==
++  on-leave  on-leave:def
++  on-fail   on-fail:def
--
::
|%
++  snapshot-fail
  |=  =bowl:gall
  ~&  >>  "%urb-watcher's request for a snapshot failed. Beginning self-chain-watching."
  ^-  (list card)
  :~  :*  %pass  /blocks  %arvo  %k
          %lard  q.byk.bowl
          (get-blocks [rpc urb-state]:state)
      ==
  ==
::
::  Confidential-comet helpers.
::
::  Is this ship known to be PUBLIC on-chain? (In unv-ids but not in the
::  confidential registry: it spawned or continued via an on-chain reveal,
::  which is permanent per the protocol.)
++  known-public
  |=  who=@p
  ^-  ?
  ?&  (~(has by unv-ids.urb-state) who)
      !(~(has by conf) who)
  ==
::
::  The sont we currently track for a ship, for the verifier's tracked-tip
::  reconciliation. ~ if untracked or lost ([0x0 0 0] is urb-core's
::  landed-in-the-miner-fee sentinel, meaningless to compare against).
++  tracked-sont
  |=  [st=state:urb who=@p]
  ^-  (unit sont:ord)
  ?~  point=(~(get by unv-ids.st) who)  ~
  ?:  =([0x0 0 0] sont.own.u.point)  ~
  `sont.own.u.point
::
::  Apply a verified attestation: store the point, drop any previously
::  tracked sat, track the tip sat (with its REAL on-chain output value --
::  find-block-reveals reuses it for sum-in math), and register the ship in
::  the confidential registry. Produces the new [urb-state conf].
++  apply-verified
  |=  [who=@p new=point:urb tip-value=@ud]
  ^-  [state:urb (map @p conf-meta)]
  =/  st  urb-state
  =/  old  (~(get by unv-ids.st) who)
  =.  sont-map.st
    ?~  old  sont-map.st
    ?:  =(sont.own.u.old sont.own.new)  sont-map.st
    ?:  =([0x0 0 0] sont.own.u.old)  sont-map.st
    (del:si:ol sont-map.st [txid vout off]:sont.own.u.old)
  =.  unv-ids.st  (~(put by unv-ids.st) who new)
  =.  sont-map.st
    %:  put-com:si:ol
        sont-map.st
        txid.sont.own.new
        vout.sont.own.new
        off.sont.own.new
        tip-value
        who
    ==
  [st (~(put by conf) who [attested=sont.own.new requested=~])]
::
::  Re-apply confidential points onto a block thread's result state. The
::  thread computed from a snapshot; any point applied by a verification
::  that completed while it was in flight is missing from the thread's
::  unv-ids and would be silently dropped by wholesale assignment. A ship
::  the thread DOES know wins as-is (it may have tracked the sat's moves).
::  XX residual windows, accepted for the prototype: a restored sat that
::  moved within the very blocks the thread processed points at a spent
::  output until the next packet; a RE-verification (already-known ship)
::  completing mid-flight is reverted, after which the scan re-requests a
::  packet -- chatty but convergent.
++  reapply-conf-points
  |=  [old=state:urb new=state:urb]
  ^-  state:urb
  =/  entries  ~(tap by conf)
  |-
  ?~  entries  new
  =/  [who=@p meta=conf-meta]  i.entries
  ?:  (~(has by unv-ids.new) who)
    $(entries t.entries)
  =/  point  (~(get by unv-ids.old) who)
  ?~  point
    $(entries t.entries)
  ~&  >>  "%urb-watcher: restoring {<who>}, clobbered by the block thread"
  =/  sont  sont.own.u.point
  =.  unv-ids.new  (~(put by unv-ids.new) who u.point)
  =/  vm
    ?:  =([0x0 0 0] sont)  ~
    (get-vout:si:ol sont-map.old [txid vout]:sont)
  =?  sont-map.new  ?=(^ vm)
    (put-com:si:ol sont-map.new txid.sont vout.sont off.sont value.u.vm who)
  $(entries t.entries)
::
::  Scan the confidential registry against post-block state. Classifies
::  each ship: public continuation (its own on-chain sotx moved its sat),
::  sponsor-side %point effect (sat unmoved -- stays confidential),
::  confidential move (sat moved, no sotx -> ask Ames for a fresh packet),
::  or steady state.
++  scan-conf
  |=  [fx-ships=(set ship) st=state:urb =bowl:gall]
  ^-  [cards=(list card) conf=(map @p conf-meta) own-moved=? own-public=?]
  =/  entries  ~(tap by conf)
  =/  new-conf  conf
  =|  cards=(list card)
  =|  own-moved=_|
  =|  own-public=_|
  |-
  ?~  entries  [(flop cards) new-conf own-moved own-public]
  =/  [who=@p meta=conf-meta]  i.entries
  =/  point  (~(get by unv-ids.st) who)
  ?~  point
    %-  (slog leaf+"%urb-watcher: conf ship {<who>} missing from unv-ids; dropping" ~)
    $(entries t.entries, new-conf (~(del by new-conf) who))
  =/  now-sont  sont.own.u.point
  =/  moved=?  !=(now-sont attested.meta)
  ?:  (~(has in fx-ships) who)
    ?.  moved
      ::  A %point effect with the sat unmoved is a sponsor-side action
      ::  (the parent's %adopt/%reject/%detach): the ship did nothing
      ::  on-chain itself, so it stays confidential.
      ::  XX undecidable corner: a sponsor-side action AND a confidential
      ::     move landing in the same batch misclassifies as public.
      $(entries t.entries)
    ::  Public continuation: the ship's own on-chain sotx moved its sat.
    ::  Permanent; classic chain-watching handles it from here.
    %-  (slog leaf+"%urb-watcher: {<who>} continued publicly; now a public comet" ~)
    %=  $
      entries     t.entries
      new-conf    (~(del by new-conf) who)
      own-public  |(own-public =(who our.bowl))
    ==
  ?.  moved
    $(entries t.entries)
  ::  Confidential move: the tracked sat moved with no on-chain sotx.
  ::  Ask Ames to request a fresh packet, once per landing sont.
  ~?  =([0x0 0 0] now-sont)
    "%urb-watcher: conf ship {<who>}'s sat lost to fee/tracking; requesting anyway"
  =.  own-moved  |(own-moved =(who our.bowl))
  ?:  =(who our.bowl)
    ::  Our own move is handled by the keyfile recheck, not an Ames request.
    $(entries t.entries)
  ?:  =(`now-sont requested.meta)
    $(entries t.entries)
  %=  $
    entries   t.entries
    cards     [(request-poke our.bowl who) cards]
    new-conf  (~(put by new-conf) who meta(requested `now-sont))
  ==
::
::  Serve urb-state with confidential ships stripped: their points removed
::  and their sats dropped from the sat index. Conf identities propagate
::  only via self-attestation packets, never via snapshots.
::  XX insc-ids is left untouched: confidential chains are key-path-only
::     spends, which cannot carry inscriptions.
++  filter-snapshot
  |=  [st=state:urb conf-ships=(set @p)]
  ^-  state:urb
  ?:  =(~ conf-ships)  st
  =.  unv-ids.st
    =/  ships  ~(tap in conf-ships)
    |-  ^+  unv-ids.st
    ?~  ships  unv-ids.st
    $(ships t.ships, unv-ids.st (~(del by unv-ids.st) i.ships))
  =.  sont-map.st
    =/  entries  ~(tap by sont-map.st)
    =|  acc=sont-map:ord
    |-  ^-  sont-map:ord
    ?~  entries  acc
    =/  [k=[=txid:ord =vout:ord] vm=vout-map:ord]  i.entries
    =/  sats
      %-  ~(gas by *(map off:ord sont-val:ord))
      %+  skip  ~(tap by sats.vm)
      |=  [o=off:ord sv=sont-val:ord]
      &(?=(^ com.sv) (~(has in conf-ships) u.com.sv))
    ?:  =(~ sats)
      $(entries t.entries)
    $(entries t.entries, acc (~(put by acc) k vm(sats sats)))
  st
::
::  The set of ships Jael is subscribed to (via /ship paths).
::  XX this replaces an inline version that cast the path knot directly to
::     @p without parsing, so it never matched a real ship.
++  subs-to-ships
  |=  sup=bitt:gall
  ^-  (set ship)
  %-  silt
  %+  murn
    ~(val by sup)
  |=  [ship =path]
  ^-  (unit ship)
  ?.  ?=([@ ~] path)
    ~
  (slaw %p i.path)
::
::  Placeholder pokes to the (assumed, nonexistent) Ames agent.
++  request-poke
  |=  [our=ship who=@p]
  ^-  card
  :*  %pass  /ames/request/(scot %p who)
      %agent  [our %ames]
      %poke  %attestation-request
      !>(who)
  ==
::
++  verdict-poke
  |=  [our=ship who=@p ok=?]
  ^-  card
  :*  %pass  /ames/verdict/(scot %p who)
      %agent  [our %ames]
      %poke  %attestation-verdict
      !>([who ok])
  ==
::
::  Fetch blocks in range(last-processed + 1, latest - block-confirmations)
::  from the provided RPC endpoint, then use a stateful 
::  urb-core to process these blocks, returning
::  a new urb-state and a list of fx in +on-arvo.
++  get-blocks
  |=  [rpc=req-to:btcio urb-state=state:urb]
  ^-  shed:khan
  =/  block-confirmations  1 :: 1 for alpha
  =/  i  (add block-confirmations num.block-id.urb-state) :: last processed block height + 1
  =/  uc
    %-  abed:urb-core:uc
    urb-state
  |^
  =/  m  (strand:strandio ,vase)
  ;<    latest-block=(unit @ud)
      bind:m
    (get-block-count:btcio rpc ~)
  ?~  latest-block  ~|  %couldnt-find-latest-block  !!
  ::  ~&  >  "latest block is {<u.latest-block>}"
  =/  last-settled-block  (sub u.latest-block block-confirmations)
  |-  
  ?.  (lte i last-settled-block)
    (pure:m !>([fx state]:uc))
  ;<    bluck=(unit block:bitcoin)
      bind:m
    (get-block-by-number:btcio rpc ~ i)
  ?~  bluck  ~|  %cant-find-block-by-number  !!
  ;<    new=urb-block:urb
      bind:m
    (convert-block i u.bluck)
  ::  ~&  >>  [%new new]
  ::
  ::  Find all %spawn sotx in the urb-block. For each %spawn, ++get-raw-transaction 
  ::  the commit tx and the precommit tx, which are needed to accurately track the sat.
  ::  (This assumes one %spawn per reveal transaction.)
  =|  precommits=(map [txid:ord vout:ord] [commit=urb-tx:urb precommit=urb-tx:urb])
  =/  txs  txs.new
  ::  Check all txs for %spawns
  |-
  ?~  txs
    =.  uc  (handle-block:uc new precommits)
    ~&  >  "processed block {<i>} of {<last-settled-block>}"
    ^$(i +(i))
  =/  tx-inputs  is.i.txs
  ::  Check all inputs for a %spawn. There could be multiple spawning
  ::  commit inputs to a single reveal tx
  |-  
  ?~  tx-inputs
    ^$(txs t.txs)
  =/  sots  sots.i.tx-inputs
  ?~  sots
    $(tx-inputs t.tx-inputs)
  =/  sots=(list single:skim-sotx:urb)  :: bad name shadowing
    ?:  ?=(%batch +<.sot.i.sots) 
      bat.sot.i.sots 
    ~[+.sot.i.sots]
  |-
  ?~  sots  
    ^$(tx-inputs t.tx-inputs)
  ?.  ?=(%spawn -.i.sots)
    $(sots t.sots)
  ::  ~&  >>  "%urb-watcher found a spawn!"
  ::  If we found an input with a %spawn, get the tx that generated it
  ;<  commit-tx=(unit tx:bc)  bind:m
    (get-raw-transaction:btcio rpc ~ txid.i.tx-inputs)
  ?~  commit-tx  ~|  %couldnt-fetch-tx  !!
  ;<    commit-urb-tx=urb-tx:urb  
      bind:m
    (convert-tx u.commit-tx)
  ::  ~&  >>  [%commit-tx id.commit-urb-tx]
  ::  Now find the commit tx input that matches attested spkh to get precommit tx.
  ::  (There could technically be multiple that match; we assume the first.)
  ::  To do this, we need one more inner loop to get the values of all outputs
  ::  of the precommit tx, to calculate the potential spkhs.
  =/  spkh  spkh.to.i.sots
  =/  inputs  is.commit-urb-tx
  |-
  ?~  inputs
    ::  ~&  >>>  "%urb-watcher: Couldn't find precommit tx."
    ^$(sots t.sots)
  ;<  precommit-tx=(unit tx:bc)  bind:m
    (get-raw-transaction:btcio rpc ~ txid.i.inputs)
  ?~  precommit-tx  ~|  %couldnt-fetch-tx  !!
  =/  outputs  os.u.precommit-tx
  |- 
  ?~  outputs
    ^$(inputs t.inputs)
  =/  en-out  (can 3 script-pubkey.i.outputs 8^value.i.outputs ~)  :: value as 8 bytes
  ?.  =(spkh (shay (add 8 wid.script-pubkey.i.outputs) en-out))
    $(outputs t.outputs)
  ;<    precommit-urb-tx=urb-tx:urb  
      bind:m
    (convert-tx u.precommit-tx)
  ::  ~&  >>  [%precommit-tx id.precommit-urb-tx]
  %=  ^^^$
    tx-inputs   t.tx-inputs
    precommits  %+  ~(put by precommits) 
                  [txid.i.tx-inputs pos.i.tx-inputs]
                [commit-urb-tx precommit-urb-tx]
  ==  
  ::
  ::  Convert a block:bitcoin into a urb-block:urb.
  ::  This requires an async +get-raw-transaction call.
  ++  convert-block
    |=  [=num:id:block:bitcoin =block:bitcoin]
    =/  m  (strand:strandio ,urb-block:urb)
    ::  XX Like urb-block, block:bitcoin apparently doesn't actually
    ::     include its num yet either
    :: ?.  =(num num:block)
    ::   ~&  >>  "error: %ord-watcher's num != num:block"
    ::   !!
    ::  Filter block to urb-relevant txs.
    ::  ~&  >>  "Filtering block {<i>}"
    =/  revs-and-block  (find-block-reveals:uc block)
    =/  reveals   -.revs-and-block
    ::  ~&  [%reveals reveals]
    =/  block  +.revs-and-block
    ::  ~&  [%block block]
    =/  txs    (tail txs.block)  :: cb has no prevouts
    ::
    ::  A block:btc does not include input values, but we need those for sont
    ::  math, so for every remaining tx in our filtered block:btc, fetch the
    ::  prev-tx that generated each of its inputs, get all outputs of
    ::  that prev-tx, and associate it with that utxo in the reveals map.
    ::  
    ::  ("deps" probably was a better name, then)
    |-  
    ^-  form:m
    ?~  txs
      ::  ~&  >>  "Applying prevouts to block {<i>}"
      (pure:m (apply-prevouts-and-urbify:uc block reveals))
    =/  inputs  is.i.txs
    :: ~&  [%inputs inputs]
    |-  
    ^-  form:m
    :: XX refactor to use gettxout
    ?~  inputs  
      ^$(txs t.txs)
    =/  rev  (~(get by reveals) [txid pos]:i.inputs)
    :: ~&  [%rev rev]
    ?:  &(?=(^ rev) ?=(^ value.u.rev))
      $(inputs t.inputs)
    :: ~&  'fetching prev-tx'
    ;<  prev-tx=(unit tx:bc)  bind:m
      (get-raw-transaction:btcio rpc ~ txid.i.inputs)
    :: ~&  [%prev-tx prev-tx]
    ?~  prev-tx  ~|  %couldnt-fetch-tx  !!
    =/  prev-outputs  os.u.prev-tx
    =|  pos=@ud
    |-  
    ^-  form:m
    ?~  prev-outputs  
      ^$(inputs t.inputs)
    =/  rev  (~(get by reveals) [id.u.prev-tx pos])
    ?:  &(?=(^ rev) ?=(^ value.u.rev))  
      $(prev-outputs t.prev-outputs, pos +(pos))
    =/  sots=(list raw-sotx:urb)  ?~(rev ~ sots.u.rev)
    %=  $
      prev-outputs  t.prev-outputs
      pos  +(pos)
      reveals  (~(put by reveals) [id.u.prev-tx pos] [sots `value.i.prev-outputs])
    ==
  ::
  ::  Use a similar loop to ++convert-block to convert
  ::  a single tx:bitcoin to urb-tx:urb, but without regard
  ::  for sots, only values.
  ++  convert-tx
    |=  old-tx=tx:bc
    =/  m  (strand:strandio ,urb-tx:urb)
    =/  old-inputs  is.old-tx
    =|  new-inputs=(list [[sots=(list raw-sotx:urb) value=@ud] inputw:tx:bitcoin])
    |-  
    ^-  form:m
    ?~  old-inputs  
      %-  pure:m
      :*  id.old-tx
          new-inputs
          os.old-tx
          locktime.old-tx
          nversion.old-tx
          segwit.old-tx
      ==
    ;<  prev-tx=(unit tx:bc)  bind:m
      (get-raw-transaction:btcio rpc ~ txid.i.old-inputs)
    ?~  prev-tx  ~|  %couldnt-fetch-tx  !!
    =/  prev-outputs  os.u.prev-tx
    =|  pos=@ud
    |-  
    ^-  form:m
    ?~  prev-outputs  
      ^$(old-inputs t.old-inputs)
    ?.  ?&  =(id.u.prev-tx txid.i.old-inputs) 
            =(pos pos.i.old-inputs)
        ==
      $(prev-outputs t.prev-outputs, pos +(pos))
    =/  new-input  [[~ value.i.prev-outputs] i.old-inputs]
    %=  ^$
      old-inputs  t.old-inputs
      new-inputs  [new-input new-inputs]
    ==
  --
::
::  Conversion arms. 
::  fx are urb-core's type for urb effects. 
::  udiffs are Jael's type for PKI updates. 
::  cards for Jael contain udiffs.
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
  [%give %fact [/(scot %p ship)]~ %azimuth-udiffs !>([udiff]~)]
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
      %sponsor
    `[ship.eu id %spon sponsor.pdiff]
  ::
      %keys
    `[ship.eu id %keys [life.pdiff (sub (end 3 pass.pdiff) 'a') pass.pdiff] %.y]
  ::
      %fief
    `[ship.eu id %fief fief.pdiff]
  ==
::
::  The four udiffs that describe one point to Jael.
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
    points      t.points
    new-udiffs  %+  welp
                  (point-to-udiffs ship.i.points +.i.points id)
                new-udiffs
  ==
--
