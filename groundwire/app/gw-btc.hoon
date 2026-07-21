::  %gw-btc (né %urb-watcher, né %groundwire)
::
::  This agent is the Groundwire equivalent of %azimuth and %eth-watcher.
::  It fetches Bitcoin blocks on a timer and parses them for Jael events.
::  Its helper core at the bottom works in conjunction with lib/urb-core.
::
::  It is also the verifier agent for the %gw-btc PKI domain in the
::  confidential-comets kernel protocol (see the companion spec
::  doc/confidential-comets-agent.md and, in gwbtc/urbit,
::  pkg/arvo/doc/spec/confidential-comets.md).  On boot it registers
::  itself with Jael via a %anex task, so that when a confidential
::  (suite-%c) comet self-attests to us, Ames routes the attestation
::  here as a %jael-writ poke.  We answer with a %writ-response fact; on
::  success Jael stores the point and promotes the comet.  Two verify
::  paths:
::    - INDEXED (fast, synchronous): the comet did an on-chain reveal our
::      block-watcher already parsed into unv-ids -- consult that point.
::    - CONFIDENTIAL (async khan thread): the comet is unknown to our
::      index, so run lib/gw-verify's full section-7 custody walk against
::      our bitcoin node, answering with the fact when the thread returns.
::  A %jael-anew poke (our own comet asking for a fresh attestation) is
::  answered with an %anew-response fact carrying the re-encoded pass.
::
::  The domain name is this agent's name (1:1 by construction): a comet
::  commits the tag %gw-btc in its key tweak, and Jael derives the same
::  tag from the gall duct our %anex arrives on.  Because the tag is
::  hashed into every comet's signing key (and thus its @p), it names the
::  Groundwire Bitcoin PKI DOMAIN, not this implementation -- do not
::  rename it to track code changes.
::
::  Change new-rpc and start-height in ++init to change the network.
::  If you're using this in conjunction with the SPV wallet, that
::  will need to be pointed to the same Bitcoin network as this.
::  The RPC node must have -txindex enabled for ++get-raw-transaction to succeed,
::  which means it can't be pruned.
::
::  You may want to change block-confirmations as well.
::
/-  bitcoin, spider, ord, urb
/+  bc=bitcoin, btcio, dbug, default-agent, uc=urb-core, strandio, verb
/+  gwv=gw-verify
::
|%
+$  card  card:agent:gall
+$  state-0
  $:  %0
      rpc=req-to:btcio
      urb-state=state:urb
  ==
--
::
%-  agent:dbug
^-  agent:gall
=|  state-0
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
  :_  %=  this
        rpc  :*  'https://alpha.groundwire.dev/rpc'
                 %basic
                 'mainnetrpcuser:fc3d36ce83e15484e75a658b2a9a8a90a66f4cb017ace74c8631fe082b93adbf'
             ==
      ==
  :~  [%pass /anex %arvo %j %anex /writs]
  ==
::
++  on-save
  ^-  vase
  !>(state)
::
++  on-load
  |=  =vase
  ^-  (quip card _this)
  `this(state !<(state-0 vase))
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?+    mark  !!
      ::  Jael forwards a comet self-attestation for on-chain verification
      ::  (%jael-writ) or asks us to refresh our own (%jael-anew).  Both are
      ::  local vane->agent pokes ([our our /jael]), so gate on src.
      ::
      %noun
    ?.  =(our.bowl src.bowl)
      ~&  >>>  [%gw-btc %foreign-noun-poke src.bowl]
      `this
    =/  poke  !<(jael-poke:urb vase)
    ?-    -.poke
        %jael-writ
      ::  INDEXED fast path: only a SUCCESSFUL indexed verify short-circuits.
      ::  If our block-watcher parsed this comet's on-chain reveal into
      ::  unv-ids and it still validates, answer synchronously; otherwise
      ::  fall through to the confidential walk (an indexed comet may have
      ::  rotated its key off-chain via a %state commitment the walk
      ::  validates -- so a stale indexed key must NOT hard-fail the writ).
      =/  ind=(unit point:jael)
        ?~  pt=(~(get by unv-ids.urb-state) who.poke)  ~
        (verify-indexed dom.poke who.poke pass.poke u.pt)
      ?^  ind
        :_  this
        :~  :*  %give  %fact  ~[/writs]
                %writ-response  !>(`writ-response:jael`[dom.poke who.poke ind])
            ==
        ==
      ::  CONFIDENTIAL path (unindexed, or indexed-but-unverified): run
      ::  lib/gw-verify's full section-7 custody walk in a khan thread; the
      ::  %writ-response fact is emitted when it returns (the [%writ @ @ ~]
      ::  case in +on-arvo).  dom + who ride the wire so the response is
      ::  answered under the routed domain, not just our name.
      :_  this
      :~  :*  %pass  /writ/(scot %tas dom.poke)/(scot %p who.poke)  %arvo  %k
              %lard  q.byk.bowl
              (writ-shed dom.poke who.poke pass.poke ~(key by unv-ids.urb-state) rpc)
          ==
      ==
    ::
        %jael-anew
      ::  our own comet asking for a fresh self-attestation.  re-encode
      ::  our current pass (the block-watcher keeps xtr current); if we
      ::  don't index ourselves yet, stay silent.
      ::
      ?~  pas=(fresh-pass our.bowl)
        `this
      :_  this
      :~  :*  %give  %fact  ~[/writs]
              %anew-response  !>(`anew-response:jael`[dom.poke u.pas])
          ==
      ==
    ==
  ::
      %urb-start-indexing
    =/  start-urb  ;;((unit state:urb) !<((unit noun) vase))
    ?~  start-urb
      %-  (slog :_(~ [%leaf "%gw-btc: indexing from block {<num.block-id:(state:urb default-urb-state)>}"]))
      :_  this(urb-state default-urb-state)
      :~  :*  %pass  /timer
              %arvo  %b
              %wait  now.bowl
          ==
      ==
    %-  (slog :_(~ [%leaf "%gw-btc: processing groundwire snapshot ({<~(wyt by unv-ids.u.start-urb)>} points)"]))
    :_  this(urb-state u.start-urb)
    :~  (listen-to-urb ~(key by unv-ids.u.start-urb) [%| dap.bowl])
        :*  %pass  /timer
            %arvo  %b
            %wait  (add ~s30 now.bowl)
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
    ::  /x/urb-state — entire urb-state for snapshots
    ::
      [%x %urb-state ~]
    ``noun+!>(urb-state)
  ==
::
++  on-watch
  |=  =(pole knot)
  ^-  (quip card _this)
  ?+    pole  (on-watch:def pole)
  ::
  ::  %urb-snapshot listens for new urb-states
      [%urb-state ~]
    :_  this
    :~  :*  %give  %fact  ~
            %urb-state  !>(urb-state)
        ==
    ==
  ::
  ::  Jael subscribes to / (aka ~) if it hears
  ::  that this agent is the default PKI source
      ~
    `this
  ::
  ::  Jael subcribes to /ship when it hears about a new ship
      [=ship ~]
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
  ::  A confidential +writ-shed verify thread returned.  Give the
  ::  %writ-response fact carrying its verdict (or ~ on failure / crash).
  ::  dom + who ride the wire (/writ/<dom>/<who>).
      [%writ @ @ ~]
    ?+    sign-arvo  (on-arvo:def wire sign-arvo)
        [%khan %arow *]
      =/  dom=@tas  (slav %tas i.t.wire)
      =/  who=ship  (slav %p i.t.t.wire)
      =/  res=(unit point:jael)
        ?.  ?=([%khan %arow %.y %noun *] sign-arvo)
          ::  thread bailed (rpc failure, unparsable packet): report failure.
          %-  (slog leaf+"%gw-btc: writ verify thread failed for {<who>}" ~)
          ~
        =/  [%khan %arow %.y %noun =vase]  sign-arvo
        !<((unit point:jael) vase)
      :_  this
      :~  :*  %give  %fact  ~[/writs]
              %writ-response  !>(`writ-response:jael`[dom who res])
          ==
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
  ::  Our +get-blocks thread returned. Update
  ::  urb-state, emit udiffs to Jael and full
  ::  urb-state snapshots to /urb-state watchers,
  ::  and set a timer to run the thread again.
      [%blocks ~]
    ?+    sign-arvo  (on-arvo:def wire sign-arvo)
        [%khan %arow *]
      ?.  -.p.sign-arvo
        ?>  ?=([%khan %arow %.n *] sign-arvo)
        %-  (slog leaf+"%gw-btc: thread failed, retrying" +.p.p.sign-arvo)
        :_  this
        :~  [%pass /timer %arvo %b %wait (add ~s30 now.bowl)]
        ==
      ?>  ?=([%khan %arow %.y %noun *] sign-arvo)
      =/  [%khan %arow %.y %noun =vase]  sign-arvo
      =/  fx-and-state
        !<  
        [(list [id:block:bitcoin effect:urb]) state:urb]
        vase
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
        %-  silt
        %+  murn
          ~(val by sup.bowl)
        |=  [ship =path]
        ^-  (unit ship)
        ::  ignore subscriptions that aren't to a /ship
        ?.  ?=([@p ~] path)
          ~
        `i.path
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
      =/  new-urb-state  +.fx-and-state
      :_  this(urb-state.state new-urb-state)
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
        :~  [%pass /timer %arvo %b %wait (add ~s30 now.bowl)]
        ==
      ?:  =(num.block-id.new-urb-state num.block-id.urb-state.state)
        ~
      :~  [%give %fact ~[/urb-state] %urb-state !>(new-urb-state)]
      ==
    ==
  ==
::
++  on-agent  on-agent:def
++  on-leave  on-leave:def
++  on-fail   on-fail:def
--
::
|%
::
::  Hard-coded initial sync state used if
::  %urb-start-indexing receives a null snapshot
++  default-urb-state
  ^-  state:urb
  =/  start-height  959.031
  =/  start-hash    0x1.b6e0.9c0a.aa11.0057.1fc0.7062.bae4.4418.da9d.d14c.bf55
  :*  [start-hash start-height]
      *sont-map:ord
      *insc-ids:ord
      *unv-ids:urb
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
  ::  ~&  >>  "%gw-btc found a spawn!"
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
    ::  ~&  >>>  "%gw-btc: Couldn't find precommit tx."
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
::  Confidential-comets verifier arms (see on-poke).
::
::  +verify-indexed: verify a comet's self-attestation against a point our
::  block-watcher already parsed from an on-chain reveal, returning the
::  verified Jael point or ~.  The caller looked the point up in unv-ids.
::
++  verify-indexed
  |=  [dom=@tas who=ship =pass pt=point:urb]
  ^-  (unit point:jael)
  ::  1. suite-%c pass, carrying the tweak-committed domain at the head
  ::     of its tweak data; it must match the domain Jael routed on
  ::
  =/  cek  +<:(com:nu:cric:crypto pass)
  ?.  ?=([%c *] cek)  ~
  ?.  =(dom `@tas`q:(rub 0 dat.tw.pub.cek))  ~
  ::  2. the name must be the hash of the tweaked key (Ames/Jael
  ::     already checked this; re-derive rather than trust)
  ?.  =(who fig:ex:(com:nu:cric:crypto pass))  ~
  ::  3. the attested MESSAGING KEY (cry) must match the one we indexed as
  ::     current.  Compare cry, not the whole pass: the pass's xtr reveal
  ::     log grows independently of the key, so a whole-pass compare would
  ::     spuriously reject a comet whose attestation carries a longer log
  ::     than the (public) reveal we parsed.  (The signing key that fixes
  ::     the @p is already pinned by the fig check in step 2.)
  =/  ind  +<:(com:nu:cric:crypto pass.net.pt)
  ?.  ?=([%c *] ind)  ~
  ?.  =(cry.pub.cek cry.pub.ind)  ~
  `(urb-point-to-jael pt who)
::
::  +writ-shed: run lib/gw-verify's full section-7 custody walk in a khan
::  thread (the confidential / unindexed path), producing its verdict as a
::  vase for the [%writ @ @ ~] case in +on-arvo.
::
++  writ-shed
  |=  [dom=@tas who=ship =pass known=(set ship) rpc=req-to:btcio]
  ^-  shed:khan
  =/  m  (strand:strandio ,vase)
  ;<  res=(unit point:jael)  bind:m  (verify:gwv dom who pass known rpc)
  (pure:m !>(res))
::
::  +fresh-pass: our own current pass, for a %jael-anew refresh
::
++  fresh-pass
  |=  who=ship
  ^-  (unit pass)
  ?~  pt=(~(get by unv-ids.urb-state) who)  ~
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
      ::  (fall sponsor self): an on-chain sponsor if present, else self
      ::  (issue #117), never a null sponsor to Jael.
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
