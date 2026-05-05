::  %urb-watcher
::
::  This agent is the Groundwire equivalent of %azimuth and %eth-watcher.
::  It fetches Bitcoin blocks on a timer and parses them for Jael events.
::  Its helper core at the bottom works in conjunction with lib/urb-core.
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
  :-  ~
  %=  this
     rpc  :*  'https://alpha.groundwire.dev/rpc'
              %basic
              'mainnetrpcuser:fc3d36ce83e15484e75a658b2a9a8a90a66f4cb017ace74c8631fe082b93adbf'
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
  `this(state !<(state-0 vase))
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?+    mark  !!
      %urb-start-indexing
    =/  start-urb  ;;((unit state:urb) !<((unit noun) vase))
    ?~  start-urb
      %-  (slog :_(~ [%leaf "%urb-watcher: indexing from block {<num.block-id:(state:urb default-urb-state)>}"]))
      :_  this(urb-state default-urb-state)
      :~  :*  %pass  /timer
              %arvo  %b
              %wait  now.bowl
          ==
      ==
    %-  (slog :_(~ [%leaf "%urb-watcher: processing groundwire snapshot ({<~(wyt by unv-ids.u.start-urb)>} points)"]))
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
            %noun  !>(urb-state)
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
        :~  [%pass /timer %arvo %b %wait (add ~s30 now.bowl)]
            [%give %fact ~[/urb-state] %noun !>(new-urb-state)]
        ==
      (jael-update filtered-udiffs)
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
  =/  start-height  943.140
  =/  start-hash    0x1.62b3.04e4.d48c.3a53.d80a.96de.0210.d325.c0a9.a464.8b3c
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
          [ship id %spon ?:(has.sponsor.net `who.sponsor.net ~)]
          [ship id %fief fief.net]
      ==
    new-udiffs
  ==
--
