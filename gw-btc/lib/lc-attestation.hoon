::  lib/lc-attestation.hoon
::
::  THE light-client access layer.  Every byte %gw-btc reads off the chain
::  comes through here, for BOTH of its jobs:
::
::    - the CONFIDENTIAL verifier: resolve a current %gw-btc custody log
::      through the local light client (the REAL gwbtc/node agent,
::      +light-client-agent below), then hand the deterministic transaction
::      vector to ++run-checks:lsa.
::
::    - the PUBLIC block scanner: fetch whole blocks by height and hand
::      them to lib/urb-core's OP_RETURN scanner as $block:bitcoin (see
::      +fetch-block-at).  There is NO Bitcoin Core RPC anywhere in %gw-btc
::      and no -txindex requirement: a light-client-only deployment indexes
::      public comets and verifies confidential ones with the same node.
::
::  Both jobs share one set of node<->desk conversions (+flip-hexb and
::  friends).  Do not add a second copy: the two byte orders type-check
::  into each other silently and a missing flip fails only on mainnet.
::
::  The node's light-client API is subscription-based: each REQUEST watch
::  answers exactly one fact under the agent's fact mark and then kicks, so
::  every fetch here is a one-shot +watch-one.  There is no lookup by bare
::  txid and no /tx-out; a transaction is addressed by [block-hash txid], and
::  tip liveness is decided by a BIP-158 compact-filter scan of every block
::  from the tip height to the chain tip.
::
::  /best-block is NOT such an endpoint: it is a PERSISTENT subscription that
::  gives a fact per block and never kicks, so it must never be read with
::  +watch-one (that deadlocks the strand forever, and with it every
::  confidential verification).  The chain tip height is therefore supplied
::  by the caller -- %gw-btc holds a live /best-block subscription of its own
::  and passes its current height in.
::
::  Fetch molds mirror the node's ++update (see sur/light-client), and the
::  node's transactions/blocks arrive as $bitcoin-common values which we
::  convert into the desk's own $tx:bitcoin before verification.
::
/-  bc=bitcoin, ord, urb, sa=self-attestation, lc=light-client, bcm=bitcoin-common
/+  lsa=self-attestation, strandio, b-fil=compact-block-filters
|%
::  +light-client-agent: the gall agent name of the local light client
::
::    gwbtc/node's desk.bill installs it as %bitcoin-client.  Every watch
::    below and %gw-btc's own /best-block subscription must name the SAME
::    agent: a wrong name is not an error anywhere, it just nacks the watch,
::    and %gw-btc then silently drops every %jael-writ forever.  One
::    constant, one place to change it.
::
++  light-client-agent  %bitcoin-client
::
::  +lc-update: a light-client fact, unpacked.  Every fact is
::  %bitcoin-client-update (node develop 29a84c8 on); the caller asserts the
::  case it asked for.  Anything else -- an older %bitcoin-client, for the
::  minutes an upgrade takes -- fails the strand here instead of being
::  misread.
::
++  lc-update
  |=  =cage
  ^-  bitcoin-client-update:lc
  ?>  ?=(%bitcoin-client-update p.cage)
  !<(bitcoin-client-update:lc q.cage)
::
::  +lc-fetch-timeout: how long ONE light-client request may take
::
::    The node's request endpoints answer when they have the data and
::    otherwise register a pending request and wait for a peer -- they never
::    nack and never answer with a placeholder.  So a +watch-one against a
::    light client whose peers have died waits FOREVER, and with it the whole
::    verification strand, and with that the ship's single-flight `inflight`
::    slot: until +stuck-job-guard (~h2) fires, every further attestation from
::    that peer is dropped.  Live mainnet 2026-08-06 (Phase 6.2): a runtime
::    fault during verification stranded the slot and two retries were
::    swallowed in silence.
::
::    Bounding each individual REQUEST -- not the whole verification, which is
::    legitimately O(blocks since the comet last moved its sat) -- turns that
::    into a strand failure inside a few minutes.  %gw-btc's %verify handler
::    deletes the inflight entry BEFORE it dispatches on the sign, so a failed
::    strand releases the slot and emits no verdict: infrastructure failure,
::    never evidence.  This is the confidential verifier's exact analogue of
::    +block-fetch-timeout in the public scanner, and the same reasoning
::    applies (see app/gw-btc.hoon).
::
++  lc-fetch-timeout  ~m5
::
++  verify-lc
  |=  $:  sat=self-attestation:sa
          tracked=(unit anchor:sa)
          known-public=(set ship)
          best-height=@ud
          from=(unit @ud)
      ==
  ^-  shed:khan
  =/  m  (strand:strandio ,vase)
  ^-  form:m
  ;<  our=@p  bind:m  get-our:strandio
  ;<  pre=verify-pre  bind:m  (fetch-pre our sat)
  ?:  ?=(%done -.pre)
    (pure:m !>([result.pre tip-spk.pre]))
  ::  TIP LIVENESS: scan every block above the tip for a spend of the exact
  ::  outpoint.  `%.y`/`%.n`/~ = unspent/spent/undeterminable (fails closed).
  ;<  live=(unit ?)  bind:m
    %:  scan-liveness
        our  tip.pre  tip-spk.pre  tip-height.pre  best-height
        from  who.sat  last.pre  %.n
    ==
  (pure:m !>([(post-checks sat pre live tracked known-public) tip-spk.pre]))
::
::  $verify-pre: everything +verify-lc learns before the liveness walk
::
::    %done is an early return, already a result.  %walk is a log whose
::    transactions are all fetched and whose tip is derived: what is left
::    is the walk and ++run-checks.  Split out so +verify-lc-batch can
::    fetch every peer's log first and then walk the chain once for all.
::
+$  verify-pre
  $%  [%done =result:sa tip-spk=hexb:bc]
      $:  %walk
          start=tx:bc
          txl=(list tx:bc)
          tip=sont:ord
          tip-spk=hexb:bc
          tip-height=@ud
          last=[height=@ud =txid:ord]
          tip-hash=(unit hax:block:bc)
      ==
  ==
::
::  +fetch-pre: fetch .sat's custody log and derive its tip
::
++  fetch-pre
  |=  [our=@p sat=self-attestation:sa]
  =/  m  (strand:strandio ,verify-pre)
  ^-  form:m
  ;<  got=(each verify-pre [term tang])  bind:m  (try-pre our sat)
  ?:  ?=(%| -.got)  (strand-fail:strandio p.got)
  (pure:m p.got)
::
::  +try-pre: +fetch-pre, but a refused fetch is returned, not thrown
::
::    The batch must not let ONE peer's bad log kill every other peer's
::    verification, so the fetch refusals +fetch-tx-at strand-fails on
::    (wrong height, unknown tx, wrong txid) come back as %| with the same
::    mote and tang.  A fetch that TIMES OUT still fails the strand: that
::    is the light client going quiet, which is everyone's problem.
::
++  try-pre
  |=  [our=@p sat=self-attestation:sa]
  =/  m  (strand:strandio ,(each verify-pre [term tang]))
  ^-  form:m
  =*  who  who.sat
  ::  EVERY early return below is a $abort, and every $abort has a class
  ::  in +abort-class:lsa.  They used to be bare cords in no class at all,
  ::  which +classify read as fraud, so each of these four returns snubbed
  ::  the peer -- including the two that can only fire when THIS DESK is
  ::  wrong.  Do not add an early return with a bare cord: +fail-result
  ::  will not take one.
  ::
  =/  done
    |=  =abort:sa
    (pure:m %& %done (fail-result:lsa who abort) *hexb:bc)
  ?~  chain.sat  (done %empty-chain)
  =/  spawn-open  (spawn-of:lsa chain.sat)
  ?~  spawn-open  (done %spawn-opening)
  =/  spawn=sont:ord  spawn.u.spawn-open
  ::  Fetch the spawn transaction by [height txid].  +fetch-tx-at resolves
  ::  the canonical block hash from the height, requests the verified tx in
  ::  that block, and strand-fails on any height/txid disagreement.  It
  ::  returns that block hash alongside the transaction: it is the one
  ::  place that knows it, and the LAST one is the provenance stamped onto
  ::  the point (.seen, sur/urb).
  ;<  start-at=(each confirmed [term tang])  bind:m
    %+  (set-timeout:strandio ,(each confirmed [term tang]))  lc-fetch-timeout
    (try-tx-at our start-height.u.spawn-open txid.spawn)
  ?:  ?=(%| -.start-at)  (pure:m %| p.start-at)
  =/  start=tx:bc  tx.p.start-at
  ::  Every custody entry supplies a height; resolve each entry's tx the
  ::  same way, in custody order.
  ;<  got=(each (list confirmed) [term tang])  bind:m
    (try-entries our chain.sat)
  ?:  ?=(%| -.got)  (pure:m %| p.got)
  =/  fetched=(list confirmed)  p.got
  =/  txl=(list tx:bc)  (turn fetched |=(c=confirmed tx.c))
  ::  Every transaction below is confirmed on the main chain at the height
  ::  and txid the log claimed -- +fetch-tx-at strand-fails rather than
  ::  return otherwise -- so a failure here is the peer's claim about that
  ::  evidence, not a failure to obtain it.  Hence %fraud; see
  ::  +abort-class:lsa for the full argument.
  ::
  =/  tip  (derive-tip:lsa spawn start txl)
  ?~  tip  (done %derive-tip)
  ::  Unreachable: +derive-tip took vout from +index-to-sont over this very
  ::  output list.  Kept as a bound because +snag below would crash without
  ::  it, and classed %unknown because reaching it means our own arithmetic
  ::  disagreed with itself -- which says nothing about the peer.
  ::
  =/  last=tx:bc  (rear txl)
  ?.  (lth vout.u.tip (lent os.last))  (done %tip-vout-range)
  =/  tip-out=output:tx:bc  (snag vout.u.tip os.last)
  ::  The tip transaction sits in the last custody entry's block.
  =/  last-entry  (rear chain.sat)
  ::  .fetched is non-empty: chain.sat was checked non-empty above and
  ::  +try-entries returns one entry per custody entry.
  ::
  %-  pure:m
  :*  %&  %walk  start  txl  u.tip  script-pubkey.tip-out
      height.last-entry
      [height.last-entry txid.last-entry]
      ?~(fetched ~ `hax:(rear fetched))
  ==
::
::  +post-checks: ++run-checks over a fetched log and its walk's answer
::
++  post-checks
  |=  $:  sat=self-attestation:sa
          pre=verify-pre
          live=(unit ?)
          tracked=(unit anchor:sa)
          known-public=(set ship)
      ==
  ^-  result:sa
  ?>  ?=(%walk -.pre)
  (run-checks:lsa sat start.pre txl.pre live tracked known-public tip-hash.pre)
::
::  +verify-lc-batch: +verify-lc for many peers, walking the chain ONCE
::
::    Each peer's log is fetched in turn, exactly as +verify-lc fetches
::    it; then ONE +scan-liveness-many walk answers every peer's liveness
::    question; then each peer's ++run-checks.  A peer's verdict is the
::    one +verify-lc would give it alone (tests/lib/lc-attestation checks
::    that).  The answer is a list of [who result tip-spk], one per peer
::    whose log could be fetched: a peer whose fetch was refused gets no
::    verdict, exactly as its lone strand would have died.
::
+$  batch-item
  $:  sat=self-attestation:sa
      tracked=(unit anchor:sa)
      from=(unit @ud)
  ==
::
++  verify-lc-batch
  |=  $:  items=(list batch-item)
          known-public=(set ship)
          best-height=@ud
      ==
  ^-  shed:khan
  =/  m  (strand:strandio ,vase)
  ^-  form:m
  ;<  our=@p  bind:m  get-our:strandio
  =|  pres=(list [item=batch-item pre=verify-pre])
  =/  its  items
  |-  ^-  form:m
  ?^  its
    ;<  got=(each verify-pre [term tang])  bind:m  (try-pre our sat.i.its)
    ?:  ?=(%| -.got)
      %-  (slog leaf+"%gw-btc: batch: {(scow %p who.sat.i.its)} gets no verdict, fetching its log failed ({<-.p.got>})" +.p.got)
      $(its t.its)
    $(its t.its, pres [[i.its p.got] pres])
  =.  pres  (flop pres)
  =/  walks=(list [item=batch-item pre=verify-pre])
    (skim pres |=([* pre=verify-pre] ?=(%walk -.pre)))
  ;<  lives=(list (unit ?))  bind:m
    %^  scan-liveness-many  our
      %+  turn  walks
      |=  [item=batch-item pre=verify-pre]
      ^-  walk-target
      ?>  ?=(%walk -.pre)
      [tip.pre tip-spk.pre tip-height.pre from.item who.sat.item last.pre]
    [best-height %.n]
  =/  live-of=(map ship (unit ?))
    =|  acc=(map ship (unit ?))
    |-  ^+  acc
    ?~  walks  acc
    ?>  ?=(^ lives)
    $(walks t.walks, lives t.lives, acc (~(put by acc) who.sat.item.i.walks i.lives))
  %-  pure:m
  !>  ^-  (list [ship result:sa hexb:bc])
  %+  turn  pres
  |=  [item=batch-item pre=verify-pre]
  ?:  ?=(%done -.pre)  [who.sat.item result.pre tip-spk.pre]
  :+  who.sat.item
    %:  post-checks
        sat.item  pre  (~(got by live-of) who.sat.item)
        tracked.item  known-public
    ==
  tip-spk.pre
::
::  +confirmed: a fetched transaction and the block hash it was found in
::
::    The hash is not evidence and nothing is checked against it -- it is
::    already implied by the height, which +fetch-tx-at verifies against
::    both the header fact and the transaction fact.  It is carried out
::    because it is the PROVENANCE the verified point records (.seen), and
::    this is the only layer that ever sees it.
::
+$  confirmed  [hax=hax:block:bc =tx:bc]
::
::  +fetch-tx-at: resolve one transaction by [height txid].
::
::    Watch /block-header/height/<height> for the canonical block hash at
::    that height (height-form always answers), verify the returned block-info
::    height matches, then watch /transaction/<hash>/<txid> and verify the
::    returned block-info height and txid.  A ~ transaction fact (unknown or
::    not on the main chain) is a strand failure.
::
++  fetch-tx-at
  |=  [our=@p height=@ud tid=@ux]
  =/  m  (strand:strandio ,confirmed)
  ^-  form:m
  ;<  got=(each confirmed [term tang])  bind:m  (try-tx-at our height tid)
  ?:  ?=(%| -.got)  (strand-fail:strandio p.got)
  (pure:m p.got)
::
::  +try-tx-at: +fetch-tx-at, with a refusal returned rather than thrown
::
++  try-tx-at
  |=  [our=@p height=@ud tid=@ux]
  =/  m  (strand:strandio ,(each confirmed [term tang]))
  ^-  form:m
  ;<  h-cage=cage  bind:m  (watch-header-height our height)
  =/  hres  =/(u (lc-update h-cage) ?>(?=(%block-header-by-height -.u) p.u))
  ?.  =(height block-height.hres)
    (pure:m %| %attestation-height-mismatch >[height block-height.hres]< ~)
  ;<  t-cage=cage  bind:m  (watch-transaction our block-hash.hres tid)
  =/  tres  =/(u (lc-update t-cage) ?>(?=(%transaction -.u) p.u))
  ?~  tres
    (pure:m %| %attestation-tx-not-found >[height tid]< ~)
  ?.  =(height block-height.tres)
    (pure:m %| %attestation-height-mismatch >[height block-height.tres]< ~)
  ?.  =(tid txid.tres)
    (pure:m %| %attestation-txid-mismatch >[tid txid.tres]< ~)
  ::  block-hash.hres and block-hash.tres are the same block: the header
  ::  fact fixed the canonical hash at .height and the transaction was
  ::  requested IN that block, with its own block-info height re-checked
  ::  above.  Carry the one we addressed the request with.
  ::
  (pure:m %& block-hash.hres (common-tx-to-bc tid transaction.tres))
::
++  try-entries
  |=  [our=@p entries=custody-log:sa]
  =/  m  (strand:strandio ,(each (list confirmed) [term tang]))
  ^-  form:m
  =|  acc=(list confirmed)
  |-
  ?~  entries  (pure:m %& (flop acc))
  ;<  con=(each confirmed [term tang])  bind:m
    %+  (set-timeout:strandio ,(each confirmed [term tang]))  lc-fetch-timeout
    (try-tx-at our height.i.entries txid.i.entries)
  ?:  ?=(%| -.con)  (pure:m %| p.con)
  $(entries t.entries, acc [p.con acc])
::
::  +scan-liveness: is the tip output still unspent?
::
::    .best-height is the chain tip the caller currently believes in (see
::    +light-client-agent's note on /best-block).  For each height from the
::    tip block up to that tip: pull the compact filter, GCS-test the tip
::    scriptPubKey, and on a match pull the block and look for an input
::    spending the exact outpoint.  The scan STARTS AT tip-height itself, not
::    tip-height+1: the tip output may be created and spent inside the same
::    block.  The tip block's filter always matches (it contains the tip tx's
::    own output spk); that ambiguity is resolved by fetching the block and
::    checking for any input on the exact outpoint -- the tip tx cannot spend
::    its own output, so only a later same-block tx can produce a match.
::    A confirmed spend => `%.n; scanning to the tip with none => `%.y; any
::    failed/unavailable fetch or inconsistency => ~ (undeterminable).
::
::    A DEGENERATE RANGE IS UNDETERMINABLE, NOT CLEAN.  If .best-height is
::    below .tip-height there is no block to look at: the loop's exit test
::    (`h > best-height`) is true on the FIRST iteration and the old code
::    returned `%.y -- "unspent" -- having examined zero blocks.  That is a
::    fail-OPEN on the one check the design most wants to fail closed, and it
::    is exactly what happened on mainnet 2026-08-06: an unsynced light client
::    reported the chain tip as genesis, and
::    `[%gw-btc-lc-scan-clean ... from=961.196 to=0]` passed `tip-unspent`
::    on no evidence at all.  A spent tip would have read identically.
::
::    RESUMABLE.  .from is the height through which an earlier walk for
::    the SAME custody entry already found the output unspent (the agent's
::    .clean checkpoint); "unspent through H" is monotone, so the walk
::    starts above it.  Every +progress-every blocks, and on completion,
::    the walk reports how far it got with a %gw-liveness-progress poke to
::    the agent, so a walk that later times out still leaves durable
::    progress behind and the next attempt continues rather than restarts.
::    .who/.last identify the peer and the entry the checkpoint is for.
::
++  progress-every  256
++  scan-liveness
  |=  $:  our=@p
          tip=sont:ord
          tip-spk=hexb:bc
          tip-height=@ud
          best-height=@ud
          from=(unit @ud)
          who=ship
          last=[height=@ud =txid:ord]
          dry=?
      ==
  =/  m  (strand:strandio ,(unit ?))
  =/  n  (strand:strandio ,~)
  ^-  form:m
  ?:  (gth tip-height best-height)
    ~&  [%gw-btc-lc-scan-degenerate tip=tip from=tip-height to=best-height]
    (pure:m ~)
  ::  .dry: report no progress (ted/bench/liveness walks made-up outpoints)
  =/  progress
    |=  to=@ud
    ^-  form:n
    ?:  dry  (pure:n ~)
    %+  poke-our:strandio  %gw-btc
    noun+!>([%gw-liveness-progress who last to])
  ::  Back into the NODE's byte order for the GCS matcher.  +match:b-fil is
  ::  the node's own matcher and consumes its targets LSB-first, exactly as
  ::  the node encoded them into the filter; .tip-spk arrives here in the
  ::  desk's big-endian order (see +flip-hexb).  THIS FLIP AND THE ONE IN
  ::  +common-out-to-bc MUST MOVE TOGETHER: drop it and the filter silently
  ::  stops matching, turning a SPENT tip into `unspent` -- a fail-OPEN.
  =/  spk=hexb:bcm  (unflip-hexb tip-spk)
  =/  h=@ud
    ?~  from  tip-height
    (max tip-height +(u.from))
  ~&  [%gw-btc-lc-scan-start tip=tip from=h to=best-height resumed=?=(^ from)]
  |-
  ^-  form:m
  ?:  (gth h best-height)
    ~&  [%gw-btc-lc-scan-clean tip=tip from=tip-height to=best-height]
    ::  A one-block walk (a fresh spawn, or a %claim, which examines only
    ::  the publication's own block) leaves nothing worth remembering:
    ::  the next walk starts at the custody entry anyway.
    ;<  ~  bind:m
      ?.  (gth best-height tip-height)  (pure:n ~)
      (progress best-height)
    (pure:m `%.y)
  ;<  res=?(%error %no-match %spent %unspent)  bind:m
    %+  (set-timeout:strandio ,?(%error %no-match %spent %unspent))
      lc-fetch-timeout
    (scan-height our h spk txid.tip vout.tip)
  ?-  res
      %error
    ~&  [%gw-btc-lc-scan-undeterminable height=h]
    (pure:m ~)
  ::
      %spent
    ~&  [%gw-btc-lc-scan-spent tip=tip height=h]
    (pure:m `%.n)
  ::
      ?(%no-match %unspent)
    ::  block h is clean; every +progress-every blocks, say so durably.
    ;<  ~  bind:m
      ?.  =(0 (mod h progress-every))  (pure:n ~)
      (progress h)
    $(h +(h))
  ==
::
::  +scan-liveness-many: +scan-liveness for many outpoints, in ONE pass
::
::    The walk is the cost of verifying a peer: one compact filter per
::    block from its last custody entry to the chain tip.  Peers whose
::    entries are near each other walk almost the same blocks, so this
::    walks the union once, upward from the lowest start.  At each height
::    the ACTIVE targets are the undecided ones whose own walk has begun;
::    the filter is fetched once and tested for all of them
::    (+match-many), and on a hit the block is fetched once and searched
::    for each hit target's outpoint.
::
::    Every target gets exactly the answer its own +scan-liveness would:
::    the same start (resume point included), the same degenerate-range
::    refusal, the same %.n at the first spending block, ~ where its walk
::    would have hit an unusable filter or block, the same progress pokes
::    (every +progress-every blocks and at the end).  A height its own
::    walk would not visit is never fetched on its behalf, and a fault at
::    a height decides only the targets active there.  A request that
::    times out still fails the strand, as it does for one target.
::
+$  walk-target
  $:  tip=sont:ord
      tip-spk=hexb:bc
      tip-height=@ud
      from=(unit @ud)
      who=ship
      last=[height=@ud =txid:ord]
  ==
::
++  scan-liveness-many
  |=  [our=@p tars=(list walk-target) best-height=@ud dry=?]
  =/  m  (strand:strandio ,(list (unit ?)))
  =/  n  (strand:strandio ,~)
  ^-  form:m
  =/  ids=(list @ud)  (gulf 1 (lent tars))
  =/  tv=(map @ud walk-target)
    =<  q
    %^  spin  tars  *(map @ud walk-target)
    |=([t=walk-target acc=(map @ud walk-target)] [~ (~(put by acc) +(~(wyt by acc)) t)])
  ::  node byte order for the GCS matcher; see the flip note in +scan-liveness
  =/  spk=(map @ud hexb:bcm)
    (~(run by tv) |=(t=walk-target (unflip-hexb tip-spk.t)))
  =/  start=(map @ud @ud)
    %-  ~(run by tv)
    |=  t=walk-target
    ?~(from.t tip-height.t (max tip-height.t +(u.from.t)))
  =|  done=(map @ud (unit ?))
  =.  done
    %-  ~(gas by done)
    %+  murn  ids
    |=  i=@ud
    =/  t  (~(got by tv) i)
    ?.  (gth tip-height.t best-height)  ~
    ~&  [%gw-btc-lc-scan-degenerate tip=tip.t from=tip-height.t to=best-height]
    `[i ~]
  =/  progress
    |=  [is=(list @ud) to=@ud]
    ^-  form:n
    ?:  |(dry ?=(~ is))  (pure:n ~)
    =/  t  (~(got by tv) i.is)
    ;<  ~  bind:n
      %+  poke-our:strandio  %gw-btc
      noun+!>([%gw-liveness-progress who.t last.t to])
    $(is t.is)
  ::  the undecided targets, and the lowest height any of them starts at
  ::  (above .best-height when there is none)
  =/  open
    |=  d=(map @ud (unit ?))
    ^-  (list @ud)
    (skip ids |=(i=@ud (~(has by d) i)))
  =/  next-start
    |=  d=(map @ud (unit ?))
    ^-  @ud
    %+  roll  (open d)
    |=([i=@ud lo=_+(best-height)] (min lo (~(got by start) i)))
  =/  h=@ud  (next-start done)
  ~&  [%gw-btc-lc-scan-many targets=(lent tars) from=h to=best-height]
  |-  ^-  form:m
  =/  live=(list @ud)
    (skim (open done) |=(i=@ud (lte (~(got by start) i) h)))
  ?:  |((gth h best-height) ?=(~ (open done)))
    ::  Every target still open walked clean to the tip.  Same end report
    ::  as +scan-liveness, per target.
    =/  rest  (open done)
    ;<  ~  bind:m
      %+  progress
        (skim rest |=(i=@ud (gth best-height tip-height:(~(got by tv) i))))
      best-height
    =.  done  (~(gas by done) (turn rest |=(i=@ud [i `%.y])))
    ~&  [%gw-btc-lc-scan-many-clean clean=(lent rest) to=best-height]
    (pure:m (turn ids |=(i=@ud (~(got by done) i))))
  ?~  live
    ::  a gap below the next target's start: nothing to look at here
    $(h (next-start done))
  ;<  res=(map @ud ?(%error %spent %clean))  bind:m
    %+  (set-timeout:strandio ,(map @ud ?(%error %spent %clean)))
      lc-fetch-timeout
    (scan-height-many our h live spk tv)
  =/  fin=(list [@ud (unit ?)])
    %+  murn  ~(tap by res)
    |=  [i=@ud r=?(%error %spent %clean)]
    ?-  r
      %clean  ~
      %error  ~&([%gw-btc-lc-scan-undeterminable height=h] `[i ~])
      %spent  ~&([%gw-btc-lc-scan-spent tip=tip:(~(got by tv) i) height=h] `[i `%.n])
    ==
  =.  done  (~(gas by done) fin)
  ;<  ~  bind:m
    ?.  =(0 (mod h progress-every))  (pure:n ~)
    (progress (skim `(list @ud)`live |=(i=@ud =(%clean (~(got by res) i)))) h)
  $(h +(h))
::
::  +scan-height-many: +scan-height for every target in .live, at .h
::
::    One filter, one block at most.  An unusable filter decides every
::    live target %error (each one's own +scan-height would have read the
::    same filter); an unusable block decides only the targets that
::    matched, the only ones whose walk would have fetched it.
::
++  scan-height-many
  |=  $:  our=@p
          h=@ud
          live=(list @ud)
          spk=(map @ud hexb:bcm)
          tv=(map @ud walk-target)
      ==
  =/  m  (strand:strandio ,(map @ud ?(%error %spent %clean)))
  ^-  form:m
  =/  all
    |=  r=?(%error %spent %clean)
    (malt (turn live |=(i=@ud [i r])))
  ;<  f-cage=cage  bind:m  (watch-filter-height our h)
  =/  fres  =/(u (lc-update f-cage) ?>(?=(%block-filter-by-height -.u) p.u))
  ?.  =(h block-height.fres)  (pure:m (all %error))
  ::  zero-width: see +scan-height
  ?:  =(0 wid.filter.fres)  (pure:m (all %error))
  =/  hit=(set @ud)
    %:  match-many
        block-hash.fres  filter.fres
        (turn live |=(i=@ud [i (~(got by spk) i)]))
    ==
  ?:  =(~ hit)  (pure:m (all %clean))
  ;<  b-cage=cage  bind:m  (watch-block-height our h)
  =/  bres  =/(u (lc-update b-cage) ?>(?=(%block-by-height -.u) p.u))
  %-  pure:m
  %-  malt
  %+  turn  live
  |=  i=@ud
  :-  i
  ?.  (~(has in hit) i)  %clean
  ?.  =(h block-height.bres)  %error
  =/  tip  tip:(~(got by tv) i)
  ?:((block-spends +.bres txid.tip vout.tip) %spent %clean)
::
::  +match-many: which of .targets the filter matches, each on its own
::
::    +match:b-fil answers "any of these?", and +all-match:b-fil keys its
::    targets by GCS value, so two targets that hash alike come back as
::    one -- a missed spend.  This walks the filter once for all of them
::    and answers per target, with exactly +match's arithmetic: target i
::    is in the result iff `(match:b-fil haz filter ~[spk-i])`.
::
++  match-many
  |=  [haz=@ux filter=hexb:bcm targets=(list [i=@ud spk=hexb:bcm])]
  ^-  (set @ud)
  =/  k  (to-key:b-fil haz)
  =/  p  p:params:b-fil
  =/  [n=@ux gcs=bits:b-fil]  (parse-filter:b-fil filter)
  =/  f  (mul n m:params:b-fil)
  =/  want=(list [v=@ i=@ud])
    %+  sort
      (turn targets |=([i=@ud spk=hexb:bcm] [(to-range:hsh:b-fil spk f k) i]))
    |=([a=[v=@ *] b=[v=@ *]] (lth v.a v.b))
  =|  acc=(set @ud)
  =/  last-val=@  0
  |-  ^-  (set @ud)
  ?~  want  acc
  ?:  =(last-val v.i.want)
    $(want t.want, acc (~(put in acc) i.i.want))
  ?:  (gth last-val v.i.want)
    $(want t.want)
  ?:  (lth wid.gcs p)  acc
  =^  delta  gcs  (de:gol:b-fil gcs p)
  $(last-val (add delta last-val))
::
::  +scan-height: liveness signal contributed by a single block height.
::
++  scan-height
  |=  [our=@p h=@ud spk=hexb:bcm tip-txid=@ux tip-vout=@ud]
  =/  m  (strand:strandio ,?(%error %no-match %spent %unspent))
  ^-  form:m
  ;<  f-cage=cage  bind:m  (watch-filter-height our h)
  =/  fres  =/(u (lc-update f-cage) ?>(?=(%block-filter-by-height -.u) p.u))
  ?.  =(h block-height.fres)
    (pure:m %error)
  ::  A zero-width filter cannot match anything, so accepting one would read
  ::  as "no spend here" for every height -- a silent fail-open.  Real BIP-158
  ::  filters are never empty (every block has a coinbase output), so this is
  ::  a protocol violation and must be undeterminable, not clean.
  ?:  =(0 wid.filter.fres)
    (pure:m %error)
  ?.  (match:b-fil block-hash.fres filter.fres ~[spk])
    (pure:m %no-match)
  ;<  b-cage=cage  bind:m  (watch-block-height our h)
  =/  bres  =/(u (lc-update b-cage) ?>(?=(%block-by-height -.u) p.u))
  ?.  =(h block-height.bres)
    (pure:m %error)
  ?:  (block-spends +.bres tip-txid tip-vout)
    (pure:m %spent)
  (pure:m %unspent)
::
::  +block-spends: does any tx in this block spend the exact outpoint?
::
++  block-spends
  |=  [blk=block:bcm tip-txid=@ux tip-vout=@ud]
  ^-  ?
  %+  lien  txs.blk
  |=  =transaction:bcm
  %+  lien  inputs.transaction
  |=  in=transaction-input:bcm
  &(=(txid.in tip-txid) =(vout.in tip-vout))
::
::  Conversions from the node's $bitcoin-common shapes to the desk's own
::  $tx:bitcoin.  Verification only reads: id, input-0's [txid vout witness],
::  and each output's [script-pubkey value] -- all preserved exactly.
::
++  common-tx-to-bc
  |=  [id=@ux ct=transaction:bcm]
  ^-  tx:bc
  :*  id
      (turn inputs.ct common-in-to-bc)
      (turn outputs.ct common-out-to-bc)
      locktime.ct
      `@ud`version.ct
      ?:(=(0 flag.ct) ~ `flag.ct)
  ==
::
::  +flip-hexb / +unflip-hexb: the node's byte order <-> the desk's
::
::    $hexb:bitcoin-common and $hexb:bitcoin are structurally identical
::    ([wid dat]) so they cast into each other silently, but they are NOT
::    the same value: the node stores a byte string with the FIRST WIRE BYTE
::    IN THE LOW BYTE (its serializers and its BIP-158 siphash both consume
::    `dat` LSB-first), while the desk's $hexb -- and +p2tr-xonly,
::    +state-key and the BIP-340 code that read it -- put the first wire
::    byte in the HIGH byte.  Real mainnet example: a P2TR scriptPubKey
::    0x5120.be3a...8341 arrives from the node as 0x4183...6a20.51.
::
::    So EVERY byte string crossing this boundary must be reversed.  Passing
::    them through unconverted makes +p2tr-xonly read the last two bytes as
::    the version prefix, and every entry-N-commitment and tip-p2tr check
::    fails on real chain data.
::
++  flip-hexb
  |=  h=hexb:bcm
  ^-  hexb:bc
  [`@ud`wid.h `@ux`(rev 3 wid.h dat.h)]
::
++  unflip-hexb
  |=  h=hexb:bc
  ^-  hexb:bcm
  [`@ud`wid.h `@ux`(rev 3 wid.h dat.h)]
::
++  common-in-to-bc
  |=  ti=transaction-input:bcm
  ^-  inputw:tx:bc
  :*  (turn witness.ti flip-hexb)
      txid.ti
      vout.ti
      [4 sequence.ti]
      ?:(=(0 wid.script-sig.ti) ~ `(flip-hexb script-sig.ti))
      ~
  ==
::
++  common-out-to-bc
  |=  to=transaction-output:bcm
  ^-  output:tx:bc
  [(flip-hexb script-pubkey.to) value.to]
::
::  ------------------------------------------------------------------
::  PUBLIC BLOCK SCANNER: whole blocks, by height
::  ------------------------------------------------------------------
::
::  +fetch-block-at: block .height as the desk's $block:bitcoin
::
::    One /block/height/<h> watch, one fact, one kick.  The node answers
::    the height form for any height it has, so a disagreeing block-info
::    height is a protocol violation and fails the strand rather than
::    silently indexing the wrong block.
::
++  fetch-block-at
  |=  [our=@p height=@ud]
  =/  m  (strand:strandio ,block:bc)
  ^-  form:m
  ;<  b-cage=cage  bind:m  (watch-block-height our height)
  =/  bres  =/(u (lc-update b-cage) ?>(?=(%block-by-height -.u) p.u))
  ?.  =(height block-height.bres)
    %+  strand-fail:strandio  %scan-block-height-mismatch
    [>[height block-height.bres]< ~]
  (pure:m (common-block-to-bc block-hash.bres height +.bres))
::
::  +common-block-to-bc: a node $block into the desk's $block:bitcoin
::
::    The node's $block is just [header txs] -- no block hash, no height,
::    and (crucially) NO TXIDS: its transactions are positional.  The
::    desk's scanner keys everything on txids, so each one is recomputed
::    here from the transaction itself (+node-txid).  The reward is the
::    height's block subsidy, exactly as the old RPC path derived it.
::
::  +fetch-blocks: every block from .from to .to, fetched CONCURRENTLY
::
::    One /block/height/<h> subscription per height, all opened before
::    any is awaited, so the light client asks a different peer for each
::    and the batch takes as long as its slowest download, not the sum.
::    Measured on a live comet: a 1.7 MB block arrives in 3 s from a fast
::    peer and 18-23 s from a slow one, and with one fetch at a time the
::    index paid the slow ones in series.
::
::    Facts are collected by wire (+take-fact-prefix) into a map keyed by
::    height, because they arrive in whatever order the peers deliver;
::    the caller walks the map in height order.  The light client kicks
::    each subscription after its fact; those kicks are ordinary inputs
::    that +take-fact-prefix skips.  A height it answers with the wrong
::    block-height fails the whole strand, as +fetch-block-at does.
::
++  fetch-blocks
  |=  [our=@p from=@ud to=@ud]
  =/  m  (strand:strandio ,(map @ud block:bc))
  ^-  form:m
  =/  want=@ud  +((sub to from))
  =/  heights=(list @ud)  (gulf from to)
  |-  ^-  form:m
  ?^  heights
    ;<  ~  bind:m
      %^  watch:strandio  /blocks/(scot %ud i.heights)
        [our %bitcoin-client]
      /block/height/(scot %ud i.heights)
    $(heights t.heights)
  =|  acc=(map @ud block:bc)
  |-  ^-  form:m
  ?:  =(want ~(wyt by acc))  (pure:m acc)
  ::  +take-fact-prefix hands back the wire from the prefix's LAST
  ::  segment on (it slags by the prefix's length, not one past it), so
  ::  the path here is /blocks/<h>, not /<h>.
  ::
  ;<  [=path =cage]  bind:m  (take-fact-prefix:strandio /blocks)
  ?.  ?=([%blocks @ ~] path)
    (strand-fail:strandio %scan-block-wire [>path< ~])
  =/  h=@ud  (slav %ud i.t.path)
  =/  bres  =/(u (lc-update cage) ?>(?=(%block-by-height -.u) p.u))
  ?.  =(h block-height.bres)
    %+  strand-fail:strandio  %scan-block-height-mismatch
    [>[h block-height.bres]< ~]
  $(acc (~(put by acc) h (common-block-to-bc block-hash.bres h +.bres)))
++  common-block-to-bc
  |=  [haz=@ux height=@ud blk=block:bcm]
  ^-  block:bc
  :*  haz
      (block-subsidy height)
      height
      %+  turn  txs.blk
      |=  ct=transaction:bcm
      ^-  tx:bc
      (common-tx-to-bc (node-txid ct) ct)
  ==
::
::  +block-subsidy: newly issued sats at .height
::
::    The first sats of the coinbase output range, before the fees that
::    ++handle-block:urb-core accumulates behind them.
::
++  block-subsidy
  |=  height=@ud
  ^-  @ud
  (div 5.000.000.000 (bex (div height 210.000)))
::
::  +node-txid: the txid of a node-shape transaction
::
::    A faithful port of +make-txid:serialization in gwbtc/node: double
::    SHA-256 over the LEGACY (witness-stripped) serialization.  It works
::    entirely in the NODE's byte order -- `dat` accumulates with the
::    first wire byte in the low byte, which is also what +shay consumes
::    -- so no flip appears anywhere in here.  The result is the ordinary
::    display-order txid as a number, i.e. the same convention the node's
::    own /transaction facts and Causeway's custody logs use.
::
::    Getting this wrong is silent: a wrong txid still indexes, it just
::    indexes a satpoint nothing will ever match.  It is pinned against
::    real mainnet data in tests/lib/lc-attestation.
::
++  node-txid
  |=  ct=transaction:bcm
  ^-  @ux
  =/  ser=hexb:bcm  (en-legacy-tx ct)
  `@ux`(shay 32 (shay wid.ser dat.ser))
::
::  +en-node-cat: append .wid bytes of .dat after the bytes already held
::
++  en-node-cat
  |=  [acc=hexb:bcm wid=@ud dat=@]
  ^-  hexb:bcm
  [(add wid.acc wid) `@ux`(can 3 ~[[wid.acc dat.acc] [wid dat]])]
::
::  +en-node-csiz: Bitcoin's compactsize varint
::
++  en-node-csiz
  |=  [acc=hexb:bcm n=@]
  ^-  hexb:bcm
  ?:  (lte n 0xfc)  (en-node-cat acc 1 n)
  =/  len  (met 3 n)
  ?:  (lte len 2)  (en-node-cat (en-node-cat acc 1 0xfd) 2 n)
  ?:  (lte len 4)  (en-node-cat (en-node-cat acc 1 0xfe) 4 n)
  (en-node-cat (en-node-cat acc 1 0xff) 8 n)
::
::  +en-legacy-tx: pre-segwit transaction serialization (no witness)
::
++  en-legacy-tx
  |=  ct=transaction:bcm
  ^-  hexb:bcm
  =/  acc=hexb:bcm  (en-node-cat [0 0x0] 4 version.ct)
  =.  acc  (en-node-csiz acc (lent inputs.ct))
  =.  acc
    =/  ins  inputs.ct
    |-  ^-  hexb:bcm
    ?~  ins  acc
    =/  a  (en-node-cat acc 32 txid.i.ins)
    =.  a  (en-node-cat a 4 vout.i.ins)
    =.  a  (en-node-csiz a wid.script-sig.i.ins)
    =.  a  (en-node-cat a wid.script-sig.i.ins dat.script-sig.i.ins)
    =.  a  (en-node-cat a 4 sequence.i.ins)
    $(ins t.ins, acc a)
  =.  acc  (en-node-csiz acc (lent outputs.ct))
  =.  acc
    =/  ous  outputs.ct
    |-  ^-  hexb:bcm
    ?~  ous  acc
    =/  a  (en-node-cat acc 8 value.i.ous)
    =.  a  (en-node-csiz a wid.script-pubkey.i.ous)
    =.  a  (en-node-cat a wid.script-pubkey.i.ous dat.script-pubkey.i.ous)
    $(ous t.ous, acc a)
  (en-node-cat acc 4 locktime.ct)
::
::  One-shot light-client REQUEST subscriptions.  Each returns the single
::  fact cage the node gives before it kicks; the caller +!< s it with the
::  update mold.  (/best-block is deliberately absent -- it never kicks.)
::
++  watch-header-height
  |=  [our=@p height=@ud]
  %-  watch-one:strandio
  :*  /lc/header-height/(scot %ud height)
      [our light-client-agent]
      /block-header/height/(scot %ud height)
  ==
::
++  watch-transaction
  |=  [our=@p haz=@ux tid=@ux]
  %-  watch-one:strandio
  :*  /lc/transaction/(scot %ux haz)/(scot %ux tid)
      [our light-client-agent]
      /transaction/(scot %ux haz)/(scot %ux tid)
  ==
::
++  watch-filter-height
  |=  [our=@p height=@ud]
  %-  watch-one:strandio
  :*  /lc/filter-height/(scot %ud height)
      [our light-client-agent]
      /block-filter/height/(scot %ud height)
  ==
::
++  watch-block-height
  |=  [our=@p height=@ud]
  %-  watch-one:strandio
  :*  /lc/block-height/(scot %ud height)
      [our light-client-agent]
      /block/height/(scot %ud height)
  ==
--
