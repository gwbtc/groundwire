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
      ==
  ^-  shed:khan
  =/  m  (strand:strandio ,vase)
  ^-  form:m
  ;<  our=@p  bind:m  get-our:strandio
  =*  who  who.sat
  ?~  chain.sat
    (pure:m !>([(fail-result:lsa who 'empty-chain') *hexb:bc]))
  =/  spawn-open  (spawn-of:lsa chain.sat)
  ?~  spawn-open
    (pure:m !>([(fail-result:lsa who 'spawn-opening') *hexb:bc]))
  =/  spawn=sont:ord  spawn.u.spawn-open
  ::  Fetch the spawn transaction by [height txid].  +fetch-tx-at resolves
  ::  the canonical block hash from the height, requests the verified tx in
  ::  that block, and strand-fails on any height/txid disagreement.
  ;<  start=tx:bc  bind:m
    %+  (set-timeout:strandio ,tx:bc)  lc-fetch-timeout
    (fetch-tx-at our start-height.u.spawn-open txid.spawn)
  ::  Every custody entry supplies a height; resolve each entry's tx the
  ::  same way, in custody order.
  ;<  txl=(list tx:bc)  bind:m  (fetch-entries our chain.sat)
  =/  tip  (derive-tip:lsa spawn start txl)
  ?~  tip
    (pure:m !>([(fail-result:lsa who 'derive-tip') *hexb:bc]))
  =/  last=tx:bc  (rear txl)
  ?.  (lth vout.u.tip (lent os.last))
    (pure:m !>([(fail-result:lsa who 'tip-vout-range') *hexb:bc]))
  =/  tip-out=output:tx:bc  (snag vout.u.tip os.last)
  =/  tip-spk=hexb:bc  script-pubkey.tip-out
  ::  The tip transaction sits in the last custody entry's block.
  =/  tip-height=@ud  height:(rear chain.sat)
  ::  TIP LIVENESS: scan every block above the tip for a spend of the exact
  ::  outpoint.  `%.y`/`%.n`/~ = unspent/spent/undeterminable (fails closed).
  ;<  live=(unit ?)  bind:m
    (scan-liveness our u.tip tip-spk tip-height best-height)
  =/  result=result:sa
    (run-checks:lsa sat start txl live tracked known-public)
  (pure:m !>([result tip-spk]))
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
  =/  m  (strand:strandio ,tx:bc)
  ^-  form:m
  ;<  h-cage=cage  bind:m  (watch-header-height our height)
  =/  hres  !<(block-header-by-height:update:lc q.h-cage)
  ?.  =(height block-height.hres)
    %+  strand-fail:strandio  %attestation-height-mismatch
    [>[height block-height.hres]< ~]
  ;<  t-cage=cage  bind:m  (watch-transaction our block-hash.hres tid)
  =/  tres  !<(transaction:update:lc q.t-cage)
  ?~  tres
    %+  strand-fail:strandio  %attestation-tx-not-found
    [>[height tid]< ~]
  ?.  =(height block-height.tres)
    %+  strand-fail:strandio  %attestation-height-mismatch
    [>[height block-height.tres]< ~]
  ?.  =(tid txid.tres)
    %+  strand-fail:strandio  %attestation-txid-mismatch
    [>[tid txid.tres]< ~]
  (pure:m (common-tx-to-bc tid transaction.tres))
::
++  fetch-entries
  |=  [our=@p entries=custody-log:sa]
  =/  m  (strand:strandio ,(list tx:bc))
  ^-  form:m
  =|  acc=(list tx:bc)
  |-
  ?~  entries  (pure:m (flop acc))
  ;<  =tx:bc  bind:m
    %+  (set-timeout:strandio ,tx:bc)  lc-fetch-timeout
    (fetch-tx-at our height.i.entries txid.i.entries)
  $(entries t.entries, acc [tx acc])
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
++  scan-liveness
  |=  $:  our=@p
          tip=sont:ord
          tip-spk=hexb:bc
          tip-height=@ud
          best-height=@ud
      ==
  =/  m  (strand:strandio ,(unit ?))
  ^-  form:m
  ?:  (gth tip-height best-height)
    ~&  [%gw-btc-lc-scan-degenerate tip=tip from=tip-height to=best-height]
    (pure:m ~)
  ::  Back into the NODE's byte order for the GCS matcher.  +match:b-fil is
  ::  the node's own matcher and consumes its targets LSB-first, exactly as
  ::  the node encoded them into the filter; .tip-spk arrives here in the
  ::  desk's big-endian order (see +flip-hexb).  THIS FLIP AND THE ONE IN
  ::  +common-out-to-bc MUST MOVE TOGETHER: drop it and the filter silently
  ::  stops matching, turning a SPENT tip into `unspent` -- a fail-OPEN.
  =/  spk=hexb:bcm  (unflip-hexb tip-spk)
  =/  h=@ud  tip-height
  |-
  ^-  form:m
  ?:  (gth h best-height)
    ~&  [%gw-btc-lc-scan-clean tip=tip from=tip-height to=best-height]
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
      %no-match  $(h +(h))
      %unspent   $(h +(h))
  ==
::
::  +scan-height: liveness signal contributed by a single block height.
::
++  scan-height
  |=  [our=@p h=@ud spk=hexb:bcm tip-txid=@ux tip-vout=@ud]
  =/  m  (strand:strandio ,?(%error %no-match %spent %unspent))
  ^-  form:m
  ;<  f-cage=cage  bind:m  (watch-filter-height our h)
  =/  fres  !<(block-filter-by-height:update:lc q.f-cage)
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
  =/  bres  !<(block-by-height:update:lc q.b-cage)
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
  =/  bres  !<(block-by-height:update:lc q.b-cage)
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
