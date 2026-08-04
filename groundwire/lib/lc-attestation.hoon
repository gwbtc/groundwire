::  lib/lc-attestation.hoon
::
::  Resolve a current %gw-btc custody log through the local %light-client
::  (the REAL gwbtc/node %bitcoin-client agent), then hand the deterministic
::  transaction vector to ++run-checks:lsa.
::
::  The node's %light-client API is subscription-based: each watch answers
::  exactly one fact under the agent's fact mark and then kicks, so every
::  fetch here is a one-shot +watch-one.  There is no lookup by bare txid and
::  no /tx-out; a transaction is addressed by [block-hash txid], and tip
::  liveness is decided by a BIP-158 compact-filter scan of every block from
::  the tip height to the chain tip.
::
::  Fetch molds mirror the node's ++update (see sur/light-client), and the
::  node's transactions/blocks arrive as $bitcoin-common values which we
::  convert into the desk's own $tx:bitcoin before verification.
::
/-  bc=bitcoin, ord, urb, sa=self-attestation, lc=light-client, bcm=bitcoin-common
/+  lsa=self-attestation, strandio, b-fil=compact-block-filters
|%
::
++  verify-lc
  |=  $:  sat=self-attestation:sa
          tracked=(unit anchor:sa)
          known-public=(set ship)
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
    (scan-liveness our u.tip tip-spk tip-height)
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
    (fetch-tx-at our height.i.entries txid.i.entries)
  $(entries t.entries, acc [tx acc])
::
::  +scan-liveness: is the tip output still unspent?
::
::    Read /best-block for the current height, then for each height from the
::    tip block up to the chain tip: pull the compact filter, GCS-test the tip
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
++  scan-liveness
  |=  [our=@p tip=sont:ord tip-spk=hexb:bc tip-height=@ud]
  =/  m  (strand:strandio ,(unit ?))
  ^-  form:m
  ;<  best-cage=cage  bind:m  (watch-best our)
  =/  bres  !<(best-block:update:lc q.best-cage)
  =/  best-height=@ud
    ?-  -.bres
      %new             block-height.bres
      %reorg-rollback  block-height.bres
    ==
  ::  cast the desk's hexb scriptPubKey into a bitcoin-common script-pubkey
  ::  so the node's own filter matcher consumes it unchanged.
  =/  spk=hexb:bcm  [`@ud`wid.tip-spk `@ux`dat.tip-spk]
  =/  h=@ud  tip-height
  |-
  ^-  form:m
  ?:  (gth h best-height)
    ~&  [%gw-btc-lc-scan-clean tip=tip from=tip-height to=best-height]
    (pure:m `%.y)
  ;<  res=?(%error %no-match %spent %unspent)  bind:m
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
++  common-in-to-bc
  |=  ti=transaction-input:bcm
  ^-  inputw:tx:bc
  :*  witness.ti
      txid.ti
      vout.ti
      [4 sequence.ti]
      ?:(=(0 wid.script-sig.ti) ~ `script-sig.ti)
      ~
  ==
::
++  common-out-to-bc
  |=  to=transaction-output:bcm
  ^-  output:tx:bc
  [script-pubkey.to value.to]
::
::  One-shot %light-client subscriptions.  Each returns the single fact cage
::  the node gives before it kicks; the caller +!< s it with the update mold.
::
++  watch-header-height
  |=  [our=@p height=@ud]
  %-  watch-one:strandio
  :*  /lc/header-height/(scot %ud height)
      [our %light-client]
      /block-header/height/(scot %ud height)
  ==
::
++  watch-transaction
  |=  [our=@p haz=@ux tid=@ux]
  %-  watch-one:strandio
  :*  /lc/transaction/(scot %ux haz)/(scot %ux tid)
      [our %light-client]
      /transaction/(scot %ux haz)/(scot %ux tid)
  ==
::
++  watch-best
  |=  our=@p
  %-  watch-one:strandio
  :*  /lc/best
      [our %light-client]
      /best-block
  ==
::
++  watch-filter-height
  |=  [our=@p height=@ud]
  %-  watch-one:strandio
  :*  /lc/filter-height/(scot %ud height)
      [our %light-client]
      /block-filter/height/(scot %ud height)
  ==
::
++  watch-block-height
  |=  [our=@p height=@ud]
  %-  watch-one:strandio
  :*  /lc/block-height/(scot %ud height)
      [our %light-client]
      /block/height/(scot %ud height)
  ==
--
