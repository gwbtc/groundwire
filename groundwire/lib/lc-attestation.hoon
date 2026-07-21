::  lib/lc-attestation.hoon
::
::  Resolve a current %gw-btc custody log through the local %light-client,
::  then hand the deterministic transaction vector to ++run-checks:lsa.
::
/-  bc=bitcoin, ord, urb, sa=self-attestation, lc=light-client
/+  lsa=self-attestation, strandio
|%
::
++  verify-lc
  |=  $:  sat=self-attestation:sa
          tracked=(unit anchor:sa)
          sponsors=(map @p point:urb)
      ==
  ^-  shed:khan
  =/  m  (strand:strandio ,vase)
  ^-  form:m
  ;<  our=@p  bind:m  get-our:strandio
  =*  who  who.sat
  ?~  chain.sat
    (pure:m !>([(fail-result:lsa who 'empty-chain') *hexb:bc]))
  ::  dat names the precommit transaction but not its block height.
  ;<  start-cage=cage  bind:m  (watch-start our txid.spawn.sat)
  =/  start=tx:bc  !<(transaction-response:lc q.start-cage)
  ::  Every custody entry supplies a height. Resolve the canonical block hash,
  ::  then request the verified transaction in that block.
  ;<  txl=(list tx:bc)  bind:m  (fetch-entries our chain.sat)
  =/  tip  (derive-tip:lsa spawn.sat start txl)
  ?~  tip
    (pure:m !>([(fail-result:lsa who 'derive-tip') *hexb:bc]))
  =/  last=tx:bc  (rear txl)
  ?.  (lth vout.u.tip (lent os.last))
    (pure:m !>([(fail-result:lsa who 'tip-vout-range') *hexb:bc]))
  =/  tip-out=output:tx:bc  (snag vout.u.tip os.last)
  =/  tip-spk=hexb:bc  script-pubkey.tip-out
  ;<  out-cage=cage  bind:m  (watch-out our u.tip)
  =/  status=tx-out-response:lc  !<(tx-out-response:lc q.out-cage)
  ?-    -.status
      %unknown
    (strand-fail:strandio %attestation-tip-unknown >[who u.tip]< ~)
  ::
      %unspent
    =/  out=output:tx:bc  +.status
    ?.  =(out tip-out)
      (strand-fail:strandio %attestation-tip-output-mismatch >[who u.tip]< ~)
    =/  result=result:sa
      (run-checks:lsa sat start txl `%.y tracked sponsors)
    (pure:m !>([result tip-spk]))
  ::
      %spent
    =/  result=result:sa
      (run-checks:lsa sat start txl `%.n tracked sponsors)
    (pure:m !>([result tip-spk]))
  ==
::
++  watch-start
  |=  [our=@p txid=@ux]
  %-  watch-one:strandio
  :*  /lc/start/(scot %ux txid)
      [our %light-client]
      /transaction/(scot %ux txid)
  ==
::
++  watch-entry
  |=  [our=@p block=@ux txid=@ux]
  %-  watch-one:strandio
  :*  /lc/entry/(scot %ux txid)
      [our %light-client]
      /transaction/(scot %ux block)/(scot %ux txid)
  ==
::
++  watch-height
  |=  [our=@p height=@ud]
  %-  watch-one:strandio
  :*  /lc/height/(scot %ud height)
      [our %light-client]
      /block-hash-by-height/(scot %ud height)
  ==
::
++  watch-out
  |=  [our=@p tip=sont:ord]
  %-  watch-one:strandio
  :*  /lc/out/(scot %ux txid.tip)/(scot %ud vout.tip)
      [our %light-client]
      /tx-out/(scot %ux txid.tip)/(scot %ud vout.tip)
  ==
::
++  fetch-entries
  |=  [our=@p entries=custody-log:sa]
  =/  m  (strand:strandio ,(list tx:bc))
  ^-  form:m
  =|  acc=(list tx:bc)
  |-
  ?~  entries  (pure:m (flop acc))
  ;<  h-cage=cage  bind:m  (watch-height our height.i.entries)
  =/  block=id:block:bc  !<(block-hash-response:lc q.h-cage)
  ?.  =(height.i.entries num.block)
    (strand-fail:strandio %attestation-height-mismatch >[height.i.entries num.block]< ~)
  ;<  t-cage=cage  bind:m
    (watch-entry our hax.block txid.i.entries)
  =/  tx=tx:bc  !<(transaction-response:lc q.t-cage)
  ?.  =(txid.i.entries id.tx)
    (strand-fail:strandio %attestation-txid-mismatch >[txid.i.entries id.tx]< ~)
  $(entries t.entries, acc [tx acc])
--
