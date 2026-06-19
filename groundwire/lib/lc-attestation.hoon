::  lib/lc-attestation.hoon
::
::  Confidential-comet keyfile verification via the %light-client agent.
::
::  This is the %light-client counterpart of lib/self-attestation's ++verify:
::  it runs the SAME pure verifier (++run-checks:lsa, the input-0 / link-is-commit
::  model) but fetches every referenced transaction through %light-client's
::  request-response watch paths (strandio +watch-one) instead of the Bitcoin
::  RPC node. The light client serves already-verified data, so the fetched txs
::  are used directly.
::
::    /transaction/[block-hash]/[txid]  -> each chain link + the precommit tx
::    /block/[block-hash]               -> the tip link's block height
::    /block-filter/[block-hash]        -> the tip-unspent scan (BIP-158)
::
::  ++verify-lc returns a vase of [result:sa tip-spk=hexb], where tip-spk is the
::  scriptPubKey of the tip output (needed by the agent to watch the sat for
::  movement; it is NOT recoverable from the chain reveals, which disclose spent
::  outputs' keys, so the verifier extracts it from the fetched tip tx).
::
/-  bc=bitcoin, ord, urb, sa=self-attestation, lc=light-client
/+  lsa=self-attestation, strandio, b158=bip-b158, bcu=bitcoin-utils
|%
::
::  +verify-lc: fetch the keyfile's txs via %light-client, then run the pure
::  checks. `tracked` is the sont the caller already tracks for this ship (if
::  any); `best` is the current chain tip; `recent` is a height->block-hash map
::  the caller maintains from /best-block (used for the tip-unspent scan);
::  `sponsors` is the caller's known points (for verifying a signed %escape's
::  sponsor signature -- a confidential comet whose parent is not yet known
::  has its escape dropped, never confirmed unverified).
++  verify-lc
  |=  $:  sat=self-attestation:sa
          tracked=(unit sont:ord)
          best=id:block:bc
          recent=(map @ud @ux)
          sponsors=(map @p point:urb)
      ==
  ^-  shed:khan
  =/  m  (strand:strandio ,vase)
  ^-  form:m
  ;<  our=@p  bind:m  get-our:strandio
  =*  who  who.sat
  ?~  chain.sat
    (pure:m !>([(fail-result:lsa who 'empty-chain') *hexb:bc]))
  ::  the precommit (funding) tx the spawn commit spends
  ;<  pre-cage=cage  bind:m
    (watch-tx our [block txid]:funding.sat)
  =/  pre=tx:bc  !<(transaction-response:lc q.pre-cage)
  ::  every chain link tx, oldest-first
  ;<  txl=(list tx:bc)  bind:m  (fetch-links our chain.sat)
  ?~  txl
    (pure:m !>([(fail-result:lsa who 'empty-chain') *hexb:bc]))
  ::  the tip output's scriptPubKey, read from the last link's tip output
  =/  last-tx=tx:bc  (rear txl)
  ?.  (lth vout.tip.sat (lent os.last-tx))
    (pure:m !>([(fail-result:lsa who 'tip-vout-range') *hexb:bc]))
  =/  tip-spk=hexb:bc  script-pubkey:(snag vout.tip.sat os.last-tx)
  ::  the tip's block height, to bound the unspent scan
  =/  last-link=link:sa  (rear chain.sat)
  ;<  blk-cage=cage  bind:m
    (watch-one:strandio /lc/block [our %light-client] /block/(scot %ux block.last-link))
  =/  tip-height=@ud  height:!<(block-response:lc q.blk-cage)
  ::  tip-unspent via a BIP-158 filter scan over (tip-height, best]
  ;<  tip-unspent=(unit ?)  bind:m
    (scan-unspent our tip-spk tip-height best recent)
  ::  block heights for links carrying a SIGNED %escape, anchoring the
  ::  sponsor-sig freshness window in run-checks (usually empty -- fetched
  ::  only for the rare escape link).
  ;<  heights=(map @ux @ud)  bind:m
    (fetch-heights our (escape-blocks chain.sat))
  ::  the pure verifier (shared, input-0 model)
  =/  =result:sa
    (run-checks:lsa sat txl pre tip-unspent tracked sponsors heights)
  (pure:m !>([result tip-spk]))
::
::  +watch-tx: request one transaction by block-hash + txid (request-response).
++  watch-tx
  |=  [our=@p block=@ux txid=@ux]
  (watch-one:strandio /lc/tx/(scot %ux txid) [our %light-client] /transaction/(scot %ux block)/(scot %ux txid))
::
::  +fetch-links: fetch every link's tx via %light-client, preserving order.
++  fetch-links
  |=  [our=@p links=(list link:sa)]
  =/  m  (strand:strandio ,(list tx:bc))
  ^-  form:m
  =|  acc=(list tx:bc)
  |-  ^-  form:m
  ?~  links  (pure:m (flop acc))
  ;<  c=cage  bind:m  (watch-tx our [block txid]:i.links)
  $(links t.links, acc [!<(transaction-response:lc q.c) acc])
::
::  +escape-blocks: block hashes of links carrying a SIGNED %escape -- the only
::  sotx whose verification needs its block height (the sponsor-sig window).
++  escape-blocks
  |=  chain=(list link:sa)
  ^-  (list @ux)
  %+  murn  chain
  |=  l=link:sa
  ^-  (unit @ux)
  ?:  %+  lien  (singles:lsa sots.l)
      |=  x=[=ship sig=(unit @) =single:skim-sotx:urb]
      &(?=(%escape -.single.x) ?=(^ sig.single.x))
    `block.l
  ~
::
::  +fetch-heights: resolve each block hash to its height via /block.
++  fetch-heights
  |=  [our=@p blocks=(list @ux)]
  =/  m  (strand:strandio ,(map @ux @ud))
  ^-  form:m
  =|  acc=(map @ux @ud)
  |-  ^-  form:m
  ?~  blocks  (pure:m acc)
  ;<  c=cage  bind:m
    (watch-one:strandio /lc/blkh/(scot %ux i.blocks) [our %light-client] /block/(scot %ux i.blocks))
  =/  resp=block-response:lc  !<(block-response:lc q.c)
  $(blocks t.blocks, acc (~(put by acc) i.blocks height.resp))
::
::  +scan-unspent: is the tip output still unspent? Scan the BIP-158 filter of
::  each block in (tip-height, best] for the tip's scriptPubKey (a spend of the
::  tip appears as its scriptPubKey in the spending block's filter). FAIL-CLOSED:
::  if ANY height in the range is missing from `recent` (so we cannot fetch its
::  filter -- e.g. a stale keyfile older than the /best-block ring), we return ~
::  (undeterminable), which the tip-unspent gate (self-attestation:518) rejects.
::  This prevents reporting a SPENT tip as UNSPENT when the spending block was
::  outside the ring. (XX a /block-hash-by-height endpoint would let the scan be
::  gap-free and accept stale-but-live tips; until then we reject rather than
::  guess.)
++  scan-unspent
  |=  $:  our=@p
          tip-spk=hexb:bc
          tip-height=@ud
          best=id:block:bc
          recent=(map @ud @ux)
      ==
  =/  m  (strand:strandio ,(unit ?))
  ^-  form:m
  =/  h=@ud  +(tip-height)
  |-  ^-  form:m
  ?:  (gth h num.best)
    (pure:m `%.y)                          ::  scanned every block, no spend: UNSPENT
  =/  hax=(unit @ux)  (~(get by recent) h)
  ?~  hax  (pure:m ~)                       ::  gap: cannot fetch -> undeterminable (reject)
  ;<  c=cage  bind:m
    (watch-one:strandio /lc/filter/(scot %ud h) [our %light-client] /block-filter/(scot %ux u.hax))
  =/  resp=block-filter-response:lc  !<(block-filter-response:lc q.c)
  =/  k=byts  (to-key:b158 (trip (to-cord:hxb:bcu `hexb:bc`[32 blockhash.resp])))
  ?:  (match:b158 filter.resp k ~[tip-spk])
    (pure:m `%.n)                          ::  scriptPubKey in filter: SPENT
  $(h +(h))
::
::  +check-filter: given a new block's hash and the tracked tips, return the
::  set of ships whose tracked tip output's scriptPubKey appears in that block's
::  BIP-158 filter -- i.e. whose ownership sat just moved on-chain.
++  check-filter
  |=  [hax=@ux tracked=(map @p [tip=sont:ord spk=hexb:bc])]
  ^-  shed:khan
  =/  m  (strand:strandio ,vase)
  ^-  form:m
  ;<  our=@p  bind:m  get-our:strandio
  ;<  c=cage  bind:m
    (watch-one:strandio /lc/filter [our %light-client] /block-filter/(scot %ux hax))
  =/  resp=block-filter-response:lc  !<(block-filter-response:lc q.c)
  =/  k=byts  (to-key:b158 (trip (to-cord:hxb:bcu `hexb:bc`[32 blockhash.resp])))
  =/  hits=(set @p)
    %-  silt
    %+  murn  ~(tap by tracked)
    |=  [who=@p tip=sont:ord spk=hexb:bc]
    ^-  (unit @p)
    ?:((match:b158 filter.resp k ~[spk]) `who ~)
  (pure:m !>(hits))
--
