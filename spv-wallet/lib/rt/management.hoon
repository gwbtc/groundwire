::  lib/rt/management.hoon
::
::  Shared helpers + full build/sign/broadcast pipeline for non-spawn urb
::  management ops performed from inside a comet's own ship (%spv-wallet
::  hot-wallet mode).
::
::  Ops:
::    %keys (rekey), %escape, %cancel-escape, %fief           — self-authored
::    %adopt, %reject, %detach                                — sponsor-authored
::    %set-mang                                               — manager
::    causeway-import-proof                                   — ingest proof.json
::
::  +run-causeway-op is the top-level pipeline: pick a signing UTXO from
::  the wallet's tracked accounts, build a confidential commit tx whose
::  output 0 is P2TR(internal=funding-xonly, merkle-root=tapleaf(sotx))
::  so the sat stays re-spendable (urb-core's ++is-sont-in-input), sign
::  the key-path input via the wallet's seed, broadcast to mempool.space.
::
::  Mirrors rt/boot.hoon's +do-commit minus the reveal step.
::
/-  s=spv-wallet, tx=transactions, urb, hd-path
/+  io=sailboxio, bip32=bip32-spv, btc=bitcoin,
    wallet-address, wallet-account, *wallet-mempool-space,
    taproot, txns=tx-build,
    bcu=bitcoin-utils, fees=tx-fees, sig=tx-sighash, urb-encoder,
    bscr=btc-script
|%
::
::  Sotx constructors — lift a skim-sotx into a full sotx with the actor's
::  from-ship. tx_sig is ~: ownership is proven by ++is-sont-in-input (the
::  commit spends the point's current sont), not by a sotx-level sig.
::
++  sot-of
  |=  [from=ship op=skim-sotx:urb]
  ^-  sotx:urb
  [[from ~] op]
::
++  rekey-sot         |=([=ship pass=@ breach=?]  ^-(sotx:urb (sot-of ship [%keys pass breach])))
++  escape-sot        |=([=ship parent=^ship sig=(unit @)]  ^-(sotx:urb (sot-of ship [%escape parent sig])))
++  cancel-escape-sot |=([=ship parent=^ship]  ^-(sotx:urb (sot-of ship [%cancel-escape parent])))
++  adopt-sot         |=([sponsor=ship child=^ship]  ^-(sotx:urb (sot-of sponsor [%adopt child])))
++  reject-sot        |=([sponsor=ship child=^ship]  ^-(sotx:urb (sot-of sponsor [%reject child])))
++  detach-sot        |=([sponsor=ship child=^ship]  ^-(sotx:urb (sot-of sponsor [%detach child])))
++  fief-sot          |=([=ship fief=(unit fief:urb)]  ^-(sotx:urb (sot-of ship [%fief fief])))
::
::  +build-mgmt-ptst: single-leaf ptst for a management sotx.
::
++  build-mgmt-ptst
  |=  sot=sotx:urb
  ^-  ptst:taproot
  =/  encoded=@  (encode:urb-encoder ~[sot])
  =/  scr=hexb:btc  (en:bscr (unv-to-script:en:urb-encoder encoded))
  [%leaf [0xc0 scr]]
::
::  +network-for: best-guess Bitcoin network for broadcasts; falls back to
::  %main if the wallet has no accounts.
::
++  network-for
  |=  state=state-0:s
  ^-  network:tx
  =/  accts  ~(tap by accounts.state)
  ?~  accts  %main
  active-network.q.i.accts
::
::  +pick-signing-utxo: find a confirmed UTXO in any signing wallet's
::  accounts that's large enough to fund a 1-in/1-out P2TR commit tx.
::
::  Returns (unit [seed=[%q @q] path=tape txid=@t vout=@ud value=@ud]).
::  If ~, the poke handler leaves the op at %pending.
::
++  pick-signing-utxo
  |=  state=state-0:s
  =|  res=(unit [seed=[%q @q] path=tape txid=@t vout=@ud value=@ud])
  =/  min-value=@ud  700     :: >= 1 commit tx fee at 2 sat/vB + margin
  =/  wallets-list=(list [pub=@ux wal=wallet:s])  ~(tap by wallets.state)
  |-  ^-  (unit [seed=[%q @q] path=tape txid=@t vout=@ud value=@ud])
  ?~  wallets-list  res
  ?.  (~(has in signing.state) fingerprint.wal.i.wallets-list)
    $(wallets-list t.wallets-list)
  =*  w  wal.i.wallets-list
  ?.  ?=([%q *] seed.w)  $(wallets-list t.wallets-list)
  =*  sd  seed.w
  =/  accts=(list [pat=account:hd-path acc-pub=@ux])  ~(tap by accounts.w)
  =/  inner
    |-  ^-  (unit [seed=[%q @q] path=tape txid=@t vout=@ud value=@ud])
    ?~  accts  ~
    =/  det  (~(get by accounts.state) acc-pub.i.accts)
    ?~  det  $(accts t.accts)
    =*  p  pat.i.accts
    =/  coin=@ud  ?:(?=(%.y -.q.p) p.q.p 0)
    =/  aidx=@ud  ?:(?=(%.y -.q.q.p) p.q.q.p 0)
    =/  net  active-network.u.det
    =/  nd  (~(get by networks.u.det) net)
    ?~  nd  $(accts t.accts)
    =/  pick-r  (walk-chain sd coin aidx 0 receiving.u.nd)
    ?^  pick-r  pick-r
    =/  pick-c  (walk-chain sd coin aidx 1 change.u.nd)
    ?^  pick-c  pick-c
    $(accts t.accts)
  ?^  inner  inner
  $(wallets-list t.wallets-list)
::
++  walk-chain
  |=  $:  seed=[%q @q]
          coin=@ud
          aidx=@ud
          chain-n=@ud
          mop=((mop @ud hd-leaf:s) gth)
      ==
  =|  out=(unit [seed=[%q @q] path=tape txid=@t vout=@ud value=@ud])
  =/  entries  (tap:((on @ud hd-leaf:s) gth) mop)
  |-  ^-  (unit [seed=[%q @q] path=tape txid=@t vout=@ud value=@ud])
  ?~  entries  out
  =/  idx=@ud  key.i.entries
  =/  leaf  val.i.entries
  =/  u  (scan-utxos utxos.main.leaf)
  ?^  u
    =/  p=tape
      ;:  weld
        "m/86'/"  (scow %ud coin)  "'/"  (scow %ud aidx)  "'/"
        (scow %ud chain-n)  "/"  (scow %ud idx)
      ==
    `[seed p txid.u.u vout.u.u value.u.u]
  $(entries t.entries)
::
++  scan-utxos
  |=  utxos=(list [txid=@t vout=@ud value=@ud =tx-status:s])
  ^-  (unit [txid=@t vout=@ud value=@ud])
  ?~  utxos  ~
  ?:  ?&  ?=([%confirmed *] tx-status.i.utxos)
          (gte value.i.utxos 700)
      ==
    `[txid vout value]:i.utxos
  $(utxos t.utxos)
::
::  +run-causeway-op: TOP-LEVEL pipeline. Returns (unit [txid note]).
::    ~            — no signing UTXO; poke handler leaves op at %pending.
::    [~ [txid *]] — commit broadcast; handler advances queue entry.
::
::  Mirrors rt/boot.hoon's +do-commit (spawn) minus the reveal step:
::    1. pick-signing-utxo                  → [seed path utxo] (or ~)
::    2. derive (priv, pub) at path via bip32
::    3. build-mgmt-ptst sot                → ptst
::    4. commit-address = tapscript-address:taproot pub ptst net
::    5. build 1-in/1-out P2TR key-path tx via tx-build
::    6. POST broadcast to mempool.space
::    7. return [txid 'broadcast']
::
::  Sponsor-side ops (%adopt/%reject/%detach) SHOULD additionally do a
::  reveal — that's left as a followup here; every op currently emits
::  the commit only.
::
++  run-causeway-op
  |=  [sot=sotx:urb state=state-0:s]
  =/  m  (fiber:io ,(unit [txid=@ux note=@t]))
  ^-  form:m
  =/  picked  (pick-signing-utxo state)
  ?~  picked
    ~&  >>  "causeway: no confirmed signing UTXO; op stays %pending"
    (pure:m ~)
  =/  sd    seed.u.picked
  =/  pt    path.u.picked
  =/  sel   [txid.u.picked vout.u.picked value.u.picked]
  =/  key-result=(each [priv=@ux pub=@ux] tang)
    %-  mule  |.
    =/  master  (from-seed:bip32 (seed-to-bytes:wallet-address sd))
    =/  derived  (derive-path:master pt)
    [prv.derived (ser-p:derived pub.derived)]
  ?:  ?=(%| -.key-result)
    ~&  >>>  ['causeway: key derivation failed' p.key-result]
    (pure:m ~)
  =/  priv=@ux  priv.p.key-result
  =/  pub=@ux   pub.p.key-result
  =/  ptst=ptst:taproot  (build-mgmt-ptst sot)
  =/  net=network:tx     (network-for state)
  =/  commit-address=@t
    (tapscript-address:taproot pub ptst (en-crypto:wallet-address net))
  =/  fee-rate=@ud  2
  =/  vbytes=@ud
    %+  add  overhead-vbytes:fees
    (add (input-vbytes:fees %p2tr) (output-vbytes:fees %p2tr))
  =/  fee=@ud  (calculate-fee:fees vbytes fee-rate)
  ?:  (lte value.sel fee)
    ~&  >>>  "causeway: UTXO too small ({<value.sel>}) for fee ({<fee>})"
    (pure:m ~)
  =/  commit-value=@ud  (sub value.sel fee)
  =/  txid-le=@ux  dat:(flip:byt:bcu [32 (rash txid.sel hex)])
  =/  inp=input:ap:tx
    :*  priv  pub  txid-le  vout.sel  value.sel
        `@ud`0xffff.ffff  [%p2tr %key-path ~]
    ==
  =/  build-result=(each tape tang)
    %-  mule  |.
    (build-transaction:txns net 2 ~[inp] ~[[commit-address commit-value]] 0)
  ?:  ?=(%| -.build-result)
    ~&  >>>  ['causeway: tx build failed' p.build-result]
    (pure:m ~)
  =/  tx-hex=@t  (crip p.build-result)
  ~&  "causeway: built commit tx ({(scow %ud (lent p.build-result))} hex chars)"
  =/  =request:http
    :*  %'POST'
        (crip (tx-base-url net))
        ~[['content-type' 'text/plain']]
        `(as-octs:mimes:html tx-hex)
    ==
  ;<  ~                      bind:m  (send-request:io request)
  ;<  =client-response:iris  bind:m  take-client-response:io
  =/  body=@t
    ?+  client-response  'broadcast-failed'
      [%finished * [~ [* [p=@ q=@]]]]
    q.data.u.full-file.client-response
    ==
  ?:  !=(64 (met 3 body))
    ~&  >>>  ['causeway: broadcast failed' body]
    (pure:m ~)
  =/  txid=@ux  (rash body hex)
  ~&  "causeway: broadcast {<txid>}"
  (pure:m `[txid 'broadcast via mempool.space'])
--
