::  lib/self-attestation.hoon
::
::  Pure verification for the current %gw-btc pass-xtr custody log,
::  protocol kelvin 9: snapshot resolution, no event replay.  The
::  latest custody-proven $snapshot is authoritative; entries without
::  an opening are plain custody moves.  The verifier recomputes every
::  commitment itself and never parses script bytes from the chain.
::
::  Fetching is deliberately outside this library: ++run-checks
::  consumes the starting transaction and one fetched transaction per
::  xtr entry, which makes the trust boundary deterministic and
::  directly testable.
::
/-  bitcoin, ord, urb, sa=self-attestation
/+  bc=bitcoin, cc=gw-btc-pass, uc=urb-core
|%
::
++  report
  |=  =verdict:sa
  ^-  tang
  :-  leaf+"%gw-btc: attestation for {<who.verdict>} is {?:(ok.verdict "VALID" "INVALID")}"
  %+  turn  checks.verdict
  |=  =check:sa
  ^-  tank
  leaf+"  [{?:(ok.check "ok" "XX")}] {(trip name.check)}"
::
++  nom
  |=  [idx=@ud suffix=@t]
  ^-  cord
  (rap 3 ~['entry-' (scot %ud idx) '-' suffix])
::
::  Decode the fixed-domain, fixed-kelvin suite-C pass.  Both atom
::  tails are required to be canonical so `cue` cannot silently accept
::  appended alternate data.  The hiding dat commitment is NOT opened
::  here: the spawn satpoint is learned only from the custody log's
::  $blind-opening and bound to the pass in ++run-checks.
++  from-xtr
  |=  [who=@p =pass]
  ^-  (unit self-attestation:sa)
  %-  mole
  |.
  =/  meta  (need (parse-pass:cc pass))
  ?>  =(domain:cc dom.meta)
  ?>  =(kelvin:cc kel.meta)
  =/  cic  (com:nu:cric:crypto pass)
  ?>  ?=(%c suite.+<.cic)
  ?>  =(who fig:ex:cic)
  =/  chain=custody-log:sa  ;;(custody-log:sa (cue xtr.meta))
  ?>  =(xtr.meta (jam chain))
  [who pass chain]
::
::  +spawn-of: the blind-opening that must sit on entry 0
::
++  spawn-of
  |=  chain=custody-log:sa
  ^-  (unit blind-opening:sa)
  ?~  chain  ~
  ?~  opening.i.chain  ~
  blind-opening.u.opening.i.chain
::
::  +openings-of: (idx, opening) pairs in custody order
::
++  openings-of
  |=  chain=custody-log:sa
  ^-  (list [idx=@ud =opening:sa])
  =/  idx=@ud  0
  |-
  ?~  chain  ~
  ?~  opening.i.chain
    $(chain t.chain, idx +(idx))
  [[idx u.opening.i.chain] $(chain t.chain, idx +(idx))]
::
++  p2tr-xonly
  |=  spk=hexb:bitcoin
  ^-  (unit @ux)
  ?.  =(34 wid.spk)  ~
  ?.  =(0x5120 (rsh [3 32] dat.spk))  ~
  `(end [3 32] dat.spk)
::
++  is-key-path
  |=  wit=witness:tx:bitcoin
  ^-  ?
  ?.  ?=([* ~] wit)  %.n
  |(=(64 wid.i.wit) =(65 wid.i.wit))
::
++  snag-input
  |=  [idx=@ud =tx:bc]
  ^-  (unit inputw:tx:bitcoin)
  ?:  (gte idx (lent is.tx))  ~
  `(snag idx is.tx)
::
++  tracked-ok
  |=  [tracked=sont:ord tip=sont:ord entering=(list sont:ord)]
  ^-  ?
  ?|  =(tracked tip)
      (lien entering |=(s=sont:ord =(s tracked)))
  ==
::
++  prefix-chain
  |=  [old=custody-log:sa new=custody-log:sa]
  ^-  ?
  |-
  ?~  old  %.y
  ?~  new  %.n
  ?.  =(i.old i.new)  %.n
  $(old t.old, new t.new)
::
::  Derive the final satpoint using only input-0 continuity and output values.
::  The light-client adapter uses this before its final /tx-out request.
++  derive-tip
  |=  [spawn=sont:ord start=tx:bc txl=(list tx:bc)]
  ^-  (unit sont:ord)
  ?.  =(id.start txid.spawn)  ~
  =/  prev=tx:bc  start
  =/  current=sont:ord  spawn
  |-
  ?~  txl  `current
  ?.  =(id.prev txid.current)  ~
  ?.  (lth vout.current (lent os.prev))  ~
  =/  spent=output:tx:bitcoin  (snag vout.current os.prev)
  ?.  (lth off.current value.spent)  ~
  =/  inp  (snag-input 0 i.txl)
  ?~  inp  ~
  ?.  =([txid.u.inp pos.u.inp] [txid.current vout.current])  ~
  =/  landed  (index-to-sont:uc off.current os.i.txl)
  ?~  landed  ~
  %=  $
    prev     i.txl
    current  [id.i.txl vout.landed off.landed]
    txl      t.txl
  ==
::
::  Verify a present opening against the sat-carrying output CREATED by
::  this entry: the canonical unspendable commitment leaf over the
::  jammed snapshot, tweaked into the claimed internal key, must equal
::  the on-chain P2TR output key exactly.  Invalid curve points are
::  packet failures, not verifier crashes.
++  opening-checks
  |=  $:  idx=@ud
          =opening:sa
          this=tx:bc
          landing=[vout=@ud off=@ud]
      ==
  ^-  (list check:sa)
  =/  key=(unit @ux)
    %-  mole
    |.((state-key:cc internal-key.opening snapshot.opening))
  =/  out=output:tx:bitcoin  (snag vout.landing os.this)
  =/  onchain  (p2tr-xonly script-pubkey.out)
  :~  :-  (nom idx 'commitment')
      ?&  ?=(^ key)
          ?=(^ onchain)
          =(u.key u.onchain)
      ==
  ==
::
++  fail-result
  |=  [who=@p name=cord]
  ^-  result:sa
  [[who %.n ~[[name %.n]]] ~ 0]
::
++  fail-checks
  |=  [who=@p checks=(list check:sa)]
  ^-  result:sa
  [[who %.n checks] ~ 0]
::
::  Pure verification boundary.  `txl` has exactly one transaction per
::  xtr entry, in custody order.  `tip-unspent=~` is unknown and fails
::  closed.  `known-public` is the set of ships the caller can vouch
::  exist as public points; a snapshot naming a sponsor outside it
::  fails the sponsor-known check (an absent sponsor projects to self).
++  run-checks
  |=  $:  sat=self-attestation:sa
          start=tx:bc
          txl=(list tx:bc)
          tip-unspent=(unit ?)
          tracked=(unit anchor:sa)
          known-public=(set ship)
      ==
  ^-  result:sa
  =*  who  who.sat
  =|  checks=(list check:sa)
  =.  checks  (snoc checks ['chain-nonempty' ?=(^ chain.sat)])
  =.  checks  (snoc checks ['chain-bounded' (lte (lent chain.sat) 1.024)])
  =.  checks  (snoc checks ['fetch-count' =((lent chain.sat) (lent txl))])
  =/  spawn-open  (spawn-of chain.sat)
  =.  checks  (snoc checks ['spawn-opening' ?=(^ spawn-open)])
  ?.  (levy checks |=(c=check:sa ok.c))
    (fail-checks who checks)
  ?>  ?=(^ spawn-open)
  =/  spawn=sont:ord  spawn.u.spawn-open
  ::  the pass's hiding dat commitment must open to exactly this spawn
  ::  satpoint and blind
  ::
  =/  meta  (parse-pass:cc pass.sat)
  =/  commit-ok=?
    ?~  meta  %.n
    =((spawn-commit:cc spawn blind.u.spawn-open) d.u.meta)
  =.  checks  (snoc checks ['spawn-commit' commit-ok])
  =.  checks  (snoc checks ['start-txid' =(id.start txid.spawn)])
  ?.  (levy checks |=(c=check:sa ok.c))
    (fail-checks who checks)
  ?.  (lth vout.spawn (lent os.start))
    (fail-checks who (snoc checks ['start-vout-range' %.n]))
  =/  start-out=output:tx:bitcoin  (snag vout.spawn os.start)
  ?.  (lth off.spawn value.start-out)
    (fail-checks who (snoc checks ['start-off-range' %.n]))
  =.  checks  (snoc checks ['start-vout-range' %.y])
  =.  checks  (snoc checks ['start-off-range' %.y])
  =/  entries=custody-log:sa  chain.sat
  =/  txs=(list tx:bc)  txl
  =/  idx=@ud  0
  =/  prev=tx:bc  start
  =/  current=sont:ord  spawn
  =|  last-height=@ud
  =|  entering=(list sont:ord)
  =|  latest=(unit [idx=@ud snap=snapshot:sa])
  |-
  ^-  result:sa
  ?~  entries
    ::  end of the walk: resolve the latest snapshot and the tip
    ::
    =.  checks  (snoc checks ['state-resolve' ?=(^ latest)])
    =.  checks  (snoc checks ['tip-unspent' =([~ %.y] tip-unspent)])
    =/  tip-out=output:tx:bitcoin  (snag vout.current os.prev)
    =.  checks
      (snoc checks ['tip-p2tr' ?=(^ (p2tr-xonly script-pubkey.tip-out))])
    ::  the carried pass's messaging key must match the latest
    ::  custody-proven snapshot
    ::
    =/  key-ok=?
      ?~  latest  %.n
      =/  got=(unit @)
        %-  mole  |.
        =/  cic  (com:nu:cric:crypto pass.sat)
        ?>  ?=(%c suite.+<.cic)
        `@`cry.pub.+<.cic
      ?~  got  %.n
      =(u.got key.snap.u.latest)
    =.  checks  (snoc checks ['pass-key' key-ok])
    ::  a named sponsor must exist as a public point; absent is self
    ::
    =/  sponsor-ok=?
      ?~  latest  %.n
      ?~  sponsor.snap.u.latest  %.y
      (~(has in known-public) u.sponsor.snap.u.latest)
    =.  checks  (snoc checks ['sponsor-known' sponsor-ok])
    =?  checks  ?=(^ tracked)
      %+  snoc  checks
      ['tracked-tip' (tracked-ok sont.own.point.u.tracked current entering)]
    =/  old-sat=(unit self-attestation:sa)
      ?~  tracked  ~
      (from-xtr who pass.net.point.u.tracked)
    =/  anchor-ok=?
      ?~  tracked  %.y
      ?~  old-sat  %.n
      ?.  =((spawn-of chain.u.old-sat) spawn-open)  %.n
      ?.  (prefix-chain chain.u.old-sat chain.sat)  %.n
      =/  boundary
        (derive-tip spawn start (scag (lent chain.u.old-sat) txl))
      =(`tip.u.tracked boundary)
    =.  checks  (snoc checks ['tracked-prefix' anchor-ok])
    =/  life-ok=?
      ?~  tracked  %.y
      ?~  latest  %.n
      (gte life.snap.u.latest life.net.point.u.tracked)
    =?  checks  ?=(^ tracked)
      (snoc checks ['life-monotonic' life-ok])
    =/  ok  (levy checks |=(c=check:sa ok.c))
    =/  point=(unit point:urb)
      ?.  &(ok ?=(^ latest))  ~
      =*  snap  snap.u.latest
      :-  ~
      :*  own=[current ~]
          rift=rift.snap
          life=life.snap
          pass=pass.sat
          ^=  sponsor
          ?~  sponsor.snap
            [%.n who]
          [%.y u.sponsor.snap]
          escape=~
          fief=fief.snap
      ==
    :+  [who ok checks]
      point
    ?.  ok  0
    ?.  (lth vout.current (lent os.prev))  0
    value:(snag vout.current os.prev)
  ?~  txs
    (fail-checks who (snoc checks ['fetch-count' %.n]))
  =*  ent  i.entries
  =*  this  i.txs
  =.  checks  (snoc checks [(nom idx 'txid') =(txid.ent id.this)])
  =.  checks
    (snoc checks [(nom idx 'height-order') ?:(=(0 idx) & (gte height.ent last-height))])
  =/  inp  (snag-input 0 this)
  ?~  inp
    (fail-checks who (snoc checks [(nom idx 'input-zero') %.n]))
  =.  checks  (snoc checks [(nom idx 'input-zero') %.y])
  =/  continuity=?
    =([txid.u.inp pos.u.inp] [txid.current vout.current])
  =.  checks  (snoc checks [(nom idx 'continuity') continuity])
  ?.  &(=(id.prev txid.current) (lth vout.current (lent os.prev)))
    (fail-checks who (snoc checks [(nom idx 'prevout-range') %.n]))
  =/  spent=output:tx:bitcoin  (snag vout.current os.prev)
  ::  After the arbitrary first-hop spend, a one-item 64/65-byte witness
  ::  is a key-path proof only when Bitcoin evaluated it against a P2TR
  ::  prevout.  Other witness programs can have the same stack shape.
  ::  All spends after the first hop must be key-path: the commitment
  ::  leaf is unspendable by construction.
  =/  spent-key  (p2tr-xonly script-pubkey.spent)
  =/  keypath=?
    ?:(=(0 idx) %.y &(?=(^ spent-key) (is-key-path witness.u.inp)))
  =.  checks  (snoc checks [(nom idx 'key-path') keypath])
  =/  off-ok=?  (lth off.current value.spent)
  =.  checks  (snoc checks [(nom idx 'off-range') off-ok])
  =/  landed  (index-to-sont:uc off.current os.this)
  =.  checks  (snoc checks [(nom idx 'sat-landed') ?=(^ landed)])
  ?.  ?&  =(txid.ent id.this)
          continuity
          keypath
          off-ok
          ?=(^ landed)
      ==
    (fail-checks who checks)
  =/  next=sont:ord  [id.this vout.landed off.landed]
  =.  entering  [current entering]
  ?~  opening.ent
    %=  $
      entries      t.entries
      txs          t.txs
      idx          +(idx)
      prev         this
      current      next
      last-height  height.ent
    ==
  =*  open  u.opening.ent
  =.  checks  (weld checks (opening-checks idx open this landed))
  ::  the dat opening may sit only on entry 0, and snapshot lives may
  ::  never regress across openings
  ::
  =.  checks
    %+  snoc  checks
    [(nom idx 'blind-opening-zero') |(=(0 idx) ?=(~ blind-opening.open))]
  =.  checks
    %+  snoc  checks
    :-  (nom idx 'life-order')
    ?~  latest  %.y
    ?&  (gte life.snapshot.open life.snap.u.latest)
        (gte rift.snapshot.open rift.snap.u.latest)
    ==
  %=  $
    entries      t.entries
    txs          t.txs
    idx          +(idx)
    prev         this
    current      next
    last-height  height.ent
    latest       `[idx snapshot.open]
  ==
--
