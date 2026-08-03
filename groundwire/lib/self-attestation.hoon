::  lib/self-attestation.hoon
::
::  Pure verification for the current %gw-btc pass-xtr custody log.
::  Fetching is deliberately outside this library: ++run-checks consumes the
::  starting transaction and one fetched transaction per xtr entry, which
::  makes the trust boundary deterministic and directly testable.
::
/-  bitcoin, ord, urb, sa=self-attestation
/+  bc=bitcoin, bscr=btc-script, cc=gw-btc-pass, tr=taproot,
    ue=urb-encoder, uc=urb-core
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
::  Decode the fixed-domain suite-C pass.  Both atom tails are required to be
::  canonical so `cue` cannot silently accept appended alternate data.
++  from-xtr
  |=  [who=@p =pass]
  ^-  (unit self-attestation:sa)
  %-  mole
  |.
  =/  meta  (need (parse-pass:cc pass))
  ?>  =(domain:cc dom.meta)
  =/  cic  (com:nu:cric:crypto pass)
  ?>  ?=(%c suite.+<.cic)
  ?>  =(who fig:ex:cic)
  ?>  =(dat.tw.pub:+<.cic (make-dat:cc spawn.meta))
  =/  chain=custody-log:sa  ;;(custody-log:sa (cue xtr.meta))
  ?>  =(xtr.meta (jam chain))
  [who spawn.meta chain]
::
::  Reconstruct a single-leaf Taproot output key.  Invalid curve points are
::  packet failures, not verifier crashes.
++  out-key
  |=  =reveal:sa
  ^-  (unit @ux)
  %-  mole
  |.
  =/  =tapleaf:tr  tapleaf.reveal
  ::  The current Groundwire commitment format is BIP-342 Tapscript.
  ::  Other leaf-version bytes can produce an algebraic TapTweak but are
  ::  not valid encodings of this protocol's script-path commitment.
  ?>  =(0xc0 version.tapleaf)
  (output-pubkey:tr internal-key.reveal `(leaf-hash:tr tapleaf))
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
::  Parse the same `urb` envelope consumed by lib/urb-core.  The packet is
::  adversarial input, so malformed scripts are virtualized to ~.
++  parse-leaf
  |=  script=hexb:bitcoin
  ^-  (unit (list raw-sotx:urb))
  %-  mole
  |.
  =/  oct=octs  [wid.script dat.script]
  ::  Match the public block parser exactly: the urb envelope must begin at
  ::  byte zero and the descriptor must re-encode to the same sized script.
  ?>  ?&  (gte p.oct 6)
          =(0x63.0375.7262 (cut 3 [(sub p.oct 6) 5] q.oct))
      ==
  =/  descr  (de:bscr oct)
  ?~  descr  !!
  ?>  =(oct (en:bscr u.descr))
  =/  unvs=(list @)  (unv:de:ue u.descr)
  (zing (turn unvs parse-roll:ue))
::
++  singles
  |=  sots=(list raw-sotx:urb)
  ^-  (list [=ship sig=(unit @) =single:skim-sotx:urb])
  %-  zing
  %+  turn  sots
  |=  r=raw-sotx:urb
  ^-  (list [=ship sig=(unit @) =single:skim-sotx:urb])
  =/  who=ship      ship.sot.r
  =/  sig=(unit @)  sig.sot.r
  =/  skim          +.sot.r
  ?:  ?=(%batch -.skim)
    (turn bat.skim |=(s=single:skim-sotx:urb [who sig s]))
  [who sig skim]~
::
++  find-spawn
  |=  sots=(list raw-sotx:urb)
  ^-  (unit single:skim-sotx:urb)
  =/  sx  (singles sots)
  |-
  ?~  sx  ~
  =*  act  single.i.sx
  ?:  ?=(%spawn -.act)  `act
  $(sx t.sx)
::
++  spawn-first-ok
  |=  sots=(list raw-sotx:urb)
  ^-  ?
  =/  sx  (singles sots)
  =/  first-ok=?
    ?~  sx  |
    ?=(%spawn -.single.i.sx)
  =/  count=@ud  0
  |-
  ?~  sx  &(first-ok =(1 count))
  =.  count  ?:(?=(%spawn -.single.i.sx) +(count) count)
  $(sx t.sx)
::
::  Validate the `%spawn` delta against the immutable starting precommit
::  satpoint.  This mirrors urb-core's precommit checks without needing input
::  values (the offset and `tej` still must fit the named output).
++  check-spawn
  |=  [sat=self-attestation:sa start=tx:bc sots=(list raw-sotx:urb)]
  ^-  (list check:sa)
  =/  found  (find-spawn sots)
  ?~  found  ~[['spawn-found' %.n]]
  =*  sp  u.found
  ?>  ?=(%spawn -.sp)
  =/  start-id=?  =(id.start txid.spawn.sat)
  =/  in-range=?  (lth vout.spawn.sat (lent os.start))
  ?.  in-range
    :~  ['spawn-found' %.y]
        ['spawn-start-id' start-id]
        ['spawn-vout-range' %.n]
    ==
  =/  out=output:tx:bitcoin  (snag vout.spawn.sat os.start)
  =/  en-out  (can 3 script-pubkey.out 8^value.out ~)
  =/  hax-out  (shay (add 8 wid.script-pubkey.out) en-out)
  =/  to-vout=?
    ?~  vout.to.sp  %.n
    =(u.vout.to.sp vout.spawn.sat)
  =/  to-off=?  =(off.to.sp off.spawn.sat)
  =/  off-ok=?  (lth off.spawn.sat value.out)
  =/  tej-ok=?  (lte (add off.to.sp tej.to.sp) value.out)
  =/  spkh-ok=?  =(spkh.to.sp hax-out)
  =/  meta  (parse-pass:cc pass.sp)
  =/  key-ok=?
    ?~  meta  %.n
    =/  cic  (com:nu:cric:crypto pass.sp)
    ?.  ?=(%c suite.+<.cic)  %.n
    ?&  =(domain:cc dom.u.meta)
        =(spawn.sat spawn.u.meta)
        =(dat.tw.pub:+<.cic (make-dat:cc spawn.sat))
        =(who.sat fig:ex:cic)
    ==
  :~  ['spawn-found' %.y]
      ['spawn-start-id' start-id]
      ['spawn-vout-range' in-range]
      ['spawn-vout' to-vout]
      ['spawn-off' to-off]
      ['spawn-off-range' off-ok]
      ['spawn-tej-range' tej-ok]
      ['spawn-precommit-spkh' spkh-ok]
      ['spawn-key' key-ok]
  ==
::
::  Verify a present reveal against the sat-carrying output CREATED by this
::  entry.  There is no separate claimed-sot list: the leaf is authoritative.
++  reveal-checks
  |=  $:  who=@p
          idx=@ud
          =reveal:sa
          this=tx:bc
          landing=[vout=@ud off=@ud]
      ==
  ^-  [checks=(list check:sa) parsed=(unit (list raw-sotx:urb))]
  =/  key  (out-key reveal)
  =/  out=output:tx:bitcoin  (snag vout.landing os.this)
  =/  onchain  (p2tr-xonly script-pubkey.out)
  =/  commit-ok=?
    ?&  ?=(^ key)
        ?=(^ onchain)
        =(u.key u.onchain)
    ==
  =/  parsed  (parse-leaf script.tapleaf.reveal)
  =/  nonempty=?  &(?=(^ parsed) ?=(^ u.parsed))
  =/  ship-ok=?
    ?~  parsed  %.n
    %+  levy  u.parsed
    |=(r=raw-sotx:urb =(who ship.sot.r))
  :_  parsed
  :~  [(nom idx 'commitment') commit-ok]
      [(nom idx 'sots-nonempty') nonempty]
      [(nom idx 'sots-ship') ship-ok]
  ==
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
::  Fold revealed Groundwire deltas in custody order.  Reveal-less entries do
::  not appear here and therefore leave networking state unchanged.
++  replay-chain
  |=  $:  who=@p
          revealed=(list [idx=@ud height=@ud sots=(list raw-sotx:urb)])
          tip=sont:ord
          sponsors=(map @p point:urb)
          initial=(unit point:urb)
      ==
  ^-  (unit point:urb)
  =/  pnt=(unit point:urb)  initial
  |-
  ?~  revealed
    ?~  pnt  ~
    `u.pnt(sont.own tip)
  =/  raws  sots.i.revealed
  |-
  ?~  raws
    ^$(revealed t.revealed)
  ?.  =(who ship.sot.i.raws)
    $(raws t.raws)
  =/  res
    (replay-singles who sponsors height.i.revealed pnt (singles ~[i.raws]))
  ?~  res  ~
  $(raws t.raws, pnt u.res)
::
++  replay-singles
  |=  $:  who=@p
          sponsors=(map @p point:urb)
          height=@ud
          pnt=(unit point:urb)
          sx=(list [=ship sig=(unit @) =single:skim-sotx:urb])
      ==
  ^-  (unit (unit point:urb))
  |-
  ?~  sx  `pnt
  =*  s  single.i.sx
  ?:  ?=(%spawn -.s)
    ?^  pnt  ~
    =/  new=point:urb
      :*  own=[[0x0 0 0] ~]
          rift=0
          life=1
          pass=pass.s
          sponsor=[%.n who]
          escape=~
          fief=fief.s
      ==
    $(sx t.sx, pnt `new)
  ?~  pnt  ~
  =/  p=point:urb  u.pnt
  ?-    -.s
      %keys
    ::  A malformed committed pass is an invalid state transition, not an
    ::  excuse to crash Gall later while comparing the carried pass.
    ?.  (same-key:cc pass.s pass.s)  ~
    =.  net.p  net.p(pass pass.s, life +(life.net.p))
    =?  rift.net.p  breach.s  +(rift.net.p)
    $(sx t.sx, pnt `p)
  ::
      %fief
    $(sx t.sx, pnt `p(fief.net fief.s))
  ::
      %escape
    ?:  =(parent.s who)
      $(sx t.sx, pnt `p(sponsor.net [%.y who], escape.net ~))
    ?^  sig.s
      =/  sponsor  (~(get by sponsors) parent.s)
      ?~  sponsor  `pnt
      =/  cac  (com:nu:cric:crypto pass.net.u.sponsor)
      =/  lo=@ud  ?:((lth height 10) 0 (sub height 10))
      ?.  %+  lien  (gulf lo +(height))
          |=  h=@ud
          (veri-octs:ed:crypto u.sig.s 512^(shaz (jam [who h])) sgn:ded:ex:cac)
        `pnt
      $(sx t.sx, pnt `p(sponsor.net [%.y parent.s], escape.net ~))
    $(sx t.sx, pnt `p(escape.net `parent.s))
  ::
      %cancel-escape
    ?.  =(`parent.s escape.net.p)  `pnt
    $(sx t.sx, pnt `p(escape.net ~))
  ::
      %adopt
    ::  A custody log proves only the attesting ship's sat history. An
    ::  adopt of another identity mutates state that cannot be reconstructed
    ::  from this packet, so the delta format must reject it.
    ?.  =(ship.s who)  ~
    $(sx t.sx, pnt `p(sponsor.net [%.y who], escape.net ~))
  ::
      %reject
    ?.  =(ship.s who)  ~
    ?.  =(`who escape.net.p)  `pnt
    $(sx t.sx, pnt `p(escape.net ~))
  ::
      %detach
    ?.  =(ship.s who)  ~
    ?.  =([%.y who] sponsor.net.p)  `pnt
    $(sx t.sx, pnt `p(sponsor.net [%.n who]))
  ::
      %set-mang
    ~
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
::  Pure verification boundary.  `txl` has exactly one transaction per xtr
::  entry, in custody order.  `tip-unspent=~` is unknown and fails closed.
++  run-checks
  |=  $:  sat=self-attestation:sa
          start=tx:bc
          txl=(list tx:bc)
          tip-unspent=(unit ?)
          tracked=(unit anchor:sa)
          sponsors=(map @p point:urb)
      ==
  ^-  result:sa
  =*  who  who.sat
  =|  checks=(list check:sa)
  =.  checks  (snoc checks ['chain-nonempty' ?=(^ chain.sat)])
  =.  checks  (snoc checks ['chain-bounded' (lte (lent chain.sat) 1.024)])
  =.  checks  (snoc checks ['fetch-count' =((lent chain.sat) (lent txl))])
  =.  checks  (snoc checks ['start-txid' =(id.start txid.spawn.sat)])
  ?.  (levy checks |=(c=check:sa ok.c))
    (fail-checks who checks)
  ?.  (lth vout.spawn.sat (lent os.start))
    (fail-checks who (snoc checks ['start-vout-range' %.n]))
  =/  start-out=output:tx:bitcoin  (snag vout.spawn.sat os.start)
  ?.  (lth off.spawn.sat value.start-out)
    (fail-checks who (snoc checks ['start-off-range' %.n]))
  =.  checks  (snoc checks ['start-vout-range' %.y])
  =.  checks  (snoc checks ['start-off-range' %.y])
  =/  entries=custody-log:sa  chain.sat
  =/  txs=(list tx:bc)  txl
  =/  idx=@ud  0
  =/  prev=tx:bc  start
  =/  current=sont:ord  spawn.sat
  =|  last-height=@ud
  =|  entering=(list sont:ord)
  =|  revealed=(list [idx=@ud height=@ud sots=(list raw-sotx:urb)])
  =|  genesis-sots=(unit (list raw-sotx:urb))
  |-
  ^-  result:sa
  ?~  entries
    =/  ordered  (flop revealed)
    =/  all-sots=(list raw-sotx:urb)
      %-  zing
      %+  turn  ordered
      |=(x=[idx=@ud height=@ud sots=(list raw-sotx:urb)] sots.x)
    =.  checks
      (weld checks (check-spawn sat start ?~(genesis-sots ~ u.genesis-sots)))
    =.  checks  (snoc checks ['spawn-entry-zero' ?=(^ genesis-sots)])
    =.  checks  (snoc checks ['spawn-first' (spawn-first-ok all-sots)])
    =.  checks  (snoc checks ['tip-unspent' =([~ %.y] tip-unspent)])
    =/  tip-out=output:tx:bitcoin  (snag vout.current os.prev)
    =.  checks
      (snoc checks ['tip-p2tr' ?=(^ (p2tr-xonly script-pubkey.tip-out))])
    =?  checks  ?=(^ tracked)
      (snoc checks ['tracked-tip' (tracked-ok sont.own.point.u.tracked current entering)])
    =/  old-sat=(unit self-attestation:sa)
      ?~  tracked  ~
      (from-xtr who pass.net.point.u.tracked)
    =/  old-chain=(unit custody-log:sa)
      ?~  old-sat  ~
      `chain.u.old-sat
    =/  anchor-ok=?
      ?~  tracked  %.y
      ?~  old-sat  %.n
      ?.  =(spawn.sat spawn.u.old-sat)  %.n
      ?.  (prefix-chain chain.u.old-sat chain.sat)  %.n
      =/  boundary
        (derive-tip spawn.sat start (scag (lent chain.u.old-sat) txl))
      =(`tip.u.tracked boundary)
    =.  checks  (snoc checks ['tracked-prefix' anchor-ok])
    =/  preliminary  (levy checks |=(c=check:sa ok.c))
    =/  cutoff=@ud
      ?~(old-chain 0 (lent u.old-chain))
    =/  suffix=(list [idx=@ud height=@ud sots=(list raw-sotx:urb)])
      %+  skip  ordered
      |=(x=[idx=@ud height=@ud sots=(list raw-sotx:urb)] (lth idx.x cutoff))
    =/  initial=(unit point:urb)
      ?~  tracked  ~
      `point.u.tracked(sont.own tip.u.tracked)
    =/  point=(unit point:urb)
      ?.  preliminary  ~
      (replay-chain who suffix current sponsors initial)
    =.  checks  (snoc checks ['state-replay' ?=(^ point)])
    =/  ok  (levy checks |=(c=check:sa ok.c))
    :+  [who ok checks]
      ?.(ok ~ point)
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
  ::  After the arbitrary precommit spend, a one-item 64/65-byte witness is
  ::  a key-path proof only when Bitcoin evaluated it against a P2TR prevout.
  ::  Other witness programs can have the same stack shape.
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
  ?~  reveal.ent
    %=  $
      entries      t.entries
      txs          t.txs
      idx          +(idx)
      prev         this
      current      next
      last-height  height.ent
    ==
  =/  [rc=(list check:sa) parsed=(unit (list raw-sotx:urb))]
    (reveal-checks who idx u.reveal.ent this landed)
  =.  checks  (weld checks rc)
  =?  genesis-sots  &(=(0 idx) ?=(^ parsed))  `u.parsed
  =?  revealed  ?=(^ parsed)  [[idx height.ent u.parsed] revealed]
  %=  $
    entries      t.entries
    txs          t.txs
    idx          +(idx)
    prev         this
    current      next
    last-height  height.ent
  ==
--
