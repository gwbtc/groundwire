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
::  +report: the operator-readable verdict, with each check CLASSIFIED
::
::    The headline names the OUTCOME, not just ok/not-ok, because the three
::    negative outcomes could not be more different: INVALID snubs the peer,
::    STALE demotes it to an alien, UNDETERMINED does nothing at all.
::    Phase 6.1 was diagnosable only because the per-check list was printed;
::    it was mis-read as fraud because nothing said which class the failing
::    check belonged to.
::
::      [ok]  passed
::      [XX]  failed, and it is FRAUD -- this is what causes a snub
::      [..]  failed, and it only means OUT OF DATE
::      [??]  failed because we could not evaluate it -- not evidence
::
++  report
  |=  =verdict:sa
  ^-  tang
  =/  outcome=tape
    ?:  ok.verdict                  "VALID"
    ?:  (unknown-verdict verdict)   "UNDETERMINED (no verdict emitted)"
    ?:  (stale-verdict verdict)     "STALE (out of date, not fraud)"
    "INVALID"
  :-  leaf+"%gw-btc: attestation for {<who.verdict>} is {outcome}"
  %+  turn  checks.verdict
  |=  =check:sa
  ^-  tank
  =/  mark=tape
    ?:  ok.check                                  "ok"
    ?:  (~(has in unknown-checks) name.check)     "??"
    ?:  (~(has in stale-checks) name.check)       ".."
    "XX"
  leaf+"  [{mark}] {(trip name.check)}"
::
::  +stale-checks: the named checks that mean OUT OF DATE, not WRONG
::
::    Decisions addendum section 3: when a comet's identity sat has been
::    spent, its old attestation is stale -- "this is not fraud and MUST
::    NOT produce %fail (a snub would block the replacement packet)".
::    That rule was written for the SCANNER path, where %gw-btc watches a
::    tracked tip get spent and emits a %stale-notice.  But the very same
::    physical fact arrives the other way round at least as often: the
::    owner moves the sat, its ship keeps sending the pass it booted with,
::    and the spent tip turns up in a PACKET -- where it is proven spent
::    by OUR OWN filter scan against the chain, not by anything the peer
::    said.  Discovered by the inbox rather than by the scanner, it is
::    still the same fact, and it must take the same path.  (Live
::    mainnet, 2026-08-05: it did not, and a comet was snubbed by its own
::    sponsor for an honest state update -- permanently, because the snub
::    then blocked the refreshed attestation that would have fixed it.)
::
::    So these three names, and only these, classify a FAILED verdict as
::    staleness:
::
::      tip-unspent     our filter scan found the log's tip outpoint spent
::      tracked-tip     our own tracker holds this comet's sat at a place
::                      this log never reaches, i.e. we are ahead of it
::      life-monotonic  the log's latest life is below one we already hold,
::                      i.e. this is an older copy of a log we know
::
::    Everything else stays a %fail, because it is evidence that was never
::    true rather than evidence that has expired: spawn-commit (the log is
::    not bound to this name), entry-N-commitment (a snapshot never
::    committed on chain), entry-N-continuity / -key-path / -sat-landed /
::    -txid (a custody hop that did not happen), entry-N-life-order (a log
::    that contradicts itself), tracked-prefix (a log that is not an
::    extension of the one we already verified -- a fork, not an old
::    copy), pass-key, and every structural check.
::
::    sponsor-known used to be in that list and is NOT any more; see
::    +unknown-checks below.  It failed for exactly the reason this comment
::    disqualifies -- our own view of the chain is behind -- and produced
::    the maximally destructive outcome anyway.
::
++  stale-checks
  ^-  (set cord)
  (silt ~['tip-unspent' 'tracked-tip' 'life-monotonic'])
::
::  +unknown-checks: the named checks that mean WE CANNOT TELL
::
::    The third class, and the one Phase 6.1 proved was missing.  A check
::    that the verifier was unable to EVALUATE is not evidence of anything:
::    not of fraud, and not even of staleness.  It must produce no verdict
::    at all -- silence -- because a negative verdict is a Jael %fail and an
::    Ames snub, and a snub then blocks the very packet that would resolve
::    the ignorance.
::
::    Live mainnet 2026-08-06: C3 verified C1's attestation while its own
::    block scanner sat 20 blocks below C3's OWN publication.  43 of 44
::    checks passed; `sponsor-known` failed because C3 did not yet know that
::    C3 existed -- and C3 snubbed the honest comet it sponsors.  The same
::    attestation from the same peer verified VALID two hours later with
::    nothing changed but the scan position.
::
::      sponsor-known  the snapshot names a sponsor we cannot see as a
::                     public point.  Our public index is a WINDOW on the
::                     chain -- it starts at an operator-chosen height and
::                     ends at whatever block the scanner has reached -- so
::                     "not in it" never distinguishes `no such comet' from
::                     `we have not looked there yet'.  A verifier that
::                     cannot see the sponsor has no evidence of fraud, only
::                     ignorance.
::      tip-scanned    the BIP-158 liveness scan could not be evaluated: an
::                     unavailable filter or block, an inconsistent answer,
::                     or a degenerate (empty) scan range.  Distinguished
::                     from `tip-unspent', which fails ONLY when the scan
::                     positively PROVED the outpoint spent.
::
++  unknown-checks
  ^-  (set cord)
  (silt ~['sponsor-known' 'tip-scanned'])
::
::  +unknown-verdict: did this verdict fail because we could not tell?
::
::    %.y when the verdict failed, at least one failing check is
::    unevaluable, and NO failing check is fraud.  A stale check failing
::    alongside an unevaluable one still reads as unknown: %stale is a real
::    finding about the peer's evidence, and we are not entitled to make it
::    while some of our own machinery came back blank.
::
::    Fraud beats both, exactly as in +stale-verdict: a peer does not get to
::    launder bad evidence by also being unknowable.
::
++  unknown-verdict
  |=  =verdict:sa
  ^-  ?
  ?:  ok.verdict  %.n
  =/  bad=(list check:sa)  (skip checks.verdict |=(c=check:sa ok.c))
  ?:  =(~ bad)  %.n
  ?.  (lien bad |=(c=check:sa (~(has in unknown-checks) name.c)))  %.n
  %+  levy  bad
  |=  c=check:sa
  ?|  (~(has in unknown-checks) name.c)
      (~(has in stale-checks) name.c)
  ==
::
::  +stale-verdict: did this verdict fail ONLY because it is out of date?
::
::    %.y exactly when the verdict failed and EVERY failing check is in
::    +stale-checks.  One genuine-fraud check failing alongside a stale
::    one still reads as fraud: a peer does not get to launder bad
::    evidence by also being out of date.
::
++  stale-verdict
  |=  =verdict:sa
  ^-  ?
  ?:  ok.verdict  %.n
  =/  bad=(list check:sa)  (skip checks.verdict |=(c=check:sa ok.c))
  ::  NB: =(~ bad) rather than ?~, which would fish-narrow .bad and leave
  ::  +levy mulling its sample against the bare ~ branch (mull-grow).
  ::
  ?:  =(~ bad)  %.n
  %+  levy  bad
  |=(c=check:sa (~(has in stale-checks) name.c))
::
::  +routable: can anything COLD-CONTACT a comet in this state?
::
::    A snapshot with neither a sponsor nor a fief is a one-way identity.
::    +urb-point-to-jael projects an absent sponsor to SELF, so the jael
::    point names the comet as its own sponsor and no peer that has
::    forgotten it can ever route to it again -- it can only ever speak
::    first, on a lane it already holds.
::
::    This is NOT a validity rule and MUST NOT become one: the decisions
::    addendum (section 2, "Fief scope") explicitly allows it, and a
::    negative verdict here would become a jael %fail and an ames snub of
::    a perfectly honest ship.  Causeway refuses to MINT one (the
::    --no-route opt-out); %gw-btc only warns its operator, so a
::    hand-rolled transaction that never touched Causeway is still
::    visible.
::
++  routable
  |=  snap=snapshot:sa
  ^-  ?
  |(?=(^ sponsor.snap) ?=(^ fief.snap))
::
::  +extend-log: append one custody entry, idempotently
::
::    The %anew ingestion path (app/gw-btc.hoon): Causeway hands us the
::    entry its custody transaction produced and we append it to the log
::    we already hold.  Re-poking the entry we ALREADY hold -- a retry, a
::    double click, a resumed script -- is a request to re-validate, not
::    a second hop: appending it twice would break input-0 continuity at
::    the duplicate and the log would then fail verification, silently,
::    forever.
::
++  extend-log
  |=  [base=custody-log:sa new=custody-entry:sa]
  ^-  custody-log:sa
  ?:  ?&(?=(^ base) =(new (rear base)))
    base
  (snoc base new)
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
::  xtr entry, in custody order.  `tip-unspent=~` is unknown: it fails
::  `tip-scanned` (never `tip-unspent`), so the verdict is not ok and no
::  point is built, but the failure is classed unevaluable rather than
::  stale or fraudulent.  `known-public` is the set of ships the caller can
::  vouch exist as public points; a snapshot naming a sponsor outside it
::  fails the sponsor-known check, which is likewise unevaluable -- the set
::  is a window on the chain, not the whole of it (an absent sponsor
::  projects to self and is always fine).
::
::  EVERY check in here must be evaluable from the arguments alone.  A
::  check that silently degrades when an input is empty or degenerate is
::  the bug class this file exists to prevent: it produces a verdict, and
::  a negative verdict is a snub.
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
    ::  THE LIVENESS SCAN IS THREE-VALUED AND MUST STAY THAT WAY.
    ::
    ::    tip-unspent=~       the scan could not be evaluated
    ::    tip-unspent=[~ %.n] the scan PROVED the outpoint spent
    ::    tip-unspent=[~ %.y] the scan proved it unspent
    ::
    ::  Collapsing the first two into one failing check (the old
    ::  `=([~ %.y] tip-unspent)`) made "we could not look" indistinguishable
    ::  from "we looked and it is gone", which routed an infrastructure
    ::  failure to the %stale demotion path.  Split them:
    ::
    ::    tip-scanned fails on ~ only          -> +unknown-checks -> silence
    ::    tip-unspent fails on [~ %.n] only    -> +stale-checks   -> %stale
    ::
    ::  tip-unspent passing VACUOUSLY on ~ is not a fail-open: tip-scanned
    ::  has already failed, so the verdict is not ok, no point is built, and
    ::  nothing is installed.  The only thing it changes is which of the two
    ::  non-fraud outcomes we take, which is the whole point.
    ::
    =.  checks  (snoc checks ['tip-scanned' ?=(^ tip-unspent)])
    =.  checks  (snoc checks ['tip-unspent' ?~(tip-unspent %.y u.tip-unspent)])
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
    ::  A named sponsor must exist as a public point; absent is self.
    ::  Failing this is IGNORANCE, not fraud -- see +unknown-checks.
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
