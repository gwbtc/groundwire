::  lib/self-attestation.hoon
::
::  Verification of CONFIDENTIAL COMET self-attestation packets (protocol 2.0).
::  See sur/self-attestation for the packet format and protocol rules.
::
::  %urb-watcher calls ++verify in a khan thread. The strand fetches every
::  referenced transaction from the Bitcoin node (the node is trusted, the same
::  trust model as %urb-watcher's block processing) and then runs the pure
::  verifier ++run-checks, which establishes:
::
::    - genesis: the %spawn's precommit output exists with the attested
::      scriptPubKey hash, the comet's suite-C networking key encodes the tweak
::      binding it to that precommit satpoint, who = fig of the key, and the
::      commit transaction actually SPENDS the precommit satpoint (so precommit
::      and commit share a controller, mirroring lib/urb-core's %spawn proof);
::    - per link: the transaction spends the exact sat-carrying output of the
::      previous transaction (full outpoint, not just txid), that output's
::      taproot key equals Q recomputed from the off-chain reveal (single-leaf
::      commitment), the spend is key-path-shaped, the leaf re-parses to
::      exactly the claimed self-enacted sotx(es), and the sat's offset is
::      tracked deterministically through the outputs (index-to-sont math, so
::      one genesis cannot fork into two divergent chains at a multi-output tx);
::    - tip: the sat's final landing equals the claimed tip (txid AND vout AND
::      off) and that output is currently unspent; if the caller already tracks
::      a sont for this ship, the packet must reconcile with it.
::
::  On an ok verdict, ++replay-chain folds the chain's sotxes into a point:urb
::  (subject-only mirror of process-unv) for storage in unv-ids/Jael.
::
::  XX trust/scope notes, accepted for the prototype:
::    - Schnorr signatures are not re-verified: the txs are on-chain, so the
::      node's consensus rules already validated them against Q; ++is-key-path
::      only guards the witness SHAPE (and rejects annexes, a false negative).
::    - gettxout includes the mempool, so a tip spent by an unconfirmed tx is
::      conservatively rejected and an unconfirmed tip is accepted.
::    - The packet is a bearer proof: any recipient can replay it to others.
::      A future challenge-response could bind packets to recipients.
::
/-  bitcoin, ord, urb, sa=self-attestation
/+  bc=bitcoin, bscr=btc-script, btcio, strandio,
    tr=taproot, ue=urb-encoder, uc=urb-core
|%
::  +report: pretty-print a verdict for the dojo log.
::
++  report
  |=  =verdict:sa
  ^-  tang
  :-  leaf+"%urb-watcher: attestation for {<who.verdict>} is {?:(ok.verdict "VALID" "INVALID")}"
  %+  turn  checks.verdict
  |=  =check:sa
  ^-  tank
  leaf+"  [{?:(ok.check "ok" "XX")}] {(trip name.check)}"
::
::  +nom: build a per-link check name like 'link-3-commitment'.
::
++  nom
  |=  [idx=@ud suffix=@t]
  ^-  cord
  (rap 3 ~['link-' (scot %ud idx) '-' suffix])
::
::  +out-key: reconstruct the taproot output x-only key from a reveal. The tree
::  is a single leaf, so the merkle root is just that leaf's hash.
::
++  out-key
  |=  =reveal:sa
  ^-  @ux
  =/  =tapleaf:tr  tapleaf.reveal
  (output-pubkey:tr internal-key.reveal `(leaf-hash:tr tapleaf))
::
::  +p2tr-xonly: extract the 32-byte x-only key from a P2TR scriptPubKey
::  (OP_1 PUSH32 <key> = 0x51 0x20 ...). Returns ~ if not a v1 taproot output.
::
++  p2tr-xonly
  |=  spk=hexb:bitcoin
  ^-  (unit @ux)
  ?.  =(34 wid.spk)  ~
  ?.  =(0x5120 (rsh [3 32] dat.spk))  ~
  `(end [3 32] dat.spk)
::
::  +is-key-path: a taproot key-path witness is a single element (the Schnorr
::  signature, 64 bytes or 65 with a sighash byte) and carries no control block
::  (a script-path spend always has >=2 elements).
::  XX annex (a trailing 0x50-prefixed element) is not handled: a key-path
::     spend with an annex is wrongly rejected (a false negative, not a hole).
::
++  is-key-path
  |=  wit=witness:tx:bitcoin
  ^-  ?
  ?.  ?=([* ~] wit)  %.n
  |(=(64 wid.i.wit) =(65 wid.i.wit))
::
::  +snag-input: the inputw at index `idx`, or ~ if out of range.
::
++  snag-input
  |=  [idx=@ud =tx:bc]
  ^-  (unit inputw:tx:bitcoin)
  ?:  (gte idx (lent is.tx))  ~
  `(snag idx is.tx)
::
::  +parse-leaf: decode the sotx(es) committed in a tapleaf script, exactly as
::  %urb-core does for on-chain reveals (btc-script -> unv -> raw-sotx).
::  The urb-encoder parsers crash on malformed input; a malicious packet must
::  yield a failed check rather than a thread crash, so the whole parse is
::  virtualized. ~ = unparsable; `~ = parsed, but no urb envelope/sotxes.
::
++  parse-leaf
  |=  script=hexb:bitcoin
  ^-  (unit (list raw-sotx:urb))
  %-  mole
  |.
  ^-  (list raw-sotx:urb)
  =/  oct=octs  [wid.script dat.script]
  =/  descr  (de:bscr oct)
  ?~  descr  !!
  =/  unvs=(list @)  (unv:de:ue u.descr)
  (zing (turn unvs parse-roll:ue))
::
::  +get-sotx: project a raw-sotx down to its sotx for structural comparison.
::
++  get-sotx
  |=  r=raw-sotx:urb
  ^-  sotx:urb
  sot.r
::
::  +singles: flatten a list of raw-sotx, expanding %batch in order and
::  carrying the enclosing sotx's ship and sig onto each single.
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
::  +find-spawn: pull the %spawn single out of a skim-sotx (bare or in a batch).
::
++  find-spawn
  |=  act=skim-sotx:urb
  ^-  (unit $>(%spawn single:skim-sotx:urb))
  ?:  ?=(%spawn -.act)  `act
  ?.  ?=(%batch -.act)  ~
  |-  ^-  (unit $>(%spawn single:skim-sotx:urb))
  ?~  bat.act  ~
  ?:  ?=(%spawn -.i.bat.act)  `i.bat.act
  $(bat.act t.bat.act)
::
::  +spawn-first-ok: the %spawn must be the FIRST single of link 0's first
::  raw-sotx, and must appear nowhere else in the chain. (This is the rule
::  urb-core's dead bat-cnt check INTENDS to enforce on-chain.)
::
++  spawn-first-ok
  |=  chain=(list link:sa)
  ^-  ?
  ?~  chain  |
  =/  first-ok=?
    ?~  sots.i.chain  |
    =/  sx  (singles ~[i.sots.i.chain])
    ?~  sx  |
    ?=(%spawn -.single.i.sx)
  =/  count=@ud
    %+  roll  `(list link:sa)`chain
    |=  [l=link:sa n=@ud]
    %+  add  n
    %+  roll  (singles sots.l)
    |=  [x=[=ship sig=(unit @) =single:skim-sotx:urb] m=@ud]
    ?:(?=(%spawn -.single.x) +(m) m)
  &(first-ok =(1 count))
::
::  +tracked-ok: reconcile the packet against a sont the verifier already
::  tracks for this ship. Pass iff the tracked sont is the packet's tip
::  (verifier up to date) or the entering sont of some link (verifier behind;
::  the packet legitimately extends its knowledge). A tracked sont matching
::  neither means the verifier is ahead of the packet or the packet describes
::  a divergent chain/different sat -- fail either way.
::
++  tracked-ok
  |=  [tracked=sont:ord tip=sont:ord entering=(list sont:ord)]
  ^-  ?
  ?|  =(tracked tip)
      (lien entering |=(s=sont:ord =(s tracked)))
  ==
::
::  +check-spawn: the genesis %spawn / precommit / tweak proof, mirroring
::  lib/urb-core's on-chain %spawn handling. Returns the precommit satpoint
::  when every check passes (the caller then proves the commit tx spends it).
::
++  check-spawn
  |=  [sat=self-attestation:sa pre=tx:bc]
  ^-  [checks=(list check:sa) psat=(unit sont:ord)]
  ?~  chain.sat  [~[['spawn-no-chain' %.n]] ~]
  =/  sots  sots.i.chain.sat
  ?~  sots  [~[['spawn-no-sots' %.n]] ~]
  =/  spawn  (find-spawn +.sot.i.sots)
  ?~  spawn  [~[['spawn-not-found' %.n]] ~]
  =*  sp  u.spawn
  ?~  vout.to.sp  [~[['spawn-no-vout' %.n]] ~]
  ?.  (lth u.vout.to.sp (lent os.pre))
    [~[['spawn-precommit-range' %.n]] ~]
  =/  out=output:tx:bitcoin  (snag u.vout.to.sp os.pre)
  ::  Rebuild the precommit output's scriptPubKey hash, as calc-precommit-sont.
  =/  en-out  (can 3 script-pubkey.out 8^value.out ~)
  =/  hax-out  (shay (add 8 wid.script-pubkey.out) en-out)
  =/  spkh-ok=?  =(hax-out spkh.to.sp)
  ::  The claimed offset (and tej) must fit inside the precommit output.
  ::  XX calc-precommit-sont's total-input-value upper bound is skipped here;
  ::     it would need the precommit's own prevout values.
  =/  off-ok=?  (lte (add off.to.sp tej.to.sp) value.out)
  =/  psat=sont:ord  [id.pre u.vout.to.sp off.to.sp]
  ::  The tweak that the networking key must encode (mirrors lib/urb-core).
  =/  tweak
    %+  rap  3
    :~  %9  ~tyr  %urb-watcher  %btc  %gw  %9
        txid.psat  vout.psat  off.psat
    ==
  =/  cac  (com:nu:cric:crypto pass.sp)
  ?.  ?=(%c suite.+<.cac)
    :_  ~
    :~  ['spawn-precommit-spkh' spkh-ok]
        ['spawn-precommit-off' off-ok]
        ['spawn-suite-c' %.n]
    ==
  ::  who must be the fingerprint of the key (the @p <-> key binding),
  ::  and the key must encode the tweak (the key <-> satpoint binding).
  =/  fig-ok=?    =(who.sat fig:ex:cac)
  =/  tweak-ok=?  =(dat.tw.pub:+<:cac tweak)
  :_  ?:(&(spkh-ok off-ok fig-ok tweak-ok) `psat ~)
  :~  ['spawn-precommit-spkh' spkh-ok]
      ['spawn-precommit-off' off-ok]
      ['spawn-suite-c' %.y]
      ['spawn-fig' fig-ok]
      ['spawn-key-tweak' tweak-ok]
  ==
::
::  +apply-commit: prove the commit tx spends the precommit satpoint (so the
::  two share a controller -- the ownership leg of urb-core's %spawn proof the
::  prototype was missing) and compute where the sat lands in the commit's
::  outputs. `prior` is the summed value of the commit's inputs before the
::  precommit-spending one (~ if unfetchable).
::
++  apply-commit
  |=  $:  commit=tx:bc
          psat=sont:ord
          pre-out-value=@ud
          prior=(unit @ud)
      ==
  ^-  [checks=(list check:sa) csat=(unit [vout=@ud off=@ud])]
  =/  spends=?
    %+  lien  is.commit
    |=  i=inputw:tx:bitcoin
    =([txid.i pos.i] [txid.psat vout.psat])
  ?.  &(spends (lth off.psat pre-out-value))
    [~[['spawn-commit-spends-precommit' %.n]] ~]
  ?~  prior
    [~[['spawn-commit-spends-precommit' %.y] ['spawn-commit-prior-values' %.n]] ~]
  =/  landing  (index-to-sont:uc (add u.prior off.psat) os.commit)
  ?~  landing
    [~[['spawn-commit-spends-precommit' %.y] ['spawn-commit-sat-landed' %.n]] ~]
  :-  ~[['spawn-commit-spends-precommit' %.y] ['spawn-commit-sat-landed' %.y]]
  `[vout.landing off.landing]
::
::  +link-checks: verify one link of the chain. `carried` is the sat's
::  [vout off] location within prev's outputs; `prior` is the summed value of
::  this tx's inputs before in.link. Returns the sat's landing in this tx.
::
++  link-checks
  |=  $:  who=@p
          idx=@ud
          =link:sa
          prev=tx:bc
          this=tx:bc
          carried=[vout=@ud off=@ud]
          prior=(unit @ud)
      ==
  ^-  [checks=(list check:sa) next=(unit [vout=@ud off=@ud])]
  =/  inp  (snag-input in.link this)
  ?~  inp  [~[[(nom idx 'input-index') %.n]] ~]
  ::  Continuity: this input must spend the exact sat-carrying output of prev
  ::  (full outpoint -- spending some OTHER output of prev is a fork attempt).
  =/  cont=?  =([txid.u.inp pos.u.inp] [id.prev vout.carried])
  ?.  (lth pos.u.inp (lent os.prev))
    [~[[(nom idx 'continuity') cont] [(nom idx 'prevout-range') %.n]] ~]
  =/  spent=output:tx:bitcoin  (snag pos.u.inp os.prev)
  ::  The carried offset must sit inside the spent output (is-sont-in-input).
  =/  off-ok=?  (lth off.carried value.spent)
  ::  Commitment: reconstruct the taproot output key from the off-chain reveal
  ::  (internal key + single leaf) and match the spent output's scriptPubKey.
  =/  recomputed=@ux  (out-key reveal.link)
  =/  onchain=(unit @ux)  (p2tr-xonly script-pubkey.spent)
  =/  commit-ok=?  &(?=(^ onchain) =(u.onchain recomputed))
  ::  Confidential: the spend must be key-path (single-sig witness).
  =/  keypath-ok=?  (is-key-path witness.u.inp)
  ::  No gaps: every link carries at least one sotx (%no-op for plain moves).
  =/  nonempty=?  ?=(^ sots.link)
  ::  Self-enacted: every sotx in the chain is from who.
  =/  ship-ok=?
    %+  levy  sots.link
    |=(r=raw-sotx:urb =(who ship.sot.r))
  ::  The committed leaf must re-parse to exactly the claimed sotx(es).
  =/  sots-ok=?
    =/  parsed  (parse-leaf script.tapleaf.reveal.link)
    ?~  parsed  %.n
    =((turn u.parsed get-sotx) (turn sots.link get-sotx))
  ::  Track the sat to its deterministic landing output in this tx.
  ::  ~ means the prior input values were unfetchable, or the sat fell into
  ::  the miner fee -- either way the single sat-thread cannot continue.
  =/  landed=(unit [vout=@ud off=@ud])
    ?~  prior  ~
    =/  l  (index-to-sont:uc (add u.prior off.carried) os.this)
    ?~(l ~ `l)
  :_  landed
  :~  [(nom idx 'continuity') cont]
      [(nom idx 'off-range') off-ok]
      [(nom idx 'commitment') commit-ok]
      [(nom idx 'key-path') keypath-ok]
      [(nom idx 'sots-nonempty') nonempty]
      [(nom idx 'sots-ship') ship-ok]
      [(nom idx 'sots-match') sots-ok]
      [(nom idx 'sat-landed') ?=(^ landed)]
  ==
::
::  +replay-chain: fold the chain's sotxes into a point:urb. A subject-only
::  mirror of process-unv in lib/urb-core: sotxes from ships other than who
::  are skipped (the 'sots-ship' check guarantees none on an ok verdict), and
::  ops targeting OTHER ships are skipped rather than mutating their points.
::  Returns ~ on structural failure (no spawn / double spawn / op before
::  spawn); per-raw-sotx precondition failures use process-unv's batch-abort
::  semantics (drop the rest of that raw-sotx's singles, keep state).
::
++  replay-chain
  |=  [who=@p chain=(list link:sa) tip=sont:ord]
  ^-  (unit point:urb)
  =|  pnt=(unit point:urb)
  =/  links  chain
  |-
  ^-  (unit point:urb)
  ?~  links
    ?~  pnt  ~
    `u.pnt(sont.own tip)
  =/  sots  sots.i.links
  |-
  ^-  (unit point:urb)
  ?~  sots
    ^$(links t.links)
  ?.  =(who ship.sot.i.sots)
    $(sots t.sots)
  =/  res  (replay-singles who pnt (singles ~[i.sots]))
  ?~  res  ~
  $(sots t.sots, pnt u.res)
::
::  +replay-singles: process one raw-sotx's flattened singles.
::  Outer unit: ~ = hard structural failure (whole replay invalid).
::  Inner unit: the (possibly absent) point state.
::
++  replay-singles
  |=  $:  who=@p
          pnt=(unit point:urb)
          sx=(list [=ship sig=(unit @) =single:skim-sotx:urb])
      ==
  ^-  (unit (unit point:urb))
  |-
  ?~  sx  `pnt
  =*  s  single.i.sx
  ?:  ?=(%spawn -.s)
    ?^  pnt  ~                    ::  double spawn: hard failure
    =/  new=point:urb
      :*  own=[[0x0 0 0] ~]      ::  placeholder; set to tip at the end
          rift=0
          life=1
          pass=pass.s
          sponsor=[%.n who]
          escape=~
          fief=fief.s
      ==
    $(sx t.sx, pnt `new)
  ?~  pnt  ~                      ::  op before spawn: hard failure
  =/  p=point:urb  u.pnt
  ?-    -.s
      %no-op
    $(sx t.sx)
  ::
      %keys
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
      ::  XX the sponsor's signature is accepted unverified: off-chain we
      ::     have neither the block-height context for the signed message
      ::     nor (subject-only) the parent's stored pass to mirror
      ::     urb-core's veri-octs check.
      $(sx t.sx, pnt `p(sponsor.net [%.y parent.s], escape.net ~))
    $(sx t.sx, pnt `p(escape.net `parent.s))
  ::
      %cancel-escape
    ?.  =(`parent.s escape.net.p)  `pnt      ::  batch-abort
    $(sx t.sx, pnt `p(escape.net ~))
  ::
      %adopt
    ?.  =(ship.s who)  $(sx t.sx)            ::  XX subject-only: skip
    $(sx t.sx, pnt `p(sponsor.net [%.y who], escape.net ~))
  ::
      %reject
    ?.  =(ship.s who)  $(sx t.sx)            ::  XX subject-only: skip
    ?.  =(`who escape.net.p)  `pnt           ::  batch-abort
    $(sx t.sx, pnt `p(escape.net ~))
  ::
      %detach
    ?.  =(ship.s who)  $(sx t.sx)            ::  XX subject-only: skip
    ?.  =([%.y who] sponsor.net.p)  `pnt     ::  batch-abort
    $(sx t.sx, pnt `p(sponsor.net [%.n who]))
  ::
      %set-mang
    ::  XX urb-core crashes (!!) on %set-mang, so there is no on-chain
    ::     behavior to mirror; skip rather than crash.
    $(sx t.sx)
  ==
::
::  +run-checks: the pure verifier. Walks the chain threading the sat's
::  location and accumulating named checks; assembles the final result.
::
++  run-checks
  |=  $:  sat=self-attestation:sa
          txl=(list tx:bc)              ::  one per link, in order
          commit=tx:bc
          pre=tx:bc
          tip-unspent=(unit ?)
          tracked=(unit sont:ord)
          commit-prior=(unit @ud)
          link-priors=(list (unit @ud))
      ==
  ^-  result:sa
  =*  who  who.sat
  ::  Genesis %spawn / precommit / tweak / fig checks.
  =/  [checks=(list check:sa) psat=(unit sont:ord)]  (check-spawn sat pre)
  ?~  psat
    [[who %.n checks] ~ 0]
  ::  The commit tx must spend the precommit satpoint; find the sat's landing.
  =/  pre-out-value=@ud
    ?.  (lth vout.u.psat (lent os.pre))  0
    value:(snag vout.u.psat os.pre)
  =/  [commit-checks=(list check:sa) csat=(unit [vout=@ud off=@ud])]
    (apply-commit commit u.psat pre-out-value commit-prior)
  =.  checks  (weld checks commit-checks)
  ?~  csat
    [[who %.n checks] ~ 0]
  ::  The %spawn must head link 0 and appear nowhere else.
  =.  checks  (snoc checks ['spawn-first' (spawn-first-ok chain.sat)])
  ::  Walk the chain, threading the sat's [vout off] location and collecting
  ::  each link's entering sont for the tracked-tip reconciliation.
  =/  links  chain.sat
  =/  priors  link-priors
  =/  idx=@ud  0
  =/  prev=tx:bc  commit
  =/  carried=[vout=@ud off=@ud]  u.csat
  =|  entering=(list sont:ord)
  |-
  ^-  result:sa
  ?~  links
    ::  Tip: the sat's final landing must be exactly the claimed tip
    ::  (txid AND vout AND off), and it must be currently unspent.
    =/  tip-sont=sont:ord  [id.prev vout.carried off.carried]
    =.  checks
      %+  weld  checks
      :~  ['tip-sont' =(tip.sat tip-sont)]
          ['tip-unspent' =([~ %.y] tip-unspent)]
      ==
    =?  checks  ?=(^ tracked)
      (snoc checks ['tracked-tip' (tracked-ok u.tracked tip.sat (flop entering))])
    =/  ok  (levy checks |=(c=check:sa ok.c))
    :+  [who ok checks]
      ?.(ok ~ (replay-chain who chain.sat tip.sat))
    ?.  ok  0
    ?.  (lth vout.carried (lent os.prev))  0
    value:(snag vout.carried os.prev)
  =/  this-tx=tx:bc  (snag idx txl)
  =/  prior=(unit @ud)  ?~(priors ~ i.priors)
  =/  [lchecks=(list check:sa) next=(unit [vout=@ud off=@ud])]
    (link-checks who idx i.links prev this-tx carried prior)
  =.  checks  (weld checks lchecks)
  ?~  next
    [[who %.n checks] ~ 0]
  %=  $
    links     t.links
    priors    ?~(priors ~ t.priors)
    idx       +(idx)
    prev      this-tx
    carried   u.next
    entering  [[id.prev vout.carried off.carried] entering]
  ==
::
::  +fail-result: a result for early structural/fetch failures.
::
++  fail-result
  |=  [who=@p name=cord]
  ^-  result:sa
  [[who %.n ~[[name %.n]]] ~ 0]
::
::  +fetch-txs: fetch every link's transaction, preserving order.
::
++  fetch-txs
  |=  [rpc=req-to:btcio links=(list link:sa)]
  =/  m  (strand:strandio ,(list (unit tx:bc)))
  ^-  form:m
  =|  acc=(list (unit tx:bc))
  |-  ^-  form:m
  ?~  links  (pure:m (flop acc))
  ;<  t=(unit tx:bc)  bind:m
    (get-raw-transaction:btcio rpc ~ txid.i.links)
  $(links t.links, acc [t acc])
::
::  +prior-sum: sum the prevout values of inputs 0..stop-1 of a tx, fetching
::  prev txs cache-first. Exact sat tracking needs only the values of inputs
::  PRECEDING the chain-spending input, so when stop=0 (the comet input first,
::  the common wallet case) this makes zero RPC calls. ~ stop or any fetch
::  failure yields a ~ sum (the caller's 'sat-landed' check then fails).
::
++  prior-sum
  |=  $:  rpc=req-to:btcio
          cache=(map txid:ord tx:bc)
          =tx:bc
          stop=(unit @ud)
      ==
  =/  m  (strand:strandio ,[(unit @ud) (map txid:ord tx:bc)])
  ^-  form:m
  ?~  stop  (pure:m [~ cache])
  =/  inputs  (scag u.stop is.tx)
  =|  sum=@ud
  |-
  ^-  form:m
  ?~  inputs  (pure:m [`sum cache])
  =/  hit  (~(get by cache) txid.i.inputs)
  ?^  hit
    ?.  (lth pos.i.inputs (lent os.u.hit))  (pure:m [~ cache])
    $(inputs t.inputs, sum (add sum value:(snag pos.i.inputs os.u.hit)))
  ;<  got=(unit tx:bc)  bind:m
    (get-raw-transaction:btcio rpc ~ txid.i.inputs)
  ?~  got  (pure:m [~ cache])
  ?.  (lth pos.i.inputs (lent os.u.got))  (pure:m [~ cache])
  %=  $
    inputs  t.inputs
    cache   (~(put by cache) txid.i.inputs u.got)
    sum     (add sum value:(snag pos.i.inputs os.u.got))
  ==
::
::  +verify: the entry point. Fetch everything from the node, then run the
::  pure verifier. `tracked` is the sont the caller already tracks for this
::  ship, if any. Produces a vase of result:sa.
::
++  verify
  |=  [rpc=req-to:btcio sat=self-attestation:sa tracked=(unit sont:ord)]
  ^-  shed:khan
  =/  m  (strand:strandio ,vase)
  ^-  form:m
  =*  who  who.sat
  ?~  chain.sat
    (pure:m !>((fail-result who 'empty-chain')))
  ;<  txs=(list (unit tx:bc))  bind:m
    (fetch-txs rpc chain.sat)
  ?:  (lien txs |=(t=(unit tx:bc) ?=(~ t)))
    (pure:m !>((fail-result who 'fetch:chain-tx')))
  =/  txl=(list tx:bc)  (turn txs need)
  ::  The genesis link spends the spawn COMMIT tx; learn its txid from the
  ::  genesis input's prevout, then fetch it and the precommit tx.
  =/  genesis=tx:bc  (snag 0 txl)
  =/  gin  (snag-input in.i.chain.sat genesis)
  ?~  gin
    (pure:m !>((fail-result who 'genesis:input-index')))
  ;<  commit=(unit tx:bc)  bind:m
    (get-raw-transaction:btcio rpc ~ txid.u.gin)
  ?~  commit
    (pure:m !>((fail-result who 'fetch:commit-tx')))
  ;<  pre=(unit tx:bc)  bind:m
    (get-raw-transaction:btcio rpc ~ precommit.sat)
  ?~  pre
    (pure:m !>((fail-result who 'fetch:precommit-tx')))
  ;<  tip-unspent=(unit ?)  bind:m
    (get-tx-out:btcio rpc ~ txid.tip.sat vout.tip.sat)
  ::  Seed the prevout cache with everything already fetched.
  =/  cache=(map txid:ord tx:bc)
    %-  ~(gas by *(map txid:ord tx:bc))
    :*  [id.u.commit u.commit]
        [id.u.pre u.pre]
        (turn txl |=(t=tx:bc [id.t t]))
    ==
  ::  Locate the commit input that spends the attested precommit output
  ::  (~ on structural failure; apply-commit then fails cleanly).
  =/  commit-stop=(unit @ud)
    ?~  sots.i.chain.sat  ~
    ?~  spawn=(find-spawn +.sot.i.sots.i.chain.sat)  ~
    ?~  vout.to.u.spawn  ~
    =/  want  [precommit.sat u.vout.to.u.spawn]
    =/  inputs  is.u.commit
    =|  i=@ud
    |-  ^-  (unit @ud)
    ?~  inputs  ~
    ?:  =([txid.i.inputs pos.i.inputs] want)  `i
    $(inputs t.inputs, i +(i))
  ;<  [commit-prior=(unit @ud) cache2=_cache]  bind:m
    (prior-sum rpc cache u.commit commit-stop)
  ::  Per-link prior sums (inputs before in.link of each link tx).
  ::  The cache is threaded as an explicit loop variable: a ;< binding would
  ::  be reset by the trap recursion and lose updates between iterations.
  ::  (links is re-widened to list: the ?~ above narrowed chain.sat to lest,
  ::  which would make the loop's ?~ mint-vain.)
  =/  links=(list link:sa)  chain.sat
  =/  ltxs  txl
  =/  cash  cache2
  =|  acc=(list (unit @ud))
  |-
  ^-  form:m
  ?~  links
    =/  =result:sa
      %:  run-checks
          sat  txl  u.commit  u.pre
          tip-unspent  tracked
          commit-prior  (flop acc)
      ==
    (pure:m !>(result))
  ?~  ltxs  !!
  ;<  [p=(unit @ud) next-cash=_cache]  bind:m
    (prior-sum rpc cash i.ltxs `in.i.links)
  $(links t.links, ltxs t.ltxs, acc [p acc], cash next-cash)
--
