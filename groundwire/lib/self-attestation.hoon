::  lib/self-attestation.hoon
::
::  Verification of CONFIDENTIAL COMET self-attestation packets (protocol 2.0).
::  See sur/self-attestation for the packet format and protocol rules.
::
::  %urb-watcher calls ++verify in a khan thread. The strand fetches every
::  referenced transaction from the Bitcoin node, each with its attested
::  blockhash so the node needs no -txindex (the node is trusted, the same
::  trust model as %urb-watcher's block processing), and then runs the pure
::  verifier ++run-checks, which establishes:
::
::    - genesis: link 0 is the spawn COMMIT transaction. The %spawn's
::      precommit output exists with the attested scriptPubKey hash, the
::      comet's suite-C networking key encodes the tweak binding it to that
::      precommit satpoint, who = fig of the key, and link 0's input 0
::      actually SPENDS the precommit satpoint (so precommit and commit share
::      a controller, mirroring lib/urb-core's %spawn proof);
::    - per link: a link IS a commit transaction. Its input 0 (the
::      sat-carrying input, by stipulation) key-path-spends the exact
::      sat-carrying output of the previous link (full outpoint, not just
::      txid); the link's OWN sat-carrying output's taproot key equals Q
::      recomputed from the off-chain reveal (single-leaf commitment); the
::      leaf re-parses to exactly the claimed self-enacted sotx(es); and the
::      sat's offset is tracked deterministically through the outputs --
::      input 0 means no input value precedes the sat, so it lands at output
::      index = its entering offset (index-to-sont math, so one genesis
::      cannot fork into two divergent chains at a multi-output tx);
::    - tip: the sat's final landing equals the claimed tip (txid AND vout AND
::      off) and that output is currently unspent; if the caller already tracks
::      a sont for this ship, the packet must reconcile with it.
::
::  Because each link discloses its OWN output's leaf, the latest sotx (the
::  tip leaf's) is enacted directly at packet-verification time -- no lag,
::  and a bare %spawn is a valid one-link keyfile. An on-chain script-path
::  reveal of the same leaf is what enacts it for chain watchers if the ship
::  later goes public.
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
::  +from-skeleton: build a full self-attestation from an off-ship skeleton,
::  re-deriving each link's sots from its leaf script (so they match the leaf
::  by construction). ~ if any leaf is unparsable.
::
++  from-skeleton
  |=  =skeleton:sa
  ^-  (unit self-attestation:sa)
  %-  mole
  |.
  ^-  self-attestation:sa
  =/  chain=(list link:sa)
    %+  turn  links.skeleton
    |=  sl=skel-link:sa
    ^-  link:sa
    =/  parsed  (need (parse-leaf script.sl))
    [txid.sl block.sl [internal-key.sl [version.sl script.sl]] parsed]
  [who.skeleton funding.skeleton chain tip.skeleton]
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
::    XX KNOWN GAP (audit P3, tracked-ok-replay-bearer): there is no tip-HEIGHT
::    monotonicity bound. An interior (older) sont of the SAME true chain passes
::    `entering`, so a re-attestation that rolls the ship back to a superseded
::    key/sponsor is blocked only by the tip-unspent check. To make rollback
::    impossible independent of tip-unspent: store the tip height alongside the
::    tracked sont (app/urb-watcher-2 state) and require an accepted
::    re-attestation's tip height >= the stored one (thread it in here).
::
++  tracked-ok
  |=  [tracked=sont:ord tip=sont:ord entering=(list sont:ord)]
  ^-  ?
  ?|  =(tracked tip)
      (lien entering |=(s=sont:ord =(s tracked)))
  ==
::
::  +check-spawn: the genesis %spawn / precommit / tweak proof, mirroring
::  lib/urb-core's on-chain %spawn handling. `link0` is the spawn COMMIT
::  transaction (the chain's first link): its input 0 must spend the attested
::  precommit satpoint (shared controller -- the ownership leg of urb-core's
::  %spawn proof). Returns the sat's landing within link 0's own outputs when
::  every check passes (input 0 means the sat enters at its precommit offset).
::
++  check-spawn
  |=  [sat=self-attestation:sa pre=tx:bc link0=tx:bc]
  ^-  [checks=(list check:sa) carried=(unit [vout=@ud off=@ud])]
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
  =/  psat=sont:ord  [txid.funding.sat u.vout.to.sp off.to.sp]
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
  ::  The spawn commit must SPEND the precommit satpoint at input 0 (the
  ::  sat-carrying input, by stipulation), and the claimed offset must sit
  ::  inside the spent precommit output.
  =/  inp  (snag-input 0 link0)
  =/  spends-ok=?
    ?~  inp  %.n
    ?&  =([txid.u.inp pos.u.inp] [txid.psat vout.psat])
        (lth off.psat value.out)
    ==
  ::  The sat enters link 0 through input 0, so no input value precedes it:
  ::  it lands at output index = its offset within the precommit output.
  ::  ~ means it fell into the miner fee -- the sat-thread cannot start.
  =/  landing  (index-to-sont:uc off.psat os.link0)
  =/  carried=(unit [vout=@ud off=@ud])
    ?~  landing  ~
    `[vout.landing off.landing]
  :_  ?:(&(spkh-ok off-ok fig-ok tweak-ok spends-ok) carried ~)
  :~  ['spawn-precommit-spkh' spkh-ok]
      ['spawn-precommit-off' off-ok]
      ['spawn-suite-c' %.y]
      ['spawn-fig' fig-ok]
      ['spawn-key-tweak' tweak-ok]
      ['spawn-spends-precommit' spends-ok]
      ['spawn-sat-landed' ?=(^ carried)]
  ==
::
::  +own-output-checks: the checks every link runs against ITS OWN
::  sat-carrying output, at the sat's landing [vout off] within this tx:
::  the off-chain reveal must recompute to the on-chain taproot key at the
::  landing vout, and the disclosed sots must be nonempty, self-enacted,
::  and exactly what the committed leaf encodes.
::
++  own-output-checks
  |=  [who=@p idx=@ud =link:sa this=tx:bc landing=[vout=@ud off=@ud]]
  ^-  (list check:sa)
  ::  Commitment: reconstruct the taproot output key from the off-chain reveal
  ::  (internal key + single leaf) and match this tx's output at the landing.
  =/  recomputed=@ux  (out-key reveal.link)
  =/  onchain=(unit @ux)
    ?.  (lth vout.landing (lent os.this))  ~
    =/  out=output:tx:bitcoin  (snag vout.landing os.this)
    (p2tr-xonly script-pubkey.out)
  =/  commit-ok=?  &(?=(^ onchain) =(u.onchain recomputed))
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
  :~  [(nom idx 'commitment') commit-ok]
      [(nom idx 'sots-nonempty') nonempty]
      [(nom idx 'sots-ship') ship-ok]
      [(nom idx 'sots-match') sots-ok]
  ==
::
::  +link-checks: verify one link of the chain. For a non-genesis link,
::  `carried` is the sat's [vout off] location within prev's outputs: input 0
::  of this tx must key-path-spend that exact outpoint, and the sat lands in
::  this tx's outputs at index = off.carried (input 0 carries no preceding
::  input value). For the genesis link (`genesis`), check-spawn already
::  proved the precommit spend (a plain wallet spend, with no witness-shape
::  constraint) and computed the landing; `carried` IS the landing within
::  this tx, so only the own-output checks remain. Returns the sat's landing.
::
++  link-checks
  |=  $:  who=@p
          idx=@ud
          =link:sa
          prev=tx:bc
          this=tx:bc
          carried=[vout=@ud off=@ud]
          genesis=?
      ==
  ^-  [checks=(list check:sa) next=(unit [vout=@ud off=@ud])]
  ?:  genesis
    [(own-output-checks who idx link this carried) `carried]
  ::  The sat-carrying input is input 0, by stipulation.
  =/  inp  (snag-input 0 this)
  ?~  inp  [~[[(nom idx 'input-zero') %.n]] ~]
  ::  Continuity: input 0 must spend the exact sat-carrying output of prev
  ::  (full outpoint -- spending some OTHER output of prev is a fork attempt).
  =/  cont=?  =([txid.u.inp pos.u.inp] [id.prev vout.carried])
  ?.  (lth pos.u.inp (lent os.prev))
    [~[[(nom idx 'continuity') cont] [(nom idx 'prevout-range') %.n]] ~]
  =/  spent=output:tx:bitcoin  (snag pos.u.inp os.prev)
  ::  The carried offset must sit inside the spent output (is-sont-in-input).
  =/  off-ok=?  (lth off.carried value.spent)
  ::  Confidential: the spend must be key-path (single-sig witness).
  =/  keypath-ok=?  (is-key-path witness.u.inp)
  ::  Track the sat to its deterministic landing output in this tx: input 0
  ::  means no input value precedes it, so it enters at index off.carried.
  ::  ~ means it fell into the miner fee -- the sat-thread cannot continue.
  =/  landed  (index-to-sont:uc off.carried os.this)
  =/  base=(list check:sa)
    :~  [(nom idx 'continuity') cont]
        [(nom idx 'off-range') off-ok]
        [(nom idx 'key-path') keypath-ok]
        [(nom idx 'sat-landed') ?=(^ landed)]
    ==
  ?~  landed  [base ~]
  =/  next=[vout=@ud off=@ud]  [vout.landed off.landed]
  [(weld base (own-output-checks who idx link this next)) `next]
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
  |=  $:  who=@p  chain=(list link:sa)  tip=sont:ord
          sponsors=(map @p point:urb)  heights=(map @ux @ud)
      ==
  ^-  (unit point:urb)
  =|  pnt=(unit point:urb)
  =/  links  chain
  |-
  ^-  (unit point:urb)
  ?~  links
    ?~  pnt  ~
    `u.pnt(sont.own tip)
  ::  the height of THIS link's block, anchoring any escape-sig freshness
  ::  window (0 when unknown -- e.g. the RPC path passes no heights, so a
  ::  signed escape then fails the window and drops, never forging a sponsor).
  =/  lh=@ud  (~(gut by heights) block.i.links 0)
  =/  sots  sots.i.links
  |-
  ^-  (unit point:urb)
  ?~  sots
    ^$(links t.links)
  ?.  =(who ship.sot.i.sots)
    $(sots t.sots)
  =/  res  (replay-singles who sponsors lh pnt (singles ~[i.sots]))
  ?~  res  ~
  $(sots t.sots, pnt u.res)
::
::  +replay-singles: process one raw-sotx's flattened singles.
::  Outer unit: ~ = hard structural failure (whole replay invalid).
::  Inner unit: the (possibly absent) point state.
::
++  replay-singles
  |=  $:  who=@p
          sponsors=(map @p point:urb)   ::  known points, for escape-sig verification
          lh=@ud                        ::  this link's block height (escape-sig window)
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
      ::  Signed escape: the sponsor must have consented. Mirror urb-core
      ::  (urb-core.hoon:500-521): look up the candidate sponsor's stored
      ::  pass and require veri-octs of the sig over (shaz (jam [who h])) for
      ::  some h in [lh-10, lh+1] (the escape link's block height +/- slack).
      ::  Unknown sponsor or a sig that verifies for no h -> drop the escape
      ::  with NO state change (never write an unverified sponsor), exactly as
      ::  urb-core does. (An unsigned escape falls through to %pending below.)
      =/  sponsor  (~(get by sponsors) parent.s)
      ?~  sponsor  $(sx t.sx)
      =/  cac  (com:nu:cric:crypto pass.net.u.sponsor)
      =/  lo=@ud  ?:((lth lh 10) 0 (sub lh 10))
      ?.  %+  lien  (gulf lo +(lh))
          |=  h=@ud
          (veri-octs:ed:crypto u.sig.s 512^(shaz (jam [who h])) sgn:ded:ex:cac)
        $(sx t.sx)
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
          pre=tx:bc
          tip-unspent=(unit ?)
          tracked=(unit sont:ord)
          sponsors=(map @p point:urb)   ::  known points, for escape-sig verification
          heights=(map @ux @ud)         ::  link block-hash -> height (escape-sig window)
      ==
  ^-  result:sa
  =*  who  who.sat
  ::  Genesis %spawn / precommit / tweak / fig checks, plus the sat's
  ::  landing within link 0 (the spawn commit) itself.
  ?~  txl
    [[who %.n ~[['spawn-no-chain' %.n]]] ~ 0]
  =/  [checks=(list check:sa) spawn-carried=(unit [vout=@ud off=@ud])]
    (check-spawn sat pre i.txl)
  ?~  spawn-carried
    [[who %.n checks] ~ 0]
  ::  The %spawn must head link 0 and appear nowhere else.
  =.  checks  (snoc checks ['spawn-first' (spawn-first-ok chain.sat)])
  ::  Walk the chain, threading the sat's [vout off] location: for the
  ::  genesis link `carried` is its landing within link 0 itself (computed
  ::  by check-spawn); thereafter it is the entering location within prev's
  ::  outputs. Each link's own landing sont except the last is collected
  ::  for the tracked-tip reconciliation.
  =/  links=(list link:sa)  chain.sat
  =/  txs=(list tx:bc)  txl
  =/  idx=@ud  0
  =/  prev=tx:bc  pre               ::  unused by the genesis link
  =/  carried=[vout=@ud off=@ud]  u.spawn-carried
  =|  landings=(list sont:ord)      ::  newest first; head is the tip's
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
      =/  inner=(list sont:ord)  ?~(landings ~ (flop t.landings))
      (snoc checks ['tracked-tip' (tracked-ok u.tracked tip.sat inner)])
    =/  ok  (levy checks |=(c=check:sa ok.c))
    :+  [who ok checks]
      ?.(ok ~ (replay-chain who chain.sat tip.sat sponsors heights))
    ?.  ok  0
    ?.  (lth vout.carried (lent os.prev))  0
    value:(snag vout.carried os.prev)
  ?~  txs  !!                       ::  one tx per link, by construction
  =/  this-tx=tx:bc  i.txs
  =/  [lchecks=(list check:sa) next=(unit [vout=@ud off=@ud])]
    (link-checks who idx i.links prev this-tx carried =(0 idx))
  =.  checks  (weld checks lchecks)
  ?~  next
    [[who %.n checks] ~ 0]
  %=  $
    links     t.links
    txs       t.txs
    idx       +(idx)
    prev      this-tx
    carried   u.next
    landings  [[id.this-tx vout.u.next off.u.next] landings]
  ==
::
::  +fail-result: a result for early structural/fetch failures.
::
++  fail-result
  |=  [who=@p name=cord]
  ^-  result:sa
  [[who %.n ~[[name %.n]]] ~ 0]
::
::  +fetch-txs: fetch every link's transaction, preserving order. Each tx is
::  fetched with its attested blockhash, so the node needs no -txindex (Core
::  validates inclusion; a lying blockhash simply fails the fetch).
::
++  fetch-txs
  |=  [rpc=req-to:btcio links=(list link:sa)]
  =/  m  (strand:strandio ,(list (unit tx:bc)))
  ^-  form:m
  =|  acc=(list (unit tx:bc))
  |-  ^-  form:m
  ?~  links  (pure:m (flop acc))
  ;<  t=(unit tx:bc)  bind:m
    (get-raw-transaction-in-block:btcio rpc ~ txid.i.links block.i.links)
  $(links t.links, acc [t acc])
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
  ;<  pre=(unit tx:bc)  bind:m
    %:  get-raw-transaction-in-block:btcio
        rpc  ~  txid.funding.sat  block.funding.sat
    ==
  ?~  pre
    (pure:m !>((fail-result who 'fetch:precommit-tx')))
  ;<  txs=(list (unit tx:bc))  bind:m
    (fetch-txs rpc chain.sat)
  ?:  (lien txs |=(t=(unit tx:bc) ?=(~ t)))
    (pure:m !>((fail-result who 'fetch:chain-tx')))
  =/  txl=(list tx:bc)  (turn txs need)
  ;<  tip-unspent=(unit ?)  bind:m
    (get-tx-out:btcio rpc ~ txid.tip.sat vout.tip.sat)
  ::  The RPC entry passes no sponsor points or link heights, so a signed
  ::  escape here drops (conservative -- never an unverified sponsor) and an
  ::  unsigned one goes pending. The %light-client path (lib/lc-attestation
  ::  +verify-lc) supplies both and performs the full escape-sig verification.
  (pure:m !>((run-checks sat txl u.pre tip-unspent tracked ~ ~)))
--
