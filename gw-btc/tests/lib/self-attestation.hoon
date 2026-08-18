::  tests/lib/self-attestation.hoon
::
::  Deterministic current-protocol (kelvin-9) vectors for
::  lib/self-attestation: a synthetic custody chain in the OP_RETURN /
::  snapshot model.  No Bitcoin node or %light-client is involved --
::  fetched transactions and final UTXO status enter at the same pure
::  boundary +run-checks exposes (the same one +verify-lc drives).
::
::  A valid chain: a start (funding) tx whose output 0 carries the spawn
::  sat; entry 0 spends it and creates a sat-carrying output whose P2TR
::  key is 5120 || state-key(internal-key, snapshot); entry 1 is a
::  key-path custody move.  Real secp is used via cric/taproot, exactly
::  as tests/lib/gw-btc-pass does.
::
/-  bitcoin, ord, urb, sa=self-attestation
/+  *test, sal=self-attestation, cc=gw-btc-pass, tr=taproot, bc=bitcoin
=>
|%
++  secp  secp256k1:secp:crypto
::  +mk-ikey: a compressed secp internal key from a scalar
::
++  mk-ikey
  |=  k=@
  ^-  @ux
  %-  compress-point:secp
  (mul-point-scalar:secp g:domain:curve:secp k)
::  +p2tr-spk: a P2TR scriptPubKey (OP_1 PUSH32 q)
::
++  p2tr-spk
  |=  q=@ux
  ^-  hexb:bitcoin
  [34 `@ux`(can 3 ~[[32 q] [2 0x5120]])]
::  +state-out: the sat-carrying output committing .snap under .ikey
::
++  state-out
  |=  [ikey=@ux snap=snapshot:sa value=@ud]
  ^-  output:tx:bitcoin
  [(p2tr-spk (state-key:cc ikey snap)) value]
::
++  keypath-wit  `(list hexb:bitcoin)`~[[64 0x0]]
::
++  mk-input
  |=  [=txid:ord pos=@ud wit=(list hexb:bitcoin)]
  ^-  inputw:tx:bitcoin
  [wit txid pos [4 0xffff.ffff] ~ ~]
::
++  mk-tx
  |=  [id=@ux is=(list inputw:tx:bitcoin) os=(list output:tx:bitcoin)]
  ^-  tx:bc
  [id is os 0 1 ~]
::
++  got-check
  |=  [v=verdict:sa name=cord]
  ^-  ?
  =/  cs  checks.v
  |-
  ?~  cs  %.n
  ?:  =(name.i.cs name)  ok.i.cs
  $(cs t.cs)
::  +has-check: was this check EMITTED at all?
::
::    +got-check answers %.n both for "failed" and "never ran", which is
::    fine when the check is unconditional and useless when it is not.
::    `tracked-lag' is emitted only for a log that really is an older copy
::    of ours, so "absent" and "failing" are different claims and the
::    tests have to be able to say which one they mean.
::
++  has-check
  |=  [v=verdict:sa name=cord]
  ^-  ?
  (lien checks.v |=(c=check:sa =(name.c name)))
::
++  no-points  *(set ship)
::  --------------------------------------------------------------------
::  Identity: spawn satpoint -> dat -> suite-C pass; who = fig(pass).
::  The messaging key (cry.pub) is independent of the mutable xtr, so it
::  is computed once and pinned into every committed snapshot.
::  --------------------------------------------------------------------
++  seed       'gw-self-attestation-test'
++  start-id   0x1a1a.1a1a
++  c0-id      0x2b2b.2b2b
++  c1-id      0x3c3c.3c3c
++  spawn      ^-(sont:ord [start-id 0 0])
++  dat        (make-dat:cc spawn)
::
++  base-pass  pub:ex:(pit:nu:cric:crypto 512 (shaz seed) %c dat 0)
++  who
  ^-  @p
  =/  cic  (com:nu:cric:crypto base-pass)
  `@p`fig:ex:cic
++  cry
  ^-  @
  =/  cic  (com:nu:cric:crypto base-pass)
  ?>  ?=(%c suite.+<.cic)
  `@`cry.pub.+<.cic
::  snapshots pin key=cry so the carried pass's messaging half matches.
::
++  snap0  ^-(snapshot:sa [life=1 rift=0 key=cry sponsor=~ fief=~])
++  snap1  ^-(snapshot:sa [life=2 rift=0 key=cry sponsor=~ fief=~])
::
++  ikey0  (mk-ikey 17)
++  ikey1  (mk-ikey 23)
::  spawn-opening on entry 0 names the sat the pass's dat commits to.
::
++  spawn-open  ^-(spawn-opening:sa [spawn start-height=778.000])
++  open0       ^-(opening:sa [ikey0 snap0 `spawn-open])
::
::  start (funding) tx: output 0 holds the spawn sat.
++  start-out  ^-(output:tx:bitcoin [(p2tr-spk (mk-ikey 5)) 10.000])
++  start-tx   (mk-tx start-id ~[(mk-input 0x9999 0 ~)] ~[start-out])
::
::  entry 0: spends the spawn sat, commits snap0.
++  c0-out  (state-out ikey0 snap0 9.500)
++  c0-tx   (mk-tx c0-id ~[(mk-input start-id 0 ~)] ~[c0-out])
::
::  entry 1: key-path custody move to an ordinary P2TR tip (no opening).
++  tip-out  ^-(output:tx:bitcoin [(p2tr-spk (output-pubkey:tr ikey1 ~)) 9.000])
++  c1-tx    (mk-tx c1-id ~[(mk-input c0-id 0 keypath-wit)] ~[tip-out])
::
++  chain
  ^-  custody-log:sa
  ~[[c0-id 100 `open0] [c1-id 101 ~]]
++  carried-pass
  pub:ex:(pit:nu:cric:crypto 512 (shaz seed) %c dat (jam chain))
++  good-sat  ^-(self-attestation:sa [who carried-pass chain])
::
::  a state-update variant: entry 1 carries an opening advancing to snap1
::  (life 2) into a committed output.
++  open1    ^-(opening:sa [ikey1 snap1 ~])
++  c1s-out  (state-out ikey1 snap1 9.000)
++  c1s-tx   (mk-tx c1-id ~[(mk-input c0-id 0 keypath-wit)] ~[c1s-out])
++  chain-s
  ^-  custody-log:sa
  ~[[c0-id 100 `open0] [c1-id 101 `open1]]
++  pass-s
  pub:ex:(pit:nu:cric:crypto 512 (shaz seed) %c dat (jam chain-s))
++  sat-s  ^-(self-attestation:sa [who pass-s chain-s])
::
::  ---------------------------------------------------------------------
::  a THIRD hop, so a log can be behind by TWO as well as by one
::  ---------------------------------------------------------------------
::
::  The tolerance is one custody-log ENTRY, so the boundary it draws --
::  0 / 1 / 2 -- only exists on a log with at least three of them.
::
++  ikey2     (mk-ikey 29)
++  c2-id     0x4e4e.4e4e
++  tip2-out  ^-(output:tx:bitcoin [(p2tr-spk (output-pubkey:tr ikey2 ~)) 8.500])
++  c2-tx     (mk-tx c2-id ~[(mk-input c1-id 0 keypath-wit)] ~[tip2-out])
++  txl3      ^-((list tx:bc) ~[c0-tx c1-tx c2-tx])
++  chain3
  ^-  custody-log:sa
  ~[[c0-id 100 `open0] [c1-id 101 ~] [c2-id 102 ~]]
++  pass3  pub:ex:(pit:nu:cric:crypto 512 (shaz seed) %c dat (jam chain3))
++  sat3   ^-(self-attestation:sa [who pass3 chain3])
::  the same custody two events earlier: one entry, one fetched tx.
::
++  chain1  ^-(custody-log:sa ~[[c0-id 100 `open0]])
++  pass1   pub:ex:(pit:nu:cric:crypto 512 (shaz seed) %c dat (jam chain1))
++  sat1    ^-(self-attestation:sa [who pass1 chain1])
::
::  ---------------------------------------------------------------------
::  the REORG fixtures: the same hops, re-mined one block over
::  ---------------------------------------------------------------------
::
::  Both height fields move -- the entry's own .height and the
::  .start-height inside entry 0's $spawn-opening -- because a reorg deep
::  enough to move the custody transaction moves the funding transaction
::  too.  Nothing else changes: same txids, same outpoints, same
::  commitments, same spawn sat.
::
++  spawn-open-r  ^-(spawn-opening:sa [spawn start-height=777.000])
++  open0-r       ^-(opening:sa [ikey0 snap0 `spawn-open-r])
++  chain-r
  ^-  custody-log:sa
  ~[[c0-id 90 `open0-r] [c1-id 91 ~]]
++  pass-r  pub:ex:(pit:nu:cric:crypto 512 (shaz seed) %c dat (jam chain-r))
::  a genuinely DIFFERENT entry 0: same shape, another transaction.
::
++  chain-f
  ^-  custody-log:sa
  ~[[0xdead.beef 100 `open0] [c1-id 101 ~]]
++  pass-f  pub:ex:(pit:nu:cric:crypto 512 (shaz seed) %c dat (jam chain-f))
::
::  +anchor-at: the anchor a verifier holds for a comet
::
::    .pas is the pass whose xtr carries the log we already verified; .tip
::    is the satpoint we tracked it to; .lyf is the life we installed.
::
++  anchor-at
  |=  [pas=pass tip=sont:ord lyf=@ud]
  ^-  (unit anchor:sa)
  =/  pt=point:urb  [[tip ~] [0 lyf pas [%.n who] ~ ~] ~]
  `[pt tip]
::  +tip-hax: the block the tip transaction was confirmed in
::
::    The provenance +verify-lc resolves from the light client and hands
::    ++run-checks, which stamps it onto the point as .seen (sur/urb).
::    Nothing is checked against it; it exists to be filtered against a
::    reorg's orphan list.
::
++  tip-hax  ^-(hax:block:bitcoin 0xb10c.b10c)
::
++  run
  |=  [sat=self-attestation:sa txl=(list tx:bc) tracked=(unit anchor:sa)]
  ^-  result:sa
  (run-checks:sal sat start-tx txl `%.y tracked no-points `tip-hax)
::
::  ---------------------------------------------------------------------
::  diagnostics (test 6.7): the operator-facing reports
::  ---------------------------------------------------------------------
::
::  These assert PRESENCE and SHAPE, never prose.  The properties that
::  matter are that a disposition cannot be silent, that its severity is
::  computed from the same value as its cards, and that no two reasons
::  read alike -- all of which survive any rewording.
::
++  has-sub
  |=  [nedl=tape hstk=tape]
  ^-  ?
  ?=(^ (find nedl hstk))
::
++  leaf-tape
  |=  =tank
  ^-  tape
  ?>  ?=(%leaf -.tank)
  p.tank
::
++  head-tape
  |=  =tang
  ^-  tape
  ?>  ?=(^ tang)
  (leaf-tape i.tang)
::
++  all-leaves
  |=  =tang
  ^-  ?
  (levy tang |=(t=tank ?=(%leaf -.t)))
::
::  every member of $writ-drop, with distinguishable numbers where the
::  case carries any.  Pinned against the MOLD by
::  +test-writ-drop-union-has-not-drifted, so a twelfth disposition
::  cannot be added without visiting this list.
::
++  all-writ-drops
  ^-  (list writ-drop:sa)
  :~  [%in-flight ~]
      [%declined ~]
      [%already-public ~]
      [%onboarding ~]
      [%foreign-kelvin ~]
      [%undecodable ~]
      [%empty-log ~]
      [%log-too-long 2.048 1.024]
      [%no-tip ~]
      [%unsynced 900.000]
      [%tip-below-log 900.000 900.100]
  ==
::
++  all-anew-refusals
  ^-  (list anew-refusal:sa)
  :~  [%in-flight 7]
      [%no-log ~]
      [%log-too-long 2.048 1.024]
      [%no-tip ~]
      [%unsynced 900.000]
      [%tip-below-log 900.000 900.100]
      [%no-pass ~]
      [%encode-failed ~]
      [%name-mismatch ~]
  ==
::
::  +mints: does this source compile against this subject type?
::
::    The exhaustiveness probe.  A ?- over a closed union that is missing
::    a case does not compile, which is how a new member is stopped from
::    reaching any branch -- least of all a silent one -- by default.
::
++  mints
  |=  [sub=type src=tape]
  ^-  ?
  ::  NB: bind the +mule product before fishing on it.  `-:(mule ...)`
  ::  re-mints the trap under a subject that has lost its $ arm.
  ::
  =/  r  (mule |.((~(mint ut sub) %noun (ream (crip src)))))
  ?=(%& -.r)
--
|%
::  ---- decode -------------------------------------------------------
++  test-from-xtr
  =/  decoded  (from-xtr:sal who carried-pass)
  ;:  weld
    (expect-eq !>(`(unit self-attestation:sa)``good-sat) !>(decoded))
    ::  the @p must match the pass fingerprint
    ::
    (expect !>(?=(~ (from-xtr:sal ~marzod carried-pass))))
    ::  a foreign pass for a foreign @p decodes to its own chain
    ::
    =/  other  (pit:nu:cric:crypto 512 (shaz 'other') %c dat 0)
    (expect !>(?=(~ (from-xtr:sal who pub:ex:other))))
  ==
::
++  test-derive-tip
  ;:  weld
    %+  expect-eq
      !>  `(unit sont:ord)``[c1-id 0 0]
      !>  (derive-tip:sal spawn start-tx ~[c0-tx c1-tx])
    ::  a broken input-0 continuity yields ~
    ::
    =/  bad=tx:bc  (mk-tx c1-id ~[(mk-input 0xdead 0 keypath-wit)] ~[tip-out])
    %+  expect-eq
      !>  `(unit sont:ord)`~
      !>  (derive-tip:sal spawn start-tx ~[c0-tx bad])
  ==
::  ---- happy paths --------------------------------------------------
++  test-run-valid-pure-move
  =/  res  (run good-sat ~[c0-tx c1-tx] ~)
  ;:  weld
    (expect !>(ok.verdict.res))
    (expect-eq !>(9.000) !>(tip-value.res))
    %+  expect-eq
      !>  `(unit point:urb)``[[[c1-id 0 0] ~] [0 1 carried-pass [%.n who] ~ ~] `tip-hax]
      !>  point.res
  ==
::
++  test-run-valid-state-update
  ::  entry 1 advances the snapshot to life 2 with a committed opening
  ::
  =/  res  (run sat-s ~[c0-tx c1s-tx] ~)
  =/  got  (need point.res)
  ;:  weld
    (expect !>(ok.verdict.res))
    (expect-eq !>(2) !>(life.net.got))
    (expect-eq !>([c1-id 0 0]) !>(sont.own.got))
  ==
::  ---- adversarial --------------------------------------------------
++  test-run-bad-state-key-commitment
  ::  the opening claims an internal key the on-chain output did not
  ::  commit; the state-key reconstruction must not match.
  ::
  =/  bad-open  ^-(opening:sa [(mk-ikey 99) snap0 `spawn-open])
  =/  bad-chain=custody-log:sa  ~[[c0-id 100 `bad-open] [c1-id 101 ~]]
  =/  bad-pass
    pub:ex:(pit:nu:cric:crypto 512 (shaz seed) %c dat (jam bad-chain))
  =/  res  (run [who bad-pass bad-chain] ~[c0-tx c1-tx] ~)
  ;:  weld
    (expect !>(!ok.verdict.res))
    (expect !>(!(got-check verdict.res 'entry-0-commitment')))
    (expect !>(?=(~ point.res)))
  ==
::
++  test-run-life-regression
  ::  entry 1's snapshot regresses life below entry 0's -- rejected
  ::
  =/  snap-lo  ^-(snapshot:sa [life=0 rift=0 key=cry sponsor=~ fief=~])
  =/  bad-open  ^-(opening:sa [ikey1 snap-lo ~])
  =/  bad-out   (state-out ikey1 snap-lo 9.000)
  =/  bad-tx    (mk-tx c1-id ~[(mk-input c0-id 0 keypath-wit)] ~[bad-out])
  =/  bad-chain=custody-log:sa  ~[[c0-id 100 `open0] [c1-id 101 `bad-open]]
  =/  bad-pass
    pub:ex:(pit:nu:cric:crypto 512 (shaz seed) %c dat (jam bad-chain))
  =/  res  (run [who bad-pass bad-chain] ~[c0-tx bad-tx] ~)
  ;:  weld
    (expect !>(!ok.verdict.res))
    (expect !>(!(got-check verdict.res 'entry-1-life-order')))
  ==
::
++  test-run-wrong-dat-binding
  ::  a pass whose dat commits a different spawn satpoint than the
  ::  spawn-opening names fails the spawn-matches binding.
  ::
  =/  wrong-dat  (make-dat:cc [start-id 7 0])
  =/  bad-pass
    pub:ex:(pit:nu:cric:crypto 512 (shaz seed) %c wrong-dat (jam chain))
  =/  res  (run [who bad-pass chain] ~[c0-tx c1-tx] ~)
  ;:  weld
    (expect !>(!ok.verdict.res))
    (expect !>(!(got-check verdict.res 'spawn-matches')))
  ==
::
++  test-run-tip-status-fails-closed
  ;:  weld
    ::  tip spent -> not ok
    ::
    =/  spent  (run-checks:sal good-sat start-tx ~[c0-tx c1-tx] `%.n ~ no-points ~)
    (expect !>(!ok.verdict.spent))
    ::  tip status unknown (~) -> fails closed
    ::
    =/  unk  (run-checks:sal good-sat start-tx ~[c0-tx c1-tx] ~ ~ no-points ~)
    (expect !>(!ok.verdict.unk))
  ==
::
++  test-run-non-input-zero-continuity
  ::  the sat must be spent through input 0; a wrong outpoint there fails
  ::
  =/  bad  (mk-tx c1-id ~[(mk-input c0-id 1 keypath-wit)] ~[tip-out])
  =/  res  (run good-sat ~[c0-tx bad] ~)
  ;:  weld
    (expect !>(!ok.verdict.res))
    (expect !>(!(got-check verdict.res 'entry-1-continuity')))
  ==
::
++  test-run-keypath-requires-p2tr-prevout
  ::  a one-item 64-byte witness alone is not a key-path proof: the spent
  ::  output must actually have been P2TR.  A 3-hop chain -- entry 1 lands
  ::  the sat in a non-P2TR (P2WSH) output; entry 2 spends it with a
  ::  64-byte witness -- must fail key-path at entry 2.
  ::
  =/  c2-id  0x4d4d.4d4d
  =/  witness-script=hexb:bitcoin  [64 (rap 3 (reap 64 0x51))]
  =/  p2wsh=hexb:bitcoin
    [34 `@ux`(can 3 ~[[32 (shax dat.witness-script)] [2 0x2000]])]
  ::  entry 1: key-path move landing in the P2WSH output
  ::
  =/  m-c1  (mk-tx c1-id ~[(mk-input c0-id 0 keypath-wit)] ~[[p2wsh 9.000]])
  ::  entry 2: spend the P2WSH output with a lone 64-byte witness item
  ::
  =/  m-c2  (mk-tx c2-id ~[(mk-input c1-id 0 ~[witness-script])] ~[tip-out])
  =/  m-chain=custody-log:sa  ~[[c0-id 100 `open0] [c1-id 101 ~] [c2-id 102 ~]]
  =/  m-pass
    pub:ex:(pit:nu:cric:crypto 512 (shaz seed) %c dat (jam m-chain))
  =/  res  (run [who m-pass m-chain] ~[c0-tx m-c1 m-c2] ~)
  ;:  weld
    (expect !>(!ok.verdict.res))
    (expect !>(!(got-check verdict.res 'entry-2-key-path')))
  ==
::
++  test-run-sponsor-not-known
  ::  a snapshot naming a sponsor that is not a known public point fails
  ::  the sponsor-known check (absent sponsor would project to self).
  ::
  =/  snap-sp  ^-(snapshot:sa [life=1 rift=0 key=cry sponsor=`~zod fief=~])
  =/  sp-open  ^-(opening:sa [ikey0 snap-sp `spawn-open])
  =/  sp-out   (state-out ikey0 snap-sp 9.500)
  =/  sp-c0    (mk-tx c0-id ~[(mk-input start-id 0 ~)] ~[sp-out])
  =/  sp-chain=custody-log:sa  ~[[c0-id 100 `sp-open] [c1-id 101 ~]]
  =/  sp-pass
    pub:ex:(pit:nu:cric:crypto 512 (shaz seed) %c dat (jam sp-chain))
  =/  sat  ^-(self-attestation:sa [who sp-pass sp-chain])
  ::  unknown sponsor -> fail
  ::
  =/  res-no   (run-checks:sal sat start-tx ~[sp-c0 c1-tx] `%.y ~ no-points ~)
  ::  sponsor in known-public -> the sponsor-known check passes
  ::
  =/  res-yes
    (run-checks:sal sat start-tx ~[sp-c0 c1-tx] `%.y ~ (silt ~[~zod]) ~)
  ;:  weld
    (expect !>(!ok.verdict.res-no))
    (expect !>(!(got-check verdict.res-no 'sponsor-known')))
    (expect !>((got-check verdict.res-yes 'sponsor-known')))
  ==
::
++  test-run-oversized-log
  ::  a custody log longer than the packet-local bound is rejected
  ::
  =/  big=custody-log:sa  (reap 1.025 [c1-id 101 ~])
  =/  sat  ^-(self-attestation:sa [who carried-pass big])
  =/  res  (run-checks:sal sat start-tx (reap 1.025 c1-tx) `%.y ~ no-points ~)
  ;:  weld
    (expect !>(!ok.verdict.res))
    (expect !>(!(got-check verdict.res 'chain-bounded')))
  ==
::
++  test-run-empty-chain
  =/  sat  ^-(self-attestation:sa [who carried-pass ~])
  =/  res  (run-checks:sal sat start-tx ~ `%.y ~ no-points ~)
  ;:  weld
    (expect !>(!ok.verdict.res))
    (expect !>(!(got-check verdict.res 'chain-nonempty')))
  ==
::
++  test-run-spawn-opening-required
  ::  entry 0 without an opening cannot open the dat commitment
  ::
  =/  bad-chain=custody-log:sa  ~[[c0-id 100 ~] [c1-id 101 ~]]
  =/  sat  ^-(self-attestation:sa [who carried-pass bad-chain])
  =/  res  (run sat ~[c0-tx c1-tx] ~)
  ;:  weld
    (expect !>(!ok.verdict.res))
    (expect !>(!(got-check verdict.res 'spawn-opening')))
  ==
::  ---- tracked (anchor) ---------------------------------------------
++  test-run-tracked-happy
  ::  re-attesting from a prior tracked point at the earlier tip extends
  ::  the same chain; the boundary tip must match.
  ::
  =/  old-chain=custody-log:sa  (scag 1 chain)
  =/  old-pass
    pub:ex:(pit:nu:cric:crypto 512 (shaz seed) %c dat (jam old-chain))
  =/  old-point=point:urb
    [[[c0-id 0 0] ~] [0 1 old-pass [%.n who] ~ ~] ~]
  =/  res  (run good-sat ~[c0-tx c1-tx] `[old-point [c0-id 0 0]])
  (expect !>(ok.verdict.res))
::
++  test-run-tracked-rejects-history-rewrite
  ::  a log that DISAGREES with the anchor's proven chain at a shared
  ::  position is rejected.  One of the nine adversarial cases that must
  ::  keep snubbing, and it is now pinned with a genuine divergence -- a
  ::  different transaction at entry 0 -- rather than with a height, which
  ::  an honest reorg changes and which is deliberately no longer a
  ::  divergence at all (see +test-run-tracked-reorg-is-not-a-fork).
  ::
  ::  The DIVERGENCE is put in the anchor rather than in the packet so
  ::  that every other check still passes and this test isolates
  ::  tracked-prefix; the relation +compare-log computes is symmetric in
  ::  which side moved.
  ::
  =/  tracked  (anchor-at pass-f [c1-id 0 0] 1)
  =/  res  (run good-sat ~[c0-tx c1-tx] tracked)
  ;:  weld
    (expect !>(!ok.verdict.res))
    (expect !>(!(got-check verdict.res 'tracked-prefix')))
    ::  a fork is not a lag, so the lag check is not even emitted ...
    (expect !>(!(has-check verdict.res 'tracked-lag')))
    ::  ... and the outcome is the snub it always was.
    (expect-eq !>(%fraud) !>((classify:sal verdict.res)))
  ==
::
::  ---------------------------------------------------------------------
::  BUG 2: a log is a fork or not by its HOPS, never by their heights
::  ---------------------------------------------------------------------
::
::  Custody entries carry a block height, and entry 0's $spawn-opening
::  carries the funding transaction's, so heights sit INSIDE the log the
::  pass commits to.  Comparing whole entries therefore made an ordinary
::  Bitcoin reorg -- which at block-confirmations=1 happens several times
::  a month -- into fraud: the comet re-finalizes with corrected heights,
::  re-attests, every walk check passes, and the prefix comparison snubs
::  it stickily for the block its own transactions were re-mined into.
::
::  No replay, no attacker.  The same transactions, the same outpoints,
::  the same commitments, one block over.
::
++  test-run-tracked-reorg-is-not-a-fork
  =/  tracked  (anchor-at pass-r [c1-id 0 0] 1)
  =/  res  (run good-sat ~[c0-tx c1-tx] tracked)
  ;:  weld
    ::  THE assertion: not a fork ...
    (expect !>((got-check verdict.res 'tracked-prefix')))
    ::  ... and not a lag either -- nothing happened, so nothing is behind
    (expect !>(!(has-check verdict.res 'tracked-lag')))
    ::  ... which leaves an honest comet VALID, as it was before the reorg
    (expect !>(ok.verdict.res))
    (expect !>(!(stale-verdict:sal verdict.res)))
  ==
::
::  ---------------------------------------------------------------------
::  BUG 1: an older copy of OUR OWN log is a lag, and a lag is never fraud
::  ---------------------------------------------------------------------
::
::  OPERATIONS.md section 6: a refreshed pass is not written back to the
::  boot keyfile, so a comet that rekeys and then reboots serves the
::  shorter, feed-baked log -- forever, and with no attacker anywhere.
::  Reproduced live on two verifiers, two subjects, both desk revisions:
::  three stale-class checks in the verdict said "old copy" and
::  tracked-prefix overruled all three with fraud.
::
::  The rule: ANY lag is staleness (team decision, 2026-08-10).  An
::  attestation packet is a bearer token, so a third party who kept an old
::  one can re-send it; a replay is always a PREFIX and never a fork, so
::  %behind is exactly the relation an attacker can manufacture.  Classing
::  a deep lag as fraud therefore handed any observer a way to get an
::  honest comet permanently snubbed.  The depth is still reported
::  (tracked-lag) -- it is just not a condemnation.
::
++  test-run-tracked-one-behind-is-stale-not-fraud
  =/  tracked  (anchor-at pass3 [c2-id 0 0] 1)
  =/  res  (run good-sat ~[c0-tx c1-tx] tracked)
  ;:  weld
    (expect !>(!ok.verdict.res))
    ::  forgiven: the fraud-class check passes ...
    (expect !>((got-check verdict.res 'tracked-prefix')))
    ::  ... and the lag is still SAID OUT LOUD, in the stale class
    (expect !>((has-check verdict.res 'tracked-lag')))
    (expect !>(!(got-check verdict.res 'tracked-lag')))
    ::  forgiveness cannot fail open, and this is why: a strictly shorter
    ::  log ends before the tracked tip, so tracked-tip must fail.
    (expect !>(!(got-check verdict.res 'tracked-tip')))
    ::  so the verdict is negative -- but a DEMOTION, never a snub.
    (expect-eq !>(%stale) !>((classify:sal verdict.res)))
    (expect !>((stale-verdict:sal verdict.res)))
    (expect !>(?=(~ point.res)))
  ==
::
++  test-run-tracked-one-behind-cannot-walk-us-backwards
  ::  the property that makes the tolerance safe even after the demotion
  ::  has dropped our anchor.  A log is shorter BECAUSE a later entry
  ::  spent its tip -- that is what the missing entry is -- so its tip
  ::  outpoint is spent on chain, and our own filter scan proves it with
  ::  no anchor involved.  The old log therefore cannot reach ok=%.y on
  ::  the next packet either, tracked or not: forgiveness only ever
  ::  chooses between two NEGATIVE outcomes.
  ::
  =/  tracked  (anchor-at pass3 [c2-id 0 0] 1)
  ::  with the anchor, and its tip proven spent
  ::
  =/  both
    (run-checks:sal good-sat start-tx ~[c0-tx c1-tx] `%.n tracked no-points ~)
  ::  and with the anchor gone -- the state the demotion leaves us in
  ::
  =/  bare  (run-checks:sal good-sat start-tx ~[c0-tx c1-tx] `%.n ~ no-points ~)
  ;:  weld
    (expect !>(!ok.verdict.both))
    (expect !>(!(got-check verdict.both 'tip-unspent')))
    (expect-eq !>(%stale) !>((classify:sal verdict.both)))
    ::  no anchor at all: tracked-prefix is vacuously ok, tracked-lag is
    ::  not even emitted -- and the spent tip alone still refuses it.
    (expect !>(!ok.verdict.bare))
    (expect !>((got-check verdict.bare 'tracked-prefix')))
    (expect !>(!(has-check verdict.bare 'tracked-lag')))
    (expect !>(!(got-check verdict.bare 'tip-unspent')))
    (expect-eq !>(%stale) !>((classify:sal verdict.bare)))
  ==
::
++  test-run-tracked-two-behind-is-stale-not-fraud
  ::  what used to be the other side of a boundary.  Two missing entries
  ::  was fraud until 2026-08-10; it is staleness now, for the same reason
  ::  one entry always was, and the depth is reported rather than judged.
  ::
  =/  tracked  (anchor-at pass3 [c2-id 0 0] 1)
  =/  res  (run sat1 ~[c0-tx] tracked)
  ;:  weld
    (expect !>(!ok.verdict.res))
    (expect !>((got-check verdict.res 'tracked-prefix')))
    ::  it IS a lag, and the report still says so.
    (expect !>((has-check verdict.res 'tracked-lag')))
    (expect !>(!(got-check verdict.res 'tracked-lag')))
    (expect-eq !>(%stale) !>((classify:sal verdict.res)))
    (expect !>((stale-verdict:sal verdict.res)))
    (expect !>(?=(~ point.res)))
  ==
::  ---------------------------------------------------------------------
::  A DEEP LAG IS STALE, AND A REPLAY IS THE REASON
::  ---------------------------------------------------------------------
::
::  The regression this pins: `%behind =(1 by.rel)' in +anchor-ok made a
::  log behind by more than one FRAUD, i.e. a permanent ames snub of the
::  comet.  Ames has already proved the packet's pass hashes to the
::  claimed @p and that its signature is good, so nobody can forge one --
::  but anybody who saw one can RE-SEND it, and a replay of a comet's own
::  earlier log is a strict prefix, never a fork.  So %behind is precisely
::  the relation an attacker can reach, at any depth, and reading depth as
::  guilt let a third party sever two honest ships permanently.
::
::  Behind by FIVE: well past the old one-entry tolerance, from a peer we
::  really do track, and it must come out %stale -- which demotes to a
::  fresh %alien and lets the replacement packet through.
::
++  test-run-tracked-five-behind-is-stale-not-fraud
  ::  the anchor's pass carries a SIX-entry log: entry 0 is the same spawn
  ::  (so the two logs are comparable at all), then five plain custody
  ::  moves.  Only the incoming log's transactions are fetched, so the
  ::  extra hops need no chain fixtures -- +compare-log reads hop identity
  ::  out of the anchor's pass and nothing else.
  ::
  =/  deep=custody-log:sa
    :~  [c0-id 100 `open0]
        [c1-id 101 ~]
        [c2-id 102 ~]
        [0xa1a1.a1a1 103 ~]
        [0xb2b2.b2b2 104 ~]
        [0xc3c3.c3c3 105 ~]
    ==
  =/  deep-pass
    pub:ex:(pit:nu:cric:crypto 512 (shaz seed) %c dat (jam deep))
  =/  tracked  (anchor-at deep-pass [0xc3c3.c3c3 0 0] 1)
  ::  sat1 is the one-entry log: behind by five.
  ::
  =/  res  (run sat1 ~[c0-tx] tracked)
  ;:  weld
    ::  the relation really is a five-deep lag, and not a fork
    %+  expect-eq
      !>  `log-relation:sal`[%behind 5]
      !>  (compare-log:sal deep chain1)
    ::  ... which +anchor-ok forgives on its own
    (expect !>((anchor-ok:sal [%behind 5] ~ [c0-id 0 0])))
    ::  the verdict is negative -- a shorter log ends before the tracked
    ::  tip, so `tracked-tip' cannot pass and no point is installed ...
    (expect !>(!ok.verdict.res))
    (expect !>(!(got-check verdict.res 'tracked-tip')))
    (expect !>(?=(~ point.res)))
    ::  ... but the fraud-class check passes, so it is not a snub
    (expect !>((got-check verdict.res 'tracked-prefix')))
    ::  the lag is reported, at its real depth, in the stale class
    (expect !>((has-check verdict.res 'tracked-lag')))
    (expect !>(!(got-check verdict.res 'tracked-lag')))
    (expect-eq !>(%stale) !>((check-class:sal 'tracked-lag')))
    ::  THE property: stale, never fraud.
    (expect-eq !>(%stale) !>((classify:sal verdict.res)))
    (expect !>((stale-verdict:sal verdict.res)))
  ==
::
++  test-run-tracked-zero-behind-is-valid
  ::  behind by nothing: the peer re-sends the log we already hold.  This
  ::  is the ordinary retransmission, and it must stay VALID.
  ::
  =/  tracked  (anchor-at pass3 [c2-id 0 0] 1)
  =/  res  (run sat3 txl3 tracked)
  ;:  weld
    (expect !>(ok.verdict.res))
    (expect !>((got-check verdict.res 'tracked-prefix')))
    (expect !>(!(has-check verdict.res 'tracked-lag')))
  ==
::
++  test-run-tracked-dropped-opening-is-a-fork
  ::  the downgrade a bare length test would miss: same length, same
  ::  txids, but the peer has DROPPED the opening it previously published
  ::  at entry 1.  The log is not shorter, so it is not behind; it
  ::  contradicts what we already verified about one on-chain output, and
  ::  it would resolve the identity to an older snapshot (an older key).
  ::  That is a fork, and forks stay fraud.
  ::
  =/  tracked  (anchor-at pass-s [c1-id 0 0] 1)
  =/  res  (run good-sat ~[c0-tx c1-tx] tracked)
  ;:  weld
    (expect !>(!ok.verdict.res))
    (expect !>(!(got-check verdict.res 'tracked-prefix')))
    (expect !>(!(has-check verdict.res 'tracked-lag')))
    (expect-eq !>(%fraud) !>((classify:sal verdict.res)))
  ==
::
::  ---------------------------------------------------------------------
::  +compare-log / +anchor-ok -- the rule itself, without a chain
::  ---------------------------------------------------------------------
::
::  +prefix-chain answered this question with a loobean, and so said the
::  same %.n for a log that DIVERGES from ours and for one that is our own
::  log with its last entries missing.  Fraud and staleness, reported as
::  one bit.  The relation is four-valued now, and these pin every value
::  and every boundary between them.
::
++  test-compare-log-counts-the-gap-in-entries
  =/  e0  `custody-entry:sa`[c0-id 100 `open0]
  =/  e1  `custody-entry:sa`[c1-id 101 ~]
  =/  e2  `custody-entry:sa`[c2-id 102 ~]
  =/  l3  `custody-log:sa`~[e0 e1 e2]
  ;:  weld
    ::  behind by 0
    (expect-eq !>(`log-relation:sal`[%same ~]) !>((compare-log:sal l3 l3)))
    ::  behind by 1, by 2, by all of it
    (expect-eq !>(`log-relation:sal`[%behind 1]) !>((compare-log:sal l3 ~[e0 e1])))
    (expect-eq !>(`log-relation:sal`[%behind 2]) !>((compare-log:sal l3 ~[e0])))
    (expect-eq !>(`log-relation:sal`[%behind 3]) !>((compare-log:sal l3 ~)))
    ::  and the ordinary direction: they are ahead of us
    (expect-eq !>(`log-relation:sal`[%extends 1]) !>((compare-log:sal ~[e0 e1] l3)))
    (expect-eq !>(`log-relation:sal`[%extends 3]) !>((compare-log:sal ~ l3)))
    (expect-eq !>(`log-relation:sal`[%same ~]) !>((compare-log:sal ~ ~)))
  ==
::
++  test-compare-log-forks-at-first-middle-and-last
  =/  e0  `custody-entry:sa`[c0-id 100 `open0]
  =/  e1  `custody-entry:sa`[c1-id 101 ~]
  =/  e2  `custody-entry:sa`[c2-id 102 ~]
  =/  l3  `custody-log:sa`~[e0 e1 e2]
  ::  a hop of the same SHAPE that is simply a different transaction
  ::
  =/  x0  `custody-entry:sa`[0xdead.beef 100 `open0]
  =/  x1  `custody-entry:sa`[0xdead.beef 101 ~]
  ;:  weld
    (expect-eq !>(`log-relation:sal`[%fork 0]) !>((compare-log:sal l3 ~[x0 e1 e2])))
    (expect-eq !>(`log-relation:sal`[%fork 1]) !>((compare-log:sal l3 ~[e0 x1 e2])))
    (expect-eq !>(`log-relation:sal`[%fork 2]) !>((compare-log:sal l3 ~[e0 e1 x1])))
    ::  a divergence is a fork whatever the LENGTHS are: a log that both
    ::  disagrees and is shorter is not a lag we may forgive ...
    (expect-eq !>(`log-relation:sal`[%fork 1]) !>((compare-log:sal l3 ~[e0 x1])))
    (expect-eq !>(`log-relation:sal`[%fork 0]) !>((compare-log:sal l3 ~[x0])))
    ::  ... and one that disagrees while being LONGER is not an extension.
    (expect-eq !>(`log-relation:sal`[%fork 1]) !>((compare-log:sal ~[e0 e1] ~[e0 x1 e2])))
  ==
::
++  test-compare-log-keys-on-hops-not-heights
  =/  e0   `custody-entry:sa`[c0-id 100 `open0]
  =/  e1   `custody-entry:sa`[c1-id 101 ~]
  ::  the same two hops after a reorg: both heights moved, including the
  ::  start-height inside entry 0's spawn-opening.
  ::
  =/  e0r  `custody-entry:sa`[c0-id 90 `open0-r]
  =/  e1r  `custody-entry:sa`[c1-id 91 ~]
  ::  same txid and height, DIFFERENT opening: an incompatible claim about
  ::  one on-chain output, and not something a reorg can produce.
  ::
  =/  open0-x  ^-(opening:sa [ikey1 snap0 `spawn-open])
  =/  e0x      `custody-entry:sa`[c0-id 100 `open0-x]
  ::  same txid and height, opening DROPPED
  ::
  =/  e0n  `custody-entry:sa`[c0-id 100 ~]
  ;:  weld
    ::  THE bug-2 property: heights are not hop identity.
    (expect-eq !>(`log-relation:sal`[%same ~]) !>((compare-log:sal ~[e0 e1] ~[e0r e1r])))
    ::  ... nor is a height a way to hide a shorter log
    (expect-eq !>(`log-relation:sal`[%behind 1]) !>((compare-log:sal ~[e0 e1] ~[e0r])))
    ::  ... and everything else still is.
    (expect-eq !>(`log-relation:sal`[%fork 0]) !>((compare-log:sal ~[e0 e1] ~[e0x e1])))
    (expect-eq !>(`log-relation:sal`[%fork 0]) !>((compare-log:sal ~[e0 e1] ~[e0n e1])))
  ==
::
++  test-anchor-ok-forgives-every-lag-and-no-fork
  =/  tip  ^-(sont:ord [c1-id 0 0])
  =/  hit  `(unit sont:ord)``tip
  =/  mis  `(unit sont:ord)``[0xdead.beef 0 0]
  ;:  weld
    ::  THE RULE, stated once: a lag is forgiven at ANY depth, because a
    ::  lag is what a replayed packet looks like and a replay says nothing
    ::  about the comet.
    (expect !>((anchor-ok:sal [%behind 1] hit tip)))
    (expect !>((anchor-ok:sal [%behind 2] hit tip)))
    (expect !>((anchor-ok:sal [%behind 3] hit tip)))
    (expect !>((anchor-ok:sal [%behind 99] hit tip)))
    ::  a forgiven lag does not consult the boundary at all -- it cannot,
    ::  because the packet never reaches the position we anchored at.
    (expect !>((anchor-ok:sal [%behind 1] ~ tip)))
    (expect !>((anchor-ok:sal [%behind 1] mis tip)))
    (expect !>((anchor-ok:sal [%behind 99] ~ tip)))
    (expect !>((anchor-ok:sal [%behind 99] mis tip)))
    ::  a fork is never forgiven, at any position, boundary or no.
    (expect !>(!(anchor-ok:sal [%fork 0] hit tip)))
    (expect !>(!(anchor-ok:sal [%fork 1] hit tip)))
    (expect !>(!(anchor-ok:sal [%fork 99] hit tip)))
    ::  at or past our anchor, the anchor must still be reachable.
    (expect !>((anchor-ok:sal [%same ~] hit tip)))
    (expect !>((anchor-ok:sal [%extends 1] hit tip)))
    (expect !>(!(anchor-ok:sal [%same ~] mis tip)))
    (expect !>(!(anchor-ok:sal [%extends 1] mis tip)))
    (expect !>(!(anchor-ok:sal [%same ~] ~ tip)))
    (expect !>(!(anchor-ok:sal [%extends 1] ~ tip)))
  ==
::
++  test-run-tracked-life-monotonic
  ::  the new snapshot life may not regress below the tracked point's
  ::
  =/  anchored=point:urb
    [[[c0-id 0 0] ~] [0 5 carried-pass [%.n who] ~ ~] ~]
  =/  res  (run good-sat ~[c0-tx c1-tx] `[anchored [c0-id 0 0]])
  ;:  weld
    (expect !>(!ok.verdict.res))
    (expect !>(!(got-check verdict.res 'life-monotonic')))
  ==
::
::  ---------------------------------------------------------------------
::  +stale-verdict -- "out of date" is not "wrong"
::  ---------------------------------------------------------------------
::
::  Decisions addendum section 3: a spent tip makes an attestation STALE,
::  and staleness "is not fraud and MUST NOT produce %fail (a snub would
::  block the replacement packet)".  The packet path used to emit a
::  negative verdict here, which becomes a jael %fail and an ames snub;
::  live on mainnet that snubbed an honest comet permanently, because the
::  snub then blocked the refreshed attestation that would have fixed it.
::  ++stale-verdict is the discriminator that routes the packet path to
::  %stale instead, so it is pinned against BOTH kinds of failure using
::  the same fixtures.
::
++  test-stale-verdict-spent-tip-is-not-fraud
  =/  spent  (run-checks:sal good-sat start-tx ~[c0-tx c1-tx] `%.n ~ no-points ~)
  ;:  weld
    (expect !>(!ok.verdict.spent))
    (expect !>(!(got-check verdict.spent 'tip-unspent')))
    ::  the whole point: this must NOT become a negative %verdict
    ::
    (expect !>((stale-verdict:sal verdict.spent)))
  ==
::
++  test-undeterminable-tip-is-neither-fraud-nor-staleness
  ::  An unavailable filter or block, or a degenerate scan range, means we
  ::  could not LOOK.  That is not fraud (it would snub) and it is not
  ::  staleness either (that demotes the peer on a finding we did not
  ::  make): it is the third class, +unknown-verdict, and it produces no
  ::  verdict at all.
  ::
  ::  It must show up as `tip-scanned` failing and `tip-unspent` NOT
  ::  failing.  Collapsing the two was how "we could not look" became "we
  ::  looked and it is gone".
  ::
  =/  unk  (run-checks:sal good-sat start-tx ~[c0-tx c1-tx] ~ ~ no-points ~)
  ;:  weld
    (expect !>(!ok.verdict.unk))
    (expect !>(!(got-check verdict.unk 'tip-scanned')))
    (expect !>((got-check verdict.unk 'tip-unspent')))
    (expect !>((unknown-verdict:sal verdict.unk)))
    (expect !>(!(stale-verdict:sal verdict.unk)))
  ==
::
::  ---------------------------------------------------------------------
::  +unknown-verdict -- "we cannot tell" is not "you are lying"
::  ---------------------------------------------------------------------
::
::  Live mainnet 2026-08-06 (Phase 6.1): C3 judged C1's attestation while
::  its own block scanner sat 20 blocks below C3's OWN publication.  43 of
::  44 checks passed; `sponsor-known` failed because C3 did not yet know
::  that C3 existed -- and C3 snubbed the honest comet it sponsors.  The
::  same attestation verified VALID two hours later with nothing changed
::  but the scan position.  A verifier's public index is a WINDOW on the
::  chain, so "not in it" can never mean "does not exist".
::
++  test-unknown-verdict-sponsor-not-known-is-not-fraud
  =/  snap-sp  ^-(snapshot:sa [life=1 rift=0 key=cry sponsor=`~zod fief=~])
  =/  sp-open  ^-(opening:sa [ikey0 snap-sp `spawn-open])
  =/  sp-out   (state-out ikey0 snap-sp 9.500)
  =/  sp-c0    (mk-tx c0-id ~[(mk-input start-id 0 ~)] ~[sp-out])
  =/  sp-chain=custody-log:sa  ~[[c0-id 100 `sp-open] [c1-id 101 ~]]
  =/  sp-pass
    pub:ex:(pit:nu:cric:crypto 512 (shaz seed) %c dat (jam sp-chain))
  =/  sat  ^-(self-attestation:sa [who sp-pass sp-chain])
  ::  the sponsor is invisible to us, and everything else is perfect
  =/  res  (run-checks:sal sat start-tx ~[sp-c0 c1-tx] `%.y ~ no-points ~)
  ::  ... and unknowable-AND-stale is still merely unknowable
  =/  both  (run-checks:sal sat start-tx ~[sp-c0 c1-tx] `%.n ~ no-points ~)
  ;:  weld
    (expect !>(!ok.verdict.res))
    (expect !>(!(got-check verdict.res 'sponsor-known')))
    ::  THE regression guard: this must never become a %fail, i.e. a snub
    (expect !>((unknown-verdict:sal verdict.res)))
    (expect !>(!(stale-verdict:sal verdict.res)))
    (expect !>((unknown-verdict:sal verdict.both)))
    (expect !>(!(stale-verdict:sal verdict.both)))
  ==
::
::  Fraud beats ignorance: a peer does not get to launder bad evidence by
::  also naming a sponsor we cannot see.
::
++  test-unknown-verdict-fraud-alongside-ignorance-is-fraud
  =/  snap-sp  ^-(snapshot:sa [life=1 rift=0 key=cry sponsor=`~zod fief=~])
  ::  the opening commits snap-sp but the OUTPUT commits snap0: forged
  =/  sp-open  ^-(opening:sa [ikey0 snap-sp `spawn-open])
  =/  sp-chain=custody-log:sa  ~[[c0-id 100 `sp-open] [c1-id 101 ~]]
  =/  sp-pass
    pub:ex:(pit:nu:cric:crypto 512 (shaz seed) %c dat (jam sp-chain))
  =/  sat  ^-(self-attestation:sa [who sp-pass sp-chain])
  =/  res  (run-checks:sal sat start-tx ~[c0-tx c1-tx] `%.y ~ no-points ~)
  ;:  weld
    (expect !>(!ok.verdict.res))
    (expect !>(!(got-check verdict.res 'entry-0-commitment')))
    (expect !>(!(unknown-verdict:sal verdict.res)))
    (expect !>(!(stale-verdict:sal verdict.res)))
  ==
::
::  A verdict that PASSED is none of the three negative classes.
::
++  test-good-verdict-is-not-unknown
  =/  good  (run good-sat ~[c0-tx c1-tx] ~)
  ;:  weld
    (expect !>(ok.verdict.good))
    (expect !>((got-check verdict.good 'tip-scanned')))
    (expect !>(!(unknown-verdict:sal verdict.good)))
    (expect !>(!(stale-verdict:sal verdict.good)))
  ==
::
++  test-stale-verdict-fraud-is-still-fraud
  ::  every genuine-fraud fixture in this file must stay a %fail.  A
  ::  forged state commitment, a log bound to another name, a log that
  ::  contradicts itself, a hop that never happened -- none of these are
  ::  "out of date", and a peer must not be able to launder them.
  ::
  =/  bad-open  ^-(opening:sa [(mk-ikey 99) snap0 `spawn-open])
  =/  bad-chain=custody-log:sa  ~[[c0-id 100 `bad-open] [c1-id 101 ~]]
  =/  bad-pass
    pub:ex:(pit:nu:cric:crypto 512 (shaz seed) %c dat (jam bad-chain))
  =/  forged-commitment  (run [who bad-pass bad-chain] ~[c0-tx c1-tx] ~)
  ::
  =/  wrong-dat  (make-dat:cc [start-id 7 0])
  =/  wrong-pass
    pub:ex:(pit:nu:cric:crypto 512 (shaz seed) %c wrong-dat (jam chain))
  =/  wrong-name  (run [who wrong-pass chain] ~[c0-tx c1-tx] ~)
  ::
  =/  broken  (mk-tx c1-id ~[(mk-input c0-id 1 keypath-wit)] ~[tip-out])
  =/  broken-hop  (run good-sat ~[c0-tx broken] ~)
  ::
  =/  empty  (run-checks:sal [who carried-pass ~] start-tx ~ `%.y ~ no-points ~)
  ::  and a PASSING verdict is never stale
  ::
  =/  good  (run good-sat ~[c0-tx c1-tx] ~)
  ;:  weld
    (expect !>(!(stale-verdict:sal verdict.forged-commitment)))
    (expect !>(!(stale-verdict:sal verdict.wrong-name)))
    (expect !>(!(stale-verdict:sal verdict.broken-hop)))
    (expect !>(!(stale-verdict:sal verdict.empty)))
    (expect !>(!(stale-verdict:sal verdict.good)))
  ==
::
++  test-stale-verdict-fraud-alongside-staleness-is-fraud
  ::  a spent tip does not launder a forged commitment: one non-stale
  ::  failing check is enough to keep the verdict a %fail.
  ::
  =/  bad-open  ^-(opening:sa [(mk-ikey 99) snap0 `spawn-open])
  =/  bad-chain=custody-log:sa  ~[[c0-id 100 `bad-open] [c1-id 101 ~]]
  =/  bad-pass
    pub:ex:(pit:nu:cric:crypto 512 (shaz seed) %c dat (jam bad-chain))
  =/  res
    (run-checks:sal [who bad-pass bad-chain] start-tx ~[c0-tx c1-tx] `%.n ~ no-points ~)
  ;:  weld
    (expect !>(!ok.verdict.res))
    (expect !>(!(got-check verdict.res 'tip-unspent')))
    (expect !>(!(got-check verdict.res 'entry-0-commitment')))
    (expect !>(!(stale-verdict:sal verdict.res)))
  ==
::
++  test-stale-verdict-tracked-tip-and-life-are-staleness
  ::  an OLD copy of a log we already hold is out of date, not wrong: our
  ::  own tracker has moved past it (tracked-tip) or holds a higher life
  ::  (life-monotonic).  Snubbing for either would blacklist an honest
  ::  ship for a packet that was true when it was sent.
  ::
  ::  the anchor: the 1-entry log we already verified, at its own tip.
  ::
  =/  old-chain=custody-log:sa  (scag 1 chain)
  =/  old-pass
    pub:ex:(pit:nu:cric:crypto 512 (shaz seed) %c dat (jam old-chain))
  ::  we hold a HIGHER life than the packet proves -- an old copy.
  ::
  =/  higher-life=point:urb
    [[[c0-id 0 0] ~] [0 5 old-pass [%.n who] ~ ~] ~]
  =/  older  (run good-sat ~[c0-tx c1-tx] `[higher-life [c0-id 0 0]])
  ::  our tracker has the sat somewhere this log never reaches.
  ::
  =/  ahead=point:urb
    [[[0xfeed 0 0] ~] [0 1 old-pass [%.n who] ~ ~] ~]
  =/  moved  (run good-sat ~[c0-tx c1-tx] `[ahead [c0-id 0 0]])
  ::  but a log that DIVERGES from the one we verified is a fork, not an
  ::  old copy -- that stays fraud.  (Pinned with a different transaction
  ::  at entry 0; it used to be pinned with a different HEIGHT, which is
  ::  what an honest reorg produces and is no longer a divergence.)
  ::
  =/  fork  (run good-sat ~[c0-tx c1-tx] (anchor-at pass-f [c1-id 0 0] 1))
  ;:  weld
    (expect !>(!ok.verdict.older))
    (expect !>(!(got-check verdict.older 'life-monotonic')))
    (expect !>((stale-verdict:sal verdict.older)))
    (expect !>(!ok.verdict.moved))
    (expect !>(!(got-check verdict.moved 'tracked-tip')))
    (expect !>((stale-verdict:sal verdict.moved)))
    (expect !>(!(got-check verdict.fork 'tracked-prefix')))
    (expect !>(!(stale-verdict:sal verdict.fork)))
  ==
::
::  ---------------------------------------------------------------------
::  +routable / +extend-log -- the two helpers the %anew path leans on
::  ---------------------------------------------------------------------
::
::  A snapshot with neither a sponsor nor a fief cannot be cold-contacted.
::  This is deliberately NOT a validity rule (a %fail here would become an
::  ames snub of an honest ship); it drives an operator warning only.
::
++  test-routable
  ;:  weld
    (expect !>(!(routable:sal `snapshot:sa`[1 0 0xabc ~ ~])))
    (expect !>((routable:sal `snapshot:sa`[1 0 0xabc `~marzod ~])))
    (expect !>((routable:sal `snapshot:sa`[1 0 0xabc ~ `[%if .1.2.3.4 8.080]])))
  ==
::
::  Appending the entry we already hold is a RE-VALIDATION request, not a
::  second hop.  Appending it twice would put a duplicate txid in the log
::  whose input 0 cannot spend the (identical) previous tip, so the log
::  would fail +run-checks from then on -- silently, and forever.
::
++  test-extend-log-is-idempotent-on-the-tip
  =/  e0  `custody-entry:sa`[0xdead 900.001 ~]
  =/  e1  `custody-entry:sa`[0xbeef 900.050 ~]
  ;:  weld
    ::  a fresh entry appends
    (expect-eq !>(`custody-log:sa`~[e0 e1]) !>((extend-log:sal ~[e0] e1)))
    ::  re-poking the tip is a no-op
    (expect-eq !>(`custody-log:sa`~[e0 e1]) !>((extend-log:sal ~[e0 e1] e1)))
    ::  an empty base starts the log (the "finalize after boot" path)
    (expect-eq !>(`custody-log:sa`~[e0]) !>((extend-log:sal ~ e0)))
    ::  a REPEAT of a non-tip entry is still an append: only the tip is
    ::  treated as a retry, because only the tip could be one.
    (expect-eq !>(`custody-log:sa`~[e0 e1 e0]) !>((extend-log:sal ~[e0 e1] e0)))
  ==
::
::  ---------------------------------------------------------------------
::  THE THIRD DOORWAY ONTO THE SNUB PATH: +verify-lc's early aborts
::  ---------------------------------------------------------------------
::
::  ++run-checks is not the only thing that produces a $result.  The
::  light-client adapter returns early in four places, each with a
::  one-check failing verdict, and those four names used to belong to no
::  class at all: +stale-checks did not have them, +unknown-checks did not
::  have them, so the classifier's fall-through called every one of them
::  fraud and %gw-btc snubbed -- stickily -- on all four.
::
::  Two of the four are genuine fraud, one is genuine fraud after an
::  argument worth reading (+abort-class in lib/self-attestation), and one
::  can only fire when THIS DESK's own arithmetic contradicts itself, for
::  which snubbing a peer is indefensible.
::
++  test-abort-classes-are-assigned-deliberately
  ;:  weld
    ::  the peer's own xtr, judged before a single watch card is emitted:
    ::  an empty log offers no evidence for the identity the pass asserts.
    (expect-eq !>(%fraud) !>((abort-class:sal %empty-chain)))
    ::  likewise structural, likewise local: entry 0 never opens the dat
    ::  commitment, so the log is not bound to the name.
    (expect-eq !>(%fraud) !>((abort-class:sal %spawn-opening)))
    ::  after the fetches -- but every fetch that reached here was
    ::  confirmed on the main chain at the claimed height and txid, because
    ::  +fetch-tx-at strand-fails rather than return otherwise.  What is
    ::  left is the peer's claim about that evidence.
    (expect-eq !>(%fraud) !>((abort-class:sal %derive-tip)))
    ::  ... and the one that is OUR bug if it ever fires: +derive-tip took
    ::  that vout from +index-to-sont over this very output list.
    (expect-eq !>(%unknown) !>((abort-class:sal %tip-vout-range)))
  ==
::
::  Each abort's own $result must carry that class all the way through
::  +classify -- the function %gw-btc actually switches on.  This is the
::  end-to-end shape of the bug: before the fix all four landed on
::  %fraud here, whatever +abort-class would have said.
::
++  test-abort-results-classify-as-their-class
  =/  cls
    |=  =abort:sa
    ^-  verdict-class:sa
    (classify:sal verdict:(fail-result:sal who abort))
  ;:  weld
    (expect-eq !>(%fraud) !>((cls %empty-chain)))
    (expect-eq !>(%fraud) !>((cls %spawn-opening)))
    (expect-eq !>(%fraud) !>((cls %derive-tip)))
    ::  THE fix: this one is silence, not a sticky snub.
    (expect-eq !>(%unknown) !>((cls %tip-vout-range)))
    ::  and the two-valued views agree with the classifier
    (expect !>((unknown-verdict:sal verdict:(fail-result:sal who %tip-vout-range))))
    (expect !>(!(stale-verdict:sal verdict:(fail-result:sal who %tip-vout-range))))
    (expect !>(!(unknown-verdict:sal verdict:(fail-result:sal who %derive-tip))))
    (expect !>(!(stale-verdict:sal verdict:(fail-result:sal who %derive-tip))))
  ==
::
::  ---------------------------------------------------------------------
::  THE SECOND DOORWAY: a POSITIVE verdict the agent refuses locally
::  ---------------------------------------------------------------------
::
::  ++run-checks says ok and %gw-btc still declines.  None of the four
::  reasons is a finding about the peer: three are unreachable unless this
::  desk contradicts itself, and the fourth is our lagging sat index
::  disagreeing with a cryptographic proof.  A peer with a perfect
::  attestation must never be snubbed for any of them.
::
++  test-refusal-classes-never-snub
  ;:  weld
    (expect-eq !>(%unknown) !>((refusal-class:sal %who-mismatch)))
    (expect-eq !>(%unknown) !>((refusal-class:sal %no-point)))
    (expect-eq !>(%unknown) !>((refusal-class:sal %pass-mismatch)))
    (expect-eq !>(%unknown) !>((refusal-class:sal %tip-owned)))
    ::  stated as the property rather than the table: no refusal is fraud.
    %-  expect  !>
    %+  levy  `(list refusal:sa)`~[%who-mismatch %no-point %pass-mismatch %tip-owned]
    |=(r=refusal:sa !=(%fraud (refusal-class:sal r)))
  ==
::
::  ---------------------------------------------------------------------
::  The classification is STRUCTURALLY exhaustive
::  ---------------------------------------------------------------------
::
::  The four abort strings are today's instance of the bug; the bug itself
::  is that they were classified by LIST MEMBERSHIP with an implicit
::  fall-through, so a name nobody remembered to add defaulted to the one
::  outcome you can never take back.  Every classifier is now a ?- over a
::  closed union, which has no default.
::
::  Asserting that from inside a suite that must itself compile takes
::  +mint directly: build the switch as source, mint it against a subject
::  holding the union, and check whether it compiles.  A missing case is
::  -lost / mint-lost, exactly as it would be in the library.
::
++  test-classification-switches-are-exhaustive
  =/  mints
    |=  [sub=type src=tape]
    ^-  ?
    ::  NB: bind the +mule product before fishing on it.  `-:(mule ...)`
    ::  re-mints the trap under a subject that has lost its $ arm
    ::  (-find.$.+2); `=/` then `-.r` is the form that compiles.
    ::
    =/  r  (mule |.((~(mint ut sub) %noun (ream (crip src)))))
    ?=(%& -.r)
  =/  ab=type   -:!>([a=*abort:sa])
  =/  rf=type   -:!>([a=*refusal:sa])
  =/  vc=type   -:!>([a=*verdict-class:sa])
  ::  the real +abort-class shape ...
  =/  ab-full
    ;:  weld
      "?-(a %empty-chain %fraud, %spawn-opening %fraud, "
      "%derive-tip %fraud, %tip-vout-range %unknown)"
    ==
  ::  ... the same switch with one reason left unclassified ...
  =/  ab-short
    "?-(a %empty-chain %fraud, %spawn-opening %fraud, %derive-tip %fraud)"
  ::  ... and a case for a name that is not in the union at all.
  =/  ab-extra
    ;:  weld
      "?-(a %empty-chain %fraud, %spawn-opening %fraud, "
      "%derive-tip %fraud, %tip-vout-range %unknown, %brand-new %fraud)"
    ==
  =/  rf-full
    ;:  weld
      "?-(a %who-mismatch %unknown, %no-point %unknown, "
      "%pass-mismatch %unknown, %tip-owned %unknown)"
    ==
  =/  rf-short  "?-(a %who-mismatch %unknown, %no-point %unknown)"
  =/  vc-full   "?-(a %fraud 1, %stale 2, %unknown 3)"
  =/  vc-short  "?-(a %fraud 1, %stale 2)"
  ;:  weld
    (expect !>((mints ab ab-full)))
    ::  THE property: dropping a case does NOT compile, so a fifth abort
    ::  reason cannot reach the snub branch by default -- it cannot reach
    ::  any branch until somebody has classified it.
    (expect !>(!(mints ab ab-short)))
    ::  ... and a case whose name is not in the union does not compile
    ::  either, so switch and union cannot drift apart in either direction.
    (expect !>(!(mints ab ab-extra)))
    ::  the same for the local-refusal classifier ...
    (expect !>((mints rf rf-full)))
    (expect !>(!(mints rf rf-short)))
    ::  ... and for the agent's own three-way outcome switch, which is what
    ::  actually chooses between silence, a demotion, and a sticky snub.
    (expect !>((mints vc vc-full)))
    (expect !>(!(mints vc vc-short)))
  ==
::
::  +abort-of derives its recognizer from the $abort MOLD, so it cannot
::  fall behind the union the way a hand-written list of tags would.  A
::  recognizer that HAD fallen behind would answer "not an abort" -- i.e.
::  fraud, i.e. a snub -- for the very reason nobody remembered to add.
::
++  test-abort-recognizer-follows-the-mold
  ;:  weld
    (expect-eq !>(`(unit abort:sa)`[~ %empty-chain]) !>((abort-of:sal 'empty-chain')))
    (expect-eq !>(`(unit abort:sa)`[~ %spawn-opening]) !>((abort-of:sal 'spawn-opening')))
    (expect-eq !>(`(unit abort:sa)`[~ %derive-tip]) !>((abort-of:sal 'derive-tip')))
    (expect-eq !>(`(unit abort:sa)`[~ %tip-vout-range]) !>((abort-of:sal 'tip-vout-range')))
    ::  ordinary check names are not aborts, and neither is the empty cord
    (expect-eq !>(`(unit abort:sa)`~) !>((abort-of:sal 'sponsor-known')))
    (expect-eq !>(`(unit abort:sa)`~) !>((abort-of:sal 'entry-0-commitment')))
    (expect-eq !>(`(unit abort:sa)`~) !>((abort-of:sal '')))
  ==
::
::  +report's marker and the emitted card come from ONE function now, so
::  an operator can never be shown [XX] ("this is fraud") for a check that
::  produced silence.  They were two copies of the same set lookups, and a
::  name in neither set printed [XX] while +classify also said fraud --
::  consistent, and consistently wrong, for all four aborts.
::
++  test-check-class-agrees-with-the-report-marker
  =/  marker
    |=  name=cord
    ^-  tape
    =/  v=verdict:sa  [who %.n ~[[name %.n]]]
    =/  lines  (report:sal v)
    ?>  ?=([* * ~] lines)
    ?>  ?=(%leaf -.i.t.lines)
    (scag 6 p.i.t.lines)
  ;:  weld
    (expect-eq !>("  [??]") !>((marker 'tip-vout-range')))
    (expect-eq !>("  [??]") !>((marker 'sponsor-known')))
    (expect-eq !>("  [??]") !>((marker 'tip-scanned')))
    (expect-eq !>("  [..]") !>((marker 'tip-unspent')))
    (expect-eq !>("  [..]") !>((marker 'life-monotonic')))
    (expect-eq !>("  [..]") !>((marker 'tracked-tip')))
    ::  the lag and the judgement passed on it are DIFFERENT classes, and
    ::  an operator has to be able to see which is which: an older copy of
    ::  our own log is staleness, a log we cannot reconcile is fraud.
    (expect-eq !>("  [..]") !>((marker 'tracked-lag')))
    (expect-eq !>("  [XX]") !>((marker 'tracked-prefix')))
    (expect-eq !>("  [XX]") !>((marker 'derive-tip')))
    (expect-eq !>("  [XX]") !>((marker 'empty-chain')))
    (expect-eq !>("  [XX]") !>((marker 'spawn-opening')))
    (expect-eq !>("  [XX]") !>((marker 'entry-0-commitment')))
  ==
::
::  ---- diagnostics (test 6.7) ----------------------------------------
::
::  THE regression test for the clean-room finding.  Four %jael-writ
::  dispositions emitted nothing at all -- and three of those four sent a
::  NEGATIVE verdict, i.e. a sticky ames snub, with no line anywhere in
::  the log.  From outside, a ship snubbing every peer it heard from was
::  indistinguishable from a ship nobody was talking to.
::
::  Asserted here: no disposition is silent, every line is printable, every
::  line names the ship it is about, and no two dispositions produce the
::  same headline (a line that says only "dropped" is barely better than
::  silence).  None of this constrains the wording.
::
++  test-every-writ-drop-is-announced
  =/  peer  ~wes
  =/  reports
    %+  turn  all-writ-drops
    |=(d=writ-drop:sa (writ-drop-report:sal peer d))
  =/  heads  (turn reports head-tape)
  ;:  weld
    ::  nothing is silent
    (expect !>((levy reports |=(t=tang ?=(^ t)))))
    ::  every line is printable text, not a structure nobody reads
    (expect !>((levy reports all-leaves)))
    ::  every headline names the ship the decision is about
    (expect !>((levy heads |=(h=tape (has-sub "~wes" h)))))
    ::  ... and says WHY: eleven dispositions, eleven distinct headlines
    (expect-eq !>((lent heads)) !>(~(wyt in (silt heads))))
  ==
::
::  A routine duplicate and a sticky snub must not look alike at a glance,
::  and "look alike" is a property of the verb the line leads with.  That
::  verb is computed from the FATE, so the register an operator reads and
::  the cards jael receives come from one value -- the same discipline
::  +report's [XX]/[..]/[??] marker follows for post-verification
::  outcomes.
::
++  test-writ-drop-severity-agrees-with-its-fate
  =/  peer  ~wes
  ;:  weld
    %-  expect  !>
    %+  levy  all-writ-drops
    |=  d=writ-drop:sa
    ^-  ?
    =/  verb  (writ-drop-verb:sal (writ-drop-fate:sal d))
    (has-sub verb (head-tape (writ-drop-report:sal peer d)))
    ::  the three that emit a negative verdict, named individually: these
    ::  are the ones that snub, and they are decided from the peer's own
    ::  pass with no chain access, so no amount of catching up helps.
    ::
    (expect-eq !>(%fail) !>((writ-drop-fate:sal [%undecodable ~])))
    (expect-eq !>(%fail) !>((writ-drop-fate:sal [%empty-log ~])))
    (expect-eq !>(%fail) !>((writ-drop-fate:sal [%log-too-long 2.048 1.024])))
    ::  READINESS NEVER CONDEMNS.  Phase 6.1 in one line.
    ::
    (expect-eq !>(%hold) !>((writ-drop-fate:sal [%no-tip ~])))
    (expect-eq !>(%refresh) !>((writ-drop-fate:sal [%unsynced 900.000])))
    (expect-eq !>(%hold) !>((writ-drop-fate:sal [%tip-below-log 900.000 900.100])))
    ::  ... nor does declining to judge a shape we cannot read
    (expect-eq !>(%drop) !>((writ-drop-fate:sal [%onboarding ~])))
    (expect-eq !>(%drop) !>((writ-drop-fate:sal [%foreign-kelvin ~])))
    ::  a snub is invisible everywhere else -- it is discoverable only
    ::  through .^(/snubbed) -- so a REFUSED report is never a bare
    ::  headline: it carries the consequence and the undo.
    ::
    %-  expect  !>
    %+  levy  all-writ-drops
    |=  d=writ-drop:sa
    ^-  ?
    ?.  ?=(%fail (writ-drop-fate:sal d))  %.y
    (gth (lent (writ-drop-report:sal peer d)) 2)
  ==
::
::  The property, not the eleven strings: a twelfth disposition does
::  not compile until it has been given a line AND a fate.  Both
::  consumers are ?- over the closed union, so this test fails the moment
::  the mold and the switches drift in either direction -- which is also
::  what keeps +all-writ-drops above honest.
::
++  test-writ-drop-union-has-not-drifted
  =/  wd=type  -:!>([a=*writ-drop:sa])
  =/  wf=type  -:!>([a=*writ-fate:sa])
  =/  ar=type  -:!>([a=*anew-refusal:sa])
  =/  wd-full
    ;:  weld
      "?-(-.a %in-flight 0, %declined 0, %already-public 0, "
      "%onboarding 0, %foreign-kelvin 0, %undecodable 0, %empty-log 0, "
      "%log-too-long 0, %no-tip 0, %unsynced 0, %tip-below-log 0)"
    ==
  =/  wd-short
    ;:  weld
      "?-(-.a %in-flight 0, %declined 0, %already-public 0, "
      "%onboarding 0, %foreign-kelvin 0, %undecodable 0, %empty-log 0, "
      "%log-too-long 0, %no-tip 0, %unsynced 0)"
    ==
  =/  wd-extra
    ;:  weld
      "?-(-.a %in-flight 0, %declined 0, %already-public 0, "
      "%onboarding 0, %foreign-kelvin 0, %undecodable 0, %empty-log 0, "
      "%log-too-long 0, %no-tip 0, %unsynced 0, %tip-below-log 0, "
      "%brand-new 0)"
    ==
  =/  wf-full   "?-(a %drop 1, %hold 2, %refresh 3, %fail 4)"
  =/  wf-short  "?-(a %drop 1, %hold 2, %refresh 3)"
  =/  ar-full
    ;:  weld
      "?-(-.a %in-flight 0, %no-log 0, %log-too-long 0, %no-tip 0, "
      "%unsynced 0, %tip-below-log 0, %no-pass 0, %encode-failed 0, "
      "%name-mismatch 0)"
    ==
  =/  ar-short
    ;:  weld
      "?-(-.a %in-flight 0, %no-log 0, %log-too-long 0, %no-tip 0, "
      "%unsynced 0, %tip-below-log 0, %no-pass 0, %encode-failed 0)"
    ==
  ;:  weld
    (expect !>((mints wd wd-full)))
    ::  THE property: a dropped case does not compile, so a new
    ::  disposition cannot reach the silent branch -- or the snub -- by
    ::  default.  It cannot reach any branch at all.
    (expect !>(!(mints wd wd-short)))
    ::  ... and a case whose tag is not in the union does not compile
    ::  either, so the switches and the mold cannot drift apart in either
    ::  direction (which is what pins +all-writ-drops).
    (expect !>(!(mints wd wd-extra)))
    ::  the same for the four-valued fate, which is what separates a
    ::  routine drop from a readiness hold from a snub ...
    (expect !>((mints wf wf-full)))
    (expect !>(!(mints wf wf-short)))
    ::  ... and for our OWN pass refresh's refusals.
    (expect !>((mints ar ar-full)))
    (expect !>(!(mints ar ar-short)))
  ==
::
::  The %anew side of the same finding.  Phase 7.2 had to eliminate six of
::  these from outside -- re-deriving each precondition against the ship's
::  own libraries -- before it could conclude the seventh was the real
::  one.  None of them may be silent and none of them may condemn anybody:
::  a refused %anew is always our own readiness or our own bookkeeping.
::
++  test-every-anew-refusal-is-announced
  =/  me  ~nec
  =/  reports
    %+  turn  all-anew-refusals
    |=(r=anew-refusal:sa (anew-refusal-report:sal me r))
  =/  heads  (turn reports head-tape)
  ;:  weld
    (expect !>((levy reports |=(t=tang ?=(^ t)))))
    (expect !>((levy reports all-leaves)))
    ::  every line names the ship, because on a relay these interleave
    ::  with peer verification for other ships
    (expect !>((levy heads |=(h=tape (has-sub "~nec" h)))))
    ::  nine refusals, nine distinct headlines
    (expect-eq !>((lent heads)) !>(~(wyt in (silt heads))))
    ::  the readiness one re-polls the light client (/is-synced does not
    ::  emit on recovery, so the refused %anew is the poll) ...
    (expect-eq !>(%refresh) !>((anew-refusal-fate:sal [%unsynced 900.000])))
    ::  ... and nothing else emits a card of any kind
    %-  expect  !>
    %+  levy  all-anew-refusals
    |=  r=anew-refusal:sa
    ^-  ?
    ?:  ?=(%unsynced -.r)  %.y
    ?=(%drop (anew-refusal-fate:sal r))
  ==
::
::  THE observed failure of test 6.7, pinned.  A verification that DIES
::  rather than answering logged one line -- `%anew self-validation ended
::  without a verdict' -- and nothing else.  It was reasonless for a
::  structural reason: +set-timeout:strandio fails with `[%timeout ~]', an
::  EMPTY tang, and khan's mote was thrown away at the call site, so the
::  commonest death of all carried no information whatsoever.  The
::  clean-room run hit it twice, from two unrelated causes, and could not
::  tell them apart.
::
++  test-strand-death-is-never-reasonless
  =/  empty   (strand-death-report:sal [%own 4 2] %timeout ~)
  =/  filled  (strand-death-report:sal [%peer ~wes 3] %thread-fail ~[leaf+"boom"])
  ;:  weld
    ::  both are printable, and neither is a bare headline
    (expect !>((all-leaves empty)))
    (expect !>((all-leaves filled)))
    ::  a death with an EMPTY tang still explains itself: the headline is
    ::  two lines, so anything beyond that is the explanation the
    ::  clean-room run did not get.
    (expect !>((gth (lent empty) 2)))
    ::  the mote is printed.  For a +set-timeout death it is the ONLY
    ::  word there is, and it was the one being discarded.
    (expect !>((lien empty |=(t=tank (has-sub "timeout" (leaf-tape t))))))
    (expect !>((lien filled |=(t=tank (has-sub "thread-fail" (leaf-tape t))))))
    ::  a supplied tang is not swallowed either
    (expect !>((lien filled |=(t=tank (has-sub "boom" (leaf-tape t))))))
    ::  the two kinds of death are told apart, and the peer's names the
    ::  peer -- a dead strand is our infrastructure failing, never
    ::  evidence, and the operator has to be able to see which ship's
    ::  verification was lost.
    (expect !>((has-sub "~wes" (head-tape filled))))
    (expect !>(!=((head-tape filled) (head-tape empty))))
  ==
--
