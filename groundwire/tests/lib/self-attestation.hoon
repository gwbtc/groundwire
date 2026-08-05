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
::
++  no-points  *(set ship)
::  --------------------------------------------------------------------
::  Identity: seed -> blind -> dat -> suite-C pass; who = fig(pass).
::  The messaging key (cry.pub) is independent of the mutable xtr, so it
::  is computed once and pinned into every committed snapshot.
::  --------------------------------------------------------------------
++  seed       'gw-self-attestation-test'
++  start-id   0x1a1a.1a1a
++  c0-id      0x2b2b.2b2b
++  c1-id      0x3c3c.3c3c
++  spawn      ^-(sont:ord [start-id 0 0])
++  blind      (make-blind:cc seed)
++  dat        (make-dat:cc spawn blind)
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
::  blind-opening on entry 0 opens the hiding dat commitment.
::
++  spawn-open  ^-(blind-opening:sa [spawn start-height=778.000 blind])
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
++  run
  |=  [sat=self-attestation:sa txl=(list tx:bc) tracked=(unit anchor:sa)]
  ^-  result:sa
  (run-checks:sal sat start-tx txl `%.y tracked no-points)
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
      !>  `(unit point:urb)``[[[c1-id 0 0] ~] 0 1 carried-pass [%.n who] ~ ~]
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
  ::  blind-opening reveals fails the spawn-commit binding.
  ::
  =/  wrong-dat  (make-dat:cc [start-id 7 0] blind)
  =/  bad-pass
    pub:ex:(pit:nu:cric:crypto 512 (shaz seed) %c wrong-dat (jam chain))
  =/  res  (run [who bad-pass chain] ~[c0-tx c1-tx] ~)
  ;:  weld
    (expect !>(!ok.verdict.res))
    (expect !>(!(got-check verdict.res 'spawn-commit')))
  ==
::
++  test-run-tip-status-fails-closed
  ;:  weld
    ::  tip spent -> not ok
    ::
    =/  spent  (run-checks:sal good-sat start-tx ~[c0-tx c1-tx] `%.n ~ no-points)
    (expect !>(!ok.verdict.spent))
    ::  tip status unknown (~) -> fails closed
    ::
    =/  unk  (run-checks:sal good-sat start-tx ~[c0-tx c1-tx] ~ ~ no-points)
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
  =/  res-no   (run-checks:sal sat start-tx ~[sp-c0 c1-tx] `%.y ~ no-points)
  ::  sponsor in known-public -> the sponsor-known check passes
  ::
  =/  res-yes
    (run-checks:sal sat start-tx ~[sp-c0 c1-tx] `%.y ~ (silt ~[~zod]))
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
  =/  res  (run-checks:sal sat start-tx (reap 1.025 c1-tx) `%.y ~ no-points)
  ;:  weld
    (expect !>(!ok.verdict.res))
    (expect !>(!(got-check verdict.res 'chain-bounded')))
  ==
::
++  test-run-empty-chain
  =/  sat  ^-(self-attestation:sa [who carried-pass ~])
  =/  res  (run-checks:sal sat start-tx ~ `%.y ~ no-points)
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
    [[[c0-id 0 0] ~] 0 1 old-pass [%.n who] ~ ~]
  =/  res  (run good-sat ~[c0-tx c1-tx] `[old-point [c0-id 0 0]])
  (expect !>(ok.verdict.res))
::
++  test-run-tracked-rejects-history-rewrite
  ::  a prefix that disagrees with the anchor's proven chain is rejected
  ::
  =/  anchored=point:urb
    [[[c1-id 0 0] ~] 0 1 carried-pass [%.n who] ~ ~]
  =/  rewritten=custody-log:sa
    ~[[c0-id 99 `open0] [c1-id 101 ~]]
  =/  bad-pass
    pub:ex:(pit:nu:cric:crypto 512 (shaz seed) %c dat (jam rewritten))
  =/  res
    (run [who bad-pass rewritten] ~[c0-tx c1-tx] `[anchored [c1-id 0 0]])
  ;:  weld
    (expect !>(!ok.verdict.res))
    (expect !>(!(got-check verdict.res 'tracked-prefix')))
  ==
::
++  test-run-tracked-life-monotonic
  ::  the new snapshot life may not regress below the tracked point's
  ::
  =/  anchored=point:urb
    [[[c0-id 0 0] ~] 0 5 carried-pass [%.n who] ~ ~]
  =/  res  (run good-sat ~[c0-tx c1-tx] `[anchored [c0-id 0 0]])
  ;:  weld
    (expect !>(!ok.verdict.res))
    (expect !>(!(got-check verdict.res 'life-monotonic')))
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
--
