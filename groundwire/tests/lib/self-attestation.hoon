::  Deterministic current-protocol vectors for lib/self-attestation.
::  No Bitcoin node or %light-client is involved: fetched transactions and
::  final UTXO status enter at the same pure boundary used by ++verify-lc.
::
/-  bitcoin, ord, urb, sa=self-attestation
/+  *test, sal=self-attestation, ue=urb-encoder, bscr=btc-script,
    tr=taproot, bc=bitcoin, cc=gw-btc-pass
=>
|%
++  secp  secp256k1:secp:crypto
::
++  mk-ikey
  |=  k=@
  ^-  @ux
  %-  compress-point:secp
  (mul-point-scalar:secp g:domain:curve:secp k)
::
++  cric-key
  |=  [sed=@ dat=@ xtr=@]
  =<  ?>(&(?=(%c suite.+<) ?=(^ sek.+<)) .)
  %:  pit:nu:cric:crypto
      512  (shaz (jam sed 1))
      %c   dat
      xtr
  ==
::
::  Encode a suite-C public pass with independently selected encryption key
::  material.  The comet fingerprint is derived from `ugn` and `dat`, so a
::  permitted networking-key rotation preserves both while changing `cry`.
++  pass-c
  |=  [ugn=@ cry=@ dat=@ xtr=@]
  ^-  pass
  =<  p
  %-  fax:plot
  :-  0
  :*  [s+~ 3 [1 'c'] ~]
      [s+~ 3 [32 ugn] ~]
      [s+~ 3 [32 cry] ~]
      (mat dat)
      ?:  =(0 xtr)  ~
      [(met 0 xtr)^xtr ~]
  ==
::
++  leaf-for
  |=  sots=(list sotx:urb)
  ^-  tapleaf:sa
  =/  unv  (full:encode:ue sots)
  =/  scr=octs  (en:bscr (unv-to-script:en:ue unv))
  [0xc0 [p.scr `@ux`q.scr]]
::
++  p2tr-spk
  |=  q=@ux
  ^-  hexb:bitcoin
  [34 `@ux`(can 3 ~[[32 q] [2 0x5120]])]
::
++  spkh-of
  |=  out=output:tx:bitcoin
  ^-  @ux
  =/  en-out  (can 3 script-pubkey.out 8^value.out ~)
  `@ux`(shay (add 8 wid.script-pubkey.out) en-out)
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
++  mk-rs
  |=  [w=@p skim=skim-sotx:urb]
  ^-  raw-sotx:urb
  [[0 0] [[w ~] skim]]
::
++  got-check
  |=  [v=verdict:sa name=cord]
  ^-  ?
  =/  cs  checks.v
  |-
  ?~  cs  !!
  ?:  =(name.i.cs name)  ok.i.cs
  $(cs t.cs)
::
++  no-points  ^-  (map @p point:urb)  ~
::
::  --------------------------------------------------------------------------
::  The precommit sat is in pre-id:0:0. Entry 0 spends it and creates a
::  spawn-committed output. Entry 1 is a reveal-less custody move.
::  --------------------------------------------------------------------------
::
++  pre-id   0x1a1a.1a1a
++  c0-id    0x2b2b.2b2b
++  c1-id    0x3c3c.3c3c
++  keys-id  0x4d4d.4d4d
++  mang-id  0x5e5e.5e5e
++  adopt-id  0x6f6f.6f6f
++  spawn-sont  ^-  sont:ord  [pre-id 0 0]
++  dat  (make-dat:cc spawn-sont)
++  base-key  (cric-key 'self-attestation-test' dat 0)
++  pas  pub:ex:base-key
++  who  `@p`fig:ex:base-key
::
++  pre-out  ^-  output:tx:bitcoin  [(p2tr-spk 0xdead) 10.000]
++  pre-tx   (mk-tx pre-id ~[(mk-input 0x9999 0 ~)] ~[pre-out])
::
++  spawn-sg
  ^-  single:skim-sotx:urb
  [%spawn pas ~ [(spkh-of pre-out) `0 0 0]]
++  spawn-rs  (mk-rs who spawn-sg)
++  spawn-leaf  (leaf-for ~[[[who ~] spawn-sg]])
++  ikey0  (mk-ikey 17)
++  q0  (output-pubkey:tr ikey0 `(leaf-hash:tr spawn-leaf))
::
++  c0-tx
  (mk-tx c0-id ~[(mk-input pre-id 0 ~)] ~[[(p2tr-spk q0) 9.500]])
::
::  q1 is an ordinary key-path output; no reveal is carried for this entry.
++  q1  (output-pubkey:tr (mk-ikey 23) ~)
++  c1-tx
  (mk-tx c1-id ~[(mk-input c0-id 0 keypath-wit)] ~[[(p2tr-spk q1) 9.000]])
::
++  chain
  ^-  custody-log:sa
  ~[[c0-id 100 `[ikey0 spawn-leaf]] [c1-id 101 ~]]
++  good-sat  ^-  self-attestation:sa  [who spawn-sont chain]
++  carried-pass  pub:ex:(cric-key 'self-attestation-test' dat (jam chain))
::
::  A later revealed %keys delta after the pure custody move.
++  rotated-pass
  |=  xtr=@
  ^-  pass
  =/  base  (cric-key 'self-attestation-test' dat 0)
  =/  rotated  (cric-key 'second-life' dat 0)
  =/  base-cic  (com:nu:cric:crypto pub:ex:base)
  =/  rotated-cic  (com:nu:cric:crypto pub:ex:rotated)
  ?>  ?=(%c suite.+<.base-cic)
  ?>  ?=(%c suite.+<.rotated-cic)
  (pass-c ugn.tw.pub.+<.base-cic cry.pub.+<.rotated-cic dat xtr)
++  keys-pas  (rotated-pass 0)
++  keys-sg  ^-  single:skim-sotx:urb  [%keys keys-pas |]
++  keys-leaf  (leaf-for ~[[[who ~] keys-sg]])
++  ikey2  (mk-ikey 31)
++  q2  (output-pubkey:tr ikey2 `(leaf-hash:tr keys-leaf))
++  keys-tx
  (mk-tx keys-id ~[(mk-input c1-id 0 keypath-wit)] ~[[(p2tr-spk q2) 8.500]])
++  keys-chain
  ^-  custody-log:sa
  (snoc chain [keys-id 102 `[ikey2 keys-leaf]])
++  keys-sat  ^-  self-attestation:sa  [who spawn-sont keys-chain]
++  carried-keys-pass
  (rotated-pass (jam keys-chain))
::
::  `%set-mang` remains unsupported by the live indexer and must therefore
::  fail closed in attestation replay rather than being treated as a no-op.
++  mang-sg  ^-  single:skim-sotx:urb  [%set-mang ~]
++  mang-leaf  (leaf-for ~[[[who ~] mang-sg]])
++  ikey3  (mk-ikey 37)
++  q3  (output-pubkey:tr ikey3 `(leaf-hash:tr mang-leaf))
++  mang-tx
  (mk-tx mang-id ~[(mk-input c1-id 0 keypath-wit)] ~[[(p2tr-spk q3) 8.000]])
++  mang-chain
  ^-  custody-log:sa
  (snoc chain [mang-id 102 `[ikey3 mang-leaf]])
++  mang-sat  ^-  self-attestation:sa  [who spawn-sont mang-chain]
::
::  Cross-identity mutations cannot be reconstructed from the attesting
::  ship's custody log and must fail closed under the current delta format.
++  adopt-sg  ^-  single:skim-sotx:urb  [%adopt ~zod]
++  adopt-leaf  (leaf-for ~[[[who ~] adopt-sg]])
++  ikey4  (mk-ikey 41)
++  q4  (output-pubkey:tr ikey4 `(leaf-hash:tr adopt-leaf))
++  adopt-tx
  (mk-tx adopt-id ~[(mk-input c1-id 0 keypath-wit)] ~[[(p2tr-spk q4) 8.000]])
++  adopt-chain
  ^-  custody-log:sa
  (snoc chain [adopt-id 102 `[ikey4 adopt-leaf]])
++  adopt-sat  ^-  self-attestation:sa  [who spawn-sont adopt-chain]
::
++  run-good
  |=  [sat=self-attestation:sa txl=(list tx:bc) tracked=(unit anchor:sa)]
  ^-  result:sa
  (run-checks:sal sat pre-tx txl `%.y tracked no-points)
--
|%
++  test-from-xtr
  =/  decoded  (from-xtr:sal who carried-pass)
  ;:  weld
    %+  expect-eq
      !>  `(unit self-attestation:sa)``good-sat
      !>  decoded
    (expect !>(?=(~ (from-xtr:sal ~marzod carried-pass))))
    =/  bad-key  (cric-key 'bad-xtr' dat 0)
    (expect !>(?=(~ (from-xtr:sal `@p`fig:ex:bad-key pub:ex:bad-key))))
  ==
::
++  test-parse-leaf
  =/  parsed  (parse-leaf:sal script:spawn-leaf)
  ?~  parsed  (expect !>(%.n))
  =/  expected=(list [=ship sig=(unit @) =single:skim-sotx:urb])
    ~[[who ~ spawn-sg]]
  %+  expect-eq
    !>  expected
    !>  (singles:sal u.parsed)
::
++  test-parse-leaf-rejects-prefixed-envelope
  =/  original=hexb:bitcoin  script:spawn-leaf
  =/  prefixed=octs
    [+(wid.original) (add dat.original (lsh [3 wid.original] 0x51))]
  (expect !>(?=(~ (parse-leaf:sal [p.prefixed q.prefixed]))))
::
++  test-parse-leaf-rejects-width-malleability
  =/  original=hexb:bitcoin  script:spawn-leaf
  (expect !>(?=(~ (parse-leaf:sal original(wid +(wid.original))))))
::
++  test-taproot-embit-vector
  ::  Independent cross-language vector generated with embit 0.8.0 from
  ::  internal scalar 1 and the single tapscript byte OP_TRUE (0x51).
  =/  internal=@ux
    0x2.79be.667e.f9dc.bbac.55a0.6295.ce87.0b07.029b.fcdb.2dce.28d9.59f2.815b.16f8.1798
  =/  expected-leaf=@ux
    0xa85b.2107.f791.b26a.84e7.586c.28ce.c7cb.6120.2ed3.d019.44d8.3250.0f36.3782.d675
  =/  expected-output=@ux
    0x9b6c.e0db.0707.e29f.92bf.8893.ed19.11d3.97e3.d2d7.6bbc.6811.0c49.da2c.eec8.be23
  =/  leaf  (leaf-hash:tr [0xc0 [1 0x51]])
  ;:  weld
    (expect-eq !>(expected-leaf) !>(leaf))
    (expect-eq !>(expected-output) !>((output-pubkey:tr internal `leaf)))
  ==
::
++  test-taproot-rejects-out-of-field-internal-key
  =/  field-prime
    p:domain:curve:secp256k1:secp:crypto
  =/  invalid=@ux
    (add field-prime (lsh [3 32] 2))
  (expect !>(?=(~ (out-key:sal [invalid [0xc0 [1 0x51]]]))))
::
++  test-derive-tip
  =/  bad=tx:bc  c1-tx
  =.  is.bad  ~
  ;:  weld
    %+  expect-eq
      !>  `(unit sont:ord)``[c1-id 0 0]
      !>  (derive-tip:sal spawn-sont pre-tx ~[c0-tx c1-tx])
    %+  expect-eq
      !>  `(unit sont:ord)`~
      !>  (derive-tip:sal spawn-sont pre-tx ~[c0-tx bad])
  ==
::
++  test-run-valid-with-pure-move
  =/  res  (run-good good-sat ~[c0-tx c1-tx] ~)
  ;:  weld
    (expect !>(ok.verdict.res))
    %+  expect-eq
      !>  9.000
      !>  tip-value.res
    %+  expect-eq
      !>  `(unit point:urb)``[[[c1-id 0 0] ~] 0 1 pas [%.n who] ~ ~]
      !>  point.res
  ==
::
++  test-run-later-reveal
  =/  res  (run-good keys-sat ~[c0-tx c1-tx keys-tx] ~)
  ;:  weld
    %+  expect-eq
      !>  `(unit self-attestation:sa)``keys-sat
      !>  (from-xtr:sal who carried-keys-pass)
    (expect !>(ok.verdict.res))
    %+  expect-eq
      !>  [2 keys-pas]
      !>  [life.net pass.net]:(need point.res)
    %+  expect-eq
      !>  [keys-id 0 0]
      !>  sont.own:(need point.res)
    (expect !>((same-key:cc carried-keys-pass pass.net:(need point.res))))
  ==
::
++  test-run-unsupported-set-mang
  =/  res  (run-good mang-sat ~[c0-tx c1-tx mang-tx] ~)
  ;:  weld
    (expect !>(!ok.verdict.res))
    (expect !>(!(got-check verdict.res 'state-replay')))
    (expect !>(?=(~ point.res)))
  ==
::
++  test-run-malformed-keys-pass
  =/  bad-sg=single:skim-sotx:urb  [%keys 42 |]
  =/  bad-leaf=tapleaf:sa  (leaf-for ~[[[who ~] bad-sg]])
  =/  bad-key  (mk-ikey 43)
  =/  bad-q  (output-pubkey:tr bad-key `(leaf-hash:tr bad-leaf))
  =/  bad-tx=tx:bc
    (mk-tx keys-id ~[(mk-input c1-id 0 keypath-wit)] ~[[(p2tr-spk bad-q) 8.500]])
  =/  bad-chain=custody-log:sa
    (snoc chain [keys-id 102 `[bad-key bad-leaf]])
  =/  bad-sat=self-attestation:sa  [who spawn-sont bad-chain]
  =/  res  (run-good bad-sat ~[c0-tx c1-tx bad-tx] ~)
  ;:  weld
    (expect !>(!ok.verdict.res))
    (expect !>(!(got-check verdict.res 'state-replay')))
    (expect !>(?=(~ point.res)))
  ==
::
++  test-run-cross-identity-delta
  =/  res  (run-good adopt-sat ~[c0-tx c1-tx adopt-tx] ~)
  ;:  weld
    (expect !>(!ok.verdict.res))
    (expect !>(!(got-check verdict.res 'state-replay')))
    (expect !>(?=(~ point.res)))
  ==
::
++  test-run-first-entry-must-reveal-spawn
  =/  s  good-sat
  =/  bad-chain=custody-log:sa  ~[[c0-id 100 ~] [c1-id 101 ~]]
  =/  res  (run-good s(chain bad-chain) ~[c0-tx c1-tx] ~)
  ;:  weld
    (expect !>(!ok.verdict.res))
    (expect !>(!(got-check verdict.res 'spawn-entry-zero')))
  ==
::
++  test-run-tampered-reveal
  =/  s  good-sat
  =/  bad-chain=custody-log:sa
    ~[[c0-id 100 `[internal-key=(mk-ikey 99) tapleaf=spawn-leaf]] [c1-id 101 ~]]
  =/  res  (run-good s(chain bad-chain) ~[c0-tx c1-tx] ~)
  ;:  weld
    (expect !>(!ok.verdict.res))
    (expect !>(!(got-check verdict.res 'entry-0-commitment')))
  ==
::
++  test-run-non-tapscript-leaf-version
  ::  Recompute the on-chain output around the same script using an invalid
  ::  protocol leaf version.  A bare tweak equality would accept this pair;
  ::  the verifier must require the canonical 0xc0 Tapscript version.
  =/  original=tapleaf:sa  spawn-leaf
  =/  bad-leaf=tapleaf:sa  original(version 0xc1)
  =/  bad-q  (output-pubkey:tr ikey0 `(leaf-hash:tr bad-leaf))
  =/  bad-c0=tx:bc
    (mk-tx c0-id ~[(mk-input pre-id 0 ~)] ~[[(p2tr-spk bad-q) 9.500]])
  =/  bad-chain=custody-log:sa
    ~[[c0-id 100 `[ikey0 bad-leaf]] [c1-id 101 ~]]
  =/  bad-sat=self-attestation:sa  [who spawn-sont bad-chain]
  =/  res  (run-good bad-sat ~[bad-c0 c1-tx] ~)
  ;:  weld
    (expect !>(!ok.verdict.res))
    (expect !>(!(got-check verdict.res 'entry-0-commitment')))
  ==
::
++  test-run-input-zero-continuity
  =/  bad=tx:bc  c1-tx
  =.  is.bad  ~[(mk-input c0-id 1 keypath-wit)]
  =/  res  (run-good good-sat ~[c0-tx bad] ~)
  ;:  weld
    (expect !>(!ok.verdict.res))
    (expect !>(!(got-check verdict.res 'entry-1-continuity')))
  ==
::
++  test-run-keypath-requires-p2tr-prevout
  ::  A one-item 64-byte witness is not sufficient by itself: P2WSH and
  ::  other witness programs can have the same stack shape.  Entry 2 must
  ::  also prove that the output created by entry 1 was P2TR.
  =/  witness-script=hexb:bitcoin
    [64 (rap 3 (reap 64 0x51))]
  =/  p2wsh=hexb:bitcoin
    [34 `@ux`(can 3 ~[[32 (shax dat.witness-script)] [2 0x2000]])]
  =/  bad-c1=tx:bc
    (mk-tx c1-id ~[(mk-input c0-id 0 keypath-wit)] ~[[p2wsh 9.000]])
  =/  bad-keys=tx:bc
    (mk-tx keys-id ~[(mk-input c1-id 0 ~[witness-script])] ~[[(p2tr-spk q2) 8.500]])
  =/  res  (run-good keys-sat ~[c0-tx bad-c1 bad-keys] ~)
  ;:  weld
    (expect !>(!ok.verdict.res))
    (expect !>(!(got-check verdict.res 'entry-2-key-path')))
  ==
::
++  test-run-final-tip-must-be-p2tr
  =/  bad=tx:bc
    (mk-tx c1-id ~[(mk-input c0-id 0 keypath-wit)] ~[[[1 0x51] 9.000]])
  =/  res  (run-good good-sat ~[c0-tx bad] ~)
  ;:  weld
    (expect !>(!ok.verdict.res))
    (expect !>(!(got-check verdict.res 'tip-p2tr')))
  ==
::
++  test-run-tip-status-fails-closed
  ;:  weld
    =/  spent
      (run-checks:sal good-sat pre-tx ~[c0-tx c1-tx] `%.n ~ no-points)
    (expect !>(!ok.verdict.spent))
    =/  unknown
      (run-checks:sal good-sat pre-tx ~[c0-tx c1-tx] ~ ~ no-points)
    (expect !>(!ok.verdict.unknown))
  ==
::
++  test-run-height-order
  =/  s  good-sat
  =/  bad-chain=custody-log:sa
    ~[[c0-id 100 `[ikey0 spawn-leaf]] [c1-id 99 ~]]
  =/  res  (run-good s(chain bad-chain) ~[c0-tx c1-tx] ~)
  ;:  weld
    (expect !>(!ok.verdict.res))
    (expect !>(!(got-check verdict.res 'entry-1-height-order')))
  ==
::
++  test-run-tracked
  =/  old-chain=custody-log:sa  (scag 1 chain)
  =/  old-pass=pass
    pub:ex:(cric-key 'self-attestation-test' dat (jam old-chain))
  =/  old-point=point:urb
    [[[c0-id 0 0] ~] 0 1 old-pass [%.n who] ~ ~]
  =/  final-point=point:urb
    [[[c1-id 0 0] ~] 0 1 carried-pass [%.n who] ~ ~]
  =/  bad-point=point:urb
    [[[0xdead 0 0] ~] 0 1 old-pass [%.n who] ~ ~]
  ;:  weld
    (expect !>(ok.verdict:(run-good good-sat ~[c0-tx c1-tx] `[old-point [c0-id 0 0]])))
    (expect !>(ok.verdict:(run-good good-sat ~[c0-tx c1-tx] `[final-point [c1-id 0 0]])))
    =/  res  (run-good good-sat ~[c0-tx c1-tx] `[bad-point [c0-id 0 0]])
    ;:  weld
      (expect !>(!ok.verdict.res))
      (expect !>(!(got-check verdict.res 'tracked-tip')))
    ==
  ==
::
++  test-run-tracked-prefix-extension
  =/  anchored=point:urb
    :*  [[c1-id 0 0] ~]
        0
        1
        carried-pass
        [%.y ~bud]
        `~nec
        `[%if p=.127.0.0.1 q=123]
    ==
  =/  res
    (run-good keys-sat ~[c0-tx c1-tx keys-tx] `[anchored [c1-id 0 0]])
  =/  got  (need point.res)
  ;:  weld
    (expect !>(ok.verdict.res))
    (expect-eq !>([%.y ~bud]) !>(sponsor.net.got))
    (expect-eq !>(`~nec) !>(escape.net.got))
    ?>  ?=(^ fief.net.got)
    =/  expected=fief:urb  [%if p=`@ifF`.127.0.0.1 q=`@udE`123]
    %+  expect-eq
      !>(expected)
    !>(u.fief.net.got)
    (expect-eq !>([2 keys-pas]) !>([life.net.got pass.net.got]))
  ==
::
++  test-run-tracked-rejects-history-rewrite
  =/  anchored=point:urb
    [[[c1-id 0 0] ~] 0 1 carried-pass [%.n who] ~ ~]
  =/  rewritten=custody-log:sa
    ~[[c0-id 99 `[ikey0 spawn-leaf]] [c1-id 101 ~]]
  =/  rewritten-sat=self-attestation:sa
    [who spawn-sont rewritten]
  =/  res
    (run-good rewritten-sat ~[c0-tx c1-tx] `[anchored [c1-id 0 0]])
  ;:  weld
    (expect !>(!ok.verdict.res))
    (expect !>(!(got-check verdict.res 'tracked-prefix')))
  ==
--
