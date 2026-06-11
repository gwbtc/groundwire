::  tests/lib/self-attestation.hoon
::
::  Unit tests for the confidential-comet verification library.
::  Everything here is pure: the end-to-end fixture hand-builds the
::  precommit -> C0 (spawn commit) -> C1 (transfer commit) chain with real
::  secp taproot commitments and a suite-C key minted with the correct
::  tweak, then drives ++run-checks directly (no RPC; the blockhashes are
::  arbitrary constants, since the pure verifier never fetches).
::
/-  bitcoin, ord, urb, sa=self-attestation
/+  *test, sal=self-attestation, ue=urb-encoder, bscr=btc-script,
    tr=taproot, bc=bitcoin
=>
|%
++  secp  secp256k1:secp:crypto
::  a real 33-byte compressed secp pubkey
++  mk-ikey
  |=  k=@
  ^-  @ux
  %-  compress-point:secp
  (mul-point-scalar:secp g:domain:curve:secp k)
::  a suite-C networking key encoding a chosen tweak (cf. ++cut in
::  tests/lib/urb-core.hoon)
++  cric-key
  |=  [sed=@ twk=@]
  =<  ?>(&(?=(%c suite.+<) ?=(^ sek.+<)) .)
  %:  pit:nu:cric:crypto
      512  (shaz (jam sed 1))
      %c   twk
      0
  ==
::  the single urb leaf committing to a list of sotx
++  leaf-for
  |=  sots=(list sotx:urb)
  ^-  tapleaf:sa
  =/  unv  (full:encode:ue sots)
  =/  scr=octs  (en:bscr (unv-to-script:en:ue unv))
  [0xc0 [p.scr `@ux`q.scr]]
::  OP_1 PUSH32 <q>
++  p2tr-spk
  |=  q=@ux
  ^-  hexb:bitcoin
  [34 `@ux`(can 3 ~[[32 q] [2 0x5120]])]
::  urb-core's scriptPubKey-output hash (as in calc-precommit-sont)
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
::  raw-sotx with a bunt raw (checks compare sot projections only)
++  mk-rs
  |=  [w=@p skim=skim-sotx:urb]
  ^-  raw-sotx:urb
  [[0 0] [[w ~] skim]]
::  a link where only the sots matter (replay/structure tests)
++  mk-link
  |=  sots=(list raw-sotx:urb)
  ^-  link:sa
  [0x0 0x0 [0x0 [0xc0 [1 0x0]]] sots]
::  look up a named check
++  got-check-in
  |=  [cs=(list check:sa) name=cord]
  ^-  ?
  |-
  ?~  cs  !!
  ?:  =(name.i.cs name)  ok.i.cs
  $(cs t.cs)
::
++  got-check
  |=  [v=verdict:sa name=cord]
  ^-  ?
  (got-check-in checks.v name)
::
::  ==========================================================================
::  End-to-end fixture: precommit -> C0 (spawn commit) -> C1 (transfer
::  commit). Each link IS a commit tx whose input 0 spends the previous
::  link's sat-carrying output and whose OWN output commits to its leaf.
::  ==========================================================================
::
++  pre-id   0x1a1a.1a1a
++  c0-id    0x2b2b.2b2b
++  c1-id    0x3c3c.3c3c
++  keys-id  0x4d4d.4d4d
::  blockhashes are arbitrary: the pure verifier never fetches
++  pre-blk  0xb0b0
++  c0-blk   0xb1b1
++  c1-blk   0xb2b2
::  the genesis tweak binds the key to the precommit satpoint [pre-id 0 0]
++  twk  (rap 3 ~[%9 ~tyr %urb-watcher %btc %gw %9 pre-id 0 0])
++  key  (cric-key 'self-attestation-test' twk)
++  who  `@p`fig:ex:key
++  pas  pub:ex:key
::
++  pre-out  ^-  output:tx:bitcoin  [(p2tr-spk 0xdead) 10.000]
++  pre-tx   (mk-tx pre-id ~[(mk-input 0x9999 0 ~)] ~[pre-out])
::
++  spawn-sg  ^-  single:skim-sotx:urb  [%spawn pas ~ [(spkh-of pre-out) `0 0 0]]
++  noop-sg   ^-  single:skim-sotx:urb  [%no-op ~]
++  spawn-rs  (mk-rs who spawn-sg)
++  noop-rs   (mk-rs who noop-sg)
::
++  spawn-leaf  (leaf-for ~[[[who ~] spawn-sg]])
++  noop-leaf   (leaf-for ~[[[who ~] noop-sg]])
::
++  ikey0  (mk-ikey 17)
++  ikey1  (mk-ikey 23)
++  q0  (output-pubkey:tr ikey0 `(leaf-hash:tr spawn-leaf))
++  q1  (output-pubkey:tr ikey1 `(leaf-hash:tr noop-leaf))
::
::  C0, the spawn commit (link 0): input 0 spends the precommit output (a
::  plain wallet spend); its OWN output commits to the spawn leaf
++  c0-tx  (mk-tx c0-id ~[(mk-input pre-id 0 ~)] ~[[(p2tr-spk q0) 9.500]])
::  C1, a transfer commit (link 1): input 0 key-path-spends C0's committed
::  output; its OWN output commits to the %no-op leaf and is the tip
++  c1-tx  (mk-tx c1-id ~[(mk-input c0-id 0 keypath-wit)] ~[[(p2tr-spk q1) 9.000]])
::
++  link0  ^-  link:sa  [c0-id c0-blk [ikey0 spawn-leaf] ~[spawn-rs]]
++  link1  ^-  link:sa  [c1-id c1-blk [ikey1 noop-leaf] ~[noop-rs]]
::
++  good-sat
  ^-  self-attestation:sa
  [who [pre-id pre-blk] ~[link0 link1] [c1-id 0 0]]
::  a bare %spawn: the one-link keyfile whose tip is C0's own output
++  spawn-sat
  ^-  self-attestation:sa
  [who [pre-id pre-blk] ~[link0] [c0-id 0 0]]
::
::  a %keys commit at the tip (the tip leaf's sotx is enacted directly)
++  keys-pas   pub:ex:(cric-key 'second-life' 0x2)
++  keys-sg    ^-  single:skim-sotx:urb  [%keys keys-pas |]
++  keys-rs    (mk-rs who keys-sg)
++  keys-leaf  (leaf-for ~[[[who ~] keys-sg]])
++  ikey2  (mk-ikey 31)
++  q2  (output-pubkey:tr ikey2 `(leaf-hash:tr keys-leaf))
++  keys-tx
  (mk-tx keys-id ~[(mk-input c0-id 0 keypath-wit)] ~[[(p2tr-spk q2) 9.000]])
++  keys-link  ^-  link:sa  [keys-id c1-blk [ikey2 keys-leaf] ~[keys-rs]]
++  keys-sat
  ^-  self-attestation:sa
  [who [pre-id pre-blk] ~[link0 keys-link] [keys-id 0 0]]
::
++  run-good
  |=  [sat=self-attestation:sa tracked=(unit sont:ord)]
  ^-  result:sa
  (run-checks:sal sat ~[c0-tx c1-tx] pre-tx `%.y tracked)
::
++  replay
  |=  chain=(list link:sa)
  ^-  (unit point:urb)
  (replay-chain:sal who chain [0xfeed 0 0])
--
|%
::  ==========================================================================
::  Encoder: the %no-op opcode
::  ==========================================================================
++  test-encoder-no-op-roundtrip
  =/  sot=sotx:urb  [[~zod ~] %no-op ~]
  =/  parsed  (parse-roll:ue (full:encode:ue ~[sot]))
  ;:  weld
    %+  expect-eq
      !>  1
      !>  (lent parsed)
    %+  expect-eq
      !>  `sotx:urb`sot
      !>  sot:(snag 0 parsed)
  ==
::
++  test-encoder-no-op-in-batch
  =/  sot=sotx:urb  [[~zod ~] %batch ~[[%no-op ~] [%fief ~]]]
  =/  parsed  (parse-roll:ue (full:encode:ue ~[sot]))
  %+  expect-eq
    !>  `sotx:urb`sot
    !>  sot:(snag 0 parsed)
::
::  ==========================================================================
::  Leaf parsing
::  ==========================================================================
++  test-parse-leaf-no-op
  =/  parsed  (parse-leaf:sal script:noop-leaf)
  ;:  weld
    (expect !>(?=(^ parsed)))
    %+  expect-eq
      !>  ~[`sotx:urb`[[who ~] noop-sg]]
      !>  (turn (need parsed) get-sotx:sal)
  ==
::
++  test-parse-leaf-garbage
  ::  a well-formed envelope around an invalid-opcode unv must yield ~
  ::  (mole'd), not crash the thread
  =/  bad-unv  (lsh [0 130] 0b111.1111)
  =/  scr=octs  (en:bscr (unv-to-script:en:ue bad-unv))
  %+  expect-eq
    !>  `(unit (list raw-sotx:urb))`~
    !>  (parse-leaf:sal [p.scr `@ux`q.scr])
::
::  ==========================================================================
::  Pure helpers
::  ==========================================================================
++  test-p2tr-xonly
  ;:  weld
    %+  expect-eq
      !>  `(unit @ux)``0xabcd
      !>  (p2tr-xonly:sal (p2tr-spk 0xabcd))
    %+  expect-eq
      !>  `(unit @ux)`~
      !>  (p2tr-xonly:sal [33 0x0])
    %+  expect-eq
      !>  `(unit @ux)`~
      !>  (p2tr-xonly:sal [34 `@ux`(can 3 ~[[32 0xabcd] [2 0x6120]])])
  ==
::
++  test-is-key-path
  ;:  weld
    (expect !>((is-key-path:sal keypath-wit)))
    (expect !>((is-key-path:sal ~[[65 0x0]])))
    (expect !>(!(is-key-path:sal ~[[64 0x0] [33 0x0]])))
    (expect !>(!(is-key-path:sal ~)))
  ==
::
++  test-find-spawn
  ;:  weld
    (expect !>(?=(^ (find-spawn:sal spawn-sg))))
    (expect !>(?=(^ (find-spawn:sal [%batch ~[noop-sg spawn-sg]]))))
    (expect !>(?=(~ (find-spawn:sal noop-sg))))
  ==
::
++  test-spawn-first-ok
  ;:  weld
    (expect !>((spawn-first-ok:sal ~[link0 link1])))
    ::  spawn in link 1
    %-  expect  !>
    !(spawn-first-ok:sal ~[(mk-link ~[noop-rs]) (mk-link ~[spawn-rs])])
    ::  spawn not first in its batch
    %-  expect  !>
    !(spawn-first-ok:sal ~[(mk-link ~[(mk-rs who [%batch ~[noop-sg spawn-sg]])])])
    ::  two spawns
    %-  expect  !>
    !(spawn-first-ok:sal ~[link0 (mk-link ~[spawn-rs])])
  ==
::
++  test-tracked-ok
  =/  tip=sont:ord  [0xaaa 0 0]
  =/  ent=(list sont:ord)  ~[[0xbbb 1 2]]
  ;:  weld
    (expect !>((tracked-ok:sal tip tip ent)))
    (expect !>((tracked-ok:sal [0xbbb 1 2] tip ent)))
    (expect !>(!(tracked-ok:sal [0xbbb 1 3] tip ent)))
    (expect !>(!(tracked-ok:sal [0xccc 0 0] tip ent)))
  ==
::
::  ==========================================================================
::  Replay
::  ==========================================================================
++  test-replay-spawn
  =/  res  (replay ~[(mk-link ~[spawn-rs])])
  ;:  weld
    (expect !>(?=(^ res)))
    %+  expect-eq
      !>  `point:urb`[[[0xfeed 0 0] ~] 0 1 pas [%.n who] ~ ~]
      !>  (need res)
  ==
::
++  test-replay-keys
  =/  pas2  pub:ex:(cric-key 'second' 0x1)
  =/  res
    (replay ~[(mk-link ~[spawn-rs]) (mk-link ~[(mk-rs who [%keys pas2 |])])])
  %+  expect-eq
    !>  [2 0 pas2]
    !>  [life.net rift.net pass.net]:(need res)
::
++  test-replay-keys-breach
  =/  pas2  pub:ex:(cric-key 'second' 0x1)
  =/  res
    (replay ~[(mk-link ~[spawn-rs]) (mk-link ~[(mk-rs who [%keys pas2 &])])])
  %+  expect-eq
    !>  [2 1 pas2]
    !>  [life.net rift.net pass.net]:(need res)
::
++  test-replay-no-op
  =/  res  (replay ~[(mk-link ~[spawn-rs]) (mk-link ~[noop-rs])])
  %+  expect-eq
    !>  `point:urb`[[[0xfeed 0 0] ~] 0 1 pas [%.n who] ~ ~]
    !>  (need res)
::
++  test-replay-escape
  =/  parent  ~marzod
  ;:  weld
    ::  no sig: escape goes pending
    =/  res
      (replay ~[(mk-link ~[spawn-rs]) (mk-link ~[(mk-rs who [%escape parent ~])])])
    %+  expect-eq
      !>  [`parent [%.n who]]
      !>  [escape.net sponsor.net]:(need res)
    ::  with a (XX unverified) sponsor sig: sponsor set, escape cleared
    =/  res
      (replay ~[(mk-link ~[spawn-rs]) (mk-link ~[(mk-rs who [%escape parent `0x1])])])
    %+  expect-eq
      !>  [~ [%.y parent]]
      !>  [escape.net sponsor.net]:(need res)
    ::  matching cancel-escape clears it
    =/  res
      %-  replay
      :~  (mk-link ~[spawn-rs])
          (mk-link ~[(mk-rs who [%escape parent ~])])
          (mk-link ~[(mk-rs who [%cancel-escape parent])])
      ==
    %+  expect-eq
      !>  `(unit @p)`~
      !>  escape.net:(need res)
  ==
::
++  test-replay-batch-abort
  ::  a failed precondition (%cancel-escape with nothing pending) drops the
  ::  REST of that raw-sotx's singles, mirroring process-unv: the %fief in
  ::  the same batch must NOT be applied
  =/  fef  `(unit fief:urb)``[%if .127.0.0.1 1.234]
  =/  res
    %-  replay
    :~  (mk-link ~[spawn-rs])
        (mk-link ~[(mk-rs who [%batch ~[[%cancel-escape ~marzod] [%fief fef]]])])
    ==
  ;:  weld
    (expect !>(?=(^ res)))
    %+  expect-eq
      !>  `(unit fief:urb)`~
      !>  fief.net:(need res)
  ==
::
++  test-replay-batch-order
  ::  flattened in order: the later %fief wins
  =/  f1  `(unit fief:urb)``[%if .127.0.0.1 1]
  =/  f2  `(unit fief:urb)``[%if .127.0.0.2 2]
  =/  res
    %-  replay
    :~  (mk-link ~[spawn-rs])
        (mk-link ~[(mk-rs who [%batch ~[[%fief f1] [%fief f2]]])])
    ==
  %+  expect-eq
    !>  f2
    !>  fief.net:(need res)
::
++  test-replay-adopt-self
  =/  res
    %-  replay
    :~  (mk-link ~[spawn-rs])
        (mk-link ~[(mk-rs who [%escape ~marzod ~])])
        (mk-link ~[(mk-rs who [%adopt who])])
    ==
  %+  expect-eq
    !>  [~ [%.y who]]
    !>  [escape.net sponsor.net]:(need res)
::
++  test-replay-failures
  ;:  weld
    ::  no spawn
    (expect !>(?=(~ (replay ~[(mk-link ~[noop-rs])]))))
    ::  double spawn
    (expect !>(?=(~ (replay ~[(mk-link ~[spawn-rs]) (mk-link ~[spawn-rs])]))))
    ::  op before spawn
    (expect !>(?=(~ (replay ~[(mk-link ~[noop-rs spawn-rs])]))))
    ::  a foreign-ship sotx is skipped, not fatal (and changes nothing)
    =/  res
      (replay ~[(mk-link ~[spawn-rs]) (mk-link ~[(mk-rs ~marzod [%keys 0x1 |])])])
    %+  expect-eq
      !>  1
      !>  life.net:(need res)
  ==
::
::  ==========================================================================
::  Link verification
::  ==========================================================================
++  test-link-checks-valid
  ;:  weld
    ::  non-genesis: continuity/key-path against prev, then own-output checks
    =/  [cs=(list check:sa) nx=(unit [vout=@ud off=@ud])]
      (link-checks:sal who 0 link1 c0-tx c1-tx [0 0] %.n)
    ;:  weld
      (expect !>((levy cs |=(c=check:sa ok.c))))
      %+  expect-eq
        !>  `(unit [vout=@ud off=@ud])``[0 0]
        !>  nx
    ==
    ::  genesis: own-output checks only, against the landing from check-spawn
    =/  [cs=(list check:sa) nx=(unit [vout=@ud off=@ud])]
      (link-checks:sal who 0 link0 pre-tx c0-tx [0 0] %.y)
    ;:  weld
      (expect !>((levy cs |=(c=check:sa ok.c))))
      %+  expect-eq
        !>  `(unit [vout=@ud off=@ud])``[0 0]
        !>  nx
    ==
  ==
::
++  test-link-checks-tampered
  ;:  weld
    ::  wrong internal key -> commitment of the OWN output fails
    =/  l  link1
    =/  [cs=(list check:sa) nx=(unit [vout=@ud off=@ud])]
      (link-checks:sal who 0 l(internal-key.reveal (mk-ikey 99)) c0-tx c1-tx [0 0] %.n)
    (expect !>(!(got-check-in cs 'link-0-commitment')))
    ::  two-element witness -> not key-path
    =/  t  c1-tx
    =/  [cs=(list check:sa) nx=(unit [vout=@ud off=@ud])]
      %:  link-checks:sal  who  0  link1  c0-tx
          t(is ~[(mk-input c0-id 0 ~[[64 0x0] [33 0x0]])])
          [0 0]  %.n
      ==
    (expect !>(!(got-check-in cs 'link-0-key-path')))
    ::  claimed sots don't match the leaf
    =/  l  link1
    =/  [cs=(list check:sa) nx=(unit [vout=@ud off=@ud])]
      (link-checks:sal who 0 l(sots ~[(mk-rs who [%fief ~])]) c0-tx c1-tx [0 0] %.n)
    (expect !>(!(got-check-in cs 'link-0-sots-match')))
    ::  empty sots -> the no-gaps rule fails
    =/  l  link1
    =/  [cs=(list check:sa) nx=(unit [vout=@ud off=@ud])]
      (link-checks:sal who 0 l(sots ~) c0-tx c1-tx [0 0] %.n)
    (expect !>(!(got-check-in cs 'link-0-sots-nonempty')))
    ::  foreign-ship sotx -> not self-enacted
    =/  l  link1
    =/  [cs=(list check:sa) nx=(unit [vout=@ud off=@ud])]
      (link-checks:sal who 0 l(sots ~[(mk-rs ~marzod noop-sg)]) c0-tx c1-tx [0 0] %.n)
    (expect !>(!(got-check-in cs 'link-0-sots-ship')))
    ::  input 0 spending an output other than the sat-carrying one -> fork
    =/  t  c1-tx
    =/  [cs=(list check:sa) nx=(unit [vout=@ud off=@ud])]
      %:  link-checks:sal  who  0  link1  c0-tx
          t(is ~[(mk-input c0-id 1 keypath-wit)])
          [0 0]  %.n
      ==
    (expect !>(!(got-check-in cs 'link-0-continuity')))
    ::  no inputs at all -> the input-0 stipulation cannot be met
    =/  t  c1-tx
    =/  [cs=(list check:sa) nx=(unit [vout=@ud off=@ud])]
      (link-checks:sal who 0 link1 c0-tx t(is ~) [0 0] %.n)
    (expect !>(!(got-check-in cs 'link-0-input-zero')))
  ==
::
++  test-link-checks-offset
  ::  prev holds the sat in output 1 at offset 2; input 0 carries no prior
  ::  input value, so the landing index in this tx is just that offset: the
  ::  outputs are [2 5], so index 2 falls in output 1 at offset 0 -- where
  ::  the link's own commitment must sit
  =/  prev
    %^    mk-tx
        0xaaaa
      ~[(mk-input 0x9 0 ~)]
    ~[[(p2tr-spk 0x1) 3] [(p2tr-spk 0x4) 10]]
  =/  this
    %^    mk-tx
        0xbbbb
      ~[(mk-input 0xaaaa 1 keypath-wit)]
    ~[[(p2tr-spk 0x2) 2] [(p2tr-spk q1) 5]]
  =/  lnk=link:sa  [0xbbbb 0x0 [ikey1 noop-leaf] ~[noop-rs]]
  ;:  weld
    =/  [cs=(list check:sa) nx=(unit [vout=@ud off=@ud])]
      (link-checks:sal who 0 lnk prev this [1 2] %.n)
    ;:  weld
      (expect !>((levy cs |=(c=check:sa ok.c))))
      %+  expect-eq
        !>  `(unit [vout=@ud off=@ud])``[1 0]
        !>  nx
    ==
    ::  landing past the outputs (miner fee) -> sat-landed fails
    =/  fee-tx
      %^    mk-tx
          0xbbbb
        ~[(mk-input 0xaaaa 1 keypath-wit)]
      ~[[(p2tr-spk 0x2) 1] [(p2tr-spk 0x3) 1]]
    =/  [cs=(list check:sa) nx=(unit [vout=@ud off=@ud])]
      (link-checks:sal who 0 lnk prev fee-tx [1 2] %.n)
    ;:  weld
      (expect !>(!(got-check-in cs 'link-0-sat-landed')))
      (expect !>(?=(~ nx)))
    ==
  ==
::
::  ==========================================================================
::  Genesis and the full pure pipeline
::  ==========================================================================
++  test-check-spawn
  =/  [cs=(list check:sa) carried=(unit [vout=@ud off=@ud])]
    (check-spawn:sal good-sat pre-tx c0-tx)
  ;:  weld
    (expect !>((levy cs |=(c=check:sa ok.c))))
    %+  expect-eq
      !>  `(unit [vout=@ud off=@ud])``[0 0]
      !>  carried
  ==
::
++  test-check-spawn-fig-mismatch
  =/  s  good-sat
  =/  [cs=(list check:sa) carried=(unit [vout=@ud off=@ud])]
    (check-spawn:sal s(who ~marzod) pre-tx c0-tx)
  ;:  weld
    (expect !>(!(got-check-in cs 'spawn-fig')))
    (expect !>(?=(~ carried)))
  ==
::
++  test-check-spawn-not-spending
  ::  a spawn commit whose input 0 does not spend the attested precommit
  ::  output: the shared-controller proof fails (comets are not mintable
  ::  off other people's precommits)
  =/  bad-c0  (mk-tx c0-id ~[(mk-input 0x9999 0 ~)] ~[[(p2tr-spk q0) 9.500]])
  =/  [cs=(list check:sa) carried=(unit [vout=@ud off=@ud])]
    (check-spawn:sal good-sat pre-tx bad-c0)
  ;:  weld
    (expect !>(!(got-check-in cs 'spawn-spends-precommit')))
    (expect !>(?=(~ carried)))
  ==
::
++  test-run-checks-valid
  =/  res  (run-good good-sat ~)
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
++  test-run-checks-spawn-only
  ::  a bare %spawn is a valid one-link keyfile: the spawn commit's own
  ::  output is the tip, and its leaf is enacted directly
  =/  res  (run-checks:sal spawn-sat ~[c0-tx] pre-tx `%.y ~)
  ;:  weld
    (expect !>(ok.verdict.res))
    %+  expect-eq
      !>  9.500
      !>  tip-value.res
    %+  expect-eq
      !>  `(unit point:urb)``[[[c0-id 0 0] ~] 0 1 pas [%.n who] ~ ~]
      !>  point.res
  ==
::
++  test-run-checks-keys-tip
  ::  the TIP leaf's sotx is enacted at packet-verification time (no
  ::  one-link lag): a %keys commit at the tip must already show in the
  ::  replayed point
  =/  res  (run-checks:sal keys-sat ~[c0-tx keys-tx] pre-tx `%.y ~)
  ;:  weld
    (expect !>(ok.verdict.res))
    %+  expect-eq
      !>  [2 keys-pas]
      !>  [life.net pass.net]:(need point.res)
  ==
::
++  test-run-checks-bad-tip
  =/  s  good-sat
  =/  res  (run-good s(tip [c1-id 0 5]) ~)
  ;:  weld
    (expect !>(!ok.verdict.res))
    (expect !>(!(got-check verdict.res 'tip-sont')))
  ==
::
++  test-run-checks-tracked
  ;:  weld
    ::  verifier holds link 0's own landing (an interior link): it is
    ::  behind -> ok
    (expect !>(ok.verdict:(run-good good-sat `[c0-id 0 0])))
    ::  verifier holds the tip: up to date -> ok
    (expect !>(ok.verdict:(run-good good-sat `[c1-id 0 0])))
    ::  verifier holds an unrelated sont: divergence -> fail
    =/  res  (run-good good-sat `[0xdead 0 0])
    ;:  weld
      (expect !>(!ok.verdict.res))
      (expect !>(!(got-check verdict.res 'tracked-tip')))
    ==
  ==
--
