::  tests/lib/urb-core.hoon
::
::  Current-protocol (kelvin-9) vectors for the OP_RETURN scanner in
::  lib/urb-core: +find-block-reveals discovers public identities by
::  grepping outputs for the OP_RETURN "urb" publication (and follows
::  tracked-sat spends); +process-publication's +apply-spawn indexes a
::  public comet and +apply-state advances its snapshot under a life
::  gate; +update-sonts follows sat movement.  Real secp via cric/taproot.
::
/-  ord, urb, bitcoin, sa=self-attestation
/+  *test, ul=urb-core, ol=ord, cc=gw-btc-pass, tr=taproot
=>
|%
++  secp  secp256k1:secp:crypto
++  mk-ikey
  |=  k=@
  ^-  @ux
  (compress-point:secp (mul-point-scalar:secp g:domain:curve:secp k))
++  p2tr-spk
  |=  q=@ux
  ^-  hexb:bitcoin
  [34 `@ux`(can 3 ~[[32 q] [2 0x5120]])]
++  state-spk
  |=  [ikey=@ux snap=snapshot:sa]
  ^-  hexb:bitcoin
  (p2tr-spk (state-key:cc ikey snap))
++  mk-inputw
  |=  [=txid:ord pos=@ud]
  ^-  inputw:tx:bitcoin
  [~ txid pos [4 0xffff.ffff] ~ ~]
++  mk-tx
  |=  [id=@ux is=(list inputw:tx:bitcoin) os=(list output:tx:bitcoin)]
  ^-  tx:bitcoin
  [id is os 0 1 ~]
++  coinbase
  ^-  tx:bitcoin
  (mk-tx 0xc0.1bba ~[(mk-inputw 0x0 4.294.967.295)] ~[[[25 0x76.a914.88ac] 50.000.000]])
++  effs
  |=  fx=(list [id:block:bitcoin effect:urb])
  ^-  (list effect:urb)
  (turn fx |=([* e=effect:urb] e))
::  run one block through the full scanner pipeline
::
++  scan
  |=  [st=state:urb =block:bitcoin]
  ^-  [(list [id:block:bitcoin effect:urb]) state:urb]
  =/  oc   (abed:urb-core:ul st)
  =/  fbr  (find-block-reveals:oc block)
  =/  ub   (apply-prevouts-and-urbify:oc +.fbr -.fbr)
  abet:(handle-block:oc ub)
::  ---- identity ------------------------------------------------------
++  seed   'urb-core-comet'
++  fund   ^-(sont:ord [0xf00d 0 0])
++  blind  (make-blind:cc seed)
++  dat    (make-dat:cc fund blind)
++  pass-of
  |=  xtr=@
  ^-  pass
  pub:ex:(pit:nu:cric:crypto 512 (shaz seed) %c dat xtr)
++  who
  ^-  @p
  =/  cic  (com:nu:cric:crypto (pass-of 0))
  `@p`fig:ex:cic
++  cry
  ^-  @
  =/  cic  (com:nu:cric:crypto (pass-of 0))
  ?>  ?=(%c suite.+<.cic)
  `@`cry.pub.+<.cic
::  ---- fixtures ------------------------------------------------------
++  snap0     ^-(snapshot:sa [life=1 rift=0 key=cry sponsor=~ fief=~])
++  ikey0     (mk-ikey 11)
++  opening0  ^-(opening:sa [ikey0 snap0 `[fund start-height=700 blind]])
++  pub0      ^-(publication:sa [(pass-of 0) opening0])
++  spawn-id  0x5.9a17
++  spawn-tx
  ^-  tx:bitcoin
  %:  mk-tx  spawn-id
    ~[(mk-inputw 0xf00d 0)]
    ~[[(state-spk ikey0 snap0) 9.500] [(make-publication:cc pub0) 0]]
  ==
++  spawn-block  ^-(block:bitcoin [0xb.10c1 0 700 ~[coinbase spawn-tx]])
::  a spawn whose sat-carrying output commits a DIFFERENT internal key
::  than the opening reveals: the state-key reconstruction must not match.
::
++  bad-spawn-tx
  ^-  tx:bitcoin
  %:  mk-tx  spawn-id
    ~[(mk-inputw 0xf00d 0)]
    ~[[(state-spk (mk-ikey 99) snap0) 9.500] [(make-publication:cc pub0) 0]]
  ==
++  bad-spawn-block  ^-(block:bitcoin [0xb.10c1 0 700 ~[coinbase bad-spawn-tx]])
::  a public state update: input 0 spends the comet's tracked sat, output
::  0 commits snap1 (life 2), and the publication carries no blind-opening.
::
++  snap1     ^-(snapshot:sa [life=2 rift=0 key=cry sponsor=~ fief=~])
++  ikey1     (mk-ikey 13)
++  opening1  ^-(opening:sa [ikey1 snap1 ~])
++  pub1      ^-(publication:sa [(pass-of 1) opening1])
++  state-id  0x5.7a7e
++  state-tx
  ^-  tx:bitcoin
  %:  mk-tx  state-id
    ~[(mk-inputw spawn-id 0)]
    ~[[(state-spk ikey1 snap1) 9.000] [(make-publication:cc pub1) 0]]
  ==
++  state-block  ^-(block:bitcoin [0xb.10c2 0 701 ~[coinbase state-tx]])
::  a state update that does NOT advance life (life stays 1): rejected.
::
++  snap-noadv  ^-(snapshot:sa [life=1 rift=1 key=cry sponsor=~ fief=~])
++  opening-noadv  ^-(opening:sa [ikey1 snap-noadv ~])
++  pub-noadv   ^-(publication:sa [(pass-of 1) opening-noadv])
++  noadv-tx
  ^-  tx:bitcoin
  %:  mk-tx  state-id
    ~[(mk-inputw spawn-id 0)]
    ~[[(state-spk ikey1 snap-noadv) 9.000] [(make-publication:cc pub-noadv) 0]]
  ==
++  noadv-block  ^-(block:bitcoin [0xb.10c2 0 701 ~[coinbase noadv-tx]])
::  a plain custody move (no publication) spending the tracked sat.
::
++  move-id  0x3.0edd
++  move-tx
  ^-  tx:bitcoin
  (mk-tx move-id ~[(mk-inputw spawn-id 0)] ~[[(p2tr-spk (mk-ikey 21)) 9.400]])
++  move-block  ^-(block:bitcoin [0xb.10c2 0 701 ~[coinbase move-tx]])
--
|%
::  ---- find-block-reveals -------------------------------------------
++  test-find-detects-op-return
  =/  oc   (abed:urb-core:ul *state:urb)
  =/  fbr  (find-block-reveals:oc spawn-block)
  ;:  weld
    ::  the spawn's funding input is recorded in the reveals map
    ::
    (expect !>(?=(^ -.fbr)))
    ::  the filtered block keeps the coinbase + the publishing tx
    ::
    (expect-eq !>(2) !>((lent txs.+.fbr)))
  ==
::
++  test-find-detects-tracked-spend
  ::  a tx carrying no publication but spending a tracked sat is still
  ::  relevant (owner-initiated custody move).
  ::
  =/  tracked=state:urb
    :*  [0xb007 699]
        (put-com:si:ol *sont-map:ord spawn-id 0 0 9.500 who)
        *insc-ids:ord
        *unv-ids:urb
    ==
  =/  oc   (abed:urb-core:ul tracked)
  =/  fbr  (find-block-reveals:oc move-block)
  ;:  weld
    (expect !>(?=(^ -.fbr)))
    (expect-eq !>(2) !>((lent txs.+.fbr)))
  ==
::  ---- apply-spawn --------------------------------------------------
++  test-apply-spawn-indexes-public-comet
  =/  [fx=(list [id:block:bitcoin effect:urb]) st=state:urb]
    (scan *state:urb spawn-block)
  =/  pt  (~(get by unv-ids.st) who)
  ;:  weld
    ::  the comet is indexed at its landing satpoint
    ::
    %+  expect-eq
      !>  `[[[spawn-id 0 0] ~] 0 1 (pass-of 0) [%.n who] ~ ~]
      !>  pt
    ::  the sat is recorded as owned by the comet
    ::
    (expect-eq !>(`who) !>((get-com:si:ol sont-map.st spawn-id 0 0)))
    ::  the jael udiffs are emitted, owner first
    ::
    %+  expect-eq
      !>  ^-  (list effect:urb)
          :~  [%point who %owner [spawn-id 0 0]]
              [%point who %sponsor `who]
              [%point who %keys 1 (pass-of 0)]
              [%point who %rift 0]
              [%point who %fief ~]
          ==
      !>  (effs fx)
  ==
::
++  test-apply-spawn-rejects-bad-state-key
  ::  the sat output commits a state-key the opening did not; not indexed
  ::
  =/  [fx=(list [id:block:bitcoin effect:urb]) st=state:urb]
    (scan *state:urb bad-spawn-block)
  ;:  weld
    (expect !>(?=(~ (~(get by unv-ids.st) who))))
    (expect !>(?=(~ (effs fx))))
  ==
::  ---- apply-state --------------------------------------------------
++  test-apply-state-advances-life
  =/  [* st1=state:urb]  (scan *state:urb spawn-block)
  =/  [* st2=state:urb]  (scan st1 state-block)
  =/  pt  (need (~(get by unv-ids.st2) who))
  ;:  weld
    ::  networking fields advanced to snap1
    ::
    (expect-eq !>(2) !>(life.net.pt))
    (expect-eq !>((pass-of 1)) !>(pass.net.pt))
    ::  update-sonts moved the comet's owned sat to the new tx
    ::
    (expect-eq !>([state-id 0 0]) !>(sont.own.pt))
  ==
::
++  test-apply-state-life-gate-rejects
  ::  a publication that does not advance life leaves the networking
  ::  state untouched (life stays 1), even though the sat still moves.
  ::
  =/  [* st1=state:urb]  (scan *state:urb spawn-block)
  =/  [* st2=state:urb]  (scan st1 noadv-block)
  =/  pt  (need (~(get by unv-ids.st2) who))
  ;:  weld
    (expect-eq !>(1) !>(life.net.pt))
    (expect-eq !>((pass-of 0)) !>(pass.net.pt))
  ==
::  ---- update-sonts -------------------------------------------------
++  test-update-sonts-follows-sat
  ::  a plain move relocates the comet's sat and emits %xfer; no publication
  ::
  =/  [* st1=state:urb]     (scan *state:urb spawn-block)
  =/  [fx=(list [id:block:bitcoin effect:urb]) st2=state:urb]
    (scan st1 move-block)
  =/  pt  (need (~(get by unv-ids.st2) who))
  ;:  weld
    (expect-eq !>([move-id 0 0]) !>(sont.own.pt))
    ::  the old sat entry no longer names the comet
    ::
    (expect-eq !>(~) !>((get-com:si:ol sont-map.st2 spawn-id 0 0)))
    (expect-eq !>(`who) !>((get-com:si:ol sont-map.st2 move-id 0 0)))
    ::  a %xfer effect from the old to the new satpoint was emitted
    ::
    (expect !>((lien (effs fx) |=(e=effect:urb =(e [%xfer [spawn-id 0 0] [move-id 0 0]])))))
  ==
--
