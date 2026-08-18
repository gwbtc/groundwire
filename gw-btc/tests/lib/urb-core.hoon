::  tests/lib/urb-core.hoon
::
::  Current-protocol (kelvin-9) vectors for the OP_RETURN scanner in
::  lib/urb-core.  +find-block-reveals discovers public identities by
::  grepping outputs for the OP_RETURN "urb" publication (and follows
::  tracked-sat spends); +process-publication turns each publication into
::  a %claim -- the comet's own attestation packet, with the log
::  completed by the transaction that carried it -- and +update-sonts
::  follows sat movement.  Real secp via cric/taproot.
::
::  The scanner NO LONGER JUDGES a publication and no longer indexes one.
::  It cannot: a custody log names transactions in blocks it has already
::  streamed past.  So the tests here prove two things and hand the third
::  to ++run-checks, which is the whole point of the change:
::
::    1. what the scanner emits (one %claim, whatever it already knows
::       about the comet, and nothing written to the index), and
::    2. that the completed log is exactly the packet a peer would get,
::       so that
::    3. ++run-checks -- the SAME arm a mailed attestation goes through,
::       driven here at the same pure boundary +verify-lc drives --
::       accepts it, INCLUDING the late reveal that the old
::       ++apply-spawn gate made impossible.
::
/-  ord, urb, bitcoin, sa=self-attestation
/+  *test, ul=urb-core, ol=ord, cc=gw-btc-pass, tr=taproot, lsa=self-attestation
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
::  a one-item 64-byte witness: the shape a taproot key-path spend has
::
++  keypath-wit  `(list hexb:bitcoin)`~[[64 0x0]]
++  mk-inputw
  |=  [=txid:ord pos=@ud wit=(list hexb:bitcoin)]
  ^-  inputw:tx:bitcoin
  [wit txid pos [4 0xffff.ffff] ~ ~]
++  mk-tx
  |=  [id=@ux is=(list inputw:tx:bitcoin) os=(list output:tx:bitcoin)]
  ^-  tx:bitcoin
  [id is os 0 1 ~]
++  coinbase
  ^-  tx:bitcoin
  (mk-tx 0xc0.1bba ~[(mk-inputw 0x0 4.294.967.295 ~)] ~[[[25 0x76.a914.88ac] 50.000.000]])
++  effs
  |=  fx=(list [id:block:bitcoin effect:urb])
  ^-  (list effect:urb)
  (turn fx |=([* e=effect:urb] e))
::  +claims: the %claim effects a scan produced
::
++  claims
  |=  fx=(list [id:block:bitcoin effect:urb])
  ^-  (list [who=ship =pass])
  %+  murn  (effs fx)
  |=  e=effect:urb
  ^-  (unit [ship pass])
  ?.(?=([%claim *] e) ~ `[who.e pass.e])
::  run one block through the full scanner pipeline.  .st's cursor must
::  sit one below the block's height: ++handle-block increments it, and
::  that incremented value is the height a %claim's completed entry
::  carries.
::
++  scan
  |=  [st=state:urb =block:bitcoin]
  ^-  [(list [id:block:bitcoin effect:urb]) state:urb]
  =/  oc   (abed:urb-core:ul st)
  =/  fbr  (find-block-reveals:oc block)
  ::  exactly what +get-blocks does, and it matters: .hax is the block
  ::  under scan, and ++update-comet stamps it onto the point it moves as
  ::  provenance.  (++handle-block advances .num itself.)
  ::
  =.  oc   oc(hax.block-id.state hax.block)
  =/  ub   (apply-prevouts-and-urbify:oc +.fbr -.fbr)
  abet:(handle-block:oc ub)
::  a fresh index whose cursor sits just below .h
::
++  st-below
  |=  h=@ud
  ^-  state:urb
  [[0xb.10c0 (dec h)] *sont-map:ord *insc-ids:ord *unv-ids:urb]
::  ---- identity ------------------------------------------------------
++  seed   'urb-core-comet'
++  fund   ^-(sont:ord [0xf00d 0 0])
++  dat    (make-dat:cc fund)
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
::  ---- the chain this comet actually walked ---------------------------
::
::  699  fund-tx     an ordinary wallet UTXO; output 0 carries the sat
::  700  spawn-tx    entry 0: spends it, commits snap0, PUBLISHES
::  701  move-tx     entry 1: a silent key-path custody move
::  702  late-tx     entry 2: spends the moved sat, commits snap1, and
::                   PUBLISHES -- long after the spawn, from a satpoint
::                   that is nowhere near the funding one
::
++  snap0     ^-(snapshot:sa [life=1 rift=0 key=cry sponsor=~ fief=~])
++  snap1     ^-(snapshot:sa [life=2 rift=0 key=cry sponsor=~ fief=~])
++  ikey0     (mk-ikey 11)
++  ikey1     (mk-ikey 13)
++  ikey2     (mk-ikey 17)
++  spawn-open  ^-(spawn-opening:sa [fund start-height=699])
++  opening0  ^-(opening:sa [ikey0 snap0 `spawn-open])
++  opening2  ^-(opening:sa [ikey2 snap1 ~])
::
++  fund-id   0xf00d
++  spawn-id  0x5.9a17
++  move-id   0x3.0edd
++  late-id   0x1a.7e00
::
++  fund-tx
  ^-  tx:bitcoin
  (mk-tx fund-id ~[(mk-inputw 0x9999 0 ~)] ~[[(p2tr-spk (mk-ikey 5)) 10.000]])
::  the spawn publication: an EMPTY custody log (this transaction is
::  entry 0), plus the opening for the hop it performs.
::
++  spawn-pub  ^-(publication:sa [(pass-of 0) opening0])
++  spawn-tx
  ^-  tx:bitcoin
  %:  mk-tx  spawn-id
    ~[(mk-inputw fund-id 0 ~)]
    ~[[(state-spk ikey0 snap0) 9.500] [(make-publication:cc spawn-pub) 0]]
  ==
++  spawn-block  ^-(block:bitcoin [0xb.10c1 0 700 ~[coinbase spawn-tx]])
::  a spawn whose sat-carrying output commits a DIFFERENT internal key
::  than the opening reveals.  The scanner still emits the claim -- it
::  does not check commitments any more -- and ++run-checks refuses it.
::
++  bad-spawn-tx
  ^-  tx:bitcoin
  %:  mk-tx  spawn-id
    ~[(mk-inputw fund-id 0 ~)]
    ~[[(state-spk (mk-ikey 99) snap0) 9.500] [(make-publication:cc spawn-pub) 0]]
  ==
++  bad-spawn-block  ^-(block:bitcoin [0xb.10c1 0 700 ~[coinbase bad-spawn-tx]])
::  entry 1: a silent custody move.  No publication, so nothing on chain
::  connects this transaction to the comet except the custody log.
::
++  move-tx
  ^-  tx:bitcoin
  %:  mk-tx  move-id
    ~[(mk-inputw spawn-id 0 keypath-wit)]
    ~[[(p2tr-spk (output-pubkey:tr ikey1 ~)) 9.400]]
  ==
++  move-block  ^-(block:bitcoin [0xb.10c2 0 701 ~[coinbase move-tx]])
::  the LATE publication.  Its pass carries the log for entries 0 and 1;
::  entry 2 is this transaction, which the payload cannot name.
::
++  carried-log
  ^-  custody-log:sa
  ~[[spawn-id 700 `opening0] [move-id 701 ~]]
++  late-pub  ^-(publication:sa [(pass-of (jam carried-log)) opening2])
++  late-tx
  ^-  tx:bitcoin
  %:  mk-tx  late-id
    ~[(mk-inputw move-id 0 keypath-wit)]
    ~[[(state-spk ikey2 snap1) 9.300] [(make-publication:cc late-pub) 0]]
  ==
++  late-block  ^-(block:bitcoin [0xb.10c3 0 702 ~[coinbase late-tx]])
::  the state a verified CONFIDENTIAL attestation leaves behind
::  (+apply-verified in app/gw-btc.hoon): the comet is in unv-ids and its
::  identity sat is tracked in sont-map.
::
++  tracked-point
  ^-  point:urb
  [[[move-id 0 0] ~] [0 1 (pass-of 0) [%.n who] ~ ~] `0xb.10c1]
++  tracked-state
  ^-  state:urb
  :*  [0xb.10c2 701]
      (put-com:si:ol *sont-map:ord move-id 0 0 9.400 who)
      *insc-ids:ord
      (~(put by *unv-ids:urb) who tracked-point)
  ==
::  ---- helpers -------------------------------------------------------
::
::  decode a claimed pass back into the self-attestation a peer would get
::
++  claimed-sat
  |=  fx=(list [id:block:bitcoin effect:urb])
  ^-  (unit self-attestation:sa)
  =/  cs  (claims fx)
  ?~  cs  ~
  (from-xtr:lsa who.i.cs pass.i.cs)
::  run the SAME pure verifier a mailed attestation goes through
::
++  verify
  |=  [sat=self-attestation:sa start=tx:bitcoin txl=(list tx:bitcoin)]
  ^-  result:sa
  (run-checks:lsa sat start txl `%.y ~ *(set ship) ~)
++  got-check
  |=  [v=verdict:sa name=cord]
  ^-  ?
  =/  cs  checks.v
  |-
  ?~  cs  %.n
  ?:  =(name.i.cs name)  ok.i.cs
  $(cs t.cs)
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
::  SAME-BLOCK CHAINING.  +find-block-reveals reads each input's prevout
::  value out of .sont-map, which is the index as it stood at the START
::  of the block -- its only writer, +handle-block, runs after the whole
::  pipeline.  So a satpoint created EARLIER IN THIS SAME BLOCK is not
::  there, and reading it as 0 is not an understated fee: +handle-tx sums
::  these into .running-value, so a tracked sat at a LATER input index
::  lands at the wrong offset.
::
::  With two tracked comets in one transaction that is a COLLISION --
::  both resolve to the same satpoint, .sont-map keeps one, and the
::  other's association is destroyed while .unv-ids still points at it.
::
::  Nothing in the tree can build this: every builder puts the identity
::  sat at input 0 (assert_identity_input_zero refuses otherwise), and
::  putting a tracked sat behind another input means spending it, so no
::  third party can induce it.  That is exactly why no arm caught it --
::  the eleven here passed before the fix and after it.
::
++  test-same-block-chain-does-not-collide-two-comets
  =/  her=@p    ~nec
  =/  a-id      0xaa.0001
  =/  b-id      0xbb.0002
  ::  Two tracked comets, each alone on its own sat -- and each with a
  ::  POINT as well as a sat.  Both halves are required: +update-sonts
  ::  reaches +update-comet for any moved sat whose .sont-val names a
  ::  comet, and that arm does `~(got by unv-ids)`, which bails on a
  ::  missing key.  Seeding only sont-map crashes the arm inside the
  ::  library before any assertion runs -- on a code path the fix does
  ::  not touch, so it crashes identically fixed or unfixed and pins
  ::  nothing at all.
  ::
  =/  pt-who=point:urb
    [[[0xaaaa 0 0] ~] [0 1 (pass-of 0) [%.n who] ~ ~] `0xb.10c0]
  =/  pt-her=point:urb
    [[[0xbbbb 0 0] ~] [0 1 (pass-of 0) [%.n her] ~ ~] `0xb.10c0]
  =/  seeded=state:urb
    :*  [0xb.10c0 700]
        %:  put-com:si:ol
          (put-com:si:ol *sont-map:ord 0xaaaa 0 0 9.400 who)
          0xbbbb  0  0  5.000  her
        ==
        *insc-ids:ord
        %+  ~(put by (~(put by *unv-ids:urb) who pt-who))
          her  pt-her
    ==
  ::  A moves who's sat.  B, IN THE SAME BLOCK, spends A's output at
  ::  input 0 and her's sat at input 1 -- so her's offset depends
  ::  entirely on A's output value, which is the value that was read
  ::  as 0.
  =/  a-tx
    %:  mk-tx  a-id
      ~[(mk-inputw 0xaaaa 0 keypath-wit)]
      ~[[[25 0x76.a914.88ac] 9.400]]
    ==
  =/  b-tx
    %:  mk-tx  b-id
      ~[(mk-inputw a-id 0 keypath-wit) (mk-inputw 0xbbbb 0 keypath-wit)]
      ~[[[25 0x76.a914.88ac] 14.400]]
    ==
  =/  chained  ^-(block:bitcoin [0xb.10c1 0 701 ~[coinbase a-tx b-tx]])
  =/  [* st=state:urb]  (scan seeded chained)
  ;:  weld
    ::  who rides input 0 through both transactions and stays at offset 0
    (expect-eq !>(`who) !>((get-com:si:ol sont-map.st b-id 0 0)))
    ::  her sits BEHIND it, at A's output value -- not on top of it.
    ::  Reading A's output as 0 puts her at offset 0 too, and one of the
    ::  two comets stops existing.
    (expect-eq !>(`her) !>((get-com:si:ol sont-map.st b-id 0 9.400)))
    ::  and they really are two distinct sats, not one overwritten twice
    (expect !>(!=(who her)))
  ==
::  ---- what a publication now produces -------------------------------
::
::  ONE %claim, and NOTHING in the index.  The scanner used to write a
::  point here, from checks it reimplemented itself; it now hands the
::  packet to the verifier and touches nothing.
::
++  test-publication-emits-a-claim-and-indexes-nothing
  =/  [fx=(list [id:block:bitcoin effect:urb]) st=state:urb]
    (scan (st-below 700) spawn-block)
  ;:  weld
    (expect-eq !>(1) !>((lent (claims fx))))
    (expect-eq !>(`who) !>(?~(cs=(claims fx) ~ `who.i.cs)))
    ::  no point, no sat association: the scanner asserts nothing
    ::
    (expect !>(?=(~ (~(get by unv-ids.st) who))))
    (expect-eq !>(~) !>((get-com:si:ol sont-map.st spawn-id 0 0)))
  ==
::
::  The claim carries the packet, completed with the carrying
::  transaction.  This is the one thing the payload could not say and
::  the block reader can.
::
++  test-claim-completes-the-log-with-its-own-transaction
  =/  [fx=(list [id:block:bitcoin effect:urb]) *]
    (scan (st-below 700) spawn-block)
  =/  sat  (claimed-sat fx)
  ;:  weld
    (expect !>(?=(^ sat)))
    %+  expect-eq
      !>  ^-  custody-log:sa
          ~[[spawn-id 700 `opening0]]
      !>  ?~(sat ~ chain.u.sat)
    ::  the name is untouched by the completion: xtr is outside the tweak
    ::
    (expect-eq !>(who) !>(?~(sat *@p who.u.sat)))
  ==
::
::  A publication for a comet the scanner ALREADY TRACKS produces the
::  same claim as one for a stranger.  There is no tracked/untracked
::  distinction left to make -- that was the three-way branch.
::
++  test-tracked-and-stranger-produce-the-same-claim
  =/  [stranger=(list [id:block:bitcoin effect:urb]) *]
    (scan (st-below 702) late-block)
  =/  [tracked=(list [id:block:bitcoin effect:urb]) *]
    (scan tracked-state late-block)
  (expect-eq !>((claims stranger)) !>((claims tracked)))
::
::  A pass whose xtr is not a canonical custody log is refused outright:
::  re-encoding it would launder a non-canonical tail into a claim that
::  looks well-formed.
::
++  test-uncueable-xtr-emits-no-claim
  =/  junk-pub  ^-(publication:sa [(pass-of 0xdead.beef.dead.beef) opening0])
  =/  junk-tx
    ^-  tx:bitcoin
    %:  mk-tx  spawn-id
      ~[(mk-inputw fund-id 0 ~)]
      ~[[(state-spk ikey0 snap0) 9.500] [(make-publication:cc junk-pub) 0]]
    ==
  =/  [fx=(list [id:block:bitcoin effect:urb]) st=state:urb]
    (scan (st-below 700) [0xb.10c1 0 700 ~[coinbase junk-tx]])
  ;:  weld
    (expect-eq !>(~) !>((claims fx)))
    (expect !>(?=(~ (~(get by unv-ids.st) who))))
  ==
::  ---- the claim, verified the way a packet is verified ---------------
::
++  test-spawn-publication-verifies
  =/  [fx=(list [id:block:bitcoin effect:urb]) *]
    (scan (st-below 700) spawn-block)
  =/  sat  (claimed-sat fx)
  ?~  sat  (expect !>(%.n))
  =/  res  (verify u.sat fund-tx ~[spawn-tx])
  ;:  weld
    (expect !>(ok.verdict.res))
    (expect !>((got-check verdict.res 'spawn-matches')))
    (expect !>((got-check verdict.res 'entry-0-commitment')))
    (expect !>(?=(^ point.res)))
  ==
::
::  THE LATE REVEAL.  A comet the watcher has NEVER tracked, whose sat
::  has moved since the spawn, publishes -- and is accepted.
::
::  This never once worked.  ++process-publication used to route it to
::  ++apply-spawn (it carries a spawn-opening and we track no point for
::  it), and ++apply-spawn demanded that input 0 BE the spawn satpoint.
::  It is not: this transaction spends the sat's CURRENT home, two hops
::  along.  The other branch refused it too, because a stranger has no
::  tracked tip to check continuity against.  Nothing in a publication
::  could bridge that gap, because the bridge is the custody log -- and
::  the custody log is now in the payload.
::
++  test-late-reveal-from-a-stranger-verifies
  =/  [fx=(list [id:block:bitcoin effect:urb]) st=state:urb]
    (scan (st-below 702) late-block)
  ::  the watcher has never heard of this comet
  ::
  =/  never-tracked  ?=(~ (~(get by unv-ids.st) who))
  =/  sat  (claimed-sat fx)
  ?~  sat  (expect !>(%.n))
  =/  res  (verify u.sat fund-tx ~[spawn-tx move-tx late-tx])
  ;:  weld
    (expect !>(never-tracked))
    ::  three hops: the two the payload carried, and this transaction
    ::
    (expect-eq !>(3) !>((lent chain.u.sat)))
    (expect !>(ok.verdict.res))
    ::  the dat commitment still binds the name to the SPAWN satpoint,
    ::  which is where the walk starts
    ::
    (expect !>((got-check verdict.res 'spawn-matches')))
    ::  and every hop between there and here checked out
    ::
    (expect !>((got-check verdict.res 'entry-1-continuity')))
    (expect !>((got-check verdict.res 'entry-2-continuity')))
    (expect !>((got-check verdict.res 'entry-2-commitment')))
    ::  the point is built at the tip this transaction created
    ::
    %+  expect-eq
      !>(`[late-id 0 0])
      !>(?~(point.res ~ `sont.own.u.point.res))
    (expect-eq !>(`2) !>(?~(point.res ~ `life.net.u.point.res)))
  ==
::
::  ... and the constraint that used to reject it really was violated:
::  input 0 of the late transaction is nowhere near the spawn satpoint.
::
++  test-late-reveal-does-not-spend-the-spawn-satpoint
  =/  ltx=tx:bitcoin  late-tx
  =/  fnd=sont:ord    fund
  =/  in0  (snag 0 is.ltx)
  (expect !>(!=([txid vout]:fnd [txid pos]:in0)))
::
::  A publication whose committed output does not match the opening is
::  emitted as a claim -- the scanner does not judge -- and then REFUSED
::  by the same ++run-checks, on the named check.
::
++  test-mismatched-commitment-is-refused-by-run-checks
  =/  [fx=(list [id:block:bitcoin effect:urb]) *]
    (scan (st-below 700) bad-spawn-block)
  =/  sat  (claimed-sat fx)
  ?~  sat  (expect !>(%.n))
  =/  res  (verify u.sat fund-tx ~[bad-spawn-tx])
  ;:  weld
    (expect-eq !>(1) !>((lent (claims fx))))
    (expect !>(!ok.verdict.res))
    (expect !>(!(got-check verdict.res 'entry-0-commitment')))
    (expect !>(?=(~ point.res)))
  ==
::  ---- update-sonts -------------------------------------------------
++  test-update-sonts-follows-sat
  ::  a plain move relocates a tracked comet's sat and emits %xfer.  The
  ::  ordinal tracker is the scanner's own job and is untouched by any of
  ::  this; the point it moves is one the VERIFIER installed.
  ::
  ::  the point as the verifier left it: last observed in the SPAWN block.
  ::
  =/  before=point:urb
    [[[spawn-id 0 0] ~] [0 1 (pass-of 0) [%.n who] ~ ~] `0xb.10c1]
  =/  seeded=state:urb
    :*  [0xb.10c1 700]
        (put-com:si:ol *sont-map:ord spawn-id 0 0 9.500 who)
        *insc-ids:ord
        (~(put by *unv-ids:urb) who before)
    ==
  =/  [fx=(list [id:block:bitcoin effect:urb]) st=state:urb]
    (scan seeded move-block)
  =/  pt  (need (~(get by unv-ids.st) who))
  ;:  weld
    (expect-eq !>([move-id 0 0]) !>(sont.own.pt))
    ::  A CUSTODY MOVE IS AN OBSERVATION, so it REFRESHES the provenance:
    ::  .seen was the spawn block (0xb.10c1) and is now the block this
    ::  move was found in (0xb.10c2, +move-block).  It is the most recent
    ::  evidence for the point, not its origin -- which is the whole
    ::  distinction, because it is what a reorg of THIS block invalidates.
    ::
    (expect-eq !>(`(unit @ux)``0xb.10c1) !>(seen.before))
    (expect-eq !>(`(unit @ux)``0xb.10c2) !>(seen.pt))
    ::  the old sat entry no longer names the comet
    ::
    (expect-eq !>(~) !>((get-com:si:ol sont-map.st spawn-id 0 0)))
    (expect-eq !>(`who) !>((get-com:si:ol sont-map.st move-id 0 0)))
    ::  a %xfer effect from the old to the new satpoint was emitted
    ::
    (expect !>((lien (effs fx) |=(e=effect:urb =(e [%xfer [spawn-id 0 0] [move-id 0 0]])))))
  ==
--
