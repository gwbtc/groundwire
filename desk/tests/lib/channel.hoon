::  channel.hoon: test channel operations
::
/+  *test
/+  *utilities, channel, psbt
/+  keys=key-generation, secret=commitment-secret
|%
++  one-bitcoin-in-sats
  ^-  sats:bc
  100.000.000
::
++  one-bitcoin-in-msats
  ^-  msats
  (sats-to-msats one-bitcoin-in-sats)
::
++  next-local-commitment
  |=  c=chan
  ^-  commitment
  =+  previous=(~(oldest-unrevoked-commitment channel c) %remote)
  ?~  previous  !!
  =+  local-index=msg-idx.our.u.previous
  =+  local-htlc-index=htlc-idx.our.u.previous
  =+  next-height=+(height.commitments.c)
  =+  secret-and-point=(secret-and-point:channel %local next-height)
  ?>  ?=([%& *] secret-and-point)
  =/  [secret=(unit @) point=point]  +.secret-and-point
  =+  keys=(derive-commitment-keys:channel %local point)
  =+  ^=  commitment-and-state
  %:  ~(next-commitment channel c)
    whose=%local
    our-index=local-index
    our-htlc-index=local-htlc-index
    her-index=update-count.her.updates.c
    her-htlc-index=htlc-count.her.updates.c
    keys=keys
  ==
  ?>  ?=([%& *] commitment-and-state)
  +<.commitment-and-state
::
++  next-remote-commitment
  |=  c=chan
  ^-  commitment
  =+  previous=(~(oldest-unrevoked-commitment channel c) %local)
  ?~  previous  !!
  =+  remote-index=msg-idx.her.u.previous
  =+  remote-htlc-index=htlc-idx.her.u.previous
  =+  commitment-point=next-per-commitment-point.her.config.c
  =+  keys=(derive-commitment-keys:channel %remote commitment-point)
  =+  ^=  commitment-and-state
  %:  ~(next-commitment channel c)
    whose=%remote
    our-index=update-count.our.updates.c
    our-htlc-index=htlc-count.our.updates.c
    her-index=remote-index
    her-htlc-index=remote-htlc-index
    keys=keys
  ==
  ?>  ?=([%& *] commitment-and-state)
  +<.commitment-and-state
::
++  next-commitment-state
  |=  [c=chan whose=owner]
  ^-  commitment
  ?-  whose
    %local   (next-local-commitment c)
    %remote  (next-remote-commitment c)
  ==
::
++  next-commitment
  |=  [c=chan whose=owner]
  ^-  psbt:psbt
  tx:(next-commitment-state c whose)
::
++  latest-commitment
  |=  [c=chan whose=owner]
  ^-  psbt:psbt
  =+  latest=(~(latest-commitment channel c) whose)
  ?~  latest  !!
  tx.u.latest
::
++  oldest-unrevoked-commitment
  |=  [c=chan whose=owner]
  ^-  psbt:psbt
  tx:(need (~(oldest-unrevoked-commitment channel c) whose))
::
++  latest-feerate
  |=  [c=chan whose=owner]
  ^-  sats:bc
  fee-per-kw:(need (~(latest-commitment channel c) whose))
::
++  next-feerate
  |=  [c=chan whose=owner]
  ^-  sats:bc
  fee-per-kw:(next-commitment-state c whose)
::
++  oldest-unrevoked-feerate
  |=  [c=chan whose=owner]
  ^-  sats:bc
  fee-per-kw:(need (~(oldest-unrevoked-commitment channel c) whose))
::
++  make-channel
  |=  $:  initiator=?
          anchor=?
          =local-funding=pubkey
          =remote-funding=pubkey
          =local=basepoints
          =remote=basepoints
          local-csv=blocks
          remote-csv=blocks
          =local-dust-limit=sats:bc
          =remote-dust-limit=sats:bc
          local-amount=msats
          remote-amount=msats
          =first-per-commitment=point
          =next-per-commitment=point
          local-seed=@
          initial-feerate=(unit sats:bc)
      ==
  |^  ^-  chan
  :*
    id=chanid
    state=%open
    funding-output=funding-outpoint
    constraints=constraints
    config=[our=local-cfg her=remote-cfg]
    commitments=[our=~ her=~ height=0]
    updates=[our=*update-log her=*update-log]
    sent-msats=0
    recd-msats=0
    revocations=*revocation-store
  ==
  ++  funding-outpoint
    |^  ^-  outpoint
    :*  txid=funding-txid
        pos=funding-output-index
        sats=funding-amount
    ==
    ++  funding-amount  one-bitcoin-in-sats
    ++  funding-output-index  0
    ++  funding-txid
      ^-  hexb:bc
      :-  32
      0x8984.484a.580b.825b.9972.d7ad.b150.50b3.ab62.4ccd.7319.46b3.eedd.b92f.4e7e.f6be
    --
  ::
  ++  chanid
    %+  make-channel-id:channel
      txid:funding-outpoint
    pos:funding-outpoint
  ::
  ++  local-cfg
    ^-  local-config
    =|  conf=local-config
    %=  conf
      ship                            ?:(initiator ~zod ~bus)
      basepoints                      local-basepoints
      pub.multisig-key                local-funding-pubkey
      prv.multisig-key
        prv:(generate-keypair:keys local-seed %main %multisig)
      to-self-delay                   local-csv
      dust-limit-sats                 local-dust-limit-sats
      max-htlc-value-in-flight-msats  (mul (sats-to-msats 100.000.000) 5)
      max-accepted-htlcs              5
      initial-msats                   local-amount
      reserve-sats                    0
      per-commitment-secret-seed
        prv:(generate-keypair:keys local-seed %main %revocation-root)
      funding-locked-received         %.y
      htlc-minimum-msats              1
      upfront-shutdown-script         0^0x0
      anchor-outputs                  anchor
      seed                            local-seed
    ==
  ::
  ++  remote-cfg
    ^-  remote-config
    =|  conf=remote-config
    %=  conf
      ship                            ?.(initiator ~zod ~bus)
      basepoints                      remote-basepoints
      pub.multisig-key                remote-funding-pubkey
      to-self-delay                   remote-csv
      dust-limit-sats                 remote-dust-limit-sats
      max-htlc-value-in-flight-msats  (mul (sats-to-msats 100.000.000) 5)
      max-accepted-htlcs              5
      initial-msats                   remote-amount
      reserve-sats                    0
      htlc-minimum-msats              1
      upfront-shutdown-script         0^0x0
      anchor-outputs                  anchor
      next-per-commitment-point       next-per-commitment-point
      current-per-commitment-point    first-per-commitment-point
    ==
  ::
  ++  constraints
    :*  initiator=initiator
        anchor-outputs=anchor
        capacity=sats:funding-outpoint
        initial-feerate=?~(initial-feerate 6.000 u.initial-feerate)
        funding-tx-min-depth=3
    ==
  --
::
++  make-test-channels
  |=  $:  initial-feerate=(unit sats:bc)
          local-msats=(unit msats)
          remote-msats=(unit msats)
          eny=@uvJ
      ==
  ^-  [alice=chan bob=chan]
  =+  rng=~(. og eny)
  =^  funding-txid  rng  (rads:rng (bex 256))
  =+  funding-index=0
  =+  ^=  funding-sats
      ?:  ?&  ?=(^ local-msats)
              ?=(^ remote-msats)
          ==
        %-  msats-to-sats
        %+  add
          u.local-msats
        u.remote-msats
      (mul one-bitcoin-in-sats 10)
  =+  funding-outpoint=[txid=32^funding-txid pos=0 sats=funding-sats]
  ::
  =+  ^=  local-amount
      %+  fall  local-msats
      (div (sats-to-msats funding-sats) 2)
  ::
  =+  ^=  remote-amount
      %+  fall  remote-msats
      (div (sats-to-msats funding-sats) 2)
  ::
  =+  feerate=(fall initial-feerate 6.000)
  =^  alice-seed  rng  (rads:rng (bex 256))
  =+  alice-revocation-root=(generate-keypair:keys alice-seed %main %revocation-root)
  =+  alice-multisig=(generate-keypair:keys alice-seed %main %multisig)
  =+  alice-pubkey=pub.alice-multisig
  =+  ^=  alice-basepoints
      ^-  basepoints
      (generate-basepoints:keys alice-seed %main)
  =+  ^=  alice-first
      ^-  point
      %-  compute-commitment-point:secret
      %^    generate-from-seed:secret
          prv.alice-revocation-root
        first-index:secret
      ~
  ::
  =^  bob-seed  rng  (rads:rng (bex 256))
  =+  bob-revocation-root=(generate-keypair:keys bob-seed %main %revocation-root)
  =+  bob-multisig=(generate-keypair:keys bob-seed %main %multisig)
  =+  bob-pubkey=pub.bob-multisig
  =+  ^=  bob-basepoints
      ^-  basepoints
      (generate-basepoints:keys bob-seed %main)
  =+  ^=  bob-first
      ^-  point
      %-  compute-commitment-point:secret
      %^    generate-from-seed:secret
          prv.bob-revocation-root
        first-index:secret
      ~
  ::
  =+  ^=  alice
      %:  make-channel
        initiator=%.y
        anchor=%.n
        local-funding-pubkey=alice-pubkey
        remote-funding-pubkey=bob-pubkey
        local-basepoints=alice-basepoints
        remote-basepoints=bob-basepoints
        local-csv=5
        remote-csv=4
        local-dust-limit-sats=200
        remote-dust-limit-sats=1.300
        local-amount=local-amount
        remote-amount=remote-amount
        first-per-commitment-point=*point
        next-per-commitment-point=*point
        local-seed=alice-seed
        initial-feerate=`feerate
      ==
  ::
  =+  ^=  bob
      %:  make-channel
        initiator=%.n
        anchor=%.n
        local-funding-pubkey=bob-pubkey
        remote-funding-pubkey=alice-pubkey
        local-basepoints=bob-basepoints
        remote-basepoints=alice-basepoints
        local-csv=4
        remote-csv=5
        local-dust-limit-sats=1.300
        remote-dust-limit-sats=200
        local-amount=remote-amount
        remote-amount=local-amount
        first-per-commitment-point=*point
        next-per-commitment-point=*point
        local-seed=bob-seed
        initial-feerate=`feerate
      ==
  ::  simulating funding-created/funding-signed:
  ::
  =^  sig-from-alice  alice
    (~(sign-first-commitment channel alice) bob-first)
  =^  sig-from-bob  bob
    (~(sign-first-commitment channel bob) alice-first)
  ::
  =.  alice  (~(receive-first-commitment channel alice) sig-from-bob)
  =.  bob    (~(receive-first-commitment channel bob) sig-from-alice)
  ::  simulating funding-locked:
  ::
  =/  alice-second=point
    %-  compute-commitment-point:secret
    %^    generate-from-seed:secret
        prv.alice-revocation-root
      (dec first-index:secret)
    ~
  =/  bob-second=point
    %-  compute-commitment-point:secret
    %^    generate-from-seed:secret
        prv.bob-revocation-root
      (dec first-index:secret)
    ~
  =.  alice  alice(next-per-commitment-point.her.config bob-second)
  =.  bob    bob(next-per-commitment-point.her.config alice-second)
  ::
  [alice=alice bob=bob]
::  +make-htlc: utility for generating test htlcs
::
++  make-htlc
  |=  [id=htlc-id =amount=msats]
  ^-  [htlc=update-add-htlc:msg preimage=hexb:bc]
  =+  preimage=32^(fil 3 32 id)
  =+  payment-hash=(sha256:bcu:bc preimage)
  =|  htlc=update-add-htlc:msg
  :_  preimage
  %=  htlc
    htlc-id       id
    payment-hash  payment-hash
    amount-msats  amount-msats
    cltv-expiry   5
  ==
::  +test-channel: test channel operations
::
++  test-channel
  =+  (make-test-channels ~ ~ ~ `@uvJ`42)
  =+  (make-htlc htlc-count.our.updates.alice (sats-to-msats 100.000.000))
  =^  alice-htlc  alice  (~(add-htlc channel alice) htlc)
  =^  bob-htlc    bob    (~(receive-htlc channel bob) htlc)
  |^
  ;:  weld
    %+  category  "check-concurrent-reversed-payment"
    check-concurrent-reversed-payment
  ::
    %+  category  "check-simple-add-settle-workflow"
    check-simple-add-settle-workflow
  ::
    %+  category  "check-update-fee-sender-commits"
    check-update-fee-sender-commits
  ::
    %+  category  "check-update-fee-receiver-commits"
    check-update-fee-receiver-commits
  ==
  ++  check-concurrent-reversed-payment
    =/  htlc-2=update-add-htlc:msg
      %=  htlc
        htlc-id       htlc-count.our.updates.bob
        payment-hash  (sha256:bcu:bc 32^(fil 3 32 0x2))
        amount-msats  (add amount-msats.htlc 1.000)
      ==
    =/  [bob-htlc=update-add-htlc:msg bob-2=chan]
      (~(add-htlc channel bob) htlc-2)
    =/  [alice-htlc=update-add-htlc:msg alice-2=chan]
      (~(receive-htlc channel alice) htlc-2)
    =/  [bob-sigs=(pair hexb:bc (list hexb:bc)) bob-3=chan]
      ~(sign-next-commitment channel bob-2)
    =/  alice-3=chan
      (~(receive-new-commitment channel alice-2) bob-sigs)
    =/  [alice-rev=revoke-and-ack:msg alice-4=chan]
      ~(revoke-current-commitment channel alice-3)
    =/  bob-4=chan
      (~(receive-revocation channel bob-3) alice-rev)
    ;:  weld
      %+  category  "alice added the HTLC"
      ;:  weld
        %+  expect-eq
        !>  2
        !>  (lent outputs:(latest-commitment alice-2 %local))
      ::
        %+  expect-eq
        !>  3
        !>  (lent outputs:(next-commitment alice-2 %local))
      ::
        %+  expect-eq
        !>  2
        !>  (lent outputs:(latest-commitment alice-2 %remote))
      ::
        %+  expect-eq
        !>  3
        !>  (lent outputs:(next-commitment alice-2 %remote))
      ==
    ::
      %+  category  "alice received bob's signatures"
      ;:  weld
        %+  expect-eq
        !>  3
        !>  (lent outputs:(latest-commitment alice-3 %local))
      ::
        %+  expect-eq
        !>  2
        !>  (lent outputs:(latest-commitment alice-3 %remote))
      ::
        %+  expect-eq
        !>  3
        !>  (lent outputs:(next-commitment alice-3 %remote))
      ==
    ::
      %+  category  "alice revoked current commitment"
      ;:  weld
        %+  expect-eq
        !>  3
        !>  (lent outputs:(latest-commitment alice-4 %local))
      ::
        %+  expect-eq
        !>  2
        !>  (lent outputs:(latest-commitment alice-4 %remote))
      ::
        %+  expect-eq
        !>  4
        !>  (lent outputs:(next-commitment alice-4 %remote))
      ==
    ==
  ::
  ++  check-simple-add-settle-workflow
    =/  alice=chan  alice
    =/  bob=chan    bob
    =/  htlc=update-add-htlc:msg        htlc
    =/  bob-htlc=update-add-htlc:msg    bob-htlc
    =/  alice-htlc=update-add-htlc:msg  alice-htlc
    ::
    =+  ^=  local-outs
        ^-  (list output:psbt)
        %+  sort  outputs:(latest-commitment alice %local)
        |=  [a=output:psbt b=output:psbt]
        (lte wid.script-pubkey.a wid.script-pubkey.b)
    ::
    =+  ^=  remote-outs
        ^-  (list output:psbt)
        %+  sort  outputs:(latest-commitment alice %remote)
        |=  [a=output:psbt b=output:psbt]
        (lte wid.script-pubkey.a wid.script-pubkey.b)
    ::  Next alice commits this change by sending a signature message. Since
    ::  we expect the messages to be ordered, Bob will receive the HTLC we
    ::  just sent before he receives this signature, so the signature will
    ::  cover the HTLC.
    ::
    =/  [[alice-sig=signature alice-htlc-sigs=(list signature)] alice-2=chan]
      ~(sign-next-commitment channel alice)
    ::  Bob receives this signature message, and checks that this covers the
    ::  state he has in his remote log. This includes the HTLC just sent
    ::  from Alice.
    ::
    =/  bob-2=chan
      %+  ~(receive-new-commitment channel bob)
        alice-sig
      alice-htlc-sigs
    ::  Bob revokes his prior commitment given to him by Alice, since he now
    ::  has a valid signature for a newer commitment.
    ::
    =/  [bob-revocation=revoke-and-ack:msg bob-3=chan]
      ~(revoke-current-commitment channel bob-2)
    ::  Bob finally sends a signature for Alice's commitment transaction.
    ::  This signature will cover the HTLC, since Bob will first send the
    ::  revocation just created. The revocation also acks every received
    ::  HTLC up to the point where Alice sent her signature.
    ::
    =/  [[bob-sig=signature bob-htlc-sigs=(list signature)] bob-4=chan]
      ~(sign-next-commitment channel bob-3)
    ::  Alice then processes this revocation, sending her own revocation for
    ::  her prior commitment transaction. Alice shouldn't have any HTLCs to
    ::  forward since she's sending an outgoing HTLC.
    ::
    =/  alice-3=chan
      (~(receive-revocation channel alice-2) bob-revocation)
    ::  Alice then processes bob's signature, and since she just received
    ::  the revocation, she expect this signature to cover everything up to
    ::  the point where she sent her signature, including the HTLC.
    ::
    =/  alice-4=chan
      (~(receive-new-commitment channel alice-3) bob-sig bob-htlc-sigs)
    ::
    =/  tx0=psbt:psbt  ~(force-close-tx channel alice)
    =/  tx1=psbt:psbt  ~(force-close-tx channel alice-4)
    ::  Alice then generates a revocation for bob.
    ::
    =/  [alice-revocation=revoke-and-ack:msg alice-5=chan]
      ~(revoke-current-commitment channel alice-4)
    =/  tx2=psbt:psbt  ~(force-close-tx channel alice-5)
    ::  Finally Bob processes Alice's revocation, at this point the new HTLC
    ::  is fully locked in within both commitment transactions. Bob should
    ::  also be able to forward an HTLC now that the HTLC has been locked
    ::  into both commitment transactions.
    ::
    =/  bob-5=chan
      (~(receive-revocation channel bob-4) alice-revocation)
    :: Now we'll repeat a similar exchange, this time with Bob settling the
    :: HTLC once he learns of the preimage.
    ::
    =/  bob-6=chan
      (~(settle-htlc channel bob-5) preimage htlc-id.bob-htlc)
    ::
    =/  alice-6=chan
      (~(receive-htlc-settle channel alice-5) preimage htlc-id.alice-htlc)
    ::
    =/  tx3=psbt:psbt  ~(force-close-tx channel alice-6)
    ::
    =/  [[bob-sig-2=signature bob-htlc-sigs-2=(list signature)] bob-7=chan]
      ~(sign-next-commitment channel bob-6)
    ::
    =/  alice-7=chan
      (~(receive-new-commitment channel alice-6) bob-sig-2 bob-htlc-sigs-2)
    ::
    =/  tx4=psbt:psbt:psbt  ~(force-close-tx channel alice-7)
    ::
    =/  [alice-revocation-2=revoke-and-ack:msg alice-8=chan]
      ~(revoke-current-commitment channel alice-7)
    ::
    =/  [[alice-sig-2=signature alice-htlc-sigs-2=(list signature)] alice-9=chan]
      ~(sign-next-commitment channel alice-8)
    ::
    =/  bob-8=chan
      (~(receive-revocation channel bob-7) alice-revocation-2)
    ::
    =/  bob-9=chan
      %+  ~(receive-new-commitment channel bob-8)
        alice-sig-2
      alice-htlc-sigs-2
    ::
    =/  [bob-revocation-2=revoke-and-ack:msg bob-10=chan]
      ~(revoke-current-commitment channel bob-9)
    ::
    =/  alice-10=chan
      (~(receive-revocation channel alice-9) bob-revocation-2)
    ::
    ;:  weld
      %-  expect
        !>  (lth value:(head (tail local-outs)) (mul 5 (pow 10 8)))
      %+  expect-eq
        !>  (mul 5 (pow 10 8))
        !>  value:(head local-outs)
    ::
      %-  expect
        !>  (lth value:(head remote-outs) (mul 5 (pow 10 8)))
      %+  expect-eq
        !>  (mul 5 (pow 10 8))
        !>  value:(head (tail remote-outs))
    ::
      %-  expect
        !>  %-  ~(signature-fits channel alice)
            (latest-commitment alice %local)
    ::
      %+  expect-eq
        !>  0
        !>  (~(oldest-unrevoked-commitment-number channel alice) %local)
    ::
      %-  expect
        !>  %-  ~(signature-fits channel alice)
            (latest-commitment alice %local)
    ::
      %+  expect-eq
        !>  1
        !>  (lent alice-htlc-sigs)
      %-  expect
        !>  %-  ~(signature-fits channel alice-2)
            (latest-commitment alice-2 %local)
    ::
      %-  expect
        !>  %-  ~(signature-fits channel bob-2)
            (latest-commitment bob-2 %local)

      %-  expect
        !>  %-  ~(signature-fits channel bob-2)
            (latest-commitment bob-2 %local)
    ::
      %+  expect-eq
        !>  0
        !>  (~(oldest-unrevoked-commitment-number channel bob-2) %remote)
    ::
      %-  expect
        !>  %-  ~(signature-fits channel bob-3)
            (latest-commitment bob-3 %local)
    ::
      %-  expect
        !>  %-  ~(signature-fits channel bob-4)
            (latest-commitment bob-4 %local)
    ::
      %+  expect-eq
        !>  1
        !>  (lent bob-htlc-sigs)
    ::
      %-  expect
        !>  %-  ~(signature-fits channel alice-2)
            (latest-commitment alice-2 %local)
    ::
      %+  expect-eq
        !>  2
        !>  (lent outputs:(latest-commitment alice-2 %local))
      %+  expect-eq
        !>  2
        !>  (lent outputs:(next-commitment alice-2 %local))
      %+  expect-eq
        !>  2
        !>  (lent outputs:(oldest-unrevoked-commitment alice-2 %remote))
      %+  expect-eq
        !>  3
        !>  (lent outputs:(latest-commitment alice-2 %remote))
    ::
      %-  expect
        !>  %-  ~(signature-fits channel alice-3)
            (latest-commitment alice-3 %local)
    ::
      %+  expect-eq
        !>  2
        !>  (lent outputs:(latest-commitment alice-3 %local))
      %+  expect-eq
        !>  3
        !>  (lent outputs:(latest-commitment alice-3 %remote))
      %+  expect-eq
        !>  2
        !>  (lent outputs:~(force-close-tx channel alice-3))
    ::
      %+  expect-eq
        !>  1
        !>  (lent (skim list.our.updates.alice-3 |=(u=update ?=([%add-htlc *] u))))
    ::
      %+  expect-eq
        !>  3
        !>  (lent outputs:(latest-commitment alice-4 %remote))
      %+  expect-eq
        !>  3
        !>  (lent outputs:~(force-close-tx channel alice-4))
    ::
      %+  expect-eq
        !>  1
        !>  (lent (skim list.our.updates.alice-4 |=(u=update ?=([%add-htlc *] u))))
    ::
      %-  expect
        !>  !=(tx0 tx1)
    ::
      %+  expect-eq
        !>  tx1
        !>  tx2
    ::
      %-  expect
        !>  %-  ~(signature-fits channel bob-4)
            (latest-commitment bob-4 %local)
    ::
      %+  expect-eq
        !>  0
        !>  sent-msats.alice-5
      %+  expect-eq
        !>  0
        !>  recd-msats.alice-5
      %+  expect-eq
        !>  0
        !>  sent-msats.bob-5
      %+  expect-eq
        !>  0
        !>  recd-msats.bob-5
      %+  expect-eq
        !>  1
        !>  height.commitments.bob-5
      %+  expect-eq
        !>  1
        !>  height.commitments.alice-5
    ::
      %+  expect-eq
        !>  3
        !>  (lent outputs:(latest-commitment alice-5 %local))
      %+  expect-eq
        !>  3
        !>  (lent outputs:(latest-commitment bob-5 %local))
    ::
    ::  check output values
    ::
      %+  expect-eq
        !>  tx2
        !>  tx3
    ::
      %+  expect-eq
        !>  0
        !>  (lent bob-htlc-sigs-2)
    ::
      %-  expect
        !>  !=(tx3 tx4)
    ::
      %+  expect-eq
        !>  500.000.000.000
        !>  balance.our:(need (~(latest-commitment channel alice-7) %local))
    ::
      %+  expect-eq
        !>  ~
        !>  alice-htlc-sigs-2
      %+  expect-eq
        !>  3
        !>  (lent outputs:(latest-commitment bob-6 %local))
    ::
      %+  category  "1 BTC should be sent by Alice, 1 received by Bob"
      ;:  weld
        %+  expect-eq
          !>  one-bitcoin-in-msats
          !>  sent-msats.alice-10
        %+  expect-eq
          !>  0
          !>  recd-msats.alice-10
        %+  expect-eq
          !>  one-bitcoin-in-msats
          !>  recd-msats.bob-10
        %+  expect-eq
          !>  0
          !>  sent-msats.bob-10
        %+  expect-eq
          !>  2
          !>  height.commitments.bob-10
        %+  expect-eq
          !>  2
          !>  height.commitments.alice-10
      ==
    ::
      %+  category  "logs should be cleared on both sides"
      ;:  weld
        %+  expect-eq
          !>  0
          !>  (lent list.our.updates.alice-10)
        %+  expect-eq
          !>  0
          !>  (lent list.her.updates.alice-10)
        %-  expect
          !>  !=(0 update-count.our.updates.alice-10)
        %-  expect
          !>  !=(0 update-count.her.updates.alice-10)
      ==
    ==
  ::
  ++  alice-to-bob-fee-update
    |=  [alice=chan bob=chan feerate=(unit sats:bc)]
    ^-  [fee=sats:bc alice=chan bob=chan]
    =+  fee=(fall feerate 111)
    =.  alice  (~(update-fee channel alice) fee)
    =.  bob  (~(receive-update-fee channel bob) fee)
    [fee=fee alice=alice bob=bob]
  ::
  ++  check-update-fee-sender-commits
    =/  alice=chan  alice
    =/  bob=chan    bob
    =/  old-feerate=sats:bc
      (next-feerate alice %local)
    =/  [fee=sats:bc alice=chan bob=chan]
      (alice-to-bob-fee-update alice bob ~)
    =/  [[alice-sig=signature alice-htlc-sigs=(list signature)] alice-2=chan]
      ~(sign-next-commitment channel alice)
    =/  bob-2=chan
      (~(receive-new-commitment channel bob) alice-sig alice-htlc-sigs)
    =/  [bob-rev=revoke-and-ack:msg bob-3=chan]
      ~(revoke-current-commitment channel bob-2)
    =/  alice-3=chan
      (~(receive-revocation channel alice-2) bob-rev)
    =/  [[bob-sig=signature bob-htlc-sigs=(list signature)] bob-4=chan]
      ~(sign-next-commitment channel bob-3)
    =/  alice-4=chan
      (~(receive-new-commitment channel alice-3) bob-sig bob-htlc-sigs)
    =/  [alice-rev=revoke-and-ack:msg alice-5=chan]
      ~(revoke-current-commitment channel alice-4)
    =/  bob-5=chan
      (~(receive-revocation channel bob-4) alice-rev)
    ;:  weld
      %+  expect-eq
        !>  old-feerate
        !>  (next-feerate alice %local)
      %-  expect
        !>  !=(fee (oldest-unrevoked-feerate bob-2 %local))
      %+  expect-eq
        !>  fee
        !>  (latest-feerate bob-2 %local)
      %+  expect-eq
        !>  fee
        !>  (oldest-unrevoked-feerate bob-3 %local)
      %-  expect
        !>  !=(fee (oldest-unrevoked-feerate alice-4 %local))
      %+  expect-eq
        !>  fee
        !>  (latest-feerate alice-4 %local)
      %+  expect-eq
        !>  fee
        !>  (oldest-unrevoked-feerate alice-5 %local)
      %+  expect-eq
        !>  fee
        !>  (oldest-unrevoked-feerate bob-5 %local)
      %+  expect-eq
        !>  fee
        !>  (latest-feerate bob-5 %local)
    ==
  ::
  ++  check-update-fee-receiver-commits
    =/  alice=chan  alice
    =/  bob=chan    bob
    =/  [fee=sats:bc alice=chan bob=chan]
      (alice-to-bob-fee-update alice bob ~)
    =/  [[bob-sig=signature bob-htlc-sigs=(list signature)] bob=chan]
      ~(sign-next-commitment channel bob)
    =/  alice=chan
      (~(receive-new-commitment channel alice) bob-sig bob-htlc-sigs)
    =/  [alice-rev=revoke-and-ack:msg alice=chan]
      ~(revoke-current-commitment channel alice)
    =/  bob=chan
      (~(receive-revocation channel bob) alice-rev)
    =/  [[alice-sig=signature alice-htlc-sigs=(list signature)] alice=chan]
      ~(sign-next-commitment channel alice)
    =/  bob=chan
      (~(receive-new-commitment channel bob) alice-sig alice-htlc-sigs)
    =/  [bob-rev=revoke-and-ack:msg bob-2=chan]
      ~(revoke-current-commitment channel bob)
    =/  [[bob-sig=signature bob-htlc-sigs=(list signature)] bob-3=chan]
      ~(sign-next-commitment channel bob-2)
    =/  alice=chan
      (~(receive-revocation channel alice) bob-rev)
    =/  alice=chan
      (~(receive-new-commitment channel alice) bob-sig bob-htlc-sigs)
    =/  [alice-rev=revoke-and-ack:msg alice-2=chan]
      ~(revoke-current-commitment channel alice)
    =/  bob-4=chan
      (~(receive-revocation channel bob-3) alice-rev)
    ;:  weld
      %-  expect
        !>  !=(fee (oldest-unrevoked-feerate bob %local))
      %+  expect-eq
        !>  fee
        !>  (latest-feerate bob %local)
      %+  expect-eq
        !>  fee
        !>  (oldest-unrevoked-feerate bob-2 %local)
    ::
      %-  expect
        !>  !=(fee (oldest-unrevoked-feerate alice %local))
      %+  expect-eq
        !>  fee
        !>  (latest-feerate alice %local)
      %+  expect-eq
        !>  fee
        !>  (oldest-unrevoked-feerate alice-2 %local)
    ::
      %+  expect-eq
        !>  fee
        !>  (oldest-unrevoked-feerate bob-4 %local)
      %+  expect-eq
        !>  fee
        !>  (latest-feerate bob-4 %local)
    ==
  --
::
++  force-state-transition
  |=  [a=chan b=chan]
  ^-  [a=chan b=chan]
  =^  a-sigs  a  ~(sign-next-commitment channel a)
  =.  b  (~(receive-new-commitment channel b) a-sigs)
  =^  b-rev   b  ~(revoke-current-commitment channel b)
  =^  b-sigs  b  ~(sign-next-commitment channel b)
  =.  a  (~(receive-revocation channel a) b-rev)
  =.  a  (~(receive-new-commitment channel a) b-sigs)
  =^  a-rev  a  ~(revoke-current-commitment channel a)
  =.  b  (~(receive-revocation channel b) a-rev)
  [a=a b=b]
::
++  test-invalid-commit-sig
  =+  (make-test-channels ~ ~ ~ `@uvJ`42)
  =+  (make-htlc 0 100.000)
  =^  alice-htlc  alice
    (~(add-htlc channel alice) htlc)
  =^  bob-htlc  bob
    (~(receive-htlc channel bob) alice-htlc)
  =^  alice-sigs  alice
    ~(sign-next-commitment channel alice)
  =/  [alice-sig=signature alice-htlc-sigs=(list signature)]
    alice-sigs
  =.  dat.alice-sig  (mix dat.alice-sig 88)
  %-  expect-fail
    |.  (~(receive-new-commitment channel bob) alice-sig alice-htlc-sigs)
::
++  test-can-pay
  =+  (make-test-channels ~ ~ ~ `@uvJ`42)
  =+  capacity=(mul one-bitcoin-in-sats 10)
  ;:  weld
    %-  expect
    !>  (~(can-pay channel alice) 1)
  ::
    %-  expect
    !>  (~(can-pay channel bob) 1)
  ::
    %-  expect
    !>  %-  ~(can-pay channel alice)
        %-  sats-to-msats
          (div capacity 4)
  ::
    %-  expect
    !>  %-  ~(can-pay channel bob)
        %-  sats-to-msats
          (div capacity 4)
  ::
    %+  expect-eq
    !>  %.n
    !>  (~(can-pay channel alice) (sats-to-msats capacity))
  ::
    %+  expect-eq
    !>  %.n
    !>  (~(can-pay channel bob) (sats-to-msats capacity))
  ==
--
