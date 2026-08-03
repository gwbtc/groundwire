::  Deterministic strand/card-boundary coverage for lib/lc-attestation.
::
::  The test drives the real khan shed, checks every %light-client watch card,
::  and supplies the corresponding watch-ack/fact/kick sequence from fixed
::  transaction vectors.  It needs neither a Bitcoin node nor a Gall mock.
::
/-  bitcoin, ord, urb, sa=self-attestation, lc=light-client
/+  *test, lca=lc-attestation, ue=urb-encoder, bscr=btc-script,
    tr=taproot, bc=bitcoin, cc=gw-btc-pass, strandio, libstrand=strand
=,  strand=strand:libstrand
=/  m  (strand:strandio ,vase)
=>
|%
+$  halt
  $%  [%wait =form:m]
      [%skip =form:m]
      [%fail err=(pair term tang)]
      [%done value=vase]
  ==
+$  turn  [cards=(list card:agent:gall) =halt]
::
++  test-bowl
  ^-  bowl:strand
  :*  our=~zod
      src=~zod
      tid='lc-attestation-test'
      mom=~
      wex=*boat:gall
      sup=*bitt:gall
      eny=`@uvJ`0
      now=~2026.7.20
      byk=[~zod %groundwire %da ~2026.7.20]
  ==
::
::  Run continuations eagerly until the strand blocks, fails, or completes.
++  step
  |=  [=form:m in=(unit input:strand)]
  ^-  turn
  =/  out=output:m  (form [test-bowl in])
  ?-  -.next.out
    %wait  [cards.out %wait form]
    %skip  [cards.out %skip form]
    %fail  [cards.out %fail err.next.out]
    %done  [cards.out %done value.next.out]
    %cont
      =/  nex=turn  $(form self.next.out, in ~)
      [(weld cards.out cards.nex) halt.nex]
  ==
::
++  inject
  |=  [st=turn in=input:strand]
  ^-  turn
  ?>  ?=(%wait -.halt.st)
  (step form.halt.st `in)
::
::  A one-shot Gall watch completes only after ack, fact, and kick.
++  answer-watch
  |=  [st=turn =wire =cage]
  ^-  turn
  =/  ack=turn  (inject st [%agent watch+wire %watch-ack ~])
  ?>  &(=(~ cards.ack) ?=(%wait -.halt.ack))
  =/  fac=turn  (inject ack [%agent watch+wire %fact cage])
  ?>  &(=(~ cards.fac) ?=(%wait -.halt.fac))
  (inject fac [%agent watch+wire %kick ~])
::
++  expect-watch
  |=  [st=turn =wire =path]
  ^-  tang
  ;:  weld
    (expect !>(?=(%wait -.halt.st)))
    %+  expect-eq
      !>(~[[%pass watch+wire %agent [~zod %light-client] %watch path]])
      !>(cards.st)
  ==
::
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
++  leaf-for
  |=  sots=(list sotx:urb)
  ^-  tapleaf:sa
  =/  unv  (full:encode:ue sots)
  =/  scr=octs  (en:bscr (unv-to-script:en:ue unv))
  [0xc0 [p.scr `@ux`q.scr]]
::
++  p2tr-spk
  |=  q=@ux
  ^-  hexb:bc
  [34 `@ux`(can 3 ~[[32 q] [2 0x5120]])]
::
++  spkh-of
  |=  out=output:tx:bc
  ^-  @ux
  =/  en-out  (can 3 script-pubkey.out 8^value.out ~)
  `@ux`(shay (add 8 wid.script-pubkey.out) en-out)
::
++  mk-input
  |=  [=txid:ord pos=@ud wit=(list hexb:bc)]
  ^-  inputw:tx:bc
  [wit txid pos [4 0xffff.ffff] ~ ~]
::
++  mk-tx
  |=  [id=@ux is=(list inputw:tx:bc) os=(list output:tx:bc)]
  ^-  tx:bc
  [id is os 0 1 ~]
::
++  mk-rs
  |=  [w=@p skim=skim-sotx:urb]
  ^-  raw-sotx:urb
  [[0 0] [[w ~] skim]]
::
++  pre-id   0x1a1a.1a1a
++  c0-id    0x2b2b.2b2b
++  block-hax  0xabcd.1234
++  spawn-sont  ^-  sont:ord  [pre-id 0 0]
++  dat  (make-dat:cc spawn-sont)
++  key  (cric-key 'lc-attestation-test' dat 0)
++  pas  pub:ex:key
++  who  `@p`fig:ex:key
::
++  pre-out  ^-  output:tx:bc  [(p2tr-spk 0xdead) 10.000]
++  pre-tx   (mk-tx pre-id ~[(mk-input 0x9999 0 ~)] ~[pre-out])
::
++  spawn-sg
  ^-  single:skim-sotx:urb
  [%spawn pas ~ [(spkh-of pre-out) `0 0 0]]
++  spawn-rs  (mk-rs who spawn-sg)
++  spawn-leaf  (leaf-for ~[[[who ~] spawn-sg]])
++  ikey0  (mk-ikey 17)
++  q0  (output-pubkey:tr ikey0 `(leaf-hash:tr spawn-leaf))
++  c0-out  ^-  output:tx:bc  [(p2tr-spk q0) 9.500]
++  c0-tx  (mk-tx c0-id ~[(mk-input pre-id 0 ~)] ~[c0-out])
::
++  chain
  ^-  custody-log:sa
  ~[[c0-id 100 `[ikey0 spawn-leaf]]]
++  sat  ^-  self-attestation:sa  [who spawn-sont chain]
::
++  start-wire  /lc/start/(scot %ux pre-id)
++  start-path  /transaction/(scot %ux pre-id)
++  height-wire  /lc/height/(scot %ud 100)
++  height-path  /block-hash-by-height/(scot %ud 100)
++  entry-wire  /lc/entry/(scot %ux c0-id)
++  entry-path  /transaction/(scot %ux block-hax)/(scot %ux c0-id)
++  out-wire  /lc/out/(scot %ux c0-id)/(scot %ud 0)
++  out-path  /tx-out/(scot %ux c0-id)/(scot %ud 0)
::
++  reach-out
  ^-  turn
  =/  st0=turn  (step (verify-lc:lca sat ~ *unv-ids:urb) ~)
  =/  st1=turn  (answer-watch st0 start-wire [%noun !>(pre-tx)])
  =/  block=id:block:bc  [block-hax 100]
  =/  st2=turn  (answer-watch st1 height-wire [%noun !>(block)])
  (answer-watch st2 entry-wire [%noun !>(c0-tx)])
::
++  verify-status
  |=  status=tx-out-response:lc
  ^-  result:sa
  =/  fin=turn  (answer-watch reach-out out-wire [%noun !>(status)])
  ?>  ?=(%done -.halt.fin)
  =/  [res=result:sa tip-spk=hexb:bc]
    !<([result:sa hexb:bc] value.halt.fin)
  res
--
|%
++  test-valid-light-client-vector
  =/  st0=turn  (step (verify-lc:lca sat ~ *unv-ids:urb) ~)
  =/  ck0=tang  (expect-watch st0 start-wire start-path)
  =/  st1=turn  (answer-watch st0 start-wire [%noun !>(pre-tx)])
  =/  ck1=tang  (expect-watch st1 height-wire height-path)
  =/  block=id:block:bc  [block-hax 100]
  =/  st2=turn  (answer-watch st1 height-wire [%noun !>(block)])
  =/  ck2=tang  (expect-watch st2 entry-wire entry-path)
  =/  st3=turn  (answer-watch st2 entry-wire [%noun !>(c0-tx)])
  =/  ck3=tang  (expect-watch st3 out-wire out-path)
  =/  status=tx-out-response:lc  [%unspent c0-out]
  =/  st4=turn  (answer-watch st3 out-wire [%noun !>(status)])
  ?>  ?=(%done -.halt.st4)
  =/  [res=result:sa tip-spk=hexb:bc]
    !<([result:sa hexb:bc] value.halt.st4)
  ;:  weld
    ck0  ck1  ck2  ck3
    (expect !>(=(~ cards.st4)))
    (expect !>(ok.verdict.res))
    %+  expect-eq
      !>(`(unit point:urb)``[[[c0-id 0 0] ~] 0 1 pas [%.n who] ~ ~])
      !>(point.res)
    %+  expect-eq
      !>((p2tr-spk q0))
      !>(tip-spk)
  ==
::
++  test-height-mismatch-fails-strand
  =/  st0=turn  (step (verify-lc:lca sat ~ *unv-ids:urb) ~)
  =/  st1=turn  (answer-watch st0 start-wire [%noun !>(pre-tx)])
  =/  bad=id:block:bc  [block-hax 99]
  =/  st2=turn  (answer-watch st1 height-wire [%noun !>(bad)])
  ?>  ?=(%fail -.halt.st2)
  ;:  weld
    (expect !>(=(~ cards.st2)))
    %+  expect-eq
      !>(%attestation-height-mismatch)
      !>(-.err.halt.st2)
  ==
::
++  test-txid-mismatch-fails-strand
  =/  st0=turn  (step (verify-lc:lca sat ~ *unv-ids:urb) ~)
  =/  st1=turn  (answer-watch st0 start-wire [%noun !>(pre-tx)])
  =/  block=id:block:bc  [block-hax 100]
  =/  st2=turn  (answer-watch st1 height-wire [%noun !>(block)])
  =/  bad=tx:bc  c0-tx
  =.  id.bad  0xdead.beef
  =/  st3=turn  (answer-watch st2 entry-wire [%noun !>(bad)])
  ?>  ?=(%fail -.halt.st3)
  ;:  weld
    (expect !>(=(~ cards.st3)))
    %+  expect-eq
      !>(%attestation-txid-mismatch)
      !>(-.err.halt.st3)
  ==
::
++  test-spent-tip-fails-closed
  =/  res=result:sa  (verify-status [%spent ~])
  ;:  weld
    (expect !>(!ok.verdict.res))
    (expect !>(?=(~ point.res)))
  ==
::
++  test-unknown-tip-fails-closed
  =/  fin=turn
    (answer-watch reach-out out-wire [%noun !>(`tx-out-response:lc`[%unknown ~])])
  ?>  ?=(%fail -.halt.fin)
  ;:  weld
    (expect !>(=(~ cards.fin)))
    %+  expect-eq
      !>(%attestation-tip-unknown)
      !>(-.err.halt.fin)
  ==
--
