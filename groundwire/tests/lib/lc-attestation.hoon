::  tests/lib/lc-attestation.hoon
::
::  Strand/card-boundary coverage for the REAL %light-client fetch layer of
::  lib/lc-attestation, reconciled to the gwbtc/node %bitcoin-client API.
::
::  The test drives the actual khan shed, asserts every emitted watch card
::  against the node's real paths, and feeds back the corresponding $update
::  fact from fixed vectors.  Transactions are looked up by [height txid]
::  (never bare txid), and tip liveness is decided by a BIP-158 compact
::  filter scan from the tip block up to the chain tip.  Node facts carry
::  $bitcoin-common shapes; the fetch layer converts them to $tx:bitcoin.
::  No Bitcoin node or Gall mock is needed.
::
/-  bitcoin, ord, urb, sa=self-attestation, lc=light-client, bcm=bitcoin-common
/+  *test, lca=lc-attestation, tr=taproot, bc=bitcoin, cc=gw-btc-pass,
    b-fil=compact-block-filters, strandio, libstrand=strand
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
::  a driven strand position: the cards it just emitted, and its halt state.
+$  pace  [cards=(list card:agent:gall) =halt]
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
  ^-  pace
  =/  out=output:m  (form [test-bowl in])
  ?-  -.next.out
    %wait  [cards.out %wait form]
    %skip  [cards.out %skip form]
    %fail  [cards.out %fail err.next.out]
    %done  [cards.out %done value.next.out]
    %cont
      =/  nex=pace  $(form self.next.out, in ~)
      [(weld cards.out cards.nex) halt.nex]
  ==
::
++  inject
  |=  [st=pace in=input:strand]
  ^-  pace
  ?>  ?=(%wait -.halt.st)
  (step form.halt.st `in)
::
::  A one-shot Gall watch completes only after ack, fact, and kick.
++  answer-watch
  |=  [st=pace =wire =cage]
  ^-  pace
  =/  ack=pace  (inject st [%agent watch+wire %watch-ack ~])
  ?>  &(=(~ cards.ack) ?=(%wait -.halt.ack))
  =/  fac=pace  (inject ack [%agent watch+wire %fact cage])
  ?>  &(=(~ cards.fac) ?=(%wait -.halt.fac))
  (inject fac [%agent watch+wire %kick ~])
::
++  expect-watch
  |=  [st=pace =wire =path]
  ^-  tang
  ;:  weld
    (expect !>(?=(%wait -.halt.st)))
    %+  expect-eq
      !>(~[[%pass watch+wire %agent [~zod %light-client] %watch path]])
      !>(cards.st)
  ==
::  --------------------------------------------------------------------
::  Node-shape (bitcoin-common) fact builders.
::  --------------------------------------------------------------------
++  bc-hexb-to-common
  |=  h=hexb:bitcoin
  ^-  hexb:bcm
  [`@ud`wid.h `@ux`dat.h]
::
++  bc-out-to-common
  |=  o=output:tx:bitcoin
  ^-  transaction-output:bcm
  ::  output = [script-pubkey value]; positional access sidesteps a lazy
  ::  face-lookup quirk on the mold-cast tx-core sub-types.
  [+.o (bc-hexb-to-common -.o)]
::
++  bc-in-to-common
  |=  iw=inputw:tx:bitcoin
  ^-  transaction-input:bcm
  ::  inputw = [witness txid pos sequence script-sig pubkey]
  :*  +<.iw                              :: txid
      +>-.iw                             :: pos -> vout
      [0 0x0]                            :: script-sig (empty)
      0xffff.ffff                        :: sequence
      (turn `(list hexb:bitcoin)`-.iw bc-hexb-to-common)
  ==
::  +bc-to-common: the exact inverse (for verification-relevant fields) of
::  +common-tx-to-bc:lca, so a round-trip reproduces the fixture tx.
::
++  bc-to-common
  |=  t=tx:bc
  ^-  transaction:bcm
  :*  `@ux`nversion.t
      ?~(segwit.t 0 u.segwit.t)
      (turn is.t bc-in-to-common)
      (turn os.t bc-out-to-common)
      locktime.t
  ==
::
++  mk-bi
  |=  [haz=@ux het=@ud]
  ^-  block-info:lc
  [haz het `0 ~ 0x0]
::
++  dummy-hdr  ^-(block-header:bcm [0x0 0x0 0x0 0 0x0 0x0])
::
++  hdr-fact
  |=  [haz=@ux het=@ud]
  ^-  cage
  [%noun !>(`block-header-by-height:update:lc`[(mk-bi haz het) dummy-hdr])]
::
++  tx-fact
  |=  [haz=@ux het=@ud tid=@ux t=tx:bc]
  ^-  cage
  =/  upd=transaction:update:lc  [(mk-bi haz het) 0 tid 0x0 (bc-to-common t)]
  [%noun !>(upd)]
::
++  tx-fact-empty
  ^-  cage
  [%noun !>(`transaction:update:lc`~)]
::
++  best-fact
  |=  [het=@ud haz=@ux]
  ^-  cage
  [%noun !>(`best-block:update:lc`[%new het haz])]
::
++  filter-fact
  |=  [haz=@ux het=@ud f=hexb:bcm]
  ^-  cage
  [%noun !>(`block-filter-by-height:update:lc`[(mk-bi haz het) f])]
::
++  block-fact
  |=  [haz=@ux het=@ud txs=(list transaction:bcm)]
  ^-  cage
  [%noun !>(`block-by-height:update:lc`[(mk-bi haz het) [dummy-hdr txs]])]
::  --------------------------------------------------------------------
::  BIP-158 filter builder (node byte convention: filter dat is LSB-first,
::  compactsize prefix in the low byte).  Self-verified in +test-filter-
::  encoder against the same +match:b-fil the fetch layer uses.
::  --------------------------------------------------------------------
++  en-compactsize
  |=  n=@
  ^-  hexb:bcm
  ?:  (lte n 0xfc)          [1 `@ux`n]
  ?:  (lte n 0xffff)        [3 `@ux`(con 0xfd (lsh [3 1] n))]
  ?:  (lte n 0xffff.ffff)   [5 `@ux`(con 0xfe (lsh [3 1] n))]
  [9 `@ux`(con 0xff (lsh [3 1] n))]
::
++  mk-filter
  |=  [haz=@ux targets=(list hexb:bcm)]
  ^-  hexb:bcm
  =/  k  (to-key:b-fil haz)
  =/  n  (lent targets)
  =/  hs  (set-construct:hsh:b-fil targets k (mul n m:params:b-fil))
  ::  Golomb-Rice delta-encode the sorted hashes into a big-endian bit
  ::  stream (first-written bit ends up highest, matching +read-bit:str).
  =/  gcs=bits:b-fil  [0 0b0]
  =/  last=@  0
  =.  gcs
    |-  ^-  bits:b-fil
    ?~  hs  gcs
    $(hs t.hs, last i.hs, gcs (en:gol:b-fil gcs (sub i.hs last) p:params:b-fil))
  =/  nbits  wid.gcs
  =/  nbytes  (add (div nbits 8) ?:(=(0 (mod nbits 8)) 0 1))
  ::  left-justify the stream in `nbytes` bytes (zero padding at the low end)
  =/  g-atom  (lsh [0 (sub (mul 8 nbytes) nbits)] dat.gcs)
  ::  parse-filter does (rev 3 d) to recover the MSB-first stream, so store
  ::  the byte-reversed (LSB-first) GCS bytes and prepend the compactsize.
  =/  d-dat  (rev 3 nbytes g-atom)
  =/  cs  (en-compactsize n)
  [(add wid.cs nbytes) `@ux`(con dat.cs (lsh [3 wid.cs] d-dat))]
::  --------------------------------------------------------------------
::  Crypto + transaction fixtures (kelvin-9 snapshot model), identical to
::  tests/lib/self-attestation: a start (funding) tx whose output 0 holds
::  the spawn sat; entry 0 spends it committing snap0; entry 1 is a key-path
::  custody move to an ordinary P2TR tip.
::  --------------------------------------------------------------------
++  secp  secp256k1:secp:crypto
++  mk-ikey
  |=  k=@
  ^-  @ux
  %-  compress-point:secp
  (mul-point-scalar:secp g:domain:curve:secp k)
::
++  p2tr-spk
  |=  q=@ux
  ^-  hexb:bitcoin
  [34 `@ux`(can 3 ~[[32 q] [2 0x5120]])]
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
++  no-points  *(set ship)
::
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
  =/  cic  (com:nu:cric:crypto base-pass)
  `@p`fig:ex:cic
++  cry
  ^-  @
  =/  cic  (com:nu:cric:crypto base-pass)
  ?>  ?=(%c suite.+<.cic)
  `@`cry.pub.+<.cic
::
++  snap0  ^-(snapshot:sa [life=1 rift=0 key=cry sponsor=~ fief=~])
++  ikey0  (mk-ikey 17)
++  ikey1  (mk-ikey 23)
++  spawn-open  ^-(blind-opening:sa [spawn start-height=778.000 blind])
++  open0       ^-(opening:sa [ikey0 snap0 `spawn-open])
::
++  start-out  ^-(output:tx:bitcoin [(p2tr-spk (mk-ikey 5)) 10.000])
++  start-tx   (mk-tx start-id ~[(mk-input 0x9999 0 ~)] ~[start-out])
::
++  c0-out  (state-out ikey0 snap0 9.500)
++  c0-tx   (mk-tx c0-id ~[(mk-input start-id 0 ~)] ~[c0-out])
::
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
::  the tip output's scriptPubKey, computed directly (same value the fetch
::  layer reads off c1-tx's output 0) to avoid a lazy face-lookup on a
::  mold-cast tx-core output type.
++  tip-spk      `hexb:bitcoin`(p2tr-spk (output-pubkey:tr ikey1 ~))
++  tip-spk-bcm  (bc-hexb-to-common tip-spk)
::  canonical block hashes for each fetched height
++  h-start  0xa0
++  h-c0     0xb0
++  h-c1     0xc0
++  h-102    0xd0
::  the tip outpoint is [c1-id 0]; a later tx spending it makes the tip spent.
++  spend-tx-common
  ^-  transaction:bcm
  :*  0x0  0
      ~[`transaction-input:bcm`[c1-id 0 [0 0x0] 0xffff.ffff ~]]
      ~[`transaction-output:bcm`[500 (bc-hexb-to-common (p2tr-spk 0xfeed))]]
      0
  ==
::  --------------------------------------------------------------------
::  Wire / path builders for the reconciled node API.
::  --------------------------------------------------------------------
++  hdr-wire  |=(h=@ud /lc/header-height/(scot %ud h))
++  hdr-path  |=(h=@ud /block-header/height/(scot %ud h))
++  tx-wire   |=([haz=@ux tid=@ux] /lc/transaction/(scot %ux haz)/(scot %ux tid))
++  tx-path   |=([haz=@ux tid=@ux] /transaction/(scot %ux haz)/(scot %ux tid))
++  best-wire  /lc/best
++  best-path  /best-block
++  filter-wire  |=(h=@ud /lc/filter-height/(scot %ud h))
++  filter-path  |=(h=@ud /block-filter/height/(scot %ud h))
++  block-wire   |=(h=@ud /lc/block-height/(scot %ud h))
++  block-path   |=(h=@ud /block/height/(scot %ud h))
::  --------------------------------------------------------------------
::  Drive the six transaction fetches + best-block, leaving the strand
::  blocked on the first filter watch (height = tip-height = 101).
::  --------------------------------------------------------------------
++  drive-fetches
  |=  [best-het=@ud best-haz=@ux]
  ^-  pace
  =/  s0  (step (verify-lc:lca good-sat ~ no-points) ~)
  =/  s1  (answer-watch s0 (hdr-wire 778.000) (hdr-fact h-start 778.000))
  =/  s2  (answer-watch s1 (tx-wire h-start start-id) (tx-fact h-start 778.000 start-id start-tx))
  =/  s3  (answer-watch s2 (hdr-wire 100) (hdr-fact h-c0 100))
  =/  s4  (answer-watch s3 (tx-wire h-c0 c0-id) (tx-fact h-c0 100 c0-id c0-tx))
  =/  s5  (answer-watch s4 (hdr-wire 101) (hdr-fact h-c1 101))
  =/  s6  (answer-watch s5 (tx-wire h-c1 c1-id) (tx-fact h-c1 101 c1-id c1-tx))
  (answer-watch s6 best-wire (best-fact best-het best-haz))
::
++  done-result
  |=  st=pace
  ^-  [result:sa hexb:bitcoin]
  ?>  ?=(%done -.halt.st)
  !<([result:sa hexb:bitcoin] value.halt.st)
--
|%
::  ---- filter encoder sanity ----------------------------------------
++  test-filter-encoder
  =/  fil  (mk-filter h-c1 ~[tip-spk-bcm])
  =/  other  (bc-hexb-to-common (p2tr-spk 0xc0ff.ee00))
  ;:  weld
    (expect !>((match:b-fil h-c1 fil ~[tip-spk-bcm])))
    (expect !>(!(match:b-fil h-c1 fil ~[other])))
  ==
::  ---- full fetch path assertions + unspent verdict -----------------
++  test-fetch-sequence-and-unspent
  =/  s0  (step (verify-lc:lca good-sat ~ no-points) ~)
  =/  c0c  (expect-watch s0 (hdr-wire 778.000) (hdr-path 778.000))
  =/  s1  (answer-watch s0 (hdr-wire 778.000) (hdr-fact h-start 778.000))
  =/  c1c  (expect-watch s1 (tx-wire h-start start-id) (tx-path h-start start-id))
  =/  s2  (answer-watch s1 (tx-wire h-start start-id) (tx-fact h-start 778.000 start-id start-tx))
  =/  c2c  (expect-watch s2 (hdr-wire 100) (hdr-path 100))
  =/  s3  (answer-watch s2 (hdr-wire 100) (hdr-fact h-c0 100))
  =/  c3c  (expect-watch s3 (tx-wire h-c0 c0-id) (tx-path h-c0 c0-id))
  =/  s4  (answer-watch s3 (tx-wire h-c0 c0-id) (tx-fact h-c0 100 c0-id c0-tx))
  =/  c4c  (expect-watch s4 (hdr-wire 101) (hdr-path 101))
  =/  s5  (answer-watch s4 (hdr-wire 101) (hdr-fact h-c1 101))
  =/  c5c  (expect-watch s5 (tx-wire h-c1 c1-id) (tx-path h-c1 c1-id))
  =/  s6  (answer-watch s5 (tx-wire h-c1 c1-id) (tx-fact h-c1 101 c1-id c1-tx))
  =/  c6c  (expect-watch s6 best-wire best-path)
  ::  best-height == tip-height (101): the scan visits only the tip block.
  =/  s7  (answer-watch s6 best-wire (best-fact 101 h-c1))
  =/  c7c  (expect-watch s7 (filter-wire 101) (filter-path 101))
  =/  s8  (answer-watch s7 (filter-wire 101) (filter-fact h-c1 101 (mk-filter h-c1 ~[tip-spk-bcm])))
  =/  c8c  (expect-watch s8 (block-wire 101) (block-path 101))
  ::  tip block contains only c1-tx (no tx spends the tip outpoint) -> unspent
  =/  s9  (answer-watch s8 (block-wire 101) (block-fact h-c1 101 ~[(bc-to-common c1-tx)]))
  =/  [res=result:sa tip=hexb:bitcoin]  (done-result s9)
  ;:  weld
    c0c  c1c  c2c  c3c  c4c  c5c  c6c  c7c  c8c
    (expect !>(=(~ cards.s9)))
    (expect !>(ok.verdict.res))
    (expect !>(?=(^ point.res)))
    (expect-eq !>(tip-spk) !>(tip))
    (expect-eq !>(`sont:ord`[c1-id 0 0]) !>(sont.own:(need point.res)))
  ==
::  ---- tip spent in a later block -----------------------------------
++  test-tip-spent-in-later-block
  ::  best-height 102: tip block (101) clean, spend lands at 102 -> spent
  =/  s7  (drive-fetches 102 h-102)
  =/  s8  (answer-watch s7 (filter-wire 101) (filter-fact h-c1 101 (mk-filter h-c1 ~[tip-spk-bcm])))
  =/  s9  (answer-watch s8 (block-wire 101) (block-fact h-c1 101 ~[(bc-to-common c1-tx)]))
  =/  s10  (answer-watch s9 (filter-wire 102) (filter-fact h-102 102 (mk-filter h-102 ~[tip-spk-bcm])))
  =/  s11  (answer-watch s10 (block-wire 102) (block-fact h-102 102 ~[spend-tx-common]))
  =/  [res=result:sa tip=hexb:bitcoin]  (done-result s11)
  ;:  weld
    (expect-watch s7 (filter-wire 101) (filter-path 101))
    (expect-watch s9 (filter-wire 102) (filter-path 102))
    (expect-watch s10 (block-wire 102) (block-path 102))
    (expect !>(!ok.verdict.res))
    (expect !>(?=(~ point.res)))
  ==
::  ---- same-block spend (scan must start AT tip-height) --------------
++  test-same-block-spend
  ::  best-height 101: the tip output is created and spent in block 101
  =/  s7  (drive-fetches 101 h-c1)
  =/  s8  (answer-watch s7 (filter-wire 101) (filter-fact h-c1 101 (mk-filter h-c1 ~[tip-spk-bcm])))
  ::  the tip block holds c1-tx AND a later tx spending the tip outpoint
  =/  s9
    %^    answer-watch  s8  (block-wire 101)
    (block-fact h-c1 101 ~[(bc-to-common c1-tx) spend-tx-common])
  =/  [res=result:sa tip=hexb:bitcoin]  (done-result s9)
  ;:  weld
    (expect-watch s7 (filter-wire 101) (filter-path 101))
    (expect !>(!ok.verdict.res))
    (expect !>(?=(~ point.res)))
  ==
::  ---- undeterminable filter -> fail closed --------------------------
++  test-unknown-filter-fails-closed
  ::  a filter fact whose block-info height disagrees with the request is
  ::  unusable; liveness is undeterminable (~) and run-checks fails closed.
  =/  s7  (drive-fetches 101 h-c1)
  =/  s8
    %^    answer-watch  s7  (filter-wire 101)
    (filter-fact h-c1 999 (mk-filter h-c1 ~[tip-spk-bcm]))
  =/  [res=result:sa tip=hexb:bitcoin]  (done-result s8)
  ;:  weld
    (expect !>(!ok.verdict.res))
    (expect !>(?=(~ point.res)))
  ==
::  ---- height mismatch on a tx fetch -> strand fail ------------------
++  test-header-height-mismatch-fails-strand
  =/  s0  (step (verify-lc:lca good-sat ~ no-points) ~)
  =/  s1  (answer-watch s0 (hdr-wire 778.000) (hdr-fact h-start 778.000))
  =/  s2  (answer-watch s1 (tx-wire h-start start-id) (tx-fact h-start 778.000 start-id start-tx))
  ::  entry 0 header comes back with the wrong height
  =/  s3  (answer-watch s2 (hdr-wire 100) (hdr-fact h-c0 99))
  ?>  ?=(%fail -.halt.s3)
  ;:  weld
    (expect !>(=(~ cards.s3)))
    (expect-eq !>(%attestation-height-mismatch) !>(-.err.halt.s3))
  ==
::  ---- txid mismatch on a tx fetch -> strand fail --------------------
++  test-txid-mismatch-fails-strand
  =/  s0  (step (verify-lc:lca good-sat ~ no-points) ~)
  =/  s1  (answer-watch s0 (hdr-wire 778.000) (hdr-fact h-start 778.000))
  ::  the transaction fact reports a different txid than requested
  =/  s2  (answer-watch s1 (tx-wire h-start start-id) (tx-fact h-start 778.000 0xdead.beef start-tx))
  ?>  ?=(%fail -.halt.s2)
  ;:  weld
    (expect !>(=(~ cards.s2)))
    (expect-eq !>(%attestation-txid-mismatch) !>(-.err.halt.s2))
  ==
::  ---- transaction unknown (~) -> strand fail -----------------------
++  test-tx-not-found-fails-strand
  =/  s0  (step (verify-lc:lca good-sat ~ no-points) ~)
  =/  s1  (answer-watch s0 (hdr-wire 778.000) (hdr-fact h-start 778.000))
  =/  s2  (answer-watch s1 (tx-wire h-start start-id) tx-fact-empty)
  ?>  ?=(%fail -.halt.s2)
  ;:  weld
    (expect !>(=(~ cards.s2)))
    (expect-eq !>(%attestation-tx-not-found) !>(-.err.halt.s2))
  ==
--
