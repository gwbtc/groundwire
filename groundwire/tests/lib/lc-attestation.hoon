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
::  TWO THINGS THIS FILE EXISTS TO PIN, both of which shipped broken and
::  made confidential comets 100% non-functional against real mainnet:
::
::    1. The strand must never watch /best-block.  That endpoint is a
::       PERSISTENT subscription (fact per block, no kick), so a
::       +watch-one on it hangs forever.  The chain tip is an ARGUMENT.
::       +test-no-best-block-watch asserts no such card is ever emitted.
::
::    2. The node's $hexb is byte-reversed relative to the desk's.  The
::       fixtures below therefore build node facts in the NODE's real byte
::       order (+bc-hexb-to-common reverses), so a fetch layer that forgets
::       to convert fails these tests instead of failing on mainnet.  Note
::       the flip has TWO halves that must move together -- see
::       +test-spent-tip-is-not-masked-by-byte-order.
::
/-  bitcoin, ord, urb, sa=self-attestation, lc=light-client, bcm=bitcoin-common
/+  *test, lca=lc-attestation, lsa=self-attestation, tr=taproot, bc=bitcoin,
    cc=gw-btc-pass, b-fil=compact-block-filters, strandio, libstrand=strand
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
++  watch-card
  |=  [=wire =path]
  ^-  card:agent:gall
  [%pass watch+wire %agent [~zod light-client-agent:lca] %watch path]
::
++  expect-watch
  |=  [st=pace =wire =path]
  ^-  tang
  ;:  weld
    (expect !>(?=(%wait -.halt.st)))
    (expect-eq !>(~[(watch-card wire path)]) !>(cards.st))
  ==
::  --------------------------------------------------------------------
::  Node-shape (bitcoin-common) fact builders.
::  --------------------------------------------------------------------
::  +bc-hexb-to-common: put a desk byte string into the NODE's byte order
::
::    The node holds the first wire byte in the LOW byte of `dat`; the desk
::    holds it in the HIGH byte.  This is the exact inverse of
::    +flip-hexb:lca, so a fixture built here and read back through the
::    fetch layer reproduces the original desk value.
::
++  bc-hexb-to-common
  |=  h=hexb:bitcoin
  ^-  hexb:bcm
  [`@ud`wid.h `@ux`(rev 3 wid.h dat.h)]
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
::  the persistent endpoint the strand must NEVER watch
++  best-path  /best-block
++  filter-wire  |=(h=@ud /lc/filter-height/(scot %ud h))
++  filter-path  |=(h=@ud /block-filter/height/(scot %ud h))
++  block-wire   |=(h=@ud /lc/block-height/(scot %ud h))
++  block-path   |=(h=@ud /block/height/(scot %ud h))
::  --------------------------------------------------------------------
::  Drive the six transaction fetches, leaving the strand blocked on the
::  first filter watch (height = tip-height = 101).  .best-het is the
::  chain tip the CALLER supplies -- there is no /best-block round trip.
::  --------------------------------------------------------------------
++  start-verify
  |=  best-het=@ud
  ^-  pace
  (step (verify-lc:lca good-sat ~ no-points best-het) ~)
::
++  drive-fetches
  |=  best-het=@ud
  ^-  pace
  =/  s0  (start-verify best-het)
  =/  s1  (answer-watch s0 (hdr-wire 778.000) (hdr-fact h-start 778.000))
  =/  s2  (answer-watch s1 (tx-wire h-start start-id) (tx-fact h-start 778.000 start-id start-tx))
  =/  s3  (answer-watch s2 (hdr-wire 100) (hdr-fact h-c0 100))
  =/  s4  (answer-watch s3 (tx-wire h-c0 c0-id) (tx-fact h-c0 100 c0-id c0-tx))
  =/  s5  (answer-watch s4 (hdr-wire 101) (hdr-fact h-c1 101))
  (answer-watch s5 (tx-wire h-c1 c1-id) (tx-fact h-c1 101 c1-id c1-tx))
::
::  every card a run of the strand emitted, in order
++  all-cards
  |=  states=(list pace)
  ^-  (list card:agent:gall)
  (zing (turn states |=(st=pace cards.st)))
::
++  done-result
  |=  st=pace
  ^-  [result:sa hexb:bitcoin]
  ?>  ?=(%done -.halt.st)
  !<([result:sa hexb:bitcoin] value.halt.st)
::  --------------------------------------------------------------------
::  REAL mainnet byte-order vector.  The sat-carrying output of the spawn
::  transaction of the live comet ~havnyl-lonpub-botben-hidleb--lomper-
::  marryc-lanmec-daplyd (2c66c654...  in block 961.055).  `real-spk` is
::  the scriptPubKey as Bitcoin and this desk write it; `real-spk-node` is
::  the byte string the node's %bitcoin-client actually delivered for it,
::  observed on a synced mainnet light client.
::  --------------------------------------------------------------------
++  real-spk
  ^-  hexb:bitcoin
  :-  34
  0x5120.be3a.7444.26a8.7262.b1bd.cc14.4b43.5109.9503.34de.f1f2.6747.d316.367d.6af6.8341
::
++  real-spk-node
  ^-  hexb:bcm
  :-  34
  0x4183.f66a.7d36.16d3.4767.f2f1.de34.0395.0951.434b.14cc.bdb1.6272.a826.4474.3abe.2051
::
++  real-xonly
  ^-  @ux
  0xbe3a.7444.26a8.7262.b1bd.cc14.4b43.5109.9503.34de.f1f2.6747.d316.367d.6af6.8341
--
|%
::  ---- node <-> desk byte order, pinned to real mainnet data ---------
::
::    $hexb:bitcoin-common and $hexb:bitcoin are structurally identical,
::    so a missing conversion type-checks perfectly and only fails against
::    a real node.  Shipped code passed scriptPubKeys straight through and
::    +p2tr-xonly then read 0x2051 as the version prefix, so EVERY
::    entry-N-commitment and tip-p2tr check failed on real chain data.
::
++  test-node-hexb-byte-order
  =/  converted  (flip-hexb:lca real-spk-node)
  ;:  weld
    ::  the node's bytes convert to exactly the on-chain scriptPubKey
    (expect-eq !>(real-spk) !>(converted))
    ::  ... which +p2tr-xonly can actually read
    (expect-eq !>(`(unit @ux)`[~ real-xonly]) !>((p2tr-xonly:lsa converted)))
    ::  ... and which the UNconverted value cannot pretend to be
    (expect-eq !>(`(unit @ux)`~) !>((p2tr-xonly:lsa `hexb:bitcoin`real-spk-node)))
    ::  round trip is the identity in both directions
    (expect-eq !>(real-spk-node) !>((unflip-hexb:lca converted)))
    (expect-eq !>(real-spk) !>((flip-hexb:lca (unflip-hexb:lca real-spk))))
  ==
::  ---- a whole output crosses the boundary intact --------------------
++  test-common-out-conversion
  =/  out  (common-out-to-bc:lca `transaction-output:bcm`[1.889 real-spk-node])
  ;:  weld
    (expect-eq !>(real-spk) !>(`hexb:bitcoin`-.out))
    (expect-eq !>(`@ud`1.889) !>(`@ud`+.out))
  ==
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
  ::  best-height == tip-height (101): the scan visits only the tip block.
  =/  s0  (start-verify 101)
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
  =/  c6c  (expect-watch s6 (filter-wire 101) (filter-path 101))
  =/  s7  (answer-watch s6 (filter-wire 101) (filter-fact h-c1 101 (mk-filter h-c1 ~[tip-spk-bcm])))
  =/  c7c  (expect-watch s7 (block-wire 101) (block-path 101))
  ::  tip block contains only c1-tx (no tx spends the tip outpoint) -> unspent
  =/  s8  (answer-watch s7 (block-wire 101) (block-fact h-c1 101 ~[(bc-to-common c1-tx)]))
  =/  [res=result:sa tip=hexb:bitcoin]  (done-result s8)
  ;:  weld
    c0c  c1c  c2c  c3c  c4c  c5c  c6c  c7c
    (expect !>(=(~ cards.s8)))
    (expect !>(ok.verdict.res))
    (expect !>(?=(^ point.res)))
    ::  the tip scriptPubKey came back through the node's byte order and
    ::  out again in the desk's -- unchanged.
    (expect-eq !>(tip-spk) !>(tip))
    (expect-eq !>(`sont:ord`[c1-id 0 0]) !>(sont.own:(need point.res)))
  ==
::  ---- /best-block must never be watched from the strand -------------
::
::    /best-block is a PERSISTENT subscription on the node: it gives a
::    fact for every new block and NEVER kicks.  A +watch-one on it blocks
::    the strand forever, so tip liveness is never determined and NO
::    confidential attestation can ever produce a verdict.  The chain tip
::    is passed in by %gw-btc instead, which holds its own subscription.
::
++  test-no-best-block-watch
  =/  s0  (start-verify 101)
  =/  s1  (answer-watch s0 (hdr-wire 778.000) (hdr-fact h-start 778.000))
  =/  s2  (answer-watch s1 (tx-wire h-start start-id) (tx-fact h-start 778.000 start-id start-tx))
  =/  s3  (answer-watch s2 (hdr-wire 100) (hdr-fact h-c0 100))
  =/  s4  (answer-watch s3 (tx-wire h-c0 c0-id) (tx-fact h-c0 100 c0-id c0-tx))
  =/  s5  (answer-watch s4 (hdr-wire 101) (hdr-fact h-c1 101))
  =/  s6  (answer-watch s5 (tx-wire h-c1 c1-id) (tx-fact h-c1 101 c1-id c1-tx))
  =/  s7  (answer-watch s6 (filter-wire 101) (filter-fact h-c1 101 (mk-filter h-c1 ~[tip-spk-bcm])))
  =/  s8  (answer-watch s7 (block-wire 101) (block-fact h-c1 101 ~[(bc-to-common c1-tx)]))
  ;:  weld
    ::  the run completed rather than blocking on a subscription
    (expect !>(?=(%done -.halt.s8)))
    ::  EVERY card a complete verification emits, exhaustively.  No
    ::  best-path watch appears, and none can be added without failing
    ::  here.
    %+  expect-eq
      !>  ^-  (list card:agent:gall)
          :~  (watch-card (hdr-wire 778.000) (hdr-path 778.000))
              (watch-card (tx-wire h-start start-id) (tx-path h-start start-id))
              (watch-card (hdr-wire 100) (hdr-path 100))
              (watch-card (tx-wire h-c0 c0-id) (tx-path h-c0 c0-id))
              (watch-card (hdr-wire 101) (hdr-path 101))
              (watch-card (tx-wire h-c1 c1-id) (tx-path h-c1 c1-id))
              (watch-card (filter-wire 101) (filter-path 101))
              (watch-card (block-wire 101) (block-path 101))
          ==
      !>((all-cards ~[s0 s1 s2 s3 s4 s5 s6 s7 s8]))
  ==
::  ---- tip spent in a later block -----------------------------------
::
::    THE fail-open regression guard.  The GCS matcher is the NODE's own,
::    and consumes its targets in the NODE's byte order, while the tip
::    scriptPubKey now reaches +scan-liveness in the DESK's.  Convert the
::    transaction side (+common-out-to-bc) without also converting back
::    here and the filter silently stops matching: a SPENT tip is reported
::    UNSPENT and a stale identity verifies.  Because the filter below is
::    built from the same node-order bytes a real node would use, half a
::    fix fails this test rather than shipping a fail-open.
::
++  test-spent-tip-is-not-masked-by-byte-order
  ::  best-height 102: tip block (101) clean, spend lands at 102 -> spent
  =/  s6  (drive-fetches 102)
  =/  s7  (answer-watch s6 (filter-wire 101) (filter-fact h-c1 101 (mk-filter h-c1 ~[tip-spk-bcm])))
  =/  s8  (answer-watch s7 (block-wire 101) (block-fact h-c1 101 ~[(bc-to-common c1-tx)]))
  =/  s9  (answer-watch s8 (filter-wire 102) (filter-fact h-102 102 (mk-filter h-102 ~[tip-spk-bcm])))
  =/  s10  (answer-watch s9 (block-wire 102) (block-fact h-102 102 ~[spend-tx-common]))
  =/  [res=result:sa tip=hexb:bitcoin]  (done-result s10)
  ;:  weld
    (expect-watch s6 (filter-wire 101) (filter-path 101))
    (expect-watch s8 (filter-wire 102) (filter-path 102))
    (expect-watch s9 (block-wire 102) (block-path 102))
    ::  the filter for 102 MATCHED (else no block would have been fetched)
    (expect !>(!ok.verdict.res))
    (expect !>(?=(~ point.res)))
    ::  and the failure named is tip liveness, not something incidental
    (expect !>((lien checks.verdict.res |=(c=check:sa &(=(%tip-unspent name.c) !ok.c)))))
  ==
::  ---- same-block spend (scan must start AT tip-height) --------------
++  test-same-block-spend
  ::  best-height 101: the tip output is created and spent in block 101
  =/  s6  (drive-fetches 101)
  =/  s7  (answer-watch s6 (filter-wire 101) (filter-fact h-c1 101 (mk-filter h-c1 ~[tip-spk-bcm])))
  ::  the tip block holds c1-tx AND a later tx spending the tip outpoint
  =/  s8
    %^    answer-watch  s7  (block-wire 101)
    (block-fact h-c1 101 ~[(bc-to-common c1-tx) spend-tx-common])
  =/  [res=result:sa tip=hexb:bitcoin]  (done-result s8)
  ;:  weld
    (expect-watch s6 (filter-wire 101) (filter-path 101))
    (expect !>(!ok.verdict.res))
    (expect !>(?=(~ point.res)))
  ==
::  ---- undeterminable filter -> fail closed --------------------------
++  test-unknown-filter-fails-closed
  ::  a filter fact whose block-info height disagrees with the request is
  ::  unusable; liveness is undeterminable (~) and run-checks fails closed.
  =/  s6  (drive-fetches 101)
  =/  s7
    %^    answer-watch  s6  (filter-wire 101)
    (filter-fact h-c1 999 (mk-filter h-c1 ~[tip-spk-bcm]))
  =/  [res=result:sa tip=hexb:bitcoin]  (done-result s7)
  ;:  weld
    (expect !>(!ok.verdict.res))
    (expect !>(?=(~ point.res)))
  ==
::  ---- height mismatch on a tx fetch -> strand fail ------------------
++  test-header-height-mismatch-fails-strand
  =/  s0  (start-verify 101)
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
  =/  s0  (start-verify 101)
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
  =/  s0  (start-verify 101)
  =/  s1  (answer-watch s0 (hdr-wire 778.000) (hdr-fact h-start 778.000))
  =/  s2  (answer-watch s1 (tx-wire h-start start-id) tx-fact-empty)
  ?>  ?=(%fail -.halt.s2)
  ;:  weld
    (expect !>(=(~ cards.s2)))
    (expect-eq !>(%attestation-tx-not-found) !>(-.err.halt.s2))
  ==
--
