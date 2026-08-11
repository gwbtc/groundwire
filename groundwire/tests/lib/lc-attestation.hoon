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
    cc=gw-btc-pass, b-fil=compact-block-filters, strandio, libstrand=strand,
    ul=urb-core, ol=ord, bcu=bitcoin-utils
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
::  +no-timers: a strand's LIGHT-CLIENT cards, with timeout bookkeeping cut
::
::    Every one-shot request is wrapped in +set-timeout (see
::    +lc-fetch-timeout in lib/lc-attestation): the node's endpoints answer
::    when they have the data and otherwise wait for a peer forever, so a
::    light client whose peers have died wedges the whole verification --
::    and with it the ship's single-flight `inflight` slot, for the two
::    hours of +stuck-job-guard (live mainnet, Phase 6.2).  Bounding each
::    request turns that into a strand failure in minutes, which %gw-btc
::    already handles by releasing the slot and emitting no verdict.
::
::    That adds a %wait before each request and a %rest after it.  It is
::    bookkeeping rather than protocol, so the card-sequence assertions
::    look past it; +test-every-request-is-timeout-bounded pins it
::    directly instead.
::
++  no-timers
  |=  cards=(list card:agent:gall)
  ^-  (list card:agent:gall)
  %+  skip  cards
  |=  c=card:agent:gall
  ?=([%pass [%timeout *] %arvo %b *] c)
::
++  expect-watch
  |=  [st=pace =wire =path]
  ^-  tang
  ;:  weld
    (expect !>(?=(%wait -.halt.st)))
    (expect-eq !>(~[(watch-card wire path)]) !>((no-timers cards.st)))
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
  (no-timers (zing (turn states |=(st=pace cards.st))))
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
::  --------------------------------------------------------------------
::  REAL MAINNET PUBLIC SPAWN -- the whole transaction, byte for byte.
::
::    ec5c1fbe...97c8 in block 961.059 is the on-chain spawn of the live
::    PUBLIC comet C3
::    ~ligdes-risbur-folmus-mattyp--firpec-lispec-noddyl-daplyd.  Input 0
::    spends its funding satpoint 72340acb...:1; output 0 is the P2TR
::    committing its initial snapshot; output 1 is the OP_RETURN
::    publication that opens that commitment.
::
::    Written below in the DESK's byte order and converted to the NODE's
::    by +bc-hexb-to-common, so what these tests hand the fetch layer is
::    what a synced %bitcoin-client delivers over /block/height/961059.
::  --------------------------------------------------------------------
++  c3-height  961.059
++  c3-block-hash
  ^-  @ux
  0x321b.0588.8664.9375.5cb5.3fd3.7d34.c575.febf.d7bf.de4a
::
++  c3-txid
  ^-  @ux
  0xec5c.1fbe.b697.211b.fcf5.87e3.b985.f6e9.9e5f.f8e8.d2d9.2f08.3a25.d55b.3bcc.97c8
::
++  c3-funding-txid
  ^-  @ux
  0x7234.0acb.1b42.16f3.e1ff.0da2.2322.79e4.3326.cd53.0833.31fb.f2ee.7774.daa9.bc54
::  output 0: the P2TR whose output key IS (state-key ikey snapshot)
++  c3-state-spk
  ^-  hexb:bitcoin
  :-  34
  0x5120.0ca2.828c.764a.0f3e.76e3.3df0.7be9.8703.06ec.5650.af8e.cae8.8f6f.0c64.2ec8.0bee
::  output 1: the 263-byte OP_RETURN publication, assembled in 32-byte
::  pieces because Hoon has no multi-line atom literal
++  c3-opret-spk
  ^-  hexb:bitcoin
  %-  cat:byt:bcu
  :~  [32 0x6a03.7572.6201.094c.fe01.e0d7.b1e4.3183.30b6.a4cb.f611.89b7.4843.1a2f.0db8.d52c]
      [32 0x9d5a.33a3.60b9.13da.ecf2.313f.ac2b.1c52.3e5e.b8af.7368.89af.70ca.b782.48c0.6336]
      [32 0x75d3.6206.afef.feb2.7a19.4b98.6f00.8b80.efec.ae45.8c6e.8c24.0559.3752.59bd.9ddf]
      [32 0x4ec3.e010.4ec2.b493.1a7b.ccf7.4d2d.ea25.3ced.8334.54ea.a369.0014.18b8.43cf.e76c]
      [32 0x4dbf.a406.afb4.4181.2664.9f53.46ab.a731.8cf0.e114.672d.5d7f.4f09.b738.0320.c015]
      [32 0xe29.1f2f.dcd7.39b4.c457.38e5.5b41.24e0.319b.ba69.3183.d777.7f59.bd8c.25cc.779a]
      [32 0x5e0.9f8a.3755.9bee.ce5d.7e3f.6606.61aa.d964.863c.4f64.44b4.e13f.7cde.4268.6359]
      [32 0x8146.1e9b.0004.4820.8cb0.4b69.dd41.709a.02b3.5758.771b.a7b8.403e.86f2.d114.50f2]
      [7 0x6b.623b.bd79.5d07]
  ==
::
++  c3-witness
  ^-  hexb:bitcoin
  %-  cat:byt:bcu
  :~  [32 0xffc8.1c01.1756.f099.9034.69e9.13fa.7414.a333.c51d.333d.226c.2b85.e677.2cce.f066]
      [32 0x9f.6178.5555.1eda.3750.6a6e.59a6.cd96.a719.daf1.13f5.1f73.fe46.8d19.b375.a90f]
  ==
::  the same transaction as the node hands it over: no txid (the node's
::  $block is positional), witness present, every byte string reversed.
++  c3-tx-node
  ^-  transaction:bcm
  :*  0x2  1
      :~  ^-  transaction-input:bcm
          :*  c3-funding-txid
              1
              `hexb:bcm`[0 0x0]
              0xffff.ffff
              ~[(bc-hexb-to-common c3-witness)]
          ==
      ==
      :~  `transaction-output:bcm`[1.615 (bc-hexb-to-common c3-state-spk)]
          `transaction-output:bcm`[0 (bc-hexb-to-common c3-opret-spk)]
      ==
      0
  ==
::  a stand-in coinbase; +find-block-reveals never scans it, but every
::  block has one and the scanner's sat math indexes through it.
++  cb-tx-node
  ^-  transaction:bcm
  :*  0x1  0
      ~[`transaction-input:bcm`[0x0 4.294.967.295 [4 0x0] 0xffff.ffff ~]]
      ~[`transaction-output:bcm`[312.500.000 (bc-hexb-to-common [25 0x76.a914.88ac])]]
      0
  ==
::  THE CONTRAST: a CONFIDENTIAL spawn.  C1
::  ~havnyl-lonpub-botben-hidleb--lomper-marryc-lanmec-daplyd committed
::  its snapshot in exactly the same shape -- a real P2TR whose output
::  key is a real state-key (+real-spk-node, the bytes the node delivered
::  for it at height 961.055) -- and published NOTHING.  A scanner that
::  indexed "any taproot output" would pick this up; the real one must
::  not.  (Only the input's txid is synthetic: C1's funding outpoint is
::  not needed to make the point, and nothing here reads it.)
++  conf-tx-node
  ^-  transaction:bcm
  :*  0x2  1
      :~  ^-  transaction-input:bcm
          :*  0x394f.3678.9ed2.f0f1.b076.a24c
              1
              `hexb:bcm`[0 0x0]
              0xffff.ffff
              ~[(bc-hexb-to-common c3-witness)]
          ==
      ==
      ~[`transaction-output:bcm`[1.889 real-spk-node]]
      0
  ==
::
::  +scan-block: exactly what +get-blocks:gw-btc does with a fetched block
++  scan-block
  |=  [st=state:urb =block:bitcoin]
  ^-  [(list [id:block:bitcoin effect:urb]) state:urb]
  =/  oc   (abed:urb-core:ul st)
  =/  fbr  (find-block-reveals:oc block)
  =.  oc   oc(hax.block-id.state hax.block)
  abet:(handle-block:oc (apply-prevouts-and-urbify:oc +.fbr -.fbr))
::  the block as it arrives from /block/height/961059, converted by the
::  code under test.
++  c3-block
  ^-  block:bitcoin
  %^  common-block-to-bc:lca  c3-block-hash  c3-height
  [dummy-hdr ~[cb-tx-node c3-tx-node]]
::
++  conf-block
  ^-  block:bitcoin
  %^  common-block-to-bc:lca  c3-block-hash  c3-height
  [dummy-hdr ~[cb-tx-node conf-tx-node]]
::
++  empty-at
  |=  height=@ud
  ^-  state:urb
  [[0x0 (dec height)] *sont-map:ord *insc-ids:ord *unv-ids:urb]
::
++  effs
  |=  fx=(list [id:block:bitcoin effect:urb])
  ^-  (list effect:urb)
  (turn fx |=([* e=effect:urb] e))
::
++  c3  ~ligdes-risbur-folmus-mattyp--firpec-lispec-noddyl-daplyd
::  --------------------------------------------------------------------
::  Fixtures for +verify-lc's EARLY ABORTS -- the returns that never reach
::  ++run-checks and therefore carry their own $abort rather than a list
::  of checks.
::  --------------------------------------------------------------------
::
::  a well-formed suite-C %gw-btc pass whose xtr decodes to an EMPTY log
++  empty-pass  pub:ex:(pit:nu:cric:crypto 512 (shaz seed) %c dat (jam ~))
++  empty-sat   ^-(self-attestation:sa [who empty-pass ~])
::  ... and one whose entry 0 carries no opening, so nothing opens the
::  hiding dat commitment and the log is never bound to the name.
++  no-open-chain  ^-(custody-log:sa ~[[c0-id 100 ~] [c1-id 101 ~]])
++  no-open-pass
  pub:ex:(pit:nu:cric:crypto 512 (shaz seed) %c dat (jam no-open-chain))
++  no-open-sat  ^-(self-attestation:sa [who no-open-pass no-open-chain])
::  the tip transaction with the SAME txid the log claims (so +fetch-tx-at
::  accepts it) but an input 0 that spends something else entirely, so the
::  custody hop the log describes did not happen.
++  c1-tx-broken
  (mk-tx c1-id ~[(mk-input 0xbaad.0000 0 keypath-wit)] ~[tip-out])
::
++  start-verify-sat
  |=  [sat=self-attestation:sa best-het=@ud]
  ^-  pace
  (step (verify-lc:lca sat ~ no-points best-het) ~)
::  +drive-fetches, with the LAST transaction supplied by the caller.
::
++  drive-fetches-tip
  |=  [best-het=@ud tip-tx=tx:bc]
  ^-  pace
  =/  s0  (start-verify best-het)
  =/  s1  (answer-watch s0 (hdr-wire 778.000) (hdr-fact h-start 778.000))
  =/  s2  (answer-watch s1 (tx-wire h-start start-id) (tx-fact h-start 778.000 start-id start-tx))
  =/  s3  (answer-watch s2 (hdr-wire 100) (hdr-fact h-c0 100))
  =/  s4  (answer-watch s3 (tx-wire h-c0 c0-id) (tx-fact h-c0 100 c0-id c0-tx))
  =/  s5  (answer-watch s4 (hdr-wire 101) (hdr-fact h-c1 101))
  (answer-watch s5 (tx-wire h-c1 c1-id) (tx-fact h-c1 101 c1-id tip-tx))
::  the single failing check name an abort result carries
::
++  only-check
  |=  =verdict:sa
  ^-  cord
  ?>  ?=([* ~] checks.verdict)
  ?>  !ok.i.checks.verdict
  name.i.checks.verdict
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
    (expect !>(=(~ (no-timers cards.s8))))
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
::  ---- a DEGENERATE scan range is undeterminable, not clean ----------
::
::    THE Phase 6.1 fail-open.  +scan-liveness's loop exits when
::    `h > best-height`; with a chain tip BELOW the tip block that test is
::    true on the first iteration, and the old code returned `%.y --
::    "unspent" -- having looked at nothing.  Live mainnet 2026-08-06:
::    `[%gw-btc-lc-scan-clean ... from=961.196 to=0]`, on an unsynced
::    light client that believed the chain tip was genesis.  A SPENT tip
::    would have read identically, so this is a fail-open on the one check
::    the design most wants to fail closed.
::
::    The tip block here is 101 and the caller's tip is 100.  Nothing may
::    be scanned, the result must be undeterminable, and the verdict must
::    name `tip-scanned` (unevaluable) rather than `tip-unspent` (proven
::    spent) -- the difference between silence and a peer demotion.
::
++  test-degenerate-scan-range-is-undeterminable
  =/  s6  (drive-fetches 100)
  =/  [res=result:sa tip=hexb:bitcoin]  (done-result s6)
  ;:  weld
    ::  the strand finished WITHOUT asking for a single filter or block
    (expect !>(?=(%done -.halt.s6)))
    (expect-eq !>(~) !>((no-timers cards.s6)))
    (expect !>(!ok.verdict.res))
    (expect !>(?=(~ point.res)))
    ::  unevaluable, not spent
    (expect !>((lien checks.verdict.res |=(c=check:sa &(=(%tip-scanned name.c) !ok.c)))))
    (expect !>((lien checks.verdict.res |=(c=check:sa &(=(%tip-unspent name.c) ok.c)))))
    ::  ... so it produces NO verdict at all rather than a demotion
    (expect !>((unknown-verdict:lsa verdict.res)))
    (expect !>(!(stale-verdict:lsa verdict.res)))
  ==
::  ---- every light-client request is timeout-bounded -----------------
::
::    Phase 6.2: a runtime fault during verification left the strand
::    wedged on a light-client watch that would never answer, holding the
::    peer's single-flight slot for the full +stuck-job-guard of two
::    hours, during which two retries were dropped in silence.  The node
::    never nacks -- an endpoint it cannot serve registers a pending
::    request and waits -- so the bound has to be here.
::
++  test-every-request-is-timeout-bounded
  =/  s0  (start-verify 101)
  =/  timers
    %+  skim  cards.s0
    |=(c=card:agent:gall ?=([%pass [%timeout *] %arvo %b %wait *] c))
  ;:  weld
    ::  the very first request is preceded by exactly one behn timer ...
    (expect-eq !>(1) !>((lent timers)))
    ::  ... and nothing else rides along with it
    (expect-eq !>(2) !>((lent cards.s0)))
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
::  ---- the node's blocks carry no txids; we recompute them -----------
::
::    A block fact is positional: [header txs], no hashes anywhere.  The
::    whole public scanner keys on txids, so every one is recomputed from
::    the transaction itself.  A wrong txid is SILENT -- the comet still
::    indexes, just at a satpoint nothing will ever spend -- so this is
::    pinned against a real mainnet transaction rather than a round trip.
::
::    Note it is a SEGWIT transaction (flag=1, a 64-byte key-path witness):
::    the legacy, witness-stripped serialization is what a txid commits
::    to, and using the segwit one would produce the wtxid instead.
::
++  test-real-mainnet-txid
  =/  blk  c3-block
  ;:  weld
    (expect-eq !>(c3-txid) !>((node-txid:lca c3-tx-node)))
    ::  ... and it lands on the transaction inside the converted block
    (expect-eq !>(c3-txid) !>(id:(snag 1 txs.blk)))
    ::  the block's own identity survives the conversion
    (expect-eq !>(c3-block-hash) !>(hax.blk))
    (expect-eq !>(c3-height) !>(height.blk))
    ::  block 961.059 is in the fourth halving epoch: 3.125 BTC
    (expect-eq !>(`@ud`312.500.000) !>(reward.blk))
  ==
::  ---- a REAL on-chain publication is CLAIMED, off real bytes --------
::
::    Phase-1 test 1.5, offline: the identity is learned from the chain
::    alone, with no packet exchange of any kind.
::
::    This arm used to assert that the scan INDEXED C3 -- a point in
::    .unv-ids, an owner in the sat index, a %point/%owner effect.  It
::    does not any more, and 4ae85b8 is why: a publication IS the
::    comet's whole attestation packet, and the scanner cannot walk a
::    custody log because the log names transactions in blocks the
::    scanner has already streamed past.  So +process-publication reads
::    the envelope, completes the log with the two facts only a block
::    reader has -- this transaction's txid and this block's height --
::    and emits a %claim, which %gw-btc hands to the SAME +verify-lc a
::    mailed %jael-writ gets.  Indexing is that arm's business now.
::
::    What is left to pin here is the envelope, and it is worth pinning
::    off REAL MAINNET BYTES rather than a synthetic vector (which
::    tests/lib/urb-core.hoon already does at length): one %claim, named
::    by the fingerprint of C3's own pass, carrying the log completed
::    with the entry the publisher could not write down -- and an index
::    that is still empty, because nothing has been judged yet.
::
++  test-real-mainnet-publication-claims-c3
  =/  [fx=(list [id:block:bitcoin effect:urb]) st=state:urb]
    (scan-block (empty-at c3-height) c3-block)
  =/  cs=(list [who=ship =pass])
    %+  murn  (effs fx)
    |=  e=effect:urb
    ^-  (unit [ship pass])
    ?.(?=([%claim *] e) ~ `[who.e pass.e])
  ::  exactly one %claim, and it is the ONLY effect: an unjudged
  ::  publication tells jael nothing.
  ?>  ?=([* ~] cs)
  ::  the claimed pass is a well-formed %gw-btc self-attestation whose
  ::  fingerprint is the name it was claimed under -- +from-xtr checks
  ::  the domain, the kelvin, the suite, the @p and the log's canonical
  ::  jam, so this is not a shape assertion, it is the packet grammar.
  =/  sat  (from-xtr:lsa who.i.cs pass.i.cs)
  ?>  ?=(^ sat)
  ::  C3 published AT SPAWN, so the log it carried was empty and the
  ::  completed one is a single entry: this transaction, at this block's
  ::  height, opening the snapshot that output 0 commits.
  ?>  ?=([* ~] chain.u.sat)
  =*  ent  i.chain.u.sat
  ?>  ?=(^ opening.ent)
  ;:  weld
    (expect-eq !>(`@p`c3) !>(who.i.cs))
    (expect-eq !>(`(list effect:urb)`~[[%claim who.i.cs pass.i.cs]]) !>((effs fx)))
    (expect-eq !>(`txid:ord`c3-txid) !>(txid.ent))
    (expect-eq !>(`@ud`c3-height) !>(height.ent))
    (expect-eq !>(`life`1) !>(life.snapshot.u.opening.ent))
    (expect-eq !>(`rift`0) !>(rift.snapshot.u.opening.ent))
    ::  NOTHING is indexed.  The claim is evidence, not a verdict.
    (expect-eq !>(*unv-ids:urb) !>(unv-ids.st))
    (expect-eq !>(*(unit @p)) !>((get-com:si:ol sont-map.st c3-txid 0 0)))
    ::  the scanner's cursor advanced onto the block it just read
    (expect-eq !>(`id:block:bitcoin`[c3-block-hash c3-height]) !>(block-id.st))
  ==
::  ---- a CONFIDENTIAL spawn must NOT appear --------------------------
::
::    C1 and C2 spawned with the same on-chain shape as C3 -- a P2TR
::    output whose key is a genuine state-key -- and published no
::    OP_RETURN.  They are visible only to the confidential verifier, and
::    the public index must never learn them.  This is the contrast that
::    proves the scanner reads PUBLICATIONS and not merely taproot
::    outputs: same block height, same converted-block path, same
::    scanner; the only difference is the OP_RETURN.
::
++  test-confidential-spawn-is-not-indexed
  =/  [fx=(list [id:block:bitcoin effect:urb]) st=state:urb]
    (scan-block (empty-at c3-height) conf-block)
  ;:  weld
    (expect-eq !>(*unv-ids:urb) !>(unv-ids.st))
    (expect-eq !>(*(list effect:urb)) !>((effs fx)))
    ::  it really is a well-formed P2TR state commitment, not junk that
    ::  would have been skipped for some incidental reason
    %+  expect-eq
      !>  `(unit @ux)`[~ real-xonly]
      !>  (p2tr-xonly:lsa (flip-hexb:lca real-spk-node))
    ::  and the cursor still advanced: the block was scanned, not skipped
    (expect-eq !>(c3-height) !>(num.block-id.st))
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
::  ---- +verify-lc's EARLY ABORTS, and what each one is worth -----------
::
::    THE fail-open-to-snub path this section exists to close.  Four
::    returns in +verify-lc never reach ++run-checks and so never produce
::    a list of checks -- just one name.  Those names belonged to no class
::    at all, and %gw-btc's classifier fell through to fraud for anything
::    it did not recognise, so ALL FOUR ended in a sticky ames snub.  Two
::    of them deserve it, one deserves it after an argument, and one fires
::    only when this desk's own arithmetic is wrong.
::
::    Each test below drives the REAL strand to the real abort and pins
::    both the name and the class the agent will route on.
::
::  An empty custody log is judged before a single watch card is emitted:
::  no fetch can have gone wrong, because no fetch happened.  A suite-C
::  %gw-btc pass asserts a confidential identity and this one offers no
::  evidence for it -- exactly the condition ++run-checks calls
::  `chain-nonempty' and also treats as fraud.
::
++  test-empty-chain-abort-is-fraud
  =/  st  (start-verify-sat empty-sat 101)
  =/  [res=result:sa *]  (done-result st)
  ;:  weld
    ::  finished immediately, having asked the light client for nothing
    (expect !>(?=(%done -.halt.st)))
    (expect-eq !>(~) !>((no-timers cards.st)))
    (expect !>(!ok.verdict.res))
    (expect-eq !>('empty-chain') !>((only-check verdict.res)))
    ::  ... and it really is the fraud class, i.e. this one DOES snub
    (expect-eq !>(%fraud) !>((classify:lsa verdict.res)))
    (expect !>(!(unknown-verdict:lsa verdict.res)))
    (expect !>(!(stale-verdict:lsa verdict.res)))
  ==
::
::  Entry 0 with no opening never opens the pass's hiding dat commitment,
::  so the log is not bound to the name it arrived under.  Again purely
::  structural, again decided from the peer's own xtr before any fetch.
::
++  test-spawn-opening-abort-is-fraud
  =/  st  (start-verify-sat no-open-sat 101)
  =/  [res=result:sa *]  (done-result st)
  ;:  weld
    (expect !>(?=(%done -.halt.st)))
    (expect-eq !>(~) !>((no-timers cards.st)))
    (expect !>(!ok.verdict.res))
    (expect-eq !>('spawn-opening') !>((only-check verdict.res)))
    (expect-eq !>(%fraud) !>((classify:lsa verdict.res)))
    (expect !>(!(unknown-verdict:lsa verdict.res)))
  ==
::
::  +derive-tip is the one that sits AFTER the fetches, so "the fetch went
::  wrong" would be a fair reading -- except it cannot be.  Every
::  transaction reaching +derive-tip came through +fetch-tx-at, which
::  STRAND-FAILS (see the three tests above) rather than return on a
::  height mismatch, a txid mismatch, or an unknown transaction, and a
::  failed strand emits no verdict at all.  So the evidence here was fully
::  obtained and is genuinely on-chain; what fails is the peer's claim
::  about it.  The tip transaction below has the exact txid the log
::  claims -- the fetch layer is perfectly happy with it -- and an input 0
::  that spends an outpoint the log never mentions.  That hop did not
::  happen, and ++run-checks would call the same thing
::  `entry-1-continuity' and snub for it.
::
++  test-derive-tip-abort-is-fraud
  =/  st  (drive-fetches-tip 101 c1-tx-broken)
  =/  [res=result:sa *]  (done-result st)
  ;:  weld
    ::  it got all the way through the six fetches before aborting
    (expect !>(?=(%done -.halt.st)))
    (expect !>(!ok.verdict.res))
    (expect-eq !>('derive-tip') !>((only-check verdict.res)))
    (expect-eq !>(%fraud) !>((classify:lsa verdict.res)))
    (expect !>(!(unknown-verdict:lsa verdict.res)))
    (expect !>(!(stale-verdict:lsa verdict.res)))
    ::  no liveness scan was attempted -- the log never produced a tip to
    ::  scan for, so the abort really did happen where we think it did.
    (expect-eq !>(~) !>((no-timers cards.st)))
  ==
::
::  The fourth abort, %tip-vout-range, has NO test here on purpose, and
::  the reason is the reason it is classed %unknown: it is unreachable.
::  +derive-tip's last hop takes vout from +index-to-sont:urb-core, which
::  only ever names an output that exists, over the very list the bound
::  re-checks -- so no chain data, honest or forged, can drive the strand
::  into it.  Reaching it would mean this desk's ordinal arithmetic had
::  contradicted itself, which is evidence about us and none about the
::  peer.  Its class and its routing are pinned where they can be:
::  +test-abort-classes-are-assigned-deliberately in
::  tests/lib/self-attestation, and +test-abort-names-never-snub-when-
::  unknown in tests/app/gw-btc.
::
::  What IS testable here is that the abort results are shaped the way the
::  classifier expects: exactly one failing check, named by the abort.
::
++  test-abort-results-are-single-check
  =/  a  (start-verify-sat empty-sat 101)
  =/  b  (start-verify-sat no-open-sat 101)
  =/  c  (drive-fetches-tip 101 c1-tx-broken)
  =/  shape
    |=  st=pace
    ^-  ?
    =/  [res=result:sa *]  (done-result st)
    ?&  ?=([* ~] checks.verdict.res)
        !ok.verdict.res
        ?=(~ point.res)
        =(0 tip-value.res)
    ==
  ;:  weld
    (expect !>((shape a)))
    (expect !>((shape b)))
    (expect !>((shape c)))
  ==
--
