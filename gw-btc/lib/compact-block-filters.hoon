::
::  lib/compact-block-filters.hoon
::
::  BIP-158 compact block filter (GCS) matching, copied VERBATIM from the
::  real node's lib/bitcoin/compact-block-filters.hoon (gwbtc/node, branch
::  develop) so that %gw-btc tests a scriptPubKey against a %bitcoin-client
::  filter with bit-identical siphash/GCS math to the node that produced and
::  verified that filter.  The ONLY change from the node's file is that the
::  three `de` arms parse-filter needs (de-abed/de-compactsize/de-abet) are
::  inlined below instead of imported from the heavier bitcoin-serialization
::  lib.  M=784931, P=19, key = first 16 bytes of the block hash.  Do NOT
::  alter the siphash / to-range / match logic.
::
/-  *bitcoin-common
|%
+$  filter         hexb
+$  filter-header  @ux
::
+$  bits  [wid=@ud dat=@ub]
::
++  genesis-filter         [4 0x80a8.7f01]
++  genesis-filter-header  0x2c2.3921.80d0.ce2b.5b6f.8b08.d39a.11ff.e831.c673.311a.3ecf.77b9.7fc3.f030.3c9f
::
::  minimal byte-stream reader: exactly the arms parse-filter needs, copied
::  from bitcoin-serialization's +de core (bytes consumed LSB-first).
::
++  de
  |_  byt=hexb
  ++  de-core  .
  ++  de-abet  byt
  ++  de-abed  |=(leb=hexb de-core(byt leb))
  ++  de-peek  |=(wid=@ud (end [3 wid] dat.byt))
  ++  de-read
    |=  wid=@ud
    :-  (de-peek wid)
    %_  de-core
        wid.byt  ?:((lth wid wid.byt) (sub wid.byt wid) 0)
        dat.byt  (rsh [3 wid] dat.byt)
    ==
  ::
  ++  de-compactsize
    ^-  [@ _de-core]
    =^  prefix  de-core  (de-read 1)
    ?:  (lte prefix 0xfc)  [prefix de-core]
    ?:  =(prefix 0xfd)  (de-read 2)
    ?:  =(prefix 0xfe)  (de-read 4)
    ?:  =(prefix 0xff)  (de-read 8)
    ~|  %invalid-compactsize
    !!
  --
::
++  make-filter-header
  |=  [prev=filter-header filter-hash=@ux]
  ^-  filter-header
  %+  shay  32
  %+  shay  64
  %+  can  3
  :~  [32 filter-hash]
      [32 prev]
  ==
::
++  make-filter-hash
  |=  fil=filter
  ^-  @ux
  %+  shay  32
  %-  shay
      fil
::
++  params
  |%
  ++  p  19
  ++  m  784.931
  --
::
++  siphash
  |=  [k=byts m=byts]
  ^-  byts
  |^
  ?>  =(wid.k 16)
  ?>  (lte (met 3 dat.k) wid.k)
  ?>  (lte (met 3 dat.m) wid.m)
  (fin (comp m (init dat.k)))
  :: Initialise internal state
  ::
  ++  init
    |=  k=@
    ^-  [@ @ @ @]
    =/  k0=@  (end [6 1] k)
    =/  k1=@  (cut 6 [1 1] k)
    :^    (mix k0 0x736f.6d65.7073.6575)
        (mix k1 0x646f.7261.6e64.6f6d)
      (mix k0 0x6c79.6765.6e65.7261)
    (mix k1 0x7465.6462.7974.6573)
  ::
  :: Compression rounds
  ++  comp
    |=  [m=byts v=[v0=@ v1=@ v2=@ v3=@]]
    ^-  [@ @ @ @]
    =/  len=@ud  (div wid.m 8)
    =/  last=@  (lsh [3 7] (mod wid.m 256))
    =|  i=@ud
    =|  w=@
    |-
    =.  w  (cut 6 [i 1] dat.m)
    ?:  =(i len)
      =.  v3.v  (mix v3.v (mix last w))
      =.  v  (rnd (rnd v))
      =.  v0.v  (mix v0.v (mix last w))
      v
    %=  $
      v  =.  v3.v  (mix v3.v w)
         =.  v  (rnd (rnd v))
         =.  v0.v  (mix v0.v w)
         v
      i  (add i 1)
    ==
  ::
  :: Finalisation rounds
  ++  fin
    |=  v=[v0=@ v1=@ v2=@ v3=@]
    ^-  byts
    =.  v2.v  (mix v2.v 0xff)
    =.  v  (rnd (rnd (rnd (rnd v))))
    :-  8
    :(mix v0.v v1.v v2.v v3.v)
  ::
  :: Sipround
  ++  rnd
    |=  [v0=@ v1=@ v2=@ v3=@]
    ^-  [@ @ @ @]
    =.  v0  (~(sum fe 6) v0 v1)
    =.  v2  (~(sum fe 6) v2 v3)
    =.  v1  (~(rol fe 6) 0 13 v1)
    =.  v3  (~(rol fe 6) 0 16 v3)
    =.  v1  (mix v1 v0)
    =.  v3  (mix v3 v2)
    =.  v0  (~(rol fe 6) 0 32 v0)
    =.  v2  (~(sum fe 6) v2 v1)
    =.  v0  (~(sum fe 6) v0 v3)
    =.  v1  (~(rol fe 6) 0 17 v1)
    =.  v3  (~(rol fe 6) 0 21 v3)
    =.  v1  (mix v1 v2)
    =.  v3  (mix v3 v0)
    =.  v2  (~(rol fe 6) 0 32 v2)
    [v0 v1 v2 v3]
  --
::  +str: bit streams
::   read is from the front
::   write appends to the back
::
++  str
  |%
  ++  read-bit
    |=  s=bits
    ^-  [bit=@ub rest=bits]
    ?>  (gth wid.s 0)
    :*  ?:((gth wid.s (met 0 dat.s)) 0b0 0b1)
        [(dec wid.s) (end [0 (dec wid.s)] dat.s)]
    ==
  ::
  ++  read-bits
    |=  [n=@ s=bits]
    ^-  [bits rest=bits]
    =|  bs=bits
    |-
    ?:  =(n 0)  [bs s]
    =^  b  s  (read-bit s)
    $(n (dec n), bs (write-bits bs [1 b]))
  ::
  ++  write-bits
    |=  [s1=bits s2=bits]
    ^-  bits
    [(add wid.s1 wid.s2) (can 0 ~[s2 s1])]
  --
::  +gol: Golomb-Rice encoding/decoding
::
++  gol
  |%
  ::  +en: encode x and append to end of s
  ::   - s: bits stream
  ::   - x: number to add to the stream
  ::   - p: golomb-rice p param
  ::
  ++  en
    |=  [s=bits x=@ p=@]
    ^-  bits
    =+  q=(rsh [0 p] x)
    =+  unary=[+(q) (lsh [0 1] (dec (bex q)))]
    =+  r=[p (end [0 p] x)]
    %+  write-bits:str  s
    (write-bits:str unary r)
  ::
  ++  de
    |=  [s=bits p=@]
    ^-  [delta=@ rest=bits]
    |^  ?>  (gth wid.s 0)
    =^  q  s  (get-q s)
    =^  r  s  (read-bits:str p s)
    [(add dat.r (lsh [0 p] q)) s]
    ::
    ++  get-q
      |=  s=bits
      =|  q=@
      =^  first-bit  s  (read-bit:str s)
      |-
      ?:  =(0 first-bit)  [q s]
      =^  b  s  (read-bit:str s)
      $(first-bit b, q +(q))
    --
  --
::  +hsh
::
++  hsh
  |%
  ::  +to-range
  ::   - item: scriptpubkey to hash
  ::   - f: N*M
  ::   - k: key for siphash (end of blockhash, reversed)
  ::
  ++  to-range
    |=  [item=hexb f=@ k=hexb]
    ^-  @
    (rsh [0 64] (mul f dat:(siphash k item)))
  ::  +set-construct: return sorted hashes of scriptpubkeys
  ::
  ++  set-construct
  |=  [items=(list hexb) k=hexb f=@]
    ^-  (list @)
    %+  sort
      %+  turn  items
      |=  item=hexb
      (to-range item f k)
    lth
  --
::  +parse-filter: takes a filter as little endian bytes, and produces a gcs (big endian)
::
++  parse-filter
  |=  =filter
  ^-  [n=@ux gcs-set=bits]
  =/  de-core  (de-abed:de filter)
  =^  n  de-core  de-compactsize:de-core
  =/  d  de-abet:de-core
  :-  n
  :-  (mul 8 wid.d)
  %+  rev  3  d
::  +to-key: blockhash (little endian) to key for siphash
::
++  to-key
  |=  haz=block-hash
  ^-  hexb
  :-  16
  %+  end  [3 16]
      haz
::  +match: whether block filter matches *any* target scriptpubkeys
::   - block-hash: of the block corresponding to the filter
::   - filter: full block filter, with leading N
::   - targets: scriptpubkeys to match
::
++  match
  |=  [=block-hash =filter targets=(list script-pubkey)]
  ^-  ?
  =/  k  (to-key block-hash)
  =/  [p=@ m=@]  [p:params m:params]
  =/  [n=@ux gcs-set=bits]  (parse-filter filter)
  =+  target-hs=(set-construct:hsh targets k (mul n m))
  =+  last-val=0
  |-
  ?~  target-hs  %.n
  ?:  =(last-val i.target-hs)
    %.y
  ?:  (gth last-val i.target-hs)
    $(target-hs t.target-hs)
  ::  last-val is less than target: check next val in GCS, if any
  ::
  ?:  (lth wid.gcs-set p)  %.n
  =^  delta  gcs-set
    (de:gol gcs-set p)
  $(last-val (add delta last-val))
::  +all-match: returns all target scriptpubkeys that match
::   - block-hash: of the block corresponding to the filter
::   - filter: full block filter, with leading N
::   - targets: scriptpubkeys to match
::
++  all-match
  |=  [=block-hash =filter targets=(list script-pubkey)]
  ^-  (set script-pubkey)
  %-  ~(gas in *(set hexb))
  =/  k  (to-key block-hash)
  =/  [p=@ m=@]  [p:params m:params]
  =/  [n=@ux gcs-set=bits]  (parse-filter filter)
  =/  target-map=(map @ hexb)
    %-  ~(gas by *(map @ hexb))
    %+  turn  targets
    |=  t=hexb
    [(to-range:hsh t (mul n m) k) t]
  =+  target-hs=(sort ~(tap in ~(key by target-map)) lth)
  =+  last-val=0
  =|  matches=(list @)
  |-
  ?~  target-hs
    (murn matches ~(get by target-map))
  ?:  =(last-val i.target-hs)
    %=  $
        target-hs  t.target-hs
        matches    [last-val matches]
    ==
  ?:  (gth last-val i.target-hs)
    $(target-hs t.target-hs)
  ::  last-val is less than target: get next val in GCS, if any
  ::
  ?:  (lth wid.gcs-set p)
    (murn matches ~(get by target-map))
  =^  delta  gcs-set
    (de:gol gcs-set p)
  $(last-val (add delta last-val))
::
--
