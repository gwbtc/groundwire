::  bip32-spv: fork of vendor/base-dev lib/bip32.hoon
::
::  adds ++address-p2sh (BIP49) and ++address-p2wpkh (BIP84)
::  for deriving addresses directly from bip32 keys.
::  also uses `this` instead of `+>` for door subject reference.
::
::  bip32 implementation in hoon
::
::  to use, call one of the core initialization arms.
::  using the produced core, derive as needed and take out the data you want.
::
::NOTE  tested to be correct against
::      https://en.bitcoin.it/wiki/BIP_0032_TestVectors
::
/+  btc=bitcoin
=,  hmac:crypto
=,  secp:crypto
=+  ecc=secp256k1
::  prv:  private key
::  pub:  public key
::  cad:  chain code
::  dep:  depth in chain
::  ind:  index at depth
::  pif:  parent fingerprint (4 bytes)
::
|_  [prv=@ pub=point.ecc cad=@ dep=@ud ind=@ud pif=@]
+*  this  .
::  elliptic curve operations and values
::
++  point  priv-to-pub.ecc
++  ser-p  compress-point.ecc
++  n      n:t.ecc
::  core initialization
::
++  from-seed
  |=  byts
  ^+  this
  =+  der=(hmac-sha512l [12 'dees nioctiB'] [wid dat])
  =+  pri=(cut 3 [32 32] der)
  this(prv pri, pub (point pri), cad (cut 3 [0 32] der))
::
++  from-private
  |=  [key=@ cai=@]
  ^+  this
  this(prv key, pub (point key), cad cai)
::
++  from-public
  |=  [key=@ cai=@]
  ^+  this
  this(pub (decompress-point.ecc key), cad cai)
::
++  from-public-point
  |=  [pon=point.ecc cai=@]
  ^+  this
  this(pub pon, cad cai)
::
++  from-extended
  |=  t=tape
  ^+  this
  =+  x=(de-base58check 4 t)
  =>  |%
      ++  take
        |=  b=@ud
        ^-  [v=@ x=@]
        :-  (end [3 b] x)
        (rsh [3 b] x)
      --
  =^  k  x  (take 33)
  =^  c  x  (take 32)
  =^  i  x  (take 4)
  =^  p  x  (take 4)
  =^  d  x  (take 1)
  ?>  =(0 x)  ::  sanity check
  %.  [d i p]
  =<  set-metadata
  =+  v=(swag [1 3] t)
  ?:  =("prv" v)  (from-private k c)
  ?:  =("pub" v)  (from-public k c)
  !!
::
++  set-metadata
  |=  [d=@ud i=@ud p=@]
  ^+  this
  this(dep d, ind i, pif p)
::  derivation
::
++  derivation-path
  ;~  pfix
    ;~(pose (jest 'm/') (easy ~))
    %+  most  fas
    ;~  pose
      %+  cook
        |=(i=@ (add i (bex 31)))
      ;~(sfix dem soq)
      dem
    ==
  ==
::
++  derive-path
  |=  t=tape
  ^+  this
  %-  derive-sequence
  (scan t derivation-path)
::
++  derive-sequence
  |=  j=(list @u)
  ^+  this
  ?~  j  this
  =.  this  (derive i.j)
  $(j t.j)
::
++  derive
  ^-  $-(@u _this)
  ?:  =(0 prv)
    derive-public
  derive-private
::
++  derive-private
  |=  i=@u
  ^+  this
  ::  we must have a private key to derive the next one
  ::
  ?:  =(0 prv)
    ~|  %know-no-private-key
    !!
  ::  derive child at i
  ::
  =/  [left=@ right=@]
    =-  [(cut 3 [32 32] -) (cut 3 [0 32] -)]
    %+  hmac-sha512l  [32 cad]
    :-  37
    ?:  (gte i (bex 31))
      ::  hardened child
      ::
      (can 3 ~[4^i 32^prv 1^0])
    ::  normal child
    ::
    (can 3 ~[4^i 33^(ser-p (point prv))])
  =+  key=(mod (add left prv) n)
  ::  rare exception, invalid key, go to the next one
  ::
  ?:  |(=(0 key) (gte left n))  $(i +(i))
  %=  this
    prv   key
    pub   (point key)
    cad   right
    dep   +(dep)
    ind   i
    pif   fingerprint
  ==
::
++  derive-public
  |=  i=@u
  ^+  this
  ::  public keys can't be hardened
  ::
  ?:  (gte i (bex 31))
    ~|  %cant-derive-hardened-public-key
    !!
  ::  derive child at i
  ::
  =/  [left=@ right=@]
    =-  [(cut 3 [32 32] -) (cut 3 [0 32] -)]
    %+  hmac-sha512l  [32 cad]
    37^(can 3 ~[4^i 33^(ser-p pub)])
  ::  rare exception, invalid key, go to the next one
  ::
  ?:  (gte left n)  $(i +(i))  ::TODO  or child key is "point at infinity"
  %=  this
    pub   (add-points.ecc (point left) pub)
    cad   right
    dep   +(dep)
    ind   i
    pif   fingerprint
  ==
::  rendering
::
++  private-key     `@`?.(=(0 prv) prv ~|(%know-no-private-key !!))
++  public-key      `@`(ser-p pub)
++  chain-code      `@`cad
++  private-chain   `[@ @]`[private-key cad]
++  public-chain    `[@ @]`[public-key cad]
::
++  identity        `@`(hash160 public-key)
++  fingerprint     `@`(cut 3 [16 4] identity)
::  removes checksum
::
++  address
  |=  network=?(%main %regtest %testnet)
  ^-  @uc
  %+  rsh  [3 4]
  %+  en-base58check
    [4 (version-bytes network %pub %.n)]
  [20 identity]
::
++  address-p2sh
  |=  network=?(%main %regtest %testnet)
  ^-  @uc
  ::  Generate P2SH-wrapped SegWit address (BIP49)
  =/  pubkey-bytes=hexb:btc  [33 (ser-p pub)]
  =/  addr=address:btc  (from-pubkey:adr:btc %49 network pubkey-bytes)
  ?>  ?=([%base58 *] addr)
  +.addr
::
++  address-p2wpkh
  |=  network=?(%main %regtest %testnet)
  ^-  tape
  ::  Generate native SegWit bech32 address (BIP84)
  =/  pubkey-bytes=hexb:btc  [33 (ser-p pub)]
  =/  addr=address:btc  (from-pubkey:adr:btc %84 network pubkey-bytes)
  ?>  ?=([%bech32 *] addr)
  (trip +.addr)
::
++  prv-extended
  |=  network=?(%main %regtest %testnet)
  ^-  tape
  %+  en-b58c-bip32
    (version-bytes network %prv %.y)
  (build-extended private-key)
::
++  pub-extended
  |=  network=?(%main %regtest %testnet)
  ^-  tape
  %+  en-b58c-bip32
    (version-bytes network %pub %.y)
  (build-extended public-key)
::
++  build-extended
  |=  key=@
  ^-  @
  %+  can  3
  :~  33^key
      32^cad
      4^ind
      4^pif
      1^dep
  ==
::
++  en-b58c-bip32
  |=  [v=@ k=@]
  ^-  tape
  %-  en-base58:mimes:html
  (en-base58check [4 v] [74 k])
::  base58check
::
::  v: version bytes
::  d: data
::
++  en-base58check
  |=  [v=byts d=byts]
  ^-  @
  =/  [=step p=@]  [(add wid.v wid.d) (can 3 ~[d v])]
  =/  chk=@  (rsh [3 28] (sha-256l:sha 32 (sha-256l:sha step^p)))
  (can 3 ~[4^chk step^p])
::  vw: amount of version bytes
::
++  de-base58check
  |=  [vw=@u t=tape]
  ^-  @
  =/  x=@     (de-base58:mimes:html t)
  =/  hash=@  (sha-256l:sha 32 (sha-256:sha (rsh [3 4] x)))
  ?>  =((end [3 4] x) (rsh [3 28] hash))
  (cut 3 [vw (sub (met 3 x) (add 4 vw))] x)
::
++  hash160
  |=  d=@
  ^-  @
  (ripemd-160:ripemd:crypto 32 (sha-256:sha d))
::
++  version-bytes
  |=  [network=?(%main %regtest %testnet) type=?(%pub %prv) bip32=?]
  ^-  @ux
  ?+  [network type bip32]  !!
    [%main %pub %.n]     0x0
    [%main %pub %.y]     0x488.b21e
    [%main %prv %.n]     0x80
    [%main %prv %.y]     0x488.ade4
    [%regtest %pub %.n]  0x6f
    [%regtest %pub %.y]  0x435.87cf
    [%regtest %prv %.n]  0xef
    [%regtest %prv %.y]  0x435.8394
    [%testnet %pub %.n]  0x6f
    [%testnet %pub %.y]  0x435.87cf
    [%testnet %prv %.n]  0xef
    [%testnet %prv %.y]  0x435.8394
  ==
--
