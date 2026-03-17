::  taproot.hoon - Taproot script tree utilities
::
::  Provides operations on partial tapscript trees (ptst):
::  - Hash computation (TapLeaf, TapBranch)
::  - Leaf enumeration with axis addressing
::  - Merkle proof extraction
::
/+  bcu=bitcoin-utils, btc=bitcoin
::
::  secp256k1 curve order (n)
::
|%
++  secp-n  0xffff.ffff.ffff.ffff.ffff.ffff.ffff.fffe.baae.dce6.af48.a03b.bfd2.5e8c.d036.4141
::  Taproot script tree types
::
+$  tapleaf  [version=@ux script=hexb:btc]
+$  ptst  ::  partial tapscript tree
  $@  ~
  $%  [%leaf =tapleaf]
      [%opaque hash=@ux]
      [%branch l=ptst r=ptst]
  ==
::
::  +leaf-hash: compute TapLeaf hash for a tapleaf
::
++  leaf-hash
  |=  =tapleaf
  ^-  @ux
  =/  script-len=@  wid.script.tapleaf
  =/  compact-size=hexb:btc
    ?:  (lth script-len 0xfd)  [1 `@ux`script-len]
    ?:  (lth script-len 0x1.0000)
      (cat:byt:bcu ~[[1 0xfd] (flip:byt:bcu [2 script-len])])
    ?:  (lth script-len 0x1.0000.0000)
      (cat:byt:bcu ~[[1 0xfe] (flip:byt:bcu [4 script-len])])
    (cat:byt:bcu ~[[1 0xff] (flip:byt:bcu [8 script-len])])
  =/  leaf-data=hexb:btc
    %-  cat:byt:bcu
    :~  [1 version.tapleaf]
        compact-size
        script.tapleaf
    ==
  (tagged-hash 'TapLeaf' leaf-data)
::
::  +hash: get the hash of a ptst node
::
++  hash
  |=  tree=ptst
  ^-  @ux
  ?~  tree  0x0
  ?-  -.tree
    %leaf    (leaf-hash tapleaf.tree)
    %opaque  hash.tree
      %branch
    =/  left-hash=@ux   (hash l.tree)
    =/  right-hash=@ux  (hash r.tree)
    ::  TapBranch: sort hashes lexicographically
    =/  [first=@ux second=@ux]
      ?:  (lth left-hash right-hash)
        [left-hash right-hash]
      [right-hash left-hash]
    =/  branch-data=hexb:btc
      (cat:byt:bcu ~[[32 first] [32 second]])
    (tagged-hash 'TapBranch' branch-data)
  ==
::  +leaves: get all leaves with their axis addresses
::
::  Uses Nock axis addressing: 1=root, 2=left, 3=right, etc.
::
++  leaves
  |=  tree=ptst
  ^-  (list [axis=@ =tapleaf])
  (leaves-at tree 1)
::
++  leaves-at
  |=  [tree=ptst axis=@]
  ^-  (list [axis=@ =tapleaf])
  ?~  tree  ~
  ?-  -.tree
    %leaf    [[axis tapleaf.tree] ~]
    %opaque  ~
      %branch
    %+  weld
      (leaves-at l.tree (mul 2 axis))
    (leaves-at r.tree +((mul 2 axis)))
  ==
::  +proof: get merkle proof for leaf at given axis
::
::  Returns ~ if no leaf at that axis.
::  Returns list of sibling hashes from leaf to root.
::
++  proof
  |=  [tree=ptst axis=@]
  ^-  (unit (list @ux))
  ?~  tree  ~
  ?:  =(1 axis)
    ::  At root - must be a leaf
    ?.  ?=(%leaf -.tree)  ~
    `~
  ::  Navigate to target, collecting sibling hashes
  =/  path=(list ?)  (axis-to-path axis)
  (proof-walk tree path ~)
::
++  proof-walk
  |=  [tree=ptst path=(list ?) siblings=(list @ux)]
  ^-  (unit (list @ux))
  ?~  tree  ~
  ?~  path
    ::  Reached target - must be a leaf
    ?.  ?=(%leaf -.tree)  ~
    `siblings
  ?.  ?=(%branch -.tree)  ~
  =/  go-left=?  i.path
  =/  sibling-hash=@ux
    ?:  go-left
      (hash r.tree)
    (hash l.tree)
  =/  next-tree=ptst  ?:(go-left l.tree r.tree)
  $(tree next-tree, path t.path, siblings (snoc siblings sibling-hash))
::  +axis-to-path: convert axis to list of left/right decisions
::
::  Returns path from root to target (%.y = left, %.n = right)
::
++  axis-to-path
  |=  axis=@
  ^-  (list ?)
  ?:  =(1 axis)  ~
  =/  parent=@  (div axis 2)
  =/  is-left=?  =(0 (mod axis 2))
  (snoc (axis-to-path parent) is-left)
::  +tagged-hash: BIP-340 tagged hash
::
::  tagged_hash(tag, data) = SHA256(SHA256(tag) || SHA256(tag) || data)
::  Defers to the standard library's implementation (same as groundwire)
::
++  tagged-hash
  |=  [tag=@t data=hexb:btc]
  ^-  @ux
  `@ux`(tagged-hash:schnorr:secp256k1:secp:crypto tag [p=wid.data q=dat.data])
::  +has-leaf: check if tree contains at least one leaf
::
++  has-leaf
  |=  tree=ptst
  ^-  ?
  ?~  tree  %.n
  ?-  -.tree
    %leaf    %.y
    %opaque  %.n
    %branch  ?|((has-leaf l.tree) (has-leaf r.tree))
  ==
::
::  Constructors
::
::  +make-leaf: create a leaf node
::
++  make-leaf
  |=  =tapleaf
  ^-  ptst
  [%leaf tapleaf]
::
::  +make-branch: create a branch node
::
++  make-branch
  |=  [l=ptst r=ptst]
  ^-  ptst
  [%branch l r]
::
::  +make-opaque: create an opaque node from a known hash
::
++  make-opaque
  |=  h=@ux
  ^-  ptst
  [%opaque h]
::
::  ============================================================================
::  Key Tweaking (BIP-341)
::  ============================================================================
::
::  +x-only: extract x-coordinate from compressed pubkey
::
::  Takes 33-byte compressed pubkey, returns 32-byte x-only pubkey.
::  The y-coordinate parity is implicit in taproot (always lift to even y).
::
++  x-only
  |=  pubkey=@ux
  ^-  @ux
  ::  Compressed pubkey is 02/03 prefix + 32-byte x-coordinate
  ::  Extract just the x-coordinate (low 32 bytes)
  (end [3 32] pubkey)
::
::  +has-even-y: check if compressed pubkey has even y-coordinate
::
++  has-even-y
  |=  pubkey=@ux
  ^-  ?
  ::  02 prefix = even y, 03 prefix = odd y
  =/  prefix=@  (rsh [3 32] pubkey)
  =(0x2 prefix)
::
::  +compute-tweak: compute taproot tweak value
::
::  tweak = tagged_hash("TapTweak", internal_pubkey_x || merkle_root)
::  If merkle_root is ~, tweak = tagged_hash("TapTweak", internal_pubkey_x)
::
++  compute-tweak
  |=  [internal-pubkey-x=@ux merkle-root=(unit @ux)]
  ^-  @ux
  =/  data=hexb:btc
    ?~  merkle-root
      [32 internal-pubkey-x]
    (cat:byt:bcu ~[[32 internal-pubkey-x] [32 u.merkle-root]])
  (tagged-hash 'TapTweak' data)
::
::  +tweak-pubkey: compute tweaked output pubkey
::
::  Takes internal pubkey (33-byte compressed) and optional merkle root.
::  Returns [tweaked-x-only-pubkey parity] where parity is the y-coordinate parity.
::
::  Q = P + t*G where t is the tweak
::
++  tweak-pubkey
  |=  [internal-pubkey=@ux merkle-root=(unit @ux)]
  ^-  [x=@ux parity=?]
  =,  secp256k1:secp:crypto
  ::  Get internal pubkey as a point
  =/  p=point  (decompress-point internal-pubkey)
  ::  If P has odd y, we need to negate it (use -P as internal key)
  ::  This ensures the internal key has even y for consistent tweaking
  =/  p-even=point
    ?:  =(0 (mod y.p 2))
      p
    [x.p (sub p:domain:curve y.p)]
  ::  Compute tweak
  =/  tweak=@ux  (compute-tweak (x-only internal-pubkey) merkle-root)
  ::  Q = P + t*G
  =/  t-times-g=point  (mul-point-scalar g:domain:curve tweak)
  =/  q=point  (add-points p-even t-times-g)
  ::  Return x-only pubkey and parity
  :-  x.q
  !=(0 (mod y.q 2))
::
::  +tweak-privkey: compute tweaked private key for signing
::
::  Takes private key, internal pubkey, and optional merkle root.
::  Returns tweaked private key suitable for Schnorr signing.
::
::  If internal pubkey has even y: d' = (d + t) mod n
::  If internal pubkey has odd y:  d' = (n - d + t) mod n
::
::  Note: BIP-340 signing handles Q's y-parity internally.
::
++  tweak-privkey
  |=  [privkey=@ux internal-pubkey=@ux merkle-root=(unit @ux)]
  ^-  @ux
  ::  Check if internal pubkey has even y
  =/  even-y=?  (has-even-y internal-pubkey)
  ::  Compute tweak
  =/  tweak=@ux  (compute-tweak (x-only internal-pubkey) merkle-root)
  ::  Negate privkey if pubkey has odd y, then add tweak
  =/  d=@ux
    ?:  even-y
      privkey
    (sub secp-n privkey)
  (mod (add d tweak) secp-n)
::
::  +output-pubkey: compute taproot output key (for scriptPubKey)
::
::  Convenience function that returns just the x-only output pubkey.
::
++  output-pubkey
  |=  [internal-pubkey=@ux merkle-root=(unit @ux)]
  ^-  @ux
  x:(tweak-pubkey internal-pubkey merkle-root)
::
::  +merkle-root-from-proof: compute merkle root from leaf and proof
::
::  Given a tapleaf and its merkle proof (list of sibling hashes),
::  reconstructs the merkle root by walking up the tree.
::
++  merkle-root-from-proof
  |=  [=tapleaf proof=(list @ux)]
  ^-  @ux
  =/  current=@ux  (leaf-hash tapleaf)
  |-
  ?~  proof
    current
  ::  TapBranch: sort hashes lexicographically before hashing
  =/  sibling=@ux  i.proof
  =/  [first=@ux second=@ux]
    ?:  (lth current sibling)
      [current sibling]
    [sibling current]
  =/  branch-data=hexb:btc
    (cat:byt:bcu ~[[32 first] [32 second]])
  $(current (tagged-hash 'TapBranch' branch-data), proof t.proof)
::
::  ============================================================================
::  Taproot Address Handling (BIP-350 bech32m)
::  ============================================================================
::
::  Bech32m uses a different checksum constant than bech32
::
++  bech32m-constant  0x2bc8.30a3
::
::  +verify-checksum-bech32m: verify bech32m checksum
::
++  verify-checksum-bech32m
  |=  [hrp=tape data-and-checksum=(list @)]
  ^-  ?
  %-  |=(a=@ =(bech32m-constant a))
  %-  polymod:bech32:btc
  (weld (expand-hrp:bech32:btc hrp) data-and-checksum)
::
::  +is-taproot-address: check if address is a taproot address
::
++  is-taproot-address
  |=  addr=@t
  ^-  ?
  =/  addr-tape=tape  (cass (trip addr))
  ?|  =("bc1p" (scag 4 addr-tape))
      =("tb1p" (scag 4 addr-tape))
      =("bcrt1p" (scag 6 addr-tape))
  ==
::
::  +from-address: decode taproot address to 32-byte x-only pubkey
::
::  Decodes bc1p/tb1p/bcrt1p addresses (witness version 1, bech32m checksum).
::
++  from-address
  |=  body=cord
  ^-  hexb:btc
  ~|  "Invalid taproot address"
  =/  bech=tape  (cass (trip body))
  =/  pos=(list @)  (flop (fand "1" bech))
  ?>  ?=(^ pos)
  =/  last-1=@  i.pos
  ?>  (is-valid:bech32:btc bech last-1)
  =/  hrp=tape  (scag last-1 bech)
  =/  encoded-data-and-checksum=(list @)
    (slag +(last-1) bech)
  =/  data-and-checksum=(list @)
    (murn encoded-data-and-checksum charset-to-value:bech32:btc)
  ?>  =((lent encoded-data-and-checksum) (lent data-and-checksum))
  ?>  (verify-checksum-bech32m hrp data-and-checksum)
  =/  checksum-pos=@  (sub (lent data-and-checksum) 6)
  =/  data=(list @)  (scag checksum-pos data-and-checksum)
  =/  bs=bits:bcu  (from-atoms:bit:bcu 5 data)
  =/  byt-len=@  (div (sub wid.bs 5) 8)
  ::  Assert witness version 1
  ?>  =(5^0b1 (take:bit:bcu 5 bs))
  ::  Assert 32 bytes (x-only pubkey)
  ?>  =(32 byt-len)
  [32 `@ux`dat:(take:bit:bcu 256 (drop:bit:bcu 5 bs))]
::
::  +to-script-pubkey: create taproot scriptPubKey from address
::
::  Returns OP_1 <32-byte x-only pubkey> (34 bytes total)
::
++  to-script-pubkey
  |=  addr=@t
  ^-  hexb:btc
  =/  pubkey=hexb:btc  (from-address addr)
  %-  cat:byt:bcu
  :~  1^0x51      ::  OP_1 (witness version 1)
      1^wid.pubkey  ::  push 32
      pubkey
  ==
::
::  +tapscript-address: generate taproot address with script tree
::
::  Takes internal pubkey (33-byte compressed) and script tree.
::  Returns bech32m address for the tweaked output key.
::
++  tapscript-address
  |=  [internal-pubkey=@ux tree=ptst net=?(%main %testnet %regtest)]
  ^-  @t
  ::  Compute merkle root from tree (~ if empty tree)
  =/  merkle-root=(unit @ux)
    ?~  tree  ~
    `(hash tree)
  ::  Compute tweaked output pubkey
  =/  tweaked-x=@ux  (output-pubkey internal-pubkey merkle-root)
  ::  Encode as bech32m taproot address
  =/  encoded=(unit @t)  (encode-taproot:bech32:btc net [32 tweaked-x])
  (need encoded)
::
::  ============================================================================
::  Simple Tapscript Constructors (for testing)
::  ============================================================================
::
::  +checksig-script: <pubkey> OP_CHECKSIG
::
::  Single-key spend script for script-path.
::
++  checksig-script
  |=  pubkey=@ux
  ^-  hexb:btc
  ::  x-only pubkey (32 bytes) + OP_CHECKSIG (0xac)
  =/  x-only=@ux  (x-only pubkey)
  %-  cat:byt:bcu
  :~  [1 32]        ::  push 32 bytes
      [32 x-only]   ::  x-only pubkey
      [1 0xac]      ::  OP_CHECKSIG
  ==
::
::  +csv-checksig-script: <delay> OP_CSV OP_DROP <pubkey> OP_CHECKSIG
::
::  Timelocked single-key spend (relative timelock).
::
++  csv-checksig-script
  |=  [delay=@ud pubkey=@ux]
  ^-  hexb:btc
  =/  x-only=@ux  (x-only pubkey)
  ::  Encode delay as minimal push
  =/  delay-push=hexb:btc
    ?:  (lth delay 0x80)
      ?:  =(0 delay)  [1 0x0]
      [1 `@ux`delay]
    ?:  (lth delay 0x100)
      [2 `@ux`(con (lsh [3 1] 0x1) delay)]  ::  OP_PUSHBYTES_1 + byte
    [3 `@ux`(con (lsh [3 2] 0x2) delay)]    ::  OP_PUSHBYTES_2 + le16
  %-  cat:byt:bcu
  :~  delay-push
      [1 0xb2]      ::  OP_CHECKSEQUENCEVERIFY
      [1 0x75]      ::  OP_DROP
      [1 32]        ::  push 32 bytes
      [32 x-only]   ::  x-only pubkey
      [1 0xac]      ::  OP_CHECKSIG
  ==
--
