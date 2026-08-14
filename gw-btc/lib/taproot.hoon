::  taproot.hoon - Taproot script tree utilities
::
::  Ported into the groundwire desk from spv-wallet/lib/taproot.hoon, trimmed to
::  the arms the self-attestation verifier (lib/self-attestation, used by
::  %gw-btc) needs to VERIFY a confidential comet's commitments:
::  TapLeaf/TapBranch hashing, BIP-341 key tweaking, and the output-key
::  reconstruction used to check that an off-chain-revealed tapleaf was committed
::  in an on-chain taproot output. The bech32/address helpers were dropped.
::
::  Provides operations on partial tapscript trees (ptst):
::  - Hash computation (TapLeaf, TapBranch)
::  - Leaf enumeration with axis addressing
::  - Merkle proof extraction / reconstruction
::  - Output-key tweaking (Q = P + t*G)
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
++  make-leaf    |=(=tapleaf ^-(ptst [%leaf tapleaf]))
++  make-branch  |=([l=ptst r=ptst] ^-(ptst [%branch l r]))
++  make-opaque  |=(h=@ux ^-(ptst [%opaque h]))
::
::  ============================================================================
::  Key Tweaking (BIP-341)
::  ============================================================================
::
::  +x-only: extract x-coordinate from compressed pubkey
::
::  Takes 33-byte compressed pubkey, returns 32-byte x-only pubkey.
::
++  x-only
  |=  pubkey=@ux
  ^-  @ux
  (end [3 32] pubkey)
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
::  Returns [tweaked-x-only-pubkey parity].  Q = P + t*G.
::
++  tweak-pubkey
  |=  [internal-pubkey=@ux merkle-root=(unit @ux)]
  ^-  [x=@ux parity=?]
  =,  secp256k1:secp:crypto
  ::  The wire format carries a compressed key, while BIP-341 commits only
  ::  its x-coordinate and lifts it to the unique even-y curve point.
  =/  prefix  (rsh [3 32] internal-pubkey)
  ?>  |(=(2 prefix) =(3 prefix))
  =/  internal-x  (x-only internal-pubkey)
  =/  lifted  (lift-x:schnorr internal-x)
  ?~  lifted  !!
  =/  p-even=point  u.lifted
  ::  Compute tweak
  =/  tweak=@ux  (compute-tweak internal-x merkle-root)
  ?>  (lth tweak secp-n)
  ::  Q = P + t*G
  =/  t-times-g=point  (mul-point-scalar g:domain:curve tweak)
  =/  q=point  (add-points p-even t-times-g)
  ?<  =([0 0] q)
  :-  x.q
  !=(0 (mod y.q 2))
::
::  +output-pubkey: compute taproot output key (for scriptPubKey)
::
::  Convenience: returns just the x-only output pubkey.
::
++  output-pubkey
  |=  [internal-pubkey=@ux merkle-root=(unit @ux)]
  ^-  @ux
  x:(tweak-pubkey internal-pubkey merkle-root)
::
::  +merkle-root-from-proof: compute merkle root from leaf and proof
::
::  Given a tapleaf and its merkle proof (list of sibling hashes),
::  reconstructs the merkle root by walking up the tree.  An empty proof
::  yields the leaf hash itself (a single-leaf tree).
::
++  merkle-root-from-proof
  |=  [=tapleaf proof=(list @ux)]
  ^-  @ux
  =/  current=@ux  (leaf-hash tapleaf)
  |-
  ?~  proof
    current
  =/  sibling=@ux  i.proof
  =/  [first=@ux second=@ux]
    ?:  (lth current sibling)
      [current sibling]
    [sibling current]
  =/  branch-data=hexb:btc
    (cat:byt:bcu ~[[32 first] [32 second]])
  $(current (tagged-hash 'TapBranch' branch-data), proof t.proof)
--
