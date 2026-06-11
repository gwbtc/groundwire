::  taproot.hoon - Taproot verification utilities
::
::  Ported into the groundwire desk from spv-wallet/lib/taproot.hoon, trimmed
::  to VERIFICATION arms only: what the self-attestation verifier
::  (lib/self-attestation, used by %urb-watcher) needs to check a confidential
::  comet's commitments — TapLeaf hashing and BIP-341 key tweaking, i.e.
::  reconstructing the output key Q from a disclosed internal key and single
::  leaf to compare against an on-chain taproot output.
::
::  Builder-side taproot (script trees, addresses, signing) lives in p2tr:gw
::  (lib/groundwire.hoon). That code RE-ENCODES scripts from parsed form, so
::  it must NOT be used for verification: a leaf hash must cover the exact
::  revealed script bytes, not a re-encoding of them.
::
/+  bcu=bitcoin-utils, btc=bitcoin
::
::  secp256k1 curve order (n)
::
|%
++  secp-n  0xffff.ffff.ffff.ffff.ffff.ffff.ffff.fffe.baae.dce6.af48.a03b.bfd2.5e8c.d036.4141
::  +tapleaf: a Taproot leaf — leaf version + the tapscript bytes
::
+$  tapleaf  [version=@ux script=hexb:btc]
::
::  +tagged-hash: BIP-340 tagged hash
::
::  tagged_hash(tag, data) = SHA256(SHA256(tag) || SHA256(tag) || data)
::  Defers to the standard library's implementation (same as groundwire)
::
++  tagged-hash
  |=  [tag=@t data=hexb:btc]
  ^-  @ux
  `@ux`(tagged-hash:schnorr:secp256k1:secp:crypto tag [p=wid.data q=dat.data])
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
  ::  Get internal pubkey as a point
  =/  p=point  (decompress-point internal-pubkey)
  ::  If P has odd y, negate it (lift to even y) for consistent tweaking
  =/  p-even=point
    ?:  =(0 (mod y.p 2))
      p
    [x.p (sub p:domain:curve y.p)]
  ::  Compute tweak
  =/  tweak=@ux  (compute-tweak (x-only internal-pubkey) merkle-root)
  ::  Q = P + t*G
  =/  t-times-g=point  (mul-point-scalar g:domain:curve tweak)
  =/  q=point  (add-points p-even t-times-g)
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
--
