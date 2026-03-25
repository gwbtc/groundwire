::  Bitcoin SPV (Simplified Payment Verification) Library
::
::  ENDIANNESS HANDLING:
::  Bitcoin uses mixed endianness conventions:
::  - Block headers are parsed as big-endian (MSB first)
::  - Hash fields within headers (prev-hash, merkle-root) are little-endian
::  - Block hashes (double-SHA256 output) are displayed in little-endian
::
::  This library:
::  1. Parses header hex with flipped byte list to create big-endian atom
::  2. Extracts fields using adjusted cut positions (due to flipped bytes)
::  3. Flips hash fields after extraction to convert to big-endian representation
::  4. Flips final block hash to little-endian for display/comparison
::
/-  *bitcoin-spv, *bitcoin, *spv-wallet
/+  bcu=bitcoin-utils
|%
:: types
::
+$  merkle-proof
  $:  txid=@t
      siblings=(list @t)
      position=@
      block-height=@
  ==
:: crypto
::
::  +bitcoin-hash: Double-SHA256 for Bitcoin merkle trees
::
::  Concatenates two 32-byte hashes (in INTERNAL byte order) and applies double-SHA256.
::  IMPORTANT: Input hashes must be in internal byte order (actual SHA256 output).
::  APIs return hashes in display format (little-endian) which must be flipped before hashing.
::
++  bitcoin-hash
  |=  [left=@ux right=@ux]
  ^-  @ux
  ::  Concatenate left || right as byte strings
  ::  Since can builds LSB-first, we put right then left to get left||right in the result
  =/  concatenated=hexb  [64 (can 3 ~[[32 right] [32 left]])]
  ::  Apply double-SHA256
  =/  hash-result=hexb  (dsha256:bcu concatenated)
  dat.hash-result
::
++  bitcoin-hash-hex
  |=  [left=@t right=@t]
  ^-  @ux
  =/  left-hex=@ux   (rash left hex)
  =/  right-hex=@ux  (rash right hex)
  (bitcoin-hash left-hex right-hex)
::
::  +hash-to-hex: Convert a 32-byte hash to a 64-character hex string with leading zeros
::
++  hash-to-hex
  |=  hash=@ux
  ^-  @t
  =/  hex-chars=tape  "0123456789abcdef"
  =/  result=tape
    =|  acc=tape
    =/  remaining=@ux  hash
    |-  ^-  tape
    ?:  =(64 (lent acc))  acc
    =/  byte=@ud  (mod remaining 256)
    =/  high=@ud  (div byte 16)
    =/  low=@ud   (mod byte 16)
    =/  high-char=@tD  (snag high hex-chars)
    =/  low-char=@tD   (snag low hex-chars)
    $(acc (weld (trip (cat 3 high-char low-char)) acc), remaining (div remaining 256))
  (crip result)
:: verification
::
++  verify-merkle-proof
  |=  [txid=@t siblings=(list @t) pos=@ expected-root=@t]
  ^-  ?
  ::  Convert from display format (little-endian) to internal byte order for hashing
  =/  current=@ux  dat:(flip:byt:bcu [32 (rash txid hex)])
  =/  expected=@ux  dat:(flip:byt:bcu [32 (rash expected-root hex)])
  =/  position=@  pos
  |-
  ?~  siblings
    =(current expected)
  ::  Flip sibling hash to internal byte order
  =/  sibling=@ux  dat:(flip:byt:bcu [32 (rash i.siblings hex)])
  =/  is-left=?  =((mod position 2) 0)
  =/  combined=@ux
    ?:  is-left
      (bitcoin-hash current sibling)
    (bitcoin-hash sibling current)
  $(current combined, position (div position 2), siblings t.siblings)
::
++  verify-transaction-inclusion
  |=  [proof=merkle-proof block-merkle-root=@t]
  ^-  ?
  (verify-merkle-proof txid.proof siblings.proof position.proof block-merkle-root)
:: header parsing
::
::  +parse-hex-bytes: Parse hex string into bytes preserving order and leading zeros
::
::  Bitcoin wire format is big-endian (MSB first). Parses each hex byte pair
::  individually to preserve leading zeros, then flips the list before using can
::  (which builds LSB-first) to produce correct big-endian atom.
::
::  CRITICAL: Because we flop the list before can, byte 0 ends up at the highest
::  position in the resulting atom. This means cut operations must adjust their
::  positions accordingly: to extract bytes [pos, pos+len), use (cut 3 [(sub total (add pos len)) len])
::
++  parse-hex-bytes
  |=  [hex-string=@t expected-bytes=@ud]
  ^-  @ux
  =/  parse-byte  |=(pos=@ud (rash (cut 3 [pos 2] hex-string) hex))
  =/  all-bytes=(list [@ @])
    %+  turn  (gulf 0 (dec expected-bytes))
    |=  i=@ud
    [1 (parse-byte (mul i 2))]
  (can 3 (flop all-bytes))
::  +parse-bitcoin-header: Parse 160-char hex string into 80-byte Bitcoin header
::
++  parse-bitcoin-header
  |=  raw-header=@t
  ^-  @ux
  =/  hex-length  (lent (trip raw-header))
  ~|  "Bitcoin header must be exactly 160 hex chars, got {<hex-length>}"
  ?>  =(hex-length 160)
  (parse-hex-bytes raw-header 80)
::  +compute-block-hash: Double SHA256 hash of Bitcoin header
::
::  Bitcoin uses double-SHA256 on the 80-byte header. The hash is computed in
::  big-endian format, then flipped to little-endian for display (which is how
::  block hashes are conventionally represented in Bitcoin).
::
++  compute-block-hash
  |=  raw-header=@t
  ^-  @ux
  =/  bitcoin-header=@ux  (parse-bitcoin-header raw-header)
  =/  header-hexb=hexb  [80 bitcoin-header]
  =/  hash-result=hexb  (dsha256:bcu header-hexb)
  =/  flipped-hash=hexb  (flip:byt:bcu hash-result)
  dat.flipped-hash
::  +parse-header-fields: Parse raw header hex into individual fields
::
::  Extracts the 6 fields from an 80-byte Bitcoin block header. The header atom
::  has flipped byte order (see parse-hex-bytes), so cut positions are adjusted.
::  Hash fields (prev-hash, merkle-root) are stored little-endian in Bitcoin wire
::  format, so we flip them after extraction to match our big-endian representation.
::
++  parse-header-fields
  |=  raw-header=@t
  ^-  [version=@ud prev-hash=@ux merkle-root=@ux timestamp=@ud bits=@ux nonce=@ud]
  =/  header=@ux  (parse-bitcoin-header raw-header)
  ::  Header atom has flipped byte order: byte 0 is at bit position 608-639 (76*8 to 79*8)
  ::  To extract wire format bytes [pos, pos+len), use: (cut 3 [(sub 80 (add pos len)) len])
  =/  version=@ud      (cut 3 [(sub 80 4) 4] header)          ::  bytes 0-3   -> pos 76
  =/  prev-hash-be=@ux    (cut 3 [(sub 80 36) 32] header)     ::  bytes 4-35  -> pos 44
  =/  merkle-root-be=@ux  (cut 3 [(sub 80 68) 32] header)     ::  bytes 36-67 -> pos 12
  =/  timestamp=@ud    (cut 3 [(sub 80 72) 4] header)         ::  bytes 68-71 -> pos 8
  =/  bits=@ux         (cut 3 [(sub 80 76) 4] header)         ::  bytes 72-75 -> pos 4
  =/  nonce=@ud        (cut 3 [(sub 80 80) 4] header)         ::  bytes 76-79 -> pos 0
  ::  Flip hash fields to little-endian format
  =/  prev-hash=@ux    dat:(flip:byt:bcu [32 prev-hash-be])
  =/  merkle-root=@ux  dat:(flip:byt:bcu [32 merkle-root-be])
  [version prev-hash merkle-root timestamp bits nonce]
::  +parse-raw-to-header: Complete raw hex to block-header conversion
::
++  parse-raw-to-header
  |=  [raw-header=@t height=@ud]
  ^-  block-header
  =/  computed-hash=@ux  (compute-block-hash raw-header)
  =/  [version=@ud prev-hash=@ux merkle-root=@ux timestamp=@ud bits=@ux nonce=@ud]
    (parse-header-fields raw-header)
  :*  height=height
      version=version
      prev-hash=`@uvI`prev-hash
      merkle-root=`@uvI`merkle-root
      timestamp=timestamp
      bits=bits
      nonce=nonce
      computed-hash=`@uvI`computed-hash
      verified-pow=%.n
      cumulative-work=0
      children=*(set @uvI)
      raw=raw-header
  ==
::
:: PoW validation
::
++  bits-to-target
  |=  bits=@ux
  ^-  @ux
  ::  Bitcoin compact bits format: 0xAABBCCDD
  ::  AA = exponent, BBCCDD = mantissa
  ::  target = mantissa * 2^(8*(exponent-3))
  =/  exponent  (rsh [3 3] bits)  :: Top byte
  =/  mantissa  (end [3 3] bits)  :: Bottom 3 bytes
  ?:  (gth exponent 3)
    (lsh [3 (sub exponent 3)] mantissa)
  (rsh [3 (sub 3 exponent)] mantissa)
::
++  calculate-work
  |=  target=@ux
  ^-  @ud
  ::  Work = 2^256 / (target + 1)
  =/  max-hash=@  (bex 256)  :: 2^256
  (div max-hash +(target))
::
++  validate-header-with-hash
  |=  [header=block-header computed-hash=@ux]
  ^-  [validated-header=block-header pow-valid=? work=@ud]
  =/  target=@ux  (bits-to-target bits.header)
  =/  pow-valid=?  (lth computed-hash target)
  =/  work=@ud  (calculate-work target)
  :-  header(computed-hash `@uvI`computed-hash, verified-pow pow-valid)
  [pow-valid work]
::
:: Chain validation
::
++  validate-chain-connection
  |=  $:  header=block-header
          headers=(map @uvI block-header)
          checkpoint-hash=@uvI
      ==
  ^-  [valid=? parent=(unit block-header)]
  ::  Allow if this is first block
  ?:  =(0 (lent ~(tap by headers)))
    [%.y ~]
  ::  Allow if connects to checkpoint
  ?:  =(prev-hash.header checkpoint-hash)
    [%.y ~]
  ::  Check if parent exists in chain
  =/  parent=(unit block-header)  (~(get by headers) prev-hash.header)
  ?~  parent
    [%.n ~]
  [%.y parent]
::
++  add-header-to-chain
  |=  $:  header=block-header
          parent=(unit block-header)
          headers=(map @uvI block-header)
      ==
  ^-  (map @uvI block-header)
  ::  If no parent, just add header
  ?~  parent
    (~(put by headers) computed-hash.header header)
  ::  Update parent's children set, then add header
  =/  updated-parent=block-header
    u.parent(children (~(put in children.u.parent) computed-hash.header))
  =/  headers-with-parent  (~(put by headers) prev-hash.header updated-parent)
  (~(put by headers-with-parent) computed-hash.header header)
::
++  calculate-cumulative-work
  |=  [parent=(unit block-header) block-work=@ud]
  ^-  @ud
  ?~  parent
    block-work
  (add cumulative-work.u.parent block-work)
::
::  Verify parsed merkle proof against block header
::  Returns success or error tang
::
++  verify-parsed-merkle-proof
  |=  [txid=@t merkles=(list @t) pos=@ud header=block-header]
  ^-  (each ~ tang)
  %-  mule  |.
  =/  merkle-root-hex=@t
    (hash-to-hex `@ux`merkle-root.header)
  =/  verification-result=?
    (verify-merkle-proof txid merkles pos merkle-root-hex)
  ?.  verification-result
    ~|  %merkle-proof-invalid
    ~|  "Merkle proof verification failed"
    !!
  ~
::
::  Get spv-chain for a network (with safe checkpoint defaults)
::
++  get-spv-chain
  |=  [net=network spv=(map network spv-chain)]
  ^-  spv-chain
  =/  chain=(unit spv-chain)  (~(get by spv) net)
  ?^  chain  u.chain
  ::  Return empty chain with network-specific checkpoint defaults
  =/  [height=@ud hash=@uvI]
    ?+  net  !!
      %main      [925.000 `@uvI`(rash '0000000000000000000067f9f40ca6960173ebee423f6130138762dfc40630bf' hex)]
      %testnet3  [2.500.000 `@uvI`(rash '000000000000004a6039a59a81ad9ca1f6b143ad7c487f2c9e1c1d8e96e1e5ba' hex)]
      %testnet4  [111.000 `@uvI`(rash '0000000007838a124187712a910ef0573cb76104c9a612d8dc4029b964a79007' hex)]
    ==
  :*  ~  ~  ~  height  hash  ~  ~  ==
--