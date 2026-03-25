::  auth.hoon - Authorization proof building
::
::  Builds script-sig and witness from signatures based on spend-type.
::  Pure formatting - no cryptography, just serialization.
::
::  Input:  signature bytes + pubkey (where needed)
::  Output: [script-sig witness] pair for transaction encoding
::
::  Structure:
::    1. Shared helpers build common witness patterns
::    2. Per-type functions assemble the final [script-sig witness]
::
::  Spend type formats:
::    - Legacy:    script-sig contains sig+pubkey, witness empty
::    - SegWit v0: script-sig empty (or redeem), witness contains sig+pubkey
::    - Taproot:   script-sig empty, witness contains sig (+ script data)
::
/+  bcu=bitcoin-utils, btc=bitcoin, taproot
|%
::
::  Shared Helpers
::
::  Build SegWit v0 witness stack with signature and pubkey
::
::  Witness is a stack of items, each prefixed with its length.
::  For P2WPKH/P2SH-P2WPKH: 2 items (signature, then pubkey).
::  Verifier pops pubkey, pops signature, checks sig against pubkey.
::
++  witness-sig-pubkey
  |=  [signature=hexb:btc pubkey=@]
  ^-  hexb:btc
  =/  pubkey-len=@  (met 3 pubkey)
  %-  cat:byt:bcu
  :~  [1 2]                           ::  item count: 2
      [1 wid.signature]               ::  item 1 length
      signature                       ::  item 1: DER signature + sighash byte
      [1 pubkey-len]                  ::  item 2 length
      [pubkey-len pubkey]             ::  item 2: compressed pubkey (33 bytes)
  ==
::  Build taproot control block for script-path spending
::
::  Format: <control-byte> <32-byte internal pubkey> <proof hashes...>
::  Control byte encodes leaf version (0xc0) + output pubkey y-parity.
::
++  control-block
  |=  [pubkey=@ =tapleaf:taproot proof=(list @ux)]
  ^-  hexb:btc
  ::  Compute tweaked pubkey to get y-parity
  ::
  =/  [tweaked-x=@ux parity=?]
    =/  merkle-root=@ux  (merkle-root-from-proof:taproot tapleaf proof)
    (tweak-pubkey:taproot pubkey `merkle-root)
  ::  Control byte: leaf version 0xc0 + parity bit
  ::
  =/  control-byte=@ux  ?:(parity (add 0xc0 1) 0xc0)
  ::  Extract x-only internal pubkey
  ::
  =/  internal-x=@ux  (x-only:taproot pubkey)
  ::  Serialize proof hashes
  ::
  =/  proof-bytes=hexb:btc
    (cat:byt:bcu (turn proof |=(h=@ux [32 h])))
  ::  Assemble control block
  ::
  %-  cat:byt:bcu
  :~  [1 control-byte]
      [32 internal-x]
      proof-bytes
  ==
::
::  ============================================================================
::  Legacy: P2PKH
::  ============================================================================
::
::  Original Bitcoin format. Signature and pubkey in script-sig.
::
++  p2pkh
  |=  [signature=hexb:btc pubkey=@]
  ^-  [script-sig=hexb:btc witness=hexb:btc]
  ::  Build script-sig: <sig-len> <sig> <pubkey-len> <pubkey>
  ::
  =/  pubkey-len=@  (met 3 pubkey)
  =/  script-sig=hexb:btc
    %-  cat:byt:bcu
    :~  [1 wid.signature]
        signature
        [1 pubkey-len]
        [pubkey-len pubkey]
    ==
  ::  Witness is empty for legacy
  ::
  [script-sig [0 0x0]]
::
::  ============================================================================
::  SegWit v0: P2WPKH, P2SH-P2WPKH
::  ============================================================================
::
::  BIP-141 witness format. Signature and pubkey in witness.
::  P2SH-wrapped version includes redeem script in script-sig.
::
++  p2wpkh
  |=  [signature=hexb:btc pubkey=@]
  ^-  [script-sig=hexb:btc witness=hexb:btc]
  ::  script-sig empty, witness has sig+pubkey
  ::
  [[0 0x0] (witness-sig-pubkey signature pubkey)]
::
++  p2sh-p2wpkh
  |=  [signature=hexb:btc pubkey=@]
  ^-  [script-sig=hexb:btc witness=hexb:btc]
  ::  Hash pubkey for redeem script
  ::
  =/  pubkey-hexb=hexb:btc  [33 pubkey]
  =/  pubkey-hash=@ux  dat:(hash-160:bcu pubkey-hexb)
  ::  Build redeem script: OP_0 (0x00) + PUSH20 (0x14) + hash
  ::
  =/  redeem-script=@ux  (con (lsh [3 20] 0x14) pubkey-hash)
  ::  Build script-sig: PUSH22 (0x16) + redeem-script
  ::
  =/  script-sig=hexb:btc  [23 (con (lsh [3 22] 0x16) redeem-script)]
  ::  Witness same as P2WPKH
  ::
  [script-sig (witness-sig-pubkey signature pubkey)]
::
::  ============================================================================
::  Taproot: P2TR key-path, script-path
::  ============================================================================
::
::  BIP-341 witness format. Signature only for key-path.
::  Script-path adds script and control block.
::
++  p2tr-keypath
  |=  signature=hexb:btc
  ^-  [script-sig=hexb:btc witness=hexb:btc]
  ::  Build witness: just the Schnorr signature (64 or 65 bytes)
  ::  Verifier checks sig against the tweaked output pubkey.
  ::
  =/  witness=hexb:btc
    %-  cat:byt:bcu
    :~  [1 1]                         ::  item count: 1
        [1 wid.signature]             ::  item 1 length
        signature                     ::  item 1: Schnorr sig (64B, or 65B with sighash)
    ==
  ::  script-sig empty for taproot
  ::
  [[0 0x0] witness]
::
++  p2tr-scriptpath
  |=  $:  signature=hexb:btc
          pubkey=@
          =tapleaf:taproot
          proof=(list @ux)
          custom-witness=(list hexb:btc)
      ==
  ^-  [script-sig=hexb:btc witness=hexb:btc]
  ::  Get script bytes and build control block
  ::
  =/  script-bytes=hexb:btc  script.tapleaf
  =/  cb=hexb:btc  (control-block pubkey tapleaf proof)
  ::  Count witness items: sig + custom items + script + control-block
  ::
  =/  item-count=@ud  (add 3 (lent custom-witness))
  ::  Build witness stack (bottom to top when executed):
  ::    - signature: proves authorization
  ::    - custom items: script-specific data (e.g. preimages)
  ::    - script: the tapleaf script being executed
  ::    - control block: proves script is in the taproot tree
  ::
  =/  witness=hexb:btc
    %-  cat:byt:bcu
    %-  zing
    :~  ~[[1 item-count]]                           ::  witness item count
        ~[[1 wid.signature] signature]              ::  Schnorr signature
        %+  turn  custom-witness                    ::  script-specific data
        |=(w=hexb:btc (cat:byt:bcu ~[[1 wid.w] w]))
        ~[[1 wid.script-bytes] script-bytes]        ::  tapleaf script
        ~[[1 wid.cb] cb]                            ::  control block
    ==
  ::  script-sig empty for taproot
  ::
  [[0 0x0] witness]
--
