::  signer.hoon - Cryptographic signature generation
::
::  Pure cryptography - signs hashes, returns signature bytes.
::  No transaction structure knowledge, just signing primitives.
::
::  Input:  sighash (the hash to sign) + private key
::  Output: signature bytes ready for authorization proof
::
::  Algorithms:
::    - ECDSA: Legacy and SegWit v0 (P2PKH, P2WPKH, P2SH-P2WPKH)
::    - Schnorr: Taproot (P2TR key-path and script-path)
::
/+  bcu=bitcoin-utils, btc=bitcoin, der
|%
::
::  ============================================================================
::  ECDSA (secp256k1)
::  ============================================================================
::
::  Used by legacy P2PKH and SegWit v0 (P2WPKH, P2SH-P2WPKH).
::  Returns DER-encoded signature with SIGHASH_ALL appended.
::
::  DER (Distinguished Encoding Rules) is ASN.1 binary format.
::  Bitcoin requires signatures in this standard format.
::
++  ecdsa
  |=  [sighash=hexb:btc privkey=@ux]
  ^-  hexb:btc
  ::  Sign hash with secp256k1 ECDSA
  ::
  =/  [v=@ r=@ s=@]
    (ecdsa-raw-sign:secp256k1:secp:crypto `@uvI`dat.sighash privkey)
  ::  DER encode the (r, s) pair
  ::  flip converts from big-endian (DER standard) to little-endian (hexb)
  ::
  =/  der-sig=hexb:btc
    %-  flip:byt:bcu
    %-  en:der
    :-  %seq
    :~  [%int r]                                ::  r: first half of signature
        [%int s]                                ::  s: second half of signature
    ==
  ::  Append SIGHASH_ALL (0x01) byte
  ::
  (cat:byt:bcu ~[der-sig [1 1]])
::
::  ============================================================================
::  Schnorr (BIP-340)
::  ============================================================================
::
::  Used by Taproot (P2TR) for both key-path and script-path spending.
::  Returns 64-byte signature, or 65 bytes if non-default sighash type.
::
::  Note: For key-path spending, privkey must be tweaked first.
::  Use tweak-privkey:taproot to compute the tweaked key.
::
++  schnorr
  |=  [sighash=hexb:btc privkey=@ux sighash-type=@ud]
  ^-  hexb:btc
  ::  Auxiliary randomness for BIP-340 nonce generation
  ::  Zero produces deterministic signatures (safe but predictable).
  ::  Random aux adds side-channel resistance if privkey is reused.
  ::  TODO: Use Urbit entropy (eny) for production hardening.
  ::
  =/  aux=@ux  0x0
  ::  Sign hash with BIP-340 Schnorr
  ::
  =/  sig=@  (sign:schnorr:secp256k1:secp:crypto privkey dat.sighash aux)
  ::  SIGHASH_DEFAULT (0x00): return 64-byte signature
  ::  Other types: append sighash byte (65 bytes total)
  ::
  ?:  =(0 sighash-type)
    [64 sig]
  (cat:byt:bcu ~[[64 sig] [1 sighash-type]])
--
