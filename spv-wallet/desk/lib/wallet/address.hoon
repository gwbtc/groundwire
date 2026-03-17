/-  *spv-wallet, btc-sur=bitcoin
/+  bip39, bip32, btc=bitcoin, b173=bip-b173, sailbox, json-utils, taproot
|%
::  Map spv-wallet network to crypto-level network for address encoding
::
++  en-crypto
  |=  =network
  ^-  ?(%main %testnet %regtest)
  ?-  network
    %main      %main
    %regtest   %regtest
    %testnet3  %testnet
    %testnet4  %testnet
    %signet    %testnet
  ==
::  Convert seed phrase to extended public key
::
++  seed-to-bytes
  |=  =seed
  ^-  hexb:btc-sur
  ?-  -.seed
    %t  64^(to-seed:bip39 (trip t.seed) "")
    %q  =/  val=@  `@`q.seed
        [(met 3 val) val]
  ==
::
++  seed-to-pubkey
  |=  =seed
  ^-  @ux
  =/  master-wallet  (from-seed:bip32 (seed-to-bytes seed))
  public-key:master-wallet
::  Convert bech32 address to P2WPKH script-pubkey
::  Returns hexb: [22 0x0014<20-byte-hash>]
::
++  address-to-script-pubkey
  |=  address=@t
  ^-  hexb:btc-sur
  ::  Decode the bech32 address to get the pubkey hash
  =/  hash=hexb:btc-sur  (from-address:b173 address)
  ?>  =(20 wid.hash)  ::  Must be 20 bytes for P2WPKH
  ::  Build P2WPKH script: OP_0 (0x00) + PUSH20 (0x14) + hash
  ::  Shift hash left by 20 bytes and OR with hash
  =/  script-data=@ux  (con (lsh [3 20] 0x14) dat.hash)
  [22 script-data]
::  Convert script-pubkey hex text to hexb
::
++  script-hex-to-hexb
  |=  script-hex=@t
  ^-  hexb:btc-sur
  =/  script-data=@ux  (rash script-hex hex)
  =/  byte-length=@ud  (div (met 3 script-data) 2)
  [byte-length script-data]
::  Format a path segment (hardened or not)
::
++  format-seg
  |=  seg=seg:hd-path
  ^-  tape
  =/  [hardened=? index=@ud]  seg
  ?:  hardened
    (weld (numb:sailbox index) "'")
  (scow %ud index)
::  Format an account path as a human-readable string
::
++  format-account-path
  |=  acct=account:hd-path
  ^-  tape
  =/  [purpose=seg:hd-path coin-type=seg:hd-path account=seg:hd-path]  acct
  %+  weld  "m/"
  %+  weld  (format-seg purpose)
  %+  weld  "/"
  %+  weld  (format-seg coin-type)
  %+  weld  "/"
  (format-seg account)
::  Derive a Bitcoin address at a specific index
::  Returns the address as a cord
::
++  derive-address-at-index
  |=  $:  =seed
          acct=account:hd-path
          chain=?(%receiving %change)
          index=@ud
          net=?(%main %testnet %regtest)
      ==
  ^-  @t
  =/  master-wallet  (from-seed:bip32 (seed-to-bytes seed))
  ::  Extract account path components
  =/  [purpose=@ coin-type=@ acct-num=@]
    :-  q.purpose.acct
    :-  q.coin-type.acct
    q.account.acct
  ::  Build derivation path
  =/  chain-num=@ud  ?:(=(chain %receiving) 0 1)
  =/  path=tape
    %+  weld  "m/"
    %+  weld  (scow %ud purpose)
    %+  weld  "'/"
    %+  weld  (scow %ud coin-type)
    %+  weld  "'/"
    %+  weld  (scow %ud acct-num)
    %+  weld  "'/"
    %+  weld  (scow %ud chain-num)
    %+  weld  "/"
    (scow %ud index)
  ::  Derive the address
  =/  derived  (derive-path:master-wallet path)
  =/  network=?(%main %testnet %regtest)  net
  ?:  =(86 purpose)
    ::  BIP-86 taproot: key-only spend (empty script tree)
    (tapscript-address:taproot public-key:derived ~ network)
  =/  address-str=tape
    ?:  =(84 purpose)
      (address-p2wpkh:derived network)
    ?:  =(49 purpose)
      (slag 2 (scow %uc (address-p2sh:derived network)))
    (slag 2 (scow %uc (address:derived network)))
  (crip address-str)
::  Get the next unused address from a map of addresses
::  Respects BIP-44 gap limit (20 unused addresses)
::
++  get-next-unused-address
  |=  leaf-mop=((mop @ud hd-leaf) gth)
  ^-  (unit @t)
  =/  leaf-list=(list [@ud hd-leaf])
    (tap:((on @ud hd-leaf) gth) leaf-mop)
  =|  last-unused=(unit (pair @ud @t))
  |-
  ?~  leaf-list
    ?~(last-unused ~ `q.u.last-unused)
  =/  [idx=@ud =hd-leaf]  i.leaf-list
  =/  details=address-details  main.hd-leaf
  =/  has-txs=?
    ?~  info.details  %.n
    (gth tx-count.u.info.details 0)
  ?:  has-txs
    ::  Found address with txs - check gap limit
    ?~  last-unused
      ~
    =/  gap=@ud  (sub p.u.last-unused idx)
    ?:  (lth gap 20)
      `q.u.last-unused
    ~
  $(leaf-list t.leaf-list, last-unused `[idx address.details])
--
