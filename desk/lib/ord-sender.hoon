/-  urb
/+  *ord, ul=urb, *mip, lais, crac, gw=groundwire, bip32, scr=btc-script
=*  raws  raws:gw
|%
::  Take a list of sotx and return a Taproot script.
++  make-unv-script
  |=  sots=(list sotx:urb)
  ^-  script:scr
  =/  unv=@  (encode:lais sots)
  =/  tscr  (unv-to-script:en:ul unv)
  ::  The rest of this arm just sanity checks our
  ::  new lib/urb encoding arm against the old library.
  =/  wit   (en:bscr tscr)
  =/  de-wit  (need (de:bscr wit))
  ?>  =(de-wit tscr)
  =/  de-unv  (unv:de:ul de-wit)
  ?>  ?=([* ~] de-unv)
  =/  rol  (parse-roll:ul i.de-unv)
  ~|  %failed-to-parse-the-same
  ?>  =((turn rol |=([* sotx:urb] +<+)) sots)
  tscr
::
::  Simple spend-path Taproot script.
::  XX Which key is this?
++  make-spend-script
  |=  [int-key=@ rest=script:scr]
  ^-  script:scr:gw
  [[%op-push ~ (flipb:gw 32^int-key)] %op-checksig rest]
::
++  make-output
  |=  $:  int-key=keypair:gw
          val=(unit @ud)
          scr=(unit script:scr)
      ==
  ^-  output:tx:gw
  =|  out=output:tx:gw
  =?  spend-script.out  ?=(^ scr)
    `(make-spend-script x.pub.int-key u.scr)
  =.  script-pubkey.out
    ~(scriptpubkey p2tr:gw `x.pub.int-key spend-script.out ~)
  =?  value.out  ?=(^ val)  u.val
  %_  out
    internal-keys      int-key
  ==
::
+$  utxo  [outpoint:gw output:gw]
::
::  A single-UTXO Taproot wallet, useful for managing
::  a comet identity.
++  wallet  
  |_  [sed=@ i=@ =utxo eny=@]
  +*  cor  .
  ::
  ::  Generate new wallet.
  ++  nu
    |=  [sed=@ i=@ =^utxo]
    ^+  cor
    cor(sed sed, i i, utxo utxo, eny (shas %urb-test-wal-sed sed))
  ::
  ::  Derive a new BIP-32 keypair.
  ++  derive
    ^-  [keypair:gw _i]
    :_  +(i)
    [pub prv]:(derive-sequence:(from-seed:bip32 32^sed) ~[i 0 0])
  ::
  ++  build-output
    |=  scr=(unit script:scr)
    ^-  [output:gw _cor]
    =^  kp  i  derive
    :_  cor
    (make-output kp ~ scr)
  ::
  ++  spend
    |=  [out=output:gw]
    ^-  [byts _cor]
    =|  =tx:gw
    =.  tx  (~(add-input-1 build:gw tx) utxo ~ ~)
        :: by passing ~ we default to SIGHASH_DEFAULT, equivalent to SIGHASH_ALL, so when we sign this input later we'll commit to all and only
        :: the inputs and outputs we've added to the transaction up to that point
    =.  value.out
      ?~  spend-script.utxo  (sub value.utxo 150)
      (sub value.utxo (add (lent u.spend-script.utxo) 400))
    =.  tx  (~(add-output-1 build:gw tx) out)
    =^  tx  eny  (~(finalize build:gw tx) eny)
    =/  raw=octs  (txn:encode:gw tx)
    =/  txid=@ux  (txid:encode:gw tx)
    =.  utxo  [[txid 0] out]
    [raw cor] 
  --
::
++  compress-point  compress-point:secp256k1:secp:crypto
::
--
::
::  Wrap the wallet core with Urbit operations.
|%
++  walt
  |_  [sed=@uw lyf=_1 xtr=@ wal=_wallet]
  +*  cor  .
  ::
  ++  nu
    |=  [xtr=@ wal=_wallet]
    ^+  cor
    cor(sed sed:wal, xtr xtr, wal wal)
  ::
  ++  cac
    =<  ?>(&(?=(%c suite.+<) ?=(^ sek.+<)) .)
    %:  pit:nu:crac
        512  (shaz (jam sed lyf))
        %c   (rap 3 ~[lyf %btc %ord %gw %test])
        xtr
    ==
  ::
  ++  fig  `@p`fig:ex:cac(lyf 1)
  ++  unv-tx
    |%
    ++  skim
      |%
      ++  spawn
        |=  $:  ::from=(unit [=pos:urb =off:urb])
                out=[spk=output:gw pos=(unit pos:urb) =off:urb tej=off:urb]
            ==
        =/  utxo  utxo:wal
        =.  value.spk.out
          ?~  spend-script.utxo  (sub value.utxo 150)
          (sub value.utxo (add (lent u.spend-script.utxo) 400))
        =.  value.spk.out
          ?~  spend-script.spk.out  (sub value.spk.out 150)
          (sub value.spk.out (add (lent u.spend-script.spk.out) 400))
        =/  en-out  (can 3 script-pubkey.spk.out 8^value.spk.out ~)
        =/  hax-out  (shay (add 8 p.script-pubkey.spk.out) en-out)
        ^-  single:skim-sotx:urb
        [%spawn pub:ex:cac out(spk hax-out)]
      ::
      ++  keys
        |=  bec=?
        ^+  [*single:skim-sotx:urb cor]
        =.  cor  cor(lyf +(lyf))
        [%keys pub:ex:cac bec]^cor
      ::
      ++  escape
        |=  her=@p
        ^-  single:skim-sotx:urb
        [%escape her]
      ::
      ++  adopt
        |=  her=@p
        ^-  single:skim-sotx:urb
        [%adopt her]
      ::
      ++  fief
        |=  fief=(unit ^^fief)
        ^-  single:skim-sotx:urb
        [%fief fief]
      ::
      ++  batch
        |=  sots=(list single:skim-sotx:urb)
        ^-  skim-sotx:urb
        [%batch sots]
      --
::
    ++  sign-batch
      |=  sots=(list single:skim-sotx:urb)
      ^-  sotx:urb
      =/  sot  (batch:skim +<)
      =/  ent  (skim:encode:lais sot)
      =/  sig  (sign-octs-raw:ed:crypto 512^(shaz ent) [sgn.pub sgn.sek]:+<:cac)
      [fig^[~ sig] sot]
    ::
    ++  sign-skim
      |=  sot=skim-sotx:urb
      ^-  sotx:urb
      =/  ent  (skim:encode:lais sot)
      =/  sig  (sign-octs-raw:ed:crypto 512^(shaz ent) [sgn.pub sgn.sek]:+<:cac)
      [fig^[~ sig] sot]
    ::
    ++  spawn
      |=  $:  ::from=(unit [=pos:urb =off:urb])
              out=[spk=output:gw pos=(unit pos:urb) =off:urb tej=off:urb]
          ==
      ^-  sotx:urb
      =/  sot=skim-sotx:urb  (spawn:skim +<)
      (sign-skim sot)
    ::
    ++  keys
      |=  bec=?
      ^+  [*sotx:urb cor]
      =.  cor  cor(lyf +(lyf))
      =^  sot=skim-sotx:urb  cor  (keys:skim +<)
      (sign-skim sot)^cor
    ::
    ++  escape
      |=  her=@p
      ^-  sotx:urb
      [fig^~ (escape:skim +<)]
    ::
    ++  cancel-escape
      |=  her=@p
      ^-  sotx:urb
      [fig^~ [%cancel-escape her]]
    ::
    ++  adopt
      |=  her=@p
      ^-  sotx:urb
      [fig^~ (adopt:skim +<)]
    ::
    ++  reject
      |=  her=@p
      ^-  sotx:urb
      [fig^~ [%reject her]]
    ::
    ++  detach
      |=  her=@p
      ^-  sotx:urb
      [fig^~ [%detach her]]
    ::
    ++  fief
      |=  fef=(unit ^fief)
      ^-  sotx:urb
      [fig^~ (fief:skim +<)]
    ::
    ++  set-mang
      |=  man=(unit mang:urb)
      ^-  sotx:urb
      [fig^~ [%set-mang man]]
    --
  ++  btc
    |%
    ++  make-key-out
      =^  out  wal  (build-output:wal ~)
      out^cor

    ++  spend
      |=  [out=output:gw]
      ^-  [byts _cor]
      =^  res  wal  (spend:wal out)
      [res cor]
    ::
    ++  spawn
      |=  $:  ::from=(unit [=pos =off:urb])
              out=[spk=output:gw pos=(unit pos:urb) =off:urb tej=off:urb]
          ==
      ^-  [output:gw _cor]
      =^  out  wal
        %-  build-output:wal
        `(make-unv-script (spawn:unv-tx +<) ~)
      out^cor
    ::
    ++  keys
      |=  bec=?
      ^-  [output:gw _cor]
      =^  sot  cor  (keys:unv-tx +<)
      =^  out  wal
        %-  build-output:wal
        `(make-unv-script sot ~)
      out^cor
    ::::
    ++  escape
      |=  her=@p
      ^-  [output:gw _cor]
      =^  out  wal
        %-  build-output:wal
        `(make-unv-script (escape:unv-tx +<) ~)
      out^cor
    ::::
    ::++  cancel-escape
    ::  |=  her=@p
    ::  ^-  [output:gw _cor]
    ::  =^  out  wal
    ::    %^  build-output:wal  (sub val:wal 150)
    ::   (make-unv-script (cancel-escape:unv-tx +<) ~)
    ::  out^cor
    ::::
    ++  adopt
      |=  her=@p
      ^-  [output:gw _cor]
      =^  out  wal
        %-  build-output:wal
        `(make-unv-script (adopt:unv-tx +<) ~)
      out^cor
    ::::
    ::++  reject
    ::  |=  her=@p
    ::  ^-  [output:gw _cor]
    ::  =^  out  wal
    ::    %^  build-output:wal  (sub val:wal 150)
    ::   (make-unv-script (reject:unv-tx +<) ~)
    ::  out^cor
    ::::
    ::++  detach
    ::  |=  her=@p
    ::  ^-  [output:gw _cor]
    ::  =^  out  wal
    ::    %^  build-output:wal  (sub val:wal 150)
    ::   (make-unv-script (detach:unv-tx +<) ~)
    ::  out^cor
    ::::
    ::++  fief
    ::  |=  fef=(unit ^fief)
    ::  ^-  [output:gw _cor]
    ::  =^  out  wal
    ::    %^  build-output:wal  (sub val:wal 150)
    ::   (make-unv-script (fief:unv-tx +<) ~)
    ::  out^cor
    ::::
    ::++  set-mang
    ::  |=  man=(unit mang-tx)
    ::  ^-  [output:gw _cor]
    ::  =^  out  wal
    ::    %^  build-output:wal  (sub val:wal 150)
    ::   (make-unv-script (set-mang:unv-tx +<) ~)
    ::  out^cor
    ::::
    --
  --
--
