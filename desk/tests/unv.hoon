/-  urb
/+  *ord, *test, *mip, lais, crac, gw=groundwire, bip32, b173=bip-b173, rpc=json-rpc, scr=btc-script, strandio, btcio, bc=bitcoin
=*  raws  raws:gw
|%
++  make-unv-script
  |=  sots=(list sotx:urb)
  ^-  script:scr
  =/  en-sots  (encode:lais sots)
  =/  tscr  (unv-to-script:en en-sots)
  =/  wit   (en:bscr tscr)
  =/  de-wit  (need (de:bscr wit))
  ?>  =(de-wit tscr)
  =/  de-unv  (unv:de de-wit)
  ?>  ?=([* ~] de-unv)
  =/  rol  (parse-roll i.de-unv)
  ~|  %failed-to-parse-the-same
  ?>  =((turn rol |=([* sotx:urb] +<+)) sots)
  tscr
::
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
++  wallet  
  |_  [sed=@ i=@ =utxo eny=@]
  +*  cor  .
  ++  nu
    |=  [sed=@ i=@ =^utxo]
    ^+  cor
    cor(sed sed, i i, utxo utxo, eny (shas %urb-test-wal-sed sed))
  ::
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
::
--
=/  oc  ord-core
|%
+$  waletz  [ali=_wallet bob=_wallet car=_wallet dav=_wallet]
++  make-walz
  |=  waletz
  :*  ali=(nu:walt 0xcafe.babe ali)
      bob=(nu:walt 0xcafe.babe bob)
      car=(nu:walt 0xcafe.babe car)
      dav=(nu:walt 0xcafe.babe dav)
  ==
::
::++  count-sonts
::  |=  sm=sont-map
::  %-  ~(rep by sm)
::  |=  [[* a=(mip pos off:urb sont-val)] b=@]
::  %-  ~(rep by a)
::  |=  [[* a=(map off:urb sont-val)] =_b]
::  (add ~(wyt by a) b)
::
++  make-batch
  !!
  ::=.  oc  test-0
  ::=.  sal  (add 4 sal)
  ::=.  oc  test-1
  ::oc
::
++  test-0
  |=  =waletz
  ^-  (list byts)
  =+  (make-walz waletz)
  =^  adopt-commit-out-ali  ali  (adopt:btc:ali fig:ali)
  =^  spawn-commit-out-ali  ali  (spawn:btc:ali adopt-commit-out-ali `0 0 0)
  =^  spawn-commit-tx-ali  ali  (spend:btc:ali spawn-commit-out-ali)
  =^  adopt-commit-tx-ali  ali  (spend:btc:ali adopt-commit-out-ali)
  =^  keys-commit-ali  ali  (keys:btc:ali |)
  =^  keys-commit-tx-ali  ali  (spend:btc:ali keys-commit-ali)
  =^  adopt-car-commit-ali  ali  (adopt:btc:ali fig:car)
  =^  adopt-car-commit-tx-ali  ali  (spend:btc:ali adopt-car-commit-ali)

  =^  escape-commit-out-car  car  (escape:btc:car fig:ali)
  =^  spawn-commit-out-car  car  (spawn:btc:car escape-commit-out-car `0 0 0)
  =^  spawn-commit-tx-car  car  (spend:btc:car spawn-commit-out-car)
  =^  escape-commit-tx-car  car  (spend:btc:car escape-commit-out-car)
  =^  keyspend-out-0-car   car  make-key-out:btc:car
  =^  keyspend-tx-0-car       car  (spend:btc:car keyspend-out-0-car)


  =^  keyspend-out-0-ali   ali  make-key-out:btc:ali
  =^  keyspend-tx-0-ali       ali  (spend:btc:ali keyspend-out-0-ali)

  ::=^  keys-commit-ali  ali  (keys:btc:ali |)
  ::=/  nkeys-commit-ali  (can 3 script-pubkey.keys-commit-ali
  ::8^value.keys-commit-ali ~)
  ::=/  hkeys-commit-ali  (shay (add 8 p.script-pubkey.keys-commit-ali) nkeys-out-ali)


  :~  spawn-commit-tx-ali
      adopt-commit-tx-ali
      keys-commit-tx-ali
      spawn-commit-tx-car
      escape-commit-tx-car
      adopt-car-commit-tx-ali
      keyspend-tx-0-car
      keyspend-tx-0-ali
  ==

  ::oc

  ::=.  oc
  ::  %+  handle-tx:oc  0xcafe.babe
  ::  =-  (spawns 0xcafe.beef - ~)
  ::  :~  [val=100 ali spk=[256 (shax 'ali-spawn')] pos=0 off=0 xsot=~]
  ::      [val=100 bob spk=[256 (shax 'bob-spawn')] pos=1 off=0 xsot=~]
  ::      [val=100 car spk=[256 (shax 'car-spawn')] pos=2 off=0 xsot=~]
  ::      [val=100 dav spk=[256 (shax 'dav-spawn')] pos=3 off=0 xsot=~]
  ::  ==
  ::=/  out-ali  (adopt:btc:ali fig:ali)
  ::=/  out-bob  (adopt:btc:bob fig:bob)
  ::=/  tx-ali  (sing ali oc 100 (adopt:ali fig:ali))
  ::=/  tx-bob  (sing bob oc 100 (adopt:bob fig:bob))
  ::=.  oc  (handle-tx:oc 0xcafe.feed tx-ali)
  ::=.  oc  (handle-tx:oc 0xdead.babe tx-bob)
  ::=^  tx-ali  ali  =^(so ali (keys:ali |) (sing ali oc 100 so)^ali)
  ::=^  tx-bob  bob  =^(so bob (keys:bob &) (sing bob oc 100 so)^bob)
  ::=.  oc  (handle-tx:oc 0xdead.f00d tx-ali)
  ::=.  oc  (handle-tx:oc 0xbeef.f00d tx-bob)
  ::=/  tx-ali  (sing ali oc 100 (fief:ali ~ %if .127.0.0.1 (add sal 1.337)))
  ::=/  tx-bob  (sing bob oc 100 (fief:bob ~ %if .127.0.0.2 (add sal 1.338)))
  ::=.  oc  (handle-tx:oc 0xbeef.feed tx-ali)
  ::=.  oc  (handle-tx:oc 0xbabe.feed tx-bob)
  ::=/  tx-car  (sing car oc 100 (escape:car fig:ali))
  ::=/  tx-dav  (sing dav oc 100 (escape:dav fig:bob))
  ::=.  oc  (handle-tx:oc 0xbeef.babe tx-car)
  ::=.  oc  (handle-tx:oc 0xf00d.cafe tx-dav)
  ::=/  tx-ali  (sing ali oc 100 (adopt:ali fig:car))
  ::=/  tx-bob  (sing bob oc 100 (adopt:bob fig:dav))
  ::=.  oc  (handle-tx:oc 0x1337.f00d tx-ali)
  ::=.  oc  (handle-tx:oc 0x1337.babe tx-bob)
  ::oc

::++  test-0
::  =+  make-walz
::  =.  oc
::    %+  handle-tx:oc  0xcafe.babe
::    =-  (spawns 0xcafe.beef - ~)
::    :~  [val=100 ali spk=[256 (shax 'ali-spawn')] pos=0 off=0 xsot=~]
::        [val=100 bob spk=[256 (shax 'bob-spawn')] pos=1 off=0 xsot=~]
::        [val=100 car spk=[256 (shax 'car-spawn')] pos=2 off=0 xsot=~]
::        [val=100 dav spk=[256 (shax 'dav-spawn')] pos=3 off=0 xsot=~]
::    ==
::  =/  out-ali  (adopt:btc:ali fig:ali)
::  =/  out-bob  (adopt:btc:bob fig:bob)
::  =/  tx-ali  (sing ali oc 100 (adopt:ali fig:ali))
::  =/  tx-bob  (sing bob oc 100 (adopt:bob fig:bob))
::  =.  oc  (handle-tx:oc 0xcafe.feed tx-ali)
::  =.  oc  (handle-tx:oc 0xdead.babe tx-bob)
::  =^  tx-ali  ali  =^(so ali (keys:ali |) (sing ali oc 100 so)^ali)
::  =^  tx-bob  bob  =^(so bob (keys:bob &) (sing bob oc 100 so)^bob)
::  =.  oc  (handle-tx:oc 0xdead.f00d tx-ali)
::  =.  oc  (handle-tx:oc 0xbeef.f00d tx-bob)
::  =/  tx-ali  (sing ali oc 100 (fief:ali ~ %if .127.0.0.1 (add sal 1.337)))
::  =/  tx-bob  (sing bob oc 100 (fief:bob ~ %if .127.0.0.2 (add sal 1.338)))
::  =.  oc  (handle-tx:oc 0xbeef.feed tx-ali)
::  =.  oc  (handle-tx:oc 0xbabe.feed tx-bob)
::  =/  tx-car  (sing car oc 100 (escape:car fig:ali))
::  =/  tx-dav  (sing dav oc 100 (escape:dav fig:bob))
::  =.  oc  (handle-tx:oc 0xbeef.babe tx-car)
::  =.  oc  (handle-tx:oc 0xf00d.cafe tx-dav)
::  =/  tx-ali  (sing ali oc 100 (adopt:ali fig:car))
::  =/  tx-bob  (sing bob oc 100 (adopt:bob fig:dav))
::  =.  oc  (handle-tx:oc 0x1337.f00d tx-ali)
::  =.  oc  (handle-tx:oc 0x1337.babe tx-bob)
::  oc
::
::++  test-1
::  =+  make-walz
::  =+  [kex nali]=(keys:skim:ali |)
::  =+  [kex nbob]=(keys:skim:bob &)
::  =.  oc
::    %+  handle-tx:oc  0xcafe.babe
::    %^  spawns  0xcafe.beef
::      ^-  (list [val=@ud wat=_walt spk=byts pos=@ud off=@ud xsot=(list single:skim-sotx:urb)])
::      :~  =-  [val=100 ali spk=[256 (shax 'ali-spawn')] pos=0 off=0 xsot=-]
::          ~[(adopt:skim:ali fig:ali) kex (fief:skim:ali ~ %if .127.0.0.1 (add sal 1.337))]
::          =-  [val=100 bob spk=[256 (shax 'bob-spawn')] pos=1 off=0 xsot=-]
::          ~[(adopt:skim:bob fig:bob) kex (fief:skim:bob ~ %if .127.0.0.2 (add sal 1.338))]
::          [val=100 car spk=[256 (shax 'car-spawn')] pos=2 off=0 ~[(escape:skim:car fig:ali)]]
::          [val=100 dav spk=[256 (shax 'dav-spawn')] pos=3 off=0 ~[(escape:skim:dav fig:bob)]]
::      ==
::    ^-  (list sotx:urb)
::    :~  (adopt:ali fig:car)
::        (adopt:bob fig:dav)
::    ==
::  =.  ali  nali
::  =.  bob  nbob
::  oc
::::
::++  sing
::  |=  [wat=_walt oc=_ord-core val=@ sot=sotx:urb]
::  ^-  tx:gw
::  =/  pon  (~(got by unv-ids:oc) fig:wat)
::  =|  tx=dataw:tx
::  =.  is.tx  ~[(make-unv-input txid.sont.own.pon pos.sont.own.pon val ~[sot])]
::  =.  os.tx  ~[spk^val]
::  tx
::::
::::++  adopt
::::  |=  [wat=_walt oc=_ord-core val=@ who=@p]
::::  ^+  *dataw:tx
::::  =/  pon  (~(got by unv-ids:oc) fig:wat)
::::  =/  sot  (adopt:wat who)
::::  =|  tx=dataw:tx
::::  =.  is.tx  ~[(make-unv-input txid.sont.own.pon pos.sont.own.pon val ~[sot])]
::::  =.  os.tx  ~[[256 (shax 0)]^val]
::::  tx
::::::
::::++  escape
::::  |=  [wat=_walt oc=_ord-core val=@ who=@p]
::::  ^+  *dataw:tx
::::  =/  pon  (~(got by unv-ids:oc) fig:wat)
::::  =/  sot  (escape:wat who)
::::  =|  tx=dataw:tx
::::  =.  is.tx  ~[(make-unv-input txid.sont.own.pon pos.sont.own.pon val ~[sot])]
::::  =.  os.tx  ~[[256 (shax 0)]^val]
::::  tx
::::::
::::++  fief
::::  |=  [wat=_walt oc=_ord-core val=@ fef=(unit ^fief)]
::::  ^+  *dataw:tx
::::  =/  pon  (~(got by unv-ids:oc) fig:wat)
::::  =/  sot  (fief:wat fef)
::::  =|  tx=dataw:tx
::::  =.  is.tx  ~[(make-unv-input txid.sont.own.pon pos.sont.own.pon val ~[sot])]
::::  =.  os.tx  ~[[256 (shax 0)]^val]
::::  tx
::::::
::::++  keys
::::  |=  [wat=_walt oc=_ord-core val=@ bec=?]
::::  ^+  [*dataw:tx wat]
::::  =/  pon  (~(got by unv-ids:oc) fig:wat)
::::  =^  sot  wat  (keys:wat bec)
::::  =|  tx=dataw:tx
::::  =.  is.tx  ~[(make-unv-input txid.sont.own.pon pos.sont.own.pon val ~[sot])]
::::  =.  os.tx  ~[[256 (shax 0)]^val]
::::  tx^wat
::::
::
::++  make-unv-input
::  |=  [itxid=@ pos=@ud value=@ sots=(list sotx:urb)]
::  ^-  inputw:tx
::  =/  en-sots  (encode:lais sots)
::  =/  tscr  (unv-to-script:en en-sots)
::  =/  wit   (en:bscr tscr)
::  =/  de-wit  (need (de:bscr wit))
::  ?>  =(de-wit tscr)
::  =/  de-unv  (unv:de de-wit)
::  ?>  ?=([* ~] de-unv)
::  =/  rol  (parse-roll i.de-unv)
::  ~|  %failed-to-parse-the-same
::  ?>  =((turn rol |=([* sotx:urb] +<+)) sots)
::  =|  in=inputw:tx
::  =.  value.in  value
::  =.  witness.in  [wit [0 0] ~]
::  =.  txid.in  itxid
::  =.  pos.in  pos
::  in
::::
::++  spawns
::  |=  $:  itxid=@ux  hers=(list [val=@ud wat=_walt spk=byts pos=@ud off=@ud xsot=(list single:skim-sotx:urb)])
::          aft=(list sotx:urb)
::      ==
::  |^  ^-  dataw:tx
::  =|  tx=dataw:tx
::  =.  is.tx  ~[make-input]
::  =.  os.tx  make-outputs
::  tx
::  ::
::  ++  make-outputs
::    |-  ^-  (list output:tx)
::    ?~  hers  ~
::    :_  $(hers t.hers)
::    [spk.i.hers val.i.hers]
::  ::
::  ++  make-input
::    ::=.  sots  (weld sots sots)
::    ::=/  sots  make-sots  ::(slag wut make-sots)
::    =/  sots  (weld make-sots aft)
::    (make-unv-input itxid 0 (add 1.000 (roll hers |=([[a=@ *] b=@] (add a b)))) sots)
::  ::
::  ++  make-sots
::    =/  os  make-outputs
::    =|  sots=(list sotx:urb)
::    =|  i=@ud
::    |-  ^+  sots
::    ?>  ?=(^ os)
::    ?~  hers  (flop sots)
::    =/  en-out  (can 3 spk.i.hers 8^value.i.os ~)
::    =/  hax-out  (shay (add 8 wid.script-pubkey.i.os) en-out)
::    =;  sot=sotx:urb  $(sots sot^sots, hers t.hers, i +(i))
::    =/  sot=single:skim-sotx:urb  (spawn:skim:wat.i.hers hax-out ?:(=(0 (mod i 2)) ~ `pos.i.hers) off.i.hers 0)
::    (sign-skim:wat.i.hers ?:(=(~ xsot.i.hers) sot [%batch sot xsot.i.hers]))
::  ::
::  --
::::
::::++  keys
::::  ^-  [sots=(list sotx:urb) =_wats]
::::  =|  n=@ud
::::  =/  taws  ^+(wats ~)
::::  =|  sots=(list sotx:urb)
::::  |-  ^+  [sots wats]
::::  ?~  wats  [(flop sots) (flop taws)]
::::  =*  wat  i.wats
::::  =^  sot  wat  (keys:wat =(0 (mod n 2)))
::::  $(sots sot^sots, taws wat^taws, n +(n), wats t.wats)
::::
::
::::
::++  old-spawns
::  |=  [itxid=@ux os-vals=(list @ud) off=@ud wats=(list _walt)]
::  |^  ^-  dataw:tx  ::^-  (list sotx:urb)
::  =|  tx=dataw:tx
::  =.  is.tx  ~[make-input]
::  =.  os.tx  make-outputs
::  tx
::
::  ++  make-outputs
::    ~+  =|  pos=@ud
::    |-  ^-  (list output:tx)
::    ?~  os-vals  ~
::    :_  $(os-vals t.os-vals, pos +(pos))
::    [256^(shax pos) i.os-vals]
::  ::
::  ++  make-input
::    ::=.  sots  (weld sots sots)
::    ::=/  sots  make-sots  ::(slag wut make-sots)
::    =/  sots  make-sots
::    (make-unv-input itxid 0 (add 1.000 (roll os-vals add)) sots)
::  ::
::  ++  make-sots
::    =/  os  make-outputs
::    =/  l  (lent wats)
::    =/  tot  (roll os |=([[* a=@] b=@] (add a b)))
::    =/  feq  (rsh 8 (div (lsh 8 (dec (sub tot off))) l))
::    ~|  %spawn-test-insufficient-sats
::    ?>  !=(0 tot)
::    ?>  !=(0 feq)
::    ?>  (lth (mul feq l) tot)
::    ::?>  (lth off feq)
::    =|  sats=@ud
::    =|  sots=(list sotx:urb)
::    =|  i=@ud
::    =|  pos=@ud
::    ~|  %spawn-test-shouldnt-happen
::    |-  ^+  sots
::    ?>  ?=(^ os)
::    ?~  wats  (flop sots)
::    =/  xat  (add (mul i feq) off)
::    =/  nsats  (add sats value.i.os)
::    ?:  (lte nsats xat)
::      ?>  ?=(^ t.os)
::      $(os t.os, sats nsats, pos +(pos))
::    =.  i  +(i)
::    =/  en-out  (can 3 script-pubkey.i.os 8^value.i.os ~)
::    =/  hax-out  (shay (add 8 wid.script-pubkey.i.os) en-out)
::    =/  sot
::      (spawn:i.wats hax-out ?:(=(0 (mod i 2)) ~ `pos) (sub xat sats) 0)
::    $(sots sot^sots, wats t.wats)
::  ::
::  --
::
::++  keys
::  ^-  [sots=(list sotx:urb) =_wats]
::  =|  n=@ud
::  =/  taws  ^+(wats ~)
::  =|  sots=(list sotx:urb)
::  |-  ^+  [sots wats]
::  ?~  wats  [(flop sots) (flop taws)]
::  =*  wat  i.wats
::  =^  sot  wat  (keys:wat =(0 (mod n 2)))
::  $(sots sot^sots, taws wat^taws, n +(n), wats t.wats)
--
