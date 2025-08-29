/+  *ord, *test, *mip, lais, crac, gw=groundwire, bip32, b173=bip-b173, rpc=json-rpc, scr=btc-script, strandio, btcio, psbt, bc=bitcoin
=*  raws  raws:gw
|%
++  make-unv-script
  |=  sots=(list sotx)
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
  ?>  =((turn rol |=([* sotx] +<+)) sots)
  tscr
::
++  make-spend-script
  |=  [int-key=@ rest=script:scr]
  ^-  script:scr:gw
  [[%op-push ~ (flipb:gw 32^int-key)] %op-checksig rest]
::
++  make-output
  |=  $:  val=sats:gw
          int-key=keypair:gw
          scr=(unit script:scr)
      ==
  ^-  output:tx:gw
  =?  scr  ?=(^ scr)
    `(make-spend-script x.pub.int-key u.scr)
  =|  =output:tx:gw
  %_  output
    internal-keys      int-key
    spend-script       scr
    value              val
    script-pubkey      ~(scriptpubkey p2tr:gw `x.pub.int-key scr ~)
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
    cor(sed sed, i i, utxo utxo, eny (shas %gw-test-wal-sed sed))
  ::
  ++  derive
    ^-  [keypair:gw _i]
    :_  +(i)
    [pub prv]:(derive-sequence:(from-seed:bip32 32^sed) ~[i 0 0])
  ::
  ++  build-output
    |=  scr=(unit script:scr)
    ^-  [output:gw _cor]
    =/  val
      ?~  spend-script.utxo  (sub value.utxo 150)
      (sub value.utxo (add (lent u.spend-script.utxo) 400))

    =^  kp  i  derive
    :_  cor
    (make-output val kp scr)
  ::
  ++  spend
    |=  [out=output:gw]
    ^-  [byts _cor]
    =|  =tx:gw
    =.  tx  (~(add-input-1 build:gw tx) utxo ~ ~)
        :: by passing ~ we default to SIGHASH_DEFAULT, equivalent to SIGHASH_ALL, so when we sign this input later we'll commit to all and only
        :: the inputs and outputs we've added to the transaction up to that point
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
        |=  $:  ::from=(unit [=pos =off])
                out=[spkvh=@ux pos=(unit pos) =off tej=off]
            ==
        ^-  single:skim-sotx
        [%spawn pub:ex:cac +<]
      ::
      ++  keys
        |=  bec=?
        ^+  [*single:skim-sotx cor]
        =.  cor  cor(lyf +(lyf))
        [%keys pub:ex:cac bec]^cor
      ::
      ++  escape
        |=  her=@p
        ^-  single:skim-sotx
        [%escape her]
      ::
      ++  adopt
        |=  her=@p
        ^-  single:skim-sotx
        [%adopt her]
      ::
      ++  fief
        |=  fief=(unit ^^fief)
        ^-  single:skim-sotx
        [%fief fief]
      ::
      ++  batch
        |=  sots=(list single:skim-sotx)
        ^-  skim-sotx
        [%batch sots]
      --
::
    ++  sign-batch
      |=  sots=(list single:skim-sotx)
      ^-  sotx
      =/  sot  (batch:skim +<)
      =/  ent  (skim:encode:lais sot)
      =/  sig  (sign-octs-raw:ed 512^(shaz ent) [sgn.pub sgn.sek]:+<:cac)
      [fig^[~ sig] sot]
    ::
    ++  sign-skim
      |=  sot=skim-sotx
      ^-  sotx
      =/  ent  (skim:encode:lais sot)
      =/  sig  (sign-octs-raw:ed 512^(shaz ent) [sgn.pub sgn.sek]:+<:cac)
      [fig^[~ sig] sot]
    ::
    ++  spawn
      |=  $:  ::from=(unit [=pos =off])
              out=[spkvh=@ux pos=(unit pos) =off tej=off]
          ==
      ^-  sotx
      =/  sot=skim-sotx  (spawn:skim +<)
      (sign-skim sot)
    ::
    ++  keys
      |=  bec=?
      ^+  [*sotx cor]
      =.  cor  cor(lyf +(lyf))
      =^  sot=skim-sotx  cor  (keys:skim +<)
      (sign-skim sot)^cor
    ::
    ++  escape
      |=  her=@p
      ^-  sotx
      [fig^~ (escape:skim +<)]
    ::
    ++  cancel-escape
      |=  her=@p
      ^-  sotx
      [fig^~ [%cancel-escape her]]
    ::
    ++  adopt
      |=  her=@p
      ^-  sotx
      [fig^~ (adopt:skim +<)]
    ::
    ++  reject
      |=  her=@p
      ^-  sotx
      [fig^~ [%reject her]]
    ::
    ++  detach
      |=  her=@p
      ^-  sotx
      [fig^~ [%detach her]]
    ::
    ++  fief
      |=  fef=(unit ^fief)
      ^-  sotx
      [fig^~ (fief:skim +<)]
    ::
    ++  set-mang
      |=  man=(unit mang)
      ^-  sotx
      [fig^~ [%set-mang man]]
    --
  ++  btc
    |%
    ++  spend
      |=  [out=output:gw]
      ^-  [byts _cor]
      =^  res  wal  (spend:wal out)
      [res cor]
    ::
    ++  spawn
      |=  $:  ::from=(unit [=pos =off])
              out=[spkvh=@ux pos=(unit pos) =off tej=off]
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
    ::++  escape
    ::  |=  her=@p
    ::  ^-  [output:gw _cor]
    ::  =^  out  wal
    ::    %^  build-output:wal  (sub val:wal 150)
    ::   (make-unv-script (escape:unv-tx +<) ~)
    ::  out^cor
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
    ::  |=  man=(unit mang)
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
++  count-sonts
  |=  sm=sont-map
  %-  ~(rep by sm)
  |=  [[* a=(mip pos off sont-val)] b=@]
  %-  ~(rep by a)
  |=  [[* a=(map off sont-val)] =_b]
  (add ~(wyt by a) b)
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
  =/  nadopt-commit-out-ali  (can 3 script-pubkey.adopt-commit-out-ali 8^value.adopt-commit-out-ali ~)
  =/  hadopt-commit-out-ali  (shay (add 8 p.script-pubkey.adopt-commit-out-ali) nadopt-commit-out-ali)
  =^  spawn-commit-out-ali  ali  (spawn:btc:ali hadopt-commit-out-ali `0 0 0)
  =^  spawn-commit-tx-ali  ali  (spend:btc:ali spawn-commit-out-ali)
  ::=^  adopt-commit-tx-ali  ali  (spend:btc:ali adopt-commit-out-ali)
  ::=^  keys-commit-ali  ali  (keys:btc:ali |)
  ::=/  nkeys-commit-ali  (can 3 script-pubkey.keys-commit-ali
  ::8^value.keys-commit-ali ~)
  ::=/  hkeys-commit-ali  (shay (add 8 p.script-pubkey.keys-commit-ali) nkeys-out-ali)


  ::~[spawn-commit-tx-ali adopt-commit-tx-ali]
  ~[spawn-commit-tx-ali]

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
::      ^-  (list [val=@ud wat=_walt spk=byts pos=@ud off=@ud xsot=(list single:skim-sotx)])
::      :~  =-  [val=100 ali spk=[256 (shax 'ali-spawn')] pos=0 off=0 xsot=-]
::          ~[(adopt:skim:ali fig:ali) kex (fief:skim:ali ~ %if .127.0.0.1 (add sal 1.337))]
::          =-  [val=100 bob spk=[256 (shax 'bob-spawn')] pos=1 off=0 xsot=-]
::          ~[(adopt:skim:bob fig:bob) kex (fief:skim:bob ~ %if .127.0.0.2 (add sal 1.338))]
::          [val=100 car spk=[256 (shax 'car-spawn')] pos=2 off=0 ~[(escape:skim:car fig:ali)]]
::          [val=100 dav spk=[256 (shax 'dav-spawn')] pos=3 off=0 ~[(escape:skim:dav fig:bob)]]
::      ==
::    ^-  (list sotx)
::    :~  (adopt:ali fig:car)
::        (adopt:bob fig:dav)
::    ==
::  =.  ali  nali
::  =.  bob  nbob
::  oc
::::
::++  sing
::  |=  [wat=_walt oc=_ord-core val=@ sot=sotx]
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
::  |=  [itxid=@ pos=@ud value=@ sots=(list sotx)]
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
::  ?>  =((turn rol |=([* sotx] +<+)) sots)
::  =|  in=inputw:tx
::  =.  value.in  value
::  =.  witness.in  [wit [0 0] ~]
::  =.  txid.in  itxid
::  =.  pos.in  pos
::  in
::::
::++  spawns
::  |=  $:  itxid=@ux  hers=(list [val=@ud wat=_walt spk=byts pos=@ud off=@ud xsot=(list single:skim-sotx)])
::          aft=(list sotx)
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
::    =|  sots=(list sotx)
::    =|  i=@ud
::    |-  ^+  sots
::    ?>  ?=(^ os)
::    ?~  hers  (flop sots)
::    =/  en-out  (can 3 spk.i.hers 8^value.i.os ~)
::    =/  hax-out  (shay (add 8 wid.script-pubkey.i.os) en-out)
::    =;  sot=sotx  $(sots sot^sots, hers t.hers, i +(i))
::    =/  sot=single:skim-sotx  (spawn:skim:wat.i.hers hax-out ?:(=(0 (mod i 2)) ~ `pos.i.hers) off.i.hers 0)
::    (sign-skim:wat.i.hers ?:(=(~ xsot.i.hers) sot [%batch sot xsot.i.hers]))
::  ::
::  --
::::
::::++  keys
::::  ^-  [sots=(list sotx) =_wats]
::::  =|  n=@ud
::::  =/  taws  ^+(wats ~)
::::  =|  sots=(list sotx)
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
::  |^  ^-  dataw:tx  ::^-  (list sotx)
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
::    =|  sots=(list sotx)
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
::  ^-  [sots=(list sotx) =_wats]
::  =|  n=@ud
::  =/  taws  ^+(wats ~)
::  =|  sots=(list sotx)
::  |-  ^+  [sots wats]
::  ?~  wats  [(flop sots) (flop taws)]
::  =*  wat  i.wats
::  =^  sot  wat  (keys:wat =(0 (mod n 2)))
::  $(sots sot^sots, taws wat^taws, n +(n), wats t.wats)
::
--
