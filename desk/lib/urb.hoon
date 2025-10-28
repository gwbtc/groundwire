/-  bitcoin, ord, urb
/+  bscr=btc-script, crac, ol=ord
::=|  lac=_&
=|  lac=_|
|%
++  debug
  |*  [meg=@t *]
  ?:  lac
    +<+
  ~>  %slog.[0 meg]
  +<+
::
++  en
  |%
  ++  unv-to-script
    |=  dat=@
    ^-  script:ord
    =/  len  (met 3 dat)
    :*  [%op-push %num %1 %0]
        %op-if
        op-push+~+3+'urb'
        (snoc (push-data:en:ol len dat) %op-endif)
     ==
  --
::
++  de
  |%
  ++  unv
    |=  =script:ord
    ^-  (list @)
    ?~  script  ~
    ?.  ?=([[%op-push * * %0] %op-if [%op-push * * %'urb'] *] script)
      $(script t.script)
    =>  .(script t.t.t.script)
    |^  ^-  (list @)
    =^  unv  script  fetch-unv
    ?~  unv  ~  [p:(fax:plot bloq=3 u.unv) ^$]
    ::
    ++  fetch-unv
      ^-  [(unit (list plat:plot)) script:ord]
      ?>  ?=(^ script)
      |-  ^-  [(unit (list plat:plot)) script:ord]
      ?:  ?=(%op-endif i.script)  [~ ~]^~
      ?.  ?=([[%op-push *] ^] script)  ~^~
      =/  rest  $(script t.script)
      ?~  -.rest  ~^~
      [~ octs.i.script u.-.rest]^+.rest
    --
  --
++  parse-roll
  |=  batch=@
  =|  roll=(list raw-sotx:urb)
  =|  cur=@ud
  =/  las  (met 0 batch)
  =|  num-msgs=@ud
  |-  ^+  roll
  ?:  (gte cur las)
    (flop roll)
  =/  parse-result  (parse-raw-tx cur batch)
  ::  Parsing failed, abort batch
  ::
  ?~  parse-result
    (debug %parse-failed ~)
  =^  raw-tx  cur  u.parse-result
  $(roll [raw-tx roll], num-msgs +(num-msgs))
::
++  parse-raw-tx
  |=  [cur=@ud batch=@]
  ^-  (unit [raw-sotx:urb cur=@ud])
  |^  ^-  (unit [raw-sotx:urb cur=@ud])
  =/  sig  take-sig
  ?~  sig  (debug %no-sig ~)
  =^  sig  cur  u.sig
  =^  from-ship=ship    cur  (take 0 128)
  =/  res=(unit [tx=skim-sotx:urb cur=@ud])  parse-tx
  ?~  res  ~
  =/  dif  (sub cur.u.res cur)
  =/  len  =>((dvr dif 8) ?:(=(0 q) p +(p)))
  :-  ~
  :_  cur.u.res
  :-  [len (cut 0 [cur dif] batch)]
  [[from-ship sig] tx.u.res]
  ::
  ++  parse-tx
    |-  ^-  res=(unit [tx=skim-sotx:urb cur=@ud])
    =^  op   cur  (take 0 7)
    ?+    op  (debug %strange-opcode ~)
      ::  %0
      ::=^  reset=@         cur  (take 0)
      ::=^  =sont:ord        cur  (take 3 20)
      ::`[[%transfer-point sont =(0 reset)] cur]
    ::
        %1
      |^  ^+  ^$
      =^  pad=@     cur  (take 0)
      =^  =pass     cur  take-atom
      =/  to            take-to
      ?~  to  (debug %no-to ~)
      =^  to        cur  u.to
      ::=/  fom            take-from
      ::?~  fom  ~
      ::=^  fom       cur  u.fom
      ::`[[%spawn pass fom to] cur]
      `[[%spawn pass to] cur]
      ::
      ++  take-from
        ^-  (unit [(unit [=pos:urb =off:urb]) cur=@])
        =^  fro-o  cur  (take 0 2)
        ?:  =(fro-o 0)  `[~ cur]
        ?.  =(fro-o 1)   (debug %no-fro ~)
        =^  pos    cur   take-atom
        =^  off    cur   take-atom
        `[`[pos off] cur]
      ::
      ++  take-to
        ^-  (unit [[spkh=@ux pos=(unit pos:urb) =off:urb tej=off:urb] cur=@])
        =^  spkh  cur  (take 0 256)
        =^  off    cur  take-atom
        =^  tej    cur  take-atom
        =^  pos-o  cur  (take 0 2)
        ?:  =(pos-o 0)
          `[[spkh ~ off tej] cur]
        ?.  =(pos-o 1)  (debug %take-to ~)
        =^  pos    cur   take-atom
        `[[spkh `pos off tej] cur]
      --
    ::
        %2
      =^  breach=@        cur  (take 0)
      =^  =pass     cur  take-atom
      `[[%keys pass =(0 breach)] cur]
    ::
        %3   =^(res cur take-ship `[[%escape res] cur])
        %4   =^(res cur take-ship `[[%cancel-escape res] cur])
        %5   =^(res cur take-ship `[[%adopt res] cur])
        %6   =^(res cur take-ship `[[%reject res] cur])
        %7   =^(res cur take-ship `[[%detach res] cur])
        %8   =^(res cur take-mang ?~(res ~ `[[%set-mang u.res] cur]))
        ::%9   =^(res cur take-sont `[[%set-spawn-proxy res] cur])
        %10
      =^  len  cur  take-atom
      =|  bat=(list single:skim-sotx:urb)
      |-  ^+  ^$
      ?:  =(len 0)  ~^[%batch (flop bat)]^cur
      =/  one  ,:^$
      ?~  one  ~
      ?:  ?=([%batch *] -.u.one)  ~
      =^  one  cur  u.one
      $(len (dec len), bat one^bat)
    ::
        %11
      =^  pad=@   cur   (take 0)
      =^  typ     cur   (take 0 2)
      ?+  typ  ~
          %0
        `[fief/~ cur]
          %1
        !!
        ::=^  len  cur  (take 0 2)
        ::?:  (lth 3 len)  ~
        ::=|  tufs=(list turf)
        ::|-  ^+  ^$
        ::?:  =(len 0)  `[fief/[%turf tufs]]
        ::=^  let  cur  take-atom
        ::=;  tuf
        ::  =^
        ::=|  i=@ud
        ::|-  @ud
        ::=^  car  cur  (take 3)
        ::?.  |(=('-' car) =('.' car) (gte 'a'
      ::
          %2
        =^  pip  cur  (take 3 4)
        =^  por  cur  (take 3 2)
        `[fief/`[%if pip por] cur]
      ::
          %3
        =^  pip  cur  (take 0 128)
        =^  por  cur  (take 3 2)
        `[fief/`[%is pip por] cur]
      ==
    ==
  ::
  ::  Take a bite
  ::
  ++  take
    |=  =bite
    ^-  [@ @ud]
    =/  =step
      ?@  bite  (bex bite)
      (mul step.bite (bex bloq.bite))
    [(cut 0 [cur step] batch) (add cur step)]
  ::
  ++  take-mang
    ^-  [(unit (unit mang:urb)) @ud]
    =^  typ  cur  (take 2)
    ?+    typ  [~ cur]
        %0  [[~ ~] cur]
        %1
      =^  sont  cur  take-sont
      ?~  sont  [~ cur]
      [``[%sont u.sont] cur]
        %2
      =^  pass  cur  (take 0 256)
      [``[%pass pass] cur]
    ==
  ::
  ++  take-atom
    ^-  [@ @ud]
    =/  m  (rub cur batch)
    [q.m (add cur p.m)]
  ::  Encode ship and sont
  ::
  ++  take-sont
    ^-  [(unit sont:urb) @ud]
    =^  pad=@  cur  (take 0)
    ?.  =(pad 0)  ~^cur
    =^  txid    cur  (take 0 256)
    =^  pos    cur   take-atom
    =^  off    cur   take-atom
    [`[txid pos off] cur]
  ::  Encode escape-related txs
  ::
  ++  take-ship
    ^-  [ship @ud]
    =^  pad=@       cur  (take 0)
    =^  other=ship  cur  (take 0 128)
    [other cur]
  ::
  ++  take-sig
    ^-  (unit [(unit @) @ud])
    =^  typ  cur  (take 0 2)
    ?:  =(typ 0)  `[~ cur]
    ?.  =(typ 1)  (debug %take-sig ~)
    =^  sig  cur  (take 0 512)
    `[`sig cur]
  --
::
++  ord-core
  =|  state:ord
  =*  state  -
  |_  $:  ::
          :: cards=(list card:agent:gall)
          fx=(list [id:block:bitcoin effect:ord])
          cb-tx=[val=@ud urb-tx:urb]
          ::n-map=_n-map
      ==
  +*  cor  .
  ++  abed
    |=  =state:ord
    ^+  cor
    cor(state state)
  ::
  ++  emit
    |=  fc=effect:ord
    ^+  cor
    cor(fx :-([block-id fc] fx))
  ::
  ++  emil
    |=  fy=(list effect:ord)
    ^+  cor
    ?~  fy  cor
    =.  cor  (emit i.fy)
    $(fy t.fy)
  ::
  ++  abet
    ^+  [fx state]
    [(flop fx) state]
  ::
  ++  find-block-deps
    :: in order to properly fulfill the coinbase transaction, we need to
    :: keep track of the fee change from skipped tx's
    |=  [num=@ud =block:bitcoin]
    ::  =*  block  +<
    =|  deps=(map [txid:ord pos:urb] [sots=(list raw-sotx:urb) value=(unit @ud)])
    =|  tx-fil=(list tx:bitcoin)
    ^+  [deps block]
    ?.  ?|  =(num start-height:urb)
            =(num +(num.block-id.state))
        ==
      %-  (slog leaf+"can't handle block {<num>}, expected block {<+(num.block-id.state)>}" ~)
      ::  XX should crash
      [deps block]
    =>  ?>  ?=(^ txs.block)
        :-  cb-tx=i.txs.block
        %=  .
          txs.block  t.txs.block
        ==
    |-  ^+  [deps block]
    ?~  txs.block
      :-  deps
      %=  block
        txs  :-(cb-tx (flop tx-fil))
      ==
    =/  is  is.i.txs.block
    =|  ned=_|
    |^  ^+  ^$
        ?~  is
          %=  ^$
            txs.block  t.txs.block
            tx-fil     ?.  ned
                         tx-fil
                        [i.txs.block :-(i.txs.block tx-fil)]
          ==
        =/  value=(unit @ud)
          =/  vout  (get-vout:si:ol sont-map [txid pos]:i.is)
          ?~  vout
            ~
          `value.u.vout
        =.  ned  |(ned ?=(^ value))
        =/  raw-script=(unit octs)
          =/  rwit
            (flop witness.i.is)
          ?.  ?=([* ^] rwit)
            ~
          ?.  =+  i.rwit
              ?&  !=(0 wid)
                  =(0x50 (cut 3 [(dec wid) 1] dat))
              ==
            `i.t.rwit
          ?~  t.t.rwit
            ~
          `i.t.t.rwit
        ?~  raw-script
          (add-to-deps ~ value)
        =/  descr
          (de:bscr u.raw-script)
        ?~  descr
          (add-to-deps ~ value)
        ~|  [=+(u.raw-script [p `@ux`q]) =+((en:bscr u.descr) [p `@ux`q])]
        ?>  =(u.raw-script (en:bscr u.descr))
        =/  unvs=(unit (list @))  (some (unv:de u.descr))
        ?~  unvs  (add-to-deps ~ value)
        =/  sots=(list raw-sotx:urb)
          (zing (turn u.unvs parse-roll))
        (add-to-deps(ned &) sots value)
      ::
    ++  add-to-deps
      |=  [sots=(list raw-sotx:urb) value=(unit @ud)]
      ^+  ^$
      ?>  ?=(^ is)
      ?:  ?&  =(~ sots)
              =(~ value)
          ==
        ^$(is t.is)
      %=  ^$
        is  t.is
        deps  (~(put by deps) [txid pos]:i.is sots value)
      ==
    --
  ::
  ++  apply-block-deps
    |=  [[num=@ud block:bitcoin] deps=(map [txid:ord pos:urb] [sots=(list raw-sotx:urb) value=(unit @ud)])]
    ^-  [num=@ud urb-block:urb]
    =*  block  +<-
    =>  ?>(?=(^ txs) [cb-tx=i.txs .(txs t.txs)])
    =-  %=  block
          txs  ^-  (list urb-tx:urb)
               %+  welp
                 ^-  (list urb-tx:urb)
                 :~  ^-  urb-tx:urb
                     %=  cb-tx
                       is  %+  turn
                             is.cb-tx
                           |=  inputw:tx:bitcoin
                           ^-  input:urb-tx:urb
                           [[~ 0] +<]
                     ==
                 ==
               ^-  (list urb-tx:urb)
               -
        ==
    |-  ^-  (list tx:urb-tx:urb)
    ?~  txs  ~
    =/  is  is.i.txs
    =-  [i.txs(is -) $(txs t.txs)]
    |-  ^-  (list input:urb-tx:urb)
    ?~  is  ~
    =/  dep  (~(got by deps) [txid pos]:i.is)
    [dep(value (need value.dep)) i.is]^$(is t.is)
  ::
  ++  handle-block
    |=  [=num:block:bitcoin =urb-block:urb]
    ^+  cor
    ?.  ?|  =(num start-height:urb)
            =(num +(num.block-id.state))
        ==
      %-  (slog leaf+"can't handle block {<num>}, expected block {<+(num.block-id.state)>}" ~)
      ::  XX should crash
      ::     i kinda don't mind a stateful core just
      ::     returning existing state on error like a
      ::     gall agent would return `this, but how is
      ::     the code calling this function supposed to
      ::     handle that case?
      cor
    =.  num.block-id.state  +(num.block-id.state)
    ?~  txs.urb-block
      ::  XX should crash?
      cor
    =>  %=  .
          txs.urb-block  t.txs.urb-block
          cb-tx         [reward.urb-block i.txs.urb-block]
        ==
    |-  ^+  cor
    :: todo: handle coinbase tx
    ?~  txs.urb-block
      cor
    =.  cor  (handle-tx i.txs.urb-block)
    $(txs.urb-block t.txs.urb-block)
  ::
  ++  handle-tx
    =|  val=@ud
    =|  idx=@ud
    |=  tx=urb-tx:urb
    ^+  cor
    =/  sum-out  (roll os.tx |=([[* a=@] b=@] (add a b)))
    =/  sum-in  (roll is.tx |=([a=input:urb-tx:urb b=@] (add value.a b)))
    =/  is  is.tx
    ?~  is  cor
    |^  ^+  cor
    ::  XX: moved check-for-insc before sont-track-input... consider for
    ::  child etc
    =.  cor  process-unv
    ::=.  cor  check-for-insc
    =.  cor  sont-track-input
    next-input
    ::
    ++  process-unv
      ^+  cor
      =/  sots  sots.i.is
      |-  ^+  cor
      ?~  sots  cor
      =*  raw  raw.i.sots
      =*  who  ship.sot.i.sots
      =*  sig   sig.sot.i.sots
      =-  $.+(cor -, sots t.sots)
      =/  sots=(list single:skim-sotx:urb)
        ?:(?=(%batch +<.sot.i.sots) bat.sot.i.sots ~[+.sot.i.sots])
      =/  point  (~(get by unv-ids) who)
      =|  bat-cnt=@
      |^  ^+  cor
      =.  bat-cnt  +(bat-cnt)
      ?~  sots  cor
      =*  sot  i.sots
      ?:  ?=(%spawn -.sot)
        :: XX: more ordering constraints?
        ?.  =(1 bat-cnt)  cor
        ?~  sig    cor
        ?^  point  cor
        ?:  (~(has by unv-ids) who)  cor ::$(sots t.sots)
        =/  cac  (com:nu:crac pass.sot)
        ?.  =(who fig:ex:cac)  cor :: $(sots t.sots)
        ?~  sat=(get-spawn-sont +>.sot)  cor :: $(sots t.sots)
        ?.  ?=(%c suite.+<.cac)  cor
        ?.  =(dat.tw.pub:+<:cac (rap 3 ~[lyf=1 %btc %ord %gw %test]))  cor
        ?.  (veri-octs:ed:crypto u.sig 512^(shal raw.i.^sots) sgn:ded:ex:cac)
          cor
        =/  sponsor  `@p`(end 4 who)
        =/  =point:ord
          :*  own=[u.sat ~]
              rift=0
              life=1
              pass=pass.sot
              sponsor=[& sponsor]
              escape=~
              fief=~
          ==
        =.  cor
          %-  emil
            :~  [%point who %owner u.sat]
                [%point who %sponsor `sponsor]
                [%point who %keys 1 pass.sot]
            ==
        %_    $
            point    `point
            sots     t.sots
            sont-map  (put-com:si:ol sont-map txid.u.sat pos.u.sat off.u.sat value.i.is who)
            unv-ids   (~(put by unv-ids) who point)
        ==
      ::=^  point  cor  (spend-point point)
      ?~  point  cor
      ?.  (spending-sont sont.own.u.point)  cor
      ?-    -.sot
          %set-mang
        !!
        ::=.  cor  (emit [%point who %mang mang.sot])
        ::%_    $
        ::    sots     t.sots
        ::    unv-ids   (~(put by unv-ids) who u.point)
        ::==
      ::
          %fief
        =.  fief.net.u.point  fief.sot
        =.  cor  (emit [%point who %fief fief.sot])
        %_    $
            sots     t.sots
            unv-ids   (~(put by unv-ids) who u.point)
        ==
      ::
          %escape
        ?:  =(parent.sot who)
          =.  sponsor.net.u.point  &/who
          =.  escape.net.u.point   ~
          =.  cor  (emit [%point who %sponsor `who])
          %_    $
              sots     t.sots
              unv-ids   (~(put by unv-ids) who u.point)
          ==
        =.  escape.net.u.point  `parent.sot
        =.  cor  (emit [%point who %escape `parent.sot])
        %_    $
            sots     t.sots
            unv-ids   (~(put by unv-ids) who u.point)
        ==
      ::
          %cancel-escape
        ?.  =([~ parent.sot] escape.net.u.point)  cor ::$(sots t.sots)
        =.  escape.net.u.point  ~
        =.  cor  (emit [%point who %escape ~])
        %_    $
            sots     t.sots
            unv-ids   (~(put by unv-ids) who u.point)
        ==
      ::
          %detach
        ?~  child=(~(get by unv-ids) ship.sot)  cor ::$(sots t.sots)
        ?.  =([& who] sponsor.net.u.child)  cor ::$(sots t.sots)
        =.  sponsor.net.u.child  |/who
        =.  cor  (emit [%point ship.sot %sponsor ~])
        %_    $
            sots     t.sots
            unv-ids   (~(put by unv-ids) ship.sot u.child)
        ==
      ::
          %adopt
        ?:  =(ship.sot who)
          =.  sponsor.net.u.point  &/who
          =.  escape.net.u.point   ~
          =.  cor  (emit [%point ship.sot %sponsor `who])
          %_    $
              sots     t.sots
              unv-ids   (~(put by unv-ids) who u.point)
          ==
        ?~  child=(~(get by unv-ids) ship.sot)  cor ::$(sots t.sots)
        ?.  =([~ who] escape.net.u.child)  cor ::$(sots t.sots)
        =.  escape.net.u.child  ~
        =.  sponsor.net.u.child  &/who
        =.  cor  (emit [%point ship.sot %sponsor `who])
        %_    $
            sots     t.sots
            unv-ids   (~(put by unv-ids) ship.sot u.child)
        ==
      ::
          %reject
        ?~  child=(~(get by unv-ids) ship.sot)  cor ::$(sots t.sots)
        ?.  =([~ who] escape.net.u.child)  cor ::$(sots t.sots)
        =.  escape.net.u.child  ~
        =.  cor  (emit [%point ship.sot %escape ~])
        %_    $
            sots     t.sots
            unv-ids   (~(put by unv-ids) ship.sot u.child)
        ==
      ::
          %keys
        ::=/  cac  (com:nu:crac pass.sot)
        ::?~  sig                  cor
        ::?.  ?=(%c suite.+<.cac)  cor
        ::?.  =(dat.tw.pub:+<:cac (rap 3 ~[+(life.net.u.point) %btc %ord %gw %test]))  cor
        ::?.  (veri-octs:ed:crypto u.sig 512^(shal raw.i.^sots) sgn:ded:ex:cac)
        ::  cor
        =.  net.u.point
          net.u.point(pass pass.sot, life +(life.net.u.point))
        =?  rift.net.u.point  breach.sot  +(rift.net.u.point)
        =.  cor  %-  emil
          :*  [%point who %keys life.net.u.point pass.sot]
              ?.  breach.sot  ~
              [%point who %rift rift.net.u.point]^~
          ==
        %_    $
            sots     t.sots
            unv-ids   (~(put by unv-ids) who u.point)
        ==
      ::
      ==
      ::
      ::++  spend-point
      ::  |=  point=(unit ^point)
      ::  ^+  [point cor]
      ::  ?~  point  ~^cor
      ::  ?:  &(?=(~ sig) (spending-sont sont.own.u.point))
      ::    point^cor
      ::  ?~  sig  [~ cor]
      ::  :: todo: rethink
      ::  ::?.  ?=([~ %pass *] mang.own.u.point)  [~ cor]
      ::  ::?:  =(txid (cut 8 [1 1] pass.u.mang.own.u.point))  [~ cor]
      ::  ::=/  pub  (end 8 pass.u.mang.own.u.point)
      ::  ::=/  tw  (scap:ed:crypto pub (shax:sha pass.u.mang.own.u.point))
      ::  ::?.  (veri-octs:ed:crypto u.sig raw tw)  [~ cor]
      ::  ::=.  pass.u.mang.own.u.point  (can 8 [1 pub] [1 txid] ~)
      ::  ::[point cor(unv-ids (~(put by unv-ids) who u.point))]
      ::  !!
      ::
      ++  spending-sont
        |=  sot=sont:ord
        ~|  [s=sot [txid pos value]:i.is]
        ?.  =([txid pos]:sot [txid pos]:i.is)  |
        ~|  %fatal-tracking-error
        ?>  (lth off.sot value.i.is)  &
      ::
      ++  get-spawn-sont
        |=  $:  ::from=(unit [=pos:urb =off:urb])
                out=[spkh=@ux pos=(unit pos:urb) =off:urb tej=off:urb]
            ==
        ^-  (unit sont:ord)
        =|  out-pos=@ud
        =|  out-val=@ud
        =/  os  os.tx
        |-  ^-  (unit sont:ord)
        ?~  os  ~
        ?:  &(?=(^ pos.out) (lth u.pos.out out-pos))  ~
        ?:  |((lte (add out-val value.i.os) val) &(?=(^ pos.out) !=(out-pos u.pos.out)))
          $(out-val (add out-val value.i.os), os t.os, out-pos +(out-pos))
        ?:  (lte (add val value.i.is) :(add out-val off.out tej.out))  ~
        ?:  (lte value.i.os (add [off tej]:out))
          $(out-val (add out-val value.i.os), os t.os, out-pos +(out-pos))
        =/  sat=sont:ord  [txid.i.is pos.i.is (sub (add out-val off.out) val)]
        ::?.  |(?=(~ from) !=(u.from [pos off]:sat))  ~
        ?^  (get-com:si:ol sont-map sat)
          ?^(pos.out ~ $(out-val (add out-val value.i.os), os t.os, out-pos +(out-pos)))
        =/  en-out  (can 3 script-pubkey.i.os 8^value.i.os ~)
        =/  hax-out  (shay (add 8 wid.script-pubkey.i.os) en-out)
        ?:  =(hax-out spkh.out)  `sat
        ?.  =(~ pos.out)  ~
        $(out-val (add out-val value.i.os), os t.os, out-pos +(out-pos))
      --
    ::
    ::++  check-for-insc
    ::  ^+  cor
    ::  =/  raw-script=(unit octs)
    ::    =/  rwit  (flop witness.i.is)
    ::    ?.  ?=([* ^] rwit)  ~
    ::    ?.  =+(,.-.rwit &(!=(0 wid) =(0x50 (rsh [3 (dec wid)] dat))))  `i.t.rwit
    ::    ?~(t.t.rwit ~ `i.t.rwit)
    ::  ?~  raw-script  cor
    ::  ::=/  scr  (mole |.((de:bscr u.raw-script)))
    ::  :: XX: make crash-proof
    ::  ::=/  scr  (de:bscr u.raw-script)
    ::  ?~  scr=(de:bscr u.raw-script)  cor
    ::  ?>  =(u.raw-script (en:bscr u.scr))
    ::  =/  mails=(list mail)  (mails:de:ol u.scr)
    ::  |-  ^+  cor
    ::  ?~  mails  cor
    ::  =/  pntr=@ud  ?:(?=([* %& *] pntr.i.mails) p.+.pntr.i.mails 0)
    ::  =/  =insc  id.tx^idx
    ::  =/  nsont  (pntr-to-sont pntr)
    ::  ?~  nsont
    ::    :: the ordinals docs suggests that if the pointer index is
    ::    :: invalid, then it is treated normally i.e. on 0 index
    ::    =.  cor  (emit [%insc insc ~ i.mails])
    ::    %_  $
    ::      idx        +(idx)
    ::      mails      t.mails
    ::      insc-ids   (~(put by insc-ids) insc [[id.tx 0 0] i.mails])
    ::    ==
    ::  =.  cor  (emit [%insc insc nsont i.mails])
    ::  %_  $
    ::    idx     +(idx)
    ::    mails   t.mails
    ::    sont-map  (put-ins:si:ol sont-map txid.nsont pos.nsont off.nsont insc^~^~)
    ::    insc-ids   (~(put by insc-ids) insc [nsont i.mails])
    ::   ==
     ::
    ++  pntr-to-sont
      |=  pntr=@ud
      ^-  $@(~ sont:ord)
      ?.  (lth pntr sum-out)
        =/  sont  (pointer-to-sont:ol (add val.cb-tx (sub pntr sum-out)) os.cb-tx)
        ?:  |(=(~ sont) (lte sum-in pntr))  ~
        ?>  ?=(^ sont)
        [id.cb-tx pos.sont off.sont]
      ?~  sont=(pointer-to-sont:ol pntr os.tx)  !!
      [id.tx pos.sont off.sont]
      ::=/  =txid:ord  txid.i.is
      ::  check for pointer validity here
      ::?.  &(?=([* %& *] pntr) (lth p.+.pntr sum-outs))
      ::  ?~  tracked=(off-to-sont idx)  ~
      ::  [txid tracked]
      ::?~  tagged=(pointer-to-sont:ol p.+.pntr os.tx)  !!
      ::[txid tagged]
    ::
    ::++  inscription-to-sont
    ::  |=  mail
    ::  ^-  $@(~ sont)
    ::  =/  =txidash  txid.i.is
    ::  ::  check for pointer validity here
    ::  ?.  &(?=([* %& *] pntr) (lth p.+.pntr sum-outs))
    ::    ?~  tracked=(off-to-sont idx)  ~
    ::    [txidash tracked]
    ::  ?~  tagged=(pointer-to-sont:ol p.+.pntr os.tx)  !!
    ::  [txidash tagged]
    ::
    ++  off-to-sont
      |=  off=@ud
      ^-  $@(~ sont:ord)
      ::  todo: double check this shorter code does what's intended
      (pntr-to-sont (add val off))
    ::  ?.  (lth (add val off) sum-out)
    ::    ?~  sont=(pointer-to-sont:ol (add val.cb-tx (sub (add val off) sum-out)) os.cb-tx)
    ::      ~
    ::    [txid.cb-tx pos.sont off.sont]
    ::  ?~  sont=(pointer-to-sont:ol (add val off) os.tx)  !!
    ::  [txid pos.sont off.sont]
    ::
    ++  sont-track-input
      :: XX: todo: optimize for updates per-input
      ^+  cor
      ?~  itxo=(~(get by sont-map) txid.i.is pos.i.is)  cor
      =.  sont-map  (~(del by sont-map) txid.i.is pos.i.is)
      =/  isonts  ~(tap by sats.u.itxo)
      |-  ^+  cor
      ?~  isonts  cor
      =/  osont=sont:ord  [txid.i.is pos.i.is p.i.isonts] 
      ?~  nsont=(off-to-sont p.i.isonts)
        =.  state  (update-ids:ol state q.i.isonts [0x0 0 0])
        =.  cor  (emit [%xfer osont [0x0 0 0]])
        %_  $
          isonts  t.isonts
        ==
      =.  state  (update-ids:ol state q.i.isonts nsont)
      =.  cor  (emit [%xfer osont nsont])
      %_  $
        isonts   t.isonts
        sont-map  (put-all:si:ol sont-map txid.nsont pos.nsont off.nsont value.i.is q.i.isonts)
      ==
    ::
    ++  next-input
      ^+  cor
      =<  ?~(t.is cor $(is t.is)) 
      ?.  (lth sum-out (add val value.i.is))  .(val (add val value.i.is))
      .(val.cb-tx (add val.cb-tx (sub (add val value.i.is) sum-out)), val sum-out)
    --
  --
--
