::  lib/urb-encoder.hoon
::
::  encode and decode:
::  - urb actions (sotx:urb)
::  - pushdata for Taproot scripts, sometimes called an "unv"
::  - bitcoin scripts
::
::  "unv" is short for "urbit envelope."
::  an unv is an atom that comes from the body
::  of an ordinal-style Taproot script tagged 
::  with "urb" instead of "ord":
::
::  OP_PUSH 1 0
::  OP_IF
::    "urb"
::    <len, dat>
::  OP_ENDIF
::
::  An unv is variable-sized and 
::  gets parsed into a list of "raw-sotx".
::  an ordinal script can also include multiple
::  unvs, giving us a (list (list raw-sotx))
::  that we flatten.
::
::  A raw-sotx is a [raw=octs sotx].
::  we keep the raw data around even after parsing
::  because several proof steps depend on it.
::
::  A sotx is similar to a jael udiff, and
::  indeed gets turned into a jael udiff,
::  which is what %urb-core ultimately
::  uses this library for.
::
/-  ord, urb
/+  bscr=btc-script, ol=ord
|%
++  encode
  =<  full
  =,  ord
  =<  |%
      ++  full
        |=  sots=(list sotx:urb)
        p:(fax:plot (^full sots))
      ::
      ++  skim
        |=  sot=skim-sotx:urb
        p:(fax:plot [0 (^skim sot)])
      --
  |%
  ++  full
    |=  sots=(list sotx:urb)
    :-  bloq=0
    |-  ^-  (list plat:plot)
    ?~  sots  ~
    =*  our  ship.i.sots
    =*  sig   sig.i.sots
    :*  [s+~ (en-sig sig)]
        [128 our]
        [s+~ 0 (skim +.i.sots)]
        $(sots t.sots)
     ==
  ::
  ++  skim
    |=  sot=skim-sotx:urb
    ^-  (list plat:plot)
    ?-    -.sot
        %batch
      =/  l  (lent bat.sot)
      ?>  (lth 1 l)
      :+  [7 10]  (mat l)
      |-  ^+  ^$
      ?~  bat.sot  ~
      [[s+~ 0 ^$(sot i.bat.sot)] $(bat.sot t.bat.sot)]
    ::
        %set-mang
      ?~  mang.sot  [[7 8] [2 0] ~]
      ?-  -.u.mang.sot
          %sont
        [[7 8] [2 1] (en-sont sont.u.mang.sot)]
          %pass
        [[7 8] [2 2] [256 pass.u.mang.sot] ~]
      ==
    ::
        %spawn
      |^  ^+  ^$
      =+  m=(mat pass.sot)
      ::[[7 1] [1 0] m en-to en-from]
      ::  LLM: added en-fief to encode inline (unit fief) for spawn
      [[7 1] [1 0] m en-fief en-to ~]
      ::
      ::  LLM: reuses the fief encoding format from standalone %fief (opcode 11)
      ::  but without the opcode/pad prefix, since spawn has its own
      ++  en-fief
        ^-  plat:plot
        :+  s+~  0
        ?~  fief.sot  ~[[2 0]]
        =*  fef  u.fief.sot
        ?-  -.fef
          %turf  !!
          %if  ~[[2 2] [32 p.fef] [16 q.fef]]
          %is  ~[[2 3] [128 p.fef] [16 q.fef]]
        ==
      ::
      ++  en-to
        ^-  plat:plot
        :+  s+~  0
        :*  [256 spkh.to.sot]
            (mat off.to.sot)  (mat tej.to.sot)
            ?~(vout.to.sot [2 0]^~ [2 1]^(mat u.vout.to.sot)^~)
            ::?~(vout.to.sot ~ [(mat u.vout.to.sot) ~])
        ==
      ::  ++  en-from
      ::    ^-  plat:plot
      ::    ?~  from.sot  [2 0]
      ::    [%s 0 ~[[2 1] (mat vout.from) (mat sat.from)]]
      --
    ::
        %keys
      =+  m=(mat pass.sot)
      [[7 2] [1 breach.sot] m ~]
    ::
        %fief
      :+  [7 11]  [1 0]
      ?~  fief.sot  ~[[2 0]]
      =*  fef  u.fief.sot
      ?-  -.fef
        %turf  !!
        %if  ~[[2 2] [32 p.fef] [16 q.fef]]
        %is  ~[[2 3] [128 p.fef] [16 q.fef]]
      ==
    ::
    ::  LLM: separated %escape to encode new sig=(unit @) field
        %escape
      [[7 3] [1 0] [128 parent.sot] [s+~ (en-sig sig.sot)] ~]
    ::
        ?(%cancel-escape %adopt %reject %detach)
      =-  [[7 -] [1 0] [128 +.sot] ~]
      ?-  -.sot
        %cancel-escape  4
        %adopt          5
        %reject         6
        %detach         7
      ==
    ==
  ++  en-sig
    |=  sig=(unit @)
    ^-  plot
    ?~  sig  [bloq=0 [2 0] ~]
    [bloq=0 [2 1] [512 u.sig] ~]
  ::
  ++  en-sont
    |=  sont:ord
    ^-  (list plat:plot)
    =/  mi  (mat vout)
    =/  mo  (mat off)
    [[1 0] [256 txid] mi mo ~]
  --
::
::  The remaining code in this library was 
::  moved here from lib/urb.hoon:
::
::  Wrap a unv in a no-op urb-tagged Taproot script.
++  en
  |%
  ++  unv-to-script
    |=  dat=@
    ^-  script:bscr
    =/  len  (met 3 dat)
    :*  [%op-push %num %1 %0]
        %op-if
        [%op-push ~ %3 'urb']
        (snoc (push-data:en:ol len dat) %op-endif)
     ==
  --
::
:: Unwrap a script into a list of unvs.
++  de
  |%
  ++  unv
    |=  =script:bscr
    ^-  (list @)
    ?~  script  ~
    :: Strip leading ops until we hit the expected urb envelope format
    ?.  ?=([[%op-push * * %0] %op-if [%op-push * * %'urb'] *] script)
      $(script t.script)
    =>  .(script t.t.t.script)
    |^  ^-  (list @)
    =^  unv  script  fetch-unv
    ?~  unv  ~  [p:(fax:plot bloq=3 u.unv) ^$]
    ::
    ++  fetch-unv
      ^-  [(unit (list plat:plot)) script:bscr]
      ?>  ?=(^ script)
      |-  ^-  [(unit (list plat:plot)) script:bscr]
      ?:  ?=(%op-endif i.script)  [~ ~]^~
      ?.  ?=([[%op-push *] ^] script)  ~^~
      =/  rest  $(script t.script)
      ?~  -.rest  ~^~
      [~ octs.i.script u.-.rest]^+.rest
    --
  --
::
::  The following parsing code is adapted from %naive.
::
::  Parse a unv encoding multiple
::  raw-tx into a list of raw-sotx.
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
    ~&  >>>  %parse-failed  !!
  =^  raw-tx  cur  u.parse-result
  $(roll [raw-tx roll], num-msgs +(num-msgs))
::
::  Given an index and a variable-sized atom,
::  parse the raw-tx at that index into a raw-sotx.
++  parse-raw-tx
  |=  [cur=@ud batch=@]
  ^-  (unit [raw-sotx:urb cur=@ud])
  |^  ^-  (unit [raw-sotx:urb cur=@ud])
  =/  sig  take-sig
  ?~  sig  ~&  >>>  %no-sig  !!
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
    ?+    op  ~&  >>>  %strange-opcode  !!
      ::  %0
      ::=^  reset=@         cur  (take 0)
      ::=^  =sont:ord        cur  (take 3 20)
      ::`[[%transfer-point sont =(0 reset)] cur]
    ::
        %1
      |^  ^+  ^$
      =^  pad=@     cur  (take 0)
      =^  =pass     cur  take-atom
      ::  LLM: added take-fief to decode inline (unit fief) for spawn
      =^  fief=(unit fief:urb)  cur  take-fief
      =/  to            take-to
      ?~  to  ~&  >>>  %no-to  !!
      =^  to        cur  u.to
      `[[%spawn pass fief to] cur]
      ::
      ++  take-from
        ^-  (unit [(unit [=vout:ord =off:ord]) cur=@])
        =^  fro-o  cur  (take 0 2)
        ?:  =(fro-o 0)  `[~ cur]
        ?.  =(fro-o 1)   ~&  >>>  %no-fro  !!
        =^  vout    cur   take-atom
        =^  off    cur   take-atom
        `[`[vout off] cur]
      ::
      ++  take-to
        ^-  (unit [[spkh=@ux vout=(unit vout:ord) =off:ord tej=off:ord] cur=@])
        =^  spkh  cur  (take 0 256)
        =^  off    cur  take-atom
        =^  tej    cur  take-atom
        =^  vout-o  cur  (take 0 2)
        ?:  =(vout-o 0)
          `[[spkh ~ off tej] cur]
        ?.  =(vout-o 1)  ~&  >>>  %take-to  !!
        =^  vout    cur   take-atom
        `[[spkh `vout off tej] cur]
      --
    ::
        %2
      =^  breach=@        cur  (take 0)
      =^  =pass     cur  take-atom
      `[[%keys pass =(0 breach)] cur]
    ::
    ::  LLM: %escape now takes sig=(unit @) after parent ship
        %3
      =^  res=ship  cur  take-ship
      =/  sig  take-sig
      ?~  sig  ~
      =^  sig  cur  u.sig
      `[[%escape res sig] cur]
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
    ::  LLM: refactored to use shared ++take-fief arm
        %11
      =^  pad=@  cur  (take 0)
      =^  fief=(unit fief:urb)  cur  take-fief
      `[[%fief fief] cur]
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
    ^-  [(unit sont:ord) @ud]
    =^  pad=@  cur  (take 0)
    ?.  =(pad 0)  ~^cur
    =^  txid    cur  (take 0 256)
    =^  vout    cur   take-atom
    =^  off    cur   take-atom
    [`[txid vout off] cur]
  ::  LLM: decode inline (unit fief), same format as standalone %fief
  ::  (opcode 11) but without the pad bit
  ++  take-fief
    ^-  [(unit fief:urb) @ud]
    =^  typ  cur  (take 0 2)
    ?+  typ  [~ cur]
        %0  [~ cur]
        %2
      =^  pip  cur  (take 3 4)
      =^  por  cur  (take 3 2)
      [`[%if pip por] cur]
        %3
      =^  pip  cur  (take 0 128)
      =^  por  cur  (take 3 2)
      [`[%is pip por] cur]
    ==
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
    ?.  =(typ 1)  ~&  >>  %take-sig  !!
    =^  sig  cur  (take 0 512)
    `[`sig cur]
  --
--
