=>  ..zuse
|%
++  ceri
  |%
  ++  cue                                                 ::  unpack
    |=  [m=(map @ *) a=@]
    ^-  *
    =+  b=0
    =<  q
    |-  ^-  [p=@ q=* r=(map @ *)]
    ?:  =(0 (cut 0 [b 1] a))
      =+  c=(rub +(b) a)
      [+(p.c) q.c (~(put by m) b q.c)]
    =+  c=(add 2 b)
    ?:  =(0 (cut 0 [+(b) 1] a))
      =+  u=$(b c)
      =+  v=$(b (add p.u c), m r.u)
      =+  w=[q.u q.v]
      [(add 2 (add p.u p.v)) w (~(put by r.v) b w)]
    ?:  =(0 (cut 0 [c 1] a))
      =+  d=(rub +(c) a)
      [(add 3 p.d) (need (~(get by m) q.d)) m]
    ?:  =(0 (cut 0 [+(c) 1] a))
      =+  d=(rub (add c 2) a)
      [(add 4 p.d) (need (~(get by m) q.d)) m]
    !!
  ::
  ++  jam                                                 ::  pack
    |=  [om=(map * @) a=*]
    ^-  @
    =+  m=om
    =+  b=0
    =<  q
    |-  ^-  [p=@ q=@ r=(map * @)]
    =+  c=(~(get by m) a)
    ?~  c
      =>  .(m (~(put by m) a b))
      ?:  ?=(@ a)
        =+  d=(mat a)
        [(add 1 p.d) (lsh 0 q.d) m]
      =>  .(b (add 2 b))
      =+  d=$(a -.a)
      =+  e=$(a +.a, b (add b p.d), m r.d)
      [(add 2 (add p.d p.e)) (mix 1 (lsh [0 2] (cat 0 q.d q.e))) r.e]
    ::  todo: factor other bits into length calculation
    ?:  ?&(?=(@ a) (lte (met 0 a) (met 0 u.c)))
      =+  d=(mat a)
      [(add 1 p.d) (lsh 0 q.d) m]
    ?.  (~(has by om) a)
      =+  d=(mat u.c)
      [(add 3 p.d) (mix 3 (lsh [0 3] q.d)) m]
    ?:  =(0 (cut 0 [0 1] u.c))
      =+  d=(mat u.c)
      [(add 4 p.d) (mix 7 (lsh [0 4] q.d)) m]
    !!
  ::
  ++  mat                                                 ::  length-encode
    |=  a=@
    ^-  [p=@ q=@]
    ?:  =(0 a)
      [1 1]
    =+  b=(met 0 a)
    =+  c=(met 0 b)
    :-  (add (add c c) b)
    (cat 0 (bex c) (mix (end [0 (dec c)] b) (lsh [0 (dec c)] a)))
  ::
  ++  rub                                                 ::  length-decode
    |=  [a=@ b=@]
    ^-  [p=@ q=@]
    =+  ^=  c
        =+  [c=0 m=(met 0 b)]
        |-  ?<  (gth c m)
        ?.  =(0 (cut 0 [(add a c) 1] b))
          c
        $(c +(c))
    ?:  =(0 c)
      [1 0]
    =+  d=(add a +(c))
    =+  e=(add (bex (dec c)) (cut 0 [d (dec c)] b))
    [(add (add c c) e) (cut 0 [(add d (dec c)) e] b)]
  --
--
::  Laconic bit
::
::=|  lac=?
=+  lac=`?`&
::  Constants
::  Types
|%
+$  nonce     @ud
::
+$  kv-pok
  $%  [%put key=@t val=json]
      [%del key=@t]
  ==
+$  kv-dif  kv-pok
+$  pki-pok
  $%  [%keys breach=? suite=@ud cry=@ unsgn=@]
      [%ip =ip] 
      ::[%sponsor sponsor=(unit @p)]
      ::[%escape to=(unit @p)]
      ::[%owner =address]
      ::[%spawn-proxy =address]
      ::[%management-proxy =address]
      ::[%voting-proxy =address]
      ::[%transfer-proxy =address]
      ::[%dominion =dominion]
  ==
::
+$  pki-dif
  $%  [%keys breach=? suite=@ud cry=@ unsgn=@ dat=@]
      [%ip =ip] 
      ::[%sponsor sponsor=(unit @p)]
      ::[%escape to=(unit @p)]
      ::[%owner =address]
      ::[%spawn-proxy =address]
      ::[%management-proxy =address]
      ::[%voting-proxy =address]
      ::[%transfer-proxy =address]
      ::[%dominion =dominion]
  ==
+$  txh   @ux   ::  txid
+$  pos   @ud   ::  index in tx output set
+$  off   @ud   ::  sat index in single output amount
+$  pntr  @ud   ::  sat index in total amount of tx outputs
+$  sont  [=txh =pos =off]

::
+$  poke
  $%  [%kv kv-pok]
      [%pki pki-pok]
  ==
::
+$  dif
  $%  [%kv kv-dif]
      [%pki pki-dif]
  ==
+$  noof  [who=@p =dif]
+$  ip  $%([%4 p=@if] [%6 p=@is])
::
::  todo: making who a pointer reference would be shorter
::  but also, management key stuff, could maybe even rekey with ed25519
::  points, etc
+$  nook
  $%  $:  %boot  sig=@ux  icom=$%([%pntr p=pntr] [%sont p=sont])
          ocom=[spk=@ux val=@ud]  ip=$@(~ ip)  suite=@ud  cry=@ux
          unsgn=@ux  spon=(unit @p)
      ==
      [%pokes sig=(unit @ux) who=@p nonce=@ud pokes=(list poke)]
  ==
--
::
|%
++  de-nooks
  |=  dat=@
  ^-  (unit (list nook))
  !!
::++  en-pokes
::  |=  pokes=(list poke)
::  (can (turn pokes en-poke))
::::
::++  en-poke
::  |=  =poke
::  |-  ^-  [wid=@ dat=@]
::  ?-    poke
::      [%pki %keys *]
::    (can 1^0 1^0 1^breach.poke 32^suite.poke 256^cry.poke 256^unsgn.poke ~)
::  ::
::      [%pki %ip %4 *]  (can 1^0 1^1 1^0 32^p.ip.poke ~)
::      [%pki %ip %6 *]  (can 1^0 1^1 1^1 128^p.ip.poke ~)
::      [%kv %put *]  (can 1^1 1^0 (mat key.poke) (mat (en:json:html val.poke)) ~)
::      [%kv %del *]  (can 1^1 1^1 (mat key.poke) ~)
::  ==
::::
::++  en-nook
::  ::  the first bit is 0 for version 0
::  |=  nook
::  ^-  [p=@ q=@]
::  ?-    +<
::      [%boot *]
::    %-  can
::    %-  zing
::    :~  ~[1^0 1^0 32^off 256^sig]
::        ?-(ip ~ ~[1^0], [%4 *] ~[1^1 1^0 32^p.ip], [%6 *] ~[1^1 1^1 128^p.ip])
::        ~[32^suite 256^cry 256^unsgn (mat output)]
::        ?~(spon ~[1^0] ~[1^1 128^u.spon])
::    ==
::  ::
::      [%pokes *]
::    %-  can
::    %-  zing
::    :~  ?~(sig ~[1^0] ~[1^1 256^u.sig])
::        ~[1^0 1^1 128^who 32^nonce (mat (lent pokes)) (en-pokes pokes)]
::    ==
::  ==
::::
::++  de-nook
::  |=  [pos=@ud dat=@]
::  ^-  (unit [nook @ud])
::  =+  ~(. decoder pos dat)
::  =^  ver  pos  (take 0)
::  ?.  =(0 ver)  ~
::  =^  op  pos  (take 0)
::  ?+    op  (debug %invalid-op-code ~)
::      %0
::    =^  off  pos  (take 32)
::    =^  sig  pos  (take 256)
::    =^  ip-p  pos  (take 0)
::    =^  ip  pos  ?~(ip-p `pos take-ip)
::    =^  suite  pos  (take 32)
::    =^  cry  pos  (take 256)
::    =^  unsgn  pos  (take 256)
::    =^  output  pos  take-atom
::    =^  spon-p  pos  (take 0)
::    =^  spon  pos  ?~(spon-p `pos [`- +]:(take 128))
::    `[%boot off sig ip suite cry unsgn output spon]^pos
::  ::
::      %1
::    =^  sig-p  pos  (take 0)
::    =^  sig  pos  ?~(sig-p `pos [`- +]:(take 256))
::    =^  who  pos  (take 128)
::    =^  nonce  pos  (take 32)
::    =^  pok-len  pos  take-atom
::    ?~  pokes=(de-pokes pos dat pok-len)  (debug %de-nook-failed ~)
::    =^  pokes  pos  u.pokes
::    `[%pokes sig who nonce pokes]^pos
::  ==
::::
::++  can
::  |=  a=(list [p=@ q=@])
::  ^-  [p=@ q=@]
::  :_  (^can 0 a)
::  (roll a |=([[a=@ *] b=@] (add a b)))
::::
::++  debug
::  |*  [meg=@t *]
::  ?:  lac
::    +<+
::  ~>  %slog.[0 meg]
::  +<+
::::
::++  de-nooks
::  |=  dat=@
::  =|  pos=@
::  =|  nooks=(list nook)
::  =/  las  (met 0 dat)
::  |-  ^-  (unit (list nook))
::  ?:  (gte pos las)
::    `(flop nooks)
::  =/  nook  (de-nook pos dat)
::  ::  Parsing failed, abort nooks
::  ::
::  ?~  nook
::    (debug %de-nooks-failed ~)
::  $(nooks [-.u.nook nooks], pos +.u.nook)
::::
::++  de-pokes
::  |=  [pos=@ud dat=@ n=@]
::  =|  m=@
::  =|  pokes=(list poke)
::  =/  las  (met 0 dat)
::  |-  ^-  (unit [(list poke) @ud])
::  ?:  |(=(m n) (gte pos las))
::    ?.  =(m n)  (debug %not-enough-pokes ~)
::    `[(flop pokes) pos]
::  =/  poke  (de-poke pos dat)
::  ::  Parsing failed, abort pokes
::  ::
::  ?~  poke
::    (debug %de-pokes-failed ~)
::  $(pokes [-.u.poke pokes], pos pos.u.poke, m +(m))
::::
::++  de-poke
::  |=  [pos=@ud dat=@]
::  =+  ~(. decoder pos dat)
::  |^  ^-  (unit [poke pos=@ud])
::  =^  op  pos  (take 0)
::  ?+    op  (debug %strange-opcode ~)
::      %0  take-pki
::      %1  take-kv
::  ==
::  ::
::  ++  take-pki
::    ^-  (unit [poke pos=@ud])
::    =^  op   pos  (take 0 1)
::    ?+    op  (debug %strange-opcode ~)
::      ::   %0
::      :: =^  breach=@         pos  (take 0)
::      :: =^  =address        pos  (take 3 20)
::      :: `[[%transfer-point address =(0 breach)] pos]
::    :::: 
::      ::   %1
::      :: =^  pad=@     pos  (take 0)
::      :: =^  =ship     pos  (take 3 4)
::      :: =^  =address  pos  (take 3 20)
::      :: `[[%spawn ship address] pos]
::    ::
::        %0
::      =^  breach=@  pos  (take 0)
::      =^  suite=@   pos  (take 3 4)
::      =^  cry=@     pos  (take 3 32)
::      =^  unsgn=@   pos  (take 3 32)
::      `[pki+[%keys =(& breach) suite cry unsgn] pos]
::    ::
::        %1
::      =^  ip  pos  take-ip
::      `[pki+ip+ip pos]
::    ::
::        ::%3   =^(res pos take-ship `[[%escape res] pos])
::        ::%4   =^(res pos take-ship `[[%cancel-escape res] pos])
::        ::%5   =^(res pos take-ship `[[%adopt res] pos])
::        ::%6   =^(res pos take-ship `[[%reject res] pos])
::        ::%7   =^(res pos take-ship `[[%detach res] pos])
::        ::%8   =^(res pos take-address `[[%set-management-proxy res] pos])
::        ::%9   =^(res pos take-address `[[%set-spawn-proxy res] pos])
::        ::%10  =^(res pos take-address `[[%set-transfer-proxy res] pos])
::    ==
::  ::
::  ++  take-kv
::    ^-  (unit [poke pos=@ud])
::    =^  op   pos  (take 0 1)
::    ?+    op  (debug %strange-opcode ~)
::        %0
::      =^  key  pos  take-atom
::      =^  jon  pos  take-atom
::      =/  jon  (de:json:html jon)
::      ?~  jon  (debug %bad-json ~)
::      `[kv+[%put key u.jon] pos]
::    ::
::        %1
::      =^  key  pos  take-atom
::      `[kv+[%del key] pos]
::    ==
::  --
::::
::++  decoder
::  |_  [pos=@ud dat=@]
::  ::
::  ::  Take a bite
::  ::
::  ++  take
::    |=  =bite
::    ^-  [@ @ud]
::    =/  =step
::      ?@  bite  (bex bite)
::      (mul step.bite (bex bloq.bite))
::    [(cut 0 [pos step] dat) (add pos step)]
::  ::  Encode escape-related txs
::  ::
::  ++  take-ship
::    ^-  [ship @ud]
::    =^  pad=@       pos  (take 0)
::    =^  other=ship  pos  (take 3 4)
::    [other pos]
::  ::  Take atom
::  ::
::  ++  take-atom
::    ^-  [@ @ud]
::    =/  m  (rub pos dat)
::    [q.m (add pos p.m)]
::  ::
::  ++  take-ip
::    ^-  [ip @ud]
::    =^  ver=@  pos  (take 0)
::    ?:  =(ver 0)
::      =^  ip=@  pos  (take 0 32)
::      [[%4 ip] pos]
::    =^  ip=@  pos  (take 0 128)
::    [[%6 ip] pos]
::  --
--
