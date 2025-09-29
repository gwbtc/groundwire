::  XX would we ever use a wiry.hoon here?
::/+  wiry
::=>  wiry
/-  bitcoin, ord
/+  bscr=btc-script
::  XX remove this tistar
=*  sha  ..shax
::=|  lac=_&
=|  lac=_|
|%
::
::+$  block
::  $:  hax=@ux
::      reward=@ud
::      height=@ud
::      txs=(list [=txid:ord tx=dataw:tx])
::  ==
::
++  en
  |%
  ++  mails-to-script
    |=  mails=(list mail:ord)
    ^-  script:ord
    (zing (turn mails mail-to-script))
  ::
  ++  mail-to-script
    |=  =mail:ord
    ^-  script:ord
    (draft-to-script (mail-to-draft mail))
  ::
  ++  mail-to-draft
    |=  =mail:ord
    |^  ^-  draft:ord
    %-  ~(gas by *draft:ord)
    %-  zing
    ^-  (list (list (pair @ud octs)))
    :~  ?~(mime.mail ~ [1 p.mime.mail p.+.mime.mail]^~)
        ?~(code.mail ~ [9 p.code.mail p.+.code.mail]^~)
        ?~(pntr.mail ~ [2 p.pntr.mail p.+.pntr.mail]^~)
        ?~(rent.mail ~ [3 (insc rent.mail)]^~)
        ?~(gate.mail ~ [11 (insc gate.mail)]^~)
        ?~(meta.mail ~ [5 meta.mail]^~)
        ?~(prot.mail ~ [7 prot.mail]^~)
        ?~(data.mail ~ [0 data.mail]^~)
    ==
    :::~  ?~(mime.mail ~ [1 p.mime.mail (rev 3 p.mime.mail p.+.mime.mail)]^~)
    ::    ?~(code.mail ~ [9 p.code.mail (rev 3 p.code.mail p.+.code.mail)]^~)
    ::    ?~(pntr.mail ~ [2 p.pntr.mail (rev 3 p.pntr.mail p.+.pntr.mail)]^~)
    ::    ?~(rent.mail ~ [3 (insc rent.mail)]^~)
    ::    ?~(gate.mail ~ [11 (insc gate.mail)]^~)
    ::    ?~(meta.mail ~ [5 p.meta.mail (rev 3 p.meta.mail q.meta.mail)]^~)
    ::    ?~(prot.mail ~ [7 p.prot.mail (rev 3 p.prot.mail q.prot.mail)]^~)
    ::    ?~(data.mail ~ [0 p.data.mail (rev 3 p.data.mail q.data.mail)]^~)
    ::==
    ::
    ++  insc
      |=  [p=@ud oid=(each insc:ord @)]
      ^-  octs
      :-  p
      ::?.  ?=(%& -.oid)  (rev 3 p p.oid)
      ::(con (lsh [3 (sub p 32)] (rev 3 32 txid.p.oid)) (rev 3 (sub p 32) idx.p.oid))
      ?.  ?=(%& -.oid)  p.oid
      (con (lsh [3 (sub p 32)] txid.p.oid) idx.p.oid)
    --
  ::
  ++  rip-octs
    |=  octs
    ^-  (list octs)
    =/  met-q  (met 3 q)
    ?>  (lte met-q p) :: todo: prob unnecessary
    =/  ripped  (rip [3 520] q)
    |-  ^-  (list octs)
    ?~  ripped  ~
    ?~  t.ripped  [(met 3 i.ripped) i.ripped]^~
    [520 i.ripped]^$(ripped t.ripped)
  ::
  ++  push-data
    |=  data=octs
    =/  ripped  (rip-octs data)
    ?:  =(ripped ~)  !! ::~|(%en-draft-push-no-content !!)
    |-  ^-  script:ord
    ?~  ripped  ~
    :-  (push-one-data i.ripped)
    $(ripped t.ripped)
  ::
  ++  push-one-data
    |=  octs
    ^-  op:bscr
    ?>  (lte (met 3 q) p)
    ?>  !=(0 p)
    ?>  (lte p 520)
    ?:  (lte p 0x4b)  op-push+~+p^q
    ?:  (lte p 0xff)  op-push+1+p^q
    ?>  (lte p 520)
    op-push+2+p^q
  ::
  ++  draft-to-script
    |=  =draft:ord
    ^-  script:ord
    =/  data  (~(get by draft) 0)
    =/  meta  (~(get by draft) 5)
    =.  draft  (~(del by (~(del by draft) 0)) 5)
    =/  tags  (sort ~(tap by draft) |=([[a=@ *] [b=@ *]] (lth a b)))
    =-  [op-push+num+1+0 %op-if op-push+~+3+'ord' -]
    |^  ^-  script:ord
    ?~  tags  push-meta
    :+  op-push+num+1+p.i.tags
      (push-one-data q.i.tags)
    $(tags t.tags)
    ::
    ++  push-meta
      ^-  script:ord
      ?~  meta  push-data
      =/  ripped  (rip-octs u.meta)
      |-  ^-  script:ord
      ?~  ripped  push-data
      :+  op-push+num+1+5
        (push-one-data i.ripped)
      $(ripped t.ripped)
    ::
    ++  push-data
      ^-  script:ord
      ?~  data  [%op-endif ~]
      =-  [op-push+num+1+0 -]
      (^push-data u.data)
    --
  --
::
++  de
  |%
  ++  mails
    |=  =script:ord
    (turn (drafts script) draft-to-mail)
  ::
  ++  draft-to-mail
    |=  =draft:ord
    ^-  mail:ord
    :*  (biff (~(get by draft) 1) ascii)  :: tag 1 mimetype 
        (biff (~(get by draft) 9) ascii)  :: tag 9 content encoding
        (biff (~(get by draft) 2) pntr)   :: tag 2 pointer (sat index in outputs of reveal tx)
        (biff (~(get by draft) 3) insc)   :: tag 3 parent
        (biff (~(get by draft) 11) insc)  :: tag 11 delegate
        (fall (~(get by draft) 5) ~)      :: tag 5 metadata (multple pushes)
        (fall (~(get by draft) 7) ~)      :: tag 7 meta protocol
        (fall (~(get by draft) 0) ~)      :: tag 0 content (all pushed data after push of 0 tag)
    ==
  ::
  :: gates don't return a unit but biffing them above compiles. sarpen: "I have committed more wet gate atrocities than anyone." ok
  ++  ascii
    |=  octs
    ^-  [p=@ud (each @t @)]
    ?.  (levy (rip 3 q) |=(@ (lth +< 128)))  [p |+q]
    [p &+q]
  ::
  ++  pntr
    |=  octs
    ^-  [p=@ud (each @ud @)]
    ?.  &((lte p 5) (lte q 0xffff.ffff))  [p |+q]
    ::[p &+(rev 3 p q)]
    [p &+q]
  ::
  ++  insc
    |=  octs
    ^-  [p=@ud (each insc:ord @)]
    ?.  (lth p 33)  [p |+q]
    ::=/  tx  (rev 3 32 (cut 3 [(sub p 32) 32] q))
    =/  tx  (cut 3 [(sub p 32) 32] q)
    =/  ilen  (sub p 32)
    ::[p &+[tx (rev 3 ilen (cut 3 [0 ilen] q))]]
    [p &+[tx (cut 3 [0 ilen] q)]]
  ::
  ++  drafts
    |=  =script:ord
    ^-  (list draft:ord)
    ?~  script  ~
    ?.  ?=([[%op-push * * %0] %op-if [%op-push * * %'ord'] *] script)  $(script t.script)
    =>  .(script t.t.t.script)
    |^  ^-  (list draft:ord)
    =^  tags  script  fetch-tags
    ?~  tags  ^$  [u.tags ^$]
    ::
    ++  fetch-tags
      ^-  [(unit draft:ord) script:ord]
      ?>  ?=(^ script)
      =|  tags=(map @ud (list octs))
      |-  ^+  fetch-tags
      ?:  ?=(%op-endif i.script)
        :_  t.script
        `(~(run by tags) |=((list octs) (roll +< |=([a=octs b=octs] (add p.a p.b)^(cat 3 q.a q.b)))))
      ?>  ?=(^ t.script)
      ?.  ?=([[%op-push *] [%op-push *] * *] script)
        =>  .(script `(lest op:script:ord)`t.script)
        |-  ^+  fetch-tags
        ?:  ?=(%op-endif -.script)  ~^t.script
        ?>  ?=(^ t.script)
        $(script t.script)
      =*  tag  q.octs.i.script
      =*  dat  octs.i.t.script
      ?.  =(tag 0)
        %_  $
          script  t.t.script
          tags
            ?~  d=(~(get by tags) tag)  (~(put by tags) tag dat^~)
            ?.  =(tag 5)  tags
            (~(put by tags) tag dat^u.d)
        ==
      =|  dats=(list octs)
      =>  .(script `(lest op:script:ord)`t.script)
      |-  ^+  fetch-tags
      ?.  ?=(%op-endif i.script)
        ?>  ?=([[%op-push *] ^] script)
        $(dats octs.i.script^dats, script t.script) 
      :_  t.script
      :-  ~
      %-  ~(run by (~(put by tags) 0 dats))
      |=((list octs) (roll +< |=([a=octs b=octs] (add p.a p.b)^(cat 3 q.a q.b))))
    --
  ::
  --
::
++  shan
  |=  a=*
  ?@  a  (shax:sha (cat 3 %atom a)) 
  (shax:sha (rep 3 %cell $(a -.a) $(a +.a) ~))
::
++  si
  |%
  ++  get
    |=  [a=sont-map:ord =txid:ord =pos:ord =off:ord]
    ^-  (unit sont-val:ord)
    ?~  b=(~(get by a) txid pos)  ~
    (~(get by sats.u.b) off)
  ::
  ++  get-com
    |=  [a=sont-map:ord =txid:ord =pos:ord =off:ord]
    ^-  (unit @p)
    ?~(b=(get +<) ~ com.u.b)
  ::
  ++  get-vout
    |=  [a=sont-map:ord =txid:ord =pos:ord]
    ^-  (unit vout-map:ord)
    (~(get by a) txid pos)
  ::
  ++  put-all
    |=  [a=sont-map:ord =txid:ord =pos:ord =off:ord val=@ud com=(unit @p) ins=(set insc:ord)]
    ^-  sont-map:ord
    %+  ~(put by a)  [txid pos]
    =/  b=vout-map:ord  (~(gut by a) [txid pos] [0 ~])
    =/  c=sont-val:ord  (~(gut by sats.b) off [~ ~])
    val^(~(put by sats.b) off c(com com, ins (~(uni in ins.c) ins)))
  ::
  ++  put-ins
    |=  [a=sont-map:ord =txid:ord =pos:ord =off:ord val=@ud ins=(set insc:ord)]
    ^-  sont-map:ord
    %+  ~(put by a)  [txid pos]
    =/  b=vout-map:ord  (~(gut by a) [txid pos] [0 ~])
    =/  c=sont-val:ord  (~(gut by sats.b) off [~ ~])
    val^(~(put by sats.b) off c(ins (~(uni in ins.c) ins)))
  ::
  ++  put-com
    |=  [a=sont-map:ord =txid:ord =pos:ord =off:ord val=@ud com=@p]
    ^-  sont-map:ord
    %+  ~(put by a)  [txid pos]
    =/  b=vout-map:ord  (~(gut by a) [txid pos] [0 ~])
    =/  c=sont-val:ord  (~(gut by sats.b) off [~ ~])
    val^(~(put by sats.b) off c(com `com))
  ::
  ++  del
    |=  [a=sont-map:ord =txid:ord =pos:ord =off:ord]
    ^-  sont-map:ord
    ?~  b=(~(get by a) [txid pos])  a
    =/  c  (~(del by sats.u.b) off)
    ?:  =(c ~)  (~(del by a) txid off)
    (~(put by a) [txid pos] u.b(sats c))
  --
::++  ming
::  |%
::  ++  add-ship
::    |=  [mangs=mang-map =pass who=@p]
::    ^+  mangs
::    %+  ~(put by mangs)  pass
::    =+((~(got by mangs) pass) [txid (~(put in whos) who)])
::  ::
::  ++  del-ship
::    |=  [mangs=mang-map =pass who=@p]
::    ^+  mangs
::    =/  m  =+((~(got by mangs) pass) [txid (~(del in whos) who)])
::    ?~  whos.m  (~(del by mangs) pass)
::    (~(put by mangs) pass m)
::  ::
::  ++  init
::    |=  [old=mang-map new=mang-map txid=pass who=@p]
::    ^+  mangs
::    %+  ~(put by mangs)  pass
::    =+((~(got by mangs) pass) [txid (~(put in whos) who)])
::  --
::::
::+$  mang-map  (map pass [=txid:ord whos=(set @p)])
++  pointer-to-sont
  =|  pos=@ud
  |=  [pntr=@ud outs=(list output:tx:bitcoin)]
  ^-  $@(~ [pos=@ud off=@ud])
  ?~  outs  ~
  ?:  (lth pntr value.i.outs)  [pos pntr]
  $(pos +(pos), pntr (sub pntr value.i.outs))
::
++  update-ins
  |=  [=state:ord oids=(set insc:ord) =sont:ord]
  ?:  =(~ oids)  state
  %-  ~(rep in oids)
  |:  [*=insc:ord state]
  =/  dat  (~(got by insc-ids) insc)
  state(insc-ids (~(put by insc-ids) insc dat(sont sont)))
::
++  update-com
  |=  [=state:ord com=@p =sont:ord]
  =/  point  (~(got by unv-ids:state) com)
  state(unv-ids (~(put by unv-ids:state) com point(sont.own sont)))
::
++  update-ids
  |=  [=state:ord old=sont-val:ord =sont:ord]
  =.  state  (update-ins state ins.old sont)
  ?~  com.old  state
  (update-com state u.com.old sont)
::
--
