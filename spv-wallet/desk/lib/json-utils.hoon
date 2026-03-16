|%
:: grab the head of a list if it exists
::
++  glow
  |*  =(list)
  ^-  (unit _?>(?=(^ list) i.list))
  ?~(list ~ `i.list)
::
++  jo
  |_  jon=json
  ++  get
    |=  =path :: insane path
    ^-  (unit json)
    ?~  path
      [~ jon]
    ?@  jon  ~
    ?+    -.jon  ~
        %o
      ?~  get=(~(get by p.jon) i.path)
        ~
      $(path t.path, jon u.get)
      ::
        %a
      ?~  rus=(rush i.path dem:ag)
        ~
      ?~  wag=(swag [u.rus 1] p.jon)
        ~
      $(path t.path, jon i.wag)
    ==
  ::
  ++  got                                               ::  need value by key
    |=  =path
    (need (get path))
  ::
  ++  gut                                               ::  fall value by key
    |=  [=path jon=json]
    (fall (get path) jon)
  ::
  ++  has                                               ::  key existence check
    |*  =path
    !=(~ (get path))
  :: de-json after get
  ::
  ++  deg
    |*  [=path [de=$-(json *)]]
    (bind (get path) de)
  ::
  ++  dog
    |*  [=path [de=$-(json *)]]
    (de (got path))
  ::
  ++  dug
    |*  [=path [de=$-(json *)] fel=*]
    (fall (deg path de) fel)
  ::
  ++  del
    |=  =path
    ^-  json
    ?>  ?=(^ path)
    ?:  ?=([i=@ta t=~] path)
      ?@  jon  jon
      ?+    -.jon  jon
          %o
        [%o p=(~(del by p.jon) i.path)]
        ::
          %a
        ?~  rus=(rush i.path dem:ag)
          jon
        [%a p=(oust [u.rus 1] p.jon)]
      ==
    ?@  jon  jon
    ?+    -.jon  jon
        %o
      ?~  get=(~(get by p.jon) i.path)
        jon
      [%o p=(~(put by p.jon) i.path $(path t.path, jon u.get))]
      ::
        %a
      ?~  rus=(rush i.path dem:ag)
        jon
      ?~  wag=(swag [u.rus 1] p.jon)
        jon
      [%a p=(into (oust [u.rus 1] p.jon) u.rus $(path t.path, jon i.wag))]
    ==
  ::
  ++  put
    |=  [=path val=json]
    ^-  json
    =-  (fall - jon)
    %-  mole  |.
    |-
    ^-  json
    ?>  ?=(^ path)
    ?:  ?=([i=@ta t=~] path)
      ?@  jon  jon
      ?+    -.jon  jon
        %a  [%a p=(into p.jon (rash i.path dem:ag) val)]
          %o
        :: remove leading ! to allow escaping array creation
        ::
        =?  i.path  ?=(%'!' (end 3 i.path))  (rsh [3 1] i.path)
        [%o p=(~(put by p.jon) i.path val)]
      ==
    ?@  jon  jon
    ?+    -.jon  jon
        %o
      :: remove leading ! to allow escaping array creation
      ::
      =?  i.path  ?=(%'!' (end 3 i.path))  (rsh [3 1] i.path)
      ?^  get=(~(get by p.jon) i.path)
        [%o p=(~(put by p.jon) i.path $(path t.path, jon u.get))]
      ?^  (biff (glow t.path) (curr rush dem:ag))
        [%o p=(~(put by p.jon) i.path $(path t.path, jon a+~))]
      [%o p=(~(put by p.jon) i.path $(path t.path, jon o+~))]
      ::
        %a
      =/  idx=@ud  (rash i.path dem:ag)
      ?^  wag=(swag [idx 1] p.jon)
        [%a p=(into (oust [idx 1] p.jon) idx $(path t.path, jon i.wag))]
      ?^  (biff (glow t.path) (curr rush dem:ag))
        [%a p=(into p.jon idx $(path t.path, jon a+~))]
      [%a p=(into p.jon idx $(path t.path, jon o+~))]
    ==
  ::
  ++  mar
    |=  [=path val=(unit json)]
    ?~  val
      (del path)
    (put path u.val)
  ::
  ++  gas
    |=  vals=(list [=path val=json])
    ^-  json
    ?~  vals
      jon
    $(vals t.vals, jon (put path.i.vals val.i.vals))
  --
--
