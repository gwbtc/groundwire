/-  ord, urb
/+  der, scr=btc-script
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
      [[7 1] [1 0] m en-to ~]
      ::
      ++  en-to
        ^-  plat:plot
        :+  s+~  0
        :*  [256 spkh.to.sot]
            (mat off.to.sot)  (mat tej.to.sot)
            ?~(pos.to.sot [2 0]^~ [2 1]^(mat u.pos.to.sot)^~)
            ::?~(pos.to.sot ~ [(mat u.pos.to.sot) ~])
        ==
      ::  ++  en-from
      ::    ^-  plat:plot
      ::    ?~  from.sot  [2 0]
      ::    [%s 0 ~[[2 1] (mat pos.from) (mat sat.from)]]
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
        ?(%escape %cancel-escape %adopt %reject %detach)
      =-  [[7 -] [1 0] [128 +.sot] ~]
      ?-  -.sot
        %escape         3
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
    =/  mi  (mat pos)
    =/  mo  (mat off)
    [[1 0] [256 txid] mi mo ~]
  --
--
