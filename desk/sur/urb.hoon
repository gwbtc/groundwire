/-  bitcoin, ord
|%
::
+|  %constants
::
++  start-height  1
++  start-hash    0x0
::
+|  %types
::
+$  txid
  $+  urb-txid
  @ux   ::  txid
::
+$  pos
  $+  urb-pos
  @ud   ::  index in tx output set
::
+$  off
  $+  urb-off
  @ud   ::  sat index in single output amount
::
+$  sont
  $+  urb-sont
  [=txid =pos =off]
::
+$  unv-ids
  $+  urb-unv-ids
  (map @p point)
::
+$  raw-sotx
  $+  urb-raw-sotx
  [raw=octs sot=sotx]
::
+$  sotx
  $+  urb-sotx
  [[=ship sig=(unit @)] skim-sotx]
::
+$  mang
  $+  urb-mang
  $%([%sont =sont] [%pass =pass])
::
+$  point
  $+  urb-point
  $:  ::  domain
      ::
      ::=dominion
      ::
      ::  ownership
      ::
      $=  own
      $:  =sont
          mang=(unit mang)
      ==
      ::
      ::  networking
      ::
      $=  net
      $:  rift=@ud
          =life
          =pass
          sponsor=[has=? who=@p]
          escape=(unit @p)
          fief=(unit fief)
      ==
  ==
::
+$  turf
  $+  urb-turf
  (list @t)                                     ::  domain, tld first
::
+$  fief
  $+  urb-fief
  $%  [%turf p=(list turf) q=@udE]
      [%if p=@ifF q=@udE]
      [%is p=@isH q=@udE]
  ==
::
+$  diff
  $+  urb-diff
  $%  [%dns domains=(list @t)]
      $:  %point  =ship
          $%  [%rift =rift]
              [%keys =life =pass]
              [%sponsor sponsor=(unit @p)]
              [%escape to=(unit @p)]
              [%owner =sont]
              ::[%spawn-proxy =sont]
              [%mang mang=(unit mang)]
              ::[%voting-proxy =sont]
              ::[%transfer-proxy =sont]
              ::[%dominion =dominion]
              [%fief fief=(unit fief)]
  ==  ==  ==
::
+|  %batteries
::
++  skim-sotx
  =<  many
  |%
  +$  many
    $+  urb-skim-many
    $%  single
        [%batch bat=(list single)]
    ==
  ::
  +$  single
    $+  urb-skim-single
    $%  $:  %spawn  =pass
            ::from=(unit [=pos =off])
            to=[spkh=@ux pos=(unit pos) =off tej=off]
        ==
        [%keys =pass breach=?]
        [%escape parent=ship]
        [%cancel-escape parent=ship]
        [%adopt =ship]
        [%reject =ship]
        [%detach =ship]
        [%fief fief=(unit fief)]
        [%set-mang mang=(unit mang)]
    ==
  ::
  --
++  gw-tx
  =<  tx
  |%
  +$  tx
    $+  urb-gw-tx
    [id=txid data]
  ::
  +$  p-tx
    $+  urb-gw-tx-p-tx
    [id=txid data]
  ::
  +$  data
    $+  urb-gw-tx-data
    $:  is=(list input)
        os=(list output:tx:bitcoin)
        locktime=@ud
        nversion=@ud
        segwit=(unit @ud)
    ==
  ::
  +$  input
    $+  urb-gw-tx-input
    [[sots=(list raw-sotx) value=@ud] inputw:tx:bitcoin]
  --
::
++  gw-block
  =<  block
  |%
  +$  id
    $+  urb-gw-block-id
    [=hax =num]
  ::
  +$  hax
    $+  urb-gw-block-hax
    @ux
  ::
  +$  num
    $+  urb-gw-block-num
    @ud
  ::
  +$  block
    $+  urb-gw-block
    $:  =hax
        reward=@ud
        height=@ud
        txs=(list gw-tx)
    ==
  --
--
