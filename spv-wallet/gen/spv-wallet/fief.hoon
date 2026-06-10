::  :spv-wallet|fief <point> [fief]
::
::  %fief sotx. Set or clear a comet's static-routing fief. fief=~ clears.
::  To set an IPv4 fief: `[%if ip=@ux port=@ud]`. For IPv6: `[%is ip=@ux port=@ud]`.
::
/-  urb
/+  mg=rt-management
::
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        [=ship ~]
        [fief=(unit fief:urb) ~]
    ==
=/  id=@ta  (cat 3 'cw_fief_' (scot %uv eny))
=/  sot=sotx:urb  (fief-sot:mg ship fief)
[%fiber-poke [id causeway-fief+!>(sot)]]
