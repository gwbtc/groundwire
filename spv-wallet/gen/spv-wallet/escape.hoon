::  :spv-wallet|escape <point> <parent> [sig]
::
::  %escape sotx. Asks `parent` to adopt `point`. If `sig` is provided, it is
::  the sponsor's off-chain pre-signature over (shaz (jam [point height])) —
::  this lets the escape complete in a single on-chain step without a
::  separate %adopt round-trip.
::
/-  urb
/+  mg=rt-management
::
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        [=ship parent=^ship ~]
        [sig=(unit @) ~]
    ==
=/  id=@ta  (cat 3 'cw_escape_' (scot %uv eny))
=/  sot=sotx:urb  (escape-sot:mg ship parent sig)
[%fiber-poke [id causeway-escape+!>(sot)]]
