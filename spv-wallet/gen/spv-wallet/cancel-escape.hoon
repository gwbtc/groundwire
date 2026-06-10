::  :spv-wallet|cancel-escape <point> <parent>
::
::  %cancel-escape sotx. Rescinds a pending escape request.
::
/-  urb
/+  mg=rt-management
::
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        [=ship parent=^ship ~]
        ~
    ==
=/  id=@ta  (cat 3 'cw_cancel_' (scot %uv eny))
=/  sot=sotx:urb  (cancel-escape-sot:mg ship parent)
[%fiber-poke [id causeway-cancel-escape+!>(sot)]]
