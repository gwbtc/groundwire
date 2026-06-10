::  :spv-wallet|detach <sponsor> <child>
::
::  %detach sotx. Sponsor releases a child (unilateral breakup).
::  **Reveal-mandatory.**
::
/-  urb
/+  mg=rt-management
::
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        [sponsor=ship child=^ship ~]
        ~
    ==
=/  id=@ta  (cat 3 'cw_detach_' (scot %uv eny))
=/  sot=sotx:urb  (detach-sot:mg sponsor child)
[%fiber-poke [id causeway-detach+!>(sot)]]
