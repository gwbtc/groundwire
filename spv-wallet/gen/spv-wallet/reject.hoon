::  :spv-wallet|reject <sponsor> <child>
::
::  %reject sotx. Sponsor denies a pending escape. **Reveal-mandatory.**
::
/-  urb
/+  mg=rt-management
::
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        [sponsor=ship child=^ship ~]
        ~
    ==
=/  id=@ta  (cat 3 'cw_reject_' (scot %uv eny))
=/  sot=sotx:urb  (reject-sot:mg sponsor child)
[%fiber-poke [id causeway-reject+!>(sot)]]
