::  :spv-wallet|adopt <sponsor> <child>
::
::  %adopt sotx. Sponsor accepts a child's pending escape.
::  **Reveal-mandatory** — other ships discover this attestation via urb-watcher,
::  so we MUST publish the reveal on-chain (not a confidential commit).
::
/-  urb
/+  mg=rt-management
::
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        [sponsor=ship child=^ship ~]
        ~
    ==
=/  id=@ta  (cat 3 'cw_adopt_' (scot %uv eny))
=/  sot=sotx:urb  (adopt-sot:mg sponsor child)
[%fiber-poke [id causeway-adopt+!>(sot)]]
