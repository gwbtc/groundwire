::  :spv-wallet|rekey <point> <new-pass> [breach]
::
::  %keys sotx (rekey). Bumps the comet's networking key. If `breach` is %.y,
::  the rift is also bumped (destructive rotation — existing channels break).
::
::  Delegates to %spv-wallet via a %causeway-rekey poke carrying [=ship new-pass=@ breach=?].
::  The agent's handler (future work) builds commit+reveal via lib/rt/management.hoon
::  and lib/rt/boot.hoon's taproot signing pipeline.
::
/-  urb
/+  mg=rt-management
  ::  lib/rt/management — shared non-spawn helpers.
::
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        [=ship new-pass=@ ~]
        [breach=? ~]
    ==
=/  id=@ta  (cat 3 'cw_rekey_' (scot %uv eny))
=/  sot=sotx:urb  (rekey-sot:mg ship new-pass breach)
[%fiber-poke [id causeway-rekey+!>(sot)]]
