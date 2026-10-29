::  ted/bench/liveness: time the liveness walk, one peer at a time vs shared
::
::    -gw-btc!bench-liveness %each n from to
::    -gw-btc!bench-liveness %shared n from to
::
::  Walks .n made-up outpoints (random P2TR scripts, so no filter really
::  matches them) from height .from to .to against the local light
::  client, either one +scan-liveness after another (%each: what %gw-btc
::  does today for n peers) or in one +scan-liveness-many (%shared).
::  Dry: no %gw-liveness-progress pokes.  Returns the elapsed wall time.
::
/-  spider
/+  strandio, lca=lc-attestation
=,  strand=strand:strandio
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
=/  n  (strand ,(list (unit ?)))
^-  form:m
=+  !<([~ mode=?(%each %shared) peers=@ud from=@ud to=@ud] arg)
;<  our=@p  bind:m  get-our:strandio
=/  tars=(list walk-target:lca)
  %+  turn  (gulf 1 peers)
  |=  i=@ud
  =/  k=@ux  (shax (cat 3 'bench-liveness' i))
  [[k 0 0] [34 (can 3 ~[[32 k] [2 0x5120]])] from ~ our [from k]]
;<  t0=@da  bind:m  get-time:strandio
;<  lives=(list (unit ?))  bind:m
  ?:  ?=(%shared mode)
    (scan-liveness-many:lca our tars to %.y)
  =|  acc=(list (unit ?))
  |-  ^-  form:n
  ?~  tars  (pure:n (flop acc))
  ;<  l=(unit ?)  bind:n
    %:  scan-liveness:lca
        our  tip.i.tars  tip-spk.i.tars  tip-height.i.tars  to
        from.i.tars  who.i.tars  last.i.tars  %.y
    ==
  $(tars t.tars, acc [l acc])
;<  t1=@da  bind:m  get-time:strandio
%-  pure:m
!>  :*  mode=mode
        peers=peers
        blocks=+((sub to from))
        ms=(div (mul (sub t1 t0) 1.000) ~s1)
        clean=(lent (skim lives |=(l=(unit ?) =(l `%.y))))
    ==
