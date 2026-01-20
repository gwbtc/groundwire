/-  urb, spider
/+  strandio
^-  thread:spider
|=  args=vase
=/  m  (strand:strandio ,vase)
^-  form:m
=/  sed=@uw
  (need !<((unit @uw) args))
;<    =bowl:spider
    bind:m
  get-bowl:strandio
::
::  create wallet
;<    =vase
    bind:m
  ::  XX should really run /ted/btc/wallet but
  ::     +await-thread never returns for some reason
  ::  (await-thread:strandio %btc-wallet !>(`[1 sed]))
  ^-  form:m
  =/  keypair=_cric:crypto
    %:  pit:nu:cric:crypto
      512
      sed
      :*  %c
          (rap 3 ~[1 %btc %ord %gw %test])
          0  ::  extra data
      ==
    ==
  (pure:m !>(keypair))
 =/  wal  !<(_cric:crypto vase)
::
::  run %reg-tester
;<    ~
    bind:m
  %-  poke-our:strandio
  :*  %reg-tester
      %noun
      !>
      :*  sed
          ~
          :-  %batch
          :~  [%spawn pub:ex:wal [*@ux `0 0 0]]
              [%keys pub:ex:wal %.n]
  ==  ==  ==
(pure:m !>(~))
