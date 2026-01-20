/-  spider
/+  strandio
^-  thread:spider
|=  args=vase
=/  m  (strand:strandio ,vase)
^-  form:m
=/  [lyf=@ud sed=@uw]
  (need !<((unit [@ud @uw]) args))
=/  keypair=_cric:crypto
  %:  pit:nu:cric:crypto
    512
    sed
    :*  %c
        (rap 3 ~[lyf %btc %ord %gw %test])
        0  ::  extra data
    ==
  ==
(pure:m !>(keypair))
