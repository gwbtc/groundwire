/-  spider
/+  strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=/  url=@t  (need !<((unit @t) arg))
=/  =request:http  [%'GET' url ~ ~]
=/  =task:iris  [%request request *outbound-config:iris]
=/  =card:agent:gall  [%pass /http-req %arvo %i task]
;<  ~  bind:m  (send-raw-card:strandio card)
;<  res=(pair wire sign-arvo)  bind:m  take-sign-arvo:strandio
?.  ?=([%iris %http-response %finished *] q.res)
  (strand-fail:strand %bad-sign ~)
~&  +.q.res
?~  full-file.client-response.q.res
  (strand-fail:strand %no-body ~)
(pure:m !>(`@t`q.data.u.full-file.client-response.q.res))
