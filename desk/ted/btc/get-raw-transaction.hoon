/+  btcio, rpc=json-rpc, strandio
::
|=  args=vase
=/  m  (strand:strandio ,vase)
^-  form:m
;<  res=response:rpc  bind:m
  %-  get-raw-transaction:btcio
  (need !<((unit [@t (unit @t) @ux ?]) args))
(pure:m !>(res))
