/+  btcio, rpc=json-rpc, strandio
::
|%
+$  request-params
  [req-to:btcio (unit @t) wut=$%([%hax @ux] [%num @ud])]
--
|=  args=vase
=/  m  (strand:strandio ,vase)
^-  form:m
=/  req  (need !<((unit request-params) args))
;<  res=(unit block:btcio)  bind:m  (get-block:btcio req)
(pure:m !>(res))
::

