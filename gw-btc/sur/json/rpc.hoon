::  json-rpc: protocol types
::
|%
+$  batch-request
  $+  json-rpc-batch-request
  $%  [%a p=(list request)]
      [%o p=request]
  ==
::
+$  request
  $+  json-rpc-request
  $:  id=@t
      jsonrpc=@t
      method=@t
      params=request-params
  ==
::
+$  request-params
  $+  json-rpc-request-params
  $%  [%list (list json)]
      [%map (map @t json)]
      [%object (list (pair @t json))]
  ==
+$  response
  $+  json-rpc-response
  $~  [%fail *httr:eyre]
  $%  [%result id=@t res=json]
      [%error id=@t code=@t message=@t]  ::TODO  data?
      [%fail hit=httr:eyre]
      [%batch bas=(list response)]
  ==
--
