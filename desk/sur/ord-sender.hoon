::  It might make sense to fold these types into
::  the types defined in sur/urb eventually. Keep
::  that in mind if we ever do a big collapsing refactor;
::  for now, do it all manual and explicit.
|%
+$  action
  [%fief ~]
  [%escape sponsee=@p]
  [%escape-request ~] :: XX request and response probably deserve their own marks
  [%escape-choose ~]
  [%escape-response ~] :: XX
==