::  portable evidence, not an instruction to trust or install its contents.
|%
+$  bundle
  $:  %contact-bundle-1
      target=@p
      attestations=(list [who=@p =pass])
      public=(unit @p)
  ==
::
+$  failure
  $?  %missing
      %unverified
      %invalid
      %cycle
      %unroutable
  ==
::
+$  result
  $%  [%bundle value=bundle]
      [%failed who=@p reason=failure]
  ==
::
+$  source
  $%  [%public ~]
      [%attestation =pass]
      [%missing ~]
      [%unverified ~]
  ==
--
