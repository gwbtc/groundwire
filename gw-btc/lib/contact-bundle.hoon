/-  *contact-bundle
/+  lsa=self-attestation
|%
::  collect only the target and its sponsor dependencies, sponsor first.
::  public is a locally known public boundary, not a trust assertion for import.
::  no chain fetch or peer installation is performed here.
::
++  collect
  |=  [target=@p resolve=$-(@p source)]
  ^-  result
  =/  who=@p  target
  =|  seen=(set @p)
  =|  attestations=(list [who=@p =pass])
  |-
  ?:  (~(has in seen) who)  [%failed who %cycle]
  =/  got  (resolve who)
  ?-    -.got
      %missing
    [%failed who %missing]
  ::
      %unverified
    [%failed who %unverified]
  ::
      %public
    [%bundle [%contact-bundle-1 target attestations `who]]
  ::
      %attestation
    =/  sat  (from-xtr:lsa who pass.got)
    ?~  sat  [%failed who %invalid]
    ?~  (spawn-of:lsa chain.u.sat)  [%failed who %invalid]
    =/  openings  (openings-of:lsa chain.u.sat)
    ?~  openings  [%failed who %invalid]
    =/  snap  snapshot.opening:(rear openings)
    =/  cic  (com:nu:cric:crypto pass.got)
    ?.  ?=(%c suite.+<.cic)  [%failed who %invalid]
    ?.  =(key.snap cry.pub.+<.cic)
      [%failed who %invalid]
    =.  attestations  [[who pass.got] attestations]
    ?~  sponsor.snap
      ?~  fief.snap  [%failed who %unroutable]
      [%bundle [%contact-bundle-1 target attestations ~]]
    $(who u.sponsor.snap, seen (~(put in seen) who))
  ==
--
