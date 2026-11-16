/-  cb=contact-bundle, sa=self-attestation
/+  *test, bundle=contact-bundle, cc=gw-btc-pass
=>
|%
++  base
  |=  seed=@
  pub:ex:(pit:nu:cric:crypto 512 seed %c (make-dat:cc [0x1234 0 0]) 0)
++  identity
  |=  seed=@
  ^-  @p
  `@p`fig:ex:(com:nu:cric:crypto (base seed))
++  attestation
  |=  [seed=@ sponsor=(unit @p)]
  (attestation-route seed sponsor ~)
++  attestation-route
  |=  [seed=@ sponsor=(unit @p) route=(unit fief)]
  ^-  pass
  =/  cic  (com:nu:cric:crypto (base seed))
  ?>  ?=(%c suite.+<.cic)
  =/  opening=opening:sa
    [0x1 [1 0 cry.pub.+<.cic sponsor route] `[[0x1234 0 0] 100]]
  (need (with-xtr:cc (base seed) (jam `custody-log:sa`~[[0x5678 101 `opening]])))
++  collect
  |=  [who=@p sources=(map @p source:cb)]
  (collect:bundle who |=(peer=@p (~(gut by sources) peer [%missing ~])))
++  chain
  ^-  (map @p source:cb)
  (my ~[[(identity 1) [%attestation (attestation 1 `(identity 2))]] [(identity 2) [%attestation (attestation 2 `~zod)]] [~zod [%public ~]]])
--
|%
++  test-sponsor-first-public-boundary
  =/  got  (collect (identity 1) chain)
  ?>  ?=(%bundle -.got)
  ;:  weld
    (expect-eq !>(`@p`(identity 1)) !>(target.value.got))
    (expect-eq !>(`~zod) !>(public.value.got))
    (expect-eq !>(~[(identity 2) (identity 1)]) !>((turn attestations.value.got |=(p=[who=@p =pass] who.p))))
  ==
++  test-missing-sponsor
  (expect-eq !>(`result:cb`[%failed (identity 2) %missing]) !>((collect (identity 1) (~(del by chain) (identity 2)))))
++  test-unverified-sponsor
  (expect-eq !>(`result:cb`[%failed (identity 2) %unverified]) !>((collect (identity 1) (~(put by chain) (identity 2) [%unverified ~]))))
++  test-cycle
  =/  sources  (~(put by chain) (identity 2) [%attestation (attestation 2 `(identity 1))])
  (expect-eq !>(`result:cb`[%failed (identity 1) %cycle]) !>((collect (identity 1) sources)))
++  test-self-sponsor-cycle
  =/  sources  (~(put by chain) (identity 1) [%attestation (attestation 1 `(identity 1))])
  (expect-eq !>(`result:cb`[%failed (identity 1) %cycle]) !>((collect (identity 1) sources)))
++  test-wrong-identity
  =/  sources  (~(put by chain) (identity 1) [%attestation (attestation 2 `~zod)])
  (expect-eq !>(`result:cb`[%failed (identity 1) %invalid]) !>((collect (identity 1) sources)))
++  test-unroutable
  =/  sources  (~(put by chain) (identity 1) [%attestation (attestation 1 ~)])
  (expect-eq !>(`result:cb`[%failed (identity 1) %unroutable]) !>((collect (identity 1) sources)))
++  test-public-target
  (expect-eq !>(`result:cb`[%bundle [%contact-bundle-1 ~zod ~ `~zod]]) !>((collect ~zod chain)))
++  test-direct-fief-terminal
  =/  pas  (attestation-route 1 ~ `[%if .127.0.0.1 34.543])
  =/  sources  (~(put by chain) (identity 1) [%attestation pas])
  (expect-eq !>(`result:cb`[%bundle [%contact-bundle-1 (identity 1) ~[[(identity 1) pas]] ~]]) !>((collect (identity 1) sources)))
++  test-fief-does-not-skip-sponsor
  =/  sources  (~(put by chain) (identity 1) [%attestation (attestation-route 1 `~nec `[%if .127.0.0.1 34.543])])
  (expect-eq !>(`result:cb`[%failed ~nec %missing]) !>((collect (identity 1) sources)))
++  test-malformed-pass
  =/  sources  (~(put by chain) (identity 1) [%attestation 0])
  (expect-eq !>(`result:cb`[%failed (identity 1) %invalid]) !>((collect (identity 1) sources)))
++  test-large-pass-and-bundle
  ::  synthetic retained evidence exceeding both former byte caps.
  =/  cic  (com:nu:cric:crypto (base 1))
  ?>  ?=(%c suite.+<.cic)
  =/  opening=opening:sa
    [0x1 [(bex 8.388.608) 0 cry.pub.+<.cic `~zod ~] `[[0x1234 0 0] 100]]
  =/  pas  (need (with-xtr:cc (base 1) (jam `custody-log:sa`~[[0x5678 101 `opening]])))
  =/  got  (collect (identity 1) (~(put by chain) (identity 1) [%attestation pas]))
  ?>  ?=(%bundle -.got)
  ;:  weld
    (expect-eq !>(%.y) !>((gth (met 3 pas) 65.536)))
    (expect-eq !>(%.y) !>((gth (met 3 (jam value.got)) 1.048.576)))
    (expect-eq !>(`result:cb`[%bundle [%contact-bundle-1 (identity 1) ~[[(identity 1) pas]] `~zod]]) !>(`result:cb`got))
  ==
++  test-unrelated-pass-not-exported
  (expect-eq !>((collect (identity 1) chain)) !>((collect (identity 1) (~(put by chain) ~nec [%attestation 0]))))
++  test-long-chain
  =/  sources=(map @p source:cb)  (my ~[[~zod [%public ~]]])
  =/  seed=@ud  1
  |-
  ?:  =(seed 18)
    =/  got  (collect (identity 1) sources)
    ?>  ?=(%bundle -.got)
    ;:  weld
      (expect-eq !>(`~zod) !>(public.value.got))
      (expect-eq !>((turn (flop (gulf 1 17)) identity)) !>((turn attestations.value.got |=(p=[who=@p =pass] who.p))))
    ==
  %=  $
    sources  (~(put by sources) (identity seed) [%attestation (attestation seed ?:(=(seed 17) `~zod `(identity +(seed))))])
    seed  +(seed)
  ==
--
