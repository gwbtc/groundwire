/-  cb=contact-bundle, ci=contact-import, sa=self-attestation, urb, bitcoin
/+  *test, cc=gw-btc-pass
/=  gw-btc  /app/gw-btc
/=  request-mark  /mar/contact-import-1
/=  status-mark  /mar/contact-import-status-1
/=  reply-mark  /mar/contact-import-reply-1
=>
|%
++  bowl0
  ^-  bowl:agent:gall
  :*  [our=~zod src=~zod dap=%gw-btc sap=/]
      [wex=~ sup=~ sky=~]
      [act=0 eny=`@uvJ`0xbeef now=~2000.1.1 byk=[~zod %base da+~2000.1.1]]
  ==
++  base
  |=  seed=@
  pub:ex:(pit:nu:cric:crypto 512 seed %c (make-dat:cc [0x1234 0 0]) 0)
++  who
  |=  seed=@
  ^-  @p
  `@p`fig:ex:(com:nu:cric:crypto (base seed))
++  evidence
  |=  [seed=@ sponsor=(unit @p)]
  ^-  pass
  =/  cic  (com:nu:cric:crypto (base seed))
  ?>  ?=(%c suite.+<.cic)
  =/  op=opening:sa
    [0x1 [1 0 cry.pub.+<.cic sponsor ?~(sponsor `[%if .127.0.0.1 34.543] ~)] `[[0x1234 0 0] 100]]
  (need (with-xtr:cc (base seed) (jam `custody-log:sa`~[[0x5678 101 `op]])))
++  single
  ^-  bundle:cb
  [%contact-bundle-1 (who 1) ~[[(who 1) (evidence 1 ~)]] ~]
++  two
  ^-  bundle:cb
  [%contact-bundle-1 (who 2) ~[[(who 1) (evidence 1 ~)] [(who 2) (evidence 2 `(who 1))]] ~]
++  ready
  =/  agent  gw-btc
  ::  pre-import state remains loadable; no active peer verifications.
  =^  *  agent
    %-  ~(on-load agent bowl0)
    !>  :*  `state:urb`[[0xbeef 200] ~ ~ ~]
            %.y  `[0xbeef 200]  ~  ~  ~  1  ~  ~
            [`custody-log:sa`~ ~]  %.y
        ==
  agent
++  start
  |=  [agent=_gw-btc value=bundle:cb]
  (~(on-poke agent bowl0) %contact-import-1 !>(`request:ci`[0v7 value ~]))
++  status
  |=  agent=_gw-btc
  ^-  status:ci
  =/  got  (~(on-peek agent bowl0) /x/contact-import/v1)
  ?>  ?=([~ ~ *] got)
  (need !<((unit status:ci) q.u.u.got))
++  positive
  |=  [seed=@ sponsor=(unit @p)]
  ^-  sign-arvo
  :^  %khan  %arow  %.y
  :-  %noun
  !>  ^-  [result:sa hexb:bitcoin]
  :_  *hexb:bitcoin
  :*  [(who seed) %.y ~]
      :-  ~
          :*  [[seed 0 0] ~]
              [0 1 (evidence seed sponsor) ?~(sponsor [%.n (who seed)] [%.y u.sponsor]) ~ ~]
              `0xbeef
          ==
      1
  ==
++  negative
  |=  reason=cord
  ^-  sign-arvo
  :^  %khan  %arow  %.y
  :-  %noun
  !>(`[result:sa hexb:bitcoin]`[[[(who 1) %.n ~[[reason %.n]]] ~ 0] *hexb:bitcoin])
++  dangerous
  |=  cards=(list card:agent:gall)
  ^-  ?
  %+  lien  cards
  |=  c=card:agent:gall
  ?|  ?=([%give %fact * %stale-notice *] c)
      ?=([%give %fact * %verdict *] c)
  ==
--
=/  agent  gw-btc
|%
++  test-starts-first-sponsor
  =^  *  agent  (start ready two)
  (expect-eq !>(`phase:ci`[%verifying (who 1)]) !>(phase:(status agent)))
++  test-mark-roundtrips
  =/  request=request:ci  [0v7 single ~]
  =/  snapshot=(unit status:ci)  `[0v7 (who 1) 1 0 [%complete ~] ~]
  =/  response=reply:ci  [/context (need snapshot)]
  ;:  weld
    (expect-eq !>(request) !>((noun:~(grab request-mark request) noun:~(grow request-mark request))))
    (expect-eq !>(snapshot) !>((noun:~(grab status-mark snapshot) noun:~(grow status-mark snapshot))))
    (expect-eq !>(response) !>((noun:~(grab reply-mark response) noun:~(grow reply-mark response))))
  ==
++  test-imports-target-last
  =^  *  agent  (start ready two)
  =^  *  agent  (~(on-arvo agent bowl0) /contact-import-verify/1 (positive 1 ~))
  ;:  weld
    (expect-eq !>(1) !>(installed:(status agent)))
    (expect-eq !>(`phase:ci`[%verifying (who 2)]) !>(phase:(status agent)))
    =^  *  agent  (~(on-arvo agent bowl0) /contact-import-verify/2 (positive 2 `(who 1)))
    ;:  weld
      (expect-eq !>(2) !>(installed:(status agent)))
      (expect-eq !>(`phase:ci`[%complete ~]) !>(phase:(status agent)))
    ==
  ==
++  test-unknown-never-snubs
  =^  *  agent  (start ready single)
  =^  cards  agent  (~(on-arvo agent bowl0) /contact-import-verify/1 (negative 'sponsor-known'))
  ;:  weld
    (expect-eq !>(%.n) !>((dangerous cards)))
    (expect-eq !>(`phase:ci`[%unresolved (who 1) %unknown]) !>(phase:(status agent)))
  ==
++  test-invalid-never-snubs
  =^  *  agent  (start ready single)
  =^  cards  agent  (~(on-arvo agent bowl0) /contact-import-verify/1 (negative 'entry-0-commitment'))
  ;:  weld
    (expect-eq !>(%.n) !>((dangerous cards)))
    (expect-eq !>(`phase:ci`[%rejected (who 1) %invalid]) !>(phase:(status agent)))
  ==
++  test-stale-never-demotes
  =^  *  agent  (start ready single)
  =^  cards  agent  (~(on-arvo agent bowl0) /contact-import-verify/1 (negative 'tip-unspent'))
  ;:  weld
    (expect-eq !>(%.n) !>((dangerous cards)))
    (expect-eq !>(`phase:ci`[%unresolved (who 1) %stale]) !>(phase:(status agent)))
  ==
++  test-timeout-and-late-result
  =^  *  agent  (start ready single)
  =^  cards  agent  (~(on-arvo agent bowl0) /contact-import-timeout/1 [%behn %wake ~])
  =/  before  (status agent)
  =^  late  agent  (~(on-arvo agent bowl0) /contact-import-verify/1 (positive 1 ~))
  ;:  weld
    (expect-eq !>(`phase:ci`[%unresolved (who 1) %timeout]) !>(phase.before))
    (expect-eq !>(before) !>((status agent)))
    (expect-eq !>(%.n) !>((dangerous cards)))
    %+  expect-eq  !>(%.n)
    !>  %+  lien  late
        |=  c=card:agent:gall
        ?|  ?=([%pass *] c)
            ?=([%give %fact * %contact-import-status-1 *] c)
        ==
  ==
++  test-worker-crash
  =^  *  agent  (start ready single)
  =^  cards  agent
    (~(on-arvo agent bowl0) /contact-import-verify/1 [%khan %arow %.n %crash ~])
  ;:  weld
    (expect-eq !>(%.n) !>((dangerous cards)))
    (expect-eq !>(`phase:ci`[%unresolved (who 1) %verification-unavailable]) !>(phase:(status agent)))
  ==
++  test-busy-is-nacked
  =^  *  agent  (start ready single)
  (expect-fail |.((start agent two)))
++  test-retry-skips-installed-sponsor
  =^  *  agent  (start ready two)
  =^  *  agent  (~(on-arvo agent bowl0) /contact-import-verify/1 (positive 1 ~))
  =^  *  agent  (~(on-arvo agent bowl0) /contact-import-timeout/2 [%behn %wake ~])
  =^  *  agent  (start agent two)
  ;:  weld
    (expect-eq !>(1) !>(skipped:(status agent)))
    (expect-eq !>(`phase:ci`[%verifying (who 2)]) !>(phase:(status agent)))
    =^  *  agent  (~(on-arvo agent bowl0) /contact-import-timeout/2 [%behn %wake ~])
    (expect-eq !>(`phase:ci`[%verifying (who 2)]) !>(phase:(status agent)))
  ==
++  test-reload-retains-import
  =^  *  agent  (start ready single)
  =^  *  agent  (~(on-load agent bowl0) ~(on-save agent bowl0))
  =^  *  agent  (~(on-arvo agent bowl0) /contact-import-verify/1 (positive 1 ~))
  (expect-eq !>(`phase:ci`[%complete ~]) !>(phase:(status agent)))
++  test-version-17-migrates-explicitly
  =^  *  agent  (start ready single)
  =/  saved  !<([%18 core=* imports=state:ci] ~(on-save agent bowl0))
  ::  load the untagged version-17 core over an active version-18 import.
  =^  *  agent  (~(on-load agent bowl0) !>(core.saved))
  =/  migrated  !<([%18 core=* imports=state:ci] ~(on-save agent bowl0))
  ;:  weld
    (expect-eq !>(core.saved) !>(core.migrated))
    (expect-eq !>(*state:ci) !>(imports.migrated))
  ==
++  test-unknown-state-version-rejected
  (expect-fail |.((~(on-load ready bowl0) !>([%19 ~]))))
++  test-malformed-tagged-state-rejected
  (expect-fail |.((~(on-load ready bowl0) !>([%18 ~]))))
++  test-repeat-skips-installed-target
  =^  *  agent  (start ready single)
  =^  *  agent  (~(on-arvo agent bowl0) /contact-import-verify/1 (positive 1 ~))
  =^  cards  agent  (start agent single)
  ;:  weld
    (expect-eq !>(1) !>(skipped:(status agent)))
    (expect-eq !>(0) !>(installed:(status agent)))
    (expect-eq !>(`phase:ci`[%complete ~]) !>(phase:(status agent)))
    (expect-eq !>(%.n) !>((dangerous cards)))
  ==
++  test-mismatched-worker-identity
  =^  *  agent  (start ready single)
  =^  cards  agent  (~(on-arvo agent bowl0) /contact-import-verify/1 (positive 2 ~))
  ;:  weld
    (expect-eq !>(%.n) !>((dangerous cards)))
    (expect-eq !>(`phase:ci`[%unresolved (who 1) %verification-unavailable]) !>(phase:(status agent)))
  ==
++  test-public-boundary-must-be-known
  =^  cards  agent  (start ready [%contact-bundle-1 ~wes ~ `~wes])
  ;:  weld
    (expect-eq !>(%.n) !>((dangerous cards)))
    (expect-eq !>(`phase:ci`[%unresolved ~wes %public-boundary-unknown]) !>(phase:(status agent)))
  ==
++  test-malformed-bundle-rejected
  =^  cards  agent  (start ready [%contact-bundle-1 (who 1) ~[[(who 1) 0]] ~])
  ;:  weld
    (expect-eq !>(%.n) !>((dangerous cards)))
    (expect-eq !>(`phase:ci`[%rejected (who 1) %invalid]) !>(phase:(status agent)))
  ==
++  test-out-of-order-rejected
  =/  value  two
  =^  cards  agent  (start ready value(attestations (flop attestations.value)))
  ;:  weld
    (expect-eq !>(%.n) !>((dangerous cards)))
    (expect-eq !>(`phase:ci`[%rejected (who 2) %invalid]) !>(phase:(status agent)))
  ==
++  test-duplicate-rejected
  =/  value  single
  =^  cards  agent  (start ready value(attestations (weld attestations.value attestations.value)))
  (expect-eq !>(`phase:ci`[%rejected (who 1) %invalid]) !>(phase:(status agent)))
++  test-cycle-rejected
  =^  *  agent
    %+  start  ready
    [%contact-bundle-1 (who 1) ~[[(who 1) (evidence 1 `(who 2))] [(who 2) (evidence 2 `(who 1))]] ~]
  (expect-eq !>(`phase:ci`[%rejected (who 1) %cycle]) !>(phase:(status agent)))
++  test-missing-sponsor-rejected
  =^  *  agent
    (start ready [%contact-bundle-1 (who 1) ~[[(who 1) (evidence 1 `(who 2))]] ~])
  (expect-eq !>(`phase:ci`[%rejected (who 2) %missing]) !>(phase:(status agent)))
++  test-watch-and-callback
  =/  agent  ready
  =^  initial  agent  (~(on-watch agent bowl0) /contact-import/v1)
  =^  progress  agent
    (~(on-poke agent bowl0) %contact-import-1 !>(`request:ci`[0v7 single `[%caller /context]]))
  =^  finished  agent  (~(on-arvo agent bowl0) /contact-import-verify/1 (positive 1 ~))
  ;:  weld
    %+  expect-eq  !>(%.y)
    !>  %+  lien  initial
        |=  c=card:agent:gall
        =(c [%give %fact ~ %contact-import-status-1 !>(*(unit status:ci))])
    %+  expect-eq  !>(%.y)
    !>  %+  lien  progress
        |=  c=card:agent:gall
        ?=([%pass * %agent * %poke %contact-import-reply-1 *] c)
    %+  expect-eq  !>(%.y)
    !>  %+  lien  finished
        |=  c=card:agent:gall
        ?.  ?=([%pass * %agent * %poke %contact-import-reply-1 *] c)  %.n
        =/  [%pass =wire %agent destination=[@p @tas] %poke =mark =vase]  c
        =/  got  !<(reply:ci vase)
        &(=(/context reply-path.got) ?=(%complete -.phase.status.got))
  ==
++  test-unsynced-explicit
  ::  initialize readiness explicitly; the default mold's boolean is true.
  =^  *  agent  ~(on-init gw-btc bowl0)
  =^  *  agent  (start agent single)
  (expect-eq !>(`phase:ci`[%unresolved (who 1) %unsynced]) !>(phase:(status agent)))
++  test-local-only
  =/  remote  bowl0
  =.  src.remote  ~wes
  ;:  weld
    (expect-fail |.((~(on-poke ready remote) %contact-import-1 !>(`request:ci`[0v7 single ~]))))
    (expect-fail |.((~(on-peek ready remote) /x/contact-import/v1)))
    (expect-fail |.((~(on-watch ready remote) /contact-import/v1)))
  ==
--
