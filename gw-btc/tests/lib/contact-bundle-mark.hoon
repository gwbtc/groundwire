/-  cb=contact-bundle
/+  *test
/=  mark  /mar/contact-bundle-1
=>
|%
++  example
  ^-  bundle:cb
  [%contact-bundle-1 ~wes ~[[~wes (bex 512)]] `~zod]
--
|%
++  test-json-roundtrip
  (expect-eq !>(example) !>((json:~(grab mark example) json:~(grow mark example))))
++  test-json-null-boundary
  =/  value=bundle:cb  [%contact-bundle-1 ~wes ~[[~wes 123]] ~]
  (expect-eq !>(value) !>((json:~(grab mark value) json:~(grow mark value))))
++  test-mime-roundtrip
  (expect-eq !>(example) !>((mime:~(grab mark example) mime:~(grow mark example))))
++  test-mime-encoding
  =/  text  (scot %uw (jam example))
  (expect-eq !>(`mime`[/text/plain (met 3 text) text]) !>(mime:~(grow mark example)))
++  test-json-wrong-version
  =/  jon  json:~(grow mark example)
  ?>  ?=([%o *] jon)
  (expect-fail |.((json:~(grab mark example) [%o (~(put by p.jon) 'type' [%s 'contact-bundle-2'])])))
++  test-json-missing-field
  =/  jon  json:~(grow mark example)
  ?>  ?=([%o *] jon)
  (expect-fail |.((json:~(grab mark example) [%o (~(del by p.jon) 'target')])))
++  test-json-invalid-ship
  =/  jon  json:~(grow mark example)
  ?>  ?=([%o *] jon)
  (expect-fail |.((json:~(grab mark example) [%o (~(put by p.jon) 'target' [%s 'not-a-ship'])])))
++  test-mime-invalid-text
  (expect-fail |.((mime:~(grab mark example) [/text/plain 3 'bad'])))
++  test-mime-wrong-type
  (expect-fail |.((mime:~(grab mark example) [/application/json 3 'bad'])))
++  test-mime-wrong-length
  (expect-fail |.((mime:~(grab mark example) [/text/plain 0 (scot %uw (jam example))])))
++  test-mime-wrong-version
  =/  text  (scot %uw (jam [%contact-bundle-2 ~wes ~ ~]))
  (expect-fail |.((mime:~(grab mark example) [/text/plain (met 3 text) text])))
--
