/-  cb=contact-bundle
|_  value=bundle:cb
++  grow
  |%
  ++  noun  value
  ++  json
    ^-  ^json
    %-  pairs:enjs:format
    :~  ['type' [%s 'contact-bundle-1']]
        ['target' [%s (scot %p target.value)]]
        :-  'attestations'
        :-  %a
        %+  turn  attestations.value
        |=  entry=[who=@p =pass]
        %-  pairs:enjs:format
        :~  ['who' [%s (scot %p who.entry)]]
            ['pass' [%s (scot %uw pass.entry)]]
        ==
        ['public' ?~(public.value ~ [%s (scot %p u.public.value)])]
    ==
  ++  mime
    ^-  ^mime
    =/  text  (scot %uw (jam value))
    [/text/plain (met 3 text) text]
  --
++  grab
  |%
  ++  noun  bundle:cb
  ++  json
    |=  jon=^json
    ^-  bundle:cb
    ?>  ?=([%o *] jon)
    ?>  =('contact-bundle-1' (so:dejs:format (~(got by p.jon) 'type')))
    :*  %contact-bundle-1
        ((se:dejs:format %p) (~(got by p.jon) 'target'))
        %+  turn  ((ar:dejs:format same) (~(got by p.jon) 'attestations'))
        |=  entry=^json
        ^-  [who=@p =pass]
        ?>  ?=([%o *] entry)
        :-  ((se:dejs:format %p) (~(got by p.entry) 'who'))
        ((se:dejs:format %uw) (~(got by p.entry) 'pass'))
        ((mu:dejs:format (se:dejs:format %p)) (~(got by p.jon) 'public'))
    ==
  ++  mime
    |=  [type=mite bytes=octs]
    ^-  bundle:cb
    ?>  =(type /text/plain)
    ?>  =(p.bytes (met 3 q.bytes))
    ;;(bundle:cb (cue (slav %uw q.bytes)))
  --
++  grad  %noun
--
