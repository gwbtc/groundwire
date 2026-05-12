/-  urb
/+  dbug, verb, default-agent
|%
+$  card  card:agent:gall
+$  versioned-state  $%(state-0)
::
+$  state-0
  $:  %0
      end=@t
      bucket=@t
      region=@t
      access-key=@t
      secret-key=@t
  ==
::
+$  credentials-update
  $:  access-key=(unit @t)
      secret-key=(unit @t)
  ==
::
+$  config-update
  $:  end=(unit @t)
      bucket=(unit @t)
      region=(unit @t)
  ==
--
::
%+  verb  |
%-  agent:dbug
=|  state-0
=*  state  -
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init
  ^-  (quip card _this)
  :_  this
  :~  :*  %pass   /urb-watcher/update/urb-state
          %agent  [our.bowl %urb-watcher]
          %watch  /urb-state
      ==
  ==
++  on-save  !>(state)
++  on-load
  |=  =vase
  ^-  (quip card _this)
  `this(state !<(state-0 vase))
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?+    mark  (on-poke:def mark vase)
      %set-config
    =/  upd=config-update  !<(config-update vase)
    :-  ~
    %=  this
      end     ?~(end.upd end.state u.end.upd)
      bucket  ?~(bucket.upd bucket.state u.bucket.upd)
      region  ?~(region.upd region.state u.region.upd)
    ==
  ::
      %set-credentials
    =/  upd=credentials-update  !<(credentials-update vase)
    :-  ~
    %=  this
      access-key  ?~(access-key.upd access-key.state u.access-key.upd)
      secret-key  ?~(secret-key.upd secret-key.state u.secret-key.upd)
    ==
  ==
++  on-peek   on-peek:def
++  on-watch  on-watch:def
++  on-arvo
  |=  [=(pole knot) =sign-arvo]
  ^-  (quip card _this)
  ?+    pole  (on-arvo:def pole sign-arvo)
      [%upload hash=@ta ~]
    ?+    sign-arvo  (on-arvo:def pole sign-arvo)
        [%iris %http-response *]
      =/  [%iris %http-response =client-response:iris]  sign-arvo
      ?+    -.client-response  `this
          %finished
        =/  hit=httr:eyre
          %-  to-httr:iris
          [response-header.client-response full-file.client-response]
        ?:  &((gte p.hit 200) (lth p.hit 300))
          %-  (slog :_(~ [%leaf "%urb-snapshot: uploaded snapshot {<(@uv (slav %uv hash.pole))>}"]))
          `this
        ?~  full-file.client-response
          %-  (slog :_(~ [%leaf "%urb-snapshot: upload failed ({<p.hit>}) {<(@uv (slav %uv hash.pole))>}"]))
          `this
        =/  body=@t  q.data.u.full-file.client-response
        %-  (slog :_(~ [%leaf "%urb-snapshot: upload failed ({<p.hit>}) {<body>}"]))
        `this
      ::
          %cancel
        %-  (slog :_(~ [%leaf "%urb-snapshot: failed to upload snapshot {<(@uv (slav %uv hash.pole))>}"]))
        `this
      ==
    ==
  ==
++  on-leave  on-leave:def
++  on-agent
  |=  [=(pole knot) =sign:agent:gall]
  ^-  (quip card _this)
  ?+    pole  (on-agent:def pole sign)
      [%urb-watcher %update %urb-state ~]
    ?+    -.sign  (on-agent:def pole sign)
        %fact
      ?+    p.cage.sign  (on-agent:def pole sign)
          %urb-state
        ?:  =('' end.state)
          `this
        =/  urb-state=state:urb  !<(state:urb q.cage.sign)
        ::  XX sign the jammed urb-state with our private key
        =/  jammed=@        (jam urb-state)
        =/  content-length  (met 3 jammed)
        =/  body=octs       [content-length jammed]
        =/  now=@da         now.bowl
        =/  dt              (yore now)
        =/  zero-pad
          |=  [wid=@ud n=@ud]
          ^-  tape
          =/  raw=tape  (a-co:co n)
          =/  pad=@ud  ?:((gte (lent raw) wid) 0 (sub wid (lent raw)))
          (weld (reap pad '0') raw)
        =/  host=@t
          (crip "{(trip bucket.state)}.{(trip region.state)}.{(trip end.state)}")
        =/  object-path=@t
          '/snapshot/urb-snapshot.jam'
        =/  full-url=@t
          (crip "https://{(trip host)}{(trip object-path)}")
        =/  weekday
          |=  d=@da
          ^-  tape
          =/  dow  (mod (div (sub d ~1970.1.1) ~d1) 7)
          =/  days=(list tape)
            ~["Thu" "Fri" "Sat" "Sun" "Mon" "Tue" "Wed"]
          (snag dow days)
        =/  month-name
          |=  m=@ud
          ^-  tape
          =/  months=(list tape)
            ~["Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"]
          (snag (dec m) months)
        =/  http-date=@t
          %-  crip
          ;:  welp
            (weekday now)
            ", "
            (zero-pad 2 d.t.dt)
            " "
            (month-name m.dt)
            " "
            (a-co:co y.dt)
            " "
            (zero-pad 2 h.t.dt)
            ":"
            (zero-pad 2 m.t.dt)
            ":"
            (zero-pad 2 s.t.dt)
            " GMT"
          ==
        =/  hmac-sha1-cord
          |=  [key=@t msg=@t]
          ^-  @
          %-  hmac-sha1l:hmac:crypto
          :-  [(met 3 key) (swp 3 key)]
          [(met 3 msg) (swp 3 msg)]
        =/  string-to-sign-v2=@t
          %-  crip
          ;:  welp
            "PUT\0a"
            "\0a"
            "application/x-urb-jam\0a"
            (trip http-date)
            "\0a"
            "x-amz-acl:public-read\0a"
            "/"
            (trip bucket.state)
            (trip object-path)
          ==
        =/  mac-v2=@  (hmac-sha1-cord secret-key.state string-to-sign-v2)
        =/  signature-v2=@t
          %-  en:base64:mimes:html
          [20 (swp 3 mac-v2)]
        =/  authorization-v2=@t
          %-  crip
          ;:  welp
            "AWS "
            (trip access-key.state)
            ":"
            (trip signature-v2)
          ==
        :_  this
        :~  :*  %pass  /upload/(scot %uv (sham urb-state))
                %arvo  %i
                %request
                :_  *outbound-config:iris
                :*  %'PUT'
                    full-url
                    :~  ['Date' http-date]
                        ['Content-Type' 'application/x-urb-jam']
                        ['x-amz-acl' 'public-read']
                        ['Authorization' authorization-v2]
                    ==
                    `body
                ==
            ==
        ==
      ==
    ==
  ==
++  on-fail   on-fail:def
--
