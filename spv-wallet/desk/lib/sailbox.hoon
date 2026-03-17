/+  server, multipart, html-utils, default-agent
/=  x-  /mar/fiber-ack
/=  x-  /mar/fiber-poke
/=  x-  /mar/fiber-bump
|%
++  numb :: adapted from numb:enjs:format
  |=  a=@u
  ^-  tape
  ?:  =(0 a)  "0"
  %-  flop
  |-  ^-  tape
  ?:(=(0 a) ~ [(add '0' (mod a 10)) $(a (div a 10))])
::
++  hexn :: adapted from numb:enjs:format
  |=  a=@u
  ^-  tape
  ?:  =(0 a)  "0"
  %-  flop
  |-  ^-  tape
  ?:  =(0 a)
    ~
  =+  m=(mod a 16)
  :_  $(a (div a 16))
  ?:  (lth m 10)
    (add '0' m)
  (add 'a' (sub m 10))
::
++  encode-request-line
  |=  lin=request-line:server
  ^-  @t
  =/  path=tape
    %-  zing
    %+  turn  site.lin
    |=(seg=@t (weld "/" (en-urlt:html (trip seg))))
  =/  url=@t  (crip path)
  =?  url  ?=(^ ext.lin)  (cat 3 url (cat 3 '.' u.ext.lin))
  ?~  args.lin  url
  =/  query=tape
    %+  roll  `(list [@t @t])`args.lin
    |=  [[key=@t val=@t] acc=tape]
    =/  sep=tape  ?~(acc "?" "&")
    %+  weld  acc
    %+  weld  sep
    %+  weld  (en-urlt:html (trip key))
    %+  weld  "="
    (en-urlt:html (trip val))
  (cat 3 url (crip query))
::
++  login-redirect
  |=  lin=request-line:server
  ^-  simple-payload:http
  =-  [[307 ['location' -]~] ~]
  %^  cat  3
    '/~/login?redirect='
  (encode-request-line lin)
::
+$  sse-connection
  $:  started=@da
      site=(list @ta)
      args=(list [key=@t value=@t])
  ==
::
++  render-tang-to-wall
  |=  [wid=@u tan=tang]
  ^-  wall
  (zing (turn tan |=(a=tank (wash 0^wid a))))
::
++  render-tang-to-marl
  |=  [wid=@u tan=tang]
  ^-  marl
  =/  raw=(list tape)  (zing (turn tan |=(a=tank (wash 0^wid a))))
  ::
  |-  ^-  marl
  ?~  raw  ~
  [;/(i.raw) ;br; $(raw t.raw)]
::
++  two-oh-four
  ^-  simple-payload:http
  [[204 ['content-type' 'application/json']~] ~]
::
++  internal-server-error
  |=  [authorized=? msg=tape t=tang]
  ^-  simple-payload:http
  =;  =manx
    :_  `(manx-to-octs:server manx)
    [500 ['content-type' 'text/html']~]
  ;html
    ;head
      ;title:"500 Internal Server Error"
    ==
    ;body
      ;h1:"Internal Server Error"
      ;p: {msg}
      ;*  ?:  authorized
            ;=
              ;code:"*{(render-tang-to-marl 80 t)}"
            ==
          ~
    ==
  ==
::
++  method-not-allowed
  |=  method=@t
  ^-  simple-payload:http
  =;  =manx
    :_  `(manx-to-octs:server manx)
    [405 ['content-type' 'text/html']~]
  ;html
    ;head
      ;title:"405 Method Not Allowed"
    ==
    ;body
      ;h1:"Method Not Allowed: {(trip method)}"
    ==
  ==
::
++  unsupported-browser
  ^-  simple-payload:http
  =;  =manx
    :_  `(manx-to-octs:server manx)
    [426 ['content-type' 'text/html']~]
  ;html
    ;head
      ;title:"426 Upgrade Required"
    ==
    ;body
      ;h1:"Modern Browser Required"
      ;p:"This application uses subdomain isolation for security."
      ;p:"Your browser must support Fetch Metadata (sec-fetch-mode and sec-fetch-site headers)."
      ;p:"Supported browsers:"
      ;ul
        ;li:"Brave 1.8+ (August 2019)"
        ;li:"Chrome/Edge 76+ (August 2019)"
        ;li:"Firefox 90+ (July 2021)"
        ;li:"Tor Browser 10.5+ (June 2021)"
        ;li:"Safari 16.4+ (March 2023)"
      ==
      ;p:"Please upgrade your browser to continue."
    ==
  ==
::
++  cross-origin-forbidden
  |=  [mode=(unit @t) site=(unit @t)]
  ^-  simple-payload:http
  =;  =manx
    :_  `(manx-to-octs:server manx)
    [403 ['content-type' 'text/html']~]
  ;html
    ;head
      ;title:"403 Forbidden"
    ==
    ;body
      ;h1:"Cross-Origin Request Blocked"
      ;p:"This application does not accept requests from other origins."
      ;*  ?~  mode  ~
          :_  ~
          ;p
            ; Sec-Fetch-Mode:
            ;code:"{(trip u.mode)}"
          ==
      ;*  ?~  site  ~
          :_  ~
          ;p
            ; Sec-Fetch-Site:
            ;code:"{(trip u.site)}"
          ==
    ==
  ==
::
++  is-sse-request
  |=  req=inbound-request:eyre
  ^-  ?
  ?&  ?=(%'GET' method.request.req)
      .=  [~ 'text/event-stream']
      (get-header:http 'accept' header-list.request.req)
  ==
::
+$  sse-key  [id=(unit @t) event=(unit @t)]
::
+$  sse-event
  $:  id=(unit @t)
      event=(unit @t)
      data=wain
  ==
::
+$  sse-manx
  $:  id=(unit @t)
      event=(unit @t)
      =manx
  ==
::
+$  sse-json
  $:  id=(unit @t)
      event=(unit @t)
      =json
  ==
::
++  sse-events
  =|  comments=wain
  =|  retry=(unit @ud)
  |=  events=(list sse-event)
  ^-  octs
  =|  response=wain
  =?  response  ?=(^ retry)
    (snoc response (cat 3 'retry: ' (crip (numb u.retry))))
  =.  response
    |-
    ?~  events
      (snoc response '')
    =?  response  ?=(^ id.i.events)
      (snoc response (cat 3 'id: ' u.id.i.events))
    =?  response  ?=(^ event.i.events)
      (snoc response (cat 3 'event: ' u.event.i.events))
    =.  response
      %+  weld  response
      ?~  data.i.events
        ~['data: ']
      %+  turn  data.i.events
      |=(=@t (cat 3 'data: ' t))
    $(events t.events)
  =.  response
    |-
    ?~  comments
      (snoc response '')
    =.  response  (snoc response (cat 3 ': ' i.comments))
    $(comments t.comments)
  (as-octs:mimes:html (of-wain:format response))
::
++  sse-last-id
  |=  req=inbound-request:eyre
  ^-  (unit @t)
  (get-header:http 'last-event-id' header-list.request.req)
::
++  sse-header
  ^-  response-header:http
  :-  200
  :~  ['content-type' 'text/event-stream']
      ['cache-control' 'no-cache']
      ['connection' 'keep-alive']
  ==
::
++  sse-keep-alive  `octs`(as-octs:mimes:html ':\0a\0a')
::
++  give-sse-event
  |=  [eyre-id=@ta =sse-event]
  ^-  card:agent:gall
  =/  data=octs  (sse-events ~[sse-event])
  (give-response-data eyre-id `data)
::
++  kick-eyre-sub
  |=  eyre-id=@ta
  ^-  card:agent:gall
  [%give %kick ~[/http-response/[eyre-id]] ~]
::
++  manx-to-wain
  |=  =manx
  ^-  wain
  (to-wain:format (crip (en-xml:html manx)))
::
++  give-sse-manx
  |=  [eyre-id=@ta id=(unit @t) event=(unit @t) =manx]
  ^-  card:agent:gall
  =/  =sse-event  [id event (manx-to-wain manx)]
  (give-sse-event eyre-id sse-event)
::
++  json-to-wain
  |=  =json
  ^-  wain
  [(en:json:html json)]~
::
++  give-sse-json
  |=  [eyre-id=@ta id=(unit @t) event=(unit @t) =json]
  ^-  card:agent:gall
  =/  =sse-event  [id event (json-to-wain json)]
  (give-sse-event eyre-id sse-event)
::
++  give-sse-header
  |=  eyre-id=@ta
  ^-  card:agent:gall
  (give-response-header eyre-id sse-header)
::
++  give-sse-keep-alive
  |=  eyre-id=@ta
  ^-  card:agent:gall
  (give-response-data eyre-id `sse-keep-alive)
::
++  give-response-header
  |=  [eyre-id=@ta =response-header:http]
  ^-  card:agent:gall
  :^  %give  %fact  ~[/http-response/[eyre-id]]
  http-response-header+!>(response-header)
::
++  give-response-data
  |=  [eyre-id=@ta data=(unit octs)]
  ^-  card:agent:gall
  [%give %fact ~[/http-response/[eyre-id]] http-response-data+!>(data)]
::
++  give-manx-response
  |=  [eyre-id=@ta =manx]
  ^-  (list card:agent:gall)
  %+  give-simple-payload:app:server
    eyre-id
  (manx-response:gen:server manx)
::
++  mime-response
  |=  =mime
  ^-  simple-payload:http
  :_  `q.mime
  :-  200
  :~  ['cache-control' 'no-cache']
      ['content-type' (rsh [3 1] (spat p.mime))]
  ==
::
++  give-mime-response
  |=  [eyre-id=@ta =mime]
  ^-  (list card:agent:gall)
  %+  give-simple-payload:app:server
    eyre-id
  (mime-response mime)
::
+$  byte-range
  $%  [%from-to start=@ud end=@ud]  :: bytes=0-1023
      [%from start=@ud]              :: bytes=1024-
      [%suffix length=@ud]           :: bytes=-500
  ==
::  Parse Range header: handles bytes=X-Y, bytes=X-, bytes=-Y
::  NOTE: This works but could probably be written more elegantly
::
++  parse-range-header
  |=  headers=header-list:http
  ^-  (unit byte-range)
  =/  range-value=(unit @t)  (get-header:http 'range' headers)
  ?~  range-value
    ~
  ::  Strip "bytes=" prefix
  =/  val=tape  (trip u.range-value)
  ?.  =((scag 6 val) "bytes=")
    ~
  =/  range-part=tape  (slag 6 val)
  ::  Find the dash
  =/  dash-pos=(unit @ud)
    |-  ^-  (unit @ud)
    =+  pos=0
    |-  ^-  (unit @ud)
    ?~  range-part  ~
    ?:  =(i.range-part '-')  `pos
    $(range-part t.range-part, pos +(pos))
  ?~  dash-pos
    ~
  ::  Split on dash
  =/  before=tape  (scag u.dash-pos range-part)
  =/  after=tape  (slag +(u.dash-pos) range-part)
  ::  Parse three cases
  ?:  =(before ~)
    ::  bytes=-500 (suffix)
    =/  len=(unit @ud)  (rush (crip after) dem)
    ?~  len
      ~
    [~ %suffix u.len]
  ?:  =(after ~)
    ::  bytes=1024- (from)
    =/  start=(unit @ud)  (rush (crip before) dem)
    ?~  start
      ~
    [~ %from u.start]
  ::  bytes=0-1023 (from-to)
  =/  start=(unit @ud)  (rush (crip before) dem)
  =/  end=(unit @ud)  (rush (crip after) dem)
  ?.  &(?=(^ start) ?=(^ end))
    ~
  [~ %from-to u.start u.end]
::
++  slice-mime
  |=  [range=byte-range =mime]
  ^-  [content-range=@t data=octs]
  =/  total-size=@ud  p.q.mime
  ::  Calculate actual start/end positions
  =/  [rng-start=@ud rng-end=@ud]
    ?-  -.range
        %from-to
      [start.range end.range]
      ::
        %from
      [start.range (dec total-size)]
      ::
        %suffix
      =/  suf-start=@ud
        ?:  (gte length.range total-size)  0
        (sub total-size length.range)
      [suf-start (dec total-size)]
    ==
  ::  Ensure end doesn't exceed file size
  =/  actual-end=@ud  (min rng-end (dec total-size))
  ::  Calculate slice length
  =/  slice-len=@ud  +((sub actual-end rng-start))
  ::  Extract bytes and wrap with as-octs
  =/  =octs  (as-octs:mimes:html (cut 3 [rng-start slice-len] q.q.mime))
  ::  Build Content-Range header using rap
  =/  content-range=@t
    %+  rap  3
    :~  'bytes '
        (crip (a-co:co rng-start))  '-'
        (crip (a-co:co actual-end))  '/'
        (crip (a-co:co total-size))
    ==
  [content-range octs]
::
++  range-response
  |=  [=header-list:http =mime]
  ^-  simple-payload:http
  =/  range=(unit byte-range)  (parse-range-header header-list)
  ?~  range
    ::  No Range header - return full file with 200
    :_  `q.mime
    :-  200
    :~  ['cache-control' 'no-cache']
        ['accept-ranges' 'bytes']
        ['content-type' (rsh [3 1] (spat p.mime))]
    ==
  ::  Range header present - always return 206 with Content-Range
  =/  [content-range=@t data=octs]  (slice-mime u.range mime)
  :_  `data
  :-  206
  :~  ['content-range' content-range]
      ['content-length' (crip (a-co:co p.data))]
      ['cache-control' 'no-cache']
      ['accept-ranges' 'bytes']
      ['content-type' (rsh [3 1] (spat p.mime))]
  ==
::
++  fiber
  |%
  +$  pipe
    $:  boar=(unit @ta)          :: who is hogging the pipe
        proc=(map @ta proc)
    ==
  ::
  +$  poke  [=cage src=ship sap=path]
  ::
  +$  proc
    $:  =process
        =poke                    :: keep initial poke
        next=(qeu (unit intake)) :: queue of held inputs
        skip=(qeu (unit intake)) :: queue of skipped inputs
    ==
  ::
  +$  intake
    $%  [%bump =cage] :: %fiber-bump
        [%arvo =wire sign=sign-arvo]
        [%agent =wire =sign:agent:gall]
        [%watch =path]
        [%leave =path]
    ==
  ::
  +$  input
    $:  pid=@ta          :: fiber id
        =poke            :: the original poke
        =bowl:gall       :: the current bowl
        state=vase       :: state for which we are responsible
        in=(unit intake) :: command/response/data to ingest (null means start)
    ==
  ::
  ++  output-raw
    |*  value=mold
    $~  [~ *vase %done *value]
    $:  cards=(list card) :: allows for %sse card
        state=vase
        $=  next
        $%  [%wait hold=?] :: process intake and optionally claim mutex (boar)
            [%skip hold=?] :: ignore intake and optionally claim mutex
            [%cont self=(form-raw value)] :: continue to next computation
            [%fail err=tang] :: return failure
            [%done =value]   :: return result
        ==
    ==
  ::
  ++  form-raw
    |*  value=mold
    $-(input (output-raw value))
  ::
  +$  process  _*form:(fiber ,~)
  :: 
  ++  fiber
    |*  value=mold
    |%
    ++  output  (output-raw value)
    ++  form    (form-raw value)
    :: give value; leave state unchanged
    ::
    ++  pure
      |=  =value
      ^-  form
      |=  input
      ^-  output
      [~ state %done value]
    ::
    ++  bind
      |*  b=mold
      |=  [m-b=(form-raw b) fun=$-(b form)]
      ^-  form
      |=  =input
      =/  b-res=(output-raw b)  (m-b input)
      ^-  output
      :-  cards.b-res
      :-  state.b-res
      ?-    -.next.b-res
        %wait  [%wait hold.next.b-res]
        %skip  [%skip hold.next.b-res]
        %cont  [%cont ..$(m-b self.next.b-res)]
        %fail  [%fail err.next.b-res]
        %done  [%cont (fun value.next.b-res)]
      ==
    --
  :: evaluation engine for the main state and continuation monad
  ::
  ++  eval
    |%
    ++  output  (output-raw ,~)
    ::
    +$  result
      $%  [%next hold=?]
          [%fail err=tang]
          [%done ~]
      ==
    ::
    ++  take
      =|  cards=(list card) :: effects
      |=  [pid=@ta =bowl:gall state=vase =proc]
      ^-  [(list card) vase _proc result]
      =^  take=(unit intake)  next.proc  ~(get to next.proc)
      |-  :: recursion point so take can be replaced
      =/  res=(each output tang)
        (mule |.((process.proc pid poke.proc bowl state take)))
      ?:  ?=(%| -.res)
        =/  =tang  [leaf+"crash" p.res]
        :-  cards :: no output cards on failure
        :-  state :: no output state on failure
        :-  proc  
        [%fail tang]
      =/  =output  p.res
      ?-    -.next.output
          %fail
        :-  cards :: no output cards on failure
        :-  state :: no output state on failure
        :-  proc
        [%fail err.next.output]
        ::
          %done
        :-  (weld cards cards.output)
        :-  state.output
        :-  proc
        [%done ~]
        ::
          %cont
        %=  $
          cards         (weld cards cards.output)
          state         state.output
          next.proc     (~(gas to next.proc) ~(tap to skip.proc))
          skip.proc     ~
          process.proc  self.next.output
          take          ~
        ==
        ::
          %wait
        =.  cards  (weld cards cards.output)
        ?.  =(~ next.proc)
          :: recurse on queued input
          ::
          =^  top  next.proc  ~(get to next.proc)
          %=  $
            take       top
            state      state.output
          ==
        :: await input
        ::
        :-  cards
        :-  state.output
        :-  proc
        [%next hold.next.output]
        ::
          %skip
        ?:  =(~ take)
          :: can't %skip a ~ input
          ::
          =/  =tang  [leaf+"cannot skip null input" ~]
          :-  cards :: no output cards on failure
          :-  state :: no output state on failure
          :-  proc
          [%fail tang]
        :: skip input
        ::
        =.  skip.proc  (~(put to skip.proc) take)
        ?.  =(~ next.proc)
          :: recurse on queued input
          ::
          =^  top  next.proc  ~(get to next.proc)
          $(take top)
        :-  cards :: %skips can't send effects
        :-  state :: %skips can't change state
        :-  proc
        [%next hold.next.output]
      ==
    --
  --
:: HTTP/SSE agent transformer library
::
::    Wraps a Gall agent to handle HTTP requests and SSE connections.
::    Similar to shoe for CLI apps, but for web apps.
::
++  keep-alive  ~s30
++  sse-timeout  ~m2
+$  parts  (list [@t part:multipart])
::  $card: standard gall cards plus SSE effects
::
+$  card
  $%  card:agent:gall
      [%sse site=(list @t) key=(unit sse-key)]
      [%simple-payload payload=simple-payload:http]
  ==
::  +sailbox: gall agent core with extra arms for HTTP/SSE
::
++  sailbox
  $_  ^|
  |%
  ++  on-peek
    |~  [bowl:gall state=vase path]
    *(unit (unit cage))
  ::
  ++  initial  *vase
  ++  migrate  |~(vase *vase)
  ::
  ++  process  *process:fiber
  ::  +make-sse-event: generate SSE event content for a site/event
  ::
  ++  make-sse-event
    |~  $:  =bowl:gall
            state=vase
            site=(list @t)
            args=(list [key=@t value=@t])
            id=(unit @t)
            event=(unit @t)
        ==
    *wain
  ::  +first-sse-event: initial event when SSE connection opens
  ::
  ++  first-sse-event
    |~  $:  site=(list @t)
            args=(list [key=@t value=@t])
            last-event-id=(unit @t)
        ==
    *(unit sse-key)
  --
::
++  grab-part
  =|  lead=(list [@t part:multipart])
  |=  [key=@t parts=(list [@t part:multipart])]
  ^-  [(unit part:multipart) (list [@t part:multipart])]
  ?~  parts
    [~ (flop lead)]
  ?:  =(key -.i.parts)
    [[~ +.i.parts] (weld (flop lead) t.parts)]
  $(parts t.parts, lead [i.parts lead])
::  +agent: creates wrapper core that handles HTTP/SSE and calls sailbox arms
::
++  agent
  |=  app=sailbox
  ^-  agent:gall
  =>
    |%
    ++  kv  kv:html-utils
    +$  state-0
      $:  %0
          state=vase
          =pipe:fiber
          timers=(map wire @da)
          connections=(map @ta sse-connection)
          requests=(set @ta)
      ==
    --
  =|  state-0
  =*  full-state  -
  ::
  =<
  ::
  |_  =bowl:gall
  +*  this  .
      def   ~(. (default-agent this %|) bowl)
      hc    ~(. +> bowl)
  ::
  ++  on-fail
    |=  [=term =tang]
    ^-  (quip card:agent:gall agent:gall)
    :: ?<  ?=([%eyre *] sap.bowl) :: Eyre Security (never happens)
    %-  (slog leaf+"error in {<dap.bowl>}" >term< tang)
    =^  cards  full-state
      abet:(handle-fiber-poke:hc %on-fail on-fail+!>([term tang]))
    [cards this]
  ::
  ++  on-init
    ^-  (quip card:agent:gall agent:gall)
    :: ?<  ?=([%eyre *] sap.bowl) :: Eyre Security (never happens)
    =.  state  initial:app
    =^  cards  full-state
      abet:(handle-fiber-poke:hc %on-init on-init+!>(~))
    =/  until=@da  (add now.bowl keep-alive)
    :_  this(timers (~(put by timers) /timer/sse until))
    %+  welp
      cards
    [%pass /timer/sse %arvo %b %wait until]~
  ::
  ++  on-save  !>(full-state)
  ::
  ++  on-load
    |=  old-state=vase
    ^-  (quip card:agent:gall agent:gall)
    :: ?<  ?=([%eyre *] sap.bowl) :: Eyre Security (never happens)
    =.  full-state  !<(state-0 old-state)
    =.  state  (migrate:app state)
    =^  cards  full-state
      abet:handle-on-load:hc
    [cards this]
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card:agent:gall agent:gall)
    ?.  ?=(%handle-http-request mark)
      :: ?<  ?=([%eyre *] sap.bowl) :: Eyre Security
      ?+    mark  (on-poke:def mark vase)
          %fiber-bump
        =/  [fid=@ta =page]  !<([@ta page] vase)
        =/  =dais:clay  .^(dais:clay %cb /(scot %p our.bowl)/[q.byk.bowl]/(scot %da now.bowl)/[p.page])
        =^  cards  full-state
          abet:(take-bump:hc [fid p.page (vale:dais q.page)])
        [cards this]
        ::
          %fiber-poke
        =/  [fid=@ta =page]  !<([@ta page] vase)
        =/  =dais:clay  .^(dais:clay %cb /(scot %p our.bowl)/[q.byk.bowl]/(scot %da now.bowl)/[p.page])
        =^  cards  full-state
          abet:(handle-fiber-poke:hc [fid p.page (vale:dais q.page)])
        [cards this]
        ::
          %fiber-kill
        =^  cards  full-state
          abet:(handle-fiber-kill:hc !<(@ta vase))
        [cards this]
      ==
    =+  !<([eyre-id=@ta req=inbound-request:eyre] vase)
    =/  lin=request-line:server  (parse-request-line:server url.request.req)
    ::
    ::  HTTP/1.1 requires Host header - reject if missing (RFC 7230)
    ::
    =/  host-header=(unit @t)  (get-header:http 'host' header-list.request.req)
    ?~  host-header
      ~&  >>>  %missing-host-header
      :_  this
      %+  give-simple-payload:app:server
        eyre-id
      [[400 ~] `(as-octs:mimes:html '400 Bad Request - Missing Host header')]
    =/  host=@t  u.host-header
    ::
    ~&  >>  host+host
    ~&  >>  origin+(get-header:http 'origin' header-list.request.req)
    ~&  >>  referer+(get-header:http 'referer' header-list.request.req)
    ~&  >>  accept+(get-header:http 'accept' header-list.request.req)
    ~&  >>  connection+(get-header:http 'connection' header-list.request.req)
    ~&  >>  last-event-id+(get-header:http 'last-event-id' header-list.request.req)
    ~&  >>  range+(get-header:http 'range' header-list.request.req)
    ~&  >  "received {(trip method.request.req)} request for {<site.lin>}!"
    :: :: Eyre Security: Check for modern browser with fetch metadata
    :: ::
    :: =/  fetch-mode=(unit @t)  (get-header:http 'sec-fetch-mode' header-list.request.req)
    :: =/  fetch-site=(unit @t)  (get-header:http 'sec-fetch-site' header-list.request.req)
    :: ~&  >>  sec-fetch-mode+fetch-mode
    :: ~&  >>  sec-fetch-site+fetch-site
    :: ::
    :: ::  If either header is missing, reject (old/unsupported browser)
    :: ::
    :: ?:  |(?=(~ fetch-mode) ?=(~ fetch-site))
    ::   ~&  >>>  %security-violation-unsupported-browser
    ::   :_  this
    ::   %+  give-simple-payload:app:server
    ::     eyre-id
    ::   unsupported-browser
    :: ::
    :: ::  Check security policy: allow navigate OR same-origin
    :: ::
    :: ?.  |(?=([~ %navigate] fetch-mode) ?=([~ %same-origin] fetch-site))
    ::   ~&  >>>  %security-violation-cross-origin
    ::   :_  this
    ::   %+  give-simple-payload:app:server
    ::     eyre-id
    ::   (cross-origin-forbidden fetch-mode fetch-site)
    ::
    ?.  (is-sse-request req)
      =.  requests  (~(put in requests) eyre-id)
      =^  cards  full-state
        =+  res=(mule |.(abet:(handle-fiber-poke:hc eyre-id handle-http-request+!>(req))))
        ?:  ?=(%& -.res)  p.res
        ((slog p.res) (mean p.res)) :: goes to dojo and browser
      [cards this]
    ~&  %sse-request
    =/  last-event-id=(unit @t)
      (get-header:http 'last-event-id' header-list.request.req)
    =/  first=(unit sse-key)
      (first-sse-event:app site.lin args.lin last-event-id)
    =/  cards=(list card:agent:gall)
      %+  welp
        ~[(give-sse-header eyre-id)]
      ?~  first
        ~
      :_  ~
      %+  give-sse-event
        eyre-id
      :+  id.u.first  event.u.first
      %:  make-sse-event:app
        bowl
        state
        site.lin
        args.lin
        u.first
      ==
    :-  cards
    %=    this
        connections
      %+  ~(put by connections)
        eyre-id
      [now.bowl [site args]:lin]
    ==
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card:agent:gall agent:gall)
    ?:  ?=([%http-response *] path)  [~ this]
    :: ?<  ?=([%eyre *] sap.bowl) :: Eyre Security
    ?+    path  (on-watch:def path)
        [%fiber-result @ ~]
      =/  =proc:fiber  (~(got by proc.pipe) i.t.path)
      ?>  =(src.bowl src.poke.proc)
      [~ this]
      ::
        [%fiber @ *]
      =^  cards  full-state
        abet:(take-watch:hc path)
      [cards this]
    ==
  ::
  ++  on-leave
    |=  =path
    ^-  (quip card:agent:gall agent:gall)
    :: ?<  ?=([%eyre *] sap.bowl) :: Eyre Security
    ?.  ?=([%fiber @ *] path)
      (on-leave:def path)
    =^  cards  full-state
      abet:(take-leave:hc path)
    [cards this]
  ::
  ++  on-peek
    |=  =path
    ^-  (unit (unit cage))
    :: ?<  ?=([%eyre *] sap.bowl) :: Eyre Security
    (on-peek:app bowl state path)
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card:agent:gall agent:gall)
    :: ?<  ?=([%eyre *] sap.bowl) :: Eyre Security
    ?.  ?=([%fiber @ *] wire)
      (on-agent:def wire sign)
    =^  cards  full-state
      abet:(take-agent:hc wire sign)
    [cards this]
  ::
  ++  on-arvo
    |=  [=wire =sign-arvo]
    ^-  (quip card:agent:gall agent:gall)
    :: Eyre Security: we can talk to %eyre and get responses
    ::
    ?+  wire  (on-arvo:def wire sign-arvo)
        [%fiber @ *]
      =^  cards  full-state
        abet:(take-arvo:hc wire sign-arvo)
      [cards this]
      ::
        [%timer %sse ~]
      ?+  sign-arvo  (on-arvo:def wire sign-arvo)
          [%behn %wake *]
        =|  cards=(list card:agent:gall)
        =/  conn=(list [eyre-id=@ta con=sse-connection])
          ~(tap by connections)
        |-
        ?~  conn
          =/  until=@da  (add now.bowl keep-alive)
          :_  this(timers (~(put by timers) /timer/sse until))
          :_(cards [%pass /timer/sse %arvo %b %wait until])
        =.  cards
          :_  cards
          ?:  (gte now.bowl (add started.con.i.conn sse-timeout))
            (kick-eyre-sub eyre-id.i.conn)
          (give-sse-keep-alive eyre-id.i.conn)
        =?  connections  (gte now.bowl (add started.con.i.conn sse-timeout))
          (~(del by connections) eyre-id.i.conn)
        $(conn t.conn)
      ==
    ==
  --
  ::
  =|  cards=(list card:agent:gall)
  |_  =bowl:gall
  +*  this  .
  ++  emit-card   |=(=card:agent:gall this(cards [card cards]))
  ++  emit-cards  |=(cadz=(list card:agent:gall) this(cards (welp (flop cadz) cards)))
  ++  abet        [(flop cards) full-state]
  ++  deal
    |=  [pid=@ta cards=(list card)]
    ^+  this
    ?~  cards
      this
    ?:  ?=(%simple-payload -.i.cards)
      ?.  (~(has in requests) pid)
        ~&  >>>  "ignoring simple-payload for pid not in requests: {(trip pid)}"
        $(cards t.cards)
      =.  requests  (~(del in requests) pid)
      =.  this  (emit-cards (give-simple-payload:app:server pid payload.i.cards))
      $(cards t.cards)
    ?.  ?=(%sse -.i.cards)
      =.  this  (emit-card i.cards)
      $(cards t.cards)
    =/  conns=(list [eyre-id=@ta con=sse-connection])
      ~(tap by connections)
    |-
    ?~  conns
      ^$(cards t.cards)
    ?.  =(site.i.cards site.con.i.conns)
      $(conns t.conns)
    ?~  key.i.cards
      =.  this  (emit-card (kick-eyre-sub eyre-id.i.conns))
      =.  connections  (~(del by connections) eyre-id.i.conns)
      $(conns t.conns)
    =.  this
      %-  emit-card
      %+  give-sse-event 
        eyre-id.i.conns
      :+  id.u.key.i.cards
        event.u.key.i.cards
      %:  make-sse-event:app
        bowl
        state
        site.con.i.conns
        args.con.i.conns
        u.key.i.cards
      ==
    $(conns t.conns)
  ::
  ++  make-bowl
    |=  pid=@ta
    ^-  bowl:gall
    =.  wex.bowl
      %-  ~(gas by *boat:gall)
      %+  murn  ~(tap by wex.bowl)
      |=  [[=wire =ship =term] acked=? pat=path]
      ?.  ?=([%base @ *] wire)
        ~
      =/  [f=@ta w=path]  (unwrap-wire wire)
      ?.  =(f pid)
        ~
      [~ [w ship term] acked pat]
    =.  sup.bowl
      %-  ~(gas by *bitt:gall)
      %+  murn  ~(tap by sup.bowl)
      |=  [=duct =ship pat=path]
      ?.  ?=([%base @ *] pat)
        ~
      =/  [f=@ta w=path]  (unwrap-wire pat)
      ?.  =(f pid)
        ~
      [~ duct ship w]
    bowl
  ::
  ++  handle-fiber-poke
    |=  [pid=@ta =cage]
    ^+  this
    ?<  (~(has by proc.pipe) pid)
    =/  =proc:fiber  [process:app [cage [src sap]:bowl] ~ ~]
    =.  proc.pipe  (~(put by proc.pipe) pid proc)
    (process-take pid ~)
  ::
  ++  handle-fiber-kill
    |=  pid=@ta
    ^+  this
    =.  proc.pipe  (~(del by proc.pipe) pid)
    (fiber-ack pid ~ leaf+"killed" ~)
  ::
  ++  fiber-kill-all
    ^+  this
    =/  fids=(list @ta)  ~(tap in ~(key by proc.pipe))
    |-
    ?~  fids
      this
    =.  this  (handle-fiber-kill i.fids)
    $(fids t.fids)
  ::
  ++  refresh-timers
    ^+  this
    %-  emit-cards
    %-  zing
    %+  turn  ~(tap by timers)
    |=  [=wire until=@da]
    :~  [%pass /timer/sse %arvo %b %rest until]
        [%pass /timer/sse %arvo %b %wait until]
    ==
  ::
  ++  handle-on-load
    ^+  this
    =.  this  fiber-kill-all
    =.  this  refresh-timers
    (handle-fiber-poke %on-load on-load+!>(~))
  ::
  ++  wrap-wire
    |=  [pid=@ta =wire]
    ^+  wire
    (weld /fiber/[pid] wire)
  ::
  ++  unwrap-wire
    |=  =wire
    ^-  [@ta ^wire]
    ?>  ?=([%fiber @ *] wire)
    [i.t.wire t.t.wire]
  ::
  ++  package-card
    |=  [pid=@ta =card]
    ^+  card
    ?+    card  card
        [%give ?(%fact %kick) *]
      =-  card(paths.p -)
      (turn paths.p.card |=(p=path (wrap-wire pid p)))
      ::
        [%pass * *]
      [%pass (wrap-wire pid p.card) q.card]
    ==
  ::
  ++  package-cards
    |=  [pid=@ta cards=(list card)]
    ^+  cards
    ?~  cards
      ~
    :-  (package-card pid i.cards)
    $(cards t.cards)
  ::
  ++  take-bump
    |=  [pid=@ta =cage]
    ^+  this
    (process-take pid ~ %bump cage)
  ::
  ++  take-arvo
    |=  [wir=wire sign=sign-arvo]
    ^+  this
    =/  [pid=@ta =wire]  (unwrap-wire wir)
    (process-take pid ~ %arvo wire sign)
  ::
  ++  take-agent
    |=  [wir=wire =sign:agent:gall]
    ^+  this
    =/  [pid=@ta =wire]  (unwrap-wire wir)
    (process-take pid ~ %agent wire sign)
  ::
  ++  take-watch
    |=  pat=path
    ^+  this
    =/  [pid=@ta =wire]  (unwrap-wire pat)
    (process-take pid ~ %watch wire)
  ::
  ++  take-leave
    |=  pat=path
    ^+  this
    =/  [pid=@ta =wire]  (unwrap-wire pat)
    (process-take pid ~ %leave wire)
  ::
  ++  claim
    |=  pid=@ta
    ^+  this
    ?>  |(=(~ boar.pipe) =([~ pid] boar.pipe))
    this(boar.pipe [~ pid])
  ::
  ++  relinquish
    ^+  this
    =.  boar.pipe  ~
    =/  fids=(list @ta)  ~(tap in ~(key by proc.pipe))
    |-
    ?~  fids
      this
    =.  this  (process-do-next i.fids)
    $(fids t.fids)
  ::
  ++  process-take
    |=  [pid=@ta take=(unit intake:fiber)]
    ^+  this
    ?>  (~(has by proc.pipe) pid)
    =/  =proc:fiber  (~(got by proc.pipe) pid)
    =.  next.proc  (~(put to next.proc) take)
    =.  proc.pipe  (~(put by proc.pipe) pid proc)
    (process-do-next pid)
  ::
  ++  process-do-next
    |=  pid=@ta
    ^+  this
    =/  =proc:fiber  (~(got by proc.pipe) pid)
    ?.  |(=(~ boar.pipe) =([~ pid] boar.pipe))
      this
    ?:  =(~ next.proc)
      this
    =+  ^-
      $:  cards=(list card)
          new=vase
          =proc:fiber
          =result:eval:fiber
      ==
      (take:eval:fiber pid (make-bowl pid) state proc)
    =.  proc.pipe  (~(put by proc.pipe) pid proc)
    =.  state  new
    =.  this  (deal pid (package-cards pid cards))
    ?:  ?=(%next -.result)
      ?:  hold.result
        (claim pid)
      relinquish
    =.  proc.pipe  (~(del by proc.pipe) pid)
    =.  this  relinquish
    =?  this  (~(has in requests) pid)
      =/  authorized=?  =(our.bowl src.poke.proc)
      =/  payload=simple-payload:http
        ?-  -.result
          %done  two-oh-four
          %fail  (internal-server-error authorized "fiber crashed" err.result)
        ==
      =.  requests  (~(del in requests) pid)
      (emit-cards (give-simple-payload:app:server pid payload))
    %+  fiber-ack  pid
    ?-  -.result
      %done  ~
        %fail
      %-  (slog leaf+"{(trip dap.bowl)}: crash at {(trip pid)}" err.result)
      [~ err.result]
    ==
  ::
  ++  fiber-ack
    |=  [pid=@ta ack=(unit tang)]
    ^+  this
    %-  emit-cards
    :~  [%give %fact ~[/fiber-result/[pid]] %fiber-ack !>(ack)]
        [%give %kick ~[/fiber-result/[pid]] ~]
    ==
  --
--
