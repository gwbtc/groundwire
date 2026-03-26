/-  *sponsor-signer
/+  dbug, default-agent, server, schooner
|%
+$  versioned-state
  $%  state-0
  ==
+$  state-0  [%0 ~]
+$  card  card:agent:gall
--
%-  agent:dbug
^-  agent:gall
=|  state-0
=*  state  -
|_  =bowl:gall
+*  this  .
    def  ~(. (default-agent this %.n) bowl)
++  on-init
  ^-  (quip card _this)
  :_  this
  :~  :*  %pass  /eyre/connect  %arvo  %e
          %connect  `/apps/sponsor-signer  %sponsor-signer
      ==
  ==
::
++  on-save
  ^-  vase
  !>(state)
::
++  on-load
  |=  old-state=vase
  ^-  (quip card _this)
  `this(state !<(versioned-state old-state))
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  |^
  ?+    mark  (on-poke:def mark vase)
      %handle-http-request
    =^  cards  state
      (handle-http !<([@ta =inbound-request:eyre] vase))
    [cards this]
  ==
  ++  handle-http
    |=  [eyre-id=@ta =inbound-request:eyre]
    ^-  (quip card _state)
    =/  ,request-line:server
      (parse-request-line:server url.request.inbound-request)
    =+  send=(cury response:schooner eyre-id)
    ::
    ?+    site  [(send [404 ~ [%plain "404 - Not Found"]]) state]
        [%apps %sponsor-signer %sign ~]
      ?+    method.request.inbound-request
        [(send [405 ~ [%plain "Method Not Allowed"]]) state]
      ::
          %'POST'
        ?~  body.request.inbound-request
          [(send [400 ~ [%plain "Missing request body"]]) state]
        =/  jon  (de:json:html q.u.body.request.inbound-request)
        ~&  jon
        ?~  jon
          [(send [400 ~ [%plain "Invalid JSON"]]) state]
        ?.  ?=([%o *] u.jon)
          [(send [400 ~ [%plain "Expected JSON object"]]) state]
        =/  ship-text=@t
          ((ot:dejs:format ~[ship+so:dejs:format]) u.jon)
        ~&  ship-text
        =/  sponsee=(unit @p)  (slaw %p ship-text)
        ~&  sponsee
        ?~  sponsee
          ~&  %fail
          [(send [400 ~ [%plain "Invalid ship name"]]) state]
        ::  Scry jael for our deed (pass) and ring (signing key)
        ~&  sponsee
        =/  deed=[=life =pass sec=(unit @)]
          .^([life pass (unit @)] %j /(scot %p our.bowl)/deed/(scot %da now.bowl)/(scot %p our.bowl)/1)
        ~&  deed
        =/  =ring
          .^(ring %j /(scot %p our.bowl)/vein/(scot %da now.bowl)/(scot %ud life.deed))
        ~&  ring
        =/  cac  (nol:nu:cric:crypto ring)
        ?>  ?=(^ sek.+<.cac)
        ~&  %cac
        ::  Get current block height from urb-watcher
        ?.  .^(? %gu /(scot %p our.bowl)/urb-watcher/(scot %da now.bowl)/$)
          [(send [503 ~ [%plain "urb-watcher not running"]]) state]
        =/  [* height=@ud]
          .^([@ @ud] %gx /(scot %p our.bowl)/urb-watcher/(scot %da now.bowl)/block-id/block-id)
        ::  Sign: (shaz (jam [sponsee height])) using raw ed25519
        =/  msg=octs  512^(shaz (jam [u.sponsee height]))
        =/  sig=@  (sign-octs-raw:ed:crypto msg [sgn.pub sgn.sek]:+<:cac)
        ::  Self-check: verify our own signature before sending
        =/  pub-cac  (com:nu:cric:crypto pass.deed)
        =/  self-check=?  (veri-octs:ed:crypto sig msg sgn:ded:ex:pub-cac)
        ?>  self-check
        ::  Build JSON response
        =/  resp=json
          %-  pairs:enjs:format
          :~  ['sig' [%s (scot %ux sig)]]
              ['height' (numb:enjs:format height)]
          ==
        :_  state
        (send [200 ~ [%json resp]])
      ==
    ==
  --
++  on-peek  on-peek:def
++  on-watch
  |=  =path
  ^-  (quip card _this)
  ?+    path  (on-watch:def path)
      [%http-response *]
    `this
  ==
::
++  on-leave  on-leave:def
++  on-agent  on-agent:def
++  on-arvo  on-arvo:def
++  on-fail  on-fail:def
--
