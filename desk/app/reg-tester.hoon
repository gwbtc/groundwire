/-  spider, urb
/+  btcio, dbug, default-agent, strandio, verb
/=  unv-tests  /tests/unv
|%
+$  card  card:agent:gall
+$  state-0
  $:  rpc=req-to:btcio
  ==
--
%-  agent:dbug
^-  agent:gall
=|  state-0
=*  state  -
%+  verb  &
|_  =bowl:gall
+*  this   .
    def    ~(. (default-agent this %|) bowl)
::
++  on-init
  ^-  (quip card _this)
  :-  ~
  %=  this
    rpc  ['http://localhost:18443' [%basic 'bitcoinrpc:bitcoinrpc']]
  ==
::
++  on-save
  ^-  vase
  !>(state)
::
++  on-load
  |=  =vase
  ^-  (quip card _this)
  `this
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?>  =(our src):bowl
  ?+    mark  (on-poke:def mark vase)
      %noun
    ::
    ::  reg-tester runs a list of sots for one wallet per poke;
    ::  chain pokes together to test multiplayer scenarios
    =+  ^=  [sed utxo many]
    !<([@uw (unit utxo:unv-tests) many:skim-sotx:urb] vase)
    ?-    -.many
        %batch
      ::  XX why can't it find rpc.state??
      =*  rpc  ['http://localhost:18443' [%basic 'bitcoinrpc:bitcoinrpc']]
      ?>  ?=([%batch *] many)
      ?~  bat.many
        ~|  %empty-batch
        !!
      ?:  =(%spawn -.i.bat.many)
        :_  this
        :~  :*  %pass
                /res/batch/(scot %uw sed)/(scot %uv (jam many))
                %arvo
                %k
                %fard
                :*  %groundwire
                    %btc-spawn
                    [%noun !>(`[rpc sed i.bat.many])]
                ==
            ==
        ==
      ?~  utxo
        ~|  %need-utxo-to-start-with-non-spawn-sotx
        !!
      :_  this
      :~  :*  %pass
              /res/batch/(scot %uw sed)/(scot %uv (jam many))
              %arvo
              %k
              %fard
              :*  %groundwire
                  `term`(rap 3 ~[%btc- (head i.bat.many)])
                  [%noun !>(`[rpc sed utxo i.bat.many])]
              ==
          ==
      ==
    ::
        %spawn
      :_  this
      ::  XX why can't it find rpc.state??
      =*  rpc  ['http://localhost:18443' [%basic 'bitcoinrpc:bitcoinrpc']]
      :~  :*  %pass
              /res/spawn/(scot %uw sed)/(scot %uv (jam many))
              %arvo
              %k
              %fard
              [%groundwire %btc-spawn [%noun !>(`[rpc sed many])]]
          ==
      ==
    ::
        ?(%adopt %escape %fief %keys)
      ::  XX extract utxo from act
      ::  XX run relevant thread with utxo
      ~&  %not-implemented-yet
      `this
    ::
      ?(%cancel-escape %detach %reject %set-mang)
      ~|  %not-supported-by-wallet-core
      !!
    ::
    ==
  ==
::
++  on-watch
  |=  =(pole knot)
  ^-  (quip card _this)
  !!
::
++  on-peek
  |=  =(pole knot)
  ^-  (unit (unit cage))
  !!
::
++  on-arvo
  |=  [=(pole knot) =sign-arvo]
  ^-  (quip card _this)
  ?+    pole  (on-arvo:def pole sign-arvo)
      [%res tag=@tas sed=@uw hax=@uvF ~]
    ?+    sign-arvo  (on-arvo:def pole sign-arvo)
        [%khan %arow *]
      ?.  ?=(%& -.p.+.sign-arvo)
        ~&  >>>  %failed-to-process
        ~&  >>>  (many:skim-sotx:urb (cue `@uv`(slav %uv hax.pole)))
        ::  `this
        ((slog tang.p.p.+.sign-arvo) `this)
      ?>  ?=([%khan %arow %.y %noun *] sign-arvo)
      =/  [%khan %arow %.y %noun =vase]  sign-arvo
      =/  =utxo:walt:unv-tests
        !<(utxo:walt:unv-tests vase)
      ~&  >>  %utxo
      ~&  >>  utxo
      ?.  =(%batch tag.pole)
        `this
      =/  cued-many
        (many:skim-sotx:urb (cue `@uv`(slav %uv hax.pole)))
      ?>  ?=([%batch *] cued-many)
      =/  new-batch
        (oust [0 1] bat.cued-many)
      ?~  new-batch
        `this
      ::  XX why can't it find rpc.state??
      =*  rpc  ['http://localhost:18443' [%basic 'bitcoinrpc:bitcoinrpc']]
      :_  this
      :~  :*  %pass
              /res/batch/(scot %uw sed.pole)/(scot %uv (jam [%batch new-batch]))
              %arvo
              %k
              %fard
              ?:  =(%spawn -.i.new-batch)
                :*  %groundwire
                    %btc-spawn
                    [%noun !>(`[rpc sed.pole i.new-batch])]
                ==
              :*  %groundwire
                  `term`(rap 3 ~[%btc- (head i.new-batch)])
                  [%noun !>(`[rpc `@uw`(slav %uw sed.pole) utxo i.new-batch])]
              ==
          ==
      ==
    ==
  ==
::
++  on-leave  on-leave:def
++  on-agent  on-agent:def
++  on-fail   on-fail:def
--
