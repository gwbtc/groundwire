/-  ord-sender
/+  ord-sender, gw=groundwire
/+  default-agent, dbug
|%
+$  versioned-state
  $%  state-0
  ==
+$  state-0  
  $:  %0
      ::  outpoint of our comet (one for now)
      ::  hot wallet keys (generate on-init)
      ::  incoming %escape requests
      ::  received %escape rejection
      sed=@ux              :: entropy that generates our wallet
      i=@ud                :: current BIP-32 derivation nonce
      =utxo:ord-sender     :: our one utxo that this wallet holds
      xtra=@ux             :: extra entropy?
      ~
  ==
+$  card  card:agent:gall
--
%-  agent:dbug
=|  state-0
=*  state  -
^-  agent:gall
::
=<
|_  =bowl:gall
+*  this  .
    def  ~(. (default-agent this %|) bowl)
    hc   ~(. +> [bowl ~])
::
++  on-init
  ^-  (quip card _this)
  =^  cards  state  abet:init:hc
  [cards this]
::
++  on-save
  ^-  vase
  !>(state)
::
++  on-load
  |=  =vase
  ^-  (quip card _this)
  [~ this(state !<(state-0 vase))]
::
++  on-poke
  |=  =cage
  ^-  (quip card _this)
  =^  cards  state  abet:(poke:hc cage)
  [cards this]
::
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  [~ ~]
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  [~ this]
::
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card _this)
  =^  cards  state  abet:(arvo:hc [wire sign-arvo])
  [cards this]
::
++  on-watch
  |=  =path
  ^-  (quip card _this)
  `this
::
++  on-fail   on-fail:def
++  on-leave  on-leave:def
--
::
|_  [=bowl:gall deck=(list card)]
+*  that  .
::
++  emit  |=(=card that(deck [card deck]))
++  emil  |=(lac=(list card) that(deck (welp lac deck)))
++  emol  |=  lac=(list card)  (emil (flop lac))
++  abet  ^-((quip card _state) [(flop deck) state])
::
::  Generate a Bitcoin wallet and 
::  submit a %spawn transaction.
++  init
  ^+  that
  :: Set initial entropy and BIP-32 nonce.
  ::  XX STARTING POINT FOR CORE SYNC:
  ::     When does the UTXO in walw get auto-updated by calling on of these arms,
  ::     and when doesn't it? 
  ::     What's the best way of preserving the wallet core in the state of this agent?
  =.  sed   eny.bowl
  =.  i     0
  =.  xtr   0xcafe.babe   :: XX  what is xtr?
  ::    utxo = [outpoint:gw output:gw]
  ::    $:  [=txid:ord =pos:ord] 
  ::        [script-pubkey=octs value=sats:ord] 
  ::        [spend-script=(unit script:scr) internal-keys=keypair]
  ::    ==
  =.  utxo  :: XX how? fetch via HTTP and then only %spawn once that's returned?
  =/  wal   (nu:wallet [sed 0 utxo])   :: build base wallet
  =/  walw  (nu:walt [xtr wal])       :: wrap in walt
  :: now construct the spawn output + tx
  ::  XX would it be nice if all these btc arms updated the utxo for the core?
  ::     or maybe just spend?
  =^  =output:gw  walw  (spawn:btc:walw output.utxo `0 0 0)  :: XX figure out output params
  =^  raw  walw  (spend:btc:walw out)
  ::  XX submit raw to RPC endpoint  
  ::  XX save new state (utxo?)
  ::  XX dumb question: can I just include a core to the state of this agent?
  ::     %ord-watcher uses the state of the core instead
  that
::
++  poke
  |=  [=mark =vase]
  ^+  that
  ?+    mark  !!
      %ord-sender-action
    =/  act  !<(action:ord-sender vase)
    (handle-action act)
  ==
::
++  handle-action
  |=  act=action:ord-sender
  ^+  that
  ?-    -.action
  ::
  ::  Change our routing information.
      %fief
    ?>  =(our.bowl src.bowl)
    ::  XX generate %fief transaction from poke and submit
    that
  ::
  ::  Create an %escape transaction and then send it
  ::  to our desired sponsor to sign it.
      %escape
    ?>  =(our.bowl src.bowl)
    ::  XX create a transaction
    ::  XX wipe current rejection from state
    %-  emit
    :*  %pass  /requests  %agent
        [sponsee.action %ord-sender]  %poke
        %ord-sender-action  !>([%escape-request ~])
    ==
  ::
  ::  Sponsees poke us here to request our signature
  ::  on their %escape transactions.
      %escape-request
    ::  XX add escape request to state
    that
  ::
  ::  Choose to either accept and sign an %escape
  ::  request, or to reject it.
      %escape-choose
    ::  XX if accept, then sign with networking key via crac, if reject, don't
    %-  emit
    :*  %pass  /requests  %agent
        [sponsee.action %ord-sender]  %poke
        %ord-sender-action  !>([%escape-response ~])
    ==
  ::
  ::  Sponsors poke us here with either a signed
  ::  transaction, or a rejection message.
      %escape-response
    ::  XX if signed transaction, submit
    ::  XX if rejection, add rejection to state and notify user
    that
  ==
--