::  tests/app/gevulot.hoon
::
::  %gevulot's on-chain verification of the peers it installed on trust is
::  started by the user, not by an install (state-3, 2026-10-28):
::
::    - an install arms no sweep timer unless .auto-verify is on;
::    - the pane's button (%recheck) records when it was pressed;
::    - the 2 -> 3 migration turns auto-verify off and remembers that
::      nothing was asked.
::
::  Driven through the formal gall interface, like tests/app/gw-btc.hoon.
::  The scries %gevulot makes on the way (+gw-ready and friends) are
::  whole-map %gw-btc paths that always answer on a ship running the desk.
::
/-  gev=gevulot, ord
/+  *test, cc=gw-btc-pass
/=  gevulot  /app/gevulot
=>
|%
++  bowl0
  ^-  bowl:agent:gall
  :*  [our=~zod src=~zod dap=%gevulot sap=/]
      [wex=~ sup=~ sky=~]
      [act=0 eny=`@uvJ`0xbeef now=~2000.1.1 byk=[~zod %base da+~2000.1.1]]
  ==
::  mirrors of app/gevulot's state-2 and state-3, to feed +on-load and to
::  read +on-save back.  Kept in step by hand; a drift fails the ;; below.
::
+$  state-2
  $:  %2
      receive=?
      serving=?
      share-index=?
      roster=(map @p pass)
      installed=(map @p peer:gev)
  ==
+$  state-3
  $:  %3
      receive=?
      serving=?
      share-index=?
      auto-verify=?
      asked=(unit @da)
      roster=(map @p pass)
      installed=(map @p peer:gev)
  ==
++  saved
  |=  agent=agent:gall
  ^-  state-3
  ;;(state-3 q:~(on-save agent bowl0))
::  a freshly installed %gevulot: +on-init has run, so every "default no"
::  flag is off (a bare loobean bunts to %.y).
::
++  fresh
  ^-  agent:gall
  =/  agent  gevulot
  =^  *  agent  ~(on-init agent bowl0)
  agent
::  a well-formed suite-C pass, so +fingerprint can name the peer a paste
::  installs.  It need not verify: nothing here reaches the chain.
::
++  probe-pass
  ^-  pass
  =/  seed  (shaz 'gevulot-probe')
  =/  dat
    %+  can  0
    :~  (mat domain:cc)
        (mat 1)
        (mat (jam `sont:ord`[txid=0xd0d0.d0d0.d0d0.d0d0 vout=0 off=0]))
    ==
  pub:ex:(pit:nu:cric:crypto 512 seed %c dat (jam ~))
::  a LOCAL action, as the pane or the dojo sends it (src is our)
::
++  act
  |=  [agent=agent:gall a=action:gev]
  ^-  [(list card:agent:gall) agent:gall]
  (~(on-poke agent bowl0) %noun !>(a))
::
++  has-timer
  |=  cards=(list card:agent:gall)
  ^-  ?
  %+  lien  cards
  |=  c=card:agent:gall
  ?=([%pass [%recheck ~] %arvo %b %wait *] c)
::
++  has-trusted-install
  |=  cards=(list card:agent:gall)
  ^-  ?
  %+  lien  cards
  |=  c=card:agent:gall
  ?=([%pass [%gw-install ~] %agent * %poke %noun *] c)
--
|%
::  2 -> 3: everything kept, auto-verify off, nothing asked yet.
::
++  test-load-2-to-3-is-manual
  =/  old=state-2  [%2 %.y %.n %.y ~ ~]
  =/  agent  gevulot
  =^  *  agent  (~(on-load agent bowl0) !>(old))
  =/  s  (saved agent)
  ;:  weld
    (expect-eq !>(%.n) !>(auto-verify.s))
    (expect-eq !>(~) !>(asked.s))
    (expect-eq !>([%.y %.n %.y]) !>([receive.s serving.s share-index.s]))
  ==
::  the default: an install goes to Jael on trust and arms no sweep.
::
++  test-install-on-trust-arms-no-timer
  =/  agent  fresh
  =^  cards  agent  (act agent [%ingest-peer probe-pass])
  ;:  weld
    (expect-eq !>(%.n) !>(auto-verify:(saved agent)))
    (expect-eq !>(%.y) !>((has-trusted-install cards)))
    (expect-eq !>(%.n) !>((has-timer cards)))
  ==
::  the old behaviour is one toggle away: with auto-verify on, the same
::  install arms the sweep.
::
++  test-auto-verify-arms-the-timer
  =/  agent  fresh
  =^  *  agent  (act agent [%set-auto-verify %.y])
  =^  cards  agent  (act agent [%ingest-peer probe-pass])
  ;:  weld
    (expect-eq !>(%.y) !>(auto-verify:(saved agent)))
    (expect-eq !>(%.y) !>((has-timer cards)))
  ==
::  the button records when it was pressed, even with nothing to verify.
::
++  test-recheck-records-when-asked
  =/  agent  fresh
  =^  *  agent  (act agent [%recheck ~])
  (expect-eq !>(`now:bowl0) !>(asked:(saved agent)))
--
