::  %keyfile-broadcast
::
::  A trivial keyfile relay / indexer. It accepts a confidential-comet keyfile
::  (a self-attestation packet) as a poke and broadcasts it to every subscriber
::  on /keyfiles as a %self-attestation fact -- exactly the shape an
::  %urb-watcher-2 expects from an indexer: it %watches /keyfiles (via its
::  %subscribe-for-keyfiles poke, with path /keyfiles) and RE-VERIFIES each
::  keyfile in full, so this relay is never trusted and needs no validation of
::  its own. Anyone may poke a keyfile in; a bad one is simply rejected by the
::  subscribers.
::
::    poke  %self-attestation | %add-keyfile  (a self-attestation:sa)
::      -> %give %fact /keyfiles %self-attestation
::    watch /keyfiles  -> caught up with every keyfile held, then live updates
::
::  The latest keyfile per ship is retained so a NEW subscriber is caught up on
::  subscribe. That is the only state; there is no Bitcoin logic here.
::
/-  sa=self-attestation
/+  dbug, default-agent, verb
|%
+$  card  card:agent:gall
+$  versioned-state  $%(state-0)
+$  state-0
  $:  %0
      ::  latest keyfile seen per ship, replayed to new subscribers
      keyfiles=(map @p self-attestation:sa)
  ==
--
%-  agent:dbug
^-  agent:gall
=|  state-0
=*  state  -
%+  verb  &
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init  `this
++  on-save  !>(state)
++  on-load
  |=  =vase
  ^-  (quip card _this)
  `this(state !<(versioned-state vase))
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?+    mark  (on-poke:def mark vase)
  ::  a keyfile to broadcast (from Causeway / a script / dojo / anywhere)
      ?(%self-attestation %add-keyfile)
    =/  sat  !<(self-attestation:sa vase)
    :_  this(keyfiles (~(put by keyfiles) who.sat sat))
    :~  [%give %fact ~[/keyfiles] %self-attestation !>(sat)]
    ==
  ==
::
++  on-watch
  |=  =(pole knot)
  ^-  (quip card _this)
  ?+    pole  (on-watch:def pole)
  ::  a subscriber (e.g. %urb-watcher-2) wants the keyfile feed: catch it up
  ::  with every keyfile we currently hold, then it receives new ones live.
      [%keyfiles ~]
    :_  this
    %+  turn  ~(val by keyfiles)
    |=  sat=self-attestation:sa
    ^-  card
    [%give %fact ~ %self-attestation !>(sat)]
  ==
::
++  on-agent  on-agent:def
++  on-arvo   on-arvo:def
++  on-leave  on-leave:def
++  on-fail   on-fail:def
++  on-peek   on-peek:def
--
