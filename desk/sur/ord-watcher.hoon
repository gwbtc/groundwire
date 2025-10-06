/-  ord
/+  bc=bitcoin, btcio
|%
+$  state-0
  $+  state-0
  $:  %0
      ord-state=state:ord
      start=num:block:bc
      whos=(set ship)
      req-to=(unit req-to:btcio)
  ==
::
+$  action
  $+  ord-watcher-action
  ::  XX %start num should be a unit
  ::     if null, %start poke will run as-is
  ::     if not, this number will override it
  $%  [%start =num:block:bc]
      [%config-rpc =req-to:btcio]
  ==
+$  debug
  $+  ord-watcher-debug-action
  $%  [%clear-oc ~]
  ==
--
