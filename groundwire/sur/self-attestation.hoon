::  sur/self-attestation.hoon
::
::  The suite-%c self-attestation carried by a %gw-btc comet.
::
::  The pass commits immutably to a starting (precommit) satpoint in `dat`:
::
::      (cat 0 (mat %gw-btc) (jam spawn-sont))
::
::  Its mutable `xtr` is the jam of `$custody-log`, oldest first.  Each entry
::  names a transaction that spends the current sat through input 0.  A reveal
::  describes the optional single-leaf Taproot commitment in the sat-carrying
::  output created by that transaction; `~` is an ordinary custody-only move.
::
/-  bitcoin, ord, urb
|%
+$  tapleaf  [version=@ux script=hexb:bitcoin]
::
+$  reveal
  $:  internal-key=@ux
      =tapleaf
  ==
::
+$  custody-entry
  $:  =txid:ord
      height=@ud
      reveal=(unit reveal)
  ==
::
+$  custody-log  (list custody-entry)
::
+$  self-attestation
  $:  who=@p
      spawn=sont:ord
      chain=custody-log
  ==
::
+$  anchor
  $:  point=point:urb
      tip=sont:ord
  ==
::
+$  check  [name=cord ok=?]
::
+$  verdict
  $:  who=@p
      ok=?
      checks=(list check)
  ==
::
::  On success, `sont.own` in point is the derived (and checked-unspent) tip.
+$  result
  $:  =verdict
      point=(unit point:urb)
      tip-value=@ud
  ==
--
