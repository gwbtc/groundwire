::  sur/self-attestation.hoon
::
::  The suite-%c self-attestation carried by a %gw-btc comet, in the
::  OP_RETURN revision (doc/opret-revision/01-spec-revision.md as amended
::  by 04-decisions-addendum.md).
::
::  The pass commits immutably -- and hidingly -- to its spawn satpoint:
::
::      dat = (can 0 (mat %gw-btc) (mat 9) [256 d] ~)
::      d   = H_tag("gw/spawn-commit", (jam spawn-sont) || blind)
::
::  Its mutable xtr is the jam of $custody-log, oldest first.  Each entry
::  names a transaction spending the current sat through input 0; the sat
::  lands at its ordinal-arithmetic offset in the new transaction.  An
::  $opening reveals the state committed at that hop: the canonical
::  unspendable OP_RETURN tapleaf over the jammed $snapshot (see
::  +state-leaf:gw-btc-pass), tweaked into internal-key, must recompute
::  exactly the sat-carrying output's P2TR key.  Entries without an
::  opening are plain custody moves.  Exactly one entry -- entry 0, the
::  spawn -- must additionally open the dat commitment via
::  $blind-opening.
::
::  All spends after the first hop must be key-path (the commitment leaf
::  is unspendable by construction); the first hop spends an arbitrary
::  wallet UTXO.
::
/-  bitcoin, ord, urb
|%
::  $snapshot: the complete per-identity state committed on-chain.
::
::    every snapshot change increments life, and a rift increment
::    implies a life increment, so life comparison totally orders a
::    ship's states.  sponsor and fief ride the snapshot (decisions
::    addendum section 2); a named sponsor must exist as a public point
::    at verification time, and an absent sponsor projects to
::    self-sponsorship in the jael udiff.  key is the messaging half
::    (cry.pub) of the suite-%c pass; the signing half fixing the @p is
::    immutable.
::
+$  snapshot
  $:  life=@ud
      rift=@ud
      key=@
      sponsor=(unit @p)
      fief=(unit fief)
  ==
::  $blind-opening: opens the pass's hiding dat commitment
::
+$  blind-opening  [spawn=sont:ord blind=@ux]
::  $opening: reveals the state committed at one custody hop
::
::    internal-key is the 33-byte compressed P2TR internal key (02/03
::    prefix); the verifier recomputes leaf, root, and output key itself
::    and never parses script bytes from the chain.
::
+$  opening
  $:  internal-key=@ux
      =snapshot
      blind-opening=(unit blind-opening)
  ==
::
+$  custody-entry
  $:  =txid:ord
      height=@ud
      opening=(unit opening)
  ==
::
+$  custody-log  (list custody-entry)
::
+$  self-attestation
  $:  who=@p
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
