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
::    start-height is transport metadata, not part of the commitment
::    preimage: it names the block containing the transaction that
::    CREATED the spawn satpoint, so the verifier's whole fetch path
::    stays height-based (the light client cannot look transactions up
::    by bare txid).
::
+$  blind-opening  [spawn=sont:ord start-height=@ud blind=@ux]
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
::  $ingest: the Causeway -> %gw-btc custody-log ingestion poke
::
::    Decisions addendum section 5: after the owner performs a custody
::    transaction (a rekey, or a plain sat move) through Causeway and it
::    confirms, Causeway hands the ship's OWN %gw-btc agent the new xtr
::    entry and its opening -- the same channel as proof ingestion.  The
::    agent validates the extended log against the chain through
::    %light-client, rebuilds our pass, and answers jael's %anew with
::    %anew-response.
::
::    The poke is NOT trusted: everything in the entry is re-checked
::    against the chain (see +start-anew in app/gw-btc.hoon), and a poke
::    that does not validate produces silence, never a refreshed pass.
::
::    It rides the %noun mark (`:gw-btc &noun [%gw-custody-entry ...]`)
::    so no mark file is needed on either side; the agent dispatches on
::    the head tag.  Companion type to $jael-poke in sur/urb.hoon.
::
+$  ingest
  $%  [%gw-custody-entry entry=custody-entry]
  ==
::
+$  self-attestation
  $:  who=@p
      =pass
      chain=custody-log
  ==
::  $publication: the payload a PUBLIC comet reveals in a deliberate
::  OP_RETURN output (kelvin 9).  Same evidence a confidential comet
::  hands a peer in an xtr entry, but published on-chain so scanners
::  can follow public identities with no packet exchange.  The pass
::  binds the name (who = fig(pass)); the opening reveals the state
::  committed in the transaction's sat-carrying output.  A present
::  blind-opening marks a spawn (it also opens the hiding dat
::  commitment); an absent one marks a state update (rekey/breach) of
::  an already-tracked comet.
::
+$  publication  [=pass =opening]
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
