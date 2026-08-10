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
::  $publication: the FULL ATTESTATION PACKET, published on chain
::
::    A public comet reveals this in a deliberate OP_RETURN output
::    (kelvin 9).  It is not a second format and it gets no second
::    verification path: .pass is byte-for-byte the pass this comet
::    hands a peer over ames -- name, hiding dat commitment, and the
::    whole custody log in xtr -- and a watcher runs it through the
::    same +verify-lc / ++run-checks walk a mailed attestation gets.
::
::    .opening is the one hop the packet cannot contain.  A publication
::    rides the comet's own custody transaction, and that transaction's
::    txid does not exist until it is signed, so the log in xtr ends at
::    the satpoint this transaction SPENDS and .opening reveals the
::    state it commits.  The watcher is reading the block, so it knows
::    the two facts the publisher could not write down -- the txid and
::    the height -- and completes the log itself:
::
::        chain = (snoc <log from xtr>) [id.tx height `opening]
::
::    Riding the custody transaction is load-bearing twice over.  It is
::    what makes the publication the OWNER'S consent to declassify:
::    only the holder of the identity sat can build the transaction, so
::    a third party cannot publish somebody else's packet and strip
::    their confidentiality.  And it is what lets a comet publish LATE
::    -- the log proves custody from the spawn to here, so nothing has
::    to be inferred from the transaction in hand and a stranger walks
::    the same chain a peer would.
::
::    A spawn publication is the degenerate case, not a special one:
::    xtr is empty, and the completed log is the single entry whose
::    blind-opening opens the dat commitment.
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
::  $verdict-class: what a NEGATIVE outcome entitles us to do about it
::
::    Every path in %gw-btc that declines to install a point resolves to
::    exactly one of these, and the three are wildly different actions:
::
::      %fraud    jael %fail -> ames STICKY SNUB.  Reserved for evidence
::                that was never true.  A snub blocks the very packet that
::                would correct it, so it must be earned.
::      %stale    %stale-notice -> jael drops the point, ames demotes the
::                peer to a fresh alien.  Never a snub.
::      %unknown  zero cards.  We could not evaluate the question, so we
::                say nothing at all and look again next retransmission.
::
::    This is a CLOSED union on purpose: every consumer switches on it
::    with ?- , so a fourth outcome cannot be added without visiting each
::    decision point.
::
+$  verdict-class  ?(%fraud %stale %unknown)
::  $abort: why +verify-lc stopped before it could run ++run-checks
::
::    The light-client adapter has early returns that never reach the pure
::    verifier, and each one still has to produce a $result -- which means
::    each one still has to be CLASSIFIED.  They were free-form cords
::    ('empty-chain', 'derive-tip', ...) that belonged to no class, so
::    +classify's fall-through read them all as fraud and every one of
::    them snubbed the peer.  Naming them in a union makes
::    +abort-class:self-attestation a ?- that cannot compile until a new
::    reason has been assigned a class, and makes +fail-result refuse a
::    cord that is not one of them.
::
+$  abort  ?(%empty-chain %spawn-opening %derive-tip %tip-vout-range)
::  $refusal: why %gw-btc declined a verdict that PASSED every check
::
::    The second doorway onto the snub path.  ++run-checks can return
::    ok=%.y and the agent still refuse the point, because three of the
::    conditions are about OUR OWN state rather than the peer's evidence
::    (see +local-refusal in app/gw-btc.hoon).  A refusal used to fall
::    through to the same negative branch as fraud, so a peer with a
::    perfect attestation could be snubbed for something it neither sent
::    nor could observe.  Same treatment: a closed union, and a ?- that
::    forces a class onto every future member.
::
+$  refusal  ?(%who-mismatch %no-point %pass-mismatch %tip-owned)
::  $writ-drop: why a %jael-writ never became a verification
::
::    The THIRD doorway, and the one that had no witness at all.  $abort
::    and $refusal cover outcomes of a verification that RAN; these are
::    the dispositions %gw-btc reaches before it launches one, in
::    +on-poke.  Nine of them announced themselves (bf90840); four did
::    not, and from outside a silent drop is indistinguishable from a
::    wedged agent -- which is precisely the confusion test 6.7 exists to
::    catch.  Three of the four silent ones were not even drops: they
::    emitted a NEGATIVE verdict, i.e. a sticky ames snub, with no line.
::
::    Closed union, switched on with ?- by both +writ-drop-report (what
::    the operator is told) and +writ-drop-fate (what the kernel is
::    told), so a new disposition cannot be added silently and cannot
::    have its log line disagree with its cards.
::
::    Each case carries the numbers its own line needs, so the report is
::    a pure function of the drop and can be tested without the agent.
::
+$  writ-drop
  $%  [%in-flight ~]                ::  single-flight: one job per ship
      [%declined ~]                 ::  operator declined this sponsorship
      [%already-public ~]           ::  we already hold a public point
      [%onboarding ~]               ::  the public onboarding packet, no xtr
      [%foreign-kelvin ~]           ::  minted under a kelvin we cannot read
      [%undecodable ~]              ::  not a %gw-btc attestation at all
      [%empty-log ~]                ::  decodes, but its custody log is empty
      [%log-too-long len=@ud cap=@ud]
      [%no-tip ~]                   ::  no chain tip yet
      [%unsynced tip=@ud]           ::  light client not caught up
      [%tip-below-log tip=@ud need=@ud]
  ==
::  $writ-fate: what a $writ-drop does, to the kernel AND to the log
::
::    Four values because an operator has three questions -- did it
::    decline on purpose, could it not evaluate this yet, or is it
::    condemning somebody -- and the cards only answer the third.
::    %drop and %hold emit identical cards (none) and must NOT read
::    identically, because one of them clears by itself and the other
::    never will.
::
::    %drop     no cards, and nothing is pending: we declined on purpose
::              and the condition will not clear on its own.
::    %hold     no cards, but we could not evaluate it YET.  Our own
::              readiness, and the peer's next retransmission is retried.
::    %refresh  a %hold that also re-reads the light client's own sync
::              state (+refresh-synced) -- the held writ is itself the
::              poll, because /is-synced does not emit on recovery.
::    %fail     a NEGATIVE verdict: jael %fails and ames snubs, stickily,
::              which then blocks the packet that would correct it.  Three
::              drops do this and none of them used to say so.
::
::    +writ-drop-verb turns this into the word the log line leads with, so
::    the register an operator reads is derived from the same value as the
::    cards and cannot drift from them.
::
+$  writ-fate  ?(%drop %hold %refresh %fail)
::  $anew-refusal: why OUR OWN pass refresh never started
::
::    The %anew mirror of $writ-drop.  Seven of these were silent until
::    Phase 7.2 had to eliminate six of them from outside before it could
::    conclude the seventh was the real one; they are announced today, and
::    this union is what stops the tenth from being silent again.
::
::    None is a finding about anybody: every one is our own readiness or
::    our own bookkeeping, so none of them ever emits a card.
::
+$  anew-refusal
  $%  [%in-flight job=@ud]          ::  a self-validation is already running
      [%no-log ~]                   ::  nothing to validate
      [%log-too-long len=@ud cap=@ud]
      [%no-tip ~]
      [%unsynced tip=@ud]
      [%tip-below-log tip=@ud need=@ud]
      [%no-pass ~]                  ::  jael holds no suite-C pass for us
      [%encode-failed ~]            ::  +with-xtr could not rebuild it
      [%name-mismatch ~]            ::  the rebuilt pass is not ours
  ==
::  $strand-death: a verification that DIED instead of answering
::
::    Not a verdict and never evidence: the light client stopped
::    answering, a timeout fired, or spider was killed under us.  It has
::    always logged one line, and that line was the observed failure of
::    test 6.7 -- `%anew self-validation ended without a verdict', with no
::    reason, because +set-timeout:strandio fails with `[%timeout ~]' (an
::    EMPTY tang) and khan's mote was thrown away at the call site.  Say
::    which job died, what it was doing, and what an empty tang means.
::
+$  strand-death
  $%  [%peer who=@p job=@ud]        ::  verifying somebody else's attestation
      [%own job=@ud entries=@ud]    ::  validating our own custody log
  ==
::
::  On success, `sont.own` in point is the derived (and checked-unspent) tip.
+$  result
  $:  =verdict
      point=(unit point:urb)
      tip-value=@ud
  ==
--
