::  lib/self-attestation.hoon
::
::  Pure verification for the current %gw-btc pass-xtr custody log,
::  protocol kelvin 9: snapshot resolution, no event replay.  The
::  latest custody-proven $snapshot is authoritative; entries without
::  an opening are plain custody moves.  The verifier recomputes every
::  commitment itself and never parses script bytes from the chain.
::
::  Fetching is deliberately outside this library: ++run-checks
::  consumes the starting transaction and one fetched transaction per
::  xtr entry, which makes the trust boundary deterministic and
::  directly testable.
::
/-  bitcoin, ord, urb, sa=self-attestation
/+  bc=bitcoin, cc=gw-btc-pass, uc=urb-core
|%
::
::  +report: the operator-readable verdict, with each check CLASSIFIED
::
::    The headline names the OUTCOME, not just ok/not-ok, because the three
::    negative outcomes could not be more different: INVALID snubs the peer,
::    STALE demotes it to an alien, UNDETERMINED does nothing at all.
::    Phase 6.1 was diagnosable only because the per-check list was printed;
::    it was mis-read as fraud because nothing said which class the failing
::    check belonged to.
::
::      [ok]  passed
::      [XX]  failed, and it is FRAUD -- this is what causes a snub
::      [..]  failed, and it only means OUT OF DATE
::      [??]  failed because we could not evaluate it -- not evidence
::
++  report
  |=  =verdict:sa
  ^-  tang
  =/  outcome=tape
    ?:  ok.verdict  "VALID"
    ?-  (classify verdict)
      %unknown  "UNDETERMINED (no verdict emitted)"
      %stale    "STALE (out of date, not fraud)"
      %fraud    "INVALID"
    ==
  :-  leaf+"%gw-btc: attestation for {<who.verdict>} is {outcome}"
  %+  turn  checks.verdict
  |=  =check:sa
  ^-  tank
  ::  the printed marker is the SAME function that routes the outcome, so
  ::  the report and the cards can never disagree about a check's class.
  ::
  =/  mark=tape
    ?:  ok.check  "ok"
    ?-  (check-class name.check)
      %unknown  "??"
      %stale    ".."
      %fraud    "XX"
    ==
  leaf+"  [{mark}] {(trip name.check)}"
::
::  +stale-checks: the named checks that mean OUT OF DATE, not WRONG
::
::    Decisions addendum section 3: when a comet's identity sat has been
::    spent, its old attestation is stale -- "this is not fraud and MUST
::    NOT produce %fail (a snub would block the replacement packet)".
::    That rule was written for the SCANNER path, where %gw-btc watches a
::    tracked tip get spent and emits a %stale-notice.  But the very same
::    physical fact arrives the other way round at least as often: the
::    owner moves the sat, its ship keeps sending the pass it booted with,
::    and the spent tip turns up in a PACKET -- where it is proven spent
::    by OUR OWN filter scan against the chain, not by anything the peer
::    said.  Discovered by the inbox rather than by the scanner, it is
::    still the same fact, and it must take the same path.  (Live
::    mainnet, 2026-08-05: it did not, and a comet was snubbed by its own
::    sponsor for an honest state update -- permanently, because the snub
::    then blocked the refreshed attestation that would have fixed it.)
::
::    So these three names, and only these, classify a FAILED verdict as
::    staleness:
::
::      tip-unspent     our filter scan found the log's tip outpoint spent
::      tracked-tip     our own tracker holds this comet's sat at a place
::                      this log never reaches, i.e. we are ahead of it
::      life-monotonic  the log's latest life is below one we already hold,
::                      i.e. this is an older copy of a log we know
::
::    Everything else stays a %fail, because it is evidence that was never
::    true rather than evidence that has expired: spawn-commit (the log is
::    not bound to this name), entry-N-commitment (a snapshot never
::    committed on chain), entry-N-continuity / -key-path / -sat-landed /
::    -txid (a custody hop that did not happen), entry-N-life-order (a log
::    that contradicts itself), tracked-prefix (a log that is not an
::    extension of the one we already verified -- a fork, not an old
::    copy), pass-key, and every structural check.
::
::    sponsor-known used to be in that list and is NOT any more; see
::    +unknown-checks below.  It failed for exactly the reason this comment
::    disqualifies -- our own view of the chain is behind -- and produced
::    the maximally destructive outcome anyway.
::
++  stale-checks
  ^-  (set cord)
  (silt ~['tip-unspent' 'tracked-tip' 'life-monotonic'])
::
::  +unknown-checks: the named checks that mean WE CANNOT TELL
::
::    The third class, and the one Phase 6.1 proved was missing.  A check
::    that the verifier was unable to EVALUATE is not evidence of anything:
::    not of fraud, and not even of staleness.  It must produce no verdict
::    at all -- silence -- because a negative verdict is a Jael %fail and an
::    Ames snub, and a snub then blocks the very packet that would resolve
::    the ignorance.
::
::    Live mainnet 2026-08-06: C3 verified C1's attestation while its own
::    block scanner sat 20 blocks below C3's OWN publication.  43 of 44
::    checks passed; `sponsor-known` failed because C3 did not yet know that
::    C3 existed -- and C3 snubbed the honest comet it sponsors.  The same
::    attestation from the same peer verified VALID two hours later with
::    nothing changed but the scan position.
::
::      sponsor-known  the snapshot names a sponsor we cannot see as a
::                     public point.  Our public index is a WINDOW on the
::                     chain -- it starts at an operator-chosen height and
::                     ends at whatever block the scanner has reached -- so
::                     "not in it" never distinguishes `no such comet' from
::                     `we have not looked there yet'.  A verifier that
::                     cannot see the sponsor has no evidence of fraud, only
::                     ignorance.
::      tip-scanned    the BIP-158 liveness scan could not be evaluated: an
::                     unavailable filter or block, an inconsistent answer,
::                     or a degenerate (empty) scan range.  Distinguished
::                     from `tip-unspent', which fails ONLY when the scan
::                     positively PROVED the outpoint spent.
::
++  unknown-checks
  ^-  (set cord)
  (silt ~['sponsor-known' 'tip-scanned'])
::
::  +abort-of: is this check name one of +verify-lc's early aborts?
::
::    Derived from the $abort MOLD, never from a hand-written list of tags:
::    ;; normalizes .name and asserts the normalization changed nothing, so
::    a cord outside the union clams to some other tag, fails the assert,
::    and +mole gives ~.  Add a tag to $abort and this recognizes it with
::    no edit here -- which is the point, because a recognizer that had to
::    be maintained alongside the union is a list membership test again,
::    and would silently answer "not an abort" (i.e. fraud) for the one
::    reason nobody remembered to add.
::
++  abort-of
  |=  name=cord
  ^-  (unit abort:sa)
  (mole |.(;;(abort:sa name)))
::
::  +abort-class: what a +verify-lc early abort entitles us to do
::
::    THE arm this whole classification exists for.  +verify-lc returns
::    before ++run-checks in four places; each produced a one-check failing
::    verdict whose name was in neither class set, so +classify's
::    fall-through called every one of them fraud and %gw-btc snubbed.
::    Three of the four really are fraud.  One is not, and it is the one
::    that can only fire if WE are wrong.
::
::    This is a ?- over a closed union: a fifth abort reason does not
::    compile until somebody decides what it means.  That property, not
::    the four answers below, is the fix.
::
++  abort-class
  |=  =abort:sa
  ^-  verdict-class:sa
  ?-  abort
    ::  The peer's pass decodes to an EMPTY custody log.  Nothing was
    ::  fetched and nothing could have been: this is a judgement on the
    ::  peer's own xtr, reached before the first watch card.  A suite-%c
    ::  %gw-btc pass asserts a confidential identity, and an empty log
    ::  offers no evidence whatsoever for it.  ++run-checks calls the
    ::  identical condition `chain-nonempty' and treats it as fraud; the
    ::  two paths must agree, or failing EARLY becomes a way to escape a
    ::  check that failing late would have caught.
    ::
      %empty-chain  %fraud
    ::  Entry 0 carries no $blind-opening, so the log never binds itself
    ::  to the name the pass fingerprints to.  Again purely structural,
    ::  again computed from the peer's own xtr with no fetch involved, and
    ::  again ++run-checks' own `spawn-opening' check (same name, same
    ::  condition) is fraud-class.
    ::
      %spawn-opening  %fraud
    ::  +derive-tip could not walk the satpoint from the spawn to the tip.
    ::
    ::  This one deserves the argument, because it sits AFTER the fetches
    ::  and "a fetch went wrong" would be unknown, not fraud.  It cannot
    ::  be that here: every transaction handed to +derive-tip came through
    ::  +fetch-tx-at, which STRAND-FAILS (never returns) if the block-info
    ::  height disagrees, if the txid disagrees, or if the transaction is
    ::  unknown -- and a failed strand emits no verdict at all.  So by the
    ::  time +derive-tip runs, every transaction is confirmed on the main
    ::  chain at the height the peer claimed, under the txid the peer
    ::  claimed.  The evidence was fully obtained.
    ::
    ::  What is left for +derive-tip to reject is exactly the peer's claim
    ::  ABOUT that evidence: a vout that does not exist in the previous
    ::  transaction, a sat offset past the output's value, an input 0 that
    ::  does not spend the outpoint the log says it spends, or a hop that
    ::  drops the sat into fees.  ++run-checks names those same conditions
    ::  entry-N-prevout-range / -off-range / -continuity / -sat-landed and
    ::  calls every one of them fraud.  +derive-tip is only a pre-pass to
    ::  get the tip scriptPubKey for the liveness scan, so classing it
    ::  softer than the checks it duplicates would hand a forged log an
    ::  escape hatch: fail early, be forgiven.
    ::
    ::  (A byzantine light client that returned a well-formed but WRONG
    ::  transaction could make this fire for an honest peer.  That is true
    ::  of entry-N-continuity too, and of every fraud verdict this desk
    ::  emits -- the node is the trust root for all of them.  It is not a
    ::  reason to treat this one differently.)
    ::
      %derive-tip  %fraud
    ::  The derived tip names an output index outside the last fetched
    ::  transaction.  UNREACHABLE, and that is why it is unknown rather
    ::  than fraud: +derive-tip's last step sets vout from
    ::  +index-to-sont:urb-core, which only ever returns an index at which
    ::  an output actually exists, over the very list this bound re-checks.
    ::  So if this fires, the peer has not been caught at anything -- OUR
    ::  ordinal arithmetic has contradicted itself, and the honest report
    ::  is that we could not evaluate the attestation.  Snubbing a peer
    ::  over a bug in this desk is precisely the failure this file exists
    ::  to prevent.
    ::
      %tip-vout-range  %unknown
  ==
::
::  +refusal-class: what a LOCAL refusal of a passing verdict entitles us to
::
::    ++run-checks said ok, and %gw-btc still declined (see +local-refusal
::    in app/gw-btc.hoon).  None of these is a finding about the peer:
::    three are impossible unless this desk is internally inconsistent,
::    and the fourth is a disagreement between the peer's chain proof and
::    our own lagging index.  A peer that submitted a perfect attestation
::    must never be snubbed for any of them.
::
++  refusal-class
  |=  =refusal:sa
  ^-  verdict-class:sa
  ?-  refusal
    ::  The verdict names a different ship than the writ did.  The peer
    ::  never supplies who.verdict -- it is who.sat, which +pass-attestation
    ::  set from the ship in the writ after checking the pass fingerprints
    ::  to it.  A mismatch is our own inflight bookkeeping, not evidence.
    ::
      %who-mismatch  %unknown
    ::  ok=%.y with no point.  ++run-checks builds the point whenever ok
    ::  holds (ok implies `state-resolve', which implies a resolved
    ::  snapshot), so this is a contradiction inside the verifier itself.
    ::
      %no-point  %unknown
    ::  The rebuilt pass's key disagrees with the pass jael forwarded.
    ::  Those are the same pass: +pass-attestation builds the attestation
    ::  FROM the forwarded pass and ++run-checks copies it into the point.
    ::  Another internal contradiction, with nothing to attribute to the
    ::  peer.
    ::
      %pass-mismatch  %unknown
    ::  Our sat index already attributes the proven tip satpoint to a
    ::  DIFFERENT comet.  The only genuinely reachable refusal, and still
    ::  not fraud: two chain-valid logs cannot both end at one satpoint, so
    ::  a conflict means one of the two views is out of date -- and ours is
    ::  the windowed, forward-only scanner that is routinely behind, while
    ::  the peer's is a cryptographic proof against the chain.  We cannot
    ::  tell which, so we do neither thing: we refuse to overwrite the
    ::  other comet's sat AND we refuse to snub, and we look again on the
    ::  next retransmission.
    ::
      %tip-owned  %unknown
  ==
::
::  +writ-drop-fate: what a pre-verification disposition does
::
::    A ?- over the closed $writ-drop union, so a new disposition cannot
::    reach ANY branch -- least of all the destructive one -- until
::    somebody has decided what it costs the peer.  Same property, and
::    the same reason, as +abort-class above.
::
::    Paired with +writ-drop-verb and +writ-drop-report: the agent calls
::    all three from one place, so the cards jael receives, the register
::    the line is written in, and the reason it gives are all derived
::    from one value and cannot drift apart.
::
++  writ-drop-fate
  |=  drop=writ-drop:sa
  ^-  writ-fate:sa
  ?-    -.drop
    ::  Our own bookkeeping, all four.  None is a finding about the peer.
    ::  %drop rather than %hold because none of them clears by our own
    ::  chain view catching up: the block job resolves, the in-flight
    ::  verification finishes, an operator clears the refusal, a public
    ::  point makes the question moot.
    ::
      %publicizing     %drop
      %in-flight       %drop
      %declined        %drop
      %already-public  %drop
    ::  Two shapes we decline to JUDGE rather than judge negatively: the
    ::  public onboarding packet (resolved by the block scanner, and its
    ::  temporary absence from the index is not evidence) and a pass
    ::  minted under a FOREIGN protocol kelvin, which we cannot check and
    ::  must not blacklist -- snubbing there would partition the network
    ::  on every kelvin bump.
    ::
      %onboarding      %drop
      %foreign-kelvin  %drop
    ::  THE THREE THAT SNUB.  Each is a judgement on the peer's own pass,
    ::  reached with no fetch and no chain access at all: it does not
    ::  decode as a %gw-btc attestation, or it decodes to a log that
    ::  asserts a confidential identity while offering no evidence for it
    ::  (empty), or one so long that walking it is a denial of service.
    ::  Nothing about our readiness can change any of those answers, which
    ::  is exactly what separates them from the readiness holds below.
    ::
      %undecodable     %fail
      %empty-log       %fail
      %log-too-long    %fail
    ::  READINESS.  Infrastructure, never evidence -- see the gate in
    ::  +on-poke.  These emit the same cards as %drop (none) and MUST NOT
    ::  read the same: they clear on their own as our chain view catches
    ::  up, and telling an operator "dropped" for a condition that is
    ::  about to fix itself is how a healthy ship gets restarted.
    ::
    ::  %unsynced additionally re-reads the light client's own answer,
    ::  because /is-synced does not emit on recovery and a held writ is
    ::  the only poll we have (+refresh-synced).
    ::
      %no-tip          %hold
      %unsynced        %refresh
      %tip-below-log   %hold
  ==
::
::  +writ-drop-verb: the word a drop's log line leads with
::
::    The SEVERITY MARKER, and the direct analogue of +report's
::    [ok]/[XX]/[..]/[??]: it is computed from the fate, so what an
::    operator sees at a glance cannot disagree with what the kernel was
::    told.  A routine duplicate and a sticky snub must not look alike,
::    and "look alike" is a property of this word.
::
::      dropped   declined on purpose.  Nothing is pending; the condition
::                will not clear by itself.
::      held      could not evaluate it YET.  The peer retransmits and we
::                look again -- the "still working" reading.
::      REFUSED   a NEGATIVE verdict went out: jael %fails and ames snubs,
::                stickily, and the snub then blocks the packet that would
::                correct it.  Shouted, and its report carries the remedy.
::
++  writ-drop-verb
  |=  fate=writ-fate:sa
  ^-  tape
  ?-  fate
    %drop     "dropped"
    %hold     "held"
    %refresh  "held"
    %fail     "REFUSED"
  ==
::
::  +writ-drop-report: what an operator is told about a writ that stopped
::
::    Phase 6.7's finding, exactly: nine of these announced themselves and
::    four did not, and from outside a silent drop is indistinguishable
::    from an agent nobody is talking to -- or from one that is wedged.
::    Three of the silent four were not drops at all; they emitted a
::    STICKY SNUB without a word.
::
::    Being a ?- over the same closed union as +writ-drop-fate is the
::    fix, not the twelve strings: a thirteenth disposition does not
::    compile until it has a line.
::
::    Every headline is "writ from <ship> <verb>: <reason>", with the verb
::    from +writ-drop-verb, so the three questions an operator actually
::    has -- declined on purpose, could not evaluate, or condemning
::    somebody -- are answered by the first eight characters after the
::    ship's name.  The three that snub then spend two more lines on the
::    consequence and the undo, because nothing else in the log says a
::    snub happened.
::
++  writ-drop-report
  |=  [who=@p drop=writ-drop:sa]
  ^-  tang
  =/  verb=tape  (writ-drop-verb (writ-drop-fate drop))
  =/  hed=tape   "%gw-btc: writ from {(scow %p who)} {verb}: "
  ::  the two lines every REFUSED report ends with.  A snub is invisible
  ::  everywhere else -- it is discoverable only through .^(/snubbed) --
  ::  so the one place that causes it says how to see it and undo it.
  ::
  =/  snub=tang
    :~  leaf+"  (a NEGATIVE verdict: jael %fails and ames SNUBS, stickily, and the snub"
        leaf+"   then blocks the very packet that would correct it)"
        leaf+"  (inspect with .^(/snubbed) and undo with %snub %deny %del)"
    ==
  ?-    -.drop
      %publicizing
    ~[leaf+"{hed}a public-spawn replay is in progress"]
  ::
      %in-flight
    ~[leaf+"{hed}a verification is already in flight"]
  ::
      %declined
    ~[leaf+"{hed}sponsorship declined by operator"]
  ::
      %already-public
    ~[leaf+"{hed}already a public point"]
  ::
      %onboarding
    ::  ONE line, deliberately.  This is the routine disposition for a
    ::  publicly-onboarding comet and it fires once per retransmitted
    ::  writ until the block scanner indexes the spawn, so it is the
    ::  highest-frequency line in this arm by a wide margin.
    ::
    ~[leaf+"{hed}public onboarding packet (no xtr); the block scanner resolves it, so no verdict"]
  ::
      %foreign-kelvin
    :~  leaf+"{hed}pass minted under a FOREIGN protocol kelvin"
        leaf+"  (we cannot check it and must not blacklist it -- snubbing here would"
        leaf+"   make old and new ships mutually snub across a kelvin bump)"
    ==
  ::
      %undecodable
    %+  weld
      :~  leaf+"{hed}its pass is not a readable %gw-btc attestation"
          leaf+"  (not the onboarding shape and not a foreign kelvin either, so it is"
          leaf+"   malformed rather than merely unverifiable)"
      ==
    snub
  ::
      %empty-log
    %+  weld
      :~  leaf+"{hed}its pass decodes to an EMPTY custody log"
          leaf+"  (a suite-C pass asserts a confidential identity and this offers no"
          leaf+"   evidence whatsoever for it)"
      ==
    snub
  ::
      %log-too-long
    %+  weld
      :~  leaf+"{hed}custody log of {<len.drop>} entries is over the {<cap.drop>} cap"
          leaf+"  (walking it is a denial of service, and the cap is protocol rather"
          leaf+"   than readiness -- no amount of catching up changes this answer)"
      ==
    snub
  ::
      %no-tip
    ~[leaf+"{hed}no chain tip yet"]
  ::
      %unsynced
    ::  Two lines, and no more: this one fires on every held writ for the
    ::  whole of a light-client sync, which is hours on mainnet.
    ::
    :~  leaf+"{hed}light client NOT synced"
        leaf+"  (tip {<tip.drop>}; no verdict until it catches up, and this re-reads /is-synced)"
    ==
  ::
      %tip-below-log
    :~  leaf+"{hed}tip {<tip.drop>} below evidence height {<need.drop>}"
        leaf+"  (our chain view does not reach this log; that is ignorance, not fraud)"
    ==
  ==
::
::  +anew-refusal-report: why OUR OWN pass refresh never started
::
::    The %anew mirror of +writ-drop-report, and the same closed-union
::    discipline.  Phase 7.2 spent an hour eliminating six of these from
::    outside -- by re-deriving each precondition against the ship's own
::    libraries -- before it could conclude the seventh (a stranded
::    .pending slot) was the real one.  Every line names the ship,
::    because on a relay these interleave with peer verification.
::
::    None of these emits a card to anybody: they are all our own
::    readiness or our own bookkeeping.  A refused %anew leaves the
::    STORED log untouched, which is the safe direction -- a stale pass
::    would be installed in ames and rejected by every peer.
::
++  anew-refusal-report
  |=  [our=@p ref=anew-refusal:sa]
  ^-  tang
  =/  nom=tape  (scow %p our)
  ?-    -.ref
      %in-flight
    :~  leaf+"%gw-btc: %anew for {nom} refused: a self-validation is already in flight (job {<job.ref>})"
        leaf+"  (single-flight; /x/pending-own shows the slot, and it is released"
        leaf+"   either by the thread's answer or by the ~h2 leak guard)"
    ==
  ::
      %no-log
    :~  leaf+"%gw-btc: %anew for {nom} refused: no custody log to validate"
        leaf+"  (neither a stored log nor an xtr in the pass jael holds; a comet"
        leaf+"   booted from a plain feed needs a %gw-custody-entry first)"
    ==
  ::
      %log-too-long
    ~[leaf+"%gw-btc: %anew for {nom} refused: custody log too long ({<len.ref>} entries, cap {<cap.ref>})"]
  ::
      %no-tip
    ~[leaf+"%gw-btc: %anew for {nom} refused: no chain tip yet"]
  ::
      %unsynced
    :~  leaf+"%gw-btc: %anew for {nom} refused: light client not synced (tip {<tip.ref>})"
        leaf+"  (re-reading /is-synced; re-poke once the node reports it is caught up)"
    ==
  ::
      %tip-below-log
    ~[leaf+"%gw-btc: %anew for {nom} refused: tip {<tip.ref>} below evidence height {<need.ref>}"]
  ::
      %no-pass
    :~  leaf+"%gw-btc: %anew for {nom} refused: jael has no suite-C pass for us"
        leaf+"  (not a confidential comet, or the deed/vault endpoint answered with"
        leaf+"   another suite -- this ship cannot serve a %gw-btc attestation)"
    ==
  ::
      %encode-failed
    ~[leaf+"%gw-btc: %anew for {nom} refused: +with-xtr could not re-encode our pass"]
  ::
      %name-mismatch
    :~  leaf+"%gw-btc: %anew for {nom} refused: re-encoded pass does not hash to our own name"
        leaf+"  (BUG: +with-xtr has drifted from the kernel's pass encoder.  Publishing"
        leaf+"   it would install a pass that is not ours, so nothing is emitted)"
    ==
  ==
::
::  +anew-refusal-fate: does a refused %anew re-poll the light client?
::
::    Only the readiness one does, for the same reason +writ-drop-fate
::    answers %refresh: /is-synced announces losing its last peer but
::    nothing announces the recovery, so a refused %anew is the poll.
::
++  anew-refusal-fate
  |=  ref=anew-refusal:sa
  ^-  ?(%drop %refresh)
  ?-  -.ref
    %unsynced       %refresh
    %in-flight      %drop
    %no-log         %drop
    %log-too-long   %drop
    %no-tip         %drop
    %tip-below-log  %drop
    %no-pass        %drop
    %encode-failed  %drop
    %name-mismatch  %drop
  ==
::
::  +strand-death-report: a verification that DIED instead of answering
::
::    THE observed failure of test 6.7.  This path has always logged, and
::    what it logged was `%anew self-validation ended without a verdict'
::    with nothing after it -- which the clean-room run hit twice, from
::    two unrelated causes, and could not tell apart.
::
::    It was reasonless for a structural reason worth writing down:
::
::      - +set-timeout:strandio fails with `[%fail %timeout ~]', an EMPTY
::        tang, so the most common death by far carries no text at all;
::      - khan's mote (spider's `term') was dropped at the call site,
::        which threw away the one word that WAS there.
::
::    So: name the job, say what it was doing, print the mote, and when
::    the tang is empty say so explicitly along with the two causes that
::    produce an empty one.  A verification that died is infrastructure
::    failure and NEVER evidence -- no card is emitted on either path,
::    and the peer is not snubbed for our light client going quiet.
::
++  strand-death-report
  |=  [=strand-death:sa mote=term err=tang]
  ^-  tang
  =/  head=tang
    ?-  -.strand-death
        %peer
      :~  leaf+"%gw-btc: verification of {(scow %p who.strand-death)} DIED (job {<job.strand-death>}); no verdict, and no snub"
          leaf+"  (the peer is untouched: a dead strand is our infrastructure failing,"
          leaf+"   never evidence.  Its next retransmission gets a fresh slot.)"
      ==
    ::
        %own
      :~  leaf+"%gw-btc: our own %anew self-validation DIED (job {<job.strand-death>}, {<entries.strand-death>} entries)"
          leaf+"  (the stored custody log is UNCHANGED and no pass was published;"
          leaf+"   re-poke %gw-custody-entry or %jael-anew to try again)"
      ==
    ==
  =/  why=tang
    ?^  err  [leaf+"  khan reported {<mote>}:" err]
    :~  leaf+"  khan reported {<mote>} and NO reason at all.  An empty tang means either:"
        leaf+"    1. a light-client request never answered and the ~m5 +lc-fetch-timeout"
        leaf+"       fired (+set-timeout fails with an empty tang).  Check the node's"
        leaf+"       /is-synced and whether it still has live peers."
        leaf+"    2. spider was killed under us -- any bail in an unrelated thread, or a"
        leaf+"       scry jael declines, tears down EVERY strand at once."
    ==
  (weld head why)
::
::  +check-class: the class of ONE failing check, by name
::
::    The single place a check name becomes an outcome.  +report's marker
::    and the agent's card both come through here, so the operator can
::    never be shown [XX] for something that produced silence.
::
++  check-class
  |=  name=cord
  ^-  verdict-class:sa
  ?^  ab=(abort-of name)  (abort-class u.ab)
  ?:  (~(has in unknown-checks) name)  %unknown
  ?:  (~(has in stale-checks) name)    %stale
  %fraud
::
::  +classify: the class of a whole verdict
::
::    Fraud beats everything: a peer does not get to launder bad evidence
::    by also being out of date or unknowable.  Unknown then beats stale:
::    %stale is a real finding about the peer's evidence, and we are not
::    entitled to make it while some of our own machinery came back blank.
::
::    A verdict that PASSED has no failing checks to classify.  It should
::    never reach here (the caller installs the point instead), and if it
::    does, %unknown is the reading that does nothing.
::
++  classify
  |=  =verdict:sa
  ^-  verdict-class:sa
  ?:  ok.verdict  %unknown
  ::  NB: =(~ bad) rather than ?~, which would fish-narrow .bad and leave
  ::  +levy/+lien mulling their sample against the bare ~ branch
  ::  (mull-grow).
  ::
  =/  bad=(list check:sa)  (skip checks.verdict |=(c=check:sa ok.c))
  ?:  =(~ bad)  %unknown
  =/  classes=(list verdict-class:sa)
    (turn bad |=(c=check:sa (check-class name.c)))
  ?:  (lien classes |=(c=verdict-class:sa ?=(%fraud c)))    %fraud
  ?:  (lien classes |=(c=verdict-class:sa ?=(%unknown c)))  %unknown
  %stale
::
::  +unknown-verdict / +stale-verdict: the two-valued views of +classify,
::  kept because they read better at the call sites that ask one question.
::
++  unknown-verdict
  |=  =verdict:sa
  ^-  ?
  &(!ok.verdict ?=(%unknown (classify verdict)))
::
++  stale-verdict
  |=  =verdict:sa
  ^-  ?
  &(!ok.verdict ?=(%stale (classify verdict)))
::
::  +routable: can anything COLD-CONTACT a comet in this state?
::
::    A snapshot with neither a sponsor nor a fief is a one-way identity.
::    +urb-point-to-jael projects an absent sponsor to SELF, so the jael
::    point names the comet as its own sponsor and no peer that has
::    forgotten it can ever route to it again -- it can only ever speak
::    first, on a lane it already holds.
::
::    This is NOT a validity rule and MUST NOT become one: the decisions
::    addendum (section 2, "Fief scope") explicitly allows it, and a
::    negative verdict here would become a jael %fail and an ames snub of
::    a perfectly honest ship.  Causeway refuses to MINT one (the
::    --no-route opt-out); %gw-btc only warns its operator, so a
::    hand-rolled transaction that never touched Causeway is still
::    visible.
::
++  routable
  |=  snap=snapshot:sa
  ^-  ?
  |(?=(^ sponsor.snap) ?=(^ fief.snap))
::
::  +extend-log: append one custody entry, idempotently
::
::    The %anew ingestion path (app/gw-btc.hoon): Causeway hands us the
::    entry its custody transaction produced and we append it to the log
::    we already hold.  Re-poking the entry we ALREADY hold -- a retry, a
::    double click, a resumed script -- is a request to re-validate, not
::    a second hop: appending it twice would break input-0 continuity at
::    the duplicate and the log would then fail verification, silently,
::    forever.
::
++  extend-log
  |=  [base=custody-log:sa new=custody-entry:sa]
  ^-  custody-log:sa
  ?:  ?&(?=(^ base) =(new (rear base)))
    base
  (snoc base new)
::
++  nom
  |=  [idx=@ud suffix=@t]
  ^-  cord
  (rap 3 ~['entry-' (scot %ud idx) '-' suffix])
::
::  Decode the fixed-domain, fixed-kelvin suite-C pass.  Both atom
::  tails are required to be canonical so `cue` cannot silently accept
::  appended alternate data.  The hiding dat commitment is NOT opened
::  here: the spawn satpoint is learned only from the custody log's
::  $blind-opening and bound to the pass in ++run-checks.
++  from-xtr
  |=  [who=@p =pass]
  ^-  (unit self-attestation:sa)
  %-  mole
  |.
  =/  meta  (need (parse-pass:cc pass))
  ?>  =(domain:cc dom.meta)
  ?>  =(kelvin:cc kel.meta)
  =/  cic  (com:nu:cric:crypto pass)
  ?>  ?=(%c suite.+<.cic)
  ?>  =(who fig:ex:cic)
  =/  chain=custody-log:sa  ;;(custody-log:sa (cue xtr.meta))
  ?>  =(xtr.meta (jam chain))
  [who pass chain]
::
::  +spawn-of: the blind-opening that must sit on entry 0
::
++  spawn-of
  |=  chain=custody-log:sa
  ^-  (unit blind-opening:sa)
  ?~  chain  ~
  ?~  opening.i.chain  ~
  blind-opening.u.opening.i.chain
::
::  +openings-of: (idx, opening) pairs in custody order
::
++  openings-of
  |=  chain=custody-log:sa
  ^-  (list [idx=@ud =opening:sa])
  =/  idx=@ud  0
  |-
  ?~  chain  ~
  ?~  opening.i.chain
    $(chain t.chain, idx +(idx))
  [[idx u.opening.i.chain] $(chain t.chain, idx +(idx))]
::
++  p2tr-xonly
  |=  spk=hexb:bitcoin
  ^-  (unit @ux)
  ?.  =(34 wid.spk)  ~
  ?.  =(0x5120 (rsh [3 32] dat.spk))  ~
  `(end [3 32] dat.spk)
::
++  is-key-path
  |=  wit=witness:tx:bitcoin
  ^-  ?
  ?.  ?=([* ~] wit)  %.n
  |(=(64 wid.i.wit) =(65 wid.i.wit))
::
++  snag-input
  |=  [idx=@ud =tx:bc]
  ^-  (unit inputw:tx:bitcoin)
  ?:  (gte idx (lent is.tx))  ~
  `(snag idx is.tx)
::
++  tracked-ok
  |=  [tracked=sont:ord tip=sont:ord entering=(list sont:ord)]
  ^-  ?
  ?|  =(tracked tip)
      (lien entering |=(s=sont:ord =(s tracked)))
  ==
::
++  prefix-chain
  |=  [old=custody-log:sa new=custody-log:sa]
  ^-  ?
  |-
  ?~  old  %.y
  ?~  new  %.n
  ?.  =(i.old i.new)  %.n
  $(old t.old, new t.new)
::
::  Derive the final satpoint using only input-0 continuity and output values.
::  The light-client adapter uses this before its final /tx-out request.
++  derive-tip
  |=  [spawn=sont:ord start=tx:bc txl=(list tx:bc)]
  ^-  (unit sont:ord)
  ?.  =(id.start txid.spawn)  ~
  =/  prev=tx:bc  start
  =/  current=sont:ord  spawn
  |-
  ?~  txl  `current
  ?.  =(id.prev txid.current)  ~
  ?.  (lth vout.current (lent os.prev))  ~
  =/  spent=output:tx:bitcoin  (snag vout.current os.prev)
  ?.  (lth off.current value.spent)  ~
  =/  inp  (snag-input 0 i.txl)
  ?~  inp  ~
  ?.  =([txid.u.inp pos.u.inp] [txid.current vout.current])  ~
  =/  landed  (index-to-sont:uc off.current os.i.txl)
  ?~  landed  ~
  %=  $
    prev     i.txl
    current  [id.i.txl vout.landed off.landed]
    txl      t.txl
  ==
::
::  Verify a present opening against the sat-carrying output CREATED by
::  this entry: the canonical unspendable commitment leaf over the
::  jammed snapshot, tweaked into the claimed internal key, must equal
::  the on-chain P2TR output key exactly.  Invalid curve points are
::  packet failures, not verifier crashes.
++  opening-checks
  |=  $:  idx=@ud
          =opening:sa
          this=tx:bc
          landing=[vout=@ud off=@ud]
      ==
  ^-  (list check:sa)
  =/  key=(unit @ux)
    %-  mole
    |.((state-key:cc internal-key.opening snapshot.opening))
  =/  out=output:tx:bitcoin  (snag vout.landing os.this)
  =/  onchain  (p2tr-xonly script-pubkey.out)
  :~  :-  (nom idx 'commitment')
      ?&  ?=(^ key)
          ?=(^ onchain)
          =(u.key u.onchain)
      ==
  ==
::
::  +fail-result: the one-check verdict a +verify-lc early abort returns
::
::    The sample is $abort, not a cord, ON PURPOSE.  A free-form string
::    here is what put four unclassified names into the verdict stream in
::    the first place; now a new early return does not compile until its
::    reason has been added to the union, and adding it to the union does
::    not compile until +abort-class says what it means.
::
++  fail-result
  |=  [who=@p =abort:sa]
  ^-  result:sa
  [[who %.n ~[[`cord`abort %.n]]] ~ 0]
::
++  fail-checks
  |=  [who=@p checks=(list check:sa)]
  ^-  result:sa
  [[who %.n checks] ~ 0]
::
::  Pure verification boundary.  `txl` has exactly one transaction per
::  xtr entry, in custody order.  `tip-unspent=~` is unknown: it fails
::  `tip-scanned` (never `tip-unspent`), so the verdict is not ok and no
::  point is built, but the failure is classed unevaluable rather than
::  stale or fraudulent.  `known-public` is the set of ships the caller can
::  vouch exist as public points; a snapshot naming a sponsor outside it
::  fails the sponsor-known check, which is likewise unevaluable -- the set
::  is a window on the chain, not the whole of it (an absent sponsor
::  projects to self and is always fine).
::
::  EVERY check in here must be evaluable from the arguments alone.  A
::  check that silently degrades when an input is empty or degenerate is
::  the bug class this file exists to prevent: it produces a verdict, and
::  a negative verdict is a snub.
++  run-checks
  |=  $:  sat=self-attestation:sa
          start=tx:bc
          txl=(list tx:bc)
          tip-unspent=(unit ?)
          tracked=(unit anchor:sa)
          known-public=(set ship)
      ==
  ^-  result:sa
  =*  who  who.sat
  =|  checks=(list check:sa)
  =.  checks  (snoc checks ['chain-nonempty' ?=(^ chain.sat)])
  =.  checks  (snoc checks ['chain-bounded' (lte (lent chain.sat) 1.024)])
  =.  checks  (snoc checks ['fetch-count' =((lent chain.sat) (lent txl))])
  =/  spawn-open  (spawn-of chain.sat)
  =.  checks  (snoc checks ['spawn-opening' ?=(^ spawn-open)])
  ?.  (levy checks |=(c=check:sa ok.c))
    (fail-checks who checks)
  ?>  ?=(^ spawn-open)
  =/  spawn=sont:ord  spawn.u.spawn-open
  ::  the pass's hiding dat commitment must open to exactly this spawn
  ::  satpoint and blind
  ::
  =/  meta  (parse-pass:cc pass.sat)
  =/  commit-ok=?
    ?~  meta  %.n
    =((spawn-commit:cc spawn blind.u.spawn-open) d.u.meta)
  =.  checks  (snoc checks ['spawn-commit' commit-ok])
  =.  checks  (snoc checks ['start-txid' =(id.start txid.spawn)])
  ?.  (levy checks |=(c=check:sa ok.c))
    (fail-checks who checks)
  ?.  (lth vout.spawn (lent os.start))
    (fail-checks who (snoc checks ['start-vout-range' %.n]))
  =/  start-out=output:tx:bitcoin  (snag vout.spawn os.start)
  ?.  (lth off.spawn value.start-out)
    (fail-checks who (snoc checks ['start-off-range' %.n]))
  =.  checks  (snoc checks ['start-vout-range' %.y])
  =.  checks  (snoc checks ['start-off-range' %.y])
  =/  entries=custody-log:sa  chain.sat
  =/  txs=(list tx:bc)  txl
  =/  idx=@ud  0
  =/  prev=tx:bc  start
  =/  current=sont:ord  spawn
  =|  last-height=@ud
  =|  entering=(list sont:ord)
  =|  latest=(unit [idx=@ud snap=snapshot:sa])
  |-
  ^-  result:sa
  ?~  entries
    ::  end of the walk: resolve the latest snapshot and the tip
    ::
    =.  checks  (snoc checks ['state-resolve' ?=(^ latest)])
    ::  THE LIVENESS SCAN IS THREE-VALUED AND MUST STAY THAT WAY.
    ::
    ::    tip-unspent=~       the scan could not be evaluated
    ::    tip-unspent=[~ %.n] the scan PROVED the outpoint spent
    ::    tip-unspent=[~ %.y] the scan proved it unspent
    ::
    ::  Collapsing the first two into one failing check (the old
    ::  `=([~ %.y] tip-unspent)`) made "we could not look" indistinguishable
    ::  from "we looked and it is gone", which routed an infrastructure
    ::  failure to the %stale demotion path.  Split them:
    ::
    ::    tip-scanned fails on ~ only          -> +unknown-checks -> silence
    ::    tip-unspent fails on [~ %.n] only    -> +stale-checks   -> %stale
    ::
    ::  tip-unspent passing VACUOUSLY on ~ is not a fail-open: tip-scanned
    ::  has already failed, so the verdict is not ok, no point is built, and
    ::  nothing is installed.  The only thing it changes is which of the two
    ::  non-fraud outcomes we take, which is the whole point.
    ::
    =.  checks  (snoc checks ['tip-scanned' ?=(^ tip-unspent)])
    =.  checks  (snoc checks ['tip-unspent' ?~(tip-unspent %.y u.tip-unspent)])
    =/  tip-out=output:tx:bitcoin  (snag vout.current os.prev)
    =.  checks
      (snoc checks ['tip-p2tr' ?=(^ (p2tr-xonly script-pubkey.tip-out))])
    ::  the carried pass's messaging key must match the latest
    ::  custody-proven snapshot
    ::
    =/  key-ok=?
      ?~  latest  %.n
      =/  got=(unit @)
        %-  mole  |.
        =/  cic  (com:nu:cric:crypto pass.sat)
        ?>  ?=(%c suite.+<.cic)
        `@`cry.pub.+<.cic
      ?~  got  %.n
      =(u.got key.snap.u.latest)
    =.  checks  (snoc checks ['pass-key' key-ok])
    ::  A named sponsor must exist as a public point; absent is self.
    ::  Failing this is IGNORANCE, not fraud -- see +unknown-checks.
    ::
    =/  sponsor-ok=?
      ?~  latest  %.n
      ?~  sponsor.snap.u.latest  %.y
      (~(has in known-public) u.sponsor.snap.u.latest)
    =.  checks  (snoc checks ['sponsor-known' sponsor-ok])
    =?  checks  ?=(^ tracked)
      %+  snoc  checks
      ['tracked-tip' (tracked-ok sont.own.point.u.tracked current entering)]
    =/  old-sat=(unit self-attestation:sa)
      ?~  tracked  ~
      (from-xtr who pass.net.point.u.tracked)
    =/  anchor-ok=?
      ?~  tracked  %.y
      ?~  old-sat  %.n
      ?.  =((spawn-of chain.u.old-sat) spawn-open)  %.n
      ?.  (prefix-chain chain.u.old-sat chain.sat)  %.n
      =/  boundary
        (derive-tip spawn start (scag (lent chain.u.old-sat) txl))
      =(`tip.u.tracked boundary)
    =.  checks  (snoc checks ['tracked-prefix' anchor-ok])
    =/  life-ok=?
      ?~  tracked  %.y
      ?~  latest  %.n
      (gte life.snap.u.latest life.net.point.u.tracked)
    =?  checks  ?=(^ tracked)
      (snoc checks ['life-monotonic' life-ok])
    =/  ok  (levy checks |=(c=check:sa ok.c))
    =/  point=(unit point:urb)
      ?.  &(ok ?=(^ latest))  ~
      =*  snap  snap.u.latest
      :-  ~
      :*  own=[current ~]
          rift=rift.snap
          life=life.snap
          pass=pass.sat
          ^=  sponsor
          ?~  sponsor.snap
            [%.n who]
          [%.y u.sponsor.snap]
          escape=~
          fief=fief.snap
      ==
    :+  [who ok checks]
      point
    ?.  ok  0
    ?.  (lth vout.current (lent os.prev))  0
    value:(snag vout.current os.prev)
  ?~  txs
    (fail-checks who (snoc checks ['fetch-count' %.n]))
  =*  ent  i.entries
  =*  this  i.txs
  =.  checks  (snoc checks [(nom idx 'txid') =(txid.ent id.this)])
  =.  checks
    (snoc checks [(nom idx 'height-order') ?:(=(0 idx) & (gte height.ent last-height))])
  =/  inp  (snag-input 0 this)
  ?~  inp
    (fail-checks who (snoc checks [(nom idx 'input-zero') %.n]))
  =.  checks  (snoc checks [(nom idx 'input-zero') %.y])
  =/  continuity=?
    =([txid.u.inp pos.u.inp] [txid.current vout.current])
  =.  checks  (snoc checks [(nom idx 'continuity') continuity])
  ?.  &(=(id.prev txid.current) (lth vout.current (lent os.prev)))
    (fail-checks who (snoc checks [(nom idx 'prevout-range') %.n]))
  =/  spent=output:tx:bitcoin  (snag vout.current os.prev)
  ::  After the arbitrary first-hop spend, a one-item 64/65-byte witness
  ::  is a key-path proof only when Bitcoin evaluated it against a P2TR
  ::  prevout.  Other witness programs can have the same stack shape.
  ::  All spends after the first hop must be key-path: the commitment
  ::  leaf is unspendable by construction.
  =/  spent-key  (p2tr-xonly script-pubkey.spent)
  =/  keypath=?
    ?:(=(0 idx) %.y &(?=(^ spent-key) (is-key-path witness.u.inp)))
  =.  checks  (snoc checks [(nom idx 'key-path') keypath])
  =/  off-ok=?  (lth off.current value.spent)
  =.  checks  (snoc checks [(nom idx 'off-range') off-ok])
  =/  landed  (index-to-sont:uc off.current os.this)
  =.  checks  (snoc checks [(nom idx 'sat-landed') ?=(^ landed)])
  ?.  ?&  =(txid.ent id.this)
          continuity
          keypath
          off-ok
          ?=(^ landed)
      ==
    (fail-checks who checks)
  =/  next=sont:ord  [id.this vout.landed off.landed]
  =.  entering  [current entering]
  ?~  opening.ent
    %=  $
      entries      t.entries
      txs          t.txs
      idx          +(idx)
      prev         this
      current      next
      last-height  height.ent
    ==
  =*  open  u.opening.ent
  =.  checks  (weld checks (opening-checks idx open this landed))
  ::  the dat opening may sit only on entry 0, and snapshot lives may
  ::  never regress across openings
  ::
  =.  checks
    %+  snoc  checks
    [(nom idx 'blind-opening-zero') |(=(0 idx) ?=(~ blind-opening.open))]
  =.  checks
    %+  snoc  checks
    :-  (nom idx 'life-order')
    ?~  latest  %.y
    ?&  (gte life.snapshot.open life.snap.u.latest)
        (gte rift.snapshot.open rift.snap.u.latest)
    ==
  %=  $
    entries      t.entries
    txs          t.txs
    idx          +(idx)
    prev         this
    current      next
    last-height  height.ent
    latest       `[idx snapshot.open]
  ==
--
