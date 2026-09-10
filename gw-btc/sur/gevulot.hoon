::  sur/gevulot.hoon — types for the %gevulot control-pane agent
::
::    %gevulot is a Landscape-navigable control pane for off-chain
::    identity operations, layered on top of the %gw-btc verifier.  Its
::    first feature is SPONSOR-MEDIATED PEER DISCOVERY:
::
::      - A sponsee asks its sponsor to broadcast the sponsee's own
::        self-attestation to the sponsor's other sponsees.
::      - The sponsor keeps a roster of announced sponsees and pushes each
::        one to every opted-in sponsee (now and as new ones join).
::      - An opted-in sponsee INSTALLS a pushed attestation into jael on
::        TRUST when it cannot yet check the chain itself: its chosen
::        sponsor already verified the packet, so the sponsee does only the
::        offline crypto self-check and skips its own light-client sync.
::        This replaces "download the whole public index snapshot".
::
::    TRUST IS PROVISIONAL.  A trusted install is a head start, not a
::    verdict.  As soon as %gw-btc's own light client is synced, %gevulot
::    re-submits every peer it installed on trust through the SAME on-chain
::    verification a peer's direct packet gets (%jael-writ).  A valid peer
::    is confirmed idempotently; a stale one is demoted without a snub; a
::    hand-pasted forgery is caught.  Peers pasted or scanned by the user
::    are verified on chain immediately whenever the client is already
::    synced -- they carry no sponsor's word, so they get none of the
::    trust and all of the check.
::
|%
::  $provenance: how a peer entry was learned.  Drives nothing but the UI
::  and, for %paste, the "verify now if we can" bias.
::
+$  provenance  ?(%sponsor %paste %dojo)
::  $peer: a peer whose attestation we hold.  .pass is kept so we can
::  re-submit it for on-chain verification once synced; .tries bounds that
::  retry so a permanently-unconfirmable entry (stale, or a bad paste)
::  stops re-poking instead of looping forever.
::
+$  peer
  $:  =pass
      via=provenance
      since=@da
      tries=@ud
  ==
::  $action: a LOCAL action, from the UI (POST) or the dojo.
::
::    Never accepted from a remote ship -- see +on-poke's `?> =(our src)`.
::
+$  action
  $%  [%set-receive on=?]        :: sponsee: accept pushes from my sponsor?
      [%distribute ~]            :: sponsee: ask my sponsor to broadcast me
      [%withdraw ~]              :: sponsee: ask my sponsor to stop broadcasting me
      [%ingest-peer =pass]       :: sponsee: install a hand-pasted attestation
      [%forget-peer who=@p]      :: sponsee: drop a peer we installed
      [%recheck ~]               :: sponsee: re-verify all installs on chain now
      [%set-serving on=?]        :: sponsor: run the distribution service?
  ==
::  $wire: an inter-ship poke between a sponsor and its sponsees.
::
::    Rides the %noun mark (as %gw-btc's %jael-writ does), so no mark file
::    is needed and the receiver mold-casts with `;;`.
::
::    %announce CARRIES the announcer's pass.  It used to carry none, on the
::    theory that the sponsor reads the sponsee's pass back from its own
::    jael -- but jael serves a comet's %deed only for `our` (any other
::    comet's is [~ ~], which blocks the whole event: the first real
::    announce nacked, 2026-09-10).  So the sponsee sends its attestation
::    the way its ames packets do, and the sponsor checks two things
::    before broadcasting it: the pass is src's own (its fingerprint IS
::    src's @p, so a sender can only ever announce itself) and src is a
::    current sponsee per %gw-btc's on-chain view.  %peer carries the
::    peer's pass for a sponsee to install.
::
+$  wire
  $%  [%announce =pass]          :: sponsee -> sponsor: please broadcast me (my pass)
      [%withdraw ~]              :: sponsee -> sponsor: stop broadcasting me
      [%peer who=@p =pass]       :: sponsor -> sponsee: install this peer
  ==
--
