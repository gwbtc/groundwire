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
::        TRUST: its chosen sponsor already verified the packet on chain,
::        so the sponsee does only the offline crypto self-check and skips
::        its own light-client sync.  This replaces "download the whole
::        public index snapshot" for discovery.
::
::    The install rides %gw-btc's existing verdict path but with no chain
::    fetch; it is idempotent and can never emit a negative verdict, so a
::    duplicate (same QR twice, or heard-then-seen-on-chain) never snubs.
::
|%
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
      [%set-serving on=?]        :: sponsor: run the distribution service?
  ==
::  $wire: an inter-ship poke between a sponsor and its sponsees.
::
::    Rides the %noun mark (as %gw-btc's %jael-writ does), so no mark file
::    is needed and the receiver mold-casts with `;;`.
::
::    %announce carries NO pass: the sponsor already holds the announcing
::    sponsee's pass in its own jael (it verified it before sponsoring), so
::    it reads that back rather than trusting a sender-supplied packet --
::    the sender can only ask to be broadcast, not choose what is broadcast.
::    %peer carries the peer's pass for a sponsee to install.
::
+$  wire
  $%  [%announce ~]              :: sponsee -> sponsor: please broadcast me
      [%withdraw ~]              :: sponsee -> sponsor: stop broadcasting me
      [%peer who=@p =pass]       :: sponsor -> sponsee: install this peer
  ==
--
