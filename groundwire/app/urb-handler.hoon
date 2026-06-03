::  %urb-handler
::
::  Prototype off-chain verifier for CONFIDENTIAL COMET self-attestation packets.
::
::  Where %urb-watcher learns comet PKI by scanning blocks and parsing the `urb`
::  scripts revealed on-chain, %urb-handler learns it from a self-attestation
::  packet handed over off-chain (see sur/self-attestation). It takes a
::  %self-attestation poke and — like %urb-watcher — spins up a strandio thread
::  that fetches the referenced transactions from a Bitcoin node and checks them
::  against the packet, producing a verdict.
::
::  For each link in the ownership chain it verifies:
::    - the transaction exists on-chain (get-raw-transaction),
::    - it spends the previous link's output (chain continuity / no gaps),
::    - that spent output's taproot key equals the key reconstructed from the
::      off-chain reveal (internal key + single committed leaf) — i.e. the output
::      really committed to this `urb` sotx, even though nothing about the script
::      tree is on-chain,
::    - the spend is an ordinary KEY-PATH spend (single-signature witness, no
::      control block), so the comet stayed confidential,
::    - the leaf script re-parses to the sotx(es) the link claims,
::  and for the genesis link additionally reproduces %urb-core's %spawn check:
::  the precommit output's scriptPubKey hash matches, and the comet's networking
::  key encodes the tweak binding it to that precommit satpoint. Finally it
::  confirms the tip UTXO is currently unspent (gettxout).
::
::  Scope note: this is a starting point for nailing down the packet format. It
::  is deliberately NOT wired to Jael/Ames/%urb-watcher and does not store the
::  resulting point; it just reports a verdict. Full sat-offset tracking across
::  multi-input fee math and deep Schnorr signature checks are left as `XX`.
::
/-  bitcoin, spider, ord, urb, sa=self-attestation
/+  bc=bitcoin, btcio, dbug, default-agent, ol=ord,
    bscr=btc-script, ue=urb-encoder, tr=taproot, strandio, verb
::
|%
+$  card  card:agent:gall
+$  versioned-state  $%(state-0)
+$  state-0
  $:  %0
      rpc=req-to:btcio
      last=(unit verdict:sa)
  ==
--
::
%-  agent:dbug
^-  agent:gall
=|  state-0
=*  state  -
%+  verb  &
=<
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init
  ^-  (quip card _this)
  ::  Same RPC endpoint %urb-watcher uses; the node must run with -txindex.
  =/  new-rpc=req-to:btcio
    :-  'https://alpha.groundwire.dev/rpc'
    [%basic 'mainnetrpcuser:fc3d36ce83e15484e75a658b2a9a8a90a66f4cb017ace74c8631fe082b93adbf']
  `this(rpc new-rpc)
::
++  on-save  ^-(vase !>(state))
++  on-load
  |=  =vase
  ^-  (quip card _this)
  =/  old  !<(versioned-state vase)
  ?-  -.old
    %0  `this(state old)
  ==
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?+    mark  (on-poke:def mark vase)
      %self-attestation
    =/  sat  !<(self-attestation:sa vase)
    ~&  >  "%urb-handler: verifying self-attestation for {<who.sat>} ({<(lent chain.sat)>} links)..."
    :_  this
    :~  :*  %pass  /verify  %arvo  %k
            %lard  q.byk.bowl
            (verify-attestation rpc.state sat)
        ==
    ==
  ==
::
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card _this)
  ?+    wire  (on-arvo:def wire sign-arvo)
      [%verify ~]
    ?+    sign-arvo  (on-arvo:def wire sign-arvo)
        [%khan %arow *]
      ?.  -.p.sign-arvo
        ?>  ?=([%khan %arow %.n *] sign-arvo)
        %-  (slog leaf+"%urb-handler: verify thread crashed" +.p.p.sign-arvo)
        `this
      ?>  ?=([%khan %arow %.y %noun *] sign-arvo)
      =/  [%khan %arow %.y %noun =vase]  sign-arvo
      =/  =verdict:sa  !<(verdict:sa vase)
      %-  (slog (report verdict))
      `this(last `verdict)
    ==
  ==
::
++  on-peek
  |=  =(pole knot)
  ^-  (unit (unit cage))
  ?+    pole  (on-peek:def pole)
    ::  /x/last — the most recent verdict, if any
      [%x %last ~]
    ``noun+!>(last)
  ==
::
++  on-watch  on-watch:def
++  on-agent  on-agent:def
++  on-leave  on-leave:def
++  on-fail   on-fail:def
--
::
::  helper core
::
|%
::  +report: pretty-print a verdict for the dojo log.
::
++  report
  |=  =verdict:sa
  ^-  tang
  :-  leaf+"%urb-handler: attestation for {<who.verdict>} is {?:(ok.verdict "VALID" "INVALID")}"
  %+  turn  checks.verdict
  |=  =check:sa
  ^-  tank
  leaf+"  [{?:(ok.check "ok" "XX")}] {(trip name.check)}"
::
::  +verify-attestation: the strandio thread. Fetches every referenced tx from
::  the node, then runs the pure verifier, returning a +verdict in a vase.
::
++  verify-attestation
  |=  [rpc=req-to:btcio sat=self-attestation:sa]
  ^-  shed:khan
  =/  m  (strand:strandio ,vase)
  ^-  form:m
  =*  who  who.sat
  ?~  chain.sat
    (pure:m !>(`verdict:sa`[who %.n ~[['empty-chain' %.n]]]))
  ::  Phase 1: fetch from the node.
  ::  Every link's transaction, in order.
  ;<  txs=(list (unit tx:bc))  bind:m
    (fetch-txs rpc chain.sat)
  ?:  (lien txs |=(t=(unit tx:bc) ?=(~ t)))
    (pure:m !>(`verdict:sa`[who %.n ~[['fetch:chain-tx' %.n]]]))
  =/  txl=(list tx:bc)  (turn txs need)
  ::  The genesis link spends the spawn COMMIT tx; learn its txid from the
  ::  genesis input's prevout, then fetch it and the precommit tx.
  =/  genesis=tx:bc  (snag 0 txl)
  =/  gin  (snag-input in.i.chain.sat genesis)
  ?~  gin
    (pure:m !>(`verdict:sa`[who %.n ~[['genesis:input-index' %.n]]]))
  ;<  commit=(unit tx:bc)  bind:m
    (get-raw-transaction:btcio rpc ~ txid.u.gin)
  ?~  commit
    (pure:m !>(`verdict:sa`[who %.n ~[['fetch:commit-tx' %.n]]]))
  ;<  pre=(unit tx:bc)  bind:m
    (get-raw-transaction:btcio rpc ~ precommit.sat)
  ?~  pre
    (pure:m !>(`verdict:sa`[who %.n ~[['fetch:precommit-tx' %.n]]]))
  ;<  tip-unspent=(unit ?)  bind:m
    (get-tx-out:btcio rpc ~ txid.tip.sat vout.tip.sat)
  ::  Phase 2: pure verification.
  =/  =verdict:sa
    (run-checks sat txl u.commit u.pre tip-unspent)
  (pure:m !>(verdict))
::
::  +fetch-txs: fetch every link's transaction, preserving order.
::
++  fetch-txs
  |=  [rpc=req-to:btcio links=(list link:sa)]
  =/  m  (strand:strandio ,(list (unit tx:bc)))
  ^-  form:m
  =|  acc=(list (unit tx:bc))
  |-  ^-  form:m
  ?~  links  (pure:m (flop acc))
  ;<  t=(unit tx:bc)  bind:m
    (get-raw-transaction:btcio rpc ~ txid.i.links)
  $(links t.links, acc [t acc])
::
::  +run-checks: the pure verifier. Walks the chain accumulating named checks.
::
++  run-checks
  |=  $:  sat=self-attestation:sa
          txl=(list tx:bc)
          commit=tx:bc
          pre=tx:bc
          tip-unspent=(unit ?)
      ==
  ^-  verdict:sa
  ::  Genesis %spawn / precommit / tweak checks.
  =/  checks=(list check:sa)  (check-spawn sat pre)
  ::  Walk each link. `prev` is the tx whose output this link spends: the commit
  ::  tx for the genesis link, the previous chain tx thereafter.
  =/  links  chain.sat
  =/  idx=@ud  0
  =/  prev=tx:bc  commit
  |-
  ^-  verdict:sa
  ?~  links
    ::  Tip: the last tx must be the one holding the tip sat, and it must be
    ::  currently unspent on-chain.
    =/  tip-here=?  =(txid.tip.sat id.prev)
    =/  unspent=?   =([~ %.y] tip-unspent)
    =.  checks
      %+  weld  checks
      ~[['tip-matches-last-tx' tip-here] ['tip-unspent' unspent]]
    [who.sat (levy checks |=(c=check:sa ok.c)) checks]
  =/  this-tx=tx:bc  (snag idx txl)
  =.  checks  (weld checks (link-checks idx i.links prev this-tx))
  $(links t.links, idx +(idx), prev this-tx)
::
::  +link-checks: the per-link checks (continuity, commitment, key-path, sots).
::
++  link-checks
  |=  [idx=@ud =link:sa prev=tx:bc this=tx:bc]
  ^-  (list check:sa)
  =/  inp  (snag-input in.link this)
  ?~  inp  ~[[(nom idx 'input-index') %.n]]
  ::  Continuity: the spent prevout must be an output of `prev`.
  =/  cont=?  =(txid.u.inp id.prev)
  ?.  (lth pos.u.inp (lent os.prev))
    ~[[(nom idx 'continuity') cont] [(nom idx 'prevout-range') %.n]]
  =/  spent=output:tx:bitcoin  (snag pos.u.inp os.prev)
  ::  Commitment: reconstruct the taproot output key from the off-chain reveal
  ::  (internal key + single leaf) and match the spent output's scriptPubKey.
  =/  recomputed=@ux  (out-key reveal.link)
  =/  onchain=(unit @ux)  (p2tr-xonly script-pubkey.spent)
  =/  commit-ok=?  &(?=(^ onchain) =(u.onchain recomputed))
  ::  Confidential: the spend must be key-path (single-sig witness).
  =/  keypath-ok=?  (is-key-path witness.u.inp)
  ::  The committed leaf must re-parse to exactly the claimed sotx(es).
  =/  sots-ok=?
    =/  parsed  (parse-leaf script.tapleaf.reveal.link)
    =((turn parsed get-sotx) (turn sots.link get-sotx))
  :~  [(nom idx 'continuity') cont]
      [(nom idx 'commitment') commit-ok]
      [(nom idx 'key-path') keypath-ok]
      [(nom idx 'sots-match') sots-ok]
  ==
::
::  +nom: build a per-link check name like 'link-3-commitment'.
::
++  nom
  |=  [idx=@ud suffix=@t]
  ^-  cord
  (rap 3 ~['link-' (scot %ud idx) '-' suffix])
::
::  +out-key: reconstruct the taproot output x-only key from a reveal. The tree
::  is a single leaf, so the merkle root is just that leaf's hash.
::
++  out-key
  |=  =reveal:sa
  ^-  @ux
  =/  =tapleaf:tr  tapleaf.reveal
  (output-pubkey:tr internal-key.reveal `(leaf-hash:tr tapleaf))
::
::  +p2tr-xonly: extract the 32-byte x-only key from a P2TR scriptPubKey
::  (OP_1 PUSH32 <key> = 0x51 0x20 ...). Returns ~ if not a v1 taproot output.
::
++  p2tr-xonly
  |=  spk=hexb:bitcoin
  ^-  (unit @ux)
  ?.  =(34 wid.spk)  ~
  ?.  =(0x5120 (rsh [3 32] dat.spk))  ~
  `(end [3 32] dat.spk)
::
::  +is-key-path: a taproot key-path witness is a single element (the Schnorr
::  signature, 64 bytes or 65 with a sighash byte) and carries no control block.
::  XX annex (a trailing 0x50-prefixed element) is not handled.
::
++  is-key-path
  |=  wit=witness:tx:bitcoin
  ^-  ?
  ?.  ?=([* ~] wit)  %.n
  |(=(64 wid.i.wit) =(65 wid.i.wit))
::
::  +snag-input: the inputw at index `idx`, or ~ if out of range.
::
++  snag-input
  |=  [idx=@ud =tx:bc]
  ^-  (unit inputw:tx:bitcoin)
  ?:  (gte idx (lent is.tx))  ~
  `(snag idx is.tx)
::
::  +parse-leaf: decode the sotx(es) committed in a tapleaf script, exactly as
::  %urb-core does for on-chain reveals (btc-script -> unv -> raw-sotx).
::
++  parse-leaf
  |=  script=hexb:bitcoin
  ^-  (list raw-sotx:urb)
  =/  oct=octs  [wid.script dat.script]
  =/  descr  (de:bscr oct)
  ?~  descr  ~
  =/  unvs=(list @)  (unv:de:ue u.descr)
  (zing (turn unvs parse-roll:ue))
::
::  +get-sotx: project a raw-sotx down to its sotx for structural comparison.
::
++  get-sotx
  |=  r=raw-sotx:urb
  ^-  sotx:urb
  sot.r
::
::  +check-spawn: reproduce %urb-core's %spawn proof off-chain. Confirms the
::  precommit output named by the %spawn sotx exists with the attested
::  scriptPubKey hash, and that the comet's suite-C networking key encodes the
::  tweak binding it to that precommit satpoint.
::  XX the in-value upper-bound sanity check from calc-precommit-sont is omitted
::     here (it would need the precommit's own prevout values).
::
++  check-spawn
  |=  [sat=self-attestation:sa pre=tx:bc]
  ^-  (list check:sa)
  ?~  chain.sat  ~[['spawn-no-chain' %.n]]
  =/  sots  sots.i.chain.sat
  ?~  sots  ~[['spawn-no-sots' %.n]]
  =/  spawn  (find-spawn +.sot.i.sots)
  ?~  spawn  ~[['spawn-not-found' %.n]]
  =*  sp  u.spawn
  ?~  vout.to.sp  ~[['spawn-no-vout' %.n]]
  ?.  (lth u.vout.to.sp (lent os.pre))
    ~[['spawn-precommit-range' %.n]]
  =/  out=output:tx:bitcoin  (snag u.vout.to.sp os.pre)
  ::  Rebuild the precommit output's scriptPubKey hash, as calc-precommit-sont.
  =/  en-out  (can 3 script-pubkey.out 8^value.out ~)
  =/  hax-out  (shay (add 8 wid.script-pubkey.out) en-out)
  =/  spkh-ok=?  =(hax-out spkh.to.sp)
  ::  The tweak that the networking key must encode (mirrors lib/urb-core).
  =/  psat=sont:ord  [id.pre u.vout.to.sp off.to.sp]
  =/  tweak
    %+  rap  3
    :~  %9  ~tyr  %urb-watcher  %btc  %gw  %9
        txid.psat  vout.psat  off.psat
    ==
  =/  cac  (com:nu:cric:crypto pass.sp)
  ?.  ?=(%c suite.+<.cac)
    ~[['spawn-precommit-spkh' spkh-ok] ['spawn-suite-c' %.n]]
  =/  tweak-ok=?  =(dat.tw.pub:+<:cac tweak)
  :~  ['spawn-precommit-spkh' spkh-ok]
      ['spawn-suite-c' %.y]
      ['spawn-key-tweak' tweak-ok]
  ==
::
::  +find-spawn: pull the %spawn single out of a skim-sotx (bare or in a batch).
::
++  find-spawn
  |=  act=skim-sotx:urb
  ^-  (unit $>(%spawn single:skim-sotx:urb))
  ?:  ?=(%spawn -.act)  `act
  ?.  ?=(%batch -.act)  ~
  |-  ^-  (unit $>(%spawn single:skim-sotx:urb))
  ?~  bat.act  ~
  ?:  ?=(%spawn -.i.bat.act)  `i.bat.act
  $(bat.act t.bat.act)
--
