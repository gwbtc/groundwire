::  lib/gw-verify.hoon
::
::  Confidential-comets custody-walk verifier (cc-draft-2 kernel spec, section 7:
::  gwbtc/urbit pkg/arvo/doc/spec/confidential-comets.md).
::
::  %gw-btc calls +verify in a khan thread to answer a %jael-writ poke for a
::  comet that is NOT in its indexed on-chain view -- the confidential /
::  unindexed path.  (Public comets that did an on-chain reveal are answered
::  synchronously from unv-ids; see +verify-writ in app/gw-btc.)  This replaces
::  the "full variant-B tx-fetch custody walk (spec S7) is stubbed (XX)" marker
::  the agent shipped with.
::
::  The section-7 algorithm, given a suite-%c $pass carrying its own attestation:
::
::    1. decode the pass: cry (messaging key), dat (immutable tweak: mat-encoded
::       PKI domain tag + spawn satpoint) and xtr (the OFF-CHAIN reveal of the
::       on-chain custody log).  Assert the domain tag matches what Jael routed,
::       and re-derive who = fig(tweaked key) rather than trusting it.
::    2. walk the custody log spawn -> tip.  It is a LOCATOR, not a proof: each
::       [txid block-height reveal] entry names a tx the agent fetches from its
::       own node and re-checks (input-0 key-path-spends the previous
::       sat-carrying output; the sat tracks deterministically to its landing
::       output).  Commitments carry full STATES (spec section 2.1), so only
::       entries with a $reveal re-attest networking state -- their landing
::       output's taproot key must equal Q recomputed from the disclosed leaf
::       (single-leaf sparse tree, root == leaf-hash) -- and only the LATEST
::       such state is authoritative.  Bare entries just prove custody moved.
::    3. the tip must be unspent on our node, and the pass's current messaging
::       key must equal the latest attested one.
::    4. verdict: the point, from the latest committed state (rift 0, fief ~,
::       per spec section 7).
::
::  The strand does the (async) node fetches; +walk-checks is the pure verifier
::  over the fetched txs, so it is unit-testable without a node.  Ported
::  primitives (+out-key, +p2tr-xonly, +is-key-path, +parse-leaf) mirror the
::  hd/urb-handler self-attestation prototype and lib/urb-core's on-chain math.
::
::  XX flagged for spec-author (cyc) review -- consensus-critical interpretations
::  of the section-7 pseudocode; see the marked arms:
::    - entry-0 (spawn) placement + %spawn->initial-state extraction
::    - txid byte-order between dat/xtr and the node's tx ids (must match
::      Causeway's dat/xtr encoder)
::    - sponsor-consent signature verification (a deviation we add over section 7)
::
/-  bitcoin, ord, urb
/+  bc=bitcoin, bscr=btc-script, btcio, strandio, tr=taproot, ue=urb-encoder, uc=urb-core
|%
::  +out-key: reconstruct the taproot output x-only key committed by a $reveal.
::  The tree is a single leaf, so the merkle root IS that leaf's hash.
::
++  out-key
  |=  =reveal:urb
  ^-  @ux
  =/  =tapleaf:tr  [leaf-version.reveal leaf-script.reveal]
  (output-pubkey:tr internal-key.reveal `(leaf-hash:tr tapleaf))
::
::  +p2tr-xonly: extract the 32-byte x-only key from a P2TR scriptPubKey
::  (OP_1 PUSH32 <key> = 0x51 0x20 ...).  ~ if not a v1 taproot output.
::
++  p2tr-xonly
  |=  spk=hexb:bitcoin
  ^-  (unit @ux)
  ?.  =(34 wid.spk)  ~
  ?.  =(0x5120 (rsh [3 32] dat.spk))  ~
  `(end [3 32] dat.spk)
::
::  +is-key-path: a taproot key-path witness is a single element (the Schnorr
::  signature, 64 or 65 bytes) with no control block.  XX annex not handled
::  (a key-path spend with a 0x50-prefixed annex is wrongly rejected -- a false
::  negative, not a hole).
::
++  is-key-path
  |=  wit=witness:tx:bitcoin
  ^-  ?
  ?.  ?=([* ~] wit)  %.n
  |(=(64 wid.i.wit) =(65 wid.i.wit))
::
::  +snag-input: input at index `idx`, or ~ if out of range.
::
++  snag-input
  |=  [idx=@ud =tx:bc]
  ^-  (unit inputw:tx:bitcoin)
  ?:  (gte idx (lent is.tx))  ~
  `(snag idx is.tx)
::
::  +p2tr-at: the x-only taproot key of tx's output `vout`, if present + P2TR.
::
++  p2tr-at
  |=  [=tx:bc vout=@ud]
  ^-  (unit @ux)
  ?.  (lth vout (lent os.tx))  ~
  (p2tr-xonly script-pubkey:(snag vout os.tx))
::
::  +parse-leaf: decode the sotx(es) committed in a tapleaf script exactly as
::  urb-core does for on-chain reveals (btc-script -> unv -> raw-sotx).  The
::  encoder parsers crash on malformed input; a malicious packet must yield a
::  failed verdict, not a thread crash, so the parse is virtualized.  ~ =
::  unparsable.
::
++  parse-leaf
  |=  script=hexb:bitcoin
  ^-  (unit (list raw-sotx:urb))
  %-  mole
  |.
  ^-  (list raw-sotx:urb)
  =/  oct=octs  [wid.script dat.script]
  =/  descr  (de:bscr oct)
  ?~  descr  !!
  =/  unvs=(list @)  (unv:de:ue u.descr)
  (zing (turn unvs parse-roll:ue))
::
::  +find-single: the first single of the given opcode across a parsed leaf
::  (expanding %batch), or ~.
::
++  find-single
  |*  [tag=?(%spawn %state) sots=(list raw-sotx:urb)]
  |^  ^-  (unit single:skim-sotx:urb)
  |-
  ?~  sots  ~
  =/  hit  (scan-one +.sot.i.sots)
  ?^  hit  hit
  $(sots t.sots)
  ++  scan-one
    |=  act=skim-sotx:urb
    ^-  (unit single:skim-sotx:urb)
    ::  a bare single of the sought opcode (the ?= narrows act to a single
    ::  so it nests in the return type; =tag then picks the exact opcode)
    ?:  ?&(?=(?(%spawn %state) -.act) =(tag -.act))  `act
    ?.  ?=(%batch -.act)  ~
    |-  ^-  (unit single:skim-sotx:urb)
    ?~  bat.act  ~
    ?:  =(tag -.i.bat.act)  `i.bat.act
    $(bat.act t.bat.act)
  --
::
::  +parse-spawn-state: the initial networking state from entry-0's %spawn leaf.
::  life 1, messaging key = the cry of the spawn pass, sponsor = the comet's
::  structural sponsor (spec section 7: %spawn -> initial state).
::  XX interpretation: a comet's on-chain sponsor at spawn is (sein who); a
::     confidential comet can only change it via a later %state re-attestation.
::
++  parse-spawn-state
  |=  [who=ship =reveal:urb]
  ^-  (unit gw-state:urb)
  ?~  parsed=(parse-leaf leaf-script.reveal)  ~
  ?~  sp=(find-single %spawn u.parsed)  ~
  ?>  ?=(%spawn -.u.sp)
  =/  cek  +<:(com:nu:cric:crypto pass.u.sp)
  ?.  ?=([%c *] cek)  ~
  ::  bind the committed pass to who: the spawn leaf's OWN pass must
  ::  fingerprint to the comet's @p, else an attacker could copy a
  ::  victim's spawn (same dat + public cry) under a fresh ugn / @p and
  ::  ride the victim's whole chain (sat-reuse identity forgery).
  ?.  =(who fig:ex:(com:nu:cric:crypto pass.u.sp))  ~
  `[life=1 key=cry.pub.cek sponsor=`(^sein:title who)]
::
::  +parse-state: the networking state a %state leaf re-attests.  A claimed
::  sponsor is honored ONLY if its consent signature verifies (+consent-ok);
::  otherwise it is dropped, so a %state can never forge a sponsor.
::
++  parse-state
  |=  [who=ship known=(set ship) =reveal:urb]
  ^-  (unit gw-state:urb)
  ?~  parsed=(parse-leaf leaf-script.reveal)  ~
  ?~  st=(find-single %state u.parsed)  ~
  ?>  ?=(%state -.u.st)
  ::  a claimed sponsor is honored only if it is PUBLIC (known to us);
  ::  absent or unknown, fall back to the STRUCTURAL sponsor (never a null
  ::  sponsor to Jael -- see urb-core / fx-to-udiffs), same default as spawn.
  =/  spo=(unit @p)
    ?~  sponsor.u.st  `(^sein:title who)
    ?.  (consent-ok who.u.sponsor.u.st known)  `(^sein:title who)
    `who.u.sponsor.u.st
  `[life=life.u.st key=key.u.st sponsor=spo]
::
::  +consent-ok: is a %state's claimed sponsor trustworthy?  We stipulate
::  that the sponsor must be PUBLIC -- already one of the points this agent
::  (Bob) knows from the chain (its unv-ids).  If Bob knows the sponsor we
::  trust the claim outright, with NO signature check: the sponsor's public
::  on-chain existence is the authorization.  If Bob does not know it, the
::  claim is dropped (the comet falls back to its structural sponsor).
::
++  consent-ok
  |=  [spo=@p known=(set ship)]
  ^-  ?
  (~(has in known) spo)
::
::  +parse-dat-sont: recover the spawn satpoint committed in the pass tweak.
::  dat = (can 0 (mat dom) [256 txid] (mat vout) (mat off) ~) -- so after the
::  mat-encoded domain at bit 0 come 256 bits of txid, then mat(vout), mat(off).
::  XX txid here is Causeway's dat encoding (display-hex as @ux); it must use the
::     SAME byte order as the node's tx ids the walk compares against.
::
++  parse-dat-sont
  |=  dat=@
  ^-  (unit sont:ord)
  %-  mole
  |.
  ^-  sont:ord
  =/  md   (rub 0 dat)
  =/  cur  p.md
  =/  txid  (cut 0 [cur 256] dat)  =.  cur  (add cur 256)
  =/  mv   (rub cur dat)           =.  cur  (add cur p.mv)
  =/  mo   (rub cur dat)
  [txid q.mv q.mo]
::
::  +parse-custody-log: cue the reveal log out of the pass's untweaked xtr.
::  Kernel-opaque + attacker-controlled, so virtualized.  ~ = unparsable.
::
++  parse-custody-log
  |=  xtr=@
  ^-  (unit custody-log:urb)
  ::  a confidential comet always carries at least entry-0 (the spawn); an
  ::  empty xtr is not a verifiable log (fail rather than accept an empty
  ::  chain).
  ?:  =(0 xtr)  ~
  %-  mole
  |.  ;;(custody-log:urb (cue xtr))
::
::  +walk-checks: the PURE custody walk.  Given the comet's name, its current
::  messaging key, the spawn satpoint (from dat), the custody log (from xtr) and
::  the fetched tx for each entry (aligned by index; ~ = fetch failed), verify
::  every link and fold to the latest committed state.  ~ on any failure.
::
++  walk-checks
  |=  $:  who=ship
          cry=@
          known=(set ship)
          spawn-sont=sont:ord
          log=custody-log:urb
          fetched=(list (unit tx:bc))
      ==
  ^-  (unit [tip=sont:ord state=gw-state:urb])
  ?~  log  ~
  ?~  fetched  ~
  ?.  =((lent log) (lent fetched))  ~
  ::  entry-0: the spawn commit.  Its tx must be the spawn tx (dat's txid), it
  ::  must carry a %spawn reveal, and that reveal must commit to the sat's home
  ::  output (spawn-sont).  Initial state comes from the %spawn.
  ?~  tx0=i.fetched  ~
  ?~  rev0=reveal.i.log  ~
  ?.  =(id.u.tx0 txid.spawn-sont)  ~
  ?.  (lth vout.spawn-sont (lent os.u.tx0))  ~
  ::  the sat's offset must lie within its home output's value (the bound
  ::  urb-core's is-sont-in-input enforces on chain; interior links get it
  ::  for free from index-to-sont, but entry-0's offset comes from dat).
  ?.  (lth off.spawn-sont value:(snag vout.spawn-sont os.u.tx0))  ~
  ?~  onchain0=(p2tr-at u.tx0 vout.spawn-sont)  ~
  ?.  =(u.onchain0 (out-key u.rev0))  ~
  ?~  state0=(parse-spawn-state who u.rev0)  ~
  =/  state=gw-state:urb  u.state0
  =/  sont=sont:ord  spawn-sont
  =/  entries  t.log
  =/  txs      t.fetched
  |-
  ^-  (unit [tip=sont:ord state=gw-state:urb])
  ?~  entries  `[sont state]
  ?~  txs  ~
  ?~  this=i.txs  ~
  =/  entry  i.entries
  ::  the fetched tx must be the one the entry names
  ?.  =(id.u.this txid.entry)  ~
  ::  continuity: input 0 key-path-spends the exact prior sat-carrying output
  ?~  inp=(snag-input 0 u.this)  ~
  ?.  =([txid.u.inp pos.u.inp] [txid.sont vout.sont])  ~
  ?.  (is-key-path witness.u.inp)  ~
  ::  track the sat to its landing output (input 0 => no input value precedes
  ::  it, so it enters at index = its offset).  ~ = fell into the miner fee.
  =/  landed  (index-to-sont:uc off.sont os.u.this)
  ?~  landed  ~
  =/  next=sont:ord  [id.u.this vout.landed off.landed]
  ::  a state-bearing entry: the landing output must commit the revealed leaf,
  ::  and its %state becomes the new authoritative state.  A bare entry
  ::  (reveal=~) is a pure custody move and leaves the state unchanged.
  =/  new-state=(unit gw-state:urb)
    ?~  reveal.entry  `state
    ?~  onchain=(p2tr-at u.this vout.landed)  ~
    ?.  =(u.onchain (out-key u.reveal.entry))  ~
    (parse-state who known u.reveal.entry)
  ?~  new-state  ~
  $(entries t.entries, txs t.txs, sont next, state u.new-state)
::
::  +to-jael-point: project the latest committed state onto Jael's $point,
::  storing the (verified) full incoming pass as the current life's key.
::  Per spec section 7 the confidential verdict is rift 0, fief ~.
::
++  to-jael-point
  |=  [=pass state=gw-state:urb]
  ^-  point:jael
  :*  rift=0
      life.state
      (my [life.state (sub (end 3 pass) 'a') pass] ~)
      sponsor.state
      ~
  ==
::
::  +verify: the async entrypoint.  Decode + validate the pass, fetch the
::  custody log's txs (each in its attested block, so no -txindex needed),
::  run +walk-checks, confirm the tip is unspent and the pass's key is current,
::  and produce the verdict point (or ~).
::
++  verify
  |=  [dom=@tas who=ship =pass known=(set ship) rpc=req-to:btcio]
  =/  m  (strand:strandio ,(unit point:jael))
  ^-  form:m
  =/  cac  (com:nu:cric:crypto pass)
  =/  cek  +<:cac
  ?.  ?=([%c *] cek)  (pure:m ~)
  =/  cry  cry.pub.cek
  ::  domain committed in the tweak must match what Jael routed
  ?.  =(dom `@tas`q:(rub 0 dat.tw.pub.cek))  (pure:m ~)
  ::  name must be the fingerprint of the tweaked key (re-derive, don't trust)
  ?.  =(who fig:ex:cac)  (pure:m ~)
  ?~  spawn-sont=(parse-dat-sont dat.tw.pub.cek)  (pure:m ~)
  ?~  log=(parse-custody-log xtr.tw.pub.cek)  (pure:m ~)
  ::  fetch each entry's tx in its attested block (height -> hash -> in-block)
  ;<  fetched=(list (unit tx:bc))  bind:m  (fetch-log rpc u.log)
  =/  walk  (walk-checks who cry known u.spawn-sont u.log fetched)
  ?~  walk  (pure:m ~)
  ::  the tip output must be unspent on our node
  ;<  live=(unit ?)  bind:m
    (get-tx-out:btcio rpc ~ txid.tip.u.walk vout.tip.u.walk)
  ?.  =(`%.y live)  (pure:m ~)
  ::  the pass's current messaging key must be the latest attested one
  ?.  =(cry key.state.u.walk)  (pure:m ~)
  (pure:m `(to-jael-point pass state.u.walk))
::
::  +fetch-log: resolve each entry's block-height to a hash and fetch the tx in
::  that block.  Preserves order + arity so +walk-checks can zip by index; a
::  failed fetch stays ~ and fails the walk at that link.
::
++  fetch-log
  |=  [rpc=req-to:btcio log=custody-log:urb]
  =/  m  (strand:strandio ,(list (unit tx:bc)))
  ^-  form:m
  =|  acc=(list (unit tx:bc))
  |-
  ^-  form:m
  ?~  log  (pure:m (flop acc))
  ;<  bh=(unit @ux)  bind:m  (get-block-hash:btcio rpc ~ block-height.i.log)
  ?~  bh  $(log t.log, acc [~ acc])
  ;<  tx=(unit tx:bc)  bind:m
    (get-raw-transaction-in-block:btcio rpc ~ txid.i.log u.bh)
  $(log t.log, acc [tx acc])
--
