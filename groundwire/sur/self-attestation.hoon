::  sur/self-attestation.hoon
::
::  The self-attestation packet for CONFIDENTIAL COMETS (protocol 2.0).
::
::  A confidential Groundwire comet does NOT reveal its `urb` Taproot inscription
::  on-chain. Instead, on first contact it hands a peer this packet off-chain.
::  The peer's Ames pokes it to %urb-watcher (the registered handler agent for
::  the Bitcoin protocol), which fetches the referenced transactions from a
::  Bitcoin node, verifies that they form an unbroken on-chain ownership chain
::  for a single sat — from the comet's %spawn commit through to the most
::  recent UTXO — and on success stores the resulting PKI state in Jael and
::  watches the ownership sat as new blocks come in. The same packet format
::  doubles as the comet's own "attestation keyfile", poked into its own
::  %urb-watcher so the ship can serve packets to peers and track its own PKI
::  state.
::
::  The crux of confidentiality: every spend in the chain is an ordinary Taproot
::  KEY-PATH spend (a lone Schnorr signature — nothing about the script tree is
::  on-chain). The committed `urb` leaf is baked into each output key via the
::  standard TapTweak:
::
::      Q = P + tagged_hash("TapTweak", x(P) || merkle_root) * G
::
::  Here `merkle_root` is the hash of a SINGLE-LEAF tapscript tree, so it equals
::  `leaf-hash(tapleaf)` and the tree is provably sparse (one valid spend-path)
::  by construction — we omit merkle proofs in favor of stipulating that the
::  merkle root must equal the hash of the tapleaf. Each link discloses the
::  internal key `P` and the lone `tapleaf` of its OWN sat-carrying output;
::  that is exactly enough for the verifier to recompute `Q` and confirm the
::  on-chain output committed to that one `urb` sotx, without anyone ever
::  performing a script-path spend.
::
::  2.0 chain rules (enforced by lib/self-attestation):
::    - A link IS a commit transaction, paired with the reveal of ITS OWN
::      sat-carrying output's leaf and the sotx(es) that leaf encodes. The
::      leaf is enacted at packet-verification time; an on-chain script-path
::      reveal of the same leaf is what enacts it for chain watchers if the
::      ship later goes public. So the latest sotx (the tip leaf's) is
::      disclosed directly, and a bare %spawn is a valid one-link keyfile.
::    - The sat-carrying input of every link is input 0, by stipulation.
::      (This makes the sat's landing index in a link's outputs simply its
::      offset entering the tx — no prior-input value sums are needed.)
::    - NO GAPS: every link carries at least one sotx; otherwise-plain sat
::      transfers must use the %no-op sotx.
::    - SELF-ENACTED: every sotx in the chain is from `who` (a confidential
::      chain is one sat's history, authored by its owner).
::    - The %spawn appears exactly once, as the first single of link 0.
::    - The single sat is tracked by offset through every link; the chain ends
::      at `tip`, the sat's landing in the LAST link's outputs, which must be
::      currently unspent.
::    - Every referenced tx carries the hash of its containing block, so
::      verifiers can use getrawtransaction's blockhash argument and the node
::      needs no -txindex. A lying blockhash simply fails the fetch — Core
::      validates the tx's inclusion in the named block.
::    - Verifiers that already track a sont for `who` reconcile the packet
::      against it (the tracked sont must be the tip or an interior link).
::
::  We deliberately ship only txids + the off-chain reveal data; the verifier
::  re-fetches the full transactions from the node to compare against. We
::  continue to rely on bootstrapping the chain via a precommit/funding
::  transaction due to the recursive hash infeasibility of including a %spawn
::  commit txid inside itself.
::
/-  bitcoin, ord, urb
|%
::  +tapleaf: a Taproot leaf — leaf version + the tapscript bytes.
::  Structurally identical to tapleaf:taproot so it nests there directly.
::  For Groundwire the script is the `urb` envelope carrying the sotx(es).
::
+$  tapleaf  [version=@ux script=hexb:bitcoin]
::
::  +reveal: the off-chain disclosure for ONE committed taproot output.
::
::    Exactly one leaf and NO merkle proof. The committed merkle root must
::    therefore equal (leaf-hash tapleaf) — a single-leaf tree, sparse by
::    construction. internal-key is the 33-byte compressed internal pubkey P;
::    it never appears on-chain (key-path spends commit only to the tweaked
::    output key Q), so disclosing it here is what lets the verifier recompute Q.
::
+$  reveal
  $:  internal-key=@ux        :: compressed internal pubkey P (33 bytes)
      =tapleaf                 :: the single committed `urb` leaf
  ==
::
::  +link: one commit transaction in the ownership chain.
::
::    A link IS a commit: its input 0 (the sat-carrying input, by stipulation)
::    spends the sat-carrying output of the previous link (or, for the genesis
::    link, of the precommit/funding tx). `reveal` discloses how THIS tx's own
::    sat-carrying output's taproot key was built; `sots` is the action(s)
::    that output's leaf encodes (the verifier re-parses the leaf script and
::    checks it matches), enacted at packet-verification time. `block` lets
::    the verifier fetch the tx by blockhash, so the node needs no -txindex.
::
::    HARD CONSTRUCTION RULE (enforced by the verifier, REQUIRED of any tooling
::    that builds keyfiles -- wallet, Causeway): the sat-carrying input MUST be
::    INPUT 0 of every link, and the precommit spend MUST be input 0 of the
::    genesis link. The verifier cannot read input VALUES (the inputw mold has
::    none), so it tracks the sat by assuming zero preceding-input value -- only
::    true at input 0. urb-core tracks a sat in whatever input carries it; this
::    verifier is EXACTLY equivalent to urb-core when the sat is at input 0, and
::    REJECTS an otherwise-valid commit whose sat is in a non-zero input. Build
::    accordingly or the keyfile will (safely) fail verification.
::
+$  link
  $:  =txid:ord               :: this commit transaction
      block=@ux               :: hash of the block containing it
      =reveal                  :: commitment of THIS tx's sat-carrying output
      sots=(list raw-sotx:urb) :: sotx(es) the leaf encodes (must match it)
  ==
::
::  +self-attestation: the full packet.
::
::    `chain` is ordered oldest-first: the spawn commit (genesis link), then
::    every subsequent transfer commit, up to the link whose own output is the
::    current `tip` (a bare %spawn is a valid one-link chain). `funding` is
::    the funding tx that the spawn commit spends: the networking-key tweak
::    binds to a funding satpoint, and link 0's input 0 must spend it.
::
+$  self-attestation
  $:  who=@p
      funding=[=txid:ord block=@ux]  :: the funding tx the spawn commit spends
      chain=(list link)                 :: spawn commit -> ... -> tip commit
      tip=sont:ord                      :: the sat's landing in the LAST link's outputs
  ==
::
::  +check: one named verification step and whether it passed.
+$  check  [name=cord ok=?]
::
::  +verdict: the verifier's per-packet result.
::    `ok` is the conjunction of every check; `checks` is the per-step breakdown.
::
+$  verdict
  $:  who=@p
      ok=?
      checks=(list check)
  ==
::
::  +result: the verify thread's full product.
::
::    `point` is the PKI state obtained by replaying the chain's sotxes
::    (own.sont already set to the packet tip) — only meaningful when
::    ok.verdict. `tip-value` is the full on-chain value of the tip's output,
::    which the caller needs for put-com:si:ol (and which find-block-reveals
::    later reuses for sum-in math, so it must be the real on-chain value);
::    0 when not derivable.
::
+$  result
  $:  =verdict
      point=(unit point:urb)
      tip-value=@ud
  ==
::
::  +skeleton: the on-chain-derivable subset of a packet, as built off-ship
::  (e.g. by Causeway). The sotx in each link are NOT carried — they are
::  re-derived on-ship from the leaf script via parse-leaf, so they match the
::  leaf by construction. lib/self-attestation's +from-skeleton turns this into
::  a full self-attestation; the harness POSTs the jammed skeleton to
::  %urb-watcher's eyre endpoint.
::
+$  skel-link
  $:  =txid:ord           ::  this commit tx
      block=@ux           ::  hash of its containing block
      internal-key=@ux    ::  33-byte compressed internal pubkey
      version=@ux         ::  tapleaf version (0xc0)
      script=hexb:bitcoin ::  the leaf script bytes
  ==
+$  skeleton
  $:  who=@p
      funding=[=txid:ord block=@ux]
      links=(list skel-link)
      tip=sont:ord
  ==
--
