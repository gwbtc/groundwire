::  sur/self-attestation.hoon
::
::  The self-attestation packet for CONFIDENTIAL COMETS (protocol 2.0).
::
::  A confidential Groundwire comet does NOT reveal its `urb` Taproot inscription
::  on-chain. Instead, on first contact it hands a peer this packet off-chain.
::  The peer's Ames pokes it to %urb-watcher (the registered handler agent for
::  the Bitcoin protocol), which fetches the referenced transactions from a
::  Bitcoin node, verifies that they form an unbroken on-chain ownership chain
::  for a single sat — from the comet's %spawn through to the most recent UTXO —
::  and on success stores the resulting PKI state in Jael and watches the
::  ownership sat as new blocks come in. The same packet format doubles as the
::  comet's own "attestation keyfile", poked into its own %urb-watcher so the
::  ship can serve packets to peers and track its own PKI state.
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
::  merkle root must equal the hash of the tapleaf. This packet discloses, per
::  spent output, the internal key `P` and the lone `tapleaf`; that is exactly
::  enough for the verifier to recompute `Q` and confirm the on-chain output
::  committed to that one `urb` sotx, without anyone ever performing a
::  script-path spend.
::
::  2.0 chain rules (enforced by lib/self-attestation):
::    - NO GAPS: every link carries at least one sotx; otherwise-plain sat
::      transfers must use the %no-op sotx.
::    - SELF-ENACTED: every sotx in the chain is from `who` (a confidential
::      chain is one sat's history, authored by its owner).
::    - The %spawn appears exactly once, as the first single of link 0.
::    - The single sat is tracked by offset through every link; the chain ends
::      at `tip`, which must be currently unspent.
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
::  +reveal: the off-chain disclosure for ONE spent taproot output.
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
::  +link: one transaction in the ownership chain.
::
::    Each link spends the sat-bearing output of the previous link (or, for the
::    genesis link, of the spawn commit transaction). `in` is the index of the
::    input doing that spend. `reveal` discloses how the spent output's taproot
::    key was built; `sots` is the action(s) that output's leaf encodes (the
::    verifier re-parses the leaf script and checks it matches).
::
+$  link
  $:  =txid:ord               :: this transaction (fetched via get-raw-transaction)
      in=vout:ord             :: input index spending the prior committed output
      =reveal                  :: commitment disclosed for that prior output
      sots=(list raw-sotx:urb) :: sotx(es) this link enacts (must match the leaf)
  ==
::
::  +self-attestation: the full packet.
::
::    `chain` is ordered oldest-first: the genesis (spawn) link, then every
::    subsequent transfer, up to the link whose output is the current `tip`.
::    `precommit` is the txid of the pre-commit transaction, needed to
::    reproduce the spawn check (the networking-key tweak binds to the
::    precommit satpoint, and the commit transaction must spend it).
::
+$  self-attestation
  $:  who=@p                  :: the comet identity being attested
      precommit=txid:ord      :: pre-commit tx (for the genesis spawn/tweak check)
      chain=(list link)       :: spawn -> ... -> tip, oldest first
      tip=sont:ord            :: most-recent UTXO; must be currently unspent
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
--
