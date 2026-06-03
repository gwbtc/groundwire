::  sur/self-attestation.hoon
::
::  The self-attestation packet for CONFIDENTIAL COMETS.
::
::  A confidential Groundwire comet does NOT reveal its `urb` Taproot inscription
::  on-chain. Instead, on first contact it hands a peer this packet off-chain. The
::  peer pokes it to %urb-handler, which fetches the referenced transactions from a
::  Bitcoin node and verifies that they form an unbroken on-chain ownership chain
::  for a single sat — from the comet's %spawn through to the most recent UTXO.
::
::  The crux of confidentiality: every spend in the chain is an ordinary Taproot
::  KEY-PATH spend (a lone Schnorr signature — nothing about the script tree is on
::  chain). The committed `urb` leaf is baked into each output key via the standard
::  TapTweak:
::
::      Q = P + tagged_hash("TapTweak", x(P) || merkle_root) * G
::
::  Here `merkle_root` is the hash of a SINGLE-LEAF tapscript tree, so it equals
::  `leaf-hash(tapleaf)` and the tree is provably sparse (one valid spend-path) by
::  construction — there is no room for a second, hidden spend-path. This packet
::  discloses, per spent output, the internal key `P` and the lone `tapleaf`; that
::  is exactly enough for the verifier to recompute `Q` and confirm the on-chain
::  output committed to that one `urb` sotx, without anyone ever performing a
::  script-path spend.
::
::  We deliberately ship only txids + the off-chain reveal data; %urb-handler
::  re-fetches the full transactions from the node to compare against.
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
::    subsequent transfer (plain transfers carry a %no-op sotx so there are no
::    gaps), up to the link whose output is the current `tip`. `precommit` is the
::    txid of the pre-commit transaction, needed to reproduce urb-core's spawn
::    check (the networking-key tweak binds to the precommit satpoint).
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
::  +verdict: %urb-handler's result for an attestation.
::    `ok` is the conjunction of every check; `checks` is the per-step breakdown.
::    (Deriving and storing the resulting Jael point is intentionally out of
::    scope for this prototype — see the agent's header.)
::
+$  verdict
  $:  who=@p
      ok=?
      checks=(list check)
  ==
--
