::  ted/conf/attest.hoon
::
::  Harness helper: build a self-attestation packet from a plain SKELETON
::  (only on-chain-derivable data) and poke it into %urb-watcher.
::
::  The skeleton carries no sotx — this thread re-derives each link's sots
::  from its leaf script via parse-leaf:sal, so they match the leaf by
::  construction (the same parse the verifier uses). The harness builds the
::  skeleton noun in python (testnet/gwharness/packets.py) and runs this thread
::  over the conn.sock %fyrd interface.
::
::  Input (unit-wrapped, like the other ted threads):
::    `[target=?(%keyfile %peer) =skeleton]
::  target %keyfile -> mark %attestation-keyfile (our own chain),
::  target %peer    -> mark %self-attestation   (simulate a peer's packet
::                                                until kernel-direct Ames lands).
::
/-  spider, sa=self-attestation, bitcoin, ord, urb
/+  strandio, sal=self-attestation
=>
|%
+$  skel-link
  $:  txid=@ux            ::  this commit tx (display-order @ux)
      block=@ux           ::  hash of its containing block
      ikey=@ux            ::  33-byte compressed internal pubkey
      ver=@ux             ::  tapleaf version (0xc0)
      wid=@ud             ::  leaf script byte width
      dat=@ux             ::  leaf script bytes
  ==
+$  skeleton
  $:  who=@p
      precommit=[txid=@ux block=@ux]
      links=(list skel-link)
      tip=[txid=@ux vout=@ud off=@ud]
  ==
--
^-  thread:spider
|=  args=vase
=/  m  (strand:strandio ,vase)
^-  form:m
=/  [target=?(%keyfile %peer) =skeleton]
  (need !<((unit [?(%keyfile %peer) skeleton]) args))
=/  chain=(list link:sa)
  %+  turn  links.skeleton
  |=  sl=skel-link
  ^-  link:sa
  =/  script=hexb:bitcoin  [wid.sl dat.sl]
  =/  parsed  (parse-leaf:sal script)
  ?~  parsed  ~|([%unparsable-leaf txid.sl] !!)
  [txid.sl block.sl [ikey.sl [ver.sl script]] u.parsed]
=/  sat=self-attestation:sa
  [who.skeleton precommit.skeleton chain tip.skeleton]
=/  mark=@tas  ?:(=(%keyfile target) %attestation-keyfile %self-attestation)
;<  ~  bind:m  (poke-our:strandio %urb-watcher mark !>(sat))
(pure:m !>(`@t`'attested'))
