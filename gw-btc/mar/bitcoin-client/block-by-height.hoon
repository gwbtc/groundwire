::  mar/bitcoin-client/block-by-height.hoon
::
::  Pass-through mark for a fact given by the local light client
::  (gwbtc/node's %bitcoin-client).  node ships its own copy under the
::  same mar/bitcoin-client/ path, but marks resolve per-DESK: a
::  subscriber living in THIS desk cannot
::  take the fact unless Clay can build the mark HERE -- and the failure
::  is not local: an unbuildable mark on an incoming fact takes spider
::  down entirely ("spider crashed, killing all strands"), killing every
::  concurrent thread on the ship.
::
::  The path is load-bearing: a mark is named for where it lives, so
::  this file MUST sit at mar/bitcoin-client/ to answer to
::  %bitcoin-client-*.  It read %* from a flat mar/ until
::  node@063720b9 moved its own marks under mar/bitcoin-client/ and
::  renamed every fact with them.
::
/-  lc=light-client
|_  u=block-by-height:update:lc
++  grab
  |%
  ++  noun  block-by-height:update:lc
  --
++  grow
  |%
  ++  noun  u
  --
++  grad  %noun
--
