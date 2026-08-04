::  mar/is-synced.hoon
::
::  Pass-through mark for a fact given by the local light client
::  (gwbtc/node's %bitcoin-client).  The node's desk ships no mar/ files
::  for its own fact marks, and a subscriber living in THIS desk cannot
::  take the fact unless Clay can build the mark HERE -- and the failure
::  is not local: an unbuildable mark on an incoming fact takes spider
::  down entirely ("spider crashed, killing all strands"), killing every
::  concurrent thread on the ship.
::
/-  lc=light-client
|_  u=is-synced:update:lc
++  grab
  |%
  ++  noun  is-synced:update:lc
  --
++  grow
  |%
  ++  noun  u
  --
++  grad  %noun
--
