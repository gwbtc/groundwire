::  mar/bitcoin-client/update.hoon
::
::  Pass-through mark for every fact the local light client (gwbtc/node's
::  %bitcoin-client, from develop 29a84c8 on) gives: one mark for all of
::  them, carrying a $bitcoin-client-update.  It has to exist in THIS desk,
::  as the per-fact marks beside it did: a subscriber here cannot take a
::  fact whose mark Clay cannot build here, and an unbuildable mark on a
::  fact to a thread takes spider down.  See sur/light-client.hoon.
::
/-  lc=light-client
|_  u=bitcoin-client-update:lc
++  grab
  |%
  ++  noun  bitcoin-client-update:lc
  --
++  grow
  |%
  ++  noun  u
  --
++  grad  %noun
--
