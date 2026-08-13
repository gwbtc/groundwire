#!/usr/bin/env python3
"""killpeers.py <pier> -- disconnect every earth peer, one strand for all.

This replaces the `kill-peer-connections` poke, which DOES NOT EXIST at
node@063720b9 -- the ref we pin for .stale-branch.  That ref accepts
exactly four pokes:

    %log-info                        (unchanged, bare)
    %broadcast-transaction           (unchanged, bare)
    %bitcoin-client-connect-peer     (was %add-earth-peer)
    %bitcoin-client-disconnect-peer

There is no bulk "drop everything" among them, so the same effect has to
be assembled: scry the peer map out of /x/peers and poke a disconnect for
each address.

WHY THE RECOVERY NEEDS THIS AT ALL.  When the tcp-sidecar SIGSEGVs,
%bitcoin-client is never told -- it goes on believing its peers are live
and keeps waiting on sockets nobody is holding.  Re-seeding fresh peers
on top of that does not help, because the stale entries still occupy the
peer table.  They have to be dropped first, which is what the old poke
did in one shot and what this does one at a time.

One strand for all of them, like addpeers.py: each poke is a separate
event either way, but a single strand means a single conn.sock round
trip and a single timeout to reason about.
"""
import sys

sys.path.insert(0, "/opt/gw")
from gwharness.connsock import ConnSock


pier = sys.argv[1]

#  The map value is $earth-peer-info, which this does not need and does
#  not want to name -- `*` accepts whatever shape that type has at the
#  pinned ref, so a change to it cannot break the disconnect loop.  The
#  KEY is $earth-address, [net-id address port], and that is exactly what
#  %bitcoin-client-disconnect-peer takes.
body = (
    "=/  m  (strand ,vase)\n^-  form:m\n"
    ";<  our=@p   bind:m  get-our\n"
    ";<  now=@da  bind:m  get-time\n"
    "=/  pez  ;;([%all (map [@tas @ux @ud] *)] "
    ".^(* %gx /(scot %p our)/bitcoin-client/(scot %da now)/peers/noun))\n"
    "=/  eps  ~(tap by +.pez)\n"
    #  counted BEFORE the loop: inside the ?~ branch the list is ~,
    #  so (lent eps) there is always 0 and the report always lies.
    "=/  cnt  (lent eps)\n"
    "|-  ^-  form:m\n"
    "?~  eps  (pure:m !>((crip (weld \"disconnected \" (scow %ud cnt)))))\n"
    ";<  ~  bind:m  "
    "(poke-our %bitcoin-client %bitcoin-client-disconnect-peer !>(p.i.eps))\n"
    "$(eps t.eps)\n"
)
print(ConnSock(pier, timeout=600).khan_eval(body))
