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
    #  The key's head is clammed to the UNION, not to @tas.
    #
    #  node's poke arm does `!<(earth-address vaz)`, and $earth-address's
    #  head is $network-address-id -- ?(%ipv4 %ipv6 %torv2 %torv3 %i2p
    #  %cjdns %yggdrasil). A vase typed [@tas @ux @ud] does NOT nest under
    #  that: @tas is WIDER than the union, so !< on the far side refuses
    #  it and the strand dies %nest-fail.
    #
    #  It has to be ;; and not a ^- cast: `[?(%ipv4 ...) ...]`x on an
    #  @tas-typed x is itself a nest-fail (verified: -need.?(%cjdns %i2p
    #  %ipv4 ...) -have.@tas). ;; normalises instead of requiring nesting,
    #  and still refuses a genuinely wrong term.
    #
    #  Reading with the union also means the loop below hands the poke a
    #  correctly-typed key with no further work.
    "=/  pez  ;;([%all (map "
    "[?(%ipv4 %ipv6 %torv2 %torv3 %i2p %cjdns %yggdrasil) @ux @ud] *)] "
    ".^(* %gx /(scot %p our)/bitcoin-client/(scot %da now)/peers/noun))\n"
    "=/  eps  ~(tap by +.pez)\n"
    #  counted BEFORE the loop: inside the ?~ branch the list is ~,
    #  so (lent eps) there is always 0 and the report always lies.
    "=/  cnt  (lent eps)\n"
    "|-  ^-  form:m\n"
    "?~  eps  (pure:m !>((crip (weld \"disconnected \" (scow %ud cnt)))))\n"
    #  p.i.eps now carries the union type from the read above, so this
    #  needs no cast of its own.
    #
    #  Note how this hid: with an EMPTY peer list the poke never runs, so
    #  nothing is ever type-checked and the strand reports success. It
    #  failed on f1 (52 peers) and f2 (10) and "passed" where there was
    #  nothing to disconnect. A loop whose body never executes is not a
    #  passing test -- the same trap as a test that never reaches the code
    #  it is written for.
    ";<  ~  bind:m  "
    "(poke-our %bitcoin-client %bitcoin-client-disconnect-peer !>(p.i.eps))\n"
    "$(eps t.eps)\n"
)
print(ConnSock(pier, timeout=600).khan_eval(body))
