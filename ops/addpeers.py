#!/usr/bin/env python3
"""addpeers.py <pier> <ip> [ip...] -- seed earth peers, one strand for all.

The mark is %bitcoin-client-connect-peer.  It was %add-earth-peer until
node@063720b9 -- the ref we pin for .stale-branch -- moved every mark
under mar/bitcoin-client/ and renamed all of them.  The desk's FACT marks
were fixed in b064928; this is the same rename one layer out, on the
POKE side, and it is why peer seeding failed with %thread-fail while the
agent itself looked healthy: a ship with zero peers never receives a
best-block fact, so "no unexpected subscription update" was vacuously
true and the cursor simply never moved.

The payload is unchanged.  $earth-address is [net-id address port] and
%ipv4 is still %ipv4; only the mark's name moved.
"""
import sys
sys.path.insert(0, "/opt/gw")
from gwharness.connsock import ConnSock


def hoonhex(v):
    h = f"{v:x}"
    out = []
    while len(h) > 4:
        out.append(h[-4:]); h = h[:-4]
    out.append(h)
    return "0x" + ".".join(reversed(out))


pier, ips = sys.argv[1], sys.argv[2:]
vals = []
for ip in ips:
    a, b, c, d = (int(x) for x in ip.split("."))
    vals.append(hoonhex((a << 24) | (b << 16) | (c << 8) | d))
body = ("=/  m  (strand ,vase)\n^-  form:m\n"
        ";<  our=@p   bind:m  get-our\n;<  now=@da  bind:m  get-time\n"
        "=/  ips=(list @ux)  ~[" + " ".join(vals) + "]\n"
        "|-  ^-  form:m\n"
        "?~  ips  (pure:m !>('done'))\n"
        ";<  ~  bind:m  (poke-our %bitcoin-client %bitcoin-client-connect-peer !>([%ipv4 i.ips 8.333]))\n"
        "$(ips t.ips)\n")
print(len(ips), "peers ->", ConnSock(pier, timeout=600).khan_eval(body))
