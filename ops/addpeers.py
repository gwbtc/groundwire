#!/usr/bin/env python3
"""addpeers.py <pier> <ip> [ip...] -- %add-earth-peer, one strand for all."""
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
        ";<  ~  bind:m  (poke-our %bitcoin-client %add-earth-peer !>([%ipv4 i.ips 8.333]))\n"
        "$(ips t.ips)\n")
print(len(ips), "peers ->", ConnSock(pier, timeout=600).khan_eval(body))
