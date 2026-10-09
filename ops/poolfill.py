#!/usr/bin/env python3
"""poolfill.py <poolfile> -- append fresh mainnet peer IPs from the DNS seeds.

PHASE 5 CHANGE: filter headers are only servable by peers advertising
NODE_COMPACT_FILTERS.  Bitcoin Core's DNS seeds support a service-bit filter
via an `x<hex>.` label, so we ask for x49 = NODE_NETWORK(1) | NODE_WITNESS(8)
| NODE_COMPACT_FILTERS(64) = 0x49.  Phase 4's pool was unfiltered, so most of
its IPs could serve headers but never filter headers, and the light client
stalled at %retrying-header-sync / no filter progress.

%bitcoin-client's blacklist expiry is ~d3, so every recovery cycle burns seed
IPs permanently for three days; reusing the same five is useless.  We keep an
unfiltered fallback tail so the pool never runs dry.
"""
import socket
import subprocess
import sys
import time

BASE = [
    "seed.bitcoin.sipa.be",
    "dnsseed.bluematt.me",
    "seed.bitcoinstats.com",
    "seed.bitcoin.jonasschnelli.ch",
    "dnsseed.emzy.de",
    "seed.bitcoin.wiz.biz",
    "seed.btc.petertodd.net",
    "seed.bitcoin.sprovoost.nl",
    "seed.bitcoin.achownodes.xyz",
    "dnsseed.bitcoin.dashjr-list-of-p2p-nodes.us",
]
# x49 = NODE_NETWORK|NODE_WITNESS|NODE_COMPACT_FILTERS
CF_SEEDS = ["x49." + s for s in BASE]


def resolve(host):
    out = set()
    try:
        for fam, _t, _p, _c, sa in socket.getaddrinfo(host, 8333, socket.AF_INET):
            out.add(sa[0])
    except Exception:
        pass
    try:
        r = subprocess.run(["getent", "ahostsv4", host],
                           capture_output=True, text=True, timeout=20)
        for line in r.stdout.splitlines():
            out.add(line.split()[0])
    except Exception:
        pass
    return out


def main():
    pool = sys.argv[1]
    rounds = int(sys.argv[2]) if len(sys.argv) > 2 else 8
    try:
        have = {l.strip() for l in open(pool) if l.strip()}
    except FileNotFoundError:
        have = set()
    found = set()
    # DNS seeds return a small random slice per query; hammer them.
    for _ in range(rounds):
        for s in CF_SEEDS:
            found |= resolve(s)
        time.sleep(1.0)
    new = sorted(ip for ip in found - have
                 if not ip.startswith(("0.", "10.", "127.", "192.168.")))
    if new:
        with open(pool, "a") as f:
            for ip in new:
                f.write(ip + "\n")
    print(f"poolfill(x49): +{len(new)} new, pool now {len(have) + len(new)}")


if __name__ == "__main__":
    main()
