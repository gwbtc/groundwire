"""Bitcoin peers for the light client.

%bitcoin-client connects only to addresses poked in; there is no DNS seeding
inside it.  Filter headers are served only by peers advertising
NODE_COMPACT_FILTERS, so the seeds are asked with the x49 service-bit filter
(NODE_NETWORK | NODE_WITNESS | NODE_COMPACT_FILTERS).  Each seed answers a
small random slice per query, so ask several times.
"""

from __future__ import annotations

import ipaddress
import socket

from . import noun as N
from .layout import var_dir
from .ship import Ship, ip_hoon

SEEDS = [
    "seed.bitcoin.sipa.be", "dnsseed.bluematt.me", "seed.bitcoinstats.com",
    "seed.bitcoin.jonasschnelli.ch", "dnsseed.emzy.de", "seed.bitcoin.wiz.biz",
    "seed.btc.petertodd.net", "seed.bitcoin.sprovoost.nl", "seed.mainnet.achownodes.xyz",
    "dnsseed.bitcoin.dashjr-list-of-p2p-nodes.us",
]


def resolve_a(host: str) -> list[str]:
    try:
        return sorted({r[4][0] for r in socket.getaddrinfo(host, 8333, socket.AF_INET)})
    except OSError:
        return []


def _public(ip: str) -> bool:
    try:
        a = ipaddress.ip_address(ip)
    except ValueError:
        return False
    return not (a.is_private or a.is_loopback or a.is_link_local or a.is_multicast or a.is_reserved)


def _pool_file():
    return var_dir() / "peerpool.txt"


def _used_file(name: str):
    return var_dir() / f"used-{name}.txt"


def _read(p) -> list[str]:
    try:
        return [x.strip() for x in p.read_text().splitlines() if x.strip()]
    except OSError:
        return []


def pool_fill(rounds: int = 6) -> int:
    pool = set(_read(_pool_file()))
    before = len(pool)
    for _ in range(rounds):
        for s in SEEDS:
            for ip in resolve_a("x49." + s):
                if _public(ip):
                    pool.add(ip)
    _pool_file().parent.mkdir(parents=True, exist_ok=True)
    _pool_file().write_text("\n".join(sorted(pool)) + ("\n" if pool else ""))
    return len(pool) - before


def pool_left(name: str) -> int:
    used = set(_read(_used_file(name)))
    return sum(1 for ip in _read(_pool_file()) if ip not in used)


def take(name: str, n: int) -> list[str]:
    """Addresses this ship has not been handed before: a burned seed stays
    blacklisted on the ship for days."""
    used = set(_read(_used_file(name)))
    ips = [ip for ip in _read(_pool_file()) if ip not in used][:n]
    if ips:
        with open(_used_file(name), "a") as fh:
            fh.write("\n".join(ips) + "\n")
    return ips


CONNECT = 10
"""How many of the handed addresses the client dials at once: %bitcoin-client's
own target-peers (its default).  Every address goes into its pool with
%add-address; +connect-to-more-peers draws replacements from that pool when a
peer drops or a peer's addr message arrives -- but only then.  A cold start
fed nothing but %add-address never dials, so the first few are %connect-peer.
Dialing all 150 at once was the old behaviour: 150 TCP sessions from a laptop
for a client that keeps ten."""


def add_peers(ship: Ship, ips: list[str], connect: int = CONNECT, timeout: float = 300.0) -> str:
    """One strand for the whole batch: %add-address for every address, then
    %connect-peer for the first `connect` of them; returns the product ('done')."""
    if not ips:
        return ""
    pool = " ".join(ip_hoon(ip) for ip in ips)
    dial = " ".join(ip_hoon(ip) for ip in ips[:max(0, connect)])
    body = (
        f"=/  pool=(list @ux)  ~[{pool}]\n"
        f"=/  dial=(list @ux)  {'~[' + dial + ']' if dial else '~'}\n"
        "|-  ^-  form:m\n"
        "?^  pool\n"
        "  ;<  ~  bind:m  (poke-our %bitcoin-client %bitcoin-client-action "
        "!>([%add-address [%ipv4 i.pool 8.333]]))\n"
        "  $(pool t.pool)\n"
        "?~  dial  (pure:m !>('done'))\n"
        ";<  ~  bind:m  (poke-our %bitcoin-client %bitcoin-client-action "
        "!>([%connect-peer [%ipv4 i.dial 8.333]]))\n"
        "$(dial t.dial)\n"
    )
    r = ship.khan(body, timeout)
    return N.from_cord(r) if isinstance(r, int) else str(r)


def seed(ship: Ship, count: int = 150, connect: int = CONNECT, log=print) -> int:
    """Seed a light client: `count` addresses into its pool, the first `connect`
    dialed now.  Returns how many addresses were handed over."""
    if pool_left(ship.name) < count:
        log("resolving x49.* DNS seeds (NODE_COMPACT_FILTERS only)")
        added = pool_fill(6)
        log(f"pool: +{added} new addresses")
    left = pool_left(ship.name)
    if left == 0:
        raise RuntimeError("no peer addresses: DNS seeding returned nothing, and the light "
                           "client connects only to addresses handed to it")
    batch = take(ship.name, count)
    add_peers(ship, batch, connect=connect)
    dialing = min(max(0, connect), len(batch))
    log(f"{len(batch)} addresses into the client's pool; dialing {dialing} now "
        f"(it keeps {CONNECT} live and tops up from the pool)")
    return len(batch)
