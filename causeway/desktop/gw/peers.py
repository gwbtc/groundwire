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
import time

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


def add_peers(ship: Ship, ips: list[str], timeout: float = 300.0) -> str:
    """One strand for the whole batch; returns the product ('done')."""
    if not ips:
        return ""
    vals = " ".join(ip_hoon(ip) for ip in ips)
    body = (
        f"=/  ips=(list @ux)  ~[{vals}]\n"
        "|-  ^-  form:m\n"
        "?~  ips  (pure:m !>('done'))\n"
        ";<  ~  bind:m  (poke-our %bitcoin-client %bitcoin-client-action "
        "!>([%connect-peer [%ipv4 i.ips 8.333]]))\n"
        "$(ips t.ips)\n"
    )
    r = ship.khan(body, timeout)
    return N.from_cord(r) if isinstance(r, int) else str(r)


def seed(ship: Ship, batches: int = 6, size: int = 25, log=print) -> int:
    """Seed a fresh light client; returns how many addresses were handed over."""
    if pool_left(ship.name) < batches * size:
        log("resolving x49.* DNS seeds (NODE_COMPACT_FILTERS only)")
        added = pool_fill(6)
        log(f"pool: +{added} new addresses")
    left = pool_left(ship.name)
    if left == 0:
        raise RuntimeError("no peer addresses: DNS seeding returned nothing, and the light "
                           "client connects only to addresses handed to it")
    handed = 0
    for i in range(batches):
        batch = take(ship.name, size)
        if not batch:
            break
        add_peers(ship, batch)
        handed += len(batch)
        log(f"batch {i + 1}: {len(batch)} peers")
        if i + 1 < batches:
            time.sleep(10)
    return handed
