"""Lane injection + reachability probe for two-comet first contact.

On a real network the runtime discovers lanes; on this isolated regtest net we
inject them directly with ames's `%dear` task over conn.sock, exactly as vere
would. With the pier booted `-L`, outbound sends are rewritten to loopback, so
only the destination PORT has to be right — we still encode 127.0.0.1 for shape.

Ordering matters: `+sy-dear` only records the lane if the peer is ALREADY a
`%known` ames peer. A comet becomes known once its self-attestation packet has
been verified (urb-watcher -> jael %public-keys -> ames). So C-M2 cross-verifies
packets first, THEN injects lanes, THEN probes with `|hi` (a poke of the remote
ship's %hood whose positive ack proves a full ames round-trip).
"""

from __future__ import annotations

from . import noun as N


def lane_atom(port: int, ip: str = "127.0.0.1") -> int:
    """Encode an ames direct lane address: IPv4 in the low 32 bits, port in
    bits 32..47 — matching +sy-dear's `(end [0 32])` / `(cut 0 [32 16])`."""
    a, b, c, d = (int(x) for x in ip.split("."))
    ipnum = (a << 24) | (b << 16) | (c << 8) | d
    return ipnum | (port << 32)


def inject_lane(ship, peer_num: int, peer_port: int, ip: str = "127.0.0.1") -> str:
    """Inject `[%dear peer [%| addr]]` into ship's ames vane via conn %ovum.
    Returns the conn ack ('done'/'news')."""
    addr = lane_atom(peer_port, ip)
    card = (N.tas("dear"), (peer_num, (1, addr)))      # [%dear ship [%.n addr]]
    return ship.conn.ovum("a", ["ames"], card)


_HI = (
    "=/  m  (strand ,vase)  "
    ";<  ~  bind:m  (poke [{tgt} %hood] %helm-hi !>('gw-first-contact'))  "
    "(pure:m !>('hi-ok'))"
)


def _cord(v: object) -> str:
    """Decode a khan_eval @t result. The strand returns `!>('hi-ok')`, whose
    vase value arrives as a raw atom (LSB-first cord bytes); turn it back into
    the string. Pass through anything already a str (e.g. an error marker)."""
    if isinstance(v, str):
        return v
    if isinstance(v, int):
        return v.to_bytes((v.bit_length() + 7) // 8, "little").decode("latin-1")
    return str(v)


def hi_probe(ship, peer_patp: str) -> str:
    """Run `|hi peer` on `ship` (pokes the remote %hood over ames). Returns
    'hi-ok' iff the poke is positively acked — i.e. the peer received it.
    Raises ConnError / times out if unreachable."""
    body = _HI.format(tgt=peer_patp)
    return _cord(ship.conn.khan_eval(body))
