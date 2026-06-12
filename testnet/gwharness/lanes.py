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


def inject_attest_verdict(ship, peer_num: int, ok: bool) -> str:
    """Inject the kernel task [%attest-verdict ship ok] into ames via conn
    %ovum — exactly what %urb-watcher's verdict-poke passes once wired, letting
    the gate test drive verdicts without the desk. ok is a loobean: %.y=0."""
    card = (N.tas("attest-verdict"), (peer_num, 0 if ok else 1))
    return ship.conn.ovum("a", ["attest", "verdict"], card)


def inject_attest_request(ship, peer_num: int) -> str:
    """Inject [%attest-request ship] into ames (re-attestation grace)."""
    card = (N.tas("attest-request"), peer_num)
    return ship.conn.ovum("a", ["attest", "request"], card)


def hi_probe(ship, peer_patp: str) -> str:
    """Run `|hi peer` on `ship` (pokes the remote %hood over ames). Returns
    'hi-ok' iff the poke is positively acked — i.e. the peer received it.
    Raises ConnError / times out if unreachable."""
    body = _HI.format(tgt=peer_patp)
    return _cord(ship.conn.khan_eval(body))


def open_packet_blob(ship, rcvr_patp: str):
    """Scry `ship` for its OWN signed self-attestation (open-packet) addressed
    to rcvr_patp, via the public `/x//attest-packet` ames endpoint added to the
    kernel. Returns the packet bytes (a big int). The packet is REAL — signed
    with ship's networking key by the kernel's own +etch-open-packet — so it
    passes the receiver's +sift-open-packet checks (pubkey hashes to @p +
    valid ed25519 signature)."""
    body = (
        "=/  m  (strand ,vase)  ^-  form:m\n"
        "  ;<  our=@p  bind:m  get-our\n"
        "  =/  pax=path  "
        f"~[(scot %p our) %$ (scot %ud 1) %attest-packet (scot %p {rcvr_patp})]\n"
        "  =/  blob=@ux  .^(@ux %ax pax)\n"
        "  (pure:m !>(blob))"
    )
    return ship.conn.khan_eval(body)


def inject_open_packet(a, b) -> str:
    """Deliver comet B's signed suite-C open-packet (addressed to A) straight
    into A's ames as a `%hear`, firing A's +on-hear-open suite gate.

    Why direct injection: on a cold comet<->comet pair in `-L`, A can't route a
    keys-request to an %alien B (no lane; it'd go to B's unreachable sponsor),
    and `%dear` only records a lane for an already-%known peer — so the natural
    |hi flow never elicits B's open-packet. We instead scry B for the exact
    blob it would have sent and feed it to A. The lane is B's direct lane, so
    A records it in the attest entry (used later by the verify-mode jael ride).
    `a`, `b` are Comets."""
    blob = open_packet_blob(b, a.patp)
    addr = lane_atom(b.ames_port)
    card = (N.tas("hear"), ((1, addr), blob))     # [%hear [%| addr] blob]
    return a.conn.ovum("a", ["ames"], card)
