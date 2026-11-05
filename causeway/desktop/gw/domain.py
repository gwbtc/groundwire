"""A name under groundwire.me and a certificate for it, idempotently.

The sponsor runs the naming service (gwbtc/urbit-dns); the ship's %dns desk
asks it with the dns-address thread, eyre takes the turf the thread
installs, and %acme orders the certificate over port 80.
"""

from __future__ import annotations

import os
import shutil
import subprocess
import urllib.request

from . import noun as N
from .cli import info, warn
from .ship import Ship


def public_ip() -> str:
    try:
        with urllib.request.urlopen("https://api.ipify.org", timeout=10) as r:
            ip = r.read().decode().strip()
    except Exception:
        return ""
    return ip if ip.count(".") == 3 and all(p.isdigit() for p in ip.split(".")) else ""


def ensure_port80(http_port: int) -> bool:
    """One NAT rule: the certificate authority validates on 80, the ship
    listens on http_port.  Root and iptables only."""
    if http_port == 80:
        return True
    if os.geteuid() != 0 or not shutil.which("iptables"):
        warn(f"port 80: not root, or no iptables; forward port 80 to {http_port} yourself "
             "or the certificate cannot be issued")
        return False
    rule = ["-p", "tcp", "--dport", "80", "-j", "REDIRECT", "--to-ports", str(http_port)]
    check = subprocess.run(["iptables", "-t", "nat", "-C", "PREROUTING"] + rule, capture_output=True)
    if check.returncode != 0:
        r = subprocess.run(["iptables", "-t", "nat", "-A", "PREROUTING"] + rule, capture_output=True)
        if r.returncode != 0:
            warn(f"port 80: could not add the NAT rule to {http_port}")
            return False
        info(f"port 80: forwarded to {http_port}")
    if not shutil.which("netfilter-persistent") and shutil.which("apt-get"):
        subprocess.run(["apt-get", "install", "-y", "-q", "iptables-persistent"],
                       env={**os.environ, "DEBIAN_FRONTEND": "noninteractive"}, capture_output=True)
    if shutil.which("netfilter-persistent"):
        subprocess.run(["netfilter-persistent", "save"], capture_output=True)
    else:
        warn("port 80: the forward is not persistent here; run `gw name` again after a reboot")
    return True


def ensure_dns_desk(ship: Ship) -> bool:
    if not ship.has_desk("dns"):
        info(f"%dns is not on this ship; installing it from {ship.cfg.sponsor}")
        try:
            ship.poke("hood", "kiln-install", f"!>([%dns {ship.cfg.sponsor} %dns])", timeout=600)
        except Exception as e:
            warn(f"kiln-install %dns: {e}")
    if ship.wait_agent("gw-dns", tries=18, every=10):
        return True
    warn("%dns is not running after 3 minutes (still installing from the sponsor, or the sponsor is "
         "unreachable); no name this time. Run `gw name` again later.")
    return False


def groundwire_domain(ship: Ship) -> str:
    for d in ship.domains():
        if d.endswith("groundwire.me"):
            return d
    return ""


NAME_STRAND = """;<  =bowl:spider  bind:m  get-bowl
=/  tid  ;;(@ta (cat 3 'dns-address-' (scot %uv (sham eny.bowl))))
;<  ~  bind:m  (watch /res [our %spider] /thread-result/[tid])
;<  ~  bind:m
  %^  poke-our  %spider  %spider-start
  !>  ^-  start-args:spider
  [~ [~ tid] [our %dns da+now] %dns-address !>([~ [%if .GWIP]])]
;<  =cage  bind:m  (take-fact /res)
(pure:m !>([p.cage q.q.cage]))
"""


def ensure_domain(ship: Ship) -> str:
    """The ship's groundwire.me name, asking the sponsor for one if it has none."""
    ensure_port80(ship.cfg.http_port)
    if not ensure_dns_desk(ship):
        return ""
    have = groundwire_domain(ship)
    if have:
        return have
    ip = public_ip()
    if not ip:
        warn("could not learn this machine's public IPv4; no name this time")
        return ""
    info(f"asking the sponsor to name {ip}")
    try:
        r = ship.khan(NAME_STRAND.replace("GWIP", ip), timeout=300)
    except Exception as e:
        warn(f"the naming thread did not finish: {e}")
        return ""
    # [p=mark q=noun]; %thread-done carries the dns-address result
    try:
        tag = N.from_tas(r[0]) if isinstance(r, tuple) else ""
    except Exception:
        tag = ""
    if tag != "thread-done":
        warn(f"the naming thread answered %{tag or '?'} rather than a name; try `gw name` again")
        return ""
    name = _domain_from_noun(r)
    if not name:
        name = groundwire_domain(ship)
    return name


def _domain_from_noun(n) -> str:
    """Find a turf ending in %me inside the thread result."""
    found = ""

    def walk(x, depth=0):
        nonlocal found
        if found or depth > 64:
            return
        if isinstance(x, tuple):
            # a turf is a null-terminated list of @ta, TLD first
            try:
                parts = [N.from_tas(k) for k in N.list_iter(x)]
                if parts and parts[0] == "me" and all(p.isalnum() or "-" in p for p in parts):
                    found = ".".join(reversed(parts))
                    return
            except Exception:
                pass
            walk(x[0], depth + 1)
            walk(x[1], depth + 1)

    walk(n)
    return found
