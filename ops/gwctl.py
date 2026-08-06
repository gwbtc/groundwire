#!/usr/bin/env python3
"""gwctl.py -- drive a Groundwire ship over its control socket.

Runs ON the droplet (the pier's Lick socket is a unix socket).  Every ship in
the live campaign runs with `-t`, which means there is no dojo to type into:
the runbook's `> :gw-btc &noun ...` lines are notation for what this tool
actually sends.  A scry written in the runbook as

    .^(* %gx /(scot %p our)/gw-btc/(scot %da now)/ready/noun)

has to be wrapped in a `(strand ,vase)` thread and handed to khan; that
wrapping is what every command below is doing.

  gwctl.py who     <pier>                     identity, life, rift
  gwctl.py pass    <pier>                     our LIVE jael pass (hex atom)
  gwctl.py desks   <pier> <desk>=<src> ...    create/mount/fill/commit/install
  gwctl.py ready   <pier>                     %gw-btc /x/ready + block-id
  gwctl.py peers   <pier>                     light-client status out of the log
  gwctl.py seed    <pier> [batches] [size]    add NODE_COMPACT_FILTERS peers
  gwctl.py writ    <pier> <patp> <pass-hex>   verify a peer (the %jael-writ poke)
  gwctl.py point   <pier> <patp>              lyfe / dome / snub for one peer
  gwctl.py custody <pier>                     our own verified custody log
  gwctl.py poke    <pier> <agent> <mark> <vase-expr>
  gwctl.py eval    <pier> <hoon-body>         raw khan-eval, for one-offs

Never `pkill -f urbit`; nothing here kills anything.
"""
import shutil
import subprocess
import sys
import time
from pathlib import Path

sys.path.insert(0, "/opt/gw")
from gwharness.connsock import ConnSock  # noqa: E402

HDR = ("=/  m  (strand ,vase)\n^-  form:m\n"
       ";<  our=@p   bind:m  get-our\n;<  now=@da  bind:m  get-time\n")


def conn(pier, timeout=900):
    return ConnSock(Path(pier), timeout=timeout)


def ev(c, body):
    return c.khan_eval(HDR + body)


def _cord(n):
    """A Hoon @t/cord atom back to a python str (atoms are little-endian)."""
    if not isinstance(n, int) or n <= 0:
        return n
    return n.to_bytes((n.bit_length() + 7) // 8, "little").decode(errors="replace")


def hoonhex(v):
    """An int as a Hoon @ux literal: dot-grouped in 4-hex chunks."""
    h = f"{v:x}"
    out = []
    while len(h) > 4:
        out.append(h[-4:])
        h = h[:-4]
    out.append(h)
    return "0x" + ".".join(reversed(out))


def hoonud(n):
    """An int as a Hoon @ud literal: dot-grouped in 3s.  Hoon rejects 900142."""
    s = str(int(n))
    out = []
    while len(s) > 3:
        out.append(s[-3:])
        s = s[:-3]
    out.append(s)
    return ".".join(reversed(out))


# --------------------------------------------------------------------- who
def cmd_who(pier):
    c = conn(pier, 600)
    r = ev(c, """=/  lyf=life  .^(life %j /(scot %p our)/life/(scot %da now)/(scot %p our))
=/  rif  .^(@ud %j /(scot %p our)/rift/(scot %da now)/(scot %p our))
(pure:m !>([(scot %p our) life=lyf rift=rif]))""")
    who, (life, rift) = r[0], r[1]
    print(f"{_cord(who)}  life={life} rift={rift}")


def cmd_pass(pier):
    """The ship's LIVE jael pass -- NOT the artifact's pass_atom_hex, which is
    a different and shorter object (108 B vs ~330-405 B)."""
    c = conn(pier, 600)
    r = ev(c, """=/  lyf=life  .^(life %j /(scot %p our)/life/(scot %da now)/(scot %p our))
=/  rig=ring  .^(ring %j /(scot %p our)/vein/(scot %da now)/(scot %ud lyf))
=/  pas=pass  pub:ex:(nol:nu:cric:crypto rig)
(pure:m !>([life=lyf bytes=(met 3 pas) hex=(scot %ux `@ux`pas)]))""")
    life, (nbytes, hexcord) = r[0], r[1]
    print(f"life {life}  bytes {nbytes}")
    print(_cord(hexcord))


# ------------------------------------------------------------------- desks
def cmd_desks(pier, *specs):
    """desks <pier> groundwire=/opt/gw/desks/gw-cr node=/opt/gw/desks/node ...

    Four rules, all load-bearing (OPERATIONS.md 5.4):
      - commit with %.n.  `%.y` arms a 1 Hz repeating %dirk timer that
        unmounting does NOT cancel; it starves a 2-vCPU box while looking fine.
      - never leave a desk mounted: a `doc/*.md` with no %md mark in the desk
        cannot build a %mime tube and clay enters a permanent crash loop.
      - the commit can race the rsync on a big desk; re-poke until a %cx scry
        of a committed file matches the byte length on disk.
      - install only after the commit is proven to have landed.
    """
    pier = Path(pier)
    c = conn(pier, 900)
    desks = [s.split("=", 1) for s in specs]
    for desk, src in desks:
        src = Path(src)
        d = pier / desk
        if not (d / "sys.kelvin").exists():
            print(f"create %{desk}", flush=True)
            c.poke_our("hood", "kiln-merge",
                       f"!>([%{desk} our %base [%da now] %init])")
            time.sleep(6)
        c.poke_our("hood", "kiln-mount",
                   f"!>([(en-beam [[our %{desk} [%da now]] /]) %{desk}])")
        end = time.time() + 240
        while not (d / "sys.kelvin").exists() and time.time() < end:
            time.sleep(2)
        if not (d / "sys.kelvin").exists():
            sys.exit(f"ERROR: %{desk} did not mount")
        for child in d.iterdir():
            shutil.rmtree(child) if child.is_dir() else child.unlink()
        subprocess.run(["rsync", "-a", "-L", "--exclude=.git*",
                        f"{src}/", f"{d}/"], check=True)
        # pick the biggest file in the desk as the commit probe
        files = [p for p in src.rglob("*") if p.is_file()]
        probe = max(files, key=lambda p: p.stat().st_size)
        rel = "/" + str(probe.relative_to(src)).replace(".", "/")
        want = probe.stat().st_size
        deadline = time.time() + 900
        n = 0
        while True:
            c.poke_our("hood", "kiln-commit", f"!>([%{desk} %.n])")
            n += 1
            time.sleep(10)
            got = None
            try:
                got = ev(c, f"(pure:m !>((met 3 .^(@t %cx /(scot %p our)"
                            f"/{desk}/(scot %da now){rel}))))")
            except Exception:
                pass
            print(f"  %{desk} commit#{n}: clay={got} disk={want}", flush=True)
            if got == want:
                break
            if time.time() > deadline:
                sys.exit(f"ERROR: %{desk} commit never reflected {rel}")
    for desk, _src in desks:
        print(f"install %{desk}", flush=True)
        c.poke_our("hood", "kiln-install", f"!>([%{desk} our %{desk}])")
        time.sleep(20)
    for desk, _src in desks:
        c.poke_our("hood", "kiln-unmount", f"!>(`term`%{desk})")
        time.sleep(2)
    print("desks committed, installed and UNMOUNTED", flush=True)


# ------------------------------------------------------------------- status
READY = """=/  r  .^(* %gx /(scot %p our)/gw-btc/(scot %da now)/ready/noun)
=/  b  .^(* %gx /(scot %p our)/gw-btc/(scot %da now)/block-id/noun)
(pure:m !>([ready=r block-id=b]))"""


def cmd_ready(pier):
    """%gw-btc's /x/ready is the authoritative readiness surface.

    %bitcoin-client's ++peek is literally `~` for EVERY path
    (app/bitcoin-client.hoon:181-184), so there is nothing to scry there --
    a tool that scries /is-synced or /best-block returns nothing and always
    has.  %gw-btc learns `synced` from its /is-synced subscription and
    surfaces it here."""
    print(ev(conn(pier, 600), READY))


def cmd_peers(pier):
    """Ask %bitcoin-client to dump status INTO THE SHIP'S LOG; there is no
    scry.  Read it back out of the pier log afterwards."""
    c = conn(pier, 300)
    c.poke_our("bitcoin-client", "log-info", "!>(~)")
    print("poked &log-info -- read /opt/gw/<pier>.log for "
          "[%is-synced] [%headers] [%filter-headers] [%live-earth-peers]")


def cmd_seed(pier, batches="6", size="25"):
    """Bulk %add-earth-peer SIGSEGVs the tcp-sidecar (gwbtc/node#1); ~25 at a
    time is stable.  Filter headers are only servable by peers advertising
    NODE_COMPACT_FILTERS, so the pool must come from x49.-filtered seeds."""
    name = str(pier).rstrip("/").split("/")[-1]
    pool, used = "/opt/gw/peerpool.txt", f"/opt/gw/used-{name}.txt"
    subprocess.run(["python3", str(Path(__file__).parent / "poolfill.py"), pool])
    Path(used).touch()
    have = set(Path(used).read_text().split())
    ips = [x for x in Path(pool).read_text().split() if x and x not in have]
    print(f"pool has {len(ips)} unused", flush=True)
    for b in range(int(batches)):
        batch = ips[b * int(size):(b + 1) * int(size)]
        if not batch:
            break
        r = subprocess.run(["python3", str(Path(__file__).parent / "addpeers.py"),
                            str(pier)] + batch, capture_output=True, text=True)
        print(f"batch {b+1}: {len(batch)} -> {r.stdout.strip()[:120]}"
              f"{r.stderr.strip()[:160]}", flush=True)
        with open(used, "a") as f:
            f.write("\n".join(batch) + "\n")
        time.sleep(30)


# -------------------------------------------------------------- verification
def cmd_writ(pier, patp, pass_hex):
    """The poke jael normally delivers.  Verification takes 100-110 s and has
    NO wall-clock deadline by design: cost is O(blocks since the comet last
    moved its sat)."""
    c = conn(pier, 900)
    h = pass_hex if pass_hex.startswith("0x") else "0x" + pass_hex
    print(c.poke_our("gw-btc", "noun",
                     f"!>([%jael-writ %gw-btc `@p`{patp} `@ux`{h}])"))


def cmd_point(pier, patp):
    """/=lyfe=/ and /=dome=/ are the clean discriminator between a Groundwire
    verdict and ordinary comet PKI: dome is [~ %gw-btc] only for the former."""
    c = conn(pier, 600)
    print(ev(c, f"""=/  l  .^((unit @ud)  %j /(scot %p our)/lyfe/(scot %da now)/{patp})
=/  d  .^((unit @tas) %j /(scot %p our)/dome/(scot %da now)/{patp})
=/  s  .^(* %ax /(scot %p our)//(scot %da now)/snubbed)
(pure:m !>([lyfe=l dome=d snub=s]))"""))


def cmd_custody(pier):
    print(ev(conn(pier, 600),
             "(pure:m !>(.^(* %gx /(scot %p our)/gw-btc/(scot %da now)"
             "/custody/noun)))"))


def cmd_poke(pier, agent, mark, expr):
    print(conn(pier, 900).poke_our(agent, mark, expr))


def cmd_eval(pier, body):
    print(ev(conn(pier, 900), body))


if __name__ == "__main__":
    if len(sys.argv) < 3:
        sys.exit(__doc__)
    globals()["cmd_" + sys.argv[1]](*sys.argv[2:])
