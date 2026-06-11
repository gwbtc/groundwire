"""Build a SOLID pill from MY arvo, on a running 408 builder ship.

Unlike build_pill.py (brass), this uses solid:pill, which PRE-INSTALLS the
kernel + userspace into the pill (brass recompiles-from-source at boot, and the
tinnus vere crashes booting brass pills via -B even for a pure-baseline pill --
proven; solid pills boot fine, like gw-solid.pill). The desk is rsynced with -L
so userspace symlinks resolve into real files (solid needs full userspace).

    python3 build_solid.py <builder-pier>
"""
import argparse
import shutil
import subprocess
import sys
import time
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from gwharness import noun as N
from gwharness.connsock import ConnSock

ROOT = Path("/Users/trent/gw-building")
ARVO = ROOT / "urbit" / "pkg" / "arvo"
OUT = ROOT / "gw-solid-mine.pill"

ap = argparse.ArgumentParser()
ap.add_argument("pier")
args = ap.parse_args()
PIER = Path(args.pier)
conn = ConnSock(PIER, timeout=600)


def mount(desk):
    conn.poke_our("hood", "kiln-mount",
                  f"!>([(en-beam [[our %{desk} [%da now]] /]) %{desk}])")


def await_path(p, t=180):
    end = time.time() + t
    while time.time() < end:
        if p.exists():
            return True
        time.sleep(2)
    return False


def populate(desk, src):
    print(f"[solid] create+populate %{desk} from {src} (-L: resolve userspace)", flush=True)
    conn.poke_our("hood", "kiln-merge", f"!>([%{desk} our %base [%da now] %init])")
    time.sleep(4)
    mount(desk)
    if not await_path(PIER / desk / "sys.kelvin", 180):
        print(f"[solid] ERROR: %{desk} did not mount", flush=True)
        sys.exit(1)
    d = PIER / desk
    for child in d.iterdir():
        shutil.rmtree(child) if child.is_dir() else child.unlink()
    # -L resolves symlinks (userspace) into real files; solid needs them
    subprocess.run(["rsync", "-a", "-L", "--exclude=.git*",
                    f"{src}/", f"{d}/"], check=True)
    conn.poke_our("hood", "kiln-commit", f"!>([%{desk} %.y])")
    time.sleep(10)


print("[solid] mount %base (to extract the pill)...", flush=True)
mount("base")
await_path(PIER / "base" / "sys.kelvin", 180)

populate("gw-base", ARVO)

print("[solid] fyrd solid:pill (slow -- compiles full kernel+userspace) ...", flush=True)
solid = """=/  m  (strand ,vase)
^-  form:m
;<  our=@p   bind:m  get-our
;<  now=@da  bind:m  get-time
=/  sys=path  /(scot %p our)/gw-base/(scot %da now)/sys
=/  dez=(list [desk path])  ~
=/  =pill:pill  (solid:pill sys dez | now & ~)
;<  ~  bind:m
  %:  send-raw-card
      %pass  /build  %arvo  %c
      %info  %base  %&
      :~  [/pill/pill %ins %pill !>(pill)]  ==
  ==
(pure:m !>('Built solid pill!'))"""
beams = N.nlist([N.path("sur", "spider", "hoon"),
                 N.path("lib", "strandio", "hoon"),
                 N.path("lib", "pill", "hoon")])
try:
    res = conn.fyrd("base", "khan-eval", "noun", "ted-eval",
                    (N.cord(solid), beams))
    print(f"[solid] solid:pill fyrd returned: {res!r}", flush=True)
except Exception as e:                                       # noqa: BLE001
    print(f"[solid] fyrd error (may still be building): {e}", flush=True)

print("[solid] waiting for the pill on disk ...", flush=True)
for i in range(1200):
    p = PIER / "base" / "pill.pill"
    if p.exists() and p.stat().st_size > 1_000_000:
        shutil.copy(p, OUT)
        print(f"[solid] FOUND {p} -> {OUT} ({OUT.stat().st_size} bytes)", flush=True)
        sys.exit(0)
    if i % 30 == 0:
        print(f"[solid]   ...{i*3}s, no pill yet", flush=True)
    time.sleep(3)
print("[solid] TIMEOUT", flush=True)
sys.exit(1)
