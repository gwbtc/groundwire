"""Build a kelvin-408 brass pill from MY arvo, on a running 408 builder ship.

Reproduces gwbtc/urbit groundwire-build.yml's pill build, but drives fyrd over
the harness ConnSock (macOS-correct newt framing; the CI's bash `nc -W`
truncates the response on macOS). The current arvo (with the Workstream-A kernel
edits) is held in a %gw-base desk -- NOT %base -- so the builder never upgrades.
brass:pill emits a fresh pill whose %base carries my ames/lull; comets boot it
directly and run my suite gate.

    python3 build_pill.py <builder-pier> [--groundwire]
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
DIST_GW = ROOT / "groundwire" / "dist-groundwire"
OUT = ROOT / "gw-base-408.pill"

ap = argparse.ArgumentParser()
ap.add_argument("pier")
ap.add_argument("--groundwire", action="store_true",
                help="bake the groundwire desk into the pill")
args = ap.parse_args()
PIER = Path(args.pier)
conn = ConnSock(PIER, timeout=240)


def scry_kelvin():
    body = ("=/  m  (strand ,vase)\n^-  form:m\n"
            ";<  our=@p  bind:m  get-our\n;<  now=@da  bind:m  get-time\n"
            "=/  kel  .^(* %cx /(scot %p our)/base/(scot %da now)/sys/kelvin)\n"
            "(pure:m !>(kel))")
    return conn.khan_eval(body)


def mount(desk):
    conn.poke_our("hood", "kiln-mount",
                  f"!>([(en-beam [[our %{desk} [%da now]] /]) %{desk}])")


def await_path(p, t=120):
    end = time.time() + t
    while time.time() < end:
        if p.exists():
            return True
        time.sleep(2)
    return False


def populate(desk, src):
    print(f"[pill] create+populate %{desk} from {src}", flush=True)
    conn.poke_our("hood", "kiln-merge", f"!>([%{desk} our %base [%da now] %init])")
    time.sleep(4)
    mount(desk)
    if not await_path(PIER / desk / "sys.kelvin", 120):
        print(f"[pill] ERROR: %{desk} did not mount", flush=True)
        sys.exit(1)
    d = PIER / desk
    for child in d.iterdir():
        shutil.rmtree(child) if child.is_dir() else child.unlink()
    subprocess.run(["rsync", "-a", "--no-links", "--exclude=.git*",
                    f"{src}/", f"{d}/"], check=True)
    conn.poke_our("hood", "kiln-commit", f"!>([%{desk} %.y])")
    time.sleep(8)


print(f"[pill] builder kelvin: {scry_kelvin()}", flush=True)
print("[pill] mount %base (to extract the pill)...", flush=True)
mount("base")
await_path(PIER / "base" / "sys.kelvin", 120)

populate("gw-base", ARVO)
dez = "~"
if args.groundwire:
    if not DIST_GW.exists():
        subprocess.run(["make", "build"], cwd=str(ROOT / "groundwire"), check=True)
    populate("groundwire", DIST_GW)
    dez = "~[[%groundwire /(scot %p our)/groundwire/(scot %da now)]]"

print("[pill] fyrd brass:pill ...", flush=True)
brass = f"""=/  m  (strand ,vase)
^-  form:m
;<  our=@p   bind:m  get-our
;<  now=@da  bind:m  get-time
=/  sys=path  /(scot %p our)/gw-base/(scot %da now)/sys
=/  dez=(list [desk path])  {dez}
=/  =pill:pill  (brass:pill sys dez & | | ~)
;<  ~  bind:m
  %:  send-raw-card
      %pass  /build  %arvo  %c
      %info  %base  %&
      :~  [/pill/pill %ins %pill !>(pill)]  ==
  ==
(pure:m !>('Built pill!'))"""
beams = N.nlist([N.path("sur", "spider", "hoon"),
                 N.path("lib", "strandio", "hoon"),
                 N.path("lib", "pill", "hoon")])
try:
    res = conn.fyrd("base", "khan-eval", "noun", "ted-eval",
                    (N.cord(brass), beams))
    print(f"[pill] brass:pill fyrd returned: {res!r}", flush=True)
except Exception as e:                                       # noqa: BLE001
    print(f"[pill] brass:pill fyrd error (may still be building): {e}", flush=True)

print("[pill] waiting for the pill in %base/pill/ ...", flush=True)
for i in range(900):
    hits = list((PIER / "base").glob("pill/*.pill"))
    if hits:
        shutil.copy(hits[0], OUT)
        print(f"[pill] FOUND {hits[0]} -> {OUT} ({OUT.stat().st_size} bytes)", flush=True)
        sys.exit(0)
    if i % 30 == 0:
        print(f"[pill]   ...{i*3}s, no pill yet", flush=True)
    time.sleep(3)
print("[pill] TIMEOUT", flush=True)
sys.exit(1)
