#!/usr/bin/env python3
"""gwsnub.py -- read and clear a ship's ames packet blocklist.

    gwsnub.py show <pier>
    gwsnub.py del  <pier> <patp>
    gwsnub.py add  <pier> <patp>

A snub is the destructive half of a Groundwire verdict: `%gw-btc` emits a
negative `%writ-response`, jael turns it into `[%give %sybl %fail ...]`, and
ames adds the ship to `snub`.  It is STICKY -- `+sy-sybl`'s `%full` branch
installs the point and never touches `ships.snub`, so a LATER correct
attestation from the same comet does not undo it (verified live, 2026-08-07).
Nothing but this clears it, which makes it the only recovery from a wrong
negative verdict.  See `doc/live-tests/PHASE2-RERUN-RESULTS.md`.

Note the wing: a bare `send-raw-card`, NOT `send-raw-card:strandio`.  Inside a
`khan-eval` thread strandio's arms are already in the subject, so the qualified
form resolves to nothing and the thread dies with a bare `%thread-fail:` and an
EMPTY tang -- which is what the older, never-working `unsnub.py` on the
droplets does.
"""
import sys
import time
from pathlib import Path

sys.path.insert(0, "/opt/gw")
from gwharness.connsock import ConnSock                          # noqa: E402

HDR = ("=/  m  (strand ,vase)\n^-  form:m\n"
       ";<  our=@p   bind:m  get-our\n;<  now=@da  bind:m  get-time\n")

SHOW = HDR + ("=/  s  .^([?(%allow %deny) (list ship)] %ax "
              "/(scot %p our)//(scot %da now)/snubbed)\n"
              "(pure:m !>([-.s (turn +.s |=(w=@p (scot %p w)))]))")


def _cord(n):
    return n.to_bytes((n.bit_length() + 7) // 8, "little").decode(errors="replace")


def show(c):
    form, ships = c.khan_eval(SHOW)
    out = []
    while ships != 0:
        h, ships = ships
        out.append(_cord(h))
    print(f"{_cord(form)}: {out if out else '~'}")


def main():
    if len(sys.argv) < 3:
        sys.exit(__doc__)
    mode, pier = sys.argv[1], Path(sys.argv[2])
    c = ConnSock(pier, timeout=300)
    if mode == "show":
        return show(c)
    who = sys.argv[3]
    act = {"del": "%del", "add": "%add"}[mode]
    c.khan_eval(HDR + f";<  ~  bind:m  (send-raw-card %pass /gwsnub %arvo %a "
                      f"%snub %deny {act} ~[{who}])\n(pure:m !>(0))")
    time.sleep(4)
    show(c)


if __name__ == "__main__":
    main()
