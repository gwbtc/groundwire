#!/usr/bin/env python3
"""gwsnub.py -- read and clear a ship's ames packet blocklist.

    gwsnub.py show <pier>
    gwsnub.py del  <pier> <patp>
    gwsnub.py add  <pier> <patp>

A snub is the destructive half of a Groundwire verdict: `%gw-btc` emits a
negative `%writ-response`, jael turns it into `[%give %sybl %fail ...]`, and
`+sy-sybl`'s `%fail` branch adds the ship to `snub`.  Since the kernel's
`f68a547b2b` the `%full` branch is that branch's exact inverse -- it calls
`(sy-snub %deny %del ~[her])` -- so a later POSITIVE verdict does lift an
existing snub.

That is a SAFETY NET, NOT SELF-HEALING, and the difference is why this tool
still exists.  On the classic-ames `%hear` path `+pe-hear` tests the blocklist
before it classifies the packet at all, so a snubbed peer cannot deliver the
attestation that would earn it a `%full`: the verdict has to arrive by some
route the snub does not block.  (On the mesa `%heer` path `%page` has no snub
gate, so an attestation does land and the fix really is self-healing there.
That asymmetry is an open question, not settled behaviour -- OPERATIONS.md
Section 12, "Closed by a code change".)

So there are two ways to undo a wrong snub, and they are not equivalent:

  `gwsnub.py del` sends `%snub %deny %del` STRAIGHT at ames.  It edits the
  blocklist and nothing else: no re-verification, no verdict, no point.  The
  ship is un-snubbed whether or not its attestation would verify today.

  `gwctl.py writ` re-pokes the `%jael-writ`.  Jael's `%writ` handler forwards
  to the domain's registered agent unconditionally -- the only short-circuit
  is an unknown or suspended DOMAIN, answered `%lost` -- so an already-known,
  already-snubbed ship is verified again from scratch, and a `%full` lifts
  the snub as a CONSEQUENCE of the verdict.

PREFER `gwctl.py writ` whenever the attestation is expected to pass now, which
is the usual case: the snub was our own ignorance (an unindexed sponsor, a
lagging scanner) and the verifier has since caught up.  It leaves the ship
un-snubbed AND verified, with the reason on the record.  Reach for `del` when
you need packets flowing regardless of what the chain says -- you do not have
the peer's pass to poke with, or the attestation genuinely will not verify and
you have decided to accept it anyway.  A `del` on its own leaves jael's opinion
of the ship exactly as it was.

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
