"""``gw``: one command for a Groundwire comet.

Every subcommand does its job and exits.  Nothing here stays running: the
ship runs under the operating system's service manager (see service.py).
"""

from __future__ import annotations

import argparse
import os
import sys

from .layout import GW_DIR, GwError

_TTY = sys.stdout.isatty() and not os.environ.get("NO_COLOR")
_B, _R, _Y, _G, _0 = (("\033[1m", "\033[31m", "\033[33m", "\033[32m", "\033[0m") if _TTY
                      else ("", "", "", "", ""))


def step(msg: str) -> None:
    print(f"\n{_B}==>{_0} {msg}", flush=True)


def info(msg: str = "") -> None:
    for line in str(msg).splitlines() or [""]:
        print(f"    {line}", flush=True)


def good(msg: str) -> None:
    print(f"    {_G}{msg}{_0}", flush=True)


def warn(msg: str) -> None:
    print(f"{_Y}warning:{_0} {msg}", file=sys.stderr, flush=True)


def die(msg: str, code: int = 1) -> "NoReturn":  # noqa: F821
    print(f"\n{_R}error:{_0} {msg}", file=sys.stderr, flush=True)
    sys.exit(code)


def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        prog="gw",
        description="Groundwire: mint, run and look after a comet. "
                    f"Install root: {GW_DIR} ($GROUNDWIRE_DIR).")
    p.add_argument("--json", action="store_true", help="machine-readable output where a command has it")
    sub = p.add_subparsers(dest="cmd", metavar="<command>")

    s = sub.add_parser("install", help="fetch and verify a release; put gw on the PATH")
    s.add_argument("--version", dest="tag", default="latest", help="release tag (default: the newest)")
    s.add_argument("--sha256", default="", help="require the tarball to have this digest")
    s.add_argument("--from-file", default="", help="install this tarball instead of downloading one")
    s.add_argument("--redownload", action="store_true")

    s = sub.add_parser("mint", help="mint a new comet and leave it running")
    s.add_argument("--invite", default="", metavar="CODE", help="spend a prepaid invite")
    s.add_argument("--sponsor", default="", metavar="@p")
    s.add_argument("--fief", default="", metavar="IP:PORT", help="commit a static endpoint (pins the ames port)")
    s.add_argument("--prefix", default="", metavar="LETTERS",
                   help="vanity: a 3- or 6-letter name prefix (3 is minutes, 6 is a very long time)")
    s.add_argument("--candidates", type=int, default=1, metavar="N",
                   help="mine N names and pick one (headless: the first, or --pick)")
    s.add_argument("--pick", type=int, default=0, metavar="I", help="with --candidates, take the I-th name (1-based)")
    s.add_argument("--headless", action="store_true", help="prompts instead of the full-screen interface")
    s.add_argument("--assume-saved", action="store_true", help="skip the seed-phrase read-back (scripted runs)")
    s.add_argument("--phrase-file", default="", metavar="PATH",
                   help="also write the seed phrase to this 0600 file (unattended runs)")
    s.add_argument("--resume", action="store_true", help="a previous mint died after funding: re-enter its phrase")
    s.add_argument("--skip-attestation", action="store_true",
                   help="FOR TESTING: mine and boot a comet with no funding and no spawn transaction")
    s.add_argument("--vps", action="store_true", help="public server: get a groundwire.me name and certificate")
    s.add_argument("--port", type=int, default=0, help="HTTP port (default 8080, or the next free one)")
    s.add_argument("--ames-port", type=int, default=0)
    s.add_argument("--loom", type=int, default=0)
    s.add_argument("--gateway", default="")
    s.add_argument("--no-armillary", action="store_true")
    s.add_argument("--no-bitcoin", action="store_true", help="boot only: no peers, no sync")
    s.add_argument("--no-wait", action="store_true", help="do not watch the sync after booting")
    s.add_argument("--version", dest="tag", default="", help="release to install first (default: what is installed, else the newest)")

    s = sub.add_parser("start", help="start a ship under the service manager")
    s.add_argument("comet", nargs="?")
    s.add_argument("--feed-file", default="", help="first boot: the baked feed from a finished mint")
    s.add_argument("--proof", default="", help="first boot: the spawn proof (sponsor attestation)")
    s.add_argument("--port", type=int, default=0)
    s.add_argument("--ames-port", type=int, default=0)
    s.add_argument("--vps", action="store_true")
    s.add_argument("--no-wait", action="store_true")
    s.add_argument("--detached", action="store_true", help="no service manager: a plain background process")

    s = sub.add_parser("stop", help="stop a ship, in the safe order")
    s.add_argument("comet", nargs="?")

    s = sub.add_parser("restart", help="stop and start")
    s.add_argument("comet", nargs="?")

    s = sub.add_parser("status", help="what the ship says about itself")
    s.add_argument("comet", nargs="?")
    s.add_argument("--watch", action="store_true", help="keep printing until the light client is synced")

    s = sub.add_parser("code", help="print the web login code")
    s.add_argument("comet", nargs="?")

    s = sub.add_parser("logs", help="tail the ship log")
    s.add_argument("comet", nargs="?")
    s.add_argument("-n", type=int, default=60)
    s.add_argument("-f", "--follow", action="store_true")

    s = sub.add_parser("ctl", help="talk to the ship over its control socket")
    s.add_argument("-s", "--ship", default="", metavar="@p", help="which ship (the lone one by default)")
    cs = s.add_subparsers(dest="ctl_cmd", metavar="<verb>")
    c = cs.add_parser("scry", help="gall peek: gw ctl scry <agent> <path> (e.g. bitcoin-client /is-synced)")
    c.add_argument("agent")
    c.add_argument("path")
    c = cs.add_parser("poke", help="gw ctl poke <agent> <mark> '<vase expression>'")
    c.add_argument("agent")
    c.add_argument("mark")
    c.add_argument("vase")
    c = cs.add_parser("eval", help="run a strand body; `our` and `now` are bound")
    c.add_argument("hoon")
    c = cs.add_parser("our", help="who the ship is")

    s = sub.add_parser("name", help="a groundwire.me name and certificate for a public ship")
    s.add_argument("comet", nargs="?")

    s = sub.add_parser("peers", help="seed the light client with Bitcoin peers")
    s.add_argument("comet", nargs="?")
    s.add_argument("--count", type=int, default=25)

    s = sub.add_parser("populate", help="re-run the post-boot setup steps (idempotent)")
    s.add_argument("comet", nargs="?")
    s.add_argument("--only", default="", help="comma-separated step names")

    s = sub.add_parser("update", help="move to a newer release: clean stop, swap, start")
    s.add_argument("comet", nargs="?")
    s.add_argument("--version", dest="tag", default="latest")
    s.add_argument("--from-file", default="")

    s = sub.add_parser("invite", help="prepay a comet spawn for someone else")
    isub = s.add_subparsers(dest="invite_cmd", metavar="<verb>")
    i = isub.add_parser("create", help="print a funding address, wait for the sats, print the code")
    i.add_argument("--no-wait", action="store_true")
    i = isub.add_parser("show", help="what an invite code holds")
    i.add_argument("code")

    s = sub.add_parser("mine", help="vanity mining: what it costs and how to ask for it")
    s.add_argument("--prefix", default="")

    s = sub.add_parser("ships", help="list the ships on this machine")

    s = sub.add_parser("causeway", help="run Causeway's own command line (advanced)")
    s.add_argument("args", nargs=argparse.REMAINDER)
    s = sub.add_parser("causeway-tui", help="Causeway's full-screen interface (advanced)")
    s.add_argument("args", nargs=argparse.REMAINDER)

    sub.add_parser("version", help="print the installed release")
    return p


def main(argv: list[str] | None = None) -> int:
    argv = list(sys.argv[1:] if argv is None else argv)
    # `gw causeway ...` and `gw causeway-tui ...` hand the whole tail over.
    if argv and argv[0] in ("causeway", "causeway-tui"):
        from . import commands
        return commands.cmd_causeway(argv[0], argv[1:])
    parser = build_parser()
    args = parser.parse_args(argv)
    if not args.cmd:
        parser.print_help()
        return 2
    from . import commands
    fn = getattr(commands, "cmd_" + args.cmd.replace("-", "_"), None)
    if fn is None:
        parser.print_help()
        return 2
    try:
        rc = fn(args)
        return 0 if rc is None else int(rc)
    except GwError as e:
        die(str(e))
    except KeyboardInterrupt:
        print()
        return 130
    return 0
