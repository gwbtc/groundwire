#!/usr/bin/env python3
"""gwtest.py -- run a desk's Hoon test suite and tally it HONESTLY.

Unlike the rest of `ops/`, this one runs on a LAPTOP as happily as on a
droplet: it needs a vere binary, a kernel checkout and tmux, and nothing else.
There was no test tooling in this repo at all, so every agent rolled its own
one-line `grep | wc -l` and each one was wrong in the same way.  The bug that
motivated this file:

    A test that PASSES prints   `OK      /tests/lib/foo/test-bar`
    A test that FAILS  prints   `FAILED  /tests/lib/foo/test-bar`
    A test that CRASHES prints  `CRASHED /tests/lib/foo/test-bar`
    A test file that does not COMPILE prints
                                `FAILED  /tests/lib/foo (build)`

(all four from +run-test / the gather loop in `ted/test.hoon`).  A tally that
counts only the first form reports a crashing suite as 100% green -- which is
exactly what happened: `164 OK / 0 fail` reported while one arm was crashing,
for a week.

A crash is strictly worse than a failure and must never be rounded down to
one: a FAILED arm ran and its assertion was false, a CRASHED arm never ran at
all, so every assertion it contains is simply untested.  A `(build)` failure is
worse still -- a whole FILE's arms vanish, and the arms that DID run still all
say OK, so the transcript looks healthier the more of it is missing.  This tool
therefore counts the four outcomes separately, prints every non-OK name, and
exits non-zero on any of them.  It also cross-checks its own tally against the
`ok=%.y` / `ok=%.n` the thread itself returns, and refuses to report success on
a transcript in which it found no results at all.

    gwtest.py boot   <pier> --arvo DIR --pill FILE [--urbit BIN] [--session S]
    gwtest.py commit <pier> <desk> <src-dir> [--session S]
    gwtest.py run    <pier> [<test-path> ...] [--desk D] [--session S]
    gwtest.py tally  [transcript ...]

`boot` and `commit` exist because the two traps they dodge cost real hours:

  * `pkg/arvo` is a tree of SYMLINKS into `pkg/base-dev`.  Copy it without
    dereferencing and you get a ship that boots and silently fails to commit
    %base.  `boot` copies with `-L` into a scratch dir and boots from that.
  * You cannot `urbit ... | tee log`.  A pipe is not a tty, so vere refuses
    ("use -t to disable interactivity") -- and `-t` then costs you the dojo,
    which is the only way in without a conn.sock client.  `boot` runs vere
    under tmux and captures with `tmux pipe-pane`, which keeps the tty.

The transcript that `pipe-pane` captures is terminal output: ANSI colour,
cursor addressing, and a `~zod:dojo> ` prompt redrawn in front of nearly every
slog line.  +clean strips all of it before anything is matched, and the
matcher anchors on the leading `/` of the test path rather than on the start
of a line, because the prompt is a line prefix.
"""
import argparse
import os
import re
import shutil
import subprocess
import sys
import time
from pathlib import Path

DEFAULT_SESSION = "gwtest"

# CSI / two-char escapes.  tmux redraws the dojo prompt constantly, so a
# transcript is roughly half escape bytes by volume.
_ANSI = re.compile(r"\x1b\[[0-9;?]*[a-zA-Z]|\x1b[=>78MD]|\x1b\][^\x07]*\x07")

# +run-test formats its verdict as "OK      ", "FAILED  ", "CRASHED " -- note
# the padding differs (6, 2 and 1 space), so never match on a fixed run of
# spaces.  The path always starts with `/` (it is a +spud), and requiring that
# is what keeps ordinary slog prose containing the word "ok" out of the count.
_RESULT = re.compile(r"\b(OK|FAILED|CRASHED)\s+(/\S*)")
_BUILT = re.compile(r"\bbuilt\s+(/\S*)")
# The thread's own return value, as the dojo prints it.
_VERDICT = re.compile(r"\bok=%\.([yn])\b")


def clean(raw: bytes) -> list[str]:
    """A terminal capture as plain lines: escapes gone, prompts gone."""
    txt = _ANSI.sub("", raw.decode("utf8", errors="replace"))
    txt = txt.replace("\r\n", "\n").replace("\r", "\n")
    out = []
    for line in txt.split("\n"):
        # The prompt is redrawn in FRONT of slog lines, sometimes twice.
        line = re.sub(r"^(?:~[a-z-]+:dojo>\s*)+", "", line)
        out.append(line.rstrip())
    return out


class Tally:
    """OK / FAILED / CRASHED / build-failure, counted apart and never merged."""

    def __init__(self):
        self.ok: list[str] = []
        self.failed: list[str] = []
        self.crashed: list[str] = []
        self.build_failed: list[str] = []
        self.built: list[str] = []
        self.verdict: str | None = None   # the thread's own ok=%.y / %.n
        self._seen: set[tuple[str, str]] = set()

    def feed(self, lines: list[str]) -> "Tally":
        for line in lines:
            for m in _BUILT.finditer(line):
                if m.group(1) not in self.built:
                    self.built.append(m.group(1))
            for m in _RESULT.finditer(line):
                verdict, path = m.group(1), m.group(2)
                # A file that does not compile is reported as a FAILED whose
                # path is the FILE, with a "(build)" suffix.  It is not one
                # bad test -- it is every arm in that file, unrun.
                if verdict == "FAILED" and "(build)" in line[m.end():m.end() + 10]:
                    bucket, key = self.build_failed, ("build", path)
                else:
                    bucket, key = {
                        "OK": self.ok, "FAILED": self.failed, "CRASHED": self.crashed,
                    }[verdict], (verdict, path)
                if key in self._seen:
                    continue
                self._seen.add(key)
                bucket.append(path)
            v = _VERDICT.search(line)
            if v:
                self.verdict = v.group(1)
        return self

    @property
    def arms(self) -> int:
        return len(self.ok) + len(self.failed) + len(self.crashed)

    @property
    def bad(self) -> int:
        return len(self.failed) + len(self.crashed) + len(self.build_failed)

    def report(self, stream=sys.stdout) -> int:
        """Print the tally; return the process exit code."""
        w = stream.write
        for path in self.build_failed:
            w(f"BUILD FAILED  {path}   (every arm in this file went unrun)\n")
        for path in self.crashed:
            w(f"CRASHED       {path}   (the arm never ran; its assertions are untested)\n")
        for path in self.failed:
            w(f"FAILED        {path}\n")

        by_file: dict[str, dict[str, int]] = {}
        for kind, paths in (("ok", self.ok), ("failed", self.failed),
                            ("crashed", self.crashed)):
            for p in paths:
                f = p.rsplit("/", 1)[0]
                by_file.setdefault(f, {"ok": 0, "failed": 0, "crashed": 0})[kind] += 1
        if by_file:
            w("\n")
            for f in sorted(by_file):
                c = by_file[f]
                extra = "".join(
                    f"  {n} {k}" for k, n in (("FAILED", c["failed"]), ("CRASHED", c["crashed"]))
                    if n
                )
                w(f"  {f:<44} {c['ok']:>4} OK{extra}\n")

        w("\n%d files built, %d arms run: %d OK, %d FAILED, %d CRASHED"
          % (len(self.built), self.arms, len(self.ok), len(self.failed), len(self.crashed)))
        if self.build_failed:
            n = len(self.build_failed)
            w(", %d FILE%s DID NOT BUILD" % (n, "" if n == 1 else "S"))
        w("\n")

        # Nothing found is not success.  An empty or truncated transcript used
        # to read as a clean run, which is the same failure mode as the crash
        # this tool exists to catch.
        if self.arms == 0 and not self.build_failed:
            w("no test results in this transcript -- did the run reach the tests?\n")
            return 2
        # The thread computes its own verdict; disagreeing with it means the
        # transcript is lying to one of us and neither answer can be trusted.
        if self.verdict == "n" and self.bad == 0:
            w("thread returned ok=%.n but this tally found nothing wrong -- "
              "the transcript is incomplete\n")
            return 2
        if self.verdict == "y" and self.bad:
            w("thread returned ok=%.y but this tally found failures -- "
              "results from an earlier run are mixed in\n")
            return 2
        return 1 if self.bad else 0


# ------------------------------------------------------------------- tmux
def tmux(*args: str, check=True) -> str:
    r = subprocess.run(["tmux", *args], capture_output=True, text=True)
    if check and r.returncode:
        raise SystemExit(f"tmux {' '.join(args)}: {r.stderr.strip()}")
    return r.stdout


def send(session: str, line: str) -> None:
    """send-keys with `--`: a dojo line starting with `-` (every thread call
    does) is otherwise eaten as a tmux option."""
    tmux("send-keys", "-t", session, "--", line, "Enter")


def wait_for(session: str, needle: str, timeout: int, poll: float = 2.0) -> bool:
    deadline = time.time() + timeout
    while time.time() < deadline:
        if needle in tmux("capture-pane", "-t", session, "-p"):
            return True
        time.sleep(poll)
    return False


def transcript_path(pier: Path) -> Path:
    return Path(f"{pier}.transcript")


def read_transcript(pier: Path, since: int = 0) -> list[str]:
    p = transcript_path(pier)
    if not p.exists():
        raise SystemExit(f"no transcript at {p} -- was the pier booted with `gwtest.py boot`?")
    with p.open("rb") as f:
        f.seek(since)
        return clean(f.read())


# -------------------------------------------------------------------- boot
def cmd_boot(a) -> int:
    pier = Path(a.pier).resolve()
    if pier.exists():
        raise SystemExit(f"{pier} already exists; remove it or pick another path")
    urbit = shutil.which(a.urbit) or a.urbit
    if not Path(urbit).exists():
        raise SystemExit(f"no vere binary at {urbit}")

    # pkg/arvo is symlinks into pkg/base-dev.  -L or the ship boots and then
    # silently commits nothing to %base.
    arvo = pier.parent / f".{pier.name}-arvo"
    if arvo.exists():
        shutil.rmtree(arvo)
    arvo.parent.mkdir(parents=True, exist_ok=True)
    subprocess.run(["cp", "-RL", str(Path(a.arvo).resolve()), str(arvo)], check=True)

    log = transcript_path(pier)
    log.unlink(missing_ok=True)
    tmux("kill-session", "-t", a.session, check=False)
    tmux("new-session", "-d", "-s", a.session, "-x", "220", "-y", "60")
    tmux("pipe-pane", "-t", a.session, "-o", f"cat >> {log}")
    # `exec` so the pane IS vere: no shell to swallow a signal, and killing the
    # session stops the ship.  NO PIPE -- a pipe is not a tty and vere refuses.
    send(a.session, f"exec {urbit} -F zod -A {arvo} -B {Path(a.pill).resolve()} -c {pier}")
    print(f"booting ~zod at {pier} (transcript: {log})", flush=True)
    if not wait_for(a.session, "dojo>", a.timeout, poll=5.0):
        raise SystemExit(f"no dojo prompt after {a.timeout}s -- see {log}")
    print("booted.")
    return 0


# ------------------------------------------------------------------ commit
def _wait_dir(p: Path, timeout: int) -> bool:
    deadline = time.time() + timeout
    while time.time() < deadline:
        if p.is_dir():
            return True
        time.sleep(1)
    return False


def _desk_aeon(a, pier: Path) -> int | None:
    """The desk's revision, read out of Clay.

    %cw is clay's case scry and its $cass carries the aeon.  Unlike probing
    for a file inside the desk, it asks about a path that always exists for
    a desk that exists, so it cannot scry-lost.
    """
    at = transcript_path(pier).stat().st_size
    send(a.session, f".^(cass:clay %cw /(scot %p our)/{a.desk}/(scot %da now))")
    time.sleep(4)
    for line in read_transcript(pier, at):
        m = re.search(r"ud=([0-9.]+)", line)
        if m:
            return int(m.group(1).replace(".", ""))
    return None


def cmd_commit(a) -> int:
    pier, src = Path(a.pier).resolve(), Path(a.src).resolve()
    if not src.is_dir():
        raise SystemExit(f"{src} is not a directory (run `make build` first?)")
    mount = pier / a.desk
    # Mount first, create only if that fails.  `|new-desk` on a desk that
    # already exists opens a "overwrite it?" DIAL PROMPT, and the next line
    # you send is swallowed answering it -- so a re-run of this command used
    # to abort the new-desk and silently skip the mount as well.  Ordering it
    # this way makes the command idempotent, which is what a re-run needs.
    send(a.session, f"|mount %{a.desk}")
    if not _wait_dir(mount, 20):
        send(a.session, f"|new-desk %{a.desk}")
        time.sleep(8)
        send(a.session, f"|mount %{a.desk}")
        if not _wait_dir(mount, 60):
            raise SystemExit(f"{mount} never appeared -- see {transcript_path(pier)}")
    time.sleep(4)
    # |new-desk seeds a template; the desk we are testing is the source of
    # truth for every file in it.
    for child in mount.iterdir():
        shutil.rmtree(child) if child.is_dir() else child.unlink()
    subprocess.run(["cp", "-R", *[str(p) for p in src.iterdir()], str(mount)], check=True)
    before = _desk_aeon(a, pier)
    at = transcript_path(pier).stat().st_size
    send(a.session, f"|commit %{a.desk}")
    time.sleep(a.settle)
    # Unmount: a mounted desk keeps clay writing, and nothing here needs it.
    send(a.session, f"|unmount %{a.desk}")
    time.sleep(3)
    # Clay refuses a commit by CHANGING NOTHING.  It prints no "commit
    # failed", and the strings it does print are the mark's, not clay's --
    # the real rejection read `[%error-validating /doc/confidential-comets/md]`
    # and `[%no-cast-between %mime %md]`, so the old grep for "commit failed"
    # or "%clay" matched neither and this function returned success.
    #
    # That is not a cosmetic miss.  The pill PRE-BAKES this desk from
    # GROUNDWIRE_BRANCH (main) -- as %groundwire in releases before the
    # rename and %gw-btc after -- so a refused commit leaves MAIN's code
    # installed under the desk name we are about to test, and the run reads
    # as green-ish while testing someone else's code entirely.  It cost a
    # whole validation campaign: the "two test files do not build" finding
    # was main's files, not ours.
    #
    # So ask Clay whether the revision moved, which is the only question
    # that settles it.  Same fix, same reason, as automation/desk-commit.sh.
    bad = [l for l in read_transcript(pier, at)
           if "commit failed" in l or "%clay" in l
           or "error-validating" in l or "no-cast-between" in l
           or "validate-page-fail" in l or "error-building-tube" in l]
    for l in bad:
        print(l)
    after = _desk_aeon(a, pier)
    if before is None or after is None:
        print(f"WARNING: could not read %{a.desk}'s revision "
              f"(before={before} after={after}); commit is UNVERIFIED")
    elif after <= before:
        raise SystemExit(
            f"ERROR: %{a.desk} is still at revision {before} -- clay did not "
            f"take this commit.\n"
            f"Anything you run now tests whatever was already installed "
            f"under %{a.desk}, which on a pill-booted ship is "
            f"GROUNDWIRE_BRANCH's code, not {src}.\n"
            f"Usual causes: a file whose mark the desk does not carry (are "
            f"you committing the SOURCE tree instead of dist-gw-btc?); "
            f"sys.kelvin naming a kernel this ship is not at; the mount not "
            f"synced.\nSee {transcript_path(pier)}")
    else:
        print(f"committed %{a.desk} from {src}: revision {before} -> {after}")
    return 0


# --------------------------------------------------------------------- run
def cmd_run(a) -> int:
    pier = Path(a.pier).resolve()
    paths = a.paths or [f"/={a.desk}=/tests"]
    log = transcript_path(pier)
    at = log.stat().st_size
    send(a.session, "-test " + " ".join(paths))
    print(f"-test {' '.join(paths)} ...", flush=True)
    deadline = time.time() + a.timeout
    while time.time() < deadline:
        time.sleep(5)
        # A transcript that never grows means the capture is detached, not
        # that the tests are slow -- and a detached capture reads as a clean
        # run to anything that only counts OK lines.  Say so instead.
        if log.stat().st_size == at and time.time() - (deadline - a.timeout) > 60:
            raise SystemExit(
                f"{log} has not grown in 60s -- the tmux capture is detached.\n"
                f"    tmux pipe-pane -t {a.session} 'cat >> {log}'\n"
                "(`pipe-pane` TOGGLES: running it twice turns the capture off.)"
            )
        t = Tally().feed(read_transcript(pier, at))
        if t.verdict:
            return t.report()
    raise SystemExit(f"-test did not finish in {a.timeout}s (no ok=%.y/%.n seen)")


# ------------------------------------------------------------------- tally
def cmd_tally(a) -> int:
    t = Tally()
    if not a.files:
        t.feed(clean(sys.stdin.buffer.read()))
    for f in a.files:
        t.feed(clean(Path(f).read_bytes()))
    return t.report()


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    sub = ap.add_subparsers(dest="cmd", required=True)

    b = sub.add_parser("boot", help="boot a fake ~zod under tmux, from a kernel checkout")
    b.add_argument("pier")
    b.add_argument("--arvo", required=True, help="path to pkg/arvo in your kernel checkout")
    b.add_argument("--pill", required=True, help="a solid pill (e.g. urbit/bin/solid.pill)")
    b.add_argument("--urbit", default="urbit", help="vere binary")
    b.add_argument("--session", default=DEFAULT_SESSION)
    b.add_argument("--timeout", type=int, default=900)
    b.set_defaults(fn=cmd_boot)

    c = sub.add_parser("commit", help="fill and commit a desk from a dist directory")
    c.add_argument("pier")
    c.add_argument("desk")
    c.add_argument("src", help="e.g. dist-gw-btc")
    c.add_argument("--session", default=DEFAULT_SESSION)
    c.add_argument("--settle", type=int, default=90, help="seconds to let the commit build")
    c.set_defaults(fn=cmd_commit)

    r = sub.add_parser("run", help="-test a desk's tests and tally the result")
    r.add_argument("pier")
    r.add_argument("paths", nargs="*", help="default: /=<desk>=/tests")
    r.add_argument("--desk", default="groundwire")
    r.add_argument("--session", default=DEFAULT_SESSION)
    r.add_argument("--timeout", type=int, default=1800)
    r.set_defaults(fn=cmd_run)

    t = sub.add_parser("tally", help="tally a saved transcript (or stdin)")
    t.add_argument("files", nargs="*")
    t.set_defaults(fn=cmd_tally)

    a = ap.parse_args()
    return a.fn(a)


if __name__ == "__main__":
    sys.exit(main())
