"""Releases: find, fetch, verify, unpack, switch.

Install layout: ``releases/<tag>/`` holds one unpacked tarball, ``current``
points at the one in use, ``bin/`` holds symlinks into it.  A running ship
keeps the binary it started with (Linux and macOS both keep an unlinked
file alive), so switching ``current`` never pulls the runtime out from under
a ship; ``gw update`` stops the ship first anyway.
"""

from __future__ import annotations

import hashlib
import json
import os
import shutil
import sys
import tarfile
import tempfile
import urllib.error
import urllib.request
from datetime import datetime, timezone
from pathlib import Path

from .cli import info, warn
from .layout import GW_DIR, RELEASE_REPO, GwError, platform_tag, write_release_info

WANT = ("gw-vere", "gw-base.pill", "comet_miner", "gw")


def _get(url: str, timeout: float = 30) -> bytes:
    req = urllib.request.Request(url, headers={"User-Agent": "gw", "Accept": "application/vnd.github+json"})
    with urllib.request.urlopen(req, timeout=timeout) as r:
        return r.read()


TAG_PREFIX = "groundwire-alpha-"


def tag_version(tag: str, prefix: str = TAG_PREFIX) -> tuple[int, ...]:
    """``groundwire-alpha-2026.11.4_3`` -> ``(2026, 11, 4, 3)``: the numeric
    fields of a tag, so 2026.11.10 sorts after 2026.11.9 and ``_3`` after no
    suffix.  Anything non-numeric counts as 0."""
    rest = tag[len(prefix):] if tag.startswith(prefix) else tag
    return tuple(int(f) if f.isdigit() else 0 for f in rest.replace("_", ".").split("."))


def pick_latest(releases: list[dict], prerelease: bool = False) -> str:
    """The one rule every Groundwire installer follows (the website's
    install.sh implements the same one): the release is the highest-versioned
    ``groundwire-alpha-*`` release whose pre-release box has been cleared
    ("promoted").  While none is promoted, the highest-versioned prerelease.
    ``prerelease=True`` takes the highest version either way.  Drafts never
    count, and GitHub's list order never counts.  Promoting a release
    (``gh release edit <tag> --prerelease=false --latest``) is therefore the
    act that hands it to new users and to ``gw update``."""
    cands = [r for r in releases
             if not r.get("draft") and str(r.get("tag_name", "")).startswith(TAG_PREFIX)]
    if not cands:
        raise GwError(f"no {TAG_PREFIX}* release to install")
    if not prerelease:
        promoted = [r for r in cands if not r.get("prerelease")]
        if promoted:
            cands = promoted
    return max(cands, key=lambda r: tag_version(r["tag_name"]))["tag_name"]


def resolve_latest(repo: str = RELEASE_REPO, prerelease: bool = False) -> str:
    """The tag ``latest`` means today; see ``pick_latest`` for the rule."""
    try:
        rels = json.loads(_get(f"https://api.github.com/repos/{repo}/releases?per_page=100"))
    except Exception as e:
        raise GwError(f"could not list releases of {repo}: {e}") from e
    try:
        return pick_latest(rels, prerelease)
    except GwError as e:
        raise GwError(f"{e} in {repo} (https://github.com/{repo}/releases)") from e


def fetch(tag: str, plat: str, redownload: bool = False, repo: str = RELEASE_REPO) -> Path:
    asset = f"groundwire-{plat}.tar.gz"
    dl = GW_DIR / "var" / "dl"
    dl.mkdir(parents=True, exist_ok=True)
    out = dl / f"{tag}-{asset}"
    if out.exists() and not redownload:
        return out
    url = f"https://github.com/{repo}/releases/download/{tag}/{asset}"
    info(f"Downloading {tag}")
    part = out.with_suffix(".part")
    try:
        req = urllib.request.Request(url, headers={"User-Agent": "gw"})
        with urllib.request.urlopen(req, timeout=60) as r, open(part, "wb") as fh:
            shutil.copyfileobj(r, fh, 1 << 20)
    except urllib.error.HTTPError as e:
        part.unlink(missing_ok=True)
        raise GwError(f"download failed ({e.code}). If {tag} has no {asset}, pick one that does: "
                      f"https://github.com/{repo}/releases") from e
    except Exception as e:
        part.unlink(missing_ok=True)
        raise GwError(f"download failed: {e}") from e
    os.replace(part, out)
    return out


def sha256_of(p: Path) -> str:
    h = hashlib.sha256()
    with open(p, "rb") as fh:
        for chunk in iter(lambda: fh.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


def verify(tarball: Path, tag: str, plat: str, expect: str = "", repo: str = RELEASE_REPO) -> str:
    got = sha256_of(tarball)
    if expect:
        if got != expect.lower():
            tarball.unlink(missing_ok=True)
            raise GwError(f"sha256 mismatch: expected {expect}, got {got}. Download deleted.")
    elif tag:
        try:
            sums = _get(f"https://github.com/{repo}/releases/download/{tag}/SHA256SUMS").decode()
        except Exception:
            sums = ""
        want = ""
        for line in sums.splitlines():
            parts = line.split()
            if len(parts) >= 2 and parts[1].lstrip("*") == f"groundwire-{plat}.tar.gz":
                want = parts[0].lower()
        if sums and not want:
            warn(f"SHA256SUMS has no line for groundwire-{plat}.tar.gz; the download is not verified")
        elif want and want != got:
            tarball.unlink(missing_ok=True)
            raise GwError("sha256 does NOT match the release's SHA256SUMS.\n"
                          f"    expected  {want}\n    got       {got}\n    The download has been deleted.")
    if not tarfile.is_tarfile(tarball):
        raise GwError("the downloaded tarball is not readable as tar")
    return got


def unpack(tarball: Path, tag: str) -> Path:
    """Unpack into releases/<tag>/ (older tarballs had one top-level directory)."""
    dest = GW_DIR / "releases" / tag
    if dest.exists() and all((dest / w).exists() for w in WANT):
        return dest
    tmp = Path(tempfile.mkdtemp(prefix=f".{tag}-", dir=str(GW_DIR / "releases")))
    try:
        with tarfile.open(tarball, "r:gz") as tf:
            _safe_extract(tf, tmp)
        if not (tmp / "gw-vere").exists():
            tops = [d for d in tmp.iterdir() if d.is_dir()]
            if len(tops) == 1 and (tops[0] / "gw-vere").exists():
                for item in tops[0].iterdir():
                    shutil.move(str(item), str(tmp / item.name))
                tops[0].rmdir()
        for w in ("gw-vere", "gw-base.pill"):
            if not (tmp / w).exists():
                raise GwError(f"the release tarball has no {w} in it")
        for w in ("gw-vere", "comet_miner", "gw", "boot.sh"):
            if (tmp / w).exists():
                os.chmod(tmp / w, 0o755)
        if dest.exists():
            shutil.rmtree(dest)
        os.replace(tmp, dest)
    finally:
        shutil.rmtree(tmp, ignore_errors=True)
    return dest


def _safe_extract(tf: tarfile.TarFile, dest: Path) -> None:
    base = dest.resolve()
    for m in tf.getmembers():
        target = (dest / m.name).resolve()
        if not str(target).startswith(str(base)):
            raise GwError(f"tarball member escapes the destination: {m.name}")
    try:
        tf.extractall(dest, filter="data")          # 3.12+
    except TypeError:
        tf.extractall(dest)


def switch(dest: Path, tag: str, plat: str, sha: str) -> None:
    """Point current/ and bin/ at this release."""
    cur = GW_DIR / "current"
    tmp = GW_DIR / ".current.tmp"
    tmp.unlink(missing_ok=True)
    os.symlink(dest, tmp)
    os.replace(tmp, cur)
    bindir = GW_DIR / "bin"
    bindir.mkdir(parents=True, exist_ok=True)
    for w in ("gw", "gw-vere", "comet_miner", "gw-base.pill"):
        link = bindir / w
        if (dest / w).exists():
            if link.is_symlink() or link.exists():
                link.unlink()
            os.symlink(cur / w, link)
    write_release_info(tag, plat, sha, datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"))


def install(tag: str = "latest", expect_sha: str = "", from_file: str = "",
            redownload: bool = False, prerelease: bool = False) -> str:
    """Fetch (or take a file), verify, unpack, switch.  Returns the tag.
    ``prerelease`` only matters for ``latest``: take the newest build whether
    or not it has been promoted."""
    plat = platform_tag()
    if from_file:
        tarball = Path(from_file).expanduser().resolve()
        if not tarball.exists():
            raise GwError(f"no such file: {tarball}")
        if tag == "latest":
            tag = "local-" + sha256_of(tarball)[:12]
        # a file handed over with a real tag (the website's installer does
        # this) is still checked against that release's SHA256SUMS
        sha = verify(tarball, "" if tag.startswith("local-") else tag, plat, expect_sha)
    else:
        if tag == "latest":
            tag = resolve_latest(prerelease=prerelease)
        tarball = fetch(tag, plat, redownload)
        sha = verify(tarball, tag, plat, expect_sha)
    dest = unpack(tarball, tag)
    switch(dest, tag, plat, sha)
    return tag


def ensure_on_path() -> bool:
    """Make `gw` reachable from the user's shell, without a word.  True when
    the current PATH already finds it; False when only a new shell will,
    because the PATH line went into the shell's startup file."""
    bindir = GW_DIR / "bin"
    path = os.environ.get("PATH", "").split(os.pathsep)
    if str(bindir) in path:
        return True
    # a symlink in a writable directory already on the PATH
    for cand in ("/usr/local/bin", str(Path.home() / ".local" / "bin")):
        if cand not in path:
            continue
        d, link = Path(cand), Path(cand) / "gw"
        try:
            if not (d.is_dir() and os.access(d, os.W_OK)):
                continue
            if link.is_symlink():
                link.unlink()
            elif link.exists():
                continue                    # someone else's gw; leave it be
            os.symlink(bindir / "gw", link)
            return True
        except OSError:
            continue
    _add_to_shell_rc(bindir)
    return False


def _add_to_shell_rc(bindir: Path) -> None:
    """Append a PATH line to the startup file of the user's login shell."""
    shell = Path(os.environ.get("SHELL", "")).name
    home = Path.home()
    if shell == "fish":
        rc = home / ".config" / "fish" / "conf.d" / "groundwire.fish"
        line = f"fish_add_path {bindir}"
    else:
        if shell == "zsh":
            rc = home / ".zshrc"
        elif shell == "bash":
            rc = home / (".bash_profile" if sys.platform == "darwin" else ".bashrc")
        else:
            rc = home / ".profile"
        line = f'export PATH="{bindir}:$PATH"'
    try:
        if rc.exists() and str(bindir) in rc.read_text(errors="replace"):
            return
        rc.parent.mkdir(parents=True, exist_ok=True)
        with open(rc, "a") as fh:
            fh.write(f"\n# added by the groundwire installer\n{line}\n")
    except OSError:
        pass
