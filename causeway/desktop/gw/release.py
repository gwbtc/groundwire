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
import tarfile
import tempfile
import urllib.error
import urllib.request
from datetime import datetime, timezone
from pathlib import Path

from .cli import good, info, warn
from .layout import GW_DIR, RELEASE_REPO, GwError, platform_tag, write_release_info

WANT = ("gw-vere", "gw-base.pill", "comet_miner", "gw")


def _get(url: str, timeout: float = 30) -> bytes:
    req = urllib.request.Request(url, headers={"User-Agent": "gw", "Accept": "application/vnd.github+json"})
    with urllib.request.urlopen(req, timeout=timeout) as r:
        return r.read()


def resolve_latest(repo: str = RELEASE_REPO) -> str:
    """The newest release tag, prereleases included: the alpha channel is
    nothing but prereleases, and GitHub's /releases/latest hides them."""
    try:
        rels = json.loads(_get(f"https://api.github.com/repos/{repo}/releases?per_page=20"))
    except Exception as e:
        raise GwError(f"could not list releases of {repo}: {e}") from e
    for r in rels:
        if r.get("draft"):
            continue
        if str(r.get("tag_name", "")).startswith("groundwire-alpha-"):
            return r["tag_name"]
    for r in rels:
        if not r.get("draft"):
            return r["tag_name"]
    raise GwError(f"{repo} has no releases")


def fetch(tag: str, plat: str, redownload: bool = False, repo: str = RELEASE_REPO) -> Path:
    asset = f"groundwire-{plat}.tar.gz"
    dl = GW_DIR / "var" / "dl"
    dl.mkdir(parents=True, exist_ok=True)
    out = dl / f"{tag}-{asset}"
    if out.exists() and not redownload:
        info(f"already downloaded: {out}")
        return out
    url = f"https://github.com/{repo}/releases/download/{tag}/{asset}"
    info(url)
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
    info(f"sha256        {got}")
    if expect:
        if got != expect.lower():
            tarball.unlink(missing_ok=True)
            raise GwError(f"sha256 mismatch: expected {expect}, got {got}. Download deleted.")
        good("sha256 matches the digest you supplied")
    elif tag:
        try:
            sums = _get(f"https://github.com/{repo}/releases/download/{tag}/SHA256SUMS").decode()
        except Exception:
            warn("this release publishes no SHA256SUMS; only GitHub's TLS authenticates the download")
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
        elif want:
            good("sha256 matches the release's SHA256SUMS")
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
            redownload: bool = False) -> str:
    """Fetch (or take a file), verify, unpack, switch.  Returns the tag."""
    plat = platform_tag()
    if from_file:
        tarball = Path(from_file).expanduser().resolve()
        if not tarball.exists():
            raise GwError(f"no such file: {tarball}")
        if tag == "latest":
            tag = "local-" + sha256_of(tarball)[:12]
        sha = verify(tarball, "", plat, expect_sha)
    else:
        if tag == "latest":
            tag = resolve_latest()
        info(f"release       {tag} ({plat})")
        tarball = fetch(tag, plat, redownload)
        sha = verify(tarball, tag, plat, expect_sha)
    dest = unpack(tarball, tag)
    switch(dest, tag, plat, sha)
    good(f"installed {tag} under {GW_DIR}")
    return tag


def on_path_hint() -> str:
    """Where `gw` is reachable, and the line to add if it is not."""
    bindir = GW_DIR / "bin"
    for cand in ("/usr/local/bin", str(Path.home() / ".local" / "bin")):
        d = Path(cand)
        try:
            if os.access(d, os.W_OK) or (not d.exists() and os.access(d.parent, os.W_OK)):
                d.mkdir(parents=True, exist_ok=True)
                link = d / "gw"
                if link.is_symlink() or link.exists():
                    link.unlink()
                os.symlink(bindir / "gw", link)
                if cand in os.environ.get("PATH", "").split(os.pathsep):
                    return f"gw is on your PATH ({link})"
        except OSError:
            continue
    return f'add it to your PATH:  export PATH="{bindir}:$PATH"'
