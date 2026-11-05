"""Where everything lives on this machine.

One install root (``~/.groundwire`` or ``$GROUNDWIRE_DIR``):

    bin/                 gw, gw-vere, comet_miner, gw-base.pill (symlinks into current/)
    current -> releases/<tag>
    releases/<tag>/      one unpacked release tarball
    piers/<name>/        the ship (never written by anything but vere)
    ships/<name>/ship.toml   the ship's settings, written at mint, read by every command
    var/                 logs, the mint directory (0700), proofs, markers, download cache

The frozen ``gw`` binary finds gw-vere, comet_miner and the pill beside its
own real path (Causeway does the same when ``sys.frozen`` is set).  A dev
checkout points ``GROUNDWIRE_HOME`` at an install root instead.
"""

from __future__ import annotations

import os
import platform as _platform
import sys
from dataclasses import dataclass, field, fields
from pathlib import Path

GW_DIR = Path(os.environ.get("GROUNDWIRE_DIR") or (Path.home() / ".groundwire"))
DEFAULT_GATEWAY = os.environ.get("GW_GATEWAY", "http://146.190.199.0:80")
DEFAULT_SPONSOR = "~barmul-bolmet-ronlus-lighul--rovtun-satryc-moclug-daplyd"
DEFAULT_HTTP_PORT = int(os.environ.get("GROUNDWIRE_PORT", "8080"))
DEFAULT_LOOM = int(os.environ.get("GROUNDWIRE_LOOM", "32"))
RELEASE_REPO = os.environ.get("GROUNDWIRE_REPO", "gwbtc/release")
MEMPOOL_API = os.environ.get("MEMPOOL_API", "https://mempool.space/api")
MIN_FREE_GB = 20


class GwError(Exception):
    """A failure to report in one line and exit 1 on."""


def platform_tag() -> str:
    os_name = sys.platform
    arch = _platform.machine().lower()
    if os_name.startswith("linux"):
        if arch in ("x86_64", "amd64"):
            return "linux-x86_64"
        if arch in ("aarch64", "arm64"):
            return "linux-aarch64"
    if os_name == "darwin":
        if arch in ("arm64", "aarch64"):
            return "macos-aarch64"
        raise GwError("Intel macOS is not supported: the release builds no macos-x86_64 "
                      "artifact, and Rosetta cannot run the arm64 one.")
    raise GwError(f"unsupported platform {os_name}/{arch}; the release builds "
                  "linux-x86_64, linux-aarch64 and macos-aarch64")


def bin_dir() -> Path:
    """The directory holding gw-vere, comet_miner and gw-base.pill."""
    if getattr(sys, "frozen", False):
        return Path(sys.executable).resolve().parent
    home = os.environ.get("GROUNDWIRE_HOME")
    if home:
        return Path(home) / "bin"
    return GW_DIR / "bin"


def vere_bin() -> Path:
    return bin_dir() / "gw-vere"


def miner_bin() -> Path:
    return bin_dir() / "comet_miner"


def pill_file() -> Path:
    return bin_dir() / "gw-base.pill"


def gw_bin() -> Path:
    """How to run ourselves again (``gw causeway ...``, the TUI, a unit)."""
    if getattr(sys, "frozen", False):
        return Path(sys.executable)
    return Path(sys.executable)


def self_argv() -> list[str]:
    """argv prefix that re-enters this program: the frozen binary, or
    ``python -m gw`` in a checkout."""
    if getattr(sys, "frozen", False):
        return [sys.executable]
    return [sys.executable, "-m", "gw"]


def piers_dir() -> Path:
    return GW_DIR / "piers"


def var_dir() -> Path:
    return GW_DIR / "var"


def mint_dir() -> Path:
    return var_dir() / "mint"


def ships_dir() -> Path:
    return GW_DIR / "ships"


def ensure_layout() -> None:
    for d in (GW_DIR, piers_dir(), var_dir(), ships_dir(), GW_DIR / "bin", GW_DIR / "releases"):
        d.mkdir(parents=True, exist_ok=True)
    os.chmod(GW_DIR, 0o700)


# ---------------------------------------------------------------- ship.toml --

@dataclass
class ShipConfig:
    name: str                       # the @p without the ~
    release: str = ""
    http_port: int = DEFAULT_HTTP_PORT
    ames_port: int = 0              # 0 = let vere pick
    loom: int = DEFAULT_LOOM
    gateway: str = DEFAULT_GATEWAY
    sponsor: str = DEFAULT_SPONSOR
    domain: str = ""
    vps: bool = False
    peer_discovery: bool = True
    armillary: bool = True
    bitcoin: bool = True
    supervised: str = ""            # "systemd" | "launchd" | "detached" | ""
    created: str = ""
    extra: dict = field(default_factory=dict)

    @property
    def patp(self) -> str:
        return "~" + self.name

    @property
    def dir(self) -> Path:
        return ships_dir() / self.name

    @property
    def path(self) -> Path:
        return self.dir / "ship.toml"

    def save(self) -> None:
        self.dir.mkdir(parents=True, exist_ok=True)
        lines = ["# written by gw; edit with care, every command reads it", ""]
        for f in fields(self):
            if f.name == "extra":
                continue
            lines.append(f"{f.name} = {_toml_value(getattr(self, f.name))}")
        for k, v in self.extra.items():
            lines.append(f"{k} = {_toml_value(v)}")
        tmp = self.path.with_suffix(".tmp")
        tmp.write_text("\n".join(lines) + "\n")
        os.replace(tmp, self.path)

    @classmethod
    def load(cls, name: str) -> "ShipConfig":
        name = name.lstrip("~")
        p = ships_dir() / name / "ship.toml"
        if p.exists():
            data = _read_toml(p)
            known = {f.name for f in fields(cls)} - {"extra"}
            cfg = cls(name=name)
            for k, v in data.items():
                if k in known:
                    setattr(cfg, k, v)
                else:
                    cfg.extra[k] = v
            cfg.name = name
            return cfg
        return cls._adopt_env(name)

    @classmethod
    def _adopt_env(cls, name: str) -> "ShipConfig":
        """A pre-gw install kept var/<name>.env; read it once and keep going."""
        cfg = cls(name=name)
        env = var_dir() / f"{name}.env"
        if env.exists():
            for line in env.read_text().splitlines():
                if "=" not in line:
                    continue
                k, v = line.split("=", 1)
                v = v.strip().strip("'\"")
                if k == "GW_HTTP_PORT" and v.isdigit():
                    cfg.http_port = int(v)
                elif k == "GW_AMES_PORT" and v.isdigit():
                    cfg.ames_port = int(v)
                elif k == "GW_LOOM" and v.isdigit():
                    cfg.loom = int(v)
                elif k == "GW_DOMAIN":
                    cfg.domain = v
                elif k == "GW_VPS":
                    cfg.vps = v == "1"
            rel = release_info()
            cfg.release = rel.get("tag", "")
        return cfg


def _toml_value(v) -> str:
    if isinstance(v, bool):
        return "true" if v else "false"
    if isinstance(v, int):
        return str(v)
    s = str(v).replace("\\", "\\\\").replace('"', '\\"')
    return f'"{s}"'


def _read_toml(p: Path) -> dict:
    try:
        import tomllib  # 3.11+
        with open(p, "rb") as fh:
            return tomllib.load(fh)
    except ImportError:
        pass
    out: dict = {}
    for line in p.read_text().splitlines():
        line = line.strip()
        if not line or line.startswith("#") or "=" not in line:
            continue
        k, v = (x.strip() for x in line.split("=", 1))
        if v in ("true", "false"):
            out[k] = v == "true"
        elif v.lstrip("-").isdigit():
            out[k] = int(v)
        else:
            out[k] = v.strip('"').replace('\\"', '"').replace("\\\\", "\\")
    return out


def list_ships() -> list[str]:
    """Every ship this install knows: a ship.toml or a pier."""
    names: set[str] = set()
    if ships_dir().exists():
        for d in ships_dir().iterdir():
            if (d / "ship.toml").exists():
                names.add(d.name)
    if piers_dir().exists():
        for d in piers_dir().iterdir():
            if (d / ".urb").exists():
                names.add(d.name)
    return sorted(names)


def the_ship(name: str | None) -> ShipConfig:
    """Resolve ``--comet``/positional name, or the lone ship here."""
    if name:
        return ShipConfig.load(name.strip().lstrip("~"))
    ships = list_ships()
    if len(ships) == 1:
        return ShipConfig.load(ships[0])
    if not ships:
        raise GwError(f"no ship under {GW_DIR}; mint one with `gw mint`")
    raise GwError("more than one ship here; say which: " + ", ".join("~" + s for s in ships))


# --------------------------------------------------------------- release.txt --

def release_info() -> dict:
    p = var_dir() / "release.txt"
    out: dict = {}
    if p.exists():
        for line in p.read_text().splitlines():
            if "=" in line:
                k, v = line.split("=", 1)
                out[k.strip()] = v.strip()
    return out


def write_release_info(tag: str, plat: str, sha: str, when: str) -> None:
    var_dir().mkdir(parents=True, exist_ok=True)
    (var_dir() / "release.txt").write_text(
        f"tag={tag}\nplatform={plat}\nsha256={sha}\ninstalled={when}\n")


def validate_patp(s: str) -> str:
    """A comet is eight syllable pairs with a double hyphen in the middle; a
    moon-shaped name is four.  Returns the name without the ~."""
    n = s.strip().lstrip("~")
    if not n:
        raise GwError("a comet name is required (the ~name Causeway gave you)")
    if any(c not in "abcdefghijklmnopqrstuvwxyz-" for c in n):
        raise GwError(f"not a @p: {s}")
    if n.count("-") not in (3, 4, 7, 8):
        raise GwError(f"does not look like a comet or moon @p: {s}")
    return n
