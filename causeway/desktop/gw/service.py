"""The ship runs under the operating system, not under gw.

systemd on Linux (a system unit as root, a user unit otherwise), launchd on
macOS.  ``Restart=always`` / ``KeepAlive`` is the whole supervisor: the %mach
light client reconnects on its own, so nothing has to clear peers after a
crash any more, and a reboot brings the ship back.  Where there is no
service manager (a container, a locked-down account) the ship runs as a
plain detached process, as before.
"""

from __future__ import annotations

import os
import plistlib
import shutil
import subprocess
import sys
from pathlib import Path

from .layout import GW_DIR
from .ship import Ship


def _run(args: list[str], **kw) -> subprocess.CompletedProcess:
    return subprocess.run(args, capture_output=True, text=True, timeout=120, **kw)


def kind() -> str:
    """systemd | launchd | detached"""
    if sys.platform == "darwin" and shutil.which("launchctl"):
        return "launchd"
    if sys.platform.startswith("linux") and shutil.which("systemctl"):
        probe = _run(["systemctl"] + _user_flag() + ["--version"])
        if probe.returncode == 0:
            return "systemd"
    return "detached"


def _user_flag() -> list[str]:
    return [] if os.geteuid() == 0 else ["--user"]


def unit_name(ship: Ship) -> str:
    return f"gw-{ship.name}"


def _unit_path(ship: Ship) -> Path:
    if os.geteuid() == 0:
        return Path("/etc/systemd/system") / f"{unit_name(ship)}.service"
    return Path.home() / ".config" / "systemd" / "user" / f"{unit_name(ship)}.service"


def _plist_path(ship: Ship) -> Path:
    return Path.home() / "Library" / "LaunchAgents" / f"me.groundwire.{ship.name}.plist"


def install(ship: Ship) -> str:
    """Write (or rewrite) the unit for this ship's current settings.  Returns the kind."""
    k = kind()
    argv = ship.restart_argv()
    ship.log.parent.mkdir(parents=True, exist_ok=True)
    if k == "systemd":
        p = _unit_path(ship)
        p.parent.mkdir(parents=True, exist_ok=True)
        exec_line = " ".join(_sq(a) for a in argv)
        p.write_text(f"""[Unit]
Description=Groundwire ship {ship.cfg.patp}
After=network-online.target
Wants=network-online.target

[Service]
Type=simple
WorkingDirectory={ship.pier.parent}
ExecStartPre=/bin/rm -f {ship.pier}/.vere.lock
ExecStart={exec_line}
Restart=always
RestartSec=10
KillSignal=SIGTERM
TimeoutStopSec=90
StandardOutput=append:{ship.log}
StandardError=append:{ship.log}
LimitNOFILE=65536

[Install]
WantedBy={"multi-user.target" if os.geteuid() == 0 else "default.target"}
""")
        _run(["systemctl"] + _user_flag() + ["daemon-reload"])
        _run(["systemctl"] + _user_flag() + ["enable", unit_name(ship)])
        if os.geteuid() != 0:
            _run(["loginctl", "enable-linger", os.environ.get("USER", "")])
    elif k == "launchd":
        p = _plist_path(ship)
        p.parent.mkdir(parents=True, exist_ok=True)
        plist = {
            "Label": f"me.groundwire.{ship.name}",
            "ProgramArguments": argv,
            "WorkingDirectory": str(ship.pier.parent),
            "RunAtLoad": True,
            "KeepAlive": True,
            "StandardOutPath": str(ship.log),
            "StandardErrorPath": str(ship.log),
            "ExitTimeOut": 90,
        }
        with open(p, "wb") as fh:
            plistlib.dump(plist, fh)
    return k


def start(ship: Ship) -> str:
    k = install(ship)
    (ship.pier / ".vere.lock").unlink(missing_ok=True)
    if k == "systemd":
        r = _run(["systemctl"] + _user_flag() + ["restart", unit_name(ship)])
        if r.returncode != 0:
            raise RuntimeError(f"systemctl start failed: {r.stderr.strip() or r.stdout.strip()}")
    elif k == "launchd":
        dom = f"gui/{os.getuid()}"
        _run(["launchctl", "bootout", dom, str(_plist_path(ship))])
        r = _run(["launchctl", "bootstrap", dom, str(_plist_path(ship))])
        if r.returncode != 0:
            raise RuntimeError(f"launchctl bootstrap failed: {r.stderr.strip() or r.stdout.strip()}")
    else:
        ship.spawn_detached(ship.restart_argv())
    return k


def stop(ship: Ship) -> bool:
    """Tell the service manager to stop and stay stopped, then make sure the
    processes are gone.  True when nothing is left running."""
    k = kind()
    if k == "systemd" and _unit_path(ship).exists():
        _run(["systemctl"] + _user_flag() + ["stop", unit_name(ship)])
    elif k == "launchd" and _plist_path(ship).exists():
        _run(["launchctl", "bootout", f"gui/{os.getuid()}", str(_plist_path(ship))])
    return ship.terminate()


def forget(ship: Ship) -> None:
    """Remove the unit (a ship being deleted or moved)."""
    k = kind()
    if k == "systemd":
        _run(["systemctl"] + _user_flag() + ["disable", unit_name(ship)])
        _unit_path(ship).unlink(missing_ok=True)
        _run(["systemctl"] + _user_flag() + ["daemon-reload"])
    elif k == "launchd":
        _run(["launchctl", "bootout", f"gui/{os.getuid()}", str(_plist_path(ship))])
        _plist_path(ship).unlink(missing_ok=True)


def state(ship: Ship) -> str:
    k = kind()
    if k == "systemd" and _unit_path(ship).exists():
        r = _run(["systemctl"] + _user_flag() + ["is-active", unit_name(ship)])
        return f"systemd {unit_name(ship)}: {r.stdout.strip() or 'unknown'}"
    if k == "launchd" and _plist_path(ship).exists():
        r = _run(["launchctl", "print", f"gui/{os.getuid()}/me.groundwire.{ship.name}"])
        return f"launchd me.groundwire.{ship.name}: {'loaded' if r.returncode == 0 else 'not loaded'}"
    return "no service unit (plain background process)"


def _sq(s: str) -> str:
    return s if all(c.isalnum() or c in "-_./:=~" for c in s) else '"' + s.replace('"', '\\"') + '"'


__all__ = ["kind", "install", "start", "stop", "forget", "state", "unit_name", "GW_DIR"]
