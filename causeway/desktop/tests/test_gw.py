"""The gw command: what can be checked without a ship.

A fake comet_miner stands in for the real one so the candidates and prefix
paths run end to end; the socket client's request shape is checked against
the envelope gwlib.sh sent for a year; the shim that replaced boot.sh is
checked for every flag it translates.
"""

from __future__ import annotations

import os
import pathlib
import stat
import subprocess
import sys

import pytest

HERE = pathlib.Path(__file__).resolve().parent.parent
sys.path.insert(0, str(HERE))

import causeway as cw  # noqa: E402
from gw import cli, layout, noun as N, peers, populate, service  # noqa: E402
from gw.connsock import SPIDER_BEAMS  # noqa: E402
from gw.ship import Ship, hoon_hex, ip_hoon, strand  # noqa: E402


# ------------------------------------------------------------------ layout --

def test_ship_toml_round_trip(tmp_path, monkeypatch):
    monkeypatch.setattr(layout, "GW_DIR", tmp_path)
    cfg = layout.ShipConfig(name="losref-samfet-fabreb-nismul--matsyn-fonfen-hinrut-daplyd",
                            http_port=8082, ames_port=34567, domain="x.groundwire.me", vps=True)
    cfg.extra["note"] = 'quoted "text"'
    cfg.save()
    back = layout.ShipConfig.load("~" + cfg.name)
    assert back.http_port == 8082 and back.ames_port == 34567
    assert back.vps is True and back.domain == "x.groundwire.me"
    assert back.extra["note"] == 'quoted "text"'
    assert back.patp.startswith("~losref")


def test_ship_toml_adopts_an_old_env_file(tmp_path, monkeypatch):
    monkeypatch.setattr(layout, "GW_DIR", tmp_path)
    (tmp_path / "var").mkdir()
    (tmp_path / "var" / "sampel.env").write_text(
        "GW_NAME='sampel'\nGW_HTTP_PORT='8081'\nGW_AMES_PORT='40000'\nGW_DOMAIN='s.groundwire.me'\nGW_VPS='1'\n")
    cfg = layout.ShipConfig.load("sampel")
    assert (cfg.http_port, cfg.ames_port, cfg.domain, cfg.vps) == (8081, 40000, "s.groundwire.me", True)


def test_validate_patp():
    assert layout.validate_patp("~losref-samfet-fabreb-nismul--matsyn-fonfen-hinrut-daplyd").startswith("losref")
    assert layout.validate_patp("sampel-palnet-sampel-palnet") == "sampel-palnet-sampel-palnet"
    for bad in ("", "~zod", "~sampel palnet", "~a-b"):
        with pytest.raises(layout.GwError):
            layout.validate_patp(bad)


def test_the_ship_needs_exactly_one_without_a_name(tmp_path, monkeypatch):
    monkeypatch.setattr(layout, "GW_DIR", tmp_path)
    with pytest.raises(layout.GwError):
        layout.the_ship(None)
    for n in ("one-pier-one-pier-one-pier-one-pier", "two-pier-two-pier-two-pier-two-pier"):
        (tmp_path / "piers" / n / ".urb").mkdir(parents=True)
    with pytest.raises(layout.GwError):
        layout.the_ship(None)


# -------------------------------------------------------------- the client --

def test_hoon_literals():
    assert hoon_hex("0000abcdef12") == "0xabcd.ef12"
    assert hoon_hex("0") == "0x0"
    assert ip_hoon("192.168.1.10") == "0xc0a8.010a"
    assert ip_hoon("8.8.8.8") == "0x808.0808"


def test_khan_request_matches_the_gwlib_envelope():
    """[rid %fyrd %base %khan-eval %noun %ted-eval [source deps]] where the deps
    are /sur/spider/hoon and /lib/strandio/hoon, exactly what gwl_thread sent."""
    body = strand("(pure:m !>((scot %p our)))")
    assert body.startswith("=/  m  (strand ,vase)\n^-  form:m\n;<  our=@p   bind:m  get-our\n;<  now=@da  bind:m  get-time\n")
    page = (N.cord(body), SPIDER_BEAMS)
    req = (1, (N.tas("fyrd"), (N.tas("base"), (N.tas("khan-eval"), (N.tas("noun"), (N.tas("ted-eval"), page))))))
    assert N.cue(N.jam(req)) == req
    deps = [[N.from_tas(k) for k in N.list_iter(p)] for p in N.list_iter(SPIDER_BEAMS)]
    assert deps == [["sur", "spider", "hoon"], ["lib", "strandio", "hoon"]]
    assert N.newt_encode(req)[0] == 0


def _peek_response(agent: str, spur: str, data):
    """What vere 4.6-828244e answers a conn.sock %peek with, measured on the
    unattested test comet 2026-11-05: (unit [%omen path cask])."""
    path = N.path("gx", "~sampel-palnet", agent, "~2026.11.5", spur, "noun")
    return (0, (N.tas("omen"), (path, (N.tas("noun"), data))))


def test_peek_unwrap_and_decoders(monkeypatch):
    from gw.connsock import _unwrap_cask
    synced = (N.tas("is-synced"), 1)
    assert _unwrap_cask(_peek_response("bitcoin-client", "is-synced", synced)) == synced
    #  older forms: [~ [~ cask]] and a bare [path cask]
    assert _unwrap_cask((0, (0, (N.tas("noun"), synced)))) == synced
    assert _unwrap_cask((N.path("gx", "x"), (N.tas("noun"), synced))) == synced
    assert _unwrap_cask(0) is None

    cfg = layout.ShipConfig(name="sampel-palnet-sampel-palnet")
    ship = Ship(cfg)
    answers = {
        "/is-synced": (N.tas("is-synced"), 0),
        "/best-block": (N.tas("best-block"), (N.tas("new"), (974_683, 0xabc))),
        "/peers": (N.tas("peers"), (N.tas("all"), ((("k", "v"), ((("k2", "v2"), (0, 0)), 0))))),
    }
    monkeypatch.setattr(ship, "peek", lambda agent, path, timeout=60.0: answers.get(path))
    assert ship.synced() is True
    assert ship.best_block() == 974_683
    assert ship.peer_count() == 2
    answers["/is-synced"] = (N.tas("is-synced"), 1)
    assert ship.synced() is False
    answers["/is-synced"] = None
    assert ship.synced() is None


# ------------------------------------------------------------------- peers --

def test_peer_pool_hands_each_address_out_once(tmp_path, monkeypatch):
    monkeypatch.setattr(layout, "GW_DIR", tmp_path)
    monkeypatch.setattr(peers, "var_dir", lambda: tmp_path / "var")
    (tmp_path / "var").mkdir()
    (tmp_path / "var" / "peerpool.txt").write_text("1.1.1.1\n2.2.2.2\n3.3.3.3\n")
    assert peers.pool_left("a") == 3
    assert peers.take("a", 2) == ["1.1.1.1", "2.2.2.2"]
    assert peers.pool_left("a") == 1
    assert peers.take("a", 5) == ["3.3.3.3"]
    assert peers.take("a", 5) == []
    assert peers.pool_left("b") == 3          # another ship, its own ledger


# ----------------------------------------------------------------- service --

def test_systemd_unit_text_names_the_pier_and_restarts(tmp_path, monkeypatch):
    monkeypatch.setattr(layout, "GW_DIR", tmp_path)
    monkeypatch.setattr(service, "kind", lambda: "systemd")
    monkeypatch.setattr(service, "_run", lambda *a, **k: subprocess.CompletedProcess(a, 0, "", ""))
    monkeypatch.setattr(service, "_unit_path", lambda ship: tmp_path / "unit.service")
    cfg = layout.ShipConfig(name="sampel-palnet-sampel-palnet", http_port=8081, ames_port=34567, loom=32)
    ship = Ship(cfg)
    assert service.install(ship) == "systemd"
    text = (tmp_path / "unit.service").read_text()
    assert "Restart=always" in text
    assert str(ship.pier) in text and "--http-port 8081" in text and "-p 34567" in text
    assert "-c " not in text and "-G " not in text          # the restart form, never a boot


# ---------------------------------------------------------------- populate --

def test_populate_steps_are_named_and_ordered():
    names = [s.name for s in populate.STEPS]
    assert names[:3] == ["peer-discovery", "sponsor", "hi"]
    assert "wallet-sweep" in names and "armillary" in names
    assert all(s.agent for s in populate.STEPS)


# --------------------------------------------------------------------- cli --

@pytest.mark.parametrize("argv", [
    ["install"], ["mint", "--invite", "x", "--prefix", "los", "--candidates", "3", "--pick", "2"],
    ["start", "~sampel", "--feed-file", "f"], ["stop", "~sampel"], ["status", "--watch"],
    ["code"], ["logs", "-n", "5"], ["ctl", "-s", "~sampel", "scry", "bitcoin-client", "/is-synced"],
    ["ctl", "poke", "hood", "helm-hi", "!>(~)"], ["name"], ["peers", "--count", "5"],
    ["populate", "--only", "hi"], ["update", "--version", "groundwire-alpha-2026.11.5"],
    ["invite", "create"], ["invite", "show", "01ab"], ["mine", "--prefix", "los"], ["ships"], ["version"],
])
def test_every_subcommand_parses(argv):
    args = cli.build_parser().parse_args(argv)
    assert args.cmd == argv[0]


def test_causeway_passthrough_is_not_parsed(monkeypatch):
    seen = {}
    from gw import commands
    monkeypatch.setattr(commands, "cmd_causeway", lambda which, rest: seen.update(which=which, rest=rest) or 0)
    assert cli.main(["causeway", "spawn", "generate", "--invite", "x"]) == 0
    assert seen == {"which": "causeway", "rest": ["spawn", "generate", "--invite", "x"]}


# ------------------------------------------------------- causeway additions --

FAKE_MINER = """#!/usr/bin/env python3
import os, random, sys
# a comet name whose first syllables come from a counter file, so a prefix
# search terminates deterministically
args = sys.argv[1:]
prefix = None
if "--prefix" in args:
    prefix = args[args.index("--prefix") + 1]
n_file = os.environ["FAKE_MINER_COUNTER"]
n = int(open(n_file).read() or 0) + 1
open(n_file, "w").write(str(n))
syll = ["losref", "samfet", "fabreb", "nismul"]
first = prefix if prefix else (syll[n % 4] if n % 3 else "lostec")
print("seed: 0w1")
print("ring: 0w2")
print(f"comet: ~{first}-samfet-fabreb-nismul--matsyn-fonfen-hinrut-daplyd")
print("feed: 0w3.feed")
"""


@pytest.fixture
def fake_miner(tmp_path):
    m = tmp_path / "comet_miner"
    m.write_text(FAKE_MINER)
    m.chmod(0o755)
    counter = tmp_path / "counter"
    counter.write_text("0")
    os.environ["FAKE_MINER_COUNTER"] = str(counter)
    yield m, counter
    os.environ.pop("FAKE_MINER_COUNTER", None)


def test_candidates_lists_names_and_pick_takes_one(fake_miner, capsys, monkeypatch):
    miner, counter = fake_miner
    monkeypatch.setattr(cw, "patp_to_mnemonym", lambda p: ".nym." + p[1:7])
    r = cw.mine_comet_choice("ab" * 32, 0, 0, str(miner), candidates=3, pick=2)
    out = capsys.readouterr().out
    assert "candidate 3/3" in out and "Taking" in out
    assert r["comet"].startswith("~") and r["feed"] == "0w3.feed"
    assert int(counter.read_text()) == 3


def test_one_syllable_prefix_reruns_the_miner_until_it_matches(fake_miner, monkeypatch):
    miner, counter = fake_miner
    monkeypatch.setattr(cw, "patp_to_mnemonym", lambda p: p)
    r = cw.mine_comet_choice("ab" * 32, 0, 0, str(miner), prefix="los", candidates=1)
    assert r["comet"].startswith("~los")
    assert int(counter.read_text()) >= 1


def test_six_letter_prefix_goes_to_the_miner(fake_miner, monkeypatch):
    miner, counter = fake_miner
    monkeypatch.setattr(cw, "patp_to_mnemonym", lambda p: p)
    r = cw.mine_comet_choice("ab" * 32, 0, 0, str(miner), prefix="losref", candidates=1)
    assert r["comet"].startswith("~losref") and int(counter.read_text()) == 1


def test_prefix_validation():
    import click
    assert cw.validate_name_prefix("~LOS") == "los"
    assert cw.validate_name_prefix("losref") == "losref"
    for bad in ("lo", "losr", "los1", "zzzzzz"):
        with pytest.raises(click.UsageError):
            cw.validate_name_prefix(bad)


def test_phrase_file_is_0600_and_never_overwritten(tmp_path):
    import click
    p = tmp_path / "phrase"
    cw.write_phrase_file(str(p), "abandon " * 11 + "about")
    assert stat.S_IMODE(p.stat().st_mode) == 0o600
    assert p.read_text().split()[-1] == "about"
    with pytest.raises(click.UsageError):
        cw.write_phrase_file(str(p), "other")


# -------------------------------------------------------------------- shim --

def test_boot_sh_is_a_shim_that_translates_to_gw():
    shim = HERE.parent / "public" / "boot.sh"
    src = shim.read_text()
    assert len(src.splitlines()) < 90
    for old, new in (("--mint", 'exec "$GW" mint'), ("--status", 'exec "$GW" status'),
                     ("--code", 'exec "$GW" code'), ("--stop", 'exec "$GW" stop'),
                     ("--comet", 'exec "$GW" start "$comet"')):
        assert old in src and new in src
    assert "gwlib" not in src and "gwsup" not in src
    r = subprocess.run(["bash", "-n", str(shim)], capture_output=True)
    assert r.returncode == 0, r.stderr
