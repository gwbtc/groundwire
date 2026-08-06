"""Tests for gw-onboard.py pure functions."""

import importlib.util
import os
import sys
import tempfile
import unittest
from unittest.mock import MagicMock, patch

# gw-onboard.py has a hyphen so we load it dynamically
spec = importlib.util.spec_from_file_location("gw_onboard", "./gw-onboard.py")
gw = importlib.util.module_from_spec(spec)
sys.modules["gw_onboard"] = gw
spec.loader.exec_module(gw)


class TestEncodeQ(unittest.TestCase):
    def test_zero(self):
        self.assertEqual(gw.encode_q(0), "~zod")

    def test_single_byte(self):
        self.assertEqual(gw.encode_q(255), "~fes")

    def test_known_ticket(self):
        self.assertEqual(gw.encode_q(578437695752307201), "~marbud-wansev-litsut-hidful")

    def test_all_suffixes_reachable(self):
        """Every suffix index 0-255 should produce a valid 3-char syllable."""
        for i in range(256):
            q = gw.encode_q(i)
            self.assertTrue(q.startswith("~"))
            self.assertEqual(len(q), 4)  # ~ + 3-char suffix


class TestDecodeQ(unittest.TestCase):
    def test_zero(self):
        self.assertEqual(gw.decode_q("~zod"), 0)

    def test_single_byte(self):
        self.assertEqual(gw.decode_q("~fes"), 255)

    def test_known_ticket(self):
        self.assertEqual(gw.decode_q("~marbud-wansev-litsut-hidful"), 578437695752307201)

    def test_invalid_syllable(self):
        with self.assertRaises(ValueError):
            gw.decode_q("~xyz")


class TestEncodeDecodeRoundtrip(unittest.TestCase):
    def test_roundtrip_zero(self):
        self.assertEqual(gw.decode_q(gw.encode_q(0)), 0)

    def test_roundtrip_small(self):
        for i in range(256):
            self.assertEqual(gw.decode_q(gw.encode_q(i)), i)

    def test_roundtrip_two_byte(self):
        for val in [256, 1000, 32768, 65535]:
            self.assertEqual(gw.decode_q(gw.encode_q(val)), val)

    def test_roundtrip_eight_byte(self):
        val = 578437695752307201
        self.assertEqual(gw.decode_q(gw.encode_q(val)), val)

    def test_roundtrip_random(self):
        """Roundtrip a spread of values across the 64-bit range."""
        import secrets

        for _ in range(50):
            val = int.from_bytes(secrets.token_bytes(8), "little")
            self.assertEqual(gw.decode_q(gw.encode_q(val)), val)


class TestDeriveTaprootAddress(unittest.TestCase):
    def test_known_vector(self):
        """Pin the address derivation so embit updates don't silently break it."""
        seed = bytes([0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08])
        self.assertEqual(
            gw.derive_taproot_address(seed),
            "tb1pcnud6dx8f3fd3ppdhq3nhprtkxa663j94t3zqx23ygjrc8utvcuqtenhft",
        )

    def test_deterministic(self):
        """Same seed must always produce the same address."""
        seed = bytes([0xFF] * 8)
        a1 = gw.derive_taproot_address(seed)
        a2 = gw.derive_taproot_address(seed)
        self.assertEqual(a1, a2)

    def test_different_seeds_differ(self):
        a1 = gw.derive_taproot_address(bytes([0x00] * 8))
        a2 = gw.derive_taproot_address(bytes([0x01] * 8))
        self.assertNotEqual(a1, a2)


class TestFormatHoonUx(unittest.TestCase):
    def test_known_value(self):
        self.assertEqual(gw.format_hoon_ux("deadbeefcafebabe"), "0xdead.beef.cafe.babe")

    def test_short_value(self):
        self.assertEqual(gw.format_hoon_ux("ff"), "0xff")

    def test_zero(self):
        self.assertEqual(gw.format_hoon_ux("0"), "0x0")

    def test_leading_zeros_stripped(self):
        self.assertEqual(gw.format_hoon_ux("000abc"), "0xabc")

    def test_five_chars(self):
        self.assertEqual(gw.format_hoon_ux("12345"), "0x1.2345")


class TestMakeTweakExpr(unittest.TestCase):
    def test_known_value(self):
        txid = "ab" * 32
        self.assertEqual(
            gw.make_tweak_expr(txid, 0, 0),
            "(rap 3 ~[%9 ~tyr %urb-watcher %btc %gw %9 0xabab.abab.abab.abab.abab.abab.abab.abab"
            ".abab.abab.abab.abab.abab.abab.abab.abab 0 0])",
        )

    def test_vout_and_off(self):
        result = gw.make_tweak_expr("ff" * 32, 3, 7)
        self.assertIn(" 3 7])", result)


class TestDetectZigTarget(unittest.TestCase):
    def test_linux_x86_64(self):
        with (
            patch("platform.machine", return_value="x86_64"),
            patch("platform.system", return_value="Linux"),
        ):
            self.assertEqual(gw._detect_zig_target(), "x86_64-linux-none")

    def test_macos_arm64(self):
        with (
            patch("platform.machine", return_value="arm64"),
            patch("platform.system", return_value="Darwin"),
        ):
            self.assertEqual(gw._detect_zig_target(), "aarch64-macos-none")

    def test_linux_aarch64(self):
        with (
            patch("platform.machine", return_value="aarch64"),
            patch("platform.system", return_value="Linux"),
        ):
            self.assertEqual(gw._detect_zig_target(), "aarch64-linux-none")

    def test_macos_x86_64(self):
        with (
            patch("platform.machine", return_value="x86_64"),
            patch("platform.system", return_value="Darwin"),
        ):
            self.assertEqual(gw._detect_zig_target(), "x86_64-macos-none")


class TestNormalizeTicket(unittest.TestCase):
    def test_already_normalized(self):
        self.assertEqual(
            gw.normalize_ticket("~marbud-wansev-litsut-hidful"), "~marbud-wansev-litsut-hidful"
        )

    def test_missing_tilde(self):
        self.assertEqual(
            gw.normalize_ticket("marbud-wansev-litsut-hidful"), "~marbud-wansev-litsut-hidful"
        )

    def test_whitespace(self):
        self.assertEqual(
            gw.normalize_ticket("  ~marbud-wansev-litsut-hidful  "), "~marbud-wansev-litsut-hidful"
        )

    def test_whitespace_no_tilde(self):
        self.assertEqual(
            gw.normalize_ticket("  marbud-wansev-litsut-hidful\n"), "~marbud-wansev-litsut-hidful"
        )


class TestConfirmMasterTicket(unittest.TestCase):
    def test_correct_on_first_try(self):
        ticket = "~marbud-wansev-litsut-hidful"
        with patch("builtins.input", return_value="~marbud-wansev-litsut-hidful"):
            gw.confirm_master_ticket(ticket)

    def test_correct_without_tilde(self):
        ticket = "~marbud-wansev-litsut-hidful"
        with patch("builtins.input", return_value="marbud-wansev-litsut-hidful"):
            gw.confirm_master_ticket(ticket)

    def test_wrong_then_correct(self):
        ticket = "~marbud-wansev-litsut-hidful"
        with patch("builtins.input", side_effect=["~wrong-ticket", "~marbud-wansev-litsut-hidful"]):
            gw.confirm_master_ticket(ticket)

    def test_eof_then_correct(self):
        ticket = "~marbud-wansev-litsut-hidful"
        with patch("builtins.input", side_effect=[EOFError, "~marbud-wansev-litsut-hidful"]):
            gw.confirm_master_ticket(ticket)


class TestCopyToClipboard(unittest.TestCase):
    def test_no_clipboard_tool_available(self):
        with patch("shutil.which", return_value=None):
            self.assertFalse(gw.copy_to_clipboard("test"))

    def test_darwin_pbcopy(self):
        with (
            patch.object(gw.platform, "system", return_value="Darwin"),
            patch("shutil.which", return_value="/usr/bin/pbcopy"),
            patch("subprocess.run") as mock_run,
        ):
            mock_run.return_value = MagicMock(returncode=0)
            self.assertTrue(gw.copy_to_clipboard("hello"))
            mock_run.assert_called_once_with(
                ["pbcopy"], input="hello", text=True, check=True, timeout=5
            )

    def test_linux_xclip(self):
        with (
            patch.object(gw.platform, "system", return_value="Linux"),
            patch.object(gw.platform, "uname", return_value=MagicMock(release="5.15.0-generic")),
            patch.dict(gw.os.environ, {}, clear=True),
            patch("shutil.which", side_effect=lambda x: "/usr/bin/xclip" if x == "xclip" else None),
            patch("subprocess.run") as mock_run,
        ):
            mock_run.return_value = MagicMock(returncode=0)
            self.assertTrue(gw.copy_to_clipboard("hello"))
            mock_run.assert_called_once_with(
                ["xclip", "-selection", "clipboard"],
                input="hello",
                text=True,
                check=True,
                timeout=5,
            )

    def test_subprocess_error_returns_false(self):
        import subprocess

        with (
            patch.object(gw.platform, "system", return_value="Darwin"),
            patch("shutil.which", return_value="/usr/bin/pbcopy"),
            patch("subprocess.run", side_effect=subprocess.SubprocessError),
        ):
            self.assertFalse(gw.copy_to_clipboard("hello"))


class TestAgentInstructionFiles(unittest.TestCase):
    def test_creates_agents_stub_and_claude_symlink(self):
        with tempfile.TemporaryDirectory() as tmp:
            gw._write_agent_instruction_files(
                tmp, "~sample-pier", "sample-pier", attested_to_bitcoin=False
            )

            agents_path = os.path.join(tmp, "AGENTS.md")
            claude_path = os.path.join(tmp, "CLAUDE.md")
            self.assertTrue(os.path.isfile(agents_path))
            with open(agents_path, encoding="utf-8") as f:
                content = f.read()
            self.assertIn("You are ~sample-pier", content)
            self.assertIn("configured for you as sample-pier", content)
            self.assertIn(
                "You do not have a permanent, sybil-resistant Groundwire ID attested to on the Bitcoin mainnet.",
                content,
            )
            self.assertTrue(os.path.islink(claude_path))
            self.assertEqual(os.readlink(claude_path), "AGENTS.md")

    def test_preserves_existing_agents_content(self):
        with tempfile.TemporaryDirectory() as tmp:
            agents_path = os.path.join(tmp, "AGENTS.md")
            with open(agents_path, "w", encoding="utf-8") as f:
                f.write("custom instructions\n")

            gw._write_agent_instruction_files(
                tmp, "~sample-pier", "sample-pier", attested_to_bitcoin=True
            )

            with open(agents_path, encoding="utf-8") as f:
                self.assertEqual(f.read(), "custom instructions\n")

    def test_renders_attested_groundwire_id_status(self):
        content = gw._render_agent_instructions(
            "~sample-pier", "sample-pier", attested_to_bitcoin=True
        )

        self.assertIn(
            "You have a permanent, sybil-resistant Groundwire ID attested to on the Bitcoin mainnet.",
            content,
        )
        self.assertNotIn("may or may not", content)

    def test_mcp_server_name_for_long_pier(self):
        self.assertEqual(
            gw._mcp_server_name_for_pier("watwyd-bannyt-parmep-sivpes-motweb-daplyd"),
            "watwyd_daplyd",
        )

    def test_write_ship_mcp_configs_creates_agent_instruction_files(self):
        with tempfile.TemporaryDirectory() as tmp:
            pier_path = os.path.join(tmp, "sample-pier")
            os.mkdir(pier_path)

            gw._write_ship_mcp_configs(
                pier_path, 8080, "urbauth-test=abc", "~sample-pier", attested_to_bitcoin=True
            )

            agents_path = os.path.join(pier_path, "AGENTS.md")
            self.assertTrue(os.path.isfile(agents_path))
            with open(agents_path, encoding="utf-8") as f:
                self.assertIn(
                    "You have a permanent, sybil-resistant Groundwire ID attested to on the Bitcoin mainnet.",
                    f.read(),
                )
            claude_path = os.path.join(pier_path, "CLAUDE.md")
            self.assertTrue(os.path.islink(claude_path))
            self.assertEqual(os.readlink(claude_path), "AGENTS.md")
            self.assertTrue(os.path.isfile(os.path.join(pier_path, ".mcp.json")))


if __name__ == "__main__":
    unittest.main()
