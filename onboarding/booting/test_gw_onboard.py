"""Tests for gw-onboard.py pure functions."""

import importlib.util
import sys
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


if __name__ == "__main__":
    unittest.main()
