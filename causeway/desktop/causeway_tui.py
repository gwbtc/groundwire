"""Causeway Textual TUI — richer terminal UI over the same causeway.py logic.

Launch with `causeway-tui` once the package is installed, or
`python3 causeway_tui.py` during development.
"""

from __future__ import annotations

import asyncio
import json
import os
import sys
import time
from dataclasses import dataclass, field
from typing import Optional

from textual import work
from textual.app import App, ComposeResult
from textual.binding import Binding
from textual.containers import Horizontal, Vertical, VerticalScroll
from textual.reactive import reactive
from textual.screen import Screen
from textual.widgets import (
    Button,
    Checkbox,
    DataTable,
    Footer,
    Header,
    Input,
    Label,
    Log,
    Markdown,
    RadioButton,
    RadioSet,
    Static,
    TextArea,
)

import causeway as cw


# ---------------------------------------------------------------------------
# App-wide state carried between screens.
# ---------------------------------------------------------------------------


@dataclass
class FlowState:
    """Per-flow state threaded through screens."""

    network: str = "main"
    output_dir: str = "."
    miner_bin: str = cw.COMET_MINER_BIN
    mempool_base: str = cw.MEMPOOL_API_URL

    # Spawn context
    xpub_input: str = ""
    source: Optional[cw.KeySource] = None
    faucet_invite: Optional[str] = None
    mnemonic: Optional[str] = None  # generate-new-wallet
    utxos: list = field(default_factory=list)
    picked_utxo: Optional[dict] = None
    comet: Optional[str] = None
    feed: Optional[str] = None
    ring: Optional[str] = None
    pass_atom: Optional[int] = None
    # kelvin-9: the sat output commits a snapshot; a seed-derived blind hides the
    # spawn satpoint in dat and is opened later in the xtr / a public OP_RETURN.
    # The blind is ALWAYS derived from a BIP-39 phrase the user holds — the
    # wallet seed when we generated it, else `blind_mnemonic`, a dedicated
    # recovery phrase minted on the connect-wallet path (where we only have an
    # xpub).  A random blind would make the comet unrecoverable.
    snapshot: Optional[dict] = None
    blind: Optional[bytes] = None
    blind_seed: Optional[int] = None
    blind_mnemonic: Optional[str] = None  # connect-wallet: no seed to derive from
    blind_derivation: Optional[str] = None
    publish: bool = False
    # Routing (decisions-addendum section 2): a snapshot with neither a sponsor
    # nor a fief is a one-way identity — nothing can cold-contact the comet.
    # Causeway refuses to mint one unless `no_route` is ticked deliberately.
    sponsor_input: str = ""
    sponsor_atom: Optional[int] = None
    # The $fief noun (from cw.parse_fief_arg), not the raw string.
    fief_input: str = ""
    fief_noun: Optional[tuple] = None
    no_route: bool = False
    # A wrapper (boot.sh --mint) launched us and will finalize + boot after we
    # exit; the DoneScreen offers quit-and-continue instead of back-to-landing.
    handoff: bool = False
    psbt_b64_unsigned: Optional[str] = None
    psbt_b64_signed: Optional[str] = None
    commit_txid: Optional[str] = None
    proof_path: Optional[str] = None
    op_name: str = "spawn"

    # Manage context (kelvin-9: rekey is the only on-chain management op;
    # sponsorship + escape are off-chain).  A rekey spends the point's current
    # sat-carrying output key-path, identified by --prior-proof.
    point: Optional[str] = None
    new_pass_hex: Optional[str] = None
    prior_proof: Optional[dict] = None


# ---------------------------------------------------------------------------
# Base screen — shared theming and exit binding.
# ---------------------------------------------------------------------------


class BaseScreen(Screen):
    BINDINGS = [
        Binding("q", "quit_app", "Quit"),
        Binding("escape", "app.pop_screen", "Back", show=False),
    ]

    def action_quit_app(self) -> None:
        self.app.exit()


# ---------------------------------------------------------------------------
# Landing
# ---------------------------------------------------------------------------


class LandingScreen(BaseScreen):
    CSS = """
    Screen { align: center middle; }
    #hero { width: 70; border: round #ff6a00; padding: 2 4; }
    #title { content-align: center middle; color: #ff6a00; text-style: bold; }
    #tagline { content-align: center middle; color: #888; padding-bottom: 1; }
    .spacer { height: 1; }
    Button { width: 60; margin: 1 0; }
    """

    def compose(self) -> ComposeResult:
        yield Header()
        yield Vertical(
            Static("CAUSEWAY", id="title"),
            Static("Confidential comet spawning + management", id="tagline"),
            Static(" ", classes="spacer"),
            Button("Spawn a new comet", id="spawn", variant="primary"),
            Button("Manage an existing comet  (advanced)", id="manage"),
            Button("Inspect / verify a proof.json", id="proof"),
            Static(" ", classes="spacer"),
            Static("Q to quit — Esc to go back", id="hint"),
            id="hero",
        )
        yield Footer()

    def on_button_pressed(self, event: Button.Pressed) -> None:
        bid = event.button.id
        if bid == "spawn":
            self.app.push_screen(SpawnMethodScreen())
        elif bid == "manage":
            self.app.push_screen(ManagePickOpScreen())
        elif bid == "proof":
            self.app.push_screen(ProofOpenScreen())


# ---------------------------------------------------------------------------
# Spawn — method selection
# ---------------------------------------------------------------------------


class SpawnMethodScreen(BaseScreen):
    CSS = """
    Screen { align: center middle; }
    #panel { max-width: 100%; max-height: 100%; overflow-y: auto; width: 72; border: round #ff6a00; padding: 2 4; }
    #title { content-align: center middle; color: #ff6a00; text-style: bold; padding-bottom: 1; }
    .hint { color: #888; }
    Button { width: 62; margin: 1 0; }
    """

    def compose(self) -> ComposeResult:
        yield Header()
        yield Vertical(
            Static("SPAWN", id="title"),
            Static(
                "Two ways to spawn. Either produces a single confidential commit tx "
                "that any Bitcoin wallet can sign."
            ),
            Static(" "),
            Static("Sponsor (@p or mnemonym) — peers route to your comet through it:", classes="hint"),
            Input(placeholder="~sampel-palnet", id="sponsor",
                  value=self.app.state.sponsor_input),  # type: ignore[attr-defined]
            Static(" "),
            Static("Fief (IP:PORT) — a static endpoint peers can reach you at directly. "
                   "Leave blank unless you know you need one; if set, the ship must "
                   "actually bind this port. A comet that others will name as their "
                   "SPONSOR needs one.", classes="hint"),
            Input(placeholder="203.0.113.7:34343", id="fief",
                  value=self.app.state.fief_input),  # type: ignore[attr-defined]
            Checkbox("no-route: mint with NO sponsor and NO fief (outbound-only)", id="no-route"),
            Static(" "),
            Button("Connect Wallet  (paste xpub, sign PSBT externally)", id="connect", variant="primary"),
            Button("Generate New Wallet  (fresh BIP-39 seed in memory)", id="generate"),
            Static(" "),
            Static("Connect Wallet is recommended if you already hold BTC in Sparrow, Passport, Keystone, etc.", classes="hint"),
            Static("", id="err"),
            id="panel",
        )
        yield Footer()

    def on_button_pressed(self, event: Button.Pressed) -> None:
        state: FlowState = self.app.state  # type: ignore[attr-defined]
        if event.button.id not in ("connect", "generate"):
            return
        state.op_name = "spawn"
        # Routing gate, BEFORE any wallet/faucet/mining work: refuse to mint a
        # comet nothing can ever contact (see cw.assert_routable).
        err = self.query_one("#err", Static)
        state.sponsor_input = self.query_one("#sponsor", Input).value.strip()
        state.fief_input = self.query_one("#fief", Input).value.strip()
        state.no_route = self.query_one("#no-route", Checkbox).value
        try:
            state.sponsor_atom = cw.resolve_sponsor(state.sponsor_input or None)
            state.fief_noun = cw.parse_fief_arg(state.fief_input or None)
            cw.assert_routable({"sponsor": state.sponsor_atom,
                                "fief": state.fief_noun},
                               state.no_route)
        except Exception as e:  # noqa: BLE001
            err.update(str(e))
            return
        err.update("")
        # FlowState survives a trip back to the landing screen, so clear the
        # key material a previous spawn left behind. Otherwise a stale
        # `mnemonic` would silently back the next comet's blind (and skip the
        # blind-phrase prompt) even though that wallet is not the one funding it.
        state.mnemonic = None
        state.blind_mnemonic = None
        state.blind = state.blind_seed = state.blind_derivation = None
        state.source = None
        state.picked_utxo = None
        if event.button.id == "connect":
            self.app.push_screen(XpubInputScreen(mode="connect"))
        else:
            self.app.push_screen(GenerateSeedScreen())


# ---------------------------------------------------------------------------
# Xpub / descriptor input — shared by spawn-connect and manage-*
# ---------------------------------------------------------------------------


class XpubInputScreen(BaseScreen):
    CSS = """
    Screen { align: center middle; }
    #panel { max-width: 100%; max-height: 100%; overflow-y: auto; width: 88; border: round #ff6a00; padding: 2 4; }
    #title { content-align: center middle; color: #ff6a00; text-style: bold; padding-bottom: 1; }
    .label { color: #bbb; padding-top: 1; }
    .hint { color: #888; padding-top: 1; }
    Input { width: 100%; }
    #invite { width: 40; }
    #actions { padding-top: 1; }
    Button { margin-right: 2; }
    #err { color: red; padding-top: 1; }
    """

    def __init__(self, *, mode: str = "connect") -> None:
        super().__init__()
        self.mode = mode

    def compose(self) -> ComposeResult:
        yield Header()
        yield Vertical(
            Static("KEY SOURCE", id="title"),
            Static(
                "Paste an xpub or a BIP-380 output descriptor such as "
                "[i]tr([fpr/86h/0h/0h]xpub...)[/i]. The descriptor form is strongly preferred — "
                "it includes the master fingerprint your signer needs.",
                markup=True,
            ),
            Static("Xpub / descriptor:", classes="label"),
            Input(placeholder="xpub... or tr([fpr/86h/0h/0h]xpub...)", id="xpub"),
            Static("Faucet invite (optional, sends 1000 sats to your first address):", classes="label"),
            Input(placeholder="blank to skip", id="invite"),
            Horizontal(
                Button("Continue →", id="continue", variant="primary"),
                Button("Back", id="back"),
                id="actions",
            ),
            Static("", id="err"),
            id="panel",
        )
        yield Footer()

    def on_button_pressed(self, event: Button.Pressed) -> None:
        if event.button.id == "back":
            self.app.pop_screen()
            return
        if event.button.id != "continue":
            return
        state: FlowState = self.app.state  # type: ignore[attr-defined]
        xpub_val = self.query_one("#xpub", Input).value.strip()
        invite_val = self.query_one("#invite", Input).value.strip() or None
        if not xpub_val:
            self.query_one("#err", Static).update("xpub/descriptor is required")
            return
        try:
            state.source = cw.parse_key_source(xpub_val, network=state.network)
        except Exception as e:
            self.query_one("#err", Static).update(f"could not parse: {e}")
            return
        state.xpub_input = xpub_val
        state.faucet_invite = invite_val
        self.app.push_screen(UtxoPickerScreen())


# ---------------------------------------------------------------------------
# Generate new wallet — seed phrase + saved-confirmation
# ---------------------------------------------------------------------------


class GenerateSeedScreen(BaseScreen):
    CSS = """
    Screen { align: center middle; }
    #panel { max-width: 100%; max-height: 100%; overflow-y: auto; width: 90; border: round #ff6a00; padding: 2 4; }
    #title { content-align: center middle; color: #ff6a00; text-style: bold; padding-bottom: 1; }
    #warn { color: #ff6a00; text-style: bold; padding: 1 0; }
    #seed-box { border: heavy #ff6a00; padding: 1 2; margin: 1 0; height: auto; }
    #seed-box > Static { width: 1fr; height: auto; }
    Button { margin-right: 2; }
    #actions { padding-top: 1; }
    #err { color: red; }
    """

    def compose(self) -> ComposeResult:
        state: FlowState = self.app.state  # type: ignore[attr-defined]
        if state.mnemonic is None:
            state.mnemonic = cw.generate_new_mnemonic(strength_bits=128)
        yield Header()
        words = state.mnemonic.split()
        # Columnar layout: col 0 = words 1..4, col 1 = 5..8, col 2 = 9..12.
        # (Previously interleaved mod-3, and without per-column widths
        # only the first column rendered — the others got clipped.)
        rows = (len(words) + 2) // 3
        cols: list[list[str]] = [[] for _ in range(3)]
        for i, w in enumerate(words):
            cols[i // rows].append(f"{i+1:>2}. {w}")
        col_static = [Static("\n".join(c)) for c in cols]
        yield Vertical(
            Static("GENERATED SEED PHRASE", id="title"),
            Static("SAVE THIS NOW — losing it is fatal.", id="warn"),
            Horizontal(*col_static, id="seed-box"),
            Static(
                "Type the full phrase back below to confirm you wrote it down:",
            ),
            Input(placeholder="word1 word2 word3 ...", id="confirm"),
            Static("Faucet invite (optional — sends 1000 sats to your first receive address):"),
            Input(placeholder="blank to skip / fund manually", id="invite"),
            Horizontal(
                Button("I saved it — continue", id="continue", variant="primary"),
                Button("Back", id="back"),
                id="actions",
            ),
            Static("", id="err"),
            id="panel",
        )
        yield Footer()

    def on_button_pressed(self, event: Button.Pressed) -> None:
        if event.button.id == "back":
            self.app.pop_screen()
            return
        if event.button.id != "continue":
            return
        state: FlowState = self.app.state  # type: ignore[attr-defined]
        typed = self.query_one("#confirm", Input).value.strip().lower()
        if typed != (state.mnemonic or "").lower():
            self.query_one("#err", Static).update("That doesn't match. Try again.")
            return
        state.faucet_invite = (self.query_one("#invite", Input).value.strip() or None)
        # Build source from seed
        root = cw.mnemonic_to_hdkey(state.mnemonic or "", network=state.network)
        account_path = [cw._hardened(86), cw._hardened(0 if state.network == "main" else 1), cw._hardened(0)]
        acc_xpub = root.derive(cw._path_to_str(account_path))
        fpr = cw.hdkey_fingerprint(root)
        state.source = cw.KeySource(xpub=acc_xpub, master_fingerprint=fpr, account_path=account_path, network=state.network)
        self.app.push_screen(WaitForFundingScreen())


class WaitForFundingScreen(BaseScreen):
    CSS = """
    Screen { align: center middle; }
    #panel { max-width: 100%; max-height: 100%; overflow-y: auto; width: 88; border: round #ff6a00; padding: 2 4; }
    #title { content-align: center middle; color: #ff6a00; text-style: bold; padding-bottom: 1; }
    #addr { text-style: bold; color: #ff6a00; padding: 1 0; }
    #qr { padding: 0 0 1 0; }
    .hint { color: #888; }
    #log { height: 5; border: round #888; padding: 0 1; }
    Button { margin-right: 2; }
    """

    def compose(self) -> ComposeResult:
        yield Header()
        yield Vertical(
            Static("WAITING FOR FUNDING", id="title"),
            Static("Send at least 1000 sats to this address:"),
            Static("(loading...)", id="addr"),
            Static("", id="qr"),
            Static("Scan the QR from a phone wallet, or press Copy. "
                   "(Select-to-copy needs \u2325/Shift held while dragging -- "
                   "the TUI owns the mouse.)", classes="hint"),
            Log(id="log"),
            Horizontal(
                Button("Copy address", id="copy-addr", variant="primary"),
                Button("Back", id="back"),
                Button("Scan now (skip polling)", id="skip"),
            ),
            id="panel",
        )
        yield Footer()

    _superseded: bool = False

    def on_mount(self) -> None:
        self._superseded = False
        self._poll = self.poll_worker()

    def on_screen_resume(self) -> None:
        """Restart polling when a covering screen pops back to us.

        Textual fires on_mount ONCE per screen instance; popping back from
        "Scan now" (UtxoPickerScreen) re-exposes this screen with
        _superseded still True and the worker cancelled -- a dead screen
        that looks exactly like a live one.  Found by a user clicking Scan
        now, then Back, then waiting on a poll that would never come.

        Restart ONLY while funding is still the open question: after the
        success path (picked_utxo set, MiningScreen pushed) a resume must
        not spin up a second poll and push a second MiningScreen.
        """
        state: FlowState = self.app.state  # type: ignore[attr-defined]
        if state.picked_utxo is not None:
            return
        self._superseded = False
        self._poll = self.poll_worker()   # exclusive=True: at most one runs

    @work(exclusive=True, thread=True)
    def poll_worker(self) -> None:
        state: FlowState = self.app.state  # type: ignore[attr-defined]
        if state.source is None:
            return
        first_addr, _spk, _xonly, _p = state.source.derive_address(0, 0)
        self.app.call_from_thread(self.query_one("#addr", Static).update, first_addr)
        self._addr = first_addr
        try:
            self.app.call_from_thread(
                self.query_one("#qr", Static).update, cw.qr_ascii(first_addr))
        except Exception:  # noqa: BLE001 -- a QR is a convenience, never a blocker
            pass
        log = self.query_one("#log", Log)

        if state.faucet_invite:
            self.app.call_from_thread(log.write_line, f"Requesting 1000 sats from faucet (invite={state.faucet_invite})...")
            try:
                fxid = cw.request_faucet(first_addr, invite=state.faucet_invite)
                if fxid:
                    self.app.call_from_thread(log.write_line, f"Faucet sent: {fxid}")
                else:
                    self.app.call_from_thread(log.write_line, "Faucet failed (continuing anyway).")
            except Exception as e:
                self.app.call_from_thread(log.write_line, f"Faucet error: {e}")

        # Poll mempool until we see a confirmed UTXO on any xpub address.
        while not self._superseded:
            try:
                utxos = cw.scan_addresses(state.source, n_receive=5, n_change=2, mempool_base=state.mempool_base)
                confirmed = [u for u in utxos if u["confirmed"]]
                if self._superseded:  # user chose the manual path while we scanned
                    return
                if confirmed:
                    state.utxos = utxos
                    state.picked_utxo = max(confirmed, key=lambda u: u["value"])
                    self.app.call_from_thread(log.write_line,
                        f"Confirmed UTXO: {state.picked_utxo['value']} sat at "
                        f"{state.picked_utxo['txid'][:16]}...:{state.picked_utxo['vout']}")
                    self.app.call_from_thread(self.app.push_screen, MiningScreen())
                    return
                self.app.call_from_thread(log.write_line,
                    f"No confirmed UTXO yet ({len(utxos)} unconfirmed) — sleeping {cw.POLL_INTERVAL}s")
            except Exception as e:
                self.app.call_from_thread(log.write_line, f"Poll error: {e}")
            time.sleep(cw.POLL_INTERVAL)

    def _stop_polling(self) -> None:
        # Prevent the background poll from overwriting picked_utxo or pushing a
        # second MiningScreen once the user has taken the manual path.
        self._superseded = True
        worker = getattr(self, "_poll", None)
        if worker is not None:
            worker.cancel()

    _addr: str = ""

    def on_button_pressed(self, event: Button.Pressed) -> None:
        if event.button.id == "copy-addr":
            if not self._addr:
                self.notify("address not derived yet", severity="warning")
            elif cw.copy_to_clipboard(self._addr):
                self.notify("address copied")
            else:
                # No native clipboard tool (likely SSH): OSC 52 through the
                # terminal, which most modern emulators honour.
                self.app.copy_to_clipboard(self._addr)
                self.notify("sent to clipboard via OSC 52 (terminal-dependent)")
            return
        if event.button.id == "back":
            self._stop_polling()
            self.app.pop_screen()
        elif event.button.id == "skip":
            self._stop_polling()
            self.app.push_screen(UtxoPickerScreen())


# ---------------------------------------------------------------------------
# UTXO picker — DataTable
# ---------------------------------------------------------------------------


class UtxoPickerScreen(BaseScreen):
    CSS = """
    Screen { align: center middle; }
    #panel { max-width: 100%; max-height: 100%; overflow-y: auto; width: 100; height: 30; border: round #ff6a00; padding: 1 2; }
    #title { content-align: center middle; color: #ff6a00; text-style: bold; }
    DataTable { height: 1fr; }
    #actions { padding-top: 1; }
    Button { margin-right: 2; }
    #status { color: #888; padding-top: 1; }
    """

    BINDINGS = BaseScreen.BINDINGS + [Binding("enter", "pick_selected", "Pick", show=True)]

    def compose(self) -> ComposeResult:
        yield Header()
        yield Vertical(
            Static("SELECT FUNDING UTXO", id="title"),
            DataTable(id="table"),
            Static("Loading UTXOs...", id="status"),
            Horizontal(
                Button("Pick highlighted row", id="pick", variant="primary"),
                Button("Refresh", id="refresh"),
                Button("Back", id="back"),
                id="actions",
            ),
            id="panel",
        )
        yield Footer()

    def on_mount(self) -> None:
        table = self.query_one("#table", DataTable)
        table.cursor_type = "row"
        table.add_columns("#", "Value (sat)", "Address", "txid:vout", "path", "conf")
        self.scan()

    @work(exclusive=True, thread=True)
    def scan(self) -> None:
        state: FlowState = self.app.state  # type: ignore[attr-defined]
        if state.source is None:
            return
        status = self.query_one("#status", Static)
        self.app.call_from_thread(status.update, "Scanning addresses...")
        try:
            state.utxos = cw.scan_addresses(state.source, mempool_base=state.mempool_base)
        except Exception as e:
            self.app.call_from_thread(status.update, f"scan error: {e}")
            return
        table = self.query_one("#table", DataTable)
        self.app.call_from_thread(table.clear)
        for i, u in enumerate(state.utxos, 1):
            conf = "✓" if u["confirmed"] else "…"
            self.app.call_from_thread(table.add_row,
                str(i),
                f"{u['value']:,}",
                u["address"][:28] + "...",
                f"{u['txid'][:10]}...:{u['vout']}",
                f".../{u['change']}/{u['index']}",
                conf,
            )
        self.app.call_from_thread(status.update,
            f"{len(state.utxos)} UTXOs found — up+down to navigate, Enter or 'Pick' to select")

    def action_pick_selected(self) -> None:
        self.pick_current()

    def pick_current(self) -> None:
        state: FlowState = self.app.state  # type: ignore[attr-defined]
        table = self.query_one("#table", DataTable)
        if table.row_count == 0:
            return
        row = table.cursor_row
        if row is None or row >= len(state.utxos):
            return
        state.picked_utxo = state.utxos[row]
        if state.picked_utxo["value"] < 630:
            self.query_one("#status", Static).update(
                f"UTXO too small: {state.picked_utxo['value']} < 630 sats minimum"
            )
            return
        # For spawn we need to mine; for manage we skip mining.  The blind
        # phrase ceremony is retired: a connect-wallet spawn's blind is fresh
        # entropy recorded in proof.json (the identity bundle: proof + feed).
        # BlindPhraseScreen survives only for a deliberate re-spawn from a
        # phrase minted before the retirement (state.blind_mnemonic pre-set).
        if state.op_name != "spawn":
            self.app.push_screen(PsbtBuildScreen())
        else:
            self.app.push_screen(MiningScreen())

    def on_button_pressed(self, event: Button.Pressed) -> None:
        if event.button.id == "pick":
            self.pick_current()
        elif event.button.id == "refresh":
            self.scan()
        elif event.button.id == "back":
            self.app.pop_screen()


# ---------------------------------------------------------------------------
# Blind recovery phrase — connect-wallet only
# ---------------------------------------------------------------------------


class BlindPhraseScreen(BaseScreen):
    """Collect the BIP-39 phrase a connect-wallet spawn derives its blind from.

    The connect flow only ever holds an account xpub (watch-only wallet or
    hardware signer), so there is no wallet seed to derive the blind from.
    Rather than mint an unrecoverable random blind, we generate a dedicated
    phrase and make the user write it back, or accept one they already hold."""

    CSS = """
    Screen { align: center middle; }
    #panel { max-width: 100%; max-height: 100%; overflow-y: auto; width: 92; border: round #ff6a00; padding: 2 4; }
    #title { content-align: center middle; color: #ff6a00; text-style: bold; padding-bottom: 1; }
    #warn { color: #ff6a00; text-style: bold; padding: 1 0; }
    #seed-box { border: heavy #ff6a00; padding: 1 2; margin: 1 0; height: auto; }
    #seed-box > Static { width: 1fr; height: auto; }
    Button { margin-right: 2; }
    #actions { padding-top: 1; }
    #err { color: red; }
    #note { color: #888; }
    """

    def compose(self) -> ComposeResult:
        state: FlowState = self.app.state  # type: ignore[attr-defined]
        if state.blind_mnemonic is None:
            state.blind_mnemonic = cw.generate_new_mnemonic(strength_bits=128)
        yield Header()
        words = state.blind_mnemonic.split()
        rows = (len(words) + 2) // 3
        cols: list[list[str]] = [[] for _ in range(3)]
        for i, w in enumerate(words):
            cols[i // rows].append(f"{i+1:>2}. {w}")
        col_static = [Static("\n".join(c)) for c in cols]
        yield Vertical(
            Static("BLIND RECOVERY PHRASE", id="title"),
            Static(
                "Your wallet is external, so Causeway cannot derive this comet's\n"
                "blind from your wallet seed. This separate phrase is REQUIRED to\n"
                "ever prove or recover the identity — without it nobody, not even\n"
                "the holder of the wallet seed, can open the dat commitment your\n"
                "@p is built from. As important as your wallet seed. Write it down.",
                id="warn",
            ),
            Horizontal(*col_static, id="seed-box"),
            Static("Type the full phrase back below to confirm you wrote it down:"),
            Input(placeholder="word1 word2 word3 ...", id="confirm"),
            Static(
                "Or paste an existing blind recovery phrase (re-spawn / recovery):",
                id="note",
            ),
            Input(placeholder="blank to use the generated phrase above", id="existing"),
            Horizontal(
                Button("I saved it — continue", id="continue", variant="primary"),
                Button("Back", id="back"),
                id="actions",
            ),
            Static("", id="err"),
            id="panel",
        )
        yield Footer()

    def on_button_pressed(self, event: Button.Pressed) -> None:
        if event.button.id == "back":
            self.app.pop_screen()
            return
        if event.button.id != "continue":
            return
        state: FlowState = self.app.state  # type: ignore[attr-defined]
        err = self.query_one("#err", Static)
        existing = self.query_one("#existing", Input).value.strip()
        if existing:
            try:
                state.blind_mnemonic = cw.normalize_blind_mnemonic(existing)
            except ValueError as e:
                err.update(f"supplied phrase rejected: {e}")
                return
        else:
            typed = self.query_one("#confirm", Input).value.strip().lower()
            if typed != (state.blind_mnemonic or "").lower():
                err.update("That doesn't match. Try again.")
                return
        self.app.push_screen(MiningScreen())


# ---------------------------------------------------------------------------
# Mining screen — subprocess progress
# ---------------------------------------------------------------------------


class MiningScreen(BaseScreen):
    CSS = """
    Screen { align: center middle; }
    #panel { max-width: 100%; max-height: 100%; overflow-y: auto; width: 80; border: round #ff6a00; padding: 2 4; }
    #title { content-align: center middle; color: #ff6a00; text-style: bold; padding-bottom: 1; }
    #status { text-style: bold; padding: 1 0; }
    Log { height: 15; border: round #888; }
    """

    def compose(self) -> ComposeResult:
        yield Header()
        yield Vertical(
            Static("MINING COMET", id="title"),
            Static("(may take a few seconds — ~daplyd constraint)", id="status"),
            Log(id="log"),
            id="panel",
        )
        yield Footer()

    def on_mount(self) -> None:
        self.run_mine()

    @work(exclusive=True, thread=True)
    def run_mine(self) -> None:
        state: FlowState = self.app.state  # type: ignore[attr-defined]
        if state.picked_utxo is None:
            return
        log = self.query_one("#log", Log)
        u = state.picked_utxo
        self.app.call_from_thread(log.write_line, f"Spawn satpoint: txid={u['txid'][:16]}... vout={u['vout']} off=0")
        self.app.call_from_thread(log.write_line, f"Miner: {state.miner_bin}")
        if not os.path.exists(state.miner_bin):
            self.app.call_from_thread(log.write_line, f"ERROR: miner binary not found at {state.miner_bin}")
            self.app.call_from_thread(self.query_one("#status", Static).update, "miner not found — configure --miner")
            return
        # Blind derivation, in preference order: the wallet seed when we
        # generated it (free second recovery route); a supplied legacy blind
        # phrase; else fresh entropy recorded only in proof.json.  The file is
        # the custody object either way -- the ring is nondeterministically
        # mined and lives only in the feed, so file custody was never optional.
        phrase = state.mnemonic or state.blind_mnemonic
        if phrase:
            state.blind_derivation = (
                cw.BLIND_DERIV_WALLET_SEED if state.mnemonic else cw.BLIND_DERIV_BLIND_MNEMONIC
            )
            state.blind_seed, state.blind = cw.blind_from_mnemonic(phrase, u["txid"], u["vout"])
        else:
            state.blind_seed = int.from_bytes(os.urandom(32), "big")
            state.blind = cw.make_blind(state.blind_seed)
            state.blind_derivation = cw.BLIND_DERIV_PROOF_FILE
            self.app.call_from_thread(log.write_line,
                "Blind: fresh entropy, recorded in the proof file. Your identity")
            self.app.call_from_thread(log.write_line,
                "bundle is proof.json + the feed — back both up.")
        self.app.call_from_thread(log.write_line, f"Blind derived from {state.blind_derivation}")
        try:
            result = cw.mine_comet_from_utxo(u["txid"], u["vout"], 0, state.blind_seed, state.miner_bin)
        except Exception as e:
            self.app.call_from_thread(log.write_line, f"Mining error: {e}")
            return
        state.comet = result["comet"]
        state.feed = result["feed"]
        state.ring = result.get("ring", "")
        state.pass_atom = cw.derive_pass_from_ring(state.ring)
        self.app.call_from_thread(log.write_line, f"Mined {cw.patp_to_mnemonym(state.comet)}")
        self.app.call_from_thread(log.write_line, f"  @p {state.comet}")
        self.app.call_from_thread(log.write_line, f"Pass atom: 0x{state.pass_atom:x}")

        # The initial snapshot the sat output commits to (life 1, rift 0).
        state.snapshot = cw._initial_snapshot(state.pass_atom, state.sponsor_atom,
                                              state.fief_noun)
        cw.assert_routable(state.snapshot, state.no_route)
        self.app.call_from_thread(log.write_line, f"Initial snapshot: life=1 rift=0 key=0x{state.snapshot['key']:x}")
        self.app.call_from_thread(self.app.push_screen, PsbtBuildScreen())


# ---------------------------------------------------------------------------
# PSBT build + sign
# ---------------------------------------------------------------------------


class PsbtBuildScreen(BaseScreen):
    CSS = """
    Screen { align: center middle; }
    #panel { max-width: 100%; max-height: 100%; overflow-y: auto; width: 100; height: 40; border: round #ff6a00; padding: 1 2; }
    #title { content-align: center middle; color: #ff6a00; text-style: bold; }
    #b64 { height: 10; border: round #888; }
    #signed-in { height: 8; border: round #888; }
    Button { margin-right: 2; }
    #actions { padding-top: 1; }
    #status { color: #888; padding-top: 1; }
    """

    def compose(self) -> ComposeResult:
        yield Header()
        yield Vertical(
            Static("BUILD & SIGN COMMIT PSBT", id="title"),
            Static("Unsigned PSBT (base64) — load into your wallet, sign, paste the signed version below:"),
            TextArea(id="b64", read_only=True),
            Static("Signed PSBT:"),
            TextArea(id="signed-in"),
            Static("Status: —", id="status"),
            Horizontal(
                Button("Sign in-process  (Generate-Wallet only)", id="self-sign"),
                Button("Broadcast signed →", id="broadcast", variant="primary"),
                Button("Back", id="back"),
                id="actions",
            ),
            id="panel",
        )
        yield Footer()

    def on_mount(self) -> None:
        state: FlowState = self.app.state  # type: ignore[attr-defined]
        self.build_psbt()
        # If we have a mnemonic, we can self-sign.
        if state.mnemonic is None:
            self.query_one("#self-sign", Button).disabled = True

    def build_psbt(self) -> None:
        state: FlowState = self.app.state  # type: ignore[attr-defined]
        try:
            if state.op_name == "spawn":
                if state.picked_utxo is None or state.source is None or state.snapshot is None:
                    self.query_one("#status", Static).update("missing prerequisites — go back")
                    return
                u = state.picked_utxo
                pub_pass = pub_opening = None
                if state.publish:
                    pub_pass = state.pass_atom
                    pub_opening = cw._spawn_publication_opening(
                        u["xonly"], state.snapshot, u, state.blind or b""
                    )
                p, proof = cw.build_spawn_psbt(
                    utxo_txid=u["txid"],
                    utxo_vout=u["vout"],
                    utxo_value=u["value"],
                    utxo_script_pubkey=u["scriptpubkey"],
                    funding_internal_xonly=u["xonly"],
                    funding_path=u["path"],
                    funding_fingerprint=state.source.master_fingerprint,
                    snapshot=state.snapshot,
                    publication_pass_atom=pub_pass,
                    publication_opening=pub_opening,
                    fee_rate=2,
                    network=state.network,
                )
                cw._finish_spawn_proof(
                    proof, comet=state.comet or "", pass_atom=state.pass_atom or 0,
                    blind=state.blind or b"", blind_seed=state.blind_seed or 0, utxo=u,
                    blind_derivation=state.blind_derivation or cw.BLIND_DERIV_WALLET_SEED,
                )
            else:
                # Rekey: spend the point's current sat output key-path, identified
                # by --prior-proof (build_rekey_psbt).
                if state.prior_proof is None or state.snapshot is None:
                    self.query_one("#status", Static).update("no prior proof / snapshot — go back")
                    return
                p, proof = cw.build_rekey_psbt(
                    prior_proof=state.prior_proof,
                    new_snapshot=state.snapshot,
                    fee_rate=2,
                    network=state.network,
                )
                proof["op"] = state.op_name
                proof["patp"] = state.point or ""
            state.psbt_b64_unsigned = p.to_base64()
            # stash proof temporarily in state via a closure
            self._pending_proof = proof  # type: ignore[attr-defined]
            self.query_one("#b64", TextArea).text = state.psbt_b64_unsigned
            self.query_one("#status", Static).update("PSBT built — sign externally, paste signed below")
        except Exception as e:
            self.query_one("#status", Static).update(f"build failed: {e}")

    def on_button_pressed(self, event: Button.Pressed) -> None:
        state: FlowState = self.app.state  # type: ignore[attr-defined]
        if event.button.id == "back":
            self.app.pop_screen()
            return
        if event.button.id == "self-sign":
            if state.mnemonic is None:
                return
            try:
                from embit import bip32 as _bip32, psbt as _psbt
                root = cw.mnemonic_to_hdkey(state.mnemonic, network=state.network)
                p = _psbt.PSBT.from_base64(state.psbt_b64_unsigned or "")
                p.sign_with(root)
                state.psbt_b64_signed = p.to_base64()
                self.query_one("#signed-in", TextArea).text = state.psbt_b64_signed
                self.query_one("#status", Static).update("signed in-process — click Broadcast")
            except Exception as e:
                self.query_one("#status", Static).update(f"self-sign failed: {e}")
            return
        if event.button.id == "broadcast":
            b64 = self.query_one("#signed-in", TextArea).text.strip()
            if not b64:
                self.query_one("#status", Static).update("paste a signed PSBT first")
                return
            state.psbt_b64_signed = b64
            self.broadcast_worker()

    @work(exclusive=True, thread=True)
    def broadcast_worker(self) -> None:
        state: FlowState = self.app.state  # type: ignore[attr-defined]
        status = self.query_one("#status", Static)
        try:
            # The signed tx's txid is deterministic (segwit), so we know the
            # commit_txid before broadcasting. Persist the proof FIRST — it is
            # the only durable record of the snapshot (the blind itself is
            # re-derivable from the user's phrase + the spawn outpoint, but the
            # snapshot and pass are not); if we broadcast first and then crash
            # before writing it, an already-spent sat is left with no proof.
            # ... and before any of that, check the signer handed back the
            # transaction we built.  The paste box accepts any PSBT; a
            # segwit txid does not change under signing, so one comparison
            # settles it.  Without this the proof written below can describe
            # a transaction that was never broadcast.
            cw.assert_signed_is_what_we_built(
                state.psbt_b64_unsigned or "", state.psbt_b64_signed or ""
            )
            commit_txid, tx_hex = cw._extract_tx_from_psbt(state.psbt_b64_signed or "")
            proof = getattr(self, "_pending_proof", None) or {}
            proof["commit_txid"] = commit_txid
            os.makedirs(state.output_dir, exist_ok=True)
            pier = (state.comet or state.point or "unknown").lstrip("~")
            suffix = f"{pier}-{state.op_name}-{commit_txid[:10]}.proof.json"
            proof_path = os.path.join(state.output_dir, suffix)
            cw.write_proof_json(proof, proof_path)
            state.proof_path = proof_path

            self.app.call_from_thread(status.update, f"proof saved — broadcasting {commit_txid}")
            broadcast_id = cw._broadcast_tx(tx_hex, mempool_base=state.mempool_base)
            state.commit_txid = broadcast_id
            self.app.call_from_thread(self.app.push_screen, DoneScreen())
        except Exception as e:
            self.app.call_from_thread(status.update, f"broadcast failed: {e}")


# ---------------------------------------------------------------------------
# Done screen — boot command or manage summary
# ---------------------------------------------------------------------------


class DoneScreen(BaseScreen):
    CSS = """
    Screen { align: center middle; }
    #panel { max-width: 100%; max-height: 100%; overflow-y: auto; width: 96; border: round green; padding: 2 4; }
    #title { content-align: center middle; color: green; text-style: bold; padding-bottom: 1; }
    #boot { padding: 1 0; background: #111; color: #eee; }
    .label { color: #bbb; padding-top: 1; }
    """

    def compose(self) -> ComposeResult:
        state: FlowState = self.app.state  # type: ignore[attr-defined]
        yield Header()
        if state.op_name == "spawn":
            comet = state.comet or "<unknown>"
            feed = state.feed or ""
            proof_path = state.proof_path or ""
            # The raw miner feed is written to a file, 0600, next to the proof
            # -- never rendered on screen and never baked into a command.  This
            # screen used to show a copy-pasteable boot line carrying the raw
            # feed, with the finalize note underneath: following it boots a
            # comet with an EMPTY custody log that no peer can ever verify,
            # after the sats are spent.  The CLI had the same flaw; both ends
            # now hand you finalize, which is where a bootable feed first
            # exists.
            feed_path = ""
            if feed and proof_path:
                feed_path = os.path.splitext(proof_path)[0] + ".feed"
                try:
                    cw.write_feed_file(feed_path, feed)
                except OSError:
                    feed_path = ""
            if feed_path:
                cmd = (
                    f"~/.groundwire/causeway finalize {proof_path} \\\n"
                    f"  --feed-file {feed_path} --out-feed {feed_path}.baked"
                )
            else:
                cmd = f"~/.groundwire/causeway finalize {proof_path} --feed <miner feed>"
            comet_mnemo = cw.patp_to_mnemonym(comet) if comet != "<unknown>" else comet
            if state.blind_derivation == cw.BLIND_DERIV_BLIND_MNEMONIC:
                recovery = (
                    "Recovery: this comet's blind is derived from your BLIND RECOVERY\n"
                    "PHRASE + the spawn outpoint. Keep the phrase — it is the only way\n"
                    "to reopen the dat commitment if the proof file is lost."
                )
            elif state.blind_derivation == cw.BLIND_DERIV_WALLET_SEED:
                recovery = (
                    "Recovery: this comet's blind is derived from your wallet seed\n"
                    "phrase + the spawn outpoint, so the seed alone can reopen the dat\n"
                    "commitment even if the proof file is lost."
                )
            else:
                recovery = (
                    "Custody: your identity bundle is the proof file + the feed file.\n"
                    "Back BOTH up like a wallet. The blind lives only in the proof;\n"
                    "the ship's key lives only in the feed. No phrase regenerates them."
                )
            self._cmd = cmd
            yield Vertical(
                Static("SPAWN COMPLETE", id="title"),
                Static(f"Comet: {comet_mnemo}", classes="label"),
                Static(f"@p:    {comet}", classes="label"),
                Static(f"Commit txid: {state.commit_txid}", classes="label"),
                Static(f"Proof: {proof_path}", classes="label"),
                Static(recovery, classes="label"),
                Static(
                    "DO NOT BOOT YET — the feed still carries an empty custody log.\n"
                    "Once the spawn tx confirms, run this; it bakes the log and\n"
                    "prints the boot command:",
                    classes="label",
                ),
                Static(cmd, id="boot"),
                Static(
                    "⚠ Runtime does not yet consume --proof. The proof file will be "
                    "saved to ~/.groundwire/ but won't propagate via Ames until runtime support lands.",
                    classes="label",
                ),

                Button("Copy the finalize command", id="copy-cmd"),
                (Button("Quit — the installer continues (finalize + boot)",
                        id="handoff-quit", variant="success")
                 if state.handoff else
                 Button("Done  →  back to landing", id="home", variant="primary")),
                id="panel",
            )
        else:
            point_mnemo = cw.patp_to_mnemonym(state.point) if state.point else "?"
            yield Vertical(
                Static(f"{state.op_name.upper()} BROADCAST", id="title"),
                Static(f"Point: {point_mnemo}", classes="label"),
                Static(f"@p:    {state.point}", classes="label"),
                Static(f"Commit txid: {state.commit_txid}", classes="label"),
                Static(f"Proof: {state.proof_path}", classes="label"),
                Static(
                    "After it confirms, hand the new xtr entry + opening to your\n"
                    "ship's %gw-btc agent (the %anew poke) so peers can re-verify\n"
                    f"you. Keep {state.proof_path} as --prior-proof for the next op.",
                    classes="label",
                ),
                Button("Done  →  back to landing", id="home", variant="primary"),
                id="panel",
            )
        yield Footer()

    _cmd: str = ""

    def on_button_pressed(self, event: Button.Pressed) -> None:
        if event.button.id == "copy-cmd":
            if self._cmd and cw.copy_to_clipboard(self._cmd):
                self.notify("command copied")
            elif self._cmd:
                self.app.copy_to_clipboard(self._cmd)
                self.notify("sent via OSC 52 (terminal-dependent)")
            return
        if event.button.id == "handoff-quit":
            # boot.sh --mint is waiting on our exit status; it finds the proof
            # and the feed file on disk (both already written) and carries on
            # with finalize + boot.  Nothing to pass -- the disk is the seam.
            self.app.exit(0)
            return
        if event.button.id == "home":
            # Pop everything back to landing
            self.app.pop_screen()
            while len(self.app.screen_stack) > 1:
                self.app.pop_screen()


# ---------------------------------------------------------------------------
# Manage flow
# ---------------------------------------------------------------------------


class ManagePickOpScreen(BaseScreen):
    CSS = """
    Screen { align: center middle; }
    #panel { max-width: 100%; max-height: 100%; overflow-y: auto; width: 72; border: round #ff6a00; padding: 2 4; }
    #title { content-align: center middle; color: #ff6a00; text-style: bold; padding-bottom: 1; }
    RadioSet { padding: 1 0; }
    Button { margin-right: 2; }
    """

    def compose(self) -> ComposeResult:
        yield Header()
        yield Vertical(
            Static("MANAGE — pick an operation", id="title"),
            Static(
                "kelvin-9: rekey (messaging-key rotation / breach) is the only\n"
                "on-chain management op. Sponsorship and escape are off-chain.",
                classes="label",
            ),
            RadioSet(
                RadioButton("rekey — rotate messaging key", id="rekey", value=True),
                id="ops",
            ),
            Horizontal(
                Button("Continue →", id="continue", variant="primary"),
                Button("Back", id="back"),
            ),
            id="panel",
        )
        yield Footer()

    def on_button_pressed(self, event: Button.Pressed) -> None:
        if event.button.id == "back":
            self.app.pop_screen()
            return
        if event.button.id != "continue":
            return
        rs = self.query_one(RadioSet)
        op_id = rs.pressed_button.id if rs.pressed_button else "rekey"
        state: FlowState = self.app.state  # type: ignore[attr-defined]
        state.op_name = op_id or "rekey"
        self.app.push_screen(ManageFormScreen(op=state.op_name))


class ManageFormScreen(BaseScreen):
    CSS = """
    Screen { align: center middle; }
    #panel { max-width: 100%; max-height: 100%; overflow-y: auto; width: 90; border: round #ff6a00; padding: 2 4; }
    #title { content-align: center middle; color: #ff6a00; text-style: bold; padding-bottom: 1; }
    Input { width: 100%; }
    .label { color: #bbb; padding-top: 1; }
    #err { color: red; padding-top: 1; }
    Button { margin-right: 2; }
    """

    def __init__(self, *, op: str) -> None:
        super().__init__()
        self.op = op

    def compose(self) -> ComposeResult:
        yield Header()
        children: list = [
            Static("REKEY — rotate messaging key", id="title"),
            Static("Target comet (mnemonym or @p):", classes="label"),
            Input(placeholder=".routine.inhale… or ~sampel-palnet", id="point"),
            Static("Prior proof.json (spawn's or last rekey's):", classes="label"),
            Input(placeholder="/path/to/~sampel-palnet-spawn.proof.json", id="prior-proof"),
            Static("New pass (hex — your ship's new ring's pass):", classes="label"),
            Input(placeholder="deadbeef...", id="new-pass-hex"),
            Static("Breach (bump rift as well as life)?", classes="label"),
            Checkbox("breach", id="breach"),
            Static("Sponsor for the NEW snapshot (blank = keep the current one):", classes="label"),
            Input(placeholder="~sampel-palnet", id="sponsor"),
            Static("Fief for the NEW snapshot, IP:PORT (blank = keep the current one):", classes="label"),
            Input(placeholder="203.0.113.7:34343", id="fief"),
            Checkbox("no-route: commit no sponsor and no fief (outbound-only)", id="no-route"),
            Horizontal(
                Button("Continue →", id="continue", variant="primary"),
                Button("Back", id="back"),
            ),
            Static("", id="err"),
        ]
        yield Vertical(*children, id="panel")
        yield Footer()

    def on_button_pressed(self, event: Button.Pressed) -> None:
        if event.button.id == "back":
            self.app.pop_screen()
            return
        if event.button.id != "continue":
            return
        state: FlowState = self.app.state  # type: ignore[attr-defined]
        state.op_name = "rekey"
        err = self.query_one("#err", Static)
        try:
            entry = self.query_one("#point", Input).value.strip()
            # Accept a mnemonym or @p; canonicalize to @p for filenames/proof.
            state.point = cw.int_to_patp(cw.resolve_id(entry))
        except Exception as e:
            err.update(f"parse error: {e}")
            return
        # A rekey spends the point's current sat-carrying output key-path, so a
        # prior proof (with its snapshot) is required.
        prior_path = self.query_one("#prior-proof", Input).value.strip()
        if not prior_path:
            err.update("prior proof.json is required — it's how this op spends the point's sat")
            return
        try:
            state.prior_proof = cw.load_proof_json(prior_path)
            if not state.prior_proof.get("commit_txid"):
                err.update("prior proof has no commit_txid — was its tx broadcast?")
                return
        except Exception as e:
            err.update(f"couldn't load prior proof: {e}")
            return
        np = self.query_one("#new-pass-hex", Input).value.strip()
        try:
            state.new_pass_hex = np
            new_key = cw.messaging_key_from_pass(int.from_bytes(bytes.fromhex(np), "little"))
        except Exception as e:
            err.update(f"bad hex: {e}")
            return
        breach = self.query_one("#breach", Checkbox).value
        state.no_route = self.query_one("#no-route", Checkbox).value
        try:
            state.sponsor_atom = cw.resolve_sponsor(
                self.query_one("#sponsor", Input).value.strip() or None)
            state.fief_input = self.query_one("#fief", Input).value.strip()
            state.fief_noun = cw.parse_fief_arg(state.fief_input or None)
        except Exception as e:  # noqa: BLE001
            err.update(str(e))
            return
        prior_snap = state.prior_proof.get("snapshot") or {}
        # Every snapshot change bumps life; a rift bump implies a life bump.
        state.snapshot = {
            "life": int(prior_snap.get("life", 0)) + 1,
            "rift": int(prior_snap.get("rift", 0)) + (1 if breach else 0),
            "key": new_key,
            "sponsor": (state.sponsor_atom if state.sponsor_atom is not None
                        else prior_snap.get("sponsor")),
            # Set-or-carry, matching `causeway rekey --fief`.  Carry-forward
            # was hardcoded here, which made the TUI strictly less capable
            # than the CLI: a comet minted without a fief could never be
            # given one from this screen, and a fief is what a SPONSOR needs.
            "fief": (state.fief_noun if state.fief_noun is not None
                     else prior_snap.get("fief")),
        }
        # A state update that strands the comet is refused here too.
        try:
            cw.assert_routable(state.snapshot, state.no_route)
        except Exception as e:  # noqa: BLE001
            err.update(str(e))
            return
        # No UTXO picker for a rekey: the input is fixed — the prior proof's
        # sat-carrying output (the point's current custody home).
        self.app.push_screen(PsbtBuildScreen())


# ---------------------------------------------------------------------------
# Proof viewer
# ---------------------------------------------------------------------------


class ProofOpenScreen(BaseScreen):
    CSS = """
    Screen { align: center middle; }
    #panel { max-width: 100%; max-height: 100%; overflow-y: auto; width: 80; border: round #ff6a00; padding: 2 4; }
    #title { content-align: center middle; color: #ff6a00; text-style: bold; padding-bottom: 1; }
    Input { width: 100%; }
    #status { padding-top: 1; }
    Button { margin-right: 2; }
    """

    def compose(self) -> ComposeResult:
        yield Header()
        yield Vertical(
            Static("INSPECT / VERIFY PROOF", id="title"),
            Static("Path to proof.json:"),
            Input(placeholder="/path/to/comet.proof.json", id="path"),
            Horizontal(
                Button("Verify (offline)", id="verify-off", variant="primary"),
                Button("Verify (on-chain)", id="verify-on"),
                Button("Show", id="show"),
                Button("Back", id="back"),
            ),
            Static("", id="status"),
            id="panel",
        )
        yield Footer()

    def on_button_pressed(self, event: Button.Pressed) -> None:
        if event.button.id == "back":
            self.app.pop_screen()
            return
        state: FlowState = self.app.state  # type: ignore[attr-defined]
        path = self.query_one("#path", Input).value.strip()
        status = self.query_one("#status", Static)
        if not path:
            status.update("path is required")
            return
        try:
            proof = cw.load_proof_json(path)
        except Exception as e:
            status.update(f"load failed: {e}")
            return
        if event.button.id == "show":
            status.update(json.dumps(proof, indent=2)[:2000])
            return
        if event.button.id == "verify-off":
            ok, reason = cw.verify_proof_self(proof)
        else:
            ok, reason = cw.verify_proof_onchain(proof, mempool_base=state.mempool_base)
        status.update(f"{'OK' if ok else 'FAIL'}: {reason}")


# ---------------------------------------------------------------------------
# App
# ---------------------------------------------------------------------------


def env_prefill(state: "FlowState") -> "FlowState":
    """Seed a FlowState from CAUSEWAY_* environment variables.

    This is the TUI's argument surface.  boot.sh --mint launches the TUI as
    the default face of minting, and a full-screen app cannot take flags the
    way a CLI does -- so the wrapper passes its --sponsor / --fief / work dir
    through the environment, and the spawn screen comes up with them filled
    in.  A user can still edit the fields; these are defaults, not overrides.

      CAUSEWAY_SPONSOR      pre-fill the sponsor field
      CAUSEWAY_FIEF         pre-fill the fief field (IP:PORT)
      CAUSEWAY_OUTPUT_DIR   where proofs and feed files are written
      CAUSEWAY_HANDOFF=1    a wrapper is waiting: the spawn-complete screen
                            offers "quit and continue" and the app exits so
                            the wrapper can finalize and boot
    """
    state.sponsor_input = os.environ.get("CAUSEWAY_SPONSOR", state.sponsor_input)
    state.fief_input = os.environ.get("CAUSEWAY_FIEF", state.fief_input)
    state.output_dir = os.environ.get("CAUSEWAY_OUTPUT_DIR", state.output_dir)
    state.handoff = os.environ.get("CAUSEWAY_HANDOFF", "") == "1"
    return state


class CausewayApp(App):
    CSS = """
    Screen { background: #0a0a0a; }
    """

    def __init__(self) -> None:
        super().__init__()
        self.state = env_prefill(FlowState())

    def on_mount(self) -> None:
        self.push_screen(LandingScreen())


def main() -> None:
    CausewayApp().run()


if __name__ == "__main__":
    main()
