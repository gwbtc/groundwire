"""Causeway Textual TUI — richer terminal UI over the same causeway.py logic.

Launch with `causeway-tui` once the package is installed, or
`python3 causeway_tui.py` during development.
"""

from __future__ import annotations

import asyncio
import json
import os
import sys
import threading
import time
from dataclasses import dataclass, field
from typing import Optional

from textual import work
from textual.worker import get_current_worker
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
    # kelvin-9: the sat output commits a snapshot; the pass's dat names the
    # spawn satpoint in plaintext.  Nothing here is secret but the ring, which
    # the miner returns in the feed.
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
    #  Peer discovery (default on): after boot, Causeway asks %gevulot to
    #  accept the attestations this comet's sponsor pushes, so it reaches
    #  peers without waiting for its own light client to sync.
    peer_discovery: bool = True
    # A wrapper (boot.sh --mint) launched us and will finalize + boot after we
    # exit; the DoneScreen offers quit-and-continue instead of back-to-landing.
    handoff: bool = False
    confirm_height: Optional[int] = None
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
            Static("Sponsor (@p or mnemonym) — peers route to your comet through it. "
                   "Pre-filled with Groundwire's own sponsor; edit it to use yours:", classes="hint"),
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
            Checkbox("Peer discovery: learn peers from your sponsor without waiting to sync (recommended)",
                     value=True, id="peer-discovery"),
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
        state.peer_discovery = self.query_one("#peer-discovery", Checkbox).value
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
        # key material a previous spawn left behind: a stale `mnemonic` is not
        # the wallet funding the next comet.
        state.mnemonic = None
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
        # For spawn we need to mine; for manage we skip mining.
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
        try:
            result = cw.mine_comet_from_utxo(u["txid"], u["vout"], 0, state.miner_bin)
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
    #panel { max-width: 100%; max-height: 100%; overflow-y: auto; width: 100; height: auto; border: round #ff6a00; padding: 1 2; }
    #title { content-align: center middle; color: #ff6a00; text-style: bold; }
    #b64 { height: 7; border: round #888; }
    #signed-in { height: 6; border: round #888; }
    Button { margin-right: 2; }
    #actions { padding-top: 1; }
    #status { color: #888; padding-top: 1; }
    .hint { color: #888; }
    #clip { padding-top: 0; }
    """

    def compose(self) -> ComposeResult:
        yield Header()
        yield Vertical(
            Static("BUILD & SIGN COMMIT PSBT", id="title"),
            Static("", id="psbt-copy"),
            TextArea(id="b64", read_only=True),
            Static("", id="psbt-file", classes="hint"),
            Horizontal(
                Button("Copy unsigned PSBT", id="copy-psbt"),
                Button("Paste signed PSBT", id="paste-signed"),
                id="clip",
            ),
            Static("", id="signed-label"),
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
        # Generate flow: Causeway signs; the manual button is redundant and
        # the copy says broadcast.  Connect flow: external signing is the
        # point; the self-sign button is impossible and hidden.
        if state.mnemonic is None:
            #  The wallet signs AND broadcasts -- every real wallet does both
            #  in one motion, and this screen watches the chain.  The paste
            #  path earned its removal: there was no circumstance left where
            #  a wallet could sign but not send.  (Scripts and air-gapped
            #  rigs still have the CLI's --signed-psbt.)
            self.query_one("#self-sign", Button).display = False
            for wid in ("#signed-in", "#signed-label", "#paste-signed", "#broadcast"):
                self.query_one(wid).display = False
            self.query_one("#psbt-copy", Static).update(
                "Unsigned PSBT (base64) — load it into your wallet, review, sign, and "
                "BROADCAST it there. This screen continues by itself once the network has it:")
        else:
            self.query_one("#self-sign", Button).display = False
            self.query_one("#psbt-copy", Static).update(
                "Spawn transaction (signed by the generated wallet — shown for inspection):")
            self.query_one("#signed-label", Static).update("Signed transaction:")

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
                        u["xonly"], state.snapshot, u
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
                    proof, comet=state.comet or "", pass_atom=state.pass_atom or 0, utxo=u,
                )
                #  boot.sh reads this to decide whether to enable %gevulot
                #  peer discovery once the ship is up (default on).
                proof["peer_discovery"] = state.peer_discovery
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
            # A ~600-char base64 blob is the one thing on this screen that
            # cannot be selected by mouse in a TUI, so it also goes to DISK:
            # every external wallet (Sparrow, BlueWallet, Passport, Keystone)
            # opens a .psbt file, and Sparrow writes its signed result back
            # beside it.  Named for the comet/point so two spawns don't clash.
            try:
                os.makedirs(state.output_dir, exist_ok=True)
                who = (state.comet or state.point or "unknown").lstrip("~")
                self._psbt_path = os.path.join(state.output_dir, f"{who}-{state.op_name}.psbt")
                with open(self._psbt_path, "wb") as fh:
                    fh.write(p.serialize())
                self.query_one("#psbt-file", Static).update(
                    f"Also written as a file: {self._psbt_path}  (open it in your wallet; "
                    f"a signed .psbt saved next to it can be pasted below)")
            except OSError as e:
                self._psbt_path = ""
                self.query_one("#psbt-file", Static).update(f"(could not write .psbt file: {e})")
            # The proof and the feed go to disk NOW, before anyone signs.  A
            # segwit txid is fixed before signing, so the proof can already
            # name it -- and it must: the wallet that signs normally
            # broadcasts too, and from that moment the sat is spent.  The
            # proof is the only record of what was committed; the feed is the
            # comet's private key.  Neither is regenerable, and until this
            # was written here a crash anywhere in the 10-60 minute wait for
            # confirmation lost the feed of a comet already on chain.
            commit_txid = p.tx.txid().hex()
            proof["commit_txid"] = commit_txid
            self._pending_proof = proof  # type: ignore[attr-defined]
            os.makedirs(state.output_dir, exist_ok=True)
            pier = (state.comet or state.point or "unknown").lstrip("~")
            proof_path = os.path.join(
                state.output_dir, f"{pier}-{state.op_name}-{commit_txid[:10]}.proof.json")
            cw.write_proof_json(proof, proof_path)
            state.proof_path = proof_path
            state.commit_txid = commit_txid
            if state.op_name == "spawn" and state.feed:
                cw.write_feed_file(os.path.splitext(proof_path)[0] + ".feed", state.feed)
            self.query_one("#b64", TextArea).text = state.psbt_b64_unsigned
            if state.mnemonic is not None:
                # Generate-Wallet flow: Causeway holds the seed that owns the
                # sats, so it signs.  Prompting a user to carry this PSBT to
                # "their wallet" was a hand-off to a wallet that does not
                # exist -- the paste UI is the CONNECT flow's, where the seed
                # deliberately lives elsewhere.
                try:
                    from embit import psbt as _psbt
                    root = cw.mnemonic_to_hdkey(state.mnemonic, network=state.network)
                    signed = _psbt.PSBT.from_base64(state.psbt_b64_unsigned)
                    signed.sign_with(root)
                    state.psbt_b64_signed = signed.to_base64()
                    self.query_one("#signed-in", TextArea).text = state.psbt_b64_signed
                    self.query_one("#status", Static).update(
                        "signed with the generated wallet — click Broadcast")
                except Exception as e:  # noqa: BLE001
                    self.query_one("#status", Static).update(f"self-sign failed: {e}")
            else:
                self.query_one("#status", Static).update(
                    f"watching the chain for {commit_txid[:16]}… — sign and broadcast "
                    "in your wallet; this screen moves on by itself")
                self.watch_chain_worker()
        except Exception as e:
            self.query_one("#status", Static).update(f"build failed: {e}")

    _psbt_path: str = ""
    _advance_lock = threading.Lock()
    _advanced: bool = False
    WATCH_POLL_SECONDS: int = 20

    def _advance_once(self, next_screen) -> bool:
        """Push the next screen exactly once, whichever route got there
        first: the chain watcher, or a paste-and-broadcast."""
        with self._advance_lock:
            if self._advanced:
                return False
            self._advanced = True
        # push first: this may be running ON the watch worker being cancelled
        self.app.call_from_thread(self.app.push_screen, next_screen())
        self.workers.cancel_group(self, "watch")
        return True

    @work(exclusive=True, thread=True, group="watch", exit_on_error=False)
    def watch_chain_worker(self) -> None:
        """Connect flow: the wallet signs AND broadcasts, so all Causeway has
        to do is notice.  Polls the network for the txid the proof already
        names; a paste in the box below is the alternative, not the rule."""
        state: FlowState = self.app.state  # type: ignore[attr-defined]
        status = self.query_one("#status", Static)
        worker = get_current_worker()
        txid = state.commit_txid or ""
        start = time.monotonic()
        while not worker.is_cancelled:
            try:
                seen = bool(cw.tx_hex_if_seen(txid, mempool_base=state.mempool_base))
            except Exception:  # noqa: BLE001 -- a watcher must never take the app down
                seen = False
            if seen:
                self.app.call_from_thread(status.update, f"seen on the network: {txid}")
                self._advance_once(ConfirmWaitScreen if state.op_name == "spawn" else DoneScreen)
                return
            mins = int(time.monotonic() - start) // 60
            self.app.call_from_thread(
                status.update,
                f"watching the chain for {txid[:16]}… ({mins} min) — sign and "
                "broadcast in your wallet")
            for _ in range(self.WATCH_POLL_SECONDS):
                if worker.is_cancelled:
                    return
                time.sleep(1)

    def on_button_pressed(self, event: Button.Pressed) -> None:
        state: FlowState = self.app.state  # type: ignore[attr-defined]
        if event.button.id == "back":
            self.workers.cancel_group(self, "watch")
            self.app.pop_screen()
            return
        if event.button.id == "copy-psbt":
            b64 = state.psbt_b64_unsigned or ""
            if not b64:
                self.notify("no PSBT built yet", severity="warning")
            elif cw.copy_to_clipboard(b64):
                self.notify(f"unsigned PSBT copied ({len(b64)} chars)")
            else:
                self.app.copy_to_clipboard(b64)
                self.notify("sent via OSC 52 (terminal-dependent)")
            return
        if event.button.id == "paste-signed":
            # The inverse problem: a signed PSBT is just as unpasteable by
            # mouse.  Read the system clipboard into the box.
            txt = cw.paste_from_clipboard()
            b64 = cw.signed_input_to_base64((txt or "").encode())
            if b64:
                self.query_one("#signed-in", TextArea).text = b64
                self.notify(f"pasted a PSBT ({len(b64)} chars) from clipboard")
            elif txt and txt.strip():
                self.notify("clipboard has text but it is not a PSBT (base64/hex)", severity="warning")
            else:
                self.notify("clipboard empty or unreadable — type a .psbt path instead", severity="warning")
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
            raw = self.query_one("#signed-in", TextArea).text.strip()
            if not raw:
                self.query_one("#status", Static).update(
                    "paste a signed PSBT first (base64, or the path to a signed .psbt file)")
                return
            # Accept a FILE PATH as well as base64: Sparrow and friends save a
            # signed .psbt next to the one they opened, and typing a short
            # path is the one thing that IS easy in a TUI.  Binary or base64
            # inside the file, either is fine.
            b64 = raw
            if os.path.isfile(os.path.expanduser(raw)):
                data = open(os.path.expanduser(raw), "rb").read()
                b64 = cw.signed_input_to_base64(data)
                if not b64:
                    self.query_one("#status", Static).update(
                        f"{raw}: not a signed PSBT (binary/base64/hex) nor a signed raw transaction.")
                    return
                self.query_one("#signed-in", TextArea).text = b64
                self.notify(f"loaded signed PSBT from {raw}")
            else:
                # pasted text: base64 with any whitespace/newlines, or hex
                b64 = cw.signed_input_to_base64(raw.encode()) or ""
                if not b64:
                    self.query_one("#status", Static).update(
                        "not a signed PSBT (base64 cHNidP8... / hex 70736274ff...) "
                        "nor a signed raw transaction (hex 0200...)")
                    return
            state.psbt_b64_signed = b64
            self.broadcast_worker()

    @work(exclusive=True, thread=True, group="broadcast")
    def broadcast_worker(self) -> None:
        state: FlowState = self.app.state  # type: ignore[attr-defined]
        status = self.query_one("#status", Static)
        try:
            # The proof was written when the PSBT was built (see build_psbt);
            # what is left is to check the signer handed back the transaction
            # that proof describes -- the paste box accepts anything, and a
            # segwit txid does not change under signing, so one comparison
            # settles it -- and to send it.  _broadcast_tx treats "the
            # network already has it" as success, so a wallet that broadcast
            # on its own a moment ago is not an error here.
            cw.assert_signed_is_what_we_built(
                state.psbt_b64_unsigned or "", state.psbt_b64_signed or ""
            )
            commit_txid, tx_hex = cw._extract_tx_from_psbt(state.psbt_b64_signed or "")
            self.app.call_from_thread(status.update, f"broadcasting {commit_txid}")
            broadcast_id = cw._broadcast_tx(tx_hex, mempool_base=state.mempool_base)
            state.commit_txid = broadcast_id
            self._advance_once(ConfirmWaitScreen if state.op_name == "spawn" else DoneScreen)
        except Exception as e:
            self.app.call_from_thread(status.update, f"broadcast failed: {e}")


# ---------------------------------------------------------------------------
# Confirmation wait — the TUI owns the wait, so SPAWN COMPLETE means complete
# ---------------------------------------------------------------------------


class ConfirmWaitScreen(BaseScreen):
    CSS = """
    Screen { align: center middle; }
    #panel { max-width: 100%; max-height: 100%; overflow-y: auto; width: 84; border: round #ff6a00; padding: 2 4; }
    #title { content-align: center middle; color: #ff6a00; text-style: bold; padding-bottom: 1; }
    #txid { color: #ff6a00; }
    .hint { color: #888; padding-top: 1; }
    """

    def compose(self) -> ComposeResult:
        state: FlowState = self.app.state  # type: ignore[attr-defined]
        yield Header()
        yield Vertical(
            Static("WAITING FOR SPAWN CONFIRMATION", id="title"),
            Static("The network has your spawn transaction. Nothing else to do or keep."),
            Static(state.commit_txid or "", id="txid"),
            Static("checking…", id="progress"),
            Static("Usually 10\u201360 minutes. Quitting is safe: the proof and feed are "
                   "on disk, and re-running the installer picks up from here.",
                   classes="hint"),
            id="panel",
        )
        yield Footer()

    def on_mount(self) -> None:
        self._watch = self.confirm_worker()

    @work(exclusive=True, thread=True)
    def confirm_worker(self) -> None:
        state: FlowState = self.app.state  # type: ignore[attr-defined]
        progress = self.query_one("#progress", Static)
        start = time.monotonic()
        while True:
            try:
                st = cw.mempool_get(f"/tx/{state.commit_txid}",
                                    base=state.mempool_base).get("status", {})
                if st.get("confirmed"):
                    state.confirm_height = st.get("block_height")
                    self.app.call_from_thread(self.app.push_screen, DoneScreen())
                    return
            except Exception:  # noqa: BLE001 -- propagation 404s and blips are normal
                pass
            mins = int(time.monotonic() - start) // 60
            self.app.call_from_thread(
                progress.update, f"not confirmed yet — {mins} min elapsed, checking every 30s")
            time.sleep(30)


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
            # The feed file beside the proof is the handoff seam boot.sh reads
            # (${proof%.json}.feed).  Written whether or not anyone is waiting.
            feed_path = ""
            if feed and proof_path:
                feed_path = os.path.splitext(proof_path)[0] + ".feed"
                try:
                    cw.write_feed_file(feed_path, feed)
                except OSError:
                    feed_path = ""
            self._cmd = (
                f"~/.groundwire/causeway finalize {proof_path} "
                f"--feed-file {feed_path} --out-feed {feed_path}.baked"
                if feed_path else "")
            comet_mnemo = cw.patp_to_mnemonym(comet) if comet != "<unknown>" else comet
            confirmed = (f"Confirmed in block {state.confirm_height:,}."
                         if state.confirm_height else "Confirmed.")
            yield Vertical(
                Static("SPAWN COMPLETE", id="title"),
                Static(f"Comet: {comet_mnemo}", classes="label"),
                Static(f"@p:    {comet}", classes="label"),
                Static(confirmed.replace(",", "."), classes="label"),
                Static(f"Records saved to {os.path.dirname(proof_path) or '.'}", classes="label"),
                (Button(f"Boot {comet}", id="handoff-quit", variant="success")
                 if state.handoff else
                 Button("Copy the finalize command", id="copy-cmd")),
                (Static("", classes="label") if state.handoff else
                 Static("Run it once, then boot.sh with the feed it writes.", classes="label")),
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
    #  Groundwire's sponsor as a VISIBLE, EDITABLE prefill -- the field
    #  shows exactly what will be committed and whose it is; clearing it
    #  and ticking no-route is still one keystroke away.
    if not state.sponsor_input:
        state.sponsor_input = cw.DEFAULT_SPONSOR
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
        #  Causeway is minting + management only.  Running (or resuming) a
        #  ship is the runner's job (boot.sh --comet <@p>, off the pier),
        #  so the TUI does not offer to boot an existing identity -- the
        #  chooser that did paired a stale proof with a shared feed and
        #  booted the wrong ship.
        self.push_screen(LandingScreen())


def main() -> None:
    CausewayApp().run()


if __name__ == "__main__":
    main()
