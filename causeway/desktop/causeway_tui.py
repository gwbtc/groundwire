"""Causeway Textual TUI — richer terminal UI over the same causeway.py logic.

Launch with `causeway-tui` once the package is installed, or
`python3 causeway_tui.py` during development.
"""

from __future__ import annotations

import asyncio
import ipaddress
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
    pass_atom: Optional[int] = None
    attestation: Optional[bytes] = None
    psbt_b64_unsigned: Optional[str] = None
    psbt_b64_signed: Optional[str] = None
    commit_txid: Optional[str] = None
    proof_path: Optional[str] = None
    op_name: str = "spawn"

    # Manage context
    point: Optional[str] = None
    parent: Optional[str] = None
    new_pass_hex: Optional[str] = None
    fief_ip: Optional[str] = None
    fief_port: Optional[int] = None
    escape_sig_hex: Optional[str] = None


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
    #panel { width: 72; border: round #ff6a00; padding: 2 4; }
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
            Button("Connect Wallet  (paste xpub, sign PSBT externally)", id="connect", variant="primary"),
            Button("Generate New Wallet  (fresh BIP-39 seed in memory)", id="generate"),
            Static(" "),
            Static("Connect Wallet is recommended if you already hold BTC in Sparrow, Passport, Keystone, etc.", classes="hint"),
            id="panel",
        )
        yield Footer()

    def on_button_pressed(self, event: Button.Pressed) -> None:
        state: FlowState = self.app.state  # type: ignore[attr-defined]
        if event.button.id == "connect":
            state.op_name = "spawn"
            self.app.push_screen(XpubInputScreen(mode="connect"))
        elif event.button.id == "generate":
            state.op_name = "spawn"
            self.app.push_screen(GenerateSeedScreen())


# ---------------------------------------------------------------------------
# Xpub / descriptor input — shared by spawn-connect and manage-*
# ---------------------------------------------------------------------------


class XpubInputScreen(BaseScreen):
    CSS = """
    Screen { align: center middle; }
    #panel { width: 88; border: round #ff6a00; padding: 2 4; }
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
    #panel { width: 90; border: round #ff6a00; padding: 2 4; }
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
    #panel { width: 88; border: round #ff6a00; padding: 2 4; }
    #title { content-align: center middle; color: #ff6a00; text-style: bold; padding-bottom: 1; }
    #addr { text-style: bold; color: #ff6a00; padding: 1 0; }
    #log { height: 10; border: round #888; padding: 0 1; }
    Button { margin-right: 2; }
    """

    def compose(self) -> ComposeResult:
        yield Header()
        yield Vertical(
            Static("WAITING FOR FUNDING", id="title"),
            Static("Send at least 1000 sats to this address:"),
            Static("(loading...)", id="addr"),
            Log(id="log"),
            Horizontal(
                Button("Back", id="back"),
                Button("Scan now (skip polling)", id="skip"),
            ),
            id="panel",
        )
        yield Footer()

    def on_mount(self) -> None:
        self.poll_worker()

    @work(exclusive=True, thread=True)
    def poll_worker(self) -> None:
        state: FlowState = self.app.state  # type: ignore[attr-defined]
        if state.source is None:
            return
        first_addr, _spk, _xonly, _p = state.source.derive_address(0, 0)
        self.app.call_from_thread(self.query_one("#addr", Static).update, first_addr)
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
        while True:
            try:
                utxos = cw.scan_addresses(state.source, n_receive=5, n_change=2, mempool_base=state.mempool_base)
                confirmed = [u for u in utxos if u["confirmed"]]
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

    def on_button_pressed(self, event: Button.Pressed) -> None:
        if event.button.id == "back":
            self.app.pop_screen()
        elif event.button.id == "skip":
            self.app.push_screen(UtxoPickerScreen())


# ---------------------------------------------------------------------------
# UTXO picker — DataTable
# ---------------------------------------------------------------------------


class UtxoPickerScreen(BaseScreen):
    CSS = """
    Screen { align: center middle; }
    #panel { width: 100; height: 30; border: round #ff6a00; padding: 1 2; }
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
        if state.op_name == "spawn":
            self.app.push_screen(MiningScreen())
        else:
            self.app.push_screen(PsbtBuildScreen())

    def on_button_pressed(self, event: Button.Pressed) -> None:
        if event.button.id == "pick":
            self.pick_current()
        elif event.button.id == "refresh":
            self.scan()
        elif event.button.id == "back":
            self.app.pop_screen()


# ---------------------------------------------------------------------------
# Mining screen — subprocess progress
# ---------------------------------------------------------------------------


class MiningScreen(BaseScreen):
    CSS = """
    Screen { align: center middle; }
    #panel { width: 80; border: round #ff6a00; padding: 2 4; }
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
        self.app.call_from_thread(log.write_line, f"Tweak: txid={u['txid'][:16]}... vout={u['vout']} off=0")
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
        ring_uw = result.get("ring", "")
        tweak_raw = cw.build_tweak_bytes(u["txid"], u["vout"], 0)
        state.pass_atom = cw.derive_pass_from_ring(ring_uw, tweak_raw)
        self.app.call_from_thread(log.write_line, f"Mined {state.comet}")
        self.app.call_from_thread(log.write_line, f"Pass atom: 0x{state.pass_atom:x}")

        # Build the %spawn sotx attestation
        comet_p = cw.patp_to_int(state.comet)
        spkh = cw.compute_spkh(u["address"], u["value"])
        state.attestation = cw.encode_spawn_sotx(
            comet_p=comet_p,
            pass_atom=state.pass_atom,
            spkh=spkh,
            vout=u["vout"],
            off=0,
            tej=0,
            fief=None,
        )
        self.app.call_from_thread(log.write_line, f"Attestation: {len(state.attestation)} bytes")
        self.app.call_from_thread(self.app.push_screen, PsbtBuildScreen())


# ---------------------------------------------------------------------------
# PSBT build + sign
# ---------------------------------------------------------------------------


class PsbtBuildScreen(BaseScreen):
    CSS = """
    Screen { align: center middle; }
    #panel { width: 100; height: 40; border: round #ff6a00; padding: 1 2; }
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
        if state.picked_utxo is None or state.source is None or state.attestation is None:
            self.query_one("#status", Static).update("missing prerequisites — go back")
            return
        try:
            p, proof = cw.build_confidential_commit_psbt(
                utxo_txid=state.picked_utxo["txid"],
                utxo_vout=state.picked_utxo["vout"],
                utxo_value=state.picked_utxo["value"],
                utxo_script_pubkey=state.picked_utxo["scriptpubkey"],
                funding_internal_xonly=state.picked_utxo["xonly"],
                funding_path=state.picked_utxo["path"],
                funding_fingerprint=state.source.master_fingerprint,
                attestation_bytes=state.attestation,
                fee_rate=2,
                network=state.network,
            )
            proof["op"] = state.op_name
            proof["patp"] = state.comet or state.point or ""
            if state.pass_atom is not None:
                proof["pass_atom_hex"] = hex(state.pass_atom)
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
            commit_txid, tx_hex = cw._extract_tx_from_psbt(state.psbt_b64_signed or "")
            self.app.call_from_thread(status.update, f"extracted tx — broadcasting {commit_txid}")
            broadcast_id = cw._broadcast_tx(tx_hex, mempool_base=state.mempool_base)
            state.commit_txid = broadcast_id
            proof = getattr(self, "_pending_proof", None) or {}
            proof["commit_txid"] = broadcast_id
            os.makedirs(state.output_dir, exist_ok=True)
            pier = (state.comet or state.point or "unknown").lstrip("~")
            suffix = f"{pier}-{state.op_name}-{broadcast_id[:10]}.proof.json"
            proof_path = os.path.join(state.output_dir, suffix)
            cw.write_proof_json(proof, proof_path)
            state.proof_path = proof_path
            self.app.call_from_thread(self.app.push_screen, DoneScreen())
        except Exception as e:
            self.app.call_from_thread(status.update, f"broadcast failed: {e}")


# ---------------------------------------------------------------------------
# Done screen — boot command or manage summary
# ---------------------------------------------------------------------------


class DoneScreen(BaseScreen):
    CSS = """
    Screen { align: center middle; }
    #panel { width: 96; border: round green; padding: 2 4; }
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
            cmd = (
                f"curl -fsSL https://groundwire.io/causeway/boot.sh | \\\n"
                f"  bash -s -- --comet {comet} --feed {feed} --proof {proof_path}"
            )
            yield Vertical(
                Static("SPAWN COMPLETE", id="title"),
                Static(f"Comet: {comet}", classes="label"),
                Static(f"Commit txid: {state.commit_txid}", classes="label"),
                Static(f"Proof: {proof_path}", classes="label"),
                Static("Run this to boot:", classes="label"),
                Static(cmd, id="boot"),
                Static(
                    "⚠ Runtime does not yet consume --proof. The proof file will be "
                    "saved to ~/.groundwire/ but won't propagate via Ames until runtime support lands.",
                    classes="label",
                ),
                Button("Done  →  back to landing", id="home", variant="primary"),
                id="panel",
            )
        else:
            yield Vertical(
                Static(f"{state.op_name.upper()} BROADCAST", id="title"),
                Static(f"Point: {state.point}", classes="label"),
                Static(f"Commit txid: {state.commit_txid}", classes="label"),
                Static(f"Proof: {state.proof_path}", classes="label"),
                Static(
                    "Import the proof into your ship's Ames state via "
                    f"`:spv-wallet|import-proof ...` (see {state.proof_path}).",
                    classes="label",
                ),
                Button("Done  →  back to landing", id="home", variant="primary"),
                id="panel",
            )
        yield Footer()

    def on_button_pressed(self, event: Button.Pressed) -> None:
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
    #panel { width: 72; border: round #ff6a00; padding: 2 4; }
    #title { content-align: center middle; color: #ff6a00; text-style: bold; padding-bottom: 1; }
    RadioSet { padding: 1 0; }
    Button { margin-right: 2; }
    """

    def compose(self) -> ComposeResult:
        yield Header()
        yield Vertical(
            Static("MANAGE — pick an operation", id="title"),
            RadioSet(
                RadioButton("rekey (%keys)", id="rekey", value=True),
                RadioButton("escape (%escape)", id="escape"),
                RadioButton("cancel-escape (%cancel-escape)", id="cancel-escape"),
                RadioButton("fief (%fief)", id="fief"),
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
    #panel { width: 90; border: round #ff6a00; padding: 2 4; }
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
            Static(f"{self.op.upper()} — fill in op fields", id="title"),
            Static("Target @p:", classes="label"),
            Input(placeholder="~sampel-palnet", id="point"),
            Static("Xpub / descriptor:", classes="label"),
            Input(placeholder="xpub... or tr([fp/86h/0h/0h]xpub...)", id="xpub"),
        ]
        if self.op == "rekey":
            children += [
                Static("New pass (hex):", classes="label"),
                Input(placeholder="deadbeef...", id="new-pass-hex"),
            ]
        elif self.op in ("escape", "cancel-escape"):
            children += [
                Static("Parent @p:", classes="label"),
                Input(placeholder="~daplyd", id="parent"),
            ]
            if self.op == "escape":
                children += [
                    Static("Escape sig (hex, optional):", classes="label"),
                    Input(placeholder="leave blank for no pre-sig", id="escape-sig"),
                ]
        elif self.op == "fief":
            children += [
                Static("IP (v4 or v6):", classes="label"),
                Input(placeholder="1.2.3.4 or ::1", id="ip"),
                Static("Port:", classes="label"),
                Input(placeholder="31337", id="port"),
            ]
        children += [
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
        err = self.query_one("#err", Static)
        try:
            state.point = self.query_one("#point", Input).value.strip()
            comet_p = cw.patp_to_int(state.point)
            xpub_val = self.query_one("#xpub", Input).value.strip()
            state.source = cw.parse_key_source(xpub_val, network=state.network)
        except Exception as e:
            err.update(f"parse error: {e}")
            return
        if self.op == "rekey":
            np = self.query_one("#new-pass-hex", Input).value.strip()
            try:
                state.new_pass_hex = np
                pass_atom = int.from_bytes(bytes.fromhex(np), "little")
            except Exception as e:
                err.update(f"bad hex: {e}")
                return
            state.attestation = cw.encode_keys_sotx(comet_p=comet_p, pass_atom=pass_atom, breach=False)
        elif self.op == "escape":
            state.parent = self.query_one("#parent", Input).value.strip()
            sig_hex = self.query_one("#escape-sig", Input).value.strip() or None
            esc_sig = int.from_bytes(bytes.fromhex(sig_hex), "little") if sig_hex else None
            state.attestation = cw.encode_escape_sotx(
                comet_p=comet_p,
                parent_p=cw.patp_to_int(state.parent),
                escape_sig=esc_sig,
            )
        elif self.op == "cancel-escape":
            state.parent = self.query_one("#parent", Input).value.strip()
            state.attestation = cw.encode_cancel_escape_sotx(
                comet_p=comet_p, parent_p=cw.patp_to_int(state.parent)
            )
        elif self.op == "fief":
            ip_str = self.query_one("#ip", Input).value.strip()
            try:
                port = int(self.query_one("#port", Input).value.strip())
            except Exception as e:
                err.update(f"bad port: {e}")
                return
            if ":" in ip_str:
                ip_int = int(ipaddress.IPv6Address(ip_str)); fief = ("is", ip_int, port)
            else:
                ip_int = int(ipaddress.IPv4Address(ip_str)); fief = ("if", ip_int, port)
            state.attestation = cw.encode_fief_sotx(comet_p=comet_p, fief=fief)
        self.app.push_screen(UtxoPickerScreen())


# ---------------------------------------------------------------------------
# Proof viewer
# ---------------------------------------------------------------------------


class ProofOpenScreen(BaseScreen):
    CSS = """
    Screen { align: center middle; }
    #panel { width: 80; border: round #ff6a00; padding: 2 4; }
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


class CausewayApp(App):
    CSS = """
    Screen { background: #0a0a0a; }
    """

    def __init__(self) -> None:
        super().__init__()
        self.state = FlowState()

    def on_mount(self) -> None:
        self.push_screen(LandingScreen())


def main() -> None:
    CausewayApp().run()


if __name__ == "__main__":
    main()
