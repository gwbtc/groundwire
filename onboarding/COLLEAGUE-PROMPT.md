# Prompt for a Claude minting a comet on a droplet

Paste this into a local Claude Code session. Replace `<DROPLET_IP>` with the
droplet's address and `<KEY>` with the SSH private key authorized on it.

> You're helping stand up a **Groundwire confidential comet** on a remote
> droplet. Groundwire is an Urbit fork where a ship's identity is anchored to a
> Bitcoin sat. You'll drive the droplet **over SSH from here** — do not try to
> run anything locally. Read the guide first and follow it; don't improvise
> around the money or key steps:
> https://raw.githubusercontent.com/gwbtc/groundwire/hd/cc-landing/onboarding/TEAM-ONBOARDING.md
>
> Access: `ssh -i ~/.ssh/<KEY> root@<DROPLET_IP>` (host key: accept on first
> connect). Use `-o BatchMode=yes` so nothing ever blocks on a password.
>
> The mint (`boot.sh --mint`) **needs a real terminal** — it reads prompts from
> `/dev/tty` — so a plain `ssh host 'command'` will fail. Run it inside a
> **tmux session on the droplet** and drive it from here, exactly as the guide's
> "flow" section shows: install tmux if missing, fetch `boot.sh` from the
> branch, write the `~/mint.sh` wrapper (the guide gives it verbatim — the
> sponsor's leading `~` must be quoted inside the file), start it in tmux, then
> poll `~/mint.log` and answer with `tmux send-keys`.
>
> Hard rules:
> - It prints a **12-word recovery phrase** and asks you to re-enter it. Save
>   the words to a private local file (`chmod 600`) and tell me you did, then
>   type them back with `tmux send-keys`. That phrase is the *only* key that can
>   ever rekey this comet (the ship boots from a separate feed file).
> - It prints a **Bitcoin funding address** (bc1p…) and waits. **Stop and give
>   me that address** so I can pass it to the person funding it. **Do not** fund
>   it yourself, invent or broadcast a transaction, or use a faucet. Nothing to
>   type — it watches the chain. Poll `~/mint.log` every few minutes; once the
>   funding confirms it mines, spawns, boots, and exits on its own.
> - After boot the Bitcoin light-client sync takes a **few hours** — expected,
>   not a hang.
>
> When done, report: the comet's **@p** (and mnemonym), the **web login code**
> (`ssh … 'bash ~/boot.sh --code'`), and confirm `bash ~/boot.sh --status` shows
> the ship up with `%gw-btc` syncing. If anything is ambiguous, ask me rather
> than guessing.
