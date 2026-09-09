# Onboarding a colleague — the message and the Claude prompt

Trent boots one DigitalOcean droplet per person. The colleague's **local**
Claude drives it over SSH (it does not run on the droplet).

## The exchange, per person

1. Colleague sends Trent **their Claude's SSH public key** (their Claude can
   print it: `cat ~/.ssh/id_ed25519.pub`, or generate one).
2. Trent boots the droplet named after them, adds that key to
   `root`'s `authorized_keys`, and sends back **the IP**.
3. Colleague pastes the prompt below into their Claude, IP filled in.
4. Partway through, the Claude hands the colleague a **Bitcoin address**; they
   send it to Trent, who funds it. Everything else is hands-off.

## Message to the colleague

> You're getting a Groundwire comet — project context:
> https://github.com/gwbtc/groundwire/pull/129
>
> First, send me your Claude's SSH public key (`cat ~/.ssh/id_ed25519.pub`).
> I'll boot you a droplet, authorize the key, and send back its IP. Then paste
> the prompt below into your Claude with the IP filled in. It mints and boots
> your comet remotely. **One thing needs you:** partway through it'll give you a
> Bitcoin address — send it to me and I'll fund it (I'm covering the sats). It
> also shows a 12-word recovery phrase; your Claude saves it. After that it's
> hands-off; the first sync takes a few hours.

## The prompt for their Claude

Replace `<DROPLET_IP>` and `<KEY>` (the private key whose public half was sent).

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
