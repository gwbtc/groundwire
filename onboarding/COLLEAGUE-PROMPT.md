# Prompt for a Claude minting a comet on a droplet

Fill in the three fields at the top, then paste the whole block into a local
Claude Code session. `KEY` is a key made for that Claude and authorized on the
droplet by Trent — not a personal key — so its access can be revoked on its
own. `INVITE` is a 74-character hex code from Trent (a spawn he has already
paid for; see `causeway/desktop/README.md`, "Invites"), or `none` if he will
fund the address the mint prints instead. The code is the sats: anyone holding
it can spend them, so pass it the way you would pass money.

> ```
> DROPLET_IP = <the droplet's address>
> KEY        = <the SSH private key authorized on it, a file in ~/.ssh/>
> INVITE     = <74-character hex invite code from Trent, or: none>
> ```
>
> You're helping stand up a **Groundwire confidential comet** on a remote
> droplet, using the three values above. The release is
> **`groundwire-alpha-2026.10.7`**; use exactly that tag in every
> `boot.sh --version`. (An older one
> sends the invite to a retired faucet: "Faucet error: Invalid invite code".) Groundwire is an Urbit fork where a
> ship's identity is anchored to a Bitcoin sat. You'll drive the droplet **over
> SSH from here** — do not try to run anything locally. Read the guide first
> and follow it; don't improvise around the money or key steps:
> https://raw.githubusercontent.com/gwbtc/groundwire/hd/cc-landing/onboarding/TEAM-ONBOARDING.md
>
> Access: `ssh -i ~/.ssh/KEY root@DROPLET_IP` (host key: accept on first
> connect). Use `-o BatchMode=yes` so nothing ever blocks on a password.
>
> The mint (`boot.sh --mint`) **needs a real terminal** — it reads prompts from
> `/dev/tty` — so a plain `ssh host 'command'` will fail. Run it inside a
> **tmux session on the droplet** and drive it from here, exactly as the guide's
> "flow" section shows, **using its commands verbatim** — they encode fixes that
> are easy to get wrong (a shell *function* for ssh, not a string variable;
> `python3-venv` and 8 GB of swap installed first; the sponsor's leading `~`
> quoted inside the wrapper file; `PYTHONUNBUFFERED=1` so the funding address
> actually reaches the log). Start it in tmux, then poll `~/mint.log` and answer
> with `tmux send-keys`.
>
> If `INVITE` is a code (not `none`): add `--invite INVITE` to the `boot.sh
> --mint` line in the guide's wrapper file (step 2), exactly as the guide's
> note under that step shows. The mint then spends the sats already behind the
> code and **never prints a funding address** — the "funding address" rule
> below does not happen, and the leftover lands in the ship's Wallet app. Put
> the code only in that wrapper file: not in chat, not in a log you paste
> back, not on any other command line.
>
> Hard rules:
> - It prints a **12-word recovery phrase** and asks you to re-enter it. Save
>   the words to a private local file (`chmod 600`) and tell me you did, then
>   type them back with `tmux send-keys`. That phrase is the *only* key that can
>   ever rekey this comet (the ship boots from a separate feed file). It asks
>   for the phrase a **second time** right after the spawn is broadcast — answer
>   that the same way.
> - With `INVITE = none`, it prints a **Bitcoin funding address** (bc1p…) and
>   waits. **Stop and give me that address** so I can pass it to the person
>   funding it. **Do not** fund it yourself, invent or broadcast a transaction,
>   or use a faucet. Nothing to type — it watches the chain. Poll `~/mint.log`
>   every few minutes.
> - If the mint dies **after** the address was funded (or after an invite's
>   sats were spent), do **not** start a fresh mint (that strands the sats).
>   Re-run it with `--resume` as the guide describes; it asks for the saved
>   phrase and picks up where it stopped.
> - **The mint ends with the ship stopped — that is by design.** It boots once to
>   set the peer-discovery opt-in, then stops and prints the run command. Start
>   it with the guide's step-5 command, verbatim — `bash ~/boot.sh --detach
>   --vps --version groundwire-alpha-2026.10.7 --comet
>   '<@p>'` — over plain ssh (no tmux needed), then verify. `--vps` gives the
>   comet a groundwire.me name with HTTPS; from then on the plain
>   `http://<ip>:<port>` address redirects to it, so report the name. Before
>   that, check `pgrep -a -f 'gw-vere -t'`: if a vere from the mint is still
>   exiting, wait for it — two boots on one pier collide.
> - If the mint's one-time boot crashes, or `--status` shows `headers 1` /
>   `live peers 0` minutes after boot, follow the guide's two recovery
>   headings under "The flow" — "If the mint's one-time boot died" and "If
>   sync never starts" — rather than improvising. If the mint's own stop step
>   reports that vere has not exited after 900 s, the comet is minted and the
>   ship is simply still running: stop and tell me, do not kill anything.
> - **Never kill a process on the droplet from a `pgrep` count.** ssh runs your
>   command inside a `bash -c` wrapper that matches the same pattern, so the
>   count over-reads by one. Look at `ps -eo pid,etime,args` and reason about
>   the actual pids before touching anything.
> - After boot the Bitcoin light-client sync takes a **few hours** — expected,
>   not a hang. **While it runs the whole ship is slow**: an Urbit ship is one
>   event loop, and the sync pins it (measured: ~90% of a core for hours), so
>   the web UI and every app (Drive, chat) will lag or stall until it finishes.
>   Do not restart the ship, kill anything, or "fix" performance over this.
>   It is done when the ship's log (`~/.groundwire/var/<comet>.log`) prints
>   `%gw-btc: light client is SYNCED` — or the Gevulot pane at
>   `https://<name>/apps/gevulot` says "light client synced".
>
> When done, report:
> - the comet's **@p** (and mnemonym);
> - the **web UI URL**: the `https://<name>.groundwire.me` address that
>   `bash ~/boot.sh --status` shows as `web` (with `--vps` the ship has a
>   name and a certificate; plain `http://<ip>:<port>` redirects there). If
>   the name is not there yet, give `http://DROPLET_IP:<port>`, where
>   `<port>` is the `--http-port` value on the running ship's command line —
>   read it with `ssh … "pgrep -a -f gw-vere | grep -o -- '--http-port [0-9]*'"`
>   rather than assuming 8080 (boot.sh picks a nearby free port if 8080 is
>   taken);
> - the **web login code** (`ssh … 'bash ~/boot.sh --code'`) — I paste it at
>   that URL to log in;
> - confirm `bash ~/boot.sh --status` shows the ship up with `%gw-btc`
>   syncing;
> - and **tell me plainly that the ship will be slow for the next few hours**
>   while the light client syncs, that this is expected, and how I can tell
>   when it's finished (the log line or the Gevulot pane above).
>
> If anything is ambiguous, ask me rather than guessing.
