# Prompt for a Claude minting a comet on a droplet

Fill in the fields at the top, then paste the whole block into a local Claude
Code session. `INVITE` is a 74-character hex code from Trent (a spawn he has
already paid for; see `causeway/desktop/README.md`, "Invites"), or `none` if
he will fund the address the mint prints instead. The code is the sats: anyone
holding it can spend them, so pass it the way you would pass money.

Leave `DROPLET_IP` and `KEY` as `new`, and that Claude makes a fresh SSH key
and walks you through creating the droplet in your own DigitalOcean account
(about $24 a month). If Trent already made a droplet and authorized a key for
you, fill in its address and the key file instead. Either way the key belongs
to that Claude, not to you, so its access can be revoked on its own.

To test the flow first with no sats and no droplet, skip this prompt and
follow "Dry run" in `TEAM-ONBOARDING.md`.

> ```
> INVITE     = <74-character hex invite code from Trent, or: none>
> DROPLET_IP = new   (or the address of an existing droplet)
> KEY        = new   (or the SSH private key authorized on it, a file in ~/.ssh/)
> ```
>
> You're helping stand up a **Groundwire confidential comet** on a remote
> droplet, using the values above. Groundwire is an Urbit fork where a ship's
> identity is anchored to a Bitcoin sat. You'll drive the droplet **over SSH
> from here**: do not run the comet locally. Read the guide first and follow
> it; don't improvise around the money or key steps:
> https://raw.githubusercontent.com/gwbtc/groundwire/hd/cc-landing/onboarding/TEAM-ONBOARDING.md
>
> **The release tag.** The release is **`groundwire-alpha-2026.10.8`**. Use
> exactly that tag in every `boot.sh --version`: the mint wrapper (guide step
> 2) and the run command (step 5). If the guide or anything else shows a
> different tag, this one wins. Never use `latest` (the daily, which cannot
> mint confidential comets), an `rc` tag, or a tag you looked up yourself; an
> older release sends the invite to a retired faucet ("Faucet error: Invalid
> invite code"). Before step 1, confirm the tag exists:
> `curl -fsI https://github.com/gwbtc/urbit/releases/tag/groundwire-alpha-2026.10.8`
> must succeed. If it does not, stop and ask me.
>
> **The person you're helping may not be technical.** Explain each step in
> plain words before you take it, one step at a time, and do not assume they
> know what SSH, a droplet or a terminal is. Do everything you can yourself.
> Hand them only what needs their own hands: signing in, paying, and approving.
>
> **First, the key and the droplet** (when `KEY` and `DROPLET_IP` are `new`):
>
> 1. **Make a fresh SSH key** on this machine, for this comet only:
>    `ssh-keygen -t ed25519 -N '' -C groundwire-comet-$(date +%F) -f ~/.ssh/groundwire-comet`.
>    If that file exists, choose a new name; never overwrite a key. Leave the
>    passphrase empty so the key works under `BatchMode`. That file is `KEY`
>    from here on. Tell the person what you made and where, and that it
>    unlocks their server, so they should guard it like a house key.
> 2. **Create the droplet** in their DigitalOcean account. They sign in, or
>    sign up and add a card, themselves; never type a password or card number
>    for them. If you have a browser tool (Claude in Chrome or a built-in
>    browser), drive the DigitalOcean web UI with it; otherwise talk them
>    through each click. Use these settings:
>    - Start at `https://cloud.digitalocean.com/droplets/new?region=sfo2&size=s-2vcpu-4gb`,
>      which pre-fills the region and plan.
>    - **Region:** any region that offers the plan. If the page says "Basic -
>      Regular plans are currently unavailable in your selected region", pick
>      another. SFO2 offered it on 2026-10-07; SFO3 did not.
>    - **Image:** Ubuntu 24.04 (LTS) x64.
>    - **Plan:** Basic → Regular, 2 vCPU / 4 GB / 80 GB, $24/month (slug
>      `s-2vcpu-4gb`). Smaller plans run out of memory.
>    - **Authentication:** SSH Key → Add SSH Key. Paste the contents of
>      `~/.ssh/groundwire-comet.pub` (the public half, never the private file)
>      and give it the key's name. Tick **only that key**.
>    - **Name:** `groundwire-comet`. Leave backups, volumes and extras off.
>    - Before you press **Create Droplet**, tell the person the monthly price
>      and wait for a clear yes: it bills their card.
>
>    The DigitalOcean form sometimes ignores clicks made through a page
>    element reference. Click by screen position, then read the page back to
>    confirm the region, the plan, and "1 / N" keys selected.
> 3. **Wait for the droplet** to show **Active**, and copy its public IPv4
>    address. That is `DROPLET_IP`. For the first minute ssh may refuse while
>    it boots; retry every ten seconds.
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
> with `tmux send-keys`. The mint is done when the log prints `<@p> is ready.
> It is NOT running right now.`; a line starting `error:` means it died.
>
> If `INVITE` is a code (not `none`): add `--invite INVITE` to the `boot.sh
> --mint` line in the guide's wrapper file (step 2), exactly as the guide's
> note under that step shows. The mint then spends the sats already behind the
> code and **never prints a funding address** — the "funding address" rule
> below does not happen, and the leftover lands in the ship's Wallet app. Put
> the code only in that wrapper file: not in chat, not in a log you paste
> back, not on any other command line. Write the file by piping it to
> `ssh … 'cat > ~/mint.sh'` on stdin, so the code never rides an ssh argument.
>
> Hard rules:
> - It prints a **12-word recovery phrase** and asks you to re-enter it. Save
>   the words to a private local file (`chmod 600`) and tell me you did, then
>   type them back with `tmux send-keys`. That phrase is the *only* key that can
>   ever rekey this comet (the ship boots from a separate feed file). It asks
>   for the phrase a **second time** right after the spawn is broadcast — answer
>   that the same way. Tell the person, in plain words, to also write the
>   words on paper and keep them somewhere safe.
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
>   --vps --version groundwire-alpha-2026.10.8 --comet
>   '<@p>'` — over plain ssh (no tmux needed), then verify. `--vps` gives the
>   comet a groundwire.me name with HTTPS; from then on the plain
>   `http://<ip>:<port>` address redirects to it, so report the name. Step 5
>   can take half an hour while the sync keeps the ship busy, so give it a
>   long timeout or run it in the background.
> - **Expect the mint's stop step to kill vere.** It asks vere to stop, waits
>   30 s, then kills it and prints `vere did not exit on SIGTERM within 30s;
>   sending SIGKILL`. On 2026-10-07 vere ignored the request for 900 s,
>   because the sponsor's desk updates kept it busy. A hard kill is safe: the
>   event log is durable and the pier replays on the next boot. If the mint
>   still ends with `error: vere (pid N) has not exited`, the comet is minted
>   and on chain; nothing is lost. Handle it yourself, without asking:
>   1. Kill it: `pkill -9 -f '[g]w-vere .*<@p without the ~>'`. The brackets
>      keep the pattern from matching the ssh `bash -c` wrapper that carries
>      it.
>   2. Confirm that `ps -eo pid,etime,args | grep '[g]w-vere'` prints nothing
>      and that `ss -ltn | grep ':8080 '` prints nothing, then run step 5.
>      Two boots on one pier collide.
> - If the mint's one-time boot crashes, or `--status` shows `headers 1` /
>   `live peers 0` minutes after boot, follow the guide's two recovery
>   headings under "The flow" — "If the mint's one-time boot died" and "If
>   sync never starts" — rather than improvising.
> - **Never kill a process on the droplet from a `pgrep` count.** ssh runs your
>   command inside a `bash -c` wrapper that matches the same pattern, so the
>   count over-reads by one. Look at `ps -eo pid,etime,args` and reason about
>   the actual pids before touching anything. The stop-step kill above is the
>   one sanctioned kill, and its bracketed pattern avoids the wrapper.
> - **Go easy on the control socket while the sync runs.** `--status` and
>   `--code` talk to the ship over a socket and give up after a timeout. On
>   this release a client that gives up mid-reply crashes vere (ship log:
>   `newt: write failed broken pipe`, then `loom: external fault`). The
>   supervisor restarts it, but each crash costs minutes. Check status
>   sparingly, and read the ship log and `ps` first.
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
> - the **web login code**, which I paste at that URL to log in. Try
>   `ssh … 'bash ~/boot.sh --code'` once. If it comes back empty or says the
>   ship is not running, the 90 s timeout ran out; fetch the code this way
>   instead, which waits up to 30 minutes and survives a dropped ssh:
>   `ssh … 'umask 077; nohup bash -c "export GW_DIR=/root/.groundwire SOCK_TOOL=python3; . ~/.groundwire/var/<@p without the ~>.env; . ~/.groundwire/lib/gwlib.sh; gwl_code 1800 > ~/code.txt" >/dev/null 2>&1 &'`,
>   then `ssh … 'cat ~/code.txt && rm ~/code.txt'` once it has a line;
> - confirm `bash ~/boot.sh --status` shows the ship up with `%gw-btc`
>   syncing;
> - where the SSH key and the recovery-phrase file are on this machine, and
>   that the droplet bills about $24 a month until it is destroyed;
> - and **tell me plainly that the ship will be slow for the next few hours**
>   while the light client syncs, that this is expected, and how I can tell
>   when it's finished (the log line or the Gevulot pane above).
>
> If anything is ambiguous, ask me rather than guessing.
