# Team onboarding — spawn a confidential comet on the current branch

**Audience: a teammate, or a teammate's Claude, standing up a comet on a fresh
DigitalOcean droplet.** Follow this top to bottom. It is the supported happy
path as of 2026-09-09; the deep reference is
[`ops/doc/OPERATIONS.md`](../ops/doc/OPERATIONS.md).

## What you're building

One **confidential comet** per person, minted off our development branches,
sponsored by Groundwire's sponsor so we can message each other (Drive, Talon)
off Signal. Trent custodies the Bitcoin — **he funds each spawn**, so there is
exactly one human checkpoint in an otherwise scriptable flow.

## The branches and the release

| what | where |
|---|---|
| Desk (`%gw-btc` + `%gevulot`) | `gwbtc/groundwire` @ **`hd/cc-landing`** |
| Kernel + vere | `gwbtc/urbit` @ **`hd/cc-kernel`** |
| Release to boot from | the newest **`groundwire-rc-*`** prerelease of `gwbtc/urbit` |

Find the exact RC tag (do not use `latest` — that's the daily, which can't do
confidential comets):

```bash
gh release list -R gwbtc/urbit | grep groundwire-rc | head -1
# e.g. groundwire-rc-2026.9.9
```

## The sponsor — nothing to memorize

The comet must commit a sponsor so peers can route to it. Causeway's built-in
default **is** Groundwire's sponsor, so if you pass nothing you get the right
one (announced in yellow before anything is spent). Pass it explicitly anyway,
so a future change to the default can't silently repoint you:

```
~barmul-bolmet-ronlus-lighul--rovtun-satryc-moclug-daplyd   (fief 146.190.199.0:34344)
```

> Do **not** use `~ligdes-risbur-folmus-mattyp-…` — it was minted under the old
> blind-`dat` format and is refused by the current verifier. `~barmul` is the
> only live sponsor.

## The flow (a local Claude driving the droplet over SSH)

The intended model: **your Claude runs on your laptop and SSHes into the
droplet** — it does not run on the droplet. Trent boots the droplet and
authorizes your Claude's SSH public key on it; you get back an IP.

One hard constraint shapes the recipe: `boot.sh --mint` reads its prompts from
`/dev/tty` — it **needs a real terminal**, so a plain `ssh host 'boot.sh
--mint'` fails, and it does not forward `--assume-saved`. Run the mint inside a
**tmux session on the droplet**: tmux provides the terminal, the session
survives your SSH connection, and your Claude drives it with `send-keys` and
reads it from a log. (This is exactly how the ops campaign drove ships.)

```bash
H=root@<DROPLET_IP>; K=~/.ssh/<your-claude-key>
S="ssh -o BatchMode=yes -i $K $H"

# 1. one-time setup on the droplet: tmux + boot.sh from the branch
$S 'command -v tmux >/dev/null || (apt-get update -qq && apt-get install -y -qq tmux)
    curl -fsSL -o ~/boot.sh https://raw.githubusercontent.com/gwbtc/groundwire/hd/cc-landing/causeway/public/boot.sh'

# 2. a wrapper script, so the sponsor's leading ~ is quoted once and never
#    tilde-expanded by a shell (a bare ~barmul... is "no such user" to bash)
$S 'cat > ~/mint.sh <<'"'"'EOF'"'"'
bash ~/boot.sh --mint --headless --detach \
  --version groundwire-rc-2026.9.9 \
  --sponsor "~barmul-bolmet-ronlus-lighul--rovtun-satryc-moclug-daplyd" 2>&1 | tee ~/mint.log
EOF
chmod +x ~/mint.sh'

# 3. start the mint in tmux (gives it the tty it needs; survives disconnects)
$S 'tmux new -d -s mint && tmux send-keys -t mint "bash ~/mint.sh" Enter'

# 4. drive it by polling the log (read-only) and answering with send-keys
$S 'tail -40 ~/mint.log'
#   a) it prints a 12-word RECOVERY PHRASE, then "Please re-enter your seed
#      phrase to confirm you wrote it down" -> save the words, then type them back:
$S 'tmux send-keys -t mint "<the twelve words>" Enter'
#   b) it prints a bc1p... FUNDING ADDRESS, then "Waiting for funding
#      transaction to confirm..." -> hand the address to Trent; keep polling
#      the log every few minutes. Nothing to type: it watches the chain itself.
#   c) on confirmation it mines, spawns, bakes the feed, boots --detach, and exits.

# 5. verify
$S 'bash ~/boot.sh --status; bash ~/boot.sh --code'
```

What happens, in order:

1. Downloads and installs the RC (vere, pill, `%gw-btc`+`%gevulot` desk,
   `comet_miner`, Causeway).
2. Generates a fresh BIP-39 wallet, mines the comet, and **prints a recovery
   phrase and a P2TR funding address, then waits** — polling the chain for that
   address to be funded.
   - **Save the recovery phrase.** With external-wallet custody removed, that
     phrase is the *only* key that can ever rekey/breach this comet. (It is not
     needed just to run the ship — the boot feed is — but losing it means the
     identity can never be rotated.)
   - **→ Send the funding address to Trent.** He sends the spawn sats. This is
     the one manual checkpoint.
3. On confirmation it bakes the custody log into the boot feed and boots the
   comet, detached.

## Verify it worked

```bash
bash boot.sh --status              # ship up? which comet? sync state?
bash boot.sh --code                # web login code (+code) for Landscape
```

In the dojo (`boot.sh --comet '<@p>'` attaches, or via the control socket):

- `%gw-btc` is installed and syncing its light client (first sync is slow —
  low hours; that's expected, not a hang).
- Once synced, you and the other team comets should discover each other through
  the sponsor (see the note below).

## For a driving Claude

- This is **not** fully autonomous: step 2 blocks until Trent funds the address.
  Relay the address to Trent, then wait — `boot.sh` continues on its own once
  the funding tx confirms. Do not fabricate the funding.
- Capture stdout: the recovery phrase and address are printed once. Keep the
  phrase somewhere Trent can retrieve it (a `0600` file on the droplet is fine
  for a test comet); never put it on a command line.
- Everything else — install, mine, sign, broadcast, finalize, boot — needs no
  human. Causeway generates and signs the wallet itself; no external wallet is
  involved.

## Sponsor prerequisite (Trent / infra owner)

Peer **discovery** between team comets runs through `%gevulot` on the sponsor.
`~barmul` booted 2026-08-18, two days before `%gevulot` existed, so it must have
the current desk installed once before discovery works. This does **not** need a
re-mint — a local desk install on the sponsor pier keeps its @p, keys and fief:

```
# on the sponsor droplet (146.190.199.0, pier /opt/gw/piers/sponsor), in its dojo:
|mount %gw-btc                       :: then rsync the hd/cc-landing desk in
|commit %gw-btc
|install our %gw-btc                 :: desk.bill pulls in %gevulot; serving defaults ON
```

Then confirm `%gw-btc` is synced past each new sponsee's spawn (it drops an
`%announce` from a sponsee it hasn't yet verified on chain). Until this is done,
comets still boot and are correctly sponsored — they just fall back to each
ship's own (slow) light-client sync to find peers instead of a fast push.
