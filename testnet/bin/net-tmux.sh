#!/usr/bin/env bash
# Attach a tmux dashboard over the running regtest comet net. Read-only panes:
# bitcoind/chain info, the live web dashboard, per-pier slog tails, and the
# runner log. Safe to attach/detach any time; closing it kills nothing.
#
#   bin/net-tmux.sh            # create/attach session "gw"
set -euo pipefail
cd "$(dirname "$0")/.."
SESSION="${GW_TMUX_SESSION:-gw}"
PY="python3 -m gwharness"

if tmux has-session -t "$SESSION" 2>/dev/null; then
  exec tmux attach -t "$SESSION"
fi

tmux new-session -d -s "$SESSION" -n dashboard
tmux send-keys -t "$SESSION:dashboard" "$PY dashboard" C-m

tmux new-window -t "$SESSION" -n chain
tmux send-keys -t "$SESSION:chain" \
  "while true; do clear; $PY info; echo; date; sleep 3; done" C-m

tmux new-window -t "$SESSION" -n runner
tmux send-keys -t "$SESSION:runner" \
  "tail -F run/scenarios.log run/m1.log 2>/dev/null" C-m

tmux new-window -t "$SESSION" -n piers
tmux send-keys -t "$SESSION:piers" \
  "tail -F run/logs/*.log 2>/dev/null || (echo 'no piers yet'; sleep 5)" C-m

tmux select-window -t "$SESSION:dashboard"
exec tmux attach -t "$SESSION"
