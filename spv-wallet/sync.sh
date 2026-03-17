#!/bin/bash

# Resolve script directory
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

# Load config
DESTS=$(python3 -c "import json; [print(d) for d in json.load(open('$SCRIPT_DIR/config.json'))['dests']]")
SOURCE="$SCRIPT_DIR/desk/"

sync_all() {
    while IFS= read -r dest; do
        rsync -av --delete "$SOURCE" "$dest"
    done <<< "$DESTS"
}

# Initial sync
echo "Starting sync: $SOURCE -> $(echo "$DESTS" | wc -l | tr -d ' ') destinations"
sync_all

# Watch for changes and sync
fswatch -o "$SOURCE" | while read f; do
    echo "Change detected, syncing..."
    sync_all
    echo "Sync complete at $(date +%H:%M:%S)"
done
