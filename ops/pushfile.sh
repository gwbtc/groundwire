#!/bin/bash
# pushfile.sh <local-file> <host> <remote-path>   -- run FROM your laptop
#
# Copy one file to a droplet and PROVE it arrived intact: sha256 and byte
# count compared against the local original, refusing on any mismatch.
#
# WHY THIS EXISTS.  `ssh -n` silently truncated a script to ZERO BYTES on
# this rig, and the zero-byte file then passed `bash -n` -- so a syntax
# check "validated" a file with nothing in it, and the deploy looked clean.
# Same family as everything else that has gone wrong here: a command that
# succeeds at doing nothing.  Nothing below believes a remote file it has
# not hashed.
#
# Env:
#   GW_SSH_KEY   private key (default ~/.ssh/id_ed25519)
#   GW_SSH_USER  remote user (default root)
set -eu

SRC="${1:?local file}"
HOST="${2:?host}"
DST="${3:?remote path}"

KEY="${GW_SSH_KEY:-$HOME/.ssh/id_ed25519}"
USER="${GW_SSH_USER:-root}"
SSHO="-o BatchMode=yes -o StrictHostKeyChecking=accept-new -o ConnectTimeout=15 -i $KEY"

[ -s "$SRC" ] || { echo "REFUSING: $SRC is empty or missing"; exit 1; }
[ -r "$KEY" ] || { echo "REFUSING: cannot read key $KEY (set GW_SSH_KEY)"; exit 1; }

WANT=$(shasum -a 256 "$SRC" | cut -d' ' -f1)
SZ=$(wc -c < "$SRC" | tr -d ' ')

#  base64 over the wire: a raw heredoc is what got truncated, and binary or
#  quote-heavy payloads do not survive a naive `cat >` either.
base64 < "$SRC" | ssh $SSHO "$USER@$HOST" \
  "cat > /tmp/.push.b64 && base64 -d < /tmp/.push.b64 > '$DST' && chmod +x '$DST' && rm -f /tmp/.push.b64"

GOT=$(ssh $SSHO "$USER@$HOST" "sha256sum '$DST' | cut -d' ' -f1; wc -c < '$DST'" < /dev/null)
RHASH=$(echo "$GOT" | head -1)
RSZ=$(echo "$GOT" | tail -1 | tr -d ' ')

if [ "$RHASH" != "$WANT" ] || [ "$RSZ" != "$SZ" ]; then
  echo "PUSH FAILED $HOST:$DST  local $WANT/$SZ  remote $RHASH/$RSZ"
  exit 1
fi
echo "pushed $HOST:$DST  $SZ bytes  sha256 ${WANT:0:16}"
