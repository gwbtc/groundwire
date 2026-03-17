#!/usr/bin/env bash
set -euo pipefail

#
# Sets up a local mempool.space instance for regtest.
# Same API as mempool.space — just point your code at localhost:8080.
#
# What this does:
#   1. Installs Docker (via colima + docker CLI) if missing
#   2. Starts a regtest bitcoind
#   3. Starts electrs (indexes the regtest chain)
#   4. Starts mempool backend + frontend + mariadb
#   5. Creates a wallet and mines 101 blocks so you have funds
#
# Usage:
#   ./scripts/setup-mempool.sh          # start everything
#   ./scripts/setup-mempool.sh stop     # tear it all down
#   ./scripts/setup-mempool.sh mine N   # mine N blocks
#   ./scripts/setup-mempool.sh status   # check what's running
#

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
MEMPOOL_DIR="$SCRIPT_DIR/.mempool-regtest"
BITCOIN_DIR="$MEMPOOL_DIR/bitcoin"
ELECTRS_DIR="$MEMPOOL_DIR/electrs"
DATA_DIR="$MEMPOOL_DIR/data"
MYSQL_DIR="$MEMPOOL_DIR/mysql"

# Use docker-compose standalone or docker compose plugin
if docker compose version &>/dev/null; then
  DC="docker compose"
elif command -v docker-compose &>/dev/null; then
  DC="docker-compose"
else
  DC="docker compose"
fi

RPC_USER="spvwallet"
RPC_PASS="spvwallet"
RPC_PORT="18443"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m'

log()  { echo -e "${GREEN}[mempool]${NC} $*"; }
warn() { echo -e "${YELLOW}[mempool]${NC} $*"; }
err()  { echo -e "${RED}[mempool]${NC} $*" >&2; }

# ---------------------------------------------------------------------------
# Install dependencies
# ---------------------------------------------------------------------------
ensure_brew() {
  if ! command -v brew &>/dev/null; then
    err "Homebrew not found. Install it: https://brew.sh"
    exit 1
  fi
}

ensure_docker() {
  if command -v docker &>/dev/null; then
    log "Docker CLI found: $(docker --version)"
  else
    log "Installing Docker CLI + colima..."
    ensure_brew
    brew install docker docker-compose colima
    log "Docker installed."
  fi

  # Ensure docker-compose plugin is linked
  mkdir -p ~/.docker
  if [ ! -f ~/.docker/config.json ]; then
    echo '{}' > ~/.docker/config.json
  fi
  if ! grep -q cliPluginsExtraDirs ~/.docker/config.json 2>/dev/null; then
    # Add the plugin path so `docker compose` works
    python3 -c "
import json
with open('$HOME/.docker/config.json') as f:
    cfg = json.load(f)
cfg['cliPluginsExtraDirs'] = ['/opt/homebrew/lib/docker/cli-plugins', '/usr/local/lib/docker/cli-plugins']
with open('$HOME/.docker/config.json', 'w') as f:
    json.dump(cfg, f, indent=2)
" 2>/dev/null || true
  fi
}

ensure_colima() {
  if ! command -v colima &>/dev/null; then
    ensure_brew
    brew install colima
  fi
  if ! colima status &>/dev/null; then
    log "Starting colima (Docker VM)..."
    colima start --cpu 2 --memory 4
    log "Colima running."
  else
    log "Colima already running."
  fi
}

ensure_bitcoind() {
  if command -v bitcoind &>/dev/null; then
    return
  fi
  log "Installing Bitcoin Core..."
  ensure_brew
  brew install bitcoin
}

# ---------------------------------------------------------------------------
# Bitcoin regtest
# ---------------------------------------------------------------------------
start_bitcoind() {
  mkdir -p "$BITCOIN_DIR"

  # Write config
  cat > "$BITCOIN_DIR/bitcoin.conf" <<CONF
regtest=1
server=1
txindex=1
[regtest]
rpcuser=$RPC_USER
rpcpassword=$RPC_PASS
rpcport=$RPC_PORT
rpcallowip=0.0.0.0/0
rpcbind=0.0.0.0
fallbackfee=0.0001
CONF

  # Write a cookie file for electrs (same format as bitcoind .cookie)
  mkdir -p "$BITCOIN_DIR/regtest"
  echo -n "${RPC_USER}:${RPC_PASS}" > "$BITCOIN_DIR/regtest/.cookie"

  if bitcoin-cli -datadir="$BITCOIN_DIR" -rpcport=$RPC_PORT getblockchaininfo &>/dev/null; then
    log "bitcoind already running."
    return
  fi

  log "Starting bitcoind (regtest)..."
  if ! bitcoind -datadir="$BITCOIN_DIR" -daemon 2>/dev/null; then
    # Lock file exists but RPC not responding — wait for it
    log "Waiting for bitcoind..."
  fi

  # Wait for RPC
  for i in $(seq 1 30); do
    if bitcoin-cli -datadir="$BITCOIN_DIR" -rpcport=$RPC_PORT getblockchaininfo &>/dev/null; then
      log "bitcoind ready."
      return
    fi
    sleep 1
  done
  err "bitcoind failed to start"
  exit 1
}

bitcoin_cli() {
  bitcoin-cli -datadir="$BITCOIN_DIR" -rpcport=$RPC_PORT "$@"
}

setup_wallet_and_mine() {
  # Create default wallet if needed
  if ! bitcoin_cli listwallets 2>/dev/null | grep -q "default"; then
    bitcoin_cli createwallet "default" 2>/dev/null || true
    bitcoin_cli loadwallet "default" 2>/dev/null || true
  fi

  local blocks
  blocks=$(bitcoin_cli getblockcount 2>/dev/null || echo "0")
  if [ "$blocks" -lt 101 ]; then
    local addr
    addr=$(bitcoin_cli getnewaddress)
    log "Mining 101 blocks to $addr..."
    bitcoin_cli generatetoaddress 101 "$addr" >/dev/null
    log "Mined 101 blocks. Mature coinbase ready."
  else
    log "Chain already has $blocks blocks."
  fi

  # Mine one more block so the tip timestamp is recent.
  # Without this, bitcoind reports initialblockdownload=true
  # and electrs hangs waiting for IBD to finish.
  local ibd
  ibd=$(bitcoin_cli getblockchaininfo 2>/dev/null | grep -o '"initialblockdownload":[a-z]*' | cut -d: -f2)
  if [ "$ibd" = "true" ]; then
    local addr
    addr=$(bitcoin_cli getnewaddress)
    bitcoin_cli generatetoaddress 1 "$addr" >/dev/null
    log "Mined extra block to exit IBD mode."
  fi
}

# ---------------------------------------------------------------------------
# Docker compose for electrs + mempool
# ---------------------------------------------------------------------------
write_docker_compose() {
  mkdir -p "$DATA_DIR" "$MYSQL_DIR/data"

  # Get host IP that Docker containers can reach
  local host_ip
  host_ip=$(colima ssh -- ip route show default 2>/dev/null | awk '{print $3}' || echo "host.docker.internal")

  cat > "$MEMPOOL_DIR/docker-compose.yml" <<YAML
services:
  electrs:
    image: blockstream/esplora:latest
    restart: on-failure
    stop_grace_period: 5s
    entrypoint: ["/srv/explorer/electrs_bitcoin/bin/electrs"]
    command: >
      --network regtest
      --daemon-rpc-addr ${host_ip}:${RPC_PORT}
      --http-addr 0.0.0.0:3000
      --cookie ${RPC_USER}:${RPC_PASS}
      --db-dir /data
      --jsonrpc-import
    volumes:
      - ./electrs-data:/data
    ports:
      - "3000:3000"
    healthcheck:
      test: ["CMD-SHELL", "bash -c 'echo > /dev/tcp/localhost/3000' || exit 1"]
      interval: 5s
      timeout: 5s
      retries: 20
      start_period: 30s

  db:
    image: mariadb:10.5.21
    restart: on-failure
    stop_grace_period: 5s
    environment:
      MYSQL_DATABASE: "mempool"
      MYSQL_USER: "mempool"
      MYSQL_PASSWORD: "mempool"
      MYSQL_ROOT_PASSWORD: "admin"
      MARIADB_AUTO_UPGRADE: "1"
    volumes:
      - ./mysql/data:/var/lib/mysql
    healthcheck:
      test: ["CMD", "mysqladmin", "ping", "-h", "localhost", "-u", "mempool", "-pmempool"]
      interval: 5s
      timeout: 5s
      retries: 10

  api:
    image: mempool/backend:latest
    restart: on-failure
    stop_grace_period: 5s
    depends_on:
      db:
        condition: service_healthy
      electrs:
        condition: service_healthy
    environment:
      MEMPOOL_BACKEND: "esplora"
      MEMPOOL_NETWORK: "regtest"
      MEMPOOL_HTTP_PORT: "8999"
      CORE_RPC_HOST: "${host_ip}"
      CORE_RPC_PORT: "${RPC_PORT}"
      CORE_RPC_USERNAME: "${RPC_USER}"
      CORE_RPC_PASSWORD: "${RPC_PASS}"
      ESPLORA_REST_API_URL: "http://electrs:3000"
      DATABASE_ENABLED: "true"
      DATABASE_HOST: "db"
      DATABASE_DATABASE: "mempool"
      DATABASE_USERNAME: "mempool"
      DATABASE_PASSWORD: "mempool"
      STATISTICS_ENABLED: "true"
    command: "./wait-for-it.sh db:3306 --timeout=720 --strict -- ./start.sh"
    volumes:
      - ./data:/backend/cache
    healthcheck:
      test: ["CMD-SHELL", "node -e 'fetch(\"http://localhost:8999/api/v1/backend-info\").then(r=>{if(!r.ok)process.exit(1)}).catch(()=>process.exit(1))'"]
      interval: 10s
      timeout: 5s
      retries: 20
      start_period: 30s

  web:
    image: mempool/frontend:latest
    restart: on-failure
    stop_grace_period: 5s
    depends_on:
      api:
        condition: service_healthy
    environment:
      FRONTEND_HTTP_PORT: "8080"
      BACKEND_MAINNET_HTTP_HOST: "api"
    command: "./wait-for db:3306 --timeout=720 -- nginx -g 'daemon off;'"
    ports:
      - "8080:8080"
    healthcheck:
      test: ["CMD-SHELL", "wget -qO- http://localhost:8080/ || exit 1"]
      interval: 10s
      timeout: 5s
      retries: 10
      start_period: 20s
YAML

  log "docker-compose.yml written."
}

start_mempool() {
  cd "$MEMPOOL_DIR"
  # Clean up any leftover unhealthy containers
  if $DC ps -q 2>/dev/null | grep -q .; then
    $DC down 2>/dev/null || true
  fi
  log "Pulling images (first run may take a few minutes)..."
  $DC pull
  log "Starting mempool stack..."
  $DC up -d
  log "Waiting for services..."

  for i in $(seq 1 60); do
    if curl -sf http://localhost:8080/api/v1/backend-info &>/dev/null; then
      log "mempool API ready at http://localhost:8080"
      echo ""
      log "API base: http://localhost:8080/api"
      log "Example:  curl http://localhost:8080/api/address/<addr>"
      log "Example:  curl http://localhost:8080/api/tx/<txid>"
      log "UI:       http://localhost:8080"
      echo ""
      return
    fi
    sleep 2
  done
  warn "Services started but API not responding yet. Check: cd $MEMPOOL_DIR && $DC logs"
}

# ---------------------------------------------------------------------------
# Stop
# ---------------------------------------------------------------------------
do_stop() {
  log "Stopping mempool stack..."
  if [ -f "$MEMPOOL_DIR/docker-compose.yml" ]; then
    cd "$MEMPOOL_DIR"
    $DC down
  fi

  log "Stopping bitcoind..."
  bitcoin_cli stop 2>/dev/null || true
  # Wait for bitcoind to fully release lock
  for i in $(seq 1 15); do
    if ! bitcoin_cli getblockchaininfo &>/dev/null; then
      break
    fi
    sleep 1
  done
  log "Stopped."
}

# ---------------------------------------------------------------------------
# Mine
# ---------------------------------------------------------------------------
do_mine() {
  local n="${1:-1}"
  local addr
  addr=$(bitcoin_cli getnewaddress)
  bitcoin_cli generatetoaddress "$n" "$addr" >/dev/null
  log "Mined $n blocks."
}

# ---------------------------------------------------------------------------
# Status
# ---------------------------------------------------------------------------
do_status() {
  echo ""
  log "=== bitcoind ==="
  bitcoin_cli getblockchaininfo 2>/dev/null | grep -E '"chain"|"blocks"' || warn "not running"

  echo ""
  log "=== Docker containers ==="
  if [ -f "$MEMPOOL_DIR/docker-compose.yml" ]; then
    cd "$MEMPOOL_DIR"
    $DC ps 2>/dev/null || warn "not running"
  else
    warn "not set up"
  fi

  echo ""
  log "=== mempool API ==="
  curl -sf http://localhost:8080/api/v1/backend-info 2>/dev/null && echo "" || warn "not responding"
}

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------
case "${1:-start}" in
  start)
    ensure_docker
    ensure_colima
    ensure_bitcoind
    start_bitcoind
    setup_wallet_and_mine
    write_docker_compose
    start_mempool
    ;;
  stop)
    do_stop
    ;;
  mine)
    do_mine "${2:-1}"
    ;;
  status)
    do_status
    ;;
  *)
    echo "Usage: $0 {start|stop|mine [N]|status}"
    exit 1
    ;;
esac
