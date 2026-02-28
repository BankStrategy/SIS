#!/usr/bin/env bash
# dgm-evolve.sh — Supervisor loop for the Darwin Gödel Machine.
#
# Restarts the DGM binary whenever it exits with code 42 (successful
# self-modification). Stops on any other non-zero exit (error) after
# MAX_CONSECUTIVE_ERRORS consecutive failures.
#
# Usage: ./scripts/dgm-evolve.sh [--max-gen N] [extra flags passed to dgm]
#
# Environment:
#   GENERATION  — starting generation number (default: 0, auto-incremented)
#   CABAL_BIN   — path to cabal executable (default: auto-detected)

set -euo pipefail

# ── Configuration ────────────────────────────────────────────────────────────

MAX_GEN="${1:-100}"
LOCK_FILE="/tmp/dgm-evolve.lock"
LOG_FILE="dgm-evolve.log"
MAX_CONSECUTIVE_ERRORS=5
export GENERATION="${GENERATION:-0}"

# Detect cabal binary
if [ -z "${CABAL_BIN:-}" ]; then
  if [ -x "$HOME/.ghcup/bin/cabal" ]; then
    CABAL_BIN="$HOME/.ghcup/bin/cabal"
  elif command -v cabal &>/dev/null; then
    CABAL_BIN="cabal"
  else
    echo "[dgm-evolve] ERROR: cabal not found. Set CABAL_BIN or install via ghcup." >&2
    exit 1
  fi
fi

# ── Lock: prevent two supervisor instances ───────────────────────────────────

exec 9>"$LOCK_FILE"
if ! flock -n 9; then
  echo "[dgm-evolve] Already running (lock: $LOCK_FILE). Exiting." >&2
  exit 1
fi

# ── Logging helper ───────────────────────────────────────────────────────────

log() {
  local msg="[dgm-evolve $(date '+%Y-%m-%d %H:%M:%S')] $*"
  echo "$msg"
  echo "$msg" >> "$LOG_FILE"
}

# ── Pre-flight: build check ──────────────────────────────────────────────────

log "Pre-flight: building DGM binary..."
if ! "$CABAL_BIN" build 2>&1 | tee -a "$LOG_FILE" | tail -3; then
  log "ERROR: cabal build failed. Aborting supervisor."
  exit 1
fi
log "Build OK."

# Locate the built binary
DGM_BIN=$(find dist-newstyle -name "dgm" -type f 2>/dev/null | head -1)
if [ -z "$DGM_BIN" ]; then
  log "ERROR: dgm binary not found in dist-newstyle after build."
  exit 1
fi
log "Using binary: $DGM_BIN"

# ── Main supervisor loop ─────────────────────────────────────────────────────

consecutive_errors=0
gen=$GENERATION

log "Starting evolution from generation $gen (max: $MAX_GEN)"

while [ "$gen" -lt "$MAX_GEN" ]; do
  log "Generation $gen — running self-mod cycle..."

  export GENERATION=$gen
  set +e
  "$DGM_BIN" --self-mod 1 2>&1 | tee -a "$LOG_FILE"
  EXIT_CODE=$?
  set -e

  case $EXIT_CODE in
    42)
      gen=$((gen + 1))
      consecutive_errors=0
      log "Improved! Now at generation $gen."
      ;;
    0)
      consecutive_errors=0
      log "No improvement found (exit 0). Evolution complete at generation $gen."
      break
      ;;
    *)
      consecutive_errors=$((consecutive_errors + 1))
      log "ERROR: exit code $EXIT_CODE (consecutive errors: $consecutive_errors / $MAX_CONSECUTIVE_ERRORS)"
      if [ "$consecutive_errors" -ge "$MAX_CONSECUTIVE_ERRORS" ]; then
        log "Too many consecutive errors. Aborting to protect git history."
        exit 1
      fi
      log "Retrying after error..."
      ;;
  esac
done

log "Supervisor complete. Final generation: $gen"
