#!/usr/bin/env bash
# dgm-evolve.sh — Supervisor loop for the Darwin Gödel Machine.
#
# Restarts the DGM binary whenever it exits with code 42 (successful
# self-modification). Stops on any other non-zero exit (error) after
# MAX_CONSECUTIVE_ERRORS consecutive failures.
#
# Usage: ./scripts/dgm-evolve.sh [MAX_GEN]
#   MAX_GEN — maximum number of generations to evolve (default: 100)
#
# Environment:
#   GENERATION  — starting generation number (default: 0, auto-incremented)
#   CABAL_BIN   — path to cabal executable (optional; build check skipped if absent)

# ── Configuration ────────────────────────────────────────────────────────────

MAX_GEN="${1:-100}"
LOG_FILE="dgm-evolve.log"
MAX_CONSECUTIVE_ERRORS=5
export GENERATION="${GENERATION:-0}"

# ── Lock: prevent two supervisor instances (PID file, handles stale locks) ───

LOCK_FILE="/tmp/dgm-evolve.pid"
if [ -f "$LOCK_FILE" ]; then
  OLD_PID=$(cat "$LOCK_FILE" 2>/dev/null)
  if [ -n "$OLD_PID" ] && kill -0 "$OLD_PID" 2>/dev/null; then
    echo "[dgm-evolve] Already running (PID $OLD_PID). Exiting." >&2
    exit 1
  else
    rm -f "$LOCK_FILE"
  fi
fi
echo $$ > "$LOCK_FILE"
trap 'rm -f "$LOCK_FILE"' EXIT

# ── Logging helper ───────────────────────────────────────────────────────────

log() {
  local msg="[dgm-evolve $(date '+%Y-%m-%d %H:%M:%S')] $*"
  echo "$msg"
  echo "$msg" >> "$LOG_FILE"
}

# ── Detect cabal and run pre-flight build check (optional) ───────────────────

if [ -z "${CABAL_BIN:-}" ]; then
  if [ -x "${HOME:-}/.ghcup/bin/cabal" ]; then
    CABAL_BIN="$HOME/.ghcup/bin/cabal"
  elif command -v cabal &>/dev/null; then
    CABAL_BIN="cabal"
  fi
fi

if [ -n "${CABAL_BIN:-}" ]; then
  log "Pre-flight: building DGM binary..."
  if "$CABAL_BIN" build 2>&1 | tee -a "$LOG_FILE" | tail -3; then
    log "Build OK."
  else
    log "ERROR: cabal build failed. Aborting supervisor."
    exit 1
  fi
fi

# ── Locate DGM binary (PATH preferred; dist-newstyle as fallback) ────────────
#
# Priority: (1) $DGM_BIN env override, (2) 'dgm' on PATH, (3) dist-newstyle.
# This lets tests inject a mock 'dgm' via PATH without interference.

if [ -z "${DGM_BIN:-}" ]; then
  if command -v dgm &>/dev/null; then
    DGM_BIN="dgm"
  else
    DGM_BIN=$(find dist-newstyle -name "dgm" -type f 2>/dev/null | head -1)
  fi
fi
if [ -z "${DGM_BIN:-}" ]; then
  log "ERROR: dgm binary not found. Build with cabal or put dgm on PATH."
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
  "$DGM_BIN" --self-mod 1 2>&1 | tee -a "$LOG_FILE"
  EXIT_CODE=${PIPESTATUS[0]}

  case $EXIT_CODE in
    42)
      gen=$((gen + 1))
      consecutive_errors=0
      log "Generation $gen — evolved successfully."
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
