#!/usr/bin/env bash
# dgm-evolve.sh — DGM generation supervisor loop.
#
# Runs 'dgm --self-mod 1' in a loop, interpreting exit codes:
#   exit 42 → successful evolution  (increment GENERATION, loop)
#   exit 0  → no improvement found (loop again)
#   other   → error (stop and propagate the exit code)
#
# The GENERATION environment variable is exported before each restart so
# that 'dgm' can print the current generation number on startup.
#
# Usage: scripts/dgm-evolve.sh [max-generations]
#   max-generations  Maximum number of successful evolutions before stopping.
#                    0 (default) means unlimited.
#
# Examples:
#   scripts/dgm-evolve.sh          # run forever
#   scripts/dgm-evolve.sh 10       # stop after 10 successful generations

MAX_GENS="${1:-0}"
GENERATION="${GENERATION:-0}"
export GENERATION

echo "[dgm-evolve] Starting supervisor (max_gens=${MAX_GENS})"

while true; do
  # Honor the generation limit (0 = unlimited).
  if [ "${MAX_GENS}" -gt 0 ] && [ "${GENERATION}" -ge "${MAX_GENS}" ]; then
    echo "[dgm-evolve] Max generations (${MAX_GENS}) reached. Stopping."
    exit 0
  fi

  # Run one self-mod cycle and capture exit code without triggering 'set -e'.
  dgm --self-mod 1
  EC=$?

  if [ "$EC" -eq 42 ]; then
    # Successful evolution: increment generation counter and loop.
    GENERATION=$((GENERATION + 1))
    export GENERATION
    echo "[dgm-evolve] Generation ${GENERATION} evolved successfully."
  elif [ "$EC" -eq 0 ]; then
    # No improvement found: loop and try again.
    echo "[dgm-evolve] No improvement found. Looping..."
  else
    # Unexpected error: stop and propagate.
    echo "[dgm-evolve] ERROR: dgm exited ${EC}. Stopping." >&2
    exit "$EC"
  fi
done
