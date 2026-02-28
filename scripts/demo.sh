#!/usr/bin/env bash
# scripts/demo.sh — Annotated demo of the Darwin Gödel Machine full loop.
#
# Runs without OPENROUTER_API_KEY (falls back to heuristic mutations so the
# demo always succeeds in a clean repo checkout).
#
# What this script shows:
#   Phase 1 — 3 ExprPhase (Zero Data) cycles
#   Phase 2 — 1 SelfModPhase cycle (proposes + attempts a source mutation)
#   Phase 3 — archive statistics summary
#
# Exit codes:
#   0  — demo completed (exit-42 from SelfMod is caught and treated as success)
#   1  — build failed or binary not found

set -euo pipefail

# ─────────────────────────────────────────────────────────────────────────────
# Locate the dgm binary (prefer pre-built, fall back to cabal run)
# ─────────────────────────────────────────────────────────────────────────────

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO_ROOT"

DGM_BIN=""

# Check for a pre-built binary in the cabal dist directory.
PREBUILT="$(find dist-newstyle -name "dgm" -type f 2>/dev/null | head -1 || true)"
if [[ -n "$PREBUILT" && -x "$PREBUILT" ]]; then
  DGM_BIN="$PREBUILT"
fi

# If no pre-built binary, attempt a quick build (silent, redirect output).
if [[ -z "$DGM_BIN" ]]; then
  echo "# [demo] No pre-built binary found; building dgm (this may take a moment)..."
  if ! cabal build exe:dgm 2>/dev/null; then
    echo "# [demo] Build failed. Please run 'cabal build' manually and re-run the demo."
    exit 1
  fi
  PREBUILT="$(find dist-newstyle -name "dgm" -type f 2>/dev/null | head -1 || true)"
  if [[ -n "$PREBUILT" && -x "$PREBUILT" ]]; then
    DGM_BIN="$PREBUILT"
  fi
fi

# Absolute fallback: use 'cabal run' via a wrapper function.
if [[ -z "$DGM_BIN" ]]; then
  echo "# [demo] Could not locate dgm binary; using 'cabal run dgm --'."
  dgm() { cabal run dgm --quiet -- "$@"; }
else
  dgm() { "$DGM_BIN" "$@"; }
fi

# ─────────────────────────────────────────────────────────────────────────────
# Phase 1 — Zero Data Cycles (ExprPhase)
# ─────────────────────────────────────────────────────────────────────────────

echo ""
echo "╔══════════════════════════════════════════════════════════════╗"
echo "║  Darwin Gödel Machine — Annotated Demo                      ║"
echo "╚══════════════════════════════════════════════════════════════╝"
echo ""
echo "# Phase 1: ExprPhase — 3 Zero Data cycles"
echo "#"
echo "# The system proposes expression mutations, verifies them with"
echo "# the Gödel Gate (SBV/Z3 stub), tests them in the sandbox, and"
echo "# archives the results.  No source files are modified."
echo ""

dgm 3 || true

echo ""
echo "# Phase 1 complete.  Archive now contains expression-level entries."

# ─────────────────────────────────────────────────────────────────────────────
# Phase 2 — SelfModPhase (source mutation)
# ─────────────────────────────────────────────────────────────────────────────

echo ""
echo "# Phase 2: SelfModPhase — 1 source self-modification cycle"
echo "#"
echo "# The system discovers Haskell source files, proposes a mutation"
echo "# (heuristic, no oracle key needed), runs 'cabal test', and"
echo "# commits on success.  Failures are rolled back via invertible"
echo "# transactions (DGM.Reversal).  If a commit succeeds the process"
echo "# exits with code 42 (the biological restart signal)."
echo ""

# Exit 42 means "committed successfully — restart me".
# We catch it and continue the demo.
dgm --self-mod 1 && SELFMOD_EXIT=0 || SELFMOD_EXIT=$?

if [[ "$SELFMOD_EXIT" -eq 42 ]]; then
  echo ""
  echo "# Exit 42 received: a mutation was committed successfully."
  echo "# In production the supervisor would restart dgm at generation N+1."
elif [[ "$SELFMOD_EXIT" -eq 0 ]]; then
  echo ""
  echo "# SelfMod cycle complete (no improvement found this round)."
else
  echo ""
  echo "# SelfMod cycle finished with exit $SELFMOD_EXIT (non-fatal in demo mode)."
fi

# ─────────────────────────────────────────────────────────────────────────────
# Phase 3 — Archive statistics
# ─────────────────────────────────────────────────────────────────────────────

echo ""
echo "# Phase 3: Archive statistics"
echo "#"
echo "# The expanding archive preserves every attempt — including"
echo "# rejected mutations ('stepping stones').  Statistics are printed"
echo "# at the end of every dgm run; the numbers above already show them."
echo "#"
echo "# To inspect the archive directly:"
echo "#   sqlite3 dgm-archive.db 'SELECT id, score, passed FROM entries ORDER BY score DESC LIMIT 10;'"
echo ""

echo "╔══════════════════════════════════════════════════════════════╗"
echo "║  Demo complete.  See README.md for next steps.              ║"
echo "╚══════════════════════════════════════════════════════════════╝"
echo ""

exit 0
