# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build and Test

GHC 9.6.x is required. The system GHC is not in PATH — always pass the compiler explicitly:

```bash
# Build (minimal — no optional backends)
~/.ghcup/bin/cabal build --with-compiler=~/.ghcup/bin/ghc-9.6

# Build with all backends enabled
~/.ghcup/bin/cabal build --with-compiler=~/.ghcup/bin/ghc-9.6 \
  -f+with-hint -f+with-exactprint -f+with-oracle -f+with-sbv -f+with-liquid

# Run all tests
~/.ghcup/bin/cabal test --with-compiler=~/.ghcup/bin/ghc-9.6 --test-show-details=direct

# Run a single test by pattern
~/.ghcup/bin/cabal test --with-compiler=~/.ghcup/bin/ghc-9.6 \
  --test-options="-p 'pattern here'"

# Run the CLI
~/.ghcup/bin/cabal run dgm --with-compiler=~/.ghcup/bin/ghc-9.6 -- --self-mod 1
~/.ghcup/bin/cabal run dgm --with-compiler=~/.ghcup/bin/ghc-9.6 -- --goal 'improve rewriting' --evolve 5

# Supervisor loop (generation track)
OPENROUTER_API_KEY=sk-... scripts/dgm-evolve.sh [max-generations]
```

The `OPENROUTER_API_KEY` is read from `.env` at repo root (gitignored) or from the environment.

## Optional Build Flags

All flags default to `False` except `with-sqlite` (defaults `True`).

| Flag | CPP guard | What it enables |
|------|-----------|----------------|
| `with-sqlite` | `WITH_SQLITE` | SQLite persistence for the archive (on by default) |
| `with-hint` | `WITH_HINT` | GHC hint interpreter (Futamura/hint self-mod track) |
| `with-exactprint` | `WITH_EXACTPRINT` | Real Haskell AST via `ghc-exactprint` + `ghc-paths` |
| `with-sbv` | `WITH_SBV` | SBV/Z3 SMT equivalence checker (Z3 at `/opt/homebrew/bin/z3`) |
| `with-liquid` | `WITH_LIQUID` | LiquidHaskell 0.9.6.3.1 pre-flight subprocess |
| `with-oracle` | `WITH_ORACLE` | OpenRouter LLM mutation proposer |

Test modules guard their flag-specific tests with the same CPP symbols, so 166 tests pass on the default build.

## Architecture

### Two self-modification tracks

**Generation track** (`DGM.SelfMod` + `DGM.SelfCompile`): writes mutated source to disk → `cabal test` subprocess → on pass, `git commit` → `exitWith (ExitFailure 42)`. The supervisor script (`scripts/dgm-evolve.sh`) detects exit 42 and restarts the binary as generation N+1, incrementing `$GENERATION`.

**Hint track** (`DGM.HintBridge`): evaluates mutated `ExprF` rewrite rules inside the running process via the GHC `hint` interpreter. New rules are appended to a `TVar [DynamicRule]` and take effect immediately without any restart. This is the Futamura projection: the interpreter boundary makes code = data.

### Cycle orchestration (`DGM.Cycle`)

`runCycleN` alternates between two phases controlled by `ccSelfModRateLimit` (default 10):
- **`ExprPhase`** — six-step Zero Data Cycle on the in-memory `ExprF` AST. Every 10 `ExprPhase` cycles triggers one `SelfModPhase`.
- **`SelfModPhase`** — full source-mutation pipeline: Oracle proposes diff → LiquidHaskell pre-flight → SBV equivalence → `cabal test` → commit or rollback.

`CycleConfig` holds all runtime state: `AgentState`, the `ArchiveHandle`, `ccDynamicRules TVar`, `ccHintEnv`, and `ccEvolutionGoal` (populated by `--goal`).

### Type-level safety (`DGM.Types` + `DGM.SafetyKernel`)

Every agent action is a `Command (l :: SafetyLevel)` GADT with three tiers:
- **`Safe`** — pure / read-only, no transaction needed
- **`Critical`** — mutates internal state, requires STM rollback snapshot
- **`Existential`** — file-system writes or process termination, requires a `QuorumProof`

`DGM.SafetyKernel` and `DGM.Types` are in `safeguardedModules` — the `SelfModPhase` pipeline rejects mutations targeting these files without an elevated quorum proof.

### Data flow

```
--goal TEXT  →  DGM.NatLang (EvolutionGoal)  →  applyGoal filters source candidates
                                                          │
DGM.Oracle (OpenRouter LLM)  ←────────────────────────────┘
     │  HsMutation diff
     ▼
DGM.Liquid  →  LiquidResult (SAFE / UNSAFE)
     │
DGM.Verification (SBV/Z3)  →  VerificationResult (Verified / Falsifiable / VTimeout)
     │  counter-examples fed back to ExprPhase as endogenous tasks
     ▼
DGM.SelfCompile  →  cabal test subprocess  →  CompileResult (pass/fail)
     │
DGM.Reversal  →  Invertible Text txn  →  rollback if fail
     │
DGM.Archive (SQLite)  →  ArchiveEntry (preserves failures as stepping-stones)
     │
exit 42  →  supervisor restarts at generation N+1
```

### Key module relationships

- `DGM.Types` — all shared types; import this first, never add circular deps here
- `DGM.AST` — `ExprF` base functor + `Fix`, `cata`, `ana`, `hylo` morphisms; no external recursion-schemes dependency
- `DGM.Rewriting` — static `ExprF` rewrite rules + `DynamicRule` type consumed by hint track
- `DGM.Archive` — `ArchiveHandle` abstraction over `InMemory` and `SQLiteBacked` backends; `addEntry` / `getBest` / `computeStats`
- `DGM.SelfMod` — `discoverSources`, `proposeSelfMutations`, `rankMutations`, `writeMutation`, `commitMutation`
- `DGM.ModGraph` — penalises mutations to high-dependency modules (e.g. `Types.hs`); `removesExportedName` rejects breaking changes
- `DGM.OracleHandle` — Bluefin-style scoped handle; proves at the call site that Oracle IO is deliberate

### `AgentState` fields

All mutable fields are `TVar`s for STM rollback:
- `stateArchive` — in-memory list shadowing the SQLite backend
- `stateCurrentAST` — the live `ExprF` tree
- `stateTypesHash` — SHA-256 of `Types.hs` at cycle start; change detection gate
- `stateSbvQueue` — `CounterExample`s from SBV, drained as endogenous `ExprTask`s
- `stateRepoRoot` — used by `SafetyKernel.checkPathWhitelist` to reject writes outside the repo

## Beads Issue Tracking

This project uses `bd` (beads) for task tracking backed by Dolt SQL. All work beads must be created from the rig directory (`gt/SIS/`) to route to the rig database — not from the project root (which would write to `.beads/`). Use `bd ready`, `bd create`, `bd update`, `bd close`.
