# Darwin Gödel Machine — Haskell MVP

A self-improving Haskell system that synthesises the Darwin Gödel Machine (DGM)
and Absolute Zero paradigms into a single autonomous research artifact.  It
proposes mutations to its own source code, verifies them formally, tests them
empirically, and commits the survivors to git — all without human intervention.

---

## What SIS Is

SIS (**S**elf-**I**mproving **S**ystem) is an honest prototype, not a
production system.  It implements the core feedback loop of the DGM paper: a
proposer generates candidate self-modifications, a verifier screens them, a
sandbox tests them, and a persistent archive remembers both successes and
"stepping-stone" failures.  The biological generation model (exit-42 restart
protocol) lets the system accumulate improvements across process boundaries.
The Absolute Zero self-play inner loop drives exploration without any external
training data.  LiquidHaskell and SBV add lightweight formal pre-flight checks
before every mutation commit.  The system is genuinely autonomous but its
intelligence is still mostly heuristic — the LLM oracle (OpenRouter) upgrades
proposal quality when an API key is available.

---

## Architecture

```
Natural-language goal (--goal)
        │
        ▼
  DGM.NatLang ──► applyGoal ──► filtered source files
        │
        ▼
  DGM.SelfMod ──► discoverSources ──► proposeSelfMutations
        │
        ▼
  DGM.Oracle (LLM) ──► HsMutation diff
        │
        ▼
  DGM.Liquid (LiquidHaskell) ──► pre-flight refinement check
        │
        ▼
  DGM.Verification (SBV/Z3) ──► SMT equivalence check
        │
        ▼
  DGM.SelfCompile ──► cabal test (compile + 153 tests)
        │
      pass?
     /     \
   yes      no ──► rollback (DGM.Reversal invertible txn)
    │
    ▼
  DGM.Archive ──► git commit (exit-42 if committed)
        │
        ▼
  supervisor restarts at next generation
```

---

## Module Map

| Module | Description |
|--------|-------------|
| `DGM` | Top-level re-export aggregator |
| `DGM.Types` | Core GADT types: `AgentState`, `Mutation`, `AuditLog`, `Value` |
| `DGM.AST` | GADT expression AST with cata/ana/hylo recursion schemes |
| `DGM.HsAST` | Haskell source AST wrapper and `HsMutation` diff type |
| `DGM.Rewriting` | Expression rewriting rules (constant-fold, identity-elim, …) |
| `DGM.Verification` | SBV/Z3 SMT equivalence checker with fallback stub |
| `DGM.Sandbox` | Mueval-bounded expression evaluator |
| `DGM.HintBridge` | GHC hint API bridge for dynamic Haskell evaluation |
| `DGM.Archive` | Expanding archive (SQLite-backed); preserves failed "stepping-stone" failures |
| `DGM.Evolution` | Anamorphic unfold → catamorphic fold evolutionary driver |
| `DGM.AbsoluteZero` | Self-play proposer: generates tasks at the Pareto frontier |
| `DGM.SafetyKernel` | GADT safety classification and quorum-proof transaction planner |
| `DGM.Cycle` | Six-step Zero Data Cycle orchestrator (ExprPhase + SelfModPhase) |
| `DGM.SelfMod` | Source discovery, mutation proposal, ranking, and git commit |
| `DGM.SelfCompile` | Cabal test runner with invertible transaction rollback |
| `DGM.ModGraph` | Module dependency graph for mutation prioritisation |
| `DGM.Reversal` | Typed invertible transactions (Bluefin-style forward/backward pairs) |
| `DGM.Liquid` | LiquidHaskell subprocess driver with result parser |
| `DGM.Oracle` | OpenRouter LLM mutation proposer (requires `OPENROUTER_API_KEY`) |
| `DGM.OracleHandle` | Scoped oracle handle with resource acquisition |
| `DGM.NatLang` | Natural-language goal parser and source-file filter (`--goal`) |

---

## Two Self-Modification Tracks

**Hint track (Futamura projection)**
Uses the GHC `hint` library to evaluate mutated expressions inside the running
process without forking.  Enabled with `-f+with-hint`.  Suitable for
ExprPhase Zero Data cycles where no file system writes are needed.

**Generation track (biological)**
Writes mutated Haskell source to disk, runs `cabal test` as a subprocess, and
commits on success.  On commit, exits with code 42 to signal the supervisor
loop (`dgm-evolve.sh`) to restart the process as generation N+1.  Each
generation inherits the accumulated archive and the improved source tree.

---

## Quick Start

```bash
# Full build with all optional backends
cabal build -f+with-hint -f+with-exactprint -f+with-oracle -f+with-liquid

# Minimal build (no external tools required)
cabal build

# Run 5 Zero Data expression cycles
cabal run dgm -- 5

# Run 1 SelfMod cycle (proposes + commits a source mutation)
OPENROUTER_API_KEY=sk-... cabal run dgm -- --self-mod 1

# Run up to 10 autonomous generations (loops until no improvement)
OPENROUTER_API_KEY=sk-... cabal run dgm -- --evolve 10

# Steer mutations toward a specific module with natural language
OPENROUTER_API_KEY=sk-... cabal run dgm -- --goal 'improve rewriting rules' --evolve 5

# Run the annotated demo (no API key needed)
bash scripts/demo.sh
```

---

## Running Tests

```bash
cabal test
```

166 tests across all modules.  All pass without any optional build flags or
external tools.  The SQLite, hint, exactprint, SBV, and LiquidHaskell test
groups are gated behind their respective CPP flags.

---

## What It Cannot Yet Do

- **SBV coverage is limited**: the SMT verifier only handles algebraic
  functions over integers/booleans.  Higher-order functions, IO, and
  concurrency are outside scope.
- **LiquidHaskell annotations are sparse**: most modules have no refinement
  types, so the LH pre-flight is a pass-through stub on the default build.
- **Oracle mutations are coarse**: the LLM generates unified diffs without
  type-checking; many proposals fail the cabal test gate and are rolled back.
- **No cross-module mutation**: the SelfMod cycle proposes one mutation to one
  file at a time; coordinated multi-file refactors are not supported.
- **Archive does not transfer across forks**: the SQLite file is local; there
  is no distributed ledger for multi-agent evolution.

---

## Architecture Decisions

**Why Bluefin-style invertible transactions?**
Every source mutation must be fully reversible.  `DGM.Reversal` encodes
forward and backward transformations as a typed pair, making rollback
impossible to forget: the type system refuses to commit without an inverse.

**Why Invertible over snapshot?**
Snapshots copy entire files; diffs are O(change size) and self-documenting.
More importantly, the archive preserves the *reason* for each reversal — the
inverse is named and logged, not just a byte dump.

**Why exit-42?**
A running process cannot hot-swap its own compiled code.  Exit-42 delegates
the restart to the supervisor shell script, which sets `GENERATION=N+1` and
re-executes the binary.  This is the simplest mechanism that survives across
GHC's separate compilation model without requiring runtime hot-loading.

**Why not exit 0 on commit?**
Exit 0 would be indistinguishable from "ran fine, nothing happened".  Exit 42
is an explicit protocol signal reserved for "I improved — restart me at the
next generation".  The supervisor only loops on 42; any other non-zero code
propagates upward as an error.
