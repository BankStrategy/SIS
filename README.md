# SIS — Self-Improving System

A Haskell system that modifies its own source code, compiles itself, runs its
own test suite, and commits the survivors to git.  Built on the Darwin Godel
Machine and Absolute Zero paradigms.  No human in the loop.

## What It Actually Does

SIS reads its own `.hs` source files, proposes mutations (via an LLM agent or
heuristic rules), applies them as unified diffs, runs `cabal test` to validate,
rolls back failures, and commits successes.  On commit it exits with code 42;
a supervisor script restarts the binary as generation N+1.  The archive
(SQLite-backed) persists both successes and failures across generations — failed
mutations are kept as stepping-stones for future breakthroughs.

The system has successfully evolved itself across multiple generations,
including finding and fixing real bugs in its own test suite.

### Three operational tracks

**ExprPhase** — Pure in-memory expression optimization.  A fixed-point AST
(`ExprF`) is mutated via rewriting rules (constant folding, identity
elimination, commutativity, eta reduction), verified, and archived.  No
external dependencies.  Runs fast, always works.

**SelfModPhase** — Real Haskell source self-modification.  The system discovers
its own source files, builds semantic context (function signatures, complexity
metrics, test coverage), sends enriched prompts to an LLM, parses the returned
unified diffs, applies them to disk, runs `cabal test`, and either keeps or
reverts.  Module dependency analysis prevents breaking changes to high-fanout
modules.

**Agentic Oracle** — Multi-turn tool-use loop.  An LLM (currently Gemini 3.1
Pro via OpenRouter) gets tools to explore the codebase: `read_file`,
`list_files`, `search_code`, `get_module_info`, `run_build`, `run_tests`,
`try_change` (dry-run), and `submit_change` (live apply).  The agent explores
for as many turns as it needs, iterates on failing diffs, and calls `finish`
when done.  In tested runs, the agent has completed 47-turn sessions with 6
successful live code changes, including finding a real bug (test suite wiring)
that humans missed.

---

## Quick Start

```bash
# Build (GHC 9.6 required)
cabal build --with-compiler=~/.ghcup/bin/ghc-9.6

# Run expression optimization cycles
cabal run dgm -- 10

# Run self-modification (reads/writes own source)
cabal run dgm -- --self-mod 3

# Autonomous evolution with LLM oracle
OPENROUTER_API_KEY=sk-... cabal run dgm -- --evolve 5

# Goal-steered evolution
OPENROUTER_API_KEY=sk-... cabal run dgm -- --goal 'improve archive module' --evolve 5

# Supervisor loop (restarts on exit 42, increments generation)
OPENROUTER_API_KEY=sk-... scripts/dgm-evolve.sh 10
```

---

## Architecture

```
  --goal "improve X"
        |
        v
  DGM.NatLang -----> filter source candidates by goal
        |
        v
  DGM.SelfMod -----> discover sources, rank by module graph
        |
        v
  DGM.AgentOracle -> multi-turn LLM agent with tools:
        |              read_file, search_code, try_change,
        |              submit_change, run_tests, finish
        v
  DGM.SelfCompile --> cabal test subprocess (120s timeout)
        |
      pass?
     /     \
   yes      no --> rollback via DGM.Reversal (invertible txn)
    |
    v
  DGM.Archive -----> SQLite persistence (successes + failures)
    |
    v
  git commit -------> exit 42
    |
    v
  supervisor restarts at generation N+1
```

Optional verification gates (when flags enabled):
- **DGM.Liquid** — LiquidHaskell pre-flight (refinement type checking)
- **DGM.Verification** — SBV/Z3 SMT equivalence proofs
- **DGM.HintBridge** — GHC hint interpreter for live expression evaluation

---

## Module Map

| Module | Role |
|--------|------|
| **DGM.Types** | GADT command types (`Safe`/`Critical`/`Existential`), `AgentState`, all shared types |
| **DGM.AST** | Fixed-point expression AST; `cata`, `ana`, `hylo` morphisms (no recursion-schemes dep) |
| **DGM.Rewriting** | 8 static rewrite rules + dynamic rule slot; drives ExprPhase mutations |
| **DGM.SafetyKernel** | Airlock kernel: STM rollback, SHA-256 quorum proofs, path whitelisting |
| **DGM.Archive** | SQLite-backed expanding memory; weighted sampling; preserves failures |
| **DGM.SelfMod** | Source discovery, mutation proposal/ranking, `writeMutation`, `commitMutation` |
| **DGM.SelfCompile** | `cabal test` subprocess driver; GHC error extraction; transactional rollback |
| **DGM.ModGraph** | Text-based module dependency graph; penalises high-fanout mutations |
| **DGM.AgentOracle** | Multi-turn agentic tool-use loop; 10 tools; raw message replay for Gemini 3.x |
| **DGM.Oracle** | OpenRouter HTTP client; unified diff parsing; 3-pass hunk matching |
| **DGM.OracleHandle** | Bluefin-style capability token proving oracle IO is deliberate |
| **DGM.OracleContext** | `cabal build -Wall` parser; extracts GHC warnings as mutation hints |
| **DGM.SemanticContext** | Function extraction, complexity scoring, test coverage mapping |
| **DGM.NatLang** | Natural-language goal parsing; filters source candidates by relevance |
| **DGM.Cycle** | Orchestrator: alternates ExprPhase / SelfModPhase based on rate limiter |
| **DGM.Evolution** | Hylomorphic evolutionary driver (unfold hypothesis space, collapse with verification) |
| **DGM.AbsoluteZero** | Self-play task generation at the Pareto frontier of capability vs difficulty |
| **DGM.Reversal** | Typed invertible transactions; `forward`/`backward` pair enforced by types |
| **DGM.HsAST** | `HsMutation` diff type; real AST via ghc-exactprint or text-based fallback |
| **DGM.Sandbox** | Bounded expression evaluator (timeout + depth limit); hint or pure fallback |
| **DGM.HintBridge** | GHC hint persistent interpreter; Futamura projection track |
| **DGM.RuleMiner** | Pattern mining from archive history; boosts successful mutation patterns |
| **DGM.Liquid** | LiquidHaskell subprocess; result parser (SAFE/UNSAFE/CRASH) |
| **DGM.Verification** | SBV/Z3 SMT solver; bounded test oracle fallback when Z3 unavailable |

---

## Build Flags

All optional.  The system is fully functional without any of them.

| Flag | Default | What it enables |
|------|---------|-----------------|
| `with-sqlite` | **On** | SQLite archive persistence (recommended) |
| `with-oracle` | Off | OpenRouter LLM oracle + agentic tool-use |
| `with-hint` | Off | GHC hint interpreter (Futamura track) |
| `with-exactprint` | Off | Real Haskell AST via ghc-exactprint |
| `with-sbv` | Off | SBV/Z3 SMT equivalence checker |
| `with-liquid` | Off | LiquidHaskell pre-flight verification |

```bash
# Minimal (expression cycles + self-mod with heuristics)
cabal build

# With LLM agent
cabal build -f+with-oracle

# Everything
cabal build -f+with-hint -f+with-exactprint -f+with-oracle -f+with-sbv -f+with-liquid
```

Every optional feature degrades gracefully: when a flag is absent, the module
compiles with a stub that returns a descriptive error, and the pipeline
continues with the next verification stage.

---

## Tests

```bash
cabal test --test-show-details=direct
```

72 tests on default build (including SQLite backend tests).  With all flags
enabled, 250+.  Test coverage spans every module: AST morphisms, rewriting
rules, safety kernel proofs, archive persistence, self-compilation scoring,
module graph analysis, commit mutation, and agentic oracle JSON parsing.

---

## Safety Model

The type system enforces three tiers of operations:

- **`Safe`** — Pure / read-only.  No transaction needed.  Executed directly.
- **`Critical`** — Mutates internal state.  Requires STM rollback snapshot.
  On failure, the snapshot restores the previous state atomically.
- **`Existential`** — File system writes or process termination.  Structurally
  requires a `QuorumProof` (SHA-256 hash with timestamp and expiry).  You
  cannot construct an `Existential` command without a valid proof — the GADT
  makes it a type error.

`DGM.Types` and `DGM.SafetyKernel` are in `safeguardedModules`: the
self-modification pipeline rejects mutations targeting these files unless
an elevated quorum proof is provided.

---

## Generation Protocol

A running Haskell process cannot hot-swap its own compiled code.  SIS uses
a biological generation model:

1. The binary proposes and tests a mutation
2. On success: `git commit`, then `exitWith (ExitFailure 42)`
3. The supervisor script (`scripts/dgm-evolve.sh`) detects exit 42
4. It increments `GENERATION`, rebuilds, and re-executes the binary
5. The new generation inherits the improved source tree and the SQLite archive

Exit 42 is reserved for "I improved — restart me".  Exit 0 means "nothing to
do".  Any other non-zero code propagates as an error.

---

## How the Agent Oracle Works

When `--self-mod` or `--evolve` is used with the oracle flag enabled, SIS
spawns a multi-turn LLM agent for each candidate mutation.  The agent:

1. Receives a task prompt with semantic context (function signatures, complexity,
   test coverage, GHC warnings)
2. Explores the codebase using `read_file`, `list_files`, `search_code`,
   `get_module_info`
3. Tests hypotheses with `try_change` (dry-run: apply, build, test, revert)
4. Commits changes with `submit_change` (apply, build, test — keep if pass,
   revert if fail)
5. Iterates: re-reads modified files, adjusts diffs, retries
6. Calls `finish` when the task is complete

The agent preserves raw assistant message JSON across turns, which is required
for Gemini 3.x models that embed `thought_signature` and `reasoning_details`
in their responses.

---

## Known Limitations

- **Diff matching is imperfect**: ~30-40% of LLM-generated diffs fail to apply
  because context lines don't exactly match the source.  The agent recovers by
  re-reading and retrying, but it wastes turns.
- **Gemini thought signatures**: Multi-turn sessions with Gemini 3.1 Pro
  occasionally fail around turn 15 due to signature validation.  The system
  falls back to a fresh session automatically.
- **Single-file mutations**: Each `submit_change` targets one file.  Coordinated
  multi-file refactors require multiple sequential submissions.
- **SBV/LH are optional**: Without Z3 or LiquidHaskell installed, formal
  verification is skipped.  The test suite remains the primary validation gate.
- **Local archive only**: The SQLite file doesn't transfer across forks or
  machines.  No distributed evolution yet.

---

## Design Decisions

**Why invertible transactions?**  Every source mutation must be fully
reversible.  `DGM.Reversal` encodes forward and backward transformations as a
typed pair — the type system refuses to commit without an inverse.

**Why exit 42?**  A process cannot hot-swap its own compiled code.  Exit 42
delegates restart to the supervisor, which rebuilds and re-executes.  Simplest
mechanism that works with GHC's separate compilation model.

**Why preserve failures in the archive?**  The optimization landscape of
self-improvement is non-convex.  A dead-end today may be a necessary
stepping-stone to a future breakthrough.  The archive is append-only by design.

**Why raw message replay?**  Gemini 3.x models embed cryptographic thought
signatures in their responses.  Reconstructing assistant messages from parsed
data strips these signatures, causing subsequent turns to fail.  SIS captures
the raw JSON and replays it verbatim.
