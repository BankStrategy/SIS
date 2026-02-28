# SELFMOD.md — SIS Self-Modification Architecture

> **Classification:** Flagship architectural document
> **Date:** 2026-02-27
> **Scope:** Full roadmap for transforming SIS from a toy ExprF language into a system
> that evolves real Haskell source code, including its own modules.

---

## Executive Summary

SIS currently implements the Darwin Gödel Machine + Absolute Zero paradigms against a
deliberately simplified expression language (`ExprF`). The evolutionary machinery —
hylomorphisms, term rewriting, archive, self-play, safety kernel — is sound and
fully tested. The critical gap: **mutations apply to toy expressions, not to the
actual `.hs` files that constitute the agent itself**.

This document specifies all architectural work required to close that gap. The end
state is a system that can:

1. Parse its own Haskell source modules into a real GHC syntax tree
2. Apply rewrite rules to that AST
3. Write the modified source back to disk
4. Compile the result with `cabal test`
5. Commit the mutation if tests pass, discard if they fail
6. Reason across module boundaries
7. Generate Absolute Zero tasks *about its own code* — "can it add a new rewrite
   rule?", "can it improve its own scoring function?"

This is recursive self-improvement in the strict sense: the system modifies the
binary that runs it.

---

## The Chicken-and-Egg Problem

Before any other section: **the bootstrapping paradox must be named and solved**.

> *To evolve Haskell code, the system needs to reason about Haskell ASTs.
> But the system currently reasons in ExprF.
> To replace ExprF with HsExpr, the system must already be able to modify itself.
> But it cannot modify itself until ExprF is replaced with HsExpr.*

This is not a theoretical obstacle — it is the central engineering challenge of any
self-referential system. The solution is a staged bootstrap with clear invariants at
each stage:

### Bootstrap Invariant Chain

```
Stage 0: ExprF system (current state)
  ↓ Foundation work (PLAN.md phases 1–6) runs entirely inside ExprF
  ↓
Stage 1: hint wired (PLAN.md Phase 3 complete)
  ↓ System can evaluate arbitrary Text Haskell via GHC-API
  ↓ First capability: run a Haskell snippet that reads a file and parses it
  ↓
Stage 2: External parser (Phase B)
  ↓ System spawns a subprocess: `ghc-exactprint-parser` binary
  ↓ Input: path to a .hs file
  ↓ Output: JSON-serialised GHC ParsedSource
  ↓ System can now inspect own source without GHC-API living in-process
  ↓
Stage 3: In-process AST (Phase B complete)
  ↓ ghc-exactprint integrated as a library
  ↓ `DGM.HsAST` module wraps ParsedSource, provides mutation primitives
  ↓ ExprF still active; HsExpr is a second representation for self-referential work
  ↓
Stage 4: File system loop (Phase C)
  ↓ System reads own .hs source, produces HsExpr mutations, writes back
  ↓ QuorumProof required for every SystemWrite (already type-enforced)
  ↓ ExprF may still exist as the task DSL; HsExpr for self-modification
  ↓
Stage 5: Self-compilation (Phase D)
  ↓ System shells out to `cabal test`; interprets exit code + output
  ↓ Pass → archive mutation as successful evolution step
  ↓ Fail → archive as failed stepping stone, rollback SystemWrite
  ↓
Stage 6: ExprF retirement (optional, Phase B+)
  ↓ Once Stage 5 is proven stable, ExprF can be retired in favour of HsExpr
  ↓ OR kept as a lightweight DSL for task description (separate from self-mod AST)
```

The key insight: **the system never needs to modify itself to get to Stage 2**. The
external subprocess approach breaks the circular dependency. The system first gains the
*ability* to parse Haskell using external tooling, then progressively internalises
that capability.

---

## Phase 0: Foundation Prerequisites (from PLAN.md)

These phases MUST be complete before any self-modification work begins. They
provide the infrastructure the self-modification loop depends on.

### Phase 0.1 — SQLite Archive Persistence (PLAN.md Phase 1)

**Dependency:** Self-modification history must survive process restarts. A mutation
that fails today is a stepping stone for tomorrow. In-memory archive is wiped on
every restart.

**Status:** Stub (TVar list in `Archive.hs`)

**What changes:**
- `Archive.hs`: Add `ArchiveBackend` type, `openArchive`/`closeArchive`/`flushArchive`/`loadArchive`
- `Types.hs`: Move `ToJSON`/`FromJSON` instances (fix orphan warnings)
- `Cycle.hs`: Flush to SQLite after each cycle, load at startup
- `dgm.cabal`: Add `sqlite-simple >= 0.4`
- `app/Main.hs`: Default to `SQLiteBacked "dgm-archive.db"`

**Schema:**
```sql
CREATE TABLE IF NOT EXISTS archive_entries (
  id          TEXT PRIMARY KEY,
  code        TEXT NOT NULL,
  parent_id   TEXT,
  score       REAL NOT NULL,
  passed      INTEGER NOT NULL,
  generation  INTEGER NOT NULL,
  counter_ex  TEXT,
  mutation_type TEXT,
  mutation_target TEXT,
  source_hash TEXT        -- SHA-256 of the .hs source at time of mutation
);
```

The `source_hash` column is added beyond PLAN.md scope: it links archive entries to
the specific version of the source code that was mutated, enabling lineage tracking
once self-modification begins.

**Acceptance criteria:**
- `cabal run dgm -- 5 && cabal run dgm -- 5` — second run shows >0 pre-existing entries
- `dgm-archive.db` grows monotonically; entries survive restart
- 34 existing tests pass with `InMemory` backend

---

### Phase 0.2 — Recursive Rewriting (PLAN.md Phase 2)

**Dependency:** Root-only mutations produce very few candidates. Self-modification
requires the ability to target any subexpression or subterm within a module — a
specific function body, a specific `where` clause, a specific argument.

**Status:** Stub (generateMutations fires only at root in `Rewriting.hs`)

**What changes:**
- `Rewriting.hs`: Replace root-only `generateMutations` with paramorphism-based
  traversal using `subtermsWithCtx` (zipper-style: subterm + reconstruction function)

**Acceptance criteria:**
- `generateMutations` on `(2 + 3) * (x + 0)` finds both `(2+3) → 5` and `(x+0) → x`
- 35 tests pass (one added)

---

### Phase 0.3 — Proposer Difficulty Adaptation (PLAN.md Phase 5)

**Dependency:** The self-modification proposer must be able to escalate task
difficulty as the system's capabilities grow. A static difficulty proposer will
never generate tasks that challenge a mature self-modifying agent.

**Status:** Stub (proposer state not threaded through `runCycleN`)

**What changes:**
- `Cycle.hs`: Thread `TVar Proposer` through cycle loop; commit updated difficulty
  after each `runSelfPlayStep`
- `app/Main.hs`: Initialize `ccProposer` from `TVar` wrapping `defaultProposer`

**Acceptance criteria:**
- After 5 self-play steps at difficulty 0.2, `propCurrentDifficulty > 0.2`
- `cabal run dgm -- 20` shows monotonically increasing task difficulty

---

### Phase 0.4 — Cryptographic Quorum Proof (PLAN.md Phase 6)

**Dependency:** Self-modification writes to the file system. The `SystemWrite`
command is `Existential`-classified and requires a `QuorumProof`. The proof must be
cryptographically unforgeable — the system cannot hallucinate a valid hash.

**Status:** Stub (`simpleHash` is a trivial `mod 1000000`)

**What changes:**
- `SafetyKernel.hs`: Replace `simpleHash` with real SHA-256 via `cryptohash-sha256`
- `Types.hs`: Add `quorumTimestamp :: Int64` and `quorumOperation :: Text` to
  `QuorumProof` (timestamp window: ±300 seconds; operation binding prevents replay)
- `dgm.cabal`: Add `cryptohash-sha256 >= 0.11`, `base16-bytestring >= 1.0`

**Acceptance criteria:**
- Proof for "terminate" rejected when presented for "write:/etc/passwd"
- Proof with timestamp 6 minutes old rejected
- SHA-256 hash of known input matches reference vector

---

### Phase 0.5 — Real GHC-API Eval via hint (PLAN.md Phase 3)

**Dependency:** Phase A (Bootstrap) requires in-process Haskell evaluation. hint
must be wired before self-compilation can work.

**Status:** CPP stub in `Sandbox.hs` (`#ifdef WITH_HINT`)

**What changes:**
- `Sandbox.hs`: Complete `hintEval` — `runInterpreter`, `setImports`, Safe Haskell
  enforcement, `race` against timeout

**Acceptance criteria:**
- `cabal build -f+with-hint` compiles clean
- `unsafePerformIO` in evaluated code is rejected by Safe Haskell gate
- Evaluated expression `"2 + 3"` returns `"5"`

---

## Phase A: GHC-API Bootstrap

**Prerequisite:** Phase 0.5 complete.

**Goal:** Wire the `hint` interpreter to execute Text Haskell snippets that can
themselves manipulate files and call GHC sub-APIs. This is the bridge from the
ExprF world into the real Haskell world.

### What Changes

**New module: `src/DGM/HintBridge.hs`**

```haskell
module DGM.HintBridge
  ( HintEnv(..)
  , newHintEnv
  , evalHaskell      -- :: HintEnv -> Text -> IO (Either Text Text)
  , typeCheckHaskell -- :: HintEnv -> Text -> IO (Either Text Text)
  ) where
```

The `HintBridge` wraps `hint`'s `Interpreter` monad with:
- A persistent interpreter state (avoids re-initialisation per call)
- Safe Haskell enforcement (`languageExtensions := [SafeHaskell]`)
- Configurable import allowlist (default: `Prelude`, `Data.List`, `Data.Text`)
- Timeout via `async`/`race` (inherits `ResourceLimits`)

**`src/DGM/Sandbox.hs`** — `runInSandbox` delegates to `HintBridge.evalHaskell` when
the `with-hint` flag is set. The `SandboxResult` is populated from hint output.

### Acceptance Criteria

- `evalHaskell env "show (length [1..10])"` returns `Right "10"`
- `evalHaskell env "import System.IO.Unsafe (unsafePerformIO); unsafePerformIO (return 1)"` returns `Left "<error: Safe Haskell violation>"`
- Round-trip: hint evaluates code that uses `Data.Text`
- All existing 34 tests pass with `with-hint` disabled

### Risks

- **GHC version pinning**: hint wraps GHC internals. The cabal flag ensures the
  non-hint path always works. Constrain `hint >= 0.9 && < 0.10` to avoid API drift.
- **Thread leaks**: hint's `Interpreter` monad can leave threads running if an
  exception escapes the `race`. Wrap in `bracket` with explicit `reset`.

---

## Phase B: Real AST — Replacing ExprF with GHC Syntax Tree

**Prerequisite:** Phase A complete.

**Goal:** Give the system the ability to represent, traverse, and mutate real Haskell
source code. This is the most architecturally significant phase.

### Library Choice: ghc-exactprint

`ghc-exactprint` is chosen over `ghc-lib` for one critical reason: **round-trip
fidelity**. `ghc-exactprint` preserves whitespace, comments, and formatting
annotations so that pretty-printed output matches the original except where
mutations were applied. `ghc-lib` produces clean but unformatted output that
diverges stylistically on every write-back.

```
dgm.cabal — add:
  ghc-exactprint >= 1.7
  ghc-parser    (transitive, pinned to same GHC version)
```

**Note:** `ghc-exactprint` requires matching the GHC version of the running
compiler. Pin to GHC 9.6.x (current toolchain).

### New Module: `src/DGM/HsAST.hs`

This module is the core of Phase B. It wraps the `ghc-exactprint` API behind a
stable interface so the rest of the system is insulated from GHC API churn.

```haskell
module DGM.HsAST
  ( -- * Parsing
    HsModule
  , parseHsFile   -- :: FilePath -> IO (Either Text HsModule)
  , parseHsText   -- :: Text -> IO (Either Text HsModule)
    -- * Printing (exact round-trip)
  , printHsModule -- :: HsModule -> Text
    -- * AST traversal
  , HsNode
  , hsNodes       -- :: HsModule -> [HsNode]  (flat traversal)
  , hsNodeText    -- :: HsNode -> Text
  , hsNodeRange   -- :: HsNode -> SrcSpan
    -- * Mutations
  , HsMutation
  , applyHsMutation  -- :: HsMutation -> HsModule -> Either Text HsModule
  , collectHsMutations -- :: HsRuleSet -> HsModule -> [HsMutation]
    -- * Rule sets
  , HsRewriteRule
  , HsRuleSet
  , defaultHsRules  -- minimal safe rules for self-modification
  ) where
```

**Key design decisions:**

1. **`HsModule` is opaque** — wraps `ghc-exactprint`'s `(Anns, ParsedSource)` pair.
   The rest of the system never sees raw GHC types.

2. **`HsNode` is positional** — identified by `SrcSpan` (file, line, column range).
   This is the natural identity for a piece of Haskell syntax.

3. **`applyHsMutation` is pure** — takes an `HsModule`, returns either a mutated
   `HsModule` or an error. No IO. The IO (file write) is in Phase C.

4. **`defaultHsRules`** — a conservative initial rule set:
   - Eta-reduction: `\x -> f x` → `f`
   - Constant folding in arithmetic literals
   - Unused import removal
   - Where-clause inlining for single-use bindings
   - No rules that change type signatures (too risky for bootstrap)

### Mutation Representation Bridge

`DGM.Types.Mutation` must be extended to carry `HsMutation` as an alternative
payload alongside the existing `ASTNode`-based mutation:

```haskell
data MutationPayload
  = ExprMutation ASTNode ASTNode   -- existing ExprF path
  | HaskellMutation HsMutation     -- new path for real .hs files

data Mutation = Mutation
  { mutationType   :: MutationType
  , mutationTarget :: Text
  , mutationPayload :: MutationPayload
  }
```

This is a backwards-compatible extension: existing tests use `ExprMutation`; new
self-modification tests use `HaskellMutation`.

### Acceptance Criteria

- `parseHsFile "src/DGM/Types.hs" >>= printHsModule` reproduces the file verbatim
  (modulo trailing newline)
- `collectHsMutations defaultHsRules <Types.hs>` produces ≥1 candidate mutations
- `applyHsMutation m hsModule >>= printHsModule` produces syntactically valid
  Haskell (validated by re-parsing the output)
- All existing 34+ tests pass (ExprF path untouched)

### Risks

- **GHC version coupling**: ghc-exactprint is tightly coupled to GHC version. Add
  CI check that `ghc --version` matches the pinned version.
- **AST complexity**: GHC's `HsExpr` has ~60 constructors. Start with a restricted
  visitor that only descends into function bodies and `where` clauses. Expand scope
  incrementally.
- **Annotation fragility**: Exact-print annotations can become invalid after
  mutations. Run `printHsModule >>= parseHsText` as a post-mutation sanity check
  before any file write.

---

## Phase C: File System Integration

**Prerequisite:** Phase B complete. Phase 0.4 (crypto quorum) complete.

**Goal:** Enable the system to read its own `.hs` source files, apply mutations,
and write the result back using the existing safety kernel.

### What Changes

**`src/DGM/SelfMod.hs`** — new module (thin orchestration layer):

```haskell
module DGM.SelfMod
  ( -- * Source discovery
    discoverSources   -- :: IO [FilePath]  (src/DGM/*.hs)
  , readOwnSource     -- :: FilePath -> IO (Either Text HsModule)
    -- * Mutation pipeline
  , proposeSelfMutations
      -- :: AgentState -> [FilePath] -> IO [(FilePath, HsMutation, HsModule)]
  , rankMutations
      -- :: [(FilePath, HsMutation, HsModule)] -> [(FilePath, HsMutation, HsModule)]
    -- * Commit path (requires quorum)
  , writeMutation
      -- :: AgentState -> AuditLog -> QuorumProof
      -- -> FilePath -> HsModule -> IO (Either Text ())
  ) where
```

**`discoverSources`**: Lists all `.hs` files under `src/DGM/` via `System.Directory`.
Does NOT cross module boundaries into `app/` or `test/` (those come in Phase F).

**`readOwnSource`**: Calls `HsAST.parseHsFile`. Returns `Left err` on parse failure.
A parse failure of own source is an integrity error — log to audit, do not proceed.

**`proposeSelfMutations`**: For each source file, calls `HsAST.collectHsMutations`.
Returns a list of `(file, mutation, mutated_module)` triples.

**`rankMutations`**: Scores candidates using a size heuristic (prefer smaller
mutations, fewer changed nodes) and a novelty score (penalise mutations already in
archive by Levenshtein distance on source text). This is the analogue of
`learnabilityScore` in `AbsoluteZero.hs` but for source-level mutations.

**`writeMutation`**: Calls `SafetyKernel.dispatch` with `SystemWrite path text proof`.
The `QuorumProof` is required. In the bootstrap phase, the quorum proof is
generated by a single-party `mockQuorumProof` that embeds the SHA-256 of the
mutation's content and the current POSIX timestamp. Once Phase 0.4 is complete,
the replay protection (timestamp window) is fully enforced.

### SystemWrite Plumbing

`Command 'Existential` already includes `SystemWrite :: FilePath -> Text -> QuorumProof -> Command 'Existential`. The `dispatchExistential` handler in `SafetyKernel.hs` must actually execute the write:

```haskell
dispatchExistential st audit (SystemWrite path content proof) = do
  ok <- verifyQuorum proof ("write:" <> T.pack path)
  if not ok
    then pure (Left "Quorum proof invalid or expired")
    else do
      writeFile path (T.unpack content)
      pure (Right ("Written: " <> T.pack path))
```

Currently `dispatchExistential` is a stub. This is the first phase where
`SystemWrite` executes a real file write.

### Safety Invariants

1. **Path whitelist**: Only paths under the repo root (determined at startup) are
   permitted. Attempts to write outside the repo return `Left "Path not whitelisted"`.
2. **No write to currently-loaded modules**: GHC cannot hot-swap a module that is
   currently in use. Writes to `DGM.HsAST` itself require a process restart to take
   effect. This is tracked by maintaining a `Set FilePath` of loaded modules.
3. **Atomic write**: Write to `.new` file, verify it parses, then `renameFile`
   (atomic on POSIX/Darwin). If rename fails, original is preserved.
4. **Audit trail**: Every `SystemWrite` is logged to `AuditLog` with SHA-256 of
   original and mutated content.

### Acceptance Criteria

- `discoverSources` finds all 10 `src/DGM/*.hs` files
- `readOwnSource "src/DGM/Types.hs"` parses successfully
- `proposeSelfMutations` returns ≥1 candidate for `Rewriting.hs` (eta-reduction, etc.)
- `writeMutation` with a valid quorum proof writes the file atomically
- `writeMutation` with an expired proof (`quorumTimestamp` > 5 minutes old) returns
  `Left "Quorum proof expired"`
- Attempt to write outside repo root returns `Left "Path not whitelisted"`

### Risks

- **Corrupting own source**: A mutation that produces invalid Haskell will fail
  Phase D (cabal test fails). The rollback mechanism (Phase D) handles this. But
  the atomic write guard (parse before rename) is a second line of defense.
- **Race between write and GHC**: If the system writes a file that GHC is currently
  reading (during a test run), the result is undefined. Enforce: never run
  `writeMutation` and `cabal test` concurrently. A `TVar Bool` guard in `SelfMod.hs`.

---

## Phase D: Self-Compilation Loop

**Prerequisite:** Phase C complete.

**Goal:** After writing a mutation, compile and test it. Score the result. Archive
the outcome regardless of pass/fail.

### What Changes

**`src/DGM/SelfCompile.hs`** — new module:

```haskell
module DGM.SelfCompile
  ( -- * Compilation
    CompileResult(..)
  , compileSelf    -- :: IO CompileResult
  , testSelf       -- :: IO CompileResult
    -- * Scoring
  , scoreCompileResult  -- :: CompileResult -> Double
  ) where

data CompileResult
  = CompileSuccess { crTestsPassed :: Int, crTestsFailed :: Int, crLatencyMs :: Double }
  | CompileFailure { crErrors :: Text, crPhase :: CompilePhase }

data CompilePhase = ParsePhase | TypeCheckPhase | LinkPhase | TestPhase
```

**`testSelf`**: Spawns `cabal test --test-show-details=direct` as a subprocess
using `System.Process.createProcess`. Captures stdout+stderr. Parses TAP or HUnit
output to extract pass/fail counts. Imposes a hard timeout (default: 120 seconds).

**`scoreCompileResult`**:
- `CompileSuccess` with all tests passing: `1.0`
- `CompileSuccess` with k/n tests passing: `k/n`
- `CompileFailure` at `TypeCheckPhase`: `0.1` (better than random; type error
  localises the problem)
- `CompileFailure` at `ParsePhase`: `0.0`

**`src/DGM/Cycle.hs`** — `runCycle` gains a `SelfModPhase` constructor:

```haskell
data CyclePhase
  = ExprPhase        -- existing: ExprF self-play loop
  | SelfModPhase     -- new: propose → write → compile → score → archive
```

The `SelfModPhase` runs `SelfMod.proposeSelfMutations`, picks the top-ranked
candidate, calls `SelfMod.writeMutation`, calls `SelfCompile.testSelf`, archives
the result, rolls back on failure.

### Rollback Mechanism

Rollback is a first-class operation, not an afterthought:

```haskell
data SelfModTxn = SelfModTxn
  { smtFile         :: FilePath
  , smtOriginalText :: Text      -- original source before mutation
  , smtMutation     :: HsMutation
  }

withSelfModTxn :: SelfModTxn -> IO (Either Text ()) -> IO (Either Text ())
withSelfModTxn txn action = do
  result <- action
  case result of
    Right () -> pure (Right ())
    Left  err -> do
      -- Restore original
      writeFile (smtFile txn) (T.unpack (smtOriginalText txn))
      pure (Left ("Rolled back: " <> err))
```

The original source text is captured before the write. If `testSelf` returns
`CompileFailure`, `withSelfModTxn` restores the original. The `ArchiveEntry` is
written with `entryPassed = False` and `entryCode` = the mutated text (not the
restored original) — preserving the failed stepping stone for future learning.

### Acceptance Criteria

- `testSelf` on clean master returns `CompileSuccess` with all tests passing
- `testSelf` after introducing a type error returns `CompileFailure { crPhase = TypeCheckPhase }`
- Rollback after `CompileFailure` restores the original file verbatim (byte-for-byte)
- Archive contains `entryPassed = False` entries after failed compile
- `cabal run dgm -- selfmod 1` (new CLI flag) runs one full self-modification cycle

### Risks

- **cabal startup latency**: `cabal test` on a warm build takes ~5 seconds.
  On a cold build (first run after source change): 30–90 seconds. The 120-second
  timeout must account for this. Consider `cabal build` (no test run) as a fast
  pre-check before the full `cabal test`.
- **Subprocess isolation**: The `cabal test` subprocess must not inherit file
  descriptors that keep locks on the SQLite archive. Use `close_fds = True` in
  `createProcess`.
- **Infinite regress risk**: If a mutation changes `SelfCompile.hs` and breaks the
  compilation scorer, the system cannot detect its own failure. Mitigation: keep
  `SelfCompile.hs` simple, well-tested, and low-priority for mutation selection
  (add a penalty in `rankMutations` for files that contain `SelfCompile` in their
  module graph).

---

## Phase E: Self-Modification Commit

**Prerequisite:** Phase D complete.

**Goal:** When `testSelf` passes, the mutation is now the *live* implementation.
The archive entry is marked `entryPassed = True` and the source change is committed
to git.

### What Changes

**`src/DGM/SelfMod.hs`** — add `commitMutation`:

```haskell
commitMutation
  :: FilePath    -- ^ path to mutated file
  -> HsMutation  -- ^ the winning mutation
  -> ArchiveEntry  -- ^ the entry to associate with this commit
  -> IO (Either Text Text)  -- ^ Right gitHash | Left err
commitMutation path mutation entry = do
  -- git add path
  -- git commit -m "selfmod: <mutationType> in <module> (gen <generation>)"
  -- return SHA of new commit
```

The git commit message encodes the archive entry ID, enabling future sessions to
trace which evolution step produced each commit.

**`app/Main.hs`** — the `selfmod` CLI mode calls the full pipeline:

```
discover → propose → rank → write → test → commit (pass) / rollback (fail) → archive
```

### Idempotency

Every `commitMutation` call checks `git status` first. If the working tree is clean
(mutation was already committed), it returns `Right <existing-hash>`. This handles
restart-after-crash gracefully.

### Acceptance Criteria

- After a successful `selfmod` cycle, `git log --oneline -1` shows a commit with
  `selfmod:` prefix
- The commit is on the polecat's feature branch (NOT main — the polecat branch
  goes through Refinery like any other change)
- Rollback does NOT produce a git commit (source restored, nothing staged)
- Two consecutive `selfmod` runs do not double-commit the same mutation

### Risks

- **Git conflict with polecat's own branch**: The polecat is on `polecat/rust/SI-8do`.
  Self-modification commits pile up on this branch. Refinery must handle a branch
  with N self-modification commits in sequence. This is a Refinery concern, not a
  SelfMod concern — but document the assumption.
- **Commit message size**: If `HsMutation` is large (whole-function rewrite), the
  commit message should truncate to 72 characters. Include full details in the
  archive entry, not the commit message.

---

## Phase F: Multi-Module Reasoning

**Prerequisite:** Phase E stable (at least one successful self-modification cycle
completed and committed).

**Goal:** Evolve mutations that span multiple modules. Track cross-module
dependencies. Avoid mutations that break import chains.

### Dependency Graph

The system must maintain a live module dependency graph. This is computed from the
parsed ASTs:

```haskell
data ModuleGraph = ModuleGraph
  { mgModules     :: Map ModuleName FilePath
  , mgImports     :: Map ModuleName [ModuleName]  -- direct imports
  , mgExports     :: Map ModuleName [HsNode]      -- exported names
  , mgUsedBy      :: Map ModuleName [ModuleName]  -- reverse: who imports me
  }

buildModuleGraph :: [FilePath] -> IO ModuleGraph
```

**`buildModuleGraph`** calls `parseHsFile` on each source file and extracts the
`import` declarations.

### Cross-Module Mutation Rules

A mutation is **cross-module safe** if and only if:

1. It does not change the type signature of any exported function
2. It does not remove any exported name
3. It does not change any typeclass instance that other modules depend on
4. If it changes the *internal* representation of a type, all modules in
   `mgUsedBy[mutated_module]` are re-compiled and their tests re-run

Rule 4 is expensive. It requires running `cabal test` with a broader scope after
each cross-module mutation. Default: only single-module mutations until Phase G
provides the self-play harness to generate and score multi-module proposals.

**`rankMutations`** (Phase C) gains a `moduleImpact` penalty:

```
moduleImpact = |{m | m ∈ mgUsedBy[file] and mutation changes exported interface}|
```

Zero-impact mutations (change only internal `where` bindings) score highest.
High-impact mutations (change `Types.hs` exported types, which cascade to ALL
modules) are heavily penalised and require an elevated quorum threshold.

### `Types.hs` Special Status

`Types.hs` is imported by every other module. A mutation to `Types.hs` that changes
a core type (`QuorumProof`, `Mutation`, `ArchiveEntry`) has system-wide impact.
Two additional safeguards:

1. Mutations to `Types.hs` are flagged `MutationType = Existential` and require a
   full multi-module recompile before scoring.
2. A separate `typesHash :: TVar Text` (SHA-256 of `Types.hs`) is checked at each
   cycle start. If it differs from the last cycle's hash, the system pauses and
   runs a full type-check before proceeding.

### Acceptance Criteria

- `buildModuleGraph` correctly identifies that all 10 `src/DGM/*.hs` modules import
  `DGM.Types`
- A mutation to `Rewriting.hs` that adds a new `where`-clause helper scores higher
  than a mutation to `Types.hs` that renames a field
- A mutation that removes an exported function is rejected before even being written
- Full multi-module recompile correctly detects that a `Types.hs` field rename
  breaks `Archive.hs`

### Risks

- **Circular imports**: GHC's import graph is a DAG; circular imports are compile
  errors. `buildModuleGraph` must detect and refuse to mutate cycles.
- **Template Haskell modules**: If any module uses TH, mutation ordering matters
  (TH runs at compile time). Currently no TH in SIS; note as a constraint.
- **GHC Plugin interference**: If the system later adds LiquidHaskell or other
  plugins, these interact with the parse/type-check cycle in non-trivial ways.
  Defer plugin support to a post-Phase-F decision.

---

## Phase G: Full Autonomous Self-Improvement Loop

**Prerequisite:** Phase F stable.

**Goal:** Wire the Absolute Zero proposer to generate tasks *about the system's own
source code*. The system proposes modifications to its own rewriting rules, scoring
functions, archive strategy, and proposer heuristics — then solves them and
empirically validates via `cabal test`.

### Task DSL Extension

The existing `Task` type in `AbsoluteZero.hs` uses `ExprF` as the task spec. Phase G
extends this with a `SelfModTask` variant:

```haskell
data TaskSpec
  = ExprTask Expr          -- existing: evaluate expression
  | SelfModTask SelfModSpec  -- new: modify own source

data SelfModSpec = SelfModSpec
  { smsTargetModule  :: ModuleName
  , smsGoal          :: SelfModGoal
  , smsConstraints   :: [SelfModConstraint]
  }

data SelfModGoal
  = AddRewriteRule   Text          -- add a named rule to Rewriting.hs
  | ImproveScoring   Text          -- improve a named scoring function
  | ReduceComplexity ModuleName    -- reduce cyclomatic complexity of module
  | ExtendArchive    Text          -- add a new field/capability to Archive
  | AddCapability    Text          -- add a wholly new module/feature
```

### Proposer Task Generation

The `proposeTask` function gains a `SelfModPhase` mode:

```haskell
proposeTask :: Proposer -> IO (Task, Proposer)
proposeTask p
  | propCurrentPhase p == SelfModMode = proposeSelfModTask p
  | otherwise = proposeExprTask p

proposeSelfModTask :: Proposer -> IO (Task, Proposer)
proposeSelfModTask p = do
  graph <- buildModuleGraph =<< discoverSources
  -- select a module with low coverage, high mutation potential
  -- propose a SelfModSpec for it
  -- set learnability: can the solver (SelfCompile) pass tests after applying?
```

The learnability reward for `SelfModTask` is the `scoreCompileResult` of the
resulting `cabal test` run. This directly couples the proposer's reward to
empirical system health.

### Autonomy Escalation

Phase G introduces a phased autonomy model:

| Autonomy Level | What the system can do without human quorum |
|----------------|---------------------------------------------|
| Level 0 (bootstrap) | Propose only; human approves each write |
| Level 1 (Phase C)   | Auto-approve writes to `where`-clause internals |
| Level 2 (Phase E)   | Auto-approve writes that improve test scores |
| Level 3 (Phase G)   | Auto-propose and auto-commit capability expansions |
| Level 4 (future)    | Modify own safety kernel (requires explicit unlock) |

Level 4 is never automated. The `SafetyKernel.hs` module has an
immutable flag in `Types.hs`:

```haskell
-- | Safety kernel source files that may NOT be autonomously mutated.
-- Modifications to these files require a human-approved quorum proof.
safeguardedModules :: Set ModuleName
safeguardedModules = Set.fromList
  [ "DGM.SafetyKernel"
  , "DGM.Types"  -- at Level 4 threshold
  ]
```

### Acceptance Criteria

- Proposer generates a `SelfModTask` with `AddRewriteRule` goal
- System mutates `Rewriting.hs` to add the rule, runs `cabal test`, archives result
- If tests pass: commit is made, difficulty increases, proposer generates a harder task
- If tests fail: rollback, archive negative entry, proposer adjusts (difficulty
  decreases or target module shifts)
- 5 successive autonomous cycles with no human intervention, at least 2 passing
- System does not attempt to mutate `DGM.SafetyKernel` or `DGM.Types` without
  explicit unlock

### Risks

- **Reward hacking**: A mutation that deletes tests makes `cabal test` pass trivially.
  Counter-measure: `scoreCompileResult` penalises decreases in `crTestsPassed`. A
  mutation that reduces the test count scores 0.0 regardless of pass/fail.
- **Runaway self-modification**: Without a pause mechanism, the system could generate
  thousands of self-modification attempts in a loop. Add: a `selfModRateLimit :: Int`
  (default: 1 self-mod cycle per 10 ExprF cycles) in `CycleConfig`.
- **Loss of test coverage**: The system might evolve away from testable states.
  Archive the full test output (not just pass/fail count) so regression is detectable.
- **Quorum degradation**: If the system mutates `SafetyKernel.hs` logic that
  validates quorum proofs, it could make all proofs valid. The `safeguardedModules`
  set is the primary defense. Secondary: ship a reference SHA-256 of
  `SafetyKernel.hs` in a separate read-only config file, checked at startup.

---

## Dependency Graph

```
Phase 0.1 (SQLite)         ←─ no dependencies
Phase 0.2 (recursive rw)   ←─ no dependencies
Phase 0.3 (proposer adapt) ←─ Phase 0.2
Phase 0.4 (crypto quorum)  ←─ no dependencies
Phase 0.5 (hint wired)     ←─ no dependencies
     │
     ↓
Phase A (hint bridge)      ←─ Phase 0.5
     │
     ↓
Phase B (ghc-exactprint)   ←─ Phase 0.1, Phase 0.4
     │
     ↓
Phase C (FS integration)   ←─ Phase B
     │
     ↓
Phase D (self-compile)     ←─ Phase C
     │
     ↓
Phase E (commit)           ←─ Phase D
     │
     ↓
Phase F (multi-module)     ←─ Phase E, Phase 0.3
     │
     ↓
Phase G (autonomous loop)  ←─ Phase F, Phase 0.3
```

Phases 0.1–0.5 and Phase A can be parallelised. Phase B gates everything above it.

---

## Milestone Definitions

| Milestone | Phases | Definition of Done |
|-----------|--------|--------------------|
| **M0: Foundation** | 0.1–0.5 | All PLAN.md gaps closed; 42 tests pass; SQLite archive survives restart; SHA-256 quorum; hint evaluates Haskell |
| **M1: Parse Own Source** | A, B | `parseHsFile "src/DGM/Types.hs"` round-trips; `collectHsMutations` finds ≥1 candidate |
| **M2: Write and Roll Back** | C | `writeMutation` writes atomically; rollback restores verbatim; quorum replay rejected |
| **M3: Compile and Score** | D | `testSelf` passes on clean master; rollback on failure; archive preserves failed mutations |
| **M4: First Self-Modification** | E | One self-mod cycle completes end-to-end; mutation committed to git; `git log` shows `selfmod:` entry |
| **M5: Multi-Module** | F | Module graph built; cross-module impact scoring works; `Types.hs` changes blocked at Level 3 |
| **M6: Autonomous Evolution** | G | 5 consecutive autonomous cycles; at least 2 produce committed improvements; rate limiter prevents runaway |

---

## File Change Summary

| Phase | New Files | Modified Files | New Dependencies |
|-------|-----------|----------------|------------------|
| 0.1 SQLite | — | `Archive.hs`, `Types.hs`, `Cycle.hs`, `dgm.cabal`, `Main.hs` | `sqlite-simple` |
| 0.2 Recursive rw | — | `Rewriting.hs`, `test/Spec.hs` | none |
| 0.3 Proposer | — | `Cycle.hs`, `Main.hs` | none |
| 0.4 Crypto | — | `SafetyKernel.hs`, `Types.hs`, `dgm.cabal` | `cryptohash-sha256`, `base16-bytestring` |
| 0.5 hint | — | `Sandbox.hs` | `hint` (already listed) |
| A Bridge | `HintBridge.hs` | `Sandbox.hs` | — |
| B HsAST | `HsAST.hs` | `Types.hs`, `dgm.cabal` | `ghc-exactprint` |
| C FS | `SelfMod.hs` | `SafetyKernel.hs` | `directory`, `unix` |
| D Compile | `SelfCompile.hs` | `Cycle.hs`, `Main.hs` | `process` |
| E Commit | — | `SelfMod.hs`, `Main.hs` | — |
| F Multi-module | — | `SelfMod.hs`, `HsAST.hs` | — |
| G Autonomous | — | `AbsoluteZero.hs`, `Cycle.hs` | — |

---

## Risk Register

| Risk | Severity | Likelihood | Mitigation |
|------|----------|-----------|------------|
| ghc-exactprint version lock | HIGH | HIGH | Pin GHC version in cabal.project; CI enforces |
| Self-modification corrupts source | CRITICAL | MEDIUM | Atomic write (parse-before-rename); rollback |
| Test deletion reward hack | CRITICAL | MEDIUM | Penalise test count reduction in scorer |
| `SafetyKernel.hs` mutation | CRITICAL | LOW | `safeguardedModules` set; reference SHA-256 |
| hint thread leak | MEDIUM | MEDIUM | `bracket` with `reset`; timeout |
| cabal build latency | MEDIUM | HIGH | Warm cache; `cabal build` fast-check before test |
| Cross-module cascade failure | HIGH | MEDIUM | Single-module-first default; impact penalty |
| Git conflict on polecat branch | LOW | MEDIUM | Refinery handles; document assumption |
| Infinite self-mod loop | HIGH | LOW | `selfModRateLimit` in `CycleConfig` |
| Archive grows unbounded | MEDIUM | HIGH | SQLite + periodic prune of non-Pareto entries |

---

## Implementation Order (Recommended)

```
Week 1:  Phases 0.1, 0.2, 0.4   (SQLite, recursive rw, crypto)
Week 2:  Phases 0.3, 0.5, A     (proposer adapt, hint, bridge)
Week 3:  Phase B                 (ghc-exactprint — most complex single phase)
Week 4:  Phase C                 (FS integration — safety critical)
Week 5:  Phase D                 (self-compile — integration testing)
Week 6:  Phase E                 (commit — end-to-end first self-mod)
Week 7+: Phases F, G             (multi-module, autonomous loop)
```

Phases 0.1 and 0.2 can be assigned to separate polecats and run in parallel.
Phase B gates all subsequent work and warrants the most engineering attention.

---

## Appendix: The Gödel Self-Reference

The deepest irony of this architecture: to evolve the module that performs
self-modification (`SelfMod.hs`), the system must use `SelfMod.hs` itself. This is
not a contradiction — it is the definition of recursive self-improvement.

The resolution is in the staging: each phase produces a *new capability* that the
next phase can use. Phase C produces the ability to write files. Phase D produces
the ability to test writes. Phase E produces the ability to commit writes. By the
time Phase G activates, the system has a full substrate it can use to improve its
own substrate — including its own self-improvement loop.

The only modules that are permanently outside this loop are those in
`safeguardedModules`. These are the Gödel sentences of the system: propositions
about the system that the system cannot prove from within itself without external
verification. They form the irreducible human-in-the-loop guarantee.

Everything else evolves.
