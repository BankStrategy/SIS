# Plan: Semantic Oracle — Engineer-Grade Self-Modification

## Problem Statement

The self-modification pipeline works end-to-end but operates at linter level:
- The Oracle sees raw source with line numbers and is told "propose ONE small safe improvement"
- The enriched path constrains to "pick ONE from VERIFIED SAFE REMOVALS" (GHC warnings)
- No type information, call graphs, complexity analysis, or test coverage data reaches the Oracle
- No failure feedback — when mutations fail, the Oracle doesn't learn why
- No per-test analysis — only aggregate pass/fail counts
- No multi-file support — can't propose cross-cutting refactors
- Scoring is binary (tests pass = 1.0, else reject)

**Goal**: Enable the Oracle to propose semantic changes — algorithmic improvements,
architectural refactors, test coverage additions, and type safety upgrades — by giving
it deep structural context and structured feedback loops.

---

## Stage 1: `DGM.SemanticContext` — Deep Module Analysis

**New module**: `src/DGM/SemanticContext.hs` (no CPP, no optional flags)

### Types

```haskell
data FunctionInfo = FunctionInfo
  { fiName       :: !Text           -- "buildModuleGraph"
  , fiTypeSig    :: Maybe Text      -- ":: [FilePath] -> IO ModuleGraph"
  , fiLineStart  :: !Int            -- first line of definition
  , fiLineCount  :: !Int            -- body length in lines
  , fiCalls      :: [Text]          -- other top-level names referenced in body
  , fiComplexity :: !Int            -- heuristic: cases + guards + wheres + depth
  }

data TestMapping = TestMapping
  { tmTestName   :: !Text           -- "buildModuleGraph: core DGM modules import..."
  , tmTestedFunc :: !Text           -- "buildModuleGraph"
  , tmTestFile   :: !FilePath       -- "test/Spec.hs"
  }

data SemanticContext = SemanticContext
  { scFilePath       :: !FilePath
  , scModuleName     :: !Text
  , scFunctions      :: [FunctionInfo]
  , scTestCoverage   :: [TestMapping]      -- which tests cover which functions
  , scUntestedExports:: [Text]             -- exported functions with 0 test matches
  , scUsedBy         :: [(Text, [Text])]   -- (export, [importing modules])
  , scRecentFailures :: [(Text, Text)]     -- (mutation desc, failure reason) from archive
  , scWarnings       :: [GhcWarning]       -- from OracleContext (already collected)
  }
```

### Implementation

**`extractFunctions :: Text -> [FunctionInfo]`** (pure text analysis):
- Scan for `name :: Type` lines → record type signatures
- Scan for `name arg1 arg2 ... =` or `name = do` lines → function start
- Track body extent via indentation (next top-level definition or end of file = end)
- Count complexity: `case`, `|` guards, `where`, `let ... in`, nested lambdas
- Extract callee names: all identifiers in body matching other top-level names

**`buildTestCoverage :: [FilePath] -> [FunctionInfo] -> IO [TestMapping]`**:
- Read `test/Spec.hs` (and any other `*Spec.hs` or `*Test.hs` files)
- Parse test names from `testCase "description"` and `testProperty "description"`
- Match function names to test descriptions (substring match on function name in test string)
- Return mapping of test → tested function

**`buildSemanticContext`**: Composes extractFunctions + buildTestCoverage + ModuleGraph
lookups + archive failure queries into a full `SemanticContext`.

### Files touched
- New: `src/DGM/SemanticContext.hs`
- Edit: `dgm.cabal` (add to exposed-modules)
- Edit: `test/Spec.hs` (add tests for extractFunctions, buildTestCoverage)

---

## Stage 2: Semantic Oracle Prompting

**Edit modules**: `Oracle.hs`, `OracleContext.hs`, `OracleHandle.hs`, `SelfMod.hs`

### New system prompt (engineer-grade)

Replace the generic "propose ONE small, safe improvement" with:

```
You are a senior Haskell engineer reviewing a module for improvement. You receive
deep structural analysis including type signatures, complexity metrics, test
coverage gaps, and cross-module dependencies. Propose exactly ONE improvement as
a unified diff. Your improvement should be in one of these categories:

- ALGORITHM: Better time complexity, fewer traversals, smarter data structures
- CORRECTNESS: Edge cases, error recovery, partial function elimination
- ARCHITECTURE: Extract helpers, reduce nesting, improve cohesion
- TEST: Add test cases for untested functions (append to test/Spec.hs)
- TYPE_SAFETY: More precise types, replace partial functions with total ones

Choose the highest-impact category for this module. Output ONLY the diff.
No explanation, no markdown fences.
```

### User prompt structure

```
## MODULE: DGM/ModGraph.hs

### TYPE SIGNATURES
buildModuleGraph :: [FilePath] -> IO ModuleGraph
moduleImpact :: ModuleGraph -> FilePath -> HsMutation -> HsModule -> Int
numDependents :: ModuleGraph -> FilePath -> Int

### FUNCTION COMPLEXITY (lines / complexity score)
extractExports: 15 lines, complexity 8 (3 cases, 2 guards, 1 where)
buildModuleGraph: 12 lines, complexity 5 (1 case, 2 binds)
moduleImpact: 3 lines, complexity 2

### TEST COVERAGE
buildModuleGraph: TESTED (3 tests)
numDependents: TESTED (2 tests)
moduleImpact: NOT TESTED  <-- improvement target
extractExports: TESTED (4 tests)

### CROSS-MODULE USAGE
buildModuleGraph: Cycle.hs (1x)
moduleImpact: SelfMod.hs (1x)
extractExports: internal only

### GHC WARNINGS
(any current -Wall warnings for this file)

### PAST FAILURES (if any)
Last attempt "refactor extractExports" failed:
  Type error line 182: Couldn't match '[Text]' with 'Maybe [Text]'

### SOURCE
---
1  module DGM.ModGraph ...
...
---

Produce a unified diff for the highest-impact improvement.
```

### Three prompt tiers (dispatched by context richness)

1. **Enriched (GHC warnings present)**: Keep existing enriched path for lint removals
   — it works and has high success rate. This is the "quick win" tier.

2. **Semantic (no warnings, rich context)**: New path using SemanticContext.
   Higher temperature (0.4) to encourage creative proposals. This is the
   "engineer-grade" tier.

3. **Standard (fallback)**: Current generic prompt as last resort when no
   semantic context is available.

### Oracle.hs changes

- Add `proposeMutationSemantic :: OracleEnv -> FilePath -> Text -> Text -> IO (Either Text HsMutation)`
  - Takes the semantic prompt as pre-built Text
  - Uses new system prompt (engineer-grade, above)
  - Temperature 0.4 (more exploratory than enriched 0.2, more creative than standard 0.3)
  - Description prefix: `"oracle-semantic: "`

- Add to OracleHandle: `proposeMutationSemanticH`

### OracleContext.hs changes

- Add `buildSemanticPrompt :: SemanticContext -> Maybe MutationContext -> Text`
  - Formats the full semantic prompt from SemanticContext
  - Always returns content (unlike buildEnrichedPrompt which returns Nothing when no warnings)

### SelfMod.hs dispatch logic changes

In `proposeSelfMutations`, per-file dispatch becomes:
1. If GHC warnings exist for this file → enriched prompt (lint tier, existing)
2. Else → build SemanticContext → semantic prompt (engineer tier, new)
3. If SemanticContext unavailable → standard prompt (fallback, existing)

### Files touched
- Edit: `src/DGM/Oracle.hs` (new proposeMutationSemantic, new system prompt)
- Edit: `src/DGM/OracleHandle.hs` (expose proposeMutationSemanticH)
- Edit: `src/DGM/OracleContext.hs` (add buildSemanticPrompt)
- Edit: `src/DGM/SelfMod.hs` (three-tier dispatch)
- Edit: `test/Spec.hs` (tests for buildSemanticPrompt formatting)

---

## Stage 3: Error Feedback Loop

**Goal**: When a mutation fails, capture WHY and feed it back to the Oracle on the
next attempt for the same file.

### ArchiveEntry extension

Add field to `Types.hs`:
```haskell
  , entryFailureReason :: Maybe Text
    -- ^ Structured failure reason: GHC error text, or failed test names.
    -- Populated on failure; Nothing on success.
```

### Failure capture in Cycle.hs

In `runMutationPipeline`, when `txnResult = Left err`:
- `err` already contains failure text ("Build pre-flight: ...", "Tests degraded, score=0.5")
- For build failures: `crErrors` from CompileResult has the GHC error output
- Store first 500 chars of error as `entryFailureReason`

Specifically:
- Build preflight failure → capture GHC error lines from stderr
- LH failure → capture "UNSAFE: `<details>`"
- Test failure → capture `crErrors` from CompileResult (test output with names)

### Failure retrieval

Add to `Archive.hs`:
```haskell
getRecentFailures :: TVar [ArchiveEntry] -> FilePath -> STM [(Text, Text)]
```
- Filters by `mutationTarget == filePath && not entryPassed`
- Returns `(entryCode, fromMaybe "" entryFailureReason)` pairs
- Most recent first, limit 3

`SemanticContext.scRecentFailures` is populated from this query. The semantic
prompt includes a "PAST FAILURES" section showing the last 3 failed mutations
for this file with their error messages, so the Oracle doesn't repeat mistakes.

### SQLite schema migration

Add column: `failure_reason TEXT` to `archive_entries` table.
On startup, `ALTER TABLE IF NOT EXISTS` (SQLite gracefully handles existing tables).

### Files touched
- Edit: `src/DGM/Types.hs` (add entryFailureReason to ArchiveEntry + LH annotation + JSON)
- Edit: `src/DGM/Cycle.hs` (capture failure reason in archive entries)
- Edit: `src/DGM/Archive.hs` (add getRecentFailures query, SQLite column)
- Edit: `src/DGM/SemanticContext.hs` (consume failure data)
- Edit: `test/Spec.hs` (test failure capture and retrieval)

---

## Stage 4: Per-Test-Case Analysis

**Goal**: Parse individual test names from `cabal test` output so the Oracle knows
exactly which tests failed and can reason about specific test coverage.

### SelfCompile.hs changes

Add `parseTestDetails :: String -> [(Text, Bool)]`:
- Parse tasty's `--test-show-details=direct` output format
- Each line like `    functionName: test description:    OK` → (name, True)
- Each line like `    functionName: test description:    FAIL` → (name, False)
- Handles indentation and multiline output
- Return list of (test name, passed?)

Extend `CompileResult`:
```haskell
data CompileResult
  = CompileSuccess
      { crTestsPassed  :: Int
      , crTestsFailed  :: Int
      , crLatencyMs    :: Double
      , crFailedTests  :: [Text]   -- NEW: names of specific failed tests
      }
  | CompileFailure { crErrors :: Text, crPhase :: CompilePhase }
```

### Integration with error feedback (Stage 3)

When tests fail (score < 1.0):
- `crFailedTests` tells us exactly which tests broke
- Store as part of `entryFailureReason`: "Tests failed: [test1, test2, ...]"
- Oracle sees on next attempt: "Last mutation broke tests: test1, test2"

### Integration with SemanticContext (Stage 1)

`scTestCoverage` can now include pass/fail status from the most recent test run:
- "buildModuleGraph: TESTED (3 tests, all passing)"
- "moduleImpact: NOT TESTED"

### Files touched
- Edit: `src/DGM/SelfCompile.hs` (parseTestDetails, extend CompileResult)
- Edit: `src/DGM/Cycle.hs` (use crFailedTests in error feedback)
- Edit: `test/Spec.hs` (test parseTestDetails with various tasty output formats)

---

## Stage 5: Multi-File Change Sets

**Goal**: Allow the Oracle to propose coordinated changes across multiple modules.

### New type in Types.hs

```haskell
data ChangeSet = ChangeSet
  { csDescription :: !Text
  , csDiffs       :: [(FilePath, HsMutation)]
  }
```

### Oracle.hs: Multi-file prompt mode

New function `proposeChangeSet`:
- System prompt: "You are reviewing multiple related Haskell modules. Propose a
  coordinated change that improves the system as a whole. You may modify any or
  all of the modules below. Output unified diffs for each file, separated by
  file headers."
- User prompt: concatenated semantic contexts for 2-3 related modules
  (grouped by ModuleGraph adjacency — modules that import each other)
- Response format: multiple `--- a/path` / `+++ b/path` diff blocks
- Parse into `ChangeSet` (multiple diffs keyed by filepath)

### Diff parser extension

Extend `parseDiffResponse` to handle multi-file diffs:
- Split on `--- a/src/DGM/` or `--- src/DGM/` boundaries
- Each block becomes a separate `HsMutation` paired with its filepath
- Return `Either Text ChangeSet` instead of `Either Text HsMutation`

Fallback: if multi-file diff is unparseable, try treating entire response as
single-file diff (current behavior).

### Cycle.hs: Atomic multi-file pipeline

New `runChangeSetPipeline`:
1. Snapshot all affected files (like SelfModTxn but for N files)
2. Write all mutations
3. Run buildPreflight once
4. Run testSelf once
5. On success: commit all files in one git commit with combined description
6. On failure: rollback ALL files to snapshots

### SelfMod.hs: Module grouping

New `groupRelatedModules :: ModuleGraph -> [FilePath] -> [[FilePath]]`:
- Group files by mutual import relationships (transitive closure, capped at 3)
- Each group = 2-3 tightly-coupled modules
- One multi-file proposal per group (instead of per-file)

### Activation criteria

Multi-file mode activates when:
- Single-file mutations for a module have accumulated 3+ blacklist entries
- The file has high cross-module coupling (mgUsedBy count > 2)
- `--goal` explicitly targets cross-cutting concerns

Otherwise, single-file mode is used (it's more reliable and cheaper).

### Files touched
- Edit: `src/DGM/Types.hs` (add ChangeSet type)
- Edit: `src/DGM/Oracle.hs` (proposeChangeSet, multi-file diff parser)
- Edit: `src/DGM/OracleHandle.hs` (expose proposeChangeSetH)
- Edit: `src/DGM/Cycle.hs` (runChangeSetPipeline, atomic multi-file commit)
- Edit: `src/DGM/SelfMod.hs` (groupRelatedModules, dispatch to multi-file)
- Edit: `src/DGM/SelfCompile.hs` (multi-file SelfModTxn variant)
- Edit: `test/Spec.hs` (test multi-file diff parsing, module grouping)

---

## Stage 6: Quality-Aware Scoring

**Goal**: Score mutations by improvement quality, not just binary pass/fail.

### enrichScore extension in SelfCompile.hs

```haskell
enrichScoreV2
  :: Double          -- base score (pass/fail)
  -> Int -> Int      -- origLen, mutLen
  -> Maybe Text      -- LH result
  -> Maybe Int       -- complexity delta (negative = improvement)
  -> Maybe Int       -- coverage delta (positive = new tests added)
  -> Bool            -- is this mutation category novel for this file?
  -> Double

-- Bonuses (all additive, capped at 1.0):
-- sizeBonus:       0.05 * max(0, origLen - mutLen) / origLen  (existing)
-- lhBonus:         0.02 if "SAFE"                             (existing)
-- complexityBonus: 0.03 * max(0, -complexityDelta) / maxComplexity
-- coverageBonus:   0.04 if new tests added (coverage delta > 0)
-- noveltyBonus:    0.01 if mutation category is first for this file
```

### Complexity measurement

Before and after mutation:
- `extractFunctions` on original source → sum of fiComplexity
- `extractFunctions` on mutated source → sum of fiComplexity
- Delta = new - old (negative = improvement, triggers bonus)

### Test coverage measurement

Before and after mutation:
- Count test matches for functions in this file
- If mutated source adds new `testCase`/`testProperty` → positive delta
- This specifically rewards the TEST mutation category

### Accept threshold

Keep `score >= 1.0` as the commit threshold.
Bonuses don't change the threshold, but they DO affect archive scoring:
- Higher-quality mutations get higher archive scores
- RuleMiner pattern matching sees higher scores for quality improvements
- Future mutation ranking boosts files where quality improvements worked

### Files touched
- Edit: `src/DGM/SelfCompile.hs` (enrichScoreV2 with complexity/coverage bonuses)
- Edit: `src/DGM/Cycle.hs` (compute complexity/coverage deltas, pass to enrichScoreV2)
- Edit: `src/DGM/SemanticContext.hs` (export complexity scoring for before/after comparison)
- Edit: `test/Spec.hs` (test enrichScoreV2 bonus calculations)

---

## Stage 7: Meaningful Verification

**Goal**: Make the formal verification layer do real work.

### LiquidHaskell: Annotation-aware checking

Instead of only checking existing annotations (there are few/none), teach the
Oracle to ADD refinement type annotations as part of its mutations.

Add to the semantic prompt categories:
```
- TYPE_SAFETY: More precise types, replace partial functions with total ones.
  You may add LiquidHaskell {-@ ... @-} refinement type annotations to strengthen
  type safety. These will be verified by LiquidHaskell.
```

When LH is enabled and the mutation adds `{-@ ... @-}` annotations:
- LH verifies them against the code
- If SAFE: the mutation is formally verified (real LH bonus = 0.02)
- If UNSAFE: the annotation is wrong (reject, archive with failure reason)

### Property-based testing via QuickCheck (TEST category)

The Oracle can propose QuickCheck properties as part of TEST mutations:
```haskell
testProperty "numDependents is non-negative" $
  forAll arbitrary $ \mg fp ->
    numDependents mg fp >= 0
```

These get verified by `cabal test` in the pipeline. If they pass, the mutation
adds test coverage AND serves as a living specification.

### SBV: Keep skipped for source mutations

SBV verification remains skipped for source-level mutations (no ExprF
representation). It stays active for ExprPhase cycles where it has actual
expressions. This is the honest position — SBV can't meaningfully verify
text-level Haskell diffs.

### Files touched
- Edit: `src/DGM/Oracle.hs` (LH annotation mention in semantic prompt)
- Edit: `src/DGM/Liquid.hs` (detect added annotations in output)
- Edit: `test/Spec.hs` (tests)

---

## Implementation Order and Dependencies

```
Stage 1: SemanticContext          <-- Foundation (no dependencies)
    |
Stage 2: Semantic Prompting       <-- Depends on Stage 1
    |
Stage 3: Error Feedback Loop      <-- Independent (can parallel with Stage 2)
    |
Stage 4: Per-Test Analysis        <-- Enhances Stage 3
    |
Stage 5: Multi-File Change Sets   <-- Depends on Stages 1-4
    |
Stage 6: Quality Scoring          <-- Depends on Stage 1 (complexity)
    |
Stage 7: Meaningful Verification  <-- Depends on Stages 2, 6
```

**Recommended execution order**: 1 → 2+3 parallel → 4 → 6 → 5 → 7

Each stage is independently testable and deployable. After each stage, run
`--self-mod 3` to verify the pipeline still works and measure improvement
in mutation quality.

---

## Success Metrics

After full implementation, measure against current baseline:

| Metric | Current | Target |
|--------|---------|--------|
| Mutation types | Lint only (unused imports, sortBy→sortOn) | Algorithm, architecture, test, type safety |
| CPP module coverage | 0% → fixed to ~55% | 100% (all files eligible) |
| Oracle context | Raw source + line numbers | Type sigs + complexity + tests + call graph + failures |
| Oracle pass rate | ~30% | ~50% (semantic context reduces blind guessing) |
| Failure learning | None (blacklist only) | Feed back GHC errors, failed test names |
| Per-test granularity | Aggregate counts only | Individual test name + pass/fail |
| Multi-file refactors | Impossible | Supported (atomic apply/test/commit) |
| Test coverage mutations | None | Oracle can add QuickCheck properties |
| Scoring granularity | Binary pass/fail | Complexity + coverage + novelty bonuses |
| Formal verification | Hollow | Real LH checks on Oracle-proposed annotations |

---

## Files Summary (all stages)

**New files:**
- `src/DGM/SemanticContext.hs`

**Modified files:**
- `src/DGM/Types.hs` — `ArchiveEntry.entryFailureReason`, `ChangeSet` type
- `src/DGM/Oracle.hs` — `proposeMutationSemantic`, `proposeChangeSet`, new system prompts, multi-file parser
- `src/DGM/OracleHandle.hs` — expose new handle functions
- `src/DGM/OracleContext.hs` — `buildSemanticPrompt`
- `src/DGM/SelfMod.hs` — three-tier dispatch, `groupRelatedModules`
- `src/DGM/SelfCompile.hs` — `parseTestDetails`, `enrichScoreV2`, `crFailedTests`, multi-file txn
- `src/DGM/Cycle.hs` — failure capture, complexity/coverage deltas, `runChangeSetPipeline`
- `src/DGM/Archive.hs` — `getRecentFailures`, SQLite schema migration
- `src/DGM/Liquid.hs` — annotation detection
- `dgm.cabal` — add SemanticContext to exposed-modules
- `test/Spec.hs` — tests for all new functionality
