# DGM Haskell — Full Implementation Plan

> **Decision already made:** SQLite for archive persistence. All other
> stated approaches below are based on study of the existing codebase.

---

## Current State

The MVP is complete and all 34 tests pass. `cabal run dgm -- 5` runs 5 full
Zero Data Cycles. The architecture is sound. What remains is replacing the five
stubs with real implementations, plus one correctness gap in the evolutionary
loop.

| Gap | File | Status |
|-----|------|--------|
| Archive in-memory only | `Archive.hs` | Stub (TVar list) |
| No real GHC-API eval | `Sandbox.hs` | CPP stub (`#ifdef WITH_HINT`) |
| No real SMT solver | `Verification.hs` | Bounded test oracle |
| Rewriting only at root | `Rewriting.hs` | Missing recursive traversal |
| Trivial quorum hash | `SafetyKernel.hs` | `simpleHash` (not crypto) |
| Proposer state not threaded | `Cycle.hs` / `AbsoluteZero.hs` | Static difficulty |

---

## Phase 1 — SQLite Archive Persistence (P0, implement first)

**Decision:** SQLite via `sqlite-simple` (already in the Haskell ecosystem, no
heavy ORM needed). The archive is the core memory of the system; everything
else depends on it surviving process restarts.

### What to change

**`dgm.cabal`** — add dependency:
```
sqlite-simple >= 0.4
```

Add a new cabal flag `with-sqlite` (default True) mirroring the `with-hint`
pattern, so CI without SQLite can still build.

**`src/DGM/Archive.hs`** — add a persistence layer alongside the existing STM
layer:

```haskell
-- New types
data ArchiveBackend
  = InMemory                    -- existing TVar backend
  | SQLiteBacked FilePath       -- new: path to .db file

-- New operations
openArchive  :: ArchiveBackend -> IO ArchiveHandle
closeArchive :: ArchiveHandle  -> IO ()
flushArchive :: ArchiveHandle  -> [ArchiveEntry] -> IO ()
loadArchive  :: ArchiveHandle  -> IO [ArchiveEntry]
```

Schema (one table, append-only):

```sql
CREATE TABLE IF NOT EXISTS archive_entries (
  id          TEXT PRIMARY KEY,
  code        TEXT NOT NULL,
  parent_id   TEXT,
  score       REAL NOT NULL,
  passed      INTEGER NOT NULL,   -- 0/1
  generation  INTEGER NOT NULL,
  counter_ex  TEXT                -- JSON, nullable
);
```

**`src/DGM/Types.hs`** — add `ArchiveEntry` `ToJSON`/`FromJSON` instances to
`DGM.Types` (moving them from `Archive.hs` orphans, fixing the existing
`-Worphans` warnings).

**`src/DGM/Cycle.hs`** — after each `archiveSuccess` / `archiveFailed` call,
flush to SQLite. On startup (`newAgentState`), load any persisted entries back
into the `TVar`.

**Integration point:** `newAgentState` gains an `ArchiveBackend` parameter. The
default in `app/Main.hs` becomes `SQLiteBacked "dgm-archive.db"`.

### Acceptance criteria
- `cabal run dgm -- 5 && cabal run dgm -- 5` — second run shows >0 entries
  already in archive at startup (stepping stones survive restart).
- `dgm-archive.db` exists after first run and grows monotonically.
- All 34 existing tests still pass (use `InMemory` backend in tests).

---

## Phase 2 — Recursive Rewriting via Cata (P1)

**This is the most impactful correctness fix.** `generateMutations` currently
only fires rules at the root of the expression. The DGM's evolutionary pressure
depends on exploring mutations throughout the tree.

### What to change

**`src/DGM/Rewriting.hs`** — replace `generateMutations`:

```haskell
-- Current (root-only):
generateMutations :: RuleSet -> Expr -> [(Expr, Mutation)]
generateMutations rules expr =
  mapMaybe (applyRule expr) rules

-- New (full tree via paramorphism):
generateMutations :: RuleSet -> Expr -> [(Expr, Mutation)]
generateMutations rules expr =
  concatMap (\(path, subexpr) ->
    mapMaybe (applyRuleAt path subexpr) rules)
  (subterms expr)
```

Use a paramorphism (`para`) to collect all subterms with their reconstruction
contexts (zipper-style), apply each rule at each position, and reconstruct the
full expression with the rule fired at that position.

Concrete approach — implement `Zipper` (or simpler: list of `(Expr, Expr ->
Expr)` pairs representing subterm + context) and rewrite via:

```haskell
-- Collect (subexpr, plug) pairs
subtermsWithCtx :: Expr -> [(Expr, Expr -> Expr)]
subtermsWithCtx = para $ \case
  -- Each functor position contributes its subterm + a plug function
  AppF (fe, fCtx) (xe, xCtx) ->
    (Fix (AppF fe xe), id)              -- root
    : [(e, \e' -> Fix (AppF e' xe)) | (e, _) <- fCtx]   -- left
    ++ [(e, \e' -> Fix (AppF fe e')) | (e, _) <- xCtx]  -- right
  ...
```

This is a standard zipper traversal. The generated mutation count grows
proportionally to tree depth — many more candidates to score.

**`src/DGM/Evolution.hs`** — no changes needed; `unfoldHypotheses` already
calls `generateMutations` and will benefit automatically.

### Acceptance criteria
- New test: `generateMutations` on `(2 + 3) * (x + 0)` finds both the
  constant-fold `(2+3) → 5` and the identity-elim `(x+0) → x`.
- 35 tests pass (one added).
- `cabal run dgm -- 10` shows more archive entries with non-zero scores
  (mutations now fire on subterms).

---

## Phase 3 — Real GHC-API Dynamic Eval via hint (P1)

The `with-hint` flag and CPP guard already exist. This phase wires it up fully.

### What to change

**`dgm.cabal`** — `hint >= 0.9` already listed under `if flag(with-hint)`. No
cabal change needed.

**`src/DGM/Sandbox.hs`** — complete `hintEval`:

```haskell
#ifdef WITH_HINT
hintEval :: SandboxConfig -> Text -> IO SandboxResult
hintEval cfg code = do
  let src = T.unpack code
  result <- race (delay (rlTimeoutMs (scLimits cfg))) (runHint cfg src)
  ...

runHint :: SandboxConfig -> String -> IO (Either String String)
runHint cfg src = do
  res <- runInterpreter $ do
    reset
    setImports (scImports cfg)
    when (scSafeOnly cfg) $ set [languageExtensions := [SafeHaskell]]
    interpret src (as :: String)
  pure $ case res of
    Left  err -> Left (displayException err)
    Right v   -> Right v
#endif
```

Key addition: `set [languageExtensions := [SafeHaskell]]` enforces Safe Haskell
module restrictions when `scSafeOnly = True`. This matches the SPEC.md §2.2.2
requirement for the Capability Sandbox tier.

**`src/DGM/Sandbox.hs`** — fix `runInSandbox` to use the `cfg` parameter
(currently unused, causing a warning):

```haskell
runInSandbox cfg code = do
  t0 <- getCurrentTime
#ifdef WITH_HINT
  result <- hintEval cfg code
#else
  result <- pureEval code
#endif
  t1 <- getCurrentTime
  let latMs = realToFrac (diffUTCTime t1 t0) * 1000
  pure result { srLatencyMs = latMs }
```

**Build gate:** `cabal build -f+with-hint` must compile without errors. The
test suite runs with `with-hint` disabled (no GHC in CI path); add an
integration test executable that enables the flag.

### Acceptance criteria
- `cabal build -f+with-hint` compiles clean.
- `cabal run dgm -f+with-hint -- 5` produces identical cycle output to the
  non-hint build (the `runExprInSandbox` path is preferred for AST-based work;
  hint path activates for `runInSandbox` with text code).
- Safe Haskell violations (e.g. `unsafePerformIO`) are rejected by the sandbox.

---

## Phase 4 — Real SMT via SBV (P2)

The bounded test oracle in `verifyEquivalence` is sound but incomplete. Full
SMT via Z3/SBV turns `Verified` from "no counter-example found in N tests" to
a genuine proof.

### What to change

**`dgm.cabal`** — add flag and dependency:

```
flag with-sbv
  description: Enable real SMT verification via SBV/Z3
  default:     False
  manual:      True

if flag(with-sbv)
  build-depends: sbv >= 10.0
  cpp-options: -DWITH_SBV
```

**`src/DGM/Verification.hs`** — replace the pure loop in `verifyEquivalence`
with an SBV symbolic check when `WITH_SBV` is defined:

```haskell
#ifdef WITH_SBV
import Data.SBV

verifyEquivalence :: EquivSpec -> IO VerificationResult
verifyEquivalence spec = do
  -- Build SBV symbolic expressions from esOriginal and esMutated
  result <- prove $ do
    inputs <- mkInputs (esInputDomain spec)   -- symbolic Int/Bool vars
    let lhs = toSBV inputs (esOriginal spec)
        rhs = toSBV inputs (esMutated spec)
    pure (lhs .== rhs)
  case result of
    ThmResult (Satisfiable _ _) ->
      pure (Falsifiable (extractCE result))
    ThmResult (Unsatisfiable _ _) ->
      pure (Verified "Q.E.D. via SBV/Z3")
    _ ->
      pure (VTimeout "SBV returned unknown/timeout")
#else
-- existing bounded oracle
#endif
```

**`toSBV`** — a catamorphism over `ExprF` to `SBV SInteger` / `SBV SBool`.
Only `LitF`, `BoolF`, `BinOpF` (`+`,`-`,`*`,`==`,`<`,`>`), `IfF`, and `VarF`
need symbolic representations. `LamF`/`LetF`/`AppF` remain outside scope for
the MVP SBV port (these would require higher-order symbolic reasoning).

**Fallback:** When the expression contains `LamF`/`AppF` (higher-order terms),
fall back to the bounded test oracle. Log `VTimeout "HOF: fell back to oracle"`.

### Acceptance criteria
- `cabal build -f+with-sbv` compiles (requires Z3 in PATH).
- `verifyEquivalence` on `2+3` vs `5` returns `Verified "Q.E.D. via SBV/Z3"`.
- `verifyEquivalence` on `x+1` vs `x` returns `Falsifiable` with a concrete
  counter-example from Z3.
- 34 existing tests pass with `with-sbv` disabled.

---

## Phase 5 — Proposer Difficulty Adaptation (P2)

**The bug:** `runCycleN` ignores the updated `Proposer` returned by
`runSelfPlayStep`. Each cycle restarts at `propCurrentDifficulty = 0.2`.
The system never adapts.

### What to change

**`src/DGM/Cycle.hs`** — thread `Proposer` state through the cycle loop:

```haskell
data CycleConfig = CycleConfig
  { ...
  , ccProposer :: TVar Proposer   -- was: embedded in SelfPlayConfig
  }

-- In runCycle:
runCycle :: CycleConfig -> IO [StepResult]
runCycle cfg = do
  prop <- atomically (readTVar (ccProposer cfg))
  let spCfg = (ccSelfPlayCfg cfg) { spProposer = prop }
  (newProp, solverResult) <- runSelfPlayStep spCfg
  atomically (writeTVar (ccProposer cfg) newProp)   -- commit updated difficulty
  ...

-- In runCycleN: no change needed (TVar handles it)
```

**`src/DGM/AbsoluteZero.hs`** — `runSelfPlayStep` already returns `(Proposer,
SolverResult)`. No change needed there.

**`app/Main.hs`** — initialize `ccProposer` from a TVar wrapping
`defaultProposer`.

### Acceptance criteria
- New test: after 5 self-play steps starting at difficulty 0.2, `propCurrentDifficulty`
  is > 0.2 (adaptation happened).
- `cabal run dgm -- 20` shows increasing task IDs over time (harder tasks
  proposed as capability grows).

---

## Phase 6 — Cryptographic QuorumProof (P3)

**Current:** `simpleHash` is a `mod 1000000` of character codes. Trivially
collides and forgeable.

### What to change

**`dgm.cabal`** — add:
```
cryptohash-sha256 >= 0.11
base16-bytestring >= 1.0
```

**`src/DGM/SafetyKernel.hs`** — replace `simpleHash`:

```haskell
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Base16 as B16
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

-- Real SHA-256 hash
simpleHash :: Text -> Text
simpleHash t =
  decodeUtf8 (B16.encode (SHA256.hash (encodeUtf8 t)))
```

`QuorumProof` generation (`mockQuorumProof`) becomes the production path for
single-party proofs. For multi-party quorum (SPEC.md §3.2), the type would
extend to carry `[Signature]` but this is out of MVP scope.

**`src/DGM/Types.hs`** — extend `QuorumProof` with a timestamp to prevent
replay attacks:

```haskell
data QuorumProof = QuorumProof
  { quorumHash      :: Text
  , quorumTimestamp :: Int64   -- POSIX seconds
  , quorumOperation :: Text    -- what this proof authorises
  } deriving (Show, Eq)
```

Update `verifyQuorum` to also check `|now - quorumTimestamp| < 300` (5-minute
window).

### Acceptance criteria
- Existing quorum tests pass with SHA-256 backend.
- A proof generated for operation "terminate" is rejected when presented for
  operation "write:/etc/passwd".
- A proof with timestamp 6 minutes old is rejected.

---

## Dependency Graph

```
Phase 1 (SQLite)         ← independent, implement first
    ↓
Phase 2 (recursive rw)  ← depends on nothing, implement in parallel with Phase 1
    ↓
Phase 5 (proposer)      ← depends on Phase 2 (more mutations → more accurate capability)
    ↓
Phase 3 (hint)          ← depends on Phase 1 (archive persists hint results)
Phase 4 (SBV)           ← depends on Phase 1 (archive persists proofs)
Phase 6 (crypto)        ← depends on nothing (pure replacement)
```

Recommended order: **1 → 2 → 5 → 6 → 3 → 4**

Phases 3 and 4 can be deferred — they require external tooling (GHC in PATH,
Z3 in PATH) and their stubs are already functional fallbacks.

---

## File Change Summary

| Phase | Files modified | New deps |
|-------|---------------|----------|
| 1 SQLite | `Archive.hs`, `Types.hs`, `Cycle.hs`, `dgm.cabal`, `app/Main.hs` | `sqlite-simple` |
| 2 Recursive rewrite | `Rewriting.hs`, `test/Spec.hs` | none |
| 3 hint GHC eval | `Sandbox.hs` | `hint` (already listed) |
| 4 SBV SMT | `Verification.hs`, `dgm.cabal` | `sbv` |
| 5 Proposer adapt | `Cycle.hs`, `Types.hs`?, `app/Main.hs` | none |
| 6 Crypto quorum | `SafetyKernel.hs`, `Types.hs`, `dgm.cabal` | `cryptohash-sha256`, `base16-bytestring` |

---

## Test Plan (per phase)

Each phase adds tests; the existing 34 must always stay green.

| Phase | New tests | Total |
|-------|-----------|-------|
| 1 | 3 (open/write/reload SQLite) | 37 |
| 2 | 2 (recursive mutations found, subterm count) | 39 |
| 3 | 2 (hint eval pass, Safe Haskell reject) — integration only | 39 |
| 4 | 2 (SBV Q.E.D., SBV counter-example) — integration only | 39 |
| 5 | 1 (capability adapts after N steps) | 40 |
| 6 | 2 (SHA-256 hash, timestamp replay reject) | 42 |

---

## Known Non-Goals (this plan)

- **LiquidHaskell type-checked refinements** — the runtime predicate approach
  in `Verification.hs` is the right MVP; full LiquidHaskell requires a
  separate GHC plugin toolchain.
- **Process-isolated sandbox (cgroups/seccomp)** — the `async`-timeout sandbox
  is adequate for the Haskell-expression language used here. Full OS
  sandboxing (Docker, gVisor) is infrastructure work, not Haskell code.
- **Multi-party quorum signatures** — Phase 6 gives cryptographic hashes;
  threshold signatures (SPEC.md §3.2) require key management infrastructure.
- **GHC `HsExpr` AST** — the `ExprF` language is intentionally simpler than
  GHC's full AST. Replacing it is a complete rewrite, not an incremental step.
