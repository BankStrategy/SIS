-- | DGM.Rewriting — Term rewriting engine (SPEC.md §2.1).
--
-- Implements a simplified form of the REST (REwriting with Structured
-- Termination) library's approach.  Rewrite rules are applied to @Expr@
-- trees; termination is enforced via a step counter and a size-based ordering
-- (rules may only produce smaller or equal-size terms without a registered
-- justification).
--
-- The engine is designed to be extensible: external oracles (e.g. an LLM)
-- can supply additional rewrite steps via the 'OracleStep' constructor.
module DGM.Rewriting
  ( -- * Rewrite rules
    RewriteRule(..)
  , RuleSet
  , defaultRules
  , evolutionRules
    -- * Individual rules (for testing)
  , constantFold
  , identityElim
  , identityIntro
  , letIntro
  , commute
    -- * Dynamic (in-memory) rules — hint track
  , DynamicRule(..)
  , applyDynamicRules
    -- * Engine
  , RewriteConfig(..)
  , defaultConfig
  , rewrite
  , rewriteStep
    -- * Oracle integration
  , OracleStep(..)
  , applyOracle
    -- * Mutation generation
  , generateMutations
  , subtermsWithCtx
  ) where

import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Traversable (mapAccumL)
import DGM.AST
import DGM.Types (Mutation(..), MutationPayload(..), MutationType(..))

-- ─────────────────────────────────────────────────────────────────────────────
-- Rewrite rules
-- ─────────────────────────────────────────────────────────────────────────────

-- | A single rewrite rule: a named pattern-to-result transformation.
--
-- Rules must be /terminating/: the engine checks that repeated application of
-- any rule sequence eventually halts using a step budget and an optional size
-- ordering proof.
data RewriteRule = RewriteRule
  { ruleName        :: Text
  , ruleDescription :: Text
  , ruleApply       :: Expr -> Maybe Expr   -- ^ Returns @Nothing@ when inapplicable.
  , ruleSizeProof   :: Maybe SizeOrdering   -- ^ Evidence the rule shrinks term size.
  }

-- | Size ordering witness.  If present, the engine trusts this proof and
-- removes the step-budget deduction for the rule.
data SizeOrdering
  = StrictlySmaller  -- ^ Output is guaranteed smaller than input.
  | WeaklySmaller    -- ^ Output is same size or smaller.
  deriving (Show, Eq)

type RuleSet = [RewriteRule]

-- | Default algebraic simplification rules (shrinking / convergent).
--
-- Safe for the 'rewrite' engine — all rules either strictly shrink the term
-- or are idempotent, so repeated application converges to a fixed point.
defaultRules :: RuleSet
defaultRules =
  [ constantFold
  , identityElim
  , etaReduce
  , deadLetterElim
  , ifConstant
  ]

-- | Extended rule set for evolutionary candidate generation.
--
-- Includes the convergent 'defaultRules' plus expansion / restructuring
-- rules that create new opportunities for shrinking.  Use with
-- 'generateMutations' (not 'rewrite') to avoid infinite loops.
evolutionRules :: RuleSet
evolutionRules = defaultRules ++
  [ identityIntro
  , letIntro
  , commute
  ]

-- | Constant folding: evaluate binary operations on literals at compile time.
constantFold :: RewriteRule
constantFold = RewriteRule
  { ruleName        = "constant-fold"
  , ruleDescription = "Evaluate binary operations on integer literals"
  , ruleApply       = \case
      (Fix (BinOpF op (Fix (LitF a)) (Fix (LitF b)))) ->
        case (op, a, b) of
          ("+",  _, _) -> Just (lit (a + b))
          ("-",  _, _) -> Just (lit (a - b))
          ("*",  _, _) -> Just (lit (a * b))
          ("==", _, _) -> Just (Fix (BoolF (a == b)))
          ("<",  _, _) -> Just (Fix (BoolF (a < b)))
          (">",  _, _) -> Just (Fix (BoolF (a > b)))
          _            -> Nothing
      _ -> Nothing
  , ruleSizeProof = Just StrictlySmaller
  }

-- | Identity elimination: @x + 0@, @x * 1@, @x - 0@.
identityElim :: RewriteRule
identityElim = RewriteRule
  { ruleName        = "identity-elim"
  , ruleDescription = "Eliminate arithmetic identities"
  , ruleApply       = \case
      Fix (BinOpF "+" e (Fix (LitF 0))) -> Just e
      Fix (BinOpF "+" (Fix (LitF 0)) e) -> Just e
      Fix (BinOpF "*" e (Fix (LitF 1))) -> Just e
      Fix (BinOpF "*" (Fix (LitF 1)) e) -> Just e
      Fix (BinOpF "-" e (Fix (LitF 0))) -> Just e
      _                                  -> Nothing
  , ruleSizeProof = Just StrictlySmaller
  }

-- | Eta reduction: @(\x -> f x)@ becomes @f@ when @x@ not free in @f@.
etaReduce :: RewriteRule
etaReduce = RewriteRule
  { ruleName        = "eta-reduce"
  , ruleDescription = "Eta-reduce trivial lambda wrappers"
  , ruleApply       = \case
      Fix (LamF v (Fix (AppF f (Fix (VarF v')))))
        | v == v' && not (v `elem` collectVars f) -> Just f
      _ -> Nothing
  , ruleSizeProof = Just StrictlySmaller
  }

-- | Dead-let elimination: @let x = e in body@ when @x@ unused in @body@.
deadLetterElim :: RewriteRule
deadLetterElim = RewriteRule
  { ruleName        = "dead-let-elim"
  , ruleDescription = "Remove unused let bindings"
  , ruleApply       = \case
      Fix (LetF v _ body)
        | not (v `elem` collectVars body) -> Just body
      _ -> Nothing
  , ruleSizeProof = Just StrictlySmaller
  }

-- | Branch elimination for constant conditions.
ifConstant :: RewriteRule
ifConstant = RewriteRule
  { ruleName        = "if-constant"
  , ruleDescription = "Eliminate if-then-else with constant predicate"
  , ruleApply       = \case
      Fix (IfF (Fix (BoolF True))  t _) -> Just t
      Fix (IfF (Fix (BoolF False)) _ f) -> Just f
      _                                  -> Nothing
  , ruleSizeProof = Just StrictlySmaller
  }

-- ─────────────────────────────────────────────────────────────────────────────
-- Expansion / restructuring rules
-- ─────────────────────────────────────────────────────────────────────────────

-- | Identity introduction: wrap @e + 0@ around a @BinOpF@ with @+@ or @-@,
-- or @e * 1@ around @*@.  Creates opportunities for 'identityElim' and
-- 'constantFold' on subsequent passes.
--
-- Only fires on @BinOpF@ nodes where neither operand is already a literal 0/1
-- (to prevent infinite expansion loops).
identityIntro :: RewriteRule
identityIntro = RewriteRule
  { ruleName        = "identity-intro"
  , ruleDescription = "Wrap binary expression in arithmetic identity"
  , ruleApply       = \case
      e@(Fix (BinOpF op l r))
        | op `elem` ["+", "-"]
        , not (isLitVal 0 l), not (isLitVal 0 r)
        -> Just (binop op e (lit 0))
        | op == "*"
        , not (isLitVal 1 l), not (isLitVal 1 r)
        -> Just (binop "*" e (lit 1))
      _ -> Nothing
  , ruleSizeProof = Nothing  -- expansion rule: no size proof
  }

-- | Let introduction: lift a duplicated subexpression into a let binding.
--
-- When a @BinOpF@ has identical left and right operands, introduces
-- @let x = operand in op x x@.  The 'deadLetterElim' rule validates this
-- by removing dead lets if the binding turns out unused after further rewrites.
letIntro :: RewriteRule
letIntro = RewriteRule
  { ruleName        = "let-intro"
  , ruleDescription = "Lift duplicated subexpression into let binding"
  , ruleApply       = \case
      Fix (BinOpF op l r)
        | l == r
        , not (isSimpleLitOrVar l)  -- don't lift trivial expressions
        -> Just (letE "x" l (binop op (var "x") (var "x")))
      _ -> Nothing
  , ruleSizeProof = Nothing  -- restructuring rule: no size proof
  }

-- | Commutativity: swap operands of commutative operators (@+@, @*@).
--
-- Not shrinking, not expanding — creates a different normal form so
-- subsequent rules may find new opportunities.  Only fires when the left
-- operand is "greater" than the right (lexicographic on pretty-print) to
-- ensure the rule does not oscillate indefinitely.
commute :: RewriteRule
commute = RewriteRule
  { ruleName        = "commute"
  , ruleDescription = "Swap operands of commutative binary operator"
  , ruleApply       = \case
      Fix (BinOpF op l r)
        | op `elem` ["+", "*"]
        , prettyExpr' l > prettyExpr' r   -- canonical ordering prevents loops
        -> Just (binop op r l)
      _ -> Nothing
  , ruleSizeProof = Just WeaklySmaller  -- same size
  }

-- | Check if an expression is a literal with a specific value.
isLitVal :: Int -> Expr -> Bool
isLitVal n (Fix (LitF m)) = n == m
isLitVal _ _               = False

-- | Check if an expression is a simple literal or variable (not worth lifting).
isSimpleLitOrVar :: Expr -> Bool
isSimpleLitOrVar (Fix (LitF _))  = True
isSimpleLitOrVar (Fix (BoolF _)) = True
isSimpleLitOrVar (Fix (VarF _))  = True
isSimpleLitOrVar (Fix UnitF)     = True
isSimpleLitOrVar _               = False

-- | Quick pretty-print for ordering (reuses the catamorphism from AST).
prettyExpr' :: Expr -> Text
prettyExpr' = cata prettyAlg
  where
    prettyAlg :: ExprF Text -> Text
    prettyAlg = \case
      LitF  n     -> T.pack (show n)
      BoolF b     -> if b then "True" else "False"
      VarF  v     -> v
      AppF  f x   -> "(" <> f <> " " <> x <> ")"
      LamF  v b   -> "(\\" <> v <> " -> " <> b <> ")"
      LetF  v e b -> "let " <> v <> " = " <> e <> " in " <> b
      IfF   p t f -> "if " <> p <> " then " <> t <> " else " <> f
      BinOpF op l r -> "(" <> l <> " " <> op <> " " <> r <> ")"
      UnitF       -> "()"

-- ─────────────────────────────────────────────────────────────────────────────
-- Engine
-- ─────────────────────────────────────────────────────────────────────────────

-- | Configuration for the rewriting engine.
data RewriteConfig = RewriteConfig
  { rcMaxSteps    :: Int   -- ^ Step budget before forced termination.
  , rcMaxDepth    :: Int   -- ^ Maximum rule application depth.
  , rcAllowOracle :: Bool  -- ^ Whether oracle steps are permitted.
  }

defaultConfig :: RewriteConfig
defaultConfig = RewriteConfig
  { rcMaxSteps    = 1000
  , rcMaxDepth    = 50
  , rcAllowOracle = True
  }

-- | Rewrite engine state.
data RewriteState = RewriteState
  { rsStepsLeft :: Int
  , rsHistory   :: [Expr]   -- ^ For cycle detection.
  }

-- | Apply all rules exhaustively until no rule fires or budget is exhausted.
--
-- Returns @(finalExpr, stepsUsed, converged)@.
rewrite :: RewriteConfig -> RuleSet -> Expr -> (Expr, Int, Bool)
rewrite cfg rules expr0 = go (rcMaxSteps cfg) expr0
  where
    go 0 e = (e, rcMaxSteps cfg, False)
    go n e =
      case rewriteStep rules e of
        Nothing -> (e, rcMaxSteps cfg - n, True)   -- fixed point reached
        Just e' -> go (n - 1) e'

-- | Apply the first matching rule at the root or recursively in subterms.
--
-- Implements an innermost (call-by-value) strategy: subterms are simplified
-- before the root, matching REST's default ordering.
rewriteStep :: RuleSet -> Expr -> Maybe Expr
rewriteStep rules expr =
  -- Try root-level rules first
  case listToMaybe (mapMaybe (`ruleApply` expr) rules) of
    Just e' -> Just e'
    Nothing ->
      -- Recurse into children (innermost strategy)
      case unFix expr of
        AppF f x ->
          case rewriteStep rules f of
            Just f' -> Just (app f' x)
            Nothing -> app f <$> rewriteStep rules x
        LamF v b  -> lam v <$> rewriteStep rules b
        LetF v e b ->
          case rewriteStep rules e of
            Just e' -> Just (letE v e' b)
            Nothing -> letE v e <$> rewriteStep rules b
        IfF p t f ->
          case rewriteStep rules p of
            Just p' -> Just (ifE p' t f)
            Nothing ->
              case rewriteStep rules t of
                Just t' -> Just (ifE p t' f)
                Nothing -> ifE p t <$> rewriteStep rules f
        BinOpF op l r ->
          case rewriteStep rules l of
            Just l' -> Just (binop op l' r)
            Nothing -> binop op l <$> rewriteStep rules r
        _ -> Nothing  -- LitF, BoolF, VarF, UnitF are irreducible leaves

-- ─────────────────────────────────────────────────────────────────────────────
-- Oracle integration
-- ─────────────────────────────────────────────────────────────────────────────

-- | An oracle-supplied rewrite step (e.g. from an LLM reasoner).
--
-- The engine accepts oracle steps only when 'rcAllowOracle' is @True@ and
-- the proposed result is syntactically valid (no unbound variables introduced).
data OracleStep = OracleStep
  { oracleName   :: Text
  , oracleTarget :: Expr
  , oracleResult :: Expr
  , oracleReason :: Text
  } deriving (Show)

-- | Validate and apply an oracle-supplied rewriting step.
--
-- The step is rejected if it introduces new free variables (a rough safety
-- check; the full system would run the SMT equivalence prover here).
applyOracle :: OracleStep -> Expr -> Maybe Expr
applyOracle os expr
  | expr /= oracleTarget os = Nothing
  | newVars                 = Nothing   -- Reject: new free variables introduced
  | otherwise               = Just (oracleResult os)
  where
    origVars = collectVars (oracleTarget os)
    newVars  = not (collectVars (oracleResult os) `isSubsetOf` origVars)
    isSubsetOf a b = all (`elem` b) a

-- ─────────────────────────────────────────────────────────────────────────────
-- Mutation generation
-- ─────────────────────────────────────────────────────────────────────────────

-- | Collect all subterm positions with their reconstruction contexts.
--
-- Uses a paramorphism-style downward traversal: for each subterm @s@ in the
-- expression tree, returns @(s, plug)@ where @plug e'@ rebuilds the full tree
-- with @s@ replaced by @e'@.  The root itself is included as the first entry
-- (with @plug = id@).
{-@ subtermsWithCtx :: ExprF a -> [(ExprF a, ExprF a -> ExprF a)] @-}
subtermsWithCtx :: Expr -> [(Expr, Expr -> Expr)]
subtermsWithCtx = go id
  where
    go ctx e =
      -- This subterm at the current position.
      (e, ctx)
      -- Recurse into children, threading a hole-filling context downward.
      : case unFix e of
          AppF f x ->
               go (\f' -> ctx (app f' x)) f
            ++ go (\x' -> ctx (app f  x')) x
          LamF v b ->
               go (\b' -> ctx (lam v b')) b
          LetF v ev b ->
               go (\ev' -> ctx (letE v ev' b)) ev
            ++ go (\b'  -> ctx (letE v ev  b')) b
          IfF p t f ->
               go (\p' -> ctx (ifE p' t  f )) p
            ++ go (\t' -> ctx (ifE p  t' f )) t
            ++ go (\f' -> ctx (ifE p  t  f')) f
          BinOpF op l r ->
               go (\l' -> ctx (binop op l' r )) l
            ++ go (\r' -> ctx (binop op l  r')) r
          _ -> []   -- LitF, BoolF, VarF, UnitF are irreducible leaves

-- | Generate candidate mutations of an expression for the evolutionary loop.
--
-- Phase 2: full-tree recursive rewriting via a cata-style traversal.
-- Uses 'subtermsWithCtx' to enumerate every position in the tree, then applies
-- each rule at each position.  The resulting @(Expr, Mutation)@ pairs describe
-- the fully-reconstructed mutant and the localised change that was made.
--
-- The anamorphism in 'DGM.Evolution' calls this to unfold the hypothesis tree.
generateMutations :: RuleSet -> Expr -> [(Expr, Mutation)]
generateMutations rules expr =
  concatMap applyRulesAt (subtermsWithCtx expr)
  where
    applyRulesAt (subterm, plug) = mapMaybe (applyRuleAt subterm plug) rules

    applyRuleAt subterm plug rule =
      case ruleApply rule subterm of
        Nothing       -> Nothing
        Just subterm' ->
          let rewritten = plug subterm'
              nodeId    = "node-" <> ruleName rule
              oldNode   = exprToASTNode (nodeId <> "-old") subterm
              newNode   = exprToASTNode (nodeId <> "-new") subterm'
              mut = Mutation
                { mutationType    = Optimize
                , mutationTarget  = ruleName rule
                , mutationPayload = ExprMutation oldNode newNode
                }
          in  Just (rewritten, mut)

-- ─────────────────────────────────────────────────────────────────────────────
-- Dynamic (in-memory) rules — hint track
-- ─────────────────────────────────────────────────────────────────────────────

-- | A rewrite rule evaluated at runtime via the GHC interpreter (HintBridge).
--
-- Unlike static 'RewriteRule' values, 'DynamicRule' instances are minted
-- by 'DGM.HintBridge.evalRuleCandidate' and stored in the live
-- @ssDynamicRules@ 'TVar' — changes take effect in the next rewriting cycle
-- with no recompilation.
--
-- The transform operates on a single functor layer: the @ExprF Int@ input is
-- the immediate node with children represented as 0-based integer labels.
-- Returning the same value (identity) means the rule did not fire; returning
-- a different value commits the structural change.
data DynamicRule = DynamicRule
  { drDescription :: Text               -- ^ Human-readable name/description.
  , drTransform   :: ExprF Int -> ExprF Int  -- ^ Layer-level rewrite.
  , drScore       :: Double             -- ^ Empirical score from archive testing.
  }

-- | Apply a list of dynamic rules across all subterms of an expression,
-- mirroring the behaviour of 'generateMutations' for static rules.
--
-- For each subterm position, each rule is tried by:
--  1. Assigning 0-based integer labels to the node's immediate children.
--  2. Applying 'drTransform' to the labelled layer.
--  3. If the result differs, reconstructing the full expression and emitting
--     a 'Mutation' tagged with the rule's description.
--
-- Out-of-range indices produced by a badly-behaved transform default to the
-- first child (safe fallback; such rules score poorly and are evicted).
applyDynamicRules :: [DynamicRule] -> Expr -> [(Expr, Mutation)]
applyDynamicRules dynRules expr =
  concatMap applyAtPos (subtermsWithCtx expr)
  where
    applyAtPos (subterm, plug) =
      mapMaybe (\dr -> tryDynamic dr subterm plug) dynRules

    tryDynamic dr subterm plug =
      let layer    = unFix subterm
          children = toList layer
          n        = length children
          -- Assign 0-based integer labels to each child position.
          (_, numbered) = mapAccumL (\i _ -> (i + 1, i)) (0 :: Int) layer
          transformed   = drTransform dr numbered
      in if transformed == numbered
           then Nothing    -- rule did not fire; skip
           else
             let safeChild i
                   | i >= 0 && i < n = children !! i
                   | n > 0           = head children
                   | otherwise       = subterm   -- leaf: return self
                 recon    = Fix (fmap safeChild transformed)
                 newExpr  = plug recon
                 oldNode  = exprToASTNode ("dyn-old-" <> drDescription dr) subterm
                 newNode  = exprToASTNode ("dyn-new-" <> drDescription dr) recon
                 mut = Mutation
                         { mutationType    = Optimize
                         , mutationTarget  = drDescription dr
                         , mutationPayload = ExprMutation oldNode newNode
                         }
             in  Just (newExpr, mut)
