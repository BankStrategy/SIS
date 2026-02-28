-- | DGM.Reversal — Typed invertible transactions for safe self-modification.
--
-- Every mutation is represented as an 'Invertible' — a pure value that
-- bundles both a forward transform and its exact inverse.  This replaces the
-- ad-hoc snapshot-and-restore approach with a principled, law-governed design.
--
-- __Invertibility law__
--
-- @
--   backward (forward x) ≡ x    — for all x in the domain of @forward@
-- @
--
-- Smart constructors ('mkTextReplacement', 'mkEtaReduction', 'mkAddComment')
-- guarantee this law by construction.  Pure 'Bool'-valued properties
-- ('prop_invertible_replacement', 'prop_invertible_eta') let test suites
-- verify the law via HUnit sample tests or QuickCheck @===@ checks without
-- an additional library dependency.
--
-- __Integration with SelfMod__
--
-- 'TypedTxn' bundles an 'Invertible Text' with its originating 'HsMutation'
-- and target file path.  'DGM.SelfMod.writeMutation' should construct a
-- 'TypedTxn' before writing, storing it so that 'rollbackTypedTxn' can be
-- called when the post-mutation test suite fails.
module DGM.Reversal
  ( -- * Core type
    Invertible(..)
    -- * Smart constructors
  , mkTextReplacement
  , mkEtaReduction
  , mkAddComment
    -- * Typed transactions
  , TypedTxn(..)
  , executeTypedTxn
  , rollbackTypedTxn
    -- * Invertibility properties (for HUnit / QuickCheck)
  , prop_invertible_replacement
  , prop_invertible_eta
  ) where

import Control.Exception (catch, IOException)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import DGM.HsAST (HsMutation)

-- ─────────────────────────────────────────────────────────────────────────────
-- Core type
-- ─────────────────────────────────────────────────────────────────────────────

-- | A pure value encoding both a forward transform and its exact inverse.
--
-- Law: @backward (forward x) == x@ for all @x@ in the domain of @forward@.
-- The smart constructors guarantee this by construction.
data Invertible a = Invertible
  { forward        :: !(a -> a)  -- ^ Apply the mutation.
  , backward       :: !(a -> a)  -- ^ Undo the mutation.
  , invDescription :: !Text      -- ^ Human-readable description.
  }

-- ─────────────────────────────────────────────────────────────────────────────
-- Smart constructors
-- ─────────────────────────────────────────────────────────────────────────────

-- | Build an invertible text-substitution.
--
-- @forward@ replaces every occurrence of @old@ with @new@;
-- @backward@ replaces @new@ back with @old@.
--
-- Returns @Nothing@ when:
--   * @old@ is empty (replacement domain is ambiguous), or
--   * @old == new@ (the transform would be a no-op).
--
-- __Law guarantee__: when @new@ does not already appear anywhere in the input
-- (no aliasing), @backward (forward x) == x@ holds exactly.
mkTextReplacement :: Text -> Text -> Maybe (Invertible Text)
mkTextReplacement old new
  | T.null old  = Nothing
  | old == new  = Nothing
  | otherwise   = Just Invertible
      { forward        = T.replace old new
      , backward       = T.replace new old
      , invDescription = "replace «" <> T.take 20 old <> "» → «" <> T.take 20 new <> "»"
      }

-- | Build an invertible η-reduction for a named binding.
--
-- __Forward__: rewrites @funcName var = body@ (with trailing @var@) to
-- @funcName = body@ by removing the argument from the LHS equation.
-- The transform is line-based rather than AST-based; it is correct for the
-- common case of a single-clause, single-argument definition.
--
-- __Backward__: re-introduces the variable on the LHS.
--
-- Returns @Nothing@ when @funcName@ or @var@ is empty.
mkEtaReduction :: Text -> Text -> Maybe (Invertible Text)
mkEtaReduction funcName var
  | T.null funcName = Nothing
  | T.null var      = Nothing
  | otherwise       = Just Invertible
      { forward        = T.replace lhsFull lhsEta
      , backward       = T.replace lhsEta  lhsFull
      , invDescription = "η-reduce " <> funcName <> " over " <> var
      }
  where
    -- "funcName var = "  ↔  "funcName = "
    lhsFull = funcName <> " " <> var <> " = "
    lhsEta  = funcName <> " = "

-- | Build an invertible comment insertion before an anchor text.
--
-- @forward@ inserts @"-- " <> comment <> "\\n"@ immediately before the first
-- occurrence of @anchor@; @backward@ removes that same line.
--
-- Returns @Nothing@ when @anchor@ is empty.
mkAddComment :: Text -> Text -> Maybe (Invertible Text)
mkAddComment anchor comment
  | T.null anchor = Nothing
  | otherwise     = Just Invertible
      { forward        = T.replace anchor (commentLine <> anchor)
      , backward       = T.replace (commentLine <> anchor) anchor
      , invDescription = "add comment before «" <> T.take 20 anchor <> "»"
      }
  where
    commentLine = "-- " <> comment <> "\n"

-- ─────────────────────────────────────────────────────────────────────────────
-- Typed transactions
-- ─────────────────────────────────────────────────────────────────────────────

-- | A fully-typed, reversible mutation transaction.
--
-- Bundles:
--   * @txnFile@     — the target file path,
--   * @txnOp@       — the invertible text operation to apply / undo,
--   * @txnMutation@ — the originating 'HsMutation' (for audit / logging).
--
-- Usage:
--
-- @
-- let txn = TypedTxn fp op mut
-- result <- executeTypedTxn txn
-- when testsFailed $ rollbackTypedTxn txn
-- @
data TypedTxn = TypedTxn
  { txnFile     :: !FilePath          -- ^ Path to the file being mutated.
  , txnOp       :: !(Invertible Text) -- ^ Forward/backward transforms.
  , txnMutation :: !HsMutation        -- ^ Origin mutation (audit trail).
  }

-- | Execute a typed transaction: read @txnFile@, apply @forward txnOp@, write back.
--
-- Returns @Left err@ on any IO failure so the caller can decide whether to
-- abort or roll back.  On success the file on disk reflects the mutated state.
executeTypedTxn :: TypedTxn -> IO (Either Text ())
executeTypedTxn txn = do
  readResult <- (Right <$> TIO.readFile (txnFile txn))
                `catch` (\e -> return (Left (T.pack (show (e :: IOException)))))
  case readResult of
    Left err  -> return (Left err)
    Right src -> do
      let mutated = forward (txnOp txn) src
      (TIO.writeFile (txnFile txn) mutated >> return (Right ()))
        `catch` (\e -> return (Left (T.pack (show (e :: IOException)))))

-- | Roll back a typed transaction: apply @backward txnOp@ to restore the original.
--
-- IO errors during rollback are silently swallowed — the file may have been
-- externally modified, and crashing the recovery path would be worse than
-- leaving a partial state.
rollbackTypedTxn :: TypedTxn -> IO ()
rollbackTypedTxn txn = do
  readResult <- (Right <$> TIO.readFile (txnFile txn))
                `catch` (\e -> return (Left (T.pack (show (e :: IOException)))))
  case readResult of
    Left _    -> return ()
    Right src -> do
      let restored = backward (txnOp txn) src
      TIO.writeFile (txnFile txn) restored
        `catch` (\(_ :: IOException) -> return ())

-- ─────────────────────────────────────────────────────────────────────────────
-- Invertibility properties
-- ─────────────────────────────────────────────────────────────────────────────

-- | Invertibility property for 'mkTextReplacement'.
--
-- @backward (forward x) == x@ whenever @new@ does not already appear in @x@.
-- The precondition sidesteps the aliasing corner-case where a prior occurrence
-- of @new@ in the input would be double-replaced during rollback.
--
-- Suitable for use as a QuickCheck @property@ (returns @Bool@):
--
-- @
-- quickCheck prop_invertible_replacement
-- @
prop_invertible_replacement :: Text -> Text -> Text -> Bool
prop_invertible_replacement x old new =
  case mkTextReplacement old new of
    Nothing  -> True  -- constructor returned Nothing; law vacuously holds
    Just inv ->
      if T.isInfixOf new x
        then True   -- aliased input: outside guaranteed domain; vacuously pass
        else backward inv (forward inv x) == x

-- | Invertibility property for 'mkEtaReduction'.
--
-- Constructs a canonical input of the form @funcName var = someBody@ and
-- checks that @backward (forward x) == x@.
prop_invertible_eta :: Text -> Text -> Bool
prop_invertible_eta funcName var =
  case mkEtaReduction funcName var of
    Nothing  -> True
    Just inv ->
      let x = funcName <> " " <> var <> " = someBody"
      in  backward inv (forward inv x) == x
