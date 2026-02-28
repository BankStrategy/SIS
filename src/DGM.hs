-- | DGM — Top-level re-export module for the Darwin Gödel Machine MVP.
--
-- Import this module for a single, convenient entry point to all public APIs.
module DGM
  ( -- * Re-exports
    module DGM.Types
  , module DGM.AST
  , module DGM.Rewriting
  , module DGM.Verification
  , module DGM.Sandbox
  , module DGM.HintBridge
  , module DGM.HsAST
  , module DGM.Archive
  , module DGM.Evolution
  , module DGM.AbsoluteZero
  , module DGM.SafetyKernel
  , module DGM.Cycle
  , module DGM.SelfMod
  , module DGM.SelfCompile
  , module DGM.ModGraph
  , module DGM.Reversal
  , module DGM.Oracle
  , module DGM.OracleHandle
  , module DGM.NatLang
  , module DGM.RuleMiner
  ) where

import DGM.Types
import DGM.AST
import DGM.Rewriting
import DGM.Verification
import DGM.Sandbox
import DGM.HintBridge
import DGM.HsAST
import DGM.Archive
import DGM.Evolution
import DGM.AbsoluteZero
import DGM.SafetyKernel
import DGM.Cycle
import DGM.SelfMod
import DGM.SelfCompile
import DGM.ModGraph
import DGM.Reversal
import DGM.Oracle
import DGM.OracleHandle
import DGM.NatLang
import DGM.RuleMiner
