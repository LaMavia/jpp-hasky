module Execution.Eval.Program where

import           Abs                   (Program, Program' (Program))
import           Common
import           Control.Monad         (void)
import           Execution.Eval.TopDef (evalTopDef)
import           Preprocessor.Stdlib   (runPrelude)
import           Runtime

evalProgram :: Program -> RT ()
evalProgram p@(Program _ defs) =
  uCatch (placeOfProgram p) $ void $ do
    envSeq (runPrelude : (evalTopDef <$> defs))


