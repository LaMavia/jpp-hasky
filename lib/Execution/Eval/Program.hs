module Execution.Eval.Program where

import           Abs                   (Program, Program' (Program))
import           Execution.Eval.TopDef (evalTopDef)
import           Runtime.RTError       (placeOfProgram, rtCatch)
import           Runtime.RTEval        (RTEval)

evalProgram :: RTEval Program ()
evalProgram p@(Program _ defs) =
  rtCatch (placeOfProgram p) (mapM_ evalTopDef defs)
