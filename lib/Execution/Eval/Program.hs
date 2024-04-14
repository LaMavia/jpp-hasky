module Execution.Eval.Program where

import           Abs
import           Runtime
import           Execution.Eval.TopDef

evalProgram :: RTEval Program ()
evalProgram (Program pos defs) s = mapM_ evalTopDef defs

