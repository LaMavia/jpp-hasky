module Execution.Eval.Expr where

import           Abs                    (Expr, Expr' (ELit))
import           Execution.Eval.Literal (evalLiteral)
import           Runtime                (RT, RTVal, placeOfExpr, rtCatch)

evalExpr :: Expr -> RT RTVal
evalExpr e@(ELit _ lit) = rtCatch (placeOfExpr e) $ evalLiteral lit
