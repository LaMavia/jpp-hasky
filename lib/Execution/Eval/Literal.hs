module Execution.Eval.Literal where
import           Abs     (Literal, Literal' (LInt))
import           Runtime (RT, RTVal (RTInt), placeOfLiteral, rtCatch)

evalLiteral :: Literal -> RT RTVal
evalLiteral l@(LInt _ n) =
  rtCatch (placeOfLiteral l) (return $ RTInt n)
