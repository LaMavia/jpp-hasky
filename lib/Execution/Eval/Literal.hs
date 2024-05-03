module Execution.Eval.Literal where
import           Abs     (Literal, Literal' (LInt))
import           Common  (placeOfLiteral, uCatch)
import           Runtime (RT, RTVal (RTInt))

evalLiteral :: Literal -> RT RTVal
evalLiteral l@(LInt _ n) =
  uCatch (placeOfLiteral l) (return $ RTInt n)
