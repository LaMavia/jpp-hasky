module TypeChecker.Check.Pattern where
import qualified Abs
import           TypeChecker.TC          (TCChecker, TCEnv, Type)
import           TypeChecker.Utils.Unify (allocETUnifier, etUnify)


typeCheckPattern :: Type -> TCChecker Abs.Expr TCEnv
typeCheckPattern t e = do
  u <- etUnify e t
  env' <- allocETUnifier u
  return (env', e)
