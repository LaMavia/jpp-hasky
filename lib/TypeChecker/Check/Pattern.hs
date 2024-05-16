{-# LANGUAGE QuasiQuotes #-}

module TypeChecker.Check.Pattern where
import qualified Abs
import           Control.Monad.State     (MonadState (get))
import           Data.String.Interpolate (i)
import           Debug.Trace             (trace)
import           Print                   (printTree)
import           TypeChecker.TC          (TCChecker, TCEnv, Type)
import           TypeChecker.Utils       (allocTCUnifier, tcUnifyExpr)


typeCheckPattern :: Type -> TCChecker Abs.Expr TCEnv
typeCheckPattern t e = do
  u <- tcUnifyExpr e t
  env' <- allocTCUnifier u
  return (env', e)
