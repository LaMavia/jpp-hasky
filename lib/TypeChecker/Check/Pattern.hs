{-# LANGUAGE QuasiQuotes #-}

module TypeChecker.Check.Pattern where
import qualified Abs
import           Control.Monad.Reader    (MonadReader (ask))
import           Data.String.Interpolate (i)
import           Debug.Trace             (trace)
import           Print                   (printTree)
import           TypeChecker.TC          (TCChecker, TCEnv, Type)
-- import           TypeChecker.Utils       (allocTCUnifier)


typeCheckPattern :: Type -> TCChecker Abs.Expr TCEnv
typeCheckPattern _t e = do
  env <- ask
  return (env, e)
  -- do
  -- u <- tcUnifyExpr e t
  -- env' <- allocTCUnifier u
  -- return (env', e)
