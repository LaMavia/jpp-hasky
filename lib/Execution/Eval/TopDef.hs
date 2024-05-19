module Execution.Eval.TopDef where

import           Abs
import           Common
import           Control.Monad.Reader (MonadReader (ask, local))
import           Execution.Eval.Expr  (evalExpr)
import           Runtime

evalTopDef :: TopDef -> RT RTEnv
evalTopDef p = uCatch (placeOfTopDef p) $ evalTopDefImpl p

evalTopDefImpl :: TopDef -> RT RTEnv
evalTopDefImpl (TDDataV {}) = ask

evalTopDefImpl (TDDataNV {}) =
  uThrow "unexpected non-generic data declaration"

evalTopDefImpl (TDDeclarationNT {}) =
  uThrow "unexpected typeless top definition"

evalTopDefImpl (TDDeclaration _ (LIdent name) _ e) = do
  env' <- allocEnv name
  () <- local (const env') $ evalExpr e >>= allocState name
  return env'
