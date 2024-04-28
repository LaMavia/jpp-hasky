module Execution.Eval.TopDef where

import           Abs
import           Control.Monad.Reader        (MonadReader (local))
import qualified Data.Map.Strict             as Map
import           Execution.Eval.Expr         (evalExpr)
import           Execution.Eval.Type         (evalType)
import           Runtime
import           TypeChecker.Utils.BoundVars (stringOfLident)

dataEntryOfConstructor :: Constructor -> (String, DataConstr)
dataEntryOfConstructor (Constructor _ (UIdent name) args) =
  (name, DConstr $ evalType <$> args)
dataEntryOfConstructor (NullaryConstr _ (UIdent name)) = (name, DConstr [])

evalTopDef :: TopDef -> RT RTEnv
evalTopDef p = rtCatch (placeOfTopDef p) $ evalTopDefImpl p

evalTopDefImpl :: TopDef -> RT RTEnv
evalTopDefImpl (TDDataV _ (UIdent dataName) args constrs) = do
  let cs = dataEntryOfConstructor <$> constrs
  let constructorMap = Map.fromList cs
  alloc dataName (RTData dataName (stringOfLident <$> args) constructorMap)

evalTopDefImpl (TDDataNV {}) =
  rtThrow "unexpected non-generic data declaration"

evalTopDefImpl (TDDeclarationNT {}) =
  rtThrow "unexpected typeless top definition"

evalTopDefImpl (TDDeclaration _ (LIdent name) _ e) = do
  env' <- allocEnv name
  () <- local (const env') $ evalExpr e >>= allocState name
  return env'
