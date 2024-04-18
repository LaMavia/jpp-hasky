module Execution.Eval.TopDef where

import           Abs
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
evalTopDef p@(TDDataV _ (UIdent dataName) args constrs) =
  rtCatch (placeOfTopDef p) $ do
    let cs = dataEntryOfConstructor <$> constrs
    let constructorMap = Map.fromList cs
    alloc dataName (RTData dataName (stringOfLident <$> args) constructorMap)

evalTopDef p@(TDDataNV {}) =
  rtCatch (placeOfTopDef p) $ rtThrow "unexpected non-generic data declaration"

evalTopDef p@(TDDeclarationNT {}) =
  rtCatch (placeOfTopDef p) $ rtThrow "unexpected typeless top definition"

evalTopDef p@(TDDeclaration _ (LIdent name) _ e) =
  rtCatch (placeOfTopDef p) $ do
    env <- allocEnv name
    val <- evalExpr e
    allocState name val
    return env
