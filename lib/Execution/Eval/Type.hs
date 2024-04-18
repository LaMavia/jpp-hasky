module Execution.Eval.Type where

import qualified Abs
import qualified Runtime as RT

evalType :: Abs.Type -> RT.Type
evalType (Abs.TVar _ (Abs.LIdent name)) = RT.TVar name
evalType (Abs.TApp _ (Abs.UIdent name) args) = RT.TIdent name (evalType <$> args)
evalType (Abs.TType _ (Abs.UIdent name)) = RT.TIdent name []
evalType (Abs.TBound _ _ t) = evalType t
