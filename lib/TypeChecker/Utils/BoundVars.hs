module TypeChecker.Utils.BoundVars where

import qualified Abs
import           Common         (unions)
import           Data.List      (singleton, union)
import qualified TypeChecker.TC as TC
import           TypeChecker.TC (TC)

type BV a = a -> [String]

stringOfLident :: Abs.LIdent -> String
stringOfLident (Abs.LIdent x) = x

bvOfTopDef :: (Show a) => BV (Abs.TopDef' a)
bvOfTopDef x = case x of
  Abs.TDDataV _ _ lidents _     -> unions $ singleton . stringOfLident <$> lidents
  Abs.TDDeclaration _ _ type_ _ -> bvOfType type_
  Abs.TDDataNV {}               -> []
  Abs.TDDeclarationNT {}        -> []

bvOfType :: (Show a) => BV (Abs.Type' a)
bvOfType x = case x of
  Abs.TVar {}       -> []
  Abs.TApp {}       -> []
  Abs.TType {}      -> []
  Abs.TBound _ vs t -> unions (singleton . stringOfLident <$> vs) `union` bvOfType t

tcBvOfType :: TC.Type -> TC [String]
tcBvOfType o = case o of
  TC.TCAny           -> return []
  TC.TCVar x         -> return [x]
  TC.TCData _ xs _ _ -> return xs
  TC.TCBound xs t    -> union xs <$> tcBvOfType t
  TC.TCApp _ ts      -> unions <$> mapM tcBvOfType ts

