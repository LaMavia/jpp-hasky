module TypeChecker.Utils.BoundVars where

import qualified Abs
import qualified Data.Set       as Set
import           TypeChecker.TC (TC,
                                 Type (TCAny, TCApp, TCBound, TCData, TCInt, TCVar))

type BV a = a -> Set.Set String

stringOfLident :: Abs.LIdent -> String
stringOfLident (Abs.LIdent x) = x

bvOfTopDef :: (Show a) => BV (Abs.TopDef' a)
bvOfTopDef x = case x of
  Abs.TDDataV _ _ lidents _     -> Set.fromList $ stringOfLident <$> lidents
  Abs.TDDeclaration _ _ type_ _ -> bvOfType type_
  Abs.TDDataNV {}               -> Set.empty
  Abs.TDDeclarationNT {}        -> Set.empty

bvOfType :: (Show a) => BV (Abs.Type' a)
bvOfType x = case x of
  Abs.TVar {} -> Set.empty
  Abs.TApp {} -> Set.empty
  Abs.TType {} -> Set.empty
  Abs.TBound _ vs t -> Set.fromList (stringOfLident <$> vs) `Set.union` bvOfType t

-- tcBvOfType :: Type -> TC (Set.Set String)
-- tcBvOfType o = case o of
--   TCAny -> return Set.empty
--   TCInt -> return Set.empty
--   TCVar x -> return $ Set.singleton x
--   TCData _ xs _ _ -> return $ Set.fromList xs
--   TCBound xs t -> return $ Set.fromList xs `Set.union` tcBvOfType t
--   TCApp t -> _a

