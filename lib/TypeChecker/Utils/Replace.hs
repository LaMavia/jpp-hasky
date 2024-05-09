module TypeChecker.Utils.Replace where
import           Data.Functor   ((<&>))
import qualified Data.Set       as Set
import           TypeChecker.TC (TC,
                                 Type (TCAny, TCApp, TCBound, TCData, TCInt, TCVar),
                                 getVar, isDefined)

replace :: Set.Set String -> Type -> TC Type
replace s (TCVar x) | x `Set.member` s = do
  isXDefined <- isDefined x
  if isXDefined then getVar x else return TCAny

replace _ v@(TCVar {}) = return v

replace s (TCBound vs t) =
  replace (s `Set.difference` Set.fromList vs) t

replace s (TCApp t ts) = mapM (replace s) ts <&> TCApp t

replace _ TCInt = return TCInt
replace _ TCAny = return TCAny
replace _ d@(TCData {}) = return d

