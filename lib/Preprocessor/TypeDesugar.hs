module Preprocessor.TypeDesugar where
import qualified Data.Map.Strict as Map
import           TypeChecker.TC  (Type (..))

typeDesugar :: Type -> Type
typeDesugar (TCApp "Fn" ts) =
  let ts' = typeDesugar <$> ts
  in foldr1 (\t u -> TCApp "Fn" [t, u]) ts'
typeDesugar (TCApp t ts) =
  TCApp t $ typeDesugar <$> ts
typeDesugar (TCBound args t) = TCBound args $ typeDesugar t
typeDesugar TCAny = TCAny
typeDesugar v@(TCVar {}) = v
typeDesugar (TCData t args dataMap c) =
  let dataMap' = Map.map (fmap typeDesugar) dataMap
  in TCData t args dataMap' c
