module TypeChecker.Check.Arg where

import qualified Abs
import           Common                 (placeOfArg, uCatch)
import           TypeChecker.Check.Type (typeCheckType)
import           TypeChecker.TC         (TCChecker, Type)

typeCheckArg :: TCChecker Abs.Arg Type
typeCheckArg a = uCatch (placeOfArg a) $ typeCheckArgImpl a

typeCheckArgImpl :: TCChecker Abs.Arg Type
typeCheckArgImpl (Abs.Arg pos xident t) = do
  (tType, t') <- typeCheckType t
  return (tType, Abs.Arg pos xident t')

