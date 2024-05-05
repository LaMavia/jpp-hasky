module TypeChecker.Check.Constructor where
import           Abs                    (Constructor,
                                         Constructor' (Constructor, NullaryConstr),
                                         UIdent (UIdent))
import           Common                 (placeOfConstructor, uCatch)
import           Control.Monad          (mapAndUnzipM)
import           TypeChecker.Check.Type (typeCheckType)
import           TypeChecker.TC         (TCChecker, Type)

typeCheckConstructor :: TCChecker Constructor (String, [Type])
typeCheckConstructor c = uCatch (placeOfConstructor c) (typeCheckConstructorImpl c)

typeCheckConstructorImpl :: TCChecker Constructor (String, [Type])
typeCheckConstructorImpl (Constructor pos (UIdent name) argTypes) = do
  (types, argTypes') <- mapAndUnzipM typeCheckType argTypes
  return ((name, types), Constructor pos (UIdent name) argTypes')

typeCheckConstructorImpl (NullaryConstr pos uident) = typeCheckConstructorImpl (Constructor pos uident [])

