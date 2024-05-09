{-# LANGUAGE QuasiQuotes #-}

module TypeChecker.Check.Type where

import qualified Abs
import           Common                  (envSeq, placeOfType, showSepList,
                                          uCatch, uThrow, withEnv)
import           Control.Monad           (mapAndUnzipM)
import           Data.String.Interpolate (i)
import           TypeChecker.TC          (TCChecker,
                                          Type (TCAny, TCApp, TCData), alloc,
                                          getVar)
import           TypeChecker.Utils       (stringOfLident)


typeCheckType :: TCChecker Abs.Type Type
typeCheckType t = uCatch (placeOfType t) (typeCheckTypeImpl t)

typeCheckTypeImpl :: TCChecker Abs.Type Type
typeCheckTypeImpl t@(Abs.TVar _ (Abs.LIdent a)) = do
  ta <- getVar a
  return (ta, t)

typeCheckTypeImpl (Abs.TApp pos (Abs.UIdent t) ts) = do
  (tsTypes, ts') <- mapAndUnzipM typeCheckType ts
  d <- getVar t
  case d of
    (TCData _ _ _ c) | c tsTypes ->
      return (TCApp t tsTypes, Abs.TApp pos (Abs.UIdent t) ts')
    _ ->
      let tsString = showSepList ", " tsTypes
      in uThrow [i|Types «#{tsString}» are not applicable to «#{d}»|]

typeCheckTypeImpl t@(Abs.TType _ (Abs.UIdent name)) = do
  d <- getVar name
  return (d, t)

typeCheckTypeImpl (Abs.TBound pos argIdents t) = do
  env' <- envSeq ((`alloc` TCAny) . stringOfLident <$> argIdents)
  (tType, t') <- withEnv env' $ typeCheckType t
  return (tType, Abs.TBound pos argIdents t')

{-
(a1, ..., an) => T(xi, ..., xm) | (ai) <= (xj)
=============
TCBound
  [TCVar "a1", ..., TCVar "an"]
  (TCApp "T" [TCVar "x1", ..., TCVar "xm"])
  (replace
    (Set.fromList ["a1", ..., "an"])
    (TCApp "T" [TCVar "x1", ..., TCVar "xm"])
    )
-}
