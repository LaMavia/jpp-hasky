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
    TCData _ args _ _ | length args == length ts' ->
      return (TCApp t tsTypes, Abs.TApp pos (Abs.UIdent t) ts')
    TCData _ args _ _ ->
      uThrow [i|Type «#{t}» expected #{length args} arguments, but was given #{length ts'}|]
    other ->
      uThrow [i|Cannot apply arguments to a non-data identifier «#{t}» = #{other}|]

typeCheckTypeImpl t@(Abs.TType _ (Abs.UIdent name)) = do
  d <- getVar name
  return (d, t)

typeCheckTypeImpl (Abs.TBound pos argIdents t) = do
  env' <- envSeq ((`alloc` TCAny) . stringOfLident <$> argIdents)
  (tType, t') <- withEnv env' $ typeCheckType t
  return (tType, Abs.TBound pos argIdents t')

