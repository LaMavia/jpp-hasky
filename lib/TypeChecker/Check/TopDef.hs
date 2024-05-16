{-# LANGUAGE QuasiQuotes #-}

module TypeChecker.Check.TopDef where

import           Abs
import           Common                        (envSeq, findDuplicates,
                                                placeOfTopDef, showSepList,
                                                uCatch, uThrow, withEnv)
import           Control.Monad                 (mapAndUnzipM, unless, when)
import           Data.List                     (sort, union, (\\))
import qualified Data.Map                      as Map
import           Data.String.Interpolate       (i)
import           Debug.Trace                   (trace, traceShow)
import           Preprocessor.TypeDesugar      (typeDesugar)
import           TypeChecker.Check.Constructor (typeCheckConstructor)
import           TypeChecker.Check.Expr        (typeCheckExpr)
import           TypeChecker.Check.Type        (typeCheckType)
import           TypeChecker.TC
import           TypeChecker.Utils             (bvOfTopDef, bvOfType,
                                                fvOfTopDef, (<:))


typeCheckTopDef :: TCChecker TopDef TCEnv
typeCheckTopDef t = uCatch (placeOfTopDef t) (typeCheckTopDefImpl t)

typeCheckTopDefImpl :: TCChecker TopDef TCEnv
typeCheckTopDefImpl td@(TDDataV pos (UIdent t) _ constructors) = do
  let explicitArgs = bvOfTopDef td
  let implicitArgs = fvOfTopDef td \\ explicitArgs
  let args = sort explicitArgs `union` implicitArgs
  let checker args' = length args == length args'
  -- temporarily alloc `t` as an opaque type
  env' <- alloc t (TCData t args Map.empty checker)
  env'' <- withEnv env' $ envSeq ((\x -> alloc x (TCVar x)) <$> args)
  (dataConstructorEntries, constructors') <- withEnv env'' $ mapAndUnzipM typeCheckConstructor constructors
  guardDuplicateConstructors dataConstructorEntries
  let dataMap = Map.fromList dataConstructorEntries
  -- add `t` constructors
  withEnv env' $ allocState t (TCData t args dataMap checker)
  return (env', TDDataV pos (UIdent t) (LIdent <$> args) constructors')
  where
    guardDuplicateConstructors :: [(String, a)] -> TC ()
    guardDuplicateConstructors dataConstructorEntries = do
      let duplicateConstructors = findDuplicates $ fst <$> dataConstructorEntries
      let hasDuplicateConstructors = not $ null duplicateConstructors
      let duplicateConstrcutorString = showSepList ", " duplicateConstructors
      when hasDuplicateConstructors (uThrow [i|Type #{t} has duplicate constructors #{duplicateConstrcutorString}|])

typeCheckTopDefImpl (TDDataNV pos t cs) = typeCheckTopDefImpl (TDDataV pos t [] cs)

typeCheckTopDefImpl (TDDeclaration pos (LIdent name) t e) = do
  (tType, t') <- typeCheckType t
  let tType' = typeDesugar tType
  let bv = bvOfType t'
  env' <- envSeq (alloc name tType : ((`alloc` TCAny) <$> bv))
  (eType, e') <- withEnv env' $ typeCheckExpr e
  let eType' = typeDesugar eType
  areValidTypes <- tType' <: eType'
  unless areValidTypes $ uThrow [i|Declared type «#{tType'}» doesn't match the actual type «#{eType'}».|]
  return (env', TDDeclaration pos (LIdent name) t' e')

typeCheckTopDefImpl (TDDeclarationNT pos (LIdent name) e) = do
  -- assign `name` an arbitrary type
  env' <- alloc name TCAny
  (eType, e') <- typeCheckExpr e
  allocState name eType
  t <- astOfType eType Nothing
  return (env', TDDeclaration pos (LIdent name) t e')
