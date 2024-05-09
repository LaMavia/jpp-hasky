{-# LANGUAGE QuasiQuotes #-}

module TypeChecker.Check.Expr where
import qualified Abs
import           Common                    (envSeq, placeOfExpr, showSepList,
                                            uCatch, uThrow, withEnv)
import           Control.Monad             (mapAndUnzipM, when, zipWithM)
import           Data.Foldable             (foldlM, foldrM)
import qualified Data.Map                  as Map
import qualified Data.Set                  as Set
import           Data.String.Interpolate   (i)
import           Print                     (Print (prt), render)
import           TypeChecker.Check.Arg     (typeCheckArg)
import           TypeChecker.Check.Type    (typeCheckType)
import           TypeChecker.TC            (TCChecker,
                                            Type (TCAny, TCApp, TCData, TCVar),
                                            alloc, astOfType, getVar)
import           TypeChecker.TCConsts      (tccAnyAst, tccBool, tccFun)
import           TypeChecker.Utils         (bvOfType, combineUnifiers,
                                            stringOfLident, tcFVOfType)
import           TypeChecker.Utils.Replace (replace)
import           TypeChecker.Utils.Unify   ((<~))


typeCheckExpr :: TCChecker Abs.Expr Type
typeCheckExpr e = uCatch (placeOfExpr e) (typeCheckExprImpl e)

typeCheckExprImpl :: TCChecker Abs.Expr Type
typeCheckExprImpl (Abs.ELet pos xe@(Abs.EId _ (Abs.LIdent x)) t ve be) = do
  (tExpected, t') <- typeCheckType t
  (tActual, ve') <- typeCheckExpr ve
  when (tExpected /= tActual) $ uThrow [i|Expected #{x} to be of type «#{tExpected}», but got «#{tActual}» instead.|]
  env' <- alloc x tActual
  (tBody, be') <- withEnv env' $ typeCheckExpr be
  return (tBody, Abs.ELet pos xe t' ve' be')

typeCheckExprImpl (Abs.ELetNT pos xe ve be) = do
  anyTypeExpr <- tccAnyAst
  (tType, e') <- typeCheckExprImpl $ Abs.ELet pos xe anyTypeExpr ve be
  t <- astOfType tType Nothing
  case e' of
    Abs.ELet _ xe' _ ve' be' ->
      return (tType, Abs.ELet pos xe' t ve' be')
    e ->
      let p = render $ prt 0 e
      in uThrow [i|Invalid typecheck expression returned «#{p}». Expected a typed `let` expression.|]

typeCheckExprImpl (Abs.EIf pos ce te ee) = do
  (cType, ce') <- typeCheckExpr ce
  when (cType /= tccBool) $ uThrow [i|If condition must be of type «Bool», but got «#{cType}» instead.|]
  (tType, te') <- typeCheckExpr te
  (eType, ee') <- typeCheckExpr ee
  when (tType /= eType) $ uThrow [i|Both branches of if must have the same type, but got «#{tType}», and «#{eType}» instead.|]
  return (tType, Abs.EIf pos ce' te' ee')

typeCheckExprImpl (Abs.ELambda pos argExprs bodyExpr) = do
  (argTypes, argExprs') <- mapAndUnzipM typeCheckArg argExprs
  let argNames = (\(Abs.Arg _ (Abs.LIdent x) _) -> x) <$> argExprs'
  env' <- envSeq $ uncurry alloc <$> zip argNames argTypes
  (bodyType, bodyExpr') <- withEnv env' $ typeCheckExpr bodyExpr
  return (tccFun (argTypes <> [bodyType]), Abs.ELambda pos argExprs' bodyExpr')

typeCheckExprImpl e@(Abs.EId _ (Abs.LIdent x)) = do
  t <- getVar x
  return (t, e)

typeCheckExprImpl e@(Abs.EConstr _ (Abs.UIdent t) (Abs.UIdent c)) = do
  d <- getVar t
  case d of
    TCData _ argIdents dataMap _ | c `Map.member` dataMap -> do
      {-
       - data T(a, b, c) = A(a, List(b)) | C(c) ;;
       - x = T.A(1, [Bool.True()]) ;; => x :: T(Int, Bool, Any) = ... ;;
       - y = T.C(0) ;;                => y :: T(Any, Any, Int) = ... ;;
       - -}
      let cArgs = dataMap Map.! c
      let bvs = Set.fromList argIdents
      let avs = Set.unions $ tcFVOfType Set.empty <$> cArgs
      let leftVs = bvs `Set.difference` avs
      x <- replace leftVs $ TCApp t $ TCVar <$> Set.toList avs
      return (x, e)
    TCData _ _ dataMap _ ->
      let constrString = showSepList " | " $ Map.keys dataMap
      in uThrow [i|«#{t}.#{c}» is not a constructor of type «#{t}». Available constructors: #{constrString}.|]
    _ ->
      uThrow [i|«#{t}» is not a type.|]

typeCheckExprImpl e = return (TCAny, e)
  -- when (c `Map.notMember` )

