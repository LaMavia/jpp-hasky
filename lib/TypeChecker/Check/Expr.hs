{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TupleSections   #-}

module TypeChecker.Check.Expr where
import qualified Abs
import           Common                    (envSeq, placeOfExpr, showSepList,
                                            uCatch, uThrow, unions, withEnv)
import           Control.Monad             (mapAndUnzipM, when)
import           Data.List                 ((\\))
import qualified Data.Map                  as Map
import qualified Data.Set                  as Set
import           Data.String.Interpolate   (i)
import           Debug.Trace               (traceShow, traceShowId)
import           Print                     (Print (prt), render)
import           TypeChecker.Check.Arg     (typeCheckArg)
import           TypeChecker.Check.Type    (typeCheckType)
import           TypeChecker.TC            (TCChecker,
                                            Type (TCAny, TCApp, TCBound, TCData, TCVar),
                                            alloc, astOfType, getVar)
import           TypeChecker.TCConsts      (pattern TccInt, tccAnyAst, tccBool,
                                            tccBoolKey, tccFun, tccIntKey)
import           TypeChecker.Utils         (tcFVOfType)
import           TypeChecker.Utils.Apply   (tcApply)
import           TypeChecker.Utils.Replace (replace)


typeCheckExpr :: TCChecker Abs.Expr Type
typeCheckExpr e = uCatch (placeOfExpr e) (typeCheckExprImpl e)

typeCheckExprImpl :: TCChecker Abs.Expr Type
typeCheckExprImpl (Abs.ELet pos xe@(Abs.EId _ (Abs.LIdent x)) t ve be) = do
  (tExpected, t') <- typeCheckType t
  envExpected <- alloc x tExpected
  (tActual, ve') <- withEnv envExpected $ typeCheckExpr ve
  when (tExpected /= tActual) $ uThrow [i|Expected #{x} to be of type «#{tExpected}», but got «#{tActual}» instead.|]
  envActual <- alloc x tActual
  (tBody, be') <- withEnv envActual $ typeCheckExpr be
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
  tBool <- getVar tccBoolKey
  (cType, ce') <- typeCheckExpr ce
  when (cType /= tBool) $ uThrow [i|If condition must be of type «Bool», but got «#{cType}» instead.|]
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
      let bvs = argIdents
      let avs = unions $ tcFVOfType <$> cArgs
      let leftVs = bvs \\ avs
      x <- replace (Set.fromList leftVs) $ TCApp t $ TCVar <$> avs
      return (TCBound avs x, e)
    TCData _ _ dataMap _ ->
      let constrString = showSepList " | " $ Map.keys dataMap
      in uThrow [i|«#{t}.#{c}» is not a constructor of type «#{t}». Available constructors: #{constrString}.|]
    _ ->
      uThrow [i|«#{t}» is not a type.|]

typeCheckExprImpl e@(Abs.EApp pos ce argExprs) = do
  let checkedCe = typeCheckExpr ce
  (tCe', ce') <-
    case ce of
      Abs.EConstr {} -> checkedCe
      Abs.EId {}     -> checkedCe
      Abs.ELambda {} -> checkedCe
      _              -> uThrow [i|Invalid caller expression «#{ce}»|]
  (argTypes', argExprs') <- mapAndUnzipM typeCheckExpr argExprs
  t <- traceShow e $ tcApply tCe' argTypes'
  return (t, Abs.EApp pos ce' argExprs')

typeCheckExprImpl e@(Abs.EMatch pos ve branches) = do
  -- (tVe', ve') <- typeCheckExpr ve
  return (TCAny, e)

typeCheckExprImpl e@(Abs.EIgnore {}) = return (TCAny, e)
typeCheckExprImpl e@(Abs.ELit _ l) =
  case l of
    Abs.LInt {} -> (,e) <$> getVar "Int"

typeCheckExprImpl (Abs.Neg pos e) = do
  tInt <- getVar tccIntKey
  (e'Type, e') <- typeCheckExpr e
  case e'Type of
    t' | tInt == t' -> return (tInt, Abs.Neg pos e')
    t'              -> uThrow [i|Expected an «Int», but got «#{t'}» instead|]

typeCheckExprImpl (Abs.Not pos e) = do
  tBool <- getVar tccBoolKey
  (e'Type, e') <- typeCheckExpr e
  case e'Type of
    t' | tBool == t' -> return (tBool, Abs.Not pos e')
    t'               -> uThrow [i|Expected a «Bool», but got «#{t'}» instead|]

typeCheckExprImpl (Abs.EMul pos a op b) = do
  tInt <- getVar tccIntKey
  (a'Type, a') <- typeCheckExpr a
  (b'Type, b') <- typeCheckExpr b
  if a'Type == b'Type && b'Type == a'Type && tInt == a'Type then
    return (tInt, Abs.EMul pos a' op b')
  else
    uThrow [i|Expected both operands to be of type «Int», but got «#{a'Type}», and «#{b'Type}» instead|]

typeCheckExprImpl (Abs.EAdd pos a op b) = do
  tInt <- getVar tccIntKey
  (a'Type, a') <- typeCheckExpr a
  (b'Type, b') <- typeCheckExpr b
  if traceShowId (a'Type == b'Type) && traceShowId (b'Type == a'Type) && tInt == a'Type then
    return (tInt, Abs.EAdd pos a' op b')
  else
    uThrow [i|Expected both operands to be of type «#{tInt}», but got «#{a'Type}», and «#{b'Type}» instead|]

typeCheckExprImpl (Abs.ERel pos a op b) = do
  tBool <- getVar tccBoolKey
  (a'Type, a') <- typeCheckExpr a
  (b'Type, b') <- typeCheckExpr b
  if a'Type == b'Type then
    return (tBool, Abs.ERel pos a' op b')
  else
    uThrow [i|Expected both operands to be of type «Int», but got «#{a'Type}», and «#{b'Type}» instead|]

typeCheckExprImpl (Abs.EAnd pos a b) = do
  tBool <- getVar tccBoolKey
  (a'Type, a') <- typeCheckExpr a
  (b'Type, b') <- typeCheckExpr b
  if a'Type == b'Type && b'Type == a'Type && tBool == a'Type then
    return (tBool, Abs.EAnd pos a' b')
  else
    uThrow [i|Expected both operands to be of type «Bool», but got «#{a'Type}», and «#{b'Type}» instead|]

typeCheckExprImpl (Abs.EOr pos a b) = do
  tBool <- getVar tccBoolKey
  (a'Type, a') <- typeCheckExpr a
  (b'Type, b') <- typeCheckExpr b
  if a'Type == b'Type && b'Type == a'Type && tBool == a'Type then
    return (tBool, Abs.EOr pos a' b')
  else
    uThrow [i|Expected both operands to be of type «Bool», but got «#{a'Type}», and «#{b'Type}» instead|]


typeCheckExprImpl e = uThrow [i|unmatched case: #{e}|]
  -- when (c `Map.notMember` )

