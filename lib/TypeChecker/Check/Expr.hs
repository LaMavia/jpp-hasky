{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TupleSections   #-}

module TypeChecker.Check.Expr where
import qualified Abs
import           Common                    (envSeq, placeOfExpr, showSepList,
                                            sniff, uCatch, uThrow, withEnv)
import           Control.Monad             (mapAndUnzipM, unless, when)
import           Control.Monad.ListM       (allM, minimumByM)
import           Data.Bifunctor            (Bifunctor (first))
import qualified Data.Map                  as Map
import           Data.String.Interpolate   (i)
import           Preprocessor.TypeDesugar  (typeDesugar)
import           Print                     (Print (prt), printTree, render)
import           TypeChecker.Check.Arg     (typeCheckArg)
import           TypeChecker.Check.Pattern (typeCheckPattern)
import           TypeChecker.Check.Type    (typeCheckType)
import           TypeChecker.TC            (TCChecker, Type (..), alloc,
                                            astOfType, getVar)
import           TypeChecker.TCConsts      (pattern TccBool, pattern TccFn,
                                            pattern TccInt, pattern TccUnif,
                                            tccAnyAst)
import           TypeChecker.Utils         (applyTTUnifier, tcApply, tcCmpM,
                                            ttUnify, (<:))


typeCheckExpr :: TCChecker Abs.Expr Type
typeCheckExpr e = uCatch (placeOfExpr e) (typeCheckExprImpl e)

typeCheckExprImpl :: TCChecker Abs.Expr Type
typeCheckExprImpl (Abs.ELet pos xe@(Abs.EId _ (Abs.LIdent x)) t ve be) = do
  (tExpected, t') <- typeCheckType t
  let tExpected' = typeDesugar tExpected
  envExpected <- alloc x tExpected'
  (tActual, ve') <- withEnv envExpected $ typeCheckExpr ve
  let tActual' = typeDesugar tActual
  areValidTypes <- tActual' <: tExpected'
  unless areValidTypes $ uThrow [i|Expected «#{printTree x}» to be of type «#{tExpected'}», but got «#{tActual'}» instead.|]
  envActual <- alloc x tActual'
  (tBody, be') <- withEnv envActual $ typeCheckExpr be
  return (typeDesugar tBody, Abs.ELet pos xe t' ve' be')

typeCheckExprImpl (Abs.ELet pos xe@(Abs.EIgnore _) t ve be) = do
  (_, t') <- typeCheckType t
  (_, ve') <- typeCheckExpr ve
  (tbe', be') <- typeCheckExpr be
  return (tbe', Abs.ELet pos xe t' ve' be')

typeCheckExprImpl (Abs.ELet pos xe t ve be) = do
  (tType, t') <- typeCheckType t
  (tActual, ve') <- first typeDesugar <$> typeCheckExpr ve
  areTypesMatching <- tActual <: tType
  unless areTypesMatching $ uThrow [i|Expected #{printTree xe} to be of type «#{tType}», but got «#{tActual}» instead.|]
  (env', xe') <- typeCheckPattern tActual xe
  (tBody, be') <- withEnv env' $ typeCheckExpr be
  return (typeDesugar tBody, Abs.ELet pos xe' t' ve' be')

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

typeCheckExprImpl (Abs.EMatch pos ve branches) = do
  (tve', ve') <- typeCheckExpr ve
  (tBranches', branches') <- mapAndUnzipM (typeCheckMatchBranch tve') branches
  minType <- minimumByM tcCmpM tBranches'
  -- assuming that <: is transitive
  areTypesValid <- allM (minType <:) tBranches'
  unless areTypesValid $
    let typeString = showSepList "," tBranches'
    in uThrow [i|Mismatched branch return types: «#{typeString}»|]
  return (typeDesugar minType, Abs.EMatch pos ve' branches')
  where
    typeCheckMatchBranch :: Type -> TCChecker Abs.MatchBranch Type
    typeCheckMatchBranch t (Abs.MBBranch bpos xe be) = do
      (env', xe') <- typeCheckPattern t xe
      (tBody, be') <- withEnv env' $ typeCheckExpr be
      return (typeDesugar tBody, Abs.MBBranch bpos xe' be')

typeCheckExprImpl (Abs.EIf pos ce te ee) = do
  let tBool = TccBool
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
  return (TccFn (argTypes <> [bodyType]), Abs.ELambda pos argExprs' bodyExpr')

typeCheckExprImpl e@(Abs.EId _ (Abs.LIdent x)) = do
  t <- getVar x
  return (t, e)

typeCheckExprImpl e@(Abs.EConstr pos _ _) = do
  typeCheckExprImpl $ Abs.EApp pos e []

typeCheckExprImpl e@(Abs.EApp pos ce@(Abs.EConstr _ (Abs.UIdent t) (Abs.UIdent c)) argExprs) = do
  (tArgExprs', argExprs') <- mapAndUnzipM typeCheckExpr argExprs
  d <- getVar t
  case d of
    TCData _ args dataMap _
      |  c `Map.member` dataMap -> do
      let cArgs = dataMap Map.! c
      unless (length cArgs == length argExprs) $ uThrow [i|Cannot apply #{length argExprs} arguments to #{t}.#{c}/#{length cArgs}|]
      u <- ttUnify  (TccUnif cArgs) (TccUnif tArgExprs')
      let args'' = applyTTUnifier u . TCVar <$> args
      let eType = TCApp t args''
      sniff [i|@checkExpr e=«#{printTree e}»,\n\tu=«#{u}»,\nt\teType=«#{eType}»|] $ return (eType, Abs.EApp pos ce argExprs')
    TCData _ _ dataMap _
      | c `Map.notMember` dataMap ->
      let csString = showSepList " | " [ [i|#{k}(#{tsString})|] :: String | (k, ts) <- Map.toList dataMap, let tsString = showSepList ", " ts  ]
      in uThrow [i|«#{c}» is not a constructor of type «#{t}». Available constructors: #{csString}|]
    td ->
      uThrow [i|Invalid constructor type «#{td}»|]

typeCheckExprImpl (Abs.EApp pos ce argExprs) = do
  let checkedCe = typeCheckExpr ce
  (tCe', ce') <-
    case ce of
      Abs.EId {}     -> checkedCe
      Abs.ELambda {} -> checkedCe
      _              -> uThrow [i|Invalid caller expression «#{printTree ce}»|]
  (argTypes', argExprs') <- mapAndUnzipM typeCheckExpr argExprs
  t <- tcApply tCe' argTypes'
  return (t, Abs.EApp pos ce' argExprs')

typeCheckExprImpl (Abs.EIgnore {}) = uThrow [i|Unexpected hole|]
typeCheckExprImpl e@(Abs.ELit _ l) =
  case l of
    Abs.LInt {} -> return (TccInt ,e)

typeCheckExprImpl (Abs.Neg pos e) = do
  let tInt = TccInt
  (e'Type, e') <- typeCheckExpr e
  case e'Type of
    t' | tInt == t' -> return (tInt, Abs.Neg pos e')
    t'              -> uThrow [i|Expected an «Int», but got «#{t'}» instead|]

typeCheckExprImpl (Abs.Not pos e) = do
  let tBool = TccBool
  (e'Type, e') <- typeCheckExpr e
  case e'Type of
    t' | tBool == t' -> return (tBool, Abs.Not pos e')
    t'               -> uThrow [i|Expected a «Bool», but got «#{t'}» instead|]

typeCheckExprImpl (Abs.EMul pos a op b) = do
  let tInt = TccInt
  (a'Type, a') <- typeCheckExpr a
  (b'Type, b') <- typeCheckExpr b
  if a'Type == b'Type && b'Type == a'Type && tInt == a'Type then
    return (tInt, Abs.EMul pos a' op b')
  else
    uThrow [i|Expected both operands to be of type «Int», but got «#{a'Type}», and «#{b'Type}» instead|]

typeCheckExprImpl (Abs.EAdd pos a op b) = do
  let tInt = TccInt
  (a'Type, a') <- typeCheckExpr a
  (b'Type, b') <- typeCheckExpr b
  if (a'Type == b'Type) && (b'Type == a'Type) && tInt == a'Type then
    return (tInt, Abs.EAdd pos a' op b')
  else
    uThrow [i|Expected both operands to be of type «#{tInt}», but got «#{a'Type}», and «#{b'Type}» instead|]

typeCheckExprImpl (Abs.ERel pos a op b) = do
  let tBool = TccBool
  (a'Type, a') <- typeCheckExpr a
  (b'Type, b') <- typeCheckExpr b
  if a'Type == b'Type then
    return (tBool, Abs.ERel pos a' op b')
  else
    uThrow [i|Expected both operands to be of type «Int», but got «#{a'Type}», and «#{b'Type}» instead|]

typeCheckExprImpl (Abs.EAnd pos a b) = do
  let tBool = TccBool
  (a'Type, a') <- typeCheckExpr a
  (b'Type, b') <- typeCheckExpr b
  if a'Type == b'Type && b'Type == a'Type && tBool == a'Type then
    return (tBool, Abs.EAnd pos a' b')
  else
    uThrow [i|Expected both operands to be of type «Bool», but got «#{a'Type}», and «#{b'Type}» instead|]

typeCheckExprImpl (Abs.EOr pos a b) = do
  let tBool = TccBool
  (a'Type, a') <- typeCheckExpr a
  (b'Type, b') <- typeCheckExpr b
  if a'Type == b'Type && b'Type == a'Type && tBool == a'Type then
    return (tBool, Abs.EOr pos a' b')
  else
    uThrow [i|Expected both operands to be of type «Bool», but got «#{a'Type}», and «#{b'Type}» instead|]


typeCheckExprImpl e = uThrow [i|unmatched case: #{e}|]
  -- when (c `Map.notMember` )

