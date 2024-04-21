{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TupleSections   #-}

module Execution.Eval.Expr where

import           Abs
import           Control.Monad.Except   (MonadError (throwError))
import           Control.Monad.Reader   (MonadReader (local))
import           Data.Foldable          (Foldable (foldl'))
import           Execution.Eval.Literal (evalLiteral)
import           Execution.Unification  (applyUnifier, unify)
import           Runtime                (RT, RTVal (RTConstr, RTInt), getVar,
                                         pattern RTCFalse, pattern RTCTrue,
                                         placeOfExpr, rtCatch, rtError,
                                         rtOfBool, rtThrow, rtcFalse, rtcTrue)


evalExpr :: Expr -> RT RTVal
evalExpr e = rtCatch (placeOfExpr e) (evalExprImpl e)

evalExprImpl :: Expr -> RT RTVal
evalExprImpl (ELit _ lit) = evalLiteral lit

evalExprImpl (Not _ expr) = do
  val <- evalExpr expr
  case val of
    RTCTrue  -> return  rtcFalse
    RTCFalse -> return  rtcTrue
    _        -> undefined

evalExprImpl (Neg _ expr) = do
  RTInt n <- evalExpr expr
  return $ RTInt (- n)

evalExprImpl (EId _ (LIdent x)) =
  getVar x

evalExprImpl (ELet _ pat _ vexpr bexpr) = do
  v <- evalExpr vexpr
  case unify pat v of
    Left x -> throwError x
    Right unifier -> do env <- applyUnifier unifier
                        local (const env) (evalExpr bexpr)

evalExprImpl (EIf _ cexpr texpr eexpr) = do
  c <- evalExpr cexpr
  case c of
    RTCTrue  -> evalExpr texpr
    RTCFalse -> evalExpr eexpr
    _        -> undefined

evalExprImpl (EMatch _ e cases) = do
  v <- evalExpr e
  let unificationResult = findUnificableBranch v cases
  case unificationResult of
    Left err              ->
      throwError err
    Right (unifier, expr) ->
      applyUnifier unifier >>= ((`local` evalExpr expr) . const)

  where
    findUnificableBranch val =
      foldl' (\u (MBBranch _ cond bexpr) ->
        u <> ((, bexpr) <$> unify cond val))
        (Left $ rtError
          (placeOfExpr e)
            "value not unifiable with any of the branches"
          )

evalExprImpl (EConstr _ (UIdent t) (UIdent c)) = do
  -- RTData _ _ constrMap <- getVar t
  -- let (Just constr) = Map.lookup c constrMap
  return $ RTConstr t c []

evalExprImpl (EApp _ cexpr@(EConstr {}) argExprs) = do
  RTConstr t c boundArgs <- evalExpr cexpr
  newArgs <- mapM evalExpr argExprs
  return $ RTConstr t c (boundArgs <> newArgs)

evalExprImpl (EMul _ aexpr op bexpr) = do
  RTInt a <- evalExpr aexpr
  RTInt b <- evalExpr bexpr
  let r = return . RTInt
  case op of
    Times _        -> r $ a * b
    Div _ | b == 0 -> rtThrow "division by 0"
    Div _          -> r $ a `div` b
    Mod _          -> r $ a `mod` b

evalExprImpl (EAdd _ aexpr op bexpr) = do
  RTInt a <- evalExpr aexpr
  RTInt b <- evalExpr bexpr
  return $ RTInt $ case op of
    Plus _  -> a + b
    Minus _ -> a - b

evalExprImpl (ERel _ aexpr (EQU _) bexpr) = do
  a <- evalExpr aexpr
  b <- evalExpr bexpr
  return $ rtOfBool $ a == b

evalExprImpl (ERel pos aexpr (NE pos') bexpr) = do
  v <- evalExprImpl (ERel pos aexpr (EQU pos') bexpr)
  return $ case v of
    RTCTrue  -> rtcFalse
    RTCFalse -> rtcTrue

evalExprImpl (ERel _ aexpr op bexpr) = do
  RTInt a <- evalExpr aexpr
  RTInt b <- evalExpr bexpr
  return $ rtOfBool $ case op of
    LTH _ -> a < b
    LE _  -> a <= b
    GTH _ -> a > b
    GE _  -> a >= b

evalExprImpl (EAnd _ aexpr bexpr) = do
  a <- evalExpr aexpr
  case a of
    RTCFalse -> return rtcFalse
    RTCTrue  -> evalExpr bexpr
    _        -> undefined

evalExprImpl (EOr _ aexpr bexpr) = do
  a <- evalExpr aexpr
  case a of
    RTCTrue  -> return rtcTrue
    RTCFalse -> evalExpr bexpr
    _        -> undefined
-- evalExprImpl x = rtCatch (placeOfExpr x) (rtThrow [i|unmatched expression: #{x}|])
