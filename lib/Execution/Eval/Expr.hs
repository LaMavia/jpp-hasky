{-# LANGUAGE QuasiQuotes   #-}
{-# LANGUAGE TupleSections #-}


module Execution.Eval.Expr where

import           Abs
import           Common
import           Control.Monad.Except    (MonadError (throwError))
import           Control.Monad.Reader    (MonadReader (ask, local))
import           Data.Foldable           (Foldable (foldl'))
import           Data.String.Interpolate (i)
import           Execution.Eval.Literal  (evalLiteral)
import           Execution.Unification   (applyUnifier, unify)
import           Print                   (printTree)
import           Runtime


evalExpr :: Expr -> RT RTVal
evalExpr e = uCatch (placeOfExpr e) (evalExprImpl e)

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

evalExprImpl (ELet _ (EId _ (LIdent name)) _ vexpr bexpr) = withFrame $ do
  env' <- allocEnv name
  local (const env') $ evalExpr vexpr >>= allocState name >> evalExpr bexpr

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
        (Left $ uError
          (placeOfExpr e)
            "value not unifiable with any of the branches"
          )

evalExprImpl (EConstr _ (UIdent t) (UIdent c)) = do
  return $ RTConstr t c []

evalExprImpl (EApp _ cexpr@(EConstr {}) argExprs) = do
  constr <- evalExpr cexpr
  args <- mapM evalExpr argExprs
  rtApply constr args

evalExprImpl (EApp _ fexpr@(ELambda {}) argExprs) = do
  func <- evalExpr fexpr
  args <- mapM evalExpr argExprs
  withFrame $ rtApply func args

evalExprImpl (EApp _ iexpr@(EId {}) argExprs) = do
  app <- evalExpr iexpr
  args <- mapM evalExpr argExprs
  rtApply app args

evalExprImpl (EApp _ expr _) = uThrow [i|Invalid caller expression '#{expr}'|]


evalExprImpl (EMul _ aexpr op bexpr) = do
  RTInt a <- evalExpr aexpr
  RTInt b <- evalExpr bexpr
  let r = return . RTInt
  case op of
    Times _        -> r $ a * b
    Div _ | b == 0 -> uThrow "division by 0"
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
    _        -> undefined

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

evalExprImpl (ELambda _ fargs body) = do
  env <- ask
  return $ RTFunc env argNames f
  where
    argNames = (\(Arg _ (LIdent x) _) -> x) <$> fargs
    f = evalExpr body

evalExprImpl e@(ELetNT {}) = uThrow [i|Unexpected untyped let binding: #{printTree e}|]
evalExprImpl e@(EList {}) = uThrow [i|Unexpected sugared list expression: #{printTree e}|]
evalExprImpl (EIgnore {}) = uThrow [i|Unexpected hole|]
