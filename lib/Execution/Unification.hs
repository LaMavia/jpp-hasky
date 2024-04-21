{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE QuasiQuotes   #-}

module Execution.Unification where

import           Abs
import           Control.Monad           (zipWithM)
import qualified Data.Map.Strict         as Map
import           Data.String.Interpolate (i)
import           Runtime

type Unifier = Map.Map String RTVal

empty :: Unifier
empty = Map.empty

singleton :: String -> RTVal -> Unifier
singleton = Map.singleton

unify :: Expr -> RTVal -> RTResult Unifier
unify (EIgnore _) _ = return empty
unify (EId _ (LIdent x)) v = return $ singleton x v
unify (EApp _ (EConstr _ (UIdent tx) (UIdent x)) args) (RTConstr ty y vs)
  | length args == length vs,
    tx == ty,
    x == y =
      foldr1 (<>) <$> zipWithM unify args vs
unify (ELit _ (LInt _ p)) (RTInt n)
  | fromInteger p == n = return empty
unify p v =
  Left
    ( rtError
        (placeOfExpr p)
        [i|Cannot match the pattern #{pText} with value #{vText}|]
    )
  where
    pText = show p
    vText = show v


applyUnifier :: Unifier -> RT RTEnv
applyUnifier unifier = envSeq (uncurry alloc <$> Map.assocs unifier)


