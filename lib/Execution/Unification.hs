{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternGuards #-}

module Execution.Unification where

import           Abs
import qualified Data.Map.Strict         as Map
import           Data.String.Interpolate (i)
import           Runtime.RTError         (RTResult, placeOfExpr, rtError)
import           Runtime.RTVal

type Unifier = Map.Map String RTVal

empty :: Unifier
empty = Map.empty

singleton :: String -> RTVal -> Unifier
singleton = Map.singleton

unify :: Expr -> RTVal -> RTResult Unifier
unify (EIgnore _) _ = return empty
unify (EId _ (LIdent x)) v = return $ singleton x v
unify (EApp _ (EConstr _ (UIdent x)) args) (RTConstr y vs)
  | length args == length vs
  , x == y
  = fmap (foldr1 (<>))
    $ mapM (uncurry unify)
    $ zip args vs
unify (ELit _ (LInt _ p)) (RTInt n)
  | fromInteger p == n = return empty

unify p v =
  Left (
    rtError
      (placeOfExpr p)
      [i|Cannot match the pattern #{pText} with value #{vText}|]
    )
  where
    pText = show p
    vText = show v
