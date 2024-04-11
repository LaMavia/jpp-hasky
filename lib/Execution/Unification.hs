{-# LANGUAGE QuasiQuotes    #-}

module Execution.Unification where

import Abs 
import Runtime.RTVal 
import Runtime.RTError (RTResult, rtError, placeOfExpr)
import qualified Data.Map.Strict as Map
import           Data.String.Interpolate (i)
import Data.List (length)
import Data.Function (on)

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
