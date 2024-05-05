{-# LANGUAGE QuasiQuotes #-}

module TypeChecker.Utils.Unify where

import           Common                  (uThrow)
import           Data.Foldable           (foldlM)
import qualified Data.Map.Strict         as Map
import           Data.String.Interpolate (i)
import           TypeChecker.TC          (TC, Type (TCApp, TCVar))

type Unifier = Map.Map String Type

applyUnifier :: Unifier -> Type -> Type
applyUnifier u (TCVar x) | x `Map.member` u = u Map.! x
applyUnifier u (TCApp t ts) = TCApp t (applyUnifier u <$> ts)
applyUnifier _ t = t

combineUnifiers :: Unifier -> Unifier -> Unifier
combineUnifiers u v = Map.map (applyUnifier v) u

infixr 6 <~
(<~) :: Type -> Type -> TC Unifier
TCVar x <~ t = return $ Map.singleton x t
TCApp tx tsx <~ TCApp ty tsy | tx == ty =
  foldlM aux Map.empty (tsx `zip` tsy)
  where
    aux :: Unifier -> (Type, Type) -> TC Unifier
    aux u (a, b) = do
      let a' = applyUnifier u a
      v <- a' <~ b
      return $ combineUnifiers u v

x <~ y
  | x == y = return Map.empty
  | otherwise = uThrow [i|Types «#{x}», and «#{y}» are not unifiable|]
