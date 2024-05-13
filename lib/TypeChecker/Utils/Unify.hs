{-# LANGUAGE QuasiQuotes   #-}
{-# LANGUAGE TupleSections #-}

module TypeChecker.Utils.Unify where

import qualified Abs
import           Common                  (uThrow)
import           Control.Monad           (when, zipWithM)
import           Data.Foldable           (foldlM)
import qualified Data.Map.Strict         as Map
import           Data.String.Interpolate (i)
import           TypeChecker.TC          (TC, TCEnv, Type (..), getVar)

type Unifier = Map.Map String Type
empty :: Unifier
empty = Map.empty

singleton :: String -> Type -> Unifier
singleton = Map.singleton

applyTTUnifier :: Unifier -> Type -> Type
applyTTUnifier u (TCVar x) | x `Map.member` u = u Map.! x
applyTTUnifier u (TCApp t ts) = TCApp t (applyTTUnifier u <$> ts)
applyTTUnifier u (TCBound _ t) = applyTTUnifier u t
applyTTUnifier _ t = t

combineUnifiers :: Unifier -> Unifier -> Unifier
combineUnifiers u v = Map.map (applyTTUnifier v) u

tcUnifyExpr :: Abs.Expr -> Type -> TC Unifier
tcUnifyExpr (Abs.EId _ (Abs.LIdent x)) t = return $ Map.singleton x t
tcUnifyExpr (Abs.EApp _ ce@(Abs.EConstr _ (Abs.UIdent tx) (Abs.UIdent cx)) tsx) (TCApp ty tsy)
  | tx == ty
  = do
  d <- getVar tx
  case d of
    TCData _ _ constrMap _ -> do
      when (cx `Map.notMember` constrMap) (uThrow [i|«#{cx}» is not a constructor of «#{d}»|])
      foldlM (\u (ta, tb) -> combineUnifiers u <$> (ta `tcUnifyExpr` tb)) Map.empty $ zip tsx tsy
    _ -> uThrow [i|«#{ce}» is not a valid constructor|]

tcUnifyExpr x y = uThrow [i|Types «#{x}», and «#{y}» are not unifiable|]

joinUnifiers :: Unifier -> Unifier -> TC Unifier
joinUnifiers a b | Map.null (Map.intersection a b) = return $ Map.union a b
joinUnifiers a b = do
  let bs = (\(k, va) -> maybe True (== va) (Map.lookup k b)) <$> Map.toList a
  let areCompatible = and bs
  if areCompatible then return (Map.union a b) else uThrow [i|inconsistent unifiers: a=«#{a}», b=«#{b}»|]

ttUnify :: Type -> Type -> TC Unifier
ttUnify (TCVar x) t = return $ Map.singleton x t

ttUnify (TCApp tx tsx) (TCApp ty tsy) | tx == ty = do
  us <- zipWithM ttUnify tsx tsy
  foldlM joinUnifiers Map.empty us

ttUnify (TCApp _ ts) TCAny = do
  us <- zipWithM ttUnify ts (repeat TCAny)
  foldlM joinUnifiers Map.empty us

ttUnify (TCBound vs t) t' = do
  u <- ttUnify t t'
  let missingVars = filter (`Map.notMember` u) vs
  let missingEntries = (, TCAny) <$> missingVars
  let uCompliment = Map.fromList missingEntries
  return $ Map.union u uCompliment

ttUnify a b =
  uThrow [i|Types «#{a}», and «#{b}» are not unifiable|]
