{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TupleSections   #-}

module TypeChecker.Utils.Unify where

import qualified Abs
import           Common                    (envSeq, uThrow, withEnv)
import           Control.Monad             (unless, zipWithM)
import           Control.Monad.Except      (MonadError (catchError))
import           Control.Monad.ListM       (allM)
import           Data.Foldable             (foldlM)
import qualified Data.Map.Strict           as Map
import qualified Data.Set                  as Set
import           Data.String.Interpolate   (i)
import           Debug.Trace               (trace)
import           TypeChecker.TC            (TC, TCEnv, Type (..), alloc, getVar)
import           TypeChecker.TCConsts      (pattern TccInt)
import           TypeChecker.Utils.Replace (replace)

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

allocTCUnifier :: Unifier -> TC TCEnv
allocTCUnifier u =
  envSeq $ uncurry alloc <$> Map.toList u

combineUnifiers :: Unifier -> Unifier -> Unifier
combineUnifiers u v = Map.map (applyTTUnifier v) u


tcUnifyExpr :: Abs.Expr -> Type -> TC Unifier
tcUnifyExpr (Abs.EIgnore _) _ = return Map.empty
tcUnifyExpr (Abs.EId _ (Abs.LIdent x)) t = return $ Map.singleton x t
tcUnifyExpr (Abs.ELit _ l) t
  | Abs.LInt {} <- l, TccInt <- t = return Map.empty
  | otherwise = uThrow [i|Literal «#{l}» is not of type «#{t}».|]
tcUnifyExpr (Abs.EApp _ ce@(Abs.EConstr _ (Abs.UIdent tx) (Abs.UIdent cx)) tsx) (TCApp ty tsy)
  | tx == ty
  = do
  d <- getVar tx
  case d of
    TCData _ args constrMap _ -> do
      unless (cx `Map.member` constrMap) $ uThrow [i|«#{cx}» is not a constructor of «#{d}»|]
      let argExprs  = constrMap Map.! cx
      env' <- envSeq $ zipWith alloc args tsy
      argExprs' <-  withEnv env' $ mapM (replace (Set.fromList args)) argExprs
      us <- zipWithM tcUnifyExpr tsx argExprs'
      foldlM joinUnifiers Map.empty us
    _ -> uThrow [i|«#{ce}» is not a valid constructor|]

tcUnifyExpr x y = uThrow [i|Types «#{x}», and «#{y}» are not unifiable|]

joinUnifiers :: Unifier -> Unifier -> TC Unifier
joinUnifiers a b | Map.null (Map.intersection a b) = return $ Map.union a b
joinUnifiers a b = do
  areCompatible <- allM (\(k, va) -> maybe (return True) (=~= va) (Map.lookup k b)) $ Map.toList a
  if areCompatible then return (Map.union a b) else uThrow [i|inconsistent unifiers: a=«#{a}», b=«#{b}»|]

ttUnify :: Type -> Type -> TC Unifier
ttUnify (TCVar x) t = return $ Map.singleton x t

ttUnify (TCApp tx tsx) (TCApp ty tsy) | tx == ty = do
  us <- zipWithM ttUnify tsx tsy
  foldlM joinUnifiers Map.empty us

ttUnify (TCApp _ ts) TCAny = do
  us <- zipWithM ttUnify ts (repeat TCAny)
  foldlM joinUnifiers Map.empty us

ttUnify TCAny _ = return Map.empty

ttUnify (TCBound vs t) t' = do
  u <- ttUnify t t'
  let missingVars = filter (`Map.notMember` u) vs
  let missingEntries = (, TCAny) <$> missingVars
  let uCompliment = Map.fromList missingEntries
  return $ Map.union u uCompliment

ttUnify _ TCAny = return Map.empty

ttUnify a@(TCData {}) b@(TCData {}) | a == b =
  return Map.empty


ttUnify a b =
  -- trace [i|@ttUnify fallthrough: a=«#{a}», b=«#{b}»|] $
  uThrow [i|Types «#{a}», and «#{b}» are not unifiable|]


infixr 6 <:
(<:) :: Type -> Type -> TC Bool
l <: r = catchError (True <$ ttUnify l r) (return . const False)

infixr 6 =~=
(=~=) :: Type -> Type -> TC Bool
a =~= b = do
  l <- a <: b
  r <- b <: a
  return $ l && r

tcCmpM :: Type -> Type -> TC Ordering
tcCmpM l r = do
  eq <- l =~= r
  lt <- l <: r
  return $ if eq then EQ else if lt then LT else GT
