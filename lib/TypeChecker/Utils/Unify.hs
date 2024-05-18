{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TupleSections   #-}

module TypeChecker.Utils.Unify where

import qualified Abs
import           Common                     (envSeq, uCatch, uThrow, withEnv)
import           Control.Monad              (unless, zipWithM)
import           Control.Monad.Except       (MonadError (catchError))
import           Control.Monad.ListM        (allM)
import           Data.Foldable              (foldlM)
import           Data.Functor               ((<&>))
import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as Set
import           Data.String.Interpolate    (i)
import           Debug.Trace                (trace)
import           Print                      (printTree)
import           TypeChecker.TC             (TC, TCEnv, Type (..), alloc,
                                             appendIota, getVar)
import           TypeChecker.TCConsts       (pattern TccInt)
import           TypeChecker.Utils.FreeVars (tcFVOfType)
import           TypeChecker.Utils.Replace  (replace)

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

joinUnifiers :: Unifier -> Unifier -> Unifier
joinUnifiers l_ r_ = trace [i|@joinUnifiers:\n l=#{l_}\n r=#{r_}\n u=#{u}|] u
  where
    u = joinUnifiersImpl l_ r_
    joinUnifiersImpl :: Unifier -> Unifier -> Unifier
    joinUnifiersImpl l r =
      let l' = Map.map (applyTTUnifier r) l
          r' = Map.difference r l
      in Map.union l' r'


ttUnify :: Type -> Type -> TC Unifier
ttUnify t_ t'_ = do
  u <- ttUnifyImpl t_ t'_
  trace [i|@ttUnify t=«#{t_}», t'=«#{t'_}», u=#{u}|] $ return u
  where
    ttUnifyImpl :: Type -> Type -> TC Unifier
    ttUnifyImpl (TCVar x) t = return $ singleton x t
    ttUnifyImpl (TCBound _ t) t' = ttUnifyImpl t t'
    ttUnifyImpl t (TCBound _ t') = ttUnifyImpl t t'
    ttUnifyImpl (TCApp t ts) (TCApp t' ts') | t == t' =
      foldlM aux empty (zip ts ts')
      where
        aux :: Unifier -> (Type, Type) -> TC Unifier
        aux u (ta, tb) = ttUnify ta tb <&> joinUnifiers u
    ttUnifyImpl l r = uThrow [i|Cannot unify types l=«#{l}», r=«#{r}».|]


rename :: Type -> TC Type
rename t = do
  let fvs = tcFVOfType t
  fvs' <- mapM appendIota fvs
  let u = Map.fromList $ zip fvs $ TCVar <$> fvs'
  let t' = applyTTUnifier u t
  trace [i|@rename t=«#{t}», t'=«#{t'}»|] $ return t'


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
  lt <- l <: r
  gt <- r <: l
  case (lt, gt) of
    (True, True)   -> return EQ
    (_, True)      -> return GT
    (True, _)      -> return LT
    (False, False) -> uThrow [i|Incomparable types a=«#{l}», and b=«#{r}».|]

tcMinM :: Type -> Type -> TC Type
tcMinM l r = do
  ord <- tcCmpM l r
  return $ case ord of
    LT -> l
    EQ -> l
    GT -> r

