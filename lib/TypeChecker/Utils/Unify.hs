{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TupleSections   #-}

module TypeChecker.Utils.Unify where

import qualified Abs
import           Common
import           Control.Monad              (filterM, unless)
import           Control.Monad.Except       (MonadError (catchError))
import           Data.Foldable              (foldlM)
import qualified Data.Map.Strict            as Map
import           Data.String.Interpolate    (i)
import           Print                      (printTree)
import           TypeChecker.TC             (TC, TCEnv, Type (..), alloc,
                                             appendIota, getVar)
import           TypeChecker.TCConsts       (pattern TccInt)
import           TypeChecker.Utils.FreeVars (tcFVOfType)

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

allocETUnifier :: Unifier -> TC TCEnv
allocETUnifier = envSeq . fmap (uncurry alloc) . Map.toList

normaliseUnifier :: Unifier -> Unifier
normaliseUnifier u =
  sniff [i|@normaliseUnifier u=«#{u}», u'=#{u'}|] u'
  where
    u' = normaliseUnifierImpl u
    normaliseUnifierImpl :: Unifier -> Unifier
    normaliseUnifierImpl = Map.filterWithKey aux
      where
        aux :: String -> Type -> Bool
        aux k (TCVar x) = k /= x
        aux _ _         = True

joinUnifiers :: Unifier -> Unifier -> TC Unifier
joinUnifiers l_ r_ = do
  u <- joinUnifiersImpl l_ r_
  sniff [i|@joinUnifiers:\n l=#{l_}\n r=#{r_}\n u=#{u}|] $ return $ normaliseUnifier u
  where
    joinUnifiersImpl :: Unifier -> Unifier -> TC Unifier
    joinUnifiersImpl l r = do
      let commonKeys = Map.keys $ Map.intersection l r
      invalidEntires <- filterM (fmap not . uncurry (<:)) [(l Map.! k, r Map.! k) | k <- commonKeys]
      unless (null invalidEntires) $ uThrow [i|Cannot join unifiers l=#{l}, r=#{r}|]
      let l' = Map.map (applyTTUnifier r) l
      let r' = Map.difference r l
      return $ Map.union l' r'


ttUnify :: Type -> Type -> TC Unifier
ttUnify t_ t'_ = do
  u <- ttUnifyImpl t_ t'_
  sniff [i|@ttUnify t=«#{t_}», t'=«#{t'_}», u=#{u}|] $ return $ normaliseUnifier u
  where
    ttUnifyImpl :: Type -> Type -> TC Unifier
    ttUnifyImpl (TCVar x) t = return $ singleton x t
    ttUnifyImpl (TCBound _ t) t' = ttUnifyImpl t t'
    ttUnifyImpl t (TCBound _ t') = ttUnifyImpl t t'
    ttUnifyImpl (TCApp t ts) (TCApp t' ts') | t == t' =
      foldlM aux empty (zip ts ts')
    ttUnifyImpl l r = uThrow [i|Cannot unify types l=«#{l}», r=«#{r}».|]

    aux :: Unifier -> (Type, Type) -> TC Unifier
    aux u (ta, tb) = ttUnify ta tb >>= joinUnifiers u



etUnify :: Abs.Expr -> Type -> TC Unifier
etUnify e_ t_ = do
  u <- etUnifyImpl e_ t_
  sniff [] $ return $ normaliseUnifier u
  where
    etUnifyImpl :: Abs.Expr -> Type -> TC Unifier
    etUnifyImpl (Abs.ELit _ l) t =
      case (l, t) of
        (Abs.LInt {}, TccInt) -> return empty
        (l', t') -> uThrow [i|Literal «#{printTree l'}» is not of type «#{t'}».|]
    etUnifyImpl (Abs.EIgnore _) _ = return empty
    etUnifyImpl (Abs.EId _ (Abs.LIdent x)) t = return $ singleton x t
    etUnifyImpl (Abs.EApp _ (Abs.EConstr _ (Abs.UIdent t) (Abs.UIdent c)) argExprs) (TCApp t' ts') | t == t' = do
      d <- getVar t
      case d of
        TCData _ dargs dataMap _ | c `Map.member` dataMap -> do
          let cargs = dataMap Map.! c
          let u = normaliseUnifier $ Map.fromList $ zip dargs ts'
          let cargs' = applyTTUnifier u <$> cargs
          foldlM aux empty (zip argExprs cargs')
        TCData {} -> uThrow [i|«#{c}» is not a constructor of type «#{d}».|]
        x -> uThrow [i|«#{x}» is not a valid constructor type.|]
    etUnifyImpl e t = uThrow [i|Cannot unify expression «#{printTree e}» with type «#{t}»|]

    aux :: Unifier -> (Abs.Expr, Type) -> TC Unifier
    aux u (e, t) = etUnify e t >>= joinUnifiers u


rename :: Type -> TC Type
rename t = do
  let fvs = tcFVOfType t
  fvs' <- mapM appendIota fvs
  let u = Map.fromList $ zip fvs $ TCVar <$> fvs'
  let t' = applyTTUnifier u t
  sniff [i|@rename t=«#{t}», t'=«#{t'}»|] $ return t'


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

