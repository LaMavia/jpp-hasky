{-# LANGUAGE QuasiQuotes #-}

module TypeChecker.Utils.Replace where
import           Common                     (envSeq, withEnv)
import           Data.Functor               ((<&>))
import qualified Data.List                  as List
import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as Set
import           Data.String.Interpolate    (i)
import           Debug.Trace                (trace)
import           TypeChecker.TC             (TC,
                                             Type (TCAny, TCApp, TCBound, TCData, TCVar),
                                             alloc, appendIota, getVar,
                                             isDefined)
import           TypeChecker.Utils.FreeVars (tcFVOfType)

replace :: Set.Set String -> Type -> TC Type
replace s (TCVar x) | x `Set.member` s = do
  isXDefined <- isDefined x
  if isXDefined then getVar x else return TCAny

replace _ v@(TCVar {}) = return v

replace s (TCBound vs t) =
  replace (s `Set.difference` Set.fromList vs) t

replace s e@(TCApp t ts) = do
  ts' <- mapM (replace s) ts
  let r = TCApp t ts'
  trace [i|@replace e=«#{e}», r=«#{r}»|] $ return r

replace _ TCAny = return TCAny
replace s (TCData t args dataMap c) = do
  let args' = List.filter (`Set.notMember` s) args
  let entries = Map.toList dataMap
  entries' <- mapM (\(k, ts) -> do ts' <- mapM (replace s) ts; return (k, ts')) entries
  let dataMap' = Map.fromList entries'
  return $ TCData t args' dataMap' c

