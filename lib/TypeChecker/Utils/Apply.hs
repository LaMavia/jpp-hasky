{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes     #-}

module TypeChecker.Utils.Apply where

import           Common                    (envSeq, uThrow, withEnv)
import qualified Data.Set                  as Set
import           Data.String.Interpolate   (i)
import           TypeChecker.TC            (TC, Type (..), alloc)
import           TypeChecker.Utils.Replace (replace)
import           TypeChecker.Utils.Unify   (applyTTUnifier, ttUnify)

tcApply :: Type -> [Type] -> TC Type
tcApply f0 = applicator f0'
    where
    f0' = case f0 of
              TCData _ args _ _ -> TCBound args f0
              f                 -> f
    applicator :: Type -> [Type] -> TC Type
    applicator f [] = return f
    applicator (TCApp "Fn" [a, r]) (x:rest) = do
      u <- ttUnify a x
      let r' = applyTTUnifier u r
      applicator r' rest
    applicator (TCBound args t) xs = do
        env' <- envSeq allocs
        t' <- withEnv env' $ replace (Set.fromList args) t
        case (leftArgs, leftXs) of
          ([], []) -> return t'
          ([], xs') -> applicator t' xs'
          (args', []) -> return $ TCBound args' t'
          (args', xs') -> uThrow [i|NEVER: unexpected leftover bound arguments «#{args'}», and parameters «#{xs'}»|]
      where
        (allocs, leftArgs, leftXs) = zipWithRest alloc args xs
        zipWithRest :: (a -> b -> c) -> [a] -> [b] -> ([c], [a], [b])
        zipWithRest f xs_ ys_ = aux xs_ ys_ []
          where
            aux [] ys' zs'            = (reverse zs', [], ys')
            aux xs' [] zs'            = (reverse zs', xs', [])
            aux (x':xs') (y':ys') zs' = aux xs' ys' (f x' y' : zs')

    applicator f xs = uThrow [i|Cannot apply arguments «#{xs}» to a type «#{f}»|]


