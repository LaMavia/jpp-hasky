{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes     #-}

module TypeChecker.Utils.Apply where

import           Common                  (uThrow)
import           Data.String.Interpolate (i)
import           TypeChecker.TC          (TC, Type (..))
import           TypeChecker.TCConsts    (pattern TccFn)
import           TypeChecker.Utils.Unify (applyTTUnifier, ttUnify)

tcApply :: Type -> [Type] -> TC Type
tcApply f0 = applicator f0'
    where
    f0' = case f0 of
              TCData _ args _ _ -> TCBound args f0
              f                 -> f
    applicator :: Type -> [Type] -> TC Type
    applicator f [] = return f
    applicator (TccFn [a, r]) (x:rest) = do
      u <- ttUnify a x
      let r' = applyTTUnifier u r
      applicator r' rest
    applicator (TCBound _ t) xs = do
      applicator t xs

    applicator f xs = uThrow [i|Cannot apply arguments «#{xs}» to a type «#{f}»|]


