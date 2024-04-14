{-# LANGUAGE QuasiQuotes #-}

module Runtime.RTVal where

import           Data.List               (intercalate)
import           Data.String.Interpolate (i)
import           Runtime.RTError

data RTVal
  = RTInt Int
  | RTConstr String [RTVal]
  | RTFunc ([RTVal] -> RTResult RTVal)

data Type 
  = TInt
  | TBool
  | TFunc [Type] Type

instance Show RTVal where
  show (RTInt n) = [i|#{n} :: Int|]
  show (RTConstr n args) = [i|#{n}(#{argStrings})|]
    where argStrings = intercalate ", " $ show <$> args

