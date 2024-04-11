{-# LANGUAGE QuasiQuotes    #-}

module Runtime.RTVal where

import Runtime.RTError 
import           Data.String.Interpolate (i)
import Data.List (intercalate)

data RTVal 
  = RTInt Int
  | RTConstr String [RTVal]
  | RTFunc ([RTVal] -> RTResult RTVal)

instance Show RTVal where
  show (RTInt n) = [i|#{n} :: Int|]
  show (RTConstr n args) = [i|#{n}(#{argStrings})|]
    where argStrings = intercalate ", " $ show <$> args
