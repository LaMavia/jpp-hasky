{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData  #-}

module Runtime.RTVal where

import           Data.List               (intercalate)
import           Data.String.Interpolate (i)
import           Runtime.RTError

data RTVal
  = RTInt !Int
  | RTConstr !String !String ![RTVal]
  | RTFunc !([RTVal] -> RTResult RTVal)
  | RTType !Type

data Type
  = TInt
  | TBool
  | TFunc ![Type] !Type

instance Show RTVal where
  show (RTInt n) = [i|#{n} :: Int|]
  show (RTConstr t n args) = [i|#{t}.#{n}(#{argStrings})|]
    where
      argStrings = intercalate ", " $ show <$> args
