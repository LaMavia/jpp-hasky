{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData  #-}

module Runtime.RTVal where

import           Data.List               (intercalate)
import qualified Data.Map.Strict         as Map
import           Data.String.Interpolate (i)
import           Runtime.RTError         (RTResult)

data RTVal
  = RTInt !Int
  | RTConstr !String !String ![RTVal]
  | RTFunc !([RTVal] -> RTResult RTVal)
  | RTType !Type
  | RTData !String !(Map.Map String DataConstr)

data Type
  = TInt
  | TVar !String
  | TIdent !String ![Type]

data DataConstr
  = DConstr ![Type]

instance Show RTVal where
  show (RTInt n) = [i|#{n} :: Int|]
  show (RTConstr t n args) = [i|#{t}.#{n}(#{argStrings})|]
    where
      argStrings = intercalate ", " $ show <$> args
