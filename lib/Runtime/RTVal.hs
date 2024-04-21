{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData  #-}


module Runtime.RTVal where

import           Data.List               (intercalate)
import qualified Data.Map.Strict         as Map
import           Data.String.Interpolate (i)
import           Runtime.RTError         (RTResult)

data RTVal
  = RTInt !Integer
  | RTConstr !String !String ![RTVal]
  | RTFunc !([RTVal] -> RTResult RTVal)
  | RTType !Type
  | -- | RTData TypeName [Arguments] { ConstrName: [ConstrArgs] }
    RTData !String ![String] !(Map.Map String DataConstr)

data Type
  = TInt
  | TVar !String
  | TIdent !String ![Type]
  | TUniv
  deriving (Eq)

newtype DataConstr = DConstr [Type] deriving (Eq)

instance Show RTVal where
  show (RTInt n) = [i|#{n} :: Int|]
  show (RTConstr t n args) = [i|#{t}.#{n}(#{argStrings})|]
    where argStrings = intercalate ", " $ show <$> args
  show (RTFunc _) = "<<function>>"
  show (RTData t args _) = [i|data #{t} (#{argStrings})|]
    where argStrings = intercalate ", " $ show <$> args
  show (RTType t) = show t

instance Eq RTVal where
  RTInt a == RTInt b                     = a == b
  RTConstr ta ca aa == RTConstr tb cb ab = ta == tb && ca == cb && aa == ab
  RTType a == RTType b                   = a == b
  RTData ta aa ma == RTData tb ab mb     = ta == tb && aa == ab && ma == mb
  _ == _                                 = False

instance Show Type where
  show TInt = "Int"
  show (TVar name) = name
  show (TIdent t args) = [i|#{t}(#{argStrings})|]
      where argStrings = intercalate ", " $ show <$> args
  show TUniv = "$Type"

