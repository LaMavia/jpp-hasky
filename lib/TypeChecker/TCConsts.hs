{-# LANGUAGE PatternSynonyms #-}
module TypeChecker.TCConsts where
import qualified Abs
import           TypeChecker.TC (TC, Type (TCAny, TCApp), astOfType)

tccAny :: Type
tccAny = TCAny

tccAnyAst :: TC Abs.Type
tccAnyAst = astOfType tccAny Nothing

tccBoolKey :: String
tccBoolKey = "Bool"

tccBool :: Type
tccBool = TCApp tccBoolKey []

pattern TccBool :: Type
pattern TccBool <- TCApp "Bool" []

tccIntKey :: String
tccIntKey = "Int"

pattern TccInt :: Type
pattern TccInt <- TCApp "Int" [] where
  TccInt = TCApp tccIntKey []


tccFun :: [Type] -> Type
tccFun = TCApp "Fn"

pattern TccFunc :: [Type] -> Type
pattern TccFunc args <- TCApp "Fn" args where
  TccFunc args = TCApp "Fn" args

