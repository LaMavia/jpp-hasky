{-# LANGUAGE PatternSynonyms #-}
module TypeChecker.TCConsts where
import qualified Abs
import           TypeChecker.TC (TC, Type (TCAny, TCApp), astOfType)

tccAny :: Type
tccAny = TCAny

tccAnyAst :: TC Abs.Type
tccAnyAst = astOfType tccAny Nothing

pattern TccBool :: Type
pattern TccBool <- TCApp "Bool" [] where
 TccBool = TCApp "Bool" []

pattern TccInt :: Type
pattern TccInt <- TCApp "Int" [] where
  TccInt = TCApp "Int" []

pattern TccFn :: [Type] -> Type
pattern TccFn args <- TCApp "Fn" args where
  TccFn [x] = x
  TccFn xs  = foldr1 (\t r -> TCApp "Fn" [t, r]) xs

pattern TccVoid :: Type
pattern TccVoid <- TCApp "Void" [] where
  TccVoid = TCApp "Void" []

pattern TccUnif :: [Type] -> Type
pattern TccUnif args <- TCApp "$" args where
  TccUnif args = TCApp "$" args
