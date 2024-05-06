module TypeChecker.TCConsts where
import qualified Abs
import           TypeChecker.TC (TC, Type (TCAny, TCApp), astOfType)

tccAny :: Type
tccAny = TCAny

tccAnyAst :: TC Abs.Type
tccAnyAst = astOfType tccAny Nothing

tccBool :: Type
tccBool = TCApp "Bool" []

tccFun :: [Type] -> Type
tccFun = TCApp "Fun"
