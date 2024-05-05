module TypeChecker.Check.Expr where
import qualified Abs
import           Common         (placeOfExpr, uCatch)
import           TypeChecker.TC (TCChecker, Type (TCAny))

typeCheckExpr :: TCChecker Abs.Expr Type
typeCheckExpr e = uCatch (placeOfExpr e) (typeCheckExprImpl e)

typeCheckExprImpl :: TCChecker Abs.Expr Type
typeCheckExprImpl e = return (TCAny, e)

