module TypeChecker.Utils.Cmp ((<:), (=~=)) where
import           Control.Monad.Except    (MonadError (catchError))
import           TypeChecker.TC          (TC, Type (..), getVar)
import           TypeChecker.Utils.Unify (ttUnify)


infixr 6 <:
(<:) :: Type -> Type -> TC Bool
l <: r = catchError (True <$ ttUnify l r) (return . const False)

infixr 6 =~=
(=~=) :: Type -> Type -> TC Bool
a =~= b = do
  l <- a <: b
  r <- b <: a
  return $ l == r
