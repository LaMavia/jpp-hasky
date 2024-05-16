module TypeChecker.Utils.Cmp ((<:), (=~=), tcCmpM ) where
import           Control.Monad.Except    (MonadError (catchError))
import           TypeChecker.TC          (TC, Type (..))
import           TypeChecker.Utils.Unify (ttUnify)


infixr 6 <:
(<:) :: Type -> Type -> TC Bool
l <: r = catchError (True <$ ttUnify l r) (return . const False)

infixr 6 =~=
(=~=) :: Type -> Type -> TC Bool
a =~= b = do
  l <- a <: b
  r <- b <: a
  return $ l && r

tcCmpM :: Type -> Type -> TC Ordering
tcCmpM l r = do
  eq <- l =~= r
  lt <- l <: r
  return $ if eq then EQ else if lt then LT else GT
